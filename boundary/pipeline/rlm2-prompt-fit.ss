;;; boundary/pipeline/rlm2-prompt-fit.ss — On-Policy Prompt Fitting
;;;
;;; Rewrites the RLM system prompt using the target model itself,
;;; so the prompt style matches the model's natural distribution.
;;; Caches fitted prompts at /tmp/fold-rlm-prompt-{slug}.txt.
;;;
;;; The structural content (action names, syntax) is validated after
;;; rewriting — if any action keywords are missing, we reject and
;;; fall back to the original prompt.

(load "boundary/pipeline/rlm-client.ss")
(load "lattice/pipeline/rlm2-hud.ss")

(doc 'module 'rlm2-prompt-fit)
(doc 'description "On-policy prompt fitting: rewrite system prompts using the target model, with structural validation and /tmp caching.")
(doc 'layer 'boundary)
(doc 'purity 'impure)

;;; ====
;;; Action Keywords (structural invariants)
;;; ====
;;; These must all appear in the rewritten prompt. If any are missing,
;;; the rewrite is rejected.

(define *rlm2-required-keywords*
  '("think" "eval" "submit" "begin"
    "search" "inspect" "exports" "load" "delegate"
    "lookup" "definition" "symbols" "outline"
    "store" "peek" "grep" "slice" "map-chunks"
    "plan!" "journal" "recall" "recall-step"
    "memorize" "remember"
    "idle" "reframe"))

;;; ====
;;; Validation
;;; ====

;;; rlm2-prompt-valid? : String -> Bool
;;; Check that all required action keywords appear in the prompt.
(define (rlm2-prompt-valid? prompt)
  (let loop ([keywords *rlm2-required-keywords*])
    (cond
      [(null? keywords) #t]
      [(rlm2-string-contains-word? prompt (car keywords))
       (loop (cdr keywords))]
      [else #f])))

;;; rlm2-prompt-missing-keywords : String -> (List String)
;;; Returns list of keywords missing from the prompt.
(define (rlm2-prompt-missing-keywords prompt)
  (filter (lambda (kw) (not (rlm2-string-contains-word? prompt kw)))
          *rlm2-required-keywords*))

;;; Simple substring check — keyword must appear as (keyword or "keyword"
(define (rlm2-string-contains-word? haystack word)
  (let ([paren-form (string-append "(" word)]
        [hlen (string-length haystack)])
    (let loop ([i 0])
      (cond
        [(>= i hlen) #f]
        [(and (<= (+ i (string-length paren-form)) hlen)
              (string=? (substring haystack i (+ i (string-length paren-form)))
                        paren-form))
         #t]
        ;; Also match bare word preceded by space/newline
        [(and (> i 0)
              (<= (+ i (string-length word)) hlen)
              (memv (string-ref haystack (- i 1)) '(#\space #\newline #\tab #\, #\:))
              (string=? (substring haystack i (+ i (string-length word))) word))
         #t]
        [else (loop (+ i 1))]))))

;;; ====
;;; Model Slug
;;; ====

;;; rlm2-model-slug : String -> String
;;; Convert a model ID to a filesystem-safe slug.
;;; "/models/Qwen3.5-27B-NVFP4" -> "qwen3.5-27b-nvfp4"
(define (rlm2-model-slug model-id)
  (let* ([s (let loop ([chars (string->list model-id)] [acc '()] [after-slash #f])
              (cond
                [(null? chars) (list->string (reverse acc))]
                [(char=? (car chars) #\/)
                 (loop (cdr chars) '() #t)]  ; restart after last slash
                [else
                 (loop (cdr chars) (cons (car chars) acc) after-slash)]))]
         ;; Lowercase
         [s (list->string (map char-downcase (string->list s)))]
         ;; Replace non-alphanumeric (except hyphen/dot) with hyphen
         [s (list->string
              (map (lambda (c)
                     (if (or (char-alphabetic? c) (char-numeric? c)
                             (char=? c #\-) (char=? c #\.))
                         c #\-))
                   (string->list s)))])
    s))

;;; ====
;;; Cache
;;; ====

(define (rlm2-prompt-cache-path model-id)
  (format "/tmp/fold-rlm-prompt-~a.txt" (rlm2-model-slug model-id)))

(define (rlm2-prompt-cache-read model-id)
  (let ([path (rlm2-prompt-cache-path model-id)])
    (if (file-exists? path)
        (guard (ex [else #f])
          (call-with-input-file path
            (lambda (p)
              (let loop ([lines '()])
                (let ([line (get-line p)])
                  (if (eof-object? line)
                      (apply string-append
                             (map (lambda (l) (string-append l "\n"))
                                  (reverse lines)))
                      (loop (cons line lines))))))))
        #f)))

(define (rlm2-prompt-cache-write model-id prompt)
  (let ([path (rlm2-prompt-cache-path model-id)])
    (call-with-output-file path
      (lambda (p) (display prompt p))
      'replace)))

;;; ====
;;; Fitting
;;; ====

;;; The meta-prompt sent to the target model asking it to rewrite.
(define *rlm2-fit-instruction*
  (string-append
    "Below is a system prompt that will be given to you in future interactions. "
    "Rewrite it in your own words and style while preserving ALL of the following exactly:\n\n"
    "1. Every action name and its syntax (e.g., (think text), (eval expr), (submit expr), etc.)\n"
    "2. Every semantic rule (e.g., begin stops on first error, think is ephemeral, etc.)\n"
    "3. The complete action grammar with all action categories\n"
    "4. All pre-loaded utility function names\n"
    "5. The example at the end\n\n"
    "You may freely change:\n"
    "- Word choice, sentence structure, explanation style\n"
    "- The ordering and grouping of information\n"
    "- How concepts are introduced and connected\n"
    "- Emphasis and formatting\n\n"
    "Output ONLY the rewritten system prompt, nothing else. "
    "No preamble, no commentary, no meta-discussion.\n\n"
    "---\n\n"))

;;; rlm2-fit-prompt : RlmProvider String -> String
;;; Takes a provider and the original system prompt. Returns a fitted prompt
;;; (from cache or by calling the model). Falls back to original on failure.
(define (rlm2-fit-prompt provider original-prompt)
  (let* ([model-id (rlm-provider-model-id provider)]
         [cached (rlm2-prompt-cache-read model-id)])
    (cond
      ;; Cache hit — validate and return
      [cached
       (if (rlm2-prompt-valid? cached)
           (begin
             (display (format "[prompt-fit] Cache hit for ~a\n"
                              (rlm2-model-slug model-id)))
             cached)
           (begin
             (display (format "[prompt-fit] Cache invalid for ~a, refitting\n"
                              (rlm2-model-slug model-id)))
             (rlm2-fit-prompt-fresh provider original-prompt)))]
      ;; Cache miss — call model
      [else
       (rlm2-fit-prompt-fresh provider original-prompt)])))

;;; rlm2-fit-prompt-fresh : RlmProvider String -> String
;;; Actually calls the model to rewrite. Validates and caches on success.
(define (rlm2-fit-prompt-fresh provider original-prompt)
  (let* ([model-id (rlm-provider-model-id provider)]
         [slug (rlm2-model-slug model-id)]
         [messages `(((role . "user")
                      (content . ,(string-append *rlm2-fit-instruction*
                                                 original-prompt))))]
         [_ (display (format "[prompt-fit] Fitting prompt for ~a...\n" slug))]
         [_ (flush-output-port)]
         [result (rlm-chat provider messages 4096 0.3)])
    (cond
      [(rlm-chat-ok? result)
       (let ([rewritten (rlm-chat-text result)])
         (cond
           [(rlm2-prompt-valid? rewritten)
            (rlm2-prompt-cache-write model-id rewritten)
            (display (format "[prompt-fit] Success — cached at ~a\n"
                             (rlm2-prompt-cache-path model-id)))
            rewritten]
           [else
            (let ([missing (rlm2-prompt-missing-keywords rewritten)])
              (display (format "[prompt-fit] Rejected — missing keywords: ~a\n"
                               missing))
              (display "[prompt-fit] Falling back to original prompt\n")
              original-prompt)]))]
      [else
       (display (format "[prompt-fit] LLM call failed: ~a\n"
                         (rlm-chat-error-msg result)))
       (display "[prompt-fit] Falling back to original prompt\n")
       original-prompt])))

;;; ====
;;; Convenience: fit from standard components
;;; ====

;;; rlm2-get-fitted-prompt : RlmProvider String -> String
;;; Build the full system prompt from base-prompt, then fit it.
;;; This is the main entry point for the bench harness.
(define (rlm2-get-fitted-prompt provider base-prompt)
  (let ([original (rlm2-build-system-prompt base-prompt)])
    (rlm2-fit-prompt provider original)))
