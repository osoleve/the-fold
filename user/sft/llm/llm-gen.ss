;;; user/sft/llm/llm-gen.ss — Shared LLM infrastructure for Phase 2
;;;
;;; Wraps boundary/pipeline/rlm-client.ss with:
;;;   - Dynamic model discovery via /v1/models
;;;   - Round-robin batch orchestration across providers
;;;   - Resume support for crash recovery
;;;   - Scheme code extraction from LLM responses
;;;
;;; Usage:
;;;   (load "user/sft/llm/llm-gen.ss")
;;;   (define providers (llm-gen/discover-providers))
;;;   (llm-gen/call (car providers) "system" "user" 1024 0.7)
;;;   (llm-gen/batch specs providers on-result on-error)

(load "boundary/pipeline/rlm-client.ss")
(load "boundary/io/json.ss")

;;; ====
;;; Provider Discovery
;;; ====

(define *llm-gen-default-endpoints*
  '("http://elsie-1:8000" "http://elsie-2:8000"))

(define (llm-gen/query-models endpoint)
  (doc 'description "Query /v1/models to discover available model IDs")
  (doc 'type '(-> String (Result (List String) String)))
  (guard (ex [else (list 'err (format "Models query failed: ~a"
                                       (if (message-condition? ex)
                                           (condition-message ex)
                                           "unknown")))])
    ;; Build a temporary provider just for the HTTP call
    (let* ([url (string-append endpoint "/v1/models")]
           [result (rlm-relay-request! url '() '() 10)])
      (if (rlm-chat-ok? result)
          (let ([parsed (parse-json-string (rlm-chat-text result))])
            (if (and parsed (assq 'data parsed))
                (let ([models (map (lambda (m) (cdr (assq 'id m)))
                                   (cdr (assq 'data parsed)))])
                  (list 'ok models))
                (list 'err "No 'data' in /v1/models response")))
          result))))

(define (llm-gen/make-provider endpoint)
  (doc 'description
    "Create a provider for an endpoint, auto-discovering the model ID.
     Falls back to first available model if multiple are served.")
  (doc 'type '(-> String (Result RlmProvider String)))
  (let ([result (llm-gen/query-models endpoint)])
    (if (and (pair? result) (eq? (car result) 'ok))
        (let ([models (cadr result)])
          (if (null? models)
              (list 'err (format "No models at ~a" endpoint))
              (let ([model-id (car models)])
                (printf "  Provider: ~a → ~a\\n" endpoint model-id)
                (list 'ok
                      (make-rlm-provider
                        (string-append endpoint "/v1/chat/completions")
                        model-id
                        #f       ; no auth for local vLLM
                        'openai)))))
        result)))

(define (llm-gen/discover-providers . endpoints)
  (doc 'description
    "Discover providers from endpoints. Returns list of live providers.
     Uses *llm-gen-default-endpoints* if none given.")
  (doc 'type '(-> (List String) ... (List RlmProvider)))
  (let* ([eps (if (null? endpoints)
                  *llm-gen-default-endpoints*
                  endpoints)]
         [results (map llm-gen/make-provider eps)]
         [live (filter-map
                 (lambda (r)
                   (and (pair? r) (eq? (car r) 'ok) (cadr r)))
                 results)])
    (printf "Discovered ~a live provider(s)\\n" (length live))
    live))

;;; ====
;;; Core LLM Call
;;; ====

(define (llm-gen/call provider system-prompt user-prompt max-tokens temperature)
  (doc 'description "Single LLM call with system+user messages")
  (doc 'type '(-> RlmProvider String String Nat Float
                  (Result String (Pair Symbol String))))
  (let ([messages `(((role . "system") (content . ,system-prompt))
                    ((role . "user")   (content . ,user-prompt)))])
    (rlm-chat provider messages max-tokens temperature)))

(define (llm-gen/call-multi provider messages max-tokens temperature)
  (doc 'description "LLM call with arbitrary message list")
  (doc 'type '(-> RlmProvider (List Message) Nat Float
                  (Result String (Pair Symbol String))))
  (rlm-chat provider messages max-tokens temperature))

;;; ====
;;; Response Parsing
;;; ====

(define (llm-gen/extract-scheme-block text)
  (doc 'description
    "Extract first ```scheme ... ``` code block from LLM response.
     Returns the code string, or #f if no block found.")
  (doc 'type '(-> String (Option String)))
  (let* ([marker-open "```scheme"]
         [marker-close "```"]
         [n (string-length text)]
         [open-len (string-length marker-open)])
    (let find-open ([i 0])
      (cond
        [(> (+ i open-len) n) #f]
        [(string=? (substring text i (+ i open-len)) marker-open)
         ;; Found opening — skip to next newline
         (let find-nl ([j (+ i open-len)])
           (cond
             [(>= j n) #f]
             [(char=? (string-ref text j) #\newline)
              ;; Content starts at j+1 — find closing ```
              (let find-close ([k (+ j 1)])
                (cond
                  [(> (+ k 3) n)
                   ;; No closing marker — return rest of text
                   (substring text (+ j 1) n)]
                  [(and (char=? (string-ref text k) #\`)
                        (char=? (string-ref text (+ k 1)) #\`)
                        (char=? (string-ref text (+ k 2)) #\`))
                   (substring text (+ j 1) k)]
                  [else (find-close (+ k 1))]))]
             [else (find-nl (+ j 1))]))]
        [else (find-open (+ i 1))]))))

(define (llm-gen/extract-all-scheme-blocks text)
  (doc 'description "Extract ALL ```scheme blocks from response")
  (doc 'type '(-> String (List String)))
  (let* ([marker-open "```scheme"]
         [marker-close "```"]
         [n (string-length text)]
         [open-len (string-length marker-open)])
    (let loop ([i 0] [acc '()])
      (cond
        [(> (+ i open-len) n) (reverse acc)]
        [(string=? (substring text i (+ i open-len)) marker-open)
         (let find-nl ([j (+ i open-len)])
           (cond
             [(>= j n) (reverse acc)]
             [(char=? (string-ref text j) #\newline)
              (let find-close ([k (+ j 1)])
                (cond
                  [(> (+ k 3) n)
                   (reverse (cons (substring text (+ j 1) n) acc))]
                  [(and (char=? (string-ref text k) #\`)
                        (char=? (string-ref text (+ k 1)) #\`)
                        (char=? (string-ref text (+ k 2)) #\`))
                   (loop (+ k 3) (cons (substring text (+ j 1) k) acc))]
                  [else (find-close (+ k 1))]))]
             [else (find-nl (+ j 1))]))]
        [else (loop (+ i 1) acc)]))))

(define (llm-gen/parse-sexp-string text)
  (doc 'description
    "Parse a string as a single S-expression. Returns sexp or #f on error.")
  (doc 'type '(-> String (Option Sexp)))
  (guard (ex [else #f])
    (read (open-input-string text))))

(define (llm-gen/parse-all-sexps text)
  (doc 'description
    "Parse a string as multiple S-expressions. Returns list of sexps.")
  (doc 'type '(-> String (List Sexp)))
  (guard (ex [else '()])
    (let ([port (open-input-string text)])
      (let loop ([acc '()])
        (let ([expr (read port)])
          (if (eof-object? expr)
              (reverse acc)
              (loop (cons expr acc))))))))

;;; ====
;;; Batch Orchestration
;;; ====

(define (llm-gen/batch specs providers on-result on-error . opts)
  (doc 'description
    "Process a list of specs through LLM providers with round-robin.
     specs: list of (spec-id system-prompt user-prompt max-tokens temperature)
     on-result: (lambda (spec-id response-text) ...) — called on success
     on-error: (lambda (spec-id error) ...) — called on failure
     opts: optional resume-file path (string)

     Logs completed spec-ids to resume-file for crash recovery.
     Returns (completed-count . error-count).")
  (let* ([resume-file (if (pair? opts) (car opts) #f)]
         [done-ids (if (and resume-file (file-exists? resume-file))
                       (llm-gen/load-resume-ids resume-file)
                       '())]
         [remaining (filter (lambda (s) (not (member (car s) done-ids))) specs)]
         [n-providers (length providers)]
         [resume-port (if resume-file
                         (open-file-output-port resume-file
                           (file-options no-fail append)
                           (buffer-mode line)
                           (make-transcoder (utf-8-codec)))
                         #f)])
    (when (pair? done-ids)
      (printf "Resuming: ~a already done, ~a remaining\\n"
              (length done-ids) (length remaining)))
    (let loop ([rem remaining] [idx 0] [ok-count (length done-ids)] [err-count 0])
      (if (null? rem)
          (begin
            (when resume-port (close-port resume-port))
            (cons ok-count err-count))
          (let* ([spec (car rem)]
                 [spec-id (list-ref spec 0)]
                 [sys-prompt (list-ref spec 1)]
                 [usr-prompt (list-ref spec 2)]
                 [max-tok (list-ref spec 3)]
                 [temp (list-ref spec 4)]
                 [provider (list-ref providers (modulo idx n-providers))]
                 [result (llm-gen/call provider sys-prompt usr-prompt max-tok temp)])
            (cond
              [(rlm-chat-ok? result)
               (on-result spec-id (rlm-chat-text result))
               (when resume-port
                 (put-string resume-port (format "~a\\n" spec-id))
                 (flush-output-port resume-port))
               (when (= 0 (modulo (+ ok-count 1) 50))
                 (printf "  Progress: ~a/~a done (~a errors)\\n"
                         (+ ok-count 1) (+ (length remaining) (length done-ids))
                         err-count))
               (loop (cdr rem) (+ idx 1) (+ ok-count 1) err-count)]
              [else
               (on-error spec-id result)
               (loop (cdr rem) (+ idx 1) ok-count (+ err-count 1))]))))))

(define (llm-gen/load-resume-ids path)
  (doc 'description "Load completed spec IDs from resume file")
  (guard (ex [else '()])
    (call-with-input-file path
      (lambda (p)
        (let loop ([acc '()])
          (let ([line (get-line p)])
            (if (eof-object? line)
                acc
                (loop (cons (string-trim-right line) acc)))))))))

(define (string-trim-right s)
  (let loop ([i (- (string-length s) 1)])
    (cond
      [(< i 0) ""]
      [(char-whitespace? (string-ref s i)) (loop (- i 1))]
      [else (substring s 0 (+ i 1))])))

;;; ====
;;; Prompt Helpers
;;; ====

(define (llm-gen/system-prompt-sft family)
  (doc 'description "Standard system prompt for SFT task generation")
  (format "You are an expert Scheme programmer working in The Fold, a content-addressable homoiconic computing environment built on Chez Scheme.

Your task is to generate ~a training examples. For each:
- Write idiomatic Fold Scheme code
- Use standard library conventions (car/cdr, let/let*, define, lambda)
- Ensure all code is syntactically valid S-expressions
- Wrap code in ```scheme blocks

Be precise. Do not include explanatory text outside of code blocks unless asked." family))

(define (llm-gen/rewrite-system-prompt)
  (doc 'description "System prompt for prompt rewriting tasks")
  "You are a technical writing expert specializing in programming task descriptions.

Your job is to rewrite formulaic, template-generated programming task prompts into natural, conversational language while preserving ALL technical requirements.

Rules:
- Keep every technical detail (function names, types, module references, assertions)
- Make the language feel like a senior developer explaining a task to a colleague
- Vary sentence structure and vocabulary
- Do not add requirements not in the original
- Do not remove any verification criteria or code blocks
- Output ONLY the rewritten prompt, no meta-commentary")

;;; ====
;;; REPL interface
;;; ====

(printf "llm-gen.ss loaded.\\n")
(printf "  (llm-gen/discover-providers)          - Find live vLLM endpoints\\n")
(printf "  (llm-gen/call provider sys usr tok t)  - Single LLM call\\n")
(printf "  (llm-gen/batch specs provs on-ok on-err [resume-file])\\n")
(printf "  (llm-gen/extract-scheme-block text)    - Extract ```scheme block\\n")
(printf "  (llm-gen/parse-sexp-string text)       - Parse string as S-expression\\n")
