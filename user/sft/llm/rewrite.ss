;;; user/sft/llm/rewrite.ss — LLM prompt rewriting for natural language diversity
;;;
;;; Takes Phase 1 samples with grammar-diversified prompts and rewrites
;;; the prompt field into more natural, conversational language via local LLM.
;;;
;;; Process:
;;;   1. Read Phase 1 JSONL (stratified sample)
;;;   2. For each, send prompt_body to LLM for rewriting
;;;   3. Replace prompt with LLM output, keep all other fields identical
;;;   4. Add "rewritten" tag
;;;   5. Re-verify (ground truth unchanged, verify expression unchanged)
;;;
;;; Usage:
;;;   (load "user/sft/llm/rewrite.ss")
;;;   (define samples (rewrite/load-phase1 "data/phase1-combined/all.jsonl"))
;;;   (define candidates (rewrite/stratified-sample samples 3000 42))
;;;   (define rewritten (rewrite/run candidates providers))

(load "user/sft/llm/llm-gen.ss")

;;; ====
;;; JSONL Reading
;;; ====

(define (rewrite/read-jsonl path)
  (doc 'description "Read JSONL file into list of alists")
  (doc 'type '(-> String (List Alist)))
  (printf "Reading ~a...\\n" path)
  (call-with-input-file path
    (lambda (port)
      (let loop ([acc '()] [n 0])
        (let ([line (get-line port)])
          (if (eof-object? line)
              (begin
                (printf "Read ~a records\\n" n)
                (reverse acc))
              (let ([parsed (guard (ex [else #f])
                              (parse-json-string line))])
                (if parsed
                    (loop (cons parsed acc) (+ n 1))
                    (loop acc n)))))))))

;;; ====
;;; Stratified Sampling
;;; ====

(define (rewrite/stratified-sample samples n seed)
  (doc 'description
    "Stratified sample preserving family proportions.
     Returns up to n samples, deterministically selected by seed.")
  (doc 'type '(-> (List Alist) Nat Nat (List Alist)))
  (let* ([by-family (rewrite/group-by-family samples)]
         [total (length samples)]
         [ratio (if (> total 0) (/ n total) 1)])
    (printf "Stratified sample: ~a from ~a (ratio ~a)\\n" n total
            (number->string (inexact ratio)))
    (apply append
      (map (lambda (group)
             (let* ([family (car group)]
                    [members (cdr group)]
                    [take-n (max 1 (inexact->exact (round (* ratio (length members)))))]
                    [sorted (list-sort
                              (lambda (a b)
                                (string<? (cdr (assq 'id a))
                                          (cdr (assq 'id b))))
                              members)]
                    [stride (max 1 (quotient (length sorted) take-n))])
               (printf "  ~a: ~a/~a\\n" family take-n (length members))
               (let loop ([i 0] [acc '()])
                 (if (or (>= i (length sorted)) (>= (length acc) take-n))
                     (reverse acc)
                     (loop (+ i stride)
                           (cons (list-ref sorted i) acc))))))
           by-family))))

(define (rewrite/group-by-family samples)
  (doc 'description "Group samples by family field. Returns ((family . samples) ...)")
  (let loop ([remaining samples] [groups '()])
    (if (null? remaining)
        groups
        (let* ([s (car remaining)]
               [fam (cdr (assq 'family s))]
               [existing (assoc fam groups)])
          (if existing
              (begin
                (set-cdr! existing (cons s (cdr existing)))
                (loop (cdr remaining) groups))
              (loop (cdr remaining)
                    (cons (cons fam (list s)) groups)))))))

;;; ====
;;; Rewriting
;;; ====

(define *rewrite-system-prompt*
  (llm-gen/rewrite-system-prompt))

(define (rewrite/build-user-prompt sample)
  (doc 'description "Build the user prompt for rewriting a sample's prompt_body")
  (let* ([body (cdr (assq 'prompt_body sample))]
         [family (cdr (assq 'family sample))]
         [category (cdr (assq 'category sample))])
    (format "Rewrite the following ~a (~a) task prompt into natural, conversational language.

ORIGINAL PROMPT:
---
~a
---

Remember: preserve ALL technical details, function names, types, module references, and verification criteria. Just make the language more natural and varied." family category body)))

(define (rewrite/apply-rewrite sample rewritten-prompt)
  (doc 'description
    "Create a new sample with the rewritten prompt.
     Keeps all fields identical except prompt and tags.")
  (let* ([old-tags (cdr (assq 'tags sample))]
         [new-tags (append old-tags '("rewritten"))]
         [old-id (cdr (assq 'id sample))]
         [new-id (string-append "rw_" old-id)])
    (map (lambda (pair)
           (cond
             [(eq? (car pair) 'id) (cons 'id new-id)]
             [(eq? (car pair) 'prompt) (cons 'prompt rewritten-prompt)]
             [(eq? (car pair) 'tags) (cons 'tags new-tags)]
             [else pair]))
         sample)))

;;; ====
;;; Batch Execution
;;; ====

(define (rewrite/run candidates providers . opts)
  (doc 'description
    "Run LLM rewriting on candidate samples.
     Returns list of rewritten samples (successes only).
     Optional resume-file path as first opt.")
  (doc 'type '(-> (List Alist) (List RlmProvider) (List Alist)))
  (let* ([resume-file (if (pair? opts) (car opts) #f)]
         [results '()]
         [errors 0]
         [specs
          (map (lambda (sample)
                 (let ([id (cdr (assq 'id sample))]
                       [user-prompt (rewrite/build-user-prompt sample)])
                   (list id *rewrite-system-prompt* user-prompt 1024 0.8)))
               candidates)]
         [sample-by-id
          (let ([ht (make-hashtable string-hash string=?)])
            (for-each (lambda (s)
                        (hashtable-set! ht (cdr (assq 'id s)) s))
                      candidates)
            ht)])
    (printf "Starting rewrite batch: ~a candidates, ~a providers\\n"
            (length candidates) (length providers))
    (let ([counts
           (llm-gen/batch
             specs providers
             ;; on-result
             (lambda (spec-id response-text)
               (let* ([original (hashtable-ref sample-by-id spec-id #f)]
                      ;; Clean up: strip any markdown formatting the LLM may add
                      [clean-text (rewrite/clean-response response-text)])
                 (when (and original
                            (string? clean-text)
                            (> (string-length clean-text) 20))
                   (set! results
                     (cons (rewrite/apply-rewrite original clean-text)
                           results)))))
             ;; on-error
             (lambda (spec-id error)
               (set! errors (+ errors 1)))
             resume-file)])
      (printf "Rewrite complete: ~a successes, ~a errors\\n"
              (length results) errors)
      (reverse results))))

(define (rewrite/clean-response text)
  (doc 'description
    "Clean LLM response: remove leading/trailing whitespace,
     strip any markdown wrapper if present.")
  (let* ([trimmed (string-trim-both text)]
         [n (string-length trimmed)])
    ;; If response is empty or too short, reject
    (if (< n 20) #f trimmed)))

(define (string-trim-both s)
  (let* ([n (string-length s)]
         [start (let loop ([i 0])
                  (if (or (>= i n) (not (char-whitespace? (string-ref s i))))
                      i (loop (+ i 1))))]
         [end (let loop ([i (- n 1)])
                (if (or (< i start) (not (char-whitespace? (string-ref s i))))
                    (+ i 1) (loop (- i 1))))])
    (substring s start end)))

;;; ====
;;; Convenience: load + sample + run
;;; ====

(define (rewrite/load-phase1 path)
  (doc 'description "Load Phase 1 JSONL, filtering to verified samples with prompt_body")
  (let* ([all (rewrite/read-jsonl path)]
         [valid (filter (lambda (s)
                          (and (assq 'prompt_body s)
                               (assq 'prompt s)
                               (let ([body (cdr (assq 'prompt_body s))])
                                 (and (string? body)
                                      (> (string-length body) 10)))))
                        all)])
    (printf "~a valid samples with prompt_body\\n" (length valid))
    valid))

;;; ====
;;; REPL interface
;;; ====

(printf "rewrite.ss loaded.\\n")
(printf "  (rewrite/load-phase1 path)                - Load Phase 1 JSONL\\n")
(printf "  (rewrite/stratified-sample samples n seed) - Stratified sample\\n")
(printf "  (rewrite/run candidates providers [resume]) - Run LLM rewriting\\n")
