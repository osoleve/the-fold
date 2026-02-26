;;; user/sft/llm/alternatives.ss — LLM-generated alternative implementations
;;;
;;; For spec-to-code tasks, generates alternative correct implementations
;;; that pass the same verify expression as the original.
;;;
;;; Process:
;;;   1. Read Phase 1 spec-to-code samples (medium+ difficulty, with verify)
;;;   2. Send type + description + "write DIFFERENT implementation" to LLM
;;;   3. LLM's implementation becomes ground_truth
;;;   4. Original verify_expr unchanged — must still pass
;;;   5. Add "alternative" tag
;;;
;;; Usage:
;;;   (load "user/sft/llm/alternatives.ss")
;;;   (define candidates (alt/select-candidates "data/phase1-combined/all.jsonl" 2000))
;;;   (define results (alt/run candidates providers))

(load "user/sft/llm/llm-gen.ss")

;;; ====
;;; Candidate Selection
;;; ====

(define (alt/read-jsonl path)
  (doc 'description "Read JSONL into alist records")
  (call-with-input-file path
    (lambda (port)
      (let loop ([acc '()] [n 0])
        (let ([line (get-line port)])
          (if (eof-object? line)
              (begin (printf "Read ~a records\\n" n) (reverse acc))
              (let ([parsed (guard (ex [else #f])
                              (parse-json-string line))])
                (if parsed
                    (loop (cons parsed acc) (+ n 1))
                    (loop acc n)))))))))

(define (alt/select-candidates path n)
  (doc 'description
    "Select candidates for alternative generation.
     Filters: spec_to_code family, medium+ difficulty, non-empty verify_expr.
     Returns up to n candidates, deterministically sampled.")
  (doc 'type '(-> String Nat (List Alist)))
  (let* ([all (alt/read-jsonl path)]
         [eligible
          (filter (lambda (s)
                    (and
                      ;; Must be spec_to_code family
                      (let ([fam (assq 'family s)])
                        (and fam (string=? (cdr fam) "spec_to_code")))
                      ;; Must have non-empty verify_expr
                      (let ([ve (assq 'verify_expr s)])
                        (and ve (string? (cdr ve))
                             (> (string-length (cdr ve)) 5)))
                      ;; Must have ground_truth
                      (let ([gt (assq 'ground_truth s)])
                        (and gt (string? (cdr gt))
                             (> (string-length (cdr gt)) 5)))
                      ;; Medium+ difficulty preferred
                      (let ([diff (assq 'difficulty s)])
                        (and diff
                             (member (cdr diff) '("medium" "hard"))))))
                  all)]
         ;; Sort for determinism, then take n via stride
         [sorted (list-sort
                   (lambda (a b)
                     (string<? (cdr (assq 'id a))
                               (cdr (assq 'id b))))
                   eligible)]
         [total (length sorted)]
         [stride (max 1 (quotient total n))]
         [selected
          (let loop ([i 0] [acc '()])
            (if (or (>= i total) (>= (length acc) n))
                (reverse acc)
                (loop (+ i stride) (cons (list-ref sorted i) acc))))])
    (printf "Alt candidates: ~a eligible, ~a selected\\n" total (length selected))
    selected))

;;; ====
;;; LLM Prompting
;;; ====

(define *alt-system-prompt*
  "You are an expert Scheme programmer working in The Fold, a computing environment built on Chez Scheme.

Your task is to write an ALTERNATIVE implementation of a function. The implementation must:
- Be semantically correct (pass the same tests as the original)
- Be DIFFERENT from the original (use a different algorithm, decomposition, or approach)
- Be idiomatic Fold Scheme
- Use standard library conventions (car/cdr, let/let*, define, lambda)

Output ONLY the function definition wrapped in a ```scheme block. No explanation.")

(define (alt/build-user-prompt sample)
  (doc 'description "Build LLM prompt asking for alternative implementation")
  (let* ([fn-name (cdr (assq 'source_function sample))]
         [module (cdr (assq 'source_module sample))]
         [body (cdr (assq 'prompt_body sample))]
         [gt (cdr (assq 'ground_truth sample))]
         [ve (cdr (assq 'verify_expr sample))])
    (format "Write a DIFFERENT implementation of `~a` (from ~a).

ORIGINAL TASK:
~a

ORIGINAL IMPLEMENTATION (do NOT copy this — write something different):
```scheme
~a
```

VERIFICATION (your implementation must pass this):
```scheme
~a
```

Write ONLY the alternative (define ...) form. Use a different approach:
- If original uses recursion, try iteration (or vice versa)
- If original uses car/cdr, try pattern matching
- If original uses let-binding, try different decomposition
- If original uses fold, try explicit recursion (or vice versa)"
            fn-name module body gt ve)))

;;; ====
;;; Result Processing
;;; ====

(define (alt/extract-define text)
  (doc 'description
    "Extract a (define ...) form from LLM response.
     Tries ```scheme block first, then raw text parsing.")
  (let ([block (llm-gen/extract-scheme-block text)])
    (if block
        ;; Got a code block — extract the define form
        (let ([sexp (llm-gen/parse-sexp-string block)])
          (if (and (pair? sexp) (eq? (car sexp) 'define))
              block
              ;; Block didn't start with define — try parsing multiple forms
              (let ([forms (llm-gen/parse-all-sexps block)])
                (let find-define ([fs forms])
                  (if (null? fs) #f
                      (if (and (pair? (car fs)) (eq? (car (car fs)) 'define))
                          (let ([p (open-output-string)])
                            (write (car fs) p)
                            (get-output-string p))
                          (find-define (cdr fs))))))))
        ;; No code block — try raw parsing
        (let ([forms (llm-gen/parse-all-sexps text)])
          (let find-define ([fs forms])
            (if (null? fs) #f
                (if (and (pair? (car fs)) (eq? (car (car fs)) 'define))
                    (let ([p (open-output-string)])
                      (write (car fs) p)
                      (get-output-string p))
                    (find-define (cdr fs)))))))))

(define (alt/make-sample original alt-ground-truth)
  (doc 'description
    "Create an alternative sample from original + new ground truth.
     Preserves all metadata, replaces ground_truth, adds tags.")
  (let* ([old-id (cdr (assq 'id original))]
         [new-id (string-append "alt_" old-id)]
         [old-tags (cdr (assq 'tags original))]
         [new-tags (append old-tags '("alternative"))])
    (map (lambda (pair)
           (cond
             [(eq? (car pair) 'id) (cons 'id new-id)]
             [(eq? (car pair) 'ground_truth) (cons 'ground_truth alt-ground-truth)]
             [(eq? (car pair) 'tags) (cons 'tags new-tags)]
             [else pair]))
         original)))

;;; ====
;;; Batch Execution
;;; ====

(define (alt/run candidates providers . opts)
  (doc 'description
    "Run LLM alternative generation on candidates.
     Returns list of alternative samples (those with valid define forms).
     Optional resume-file as first opt.")
  (doc 'type '(-> (List Alist) (List RlmProvider) (List Alist)))
  (let* ([resume-file (if (pair? opts) (car opts) #f)]
         [results '()]
         [parse-fails 0]
         [specs
          (map (lambda (sample)
                 (let ([id (cdr (assq 'id sample))]
                       [user-prompt (alt/build-user-prompt sample)])
                   (list id *alt-system-prompt* user-prompt 1024 0.4)))
               candidates)]
         [sample-by-id
          (let ([ht (make-hashtable string-hash string=?)])
            (for-each (lambda (s)
                        (hashtable-set! ht (cdr (assq 'id s)) s))
                      candidates)
            ht)])
    (printf "Starting alternatives batch: ~a candidates, ~a providers\\n"
            (length candidates) (length providers))
    (let ([counts
           (llm-gen/batch
             specs providers
             ;; on-result
             (lambda (spec-id response-text)
               (let* ([original (hashtable-ref sample-by-id spec-id #f)]
                      [alt-code (alt/extract-define response-text)])
                 (if (and original alt-code)
                     ;; Check it's genuinely different from original
                     (let ([orig-gt (cdr (assq 'ground_truth original))])
                       (if (string=? alt-code orig-gt)
                           (set! parse-fails (+ parse-fails 1))
                           (set! results
                             (cons (alt/make-sample original alt-code)
                                   results))))
                     (set! parse-fails (+ parse-fails 1)))))
             ;; on-error
             (lambda (spec-id error)
               (set! parse-fails (+ parse-fails 1)))
             resume-file)])
      (printf "Alternatives complete: ~a valid, ~a parse/dup failures\\n"
              (length results) parse-fails)
      (reverse results))))

;;; ====
;;; REPL interface
;;; ====

(printf "alternatives.ss loaded.\\n")
(printf "  (alt/select-candidates path n)    - Select medium+ s2c with verify\\n")
(printf "  (alt/run candidates providers)     - Generate alternatives via LLM\\n")
(printf "  (alt/extract-define text)          - Extract (define ...) from response\\n")
