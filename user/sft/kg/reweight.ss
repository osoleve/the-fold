;;; user/sft/kg/reweight.ss — Performance-based concept reweighting
;;;
;;; Closed-loop: generate → train → eval per concept → adjust weights.
;;; Concepts where model underperforms get upweighted in the grammar DSL.
;;;
;;; Usage:
;;;   (load "user/sft/kg/reweight.ss")
;;;   (define weights (reweight/from-eval "data/eval-results.jsonl" coverage-report))
;;;   (reweight/print-weights weights)
;;;   (reweight/export-grammar-weights weights "user/sft/concept-weights.ss")

(unless (top-level-bound? 'make-sample)
  (load "user/sft/extract/emit.ss"))
(load "boundary/io/json.ss")

;;; ====
;;; Configuration
;;; ====

(define *reweight-alpha* 0.5)         ; learning rate for weight updates
(define *reweight-min-samples* 5)     ; min eval samples to consider
(define *reweight-max-weight* 5.0)    ; cap weight to prevent runaway

;;; ====
;;; Eval result parsing
;;; ====

(define (reweight/read-eval-results path)
  ;; Read evaluation results JSONL.
  ;; Expected format: {source_module, source_function, family, correct: bool}
  (let ([p (open-input-file path)])
    (let loop ([acc '()])
      (let ([line (get-line p)])
        (if (eof-object? line)
            (begin (close-input-port p) (reverse acc))
            (let ([parsed (guard (ex [else #f])
                            (parse-json-string line))])
              (loop (if parsed (cons parsed acc) acc))))))))

;;; ====
;;; Per-concept error rates
;;; ====

(define (reweight/concept-error-rates eval-results coverage-report)
  ;; Compute error rate per concept.
  ;; Returns: ((concept error-rate total correct) ...)
  (let* ([skill-concepts (cdr (assq 'skills coverage-report))]
         ;; Build module→skill map from coverage report
         ;; (reuse the IR-based map approach)
         [concept-stats (map (lambda (c) (list c 0 0))  ; (concept total correct)
                             (cdr (assq 'concepts coverage-report)))])

    ;; Accumulate per-concept stats
    (for-each
      (lambda (result)
        (let* ([mod-str (let ([e (assq 'source_module result)])
                          (and e (cdr e)))]
               [correct (let ([e (assq 'correct result)])
                          (and e (cdr e)))]
               ;; Find which skill this module belongs to
               [mod-sym (and (string? mod-str) (string->symbol mod-str))])
          (when mod-sym
            ;; Find skill for this module, then concepts for that skill
            (for-each
              (lambda (skill-entry)
                (let ([skill (car skill-entry)]
                      [concepts (cdr skill-entry)])
                  ;; If this skill has modules containing our mod...
                  ;; Simple: check if any concept's skill matches
                  (for-each
                    (lambda (concept)
                      (let ([entry (assq concept concept-stats)])
                        (when entry
                          ;; Increment total
                          (set-car! (cdr entry) (+ (cadr entry) 1))
                          ;; Increment correct if applicable
                          (when (eq? correct #t)
                            (set-car! (cddr entry) (+ (caddr entry) 1))))))
                    concepts)))
              skill-concepts))))
      eval-results)

    ;; Compute error rates
    (filter-map
      (lambda (entry)
        (let* ([concept (car entry)]
               [total (cadr entry)]
               [correct (caddr entry)])
          (if (< total *reweight-min-samples*)
              #f  ; not enough data
              (let ([error-rate (- 1.0 (/ (exact->inexact correct) total))])
                `((concept . ,concept)
                  (error-rate . ,error-rate)
                  (total . ,total)
                  (correct . ,correct))))))
      concept-stats)))

;;; ====
;;; Weight computation
;;; ====

(define (reweight/compute-weights error-rates)
  ;; Convert error rates to grammar weights.
  ;; Higher error rate → higher weight (generate more of what's hard).
  ;; w = 1 + alpha * error_rate, capped at max_weight
  (map (lambda (entry)
         (let* ([concept (cdr (assq 'concept entry))]
                [err (cdr (assq 'error-rate entry))]
                [weight (min *reweight-max-weight*
                             (+ 1.0 (* *reweight-alpha* err)))])
           `((concept . ,concept)
             (weight . ,weight)
             (error-rate . ,err)
             (total . ,(cdr (assq 'total entry)))
             (correct . ,(cdr (assq 'correct entry))))))
       error-rates))

(define (reweight/from-eval eval-path coverage-report)
  ;; Full pipeline: read eval → compute error rates → compute weights.
  (printf "Computing concept weights from ~a...\n" eval-path)
  (let* ([results (reweight/read-eval-results eval-path)]
         [_ (printf "  ~a eval results loaded\n" (length results))]
         [rates (reweight/concept-error-rates results coverage-report)]
         [_ (printf "  ~a concepts with enough data\n" (length rates))]
         [weights (reweight/compute-weights rates)]
         [_ (printf "  Weights computed\n")])
    weights))

;;; ====
;;; Reporting
;;; ====

(define (reweight/print-weights weights)
  (printf "\n=== Concept Weights ===\n")
  (printf "~20a ~8a ~8a ~6a ~8a\n"
          "Concept" "Weight" "ErrRate" "Total" "Correct")
  (printf "~20a ~8a ~8a ~6a ~8a\n"
          "-------" "------" "-------" "-----" "-------")
  (let ([sorted (list-sort (lambda (a b)
                             (> (cdr (assq 'weight a))
                                (cdr (assq 'weight b))))
                           weights)])
    (for-each
      (lambda (w)
        (printf "~20a ~8,2f ~8,2f ~6a ~8a\n"
                (cdr (assq 'concept w))
                (cdr (assq 'weight w))
                (cdr (assq 'error-rate w))
                (cdr (assq 'total w))
                (cdr (assq 'correct w))))
      sorted)))

;;; ====
;;; Export weights for grammar DSL
;;; ====

(define (reweight/export-grammar-weights weights output-path)
  ;; Write a Scheme file that can be loaded by the grammar DSL.
  ;; Exports *concept-weights* as an alist.
  (let ([p (open-output-file output-path 'replace)])
    (display ";;; Auto-generated concept weights for grammar DSL\n" p)
    (display ";;; Generated by user/sft/kg/reweight.ss\n\n" p)
    (display "(define *concept-weights*\n  '(" p)
    (let loop ([remaining weights] [first? #t])
      (when (pair? remaining)
        (let* ([w (car remaining)]
               [concept (cdr (assq 'concept w))]
               [weight (cdr (assq 'weight w))])
          (unless first? (display "\n    " p))
          (write (cons concept weight) p)
          (loop (cdr remaining) #f))))
    (display "))\n" p)
    (close-output-port p)
    (printf "Weights exported to ~a\n" output-path)))

;;; ====
;;; Load
;;; ====

(printf "reweight.ss loaded.\n")
(printf "  (reweight/from-eval \"eval.jsonl\" coverage-report) - Compute weights\n")
(printf "  (reweight/print-weights weights)                    - Print report\n")
(printf "  (reweight/export-grammar-weights weights \"out.ss\") - Export for grammar\n")
