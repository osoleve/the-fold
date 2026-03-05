(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
;;; @module pipeline/rlm2-metrics
;;; @requires pipeline/rlm2 sort
(require 'pipeline/rlm2)
(require 'sort)

(doc 'module 'pipeline/rlm2-metrics)
(doc 'description "Post-episode trajectory analysis for RLM v2. Pure functions that
compute capability gradient metrics from step records — near-miss detection,
action diversity, progress rate, and efficiency. All functions operate on
lists of step records (not CAS hashes), so they're usable in both online
and offline analysis.")
(doc 'layer 'lattice)
(doc 'purity 'total)

;;; ====
;;; Step Record Summarization
;;; ====

(doc 'section 'rlm2-step-summary)

(doc 'type '(-> (List Rlm2StepRecord) Alist))
(doc 'description "Count actions by type across all steps")
(define (rlm2-action-histogram steps)
  (doc 'export #t)
  (let loop ([steps steps] [counts '()])
    (if (null? steps)
        (sort-by (lambda (a b) (> (cdr a) (cdr b))) counts)
        (let* ([step (car steps)]
               [action (rlm2-step-record-action step)]
               [type (if (pair? action) (car action) 'unknown)]
               [entry (assq type counts)])
          (loop (cdr steps)
                (if entry
                    (map (lambda (c)
                           (if (eq? (car c) type)
                               (cons type (+ 1 (cdr c)))
                               c))
                         counts)
                    (cons (cons type 1) counts)))))))

(doc 'type '(-> (List Rlm2StepRecord) Nat))
(doc 'description "Count distinct action types used across all steps")
(define (rlm2-action-diversity steps)
  (doc 'export #t)
  (let loop ([steps steps] [seen '()])
    (if (null? steps)
        (length seen)
        (let* ([step (car steps)]
               [action (rlm2-step-record-action step)]
               [type (if (pair? action) (car action) 'unknown)])
          (loop (cdr steps)
                (if (memq type seen) seen (cons type seen)))))))

(doc 'type '(-> (List Rlm2StepRecord) (List Rlm2StepRecord)))
(doc 'description "Extract all submit attempts from step records")
(define (rlm2-submit-attempts steps)
  (doc 'export #t)
  (filter (lambda (step)
            (let ([action (rlm2-step-record-action step)])
              (and (pair? action) (eq? (car action) 'submit))))
          steps))

(doc 'type '(-> (List Rlm2StepRecord) Nat))
(doc 'description "Count failed submit attempts (verifier rejected or eval error)")
(define (rlm2-failed-submissions steps)
  (doc 'export #t)
  (length (filter (lambda (step)
                    (let ([obs (rlm2-step-record-observation step)])
                      (and (rlm2-observation? obs)
                           (not (rlm2-observation-ok? obs)))))
                  (rlm2-submit-attempts steps))))

(doc 'type '(-> (List Rlm2StepRecord) Nat))
(doc 'description "Count successful submit attempts")
(define (rlm2-successful-submissions steps)
  (doc 'export #t)
  (length (filter (lambda (step)
                    (let ([obs (rlm2-step-record-observation step)])
                      (and (rlm2-observation? obs)
                           (rlm2-observation-ok? obs))))
                  (rlm2-submit-attempts steps))))

;;; ====
;;; Progress Rate
;;; ====

(doc 'section 'rlm2-progress)

(doc 'type '(-> (List Rlm2StepRecord) (List Boolean)))
(doc 'description "Map each step to whether its observation succeeded (ok=true)")
(define (rlm2-success-trace steps)
  (doc 'export #t)
  (map (lambda (step)
         (let ([obs (rlm2-step-record-observation step)])
           (and (rlm2-observation? obs)
                (rlm2-observation-ok? obs)
                #t)))
       steps))

(doc 'type '(-> (List Boolean) Nat Flonum))
(doc 'description "Compute success rate over a sliding window of the last N items.
Returns ratio of #t values in the window [0.0, 1.0].")
(define (rlm2-windowed-success-rate trace window)
  (doc 'export #t)
  (let* ([len (length trace)]
         [start (max 0 (- len window))]
         [window-items (list-tail trace start)]
         [successes (length (filter (lambda (x) (eq? x #t)) window-items))]
         [count (length window-items)])
    (if (= count 0) 0.0
        (/ successes count))))

(doc 'type '(-> (List Rlm2StepRecord) Flonum))
(doc 'description "Overall success rate: fraction of steps with ok=true observations")
(define (rlm2-overall-success-rate steps)
  (doc 'export #t)
  (if (null? steps) 0.0
      (let ([trace (rlm2-success-trace steps)])
        (rlm2-windowed-success-rate trace (length trace)))))

;;; ====
;;; Fuel Efficiency
;;; ====

(doc 'section 'rlm2-fuel-efficiency)

(doc 'type '(-> (List Rlm2StepRecord) Nat Flonum))
(doc 'description "Fuel utilization: fraction of initial budget consumed.
Requires the initial fuel budget as second argument.")
(define (rlm2-fuel-utilization steps initial-fuel)
  (doc 'export #t)
  (if (or (= initial-fuel 0) (null? steps))
      0.0
      (/ (length steps) initial-fuel)))

(doc 'type '(-> (List Rlm2StepRecord) Flonum))
(doc 'description "Productive action ratio: non-think, non-idle steps / total steps.
Higher = more decisive agent.")
(define (rlm2-productive-ratio steps)
  (doc 'export #t)
  (if (null? steps) 0.0
      (let ([productive (filter (lambda (step)
                                  (let ([action (rlm2-step-record-action step)])
                                    (and (pair? action)
                                         (not (memq (car action) '(think idle))))))
                                steps)])
        (/ (length productive) (length steps)))))

;;; ====
;;; Near-Miss Detection
;;; ====

(doc 'section 'rlm2-near-miss)

(doc 'type '(-> Symbol (List Rlm2StepRecord) Nat Flonum Rlm2NearMissReport))
(doc 'description "Analyze a completed episode for near-miss signals.
status: the episode termination status ('completed, 'exhausted, 'error).
steps: the full step record list.
initial-fuel: the starting fuel budget.
Returns an alist of near-miss indicators.")
(define (rlm2-near-miss-report status steps initial-fuel)
  (doc 'export #t)
  (let* ([total (length steps)]
         [submits (rlm2-submit-attempts steps)]
         [failed-subs (rlm2-failed-submissions steps)]
         [success-subs (rlm2-successful-submissions steps)]
         [diversity (rlm2-action-diversity steps)]
         [productive (rlm2-productive-ratio steps)]
         [overall-rate (rlm2-overall-success-rate steps)]
         [late-rate (if (> total 3)
                        (rlm2-windowed-success-rate
                          (rlm2-success-trace steps)
                          (min 5 (max 1 (quotient total 3))))
                        overall-rate)]
         ;; Near-miss signals
         [attempted-submit? (> (length submits) 0)]
         [had-failed-submit? (> failed-subs 0)]
         [exhausted-with-progress? (and (eq? status 'exhausted)
                                        (> overall-rate 0.3))]
         [late-improving? (> late-rate (+ overall-rate 0.1))]
         ;; Composite near-miss score [0.0, 1.0]
         ;; Higher = closer to success
         [score (min 1.0
                  (+ (if had-failed-submit? 0.3 0.0)
                     (if exhausted-with-progress? 0.2 0.0)
                     (if late-improving? 0.15 0.0)
                     (* productive 0.2)
                     (* (min 1.0 (/ diversity 8)) 0.15)))])
    (list 'rlm2-near-miss-report
          (list 'status status)
          (list 'total-steps total)
          (list 'submit-attempts (length submits))
          (list 'failed-submissions failed-subs)
          (list 'successful-submissions success-subs)
          (list 'action-diversity diversity)
          (list 'productive-ratio (exact->inexact productive))
          (list 'overall-success-rate (exact->inexact overall-rate))
          (list 'late-success-rate (exact->inexact late-rate))
          (list 'near-miss-score (exact->inexact score))
          (list 'signals
                (filter pair?
                  (list (if had-failed-submit?
                            (list 'failed-submit failed-subs)
                            '())
                        (if exhausted-with-progress?
                            (list 'exhausted-with-progress overall-rate)
                            '())
                        (if late-improving?
                            (list 'late-improving late-rate)
                            '())))))))

(doc 'type '(-> Any Boolean))
(doc 'description "True if x is a near-miss report")
(define (rlm2-near-miss-report? x)
  (doc 'export #t)
  (and (pair? x) (eq? (car x) 'rlm2-near-miss-report)))

(doc 'type '(-> Rlm2NearMissReport Any))
(doc 'description "Extract a field from a near-miss report by key")
(define (rlm2-near-miss-field report key)
  (doc 'export #t)
  (let loop ([fields (cdr report)])
    (cond
      [(null? fields) #f]
      [(and (pair? (car fields))
            (eq? (caar fields) key))
       (cadar fields)]
      [else (loop (cdr fields))])))

(doc 'type '(-> Rlm2NearMissReport Flonum))
(doc 'description "Extract the near-miss score [0.0, 1.0] from a report")
(define (rlm2-near-miss-score report)
  (doc 'export #t)
  (rlm2-near-miss-field report 'near-miss-score))

(doc 'type '(-> Rlm2NearMissReport Boolean))
(doc 'description "True if the report indicates a near-miss (score >= 0.4)")
(define (rlm2-near-miss? report)
  (doc 'export #t)
  (>= (rlm2-near-miss-score report) 0.4))

;;; ====
;;; Action Pattern Analysis
;;; ====

(doc 'section 'rlm2-patterns)

(doc 'type '(-> (List Rlm2StepRecord) (List (List Symbol))))
(doc 'description "Extract action type bigrams from step sequence.
Useful for detecting common transition patterns (e.g., search→eval, eval→store).")
(define (rlm2-action-bigrams steps)
  (doc 'export #t)
  (if (or (null? steps) (null? (cdr steps)))
      '()
      (let loop ([steps steps] [bigrams '()])
        (if (null? (cdr steps))
            (reverse bigrams)
            (let* ([a1 (rlm2-step-record-action (car steps))]
                   [a2 (rlm2-step-record-action (cadr steps))]
                   [t1 (if (pair? a1) (car a1) 'unknown)]
                   [t2 (if (pair? a2) (car a2) 'unknown)])
              (loop (cdr steps)
                    (cons (list t1 t2) bigrams)))))))

(doc 'type '(-> (List Rlm2StepRecord) (List (Symbol . Nat))))
(doc 'description "Count how many consecutive same-type action runs occurred.
Returns alist of (type . max-run-length). High values indicate repetitive behavior.")
(define (rlm2-max-runs steps)
  (doc 'export #t)
  (if (null? steps) '()
      (let loop ([steps (cdr steps)]
                 [current-type (let ([a (rlm2-step-record-action (car steps))])
                                 (if (pair? a) (car a) 'unknown))]
                 [current-run 1]
                 [maxes '()])
        (if (null? steps)
            ;; Final: update maxes with last run
            (let ([entry (assq current-type maxes)])
              (if (and entry (>= (cdr entry) current-run))
                  maxes
                  (cons (cons current-type current-run)
                        (filter (lambda (e) (not (eq? (car e) current-type))) maxes))))
            (let* ([action (rlm2-step-record-action (car steps))]
                   [type (if (pair? action) (car action) 'unknown)])
              (if (eq? type current-type)
                  (loop (cdr steps) current-type (+ current-run 1) maxes)
                  ;; Type changed: record max for previous type
                  (let* ([entry (assq current-type maxes)]
                         [new-maxes (if (and entry (>= (cdr entry) current-run))
                                        maxes
                                        (cons (cons current-type current-run)
                                              (filter (lambda (e) (not (eq? (car e) current-type)))
                                                      maxes)))])
                    (loop (cdr steps) type 1 new-maxes))))))))

;;; ====
;;; Aggregate Metrics
;;; ====

(doc 'section 'rlm2-aggregate-metrics)

(doc 'type '(-> Symbol (List Rlm2StepRecord) Nat Rlm2Metrics))
(doc 'description "Compute all metrics for an episode. Returns a comprehensive alist.")
(define (rlm2-episode-metrics status steps initial-fuel)
  (doc 'export #t)
  (list 'rlm2-episode-metrics
        (list 'histogram (rlm2-action-histogram steps))
        (list 'diversity (rlm2-action-diversity steps))
        (list 'productive-ratio (exact->inexact (rlm2-productive-ratio steps)))
        (list 'overall-success-rate (exact->inexact (rlm2-overall-success-rate steps)))
        (list 'submit-attempts (length (rlm2-submit-attempts steps)))
        (list 'failed-submissions (rlm2-failed-submissions steps))
        (list 'max-runs (rlm2-max-runs steps))
        (list 'near-miss (rlm2-near-miss-report status steps initial-fuel))))

(doc 'type '(-> Any Boolean))
(define (rlm2-episode-metrics? x)
  (doc 'export #t)
  (and (pair? x) (eq? (car x) 'rlm2-episode-metrics)))

(doc 'type '(-> Rlm2Metrics Any))
(define (rlm2-metrics-field metrics key)
  (doc 'export #t)
  (let loop ([fields (cdr metrics)])
    (cond
      [(null? fields) #f]
      [(and (pair? (car fields))
            (eq? (caar fields) key))
       (cadar fields)]
      [else (loop (cdr fields))])))
