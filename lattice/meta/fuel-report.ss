(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
;;; @module meta/fuel-report
;;; @requires prelude
(require 'prelude)

(doc 'module 'meta/fuel-report)
(doc 'description "Fuel analysis and graceful degradation reporting. Converts the
binary fuel exhaustion (suspended/completed) into a gradient with progress metrics,
severity classification, and recovery recommendations. Pure analysis functions
operating on evaluation results.")
(doc 'layer 'lattice)
(doc 'purity 'total)

;;; ====
;;; Progress Computation
;;; ====

(doc 'section 'fuel-progress)

(doc 'type '(-> Nat Nat Number))
(doc 'description "Compute evaluation progress as a ratio [0.0, 1.0].
Given initial fuel and remaining fuel, returns what fraction of fuel was consumed.
1.0 means all fuel consumed (suspended at empty), 0.0 means no fuel consumed.")
(define (fuel-progress initial-fuel remaining-fuel)
  (if (zero? initial-fuel)
      0.0
      (exact->inexact (/ (- initial-fuel remaining-fuel) initial-fuel))))

(doc 'type '(-> Nat Nat Number Boolean))
(doc 'description "True if evaluation consumed more than (1 - threshold) of fuel.
Default threshold is 0.1, meaning near-completion if > 90% consumed.")
(define (fuel-near-completion? initial-fuel remaining-fuel . threshold-arg)
  (let ([threshold (if (null? threshold-arg) 0.1 (car threshold-arg))])
    (> (fuel-progress initial-fuel remaining-fuel) (- 1.0 threshold))))

;;; ====
;;; Severity Classification
;;; ====

(doc 'section 'fuel-severity)

(doc 'type '(-> Symbol Nat Nat Symbol))
(doc 'description "Classify the severity of an evaluation result.
Returns one of:
  'completed    — evaluation finished successfully
  'near-miss    — suspended but consumed >90% of fuel (almost done)
  'partial      — suspended with 30-90% progress
  'minimal      — suspended with <30% progress (likely needs much more fuel)
  'error        — evaluation produced an error")
(define (fuel-severity result-tag initial-fuel remaining-fuel)
  (cond
    [(eq? result-tag 'ok) 'completed]
    [(eq? result-tag 'error) 'error]
    [(eq? result-tag 'suspended)
     (let ([progress (fuel-progress initial-fuel remaining-fuel)])
       (cond
         [(> progress 0.9) 'near-miss]
         [(> progress 0.3) 'partial]
         [else 'minimal]))]
    [else 'unknown]))

;;; ====
;;; Fuel Reports
;;; ====

(doc 'section 'fuel-reports)

(doc 'type '(-> Alist Nat Alist))
(doc 'description "Create a structured fuel report from an evaluation result.
Takes the raw eval result and the initial fuel budget.
Returns a tagged alist with progress, severity, and recommendation.")
(define (make-fuel-report result initial-fuel)
  (let* ([tag (if (pair? result) (car result) 'unknown)]
         [remaining (cond
                      [(eq? tag 'ok) (if (and (pair? (cdr result))
                                              (pair? (cddr result)))
                                         (caddr result)  ; (ok value fuel)
                                         0)]
                      [(eq? tag 'suspended) (if (and (pair? (cdr result))
                                                     (pair? (cddr result))
                                                     (pair? (cdddr result)))
                                                (cadddr result)  ; (suspended expr env fuel)
                                                0)]
                      [else 0])]
         [progress (fuel-progress initial-fuel remaining)]
         [severity (fuel-severity tag initial-fuel remaining)]
         [recommendation (fuel-recommendation severity initial-fuel remaining)])
    (list 'fuel-report
          (list 'result-tag tag)
          (list 'initial-fuel initial-fuel)
          (list 'remaining-fuel remaining)
          (list 'consumed (- initial-fuel remaining))
          (list 'progress progress)
          (list 'severity severity)
          (list 'recommendation recommendation))))

(doc 'type '(-> Any Boolean))
(define (fuel-report? x)
  (and (pair? x) (eq? (car x) 'fuel-report)))

(doc 'type '(-> Alist Symbol Any))
(doc 'description "Access a field from a fuel report.")
(define (fuel-report-field report key)
  (let loop ([fields (cdr report)])
    (cond
      [(null? fields) #f]
      [(and (pair? (car fields)) (eq? (caar fields) key))
       (cadar fields)]
      [else (loop (cdr fields))])))

;;; ====
;;; Recommendations
;;; ====

(doc 'section 'fuel-recommendations)

(doc 'type '(-> Symbol Nat Nat Alist))
(doc 'description "Generate a recovery recommendation based on severity.
Returns a tagged alist with strategy, suggested fuel, and rationale.")
(define (fuel-recommendation severity initial-fuel remaining-fuel)
  (case severity
    [(completed)
     (list 'recommendation
           (list 'strategy 'none)
           (list 'rationale "Evaluation completed successfully."))]
    [(near-miss)
     ;; Almost done — small fuel bump should finish it
     (let ([consumed (- initial-fuel remaining-fuel)]
           [suggested (* 2 (max 1 (- initial-fuel remaining-fuel)))])
       (list 'recommendation
             (list 'strategy 'retry-with-bump)
             (list 'suggested-fuel suggested)
             (list 'rationale
                   (format "Consumed ~a of ~a fuel (~a%). A ~ax increase should complete."
                           consumed initial-fuel
                           (round (* 100 (fuel-progress initial-fuel remaining-fuel)))
                           2))))]
    [(partial)
     ;; Moderate progress — need significantly more fuel
     (let ([progress (fuel-progress initial-fuel remaining-fuel)]
           [estimated-total (if (> (fuel-progress initial-fuel remaining-fuel) 0.01)
                                (inexact->exact
                                 (ceiling (/ initial-fuel
                                             (fuel-progress initial-fuel remaining-fuel))))
                                (* initial-fuel 10))])
       (list 'recommendation
             (list 'strategy 'retry-with-estimate)
             (list 'suggested-fuel estimated-total)
             (list 'rationale
                   (format "~a% progress with ~a fuel. Estimated ~a total needed."
                           (round (* 100 progress))
                           initial-fuel
                           estimated-total))))]
    [(minimal)
     ;; Very little progress — expression might be inherently expensive
     (list 'recommendation
             (list 'strategy 'profile-first)
             (list 'suggested-fuel (* initial-fuel 10))
             (list 'rationale
                   (format "Only ~a% progress. Consider profiling before retrying."
                           (round (* 100 (fuel-progress initial-fuel remaining-fuel))))))]
    [(error)
     (list 'recommendation
           (list 'strategy 'fix-error)
           (list 'rationale "Evaluation produced an error; more fuel won't help."))]
    [else
     (list 'recommendation
           (list 'strategy 'unknown)
           (list 'rationale "Unknown result type."))]))

;;; ====
;;; Batch Analysis
;;; ====

(doc 'section 'batch-analysis)

(doc 'type '(-> (List Alist) Alist))
(doc 'description "Summarize a batch of fuel reports into aggregate statistics.")
(define (fuel-report-summary reports)
  (let* ([total (length reports)]
         [completed (length (filter (lambda (r)
                                      (eq? 'completed (fuel-report-field r 'severity)))
                                    reports))]
         [near-misses (length (filter (lambda (r)
                                        (eq? 'near-miss (fuel-report-field r 'severity)))
                                      reports))]
         [partials (length (filter (lambda (r)
                                     (eq? 'partial (fuel-report-field r 'severity)))
                                   reports))]
         [minimals (length (filter (lambda (r)
                                     (eq? 'minimal (fuel-report-field r 'severity)))
                                   reports))]
         [errors (length (filter (lambda (r)
                                   (eq? 'error (fuel-report-field r 'severity)))
                                 reports))]
         [progresses (map (lambda (r) (fuel-report-field r 'progress)) reports)]
         [avg-progress (if (null? progresses)
                           0.0
                           (/ (apply + progresses) (length progresses)))])
    (list 'fuel-summary
          (list 'total total)
          (list 'completed completed)
          (list 'near-misses near-misses)
          (list 'partials partials)
          (list 'minimals minimals)
          (list 'errors errors)
          (list 'avg-progress (exact->inexact avg-progress))
          (list 'completion-rate
                (if (zero? total) 0.0
                    (exact->inexact (/ completed total)))))))
