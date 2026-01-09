;;; core/info-theory/test-epiplexity.ss — Tests for Epiplexity Measures
;;;
;;; Tests for prequential and requential epiplexity estimators.
;;; Uses the unified test framework.

(load "core/test-framework.ss")
(load "lattice/info/epiplexity.ss")

;;; ============================================================
;;; Approximate Equality Helper
;;; ============================================================

;;; approx-equal? : Number × Number × Number → Boolean
;;; Check if two numbers are approximately equal within epsilon.
(define (approx-equal? expected actual epsilon)
  (< (abs (- expected actual)) epsilon))

;;; assert-approx : Number × Number × Number → Unit
;;; Assert approximate equality, fail with message if not.
(define (assert-approx expected actual epsilon)
  (unless (approx-equal? expected actual epsilon)
          (set! *tests-failed* (+ *tests-failed* 1))
          (display "    ✗ ")
          (display *current-test-name*)
          (newline)
          (display "      Expected: ")
          (write expected)
          (display " ± ")
          (write epsilon)
          (newline)
          (display "      Got:      ")
          (write actual)
          (newline)))

;;; ============================================================
;;; Prequential Epiplexity Tests
;;; ============================================================

(test-group epiplexity
            
            (define-test prequential-identical-losses
              ;; Identical losses (No learning / Pure noise) -> 0 epiplexity
              (let ([losses '(1.0 1.0 1.0)]
                    [final '(1.0 1.0 1.0)])
                   (assert-approx 0 (prequential-epiplexity losses final) 1e-6)))
            
            (define-test prequential-learning-case
              ;; Learning happens: high loss initially, drops to low
              ;; Online: (10, 5, 1), Final: (1, 1, 1)
              ;; Sum((10-1) + (5-1) + (1-1)) = 9 + 4 + 0 = 13
              (let ([online '(10.0 5.0 1.0)]
                    [final  '(1.0 1.0 1.0)])
                   (assert-approx 13.0 (prequential-epiplexity online final) 1e-6)))
            
            (define-test prequential-empty-lists
              ;; Empty lists should return 0
              (assert-approx 0 (prequential-epiplexity '() '()) 1e-6))
            
            (define-test prequential-length-mismatch
              ;; Mismatched lengths should raise an error
              (assert-error (lambda () (prequential-epiplexity '(1.0 2.0) '(1.0)))))
            
            ;;; ============================================================
            ;;; Prequential Epiplexity Scalar Tests
            ;;; ============================================================
            
            (define-test prequential-scalar-basic
              ;; Online: (10, 5, 1), Baseline: 1
              ;; Sum((10-1) + (5-1) + (1-1)) = 13
              (let ([online '(10.0 5.0 1.0)]
                    [baseline 1.0])
                   (assert-approx 13.0 (prequential-epiplexity-scalar online baseline) 1e-6)))
            
            (define-test prequential-scalar-empty
              ;; Empty online losses should return 0
              (assert-approx 0 (prequential-epiplexity-scalar '() 1.0) 1e-6))
            
            ;;; ============================================================
            ;;; Requential Epiplexity Tests
            ;;; ============================================================
            
            (define-test requential-identical-distributions
              ;; Identical distributions (Teacher = Student) -> 0 KL -> 0 Epiplexity
              (let ([teacher '((0.5 0.5) (0.1 0.9))]
                    [student '((0.5 0.5) (0.1 0.9))])
                   (assert-approx 0 (requential-epiplexity teacher student) 1e-6)))
            
            (define-test requential-divergent-distributions
              ;; Divergence case: sum of KL divergences
              (let* ([p1 '(0.5 0.5)]
                     [q1 '(0.25 0.75)]
                     [kl1 (kl-divergence p1 q1)]
                     [teacher (list p1 p1)]
                     [student (list q1 q1)])
                    (assert-approx (* 2 kl1) (requential-epiplexity teacher student) 1e-6)))
            
            (define-test requential-empty-lists
              ;; Empty distribution lists should return 0
              (assert-approx 0 (requential-epiplexity '() '()) 1e-6))
            
            (define-test requential-length-mismatch
              ;; Mismatched lengths should raise an error
              (assert-error (lambda () (requential-epiplexity '((0.5 0.5)) '()))))
            
            )

;;; Print final summary
(print-summary)
