;;; lattice/linalg/test-integer-matrix-properties.ss — QuickCheck properties for integer normal forms

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'matrix)
(require 'integer-matrix)

;;; ============================================================================
;;; Helpers
;;; ============================================================================

(define gen-entry
  (gen-int-range -8 8))

(define gen-2x2
  (gen-map matrix-from-lists
           (gen-list-of 2 (gen-list-of 2 gen-entry))))

(define (diagonal-matrix? m)
  (let ([rows (matrix-rows m)]
        [cols (matrix-cols m)])
    (let loop-i ([i 0])
      (or (= i rows)
          (let loop-j ([j 0])
            (cond
              [(= j cols) (loop-i (+ i 1))]
              [(= i j) (loop-j (+ j 1))]
              [(= (matrix-ref m i j) 0) (loop-j (+ j 1))]
              [else #f]))))))

;;; ============================================================================
;;; Properties
;;; ============================================================================

(test-group integer-matrix-properties

  (define-property "smith-normal-form reconstructs U*A*V = D"
    gen-2x2
    (lambda (a)
      (let* ([result (smith-normal-form a)]
             [u (car result)]
             [d (cadr result)]
             [v (caddr result)]
             [uav (matrix-mul (matrix-mul u a) v)])
        (matrix-equal? uav d)))
    'tests 140)

  (define-property "smith-normal-form returns a diagonal D"
    gen-2x2
    (lambda (a)
      (let* ([result (smith-normal-form a)]
             [d (cadr result)])
        (diagonal-matrix? d)))
    'tests 140)

  (define-property "hermite-normal-form reconstructs U*A = H"
    gen-2x2
    (lambda (a)
      (let* ([result (hermite-normal-form a)]
             [u (car result)]
             [h (cadr result)]
             [ua (matrix-mul u a)])
        (matrix-equal? ua h)))
    'tests 140)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
