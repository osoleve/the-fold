;;; Test that the prelude loads correctly

(load "fabric/stitches/fp/prelude.ss")

(display "Testing basic operations...
")

;; Test combinators
(assert (= 42 (identity 42)))
(assert (= 5 ((const 5) 'ignored)))

;; Test algebraic
(assert (equal? '(1 2 3 4) ((monoid-op list-monoid) '(1 2) '(3 4))))

;; Test bifunctor
(assert (equal? (cons 2 10) (pair-bimap add1 (lambda (x) (* x 2)) (cons 1 5))))

;; Test contravariant
(assert (run-predicate (make-predicate (lambda (x) (> x 0))) 5))

;; Test traversable
(assert (equal? 6 (list-fold sum-monoid '(1 2 3))))

;; Test reader
(assert (= 10 (run-reader (reader-asks (lambda (x) (* x 2))) 5)))

;; Test state
(assert (equal? (cons 42 100) (run-state (state-pure 42) 100)))

;; Test validation
(assert (validation-success? (validation-success 1)))

;; Test optics
(assert (equal? 1 (lens-view _1 (cons 1 2))))

(display "
✓ FP Prelude tests passed!
")
