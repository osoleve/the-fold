;;; core/fp/control/test-random-effect.ss --- Tests for Random Effect

;;; NOTE: Run from project root

(load "core/lang/module.ss")
(load "core/test-framework.ss")
(load "lattice/fp/control/random-effect.ss")

(display "\n")
(display "====\n")
(display "         RANDOM EFFECT TESTS\n")
(display "====\n")

;;; ====
;;; Random Effect Signature Tests
;;; ====

(test-group random-effect-signature
            (define-test sig-random-exists-test
              (assert-true (effect-sig? sig-Random))
              (assert-equal 'Random (effect-sig-name sig-Random)))
            
            (define-test sig-random-operations-test
              (assert-true (operation? (lookup-operation sig-Random 'uniform)))
              (assert-true (operation? (lookup-operation sig-Random 'int-range)))
              (assert-true (operation? (lookup-operation sig-Random 'choice)))
              (assert-true (operation? (lookup-operation sig-Random 'bool)))))

;;; ====
;;; Basic Random Operations Tests
;;; ====

(test-group random-basic-operations
            (define-test random-uniform-basic-test
              ;; Same seed should produce same result
              (let ([r1 (run-random 42 random-uniform-eff)]
                    [r2 (run-random 42 random-uniform-eff)])
                   (assert-equal r1 r2)
                   (assert-true (and (>= r1 0) (< r1 1)))))
            
            (define-test random-uniform-different-seeds-test
              ;; Different seeds should (almost always) produce different results
              (let ([r1 (run-random 1 random-uniform-eff)]
                    [r2 (run-random 2 random-uniform-eff)])
                   (assert-true (not (= r1 r2)))))
            
            (define-test random-int-range-test
              (let ([r (run-random 123 (random-int-eff 10 20))])
                   (assert-true (and (>= r 10) (<= r 20)))))
            
            (define-test random-int-deterministic-test
              (let ([r1 (run-random 99 (random-int-eff 0 100))]
                    [r2 (run-random 99 (random-int-eff 0 100))])
                   (assert-equal r1 r2)))
            
            (define-test random-choice-test
              (let ([r (run-random 42 (random-choice-eff '(a b c d e)))])
                   (assert-true (if (memq r '(a b c d e)) #t #f))))
            
            (define-test random-choice-deterministic-test
              (let ([r1 (run-random 77 (random-choice-eff '(x y z)))]
                    [r2 (run-random 77 (random-choice-eff '(x y z)))])
                   (assert-equal r1 r2)))
            
            (define-test random-bool-test
              (let ([r (run-random 42 random-bool-eff)])
                   (assert-true (boolean? r))))
            
            (define-test random-float-range-test
              (let ([r (run-random 42 (random-float-eff 10.0 20.0))])
                   (assert-true (and (>= r 10.0) (< r 20.0))))))

;;; ====
;;; Derived Operations Tests
;;; ====

(test-group random-derived-operations
            (define-test random-bernoulli-true-test
              ;; With p=1, should always be true
              (let ([r (run-random 42 (random-bernoulli-eff 1.0))])
                   (assert-true r)))
            
            (define-test random-bernoulli-false-test
              ;; With p=0, should always be false
              (let ([r (run-random 42 (random-bernoulli-eff 0.0))])
                   (assert-false r)))
            
            (define-test random-list-eff-test
              (let ([r (run-random 42 (random-list-eff 5 random-uniform-eff))])
                   (assert-equal 5 (length r))
                   (assert-true (andmap (lambda (x) (and (>= x 0) (< x 1))) r))))
            
            (define-test random-shuffle-eff-test
              (let ([r (run-random 42 (random-shuffle-eff '(1 2 3 4 5)))])
                   ;; Same elements, possibly different order
                   (assert-equal 5 (length r))
                   (assert-true (andmap (lambda (x) (if (memv x r) #t #f)) '(1 2 3 4 5)))))
            
            (define-test random-shuffle-deterministic-test
              (let ([r1 (run-random 99 (random-shuffle-eff '(a b c d)))]
                    [r2 (run-random 99 (random-shuffle-eff '(a b c d)))])
                   (assert-equal r1 r2))))

;;; Helper for andmap
(define (andmap pred lst)
  (cond
   [(null? lst) #t]
   [(pred (car lst)) (andmap pred (cdr lst))]
   [else #f]))

;;; ====
;;; Composition Tests
;;; ====

(test-group random-composition
            (define-test random-sequence-test
              ;; Multiple random operations in sequence
              (let ([result (run-random 42
                                        (eff-bind random-uniform-eff
                                                  (lambda (u1)
                                                          (eff-bind random-uniform-eff
                                                                    (lambda (u2)
                                                                            (eff-return (list u1 u2)))))))])
                   (assert-equal 2 (length result))
                   ;; u1 and u2 should be different
                   (assert-true (not (= (car result) (cadr result))))))
            
            (define-test random-with-state-test
              ;; Test that we can get both result and generator state
              (let ([result (run-random-with-state-eff 42 random-uniform-eff)])
                   (assert-true (pair? result))
                   (assert-true (and (>= (car result) 0) (< (car result) 1)))
                   ;; Generator state should be a PCG
                   (assert-true (pcg? (cdr result)))))
            
            (define-test handle-random-alias-test
              ;; handle-random should be same as run-random
              (let ([r1 (run-random 42 random-uniform-eff)]
                    [r2 (handle-random 42 random-uniform-eff)])
                   (assert-equal r1 r2)))
            
            (define-test with-random-eff-scope-test
              ;; Random effect can be scoped within other effects
              ;; Use run-state-eff for State effect (run-state is for State monad)
              (let ([result (run-state-eff 0
                                           (with-random-eff 42 random-uniform-eff))])
                   (assert-true (and (>= (car result) 0) (< (car result) 1)))
                   ;; State should be unchanged
                   (assert-equal 0 (cdr result)))))

;;; ====
;;; Distribution Integration Tests
;;; ====

(test-group random-with-distributions
            ;; Test integration with distributions via random-sample-dist
            (define-test random-sample-dist-test
              ;; Sample from random-float (State computation) via effect
              (let ([r (run-random 42
                                   (random-sample-dist (random-float-range 100.0 200.0)))])
                   (assert-true (and (>= r 100.0) (< r 200.0)))))
            
            (define-test random-sample-multiple-test
              ;; Multiple distribution samples (dice)
              (let ([result (run-random 42
                                        (eff-bind (random-sample-dist (random-int-range 1 6))
                                                  (lambda (d1)
                                                          (eff-bind (random-sample-dist (random-int-range 1 6))
                                                                    (lambda (d2)
                                                                            (eff-return (+ d1 d2)))))))])
                   ;; Sum of two dice: 2-12
                   (assert-true (and (>= result 2) (<= result 12))))))

;;; ====
;;; Practical Examples
;;; ====

(test-group random-practical
            ;; Helpers (defines must precede define-test in R6RS)
            (define (make-list n val)
              (if (<= n 0) '()
                  (cons val (make-list (- n 1) val))))

            (define (estimate-pi n)
              (eff-bind (random-list-eff n
                                         (eff-bind random-uniform-eff
                                                   (lambda (x)
                                                           (eff-bind random-uniform-eff
                                                                     (lambda (y)
                                                                             (eff-return
                                                                              (if (<= (+ (* x x) (* y y)) 1.0)
                                                                                  1
                                                                                  0)))))))
                        (lambda (hits)
                                (eff-return (* 4.0 (/ (fold-left + 0 hits) n))))))

            (define (random-walk steps)
              (eff-fold (lambda (pos _)
                                (eff-bind (random-choice-eff '(-1 1))
                                          (lambda (step)
                                                  (eff-return (+ pos step)))))
                        0
                        (make-list steps '())))

            (define (coin-flip-game n-flips)
              (eff-fold (lambda (score _)
                                (eff-bind random-bool-eff
                                          (lambda (heads?)
                                                  (eff-return (if heads?
                                                                  (+ score 1)
                                                                  (- score 1))))))
                        0
                        (make-list n-flips '())))

            (define-test monte-carlo-pi-test
              (let ([pi-est (run-random 42 (estimate-pi 1000))])
                   (assert-true (and (> pi-est 2.5) (< pi-est 3.8)))))

            (define-test random-walk-deterministic-test
              (let ([r1 (run-random 42 (random-walk 100))]
                    [r2 (run-random 42 (random-walk 100))])
                   (assert-equal r1 r2)))

            (define-test coin-flip-game-test
              (let ([score (run-random 42 (coin-flip-game 100))])
                   (assert-true (and (>= score -100) (<= score 100))))))

;;; ====
;;; Performance Tests
;;; ====

(test-group random-performance
            ;; Test that large list shuffle completes in reasonable time
            ;; With O(N) implementation, shuffling 10000 elements should be fast
            ;; With O(N^2) implementation, this would take many seconds
            (define-test large-shuffle-performance-test
              (let* ([large-list (iota 10000)]  ; list of 0..9999
                     [shuffled (run-random 42 (random-shuffle-eff large-list))])
                    ;; Verify all elements are present
                    (assert-equal 10000 (length shuffled))
                    ;; Verify it's a permutation (sum should be same)
                    (assert-equal (fold-left + 0 large-list)
                                  (fold-left + 0 shuffled))
                    ;; Verify shuffle actually changed order (statistically certain)
                    (assert-false (equal? large-list shuffled)))))

;; iota is provided by prelude (via test-framework.ss)

;;; ====
;;; Summary
;;; ====

(display "\n")
(display "====\n")
(printf "Tests passed: ~a\n" *tests-passed*)
(printf "Tests failed: ~a\n" *tests-failed*)
(printf "Total tests:  ~a\n" *tests-run*)

(if (= *tests-failed* 0)
    (display "\n[SUCCESS] All Random effect tests passed.\n")
    (display "\n[FAILURE] Some Random effect tests failed.\n"))
