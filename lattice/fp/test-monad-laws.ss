;;; lattice/fp/test-monad-laws.ss — QuickCheck property tests for Functor/Monad laws

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'qc-laws)
(require 'combinators)

;;; ============================================================================
;;; Maybe generators and operations
;;; ============================================================================

(define gen-maybe-int
  (gen-frequency
    (list (cons 1 (gen-pure nothing))
          (cons 3 (gen-map just gen-int)))))

(define (maybe=? a b)
  (cond
    [(and (nothing? a) (nothing? b)) #t]
    [(and (just? a) (just? b)) (= (from-just a) (from-just b))]
    [else #f]))

;;; ============================================================================
;;; Either generators and operations
;;; ============================================================================

;; left, right, left?, right?, from-left, from-right, either-fmap, either-bind
;; are all provided by (require 'combinators)

(define gen-either-int
  (gen-frequency
    (list (cons 1 (gen-map left gen-int))
          (cons 3 (gen-map right gen-int)))))

(define (either=? a b)
  (cond
    [(and (left? a) (left? b)) (= (from-left a) (from-left b))]
    [(and (right? a) (right? b)) (= (from-right a) (from-right b))]
    [else #f]))

;;; ============================================================================
;;; List monad operations
;;; ============================================================================

(define gen-int-list
  (gen-list gen-int))

(define (list-bind m f)
  (apply append (map f m)))

(define (list-pure x) (list x))

;;; ============================================================================
;;; Maybe Functor Laws
;;; ============================================================================

(test-group maybe-functor-laws

  (define-test "Maybe functor laws"
    (assert-laws "Maybe functor"
      (check-functor-laws gen-maybe-int maybe-fmap maybe=?)))
)

;;; ============================================================================
;;; Maybe Monad Laws
;;; ============================================================================

(test-group maybe-monad-laws

  (define-test "Maybe monad laws"
    (assert-laws "Maybe monad"
      (check-monad-laws gen-maybe-int maybe-bind just gen-int maybe=?)))
)

;;; ============================================================================
;;; Either Functor Laws
;;; ============================================================================

(test-group either-functor-laws

  (define-test "Either functor laws"
    (assert-laws "Either functor"
      (check-functor-laws gen-either-int either-fmap either=?)))
)

;;; ============================================================================
;;; Either Monad Laws
;;; ============================================================================

(test-group either-monad-laws

  (define-test "Either monad laws"
    (assert-laws "Either monad"
      (check-monad-laws gen-either-int either-bind right gen-int either=?)))
)

;;; ============================================================================
;;; List Functor Laws
;;; ============================================================================

(test-group list-functor-laws

  (define-test "List functor laws"
    (assert-laws "List functor"
      (check-functor-laws gen-int-list map equal?)))
)

;;; ============================================================================
;;; List Monad Laws
;;; ============================================================================

(test-group list-monad-laws

  (define-test "List monad laws"
    (assert-laws "List monad"
      (check-monad-laws gen-int-list list-bind list-pure gen-int equal?)))
)

;;; ============================================================================
;;; Maybe-specific monad properties
;;; ============================================================================

(test-group maybe-specific

  (define-property "nothing >>= f = nothing"
    gen-int
    (lambda (_)
      (nothing? (maybe-bind nothing (lambda (x) (just (+ x 1))))))
    'tests 100)

  (define-property "just x >>= return = just x"
    gen-int
    (lambda (x)
      (maybe=? (maybe-bind (just x) just) (just x)))
    'tests 200)

  (define-property "maybe-fmap id = id"
    gen-maybe-int
    (lambda (m)
      (maybe=? (maybe-fmap (lambda (x) x) m) m))
    'tests 200)

  (define-property "maybe-fmap composition"
    gen-maybe-int
    (lambda (m)
      (let ([f (lambda (x) (+ x 1))]
            [g (lambda (x) (* x 2))])
        (maybe=? (maybe-fmap (lambda (x) (f (g x))) m)
                 (maybe-fmap f (maybe-fmap g m)))))
    'tests 200)
)

;;; ============================================================================
;;; Either-specific monad properties
;;; ============================================================================

(test-group either-specific

  (define-property "left >>= f = left (short-circuit)"
    gen-int
    (lambda (x)
      (let ([e (left x)])
        (either=? (either-bind e (lambda (y) (right (+ y 1)))) e)))
    'tests 200)

  (define-property "right x >>= return = right x"
    gen-int
    (lambda (x)
      (either=? (either-bind (right x) right) (right x)))
    'tests 200)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
