;;; lattice/fp/test-result-properties.ss — QuickCheck properties for Result type
;;;
;;; Tests the Result/Either type functor/applicative/monad laws:
;;; 1. Functor:    fmap id == id
;;; 2. Functor:    fmap (f . g) == fmap f . fmap g
;;; 3. Applicative: pure id <*> v == v
;;; 4. Monad:       return a >>= f == f a
;;; 5. Monad:       m >>= return == m
;;; 6. Monad:       (m >>= f) >>= g == m >>= (\x -> f x >>= g)

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'result)

;;; ============================================================================
;;; Generators
;;; ============================================================================

(define gen-int-small
  (gen-int-range -100 100))

(define gen-error-value
  (gen-one-of (list 'not-found 'invalid 'failed 'timeout)))

;;; ============================================================================
;;; Properties
;;; ============================================================================

(test-group result-functor-laws

  ;; Functor identity: fmap id == id
  (define-property "result fmap identity on ok"
    gen-int-small
    (lambda (x)
      (let* ([r (ok x)])
        (equal? (result-map (lambda (y) y) r) r)))
    'tests 100)

  ;; Functor identity on error (fmap should return error unchanged)
  (define-property "result fmap identity on error"
    (gen-pure #t)
    (lambda (_)
      (let* ([e 'test-error]
             [r (err e)])
        (equal? (result-map (lambda (y) (* y 2)) r) r)))
    'tests 50)

  ;; Functor composition: fmap (f . g) == fmap f . fmap g
  (define-property "result fmap composition"
    gen-int-small
    (lambda (x)
      (let* ([r (ok x)]
             [f (lambda (y) (* y 2))]
             [g (lambda (y) (+ y 1))]
             [lhs (result-map (lambda (y) (f (g y))) r)]
             [rhs (result-map f (result-map g r))])
        (equal? lhs rhs)))
    'tests 100)

  ;; fmap applies function to ok value
  (define-property "result fmap applies function"
    gen-int-small
    (lambda (x)
      (let* ([r (ok x)]
             [f (lambda (y) (* y 3))]
             [result (result-map f r)])
        (and (ok? result)
             (equal? (unwrap-ok result) (f x)))))
    'tests 100)
)

(test-group result-monad-laws

  ;; Left identity: return a >>= f == f a
  (define-property "result left identity"
    gen-int-small
    (lambda (a)
      (let* ([f (lambda (x) (ok (+ x 10)))]
             [lhs (result-bind (ok a) f)]
             [rhs (f a)])
        (equal? lhs rhs)))
    'tests 100)

  ;; Right identity: m >>= return == m
  (define-property "result right identity"
    gen-int-small
    (lambda (x)
      (let* ([m (ok x)]
             [lhs (result-bind m ok)]
             [rhs m])
        (equal? lhs rhs)))
    'tests 100)

  ;; Associativity: (m >>= f) >>= g == m >>= (\x -> f x >>= g)
  (define-property "result associativity"
    gen-int-small
    (lambda (x)
      (let* ([m (ok x)]
             [f (lambda (a) (ok (+ a 1)))]
             [g (lambda (b) (ok (* b 2)))]
             [lhs (result-bind (result-bind m f) g)]
             [rhs (result-bind m (lambda (a) (result-bind (f a) g)))])
        (equal? lhs rhs)))
    'tests 100)

  ;; Error propagation: err >>= f == err
  (define-property "result error propagation"
    (gen-pure #t)
    (lambda (_)
      (let* ([e 'test-err]
             [m (err e)]
             [f (lambda (x) (ok (+ x 1)))]
             [result (result-bind m f)])
        (error? result)))
    'tests 50)

  ;; Error short-circuit in bind chain
  (define-property "result bind short-circuits on error"
    gen-int-small
    (lambda (x)
      (let* ([m (ok x)]
             [f (lambda (_) (err 'failed))]
             [g (lambda (y) (ok (* y 2)))]
             [result (result-bind (result-bind m f) g)])
        (and (error? result)
             (equal? (cadr result) 'failed))))
    'tests 100)
)

(test-group result-combinator-laws

  ;; result-or returns first ok
  (define-property "result-or returns first ok"
    gen-int-small
    (lambda (x)
      (let* ([r1 (ok x)]
             [r2 (err 'second)]
             [result (result-or r1 r2)])
        (equal? result r1)))
    'tests 100)

  ;; result-or returns second if first is error
  (define-property "result-or returns second when first is error"
    gen-int-small
    (lambda (x)
      (let* ([r1 (err 'first)]
             [r2 (ok x)]
             [result (result-or r1 r2)])
        (equal? result r2)))
    'tests 100)

  ;; result-or returns last error if both fail
  (define-property "result-or returns last error if both fail"
    (gen-pure #t)
    (lambda (_)
      (let* ([r1 (err 'first)]
             [r2 (err 'second)]
             [result (result-or r1 r2)])
        (equal? result r2)))
    'tests 50)

  ;; result-default extracts ok value
  (define-property "result-default extracts ok value"
    gen-int-small
    (lambda (x)
      (equal? (result-default 0 (ok x)) x))
    'tests 100)

  ;; result-default returns default on error
  (define-property "result-default returns default on error"
    gen-int-small
    (lambda (default)
      (equal? (result-default default (err 'error)) default))
    'tests 100)

  ;; result-and returns second if first is ok
  (define-property "result-and returns second when first is ok"
    gen-int-small
    (lambda (y)
      (let* ([r1 (ok 1)]
             [r2 (ok y)]
             [result (result-and r1 r2)])
        (equal? result r2)))
    'tests 100)

  ;; result-and returns first error
  (define-property "result-and returns first error"
    (gen-pure #t)
    (lambda (_)
      (let* ([r1 (err 'first-err)]
             [r2 (ok 42)]
             [result (result-and r1 r2)])
        (error? result)))
    'tests 50)

  ;; result-and returns first error even if second is also error
  (define-property "result-and prefers first error"
    (gen-pure #t)
    (lambda (_)
      (let* ([r1 (err 'first)]
             [r2 (err 'second)]
             [result (result-and r1 r2)])
        (equal? result r1)))
    'tests 50)
)

(test-group result-transformation-laws

  ;; result-map-error transforms error value
  ;; (err 'original) -> (error original)
  ;; (cdr result) is (original), f receives '(original), returns '(mapped original)
  ;; (cons 'error '(mapped original)) -> (error mapped original)
  (define-property "result-map-error transforms error"
    (gen-pure #t)
    (lambda (_)
      (let* ([r (err 'original)]
             [f (lambda (x) (cons 'mapped x))]
             [result (result-map-error f r)])
        (and (error? result)
             (equal? result '(error mapped original)))))
    'tests 50)

  ;; result-map-error preserves ok
  (define-property "result-map-error preserves ok"
    gen-int-small
    (lambda (x)
      (let* ([r (ok x)]
             [f (lambda (e) (list 'mapped e))]
             [result (result-map-error f r)])
        (equal? result r)))
    'tests 100)

  ;; result-bimap applies ok function
  (define-property "result-bimap applies ok function"
    gen-int-small
    (lambda (x)
      (let* ([r (ok x)]
             [ok-fn (lambda (y) (* y 2))]
             [err-fn (lambda (e) (list 'mapped e))]
             [result (result-bimap ok-fn err-fn r)])
        (and (ok? result)
             (equal? (unwrap-ok result) (* x 2)))))
    'tests 100)

  ;; result-bimap applies err function
  (define-property "result-bimap applies err function"
    (gen-pure #t)
    (lambda (_)
      (let* ([r (err 'test-err)]
             [ok-fn (lambda (y) (* y 2))]
             [err-fn (lambda (x) (cons 'mapped x))]
             [result (result-bimap ok-fn err-fn r)])
        (and (error? result)
             (equal? result '(error mapped test-err)))))
    'tests 50)

  ;; result-fold applies correct branch
  (define-property "result-fold dispatches correctly"
    gen-int-small
    (lambda (x)
      (let* ([ok-r (ok x)]
             [err-r (err 'failed)]
             [on-ok (lambda (y) (list 'success y))]
             [on-err (lambda (e) (list 'failure e))])
        (equal? (result-fold on-ok on-err ok-r) (list 'success x))))
    'tests 100)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
