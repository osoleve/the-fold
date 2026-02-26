;;; lattice/fp/test-templates-properties.ss — QuickCheck properties for FP templates

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'templates)

;;; ============================================================================
;;; Generators
;;; ============================================================================

(define gen-int-small
  (gen-int-range -50 50))

(define gen-int-list
  (gen-list gen-int-small))

;;; ============================================================================
;;; Properties
;;; ============================================================================

(test-group templates-properties

  (define-property "mconcat monoid-sum equals fold-left +"
    gen-int-list
    (lambda (xs)
      (= (mconcat monoid-sum xs)
         (fold-left + 0 xs)))
    'tests 260)

  (define-property "verify-monoid-laws holds for monoid-sum on sample values"
    gen-int-list
    (lambda (xs)
      (verify-monoid-laws monoid-sum xs))
    'tests 200)

  (define-property "fmap-with functor-list matches map"
    gen-int-list
    (lambda (xs)
      (equal? (fmap-with functor-list (lambda (x) (+ x 1)) xs)
              (map (lambda (x) (+ x 1)) xs)))
    'tests 240)

  (define-property "replace-with functor-list sets all elements to constant"
    (gen-bind gen-int
      (lambda (k)
        (gen-map (lambda (xs) (cons k xs)) gen-int-list)))
    (lambda (args)
      (let* ([k (car args)]
             [xs (cdr args)]
             [repl (replace-with functor-list k xs)])
        (equal? repl (map (lambda (_x) k) xs))))
    'tests 220)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
