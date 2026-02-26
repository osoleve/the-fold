;;; lattice/linalg/test-dep-linalg-properties.ss — QuickCheck properties for dep-linalg

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'vec)
(require 'matrix)
(require 'dep-linalg)

;;; ============================================================================
;;; Generators
;;; ============================================================================

(define gen-int-small
  (gen-int-range -25 25))

(define gen-int-list
  (gen-list gen-int-small))

(define gen-small-dim
  (gen-int-range 0 4))

(define (gen-matrix rows cols)
  (gen-map matrix-from-lists
           (gen-list-of rows (gen-list-of cols gen-int-small))))

;;; ============================================================================
;;; Properties
;;; ============================================================================

(test-group dep-linalg-properties

  (define-property "vec-append-typed matches append at runtime"
    (gen-pair gen-int-list gen-int-list)
    (lambda (pair)
      (let* ([xs (car pair)]
             [ys (cdr pair)]
             [v1 (vec-from-list xs)]
             [v2 (vec-from-list ys)]
             [result (vec-append-typed (length xs) (length ys) 'Int v1 v2)])
        (equal? (append xs ys)
                (vec->list result))))
    'tests 220)

  (define-property "vec-map-typed preserves mapped list semantics"
    gen-int-list
    (lambda (xs)
      (let* ([v (vec-from-list xs)]
             [mapped (vec-map-typed (length xs) 'Int 'Int (lambda (x) (+ x 1)) v)])
        (equal? (map (lambda (x) (+ x 1)) xs)
                (vec->list mapped))))
    'tests 220)

  (define-property "matrix-add-typed matches matrix-add"
    (gen-bind gen-small-dim
      (lambda (rows)
        (gen-bind gen-small-dim
          (lambda (cols)
            (gen-bind (gen-matrix rows cols)
              (lambda (a)
                (gen-map (lambda (b) (list rows cols a b))
                         (gen-matrix rows cols))))))))
    (lambda (args)
      (let* ([rows (car args)]
             [cols (cadr args)]
             [a (caddr args)]
             [b (cadddr args)]
             [typed (matrix-add-typed rows cols 'Int a b)]
             [plain (matrix-add a b)])
        (matrix-equal? typed plain)))
    'tests 180)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
