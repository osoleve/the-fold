;;; lattice/linalg/test-numeric-instances-properties.ss — QuickCheck properties for numeric instances

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'vec)
(require 'matrix)
(require 'numeric-instances)

;;; ============================================================================
;;; Generators
;;; ============================================================================

(define gen-int-small
  (gen-int-range -20 20))

(define gen-same-length-pair
  (gen-bind (gen-int-range 0 20)
    (lambda (n)
      (gen-bind (gen-list-of n gen-int-small)
        (lambda (xs)
          (gen-map (lambda (ys) (cons xs ys))
                   (gen-list-of n gen-int-small)))))))

(define (gen-matrix rows cols)
  (gen-map matrix-from-lists
           (gen-list-of rows (gen-list-of cols gen-int-small))))

(define gen-same-shape-matrices
  (gen-bind (gen-int-range 0 4)
    (lambda (rows)
      (gen-bind (gen-int-range 0 4)
        (lambda (cols)
          (gen-bind (gen-matrix rows cols)
            (lambda (a)
              (gen-map (lambda (b) (cons a b))
                       (gen-matrix rows cols)))))))))

;;; ============================================================================
;;; Properties
;;; ============================================================================

(test-group numeric-instance-properties

  (define-property "vec+ matches vec-add"
    gen-same-length-pair
    (lambda (pair)
      (let* ([xs (car pair)]
             [ys (cdr pair)]
             [vx (vec-from-list xs)]
             [vy (vec-from-list ys)])
        (vec-equal? (vec+ vx vy)
                    (vec-add vx vy))))
    'tests 220)

  (define-property "vec-ap broadcasts a singleton function over values"
    (gen-bind (gen-int-range -8 8)
      (lambda (k)
        (gen-map (lambda (xs) (cons k xs))
                 (gen-list gen-int-small))))
    (lambda (args)
      (let* ([k (car args)]
             [xs (cdr args)]
             [vals (vec-from-list xs)]
             [applied (vec-ap (vec-pure (lambda (x) (+ x k))) vals)])
        (equal? (map (lambda (x) (+ x k)) xs)
                (vec->list applied))))
    'tests 200)

  (define-property "matrix-hadamard is element-wise multiplication"
    gen-same-shape-matrices
    (lambda (pair)
      (let* ([a (car pair)]
             [b (cdr pair)]
             [had (matrix-hadamard a b)]
             [manual (matrix-map2 * a b)])
        (matrix-equal? had manual)))
    'tests 180)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
