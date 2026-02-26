;;; lattice/linalg/test-iteration-properties.ss — QuickCheck properties for iteration macros

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'iteration)

;;; ============================================================================
;;; Generators
;;; ============================================================================

(define gen-int-small
  (gen-int-range -30 30))

(define gen-int-list
  (gen-list gen-int-small))

(define gen-same-length-pair
  (gen-bind (gen-int-range 0 30)
    (lambda (n)
      (gen-bind (gen-list-of n gen-int-small)
        (lambda (xs)
          (gen-map (lambda (ys) (cons xs ys))
                   (gen-list-of n gen-int-small)))))))

;;; ============================================================================
;;; Properties
;;; ============================================================================

(test-group iteration-properties

  (define-property "vec-map-idx identity body reproduces the original vector"
    gen-int-list
    (lambda (xs)
      (let* ([v (list->vector xs)]
             [mapped (vec-map-idx i v (vector-ref v i))])
        (equal? (vector->list mapped) xs)))
    'tests 240)

  (define-property "vec-fold sum matches fold-left on list view"
    gen-int-list
    (lambda (xs)
      (let* ([v (list->vector xs)]
             [sum-iter (vec-fold acc 0 x v (+ acc x))]
             [sum-list (fold-left + 0 xs)])
        (= sum-iter sum-list)))
    'tests 240)

  (define-property "vec-zip-map-idx pointwise addition matches list map"
    gen-same-length-pair
    (lambda (pair)
      (let* ([xs (car pair)]
             [ys (cdr pair)]
             [vx (list->vector xs)]
             [vy (list->vector ys)]
             [vz (vec-zip-map-idx i vx vy (+ (vector-ref vx i) (vector-ref vy i)))])
        (equal? (vector->list vz)
                (map + xs ys))))
    'tests 220)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
