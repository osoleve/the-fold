;;; lattice/data/test-community-homology-properties.ss — QuickCheck properties for community homology quality

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'graph-community)

;;; ============================================================================
;;; Generators
;;; ============================================================================

(define gen-size
  (gen-int-range 1 20))

(define gen-valid-quality-args
  (gen-bind gen-size
    (lambda (size)
      (gen-bind (gen-int-range 1 size)
        (lambda (b0)
          (let ([max-b1 (max 0 (/ (* (- size 1) (- size 2)) 2))])
            (gen-map (lambda (b1) (list size b0 b1))
                     (gen-int-range 0 max-b1))))))))

(define gen-connected-cycle-pair
  (gen-bind (gen-int-range 3 20)
    (lambda (size)
      (let* ([max-b1 (max 0 (/ (* (- size 1) (- size 2)) 2))])
        (gen-bind (gen-int-range 0 max-b1)
          (lambda (b1a)
            (gen-map (lambda (b1b) (list size b1a b1b))
                     (gen-int-range b1a max-b1))))))))

;;; ============================================================================
;;; Properties
;;; ============================================================================

(test-group community-homology-properties

  (define-property "community-homology-quality stays in [0, 1]"
    gen-valid-quality-args
    (lambda (args)
      (let* ([size (car args)]
             [b0 (cadr args)]
             [b1 (caddr args)]
             [q (community-homology-quality size b0 b1)])
        (and (<= 0.0 q) (<= q 1.0))))
    'tests 260)

  (define-property "more fragmentation does not improve score"
    (gen-bind (gen-int-range 2 20)
      (lambda (size)
        (let ([max-b1 (max 0 (/ (* (- size 1) (- size 2)) 2))])
          (gen-map (lambda (b1) (list size b1))
                   (gen-int-range 0 max-b1)))))
    (lambda (args)
      (let* ([size (car args)]
             [b1 (cadr args)]
             [connected (community-homology-quality size 1 b1)]
             [fragmented (community-homology-quality size 2 b1)])
        (>= connected fragmented)))
    'tests 220)

  (define-property "for connected communities, adding cycles is non-decreasing"
    gen-connected-cycle-pair
    (lambda (args)
      (let* ([size (car args)]
             [b1a (cadr args)]
             [b1b (caddr args)]
             [qa (community-homology-quality size 1 b1a)]
             [qb (community-homology-quality size 1 b1b)])
        (<= qa qb)))
    'tests 220)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
