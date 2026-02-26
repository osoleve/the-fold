;;; lattice/data/test-quadtree-properties.ss — QuickCheck properties for quadtree

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'quadtree)

;;; ============================================================================
;;; Helpers
;;; ============================================================================

(define gen-int-small
  (gen-int-range -80 80))

(define gen-int-list
  (gen-list gen-int-small))

(define (indexed-points xs)
  (let loop ([rest xs] [i 0] [acc '()])
    (if (null? rest)
        (reverse acc)
        (loop (cdr rest) (+ i 1) (cons (list i (car rest)) acc)))))

(define (all-pred? pred xs)
  (or (null? xs)
      (and (pred (car xs))
           (all-pred? pred (cdr xs)))))

(define (min-x pts)
  (apply min (map car pts)))

(define (max-x pts)
  (apply max (map car pts)))

(define (min-y pts)
  (apply min (map cadr pts)))

(define (max-y pts)
  (apply max (map cadr pts)))

;;; ============================================================================
;;; Properties
;;; ============================================================================

(test-group quadtree-properties

  (define-property "quadtree-build-auto preserves point count"
    gen-int-list
    (lambda (xs)
      (let* ([pts (indexed-points xs)]
             [tree (quadtree-build pts (make-bounds 0 0 1000 1000))])
        (= (quadtree-size tree) (length pts))))
    'tests 200
    'max-size 35)

  (define-property "all inserted points are members of the quadtree"
    gen-int-list
    (lambda (xs)
      (let* ([pts (indexed-points xs)]
             [tree (quadtree-build pts (make-bounds 0 0 1000 1000))])
        (all-pred? (lambda (p)
                     (quadtree-member? tree (car p) (cadr p)))
                   pts)))
    'tests 150
    'max-size 28)

  (define-property "full rectangle query returns all points"
    gen-int-list
    (lambda (xs)
      (let ([pts (indexed-points xs)])
        (let* ([tree (quadtree-build pts (make-bounds 0 0 1000 1000))]
               [result (quadtree-range-rect tree -1000 -1000 1000 1000)])
          (= (length result) (length pts)))))
    'tests 140
    'max-size 30)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
