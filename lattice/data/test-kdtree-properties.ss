;;; lattice/data/test-kdtree-properties.ss — QuickCheck properties for k-d tree

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'kdtree)

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

(test-group kdtree-properties

  (define-property "kdtree-build preserves point count"
    gen-int-list
    (lambda (xs)
      (let* ([pts (indexed-points xs)]
             [tree (kdtree-build pts)])
        (= (kdtree-size tree) (length pts))))
    'tests 200
    'max-size 35)

  (define-property "nearest of each inserted point is itself"
    gen-int-list
    (lambda (xs)
      (let* ([pts (indexed-points xs)]
             [tree (kdtree-build pts)])
        (all-pred? (lambda (p)
                     (equal? (kdtree-nearest tree p) p))
                   pts)))
    'tests 160
    'max-size 28)

  (define-property "full bounding-box range contains all points"
    gen-int-list
    (lambda (xs)
      (let ([pts (indexed-points xs)])
        (if (null? pts)
            (null? (kdtree-range (kdtree-build pts) '(0 0) '(0 0)))
            (let* ([tree (kdtree-build pts)]
                   [result (kdtree-range tree
                                         (list (min-x pts) (min-y pts))
                                         (list (max-x pts) (max-y pts)))])
              (all-pred? (lambda (p) (pair? (member p result))) pts)))))
    'tests 140
    'max-size 30)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
