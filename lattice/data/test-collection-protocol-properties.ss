;;; lattice/data/test-collection-protocol-properties.ss — QuickCheck properties for collection protocols

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'collection-impl)

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

;;; ============================================================================
;;; Properties
;;; ============================================================================

(test-group collection-protocol-properties

  (define-property "heap coll-size equals source list length"
    gen-int-list
    (lambda (xs)
      (= (coll-size (list->heap xs)) (length xs)))
    'tests 220)

  (define-property "heap coll-sum matches arithmetic sum"
    gen-int-list
    (lambda (xs)
      (let ([h (list->heap xs)])
        (= (coll-sum h) (if (null? xs) 0 (apply + xs)))))
    'tests 200)

  (define-property "keyed-insert then keyed-lookup returns inserted value"
    (gen-bind gen-int-small
      (lambda (k)
        (gen-map (lambda (v) (cons k v)) gen-int-small)))
    (lambda (kv)
      (let* ([k (car kv)]
             [v (cdr kv)]
             [tree (keyed-insert avl-empty k v)])
        (and (keyed-contains? tree k)
             (equal? v (keyed-lookup tree k)))))
    'tests 220)

  (define-property "spatial coll-size matches number of inserted points"
    gen-int-list
    (lambda (xs)
      (let* ([pts (indexed-points xs)]
             [kd (kdtree-build pts)]
             [qt (quadtree-build pts (make-bounds 0 0 1000 1000))])
        (and (= (coll-size kd) (length pts))
             (= (coll-size qt) (length pts)))))
    'tests 160
    'max-size 35)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
