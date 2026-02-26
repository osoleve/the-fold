;;; lattice/data/test-hamt-properties.ss — QuickCheck properties for HAMT

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'hamt)

;;; ============================================================================
;;; Generators
;;; ============================================================================

(define gen-key
  (gen-int-range -200 200))

(define gen-val
  (gen-int-range -1000 1000))

(define gen-kv
  (gen-bind gen-key
    (lambda (k)
      (gen-map (lambda (v) (cons k v)) gen-val))))

(define gen-kv-list
  (gen-list gen-kv))

(define (alist->hamt-last-write-wins kvs)
  (fold-left (lambda (h kv)
               (hamt-assoc (car kv) (cdr kv) h))
             hamt-empty
             kvs))

;;; ============================================================================
;;; Properties
;;; ============================================================================

(test-group hamt-properties

  (define-property "assoc then lookup returns value"
    (gen-bind gen-key
      (lambda (k)
        (gen-map (lambda (v) (cons k v)) gen-val)))
    (lambda (kv)
      (let* ([k (car kv)]
             [v (cdr kv)]
             [h (hamt-assoc k v hamt-empty)])
        (equal? v (hamt-lookup k h))))
    'tests 260)

  (define-property "re-associating existing key does not increase size"
    (gen-bind gen-key
      (lambda (k)
        (gen-bind gen-val
          (lambda (v1)
            (gen-map (lambda (v2) (list k v1 v2)) gen-val)))))
    (lambda (args)
      (let* ([k (car args)]
             [v1 (cadr args)]
             [v2 (caddr args)]
             [h1 (hamt-assoc k v1 hamt-empty)]
             [h2 (hamt-assoc k v2 h1)])
        (and (= 1 (hamt-size h1))
             (= 1 (hamt-size h2))
             (equal? v2 (hamt-lookup k h2)))))
    'tests 220)

  (define-property "dissoc removes key but preserves others"
    gen-kv-list
    (lambda (kvs)
      (let* ([h (alist->hamt-last-write-wins kvs)]
             [keys (hamt-keys h)])
        (or (null? keys)
            (let* ([k (car keys)]
                   [h2 (hamt-dissoc k h)])
              (and (not (hamt-has-key? k h2))
                   (= (hamt-size h2) (- (hamt-size h) 1)))))))
    'tests 200)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
