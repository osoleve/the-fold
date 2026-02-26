;;; lattice/data/test-data-structures-properties.ss — QuickCheck properties for stack/queue/set/dict

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'stack)
(require 'queue)
(require 'set)
(require 'dict)

;;; ============================================================================
;;; Helpers
;;; ============================================================================

(define gen-int-small
  (gen-int-range -100 100))

(define gen-int-list
  (gen-list gen-int-small))

(define (stack-pop-all s)
  (let loop ([st s] [acc '()])
    (if (stack-empty? st)
        (reverse acc)
        (let-values ([(next elem) (stack-pop st)])
          (loop next (cons elem acc))))))

(define (queue-dequeue-all q)
  (let loop ([qq q] [acc '()])
    (if (queue-empty? qq)
        (reverse acc)
        (let-values ([(next elem) (queue-dequeue qq)])
          (loop next (cons elem acc))))))

(define gen-kv
  (gen-bind gen-int-small
    (lambda (k)
      (gen-map (lambda (v) (cons k v)) gen-int-small))))

(define gen-kv-list
  (gen-list gen-kv))

(define (alist->dict-last-write-wins kvs)
  (fold-left (lambda (d kv)
               (dict-assoc (car kv) (cdr kv) d))
             dict-empty
             kvs))

;;; ============================================================================
;;; Properties
;;; ============================================================================

(test-group data-structures-properties

  (define-property "list->stack then repeated pop returns original order"
    gen-int-list
    (lambda (xs)
      (equal? (stack-pop-all (list->stack xs)) xs))
    'tests 240)

  (define-property "list->queue then repeated dequeue returns original order"
    gen-int-list
    (lambda (xs)
      (equal? (queue-dequeue-all (list->queue xs)) xs))
    'tests 240)

  (define-property "set-add changes size iff element was absent"
    (gen-bind gen-int-list
      (lambda (xs)
        (gen-map (lambda (x) (cons xs x)) gen-int-small)))
    (lambda (args)
      (let* ([xs (car args)]
             [x (cdr args)]
             [s (list->set xs)]
             [s2 (set-add x s)])
        (if (set-member? x s)
            (= (set-size s2) (set-size s))
            (= (set-size s2) (+ (set-size s) 1)))))
    'tests 220)

  (define-property "dict-assoc then lookup returns inserted value"
    (gen-bind gen-kv-list
      (lambda (kvs)
        (gen-bind gen-int-small
          (lambda (k)
            (gen-map (lambda (v) (list kvs k v)) gen-int-small)))))
    (lambda (args)
      (let* ([kvs (car args)]
             [k (cadr args)]
             [v (caddr args)]
             [d (alist->dict-last-write-wins kvs)]
             [d2 (dict-assoc k v d)])
        (and (dict-has-key? k d2)
             (equal? v (dict-lookup k d2)))))
    'tests 220)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
