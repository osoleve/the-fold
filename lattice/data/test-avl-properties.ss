;;; lattice/data/test-avl-properties.ss — QuickCheck property tests for AVL trees

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'avl-tree)

;;; ============================================================================
;;; Generators
;;; ============================================================================

;; Generate a list of unique key-value pairs
(define gen-kv-pair
  (gen-pair gen-int gen-int))

(define gen-kv-list
  (gen-list gen-kv-pair))

;; Build an AVL tree from a generated kv-list
(define gen-avl-tree
  (gen-map (lambda (kvs)
             (fold-left (lambda (t kv)
                          (avl-insert (car kv) (cdr kv) t))
                        avl-empty
                        kvs))
           gen-kv-list))

;; Generate a tree and a key to insert
(define gen-avl-and-key
  (gen-bind gen-kv-list
    (lambda (kvs)
      (gen-bind gen-int
        (lambda (k)
          (gen-map (lambda (v) (list kvs k v))
                   gen-int))))))

;;; ============================================================================
;;; Structural invariants
;;; ============================================================================

(test-group avl-invariants

  (define-property "AVL balance invariant holds after building from list"
    gen-avl-tree
    (lambda (tree) (avl-valid? tree))
    'tests 300)

  (define-property "BST ordering invariant holds after building from list"
    gen-avl-tree
    (lambda (tree) (avl-bst-valid? tree))
    'tests 300)

  (define-property "in-order traversal is sorted"
    gen-avl-tree
    (lambda (tree)
      (let ([keys (avl-keys tree)])
        (or (null? keys)
            (null? (cdr keys))
            (let loop ([prev (car keys)] [rest (cdr keys)])
              (or (null? rest)
                  (and (< prev (car rest))
                       (loop (car rest) (cdr rest))))))))
    'tests 300)
)

;;; ============================================================================
;;; Insert properties
;;; ============================================================================

(test-group avl-insert-properties

  (define-property "insert maintains AVL invariant"
    gen-avl-and-key
    (lambda (args)
      (let* ([kvs (car args)]
             [k (cadr args)]
             [v (caddr args)]
             [tree (fold-left (lambda (t kv) (avl-insert (car kv) (cdr kv) t))
                              avl-empty kvs)])
        (avl-valid? (avl-insert k v tree))))
    'tests 300)

  (define-property "insert maintains BST invariant"
    gen-avl-and-key
    (lambda (args)
      (let* ([kvs (car args)]
             [k (cadr args)]
             [v (caddr args)]
             [tree (fold-left (lambda (t kv) (avl-insert (car kv) (cdr kv) t))
                              avl-empty kvs)])
        (avl-bst-valid? (avl-insert k v tree))))
    'tests 300)

  (define-property "lookup after insert finds the inserted value"
    gen-avl-and-key
    (lambda (args)
      (let* ([kvs (car args)]
             [k (cadr args)]
             [v (caddr args)]
             [tree (fold-left (lambda (t kv) (avl-insert (car kv) (cdr kv) t))
                              avl-empty kvs)]
             [tree2 (avl-insert k v tree)])
        (equal? v (avl-lookup k tree2))))
    'tests 300)

  (define-property "insert increases size by at most 1"
    gen-avl-and-key
    (lambda (args)
      (let* ([kvs (car args)]
             [k (cadr args)]
             [v (caddr args)]
             [tree (fold-left (lambda (t kv) (avl-insert (car kv) (cdr kv) t))
                              avl-empty kvs)]
             [old-size (avl-size tree)]
             [new-size (avl-size (avl-insert k v tree))])
        (and (>= new-size old-size)
             (<= new-size (+ old-size 1)))))
    'tests 200)
)

;;; ============================================================================
;;; Delete properties
;;; ============================================================================

(test-group avl-delete-properties

  (define-property "delete maintains AVL invariant"
    (gen-bind gen-kv-list
      (lambda (kvs)
        (gen-map (lambda (k) (cons kvs k)) gen-int)))
    (lambda (args)
      (let* ([kvs (car args)]
             [k (cdr args)]
             [tree (fold-left (lambda (t kv) (avl-insert (car kv) (cdr kv) t))
                              avl-empty kvs)])
        (avl-valid? (avl-delete k tree))))
    'tests 300)

  (define-property "delete maintains BST invariant"
    (gen-bind gen-kv-list
      (lambda (kvs)
        (gen-map (lambda (k) (cons kvs k)) gen-int)))
    (lambda (args)
      (let* ([kvs (car args)]
             [k (cdr args)]
             [tree (fold-left (lambda (t kv) (avl-insert (car kv) (cdr kv) t))
                              avl-empty kvs)])
        (avl-bst-valid? (avl-delete k tree))))
    'tests 300)

  (define-property "deleted key is not found"
    (gen-bind gen-kv-list
      (lambda (kvs)
        (gen-map (lambda (k) (cons kvs k)) gen-int)))
    (lambda (args)
      (let* ([kvs (car args)]
             [k (cdr args)]
             [tree (fold-left (lambda (t kv) (avl-insert (car kv) (cdr kv) t))
                              avl-empty kvs)]
             [tree2 (avl-delete k tree)])
        (not (avl-contains? k tree2))))
    'tests 300)
)

;;; ============================================================================
;;; Roundtrip properties
;;; ============================================================================

(test-group avl-roundtrip

  (define-property "avl->list produces sorted pairs"
    gen-avl-tree
    (lambda (tree)
      (let ([pairs (avl->list tree)])
        (or (null? pairs)
            (null? (cdr pairs))
            (let loop ([prev (caar pairs)] [rest (cdr pairs)])
              (or (null? rest)
                  (and (< prev (caar rest))
                       (loop (caar rest) (cdr rest))))))))
    'tests 200)

  (define-property "size equals length of avl->list"
    gen-avl-tree
    (lambda (tree)
      (= (avl-size tree) (length (avl->list tree))))
    'tests 200)
)

;;; ============================================================================
;;; Set operations
;;; ============================================================================

(test-group avl-set-properties

  (define-property "union is commutative on key sets"
    (gen-pair (gen-list gen-int) (gen-list gen-int))
    (lambda (pair)
      (let* ([t1 (avl-from-keys (car pair))]
             [t2 (avl-from-keys (cdr pair))]
             [u1 (avl-keys (avl-set-union t1 t2))]
             [u2 (avl-keys (avl-set-union t2 t1))])
        (equal? u1 u2)))
    'tests 200
    'max-size 20)

  (define-property "intersection is subset of both"
    (gen-pair (gen-list gen-int) (gen-list gen-int))
    (lambda (pair)
      (let* ([t1 (avl-from-keys (car pair))]
             [t2 (avl-from-keys (cdr pair))]
             [inter (avl-set-intersection t1 t2)]
             [inter-keys (avl-keys inter)])
        (let loop ([ks inter-keys])
          (or (null? ks)
              (and (avl-contains? (car ks) t1)
                   (avl-contains? (car ks) t2)
                   (loop (cdr ks)))))))
    'tests 200
    'max-size 20)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
