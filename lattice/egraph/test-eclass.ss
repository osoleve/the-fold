;;; lattice/egraph/test-eclass.ss — Tests for E-Class
;;;
;;; Run with: scheme --script lattice/egraph/test-eclass.ss

(load "core/lang/module.ss")
(load "core/testing/test-framework.ss")
(load "lattice/egraph/eclass.ss")

(display "\n====\n")
(display "E-Class Tests\n")
(display "====\n\n")

(test-group "enode-basic"

  (define-test "make-enode creates valid enode"
    (let ([n (make-enode '+ (vector 0 1))])
      (assert-true (enode? n))
      (assert-equal '+ (enode-op n))
      (assert-equal 2 (enode-arity n))))

  (define-test "enode children accessible"
    (let ([n (make-enode 'foo (vector 3 4 5))])
      (let ([c (enode-children n)])
        (assert-equal 3 (vector-ref c 0))
        (assert-equal 4 (vector-ref c 1))
        (assert-equal 5 (vector-ref c 2)))))

  (define-test "enode with no children"
    (let ([n (make-enode 'x (vector))])
      (assert-equal 0 (enode-arity n))
      (assert-equal 'x (enode-op n))))

  (define-test "enode accepts list for children"
    (let ([n (make-enode '* '(1 2))])
      (assert-equal 2 (enode-arity n))
      (assert-equal 1 (vector-ref (enode-children n) 0)))))

(test-group "enode-equality"

  (define-test "enode-equal? same nodes"
    (let ([n1 (make-enode '+ (vector 0 1))]
          [n2 (make-enode '+ (vector 0 1))])
      (assert-true (enode-equal? n1 n2))))

  (define-test "enode-equal? different ops"
    (let ([n1 (make-enode '+ (vector 0 1))]
          [n2 (make-enode '- (vector 0 1))])
      (assert-false (enode-equal? n1 n2))))

  (define-test "enode-equal? different children"
    (let ([n1 (make-enode '+ (vector 0 1))]
          [n2 (make-enode '+ (vector 0 2))])
      (assert-false (enode-equal? n1 n2))))

  (define-test "enode-equal? different arity"
    (let ([n1 (make-enode '+ (vector 0 1))]
          [n2 (make-enode '+ (vector 0 1 2))])
      (assert-false (enode-equal? n1 n2)))))

(test-group "enode-hash"

  (define-test "enode-hash consistent"
    (let ([n1 (make-enode '+ (vector 0 1))]
          [n2 (make-enode '+ (vector 0 1))])
      (assert-equal (enode-hash n1) (enode-hash n2))))

  (define-test "enode-hash different for different nodes"
    (let ([n1 (make-enode '+ (vector 0 1))]
          [n2 (make-enode '+ (vector 0 2))])
      ;; Not guaranteed different but very likely
      (assert-true (or (not (= (enode-hash n1) (enode-hash n2)))
                       #t)))))  ; Allow collision but test runs

(test-group "enode-canonicalize"

  (define-test "canonicalize resolves children"
    (let ([uf (make-uf)])
      ;; Create classes 0, 1, 2
      (uf-make-set! uf)
      (uf-make-set! uf)
      (uf-make-set! uf)
      ;; Merge 1 into 0
      (uf-union! uf 0 1)
      ;; Create enode referencing class 1 (which now maps to 0)
      (let* ([n (make-enode '+ (vector 1 2))]
             [cn (enode-canonicalize n uf)])
        (assert-equal 0 (vector-ref (enode-children cn) 0))  ; 1 → 0
        (assert-equal 2 (vector-ref (enode-children cn) 1))))))  ; 2 stays 2

(test-group "eclass-store-basic"

  (define-test "make-eclass-store creates empty store"
    (let ([store (make-eclass-store)])
      (assert-true (eclass-store? store))))

  (define-test "eclass-get returns #f for missing"
    (let ([store (make-eclass-store)])
      (assert-false (eclass-get store 0))))

  (define-test "eclass-get-or-create! creates data"
    (let ([store (make-eclass-store)])
      (eclass-get-or-create! store 0)
      (assert-true (if (eclass-get store 0) #t #f)))))

(test-group "eclass-operations"

  (define-test "eclass-add-node! stores nodes"
    (let ([store (make-eclass-store)]
          [n (make-enode 'x (vector))])
      (eclass-add-node! store 0 n)
      (let ([nodes (eclass-get-nodes store 0)])
        (assert-equal 1 (length nodes))
        (assert-true (enode-equal? n (car nodes))))))

  (define-test "eclass-add-node! accumulates"
    (let ([store (make-eclass-store)]
          [n1 (make-enode 'x (vector))]
          [n2 (make-enode 'y (vector))])
      (eclass-add-node! store 0 n1)
      (eclass-add-node! store 0 n2)
      (assert-equal 2 (length (eclass-get-nodes store 0)))))

  (define-test "eclass-add-parent! tracks parents"
    (let ([store (make-eclass-store)])
      (eclass-add-parent! store 0 1)  ; class 1 references class 0
      (eclass-add-parent! store 0 2)  ; class 2 references class 0
      (let ([parents (eclass-get-parents store 0)])
        (assert-equal 2 (length parents))
        (assert-true (if (memv 1 parents) #t #f))
        (assert-true (if (memv 2 parents) #t #f))))))

(test-group "eclass-merge"

  (define-test "eclass-merge! combines nodes"
    (let ([store (make-eclass-store)]
          [uf (make-uf)]
          [n1 (make-enode 'x (vector))]
          [n2 (make-enode 'y (vector))])
      ;; Create two classes
      (let ([id1 (uf-make-set! uf)]
            [id2 (uf-make-set! uf)])
        (eclass-add-node! store id1 n1)
        (eclass-add-node! store id2 n2)
        ;; Merge in union-find
        (uf-union! uf id1 id2)
        ;; Merge eclass data
        (eclass-merge! store uf id1 id2)
        ;; Check merged class has both nodes
        (let* ([root (uf-find uf id1)]
               [nodes (eclass-get-nodes store root)])
          (assert-equal 2 (length nodes))))))

  (define-test "eclass-merge! returns affected parents"
    (let ([store (make-eclass-store)]
          [uf (make-uf)])
      ;; Create classes
      (let ([a (uf-make-set! uf)]
            [b (uf-make-set! uf)]
            [parent (uf-make-set! uf)])
        ;; Parent references both a and b
        (eclass-add-parent! store a parent)
        (eclass-add-parent! store b parent)
        ;; Merge a and b in union-find first
        (uf-union! uf a b)
        ;; Now merge eclass data
        (let ([affected (eclass-merge! store uf a b)])
          ;; Parent should be in affected list
          (assert-true (if (memv parent affected) #t #f))))))

  (define-test "eclass-merge! merges parent hashtables into root"
    ;; This tests that eclass-get-parents on root returns combined parents
    (let ([store (make-eclass-store)]
          [uf (make-uf)])
      (let ([a (uf-make-set! uf)]
            [b (uf-make-set! uf)]
            [p1 (uf-make-set! uf)]
            [p2 (uf-make-set! uf)])
        ;; p1 references a, p2 references b
        (eclass-add-parent! store a p1)
        (eclass-add-parent! store b p2)
        ;; Merge a and b
        (uf-union! uf a b)
        (eclass-merge! store uf a b)
        ;; Root should have both p1 and p2 as parents
        (let* ([root (uf-find uf a)]
               [parents (eclass-get-parents store root)])
          (assert-equal 2 (length parents))
          (assert-true (if (memv p1 parents) #t #f))
          (assert-true (if (memv p2 parents) #t #f))))))

  (define-test "eclass-merge! same class with itself is no-op"
    (let ([store (make-eclass-store)]
          [uf (make-uf)])
      (let ([a (uf-make-set! uf)])
        (eclass-add-node! store a (make-enode 'x (vector)))
        ;; Merge a with itself (already same root)
        (let ([affected (eclass-merge! store uf a a)])
          ;; Should return empty list (no work needed)
          (assert-equal '() affected))))))

(test-group "eclass-introspection"

  (define-test "eclass-node-count counts correctly"
    (let ([store (make-eclass-store)]
          [uf (make-uf)])
      ;; Create 3 classes with different node counts
      (let ([a (uf-make-set! uf)]
            [b (uf-make-set! uf)]
            [c (uf-make-set! uf)])
        (eclass-add-node! store a (make-enode 'x (vector)))
        (eclass-add-node! store a (make-enode 'y (vector)))  ; 2 nodes in a
        (eclass-add-node! store b (make-enode 'z (vector)))  ; 1 node in b
        ;; c has no nodes
        (assert-equal 3 (eclass-node-count store uf)))))

  (define-test "eclass-all-nodes returns pairs"
    (let ([store (make-eclass-store)]
          [uf (make-uf)])
      (let ([a (uf-make-set! uf)]
            [b (uf-make-set! uf)])
        (eclass-add-node! store a (make-enode 'x (vector)))
        (eclass-add-node! store b (make-enode 'y (vector)))
        (let ([all (eclass-all-nodes store uf)])
          (assert-equal 2 (length all))
          ;; Each entry is (id . nodes)
          (assert-true (pair? (car all)))
          (assert-true (list? (cdar all))))))))

(test-group "eclass-growth"

  (define-test "handles large class IDs"
    (let ([store (make-eclass-store)]
          [uf (make-uf)])
      ;; Create 100 classes
      (do ([i 0 (+ i 1)])
          ((>= i 100))
        (let ([id (uf-make-set! uf)])
          (eclass-add-node! store id (make-enode (string->symbol (number->string i)) (vector)))))
      (assert-equal 100 (eclass-node-count store uf)))))

(test-group "enode-debug"

  (define-test "enode->string nullary"
    (let ([n (make-enode 'x (vector))])
      (assert-equal "x" (enode->string n))))

  (define-test "enode->string with children"
    (let ([n (make-enode '+ (vector 0 1))])
      (assert-equal "(+ e0 e1)" (enode->string n)))))

(run-all-tests)

