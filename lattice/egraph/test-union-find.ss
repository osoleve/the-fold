;;; lattice/egraph/test-union-find.ss — Tests for Union-Find
;;;
;;; Run with: scheme --script lattice/egraph/test-union-find.ss

(load "core/testing/test-framework.ss")
(load "lattice/egraph/union-find.ss")

(display "\n====\n")
(display "Union-Find Tests\n")
(display "====\n\n")

(test-group "union-find-basic"

  (define-test "make-uf creates empty structure"
    (let ([uf (make-uf)])
      (assert-true (uf? uf))
      (assert-equal 0 (uf-size uf))
      (assert-equal 0 (uf-count uf))))

  (define-test "make-set creates singleton"
    (let ([uf (make-uf)])
      (let ([id (uf-make-set! uf)])
        (assert-equal 0 id)
        (assert-equal 1 (uf-size uf))
        (assert-equal 1 (uf-count uf))
        (assert-equal id (uf-find uf id)))))

  (define-test "multiple make-set creates distinct sets"
    (let ([uf (make-uf)])
      (let ([a (uf-make-set! uf)]
            [b (uf-make-set! uf)]
            [c (uf-make-set! uf)])
        (assert-equal 3 (uf-size uf))
        (assert-equal 3 (uf-count uf))
        (assert-false (uf-same-set? uf a b))
        (assert-false (uf-same-set? uf b c))
        (assert-false (uf-same-set? uf a c)))))

  (define-test "find returns self for singleton"
    (let ([uf (make-uf)])
      (let ([id (uf-make-set! uf)])
        (assert-equal id (uf-find uf id))))))

(test-group "union-find-union"

  (define-test "union merges two sets"
    (let ([uf (make-uf)])
      (let ([a (uf-make-set! uf)]
            [b (uf-make-set! uf)])
        (assert-false (uf-same-set? uf a b))
        (uf-union! uf a b)
        (assert-true (uf-same-set? uf a b))
        (assert-equal 2 (uf-size uf))
        (assert-equal 1 (uf-count uf)))))

  (define-test "union returns merged root"
    (let ([uf (make-uf)])
      (let ([a (uf-make-set! uf)]
            [b (uf-make-set! uf)])
        (let ([root (uf-union! uf a b)])
          (assert-equal root (uf-find uf a))
          (assert-equal root (uf-find uf b))))))

  (define-test "union is idempotent"
    (let ([uf (make-uf)])
      (let ([a (uf-make-set! uf)]
            [b (uf-make-set! uf)])
        (uf-union! uf a b)
        (uf-union! uf a b)  ; Again
        (uf-union! uf b a)  ; Reversed
        (assert-equal 1 (uf-count uf))
        (assert-true (uf-same-set? uf a b)))))

  (define-test "transitive union"
    (let ([uf (make-uf)])
      (let ([a (uf-make-set! uf)]
            [b (uf-make-set! uf)]
            [c (uf-make-set! uf)])
        (uf-union! uf a b)
        (uf-union! uf b c)
        (assert-true (uf-same-set? uf a c))
        (assert-equal 1 (uf-count uf))))))

(test-group "union-find-path-compression"

  (define-test "find applies path compression"
    (let ([uf (make-uf)])
      ;; Create chain: 0 <- 1 <- 2 <- 3
      (let ([a (uf-make-set! uf)]
            [b (uf-make-set! uf)]
            [c (uf-make-set! uf)]
            [d (uf-make-set! uf)])
        (uf-union! uf a b)
        (uf-union! uf b c)
        (uf-union! uf c d)
        ;; After find, all should point to same root
        (let ([root (uf-find uf d)])
          (assert-equal root (uf-find uf a))
          (assert-equal root (uf-find uf b))
          (assert-equal root (uf-find uf c))
          (assert-equal root (uf-find uf d)))))))

(test-group "union-find-introspection"

  (define-test "uf-roots returns all roots"
    (let ([uf (make-uf)])
      (let ([a (uf-make-set! uf)]
            [b (uf-make-set! uf)]
            [c (uf-make-set! uf)])
        (assert-equal 3 (length (uf-roots uf)))
        (uf-union! uf a b)
        (assert-equal 2 (length (uf-roots uf)))
        (uf-union! uf b c)
        (assert-equal 1 (length (uf-roots uf))))))

  (define-test "uf-set-members returns all members"
    (let ([uf (make-uf)])
      (let ([a (uf-make-set! uf)]
            [b (uf-make-set! uf)]
            [c (uf-make-set! uf)])
        (uf-union! uf a b)
        (let ([members (uf-set-members uf a)])
          (assert-equal 2 (length members))
          (assert-true (pair? (memv a members)))
          (assert-true (pair? (memv b members)))
          (assert-false (memv c members))))))

  (define-test "uf-set-size returns correct size"
    (let ([uf (make-uf)])
      (let ([a (uf-make-set! uf)]
            [b (uf-make-set! uf)]
            [c (uf-make-set! uf)])
        (assert-equal 1 (uf-set-size uf a))
        (uf-union! uf a b)
        (assert-equal 2 (uf-set-size uf a))
        (assert-equal 2 (uf-set-size uf b))
        (assert-equal 1 (uf-set-size uf c)))))

  (define-test "uf-all-sets partitions elements"
    (let ([uf (make-uf)])
      (let ([a (uf-make-set! uf)]
            [b (uf-make-set! uf)]
            [c (uf-make-set! uf)]
            [d (uf-make-set! uf)])
        (uf-union! uf a b)
        (uf-union! uf c d)
        (let ([sets (uf-all-sets uf)])
          (assert-equal 2 (length sets))
          ;; Total elements across all sets = 4
          (assert-equal 4 (apply + (map length sets))))))))

(test-group "union-find-growth"

  (define-test "auto-grows past initial capacity"
    (let ([uf (make-uf)])
      ;; Create more than initial 64 elements
      (do ([i 0 (+ i 1)])
          ((>= i 100))
        (uf-make-set! uf))
      (assert-equal 100 (uf-size uf))
      (assert-equal 100 (uf-count uf))
      ;; All should be distinct
      (assert-false (uf-same-set? uf 0 99))))

  (define-test "works with large unions"
    (let ([uf (make-uf)])
      ;; Create 1000 elements
      (do ([i 0 (+ i 1)])
          ((>= i 1000))
        (uf-make-set! uf))
      ;; Union all into one set
      (do ([i 1 (+ i 1)])
          ((>= i 1000))
        (uf-union! uf 0 i))
      (assert-equal 1 (uf-count uf))
      (assert-equal 1000 (uf-set-size uf 0)))))

(run-all-tests)
