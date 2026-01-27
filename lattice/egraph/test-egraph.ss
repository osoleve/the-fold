;;; lattice/egraph/test-egraph.ss — Tests for E-Graph
;;;
;;; Run with: scheme --script lattice/egraph/test-egraph.ss

(load "core/testing/test-framework.ss")
(load "lattice/egraph/egraph.ss")

(display "\n====\n")
(display "E-Graph Tests\n")
(display "====\n\n")

(test-group "egraph-basic"

  (define-test "make-egraph creates empty egraph"
    (let ([eg (make-egraph)])
      (assert-true (egraph? eg))
      (assert-equal 0 (egraph-class-count eg))
      (assert-equal 0 (egraph-node-count eg))))

  (define-test "egraph-add-enode! creates new class"
    (let ([eg (make-egraph)]
          [n (make-enode 'x (vector))])
      (let ([id (egraph-add-enode! eg n)])
        (assert-equal 0 id)
        (assert-equal 1 (egraph-class-count eg))
        (assert-equal 1 (egraph-node-count eg)))))

  (define-test "egraph-add-enode! deduplicates via hashcons"
    (let ([eg (make-egraph)]
          [n (make-enode 'x (vector))])
      (let ([id1 (egraph-add-enode! eg n)]
            [id2 (egraph-add-enode! eg n)])
        (assert-equal id1 id2)
        (assert-equal 1 (egraph-class-count eg)))))

  (define-test "egraph-lookup finds existing nodes"
    (let ([eg (make-egraph)]
          [n (make-enode 'y (vector))])
      (assert-false (egraph-lookup eg n))
      (let ([id (egraph-add-enode! eg n)])
        (assert-equal id (egraph-lookup eg n))))))

(test-group "egraph-terms"

  (define-test "egraph-add-term! handles symbols"
    (let ([eg (make-egraph)])
      (let ([id (egraph-add-term! eg 'x)])
        (assert-equal 1 (egraph-class-count eg)))))

  (define-test "egraph-add-term! handles numbers"
    (let ([eg (make-egraph)])
      (let ([id (egraph-add-term! eg 42)])
        (assert-equal 1 (egraph-class-count eg)))))

  (define-test "egraph-add-term! handles applications"
    (let ([eg (make-egraph)])
      (let ([id (egraph-add-term! eg '(+ x y))])
        ;; Creates 3 classes: x, y, (+ x y)
        (assert-equal 3 (egraph-class-count eg)))))

  (define-test "egraph-add-term! shares subterms"
    (let ([eg (make-egraph)])
      ;; (+ x x) should create only 2 classes: x and (+ x x)
      (let ([id (egraph-add-term! eg '(+ x x))])
        (assert-equal 2 (egraph-class-count eg)))))

  (define-test "egraph-add-term! handles nested terms"
    (let ([eg (make-egraph)])
      ;; (+ (* a b) c) has 5 distinct subterms: a, b, c, (* a b), (+ (* a b) c)
      (let ([id (egraph-add-term! eg '(+ (* a b) c))])
        (assert-equal 5 (egraph-class-count eg))))))

(test-group "egraph-literals"

  (define-test "egraph-add-term! handles strings"
    (let ([eg (make-egraph)])
      (let ([id (egraph-add-term! eg "hello")])
        (assert-equal 1 (egraph-class-count eg)))))

  (define-test "egraph-add-term! distinguishes different strings"
    (let ([eg (make-egraph)])
      (let ([id1 (egraph-add-term! eg "hello")]
            [id2 (egraph-add-term! eg "world")])
        (assert-equal 2 (egraph-class-count eg))
        (assert-true (not (= (egraph-find eg id1) (egraph-find eg id2)))))))

  (define-test "egraph-add-term! deduplicates same string"
    (let ([eg (make-egraph)])
      (let ([id1 (egraph-add-term! eg "hello")]
            [id2 (egraph-add-term! eg "hello")])
        (assert-equal 1 (egraph-class-count eg))
        (assert-equal (egraph-find eg id1) (egraph-find eg id2)))))

  (define-test "egraph-add-term! handles booleans"
    (let ([eg (make-egraph)])
      (let ([id1 (egraph-add-term! eg #t)]
            [id2 (egraph-add-term! eg #f)])
        (assert-equal 2 (egraph-class-count eg))
        (assert-true (not (= (egraph-find eg id1) (egraph-find eg id2)))))))

  (define-test "egraph-add-term! handles characters"
    (let ([eg (make-egraph)])
      (let ([id1 (egraph-add-term! eg #\a)]
            [id2 (egraph-add-term! eg #\b)]
            [id3 (egraph-add-term! eg #\a)])
        (assert-equal 2 (egraph-class-count eg))
        (assert-equal (egraph-find eg id1) (egraph-find eg id3)))))

  (define-test "egraph-add-term! handles mixed literals in expressions"
    (let ([eg (make-egraph)])
      ;; (concat "hello" " " "world") has 4 classes
      (let ([id (egraph-add-term! eg '(concat "hello" " " "world"))])
        (assert-equal 4 (egraph-class-count eg))))))

(test-group "egraph-merge"

  (define-test "egraph-merge! merges two classes"
    (let ([eg (make-egraph)])
      (let ([id1 (egraph-add-term! eg 'x)]
            [id2 (egraph-add-term! eg 'y)])
        (assert-equal 2 (egraph-class-count eg))
        (egraph-merge! eg id1 id2)
        (assert-equal 1 (egraph-class-count eg))
        (assert-equal (egraph-find eg id1) (egraph-find eg id2)))))

  (define-test "egraph-merge! is idempotent"
    (let ([eg (make-egraph)])
      (let ([id1 (egraph-add-term! eg 'x)]
            [id2 (egraph-add-term! eg 'y)])
        (egraph-merge! eg id1 id2)
        (egraph-merge! eg id1 id2)  ; Again
        (assert-equal 1 (egraph-class-count eg)))))

  (define-test "egraph-merge! marks parents dirty"
    (let ([eg (make-egraph)])
      ;; Add (f x) and (f y)
      (let ([fx (egraph-add-term! eg '(f x))]
            [fy (egraph-add-term! eg '(f y))]
            [x (egraph-add-term! eg 'x)]
            [y (egraph-add-term! eg 'y)])
        ;; Merge x and y
        (egraph-merge! eg x y)
        ;; Dirty list should contain something
        (assert-true (> (length (egraph-dirty eg)) 0))))))

(test-group "egraph-rebuild"

  (define-test "egraph-rebuild! processes dirty classes"
    (let ([eg (make-egraph)])
      (let ([fx (egraph-add-term! eg '(f x))]
            [fy (egraph-add-term! eg '(f y))]
            [x (egraph-add-term! eg 'x)]
            [y (egraph-add-term! eg 'y)])
        ;; Before merge: 4 classes (f, x, y, (f x), (f y) but f is shared)
        ;; Actually: x, y, (f x), (f y) = 4 classes
        (egraph-merge! eg x y)
        (let ([processed (egraph-saturate-rebuild! eg)])
          ;; After rebuild: x=y, so (f x) = (f y), so 2 classes
          ;; x/y merged, (f x)/(f y) should merge
          (assert-true (> processed 0))
          (assert-equal (egraph-find eg fx) (egraph-find eg fy))))))

  (define-test "egraph-rebuild! processes non-root dirty IDs"
    ;; Regression test: dirty IDs that aren't roots must still trigger rebuild
    ;; of their root class. Previously, non-root dirty IDs were skipped.
    (let ([eg (make-egraph)])
      ;; Create (f a), (f b), (f c)
      (let ([fa (egraph-add-term! eg '(f a))]
            [fb (egraph-add-term! eg '(f b))]
            [fc (egraph-add-term! eg '(f c))]
            [a (egraph-add-term! eg 'a)]
            [b (egraph-add-term! eg 'b)]
            [c (egraph-add-term! eg 'c)])
        ;; Merge a=b, then b=c (b becomes non-root after first merge)
        (egraph-merge! eg a b)
        (egraph-merge! eg b c)
        (egraph-saturate-rebuild! eg)
        ;; All three (f _) terms should be equivalent
        (assert-equal (egraph-find eg fa) (egraph-find eg fb))
        (assert-equal (egraph-find eg fb) (egraph-find eg fc)))))

  (define-test "egraph-rebuild! handles chain merges"
    (let ([eg (make-egraph)])
      ;; (g (f x)) and (g (f y))
      (let ([gfx (egraph-add-term! eg '(g (f x)))]
            [gfy (egraph-add-term! eg '(g (f y)))]
            [x (egraph-add-term! eg 'x)]
            [y (egraph-add-term! eg 'y)])
        (egraph-merge! eg x y)
        (egraph-saturate-rebuild! eg)
        ;; Should propagate: x=y → (f x)=(f y) → (g (f x))=(g (f y))
        (assert-equal (egraph-find eg gfx) (egraph-find eg gfy))))))

(test-group "egraph-introspection"

  (define-test "egraph-class-nodes returns nodes in class"
    (let ([eg (make-egraph)])
      (let ([id (egraph-add-term! eg 'x)])
        (let ([nodes (egraph-class-nodes eg id)])
          (assert-equal 1 (length nodes))
          (assert-equal 'x (enode-op (car nodes)))))))

  (define-test "egraph-stats-report returns alist"
    (let ([eg (make-egraph)])
      (egraph-add-term! eg '(+ x y))
      (let ([stats (egraph-stats-report eg)])
        (assert-true (pair? (assq 'classes stats)))
        (assert-true (pair? (assq 'nodes stats)))
        (assert-true (pair? (assq 'adds stats))))))

  (define-test "egraph-class-count accurate after merges"
    (let ([eg (make-egraph)])
      (let ([a (egraph-add-term! eg 'a)]
            [b (egraph-add-term! eg 'b)]
            [c (egraph-add-term! eg 'c)])
        (assert-equal 3 (egraph-class-count eg))
        (egraph-merge! eg a b)
        (assert-equal 2 (egraph-class-count eg))
        (egraph-merge! eg b c)
        (assert-equal 1 (egraph-class-count eg))))))

(test-group "egraph-congruence"

  (define-test "congruence closure basic"
    ;; If x = y, then f(x) = f(y)
    (let ([eg (make-egraph)])
      (let ([fx (egraph-add-term! eg '(f x))]
            [fy (egraph-add-term! eg '(f y))])
        ;; Before: f(x) and f(y) are different
        (assert-true (not (= (egraph-find eg fx) (egraph-find eg fy))))
        ;; Merge x and y
        (let ([x (egraph-add-term! eg 'x)]
              [y (egraph-add-term! eg 'y)])
          (egraph-merge! eg x y)
          (egraph-saturate-rebuild! eg)
          ;; After: f(x) = f(y)
          (assert-equal (egraph-find eg fx) (egraph-find eg fy))))))

  (define-test "congruence closure nested"
    ;; If a = b, then g(f(a)) = g(f(b))
    (let ([eg (make-egraph)])
      (let ([gfa (egraph-add-term! eg '(g (f a)))]
            [gfb (egraph-add-term! eg '(g (f b)))]
            [a (egraph-add-term! eg 'a)]
            [b (egraph-add-term! eg 'b)])
        (egraph-merge! eg a b)
        (egraph-saturate-rebuild! eg)
        (assert-equal (egraph-find eg gfa) (egraph-find eg gfb)))))

  (define-test "congruence with multiple args"
    ;; If x = y, then (+ x z) = (+ y z)
    (let ([eg (make-egraph)])
      (let ([pxz (egraph-add-term! eg '(+ x z))]
            [pyz (egraph-add-term! eg '(+ y z))]
            [x (egraph-add-term! eg 'x)]
            [y (egraph-add-term! eg 'y)])
        (egraph-merge! eg x y)
        (egraph-saturate-rebuild! eg)
        (assert-equal (egraph-find eg pxz) (egraph-find eg pyz))))))

(test-group "egraph-hashcons-integrity"

  (define-test "hashcons survives rebuild"
    (let ([eg (make-egraph)])
      (let ([t1 (egraph-add-term! eg '(f x))]
            [x (egraph-add-term! eg 'x)]
            [y (egraph-add-term! eg 'y)])
        (egraph-merge! eg x y)
        (egraph-saturate-rebuild! eg)
        ;; After rebuild, (f x) should still be findable
        ;; And adding (f y) should find the same class
        (let ([t2 (egraph-add-term! eg '(f y))])
          (assert-equal (egraph-find eg t1) (egraph-find eg t2))))))

  (define-test "hashcons hit counter works"
    (let ([eg (make-egraph)])
      (egraph-add-term! eg 'x)
      (assert-equal 0 (egraph-stat-hits eg))
      (egraph-add-term! eg 'x)  ; Should hit
      (assert-equal 1 (egraph-stat-hits eg)))))

(run-all-tests)

