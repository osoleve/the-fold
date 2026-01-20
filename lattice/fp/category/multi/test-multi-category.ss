;;; test-multi-category.ss
;;; Test suite for multi-category framework
;;;
;;; Tests:
;;;   1. Category construction and operations
;;;   2. General functors (inter-category)
;;;   3. Indexed natural transformations
;;;   4. Inter-category adjunctions
;;;   5. Effect adjunctions with correct counit evaluation
;;;   6. Triangle identity verification

(load "core/testing/test-framework.ss")
(load "lattice/fp/category/multi/effect-adjunction.ss")

(test-group "multi-category"

  ;;; ====
  ;;; Category Tests
  ;;; ====

  (test-group "category"

    (define-test "cat-Set is a valid category"
      (assert-true (category? cat-Set)))

    (define-test "cat-Set has correct name"
      (assert-equal 'Set (category-name cat-Set)))

    (define-test "cat-Set identity morphism"
      (let ([id-mor (cat-id cat-Set 'any-obj)])
        (assert-equal 42 (id-mor 42))
        (assert-equal "hello" (id-mor "hello"))))

    (define-test "cat-Set composition"
      (let* ([f (lambda (x) (* x 2))]
             [g (lambda (x) (+ x 1))]
             [g-f (cat-compose cat-Set g f)])
        ;; g(f(5)) = g(10) = 11
        (assert-equal 11 (g-f 5))))

    (define-test "cat-Alg for monoid is a category"
      (let ([monoid-cat (cat-Alg sig-monoid)])
        (assert-true (category? monoid-cat))
        (assert-equal 'monoid-Alg (category-name monoid-cat))))

    (define-test "verify-category-identity passes for Set"
      (assert-true
       (verify-category-identity cat-Set 'any (lambda (x) (* x 2)) 5)))

    (define-test "verify-category-associativity passes for Set"
      (let ([f (lambda (x) (* x 2))]
            [g (lambda (x) (+ x 1))]
            [h (lambda (x) (- x 3))])
        (assert-true
         (verify-category-associativity cat-Set h g f 5)))))

  ;;; ====
  ;;; Functor General Tests
  ;;; ====

  (test-group "functor-general"

    (define-test "identity functor construction"
      (let ([id-Set (id-functor-for cat-Set)])
        (assert-true (functor-general? id-Set))
        (assert-equal 'Id-Set (functor-general-name id-Set))))

    (define-test "identity functor preserves objects"
      (let ([id-Set (id-functor-for cat-Set)])
        (assert-equal 'foo (functor-apply-obj id-Set 'foo))))

    (define-test "identity functor preserves morphisms"
      (let* ([id-Set (id-functor-for cat-Set)]
             [f (lambda (x) (* x 2))]
             [id-f (functor-apply-mor id-Set f)])
        (assert-equal 10 (id-f 5))))

    (define-test "functor composition"
      (let* ([id-Set (id-functor-for cat-Set)]
             [id-id (functor-compose-general id-Set id-Set)])
        (assert-true (functor-general? id-id))))

    (define-test "free functor construction"
      (let ([Free-monoid (make-free-functor-gen sig-monoid)])
        (assert-true (functor-general? Free-monoid))
        (assert-equal 'Free-monoid (functor-general-name Free-monoid))))

    (define-test "forgetful functor construction"
      (let ([U-monoid (make-forgetful-functor-gen sig-monoid)])
        (assert-true (functor-general? U-monoid))
        (assert-equal 'U-monoid (functor-general-name U-monoid))))

    (define-test "free-fmap-term maps over generators"
      (let ([term '(* (gen 1) (gen 2))]
            [f (lambda (x) (* x 10))])
        (assert-equal '(* (gen 10) (gen 20))
                      (free-fmap-term sig-monoid f term)))))

  ;;; ====
  ;;; Indexed Natural Transformation Tests
  ;;; ====

  (test-group "nat-indexed"

    (define-test "indexed identity transformation"
      (let* ([F (id-functor-for cat-Set)]
             [id-nat (nat-indexed-id F)])
        (assert-true (nat-indexed? id-nat))
        (assert-equal 'id (nat-indexed-name id-nat))))

    (define-test "nat-indexed-at returns function"
      (let* ([F (id-functor-for cat-Set)]
             [id-nat (nat-indexed-id F)]
             [component (nat-indexed-at id-nat 'any)])
        (assert-true (procedure? component))
        (assert-equal 42 (component 42))))

    (define-test "nat-indexed-apply works"
      (let* ([F (id-functor-for cat-Set)]
             [id-nat (nat-indexed-id F)])
        (assert-equal 42 (nat-indexed-apply id-nat 'any 42))))

    (define-test "vertical composition"
      (let* ([F (id-functor-for cat-Set)]
             [η1 (nat-indexed-id F)]
             [η2 (nat-indexed-id F)]
             [composed (nat-indexed-compose η2 η1)])
        (assert-true (nat-indexed? composed))
        (assert-equal 42 (nat-indexed-apply composed 'any 42))))

    (define-test "custom indexed transformation"
      (let* ([F (id-functor-for cat-Set)]
             ;; Transform that uses the object index
             [η (make-nat-indexed
                 'double-at
                 F F
                 ;; component-at(n) returns x -> x * n
                 (lambda (n) (lambda (x) (* x n))))])
        (assert-equal 10 (nat-indexed-apply η 2 5))
        (assert-equal 15 (nat-indexed-apply η 3 5)))))

  ;;; ====
  ;;; Adjunction Inter Tests
  ;;; ====

  (test-group "adjunction-inter"

    (define-test "adjunction construction"
      (let ([adj adj-monoid-gen])
        (assert-true (adjunction-inter? adj))))

    (define-test "unit embeds as generator"
      (let ([adj adj-monoid-gen])
        ;; η_X(x) = (gen x)
        (assert-equal '(gen 42)
                      (apply-unit adj 'carrier 42))))

    (define-test "counit evaluates in algebra - sum monoid"
      (let ([adj adj-monoid-gen])
        ;; ε_A evaluates term in algebra
        ;; For sum monoid: (* (gen 1) (gen 2)) should give 3
        (assert-equal 3
                      (apply-counit adj alg-sum-monoid
                                    '(* (gen 1) (gen 2))))))

    (define-test "counit evaluates in algebra - list monoid"
      (let ([adj adj-monoid-gen])
        ;; For list monoid: (* (gen (1)) (gen (2))) should give (1 2)
        (assert-equal '(1 2)
                      (apply-counit adj alg-list-monoid
                                    '(* (gen (1)) (gen (2)))))))

    (define-test "counit evaluates in algebra - product monoid"
      (let ([adj adj-monoid-gen])
        ;; For product monoid: (* (gen 3) (gen 4)) should give 12
        (assert-equal 12
                      (apply-counit adj alg-product-monoid
                                    '(* (gen 3) (gen 4))))))

    (define-test "counit handles identity element"
      (let ([adj adj-monoid-gen])
        ;; Identity in sum monoid is 0
        (assert-equal 0 (apply-counit adj alg-sum-monoid 'e))
        ;; Identity in product monoid is 1
        (assert-equal 1 (apply-counit adj alg-product-monoid 'e))))

    (define-test "counit handles nested operations"
      (let ([adj adj-monoid-gen])
        ;; (* (* (gen 1) (gen 2)) (gen 3)) in sum = 6
        (assert-equal 6
                      (apply-counit adj alg-sum-monoid
                                    '(* (* (gen 1) (gen 2)) (gen 3)))))))

  ;;; ====
  ;;; Triangle Identities
  ;;; ====

  (test-group "triangle-identities"

    (define-test "right triangle: gen then eval = id"
      ;; Starting with value x in carrier
      ;; η(x) = (gen x)
      ;; ε((gen x)) = x (in any algebra, gen evaluates to self)
      (assert-true
       (verify-effect-triangle-right sig-monoid alg-sum-monoid 42)))

    (define-test "right triangle works for list monoid"
      (assert-true
       (verify-effect-triangle-right sig-monoid alg-list-monoid '(a b c))))

    (define-test "right triangle works for product monoid"
      (assert-true
       (verify-effect-triangle-right sig-monoid alg-product-monoid 7))))

  ;;; ====
  ;;; Handle via Counit
  ;;; ====

  (test-group "handle-via-counit"

    (define-test "handle-via-counit evaluates correctly"
      (let ([adj adj-monoid-gen]
            [term '(* (gen 10) (* (gen 20) (gen 30)))])
        ;; In sum monoid: 10 + 20 + 30 = 60
        (assert-equal 60
                      (handle-via-counit adj alg-sum-monoid term))))

    (define-test "run-with-algebra convenience function"
      (let ([term '(* (gen 2) (gen 3))])
        ;; Product monoid: 2 * 3 = 6
        (assert-equal 6
                      (run-with-algebra sig-monoid alg-product-monoid term))))

    (define-test "fold-free applies function to generators"
      ;; Fold with f that squares each generator
      (let ([term '(* (gen 2) (gen 3))]
            [f (lambda (x) (* x x))])
        ;; After f: (* (gen 4) (gen 9))
        ;; In sum monoid: 4 + 9 = 13
        (assert-equal 13
                      (fold-free sig-monoid alg-sum-monoid f term)))))

  ;;; ====
  ;;; Group Tests
  ;;; ====

  (test-group "group-algebra"

    (define-test "group adjunction works"
      (let ([adj adj-group-gen])
        (assert-true (adjunction-inter? adj))))

    (define-test "inverse evaluation"
      (let ([adj adj-group-gen])
        ;; (inv (gen 5)) in integer group = -5
        (assert-equal -5
                      (apply-counit adj alg-integer-group
                                    '(inv (gen 5))))))

    (define-test "group identity"
      (let ([adj adj-group-gen])
        ;; e in integer group = 0
        (assert-equal 0
                      (apply-counit adj alg-integer-group 'e))))

    (define-test "complex group expression"
      (let ([adj adj-group-gen])
        ;; (* (gen 10) (* (inv (gen 3)) (gen 5))) = 10 + (-3) + 5 = 12
        (assert-equal 12
                      (apply-counit adj alg-integer-group
                                    '(* (gen 10) (* (inv (gen 3)) (gen 5))))))))

  ;;; ====
  ;;; Magma Tests (no laws)
  ;;; ====

  (test-group "magma"

    (define-test "magma adjunction works"
      (let ([adj adj-magma-gen])
        (assert-true (adjunction-inter? adj))))

    (define-test "magma evaluation with max"
      ;; Define a custom magma algebra for testing
      (let ([alg-max-magma
             (make-algebra sig-magma 'number
                           `((* . ,max)))]
            [adj adj-magma-gen])
        ;; (* (gen 3) (gen 7)) with max = 7
        (assert-equal 7
                      (apply-counit adj alg-max-magma
                                    '(* (gen 3) (gen 7)))))))

  ) ; end test-group multi-category

;;; Run tests
(run-all-tests)
