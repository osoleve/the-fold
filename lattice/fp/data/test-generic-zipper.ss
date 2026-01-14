;;; lattice/fp/data/test-generic-zipper.ss — Tests for Generic Zipper Derivation
;;;
;;; Tests the type derivative computations and verifies that derived
;;; context types match hand-written zipper implementations.
;;;
;;; Dependencies:
;;;   - core/testing/test-framework.ss
;;;   - lattice/fp/data/generic-zipper.ss

(load "core/testing/test-framework.ss")
(load "lattice/fp/data/generic-zipper.ss")

;;; ====
;;; Type Constructor Tests
;;; ====

(test-group "type-constructors"

  (define-test "type-zero is 0"
    (assert-equal 0 type-zero))

  (define-test "type-one is 1"
    (assert-equal 1 type-one))

  (define-test "type-var creates variable"
    (assert-equal '(var a) (type-var 'a)))

  (define-test "type-sum creates sum"
    (assert-equal '(+ (var a) (var b))
                  (type-sum (type-var 'a) (type-var 'b))))

  (define-test "type-prod creates product"
    (assert-equal '(* (var a) (var b))
                  (type-prod (type-var 'a) (type-var 'b))))

  (define-test "type-sum with single element returns element"
    (assert-equal (type-var 'a)
                  (type-sum (type-var 'a))))

  (define-test "type-prod with single element returns element"
    (assert-equal (type-var 'a)
                  (type-prod (type-var 'a))))

  (define-test "type-sum with no elements returns zero"
    (assert-equal type-zero (type-sum)))

  (define-test "type-prod with no elements returns one"
    (assert-equal type-one (type-prod)))

  (define-test "type-rec creates recursive type"
    (assert-equal '(mu L (+ 1 (var L)))
                  (type-rec 'L (type-sum type-one (type-var 'L)))))

  (define-test "type-list creates list type"
    (assert-equal '(mu L (+ 1 (* (var a) (var L))))
                  (type-list (type-var 'a))))

  (define-test "type-maybe creates maybe type"
    (assert-equal '(+ 1 (var a))
                  (type-maybe (type-var 'a))))

  (define-test "type-binary-tree creates binary tree type"
    (assert-equal '(mu T (+ 1 (* (var a) (var T) (var T))))
                  (type-binary-tree (type-var 'a)))))

;;; ====
;;; Type Predicates Tests
;;; ====

(test-group "type-predicates"

  (define-test "type-zero? recognizes zero"
    (assert-true (type-zero? type-zero))
    (assert-false (type-zero? type-one)))

  (define-test "type-one? recognizes one"
    (assert-true (type-one? type-one))
    (assert-false (type-one? type-zero)))

  (define-test "type-var? recognizes variable"
    (assert-true (type-var? (type-var 'a)))
    (assert-false (type-var? type-one)))

  (define-test "type-sum? recognizes sum"
    (assert-true (type-sum? (type-sum (type-var 'a) (type-var 'b))))
    (assert-false (type-sum? (type-prod (type-var 'a) (type-var 'b)))))

  (define-test "type-prod? recognizes product"
    (assert-true (type-prod? (type-prod (type-var 'a) (type-var 'b))))
    (assert-false (type-prod? (type-sum (type-var 'a) (type-var 'b)))))

  (define-test "type-rec? recognizes recursive type"
    (assert-true (type-rec? (type-list (type-var 'a))))
    (assert-false (type-rec? (type-var 'a)))))

;;; ====
;;; Derivative Tests - Basic Rules
;;; ====

(test-group "derivative-basic"

  (define-test "d(0)/da = 0"
    (assert-equal type-zero
                  (type-deriv type-zero 'a)))

  (define-test "d(1)/da = 0"
    (assert-equal type-zero
                  (type-deriv type-one 'a)))

  (define-test "d(a)/da = 1"
    (assert-equal type-one
                  (type-deriv (type-var 'a) 'a)))

  (define-test "d(b)/da = 0 when b != a"
    (assert-equal type-zero
                  (type-deriv (type-var 'b) 'a)))

  (define-test "d(a + b)/da = 1 + 0"
    (let ([result (type-deriv (type-sum (type-var 'a) (type-var 'b)) 'a)])
      (assert-true (type-sum? result))
      (assert-equal 2 (length (type-sum-components result))))))

;;; ====
;;; Derivative Tests - Product Rule
;;; ====

(test-group "derivative-product"

  (define-test "d(a × b)/da simplifies to b"
    (let ([result (type-simplify
                    (type-deriv
                      (type-prod (type-var 'a) (type-var 'b))
                      'a))])
      (assert-equal (type-var 'b) result)))

  (define-test "d(b × a)/da simplifies to b"
    (let ([result (type-simplify
                    (type-deriv
                      (type-prod (type-var 'b) (type-var 'a))
                      'a))])
      (assert-equal (type-var 'b) result)))

  (define-test "d(a × a)/da is (a + a)"
    (let ([result (type-simplify
                    (type-deriv
                      (type-prod (type-var 'a) (type-var 'a))
                      'a))])
      (assert-true (type-sum? result))
      ;; Both components should be 'a'
      (assert-equal (type-var 'a) (car (type-sum-components result)))
      (assert-equal (type-var 'a) (cadr (type-sum-components result)))))

  (define-test "d(a × b × c)/da gives (1×b×c + a×0×c + a×b×0)"
    (let* ([abc (type-prod (type-var 'a) (type-var 'b) (type-var 'c))]
           [result (type-deriv abc 'a)]
           [simplified (type-simplify result)])
      ;; After simplification should be (b × c)
      (assert-true (type-prod? simplified))
      (assert-equal 2 (length (type-prod-components simplified))))))

;;; ====
;;; Derivative Tests - List and Tree
;;; ====

(test-group "derivative-recursive"

  (define-test "d(List a)/da is List × List structure"
    (let* ([list-a (type-list (type-var 'a))]
           [deriv (type-simplify (type-deriv list-a 'a))])
      ;; The derivative should be a product (context × focus-position)
      (assert-true (type-prod? deriv))
      ;; Both components should be the list type
      (let ([comps (type-prod-components deriv)])
        (assert-equal 2 (length comps))
        ;; Each should be μL.(1 + a×L)
        (assert-true (type-rec? (car comps)))
        (assert-true (type-rec? (cadr comps))))))

  (define-test "d(Maybe a)/da is 1"
    (let* ([maybe-a (type-maybe (type-var 'a))]
           [deriv (type-simplify (type-deriv maybe-a 'a))])
      (assert-equal type-one deriv)))

  (define-test "d(a × a)/da gives a + a (two positions)"
    (let* ([pair-aa (type-prod (type-var 'a) (type-var 'a))]
           [deriv (type-simplify (type-deriv pair-aa 'a))])
      (assert-true (type-sum? deriv))
      ;; Two possible hole positions
      (assert-equal 2 (length (type-sum-components deriv))))))

;;; ====
;;; Substitution Tests
;;; ====

(test-group "substitution"

  (define-test "substitute variable"
    (assert-equal (type-var 'b)
                  (type-subst (type-var 'a) 'a (type-var 'b))))

  (define-test "substitute in product"
    (let ([result (type-subst (type-prod (type-var 'a) (type-var 'c))
                              'a
                              (type-var 'b))])
      (assert-equal '(* (var b) (var c)) result)))

  (define-test "substitute in sum"
    (let ([result (type-subst (type-sum (type-var 'a) (type-var 'c))
                              'a
                              (type-var 'b))])
      (assert-equal '(+ (var b) (var c)) result)))

  (define-test "don't substitute shadowed variable"
    (let ([result (type-subst (type-rec 'a (type-var 'a))
                              'a
                              (type-var 'b))])
      ;; 'a' is bound by mu, so shouldn't be substituted
      (assert-equal '(mu a (var a)) result)))

  (define-test "substitute non-shadowed in rec body"
    (let ([result (type-subst (type-rec 'x (type-var 'a))
                              'a
                              (type-var 'b))])
      (assert-equal '(mu x (var b)) result))))

;;; ====
;;; Simplification Tests
;;; ====

(test-group "simplification"

  (define-test "0 + a = a"
    (assert-equal (type-var 'a)
                  (type-simplify (type-sum type-zero (type-var 'a)))))

  (define-test "a + 0 = a"
    (assert-equal (type-var 'a)
                  (type-simplify (type-sum (type-var 'a) type-zero))))

  (define-test "0 × a = 0"
    (assert-equal type-zero
                  (type-simplify (type-prod type-zero (type-var 'a)))))

  (define-test "1 × a = a"
    (assert-equal (type-var 'a)
                  (type-simplify (type-prod type-one (type-var 'a)))))

  (define-test "a × 1 = a"
    (assert-equal (type-var 'a)
                  (type-simplify (type-prod (type-var 'a) type-one))))

  (define-test "1 × 1 = 1"
    (assert-equal type-one
                  (type-simplify (type-prod type-one type-one))))

  (define-test "0 + 0 = 0"
    (assert-equal type-zero
                  (type-simplify (type-sum type-zero type-zero))))

  (define-test "nested simplification"
    (let ([complex (type-sum type-zero
                             (type-prod type-one (type-var 'a)))])
      (assert-equal (type-var 'a) (type-simplify complex)))))

;;; ====
;;; Generic Zipper Structure Tests
;;; ====

(test-group "generic-zipper-structure"

  (define-test "make and query generic zipper"
    (let ([z (make-generic-zipper 'focused '(ctx1 ctx2))])
      (assert-true (generic-zipper? z))
      (assert-equal 'focused (generic-zipper-focus z))
      (assert-equal '(ctx1 ctx2) (generic-zipper-contexts z))))

  (define-test "generic-zipper-at-root? with no contexts"
    (let ([z (make-generic-zipper 'x '())])
      (assert-true (generic-zipper-at-root? z))))

  (define-test "generic-zipper-at-root? with contexts"
    (let ([z (make-generic-zipper 'x '(ctx))])
      (assert-false (generic-zipper-at-root? z))))

  (define-test "generic-zipper-depth"
    (assert-equal 0 (generic-zipper-depth (make-generic-zipper 'x '())))
    (assert-equal 2 (generic-zipper-depth (make-generic-zipper 'x '(a b)))))

  (define-test "generic-zipper-set replaces focus"
    (let* ([z (make-generic-zipper 'old '(ctx))]
           [z2 (generic-zipper-set z 'new)])
      (assert-equal 'new (generic-zipper-focus z2))
      (assert-equal '(ctx) (generic-zipper-contexts z2))))

  (define-test "generic-zipper-modify applies function"
    (let* ([z (make-generic-zipper 5 '())]
           [z2 (generic-zipper-modify z (lambda (x) (* x 2)))])
      (assert-equal 10 (generic-zipper-focus z2)))))

;;; ====
;;; Context Frame Tests
;;; ====

(test-group "context-frame"

  (define-test "make and query context frame"
    (let ([f (make-context-frame 'prod 1 '(a #f c))])
      (assert-true (context-frame? f))
      (assert-equal 'prod (context-frame-kind f))
      (assert-equal 1 (context-frame-position f))
      (assert-equal '(a #f c) (context-frame-siblings f))))

  (define-test "sum context frame"
    (let ([f (make-context-frame 'sum 0 '())])
      (assert-equal 'sum (context-frame-kind f))
      (assert-equal 0 (context-frame-position f)))))

;;; ====
;;; Zipper Type Validation
;;; ====
;;;
;;; These tests verify that the computed derivative types match
;;; the hand-crafted zipper context structures.

(test-group "zipper-type-validation"

  (define-test "list zipper context is List × List"
    ;; A list zipper has context (left, right) which are both lists
    (let* ([list-a (type-list (type-var 'a))]
           [ctx-type (context-type list-a 'a)])
      ;; Should be a product of two lists
      (assert-true (type-prod? ctx-type))
      (let ([comps (type-prod-components ctx-type)])
        (assert-equal 2 (length comps))
        ;; Both should be list types (μL. ...)
        (assert-true (type-rec? (car comps)))
        (assert-true (type-rec? (cadr comps))))))

  (define-test "maybe context is unit (no surrounding structure)"
    (let* ([maybe-a (type-maybe (type-var 'a))]
           [ctx-type (context-type maybe-a 'a)])
      (assert-equal type-one ctx-type)))

  (define-test "pair (a, b) context w.r.t. a is b"
    (let* ([pair-ab (type-prod (type-var 'a) (type-var 'b))]
           [ctx-type (context-type pair-ab 'a)])
      (assert-equal (type-var 'b) ctx-type)))

  (define-test "pair (a, a) context is (1 + 1) representing two positions"
    ;; For (a, a), there are two positions where 'a' can be
    (let* ([pair-aa (type-prod (type-var 'a) (type-var 'a))]
           [ctx-type (context-type pair-aa 'a)])
      ;; After simplification: a + a
      (assert-true (type-sum? ctx-type))
      (assert-equal 2 (length (type-sum-components ctx-type))))))

;;; ====
;;; Run All Tests
;;; ====

(run-all-tests)
