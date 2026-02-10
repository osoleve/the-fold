(load "core/lang/module.ss")
(load "core/testing/test-framework.ss")
(load "lattice/fp/category/abstract-interp.ss")

(doc 'module 'test-abstract-interp)
(doc 'description "Tests for abstract interpretation via Galois connections")

;;; ============================================================================
;;; Sign Domain Tests
;;; ============================================================================

(test-group "sign-domain"
  (define-test "sign-alpha maps integers correctly"
    (assert-equal sign-neg (sign-alpha -5))
    (assert-equal sign-zero (sign-alpha 0))
    (assert-equal sign-pos (sign-alpha 42)))

  (define-test "sign-alpha-set computes join"
    (assert-equal sign-bottom (sign-alpha-set '()))
    (assert-equal sign-pos (sign-alpha-set '(1 2 3)))
    (assert-equal sign-neg (sign-alpha-set '(-1 -2 -3)))
    (assert-equal sign-top (sign-alpha-set '(-1 0 1))))

  (define-test "sign-leq ordering"
    (assert-true (sign-leq sign-bottom sign-neg))
    (assert-true (sign-leq sign-bottom sign-top))
    (assert-true (sign-leq sign-neg sign-top))
    (assert-true (sign-leq sign-pos sign-pos))
    (assert-false (sign-leq sign-pos sign-neg))
    (assert-false (sign-leq sign-top sign-zero)))

  (define-test "sign-join is lub"
    (assert-equal sign-neg (sign-join sign-bottom sign-neg))
    (assert-equal sign-neg (sign-join sign-neg sign-neg))
    (assert-equal sign-top (sign-join sign-neg sign-pos))
    (assert-equal sign-top (sign-join sign-zero sign-neg)))

  (define-test "sign-meet is glb"
    (assert-equal sign-neg (sign-meet sign-top sign-neg))
    (assert-equal sign-zero (sign-meet sign-zero sign-zero))
    (assert-equal sign-bottom (sign-meet sign-neg sign-pos))))

;;; ============================================================================
;;; Sign Arithmetic Tests
;;; ============================================================================

(test-group "sign-arithmetic"
  (define-test "sign-add soundness"
    (assert-equal sign-pos (sign-add sign-pos sign-pos))
    (assert-equal sign-neg (sign-add sign-neg sign-neg))
    (assert-equal sign-top (sign-add sign-neg sign-pos))
    (assert-equal sign-pos (sign-add sign-zero sign-pos))
    (assert-equal sign-neg (sign-add sign-neg sign-zero)))

  (define-test "sign-negate"
    (assert-equal sign-neg (sign-negate sign-pos))
    (assert-equal sign-pos (sign-negate sign-neg))
    (assert-equal sign-zero (sign-negate sign-zero))
    (assert-equal sign-top (sign-negate sign-top)))

  (define-test "sign-mul soundness"
    (assert-equal sign-pos (sign-mul sign-pos sign-pos))
    (assert-equal sign-pos (sign-mul sign-neg sign-neg))
    (assert-equal sign-neg (sign-mul sign-neg sign-pos))
    (assert-equal sign-zero (sign-mul sign-zero sign-pos))
    (assert-equal sign-zero (sign-mul sign-neg sign-zero)))

  (define-test "sign-div"
    (assert-equal sign-pos (sign-div sign-pos sign-pos))
    (assert-equal sign-pos (sign-div sign-neg sign-neg))
    (assert-equal sign-neg (sign-div sign-pos sign-neg))
    (assert-equal sign-bottom (sign-div sign-pos sign-zero))))

;;; ============================================================================
;;; Interval Domain Tests
;;; ============================================================================

(test-group "interval-domain"
  (define-test "interval-alpha creates singletons"
    (let ([iv (interval-alpha 5)])
      (assert-true (interval? iv))
      (assert-equal 5 (interval-lo iv))
      (assert-equal 5 (interval-hi iv))))

  (define-test "interval-alpha-set creates hull"
    (let ([iv (interval-alpha-set '(1 5 3))])
      (assert-equal 1 (interval-lo iv))
      (assert-equal 5 (interval-hi iv))))

  (define-test "interval-domain-join is hull"
    (let* ([a (make-interval 0 2)]
           [b (make-interval 5 10)]
           [joined (interval-domain-join a b)])
      (assert-equal 0 (interval-lo joined))
      (assert-equal 10 (interval-hi joined))))

  (define-test "interval-domain-meet is intersection"
    (let* ([a (make-interval 0 5)]
           [b (make-interval 3 10)]
           [met (interval-domain-meet a b)])
      (assert-equal 3 (interval-lo met))
      (assert-equal 5 (interval-hi met))))

  (define-test "interval-domain-meet returns bottom for disjoint"
    (let* ([a (make-interval 0 2)]
           [b (make-interval 5 10)]
           [met (interval-domain-meet a b)])
      (assert-true (interval-bottom? met))))

  (define-test "interval-domain-leq ordering"
    (let ([inner (make-interval 2 5)]
          [outer (make-interval 0 10)])
      (assert-true (interval-domain-leq inner outer))
      (assert-false (interval-domain-leq outer inner))
      (assert-true (interval-domain-leq interval-bottom inner)))))

;;; ============================================================================
;;; Type Domain Tests
;;; ============================================================================

(test-group "type-domain"
  (define-test "type-alpha infers types"
    (assert-equal type-int (type-alpha 42))
    (assert-equal type-real (type-alpha 3.14))
    (assert-equal type-bool (type-alpha #t))
    (assert-equal type-string (type-alpha "hello"))
    (assert-equal type-list (type-alpha '(1 2 3)))
    (assert-equal type-proc (type-alpha (lambda (x) x))))

  (define-test "type-leq with numeric tower"
    (assert-true (type-leq type-int type-real))
    (assert-false (type-leq type-real type-int))
    (assert-true (type-leq type-int type-top))
    (assert-true (type-leq type-bottom type-bool)))

  (define-test "type-join"
    (assert-equal type-real (type-join type-int type-real))
    (assert-equal type-top (type-join type-int type-bool))
    (assert-equal type-string (type-join type-bottom type-string))))

;;; ============================================================================
;;; Reachability Domain Tests
;;; ============================================================================

(test-group "reach-domain"
  (define-test "reach-join"
    (assert-equal reach-top (reach-join reach-bottom reach-top))
    (assert-equal reach-top (reach-join reach-top reach-top))
    (assert-equal reach-bottom (reach-join reach-bottom reach-bottom)))

  (define-test "reach-meet"
    (assert-equal reach-bottom (reach-meet reach-bottom reach-top))
    (assert-equal reach-top (reach-meet reach-top reach-top))))

;;; ============================================================================
;;; Galois Connection Soundness Tests
;;; ============================================================================

(test-group "galois-soundness"
  (define-test "sign galois soundness"
    (assert-true (verify-sign-soundness '(-10 -1 0 1 10 100))))

  (define-test "interval galois soundness"
    (assert-true (verify-interval-soundness '(-5.5 0 3.14 100))))

  (define-test "galois closure is extensive"
    ;; For Galois connection: x ⊑ γ(α(x))
    (let* ([x 5]
           [a (sign-alpha x)]
           [pred (sign-gamma a)])
      (assert-true (pred x))))

  (define-test "galois closure on concrete values"
    ;; Closure γ∘α applied to concrete value returns predicate that accepts it
    ;; galois-closure(gc, x) = γ(α(x)), returns predicate
    (let* ([x 5]
           [closure-pred (galois-closure galois-signs x)])
      ;; The closure predicate should accept the original value
      (assert-true (closure-pred x))
      ;; And any other positive integer
      (assert-true (closure-pred 100))
      ;; But not negative
      (assert-false (closure-pred -1)))))

;;; ============================================================================
;;; Abstract Environment Tests
;;; ============================================================================

(test-group "abstract-env"
  (define-test "env creation and lookup"
    (let* ([env (make-abstract-env sign-domain)]
           [env2 (env-extend env 'x sign-pos)])
      (assert-equal sign-bottom (env-lookup env 'x))
      (assert-equal sign-pos (env-lookup env2 'x))))

  (define-test "env-join merges bindings"
    (let* ([env1 (env-extend (make-abstract-env sign-domain) 'x sign-pos)]
           [env2 (env-extend (make-abstract-env sign-domain) 'x sign-neg)]
           [joined (env-join env1 env2)])
      (assert-equal sign-top (env-lookup joined 'x)))))

;;; ============================================================================
;;; Abstract Evaluation Tests
;;; ============================================================================

(test-group "abstract-eval"
  (define-test "abstract-eval-sign literals"
    (let ([env (make-abstract-env sign-domain)])
      (assert-equal sign-pos (abstract-eval-sign 5 env))
      (assert-equal sign-neg (abstract-eval-sign -3 env))
      (assert-equal sign-zero (abstract-eval-sign 0 env))))

  (define-test "abstract-eval-sign variables"
    (let* ([env (env-extend (make-abstract-env sign-domain) 'x sign-neg)])
      (assert-equal sign-neg (abstract-eval-sign 'x env))))

  (define-test "abstract-eval-sign arithmetic"
    (let* ([env (env-extend
                 (env-extend (make-abstract-env sign-domain) 'x sign-pos)
                 'y sign-neg)])
      (assert-equal sign-top (abstract-eval-sign '(+ x y) env))
      (assert-equal sign-neg (abstract-eval-sign '(* x y) env))
      (assert-equal sign-pos (abstract-eval-sign '(* y y) env))))

  (define-test "abstract-eval-interval"
    (let* ([env (env-extend
                 (make-abstract-env interval-domain)
                 'x (make-interval 2 4))])
      (let ([result (abstract-eval-interval '(* x x) env)])
        (assert-true (interval? result))
        ;; x ∈ [2,4], x² ∈ [4,16]
        (assert-equal 4 (interval-lo result))
        (assert-equal 16 (interval-hi result))))))

;;; ============================================================================
;;; Widening Tests
;;; ============================================================================

(test-group "widening"
  (define-test "interval-widen jumps to infinity"
    (let* ([prev (make-interval 0 10)]
           [curr (make-interval -1 12)]  ; Both bounds grew
           [widened (interval-widen prev curr)])
      (assert-equal -inf.0 (interval-lo widened))
      (assert-equal +inf.0 (interval-hi widened))))

  (define-test "interval-widen stable bounds preserved"
    (let* ([prev (make-interval 0 10)]
           [curr (make-interval 2 8)]  ; Both bounds shrunk
           [widened (interval-widen prev curr)])
      (assert-equal 0 (interval-lo widened))
      (assert-equal 10 (interval-hi widened))))

  (define-test "sign-widen is just join"
    (assert-equal sign-top (sign-widen sign-neg sign-pos))
    (assert-equal sign-neg (sign-widen sign-neg sign-neg))))

;;; ============================================================================
;;; Run Tests
;;; ============================================================================

(run-all-tests)
