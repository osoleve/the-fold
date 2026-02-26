(load "core/lang/module.ss")
(load "core/testing/test-framework.ss")
(load "lattice/egraph/egraph-groebner.ss")

;;; ============================================================
;;; E-Term → Polynomial Conversion
;;; ============================================================

(test-group "eterm->mpoly"

  (define F Q-field)
  (define vars '(x y z))
  (define ordering (make-ordering 'grevlex vars))

  (define-test "numeric literal"
    (let ([p (eterm->mpoly 5 F vars ordering)])
      (assert-true (mpoly? p))
      (assert-equal 5 (mpoly-eval p '((x . 0) (y . 0) (z . 0))))))

  (define-test "single variable"
    (let ([p (eterm->mpoly 'x F vars ordering)])
      (assert-true (mpoly? p))
      (assert-equal 3 (mpoly-eval p '((x . 3) (y . 0) (z . 0))))))

  (define-test "addition"
    (let ([p (eterm->mpoly '(+ x y) F vars ordering)])
      (assert-true (mpoly? p))
      (assert-equal 7 (mpoly-eval p '((x . 3) (y . 4) (z . 0))))))

  (define-test "subtraction"
    (let ([p (eterm->mpoly '(- x y) F vars ordering)])
      (assert-true (mpoly? p))
      (assert-equal -1 (mpoly-eval p '((x . 3) (y . 4) (z . 0))))))

  (define-test "multiplication"
    (let ([p (eterm->mpoly '(* x y) F vars ordering)])
      (assert-true (mpoly? p))
      (assert-equal 12 (mpoly-eval p '((x . 3) (y . 4) (z . 0))))))

  (define-test "negation"
    (let ([p (eterm->mpoly '(neg x) F vars ordering)])
      (assert-true (mpoly? p))
      (assert-equal -3 (mpoly-eval p '((x . 3) (y . 0) (z . 0))))))

  (define-test "exponentiation"
    (let ([p (eterm->mpoly '(^ x 3) F vars ordering)])
      (assert-true (mpoly? p))
      (assert-equal 27 (mpoly-eval p '((x . 3) (y . 0) (z . 0))))))

  (define-test "nested expression"
    ;; (x + y)² = x² + 2xy + y²
    (let ([p (eterm->mpoly '(* (+ x y) (+ x y)) F vars ordering)])
      (assert-true (mpoly? p))
      ;; (3+4)² = 49 = 9 + 24 + 16
      (assert-equal 49 (mpoly-eval p '((x . 3) (y . 4) (z . 0))))))

  (define-test "non-polynomial returns #f"
    (assert-true (not (eterm->mpoly '(/ x y) F vars ordering)))
    (assert-true (not (eterm->mpoly '(sin x) F vars ordering))))

  (define-test "unknown variable returns #f"
    (assert-true (not (eterm->mpoly 'w F vars ordering)))))

;;; ============================================================
;;; Polynomial → E-Term Conversion
;;; ============================================================

(test-group "mpoly->eterm"

  (define F Q-field)
  (define vars '(x y))
  (define ordering (make-ordering 'grevlex vars))

  (define-test "constant polynomial"
    (let* ([p (mpoly-constant F vars ordering 5)]
           [t (mpoly->eterm p)])
      (assert-equal 5 t)))

  (define-test "single variable"
    (let* ([p (mpoly-var F vars ordering 'x)]
           [t (mpoly->eterm p)])
      (assert-equal 'x t)))

  (define-test "roundtrip preserves value"
    ;; x² + 2xy + y²
    (let* ([x (mpoly-var F vars ordering 'x)]
           [y (mpoly-var F vars ordering 'y)]
           [p (mpoly-add (mpoly-add (mpoly-mul x x)
                                     (mpoly-scale (mpoly-mul x y) 2))
                          (mpoly-mul y y))]
           [t (mpoly->eterm p)]
           [p2 (eterm->mpoly t F vars ordering)])
      ;; Roundtrip should give equivalent polynomial
      (assert-true (mpoly? p2))
      (assert-true (mpoly-zero? (mpoly-sub p p2))))))

;;; ============================================================
;;; Variable Extraction
;;; ============================================================

(test-group "eterm-variables"

  (define-test "numeric has no variables"
    (assert-equal '() (eterm-variables 42)))

  (define-test "single variable"
    (assert-equal '(x) (eterm-variables 'x)))

  (define-test "operators excluded"
    ;; + and * should not appear as variables
    (assert-equal '(x y) (eterm-variables '(+ x y))))

  (define-test "nested with duplicates"
    (assert-equal '(x y) (eterm-variables '(+ (* x y) (* y x)))))

  (define-test "sorted alphabetically"
    (assert-equal '(a b c) (eterm-variables '(+ c (+ b a))))))

;;; ============================================================
;;; Polynomial Equivalence
;;; ============================================================

(test-group "poly-equiv?"

  (define-test "identical terms"
    (assert-true (poly-equiv? '(+ x y) '(+ x y))))

  (define-test "commutative equivalence"
    (assert-true (poly-equiv? '(+ x y) '(+ y x))))

  (define-test "multiplication commutativity"
    (assert-true (poly-equiv? '(* x y) '(* y x))))

  (define-test "expansion equivalence"
    ;; (x+y)² = x² + 2xy + y²
    (assert-true (poly-equiv? '(* (+ x y) (+ x y))
                              '(+ (+ (^ x 2) (* 2 (* x y))) (^ y 2)))))

  (define-test "non-equivalent terms"
    (assert-true (not (poly-equiv? '(+ x y) '(* x y)))))

  (define-test "difference of squares"
    ;; (x+y)(x-y) = x² - y²
    (assert-true (poly-equiv? '(* (+ x y) (- x y))
                              '(- (^ x 2) (^ y 2))))))

;;; ============================================================
;;; Equivalence Modulo Relations
;;; ============================================================

(test-group "poly-equiv-modulo?"

  (define-test "trivial equivalence (no relations)"
    (assert-true (poly-equiv-modulo? '(+ x y) '(+ y x) '() Q-field)))

  (define-test "x² ≡ 1 modulo x²-1"
    ;; In the ring where x²-1 = 0, x² = 1
    (assert-true (poly-equiv-modulo? '(^ x 2) '1
                                     '((- (^ x 2) 1))
                                     Q-field)))

  (define-test "x³ ≡ x modulo x²-1"
    ;; x³ = x·x² = x·1 = x when x²=1
    (assert-true (poly-equiv-modulo? '(^ x 3) 'x
                                     '((- (^ x 2) 1))
                                     Q-field)))

  (define-test "non-equivalence modulo relation"
    (assert-true (not (poly-equiv-modulo? 'x 'y
                                          '((- (^ x 2) 1))
                                          Q-field)))))

;;; ============================================================
;;; Gröbner Rewrite Rules
;;; ============================================================

(test-group "groebner-rewrite-rules"

  (define-test "empty relations give no rules"
    (assert-equal '() (groebner-rewrite-rules '())))

  (define-test "single relation gives rules"
    ;; x² - 1 = 0 should give at least one rule
    (let ([rules (groebner-rewrite-rules '((- (^ x 2) 1)))])
      (assert-true (> (length rules) 0))))

  (define-test "rules are valid rule pairs"
    (let ([rules (groebner-rewrite-rules '((- (^ x 2) 1)))])
      (for-each (lambda (r) (assert-true (pair? r))) rules))))

;;; ============================================================
;;; Polynomial Identity Rules
;;; ============================================================

(test-group "poly-identity-rules"

  (define-test "returns non-empty list"
    (let ([rules (poly-identity-rules)])
      (assert-true (> (length rules) 10))))

  (define-test "rules are valid"
    (let ([rules (poly-identity-rules)])
      (for-each (lambda (r) (assert-true (pair? r))) rules))))

;;; ============================================================
;;; Cost Model
;;; ============================================================

(test-group "poly-degree-cost"

  (define-test "is a cost model"
    (assert-true (cost-model? poly-degree-cost)))

  (define-test "named poly-degree"
    (assert-equal 'poly-degree (cost-model-name poly-degree-cost))))

;;; ============================================================
;;; Polynomial Reduction
;;; ============================================================

(test-group "poly-reduce"

  (define-test "constant stays constant"
    (assert-equal 5 (poly-reduce 5)))

  (define-test "variable stays variable"
    (assert-equal 'x (poly-reduce 'x)))

  (define-test "non-polynomial passes through"
    ;; Division can't be represented as polynomial, returns as-is
    (assert-equal '(/ x y) (poly-reduce '(/ x y)))))

;;; ============================================================
;;; Reduction Modulo Relations
;;; ============================================================

(test-group "poly-reduce-modulo"

  (define-test "reduce x² modulo x²-1"
    ;; x² mod (x²-1) should give 1
    (let ([result (poly-reduce-modulo '(^ x 2) '((- (^ x 2) 1)))])
      (assert-true (poly-equiv? result '1))))

  (define-test "reduce x³ modulo x²-1"
    ;; x³ mod (x²-1) should give x
    (let ([result (poly-reduce-modulo '(^ x 3) '((- (^ x 2) 1)))])
      (assert-true (poly-equiv? result 'x))))

  (define-test "reduce with no relations"
    ;; Should just normalize
    (let ([result (poly-reduce-modulo '(+ x y) '())])
      (assert-true (poly-equiv? result '(+ x y))))))

;;; ============================================================
;;; Combined Optimization
;;; ============================================================

(test-group "poly-optimize"

  (define-test "simplify x+0"
    (let ([result (poly-optimize '(+ x 0) '())])
      (assert-true (poly-equiv? result 'x))))

  (define-test "simplify x*1"
    (let ([result (poly-optimize '(* x 1) '())])
      (assert-true (poly-equiv? result 'x))))

  (define-test "simplify x-x"
    (let ([result (poly-optimize '(- x x) '())])
      (assert-true (poly-equiv? result '0)))))

(run-all-tests)
