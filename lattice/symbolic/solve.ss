;;; @module solve
;;; @requires expr simplify

(require 'expr)
(require 'simplify)

(doc 'module 'solve)
(doc 'description "Solve equations symbolically")
(doc 'features "Linear, quadratic, cubic formulas, polynomial roots, symbolic Gaussian elimination")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'section 'equation-representation)
(doc 'note "Equation represented as (= lhs rhs) or lhs - rhs = 0 form")

(define (make-equation lhs rhs)
  (doc 'export #t)
  (doc 'type '(-> Expr Expr Equation))
  (doc 'description "Create equation from left and right hand sides")
  (list '= lhs rhs))

;;; equation? : Any → Boolean
(define (equation? x)
  (doc 'export #t)
  (and (pair? x) (eq? (car x) '=)))

;;; equation-lhs : Equation → Expr
(define (equation-lhs eq)
  (doc 'export #t)
  (cadr eq))

;;; equation-rhs : Equation → Expr
(define (equation-rhs eq)
  (doc 'export #t)
  (caddr eq))

;;; equation->expr : Equation → Expr
;;; Convert equation to "lhs - rhs" form (set equal to zero).
(define (equation->expr eq)
  (doc 'export #t)
  (simplify (difference (equation-lhs eq) (equation-rhs eq))))

;;; ====
;;; Coefficient Extraction
;;; ====

;;; collect-terms : Expr × Symbol → (Values Expr Expr)
;;; Collect terms containing var and terms not containing var.
;;; Returns (coefficient-of-var, constant-term) for linear expressions.
(define (collect-linear-coeffs expr var-sym)
  (doc 'export #t)
  (let ([simplified (simplify expr)])
    (collect-linear-coeffs-impl simplified var-sym)))

(define (collect-linear-coeffs-impl expr var-sym)
  (doc 'export #t)
  (cond
    ;; Numeric constant: coefficient=0, constant=n
    [(num? expr)
     (values (num 0) expr)]

    ;; Variable
    [(var? expr)
     (if (eq? (var-name expr) var-sym)
         (values (num 1) (num 0))      ; x → coeff=1, const=0
         (values (num 0) expr))]       ; y → coeff=0, const=y

    ;; Sum: collect from each term
    [(sum? expr)
     (let loop ([terms (sum-terms expr)]
                [coeff-acc (num 0)]
                [const-acc (num 0)])
       (if (null? terms)
           (values (simplify coeff-acc) (simplify const-acc))
           (let-values ([(c k) (collect-linear-coeffs-impl (car terms) var-sym)])
             (loop (cdr terms)
                   (sum coeff-acc c)
                   (sum const-acc k)))))]

    ;; Product: check if it's coefficient * variable
    [(product? expr)
     (let ([factors (product-factors expr)])
       (cond
         ;; Single var factor: extract coefficient
         [(and (= (length factors) 2)
               (var? (cadr factors))
               (eq? (var-name (cadr factors)) var-sym)
               (not (contains-var? (car factors) var-sym)))
          (values (car factors) (num 0))]
         [(and (= (length factors) 2)
               (var? (car factors))
               (eq? (var-name (car factors)) var-sym)
               (not (contains-var? (cadr factors) var-sym)))
          (values (cadr factors) (num 0))]
         ;; No var in product: it's a constant
         [(not (any-contains-var? factors var-sym))
          (values (num 0) expr)]
         ;; Complex product with var
         [else
          (values expr (num 0))]))]

    ;; Difference
    [(difference? expr)
     (if (diff-right expr)
         (let-values ([(c1 k1) (collect-linear-coeffs-impl (diff-left expr) var-sym)]
                      [(c2 k2) (collect-linear-coeffs-impl (diff-right expr) var-sym)])
           (values (simplify (difference c1 c2))
                   (simplify (difference k1 k2))))
         ;; Negation
         (let-values ([(c k) (collect-linear-coeffs-impl (diff-left expr) var-sym)])
           (values (simplify (make-neg c))
                   (simplify (make-neg k)))))]

    ;; Other: treat as coefficient if contains var, else constant
    [else
     (if (contains-var? expr var-sym)
         (values expr (num 0))
         (values (num 0) expr))]))

;;; any-contains-var? : (List Expr) × Symbol → Boolean
(define (any-contains-var? exprs var-sym)
  (doc 'export #t)
  (exists (lambda (e) (contains-var? e var-sym)) exprs))

;;; contains-var? : Expr × Symbol → Boolean
(define (contains-var? expr var-sym)
  (doc 'export #t)
  (cond
    [(num? expr) #f]
    [(var? expr) (eq? (var-name expr) var-sym)]
    [(sum? expr) (any-contains-var? (sum-terms expr) var-sym)]
    [(product? expr) (any-contains-var? (product-factors expr) var-sym)]
    [(difference? expr)
     (or (contains-var? (diff-left expr) var-sym)
         (and (diff-right expr) (contains-var? (diff-right expr) var-sym)))]
    [(quotient? expr)
     (or (contains-var? (quot-numer expr) var-sym)
         (contains-var? (quot-denom expr) var-sym))]
    [(power? expr)
     (or (contains-var? (pow-base expr) var-sym)
         (contains-var? (pow-exp expr) var-sym))]
    [(app? expr) (contains-var? (app-arg expr) var-sym)]
    [else #f]))

;;; ====
;;; Polynomial Coefficient Extraction
;;; ====

;;; extract-poly-coefficients : Expr × Symbol → (List Expr) | #f
;;; Extract coefficients [a0, a1, a2, ...] for polynomial in var.
;;; Returns #f if expression is not polynomial in var.
(define (extract-poly-coefficients expr var-sym)
  (doc 'export #t)
  (let ([simplified (simplify expr)])
    (if (not (is-polynomial? simplified var-sym))
        #f
        (let ([deg (polynomial-degree simplified var-sym)])
          (if (< deg 0)
              (list simplified)  ; Constant
              (let loop ([k 0] [coeffs '()])
                (if (> k deg)
                    (reverse coeffs)
                    (loop (+ k 1)
                          (cons (extract-coefficient-of-degree simplified var-sym k)
                                coeffs)))))))))

;;; is-polynomial? : Expr × Symbol → Boolean
(define (is-polynomial? expr var-sym)
  (doc 'export #t)
  (cond
    [(num? expr) #t]
    [(var? expr) #t]
    [(sum? expr) (for-all (lambda (t) (is-polynomial? t var-sym)) (sum-terms expr))]
    [(product? expr) (for-all (lambda (f) (is-polynomial? f var-sym)) (product-factors expr))]
    [(difference? expr)
     (and (is-polynomial? (diff-left expr) var-sym)
          (or (not (diff-right expr))
              (is-polynomial? (diff-right expr) var-sym)))]
    [(power? expr)
     (and (num? (pow-exp expr))
          (integer? (num-val (pow-exp expr)))
          (>= (num-val (pow-exp expr)) 0)
          (is-polynomial? (pow-base expr) var-sym))]
    [(quotient? expr)
     ;; Only polynomial if denominator doesn't contain var
     (and (not (contains-var? (quot-denom expr) var-sym))
          (is-polynomial? (quot-numer expr) var-sym))]
    [else #f]))

;;; polynomial-degree : Expr × Symbol → Integer
(define (polynomial-degree expr var-sym)
  (doc 'export #t)
  (cond
    [(num? expr) 0]
    [(var? expr) (if (eq? (var-name expr) var-sym) 1 0)]
    [(sum? expr) (apply max (map (lambda (t) (polynomial-degree t var-sym)) (sum-terms expr)))]
    [(product? expr) (apply + (map (lambda (f) (polynomial-degree f var-sym)) (product-factors expr)))]
    [(difference? expr)
     (max (polynomial-degree (diff-left expr) var-sym)
          (if (diff-right expr) (polynomial-degree (diff-right expr) var-sym) 0))]
    [(power? expr)
     (if (and (num? (pow-exp expr)) (integer? (num-val (pow-exp expr))))
         (* (polynomial-degree (pow-base expr) var-sym) (num-val (pow-exp expr)))
         0)]
    [(quotient? expr)
     (if (contains-var? (quot-denom expr) var-sym)
         -1  ; Not a polynomial
         (polynomial-degree (quot-numer expr) var-sym))]
    [else 0]))

;;; extract-coefficient-of-degree : Expr × Symbol × Nat → Expr
;;; Extract coefficient of x^k in expression.
(define (extract-coefficient-of-degree expr var-sym k)
  (doc 'export #t)
  (simplify (extract-coeff-impl (simplify expr) var-sym k)))

(define (extract-coeff-impl expr var-sym k)
  (doc 'export #t)
  (cond
    ;; Constant: coefficient of x^0 is the constant, else 0
    [(num? expr)
     (if (= k 0) expr (num 0))]

    ;; Variable x: coefficient of x^1 is 1, x^0 is 0
    [(var? expr)
     (if (eq? (var-name expr) var-sym)
         (if (= k 1) (num 1) (num 0))
         (if (= k 0) expr (num 0)))]

    ;; Sum: sum of coefficients
    [(sum? expr)
     (make-sum-from-terms
      (map (lambda (t) (extract-coeff-impl t var-sym k))
           (sum-terms expr)))]

    ;; Power x^n: coefficient of x^n is 1, else 0
    [(power? expr)
     (if (and (var? (pow-base expr))
              (eq? (var-name (pow-base expr)) var-sym)
              (num? (pow-exp expr)))
         (if (= k (num-val (pow-exp expr))) (num 1) (num 0))
         ;; More complex power
         (if (= k 0)
             (if (contains-var? expr var-sym) (num 0) expr)
             (num 0)))]

    ;; Product c*x^n: coefficient of x^n is c
    [(product? expr)
     (let ([factors (product-factors expr)])
       (extract-product-coeff factors var-sym k))]

    ;; Difference
    [(difference? expr)
     (if (diff-right expr)
         (difference (extract-coeff-impl (diff-left expr) var-sym k)
                     (extract-coeff-impl (diff-right expr) var-sym k))
         (make-neg (extract-coeff-impl (diff-left expr) var-sym k)))]

    ;; Quotient (only if denominator doesn't contain var)
    [(quotient? expr)
     (if (contains-var? (quot-denom expr) var-sym)
         (num 0)
         (division (extract-coeff-impl (quot-numer expr) var-sym k)
                   (quot-denom expr)))]

    [else (num 0)]))

;;; extract-product-coeff : (List Expr) × Symbol × Nat → Expr
(define (extract-product-coeff factors var-sym k)
  (doc 'export #t)
  (let* ([var-factors (filter (lambda (f) (contains-var? f var-sym)) factors)]
         [const-factors (filter (lambda (f) (not (contains-var? f var-sym))) factors)]
         [const-prod (if (null? const-factors) (num 1) (make-product-from-terms const-factors))])
    (cond
      ;; No variable factors: coefficient of x^0 is product, else 0
      [(null? var-factors)
       (if (= k 0) const-prod (num 0))]
      ;; Single variable factor
      [(= (length var-factors) 1)
       (let ([vf (car var-factors)])
         (cond
           ;; Just x
           [(and (var? vf) (eq? (var-name vf) var-sym))
            (if (= k 1) const-prod (num 0))]
           ;; x^n
           [(and (power? vf)
                 (var? (pow-base vf))
                 (eq? (var-name (pow-base vf)) var-sym)
                 (num? (pow-exp vf)))
            (if (= k (num-val (pow-exp vf))) const-prod (num 0))]
           [else (num 0)]))]
      ;; Multiple variable factors - need to expand
      [else (num 0)])))  ; Simplified: would need full expansion

;;; make-sum-from-terms : (List Expr) → Expr
(define (make-sum-from-terms terms)
  (doc 'export #t)
  (let ([non-zero (filter (lambda (t) (not (and (num? t) (= (num-val t) 0)))) terms)])
    (cond
      [(null? non-zero) (num 0)]
      [(= (length non-zero) 1) (car non-zero)]
      [else (cons '+ non-zero)])))

;;; make-product-from-terms : (List Expr) → Expr
(define (make-product-from-terms factors)
  (doc 'export #t)
  (cond
    [(null? factors) (num 1)]
    [(= (length factors) 1) (car factors)]
    [else (cons '* factors)]))

(doc 'section 'linear-solving)

(define (solve-linear expr var-sym)
  (doc 'export #t)
  (doc 'type '(-> Expr Symbol (Maybe Expr)))
  (doc 'description "Solve linear equation ax + b = 0 for x")
  (doc 'note "Returns x = -b/a, or #f if not solvable")
  (let-values ([(a b) (collect-linear-coeffs expr var-sym)])
    (cond
      ;; a = 0: no solution (or infinite if b = 0)
      [(and (num? a) (= (num-val a) 0))
       (if (and (num? b) (= (num-val b) 0))
           'infinite  ; 0 = 0, any value works
           #f)]       ; b = 0 with b ≠ 0, no solution
      ;; a ≠ 0: x = -b/a
      [else
       (simplify (division (make-neg b) a))])))

(doc 'section 'quadratic-solving)

(define (solve-quadratic expr var-sym)
  (doc 'export #t)
  (doc 'type '(-> Expr Symbol (Maybe (List Expr))))
  (doc 'description "Solve quadratic equation ax² + bx + c = 0 using quadratic formula")
  (doc 'note "Returns list of solutions, may include symbolic sqrt")
  (let ([coeffs (extract-poly-coefficients expr var-sym)])
    (if (or (not coeffs) (not (= (length coeffs) 3)))
        #f
        (let* ([c (list-ref coeffs 0)]   ; constant term
               [b (list-ref coeffs 1)]   ; linear coefficient
               [a (list-ref coeffs 2)]   ; quadratic coefficient
               ;; discriminant = b² - 4ac
               [disc (simplify (difference (power b (num 2))
                                           (product (num 4) (product a c))))]
               ;; -b / 2a
               [neg-b-over-2a (simplify (division (make-neg b)
                                                  (product (num 2) a)))]
               ;; sqrt(disc) / 2a
               [sqrt-disc-over-2a (simplify (division (sym-sqrt disc)
                                                      (product (num 2) a)))])
          ;; Solutions: (-b ± √disc) / 2a
          (list (simplify (sum neg-b-over-2a sqrt-disc-over-2a))
                (simplify (difference neg-b-over-2a sqrt-disc-over-2a)))))))

(doc 'section 'cubic-solving)

(define (solve-cubic expr var-sym)
  (doc 'export #t)
  (doc 'type '(-> Expr Symbol (Maybe (List Expr))))
  (doc 'description "Solve cubic equation ax³ + bx² + cx + d = 0 using Cardano's formula")
  (doc 'note "Converts to depressed cubic, returns all 3 roots using cube roots of unity")
  (let ([coeffs (extract-poly-coefficients expr var-sym)])
    (if (or (not coeffs) (not (= (length coeffs) 4)))
        #f
        (let* ([d (list-ref coeffs 0)]   ; constant
               [c (list-ref coeffs 1)]   ; linear
               [b (list-ref coeffs 2)]   ; quadratic
               [a (list-ref coeffs 3)]   ; cubic
               ;; Convert to monic: divide by a
               ;; x³ + (b/a)x² + (c/a)x + (d/a) = 0
               [b/a (simplify (division b a))]
               [c/a (simplify (division c a))]
               [d/a (simplify (division d a))]
               ;; Substitute x = t - b/(3a) to get depressed cubic t³ + pt + q = 0
               ;; p = c/a - b²/(3a²)
               ;; q = d/a - bc/(3a²) + 2b³/(27a³)
               [b2/3a2 (simplify (division (power b/a (num 2)) (num 3)))]
               [p (simplify (difference c/a b2/3a2))]
               [q (simplify (sum (difference d/a
                                             (division (product b/a c/a) (num 3)))
                                 (division (product (num 2) (power b/a (num 3)))
                                           (num 27))))]
               ;; Cardano: t = ∛(-q/2 + √Δ) + ∛(-q/2 - √Δ)
               ;; where Δ = q²/4 + p³/27
               [disc-inner (simplify (sum (division (power q (num 2)) (num 4))
                                          (division (power p (num 3)) (num 27))))]
               [sqrt-disc (sym-sqrt disc-inner)]
               [neg-q/2 (simplify (division (make-neg q) (num 2)))]
               ;; The cube root arguments
               [u-arg (simplify (sum neg-q/2 sqrt-disc))]
               [v-arg (simplify (difference neg-q/2 sqrt-disc))]
               ;; Principal cube roots
               [cbrt-u (power u-arg (division (num 1) (num 3)))]
               [cbrt-v (power v-arg (division (num 1) (num 3)))]
               ;; Cube roots of unity: ω = (-1 + i√3)/2, ω² = (-1 - i√3)/2
               ;; For symbolic output, represent as primitive roots
               [omega (division (sum (num -1) (product (var 'i) (sym-sqrt (num 3)))) (num 2))]
               [omega2 (division (difference (num -1) (product (var 'i) (sym-sqrt (num 3)))) (num 2))]
               ;; Three roots of depressed cubic:
               ;; t₁ = cbrt(u) + cbrt(v)           [principal roots]
               ;; t₂ = ω·cbrt(u) + ω²·cbrt(v)     [first rotation]
               ;; t₃ = ω²·cbrt(u) + ω·cbrt(v)     [second rotation]
               [t1 (simplify (sum cbrt-u cbrt-v))]
               [t2 (simplify (sum (product omega cbrt-u) (product omega2 cbrt-v)))]
               [t3 (simplify (sum (product omega2 cbrt-u) (product omega cbrt-v)))]
               ;; Shift back: x = t - b/(3a)
               [shift (simplify (division b/a (num 3)))]
               [x1 (simplify (difference t1 shift))]
               [x2 (simplify (difference t2 shift))]
               [x3 (simplify (difference t3 shift))])
          (list x1 x2 x3)))))

;;; ====
;;; General Polynomial Solving
;;; ====

;;; solve-polynomial : Expr × Symbol → (List Expr) | #f
;;; Solve polynomial equation = 0 for variable.
;;; Dispatches to appropriate formula based on degree.
(define (solve-polynomial expr var-sym)
  (doc 'export #t)
  (let ([deg (polynomial-degree expr var-sym)])
    (cond
      [(< deg 0) #f]                          ; Not a polynomial
      [(= deg 0)                              ; Constant
       (let ([simplified (simplify expr)])
         (if (and (num? simplified) (= (num-val simplified) 0))
             'infinite
             '()))]                           ; No solutions
      [(= deg 1)
       (let ([sol (solve-linear expr var-sym)])
         (if sol (list sol) #f))]
      [(= deg 2) (solve-quadratic expr var-sym)]
      [(= deg 3) (solve-cubic expr var-sym)]
      ;; Degree 4+: return symbolic representation
      [else
       (let ([coeffs (extract-poly-coefficients expr var-sym)])
         (if coeffs
             `((roots-of ,(cons '+ (map (lambda (c k)
                                          (product c (power (var var-sym) (num k))))
                                        coeffs
                                        (iota (length coeffs))))))
             #f))])))

;; iota is provided by prelude (via expr.ss)

(doc 'section 'main-interface)

(define (diff-to-sum expr)
  (doc 'export #t)
  (doc 'type '(-> Expr Expr))
  (doc 'description "Convert difference to sum recursively to enable expansion over differences")
  (cond
    [(num? expr) expr]
    [(var? expr) expr]
    [(sum? expr)
     (make-sum-from-terms (map diff-to-sum (sum-terms expr)))]
    [(product? expr)
     (make-product-from-terms (map diff-to-sum (product-factors expr)))]
    [(difference? expr)
     (if (diff-right expr)
         ;; (- a b) → (+ a (- b))
         (sum (diff-to-sum (diff-left expr))
              (make-neg (diff-to-sum (diff-right expr))))
         ;; Negation stays as negation
         (make-neg (diff-to-sum (diff-left expr))))]
    [(quotient? expr)
     (division (diff-to-sum (quot-numer expr))
               (diff-to-sum (quot-denom expr)))]
    [(power? expr)
     (power (diff-to-sum (pow-base expr))
            (diff-to-sum (pow-exp expr)))]
    [(app? expr)
     (make-app (app-fn expr) (diff-to-sum (app-arg expr)))]
    [else expr]))

(define (solve eq var-sym)
  (doc 'export #t)
  (doc 'type '(-> Equation Symbol (Maybe (U (List Expr) Expr Symbol))))
  (doc 'description "Solve equation for given variable")
  (doc 'note "Returns list of solutions, single solution, 'infinite, or #f; auto-expands factored forms")
  (let* ([normalized (if (equation? eq)
                         (make-equation (normalize-expr (equation-lhs eq))
                                        (normalize-expr (equation-rhs eq)))
                         (normalize-expr eq))]
         [expr (if (equation? normalized)
                   (equation->expr normalized)
                   normalized)]
         ;; Convert differences to sums so expand can distribute
         [as-sums (diff-to-sum expr)]
         ;; Expand factored products before solving
         [expanded (simplify (expand as-sums))])
    (solve-polynomial expanded var-sym)))

;;; solve-for : Expr × Symbol → Expr | #f
;;; Convenience: solve single-variable equation, return first solution.
(define (solve-for expr var-sym)
  (doc 'export #t)
  (let ([solutions (solve expr var-sym)])
    (cond
      [(eq? solutions 'infinite) (var var-sym)]  ; Any value works
      [(and (list? solutions) (not (null? solutions))) (car solutions)]
      [(and (not (list? solutions)) solutions) solutions]
      [else #f])))

(doc 'section 'linear-systems)

(define (solve-linear-system equations vars)
  (doc 'export #t)
  (doc 'type '(-> (List Equation) (List Symbol) (Maybe (Alist Symbol Expr))))
  (doc 'description "Solve system of linear equations using symbolic Gaussian elimination")
  (doc 'note "Returns alist mapping variables to solutions")
  (let* ([n (length vars)]
         [exprs (map (lambda (eq)
                       (if (equation? eq) (equation->expr eq) eq))
                     equations)]
         ;; Build augmented matrix [A | b] symbolically
         [matrix (build-coefficient-matrix exprs vars)]
         [augmented (augment-with-constants matrix exprs vars)])
    (if (not augmented)
        #f
        (let ([reduced (gaussian-eliminate-symbolic augmented n)])
          (if reduced
              (back-substitute-symbolic reduced vars)
              #f)))))

;;; build-coefficient-matrix : (List Expr) × (List Symbol) → (List (List Expr))
(define (build-coefficient-matrix exprs vars)
  (doc 'export #t)
  (map (lambda (expr)
         (map (lambda (v)
                (let-values ([(c k) (collect-linear-coeffs expr v)])
                  c))
              vars))
       exprs))

;;; augment-with-constants : Matrix × (List Expr) × (List Symbol) → Matrix | #f
(define (augment-with-constants matrix exprs vars)
  (doc 'export #t)
  (let ([constants (map (lambda (expr)
                          ;; Constant term is what remains after removing all variables
                          (let ([with-zeros (fold-left
                                             (lambda (e v)
                                               (simplify (subst e v (num 0))))
                                             expr vars)])
                            (simplify (make-neg with-zeros))))
                        exprs)])
    (map (lambda (row const) (append row (list const)))
         matrix constants)))

;;; gaussian-eliminate-symbolic : Matrix × Nat → Matrix | #f
;;; Perform Gaussian elimination symbolically.
(define (gaussian-eliminate-symbolic matrix n)
  (doc 'export #t)
  (let loop ([m matrix] [col 0])
    (if (>= col n)
        m
        (let ([pivot-row (find-nonzero-pivot m col)])
          (if (not pivot-row)
              (loop m (+ col 1))  ; Skip zero column
              (let* ([m1 (swap-rows m col pivot-row)]
                     [m2 (eliminate-below m1 col)])
                (loop m2 (+ col 1))))))))

;;; find-nonzero-pivot : Matrix × Nat → Nat | #f
(define (find-nonzero-pivot matrix col)
  (doc 'export #t)
  (let loop ([row col] [rows (list-tail matrix col)])
    (cond
      [(null? rows) #f]
      [(not (is-zero-expr? (list-ref (car rows) col))) row]
      [else (loop (+ row 1) (cdr rows))])))

;;; is-zero-expr? : Expr → Boolean
(define (is-zero-expr? expr)
  (doc 'export #t)
  (let ([s (simplify expr)])
    (and (num? s) (= (num-val s) 0))))

;;; swap-rows : Matrix × Nat × Nat → Matrix
(define (swap-rows matrix i j)
  (doc 'export #t)
  (if (= i j)
      matrix
      (let ([row-i (list-ref matrix i)]
            [row-j (list-ref matrix j)])
        (map (lambda (row idx)
               (cond [(= idx i) row-j]
                     [(= idx j) row-i]
                     [else row]))
             matrix (iota (length matrix))))))

;;; eliminate-below : Matrix × Nat → Matrix
(define (eliminate-below matrix pivot-col)
  (doc 'export #t)
  (let* ([pivot-row (list-ref matrix pivot-col)]
         [pivot-val (list-ref pivot-row pivot-col)])
    (map (lambda (row idx)
           (if (<= idx pivot-col)
               row
               (let ([factor (simplify (division (list-ref row pivot-col) pivot-val))])
                 (map (lambda (a b)
                        (simplify (difference a (product factor b))))
                      row pivot-row))))
         matrix (iota (length matrix)))))

;;; back-substitute-symbolic : Matrix × (List Symbol) → (Alist Symbol Expr) | #f
(define (back-substitute-symbolic matrix vars)
  (doc 'export #t)
  (let ([n (length vars)])
    (let loop ([row (- n 1)] [solutions '()])
      (if (< row 0)
          solutions
          (let* ([mat-row (list-ref matrix row)]
                 [rhs (list-ref mat-row n)]  ; Last column is RHS
                 [pivot (list-ref mat-row row)])
            (if (is-zero-expr? pivot)
                #f  ; Singular system
                (let* ([sum-of-known
                        (fold-left
                         (lambda (acc pair)
                           (let ([col (car pair)]
                                 [val (cdr pair)])
                             (if (> col row)
                                 (let ([var-sol (assq (list-ref vars col) solutions)])
                                   (if var-sol
                                       (sum acc (product (list-ref mat-row col) (cdr var-sol)))
                                       acc))
                                 acc)))
                         (num 0)
                         (map cons (iota n) (iota n)))]
                       [solution (simplify (division (difference rhs sum-of-known) pivot))])
                  (loop (- row 1)
                        (cons (cons (list-ref vars row) solution) solutions)))))))))

;;; subst : Expr × Symbol × Expr → Expr
;;; Substitute value for variable in expression.
(define (subst expr var-sym val)
  (doc 'export #t)
  (cond
    [(num? expr) expr]
    [(var? expr) (if (eq? (var-name expr) var-sym) val expr)]
    [(sum? expr) (make-sum (map (lambda (t) (subst t var-sym val)) (sum-terms expr)))]
    [(product? expr) (make-product (map (lambda (f) (subst f var-sym val)) (product-factors expr)))]
    [(difference? expr)
     (if (diff-right expr)
         (difference (subst (diff-left expr) var-sym val)
                     (subst (diff-right expr) var-sym val))
         (make-neg (subst (diff-left expr) var-sym val)))]
    [(quotient? expr)
     (division (subst (quot-numer expr) var-sym val)
               (subst (quot-denom expr) var-sym val))]
    [(power? expr)
     (power (subst (pow-base expr) var-sym val)
            (subst (pow-exp expr) var-sym val))]
    [(app? expr)
     (make-app (app-fn expr) (subst (app-arg expr) var-sym val))]
    [else expr]))
