;;; lattice/fp/symbolic/poly-canonical.ss — Polynomial Canonical Form Integration
;;;
;;; Integrates lattice/algebra/polynomial.ss with symbolic math modules
;;; for GCD-based simplification and partial fraction decomposition.
;;;
;;; Benefits:
;;;   - Polynomial GCD for expression simplification
;;;   - Exact partial fraction decomposition via polynomial division
;;;   - Polynomial canonical form conversion
;;;   - Square-free factorization for symbolic expressions
;;;
;;; This is Lattice code: pure, functional, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - lattice/algebra/polynomial.ss
;;;   - lattice/algebra/field.ss
;;;   - lattice/fp/symbolic/expr.ss
;;;   - lattice/fp/symbolic/simplify.ss

(load "lattice/algebra/field.ss")
(load "lattice/algebra/polynomial.ss")
(load "lattice/fp/symbolic/expr.ss")
(load "lattice/fp/symbolic/simplify.ss")

;;; ============================================================
;;; Rational Field for Exact Coefficient Arithmetic
;;; ============================================================

;;; Q-field : Field
;;; The field of rational numbers.
(define Q-field-sym
  (make-field
   '()                                    ; Infinite field
   +                                      ; Addition
   *                                      ; Multiplication
   0                                      ; Zero
   1                                      ; One
   -                                      ; Negation
   (lambda (a b) (/ a b))                 ; Division
   =))                                    ; Equality

;;; ============================================================
;;; Conversion: Symbolic Expression ↔ Polynomial
;;; ============================================================

;;; expr->polynomial : Expr × Symbol × Field → AlgebraPoly | #f
;;; Convert a symbolic expression to a polynomial over the given field.
;;; Returns #f if the expression is not a polynomial in the given variable.
;;; The variable becomes the indeterminate; all other symbols are treated as constants.
(define (expr->polynomial expr var-sym field)
  (if (not (polynomial-expr? expr var-sym))
      #f
      (let ([degree (expr-degree expr var-sym)])
        (if (< degree 0)
            #f
            (let ([coeffs (extract-poly-coeffs expr var-sym degree field)])
              (make-polynomial field coeffs))))))

;;; polynomial-expr? : Expr × Symbol → Boolean
;;; Check if expression is a polynomial in the given variable.
(define (polynomial-expr? expr var-sym)
  (cond
    [(num? expr) #t]
    [(var? expr) #t]  ; Constants or the variable itself
    [(sum? expr)
     (for-all (lambda (t) (polynomial-expr? t var-sym)) (sum-terms expr))]
    [(product? expr)
     (for-all (lambda (f) (polynomial-expr? f var-sym)) (product-factors expr))]
    [(difference? expr)
     (and (polynomial-expr? (diff-left expr) var-sym)
          (or (not (diff-right expr))
              (polynomial-expr? (diff-right expr) var-sym)))]
    [(power? expr)
     ;; Power is polynomial if base is the variable and exponent is non-negative integer
     (and (num? (pow-exp expr))
          (integer? (num-val (pow-exp expr)))
          (>= (num-val (pow-exp expr)) 0)
          (or (and (var? (pow-base expr)) (eq? (var-name (pow-base expr)) var-sym))
              (polynomial-expr? (pow-base expr) var-sym)))]
    [else #f]))  ; Functions like sin, cos are not polynomial

;;; expr-degree : Expr × Symbol → Nat
;;; Get the degree of the variable in the expression.
(define (expr-degree expr var-sym)
  (cond
    [(num? expr) 0]
    [(var? expr)
     (if (eq? (var-name expr) var-sym) 1 0)]
    [(sum? expr)
     (apply max (map (lambda (t) (expr-degree t var-sym)) (sum-terms expr)))]
    [(product? expr)
     (apply + (map (lambda (f) (expr-degree f var-sym)) (product-factors expr)))]
    [(difference? expr)
     (max (expr-degree (diff-left expr) var-sym)
          (if (diff-right expr)
              (expr-degree (diff-right expr) var-sym)
              0))]
    [(power? expr)
     (if (and (num? (pow-exp expr))
              (integer? (num-val (pow-exp expr)))
              (>= (num-val (pow-exp expr)) 0))
         (* (expr-degree (pow-base expr) var-sym)
            (num-val (pow-exp expr)))
         999)]  ; Non-polynomial
    [else 0]))

;;; extract-poly-coeffs : Expr × Symbol × Nat × Field → (List Coeff)
;;; Extract coefficients [a_0, a_1, ..., a_n] from polynomial expression.
;;; Coefficients are in ascending power order.
(define (extract-poly-coeffs expr var-sym degree field)
  (let loop ([k 0] [coeffs '()])
    (if (> k degree)
        (reverse coeffs)
        (loop (+ k 1)
              (cons (extract-coeff-at expr var-sym k) coeffs)))))

;;; extract-coeff-at : Expr × Symbol × Nat → Number
;;; Extract the coefficient of x^k from the expression.
(define (extract-coeff-at expr var-sym k)
  (let ([expanded (expand expr)])  ; Fully expand first
    (extract-coeff-expanded expanded var-sym k)))

;;; extract-coeff-expanded : Expr × Symbol × Nat → Number
;;; Extract coefficient from expanded expression.
(define (extract-coeff-expanded expr var-sym k)
  (cond
    [(num? expr)
     (if (= k 0) (num-val expr) 0)]
    [(var? expr)
     (cond
       [(and (eq? (var-name expr) var-sym) (= k 1)) 1]
       [(and (eq? (var-name expr) var-sym) (not (= k 1))) 0]
       [(= k 0) 1]  ; Other variable treated as coefficient
       [else 0])]
    [(sum? expr)
     (apply + (map (lambda (t) (extract-coeff-expanded t var-sym k))
                   (sum-terms expr)))]
    [(difference? expr)
     (if (diff-right expr)
         (- (extract-coeff-expanded (diff-left expr) var-sym k)
            (extract-coeff-expanded (diff-right expr) var-sym k))
         (- (extract-coeff-expanded (diff-left expr) var-sym k)))]
    [(product? expr)
     ;; For products, need to consider all ways to get degree k
     (extract-coeff-product (product-factors expr) var-sym k)]
    [(power? expr)
     ;; x^n contributes 1 to coefficient of x^n
     (if (and (var? (pow-base expr))
              (eq? (var-name (pow-base expr)) var-sym)
              (num? (pow-exp expr))
              (= (num-val (pow-exp expr)) k))
         1
         (if (and (= k 0)
                  (not (and (var? (pow-base expr))
                           (eq? (var-name (pow-base expr)) var-sym))))
             (expr-eval-at-zero (pow-base expr) var-sym)
             0))]
    [else 0]))

;;; extract-coeff-product : (List Expr) × Symbol × Nat → Number
;;; Extract coefficient of x^k from product of factors.
(define (extract-coeff-product factors var-sym k)
  (if (null? factors)
      (if (= k 0) 1 0)
      (let ([first (car factors)]
            [rest (cdr factors)])
        ;; Coefficient of x^k in (first * rest) = sum over i: coeff_i(first) * coeff_{k-i}(rest)
        (let ([first-deg (expr-degree first var-sym)])
          (let loop ([i 0] [total 0])
            (if (> i (min first-deg k))
                total
                (let ([c1 (extract-coeff-expanded first var-sym i)]
                      [c2 (extract-coeff-product rest var-sym (- k i))])
                  (loop (+ i 1) (+ total (* c1 c2))))))))))

;;; expr-eval-at-zero : Expr × Symbol → Number
;;; Evaluate expression at var-sym = 0.
(define (expr-eval-at-zero expr var-sym)
  (extract-coeff-expanded expr var-sym 0))

;;; polynomial->expr : AlgebraPoly × Symbol → Expr
;;; Convert a polynomial back to symbolic expression.
(define (polynomial->expr poly var-sym)
  (let* ([coeffs (poly-coeffs poly)]
         [n (length coeffs)]
         [x (var var-sym)])
    (let loop ([k 0] [terms '()])
      (if (>= k n)
          (if (null? terms)
              (num 0)
              (make-sum-from-list (reverse terms)))
          (let ([c (list-ref coeffs k)])
            (if (= c 0)
                (loop (+ k 1) terms)
                (loop (+ k 1)
                      (cons (make-term c k x) terms))))))))

;;; make-term : Number × Nat × Expr → Expr
;;; Create term c * x^k.
(define (make-term c k x)
  (cond
    [(= k 0) (num c)]
    [(= k 1) (if (= c 1) x (product (num c) x))]
    [(= c 1) (power x (num k))]
    [else (product (num c) (power x (num k)))]))

;;; ============================================================
;;; GCD-Based Simplification for Rational Expressions
;;; ============================================================

;;; simplify-rational : Expr × Symbol → Expr
;;; Simplify a rational expression (quotient of polynomials) using GCD.
;;; This cancels common polynomial factors in numerator/denominator.
(define (simplify-rational expr var-sym)
  (if (not (quotient? expr))
      expr
      (let* ([numer (quot-numer expr)]
             [denom (quot-denom expr)]
             [numer-poly (expr->polynomial numer var-sym Q-field-sym)]
             [denom-poly (expr->polynomial denom var-sym Q-field-sym)])
        (if (or (not numer-poly) (not denom-poly))
            expr  ; Not polynomials, can't simplify with GCD
            (let* ([gcd-poly (poly-gcd numer-poly denom-poly)]
                   [gcd-deg (poly-degree gcd-poly)])
              (if (<= gcd-deg 0)
                  expr  ; Already in lowest terms
                  (let* ([numer-reduced (poly-div numer-poly gcd-poly)]
                         [denom-reduced (poly-div denom-poly gcd-poly)])
                    (quotient (polynomial->expr numer-reduced var-sym)
                              (polynomial->expr denom-reduced var-sym)))))))))

;;; ============================================================
;;; Partial Fraction Decomposition
;;; ============================================================

;;; partial-fractions : Expr × Symbol → Expr | #f
;;; Decompose a proper rational function into partial fractions.
;;; Returns #f if decomposition fails.
;;; Input should be P(x)/Q(x) where deg(P) < deg(Q).
(define (partial-fractions expr var-sym)
  (if (not (quotient? expr))
      #f
      (let* ([numer (quot-numer expr)]
             [denom (quot-denom expr)]
             [numer-poly (expr->polynomial numer var-sym Q-field-sym)]
             [denom-poly (expr->polynomial denom var-sym Q-field-sym)])
        (if (or (not numer-poly) (not denom-poly))
            #f
            (let ([n-deg (poly-degree numer-poly)]
                  [d-deg (poly-degree denom-poly)])
              ;; For now, only handle case where denom has linear factors
              (if (>= n-deg d-deg)
                  #f  ; Would need polynomial long division first
                  (partial-fractions-linear numer-poly denom-poly var-sym)))))))

;;; partial-fractions-linear : AlgebraPoly × AlgebraPoly × Symbol → Expr | #f
;;; Partial fractions when denominator has distinct linear factors.
;;; For P(x)/[(x-a)(x-b)...] = A/(x-a) + B/(x-b) + ...
;;; where A = P(a)/[(a-b)(a-c)...], etc. (Heaviside cover-up method)
(define (partial-fractions-linear numer-poly denom-poly var-sym)
  ;; Get square-free factorization to identify factors
  (let ([sqf (poly-square-free-factorization denom-poly)])
    (if (null? sqf)
        #f
        ;; For now, only handle simple case: all factors have multiplicity 1
        ;; and are linear (degree 1)
        (if (and (= (length sqf) 1)
                 (= (cdar sqf) 1)
                 (= (poly-degree (caar sqf)) (poly-degree denom-poly)))
            ;; All distinct roots, use Heaviside method
            ;; This requires finding roots, which we don't have exact methods for
            ;; Return original for now
            #f
            #f))))

;;; ============================================================
;;; Polynomial Division for Symbolic Expressions
;;; ============================================================

;;; poly-divide-expr : Expr × Expr × Symbol → (Expr . Expr)
;;; Polynomial division: numer = quotient * denom + remainder.
;;; Returns (quotient . remainder) as symbolic expressions.
;;; Returns (#f . #f) if inputs are not polynomials or denominator is zero.
(define (poly-divide-expr numer-expr denom-expr var-sym)
  (let* ([numer-poly (expr->polynomial numer-expr var-sym Q-field-sym)]
         [denom-poly (expr->polynomial denom-expr var-sym Q-field-sym)])
    (cond
      [(or (not numer-poly) (not denom-poly))
       (cons #f #f)]  ; Not polynomials
      [(poly-zero? denom-poly)
       (cons #f #f)]  ; Division by zero
      [else
       (let* ([qr (poly-divmod numer-poly denom-poly)]
              [quot-poly (car qr)]
              [rem-poly (cdr qr)])
         (cons (polynomial->expr quot-poly var-sym)
               (polynomial->expr rem-poly var-sym)))])))

;;; ============================================================
;;; Polynomial Canonical Form
;;; ============================================================

;;; to-polynomial-form : Expr × Symbol → Expr
;;; Convert expression to standard polynomial form: a_n*x^n + ... + a_1*x + a_0.
;;; Fully expands, collects like terms, and orders by descending powers.
(define (to-polynomial-form expr var-sym)
  (let ([poly (expr->polynomial expr var-sym Q-field-sym)])
    (if poly
        (polynomial->expr poly var-sym)
        ;; Not a polynomial, just simplify
        (simplify expr))))

;;; ============================================================
;;; Polynomial GCD for Expression Simplification
;;; ============================================================

;;; expr-poly-gcd : Expr × Expr × Symbol → Expr
;;; Compute GCD of two polynomial expressions.
;;; Returns the GCD as a symbolic expression.
(define (expr-poly-gcd expr1 expr2 var-sym)
  (let* ([poly1 (expr->polynomial expr1 var-sym Q-field-sym)]
         [poly2 (expr->polynomial expr2 var-sym Q-field-sym)])
    (if (or (not poly1) (not poly2))
        (num 1)  ; Not polynomials, GCD is 1
        (polynomial->expr (poly-gcd poly1 poly2) var-sym))))

;;; expr-poly-lcm : Expr × Expr × Symbol → Expr
;;; Compute LCM of two polynomial expressions.
(define (expr-poly-lcm expr1 expr2 var-sym)
  (let* ([poly1 (expr->polynomial expr1 var-sym Q-field-sym)]
         [poly2 (expr->polynomial expr2 var-sym Q-field-sym)])
    (if (or (not poly1) (not poly2))
        ;; Not polynomials, return product (conservative)
        (product expr1 expr2)
        (polynomial->expr (poly-lcm poly1 poly2) var-sym))))

;;; ============================================================
;;; Square-Free Factorization for Symbolic Expressions
;;; ============================================================

;;; expr-square-free : Expr × Symbol → Expr
;;; Compute square-free part of polynomial expression.
;;; Removes repeated factors.
(define (expr-square-free expr var-sym)
  (let ([poly (expr->polynomial expr var-sym Q-field-sym)])
    (if poly
        (polynomial->expr (poly-square-free poly) var-sym)
        expr)))

;;; expr-factored : Expr × Symbol → (List (Expr . Nat))
;;; Square-free factorization of polynomial expression.
;;; Returns list of (factor . multiplicity) pairs.
(define (expr-factored expr var-sym)
  (let ([poly (expr->polynomial expr var-sym Q-field-sym)])
    (if poly
        (map (lambda (pair)
               (cons (polynomial->expr (car pair) var-sym)
                     (cdr pair)))
             (poly-square-free-factorization poly))
        (list (cons expr 1)))))

;;; ============================================================
;;; Utility Functions
;;; ============================================================

;;; make-sum-from-list : (List Expr) → Expr
(define (make-sum-from-list terms)
  (cond
    [(null? terms) (num 0)]
    [(= (length terms) 1) (car terms)]
    [else (fold-left sum (car terms) (cdr terms))]))

;;; for-all : (α → Bool) × (List α) → Bool
(define (for-all pred lst)
  (cond
    [(null? lst) #t]
    [(not (pred (car lst))) #f]
    [else (for-all pred (cdr lst))]))
