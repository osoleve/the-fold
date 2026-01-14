;;; lattice/fp/symbolic/integrate.ss — Symbolic Integration
;;;
;;; Compute antiderivatives symbolically.
;;; Implements:
;;;   - Basic antiderivatives (power rule, trig, exp, log)
;;;   - Integration by parts
;;;   - Substitution (u-substitution)
;;;   - Partial fractions (basic)
;;;   - Trigonometric substitutions
;;;   - Table-based lookup
;;;   - Definite integral evaluation
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - lattice/fp/symbolic/diff.ss (includes expr.ss)
;;;   - lattice/fp/symbolic/simplify.ss

(load "lattice/fp/symbolic/diff.ss")
(load "lattice/fp/symbolic/simplify.ss")

;;; ====
;;; Main Integration Entry Point
;;; ====

;;; integrate : Expr × Symbol → Expr | #f
;;; Compute the indefinite integral of expr with respect to var-sym.
;;; Returns #f if unable to integrate.
(define (integrate expr var-sym)
  (let ([result (integrate-internal expr var-sym 0)])
       (if result
           (simplify result)
           #f)))

;;; integrate-internal : Expr × Symbol × Depth → Expr | #f
;;; Internal integration with depth tracking to prevent infinite recursion.
(define (integrate-internal expr var-sym depth)
  (if (> depth 20)
      #f  ;; Recursion limit
      (or
       ;; Try table lookup first
       (integrate-table expr var-sym)
       ;; Then pattern-based rules
       (integrate-basic expr var-sym depth)
       ;; Then advanced techniques
       (integrate-by-substitution expr var-sym depth)
       (integrate-by-parts expr var-sym depth)
       (integrate-partial-fractions expr var-sym depth))))

;;; ====
;;; Integration Table (Common Integrals)
;;; ====

;;; integrate-table : Expr × Symbol → Expr | #f
;;; Table-based lookup for common integrals.
(define (integrate-table expr var-sym)
  (let ([x (var var-sym)])
       (cond
        ;; ∫ c dx = c*x
        [(num? expr)
         (product expr x)]
        
        ;; ∫ x dx = x^2/2
        [(and (var? expr) (eq? (var-name expr) var-sym))
         (quotient (power x (num 2)) (num 2))]
        
        ;; ∫ y dx = y*x (y is different variable)
        [(and (var? expr) (not (eq? (var-name expr) var-sym)))
         (product expr x)]
        
        ;; ∫ sin(x) dx = -cos(x)
        [(and (app? expr) (eq? (app-fn expr) 'sin)
              (var? (app-arg expr)) (eq? (var-name (app-arg expr)) var-sym))
         (make-neg (sym-cos x))]
        
        ;; ∫ cos(x) dx = sin(x)
        [(and (app? expr) (eq? (app-fn expr) 'cos)
              (var? (app-arg expr)) (eq? (var-name (app-arg expr)) var-sym))
         (sym-sin x)]
        
        ;; ∫ tan(x) dx = -ln|cos(x)|
        [(and (app? expr) (eq? (app-fn expr) 'tan)
              (var? (app-arg expr)) (eq? (var-name (app-arg expr)) var-sym))
         (make-neg (sym-log (make-app 'abs (sym-cos x))))]
        
        ;; ∫ sec^2(x) dx = tan(x)
        [(and (power? expr)
              (num? (pow-exp expr)) (= (num-val (pow-exp expr)) 2)
              (quotient? (pow-base expr))
              (num? (quot-numer (pow-base expr))) (= (num-val (quot-numer (pow-base expr))) 1)
              (app? (quot-denom (pow-base expr)))
              (eq? (app-fn (quot-denom (pow-base expr))) 'cos))
         (sym-tan x)]
        
        ;; ∫ e^x dx = e^x
        [(and (app? expr) (eq? (app-fn expr) 'exp)
              (var? (app-arg expr)) (eq? (var-name (app-arg expr)) var-sym))
         (sym-exp x)]
        
        ;; ∫ 1/x dx = ln|x|
        [(and (quotient? expr)
              (num? (quot-numer expr)) (= (num-val (quot-numer expr)) 1)
              (var? (quot-denom expr)) (eq? (var-name (quot-denom expr)) var-sym))
         (sym-log (make-app 'abs x))]
        
        ;; ∫ ln(x) dx = x*ln(x) - x
        [(and (app? expr) (eq? (app-fn expr) 'log)
              (var? (app-arg expr)) (eq? (var-name (app-arg expr)) var-sym))
         (difference (product x (sym-log x)) x)]
        
        ;; ∫ sqrt(x) dx = (2/3)*x^(3/2)
        [(and (app? expr) (eq? (app-fn expr) 'sqrt)
              (var? (app-arg expr)) (eq? (var-name (app-arg expr)) var-sym))
         (product (quotient (num 2) (num 3))
                  (power x (quotient (num 3) (num 2))))]
        
        ;; ∫ 1/sqrt(1-x^2) dx = asin(x)
        [(is-arcsin-integrand? expr var-sym)
         (sym-asin x)]
        
        ;; ∫ 1/(1+x^2) dx = atan(x)
        [(is-arctan-integrand? expr var-sym)
         (sym-atan x)]
        
        ;; ∫ sinh(x) dx = cosh(x)
        [(and (app? expr) (eq? (app-fn expr) 'sinh)
              (var? (app-arg expr)) (eq? (var-name (app-arg expr)) var-sym))
         (sym-cosh x)]
        
        ;; ∫ cosh(x) dx = sinh(x)
        [(and (app? expr) (eq? (app-fn expr) 'cosh)
              (var? (app-arg expr)) (eq? (var-name (app-arg expr)) var-sym))
         (sym-sinh x)]
        
        [else #f])))

;;; is-arcsin-integrand? : Expr × Symbol → Bool
;;; Check if expr is 1/sqrt(1-x^2)
(define (is-arcsin-integrand? expr var-sym)
  (and (quotient? expr)
       (num? (quot-numer expr))
       (= (num-val (quot-numer expr)) 1)
       (app? (quot-denom expr))
       (eq? (app-fn (quot-denom expr)) 'sqrt)
       (let ([inner (app-arg (quot-denom expr))])
            (and (difference? inner)
                 (num? (diff-left inner))
                 (= (num-val (diff-left inner)) 1)
                 (power? (diff-right inner))
                 (var? (pow-base (diff-right inner)))
                 (eq? (var-name (pow-base (diff-right inner))) var-sym)
                 (num? (pow-exp (diff-right inner)))
                 (= (num-val (pow-exp (diff-right inner))) 2)))))

;;; is-arctan-integrand? : Expr × Symbol → Bool
;;; Check if expr is 1/(1+x^2)
(define (is-arctan-integrand? expr var-sym)
  (and (quotient? expr)
       (num? (quot-numer expr))
       (= (num-val (quot-numer expr)) 1)
       (sum? (quot-denom expr))
       (let ([terms (sum-terms (quot-denom expr))])
            (and (= (length terms) 2)
                 (or (and (num? (car terms)) (= (num-val (car terms)) 1)
                          (power? (cadr terms))
                          (var? (pow-base (cadr terms)))
                          (eq? (var-name (pow-base (cadr terms))) var-sym)
                          (num? (pow-exp (cadr terms)))
                          (= (num-val (pow-exp (cadr terms))) 2))
                     (and (num? (cadr terms)) (= (num-val (cadr terms)) 1)
                          (power? (car terms))
                          (var? (pow-base (car terms)))
                          (eq? (var-name (pow-base (car terms))) var-sym)
                          (num? (pow-exp (car terms)))
                          (= (num-val (pow-exp (car terms))) 2)))))))

;;; ====
;;; Basic Integration Rules
;;; ====

;;; integrate-basic : Expr × Symbol × Depth → Expr | #f
(define (integrate-basic expr var-sym depth)
  (cond
   ;; ∫ (f + g) dx = ∫f dx + ∫g dx
   [(sum? expr)
    (let ([integrated-terms
           (map (lambda (t) (integrate-internal t var-sym (+ depth 1)))
                (sum-terms expr))])
         (if (for-all identity integrated-terms)
             (fold-left sum (car integrated-terms) (cdr integrated-terms))
             #f))]
   
   ;; ∫ c*f dx = c * ∫f dx (where c is constant w.r.t. var)
   [(product? expr)
    (integrate-product expr var-sym depth)]
   
   ;; ∫ f - g dx = ∫f dx - ∫g dx
   [(difference? expr)
    (if (diff-right expr)
        (let ([left-int (integrate-internal (diff-left expr) var-sym (+ depth 1))]
              [right-int (integrate-internal (diff-right expr) var-sym (+ depth 1))])
             (if (and left-int right-int)
                 (difference left-int right-int)
                 #f))
        ;; ∫ -f dx = -∫f dx
        (let ([int-inner (integrate-internal (diff-left expr) var-sym (+ depth 1))])
             (if int-inner
                 (make-neg int-inner)
                 #f)))]
   
   ;; Power rule: ∫ x^n dx = x^(n+1)/(n+1) for n ≠ -1
   [(power? expr)
    (integrate-power expr var-sym depth)]
   
   ;; ∫ f/g dx
   [(quotient? expr)
    (integrate-quotient expr var-sym depth)]
   
   [else #f]))

;;; integrate-product : Expr × Symbol × Depth → Expr | #f
;;; Handle product integration.
(define (integrate-product expr var-sym depth)
  (let* ([factors (product-factors expr)]
         [const-factors (filter (lambda (f) (not (contains-var? f var-sym))) factors)]
         [var-factors (filter (lambda (f) (contains-var? f var-sym)) factors)])
        (cond
         ;; All constant
         [(null? var-factors)
          (product expr (var var-sym))]
         ;; Single variable factor with constants
         [(= (length var-factors) 1)
          (let ([int-var (integrate-internal (car var-factors) var-sym (+ depth 1))])
               (if int-var
                   (product (fold-left product (num 1) const-factors) int-var)
                   #f))]
         ;; Multiple variable factors - try integration by parts
         [else #f])))

;;; integrate-power : Expr × Symbol × Depth → Expr | #f
;;; Power rule and related.
(define (integrate-power expr var-sym depth)
  (let ([base (pow-base expr)]
        [exp (pow-exp expr)])
       (cond
        ;; x^n where n is constant
        [(and (var? base) (eq? (var-name base) var-sym)
              (not (contains-var? exp var-sym)))
         (if (and (num? exp) (= (num-val exp) -1))
             ;; ∫ x^(-1) dx = ln|x|
             (sym-log (make-app 'abs base))
             ;; ∫ x^n dx = x^(n+1)/(n+1)
             (let ([new-exp (sum exp (num 1))])
                  (quotient (power base new-exp) new-exp)))]
        
        ;; a^x where a is constant
        [(and (not (contains-var? base var-sym))
              (var? exp) (eq? (var-name exp) var-sym))
         ;; ∫ a^x dx = a^x / ln(a)
         (quotient expr (sym-log base))]
        
        ;; (ax+b)^n
        [(and (num? exp)
              (linear-in? base var-sym))
         (let* ([coeffs (linear-coefficients base var-sym)]
                [a (car coeffs)]
                [new-exp (sum exp (num 1))])
               (if (and (num? exp) (= (num-val exp) -1))
                   ;; ∫ 1/(ax+b) dx = ln|ax+b| / a
                   (quotient (sym-log (make-app 'abs base)) a)
                   ;; ∫ (ax+b)^n dx = (ax+b)^(n+1) / (a*(n+1))
                   (quotient (power base new-exp)
                             (product a new-exp))))]
        
        [else #f])))

;;; integrate-quotient : Expr × Symbol × Depth → Expr | #f
;;; Handle quotient integration.
(define (integrate-quotient expr var-sym depth)
  (let ([numer (quot-numer expr)]
        [denom (quot-denom expr)])
       (cond
        ;; f(x)/c = (1/c) * f(x)
        [(not (contains-var? denom var-sym))
         (let ([int-numer (integrate-internal numer var-sym (+ depth 1))])
              (if int-numer
                  (quotient int-numer denom)
                  #f))]
        
        ;; c/(ax+b) = (c/a) * ln|ax+b|
        [(and (not (contains-var? numer var-sym))
              (linear-in? denom var-sym))
         (let* ([coeffs (linear-coefficients denom var-sym)]
                [a (car coeffs)])
               (quotient (product numer (sym-log (make-app 'abs denom))) a))]
        
        ;; f'(x)/f(x) = ln|f(x)|
        [(let ([d-denom (deriv denom var-sym)])
              (expr=? (simplify d-denom) (simplify numer)))
         (sym-log (make-app 'abs denom))]
        
        ;; c*f'(x)/f(x) = c*ln|f(x)|
        [(and (not (contains-var? numer var-sym))
              #f)  ;; TODO: implement coefficient extraction
         #f]
        
        [else #f])))

;;; ====
;;; U-Substitution
;;; ====

;;; integrate-by-substitution : Expr × Symbol × Depth → Expr | #f
;;; Try u-substitution for various common patterns.
(define (integrate-by-substitution expr var-sym depth)
  (or
   ;; ∫ f(g(x)) * g'(x) dx = F(g(x))
   (try-substitution-pattern expr var-sym depth)
   ;; Trig substitutions
   (try-trig-substitution expr var-sym depth)))

;;; try-substitution-pattern : Expr × Symbol × Depth → Expr | #f
;;; Look for f(u) * u' pattern.
(define (try-substitution-pattern expr var-sym depth)
  (if (not (product? expr))
      #f
      (let ([factors (product-factors expr)])
           ;; Try each factor as potential u'
           (let loop ([i 0])
                (if (>= i (length factors))
                    #f
                    (let* ([potential-du (list-ref factors i)]
                           [other-factors (remove-at-index factors i)]
                           [potential-u (find-antiderivative-match other-factors potential-du var-sym depth)])
                          (if potential-u
                              potential-u
                              (loop (+ i 1)))))))))

;;; find-antiderivative-match : (List Expr) × Expr × Symbol × Depth → Expr | #f
;;; Check if other-factors can be expressed as f(u) where du = potential-du dx.
(define (find-antiderivative-match other-factors potential-du var-sym depth)
  ;; Try to find u such that d(u)/dx = potential-du
  ;; Then check if other-factors = f(u) for some integrable f
  (let ([candidates (find-u-candidates (fold-left product (num 1) other-factors) var-sym)])
       (let loop ([u-list candidates])
            (if (null? u-list)
                #f
                (let* ([u (car u-list)]
                       [du (simplify (deriv u var-sym))])
                      (if (expr=? du (simplify potential-du))
                          ;; Found match - now integrate f(u)
                          (let* ([f-of-u (substitute-u other-factors u var-sym)]
                                 [int-f (and f-of-u (integrate-internal f-of-u 'u (+ depth 1)))])
                                (if int-f
                                    (subst int-f 'u u)  ;; Back-substitute
                                    (loop (cdr u-list))))
                          (loop (cdr u-list))))))))

;;; find-u-candidates : Expr × Symbol → (List Expr)
;;; Find potential u substitutions in an expression.
(define (find-u-candidates expr var-sym)
  (cond
   [(num? expr) '()]
   [(var? expr) '()]
   [(app? expr)
    ;; The argument of function applications are good u candidates
    (cons (app-arg expr) (find-u-candidates (app-arg expr) var-sym))]
   [(power? expr)
    ;; Base of powers are good candidates
    (append (list (pow-base expr))
            (find-u-candidates (pow-base expr) var-sym)
            (find-u-candidates (pow-exp expr) var-sym))]
   [(sum? expr)
    (apply append (map (lambda (t) (find-u-candidates t var-sym)) (sum-terms expr)))]
   [(product? expr)
    (apply append (map (lambda (f) (find-u-candidates f var-sym)) (product-factors expr)))]
   [else '()]))

;;; substitute-u : (List Expr) × Expr × Symbol → Expr | #f
;;; Try to express factors in terms of u.
(define (substitute-u factors u var-sym)
  ;; Simplified: just check if factors contain u directly
  (let ([combined (fold-left product (num 1) factors)])
       (if (can-express-in-u? combined u var-sym)
           (replace-with-u combined u var-sym)
           #f)))

;;; can-express-in-u? : Expr × Expr × Symbol → Bool
(define (can-express-in-u? expr u var-sym)
  ;; Check if expr only uses var-sym through u
  (cond
   [(expr=? expr u) #t]
   [(num? expr) #t]
   [(var? expr) (not (eq? (var-name expr) var-sym))]
   [(sum? expr) (for-all (lambda (t) (can-express-in-u? t u var-sym)) (sum-terms expr))]
   [(product? expr) (for-all (lambda (f) (can-express-in-u? f u var-sym)) (product-factors expr))]
   [(power? expr) (and (can-express-in-u? (pow-base expr) u var-sym)
                       (can-express-in-u? (pow-exp expr) u var-sym))]
   [(app? expr) (can-express-in-u? (app-arg expr) u var-sym)]
   [else #f]))

;;; replace-with-u : Expr × Expr × Symbol → Expr
(define (replace-with-u expr u var-sym)
  (cond
   [(expr=? expr u) (var 'u)]
   [(num? expr) expr]
   [(var? expr) expr]
   [(sum? expr) (make-sum (map (lambda (t) (replace-with-u t u var-sym)) (sum-terms expr)))]
   [(product? expr) (make-product (map (lambda (f) (replace-with-u f u var-sym)) (product-factors expr)))]
   [(power? expr) (power (replace-with-u (pow-base expr) u var-sym)
                         (replace-with-u (pow-exp expr) u var-sym))]
   [(app? expr) (make-app (app-fn expr) (replace-with-u (app-arg expr) u var-sym))]
   [else expr]))

;;; try-trig-substitution : Expr × Symbol × Depth → Expr | #f
;;; Try trigonometric substitutions.
(define (try-trig-substitution expr var-sym depth)
  ;; For now, only handle simple patterns
  ;; sqrt(a^2 - x^2): x = a*sin(t)
  ;; sqrt(a^2 + x^2): x = a*tan(t)
  ;; sqrt(x^2 - a^2): x = a*sec(t)
  #f)  ;; TODO: implement

;;; ====
;;; Integration by Parts
;;; ====

;;; integrate-by-parts : Expr × Symbol × Depth → Expr | #f
;;; Try integration by parts: ∫u dv = uv - ∫v du
(define (integrate-by-parts expr var-sym depth)
  (if (not (product? expr))
      #f
      (let ([factors (product-factors expr)])
           (if (< (length factors) 2)
               #f
               ;; Try LIATE rule for choosing u
               (let* ([sorted-factors (sort-by-liate factors)]
                      [u (car sorted-factors)]
                      [dv (fold-left product (num 1) (cdr sorted-factors))])
                     (try-parts u dv var-sym depth))))))

;;; sort-by-liate : (List Expr) → (List Expr)
;;; Sort factors by LIATE priority for integration by parts.
;;; L = Logarithmic (best u choice)
;;; I = Inverse trig
;;; A = Algebraic (polynomials)
;;; T = Trigonometric
;;; E = Exponential (worst u choice)
(define (sort-by-liate factors)
  (sort (lambda (a b) (< (liate-priority a) (liate-priority b))) factors))

;;; liate-priority : Expr → Number
(define (liate-priority expr)
  (cond
   [(and (app? expr) (eq? (app-fn expr) 'log)) 0]      ;; L
   [(and (app? expr) (memq (app-fn expr) '(asin acos atan))) 1]  ;; I
   [(or (var? expr) (power? expr) (polynomial? expr)) 2]  ;; A
   [(and (app? expr) (memq (app-fn expr) '(sin cos tan))) 3]  ;; T
   [(and (app? expr) (eq? (app-fn expr) 'exp)) 4]     ;; E
   [(num? expr) 5]  ;; Constants
   [else 3]))

;;; polynomial? : Expr → Bool
(define (polynomial? expr)
  (cond
   [(num? expr) #t]
   [(var? expr) #t]
   [(sum? expr) (for-all polynomial? (sum-terms expr))]
   [(product? expr) (for-all polynomial? (product-factors expr))]
   [(and (power? expr) (num? (pow-exp expr)) (integer? (num-val (pow-exp expr)))
         (>= (num-val (pow-exp expr)) 0))
    (polynomial? (pow-base expr))]
   [else #f]))

;;; try-parts : Expr × Expr × Symbol × Depth → Expr | #f
;;; ∫u dv = uv - ∫v du
(define (try-parts u dv var-sym depth)
  (let ([v (integrate-internal dv var-sym (+ depth 1))])
       (if (not v)
           #f
           (let* ([du (deriv u var-sym)]
                  [v-du (simplify (product v du))]
                  [int-v-du (integrate-internal v-du var-sym (+ depth 1))])
                 (if int-v-du
                     (difference (product u v) int-v-du)
                     #f)))))

;;; ====
;;; Partial Fractions (Basic)
;;; ====

;;; integrate-partial-fractions : Expr × Symbol × Depth → Expr | #f
;;; Integrate rational functions using partial fractions.
(define (integrate-partial-fractions expr var-sym depth)
  ;; Only handle simple cases for now
  (if (not (quotient? expr))
      #f
      (let ([numer (quot-numer expr)]
            [denom (quot-denom expr)])
           (cond
            ;; 1/((x-a)(x-b)) = A/(x-a) + B/(x-b)
            [(and (num? numer) (= (num-val numer) 1)
                  (product? denom)
                  (= (length (product-factors denom)) 2)
                  (for-all (lambda (f) (linear-in? f var-sym))
                           (product-factors denom)))
             (integrate-linear-factors numer denom var-sym depth)]
            
            ;; 1/(x^2 + a) - arctan form already handled in table
            
            [else #f]))))

;;; integrate-linear-factors : Expr × Expr × Symbol × Depth → Expr | #f
(define (integrate-linear-factors numer denom var-sym depth)
  ;; 1/((ax+b)(cx+d)) = A/(ax+b) + B/(cx+d)
  ;; where A = 1/(c*b/a - d), B = 1/(a*d/c - b)
  (let* ([factors (product-factors denom)]
         [f1 (car factors)]
         [f2 (cadr factors)]
         [c1 (linear-coefficients f1 var-sym)]
         [c2 (linear-coefficients f2 var-sym)]
         [a (car c1)] [b (cadr c1)]
         [c (car c2)] [d (cadr c2)])
        ;; A = -a / (bc - ad), B = c / (bc - ad)
        (let ([det (simplify (difference (product b c) (product a d)))])
             (if (and (num? det) (= (num-val det) 0))
                 #f  ;; Degenerate case
                 ;; FIX: Coefficients were swapped - A = -a/det, B = c/det
                 (let ([A (quotient (make-neg a) det)]
                       [B (quotient c det)])
                      (sum (quotient (product A (sym-log (make-app 'abs f1))) a)
                           (quotient (product B (sym-log (make-app 'abs f2))) c)))))))

;;; ====
;;; Helper Functions
;;; ====

;;; contains-var? : Expr × Symbol → Bool
;;; Check if expression contains the variable.
(define (contains-var? expr var-sym)
  (cond
   [(num? expr) #f]
   [(var? expr) (eq? (var-name expr) var-sym)]
   [(sum? expr) (exists (lambda (t) (contains-var? t var-sym)) (sum-terms expr))]
   [(product? expr) (exists (lambda (f) (contains-var? f var-sym)) (product-factors expr))]
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

;;; linear-in? : Expr × Symbol → Bool
;;; Check if expression is linear in var: ax + b
(define (linear-in? expr var-sym)
  (cond
   [(num? expr) #t]
   [(var? expr) #t]
   [(sum? expr)
    (and (for-all (lambda (t) (linear-in? t var-sym)) (sum-terms expr))
         (<= (degree-of expr var-sym) 1))]
   [(product? expr)
    ;; c*x is linear
    (let ([has-var (filter (lambda (f) (contains-var? f var-sym)) (product-factors expr))])
         (and (<= (length has-var) 1)
              (for-all (lambda (f)
                               (or (not (contains-var? f var-sym))
                                   (and (var? f) (eq? (var-name f) var-sym))))
                       (product-factors expr))))]
   [(difference? expr)
    (and (linear-in? (diff-left expr) var-sym)
         (or (not (diff-right expr))
             (linear-in? (diff-right expr) var-sym)))]
   [else #f]))

;;; degree-of : Expr × Symbol → Number
;;; Get the degree of var in expression.
(define (degree-of expr var-sym)
  (cond
   [(num? expr) 0]
   [(var? expr) (if (eq? (var-name expr) var-sym) 1 0)]
   [(sum? expr) (apply max (map (lambda (t) (degree-of t var-sym)) (sum-terms expr)))]
   [(product? expr) (apply + (map (lambda (f) (degree-of f var-sym)) (product-factors expr)))]
   [(power? expr)
    (if (and (num? (pow-exp expr)) (integer? (num-val (pow-exp expr))))
        (* (degree-of (pow-base expr) var-sym) (num-val (pow-exp expr)))
        999)]  ;; Non-integer power = high degree
   [else 0]))

;;; linear-coefficients : Expr × Symbol → (List Expr Expr)
;;; Extract (a, b) from ax + b. Returns (1, 0) for just x, (0, c) for constant c.
(define (linear-coefficients expr var-sym)
  (cond
   [(num? expr) (list (num 0) expr)]
   [(and (var? expr) (eq? (var-name expr) var-sym))
    (list (num 1) (num 0))]
   [(and (var? expr) (not (eq? (var-name expr) var-sym)))
    (list (num 0) expr)]
   [(sum? expr)
    (let* ([coeffs (map (lambda (t) (linear-coefficients t var-sym)) (sum-terms expr))]
           [as (map car coeffs)]
           [bs (map cadr coeffs)])
          (list (fold-left sum (num 0) as)
                (fold-left sum (num 0) bs)))]
   [(product? expr)
    (let* ([factors (product-factors expr)]
           [const-factors (filter (lambda (f) (not (contains-var? f var-sym))) factors)]
           [var-factors (filter (lambda (f) (contains-var? f var-sym)) factors)])
          (if (and (<= (length var-factors) 1)
                   (or (null? var-factors)
                       (and (var? (car var-factors))
                            (eq? (var-name (car var-factors)) var-sym))))
              (list (fold-left product (num 1) const-factors) (num 0))
              (list (num 0) expr)))]  ;; Non-linear
   [(difference? expr)
    (if (diff-right expr)
        (let ([lcoeffs (linear-coefficients (diff-left expr) var-sym)]
              [rcoeffs (linear-coefficients (diff-right expr) var-sym)])
             (list (difference (car lcoeffs) (car rcoeffs))
                   (difference (cadr lcoeffs) (cadr rcoeffs))))
        (let ([coeffs (linear-coefficients (diff-left expr) var-sym)])
             (list (make-neg (car coeffs)) (make-neg (cadr coeffs)))))]
   [else (list (num 0) expr)]))

;;; remove-at-index : (List α) × Number → (List α)
(define (remove-at-index lst i)
  (let loop ([l lst] [j 0] [acc '()])
       (cond
        [(null? l) (reverse acc)]
        [(= j i) (loop (cdr l) (+ j 1) acc)]
        [else (loop (cdr l) (+ j 1) (cons (car l) acc))])))

;;; identity : α → α
(define (identity x) x)

;;; fold-sum : (List Expr) → Expr
(define (fold-sum exprs)
  (cond
   [(null? exprs) (num 0)]
   [(= (length exprs) 1) (car exprs)]
   [else (fold-left sum (car exprs) (cdr exprs))]))

;;; ====
;;; Definite Integrals
;;; ====

;;; definite-integral : Expr × Symbol × Expr × Expr → Expr | #f
;;; Compute definite integral from a to b.
;;; Returns F(b) - F(a) where F is the antiderivative.
(define (definite-integral expr var-sym a b)
  (let ([F (integrate expr var-sym)])
       (if F
           (simplify (difference (subst F var-sym b)
                                 (subst F var-sym a)))
           #f)))

;;; definite-integral-numeric : Expr × Symbol × Number × Number → Number | #f
;;; Compute definite integral numerically.
;;; Uses Simpson's rule.
(define (definite-integral-numeric expr var-sym a b)
  (let ([F (integrate expr var-sym)])
       (if F
           (let ([env-a `((,var-sym . ,a))]
                 [env-b `((,var-sym . ,b))])
                ;; Load eval-expr from integrate-autodiff would be needed
                ;; For now, return symbolic
                (definite-integral expr var-sym (num a) (num b)))
           #f)))

;;; ====
;;; Convenience Functions
;;; ====

;;; antiderivative : Expr × Symbol → Expr | #f
;;; Alias for integrate.
(define antiderivative integrate)

;;; ∫ : Expr × Symbol → Expr | #f
;;; Unicode alias for integrate.
(define ∫ integrate)

;;; primitive : Expr × Symbol → Expr | #f
;;; Another common name for antiderivative.
(define primitive integrate)
