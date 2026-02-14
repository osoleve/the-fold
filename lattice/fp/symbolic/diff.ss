;;; @module diff
;;; @requires expr

(require 'expr)

(doc 'module 'diff)
(doc 'description "Symbolic computation of derivatives for algebraic expressions")
(doc 'features "Derivative rules, partial derivatives, gradients, Jacobians, Hessians, curl, divergence, Laplacian")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'section 'helper-functions)

(define (fold-sum exprs)
  (doc 'type '(-> (List Expr) Expr))
  (doc 'description "Fold list of expressions into sum using smart constructors")
  (cond
   [(null? exprs) (num 0)]
   [(null? (cdr exprs)) (car exprs)]
   [else (sum (car exprs) (fold-sum (cdr exprs)))]))

(define (fold-product exprs)
  (doc 'type '(-> (List Expr) Expr))
  (doc 'description "Fold list of expressions into product using smart constructors")
  (cond
   [(null? exprs) (num 1)]
   [(null? (cdr exprs)) (car exprs)]
   [else (product (car exprs) (fold-product (cdr exprs)))]))

(doc 'section 'basic-differentiation)

(define (deriv expr var-sym)
  (doc 'type '(-> Expr Symbol Expr))
  (doc 'description "Compute derivative of expr with respect to var-sym using standard calculus rules")
  (cond
   ;; d/dx[c] = 0 (constant rule)
   [(num? expr) (num 0)]

   ;; d/dx[x] = 1, d/dx[y] = 0 (variable rule)
   [(var? expr)
    (if (eq? (var-name expr) var-sym)
        (num 1)
        (num 0))]

   ;; d/dx[f + g] = f' + g' (sum rule)
   [(sum? expr)
    (let ([terms (sum-terms expr)])
         (fold-sum (map (lambda (e) (deriv e var-sym)) terms)))]

   ;; d/dx[f * g] = f' * g + f * g' (product rule)
   [(product? expr)
    (let ([factors (product-factors expr)])
         (cond
          ;; Binary product: use standard product rule
          [(= (length factors) 2)
           (let ([f (car factors)]
                 [g (cadr factors)])
                (sum (product (deriv f var-sym) g)
                     (product f (deriv g var-sym))))]
          ;; N-ary product: generalized product rule
          ;; d/dx[f1*f2*...*fn] = Σ(f1*...*fi'*...*fn)
          [else
           (fold-sum
            (map (lambda (i)
                         (fold-product
                          (map (lambda (j e)
                                       (if (= i j)
                                           (deriv e var-sym)
                                           e))
                               (iota (length factors))
                               factors)))
                 (iota (length factors))))]))]

   ;; d/dx[f - g] = f' - g' (difference rule)
   [(difference? expr)
    (if (diff-right expr)
        ;; Binary difference
        (difference (deriv (diff-left expr) var-sym)
                    (deriv (diff-right expr) var-sym))
        ;; Unary negation: d/dx[-f] = -f'
        (make-neg (deriv (diff-left expr) var-sym)))]

   ;; d/dx[f / g] = (f' * g - f * g') / g^2 (division rule)
   [(quotient? expr)
    (let ([f (quot-numer expr)]
          [g (quot-denom expr)])
         (division
          (difference (product (deriv f var-sym) g)
                      (product f (deriv g var-sym)))
          (power g (num 2))))]

   ;; d/dx[f^g] = f^g * (g' * ln(f) + g * f'/f) (generalized power rule)
   ;; Special cases:
   ;;   - f^n (constant exponent): n * f^(n-1) * f'
   ;;   - a^g (constant base): a^g * ln(a) * g'
   [(power? expr)
    (let ([f (pow-base expr)]
          [g (pow-exp expr)])
         (cond
          ;; x^n where n is constant: n * x^(n-1) * dx
          [(num? g)
           (product (product g (power f (difference g (num 1))))
                    (deriv f var-sym))]
          ;; a^x where a is constant: a^x * ln(a) * dx
          [(num? f)
           (product (product expr (sym-log f))
                    (deriv g var-sym))]
          ;; General case: f^g = exp(g * ln(f))
          ;; d/dx[f^g] = f^g * d/dx[g * ln(f)]
          ;; = f^g * (g' * ln(f) + g * f'/f)
          [else
           (product expr
                    (sum (product (deriv g var-sym) (sym-log f))
                         (product g (division (deriv f var-sym) f))))]))]

   ;; d/dx[fn(u)] = fn'(u) * u' (chain rule)
   [(app? expr)
    (let ([fn (app-fn expr)]
          [u (app-arg expr)]
          [du (deriv (app-arg expr) var-sym)])
         (product (deriv-fn fn u) du))]

   ;; Unknown expression type
   [else (num 0)]))

(define (deriv-fn fn arg)
  (doc 'type '(-> Symbol Expr Expr))
  (doc 'description "Compute derivative of named function, returns derivative evaluated at argument")
  (case fn
        ;; d/du[sin(u)] = cos(u)
        [(sin) (sym-cos arg)]
        ;; d/du[cos(u)] = -sin(u)
        [(cos) (make-neg (sym-sin arg))]
        ;; d/du[tan(u)] = sec^2(u) = 1/cos^2(u)
        [(tan) (division (num 1) (power (sym-cos arg) (num 2)))]
        ;; d/du[exp(u)] = exp(u)
        [(exp) (sym-exp arg)]
        ;; d/du[log(u)] = 1/u
        [(log) (division (num 1) arg)]
        ;; d/du[sqrt(u)] = 1/(2*sqrt(u))
        [(sqrt) (division (num 1) (product (num 2) (sym-sqrt arg)))]
        ;; d/du[asin(u)] = 1/sqrt(1-u^2)
        [(asin) (division (num 1) (sym-sqrt (difference (num 1) (power arg (num 2)))))]
        ;; d/du[acos(u)] = -1/sqrt(1-u^2)
        [(acos) (make-neg (division (num 1) (sym-sqrt (difference (num 1) (power arg (num 2))))))]
        ;; d/du[atan(u)] = 1/(1+u^2)
        [(atan) (division (num 1) (sum (num 1) (power arg (num 2))))]
        ;; d/du[sinh(u)] = cosh(u)
        [(sinh) (make-app 'cosh arg)]
        ;; d/du[cosh(u)] = sinh(u)
        [(cosh) (make-app 'sinh arg)]
        ;; d/du[tanh(u)] = sech^2(u) = 1/cosh^2(u)
        [(tanh) (division (num 1) (power (make-app 'cosh arg) (num 2)))]
        ;; Unknown function: return symbolic derivative
        [else (make-app 'D (make-app fn arg))]))

(doc 'section 'partial-derivatives)

(define (partial expr var-sym)
  (doc 'type '(-> Expr Symbol Expr))
  (doc 'description "Compute partial derivative ∂expr/∂var-sym (alias for deriv)")
  (deriv expr var-sym))

(doc 'section 'gradient)

(define (gradient expr vars)
  (doc 'type '(-> Expr (List Symbol) (List Expr)))
  (doc 'description "Compute gradient ∇f = [∂f/∂x₁, ∂f/∂x₂, ..., ∂f/∂xₙ]")
  (map (lambda (v) (partial expr v)) vars))

(doc 'section 'jacobian)

(define (jacobian exprs vars)
  (doc 'type '(-> (List Expr) (List Symbol) (List (List Expr))))
  (doc 'description "Compute Jacobian matrix for vector-valued function f: ℝⁿ → ℝᵐ")
  (doc 'note "exprs = [f₁, ..., fₘ], vars = [x₁, ..., xₙ], returns m×n matrix where J[i][j] = ∂fᵢ/∂xⱼ")
  (map (lambda (expr) (gradient expr vars)) exprs))

(doc 'section 'hessian)

(define (hessian expr vars)
  (doc 'type '(-> Expr (List Symbol) (List (List Expr))))
  (doc 'description "Compute Hessian matrix H = [∂²f/∂xᵢ∂xⱼ]")
  (doc 'note "Returns n×n symmetric matrix of second partial derivatives")
  (map (lambda (vi)
               (map (lambda (vj)
                            (deriv (deriv expr vi) vj))
                    vars))
       vars))

(doc 'section 'higher-order-derivatives)

(define (deriv-n expr var-sym n)
  (doc 'type '(-> Expr Symbol Nat Expr))
  (doc 'description "Compute nth derivative of expr with respect to var-sym")
  (if (<= n 0)
      expr
      (deriv-n (deriv expr var-sym) var-sym (- n 1))))

(doc 'section 'directional-derivative)

(define (directional-derivative expr vars direction)
  (doc 'type '(-> Expr (List Symbol) (List Expr) Expr))
  (doc 'description "Compute directional derivative ∇f · v")
  (doc 'note "Returns ∇f · v = Σ(∂f/∂xᵢ * vᵢ)")
  (let ([grad (gradient expr vars)])
       (fold-sum (map product grad direction))))

(doc 'section 'curl)

(define (curl field vars)
  (doc 'type '(-> (List Expr) (List Symbol) (List Expr)))
  (doc 'description "Compute curl of 3D vector field F = [F₁, F₂, F₃]")
  (doc 'note "vars should be [x, y, z], returns [∂F₃/∂y - ∂F₂/∂z, ∂F₁/∂z - ∂F₃/∂x, ∂F₂/∂x - ∂F₁/∂y]")
  (let ([F1 (car field)]
        [F2 (cadr field)]
        [F3 (caddr field)]
        [x (car vars)]
        [y (cadr vars)]
        [z (caddr vars)])
       (list
        (difference (deriv F3 y) (deriv F2 z))
        (difference (deriv F1 z) (deriv F3 x))
        (difference (deriv F2 x) (deriv F1 y)))))

(doc 'section 'divergence)

(define (divergence field vars)
  (doc 'type '(-> (List Expr) (List Symbol) Expr))
  (doc 'description "Compute divergence of vector field F = [F₁, F₂, ..., Fₙ]")
  (doc 'note "Returns ∇ · F = Σ(∂Fᵢ/∂xᵢ)")
  (fold-sum (map deriv field vars)))

(doc 'section 'laplacian)

(define (laplacian expr vars)
  (doc 'type '(-> Expr (List Symbol) Expr))
  (doc 'description "Compute Laplacian ∇²f = Σ(∂²f/∂xᵢ²)")
  (doc 'note "Returns sum of all unmixed second partial derivatives")
  (fold-sum (map (lambda (v) (deriv (deriv expr v) v)) vars)))
