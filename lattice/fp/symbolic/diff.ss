;;; core/fp/symbolic/diff.ss — Symbolic Differentiation
;;;
;;; Symbolic computation of derivatives for algebraic expressions.
;;; Implements derivative rules, partial derivatives, gradients, Jacobians, and Hessians.
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - core/fp/symbolic/expr.ss

(load "lattice/fp/symbolic/expr.ss")

;;; ====
;;; Helper Functions for Simplification
;;; ====

;;; fold-sum : (List Expr) → Expr
;;; Fold a list of expressions into a sum using smart constructors.
(define (fold-sum exprs)
  (cond
   [(null? exprs) (num 0)]
   [(null? (cdr exprs)) (car exprs)]
   [else (sum (car exprs) (fold-sum (cdr exprs)))]))

;;; fold-product : (List Expr) → Expr
;;; Fold a list of expressions into a product using smart constructors.
(define (fold-product exprs)
  (cond
   [(null? exprs) (num 1)]
   [(null? (cdr exprs)) (car exprs)]
   [else (product (car exprs) (fold-product (cdr exprs)))]))

;;; ====
;;; Basic Differentiation
;;; ====

;;; deriv : Expr × Symbol → Expr
;;; Compute the derivative of expr with respect to var-sym.
;;; Uses the standard calculus differentiation rules.
(define (deriv expr var-sym)
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
   
   ;; d/dx[f / g] = (f' * g - f * g') / g^2 (quotient rule)
   [(quotient? expr)
    (let ([f (quot-numer expr)]
          [g (quot-denom expr)])
         (quotient
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
                         (product g (quotient (deriv f var-sym) f))))]))]
   
   ;; d/dx[fn(u)] = fn'(u) * u' (chain rule)
   [(app? expr)
    (let ([fn (app-fn expr)]
          [u (app-arg expr)]
          [du (deriv (app-arg expr) var-sym)])
         (product (deriv-fn fn u) du))]
   
   ;; Unknown expression type
   [else (num 0)]))

;;; deriv-fn : Symbol × Expr → Expr
;;; Compute the derivative of a named function.
;;; Returns the derivative function evaluated at the given argument.
(define (deriv-fn fn arg)
  (case fn
        ;; d/du[sin(u)] = cos(u)
        [(sin) (sym-cos arg)]
        ;; d/du[cos(u)] = -sin(u)
        [(cos) (make-neg (sym-sin arg))]
        ;; d/du[tan(u)] = sec^2(u) = 1/cos^2(u)
        [(tan) (quotient (num 1) (power (sym-cos arg) (num 2)))]
        ;; d/du[exp(u)] = exp(u)
        [(exp) (sym-exp arg)]
        ;; d/du[log(u)] = 1/u
        [(log) (quotient (num 1) arg)]
        ;; d/du[sqrt(u)] = 1/(2*sqrt(u))
        [(sqrt) (quotient (num 1) (product (num 2) (sym-sqrt arg)))]
        ;; d/du[asin(u)] = 1/sqrt(1-u^2)
        [(asin) (quotient (num 1) (sym-sqrt (difference (num 1) (power arg (num 2)))))]
        ;; d/du[acos(u)] = -1/sqrt(1-u^2)
        [(acos) (make-neg (quotient (num 1) (sym-sqrt (difference (num 1) (power arg (num 2))))))]
        ;; d/du[atan(u)] = 1/(1+u^2)
        [(atan) (quotient (num 1) (sum (num 1) (power arg (num 2))))]
        ;; d/du[sinh(u)] = cosh(u)
        [(sinh) (make-app 'cosh arg)]
        ;; d/du[cosh(u)] = sinh(u)
        [(cosh) (make-app 'sinh arg)]
        ;; d/du[tanh(u)] = sech^2(u) = 1/cosh^2(u)
        [(tanh) (quotient (num 1) (power (make-app 'cosh arg) (num 2)))]
        ;; Unknown function: return symbolic derivative
        [else (make-app 'D (make-app fn arg))]))

;;; ====
;;; Partial Derivatives
;;; ====

;;; partial : Expr × Symbol → Expr
;;; Compute partial derivative ∂expr/∂var-sym.
;;; Alias for deriv (in single-variable case, partial = total derivative).
(define (partial expr var-sym)
  (deriv expr var-sym))

;;; ====
;;; Gradient
;;; ====

;;; gradient : Expr × (List Symbol) → (List Expr)
;;; Compute the gradient ∇f = [∂f/∂x₁, ∂f/∂x₂, ..., ∂f/∂xₙ].
;;; Returns a list of partial derivatives.
(define (gradient expr vars)
  (map (lambda (v) (partial expr v)) vars))

;;; ====
;;; Jacobian Matrix
;;; ====

;;; jacobian : (List Expr) × (List Symbol) → (List (List Expr))
;;; Compute the Jacobian matrix for vector-valued function f: ℝⁿ → ℝᵐ.
;;; exprs = [f₁, f₂, ..., fₘ] (output components)
;;; vars = [x₁, x₂, ..., xₙ] (input variables)
;;; Returns m×n matrix where J[i][j] = ∂fᵢ/∂xⱼ
(define (jacobian exprs vars)
  (map (lambda (expr) (gradient expr vars)) exprs))

;;; ====
;;; Hessian Matrix
;;; ====

;;; hessian : Expr × (List Symbol) → (List (List Expr))
;;; Compute the Hessian matrix H = [∂²f/∂xᵢ∂xⱼ].
;;; Returns n×n symmetric matrix of second partial derivatives.
(define (hessian expr vars)
  (map (lambda (vi)
               (map (lambda (vj)
                            (deriv (deriv expr vi) vj))
                    vars))
       vars))

;;; ====
;;; Higher-Order Derivatives
;;; ====

;;; deriv-n : Expr × Symbol × Nat → Expr
;;; Compute nth derivative of expr with respect to var-sym.
(define (deriv-n expr var-sym n)
  (if (<= n 0)
      expr
      (deriv-n (deriv expr var-sym) var-sym (- n 1))))

;;; ====
;;; Directional Derivative
;;; ====

;;; directional-derivative : Expr × (List Symbol) × (List Expr) → Expr
;;; Compute the directional derivative ∇f · v.
;;; vars = [x₁, x₂, ..., xₙ]
;;; direction = [v₁, v₂, ..., vₙ] (direction vector as expressions)
;;; Returns ∇f · v = Σ(∂f/∂xᵢ * vᵢ)
(define (directional-derivative expr vars direction)
  (let ([grad (gradient expr vars)])
       (fold-sum (map product grad direction))))

;;; ====
;;; Curl (for 3D vector fields)
;;; ====

;;; curl : (List Expr) × (List Symbol) → (List Expr)
;;; Compute curl of a 3D vector field F = [F₁, F₂, F₃].
;;; vars should be [x, y, z]
;;; Returns [∂F₃/∂y - ∂F₂/∂z, ∂F₁/∂z - ∂F₃/∂x, ∂F₂/∂x - ∂F₁/∂y]
;;; Assumes valid 3D input (shell layer validates).
(define (curl field vars)
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

;;; ====
;;; Divergence (for vector fields)
;;; ====

;;; divergence : (List Expr) × (List Symbol) → Expr
;;; Compute divergence of a vector field F = [F₁, F₂, ..., Fₙ].
;;; Returns ∇ · F = Σ(∂Fᵢ/∂xᵢ)
;;; Assumes valid input with same number of field components and variables.
(define (divergence field vars)
  (fold-sum (map deriv field vars)))

;;; ====
;;; Laplacian (scalar field)
;;; ====

;;; laplacian : Expr × (List Symbol) → Expr
;;; Compute the Laplacian ∇²f = Σ(∂²f/∂xᵢ²).
;;; Returns sum of all unmixed second partial derivatives.
(define (laplacian expr vars)
  (fold-sum (map (lambda (v) (deriv (deriv expr v) v)) vars)))
