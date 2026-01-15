((name "symbolic")
 (description "Symbolic computation including expression representation,
  symbolic differentiation, algebraic simplification, and symbolic integration.")

 (modules
  ((file "expr.ss")
   (purpose "Symbolic expression representation and manipulation")
   (exports
    ;; Constructors
    (num "Create a numeric constant: (num 5)")
    (var "Create a symbolic variable: (var 'x)")
    (sum "Smart sum constructor: (sum e1 e2)")
    (product "Smart product constructor: (product e1 e2)")
    (difference "Smart difference constructor: (difference e1 e2)")
    (quotient "Smart quotient constructor: (quotient e1 e2)")
    (power "Smart power constructor: (power base exp)")
    (make-neg "Negation: (make-neg e)")
    (make-app "Function application: (make-app 'sin e)")
    ;; Function constructors
    (sym-sin "Sine: (sym-sin e)")
    (sym-cos "Cosine: (sym-cos e)")
    (sym-tan "Tangent: (sym-tan e)")
    (sym-exp "Exponential: (sym-exp e)")
    (sym-log "Natural log: (sym-log e)")
    (sym-sqrt "Square root: (sym-sqrt e)")
    (sym-sinh "Hyperbolic sine: (sym-sinh e)")
    (sym-cosh "Hyperbolic cosine: (sym-cosh e)")
    (sym-tanh "Hyperbolic tangent: (sym-tanh e)")
    (sym-atan "Arctangent: (sym-atan e)")
    (sym-asin "Arcsine: (sym-asin e)")
    (sym-acos "Arccosine: (sym-acos e)")
    ;; Predicates
    (num? "Test for numeric constant")
    (var? "Test for variable")
    (sum? "Test for sum")
    (product? "Test for product")
    (difference? "Test for difference")
    (quotient? "Test for quotient")
    (power? "Test for power")
    (app? "Test for function application")
    ;; Accessors
    (num-val "Extract numeric value")
    (var-name "Extract variable name")
    (sum-terms "Get terms of sum")
    (product-factors "Get factors of product")
    (pow-base "Get base of power")
    (pow-exp "Get exponent of power")
    (app-fn "Get function name")
    (app-arg "Get function argument")
    ;; Pattern matching
    (match-expr "Match expression against pattern")
    ;; Substitution
    (subst "Substitute value for variable: (subst expr var val)")
    ;; Free variables
    (free-vars "Get free variables in expression")
    ;; Equality
    (expr=? "Structural equality")
    ;; Display
    (expr->string "Convert to readable string"))
   (example
    ";; Build expression: x^2 + 2x + 1
     (define e (sum (power (var 'x) (num 2))
                    (sum (product (num 2) (var 'x))
                         (num 1))))

     ;; Substitute x=3
     (subst e 'x (num 3))  ;; => numeric expression

     ;; Get free variables
     (free-vars e)  ;; => (x)"))

  ((file "diff.ss")
   (purpose "Symbolic differentiation")
   (exports
    (deriv "Differentiate expression: (deriv expr var-sym)")
    (partial "Partial derivative (alias for deriv)")
    (gradient "Gradient vector: (gradient expr vars)")
    (jacobian "Jacobian matrix: (jacobian exprs vars)")
    (hessian "Hessian matrix: (hessian expr vars)")
    (deriv-n "nth derivative: (deriv-n expr var-sym n)")
    (directional-derivative "Directional derivative: (directional-derivative expr vars direction)")
    (curl "Curl of 3D vector field: (curl field vars)")
    (divergence "Divergence of vector field: (divergence field vars)")
    (laplacian "Laplacian: (laplacian expr vars)"))
   (example
    ";; Derivative of x^2 is 2x
     (deriv (power (var 'x) (num 2)) 'x)
     ;; => (* (num 2) (var x))

     ;; Chain rule: d/dx sin(x^2) = 2x*cos(x^2)
     (deriv (sym-sin (power (var 'x) (num 2))) 'x)

     ;; Gradient of x^2 + y^2
     (gradient (sum (power (var 'x) (num 2))
                    (power (var 'y) (num 2)))
               '(x y))"))

  ((file "simplify.ss")
   (purpose "Algebraic simplification")
   (exports
    (simplify "Comprehensive algebraic simplification")
    (simplify-trig "Trigonometric simplifications")
    (expand "Expand products: (a+b)*c => a*c + b*c")
    (factor "Factor common terms from sums")
    (to-canonical "Convert to canonical form")
    (full-simplify "Full simplification pipeline")
    (expr-complexity "Measure expression complexity"))
   (features
    ("Collect like terms" "x + x => 2*x")
    ("Combine powers" "x * x => x^2, x^2 * x^3 => x^5")
    ("Numeric evaluation" "2 + 3 => 5, 2^3 => 8")
    ("Identity elimination" "0 + x => x, 1 * x => x, x^1 => x")
    ("Trig identities" "sin^2 + cos^2 => 1, sin/cos => tan")
    ("Exp/log identities" "exp(log(x)) => x, log(x^n) => n*log(x)")
    ("Product expansion" "(a+b)*(c+d) => ac + ad + bc + bd")
    ("Factoring" "2*x + 2*y => 2*(x+y), x^2 - y^2 => (x+y)*(x-y)"))
   (example
    ";; Collect like terms
     (simplify (sum (var 'x) (var 'x)))  ;; => (* (num 2) (var x))

     ;; Pythagorean identity
     (simplify-trig (sum (power (sym-sin (var 'x)) (num 2))
                         (power (sym-cos (var 'x)) (num 2))))
     ;; => (num 1)

     ;; Expand products
     (expand (product (sum (var 'a) (var 'b)) (var 'c)))
     ;; => (+ (* (var a) (var c)) (* (var b) (var c)))"))

  ((file "integrate.ss")
   (purpose "Symbolic integration")
   (exports
    (integrate "Compute antiderivative: (integrate expr var-sym)")
    (antiderivative "Alias for integrate")
    (definite-integral "Definite integral: (definite-integral expr var a b)")
    (∫ "Unicode alias for integrate"))
   (features
    ("Power rule" "∫x^n dx = x^(n+1)/(n+1)")
    ("Trig functions" "∫sin(x)dx = -cos(x), ∫cos(x)dx = sin(x)")
    ("Exponentials" "∫e^x dx = e^x, ∫a^x dx = a^x/ln(a)")
    ("Logarithm" "∫1/x dx = ln|x|, ∫ln(x) dx = x*ln(x) - x")
    ("Sum/difference rules" "∫(f+g)dx = ∫f dx + ∫g dx")
    ("Constant multiple" "∫c*f dx = c*∫f dx")
    ("Linear substitution" "∫f(ax+b) dx = F(ax+b)/a")
    ("Hyperbolic functions" "∫sinh(x)dx = cosh(x), ∫cosh(x)dx = sinh(x)")
    ("Inverse trig" "∫1/(1+x^2) dx = atan(x)"))
   (example
    ";; Basic integral
     (integrate (power (var 'x) (num 2)) 'x)
     ;; => (/ (^ (var x) (num 3)) (num 3))

     ;; Trig integral
     (integrate (sym-sin (var 'x)) 'x)
     ;; => (- (cos (var x)))

     ;; Definite integral
     (definite-integral (var 'x) 'x (num 0) (num 1))
     ;; => (num 1/2)"))

  ((file "integrate-autodiff.ss")
   (purpose "DEPRECATED: Redirect to lattice/autodiff/symbolic-diff.ss")
   (note "This module has moved to lattice/autodiff/symbolic-diff.ss.
          This file remains as a backwards-compatible redirect.
          Please update your imports to use the new location.")))

 (moved-modules
  ((original "integrate-autodiff.ss")
   (new-location "lattice/autodiff/symbolic-diff.ss")
   (exports
    (eval-expr "Evaluate symbolic expression numerically")
    (eval-expr-safe "Safe eval with error handling")
    (expr-to-traced "Compile expression to traced function for AD")
    (symbolic-gradient-fn "Create numerical gradient function")
    (symbolic-jacobian-fn "Create numerical Jacobian function")
    (symbolic-hessian-fn "Create numerical Hessian function")
    (verify-derivative "Verify symbolic vs AD derivative")
    (expr->infix "Pretty-print expression as infix"))))

 (dependencies
  ("core/base/prelude.ss" "Base utilities")
  ("lattice/autodiff/reverse-diff.ss" "Automatic differentiation (for integrate-autodiff)"))

 (test-files
  ("test-expr.ss" "55 tests for expression building and manipulation")
  ("test-diff.ss" "51 tests for symbolic differentiation")
  ("test-simplify.ss" "41 tests for algebraic simplification")
  ("test-integrate.ss" "27 tests for symbolic integration")
  ("test-integrate-autodiff.ss" "Tests for symbolic-AD bridge"))

 (notes
  "Symbolic expressions form an algebraic data type representing
   mathematical formulas. Unlike numeric computation, symbolic
   computation manipulates the structure of expressions.

   Key applications:
   - Computer algebra systems
   - Automatic differentiation (symbolic mode)
   - Formula simplification and canonicalization
   - Code generation from mathematical specifications

   The diff module implements standard differentiation rules:
   - Sum rule: d/dx(f+g) = df/dx + dg/dx
   - Product rule: d/dx(fg) = f'g + fg'
   - Chain rule: d/dx(f(g(x))) = f'(g(x)) * g'(x)
   - Power rule: d/dx(x^n) = n*x^(n-1)

   The integrate module implements symbolic integration:
   - Table lookup for common functions
   - Algebraic rules (sum, constant multiple)
   - Power rule and linear substitution
   - Integration by parts (limited)

   All modules use smart constructors that apply basic simplifications
   automatically, ensuring expressions stay in reasonably simplified form.

   Total: 174 tests across all modules."))
