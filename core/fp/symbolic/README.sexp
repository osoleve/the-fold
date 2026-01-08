((name "symbolic")
 (description "Symbolic computation including expression representation,
  symbolic differentiation, and algebraic simplification.")

 (modules
  ((file "expr.ss")
   (purpose "Symbolic expression representation and manipulation")
   (exports
    ;; Constructors
    (sym "Create a symbol: (sym 'x)")
    (num "Create a numeric literal: (num 5)")
    (add "Addition: (add e1 e2)")
    (sub "Subtraction: (sub e1 e2)")
    (mul "Multiplication: (mul e1 e2)")
    (div-expr "Division: (div-expr e1 e2)")
    (pow "Power: (pow base exponent)")
    (neg "Negation: (neg e)")
    (ln "Natural log: (ln e)")
    (exp-expr "Exponential: (exp-expr e)")
    (sin-expr "Sine: (sin-expr e)")
    (cos-expr "Cosine: (cos-expr e)")
    ;; Predicates
    (sym? "Test for symbol")
    (num? "Test for numeric literal")
    (add? "Test for addition")
    (mul? "Test for multiplication")
    (pow? "Test for power")
    ;; Accessors
    (expr-op "Get operator of compound expression")
    (expr-args "Get arguments of compound expression")
    (num-value "Extract numeric value")
    (sym-name "Extract symbol name")
    ;; Evaluation
    (expr-eval "Evaluate expression with environment")
    (expr-subst "Substitute variable with expression")
    (expr-vars "Get free variables in expression")
    ;; Simplification
    (simplify "Algebraic simplification")
    (expand "Expand products and powers")
    (factor "Factor common terms")
    (collect "Collect terms by variable")
    ;; Comparison
    (expr-equal? "Structural equality")
    (expr-equiv? "Mathematical equivalence (via simplification)")
    ;; Display
    (expr->string "Convert to readable string")
    (expr->latex "Convert to LaTeX"))
   (example
    ";; Build expression: x^2 + 2x + 1
     (define e (add (pow (sym 'x) (num 2))
                    (add (mul (num 2) (sym 'x))
                         (num 1))))

     ;; Evaluate at x=3
     (expr-eval e '((x . 3)))  ;; => 16

     ;; Convert to string
     (expr->string e)  ;; => \"x^2 + 2*x + 1\""))

  ((file "diff.ss")
   (purpose "Symbolic differentiation")
   (exports
    (diff "Differentiate expression with respect to variable")
    (partial "Partial derivative")
    (gradient "Gradient (vector of partial derivatives)")
    (jacobian "Jacobian matrix")
    (hessian "Hessian matrix (second derivatives)")
    (taylor "Taylor series expansion to given order")
    (diff-simplify "Differentiate and simplify"))
   (example
    ";; Derivative of x^2 is 2x
     (diff (pow (sym 'x) (num 2)) 'x)
     ;; => (mul (num 2) (sym 'x))

     ;; Chain rule: d/dx sin(x^2) = 2x*cos(x^2)
     (diff-simplify (sin-expr (pow (sym 'x) (num 2))) 'x)
     ;; => (mul (mul (num 2) (sym 'x))
     ;;         (cos-expr (pow (sym 'x) (num 2))))

     ;; Gradient of x^2 + y^2
     (gradient (add (pow (sym 'x) (num 2))
                    (pow (sym 'y) (num 2)))
               '(x y))
     ;; => ((mul (num 2) (sym 'x))
     ;;     (mul (num 2) (sym 'y)))")))

 (dependencies
  ("core/base/prelude.ss" "Base utilities")
  ("core/linalg/matrix.ss" "Matrix representation for Jacobian/Hessian"))

 (notes
  "Symbolic expressions form an algebraic data type representing
   mathematical formulas. Unlike numeric computation, symbolic
   computation manipulates the structure of expressions.

   Key applications:
   - Automatic differentiation (symbolic mode)
   - Formula simplification and canonicalization
   - Code generation from mathematical specifications
   - Computer algebra

   The diff module implements standard differentiation rules:
   - Sum rule: d/dx(f+g) = df/dx + dg/dx
   - Product rule: d/dx(fg) = f'g + fg'
   - Chain rule: d/dx(f(g(x))) = f'(g(x)) * g'(x)
   - Power rule: d/dx(x^n) = n*x^(n-1)

   Test suite: 55 tests covering expression building and differentiation."))
