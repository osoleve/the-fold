;;; core/fp/symbolic/integrate-autodiff.ss — Symbolic-Autodiff Integration
;;;
;;; Bridges symbolic differentiation with automatic differentiation:
;;;   - eval-expr: Evaluate symbolic expressions numerically
;;;   - simplify: Algebraic simplification of expressions
;;;   - expr-to-traced: Compile expressions to traced functions
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - core/fp/symbolic/diff.ss (includes expr.ss)
;;;   - core/autodiff/reverse-diff.ss

(load "lattice/fp/symbolic/diff.ss")

;;; Save symbolic functions before autodiff overwrites them
(define sym-gradient gradient)
(define sym-jacobian jacobian)
(define sym-hessian hessian)

(load "lattice/autodiff/reverse-diff.ss")

;;; Restore autodiff functions with clear names
(define autodiff-gradient gradient)

;;; ============================================================
;;; Expression Evaluation
;;; ============================================================

;;; eval-expr : Expr × Env → Number
;;; Evaluate a symbolic expression given variable bindings.
;;; env is an alist: ((x . 3.0) (y . 2.0) ...)
;;;
;;; Special constants:
;;;   - pi evaluates to π
;;;   - e evaluates to Euler's number
(define (eval-expr expr env)
  (cond
   ;; Numeric constant
   [(num? expr) (num-val expr)]
   
   ;; Variable lookup
   [(var? expr)
    (let ([binding (assq (var-name expr) env)])
         (cond
          [binding (cdr binding)]
          ;; Special constants
          [(eq? (var-name expr) 'pi) 3.141592653589793]
          [(eq? (var-name expr) 'e) 2.718281828459045]
          [else (error 'eval-expr "Unbound variable" (var-name expr))]))]
   
   ;; Sum: evaluate all terms
   [(sum? expr)
    (apply + (map (lambda (e) (eval-expr e env)) (sum-terms expr)))]
   
   ;; Product: evaluate all factors
   [(product? expr)
    (apply * (map (lambda (e) (eval-expr e env)) (product-factors expr)))]
   
   ;; Difference
   [(difference? expr)
    (if (diff-right expr)
        (- (eval-expr (diff-left expr) env)
           (eval-expr (diff-right expr) env))
        (- (eval-expr (diff-left expr) env)))]
   
   ;; Quotient
   [(quotient? expr)
    (/ (eval-expr (quot-numer expr) env)
       (eval-expr (quot-denom expr) env))]
   
   ;; Power
   [(power? expr)
    (expt (eval-expr (pow-base expr) env)
          (eval-expr (pow-exp expr) env))]
   
   ;; Function application
   [(app? expr)
    (let ([fn (app-fn expr)]
          [arg-val (eval-expr (app-arg expr) env)])
         (case fn
               [(sin) (sin arg-val)]
               [(cos) (cos arg-val)]
               [(tan) (tan arg-val)]
               [(exp) (exp arg-val)]
               [(log) (log arg-val)]
               [(sqrt) (sqrt arg-val)]
               [(asin) (asin arg-val)]
               [(acos) (acos arg-val)]
               [(atan) (atan arg-val)]
               [(sinh) (sinh arg-val)]
               [(cosh) (cosh arg-val)]
               [(tanh) (tanh arg-val)]
               [(abs) (abs arg-val)]
               [else (error 'eval-expr "Unknown function" fn)]))]
   
   [else (error 'eval-expr "Unknown expression type" expr)]))

;;; eval-expr-safe : Expr × Env → Number | #f
;;; Safe version that returns #f on error.
(define (eval-expr-safe expr env)
  (guard (c [else #f])
         (eval-expr expr env)))

;;; ============================================================
;;; Expression Simplification
;;; ============================================================

;;; simplify : Expr → Expr
;;; Simplify an expression using algebraic rules.
;;; Applies rules repeatedly until fixed point.
(define (simplify expr)
  (let ([simplified (simplify-once expr)])
       (if (expr=? simplified expr)
           simplified
           (simplify simplified))))

;;; simplify-once : Expr → Expr
;;; Single pass of simplification.
(define (simplify-once expr)
  (cond
   ;; Atoms: already simplified
   [(num? expr) expr]
   [(var? expr) expr]
   
   ;; Recursively simplify subexpressions first
   [(sum? expr)
    (simplify-sum (map simplify-once (sum-terms expr)))]
   
   [(product? expr)
    (simplify-product (map simplify-once (product-factors expr)))]
   
   [(difference? expr)
    (if (diff-right expr)
        (simplify-diff (simplify-once (diff-left expr))
                       (simplify-once (diff-right expr)))
        (simplify-neg (simplify-once (diff-left expr))))]
   
   [(quotient? expr)
    (simplify-quot (simplify-once (quot-numer expr))
                   (simplify-once (quot-denom expr)))]
   
   [(power? expr)
    (simplify-pow (simplify-once (pow-base expr))
                  (simplify-once (pow-exp expr)))]
   
   [(app? expr)
    (simplify-app (app-fn expr)
                  (simplify-once (app-arg expr)))]
   
   [else expr]))

;;; simplify-sum : (List Expr) → Expr
;;; Simplify a sum by collecting numeric terms and like terms.
(define (simplify-sum terms)
  (let* ([flat-terms (flatten-sums terms)]
         [numeric-sum (apply + (filter-map get-numeric flat-terms))]
         [non-numeric (filter (lambda (e) (not (num? e))) flat-terms)]
         [collected (collect-like-terms non-numeric)])
        (cond
         ;; All zeros
         [(and (null? collected) (= numeric-sum 0)) (num 0)]
         ;; Only numeric
         [(null? collected) (num numeric-sum)]
         ;; Only symbolic
         [(= numeric-sum 0) (make-sum-from-list collected)]
         ;; Both
         [else (make-sum-from-list (cons (num numeric-sum) collected))])))

;;; flatten-sums : (List Expr) → (List Expr)
;;; Flatten nested sums: (+ a (+ b c)) → (+ a b c)
(define (flatten-sums terms)
  (apply append
         (map (lambda (e)
                      (if (sum? e)
                          (flatten-sums (sum-terms e))
                          (list e)))
              terms)))

;;; get-numeric : Expr → Number | #f
(define (get-numeric e)
  (if (num? e) (num-val e) #f))

;;; filter-map : (α → β | #f) × (List α) → (List β)
(define (filter-map f lst)
  (let loop ([lst lst] [acc '()])
       (if (null? lst)
           (reverse acc)
           (let ([result (f (car lst))])
                (if result
                    (loop (cdr lst) (cons result acc))
                    (loop (cdr lst) acc))))))

;;; collect-like-terms : (List Expr) → (List Expr)
;;; Collect terms like x + x → 2*x
(define (collect-like-terms terms)
  (let ([table (make-hashtable equal-hash equal?)])
       ;; Count coefficients
       (for-each
        (lambda (term)
                (let-values ([(coef base) (extract-coefficient term)])
                            (let ([current (hashtable-ref table base 0)])
                                 (hashtable-set! table base (+ current coef)))))
        terms)
       ;; Rebuild terms from hashtable keys
       (let ([keys (hashtable-keys table)])
            (filter-map
             (lambda (key)
                     (let ([coef (hashtable-ref table key 0)])
                          (cond
                           [(= coef 0) #f]
                           [(= coef 1) key]
                           [else (product (num coef) key)])))
             (vector->list keys)))))

;;; extract-coefficient : Expr → (Values Number Expr)
;;; Extract numeric coefficient from a term.
;;; x → (1, x), 2*x → (2, x), -x → (-1, x)
(define (extract-coefficient term)
  (cond
   [(num? term) (values (num-val term) (num 1))]
   [(and (product? term)
         (= (length (product-factors term)) 2)
         (num? (car (product-factors term))))
    (values (num-val (car (product-factors term)))
            (cadr (product-factors term)))]
   [(and (difference? term) (not (diff-right term)))
    (let-values ([(c b) (extract-coefficient (diff-left term))])
                (values (- c) b))]
   [else (values 1 term)]))

;;; make-sum-from-list : (List Expr) → Expr
(define (make-sum-from-list terms)
  (cond
   [(null? terms) (num 0)]
   [(= (length terms) 1) (car terms)]
   [else (fold-left sum (car terms) (cdr terms))]))

;;; simplify-product : (List Expr) → Expr
;;; Simplify a product.
(define (simplify-product factors)
  (let* ([flat-factors (flatten-products factors)]
         [numeric-prod (apply * (filter-map get-numeric flat-factors))]
         [non-numeric (filter (lambda (e) (not (num? e))) flat-factors)])
        (cond
         ;; Zero factor
         [(= numeric-prod 0) (num 0)]
         ;; All ones
         [(and (null? non-numeric) (= numeric-prod 1)) (num 1)]
         ;; Only numeric
         [(null? non-numeric) (num numeric-prod)]
         ;; Numeric is 1
         [(= numeric-prod 1) (make-product-from-list non-numeric)]
         ;; Both
         [else (make-product-from-list (cons (num numeric-prod) non-numeric))])))

;;; flatten-products : (List Expr) → (List Expr)
(define (flatten-products factors)
  (apply append
         (map (lambda (e)
                      (if (product? e)
                          (flatten-products (product-factors e))
                          (list e)))
              factors)))

;;; make-product-from-list : (List Expr) → Expr
(define (make-product-from-list factors)
  (cond
   [(null? factors) (num 1)]
   [(= (length factors) 1) (car factors)]
   [else (fold-left product (car factors) (cdr factors))]))

;;; simplify-diff : Expr × Expr → Expr
(define (simplify-diff left right)
  (cond
   ;; x - 0 = x
   [(and (num? right) (= (num-val right) 0)) left]
   ;; 0 - x = -x
   [(and (num? left) (= (num-val left) 0)) (simplify-neg right)]
   ;; n - m = n-m
   [(and (num? left) (num? right))
    (num (- (num-val left) (num-val right)))]
   ;; x - x = 0
   [(expr=? left right) (num 0)]
   [else (difference left right)]))

;;; simplify-neg : Expr → Expr
(define (simplify-neg e)
  (cond
   ;; -n = -n (fold constant)
   [(num? e) (num (- (num-val e)))]
   ;; -(-x) = x
   [(and (difference? e) (not (diff-right e)))
    (diff-left e)]
   ;; -(n*x) = (-n)*x
   [(and (product? e)
         (= (length (product-factors e)) 2)
         (num? (car (product-factors e))))
    (product (num (- (num-val (car (product-factors e)))))
             (cadr (product-factors e)))]
   [else (make-neg e)]))

;;; simplify-quot : Expr × Expr → Expr
(define (simplify-quot numer denom)
  (cond
   ;; 0/x = 0
   [(and (num? numer) (= (num-val numer) 0)) (num 0)]
   ;; x/1 = x
   [(and (num? denom) (= (num-val denom) 1)) numer]
   ;; n/m
   [(and (num? numer) (num? denom))
    (let ([result (/ (num-val numer) (num-val denom))])
         (if (integer? result)
             (num result)
             (quotient numer denom)))]
   ;; x/x = 1
   [(expr=? numer denom) (num 1)]
   [else (quotient numer denom)]))

;;; simplify-pow : Expr × Expr → Expr
(define (simplify-pow base exp)
  (cond
   ;; x^0 = 1
   [(and (num? exp) (= (num-val exp) 0)) (num 1)]
   ;; x^1 = x
   [(and (num? exp) (= (num-val exp) 1)) base]
   ;; 0^n = 0 (n > 0)
   [(and (num? base) (= (num-val base) 0)
         (num? exp) (> (num-val exp) 0))
    (num 0)]
   ;; 1^n = 1
   [(and (num? base) (= (num-val base) 1)) (num 1)]
   ;; n^m = n^m (small integer exponent)
   [(and (num? base) (num? exp)
         (integer? (num-val exp))
         (<= (abs (num-val exp)) 10))
    (num (expt (num-val base) (num-val exp)))]
   [else (power base exp)]))

;;; simplify-app : Symbol × Expr → Expr
(define (simplify-app fn arg)
  (cond
   ;; sin(0) = 0
   [(and (eq? fn 'sin) (num? arg) (= (num-val arg) 0)) (num 0)]
   ;; cos(0) = 1
   [(and (eq? fn 'cos) (num? arg) (= (num-val arg) 0)) (num 1)]
   ;; exp(0) = 1
   [(and (eq? fn 'exp) (num? arg) (= (num-val arg) 0)) (num 1)]
   ;; log(1) = 0
   [(and (eq? fn 'log) (num? arg) (= (num-val arg) 1)) (num 0)]
   ;; sqrt(0) = 0, sqrt(1) = 1
   [(and (eq? fn 'sqrt) (num? arg) (= (num-val arg) 0)) (num 0)]
   [(and (eq? fn 'sqrt) (num? arg) (= (num-val arg) 1)) (num 1)]
   ;; sinh(0) = 0, cosh(0) = 1, tanh(0) = 0
   [(and (eq? fn 'sinh) (num? arg) (= (num-val arg) 0)) (num 0)]
   [(and (eq? fn 'cosh) (num? arg) (= (num-val arg) 0)) (num 1)]
   [(and (eq? fn 'tanh) (num? arg) (= (num-val arg) 0)) (num 0)]
   ;; atan(0) = 0
   [(and (eq? fn 'atan) (num? arg) (= (num-val arg) 0)) (num 0)]
   [else (make-app fn arg)]))

;;; ============================================================
;;; Expression → Traced Function Compiler
;;; ============================================================

;;; expr-to-traced : Expr × (List Symbol) → ((List TracedValue) → TracedValue)
;;; Compile a symbolic expression into a traced function.
;;; The returned function takes traced values in the order of vars
;;; and returns a traced result suitable for reverse-mode AD.
;;;
;;; Example:
;;;   (define f (expr-to-traced (power (var 'x) (num 2)) '(x)))
;;;   (gradient f '(3.0))  ; → (6.0) [derivative of x^2 at x=3]
(define (expr-to-traced expr vars)
  (lambda traced-args
          (let ([env (map cons vars traced-args)])
               (eval-traced expr env))))

;;; eval-traced : Expr × TracedEnv → TracedValue
;;; Evaluate expression with traced values.
;;; env is ((symbol . traced-value) ...)
(define (eval-traced expr env)
  (cond
   ;; Numeric constant: return as plain number (traced ops handle this)
   [(num? expr) (num-val expr)]
   
   ;; Variable: lookup traced value
   [(var? expr)
    (let ([binding (assq (var-name expr) env)])
         (cond
          [binding (cdr binding)]
          ;; Special constants as plain numbers
          [(eq? (var-name expr) 'pi) 3.141592653589793]
          [(eq? (var-name expr) 'e) 2.718281828459045]
          [else (error 'eval-traced "Unbound variable" (var-name expr))]))]
   
   ;; Sum
   [(sum? expr)
    (fold-left traced-add
               (eval-traced (car (sum-terms expr)) env)
               (map (lambda (e) (eval-traced e env))
                    (cdr (sum-terms expr))))]
   
   ;; Product
   [(product? expr)
    (fold-left traced-mul
               (eval-traced (car (product-factors expr)) env)
               (map (lambda (e) (eval-traced e env))
                    (cdr (product-factors expr))))]
   
   ;; Difference
   [(difference? expr)
    (if (diff-right expr)
        (traced-sub (eval-traced (diff-left expr) env)
                    (eval-traced (diff-right expr) env))
        (traced-neg (eval-traced (diff-left expr) env)))]
   
   ;; Quotient
   [(quotient? expr)
    (traced-div (eval-traced (quot-numer expr) env)
                (eval-traced (quot-denom expr) env))]
   
   ;; Power
   [(power? expr)
    (let ([base (eval-traced (pow-base expr) env)]
          [exp-val (eval-traced (pow-exp expr) env)])
         ;; If exponent is numeric, use traced-pow
         (if (number? exp-val)
             (traced-pow base exp-val)
             ;; General case: a^b = exp(b * ln(a))
             (traced-exp (traced-mul exp-val (traced-log base)))))]
   
   ;; Function application
   [(app? expr)
    (let ([fn (app-fn expr)]
          [arg (eval-traced (app-arg expr) env)])
         (case fn
               [(sin) (traced-sin arg)]
               [(cos) (traced-cos arg)]
               [(tan) (traced-tan arg)]
               [(exp) (traced-exp arg)]
               [(log) (traced-log arg)]
               [(sqrt) (traced-sqrt arg)]
               [(asin) (traced-asin arg)]
               [(acos) (traced-acos arg)]
               [(atan) (traced-atan arg)]
               [(sinh) (traced-sinh arg)]
               [(cosh) (traced-cosh arg)]
               [(tanh) (traced-tanh arg)]
               [else (error 'eval-traced "Unknown function" fn)]))]
   
   [else (error 'eval-traced "Unknown expression type" expr)]))

;;; ============================================================
;;; High-Level Integration Functions
;;; ============================================================

;;; symbolic-gradient-fn : Expr × (List Symbol) → ((List Number) → (List Number))
;;; Create a numerical gradient function from a symbolic expression.
;;; Internally uses symbolic differentiation, compiles to traced, uses autodiff.
;;;
;;; This gives you the best of both worlds:
;;;   - Symbolic simplification of derivatives
;;;   - Efficient numerical evaluation via autodiff
(define (symbolic-gradient-fn expr vars)
  (let* ([grad-exprs (map simplify (sym-gradient expr vars))]
         [traced-fns (map (lambda (g) (expr-to-traced g vars)) grad-exprs)])
        (lambda (point)
                (map (lambda (f)
                             (let ([result (apply f point)])
                                  (if (traced? result)
                                      (traced-value result)
                                      result)))
                     traced-fns))))

;;; symbolic-jacobian-fn : (List Expr) × (List Symbol) → ((List Number) → (List (List Number)))
;;; Create a numerical Jacobian function from symbolic expressions.
(define (symbolic-jacobian-fn exprs vars)
  (let* ([jac-exprs (sym-jacobian exprs vars)]
         [simplified (map (lambda (row) (map simplify row)) jac-exprs)]
         [traced-rows (map (lambda (row)
                                   (map (lambda (e) (expr-to-traced e vars)) row))
                           simplified)])
        (lambda (point)
                (map (lambda (row-fns)
                             (map (lambda (f)
                                          (let ([result (apply f point)])
                                               (if (traced? result)
                                                   (traced-value result)
                                                   result)))
                                  row-fns))
                     traced-rows))))

;;; symbolic-hessian-fn : Expr × (List Symbol) → ((List Number) → (List (List Number)))
;;; Create a numerical Hessian function from a symbolic expression.
(define (symbolic-hessian-fn expr vars)
  (let* ([hess-exprs (sym-hessian expr vars)]
         [simplified (map (lambda (row) (map simplify row)) hess-exprs)]
         [traced-rows (map (lambda (row)
                                   (map (lambda (e) (expr-to-traced e vars)) row))
                           simplified)])
        (lambda (point)
                (map (lambda (row-fns)
                             (map (lambda (f)
                                          (let ([result (apply f point)])
                                               (if (traced? result)
                                                   (traced-value result)
                                                   result)))
                                  row-fns))
                     traced-rows))))

;;; verify-derivative : Expr × Symbol × Number × Number → Bool
;;; Verify symbolic derivative matches autodiff at a point.
;;; Useful for testing and debugging.
(define (verify-derivative expr var-sym test-val tolerance)
  (let* ([sym-deriv (simplify (deriv expr var-sym))]
         [sym-val (eval-expr sym-deriv `((,var-sym . ,test-val)))]
         [traced-fn (expr-to-traced expr (list var-sym))]
         [ad-val (car (gradient traced-fn (list test-val)))])
        (< (abs (- sym-val ad-val)) tolerance)))

;;; ============================================================
;;; Pretty Printing
;;; ============================================================

;;; expr->infix : Expr → String
;;; Convert expression to readable infix notation.
(define (expr->infix expr)
  (cond
   [(num? expr)
    (let ([n (num-val expr)])
         (if (< n 0)
             (string-append "(" (number->string n) ")")
             (number->string n)))]
   [(var? expr) (symbol->string (var-name expr))]
   [(sum? expr)
    (string-append
     "("
     (string-join (map expr->infix (sum-terms expr)) " + ")
     ")")]
   [(product? expr)
    (string-append
     "("
     (string-join (map expr->infix (product-factors expr)) "*")
     ")")]
   [(difference? expr)
    (if (diff-right expr)
        (string-append "(" (expr->infix (diff-left expr))
                       " - " (expr->infix (diff-right expr)) ")")
        (string-append "-" (expr->infix (diff-left expr))))]
   [(quotient? expr)
    (string-append "(" (expr->infix (quot-numer expr))
                   "/" (expr->infix (quot-denom expr)) ")")]
   [(power? expr)
    (string-append "(" (expr->infix (pow-base expr))
                   "^" (expr->infix (pow-exp expr)) ")")]
   [(app? expr)
    (string-append (symbol->string (app-fn expr))
                   "(" (expr->infix (app-arg expr)) ")")]
   [else "?"]))
