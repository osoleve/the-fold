;;; core/fp/symbolic/expr.ss — Symbolic Expression Representation
;;;
;;; Core symbolic expression data structures for computer algebra.
;;; Uses S-expressions as a natural representation.
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Expression Types:
;;;   - (num n) : Numeric constant
;;;   - (var x) : Symbolic variable
;;;   - (+ e1 e2 ...) : Sum
;;;   - (* e1 e2 ...) : Product
;;;   - (- e1 e2) : Difference (or negation if unary)
;;;   - (/ e1 e2) : Division
;;;   - (^ e1 e2) : Exponentiation
;;;   - (fn arg) : Function application (sin, cos, exp, log, etc.)
;;;
;;; Dependencies:
;;;   - core/prelude.ss

(load "core/prelude.ss")

;;; ============================================================
;;; Expression Constructors
;;; ============================================================

;;; num : Number → Expr
;;; Create a numeric constant.
(define (num n)
  (list 'num n))

;;; var : Symbol → Expr
;;; Create a symbolic variable.
(define (var x)
  (list 'var x))

;;; make-sum : (List Expr) → Expr
;;; Create a sum of expressions.
(define (make-sum exprs)
  (cond
   [(null? exprs) (num 0)]
   [(= (length exprs) 1) (car exprs)]
   [else (cons '+ exprs)]))

;;; make-product : (List Expr) → Expr
;;; Create a product of expressions.
(define (make-product exprs)
  (cond
   [(null? exprs) (num 1)]
   [(= (length exprs) 1) (car exprs)]
   [else (cons '* exprs)]))

;;; make-diff : Expr × Expr → Expr
;;; Create a difference.
(define (make-diff e1 e2)
  (list '- e1 e2))

;;; make-neg : Expr → Expr
;;; Create a negation.
(define (make-neg e)
  (list '- e))

;;; make-div : Expr × Expr → Expr
;;; Create a division.
(define (make-div e1 e2)
  (list '/ e1 e2))

;;; make-pow : Expr × Expr → Expr
;;; Create an exponentiation.
(define (make-pow base exp)
  (list '^ base exp))

;;; make-app : Symbol × Expr → Expr
;;; Create a function application.
(define (make-app fn arg)
  (list fn arg))

;;; ============================================================
;;; Smart Constructors (with simplification)
;;; ============================================================

;;; sum : Expr × Expr → Expr
;;; Create a sum with basic simplifications.
(define (sum e1 e2)
  (cond
   ;; 0 + x = x
   [(and (num? e1) (= (num-val e1) 0)) e2]
   ;; x + 0 = x
   [(and (num? e2) (= (num-val e2) 0)) e1]
   ;; n1 + n2 = n1+n2 (fold constants)
   [(and (num? e1) (num? e2))
    (num (+ (num-val e1) (num-val e2)))]
   ;; Default: create sum
   [else (list '+ e1 e2)]))

;;; product : Expr × Expr → Expr
;;; Create a product with basic simplifications.
(define (product e1 e2)
  (cond
   ;; 0 * x = 0
   [(and (num? e1) (= (num-val e1) 0)) (num 0)]
   [(and (num? e2) (= (num-val e2) 0)) (num 0)]
   ;; 1 * x = x
   [(and (num? e1) (= (num-val e1) 1)) e2]
   [(and (num? e2) (= (num-val e2) 1)) e1]
   ;; n1 * n2 = n1*n2 (fold constants)
   [(and (num? e1) (num? e2))
    (num (* (num-val e1) (num-val e2)))]
   ;; Default: create product
   [else (list '* e1 e2)]))

;;; difference : Expr × Expr → Expr
;;; Create a difference with basic simplifications.
(define (difference e1 e2)
  (cond
   ;; x - 0 = x
   [(and (num? e2) (= (num-val e2) 0)) e1]
   ;; 0 - x = -x
   [(and (num? e1) (= (num-val e1) 0)) (make-neg e2)]
   ;; x - x = 0
   [(expr=? e1 e2) (num 0)]
   ;; n1 - n2 = n1-n2 (fold constants)
   [(and (num? e1) (num? e2))
    (num (- (num-val e1) (num-val e2)))]
   ;; Default
   [else (list '- e1 e2)]))

;;; quotient : Expr × Expr → Expr
;;; Create a quotient with basic simplifications.
(define (quotient e1 e2)
  (cond
   ;; 0 / x = 0 (assuming x != 0)
   [(and (num? e1) (= (num-val e1) 0)) (num 0)]
   ;; x / 1 = x
   [(and (num? e2) (= (num-val e2) 1)) e1]
   ;; x / x = 1 (assuming x != 0)
   [(expr=? e1 e2) (num 1)]
   ;; n1 / n2 when exact
   [(and (num? e1) (num? e2) (integer? (/ (num-val e1) (num-val e2))))
    (num (/ (num-val e1) (num-val e2)))]
   ;; Default
   [else (list '/ e1 e2)]))

;;; power : Expr × Expr → Expr
;;; Create a power with basic simplifications.
(define (power base exp)
  (cond
   ;; x^0 = 1
   [(and (num? exp) (= (num-val exp) 0)) (num 1)]
   ;; x^1 = x
   [(and (num? exp) (= (num-val exp) 1)) base]
   ;; 0^n = 0 (for n > 0)
   [(and (num? base) (= (num-val base) 0)
         (num? exp) (> (num-val exp) 0))
    (num 0)]
   ;; 1^n = 1
   [(and (num? base) (= (num-val base) 1)) (num 1)]
   ;; n1^n2 when both numeric and n2 is small integer
   [(and (num? base) (num? exp)
         (integer? (num-val exp))
         (<= (abs (num-val exp)) 10))
    (num (expt (num-val base) (num-val exp)))]
   ;; Default
   [else (list '^ base exp)]))

;;; ============================================================
;;; Expression Predicates
;;; ============================================================

;;; num? : Any → Boolean
(define (num? e)
  (and (pair? e) (eq? (car e) 'num)))

;;; var? : Any → Boolean
(define (var? e)
  (and (pair? e) (eq? (car e) 'var)))

;;; sum? : Any → Boolean
(define (sum? e)
  (and (pair? e) (eq? (car e) '+)))

;;; product? : Any → Boolean
(define (product? e)
  (and (pair? e) (eq? (car e) '*)))

;;; difference? : Any → Boolean
(define (difference? e)
  (and (pair? e) (eq? (car e) '-)))

;;; quotient? : Any → Boolean
(define (quotient? e)
  (and (pair? e) (eq? (car e) '/)))

;;; power? : Any → Boolean
(define (power? e)
  (and (pair? e) (eq? (car e) '^)))

;;; app? : Any → Boolean
;;; Is e a function application?
(define (app? e)
  (and (pair? e)
       (symbol? (car e))
       (not (memq (car e) '(num var + * - / ^)))))

;;; ============================================================
;;; Expression Accessors
;;; ============================================================

;;; num-val : Expr → Number
(define (num-val e)
  (cadr e))

;;; var-name : Expr → Symbol
(define (var-name e)
  (cadr e))

;;; sum-terms : Expr → (List Expr)
(define (sum-terms e)
  (cdr e))

;;; product-factors : Expr → (List Expr)
(define (product-factors e)
  (cdr e))

;;; diff-left : Expr → Expr
(define (diff-left e)
  (cadr e))

;;; diff-right : Expr → Expr
;;; Returns #f for unary negation.
(define (diff-right e)
  (if (= (length e) 3)
      (caddr e)
      #f))

;;; quot-numer : Expr → Expr
(define (quot-numer e)
  (cadr e))

;;; quot-denom : Expr → Expr
(define (quot-denom e)
  (caddr e))

;;; pow-base : Expr → Expr
(define (pow-base e)
  (cadr e))

;;; pow-exp : Expr → Expr
(define (pow-exp e)
  (caddr e))

;;; app-fn : Expr → Symbol
(define (app-fn e)
  (car e))

;;; app-arg : Expr → Expr
(define (app-arg e)
  (cadr e))

;;; ============================================================
;;; Expression Equality
;;; ============================================================

;;; list-all-equal? : (List Expr) × (List Expr) → Boolean
;;; Check if all corresponding elements are equal.
(define (list-all-equal? l1 l2)
  (cond
   [(and (null? l1) (null? l2)) #t]
   [(or (null? l1) (null? l2)) #f]
   [(not (expr=? (car l1) (car l2))) #f]
   [else (list-all-equal? (cdr l1) (cdr l2))]))

;;; expr=? : Expr × Expr → Boolean
;;; Check structural equality of expressions.
(define (expr=? e1 e2)
  (cond
   ;; Numbers
   [(and (num? e1) (num? e2))
    (= (num-val e1) (num-val e2))]
   ;; Variables
   [(and (var? e1) (var? e2))
    (eq? (var-name e1) (var-name e2))]
   ;; Sums (order matters for now)
   [(and (sum? e1) (sum? e2))
    (and (= (length e1) (length e2))
         (list-all-equal? (cdr e1) (cdr e2)))]
   ;; Products (order matters for now)
   [(and (product? e1) (product? e2))
    (and (= (length e1) (length e2))
         (list-all-equal? (cdr e1) (cdr e2)))]
   ;; Difference
   [(and (difference? e1) (difference? e2))
    (and (= (length e1) (length e2))
         (list-all-equal? (cdr e1) (cdr e2)))]
   ;; Quotient
   [(and (quotient? e1) (quotient? e2))
    (and (expr=? (quot-numer e1) (quot-numer e2))
         (expr=? (quot-denom e1) (quot-denom e2)))]
   ;; Power
   [(and (power? e1) (power? e2))
    (and (expr=? (pow-base e1) (pow-base e2))
         (expr=? (pow-exp e1) (pow-exp e2)))]
   ;; Function application
   [(and (app? e1) (app? e2))
    (and (eq? (app-fn e1) (app-fn e2))
         (expr=? (app-arg e1) (app-arg e2)))]
   ;; Default: not equal
   [else #f]))

;;; ============================================================
;;; Pattern Matching
;;; ============================================================

;;; A pattern is:
;;;   (num _) — match any number, bind to _
;;;   (var _) — match any variable, bind to _
;;;   (+ p1 p2) — match sum
;;;   (* p1 p2) — match product
;;;   (? pred) — match if predicate holds
;;;   _ — match anything (wildcard)
;;;   literal — match exactly

;;; match-expr : Expr × Pattern → Bindings | #f
;;; Try to match expression against pattern.
;;; Returns alist of bindings or #f if no match.
(define (match-expr expr pattern)
  (cond
   ;; Wildcard matches anything
   [(eq? pattern '_) '()]
   
   ;; Match number and capture
   [(and (pair? pattern) (eq? (car pattern) 'num)
         (eq? (cadr pattern) '_)
         (num? expr))
    (list (cons '_ (num-val expr)))]
   
   ;; Match number with specific value
   [(and (pair? pattern) (eq? (car pattern) 'num)
         (number? (cadr pattern)))
    (if (and (num? expr) (= (num-val expr) (cadr pattern)))
        '()
        #f)]
   
   ;; Match variable and capture
   [(and (pair? pattern) (eq? (car pattern) 'var)
         (eq? (cadr pattern) '_)
         (var? expr))
    (list (cons '_ (var-name expr)))]
   
   ;; Match variable with specific name
   [(and (pair? pattern) (eq? (car pattern) 'var)
         (symbol? (cadr pattern)))
    (if (and (var? expr) (eq? (var-name expr) (cadr pattern)))
        '()
        #f)]
   
   ;; Match named wildcard
   [(symbol? pattern)
    (list (cons pattern expr))]
   
   ;; Match sum
   [(and (sum? pattern) (sum? expr))
    (let ([pattern-terms (sum-terms pattern)]
          [expr-terms (sum-terms expr)])
         (if (= (length pattern-terms) (length expr-terms))
             (match-list expr-terms pattern-terms)
             #f))]
   
   ;; Match product
   [(and (product? pattern) (product? expr))
    (let ([pattern-factors (product-factors pattern)]
          [expr-factors (product-factors expr)])
         (if (= (length pattern-factors) (length expr-factors))
             (match-list expr-factors pattern-factors)
             #f))]
   
   ;; Match power
   [(and (power? pattern) (power? expr))
    (let ([b1 (match-expr (pow-base expr) (pow-base pattern))]
          [b2 (match-expr (pow-exp expr) (pow-exp pattern))])
         (if (and b1 b2)
             (append b1 b2)
             #f))]
   
   ;; Match function application
   [(and (app? pattern) (app? expr)
         (eq? (app-fn pattern) (app-fn expr)))
    (match-expr (app-arg expr) (app-arg pattern))]
   
   ;; Literal comparison
   [(equal? expr pattern) '()]
   
   [else #f]))

;;; match-list : (List Expr) × (List Pattern) → Bindings | #f
;;; Match a list of expressions against patterns.
(define (match-list exprs patterns)
  (if (null? exprs)
      '()
      (let ([b (match-expr (car exprs) (car patterns))])
           (if b
               (let ([rest (match-list (cdr exprs) (cdr patterns))])
                    (if rest
                        (append b rest)
                        #f))
               #f))))

;;; ============================================================
;;; Free Variables
;;; ============================================================

;;; free-vars : Expr → (List Symbol)
;;; Get all free variables in an expression.
(define (free-vars e)
  (cond
   [(num? e) '()]
   [(var? e) (list (var-name e))]
   [(sum? e) (unique (apply append (map free-vars (sum-terms e))))]
   [(product? e) (unique (apply append (map free-vars (product-factors e))))]
   [(difference? e)
    (unique (append (free-vars (diff-left e))
                    (if (diff-right e) (free-vars (diff-right e)) '())))]
   [(quotient? e)
    (unique (append (free-vars (quot-numer e))
                    (free-vars (quot-denom e))))]
   [(power? e)
    (unique (append (free-vars (pow-base e))
                    (free-vars (pow-exp e))))]
   [(app? e) (free-vars (app-arg e))]
   [else '()]))

;;; ============================================================
;;; Substitution
;;; ============================================================

;;; subst : Expr × Symbol × Expr → Expr
;;; Substitute val for var-sym in expression.
(define (subst expr var-sym val)
  (cond
   [(num? expr) expr]
   [(var? expr)
    (if (eq? (var-name expr) var-sym)
        val
        expr)]
   [(sum? expr)
    (make-sum (map (lambda (e) (subst e var-sym val)) (sum-terms expr)))]
   [(product? expr)
    (make-product (map (lambda (e) (subst e var-sym val)) (product-factors expr)))]
   [(difference? expr)
    (if (diff-right expr)
        (make-diff (subst (diff-left expr) var-sym val)
                   (subst (diff-right expr) var-sym val))
        (make-neg (subst (diff-left expr) var-sym val)))]
   [(quotient? expr)
    (make-div (subst (quot-numer expr) var-sym val)
              (subst (quot-denom expr) var-sym val))]
   [(power? expr)
    (make-pow (subst (pow-base expr) var-sym val)
              (subst (pow-exp expr) var-sym val))]
   [(app? expr)
    (make-app (app-fn expr) (subst (app-arg expr) var-sym val))]
   [else expr]))

;;; ============================================================
;;; Expression Display
;;; ============================================================

;;; expr->string : Expr → String
;;; Convert expression to readable string.
(define (expr->string e)
  (cond
   [(num? e) (number->string (num-val e))]
   [(var? e) (symbol->string (var-name e))]
   [(sum? e)
    (string-append "("
                   (string-join (map expr->string (sum-terms e)) " + ")
                   ")")]
   [(product? e)
    (string-append "("
                   (string-join (map expr->string (product-factors e)) " * ")
                   ")")]
   [(difference? e)
    (if (diff-right e)
        (string-append "(" (expr->string (diff-left e))
                       " - " (expr->string (diff-right e)) ")")
        (string-append "-" (expr->string (diff-left e))))]
   [(quotient? e)
    (string-append "(" (expr->string (quot-numer e))
                   " / " (expr->string (quot-denom e)) ")")]
   [(power? e)
    (string-append "(" (expr->string (pow-base e))
                   "^" (expr->string (pow-exp e)) ")")]
   [(app? e)
    (string-append (symbol->string (app-fn e))
                   "(" (expr->string (app-arg e)) ")")]
   [else "?"]))

;;; ============================================================
;;; Common Function Constructors
;;; ============================================================

;;; Trigonometric functions
(define (sym-sin e) (make-app 'sin e))
(define (sym-cos e) (make-app 'cos e))
(define (sym-tan e) (make-app 'tan e))

;;; Exponential and logarithmic
(define (sym-exp e) (make-app 'exp e))
(define (sym-log e) (make-app 'log e))

;;; Square root
(define (sym-sqrt e) (make-app 'sqrt e))

;;; Common constants
(define sym-pi (var 'pi))
(define sym-e (var 'e))
