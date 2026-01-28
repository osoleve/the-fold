(load "core/base/prelude.ss")

(doc 'module 'expr)
(doc 'description "Core symbolic expression data structures for computer algebra")
(doc 'note "Uses S-expressions as natural representation")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'types "num, var, +, *, -, /, ^, function-application")

(doc 'section 'expression-constructors)

(define (num n)
  (doc 'type '(-> Number Expr))
  (doc 'description "Create numeric constant expression")
  (list 'num n))

(define (var x)
  (doc 'type '(-> Symbol Expr))
  (doc 'description "Create symbolic variable expression")
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

(doc 'section 'smart-constructors)

(define (sum e1 e2)
  (doc 'type '(-> Expr Expr Expr))
  (doc 'description "Create sum with basic simplifications")
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

(define (product e1 e2)
  (doc 'type '(-> Expr Expr Expr))
  (doc 'description "Create product with basic simplifications")
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

(define (difference e1 e2)
  (doc 'type '(-> Expr Expr Expr))
  (doc 'description "Create difference with basic simplifications")
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

(define (quotient e1 e2)
  (doc 'type '(-> Expr Expr Expr))
  (doc 'description "Create quotient with basic simplifications")
  (cond
   ;; Division by zero: return symbolic form (boundary layer validates)
   [(and (num? e2) (= (num-val e2) 0))
    (list '/ e1 e2)]
   ;; 0 / x = 0 (assuming x != 0)
   [(and (num? e1) (= (num-val e1) 0)) (num 0)]
   ;; x / 1 = x
   [(and (num? e2) (= (num-val e2) 1)) e1]
   ;; x / x = 1 (assuming x != 0)
   [(expr=? e1 e2) (num 1)]
   ;; n1 / n2 when exact (and e2 != 0, checked above)
   [(and (num? e1) (num? e2) (integer? (/ (num-val e1) (num-val e2))))
    (num (/ (num-val e1) (num-val e2)))]
   ;; Default
   [else (list '/ e1 e2)]))

(define (power base exp)
  (doc 'type '(-> Expr Expr Expr))
  (doc 'description "Create power with basic simplifications")
  (doc 'note "0^0 returns 1 following CAS convention")
  (cond
   ;; x^0 = 1 (including 0^0 = 1, see note above)
   [(and (num? exp) (= (num-val exp) 0)) (num 1)]
   ;; x^1 = x
   [(and (num? exp) (= (num-val exp) 1)) base]
   ;; 0^n: returns 0 for n > 0, error for n < 0
   [(and (num? base) (= (num-val base) 0) (num? exp))
    (cond
     [(> (num-val exp) 0) (num 0)]
     [(< (num-val exp) 0)
      ;; 0^negative is undefined - return symbolic form
      ;; Boundary layer should validate/reject if needed
      (list '^ base exp)]
     [else (num 1)])]  ;; 0^0 case
   ;; 1^n = 1
   [(and (num? base) (= (num-val base) 1)) (num 1)]
   ;; n1^n2 when both numeric and n2 is small integer
   [(and (num? base) (num? exp)
         (integer? (num-val exp))
         (<= (abs (num-val exp)) 10))
    (num (expt (num-val base) (num-val exp)))]
   ;; Default
   [else (list '^ base exp)]))

(doc 'section 'predicates)

(define (num? e)
  (doc 'type '(-> α Bool))
  (doc 'description "Test if expression is numeric constant")
  (and (pair? e) (eq? (car e) 'num)))

;;; var? : α → Bool
(define (var? e)
  (and (pair? e) (eq? (car e) 'var)))

;;; sum? : α → Bool
(define (sum? e)
  (and (pair? e) (eq? (car e) '+)))

;;; product? : α → Bool
(define (product? e)
  (and (pair? e) (eq? (car e) '*)))

;;; difference? : α → Bool
(define (difference? e)
  (and (pair? e) (eq? (car e) '-)))

;;; quotient? : α → Bool
(define (quotient? e)
  (and (pair? e) (eq? (car e) '/)))

;;; power? : α → Bool
(define (power? e)
  (and (pair? e) (eq? (car e) '^)))

;;; app? : α → Bool
;;; Is e a function application?
(define (app? e)
  (and (pair? e)
       (symbol? (car e))
       (not (memq (car e) '(num var + * - / ^)))))

;;; ====
;;; Expression Accessors
;;; ====

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

;;; ====
;;; Expression Equality
;;; ====

;;; list-all-equal? : (List Expr) × (List Expr) → Bool
;;; Check if all corresponding elements are equal.
(define (list-all-equal? l1 l2)
  (cond
   [(and (null? l1) (null? l2)) #t]
   [(or (null? l1) (null? l2)) #f]
   [(not (expr=? (car l1) (car l2))) #f]
   [else (list-all-equal? (cdr l1) (cdr l2))]))

;;; expr=? : Expr × Expr → Bool
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

(doc 'section 'pattern-matching)
(doc 'note "Patterns: (num _), (var _), (+ p1 p2), (* p1 p2), (? pred), _, literal")

(define (match-expr expr pattern)
  (doc 'type '(-> Expr Pattern (Maybe Bindings)))
  (doc 'description "Try to match expression against pattern, returns alist of bindings or #f")
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
             (merge-bindings b1 b2)
             #f))]
   
   ;; Match difference
   [(and (difference? pattern) (difference? expr))
    (let ([b1 (match-expr (diff-left expr) (diff-left pattern))]
          [b2 (if (and (diff-right pattern) (diff-right expr))
                  (match-expr (diff-right expr) (diff-right pattern))
                  (if (and (not (diff-right pattern)) (not (diff-right expr)))
                      '()
                      #f))])
         (if (and b1 b2)
             (merge-bindings b1 b2)
             #f))]
   
   ;; Match quotient
   [(and (quotient? pattern) (quotient? expr))
    (let ([b1 (match-expr (quot-numer expr) (quot-numer pattern))]
          [b2 (match-expr (quot-denom expr) (quot-denom pattern))])
         (if (and b1 b2)
             (merge-bindings b1 b2)
             #f))]
   
   ;; Match function application
   [(and (app? pattern) (app? expr)
         (eq? (app-fn pattern) (app-fn expr)))
    (match-expr (app-arg expr) (app-arg pattern))]
   
   ;; Literal comparison
   [(equal? expr pattern) '()]
   
   [else #f]))

;;; bindings-consistent? : Bindings × Bindings → Bool
;;; Check if two binding lists are consistent (no conflicting values for same var).
(define (bindings-consistent? b1 b2)
  (let loop ([bindings b1])
       (if (null? bindings)
           #t
           (let* ([pair (car bindings)]
                  [var (car pair)]
                  [val (cdr pair)]
                  [existing (assq var b2)])
                 (if existing
                     (if (equal? val (cdr existing))
                         (loop (cdr bindings))
                         #f)  ; Conflict: same var, different value
                     (loop (cdr bindings)))))))

;;; merge-bindings : Bindings × Bindings → Bindings | #f
;;; Merge two binding lists, returning #f if inconsistent.
(define (merge-bindings b1 b2)
  (if (bindings-consistent? b1 b2)
      (append b1 (filter (lambda (p) (not (assq (car p) b1))) b2))
      #f))

;;; match-list : (List Expr) × (List Pattern) → Bindings | #f
;;; Match a list of expressions against patterns.
(define (match-list exprs patterns)
  (if (null? exprs)
      '()
      (let ([b (match-expr (car exprs) (car patterns))])
           (if b
               (let ([rest (match-list (cdr exprs) (cdr patterns))])
                    (if rest
                        (merge-bindings b rest)
                        #f))
               #f))))

(doc 'section 'free-variables)

(define (free-vars e)
  (doc 'type '(-> Expr (List Symbol)))
  (doc 'description "Get all free variables in expression")
  (cond
   [(num? e) '()]
   [(var? e) (list (var-name e))]
   [(sum? e) (unique (append-map free-vars (sum-terms e)))]
   [(product? e) (unique (append-map free-vars (product-factors e)))]
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

(doc 'section 'substitution)

(define (subst expr var-sym val)
  (doc 'type '(-> Expr Symbol Expr Expr))
  (doc 'description "Substitute val for var-sym in expression using smart constructors")
  (cond
   [(num? expr) expr]
   [(var? expr)
    (if (eq? (var-name expr) var-sym)
        val
        expr)]
   [(sum? expr)
    ;; Use smart constructor for binary sums, fallback for n-ary
    (let ([subst-terms (map (lambda (e) (subst e var-sym val)) (sum-terms expr))])
         (if (= (length subst-terms) 2)
             (sum (car subst-terms) (cadr subst-terms))
             (make-sum subst-terms)))]
   [(product? expr)
    ;; Use smart constructor for binary products, fallback for n-ary
    (let ([subst-factors (map (lambda (e) (subst e var-sym val)) (product-factors expr))])
         (if (= (length subst-factors) 2)
             (product (car subst-factors) (cadr subst-factors))
             (make-product subst-factors)))]
   [(difference? expr)
    (if (diff-right expr)
        (difference (subst (diff-left expr) var-sym val)
                    (subst (diff-right expr) var-sym val))
        (make-neg (subst (diff-left expr) var-sym val)))]
   [(quotient? expr)
    (quotient (subst (quot-numer expr) var-sym val)
              (subst (quot-denom expr) var-sym val))]
   [(power? expr)
    (power (subst (pow-base expr) var-sym val)
           (subst (pow-exp expr) var-sym val))]
   [(app? expr)
    (make-app (app-fn expr) (subst (app-arg expr) var-sym val))]
   [else expr]))

(doc 'section 'display)

(define (expr->string e)
  (doc 'type '(-> Expr String))
  (doc 'description "Convert expression to readable string")
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

(doc 'section 'common-functions)

(doc sym-sin 'type '(-> Expr Expr))
(doc sym-sin 'description "Trigonometric sine function")
(define (sym-sin e) (make-app 'sin e))

(doc sym-cos 'type '(-> Expr Expr))
(doc sym-cos 'description "Trigonometric cosine function")
(define (sym-cos e) (make-app 'cos e))

;;; sym-tan : Expr → Expr
;;; Trigonometric tangent function.
(define (sym-tan e) (make-app 'tan e))

;;; sym-exp : Expr → Expr
;;; Exponential function.
(define (sym-exp e) (make-app 'exp e))

;;; sym-log : Expr → Expr
;;; Natural logarithm.
(define (sym-log e) (make-app 'log e))

;;; sym-sqrt : Expr → Expr
;;; Square root.
(define (sym-sqrt e) (make-app 'sqrt e))

;;; sym-sinh : Expr → Expr
;;; Hyperbolic sine.
(define (sym-sinh e) (make-app 'sinh e))

;;; sym-cosh : Expr → Expr
;;; Hyperbolic cosine.
(define (sym-cosh e) (make-app 'cosh e))

;;; sym-tanh : Expr → Expr
;;; Hyperbolic tangent.
(define (sym-tanh e) (make-app 'tanh e))

;;; sym-atan : Expr → Expr
;;; Arctangent.
(define (sym-atan e) (make-app 'atan e))

;;; sym-asin : Expr → Expr
;;; Arcsine.
(define (sym-asin e) (make-app 'asin e))

;;; sym-acos : Expr → Expr
;;; Arccosine.
(define (sym-acos e) (make-app 'acos e))

;;; sym-pi : Expr
;;; Constant pi.
(define sym-pi (var 'pi))

;;; sym-e : Expr
;;; Constant e (Euler's number).
(define sym-e (var 'e))
