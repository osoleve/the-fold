;;; lattice/fp/symbolic/simplify.ss — Algebraic Simplification
;;;
;;; Comprehensive algebraic simplification for symbolic expressions.
;;; Implements:
;;;   - Collect like terms
;;;   - Expand products
;;;   - Factor expressions
;;;   - Trigonometric identities
;;;   - Logarithm/exponential rules
;;;   - Canonical form conversion
;;;   - Simplification heuristics
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - lattice/fp/symbolic/expr.ss

(load "lattice/fp/symbolic/expr.ss")

;;; ====
;;; Helper Functions
;;; ====

;;; filter-map : (α → β | #f) × (List α) → (List β)
(define (filter-map f lst)
  (let loop ([lst lst] [acc '()])
       (if (null? lst)
           (reverse acc)
           (let ([result (f (car lst))])
                (if result
                    (loop (cdr lst) (cons result acc))
                    (loop (cdr lst) acc))))))

;;; get-numeric : Expr → Number | #f
(define (get-numeric e)
  (if (num? e) (num-val e) #f))

;;; ====
;;; Main Simplification Entry Point
;;; ====

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

;;; ====
;;; Sum Simplification
;;; ====

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
;;; Flatten nested sums: (+ a (+ b c)) → (a b c)
(define (flatten-sums terms)
  (apply append
         (map (lambda (e)
                      (if (sum? e)
                          (flatten-sums (sum-terms e))
                          (list e)))
              terms)))

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
       ;; Rebuild terms from hashtable keys in deterministic order
       (let* ([keys (vector->list (hashtable-keys table))]
              [sorted-keys (sort (lambda (a b) (< (expr-complexity a) (expr-complexity b))) keys)])
             (filter-map
              (lambda (key)
                      (let ([coef (hashtable-ref table key 0)])
                           (cond
                            [(= coef 0) #f]
                            [(= coef 1) key]
                            [else (product (num coef) key)])))
              sorted-keys))))

;;; extract-coefficient : Expr → (Values Number Expr)
;;; Extract numeric coefficient from a term.
;;; x → (1, x), 2*x → (2, x), -x → (-1, x)
(define (extract-coefficient term)
  (cond
   [(num? term) (values (num-val term) (num 1))]
   [(and (product? term)
         (>= (length (product-factors term)) 2)
         (num? (car (product-factors term))))
    (let ([coef (num-val (car (product-factors term)))]
          [rest (cdr (product-factors term))])
         (values coef (if (= (length rest) 1)
                          (car rest)
                          (cons '* rest))))]
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

;;; ====
;;; Product Simplification
;;; ====

;;; simplify-product : (List Expr) → Expr
;;; Simplify a product by collecting numeric factors and combining powers.
(define (simplify-product factors)
  (let* ([flat-factors (flatten-products factors)]
         [numeric-prod (apply * (filter-map get-numeric flat-factors))]
         [non-numeric (filter (lambda (e) (not (num? e))) flat-factors)]
         [collected (collect-like-bases non-numeric)])
        (cond
         ;; Zero factor
         [(= numeric-prod 0) (num 0)]
         ;; All ones
         [(and (null? collected) (= numeric-prod 1)) (num 1)]
         ;; Only numeric
         [(null? collected) (num numeric-prod)]
         ;; Numeric is 1
         [(= numeric-prod 1) (make-product-from-list collected)]
         ;; Both
         [else (make-product-from-list (cons (num numeric-prod) collected))])))

;;; flatten-products : (List Expr) → (List Expr)
(define (flatten-products factors)
  (apply append
         (map (lambda (e)
                      (if (product? e)
                          (flatten-products (product-factors e))
                          (list e)))
              factors)))

;;; collect-like-bases : (List Expr) → (List Expr)
;;; Collect terms with same base: x * x → x^2
(define (collect-like-bases factors)
  (let ([table (make-hashtable equal-hash equal?)])
       ;; Sum exponents for each base
       (for-each
        (lambda (factor)
                (let-values ([(base exp) (extract-base-exponent factor)])
                            (let ([current (hashtable-ref table base (num 0))])
                                 (hashtable-set! table base (sum current exp)))))
        factors)
       ;; Rebuild factors in deterministic order (sorted by complexity)
       (let* ([keys (vector->list (hashtable-keys table))]
              [sorted-keys (sort (lambda (a b) (< (expr-complexity a) (expr-complexity b))) keys)])
             (filter-map
              (lambda (key)
                      (let ([exp (simplify (hashtable-ref table key (num 0)))])
                           (cond
                            [(and (num? exp) (= (num-val exp) 0)) #f]
                            [(and (num? exp) (= (num-val exp) 1)) key]
                            [else (power key exp)])))
              sorted-keys))))

;;; extract-base-exponent : Expr → (Values Expr Expr)
;;; Extract base and exponent: x^2 → (x, 2), x → (x, 1)
(define (extract-base-exponent expr)
  (if (power? expr)
      (values (pow-base expr) (pow-exp expr))
      (values expr (num 1))))

;;; make-product-from-list : (List Expr) → Expr
(define (make-product-from-list factors)
  (cond
   [(null? factors) (num 1)]
   [(= (length factors) 1) (car factors)]
   [else (fold-left product (car factors) (cdr factors))]))

;;; ====
;;; Difference and Negation Simplification
;;; ====

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
   ;; x - (-y) = x + y
   [(and (difference? right) (not (diff-right right)))
    (sum left (diff-left right))]
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
         (>= (length (product-factors e)) 2)
         (num? (car (product-factors e))))
    (let ([coef (num-val (car (product-factors e)))]
          [rest (cdr (product-factors e))])
         (product (num (- coef))
                  (if (= (length rest) 1)
                      (car rest)
                      (cons '* rest))))]
   [else (make-neg e)]))

;;; ====
;;; Quotient Simplification
;;; ====

;;; simplify-quot : Expr × Expr → Expr
(define (simplify-quot numer denom)
  (cond
   ;; 0/x = 0
   [(and (num? numer) (= (num-val numer) 0)) (num 0)]
   ;; x/1 = x
   [(and (num? denom) (= (num-val denom) 1)) numer]
   ;; n/m = n/m as a single numeric value
   [(and (num? numer) (num? denom))
    (num (/ (num-val numer) (num-val denom)))]
   ;; x/x = 1
   [(expr=? numer denom) (num 1)]
   ;; (c*expr)/c = expr (factor cancellation with numeric denominator)
   [(and (product? numer) (num? denom))
    (let ([factors (product-factors numer)])
         (if (and (>= (length factors) 2)
                  (num? (car factors))
                  (= (num-val (car factors)) (num-val denom)))
             ;; c matches, return rest of product
             (make-product-from-list (cdr factors))
             ;; Check if first factor is divisible by denom
             (if (and (num? (car factors)))
                 (let* ([c (num-val (car factors))]
                        [d (num-val denom)]
                        [ratio (/ c d)])
                       (if (integer? ratio)
                           (make-product-from-list
                            (cons (num ratio) (cdr factors)))
                           (quotient numer denom)))
                 (quotient numer denom))))]
   ;; (a*x)/(b*x) = a/b (cancel common factors)
   [(and (product? numer) (product? denom))
    (let ([simplified (cancel-common-factors
                       (product-factors numer)
                       (product-factors denom))])
         (if simplified
             (quotient (make-product-from-list (car simplified))
                       (make-product-from-list (cadr simplified)))
             (quotient numer denom)))]
   ;; x^a / x^b = x^(a-b)
   [(and (power? numer) (power? denom)
         (expr=? (pow-base numer) (pow-base denom)))
    (power (pow-base numer)
           (simplify (difference (pow-exp numer) (pow-exp denom))))]
   [else (quotient numer denom)]))

;;; cancel-common-factors : (List Expr) × (List Expr) → ((List Expr) (List Expr)) | #f
;;; Try to cancel common factors between numerator and denominator.
(define (cancel-common-factors numer-factors denom-factors)
  (let loop ([numer numer-factors]
             [denom denom-factors]
             [numer-acc '()])
       (if (null? numer)
           (if (or (not (equal? numer-acc numer-factors))
                   (not (equal? denom denom-factors)))
               (list (reverse numer-acc) denom)
               #f)
           (let ([found (find-and-remove (car numer) denom)])
                (if found
                    (loop (cdr numer) found numer-acc)
                    (loop (cdr numer) denom (cons (car numer) numer-acc)))))))

;;; find-and-remove : Expr × (List Expr) → (List Expr) | #f
;;; Find expr in list and return list with it removed.
(define (find-and-remove expr lst)
  (let loop ([rest lst] [acc '()])
       (cond
        [(null? rest) #f]
        [(expr=? (car rest) expr)
         (append (reverse acc) (cdr rest))]
        [else (loop (cdr rest) (cons (car rest) acc))])))

;;; ====
;;; Power Simplification
;;; ====

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
   ;; (x^a)^b = x^(a*b)
   [(power? base)
    (power (pow-base base)
           (simplify (product (pow-exp base) exp)))]
   ;; (a*b)^n = a^n * b^n (for numeric n)
   [(and (product? base) (num? exp))
    (make-product-from-list
     (map (lambda (f) (simplify-pow f exp))
          (product-factors base)))]
   [else (power base exp)]))

;;; ====
;;; Function Application Simplification
;;; ====

;;; simplify-app : Symbol × Expr → Expr
(define (simplify-app fn arg)
  (cond
   ;; === Basic trig simplifications ===
   ;; sin(0) = 0
   [(and (eq? fn 'sin) (num? arg) (= (num-val arg) 0)) (num 0)]
   ;; cos(0) = 1
   [(and (eq? fn 'cos) (num? arg) (= (num-val arg) 0)) (num 1)]
   ;; tan(0) = 0
   [(and (eq? fn 'tan) (num? arg) (= (num-val arg) 0)) (num 0)]
   
   ;; === Exponential/logarithm simplifications ===
   ;; exp(0) = 1
   [(and (eq? fn 'exp) (num? arg) (= (num-val arg) 0)) (num 1)]
   ;; log(1) = 0
   [(and (eq? fn 'log) (num? arg) (= (num-val arg) 1)) (num 0)]
   ;; log(e) = 1
   [(and (eq? fn 'log) (var? arg) (eq? (var-name arg) 'e)) (num 1)]
   ;; exp(log(x)) = x
   [(and (eq? fn 'exp) (app? arg) (eq? (app-fn arg) 'log))
    (app-arg arg)]
   ;; log(exp(x)) = x
   [(and (eq? fn 'log) (app? arg) (eq? (app-fn arg) 'exp))
    (app-arg arg)]
   ;; log(x^n) = n*log(x)
   [(and (eq? fn 'log) (power? arg))
    (product (pow-exp arg) (make-app 'log (pow-base arg)))]
   
   ;; === Square root ===
   ;; sqrt(0) = 0, sqrt(1) = 1
   [(and (eq? fn 'sqrt) (num? arg) (= (num-val arg) 0)) (num 0)]
   [(and (eq? fn 'sqrt) (num? arg) (= (num-val arg) 1)) (num 1)]
   ;; sqrt(x^2) = |x| (simplified to x for now, shell validates)
   [(and (eq? fn 'sqrt) (power? arg)
         (num? (pow-exp arg)) (= (num-val (pow-exp arg)) 2))
    (make-app 'abs (pow-base arg))]
   
   ;; === Hyperbolic ===
   ;; sinh(0) = 0, cosh(0) = 1, tanh(0) = 0
   [(and (eq? fn 'sinh) (num? arg) (= (num-val arg) 0)) (num 0)]
   [(and (eq? fn 'cosh) (num? arg) (= (num-val arg) 0)) (num 1)]
   [(and (eq? fn 'tanh) (num? arg) (= (num-val arg) 0)) (num 0)]
   
   ;; === Inverse trig ===
   ;; atan(0) = 0, asin(0) = 0, acos(1) = 0
   [(and (eq? fn 'atan) (num? arg) (= (num-val arg) 0)) (num 0)]
   [(and (eq? fn 'asin) (num? arg) (= (num-val arg) 0)) (num 0)]
   [(and (eq? fn 'acos) (num? arg) (= (num-val arg) 1)) (num 0)]
   
   ;; === Inverse of inverse ===
   ;; sin(asin(x)) = x
   [(and (eq? fn 'sin) (app? arg) (eq? (app-fn arg) 'asin))
    (app-arg arg)]
   ;; cos(acos(x)) = x
   [(and (eq? fn 'cos) (app? arg) (eq? (app-fn arg) 'acos))
    (app-arg arg)]
   ;; tan(atan(x)) = x
   [(and (eq? fn 'tan) (app? arg) (eq? (app-fn arg) 'atan))
    (app-arg arg)]
   
   [else (make-app fn arg)]))

;;; ====
;;; Product Expansion
;;; ====

;;; expand : Expr → Expr
;;; Fully expand products and powers in an expression.
;;; (a + b) * (c + d) → a*c + a*d + b*c + b*d
(define (expand expr)
  (let ([expanded (expand-once expr)])
       (if (expr=? expanded expr)
           expanded
           (expand expanded))))

;;; expand-once : Expr → Expr
;;; Single pass of expansion.
(define (expand-once expr)
  (cond
   [(num? expr) expr]
   [(var? expr) expr]
   
   [(sum? expr)
    (make-sum-from-list (map expand-once (sum-terms expr)))]
   
   [(product? expr)
    (expand-product (map expand-once (product-factors expr)))]
   
   [(difference? expr)
    (if (diff-right expr)
        (difference (expand-once (diff-left expr))
                    (expand-once (diff-right expr)))
        (make-neg (expand-once (diff-left expr))))]
   
   [(quotient? expr)
    (quotient (expand-once (quot-numer expr))
              (expand-once (quot-denom expr)))]
   
   [(power? expr)
    (let ([base (expand-once (pow-base expr))]
          [exp (expand-once (pow-exp expr))])
         ;; (a+b)^n where n is small positive integer
         (if (and (num? exp)
                  (integer? (num-val exp))
                  (> (num-val exp) 0)
                  (<= (num-val exp) 6)
                  (sum? base))
             (expand-power-of-sum base (num-val exp))
             (power base exp)))]
   
   [(app? expr)
    (make-app (app-fn expr) (expand-once (app-arg expr)))]
   
   [else expr]))

;;; expand-product : (List Expr) → Expr
;;; Expand a product by distributing over sums.
(define (expand-product factors)
  (cond
   [(null? factors) (num 1)]
   [(= (length factors) 1) (car factors)]
   [else
    (let ([first (car factors)]
          [rest-expanded (expand-product (cdr factors))])
         (cond
          ;; (a+b) * rest → a*rest + b*rest
          [(sum? first)
           (make-sum-from-list
            (map (lambda (term)
                         (simplify (product term rest-expanded)))
                 (sum-terms first)))]
          ;; first * (a+b) → first*a + first*b
          [(sum? rest-expanded)
           (make-sum-from-list
            (map (lambda (term)
                         (simplify (product first term)))
                 (sum-terms rest-expanded)))]
          [else (product first rest-expanded)]))]))

;;; expand-power-of-sum : Expr × Integer → Expr
;;; Expand (a+b+...)^n using repeated multiplication.
(define (expand-power-of-sum base n)
  (if (<= n 1)
      base
      (let loop ([acc base] [i 1])
           (if (>= i n)
               acc
               (loop (expand-once (product acc base)) (+ i 1))))))

;;; ====
;;; Factoring (Basic)
;;; ====

;;; factor : Expr → Expr
;;; Try to factor an expression.
;;; Currently supports:
;;;   - Extract common factors from sums
;;;   - Difference of squares: a^2 - b^2 → (a+b)(a-b)
;;;   - Perfect square trinomials
(define (factor expr)
  (cond
   [(sum? expr)
    (let ([factored (factor-sum (sum-terms expr))])
         (if factored factored expr))]
   [(difference? expr)
    (cond
     ;; a^2 - b^2 = (a+b)(a-b)
     [(and (diff-right expr)
           (power? (diff-left expr))
           (power? (diff-right expr))
           (num? (pow-exp (diff-left expr)))
           (= (num-val (pow-exp (diff-left expr))) 2)
           (num? (pow-exp (diff-right expr)))
           (= (num-val (pow-exp (diff-right expr))) 2))
      (let ([a (pow-base (diff-left expr))]
            [b (pow-base (diff-right expr))])
           (product (sum a b) (difference a b)))]
     [else expr])]
   [else expr]))

;;; factor-sum : (List Expr) → Expr | #f
;;; Try to factor a sum by extracting common factors.
(define (factor-sum terms)
  (let ([common (find-common-factor terms)])
       (if (and common (not (and (num? common) (= (num-val common) 1))))
           (let ([remaining (map (lambda (t) (divide-out t common)) terms)])
                (product common (make-sum-from-list remaining)))
           #f)))

;;; find-common-factor : (List Expr) → Expr | #f
;;; Find a factor common to all terms.
(define (find-common-factor terms)
  (if (null? terms)
      #f
      (let* ([first-factors (get-factors (car terms))]
             [common-factors
              (filter (lambda (f)
                              (for-all (lambda (t) (divides? f t)) (cdr terms)))
                      first-factors)])
            (if (null? common-factors)
                (num 1)
                (car common-factors)))))

;;; get-factors : Expr → (List Expr)
;;; Get the factors of an expression.
(define (get-factors expr)
  (cond
   [(product? expr) (product-factors expr)]
   [(num? expr) (list expr)]
   [else (list expr)]))

;;; divides? : Expr × Expr → Bool
;;; Check if factor divides term (loosely).
(define (divides? factor term)
  (cond
   [(expr=? factor term) #t]
   [(product? term)
    (exists (lambda (f) (expr=? factor f)) (product-factors term))]
   [(and (num? factor) (num? term))
    (integer? (/ (num-val term) (num-val factor)))]
   [else #f]))

;;; divide-out : Expr × Expr → Expr
;;; Divide factor out of term.
(define (divide-out term factor)
  (cond
   [(expr=? factor term) (num 1)]
   [(product? term)
    (let ([remaining (find-and-remove factor (product-factors term))])
         (if remaining
             (make-product-from-list remaining)
             (quotient term factor)))]
   [(and (num? factor) (num? term))
    (num (/ (num-val term) (num-val factor)))]
   [else (quotient term factor)]))

;;; ====
;;; Trigonometric Identities
;;; ====

;;; simplify-trig : Expr → Expr
;;; Apply trigonometric identities.
(define (simplify-trig expr)
  (let ([simplified (simplify-trig-once expr)])
       (if (expr=? simplified expr)
           simplified
           (simplify-trig simplified))))

;;; simplify-trig-once : Expr → Expr
(define (simplify-trig-once expr)
  (cond
   [(num? expr) expr]
   [(var? expr) expr]
   
   [(sum? expr)
    (let ([terms (map simplify-trig-once (sum-terms expr))])
         ;; sin^2(x) + cos^2(x) = 1
         (simplify-pythagorean-identity terms))]
   
   [(product? expr)
    (let ([factors (map simplify-trig-once (product-factors expr))])
         ;; 2*sin(x)*cos(x) = sin(2x) - double angle
         (simplify-double-angle-product factors))]
   
   [(difference? expr)
    (if (diff-right expr)
        (let ([left (simplify-trig-once (diff-left expr))]
              [right (simplify-trig-once (diff-right expr))])
             ;; cos^2(x) - sin^2(x) = cos(2x)
             (if (and (power? left) (power? right)
                      (app? (pow-base left)) (app? (pow-base right))
                      (num? (pow-exp left)) (= (num-val (pow-exp left)) 2)
                      (num? (pow-exp right)) (= (num-val (pow-exp right)) 2)
                      (eq? (app-fn (pow-base left)) 'cos)
                      (eq? (app-fn (pow-base right)) 'sin)
                      (expr=? (app-arg (pow-base left)) (app-arg (pow-base right))))
                 (make-app 'cos (product (num 2) (app-arg (pow-base left))))
                 (difference left right)))
        (make-neg (simplify-trig-once (diff-left expr))))]
   
   [(power? expr)
    (power (simplify-trig-once (pow-base expr))
           (simplify-trig-once (pow-exp expr)))]
   
   [(quotient? expr)
    (let ([numer (simplify-trig-once (quot-numer expr))]
          [denom (simplify-trig-once (quot-denom expr))])
         ;; sin(x)/cos(x) = tan(x)
         (if (and (app? numer) (app? denom)
                  (eq? (app-fn numer) 'sin)
                  (eq? (app-fn denom) 'cos)
                  (expr=? (app-arg numer) (app-arg denom)))
             (make-app 'tan (app-arg numer))
             (quotient numer denom)))]
   
   [(app? expr)
    (make-app (app-fn expr) (simplify-trig-once (app-arg expr)))]
   
   [else expr]))

;;; simplify-pythagorean-identity : (List Expr) → Expr
;;; sin^2(x) + cos^2(x) = 1
(define (simplify-pythagorean-identity terms)
  (let loop ([remaining terms]
             [checked '()])
       (if (null? remaining)
           (make-sum-from-list (reverse checked))
           (let ([sin-sq-match (find-sin-squared (car remaining))]
                 [cos-sq (find-matching-cos-squared (car remaining) (cdr remaining))])
                (if cos-sq
                    ;; Found sin^2 + cos^2, replace with 1
                    (let ([new-terms (cons (num 1)
                                           (append (reverse checked)
                                                   (remove-first cos-sq (cdr remaining))))])
                         (simplify-pythagorean-identity new-terms))
                    (loop (cdr remaining) (cons (car remaining) checked)))))))

;;; find-sin-squared : Expr → Expr | #f
;;; Check if expression is sin^2(x), return x if so.
(define (find-sin-squared expr)
  (if (and (power? expr)
           (num? (pow-exp expr))
           (= (num-val (pow-exp expr)) 2)
           (app? (pow-base expr))
           (eq? (app-fn (pow-base expr)) 'sin))
      (app-arg (pow-base expr))
      #f))

;;; find-matching-cos-squared : Expr × (List Expr) → Expr | #f
;;; Find cos^2(x) in terms matching sin^2(x).
(define (find-matching-cos-squared sin-sq-term terms)
  (let ([arg (find-sin-squared sin-sq-term)])
       (if arg
           (find (lambda (t)
                         (and (power? t)
                              (num? (pow-exp t))
                              (= (num-val (pow-exp t)) 2)
                              (app? (pow-base t))
                              (eq? (app-fn (pow-base t)) 'cos)
                              (expr=? (app-arg (pow-base t)) arg)))
                 terms)
           #f)))

;;; simplify-double-angle-product : (List Expr) → Expr
;;; 2*sin(x)*cos(x) = sin(2x)
(define (simplify-double-angle-product factors)
  ;; Look for pattern: 2 * sin(x) * cos(x)
  (let ([has-two (exists (lambda (f) (and (num? f) (= (num-val f) 2))) factors)]
        [sin-term (find (lambda (f) (and (app? f) (eq? (app-fn f) 'sin))) factors)]
        [cos-term (find (lambda (f) (and (app? f) (eq? (app-fn f) 'cos))) factors)])
       (if (and has-two sin-term cos-term
                (expr=? (app-arg sin-term) (app-arg cos-term)))
           ;; Replace 2*sin(x)*cos(x) with sin(2x)
           (let ([other-factors (filter (lambda (f)
                                                (and (not (and (num? f) (= (num-val f) 2)))
                                                     (not (eq? f sin-term))
                                                     (not (eq? f cos-term))))
                                        factors)])
                (if (null? other-factors)
                    (make-app 'sin (product (num 2) (app-arg sin-term)))
                    (make-product-from-list
                     (cons (make-app 'sin (product (num 2) (app-arg sin-term)))
                           other-factors))))
           (make-product-from-list factors))))

;;; remove-first : α × (List α) → (List α)
(define (remove-first elem lst)
  (cond
   [(null? lst) '()]
   [(equal? (car lst) elem) (cdr lst)]
   [else (cons (car lst) (remove-first elem (cdr lst)))]))

;;; ====
;;; Canonical Form
;;; ====

;;; to-canonical : Expr → Expr
;;; Convert expression to canonical form:
;;;   - Sums sorted by term complexity
;;;   - Products sorted by factor complexity
;;;   - Numeric constants first
(define (to-canonical expr)
  (cond
   [(num? expr) expr]
   [(var? expr) expr]
   
   [(sum? expr)
    (let ([terms (map to-canonical (sum-terms expr))])
         (make-sum-from-list (sort-by-complexity terms)))]
   
   [(product? expr)
    (let ([factors (map to-canonical (product-factors expr))])
         (make-product-from-list (sort-by-complexity factors)))]
   
   [(difference? expr)
    (if (diff-right expr)
        (difference (to-canonical (diff-left expr))
                    (to-canonical (diff-right expr)))
        (make-neg (to-canonical (diff-left expr))))]
   
   [(quotient? expr)
    (quotient (to-canonical (quot-numer expr))
              (to-canonical (quot-denom expr)))]
   
   [(power? expr)
    (power (to-canonical (pow-base expr))
           (to-canonical (pow-exp expr)))]
   
   [(app? expr)
    (make-app (app-fn expr) (to-canonical (app-arg expr)))]
   
   [else expr]))

;;; sort-by-complexity : (List Expr) → (List Expr)
;;; Sort terms/factors by complexity (simplest first).
(define (sort-by-complexity exprs)
  (sort (lambda (a b) (< (expr-complexity a) (expr-complexity b))) exprs))

;;; expr-complexity : Expr → Number
;;; Measure the complexity of an expression.
(define (expr-complexity expr)
  (cond
   [(num? expr) 0]
   [(var? expr) 1]
   [(sum? expr) (+ 2 (apply + (map expr-complexity (sum-terms expr))))]
   [(product? expr) (+ 2 (apply + (map expr-complexity (product-factors expr))))]
   [(difference? expr)
    (+ 2 (expr-complexity (diff-left expr))
       (if (diff-right expr) (expr-complexity (diff-right expr)) 0))]
   [(quotient? expr) (+ 3 (expr-complexity (quot-numer expr))
                        (expr-complexity (quot-denom expr)))]
   [(power? expr) (+ 4 (expr-complexity (pow-base expr))
                     (expr-complexity (pow-exp expr)))]
   [(app? expr) (+ 5 (expr-complexity (app-arg expr)))]
   [else 10]))

;;; ====
;;; Full Simplification Pipeline
;;; ====

;;; full-simplify : Expr → Expr
;;; Apply comprehensive simplification.
(define (full-simplify expr)
  (-> expr
      simplify
      simplify-trig
      to-canonical
      simplify))

;;; -> : Expr × (Expr → Expr) ... → Expr
;;; Threading macro for function composition.
(define-syntax ->
  (syntax-rules ()
                [(-> x) x]
                [(-> x f) (f x)]
                [(-> x f g ...) (-> (f x) g ...)]))
