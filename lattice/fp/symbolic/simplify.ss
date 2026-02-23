;;; @module simplify
;;; @requires sort expr hamt

(require 'sort)
(require 'expr)
(require 'hamt)

(doc 'module 'simplify)
(doc 'description "Comprehensive algebraic simplification for symbolic expressions")
(doc 'features "Collect like terms, expand products, factor expressions, trigonometric identities, logarithm/exponential rules, canonical form conversion, simplification heuristics")
(doc 'layer 'lattice)
(doc 'purity 'partial)

(doc 'section 'helper-functions)

;;; filter-map provided by prelude

(define (get-numeric e)
  (doc 'type '(-> Expr (Maybe Number)))
  (doc 'description "Extract numeric value from expression, returns #f if not numeric")
  (if (num? e) (num-val e) #f))

(doc 'section 'main-simplification)

(define (simplify expr)
  (doc 'type '(-> Expr Expr))
  (doc 'description "Simplify expression using algebraic rules, applying repeatedly until fixed point")
  (let ([simplified (simplify-once expr)])
       (if (expr=? simplified expr)
           simplified
           (simplify simplified))))

(define (simplify-once expr)
  (doc 'type '(-> Expr Expr))
  (doc 'description "Single pass of simplification")
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

(doc 'section 'sum-simplification)

(define (simplify-sum terms)
  (doc 'type '(-> (List Expr) Expr))
  (doc 'description "Simplify sum by collecting numeric terms and like terms")
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

(define (flatten-sums terms)
  (doc 'type '(-> (List Expr) (List Expr)))
  (doc 'description "Flatten nested sums: (+ a (+ b c)) → (a b c)")
  (append-map (lambda (e)
                      (if (sum? e)
                          (flatten-sums (sum-terms e))
                          (list e)))
              terms))

(define (collect-like-terms terms)
  (doc 'type '(-> (List Expr) (List Expr)))
  (doc 'description "Collect like terms: x + x → 2*x")
  (let ([table (fold-left
                (lambda (acc term)
                  (let-values ([(coef base) (extract-coefficient term)])
                    (let ([current (hamt-lookup-or base acc 0)])
                      (hamt-assoc base (+ current coef) acc))))
                hamt-empty
                terms)])
       ;; Rebuild terms from HAMT keys in deterministic order
       (let* ([keys (hamt-keys table)]
              [sorted-keys (sort-by (lambda (a b) (< (expr-complexity a) (expr-complexity b))) keys)])
             (filter-map
              (lambda (key)
                      (let ([coef (hamt-lookup-or key table 0)])
                           (cond
                            [(= coef 0) #f]
                            [(= coef 1) key]
                            [else (product (num coef) key)])))
              sorted-keys))))

(define (extract-coefficient term)
  (doc 'type '(-> Expr (Values Number Expr)))
  (doc 'description "Extract numeric coefficient from term")
  (doc 'note "x → (1, x), 2*x → (2, x), -x → (-1, x)")
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

(define (make-sum-from-list terms)
  (doc 'type '(-> (List Expr) Expr))
  (doc 'description "Construct sum from list of terms")
  (cond
   [(null? terms) (num 0)]
   [(= (length terms) 1) (car terms)]
   [else (fold-left sum (car terms) (cdr terms))]))

(doc 'section 'product-simplification)

(define (simplify-product factors)
  (doc 'type '(-> (List Expr) Expr))
  (doc 'description "Simplify product by collecting numeric factors and combining powers")
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

(define (flatten-products factors)
  (doc 'type '(-> (List Expr) (List Expr)))
  (doc 'description "Flatten nested products")
  (append-map (lambda (e)
                      (if (product? e)
                          (flatten-products (product-factors e))
                          (list e)))
              factors))

(define (collect-like-bases factors)
  (doc 'type '(-> (List Expr) (List Expr)))
  (doc 'description "Collect terms with same base: x * x → x^2")
  (let ([table (fold-left
                (lambda (acc factor)
                  (let-values ([(base exp) (extract-base-exponent factor)])
                    (let ([current (hamt-lookup-or base acc (num 0))])
                      (hamt-assoc base (sum current exp) acc))))
                hamt-empty
                factors)])
       ;; Rebuild factors in deterministic order (sorted by complexity)
       (let* ([keys (hamt-keys table)]
              [sorted-keys (sort-by (lambda (a b) (< (expr-complexity a) (expr-complexity b))) keys)])
             (filter-map
              (lambda (key)
                      (let ([exp (simplify (hamt-lookup-or key table (num 0)))])
                           (cond
                            [(and (num? exp) (= (num-val exp) 0)) #f]
                            [(and (num? exp) (= (num-val exp) 1)) key]
                            [else (power key exp)])))
              sorted-keys))))

(define (extract-base-exponent expr)
  (doc 'type '(-> Expr (Values Expr Expr)))
  (doc 'description "Extract base and exponent: x^2 → (x, 2), x → (x, 1)")
  (if (power? expr)
      (values (pow-base expr) (pow-exp expr))
      (values expr (num 1))))

(define (make-product-from-list factors)
  (doc 'type '(-> (List Expr) Expr))
  (doc 'description "Construct product from list of factors")
  (cond
   [(null? factors) (num 1)]
   [(= (length factors) 1) (car factors)]
   [else (fold-left product (car factors) (cdr factors))]))

(doc 'section 'difference-and-negation)

(define (simplify-diff left right)
  (doc 'type '(-> Expr Expr Expr))
  (doc 'description "Simplify difference expression")
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

(define (simplify-neg e)
  (doc 'type '(-> Expr Expr))
  (doc 'description "Simplify negation expression")
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

(doc 'section 'quotient-simplification)

(define (simplify-quot numer denom)
  (doc 'type '(-> Expr Expr Expr))
  (doc 'description "Simplify quotient expression")
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
                           (division numer denom)))
                 (division numer denom))))]
   ;; (a*x)/(b*x) = a/b (cancel common factors)
   [(and (product? numer) (product? denom))
    (let ([simplified (cancel-common-factors
                       (product-factors numer)
                       (product-factors denom))])
         (if simplified
             (division (make-product-from-list (car simplified))
                       (make-product-from-list (cadr simplified)))
             (division numer denom)))]
   ;; x^a / x^b = x^(a-b)
   [(and (power? numer) (power? denom)
         (expr=? (pow-base numer) (pow-base denom)))
    (power (pow-base numer)
           (simplify (difference (pow-exp numer) (pow-exp denom))))]
   [else (division numer denom)]))

(define (cancel-common-factors numer-factors denom-factors)
  (doc 'type '(-> (List Expr) (List Expr) (Maybe (Pair (List Expr) (List Expr)))))
  (doc 'description "Try to cancel common factors between numerator and denominator")
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

(define (find-and-remove expr lst)
  (doc 'type '(-> Expr (List Expr) (Maybe (List Expr))))
  (doc 'description "Find expr in list and return list with it removed")
  (let loop ([rest lst] [acc '()])
       (cond
        [(null? rest) #f]
        [(expr=? (car rest) expr)
         (append (reverse acc) (cdr rest))]
        [else (loop (cdr rest) (cons (car rest) acc))])))

(doc 'section 'power-simplification)

(define (simplify-pow base exp)
  (doc 'type '(-> Expr Expr Expr))
  (doc 'description "Simplify power expression")
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

(doc 'section 'function-application-simplification)

(define (simplify-app fn arg)
  (doc 'type '(-> Symbol Expr Expr))
  (doc 'description "Simplify function application with known identities")
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

(doc 'section 'product-expansion)

(define (expand expr)
  (doc 'type '(-> Expr Expr))
  (doc 'description "Fully expand products and powers")
  (doc 'note "(a + b) * (c + d) → a*c + a*d + b*c + b*d")
  (let ([expanded (expand-once expr)])
       (if (expr=? expanded expr)
           expanded
           (expand expanded))))

(define (expand-once expr)
  (doc 'type '(-> Expr Expr))
  (doc 'description "Single pass of expansion")
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
    (division (expand-once (quot-numer expr))
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

(define (expand-product factors)
  (doc 'type '(-> (List Expr) Expr))
  (doc 'description "Expand product by distributing over sums")
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

(define (expand-power-of-sum base n)
  (doc 'type '(-> Expr Integer Expr))
  (doc 'description "Expand (a+b+...)^n using repeated multiplication")
  (if (<= n 1)
      base
      (let loop ([acc base] [i 1])
           (if (>= i n)
               acc
               (loop (expand-once (product acc base)) (+ i 1))))))

(doc 'section 'factoring)

(define (factor expr)
  (doc 'type '(-> Expr Expr))
  (doc 'description "Try to factor expression")
  (doc 'note "Supports: extract common factors, difference of squares, perfect square trinomials")
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

(define (factor-sum terms)
  (doc 'type '(-> (List Expr) (Maybe Expr)))
  (doc 'description "Try to factor sum by extracting common factors")
  (let ([common (find-common-factor terms)])
       (if (and common (not (and (num? common) (= (num-val common) 1))))
           (let ([remaining (map (lambda (t) (divide-out t common)) terms)])
                (product common (make-sum-from-list remaining)))
           #f)))

(define (find-common-factor terms)
  (doc 'type '(-> (List Expr) (Maybe Expr)))
  (doc 'description "Find factor common to all terms")
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

(define (get-factors expr)
  (doc 'type '(-> Expr (List Expr)))
  (doc 'description "Get factors of expression")
  (cond
   [(product? expr) (product-factors expr)]
   [(num? expr) (list expr)]
   [else (list expr)]))

(define (divides? factor term)
  (doc 'type '(-> Expr Expr Bool))
  (doc 'description "Check if factor divides term (loosely)")
  (cond
   [(expr=? factor term) #t]
   [(product? term)
    (exists (lambda (f) (expr=? factor f)) (product-factors term))]
   [(and (num? factor) (num? term))
    (integer? (/ (num-val term) (num-val factor)))]
   [else #f]))

(define (divide-out term factor)
  (doc 'type '(-> Expr Expr Expr))
  (doc 'description "Divide factor out of term")
  (cond
   [(expr=? factor term) (num 1)]
   [(product? term)
    (let ([remaining (find-and-remove factor (product-factors term))])
         (if remaining
             (make-product-from-list remaining)
             (division term factor)))]
   [(and (num? factor) (num? term))
    (num (/ (num-val term) (num-val factor)))]
   [else (division term factor)]))

(doc 'section 'trigonometric-identities)

(define (simplify-trig expr)
  (doc 'type '(-> Expr Expr))
  (doc 'description "Apply trigonometric identities")
  (let ([simplified (simplify-trig-once expr)])
       (if (expr=? simplified expr)
           simplified
           (simplify-trig simplified))))

(define (simplify-trig-once expr)
  (doc 'type '(-> Expr Expr))
  (doc 'description "Single pass of trigonometric simplification")
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
             (division numer denom)))]
   
   [(app? expr)
    (make-app (app-fn expr) (simplify-trig-once (app-arg expr)))]
   
   [else expr]))

(define (simplify-pythagorean-identity terms)
  (doc 'type '(-> (List Expr) Expr))
  (doc 'description "Apply Pythagorean identity: sin^2(x) + cos^2(x) = 1")
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

(define (find-sin-squared expr)
  (doc 'type '(-> Expr (Maybe Expr)))
  (doc 'description "Check if expression is sin^2(x), return x if so")
  (if (and (power? expr)
           (num? (pow-exp expr))
           (= (num-val (pow-exp expr)) 2)
           (app? (pow-base expr))
           (eq? (app-fn (pow-base expr)) 'sin))
      (app-arg (pow-base expr))
      #f))

(define (find-matching-cos-squared sin-sq-term terms)
  (doc 'type '(-> Expr (List Expr) (Maybe Expr)))
  (doc 'description "Find cos^2(x) in terms matching sin^2(x)")
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

(define (simplify-double-angle-product factors)
  (doc 'type '(-> (List Expr) Expr))
  (doc 'description "Apply double angle identity: 2*sin(x)*cos(x) = sin(2x)")
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

(define (remove-first elem lst)
  (doc 'type '(-> α (List α) (List α)))
  (doc 'description "Remove first occurrence of element from list")
  (cond
   [(null? lst) '()]
   [(equal? (car lst) elem) (cdr lst)]
   [else (cons (car lst) (remove-first elem (cdr lst)))]))

(doc 'section 'canonical-form)

(define (to-canonical expr)
  (doc 'type '(-> Expr Expr))
  (doc 'description "Convert to canonical form: sorted by complexity, numeric constants first")
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
    (division (to-canonical (quot-numer expr))
              (to-canonical (quot-denom expr)))]
   
   [(power? expr)
    (power (to-canonical (pow-base expr))
           (to-canonical (pow-exp expr)))]
   
   [(app? expr)
    (make-app (app-fn expr) (to-canonical (app-arg expr)))]
   
   [else expr]))

(define (sort-by-complexity exprs)
  (doc 'type '(-> (List Expr) (List Expr)))
  (doc 'description "Sort terms/factors by complexity (simplest first)")
  (sort-by (lambda (a b) (< (expr-complexity a) (expr-complexity b))) exprs))

(define (expr-complexity expr)
  (doc 'type '(-> Expr Number))
  (doc 'description "Measure complexity of expression")
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

(doc 'section 'full-pipeline)

(define (full-simplify expr)
  (doc 'type '(-> Expr Expr))
  (doc 'description "Apply comprehensive simplification pipeline")
  (-> expr
      simplify
      simplify-trig
      to-canonical
      simplify))

(doc -> 'type '(-> Expr (-> Expr Expr) ... Expr))
(doc -> 'description "Threading macro for function composition")
(define-syntax ->
  (syntax-rules ()
                [(-> x) x]
                [(-> x f) (f x)]
                [(-> x f g ...) (-> (f x) g ...)]))
