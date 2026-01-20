(load "core/base/prelude.ss")

(doc 'module 'poly-canon)
(doc 'description "Polynomial canonicalization for normalization. Lifts arithmetic to polynomial representation.")
(doc 'layer 'core)

(doc 'section 'configuration)

(doc *poly-canon-max-depth* 'type Nat)
(doc *poly-canon-max-depth* 'description "Maximum expression depth for polynomial canonicalization.")
(define *poly-canon-max-depth* 10)

(doc *poly-canon-max-terms* 'type Nat)
(doc *poly-canon-max-terms* 'description "Maximum number of terms after expansion.")
(define *poly-canon-max-terms* 100)

(doc 'section 'arithmetic-detection)

(define (exact-number? x)
  (doc 'type (-> Any Boolean))
  (doc 'description "True for integers and exact rationals, false for floats.")
  (doc 'export #t)
  (and (number? x)
       (or (integer? x)
           (and (rational? x) (exact? x)))))

(define (arithmetic-expr? expr)
  (doc 'type (-> Any Boolean))
  (doc 'description "Check if expression is arithmetic with depth limit.")
  (doc 'export #t)
  (arithmetic-expr-depth? expr *poly-canon-max-depth*))

(define (arithmetic-expr-depth? expr depth)
  (doc 'type (-> Any Nat Boolean))
  (doc 'description "Check if expression is arithmetic up to given depth.")
  (doc 'export #t)
  (cond
    [(<= depth 0) #f]
    [(exact-number? expr) #t]
    [(symbol? expr) #t]
    [(not (pair? expr)) #f]
    [(eq? (car expr) 'quote) #f]
    [(eq? (car expr) 'dv) #t]
    [(memq (car expr) '(+ * -))
     (and (not (null? (cdr expr)))
          (andmap (lambda (a) (arithmetic-expr-depth? a (- depth 1)))
                  (cdr expr)))]
    [else #f]))

(doc 'section 'polynomial-representation)

(doc term-coeff 'type (-> Term Number))
(doc term-coeff 'description "Extract coefficient from term.")
(doc term-coeff 'export #t)
(define (term-coeff term) (car term))

(doc term-mono 'type (-> Term Monomial))
(doc term-mono 'description "Extract monomial from term.")
(doc term-mono 'export #t)
(define (term-mono term) (cadr term))

(doc make-term 'type (-> Number Monomial Term))
(doc make-term 'description "Construct term from coefficient and monomial.")
(doc make-term 'export #t)
(define (make-term coeff mono) (list coeff mono))

(doc 'section 'conversion)

(define (sexpr->poly expr)
  (doc 'type (-> Any Poly))
  (doc 'description "Convert S-expression to polynomial representation.")
  (doc 'export #t)
  (cond
    [(exact-number? expr)
     (if (= expr 0)
         '()
         (list (make-term expr '())))]

    [(symbol? expr)
     (list (make-term 1 (list (cons expr 1))))]

    [(and (pair? expr) (eq? (car expr) 'dv))
     (let ([dv-sym (string->symbol (string-append "#dv" (number->string (cadr expr))))])
       (list (make-term 1 (list (cons dv-sym 1)))))]

    [(and (pair? expr) (eq? (car expr) '+))
     (poly-add-all (map sexpr->poly (cdr expr)))]

    [(and (pair? expr) (eq? (car expr) '*))
     (poly-mul-all (map sexpr->poly (cdr expr)))]

    [(and (pair? expr) (eq? (car expr) '-))
     (if (= (length (cdr expr)) 1)
         (poly-negate (sexpr->poly (cadr expr)))
         (poly-sub (sexpr->poly (cadr expr))
                   (poly-add-all (map sexpr->poly (cddr expr)))))]

    [else (error 'sexpr->poly "not an arithmetic expression" expr)]))

(doc 'section 'polynomial-operations)

(define (poly-add-all polys)
  (doc 'type (-> (List Poly) Poly))
  (doc 'description "Add all polynomials in list.")
  (doc 'export #t)
  (fold-left poly-add '() polys))

(define (poly-add p1 p2)
  (doc 'type (-> Poly Poly Poly))
  (doc 'description "Add two polynomials.")
  (doc 'export #t)
  (poly-simplify (append p1 p2)))

(define (poly-sub p1 p2)
  (doc 'type (-> Poly Poly Poly))
  (doc 'description "Subtract two polynomials.")
  (doc 'export #t)
  (poly-add p1 (poly-negate p2)))

(define (poly-negate p)
  (doc 'type (-> Poly Poly))
  (doc 'description "Negate a polynomial.")
  (doc 'export #t)
  (map (lambda (term)
         (make-term (- (term-coeff term)) (term-mono term)))
       p))

(define (poly-mul-all polys)
  (doc 'type (-> (List Poly) Poly))
  (doc 'description "Multiply all polynomials in list.")
  (doc 'export #t)
  (if (null? polys)
      (list (make-term 1 '()))
      (fold-left poly-mul (car polys) (cdr polys))))

(define (poly-mul p1 p2)
  (doc 'type (-> Poly Poly Poly))
  (doc 'description "Multiply two polynomials.")
  (doc 'export #t)
  (poly-simplify
   (apply append
          (map (lambda (t1)
                 (map (lambda (t2)
                        (term-mul t1 t2))
                      p2))
               p1))))

(define (term-mul t1 t2)
  (doc 'type (-> Term Term Term))
  (doc 'description "Multiply two terms.")
  (doc 'export #t)
  (make-term (* (term-coeff t1) (term-coeff t2))
             (mono-mul (term-mono t1) (term-mono t2))))

(doc 'section 'monomial-operations)

(define (mono-mul m1 m2)
  (doc 'type (-> Monomial Monomial Monomial))
  (doc 'description "Multiply monomials (add exponents of like variables).")
  (doc 'export #t)
  (mono-merge m1 m2))

(define (mono-merge m1 m2)
  (doc 'type (-> Monomial Monomial Monomial))
  (doc 'description "Merge two monomials.")
  (doc 'export #t)
  (cond
    [(null? m1) m2]
    [(null? m2) m1]
    [else
     (let ([v1 (caar m1)] [e1 (cdar m1)]
           [v2 (caar m2)] [e2 (cdar m2)])
       (cond
         [(string<? (symbol->string v1) (symbol->string v2))
          (cons (car m1) (mono-merge (cdr m1) m2))]
         [(string<? (symbol->string v2) (symbol->string v1))
          (cons (car m2) (mono-merge m1 (cdr m2)))]
         [else
          (cons (cons v1 (+ e1 e2)) (mono-merge (cdr m1) (cdr m2)))]))]))

(doc 'section 'simplification)

(define (poly-simplify terms)
  (doc 'type (-> Poly Poly))
  (doc 'description "Sort terms, combine like monomials, remove zeros.")
  (doc 'export #t)
  (let* ([sorted (list-sort term<? terms)]
         [combined (combine-terms sorted)]
         [nonzero (filter (lambda (t) (not (= (term-coeff t) 0))) combined)])
    nonzero))

(define (term<? t1 t2)
  (doc 'type (-> Term Term Boolean))
  (doc 'description "Term comparison for sorting.")
  (doc 'export #t)
  (mono<? (term-mono t1) (term-mono t2)))

(define (mono<? m1 m2)
  (doc 'type (-> Monomial Monomial Boolean))
  (doc 'description "Monomial comparison for sorting.")
  (doc 'export #t)
  (cond
    [(and (null? m1) (null? m2)) #f]
    [(null? m1) #t]
    [(null? m2) #f]
    [else
     (let ([v1 (caar m1)] [e1 (cdar m1)]
           [v2 (caar m2)] [e2 (cdar m2)])
       (cond
         [(string<? (symbol->string v1) (symbol->string v2)) #t]
         [(string<? (symbol->string v2) (symbol->string v1)) #f]
         [(< e1 e2) #t]
         [(> e1 e2) #f]
         [else (mono<? (cdr m1) (cdr m2))]))]))

(define (combine-terms terms)
  (doc 'type (-> Poly Poly))
  (doc 'description "Combine adjacent terms with same monomial.")
  (doc 'export #t)
  (if (null? terms)
      '()
      (let loop ([acc (list (car terms))] [rest (cdr terms)])
        (cond
          [(null? rest) (reverse acc)]
          [(equal? (term-mono (car acc)) (term-mono (car rest)))
           (let ([new-coeff (+ (term-coeff (car acc)) (term-coeff (car rest)))])
             (loop (cons (make-term new-coeff (term-mono (car acc))) (cdr acc))
                   (cdr rest)))]
          [else
           (loop (cons (car rest) acc) (cdr rest))]))))

(doc 'section 'poly-to-sexpr)

(define (poly->sexpr p)
  (doc 'type (-> Poly Any))
  (doc 'description "Convert polynomial to S-expression.")
  (doc 'export #t)
  (cond
    [(null? p) 0]
    [(null? (cdr p)) (term->sexpr (car p))]
    [else (cons '+ (map term->sexpr p))]))

(define (term->sexpr term)
  (doc 'type (-> Term Any))
  (doc 'description "Convert term to S-expression.")
  (doc 'export #t)
  (let ([coeff (term-coeff term)]
        [mono (term-mono term)])
    (cond
      [(null? mono) coeff]
      [(= coeff 1) (mono->sexpr mono)]
      [(= coeff -1)
       (let ([m (mono->sexpr mono)])
         (if (and (pair? m) (eq? (car m) '*))
             `(* -1 ,@(cdr m))
             `(* -1 ,m)))]
      [else
       (let ([m (mono->sexpr mono)])
         (if (and (pair? m) (eq? (car m) '*))
             `(* ,coeff ,@(cdr m))
             `(* ,coeff ,m)))])))

(define (mono->sexpr mono)
  (doc 'type (-> Monomial Any))
  (doc 'description "Convert monomial to S-expression.")
  (doc 'export #t)
  (cond
    [(null? mono) 1]
    [(null? (cdr mono)) (var->sexpr (caar mono) (cdar mono))]
    [else (cons '* (map (lambda (ve) (var->sexpr (car ve) (cdr ve))) mono))]))

(define (var->sexpr v e)
  (doc 'type (-> Symbol Nat Any))
  (doc 'description "Convert variable with exponent to S-expression.")
  (doc 'export #t)
  (let ([vstr (symbol->string v)])
    (cond
      [(and (> (string-length vstr) 3)
            (string=? "#dv" (substring vstr 0 3)))
       (let ([idx (string->number (substring vstr 3 (string-length vstr)))])
         (if (= e 1)
             `(dv ,idx)
             (cons '* (make-list e `(dv ,idx)))))]
      [(= e 1) v]
      [else (cons '* (make-list e v))])))

(define (make-list n x)
  (doc 'type (-> Nat Any (List Any)))
  (doc 'description "Create list of n copies of x.")
  (doc 'export #t)
  (if (<= n 0) '() (cons x (make-list (- n 1) x))))

(doc 'section 'entry-point)

(define (poly-canonicalize expr)
  (doc 'type (-> Any Any))
  (doc 'description "Canonicalize arithmetic expression to polynomial form.")
  (doc 'export #t)
  (if (arithmetic-expr? expr)
      (let ([p (sexpr->poly expr)])
        (if (> (length p) *poly-canon-max-terms*)
            expr
            (poly->sexpr p)))
      expr))

(define (try-poly-canonicalize expr)
  (doc 'type (-> Any Any))
  (doc 'description "Try polynomial canonicalization, return original on error.")
  (doc 'export #t)
  (guard (ex [else expr])
    (poly-canonicalize expr)))
