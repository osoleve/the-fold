(load "core/base/prelude.ss")

(doc 'module 'canonical-order)
(doc 'description "Canonical ordering for expression sorting. Total, stable, deterministic order over S-expressions.")
(doc 'layer 'core)

(doc 'section 'expression-classes)

(define (expr-order-class expr)
  (doc 'type (-> Any Nat))
  (doc 'description "Returns the ordering class of an expression (0-8).")
  (doc 'export #t)
  (cond
    [(number? expr)  0]
    [(boolean? expr) 1]
    [(char? expr)    2]
    [(string? expr)  3]
    [(symbol? expr)  4]
    [(and (pair? expr) (eq? (car expr) 'dv)
          (pair? (cdr expr)) (number? (cadr expr)))
     5]  ; de Bruijn variable
    [(and (pair? expr) (eq? (car expr) 'quote))
     6]  ; quoted data
    [(pair? expr)    7]  ; compound expression
    [else            8]))

(doc 'section 'comparison-functions)

(define (canonical<? a b)
  (doc 'type (-> Any Any Boolean))
  (doc 'description "Returns #t if a should come before b in canonical order. Strict total order.")
  (doc 'export #t)
  (let ([ca (expr-order-class a)]
        [cb (expr-order-class b)])
    (cond
      [(< ca cb) #t]
      [(> ca cb) #f]
      ;; Same class: use type-specific comparison
      [(= ca 0) (number<? a b)]
      [(= ca 1) (boolean<? a b)]
      [(= ca 2) (char<? a b)]
      [(= ca 3) (string<? a b)]
      [(= ca 4) (symbol<? a b)]
      [(= ca 5) (dv<? a b)]
      [(= ca 6) (quoted<? a b)]
      [(= ca 7) (compound<? a b)]
      [else #f])))  ; class 8: equal by default

(define (canonical=? a b)
  (doc 'type (-> Any Any Boolean))
  (doc 'description "Returns #t if a and b are equal in canonical ordering.")
  (doc 'export #t)
  (and (not (canonical<? a b))
       (not (canonical<? b a))))

(define (canonical<=? a b)
  (doc 'type (-> Any Any Boolean))
  (doc 'description "Less than or equal in canonical order.")
  (doc 'export #t)
  (or (canonical<? a b) (canonical=? a b)))

(doc 'section 'type-specific-comparisons)

(define (number<? a b)
  (doc 'type (-> Number Number Boolean))
  (doc 'description "Numeric comparison. Exact numbers come before inexact of same value.")
  (doc 'export #t)
  (cond
    [(< a b) #t]
    [(> a b) #f]
    ;; Equal numeric value: prefer exact over inexact
    [(and (exact? a) (not (exact? b))) #t]
    [(and (not (exact? a)) (exact? b)) #f]
    [else #f]))

(define (boolean<? a b)
  (doc 'type (-> Boolean Boolean Boolean))
  (doc 'description "#f < #t (false before true)")
  (doc 'export #t)
  (and (not a) b))

(define (symbol<? a b)
  (doc 'type (-> Symbol Symbol Boolean))
  (doc 'description "Alphabetic comparison by symbol name.")
  (doc 'export #t)
  (string<? (symbol->string a) (symbol->string b)))

(define (dv<? a b)
  (doc 'type (-> Any Any Boolean))
  (doc 'description "De Bruijn variable comparison by index.")
  (doc 'export #t)
  (< (cadr a) (cadr b)))

(define (quoted<? a b)
  (doc 'type (-> Any Any Boolean))
  (doc 'description "Quoted data comparison by comparing quoted values.")
  (doc 'export #t)
  (datum<? (cadr a) (cadr b)))

(define (datum<? a b)
  (doc 'type (-> Any Any Boolean))
  (doc 'description "Comparison for arbitrary quoted data (not expressions).")
  (doc 'export #t)
  (cond
    [(and (null? a) (null? b)) #f]
    [(null? a) #t]
    [(null? b) #f]
    [(and (pair? a) (pair? b))
     (cond
       [(datum<? (car a) (car b)) #t]
       [(datum<? (car b) (car a)) #f]
       [else (datum<? (cdr a) (cdr b))])]
    [(pair? a) #f]  ; non-pair < pair
    [(pair? b) #t]
    ;; Atoms: use canonical<? for uniform handling
    [else (canonical<? a b)]))

(define (compound<? a b)
  (doc 'type (-> Pair Pair Boolean))
  (doc 'description "Lexicographic comparison of compound expressions.")
  (doc 'export #t)
  (cond
    [(and (null? a) (null? b)) #f]
    [(null? a) #t]
    [(null? b) #f]
    [(canonical<? (car a) (car b)) #t]
    [(canonical<? (car b) (car a)) #f]
    [else (compound<? (cdr a) (cdr b))]))

(doc 'section 'sorting-support)

(define (canonical-sort exprs)
  (doc 'type (-> (List Any) (List Any)))
  (doc 'description "Sort a list of expressions in canonical order.")
  (doc 'export #t)
  (list-sort canonical<? exprs))

(define (canonical-merge as bs)
  (doc 'type (-> (List Any) (List Any) (List Any)))
  (doc 'description "Merge two sorted lists maintaining canonical order.")
  (doc 'export #t)
  (cond
    [(null? as) bs]
    [(null? bs) as]
    [(canonical<? (car as) (car bs))
     (cons (car as) (canonical-merge (cdr as) bs))]
    [else
     (cons (car bs) (canonical-merge as (cdr bs)))]))
