(load "core/base/prelude.ss")
(load "lattice/algebra/field.ss")

(doc 'module 'multivariate)
(doc 'description "Multivariate polynomial algebra")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'note "Pure, functional implementation of multivariate polynomials:")
(doc 'note "- Sparse representation with monomial orderings")
(doc 'note "- Lexicographic, graded lex, graded reverse lex orderings")
(doc 'note "- Polynomial arithmetic over arbitrary coefficient fields")
(doc 'note "- Multivariate division algorithm")

(doc 'note "NOTE: Division operations require a Field (not just a Ring) because")
(doc 'note "multivariate polynomial division requires coefficient division.")

(doc 'section 'monomial-representation)

(doc 'note "A monomial is represented as an association list of (variable . exponent)")
(doc 'note "pairs, sorted by variable name. Only non-zero exponents are stored.")
(doc 'note "")
(doc 'note "Example: x^2 * y * z^3 is ((x . 2) (y . 1) (z . 3))")
(doc 'note "The constant monomial 1 is ()")
(define (make-monomial pairs)
  (doc 'export #t)
  (let ([filtered (filter (lambda (p) (> (cdr p) 0)) pairs)])
    (sort-by-var filtered)))

;;; sort-by-var : (List (Symbol × Nat)) → (List (Symbol × Nat))
(define (sort-by-var pairs)
  (list-sort (lambda (a b) (symbol<? (car a) (car b))) pairs))

;;; symbol<? : Symbol × Symbol → Boolean
(define (symbol<? a b)
  (string<? (symbol->string a) (symbol->string b)))

;;; mono-one : → Monomial
;;; The constant monomial 1.
(define (mono-one) '())
(doc 'export #t)

;;; mono-var : Symbol → Monomial
;;; Single variable: x^1
(define (mono-var v)
  (doc 'export #t)
  (list (cons v 1)))

;;; mono-power : Symbol × Nat → Monomial
;;; Single variable power: x^n
(define (mono-power v n)
  (doc 'export #t)
  (if (= n 0)
      '()
      (list (cons v n))))

;;; mono-degree : Monomial → Nat
;;; Total degree of monomial (sum of exponents).
(define (mono-degree m)
  (doc 'export #t)
  (apply + (map cdr m)))

;;; mono-degree-in : Monomial × Symbol → Nat
;;; Degree in specific variable.
(define (mono-degree-in m v)
  (doc 'export #t)
  (let ([pair (assq v m)])
    (if pair (cdr pair) 0)))

;;; mono-vars : Monomial → (List Symbol)
;;; Variables appearing in monomial.
(define (mono-vars m)
  (doc 'export #t)
  (map car m))

;;; mono-mul : Monomial × Monomial → Monomial
;;; Multiply monomials (add exponents).
(define (mono-mul m1 m2)
  (doc 'export #t)
  (mono-merge m1 m2))

;;; mono-merge : merge two sorted alists, adding exponents
(define (mono-merge m1 m2)
  (cond
    [(null? m1) m2]
    [(null? m2) m1]
    [else
     (let ([v1 (caar m1)] [e1 (cdar m1)]
           [v2 (caar m2)] [e2 (cdar m2)])
       (cond
         [(symbol<? v1 v2)
          (cons (car m1) (mono-merge (cdr m1) m2))]
         [(symbol<? v2 v1)
          (cons (car m2) (mono-merge m1 (cdr m2)))]
         [else  ; Same variable
          (let ([sum (+ e1 e2)])
            (if (= sum 0)
                (mono-merge (cdr m1) (cdr m2))
                (cons (cons v1 sum) (mono-merge (cdr m1) (cdr m2)))))]))]))

;;; mono-divides? : Monomial × Monomial → Boolean
;;; Does m1 divide m2? (all exponents of m1 ≤ corresponding in m2)
(define (mono-divides? m1 m2)
  (doc 'export #t)
  (let loop ([m1 m1] [m2 m2])
    (cond
      [(null? m1) #t]
      [(null? m2) #f]
      [else
       (let ([v1 (caar m1)] [e1 (cdar m1)]
             [v2 (caar m2)] [e2 (cdar m2)])
         (cond
           [(symbol<? v1 v2) #f]  ; v1 not in m2
           [(symbol<? v2 v1)      ; v2 not in m1, skip
            (loop m1 (cdr m2))]
           [else                  ; Same variable
            (and (<= e1 e2) (loop (cdr m1) (cdr m2)))]))])))

;;; mono-div : Monomial × Monomial → Monomial
;;; Divide m1 by m2 (subtract exponents). Assumes m2 divides m1.
(define (mono-div m1 m2)
  (doc 'export #t)
  (mono-div-helper m1 m2))

(define (mono-div-helper m1 m2)
  (cond
    [(null? m2) m1]
    [(null? m1) '()]  ; Should not happen if m2 divides m1
    [else
     (let ([v1 (caar m1)] [e1 (cdar m1)]
           [v2 (caar m2)] [e2 (cdar m2)])
       (cond
         [(symbol<? v1 v2)
          (cons (car m1) (mono-div-helper (cdr m1) m2))]
         [(symbol<? v2 v1)
          ;; v2 in m2 but not m1 - subtract from nothing (error case)
          (mono-div-helper m1 (cdr m2))]
         [else
          (let ([diff (- e1 e2)])
            (if (<= diff 0)
                (mono-div-helper (cdr m1) (cdr m2))
                (cons (cons v1 diff) (mono-div-helper (cdr m1) (cdr m2)))))]))]))

;;; mono-lcm : Monomial × Monomial → Monomial
;;; Least common multiple (max of each exponent).
(define (mono-lcm m1 m2)
  (doc 'export #t)
  (mono-lcm-helper m1 m2))

(define (mono-lcm-helper m1 m2)
  (cond
    [(null? m1) m2]
    [(null? m2) m1]
    [else
     (let ([v1 (caar m1)] [e1 (cdar m1)]
           [v2 (caar m2)] [e2 (cdar m2)])
       (cond
         [(symbol<? v1 v2)
          (cons (car m1) (mono-lcm-helper (cdr m1) m2))]
         [(symbol<? v2 v1)
          (cons (car m2) (mono-lcm-helper m1 (cdr m2)))]
         [else
          (cons (cons v1 (max e1 e2))
                (mono-lcm-helper (cdr m1) (cdr m2)))]))]))

;;; mono-gcd : Monomial × Monomial → Monomial
;;; Greatest common divisor (min of each exponent).
(define (mono-gcd m1 m2)
  (doc 'export #t)
  (mono-gcd-helper m1 m2))

(define (mono-gcd-helper m1 m2)
  (cond
    [(null? m1) '()]
    [(null? m2) '()]
    [else
     (let ([v1 (caar m1)] [e1 (cdar m1)]
           [v2 (caar m2)] [e2 (cdar m2)])
       (cond
         [(symbol<? v1 v2)
          (mono-gcd-helper (cdr m1) m2)]
         [(symbol<? v2 v1)
          (mono-gcd-helper m1 (cdr m2))]
         [else
          (let ([m (min e1 e2)])
            (if (= m 0)
                (mono-gcd-helper (cdr m1) (cdr m2))
                (cons (cons v1 m)
                      (mono-gcd-helper (cdr m1) (cdr m2)))))]))]))

;;; mono-equal? : Monomial × Monomial → Boolean
(define (mono-equal? m1 m2)
  (doc 'export #t)
  (equal? m1 m2))

;;; ====
;;; Monomial Orderings
;;; ====

;;; A monomial ordering is a total order on monomials that is:
;;; 1. A well-ordering (every non-empty set has a minimum)
;;; 2. Compatible with multiplication: m1 < m2 implies m1*n < m2*n

;;; mono-compare-lex : Monomial × Monomial × (List Symbol) → Integer
;;; Lexicographic order with given variable ordering.
;;; Returns -1 if m1 < m2, 0 if m1 = m2, +1 if m1 > m2.
(define (mono-compare-lex m1 m2 vars)
  (doc 'export #t)
  (let loop ([vs vars])
    (if (null? vs)
        0
        (let ([v (car vs)])
          (let ([e1 (mono-degree-in m1 v)]
                [e2 (mono-degree-in m2 v)])
            (cond
              [(> e1 e2) 1]
              [(< e1 e2) -1]
              [else (loop (cdr vs))]))))))

;;; mono-compare-grlex : Monomial × Monomial × (List Symbol) → Integer
;;; Graded lexicographic order (degree first, then lex).
(define (mono-compare-grlex m1 m2 vars)
  (doc 'export #t)
  (let ([d1 (mono-degree m1)]
        [d2 (mono-degree m2)])
    (cond
      [(> d1 d2) 1]
      [(< d1 d2) -1]
      [else (mono-compare-lex m1 m2 vars)])))

;;; mono-compare-grevlex : Monomial × Monomial × (List Symbol) → Integer
;;; Graded reverse lexicographic order.
;;; Compare by degree first, then by REVERSE lex on NEGATIVE exponents.
(define (mono-compare-grevlex m1 m2 vars)
  (doc 'export #t)
  (let ([d1 (mono-degree m1)]
        [d2 (mono-degree m2)])
    (cond
      [(> d1 d2) 1]
      [(< d1 d2) -1]
      [else
       ;; Reverse lex: compare from last variable, SMALLER exponent wins
       (let loop ([vs (reverse vars)])
         (if (null? vs)
             0
             (let ([v (car vs)])
               (let ([e1 (mono-degree-in m1 v)]
                     [e2 (mono-degree-in m2 v)])
                 (cond
                   [(< e1 e2) 1]   ; Smaller exponent is LARGER in grevlex
                   [(> e1 e2) -1]
                   [else (loop (cdr vs))])))))])))

;;; make-ordering : Symbol × (List Symbol) → (Monomial × Monomial → Integer)
;;; Create a monomial comparison function.
(define (make-ordering type vars)
  (doc 'export #t)
  (case type
    [(lex) (lambda (m1 m2) (mono-compare-lex m1 m2 vars))]
    [(grlex deglex) (lambda (m1 m2) (mono-compare-grlex m1 m2 vars))]
    [(grevlex degrevlex) (lambda (m1 m2) (mono-compare-grevlex m1 m2 vars))]
    [else (error 'make-ordering "unknown ordering type" type)]))

;;; ====
;;; Multivariate Polynomial Representation
;;; ====

;;; A multivariate polynomial is represented as:
;;;   (mpoly F vars ordering terms)
;;; where:
;;;   F = coefficient field
;;;   vars = list of variable symbols (defines ordering context)
;;;   ordering = comparison function for monomials
;;;   terms = list of (coefficient . monomial) pairs, sorted by ordering (descending)

;;; make-mpoly : Field × (List Symbol) × (M×M→Int) × (List (Coeff × Monomial)) → MPoly
(define (make-mpoly F vars ordering terms)
  (doc 'export #t)
  (list 'mpoly F vars ordering (mpoly-normalize F ordering terms)))

;;; mpoly? : Any → Boolean
(define (mpoly? p)
  (doc 'export #t)
  (and (pair? p) (eq? (car p) 'mpoly)))

;;; mpoly-field : MPoly → Field
(define (mpoly-field p) (list-ref p 1))
(doc 'export #t)
;;; Backward compatibility alias
(doc mpoly-ring 'export #t)
(define mpoly-ring mpoly-field)

;;; mpoly-vars : MPoly → (List Symbol)
(define (mpoly-vars p) (list-ref p 2))
(doc 'export #t)

;;; mpoly-ordering : MPoly → (M×M→Int)
(define (mpoly-ordering p) (list-ref p 3))
(doc 'export #t)

;;; mpoly-terms : MPoly → (List (Coeff × Monomial))
(define (mpoly-terms p) (list-ref p 4))
(doc 'export #t)

;;; mpoly-normalize : Field × (M×M→Int) × (List (Coeff × Monomial)) → (List ...)
;;; Combine like terms, remove zeros, sort by ordering (descending).
(define (mpoly-normalize F ordering terms)
  (let* ([zero (field-zero F)]
         [eq-fn (field-equal-fn F)]
         [add (field-add-op F)]
         ;; Combine like terms
         [combined (mpoly-combine-terms terms add eq-fn zero)]
         ;; Remove zero coefficients
         [nonzero (filter (lambda (t) (not (eq-fn (car t) zero))) combined)]
         ;; Sort by monomial (descending = largest first)
         [sorted (list-sort
                  (lambda (t1 t2) (> (ordering (cdr t1) (cdr t2)) 0))
                  nonzero)])
    (if (null? sorted)
        (list (cons zero (mono-one)))
        sorted)))

;;; mpoly-combine-terms : combine terms with same monomial
(define (mpoly-combine-terms terms add eq-fn zero)
  (if (null? terms)
      '()
      (let loop ([ts terms] [acc '()])
        (if (null? ts)
            acc
            (let* ([term (car ts)]
                   [coeff (car term)]
                   [mono (cdr term)]
                   [existing (assoc-mono mono acc)])
              (if existing
                  (loop (cdr ts)
                        (update-assoc-mono mono (add (car existing) coeff) acc))
                  (loop (cdr ts)
                        (cons term acc))))))))

;;; assoc-mono : Monomial × (List (Coeff × Monomial)) → (Coeff × Monomial) | #f
(define (assoc-mono mono terms)
  (cond
    [(null? terms) #f]
    [(mono-equal? mono (cdar terms)) (car terms)]
    [else (assoc-mono mono (cdr terms))]))

;;; update-assoc-mono : Monomial × Coeff × (List ...) → (List ...)
(define (update-assoc-mono mono new-coeff terms)
  (map (lambda (t)
         (if (mono-equal? mono (cdr t))
             (cons new-coeff mono)
             t))
       terms))

;;; ====
;;; MPoly Construction
;;; ====

;;; mpoly-zero : Field × (List Symbol) × (M×M→Int) → MPoly
(define (mpoly-zero F vars ordering)
  (doc 'export #t)
  (make-mpoly F vars ordering (list (cons (field-zero F) (mono-one)))))

;;; mpoly-one : Field × (List Symbol) × (M×M→Int) → MPoly
(define (mpoly-one F vars ordering)
  (doc 'export #t)
  (make-mpoly F vars ordering (list (cons (field-one F) (mono-one)))))

;;; mpoly-constant : Field × (List Symbol) × (M×M→Int) × Coeff → MPoly
(define (mpoly-constant F vars ordering c)
  (doc 'export #t)
  (make-mpoly F vars ordering (list (cons c (mono-one)))))

;;; mpoly-var : Field × (List Symbol) × (M×M→Int) × Symbol → MPoly
;;; Create polynomial for single variable.
(define (mpoly-var F vars ordering v)
  (doc 'export #t)
  (make-mpoly F vars ordering (list (cons (field-one F) (mono-var v)))))

;;; mpoly-from-terms : Field × (List Symbol) × Symbol × (List (Coeff × Monomial)) → MPoly
;;; Convenience: create mpoly with specified ordering type.
(define (mpoly-from-terms F vars ordering-type terms)
  (doc 'export #t)
  (let ([ordering (make-ordering ordering-type vars)])
    (make-mpoly F vars ordering terms)))

;;; ====
;;; MPoly Properties
;;; ====

;;; mpoly-zero? : MPoly → Boolean
(define (mpoly-zero? p)
  (doc 'export #t)
  (let* ([F (mpoly-field p)]
         [terms (mpoly-terms p)]
         [eq-fn (field-equal-fn F)]
         [zero (field-zero F)])
    (and (= (length terms) 1)
         (null? (cdar terms))  ; Constant monomial
         (eq-fn (caar terms) zero))))

;;; mpoly-degree : MPoly → Nat
;;; Total degree (maximum degree of any term).
(define (mpoly-degree p)
  (doc 'export #t)
  (apply max (map (lambda (t) (mono-degree (cdr t))) (mpoly-terms p))))

;;; mpoly-degree-in : MPoly × Symbol → Nat
;;; Degree in specific variable.
(define (mpoly-degree-in p v)
  (doc 'export #t)
  (apply max (map (lambda (t) (mono-degree-in (cdr t) v)) (mpoly-terms p))))

;;; mpoly-leading-term : MPoly → (Coeff × Monomial)
;;; Leading term (largest monomial according to ordering).
(define (mpoly-leading-term p)
  (doc 'export #t)
  (car (mpoly-terms p)))

;;; mpoly-leading-coeff : MPoly → Coeff
(define (mpoly-leading-coeff p)
  (doc 'export #t)
  (car (mpoly-leading-term p)))

;;; mpoly-leading-mono : MPoly → Monomial
(define (mpoly-leading-mono p)
  (doc 'export #t)
  (cdr (mpoly-leading-term p)))

;;; ====
;;; MPoly Arithmetic
;;; ====

;;; mpoly-add : MPoly × MPoly → MPoly
(define (mpoly-add p1 p2)
  (doc 'export #t)
  (let ([F (mpoly-field p1)]
        [vars (mpoly-vars p1)]
        [ordering (mpoly-ordering p1)])
    (make-mpoly F vars ordering
                (append (mpoly-terms p1) (mpoly-terms p2)))))

;;; mpoly-neg : MPoly → MPoly
(define (mpoly-neg p)
  (doc 'export #t)
  (let* ([F (mpoly-field p)]
         [neg (field-neg-fn F)])
    (make-mpoly (mpoly-field p)
                (mpoly-vars p)
                (mpoly-ordering p)
                (map (lambda (t) (cons (neg (car t)) (cdr t)))
                     (mpoly-terms p)))))

;;; mpoly-sub : MPoly × MPoly → MPoly
(define (mpoly-sub p1 p2)
  (doc 'export #t)
  (mpoly-add p1 (mpoly-neg p2)))

;;; mpoly-scale : MPoly × Coeff → MPoly
(define (mpoly-scale p c)
  (doc 'export #t)
  (let ([F (mpoly-field p)]
        [mul (field-mul-op (mpoly-field p))])
    (make-mpoly (mpoly-field p)
                (mpoly-vars p)
                (mpoly-ordering p)
                (map (lambda (t) (cons (mul c (car t)) (cdr t)))
                     (mpoly-terms p)))))

;;; mpoly-mul-term : MPoly × (Coeff × Monomial) → MPoly
;;; Multiply polynomial by single term.
(define (mpoly-mul-term p term)
  (doc 'export #t)
  (let* ([F (mpoly-field p)]
         [mul (field-mul-op F)]
         [c (car term)]
         [m (cdr term)])
    (make-mpoly F
                (mpoly-vars p)
                (mpoly-ordering p)
                (map (lambda (t)
                       (cons (mul c (car t))
                             (mono-mul m (cdr t))))
                     (mpoly-terms p)))))

;;; mpoly-mul : MPoly × MPoly → MPoly
;;; Multiply polynomials (distribute).
(define (mpoly-mul p1 p2)
  (doc 'export #t)
  (let ([F (mpoly-field p1)]
        [vars (mpoly-vars p1)]
        [ordering (mpoly-ordering p1)])
    (let loop ([terms1 (mpoly-terms p1)]
               [acc (mpoly-zero F vars ordering)])
      (if (null? terms1)
          acc
          (loop (cdr terms1)
                (mpoly-add acc (mpoly-mul-term p2 (car terms1))))))))

;;; mpoly-power : MPoly × Nat → MPoly
(define (mpoly-power p n)
  (doc 'export #t)
  (cond
    [(= n 0) (mpoly-one (mpoly-field p) (mpoly-vars p) (mpoly-ordering p))]
    [(= n 1) p]
    [(even? n)
     (let ([half (mpoly-power p (/ n 2))])
       (mpoly-mul half half))]
    [else
     (mpoly-mul p (mpoly-power p (- n 1)))]))

;;; ====
;;; MPoly Equality
;;; ====

;;; mpoly-equal? : MPoly × MPoly → Boolean
(define (mpoly-equal? p1 p2)
  (doc 'export #t)
  (let* ([F (mpoly-field p1)]
         [eq-fn (field-equal-fn F)]
         [t1 (mpoly-terms p1)]
         [t2 (mpoly-terms p2)])
    (and (= (length t1) (length t2))
         (let loop ([l1 t1] [l2 t2])
           (or (null? l1)
               (and (eq-fn (caar l1) (caar l2))
                    (mono-equal? (cdar l1) (cdar l2))
                    (loop (cdr l1) (cdr l2))))))))

;;; ====
;;; Multivariate Division Algorithm
;;; ====

;;; mpoly-divmod : MPoly × (List MPoly) → ((List MPoly) × MPoly)
;;; Multivariate division: f = q_1*g_1 + ... + q_s*g_s + r
;;; Returns (list-of-quotients . remainder).
;;; The remainder has no term divisible by any leading term of g_i.
(define (mpoly-divmod f gs)
  (doc 'export #t)
  (let* ([F (mpoly-field f)]
         [vars (mpoly-vars f)]
         [ordering (mpoly-ordering f)]
         [zero (mpoly-zero F vars ordering)]
         [s (length gs)]
         [qs (make-list s zero)])
    (mpoly-div-loop f gs qs zero F vars ordering)))

;;; mpoly-div-loop : main division loop
(define (mpoly-div-loop p gs qs r F vars ordering)
  (if (mpoly-zero? p)
      (cons qs r)
      (let ([lt-p (mpoly-leading-term p)])
        (let try-divisors ([i 0] [gs-remaining gs] [qs qs])
          (if (null? gs-remaining)
              ;; No divisor found, add leading term to remainder
              (let* ([term-poly (make-mpoly F vars ordering (list lt-p))]
                     [new-r (mpoly-add r term-poly)]
                     [new-p (mpoly-sub p term-poly)])
                (mpoly-div-loop new-p gs qs new-r F vars ordering))
              (let* ([g (car gs-remaining)]
                     [lt-g (mpoly-leading-term g)])
                (if (mono-divides? (cdr lt-g) (cdr lt-p))
                    ;; Divisible: compute quotient term using field division
                    (let* ([q-coeff (field-div F (car lt-p) (car lt-g))]
                           [q-mono (mono-div (cdr lt-p) (cdr lt-g))]
                           [q-term (cons q-coeff q-mono)]
                           [q-poly (make-mpoly F vars ordering (list q-term))]
                           [new-qi (mpoly-add (list-ref qs i) q-poly)]
                           [new-qs (list-set qs i new-qi)]
                           [new-p (mpoly-sub p (mpoly-mul-term g q-term))])
                      (mpoly-div-loop new-p gs new-qs r F vars ordering))
                    ;; Not divisible by this g, try next
                    (try-divisors (+ i 1) (cdr gs-remaining) qs))))))))

;;; list-set : (List α) × Nat × α → (List α)
(define (list-set lst idx val)
  (if (= idx 0)
      (cons val (cdr lst))
      (cons (car lst) (list-set (cdr lst) (- idx 1) val))))

;;; ====
;;; MPoly Evaluation
;;; ====

;;; mpoly-eval : MPoly × (List (Symbol × Coeff)) → Coeff
;;; Evaluate polynomial at given point (variable → value mapping).
(define (mpoly-eval p env)
  (doc 'export #t)
  (let* ([F (mpoly-field p)]
         [add (field-add-op F)]
         [mul (field-mul-op F)]
         [zero (field-zero F)])
    (let loop ([terms (mpoly-terms p)] [acc zero])
      (if (null? terms)
          acc
          (let* ([term (car terms)]
                 [coeff (car term)]
                 [mono (cdr term)]
                 [term-val (mpoly-eval-term mul mono env (field-one F))])
            (loop (cdr terms) (add acc (mul coeff term-val))))))))

;;; mpoly-eval-term : evaluate single monomial
(define (mpoly-eval-term mul mono env one)
  (let loop ([m mono] [acc one])
    (if (null? m)
        acc
        (let* ([v (caar m)]
               [e (cdar m)]
               [val (cdr (assq v env))]
               [power (expt-int val e mul one)])
          (loop (cdr m) (mul acc power))))))

;;; expt-int : α × Nat × (α×α→α) × α → α
;;; Integer exponentiation using multiplication.
(define (expt-int base n mul one)
  (cond
    [(= n 0) one]
    [(= n 1) base]
    [(even? n)
     (let ([half (expt-int base (/ n 2) mul one)])
       (mul half half))]
    [else (mul base (expt-int base (- n 1) mul one))]))

;;; ====
;;; Display
;;; ====

;;; mpoly->string : MPoly → String
(define (mpoly->string p)
  (doc 'export #t)
  (let* ([F (mpoly-field p)]
         [terms (mpoly-terms p)]
         [zero (field-zero F)]
         [eq-fn (field-equal-fn F)])
    (if (and (= (length terms) 1)
             (null? (cdar terms))
             (eq-fn (caar terms) zero))
        "0"
        (let loop ([ts terms] [first? #t] [result ""])
          (if (null? ts)
              result
              (let ([str (mpoly-term->string (car ts) first? F)])
                (loop (cdr ts)
                      (and first? (string=? str ""))
                      (string-append result str))))))))

;;; mpoly-term->string : (Coeff × Monomial) × Boolean × Field → String
(define (mpoly-term->string term first? F)
  (let* ([coeff (car term)]
         [mono (cdr term)]
         [zero (field-zero F)]
         [one (field-one F)]
         [eq-fn (field-equal-fn F)]
         [mono-str (mono->string mono)])
    (cond
      [(eq-fn coeff zero) ""]
      [(string=? mono-str "1")  ; Constant term
       (if first?
           (coeff->display-string coeff)
           (if (and (number? coeff) (< coeff 0))
               (string-append " - " (coeff->display-string (- coeff)))
               (string-append " + " (coeff->display-string coeff))))]
      [(eq-fn coeff one)
       (if first? mono-str (string-append " + " mono-str))]
      [(and (number? coeff) (= coeff -1))
       (if first?
           (string-append "-" mono-str)
           (string-append " - " mono-str))]
      [else
       (if first?
           (string-append (coeff->display-string coeff) "*" mono-str)
           (if (and (number? coeff) (< coeff 0))
               (string-append " - " (coeff->display-string (- coeff)) "*" mono-str)
               (string-append " + " (coeff->display-string coeff) "*" mono-str)))])))

;;; mono->string : Monomial → String
(define (mono->string m)
  (if (null? m)
      "1"
      (string-join
       (map (lambda (pair)
              (let ([v (car pair)]
                    [e (cdr pair)])
                (if (= e 1)
                    (symbol->string v)
                    (string-append (symbol->string v) "^" (number->string e)))))
            m)
       "*")))

;;; coeff->display-string : Coeff → String
(define (coeff->display-string c)
  (cond
    [(number? c) (number->string c)]
    [(symbol? c) (symbol->string c)]
    [else (format "~a" c)]))

;; string-join is provided by prelude

;;; ====
;;; Utility
;;; ====

;; make-list is provided by prelude (alias for replicate)
