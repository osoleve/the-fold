(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
;;; @module egraph-groebner
;;; @requires prelude sort field multivariate groebner egraph/extract
;;; lattice/egraph/egraph-groebner.ss — Bridge E-Graph Rewriting to Gröbner Basis
;;;
;;; Connects e-graph equality saturation with Gröbner basis polynomial
;;; identity proving. E-graphs handle structural rewrites (commutativity,
;;; associativity); Gröbner bases prove algebraic identities via ideal
;;; membership. Together they cover both syntactic and semantic equivalence.
;;;
;;; Pipeline:
;;;   1. Convert arithmetic e-terms to multivariate polynomials
;;;   2. Use Gröbner basis to detect polynomial identities
;;;   3. Generate e-graph rewrite rules from basis elements
;;;   4. Feed back into equality saturation
;;;
;;; This is Lattice code: pure conversion and rule generation.

(require 'prelude)
(require 'sort)
(require 'field)
(require 'multivariate)
(require 'groebner)
(require 'egraph/extract)  ; Pulls in egraph, match, saturation, cost

(doc 'module 'egraph-groebner)
(doc 'bridges '(algebra egraph))
(doc 'description "Bridge e-graph rewriting to Gröbner basis polynomial identity proving")
(doc 'layer 'lattice)
(doc 'tier 2)
(doc 'purity 'partial)  ; E-graph uses mutation internally

;;; ============================================================
;;; E-Term → Multivariate Polynomial Conversion
;;; ============================================================

(doc 'section 'conversion)

;;; eterm->mpoly : Term × Field × (List Symbol) × Ordering → MPoly
;;; Convert a flat arithmetic e-graph term to a multivariate polynomial.
;;;
;;; Handles: +, -, *, neg, numeric literals, and symbolic variables.
;;; Exponentiation (^ x n) is expanded for non-negative integer n.
;;; Division and transcendentals return #f (not polynomial).
;;;
;;; Examples:
;;;   (+ x (* 2 y))  → x + 2y
;;;   (* x (+ x 1))  → x² + x  (after polynomial arithmetic)
;;;   (^ x 3)        → x³
(define (eterm->mpoly term F vars ordering)
  (doc 'type (-> Term Field (List Symbol) Ordering (Or MPoly #f)))
  (doc 'description "Convert arithmetic e-graph term to multivariate polynomial.")
  (doc 'export #t)
  (cond
    ;; Numeric literal → constant polynomial
    [(number? term)
     (mpoly-constant F vars ordering term)]

    ;; Symbol → variable polynomial (if in vars list) or fail
    [(symbol? term)
     (if (memq term vars)
         (mpoly-var F vars ordering term)
         #f)]

    ;; Compound term
    [(pair? term)
     (let ([op (car term)])
       (cond
         ;; Binary addition: (+ a b)
         [(and (eq? op '+) (= (length term) 3))
          (let ([a (eterm->mpoly (cadr term) F vars ordering)]
                [b (eterm->mpoly (caddr term) F vars ordering)])
            (and a b (mpoly-add a b)))]

         ;; Binary subtraction: (- a b)
         [(and (eq? op '-) (= (length term) 3))
          (let ([a (eterm->mpoly (cadr term) F vars ordering)]
                [b (eterm->mpoly (caddr term) F vars ordering)])
            (and a b (mpoly-sub a b)))]

         ;; Binary multiplication: (* a b)
         [(and (eq? op '*) (= (length term) 3))
          (let ([a (eterm->mpoly (cadr term) F vars ordering)]
                [b (eterm->mpoly (caddr term) F vars ordering)])
            (and a b (mpoly-mul a b)))]

         ;; Unary negation: (neg a)
         [(and (eq? op 'neg) (= (length term) 2))
          (let ([a (eterm->mpoly (cadr term) F vars ordering)])
            (and a (mpoly-neg a)))]

         ;; Integer exponentiation: (^ a n) where n is non-negative integer
         [(and (eq? op '^) (= (length term) 3)
               (integer? (caddr term)) (>= (caddr term) 0))
          (let ([base (eterm->mpoly (cadr term) F vars ordering)]
                [exp (caddr term)])
            (and base (mpoly-power base exp)))]

         ;; Anything else (/, sin, cos, etc.) → not polynomial
         [else #f]))]

    [else #f]))

;;; mpoly->eterm : MPoly → Term
;;; Convert a multivariate polynomial back to a flat e-graph term.
;;; Produces right-nested binary operations.
;;;
;;; Example:
;;;   3x²y + 2x - 1 → (+ (* 3 (* (^ x 2) y)) (+ (* 2 x) -1))
(define (mpoly->eterm p)
  (doc 'type (-> MPoly Term))
  (doc 'description "Convert multivariate polynomial to flat e-graph term.")
  (doc 'export #t)
  (let ([terms (mpoly-terms p)]
        [F (mpoly-field p)])
    (if (null? terms)
        0
        (let ([eterms (map (lambda (t) (term->eterm t F)) terms)])
          (fold-right (lambda (t acc) (list '+ t acc))
                      (car (reverse eterms))
                      (reverse (cdr (reverse eterms))))))))

;;; term->eterm : (Coeff . Monomial) × Field → Term
;;; Convert a single polynomial term (coefficient × monomial) to e-term.
(define (term->eterm ct F)
  (let ([coeff (car ct)]
        [mono (cdr ct)])
    (let ([mono-term (mono->eterm mono)])
      (cond
        ;; Coefficient is 1 and monomial is non-trivial
        [(and (field-equal? F coeff (field-one F))
              mono-term)
         mono-term]
        ;; Coefficient is -1 and monomial is non-trivial
        [(and (field-equal? F coeff ((field-neg-fn F) (field-one F)))
              mono-term)
         (list 'neg mono-term)]
        ;; Monomial is trivial (constant term)
        [(not mono-term)
         coeff]
        ;; General case: coeff * monomial
        [else
         (list '* coeff mono-term)]))))

;;; mono->eterm : Monomial → Term | #f
;;; Convert monomial to e-term. Returns #f for constant monomial (empty).
(define (mono->eterm mono)
  (if (null? mono)
      #f
      (let ([factors (map (lambda (ve)
                            (let ([v (car ve)]
                                  [e (cdr ve)])
                              (if (= e 1)
                                  v
                                  (list '^ v e))))
                          mono)])
        (if (= (length factors) 1)
            (car factors)
            (fold-right (lambda (f acc) (list '* f acc))
                        (car (reverse factors))
                        (reverse (cdr (reverse factors))))))))

;;; field-equal? : Field × a × a → Boolean
;;; Test equality using field's equality function.
(define (field-equal? F a b)
  ((field-equal-fn F) a b))

;;; ============================================================
;;; Variable Extraction
;;; ============================================================

(doc 'section 'variables)

;;; eterm-variables : Term → (List Symbol)
;;; Extract all symbolic variables from an e-graph term, sorted.
(define (eterm-variables term)
  (doc 'type (-> Term (List Symbol)))
  (doc 'description "Extract sorted list of variables from an e-graph term.")
  (doc 'export #t)
  (sort-by (lambda (a b) (string<? (symbol->string a) (symbol->string b)))
           (let collect ([t term] [vars '()])
             (cond
               [(symbol? t)
                (if (and (not (memq t '(+ - * / ^ neg)))
                         (not (memq t vars)))
                    (cons t vars)
                    vars)]
               [(pair? t)
                (fold-left (lambda (acc child) (collect child acc)) vars (cdr t))]
               [else vars]))))

;;; ============================================================
;;; Polynomial Equivalence
;;; ============================================================

(doc 'section 'equivalence)

;;; poly-equiv? : Term × Term → Boolean
;;; Test if two arithmetic e-terms are equivalent as polynomials.
;;; Works by converting both to polynomials and checking if their
;;; difference is zero.
(define (poly-equiv? t1 t2)
  (doc 'type (-> Term Term Boolean))
  (doc 'description "Test polynomial equivalence of two e-graph terms.")
  (doc 'export #t)
  (poly-equiv-over? t1 t2 Q-field))

;;; poly-equiv-over? : Term × Term × Field → Boolean
;;; Test polynomial equivalence over a specific field.
(define (poly-equiv-over? t1 t2 F)
  (doc 'type (-> Term Term Field Boolean))
  (doc 'description "Test polynomial equivalence over a specific field.")
  (doc 'export #t)
  (let* ([vars (sort-by (lambda (a b) (string<? (symbol->string a) (symbol->string b)))
                          (union-symbols (eterm-variables t1) (eterm-variables t2)))]
         [ordering (make-ordering 'grevlex vars)]
         [p1 (eterm->mpoly t1 F vars ordering)]
         [p2 (eterm->mpoly t2 F vars ordering)])
    (and p1 p2
         (mpoly-zero? (mpoly-sub p1 p2)))))

;;; poly-equiv-modulo? : Term × Term × (List Term) × Field → Boolean
;;; Test if t1 ≡ t2 modulo polynomial relations.
;;; The relations define an ideal; equivalence means the difference
;;; lies in that ideal. Uses Gröbner basis for decision.
;;;
;;; Example: x² ≡ 1 modulo {x² - 1}
(define (poly-equiv-modulo? t1 t2 relations F)
  (doc 'type (-> Term Term (List Term) Field Boolean))
  (doc 'description "Test polynomial equivalence modulo an ideal.")
  (doc 'export #t)
  (let* ([vars (sort-by (lambda (a b) (string<? (symbol->string a) (symbol->string b)))
                        (fold-left (lambda (acc r) (union-symbols acc (eterm-variables r)))
                                   (union-symbols (eterm-variables t1) (eterm-variables t2))
                                   relations))]
         [ordering (make-ordering 'grevlex vars)]
         [p1 (eterm->mpoly t1 F vars ordering)]
         [p2 (eterm->mpoly t2 F vars ordering)]
         [rel-polys (map (lambda (r) (eterm->mpoly r F vars ordering)) relations)])
    (and p1 p2 (not (memq #f rel-polys))
         (let* ([diff (mpoly-sub p1 p2)]
                [basis (buchberger rel-polys)])
           (ideal-membership? diff basis)))))

;;; union-symbols : (List Symbol) × (List Symbol) → (List Symbol)
;;; Merge two sorted symbol lists, removing duplicates.
(define (union-symbols a b)
  (cond
    [(null? a) b]
    [(null? b) a]
    [(eq? (car a) (car b))
     (cons (car a) (union-symbols (cdr a) (cdr b)))]
    [(string<? (symbol->string (car a)) (symbol->string (car b)))
     (cons (car a) (union-symbols (cdr a) b))]
    [else
     (cons (car b) (union-symbols a (cdr b)))]))

;;; ============================================================
;;; Gröbner → E-Graph Rewrite Rules
;;; ============================================================

(doc 'section 'rules)

;;; groebner-rewrite-rules : (List Term) → (List Rule)
;;; Generate e-graph rewrite rules from polynomial relations.
;;; Each relation p = 0 becomes a rule that rewrites any term
;;; containing p's leading term to the negation of p's tail.
;;;
;;; Example: x² + y² - 1 = 0  →  rule: (^ x 2) ⟹ (- 1 (^ y 2))
;;;          i.e., x² can be replaced with 1 - y²
(define (groebner-rewrite-rules relations)
  (doc 'type (-> (List Term) (List Rule)))
  (doc 'description "Generate e-graph rewrite rules from polynomial relations.")
  (doc 'export #t)
  (groebner-rewrite-rules-over relations Q-field))

;;; groebner-rewrite-rules-over : (List Term) × Field → (List Rule)
;;; Generate rewrite rules from relations over a specific field.
(define (groebner-rewrite-rules-over relations F)
  (doc 'type (-> (List Term) Field (List Rule)))
  (doc 'description "Generate e-graph rewrite rules from polynomial relations over a field.")
  (doc 'export #t)
  (let* ([vars (sort-by (lambda (a b) (string<? (symbol->string a) (symbol->string b)))
                        (fold-left (lambda (acc r) (union-symbols acc (eterm-variables r)))
                                   '()
                                   relations))]
         [ordering (make-ordering 'grevlex vars)]
         ;; Convert to polynomials
         [polys (filter (lambda (x) x)
                        (map (lambda (r) (eterm->mpoly r F vars ordering)) relations))]
         ;; Compute reduced Gröbner basis
         [basis (if (null? polys) '() (reduce-basis (buchberger polys)))])
    ;; Each basis element g gives a rule: LT(g) → -tail(g)
    (filter (lambda (x) x)
            (map (lambda (g) (basis-element->rule g)) basis))))

;;; basis-element->rule : MPoly → Rule | #f
;;; Convert a single Gröbner basis element to a rewrite rule.
;;; The leading term rewrites to the negation of the remaining terms.
(define (basis-element->rule g)
  (let ([terms (mpoly-terms g)])
    (if (or (null? terms) (null? (cdr terms)))
        ;; Single-term polynomial (e.g., x = 0) — not a useful rewrite
        #f
        (let* ([F (mpoly-field g)]
               [vars (mpoly-vars g)]
               [ordering (mpoly-ordering g)]
               ;; Leading term → lhs of rule
               [lt (car terms)]
               ;; Remaining terms → negated → rhs of rule
               [tail-poly (make-mpoly F vars ordering (cdr terms))]
               [neg-tail (mpoly-neg tail-poly)]
               [lhs (term->eterm lt F)]
               [rhs (mpoly->eterm neg-tail)])
          (make-rule lhs rhs)))))

;;; ============================================================
;;; Polynomial Algebra Rules for E-Graphs
;;; ============================================================

(doc 'section 'algebra-rules)

;;; poly-identity-rules : → (List Rule)
;;; Standard polynomial identity rules for e-graph saturation.
;;; These are the structural rules that complement Gröbner's
;;; algebraic rules.
(define (poly-identity-rules)
  (doc 'type (-> (List Rule)))
  (doc 'description "Standard polynomial rewrite rules for e-graph saturation.")
  (doc 'export #t)
  (list
   ;; Additive identity
   (make-rule '(+ ?x 0) '?x)
   (make-rule '(+ 0 ?x) '?x)
   ;; Multiplicative identity
   (make-rule '(* ?x 1) '?x)
   (make-rule '(* 1 ?x) '?x)
   ;; Zero absorption
   (make-rule '(* ?x 0) '0)
   (make-rule '(* 0 ?x) '0)
   ;; Self-subtraction
   (make-rule '(- ?x ?x) '0)
   ;; Negation
   (make-rule '(neg (neg ?x)) '?x)
   (make-rule '(neg ?x) '(* -1 ?x))
   ;; Power rules
   (make-rule '(^ ?x 0) '1)
   (make-rule '(^ ?x 1) '?x)
   ;; Commutativity
   (make-rule '(+ ?x ?y) '(+ ?y ?x))
   (make-rule '(* ?x ?y) '(* ?y ?x))
   ;; Associativity
   (make-rule '(+ (+ ?x ?y) ?z) '(+ ?x (+ ?y ?z)))
   (make-rule '(* (* ?x ?y) ?z) '(* ?x (* ?y ?z)))
   ;; Distributivity
   (make-rule '(* ?x (+ ?y ?z)) '(+ (* ?x ?y) (* ?x ?z)))))

;;; ============================================================
;;; Polynomial Cost Model
;;; ============================================================

(doc 'section 'cost-model)

;;; poly-degree-cost : CostModel
;;; Cost model that prefers lower polynomial degree.
;;; Useful for extracting reduced polynomial forms.
;;; Weights: exponentiation is expensive, addition is cheap.
(doc 'type 'poly-degree-cost CostModel)
(doc 'description 'poly-degree-cost "Cost model preferring lower-degree polynomial forms.")
(doc 'export 'poly-degree-cost #t)
(define poly-degree-cost
  (make-cost-model 'poly-degree
    (lambda (node child-cost)
      (let ([op (enode-op node)]
            [children (enode-children node)])
        (cond
          ;; Exponentiation: cost proportional to exponent
          [(and (eq? op '^) (= (vector-length children) 2))
           (let ([base-cost (child-cost (vector-ref children 0))]
                 [exp-cost (child-cost (vector-ref children 1))])
             ;; Penalize high exponents
             (+ (* 5 exp-cost) base-cost))]
          ;; Multiplication: slightly more expensive (increases degree)
          [(eq? op '*)
           (+ 3 (fold-left (lambda (acc i)
                              (+ acc (child-cost (vector-ref children i))))
                            0
                            (iota (vector-length children))))]
          ;; Addition: cheap (doesn't increase degree)
          [(eq? op '+)
           (+ 1 (fold-left (lambda (acc i)
                              (+ acc (child-cost (vector-ref children i))))
                            0
                            (iota (vector-length children))))]
          ;; Negation: free
          [(eq? op 'neg)
           (if (> (vector-length children) 0)
               (child-cost (vector-ref children 0))
               1)]
          ;; Leaves (variables, constants): unit cost
          [(zero? (vector-length children)) 1]
          ;; Default
          [else
           (+ 2 (fold-left (lambda (acc i)
                              (+ acc (child-cost (vector-ref children i))))
                            0
                            (iota (vector-length children))))])))))

;;; ============================================================
;;; Combined Optimization
;;; ============================================================

(doc 'section 'optimize)

;;; poly-optimize : Term × (List Term) → Term
;;; Optimize an arithmetic term using both e-graph saturation and
;;; Gröbner basis rewriting. The combined approach handles:
;;; - Structural rewrites (commutativity, associativity) via e-graph
;;; - Algebraic identities (polynomial relations) via Gröbner
;;;
;;; Example:
;;;   (poly-optimize '(+ (* x x) (* 2 (* x y)) (* y y))
;;;                  '())
;;;   → might find simpler form via saturation
;;;
;;;   (poly-optimize '(+ (^ x 2) -1)
;;;                  '((- (^ x 2) 1)))
;;;   → 0  (since x² - 1 = 0 in the ideal)
(define (poly-optimize term relations)
  (doc 'type (-> Term (List Term) Term))
  (doc 'description "Optimize term using e-graph saturation with Gröbner-derived rules.")
  (doc 'export #t)
  (poly-optimize-over term relations Q-field))

;;; poly-optimize-over : Term × (List Term) × Field → Term
;;; Optimize over a specific coefficient field.
(define (poly-optimize-over term relations F)
  (doc 'type (-> Term (List Term) Field Term))
  (doc 'description "Optimize term using e-graph saturation with Gröbner rules over a field.")
  (doc 'export #t)
  (let* ([groebner-rules (groebner-rewrite-rules-over relations F)]
         [all-rules (append (poly-identity-rules) groebner-rules)]
         [config (make-saturation-config 30 5000 0)])
    (optimize-with-config term all-rules poly-degree-cost config)))

;;; poly-reduce : Term → Term
;;; Reduce a polynomial term to a canonical form via Gröbner normal form.
;;; Unlike poly-optimize, this doesn't use e-graph saturation — it converts
;;; to a polynomial, reduces, and converts back. Faster but less flexible.
(define (poly-reduce term)
  (doc 'type (-> Term Term))
  (doc 'description "Reduce term to polynomial normal form.")
  (doc 'export #t)
  (poly-reduce-over term Q-field))

;;; poly-reduce-over : Term × Field → Term
;;; Reduce over a specific field.
(define (poly-reduce-over term F)
  (doc 'type (-> Term Field Term))
  (doc 'description "Reduce term to polynomial normal form over a specific field.")
  (doc 'export #t)
  (let* ([vars (eterm-variables term)]
         [ordering (make-ordering 'grevlex vars)]
         [p (eterm->mpoly term F vars ordering)])
    (if p
        (mpoly->eterm p)
        ;; Not a polynomial term, return as-is
        term)))

;;; poly-reduce-modulo : Term × (List Term) → Term
;;; Reduce a term modulo polynomial relations using Gröbner normal form.
(define (poly-reduce-modulo term relations)
  (doc 'type (-> Term (List Term) Term))
  (doc 'description "Reduce term modulo polynomial relations via Gröbner normal form.")
  (doc 'export #t)
  (poly-reduce-modulo-over term relations Q-field))

;;; poly-reduce-modulo-over : Term × (List Term) × Field → Term
;;; Reduce modulo relations over a specific field.
(define (poly-reduce-modulo-over term relations F)
  (doc 'type (-> Term (List Term) Field Term))
  (doc 'description "Reduce term modulo relations over a specific field.")
  (doc 'export #t)
  (let* ([vars (fold-left (lambda (acc r) (union-symbols acc (eterm-variables r)))
                          (eterm-variables term)
                          relations)]
         [ordering (make-ordering 'grevlex vars)]
         [p (eterm->mpoly term F vars ordering)]
         [rel-polys (filter (lambda (x) x)
                            (map (lambda (r) (eterm->mpoly r F vars ordering)) relations))])
    (if (and p (not (null? rel-polys)))
        (let* ([basis (reduce-basis (buchberger rel-polys))]
               [reduced (normal-form p basis)])
          (mpoly->eterm reduced))
        (if p (mpoly->eterm p) term))))
