(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
;;; @module egraph-simplify
;;; @requires prelude egraph/extract expr
;;; lattice/symbolic/egraph-simplify.ss — E-Graph Equality Saturation for Symbolic Expressions
;;;
;;; Bridges the symbolic expression system (expr.ss) with the e-graph
;;; equality saturation engine (egraph/). Converts tagged symbolic exprs
;;; to flat S-expression terms for the e-graph, defines algebraic rewrite
;;; rules, saturates, extracts the minimum-cost form, and converts back.
;;;
;;; The key insight: symbolic exprs use tagged constructors ((num 5), (var x)),
;;; but the e-graph works on flat terms ((+ x y), literal leaves). We translate
;;; between the two representations, letting the e-graph explore the equivalence
;;; space exhaustively before extracting the simplest form.

(require 'prelude)
(require 'egraph/extract)  ; Pulls in egraph, match, saturation, cost
(require 'expr)

(doc 'module 'egraph-simplify)
(doc 'bridges '(fp egraph))
(doc 'description "E-graph equality saturation for symbolic expression simplification")
(doc 'layer 'lattice)
(doc 'tier 2)
(doc 'purity 'partial)  ; E-graph uses mutation internally

;;; ============================================================
;;; Symbolic Expr → E-Graph Term Conversion
;;; ============================================================

(doc 'section 'conversion)

;;; expr->eterm : Expr → Term
;;; Convert a tagged symbolic expression to a flat S-expression term
;;; suitable for the e-graph.
;;;
;;; Mapping:
;;;   (num n)       → n              (numeric literal)
;;;   (var x)       → x              (symbol leaf)
;;;   (+ e1 e2)     → (+ t1 t2)     (binary, flatten n-ary to right-nested)
;;;   (* e1 e2)     → (* t1 t2)     (binary, flatten n-ary to right-nested)
;;;   (- e1 e2)     → (- t1 t2)     (binary difference)
;;;   (- e1)        → (neg t1)       (unary negation)
;;;   (/ e1 e2)     → (/ t1 t2)     (division)
;;;   (^ e1 e2)     → (^ t1 t2)     (exponentiation)
;;;   (fn arg)      → (fn t-arg)    (function application)
(define (expr->eterm e)
  (doc 'type (-> Expr Term))
  (doc 'description "Convert symbolic expr to flat e-graph term.")
  (doc 'export #t)
  (cond
    [(num? e) (num-val e)]
    [(var? e) (var-name e)]
    [(sum? e)
     (let ([terms (map expr->eterm (sum-terms e))])
       (fold-right (lambda (t acc) (list '+ t acc))
                   (car (reverse terms))
                   (reverse (cdr (reverse terms)))))]
    [(product? e)
     (let ([factors (map expr->eterm (product-factors e))])
       (fold-right (lambda (f acc) (list '* f acc))
                   (car (reverse factors))
                   (reverse (cdr (reverse factors)))))]
    [(difference? e)
     (if (diff-right e)
         (list '- (expr->eterm (diff-left e)) (expr->eterm (diff-right e)))
         (list 'neg (expr->eterm (diff-left e))))]
    [(quotient? e)
     (list '/ (expr->eterm (quot-numer e)) (expr->eterm (quot-denom e)))]
    [(power? e)
     (list '^ (expr->eterm (pow-base e)) (expr->eterm (pow-exp e)))]
    [(app? e)
     (list (app-fn e) (expr->eterm (app-arg e)))]
    [else e]))

;;; eterm->expr : Term → Expr
;;; Convert an e-graph extracted term back to a tagged symbolic expression.
(define (eterm->expr t)
  (doc 'type (-> Term Expr))
  (doc 'description "Convert e-graph term back to symbolic expr.")
  (doc 'export #t)
  (cond
    [(number? t) (num t)]
    [(symbol? t) (var t)]
    [(pair? t)
     (let ([op (car t)])
       (cond
         [(eq? op '+)
          (sum (eterm->expr (cadr t)) (eterm->expr (caddr t)))]
         [(eq? op '*)
          (product (eterm->expr (cadr t)) (eterm->expr (caddr t)))]
         [(eq? op '-)
          (difference (eterm->expr (cadr t)) (eterm->expr (caddr t)))]
         [(eq? op 'neg)
          (make-neg (eterm->expr (cadr t)))]
         [(eq? op '/)
          (division (eterm->expr (cadr t)) (eterm->expr (caddr t)))]
         [(eq? op '^)
          (power (eterm->expr (cadr t)) (eterm->expr (caddr t)))]
         ;; Function application (sin, cos, exp, log, etc.)
         [(and (symbol? op) (= (length t) 2))
          (make-app op (eterm->expr (cadr t)))]
         ;; Fallback: try to normalize
         [else (normalize-expr t)]))]
    [else (normalize-expr t)]))

;;; ============================================================
;;; Algebraic Rewrite Rules
;;; ============================================================

(doc 'section 'rules)

;;; --- Additive identity ---
(define sym-add-identity-rules
  (list
   (make-rule '(+ ?x 0) '?x)
   (make-rule '(+ 0 ?x) '?x)))

;;; --- Multiplicative identity ---
(define sym-mul-identity-rules
  (list
   (make-rule '(* ?x 1) '?x)
   (make-rule '(* 1 ?x) '?x)))

;;; --- Zero multiplication ---
(define sym-mul-zero-rules
  (list
   (make-rule '(* ?x 0) '0)
   (make-rule '(* 0 ?x) '0)))

;;; --- Commutativity ---
(define sym-comm-rules
  (list
   (make-rule '(+ ?x ?y) '(+ ?y ?x))
   (make-rule '(* ?x ?y) '(* ?y ?x))))

;;; --- Associativity ---
(define sym-assoc-rules
  (list
   (make-rule '(+ (+ ?x ?y) ?z) '(+ ?x (+ ?y ?z)))
   (make-rule '(+ ?x (+ ?y ?z)) '(+ (+ ?x ?y) ?z))
   (make-rule '(* (* ?x ?y) ?z) '(* ?x (* ?y ?z)))
   (make-rule '(* ?x (* ?y ?z)) '(* (* ?x ?y) ?z))))

;;; --- Distributivity ---
(define sym-distrib-rules
  (list
   ;; Expand: a*(b+c) = a*b + a*c
   (make-rule '(* ?x (+ ?y ?z)) '(+ (* ?x ?y) (* ?x ?z)))
   ;; Factor: a*b + a*c = a*(b+c)
   (make-rule '(+ (* ?x ?y) (* ?x ?z)) '(* ?x (+ ?y ?z)))))

;;; --- Subtraction / negation ---
(define sym-sub-rules
  (list
   (make-rule '(- ?x 0) '?x)
   (make-rule '(- ?x ?x) '0)
   (make-rule '(neg (neg ?x)) '?x)
   ;; Negation as multiplication by -1
   (make-rule '(neg ?x) '(* -1 ?x))
   (make-rule '(* -1 ?x) '(neg ?x))))

;;; --- Division ---
(define sym-div-rules
  (list
   (make-rule '(/ ?x 1) '?x)
   (make-rule '(/ 0 ?x) '0)
   (make-rule '(/ ?x ?x) '1)))

;;; --- Exponentiation ---
(define sym-pow-rules
  (list
   (make-rule '(^ ?x 0) '1)
   (make-rule '(^ ?x 1) '?x)
   (make-rule '(^ 1 ?x) '1)))

;;; --- Exp / log ---
(define sym-exp-log-rules
  (list
   (make-rule '(exp (log ?x)) '?x)
   (make-rule '(log (exp ?x)) '?x)))

;;; --- Combined rule sets ---

;;; Core simplification rules: identities, zero, sub, div, pow.
;;; These are safe (no size explosion).
(define sym-core-rules
  (append sym-add-identity-rules
          sym-mul-identity-rules
          sym-mul-zero-rules
          sym-sub-rules
          sym-div-rules
          sym-pow-rules
          sym-exp-log-rules))

;;; Full simplification rules: core + commutativity + associativity + distributivity.
;;; Use with bounded fuel to prevent explosion.
(define sym-full-rules
  (append sym-core-rules
          sym-comm-rules
          sym-assoc-rules
          sym-distrib-rules))

;;; ============================================================
;;; E-Graph Simplification
;;; ============================================================

(doc 'section 'simplification)

;;; egraph-simplify : Expr → Expr
;;; Simplify a symbolic expression using e-graph equality saturation.
;;; Uses AST size as cost model to extract the smallest equivalent form.
;;;
;;; This is a drop-in alternative to `simplify` from simplify.ss.
;;; It explores the full equivalence space via saturation rather than
;;; applying rules in a fixed order.
(define (egraph-simplify e)
  (doc 'type (-> Expr Expr))
  (doc 'description "Simplify symbolic expr via e-graph equality saturation.")
  (doc 'export #t)
  (egraph-simplify-with e sym-core-rules
                        (make-saturation-config 30 5000 0)
                        ast-size-cost))

;;; egraph-simplify-full : Expr → Expr
;;; Like egraph-simplify but includes commutativity, associativity,
;;; and distributivity. More powerful but slower.
(define (egraph-simplify-full e)
  (doc 'type (-> Expr Expr))
  (doc 'description "Full e-graph simplification with all algebraic rules.")
  (doc 'export #t)
  (egraph-simplify-with e sym-full-rules
                        (make-saturation-config 15 3000 0)
                        ast-size-cost))

;;; egraph-simplify-with : Expr × Rules × Config × CostModel → Expr
;;; Core simplification pipeline: convert, saturate, extract, convert back.
(define (egraph-simplify-with e rules config cost-model)
  (doc 'type (-> Expr (List Rule) SaturationConfig CostModel Expr))
  (doc 'description "Simplify using custom rules, config, and cost model.")
  (doc 'export #t)
  (let* ([term (expr->eterm e)]
         [eg (make-egraph)]
         [root (egraph-add-term! eg term)])
    (saturate eg rules config)
    (let* ([state (make-extraction-state eg cost-model)]
           [extracted (extract state root)])
      (eterm->expr extracted))))

