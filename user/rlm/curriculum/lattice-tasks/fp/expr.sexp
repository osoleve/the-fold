;;; expr.sexp — Development tasks for lattice/fp/symbolic/expr.ss
;;;
;;; Module: expr (tier 0, depends on prelude)
;;; 631 lines, ~40 exported functions
;;; Core symbolic expression data structures for computer algebra.
;;; Tagged S-expression representation: (num n), (var x), (+ e1 e2), etc.
;;; Constructors, predicates, accessors, smart constructors with simplification,
;;; structural equality, free variable collection, substitution, pattern matching,
;;; normalization, and display.
;;;
;;; Git history:
;;;   a3abed54 refactor: Reorganize into core/lattice/shell three-layer architecture
;;;   0a3e60a9 feat(module): migrate lattice/fp/ layers 2-7 to require
;;;   3beba231 fix(symbolic,linalg,meta): resolve 3 BBS issues

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement make-sum
  ;; ──────────────────────────────────────────────
  ;; Create a sum expression from a list of terms.

  (task
    (id "fp/expr/make-sum-impl")
    (module expr)
    (tier 0)
    (type implement)
    (description "Implement make-sum: create a sum expression from a list of sub-expressions. Handle three cases with cond: (1) empty list -> return (num 0), the additive identity. (2) single element -> return that element directly (no wrapping). (3) multiple elements -> cons the '+ tag onto the list. This is a \"dumb\" constructor — it doesn't simplify. The smart constructor (sum) handles simplifications like constant folding. The n-ary representation (+ e1 e2 ... en) stores terms in cdr, retrieved by sum-terms.")
    (context "Available: null?, length, =, car, cons, num, cond. List length check + tag consing.")
    (before "(define (make-sum exprs)
  ;; TODO: create sum from list of expressions
  '())")
    (test-file "lattice/fp/symbolic/test-expr.ss")
    (test-forms (";; Empty sum is zero (additive identity)
(assert-equal (num 0) (make-sum '()))"
                 ";; Single-element sum is unwrapped
(let ([v (var 'x)])
  (assert-equal v (make-sum (list v))))"
                 ";; Multiple elements get + tag
(let ([e (make-sum (list (num 1) (num 2) (num 3)))])
  (assert-true (sum? e))
  (assert-equal 3 (length (sum-terms e))))"
                 ";; sum-terms retrieves terms back
(let ([e (make-sum (list (var 'x) (var 'y)))])
  (assert-equal 2 (length (sum-terms e))))"))
    (available-modules (prelude))
    (hints ("Three cases: null?, single (length 1), multiple"
            "Empty: (num 0)"
            "Single: (car exprs)"
            "Multiple: (cons '+ exprs)"))
    (reference-solution "(define (make-sum exprs)
  (cond
   [(null? exprs) (num 0)]
   [(= (length exprs) 1) (car exprs)]
   [else (cons '+ exprs)]))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement expr=?
  ;; ──────────────────────────────────────────────
  ;; Structural equality for symbolic expressions.

  (task
    (id "fp/expr/expr-equal-impl")
    (module expr)
    (tier 0)
    (type implement)
    (description "Implement expr=?: check structural equality of two symbolic expressions. Multi-way cond dispatch on expression type pairs: (1) both num? -> compare values with =. (2) both var? -> compare names with eq?. (3) both sum? -> same length and all corresponding terms equal (use list-all-equal? on cdr). (4) both product? -> same pattern as sums. (5) both power? -> base and exponent both equal. (6) both app? -> same function name (eq?) and equal arguments. (7) else -> #f. This is the fundamental equality used by smart constructors (e.g., difference checks expr=? for x - x = 0) and by the pattern matcher.")
    (context "Available: num?, var?, sum?, product?, power?, app?, num-val, var-name, pow-base, pow-exp, app-fn, app-arg, =, eq?, length, cdr, list-all-equal?, and, cond. Type-dispatched structural recursion.")
    (before "(define (expr=? e1 e2)
  ;; TODO: structural equality of expressions
  #f)")
    (test-file "lattice/fp/symbolic/test-expr.ss")
    (test-forms (";; Numeric equality
(assert-true (expr=? (num 5) (num 5)))
(assert-false (expr=? (num 5) (num 6)))"
                 ";; Variable equality
(assert-true (expr=? (var 'x) (var 'x)))
(assert-false (expr=? (var 'x) (var 'y)))"
                 ";; Sum equality (order matters)
(let ([s1 (list '+ (num 1) (var 'x))]
      [s2 (list '+ (num 1) (var 'x))]
      [s3 (list '+ (num 2) (var 'x))])
  (assert-true (expr=? s1 s2))
  (assert-false (expr=? s1 s3)))"
                 ";; Power equality
(let ([p1 (make-pow (var 'x) (num 2))]
      [p2 (make-pow (var 'x) (num 2))]
      [p3 (make-pow (var 'x) (num 3))])
  (assert-true (expr=? p1 p2))
  (assert-false (expr=? p1 p3)))"
                 ";; Function application equality
(let ([e1 (sym-sin (var 'x))]
      [e2 (sym-sin (var 'x))]
      [e3 (sym-cos (var 'x))])
  (assert-true (expr=? e1 e2))
  (assert-false (expr=? e1 e3)))"))
    (available-modules (prelude))
    (hints ("Dispatch on type pairs: (and (num? e1) (num? e2)) -> (= (num-val e1) (num-val e2))"
            "For sums/products: check same length AND list-all-equal? on the terms"
            "For powers: recursively check base and exponent"
            "For apps: (eq? (app-fn e1) (app-fn e2)) AND (expr=? (app-arg e1) (app-arg e2))"
            "Default: #f"))
    (reference-solution "(define (expr=? e1 e2)
  (cond
   [(and (num? e1) (num? e2))
    (= (num-val e1) (num-val e2))]
   [(and (var? e1) (var? e2))
    (eq? (var-name e1) (var-name e2))]
   [(and (sum? e1) (sum? e2))
    (and (= (length e1) (length e2))
         (list-all-equal? (cdr e1) (cdr e2)))]
   [(and (product? e1) (product? e2))
    (and (= (length e1) (length e2))
         (list-all-equal? (cdr e1) (cdr e2)))]
   [(and (power? e1) (power? e2))
    (and (expr=? (pow-base e1) (pow-base e2))
         (expr=? (pow-exp e1) (pow-exp e2)))]
   [(and (app? e1) (app? e2))
    (and (eq? (app-fn e1) (app-fn e2))
         (expr=? (app-arg e1) (app-arg e2)))]
   [else #f]))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement free-vars
  ;; ──────────────────────────────────────────────
  ;; Collect all free variables in an expression.

  (task
    (id "fp/expr/free-vars-impl")
    (module expr)
    (tier 0)
    (type implement)
    (description "Implement free-vars: collect all free variable names from a symbolic expression into a deduplicated list. Multi-way cond dispatch: (1) num? -> '() (no variables). (2) var? -> (list (var-name e)), a singleton. (3) sum? -> collect from all terms via append-map, then deduplicate with unique. (4) product? -> same pattern over product-factors. (5) difference? -> union of left and right (handle unary negation where diff-right returns #f). (6) quotient? -> union of numerator and denominator. (7) power? -> union of base and exponent. (8) app? -> recurse into argument. The key pattern is: recursively collect from sub-expressions, flatten with append-map or append, deduplicate with unique.")
    (context "Available: num?, var?, sum?, product?, difference?, quotient?, power?, app?, var-name, sum-terms, product-factors, diff-left, diff-right, quot-numer, quot-denom, pow-base, pow-exp, app-arg, list, append, append-map, unique, free-vars (recursive), cond. Recursive collection with deduplication.")
    (before "(define (free-vars e)
  ;; TODO: collect all free variable names
  '())")
    (test-file "lattice/fp/symbolic/test-expr.ss")
    (test-forms (";; Numbers have no variables
(assert-equal '() (free-vars (num 5)))"
                 ";; Variables have themselves
(assert-equal '(x) (free-vars (var 'x)))"
                 ";; Sum collects from all terms, deduplicates
(let ([fv (free-vars (list '+ (var 'x) (var 'y) (var 'x)))])
  (assert-true (and (member 'x fv) #t))
  (assert-true (and (member 'y fv) #t))
  (assert-equal 2 (length fv)))"
                 ";; Nested expressions collect from all sub-parts
(let* ([e (make-pow (list '+ (var 'x) (var 'y)) (var 'n))]
       [fv (free-vars e)])
  (assert-equal 3 (length fv))
  (assert-true (and (member 'x fv) #t))
  (assert-true (and (member 'n fv) #t)))"))
    (available-modules (prelude))
    (hints ("num? -> '()"
            "var? -> (list (var-name e))"
            "sum? -> (unique (append-map free-vars (sum-terms e)))"
            "product? -> same pattern with product-factors"
            "difference? -> union of left and (if diff-right, right's vars, else '())"
            "power? -> (unique (append (free-vars (pow-base e)) (free-vars (pow-exp e))))"
            "app? -> (free-vars (app-arg e))"))
    (reference-solution "(define (free-vars e)
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
   [else '()]))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement subst
  ;; ──────────────────────────────────────────────
  ;; Substitute a value for a variable in an expression.

  (task
    (id "fp/expr/subst-impl")
    (module expr)
    (tier 0)
    (type implement)
    (description "Implement subst: substitute val for var-sym in an expression, using smart constructors to simplify the result. Multi-way cond dispatch: (1) num? -> return as-is. (2) var? -> if name matches var-sym, return val; else return expr unchanged. (3) sum? -> map subst over terms; if binary, use smart constructor sum for simplification, otherwise use make-sum. (4) product? -> same pattern with product/make-product. (5) difference? -> if binary, use difference smart constructor on both parts; if unary negation, use make-neg. (6) quotient? -> use division smart constructor. (7) power? -> use power smart constructor. (8) app? -> make-app with substituted argument. The key insight: using smart constructors means (subst (+ x 1) 'x (num 5)) yields (num 6) via constant folding, not (+ (num 5) (num 1)).")
    (context "Available: num?, var?, sum?, product?, difference?, quotient?, power?, app?, var-name, sum-terms, product-factors, diff-left, diff-right, quot-numer, quot-denom, pow-base, pow-exp, app-fn, app-arg, eq?, map, length, =, car, cadr, sum, product, difference, division, power, make-sum, make-product, make-neg, make-app, lambda, cond. Recursive substitution with smart constructors.")
    (before "(define (subst expr var-sym val)
  ;; TODO: substitute val for var-sym using smart constructors
  expr)")
    (test-file "lattice/fp/symbolic/test-expr.ss")
    (test-forms (";; Variable match -> replaced
(let ([e (subst (var 'x) 'x (num 5))])
  (assert-true (expr=? (num 5) e)))"
                 ";; Variable no match -> unchanged
(let ([e (subst (var 'y) 'x (num 5))])
  (assert-true (expr=? (var 'y) e)))"
                 ";; Numeric constants unchanged
(let ([e (subst (num 42) 'x (num 5))])
  (assert-true (expr=? (num 42) e)))"
                 ";; Sum with constant folding: x+1 [x:=5] -> 6
(let* ([orig (list '+ (var 'x) (num 1))]
       [result (subst orig 'x (num 5))])
  (assert-true (expr=? (num 6) result)))"
                 ";; Nested: x^2 [x:=(y+1)] -> (y+1)^2
(let* ([orig (make-pow (var 'x) (num 2))]
       [result (subst orig 'x (list '+ (var 'y) (num 1)))])
  (assert-true (power? result))
  (assert-true (sum? (pow-base result))))"))
    (available-modules (prelude))
    (hints ("num? -> expr (constants are inert)"
            "var? -> (if (eq? (var-name expr) var-sym) val expr)"
            "sum? -> map subst over terms, use smart sum for binary"
            "product? -> same pattern with product"
            "difference? -> check (diff-right expr) for binary vs unary"
            "power? -> (power (subst base) (subst exp)) — smart constructor simplifies"))
    (reference-solution "(define (subst expr var-sym val)
  (cond
   [(num? expr) expr]
   [(var? expr)
    (if (eq? (var-name expr) var-sym)
        val
        expr)]
   [(sum? expr)
    (let ([subst-terms (map (lambda (e) (subst e var-sym val)) (sum-terms expr))])
      (if (= (length subst-terms) 2)
          (sum (car subst-terms) (cadr subst-terms))
          (make-sum subst-terms)))]
   [(product? expr)
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
    (division (subst (quot-numer expr) var-sym val)
              (subst (quot-denom expr) var-sym val))]
   [(power? expr)
    (power (subst (pow-base expr) var-sym val)
           (subst (pow-exp expr) var-sym val))]
   [(app? expr)
    (make-app (app-fn expr) (subst (app-arg expr) var-sym val))]
   [else expr]))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Implement expr->string
  ;; ──────────────────────────────────────────────
  ;; Convert a symbolic expression to readable string.

  (task
    (id "fp/expr/expr-to-string-impl")
    (module expr)
    (tier 0)
    (type implement)
    (description "Implement expr->string: convert a symbolic expression to a human-readable infix string. Multi-way cond dispatch: (1) num? -> number->string. (2) var? -> symbol->string. (3) sum? -> join terms with ' + ' inside parens. (4) product? -> join factors with ' * ' inside parens. (5) difference? -> if binary, 'left - right' in parens; if unary negation, '-' prefix. (6) quotient? -> 'numer / denom' in parens. (7) power? -> 'base^exp' in parens. (8) app? -> 'fname(arg)' with no space. (9) else -> '?'. Use string-append for concatenation and string-join for infix operators. The recursive calls to expr->string on sub-expressions produce the nested string representation.")
    (context "Available: num?, var?, sum?, product?, difference?, quotient?, power?, app?, num-val, var-name, sum-terms, product-factors, diff-left, diff-right, quot-numer, quot-denom, pow-base, pow-exp, app-fn, app-arg, number->string, symbol->string, string-append, string-join, map, cond. Recursive string building with infix notation.")
    (before "(define (expr->string e)
  ;; TODO: convert expression to readable string
  \"?\")")
    (test-file "lattice/fp/symbolic/test-expr.ss")
    (test-forms (";; Numbers
(assert-equal \"42\" (expr->string (num 42)))"
                 ";; Variables
(assert-equal \"x\" (expr->string (var 'x)))"
                 ";; Sums produce infix with parens
(let ([s (expr->string (list '+ (var 'x) (num 1)))])
  (assert-true (string? s))
  (assert-true (> (string-length s) 0)))"
                 ";; Function application: fname(arg)
(assert-equal \"sin(x)\" (expr->string (sym-sin (var 'x))))"))
    (available-modules (prelude))
    (hints ("num? -> (number->string (num-val e))"
            "var? -> (symbol->string (var-name e))"
            "sum? -> wrap in parens, join terms with ' + '"
            "(string-append \"(\" (string-join (map expr->string (sum-terms e)) \" + \") \")\")"
            "app? -> (string-append (symbol->string (app-fn e)) \"(\" (expr->string (app-arg e)) \")\")"))
    (reference-solution "(define (expr->string e)
  (cond
   [(num? e) (number->string (num-val e))]
   [(var? e) (symbol->string (var-name e))]
   [(sum? e)
    (string-append \"(\"
                   (string-join (map expr->string (sum-terms e)) \" + \")
                   \")\")]
   [(product? e)
    (string-append \"(\"
                   (string-join (map expr->string (product-factors e)) \" * \")
                   \")\")]
   [(difference? e)
    (if (diff-right e)
        (string-append \"(\" (expr->string (diff-left e))
                       \" - \" (expr->string (diff-right e)) \")\")
        (string-append \"-\" (expr->string (diff-left e))))]
   [(quotient? e)
    (string-append \"(\" (expr->string (quot-numer e))
                   \" / \" (expr->string (quot-denom e)) \")\")]
   [(power? e)
    (string-append \"(\" (expr->string (pow-base e))
                   \"^\" (expr->string (pow-exp e)) \")\")]
   [(app? e)
    (string-append (symbol->string (app-fn e))
                   \"(\" (expr->string (app-arg e)) \")\")]
   [else \"?\"]))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

) ;; end tasks
