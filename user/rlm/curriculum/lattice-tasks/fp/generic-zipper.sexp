;;; generic-zipper.sexp — Development tasks for lattice/fp/data/generic-zipper.ss
;;;
;;; Module: generic-zipper (tier 0, depends on prelude, combinators)
;;; 446 lines, ~30 exported functions
;;; Derives zippers automatically from algebraic data type definitions
;;; using the calculus of types. Type DSL, symbolic differentiation,
;;; type substitution, simplification, generic zipper runtime operations.
;;;
;;; Git history:
;;;   82c1f26d feat(fp/data): Add generic zipper derivation via type calculus

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement type-sum
  ;; ──────────────────────────────────────────────
  ;; Sum type constructor with edge cases.

  (task
    (id "fp/generic-zipper/type-sum-impl")
    (module generic-zipper)
    (tier 0)
    (type implement)
    (description "Implement type-sum: construct a sum type (tagged union) from zero or more component types. Handle three cases with cond: no arguments returns type-zero (the empty type, represented as 0), one argument returns that argument directly (no wrapping), two or more arguments returns a list starting with '+ followed by the types. Uses rest-args (. types). This constructor canonicalizes away trivial sums.")
    (context "Available: null?, cdr, car, cons, cond, else. Rest-args with three-way cond.")
    (before "(define (type-sum . types)
  ;; TODO: sum type constructor
  0)")
    (test-file "lattice/fp/data/test-generic-zipper.ss")
    (test-forms (";; No args: empty type
(assert-equal 0 (type-sum))"
                 ";; Single arg: unwrap
(assert-equal '(var a) (type-sum (type-var 'a)))"
                 ";; Two args: tagged sum
(assert-equal '(+ (var a) (var b)) (type-sum (type-var 'a) (type-var 'b)))"
                 ";; Three args
(assert-equal '(+ (var a) (var b) (var c))
  (type-sum (type-var 'a) (type-var 'b) (type-var 'c)))"))
    (available-modules (prelude combinators))
    (hints ("Three cases based on the types list"
            "(null? types) -> type-zero (which is 0)"
            "(null? (cdr types)) -> (car types) (single element, no wrapping)"
            "else -> (cons '+ types)"))
    (reference-solution "(define (type-sum . types)
  (cond
    [(null? types) type-zero]
    [(null? (cdr types)) (car types)]
    [else (cons '+ types)]))")
    (alternatives ())
    (source-commit "82c1f26d")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement any
  ;; ──────────────────────────────────────────────
  ;; Existential predicate with short-circuit.

  (task
    (id "fp/generic-zipper/any-impl")
    (module generic-zipper)
    (tier 0)
    (type implement)
    (description "Implement any: check if any element of a list satisfies a predicate. Return #t as soon as the first matching element is found (short-circuit), or #f if the list is exhausted. Uses cond with three branches: empty list returns #f, predicate holds on car returns #t, else recurse on cdr. This is the existential quantifier over lists.")
    (context "Available: null?, car, cdr, cond, else. Three-branch cond with recursion.")
    (before "(define (any pred lst)
  ;; TODO: does any element satisfy pred?
  #f)")
    (test-file "lattice/fp/data/test-generic-zipper.ss")
    (test-forms (";; No match
(assert-false (any odd? '(2 4 6)))"
                 ";; Match in middle
(assert-true (any odd? '(2 3 6)))"
                 ";; Empty list
(assert-false (any odd? '()))"
                 ";; All match (short-circuits on first)
(assert-true (any odd? '(1 3 5)))"))
    (available-modules (prelude combinators))
    (hints ("Base case: (null? lst) -> #f"
            "Found: (pred (car lst)) -> #t"
            "Recurse: (any pred (cdr lst))"
            "Three-branch cond"))
    (reference-solution "(define (any pred lst)
  (cond
    [(null? lst) #f]
    [(pred (car lst)) #t]
    [else (any pred (cdr lst))]))")
    (alternatives ())
    (source-commit "82c1f26d")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement type-subst
  ;; ──────────────────────────────────────────────
  ;; Type variable substitution.

  (task
    (id "fp/generic-zipper/type-subst-impl")
    (module generic-zipper)
    (tier 0)
    (type implement)
    (description "Implement type-subst: substitute all occurrences of variable v with a replacement type in a type expression. Dispatch with cond on the type structure: type-zero and type-one return themselves (no variables). For type-var, if its name equals v, return the replacement; otherwise return t unchanged. For type-sum, map type-subst over all components via type-sum-components. For type-prod, same with type-prod-components. For type-rec (mu-type), if the rec-var equals v then the binding shadows it — return t unchanged; otherwise recurse into the body. Default: return t unchanged.")
    (context "Available: type-zero?, type-one?, type-var?, type-sum?, type-prod?, type-rec?, type-var-name, type-sum-components, type-prod-components, type-rec-var, type-rec-body, eq?, map, apply, type-sum, type-prod, type-rec, cond. Recursive dispatch on type structure.")
    (before "(define (type-subst t v replacement)
  ;; TODO: substitute v -> replacement in type t
  t)")
    (test-file "lattice/fp/data/test-generic-zipper.ss")
    (test-forms (";; Substitute variable
(assert-equal '(var b) (type-subst (type-var 'a) 'a (type-var 'b)))"
                 ";; Substitute in product
(assert-equal '(* (var b) (var c))
  (type-subst (type-prod (type-var 'a) (type-var 'c)) 'a (type-var 'b)))"
                 ";; Don't substitute shadowed variable (mu binding)
(assert-equal '(mu a (var a))
  (type-subst (type-rec 'a (type-var 'a)) 'a (type-var 'b)))"
                 ";; Substitute non-shadowed in rec body
(assert-equal '(mu x (var b))
  (type-subst (type-rec 'x (type-var 'a)) 'a (type-var 'b)))"))
    (available-modules (prelude combinators))
    (hints ("Dispatch on type structure with cond"
            "type-zero?, type-one? -> return as-is (no variables)"
            "type-var? -> (if (eq? (type-var-name t) v) replacement t)"
            "type-sum? -> (apply type-sum (map (lambda (c) (type-subst c v replacement)) (type-sum-components t)))"
            "type-prod? -> same pattern with type-prod-components"
            "type-rec? -> check if rec-var shadows v: if (eq? (type-rec-var t) v) return t, else recurse into body"))
    (reference-solution "(define (type-subst t v replacement)
  (cond
    [(type-zero? t) type-zero]
    [(type-one? t) type-one]
    [(type-var? t)
     (if (eq? (type-var-name t) v)
         replacement
         t)]
    [(type-sum? t)
     (apply type-sum
            (map (lambda (c) (type-subst c v replacement))
                 (type-sum-components t)))]
    [(type-prod? t)
     (apply type-prod
            (map (lambda (c) (type-subst c v replacement))
                 (type-prod-components t)))]
    [(type-rec? t)
     (let ([rec-var (type-rec-var t)]
            [body (type-rec-body t)])
       (if (eq? rec-var v)
           t
           (type-rec rec-var (type-subst body v replacement))))]
    [else t]))")
    (alternatives ())
    (source-commit "82c1f26d")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement type-simplify
  ;; ──────────────────────────────────────────────
  ;; Algebraic type simplification.

  (task
    (id "fp/generic-zipper/type-simplify-impl")
    (module generic-zipper)
    (tier 0)
    (type implement)
    (description "Implement type-simplify: simplify type expressions by eliminating identity elements. For sums: remove all 0 components (0 is the identity for +), then collapse: no components left -> 0, one component -> unwrap, else rebuild sum. For products: first check if any component is 0 (0 annihilates products -> return 0), then remove all 1 components (1 is the identity for ×), then collapse similarly. Recursively simplify all components first, then apply these rules. For type-rec, simplify the body. Atoms (0, 1, var) return unchanged.")
    (context "Available: type-zero?, type-one?, type-var?, type-sum?, type-prod?, type-rec?, type-sum-components, type-prod-components, type-rec-var, type-rec-body, map, filter, null?, cdr, car, apply, type-sum, type-prod, type-rec, not, or, any, cond. Recursive simplification with algebraic identities.")
    (before "(define (type-simplify t)
  ;; TODO: simplify type by eliminating 0s and 1s
  t)")
    (test-file "lattice/fp/data/test-generic-zipper.ss")
    (test-forms (";; 0 + a = a
(assert-equal '(var a) (type-simplify (type-sum 0 (type-var 'a))))"
                 ";; 0 × a = 0
(assert-equal 0 (type-simplify (type-prod 0 (type-var 'a))))"
                 ";; 1 × a = a
(assert-equal '(var a) (type-simplify (type-prod 1 (type-var 'a))))"
                 ";; Nested: 0 + (1 × a) = a
(assert-equal '(var a) (type-simplify (type-sum 0 (type-prod 1 (type-var 'a)))))"))
    (available-modules (prelude combinators))
    (hints ("First recursively simplify all sub-components"
            "For sums: filter out type-zero? components, then collapse"
            "For products: check if any component is zero (annihilation), then filter out type-one? components"
            "Collapse: null? -> identity, single -> unwrap, else -> rebuild"
            "any is defined in this module for checking zero in products"))
    (reference-solution "(define (type-simplify t)
  (cond
    [(or (type-zero? t) (type-one? t)) t]
    [(type-var? t) t]
    [(type-sum? t)
     (let* ([simplified (map type-simplify (type-sum-components t))]
            [non-zero (filter (lambda (c) (not (type-zero? c))) simplified)])
       (cond
         [(null? non-zero) type-zero]
         [(null? (cdr non-zero)) (car non-zero)]
         [else (apply type-sum non-zero)]))]
    [(type-prod? t)
     (let* ([simplified (map type-simplify (type-prod-components t))]
            [has-zero (any type-zero? simplified)]
            [non-one (filter (lambda (c) (not (type-one? c))) simplified)])
       (cond
         [has-zero type-zero]
         [(null? non-one) type-one]
         [(null? (cdr non-one)) (car non-one)]
         [else (apply type-prod non-one)]))]
    [(type-rec? t)
     (type-rec (type-rec-var t)
               (type-simplify (type-rec-body t)))]
    [else t]))")
    (alternatives ())
    (source-commit "82c1f26d")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Implement type->string
  ;; ──────────────────────────────────────────────
  ;; Pretty-print a type expression.

  (task
    (id "fp/generic-zipper/type-to-string-impl")
    (module generic-zipper)
    (tier 0)
    (type implement)
    (description "Implement type->string: convert a type expression to a human-readable string. Dispatch on type structure: type-zero -> \"0\", type-one -> \"1\", type-var -> the variable name as a string. For sums: recursively convert each component, join with \" + \", wrap in parentheses. For products: same but join with \" x \" (multiplication sign). For type-rec: \"mu\" followed by the variable name, \".\", and the body string. Use string-join (from prelude) to join lists of strings with a separator. Default: \"?\".")
    (context "Available: type-zero?, type-one?, type-var?, type-sum?, type-prod?, type-rec?, type-var-name, type-sum-components, type-prod-components, type-rec-var, type-rec-body, symbol->string, string-append, map, string-join, cond. Recursive string construction.")
    (before "(define (type->string t)
  ;; TODO: pretty-print type expression
  \"?\")")
    (test-file "lattice/fp/data/test-generic-zipper.ss")
    (test-forms (";; Atoms
(assert-equal \"0\" (type->string 0))
(assert-equal \"1\" (type->string 1))"
                 ";; Variable
(assert-equal \"a\" (type->string (type-var 'a)))"
                 ";; Sum
(assert-equal \"(a + b)\" (type->string (type-sum (type-var 'a) (type-var 'b))))"
                 ";; Product
(let ([s (type->string (type-prod (type-var 'x) (type-var 'y)))])
  (assert-true (string? s)))"))
    (available-modules (prelude combinators))
    (hints ("Dispatch with cond on type predicates"
            "type-zero? -> \"0\", type-one? -> \"1\""
            "type-var? -> (symbol->string (type-var-name t))"
            "type-sum? -> map type->string over components, string-join with \" + \", wrap in parens"
            "type-prod? -> same with appropriate separator"
            "type-rec? -> (string-append \"mu\" (symbol->string (type-rec-var t)) \".\" (type->string (type-rec-body t)))"))
    (reference-solution "(define (type->string t)
  (cond
    [(type-zero? t) \"0\"]
    [(type-one? t) \"1\"]
    [(type-var? t) (symbol->string (type-var-name t))]
    [(type-sum? t)
     (let ([parts (map type->string (type-sum-components t))])
       (string-append \"(\" (string-join parts \" + \") \")\"))]
    [(type-prod? t)
     (let ([parts (map type->string (type-prod-components t))])
       (string-append \"(\" (string-join parts \" x \") \")\"))]
    [(type-rec? t)
     (string-append \"mu\" (symbol->string (type-rec-var t))
                    \".\" (type->string (type-rec-body t)))]
    [else \"?\"]))")
    (alternatives ())
    (source-commit "82c1f26d")
    (difficulty 1))

) ;; end tasks
