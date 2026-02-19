;;; cost-analysis.sexp — Development tasks for lattice/fp/analysis/cost-analysis.ss
;;;
;;; Module: cost-analysis (tier 0, depends on prelude, sort)
;;; 678 lines, ~25 exported functions
;;; Cost estimation for parallelization heuristics. Walks S-expression
;;; ASTs to estimate computational work, classify complexity, detect
;;; hotspots, and decide if parallelization is beneficial. Cost tables
;;; for primitives, HOFs, and expression forms.
;;;
;;; Git history:
;;;   a3abed54 refactor: Reorganize into core/lattice/shell three-layer architecture
;;;   71b6b156 feat(module): migrate lattice/fp/ leaves to require
;;;   515ea003 docs(lattice/fp): Convert analysis to doc forms

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement operation-cost
  ;; ──────────────────────────────────────────────
  ;; Look up the cost of a primitive or HOF operation.

  (task
    (id "fp/cost-analysis/operation-cost-impl")
    (module cost-analysis)
    (tier 0)
    (type implement)
    (description "Implement operation-cost: look up the cost of a named operation from two cost tables. First check *primitive-costs* (an alist mapping symbols like +, car, sqrt to integer costs), then check *hof-costs* (map, filter, fold-left, etc.). If found in either, return the cost (cdr of the alist entry). If not found in either, return 10 as a conservative default for unknown operations. Use assq for fast symbol lookup in alists. The cost tables use abstract work units: 1 = trivial (variable lookup), 2-5 = cheap (arithmetic), 10-20 = moderate (function call), 50+ = expensive (transcendentals).")
    (context "Available: assq, cdr, cond, *primitive-costs*, *hof-costs*. Two alist lookups with fallback.")
    (before "(define (operation-cost op)
  ;; TODO: look up cost in primitive and HOF tables
  0)")
    (test-file "lattice/fp/analysis/test-cost-analysis.ss")
    (test-forms (";; Primitive costs are positive
(assert-true (> (operation-cost '+) 0))
(assert-true (> (operation-cost 'car) 0))
(assert-true (> (operation-cost 'cons) 0))"
                 ";; Arithmetic costs are ordered: division > addition
(assert-true (> (operation-cost '/) (operation-cost '+)))"
                 ";; Transcendentals cost more than basic ops
(assert-true (> (operation-cost 'sqrt) (operation-cost '*)))"
                 ";; HOF costs are substantial
(assert-true (>= (operation-cost 'map) 10))
(assert-true (>= (operation-cost 'fold-left) 10))"
                 ";; Unknown operations get default cost
(let ([cost (operation-cost 'totally-made-up-function)])
  (assert-true (> cost 0))
  (assert-true (<= cost 20)))"))
    (available-modules (prelude sort))
    (hints ("(let ([prim-entry (assq op *primitive-costs*)] [hof-entry (assq op *hof-costs*)]) ...)"
            "If prim-entry is truthy, return (cdr prim-entry)"
            "If hof-entry is truthy, return (cdr hof-entry)"
            "Else return 10 (conservative default)"))
    (reference-solution "(define (operation-cost op)
  (let ([prim-entry (assq op *primitive-costs*)]
        [hof-entry (assq op *hof-costs*)])
    (cond
     [prim-entry (cdr prim-entry)]
     [hof-entry (cdr hof-entry)]
     [else 10])))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement estimate-list-size
  ;; ──────────────────────────────────────────────
  ;; Heuristically estimate the size of a list expression.

  (task
    (id "fp/cost-analysis/estimate-list-size-impl")
    (module cost-analysis)
    (tier 0)
    (type implement)
    (description "Implement estimate-list-size: heuristically estimate how many elements a list expression will produce, based on its syntactic form. Multi-way cond dispatch: (1) null? -> 0. (2) quoted list (quote ...) -> count elements with length. (3) literal (list ...) -> count arguments (length of cdr). (4) (iota n) where n is a number -> return n directly. (5) (range a b) where both are numbers -> (max 0 (- b a)). (6) (map f ...) or (filter p ...) -> recurse on the source list (third element). (7) else -> 10 (conservative default). This is used by estimate-hof-work to determine iteration count: (map f (iota 100)) has 100 iterations, while (map f xs) uses the default 10.")
    (context "Available: null?, pair?, eq?, car, cdr, cadr, caddr, cddr, length, number?, list?, max, -, memq, cond, and. Pattern matching on expression heads.")
    (before "(define (estimate-list-size expr)
  ;; TODO: heuristic list size from expression form
  10)")
    (test-file "lattice/fp/analysis/test-cost-analysis.ss")
    (test-forms (";; Null is empty
(assert-equal 0 (estimate-list-size '()))"
                 ";; Quoted list: count elements
(assert-equal 3 (estimate-list-size '(quote (1 2 3))))"
                 ";; Literal list: count arguments
(assert-equal 4 (estimate-list-size '(list a b c d)))"
                 ";; iota: size equals argument
(assert-equal 100 (estimate-list-size '(iota 100)))"
                 ";; Unknown expression: conservative default
(assert-equal 10 (estimate-list-size 'xs))"))
    (available-modules (prelude sort))
    (hints ("Check (null? expr) first -> 0"
            "Check for (quote ...) where quoted is a list -> (length (cadr expr))"
            "Check for (list ...) -> (length (cdr expr))"
            "Check for (iota n) where n is a number -> (cadr expr)"
            "Check for (map f source) or (filter p source) -> recurse on source"
            "Default: 10"))
    (reference-solution "(define (estimate-list-size expr)
  (cond
   [(null? expr) 0]
   [(and (pair? expr) (eq? (car expr) 'quote) (pair? (cdr expr)))
    (let ([quoted (cadr expr)])
      (if (list? quoted) (length quoted) 10))]
   [(and (pair? expr) (eq? (car expr) 'list))
    (length (cdr expr))]
   [(and (pair? expr) (eq? (car expr) 'iota)
         (pair? (cdr expr)) (number? (cadr expr)))
    (cadr expr)]
   [(and (pair? expr) (eq? (car expr) 'range)
         (pair? (cdr expr)) (number? (cadr expr))
         (pair? (cddr expr)) (number? (caddr expr)))
    (max 0 (- (caddr expr) (cadr expr)))]
   [(and (pair? expr) (memq (car expr) '(map filter)))
    (if (and (pair? (cdr expr)) (pair? (cddr expr)))
        (estimate-list-size (caddr expr))
        10)]
   [else 10]))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement expr-complexity-class
  ;; ──────────────────────────────────────────────
  ;; Classify an expression's asymptotic complexity.

  (task
    (id "fp/cost-analysis/complexity-class-impl")
    (module cost-analysis)
    (tier 0)
    (type implement)
    (description "Implement expr-complexity-class: classify an S-expression's asymptotic complexity as one of: constant, linear, quadratic, recursive, unknown. Delegates to classify-complexity with a fuel budget of 100. The classification is structural — it looks at the expression's form, not its runtime behavior. Atoms and simple arithmetic are constant. HOFs like map/filter/fold are linear. Nested HOFs (a HOF whose argument list contains another HOF) are quadratic. letrec/fix forms are recursive. For let forms, take the maximum complexity of bindings and body using combine-complexity. Lambda bodies are classified recursively. This is a one-line wrapper that provides the default fuel.")
    (context "Available: classify-complexity (the recursive workhorse, already implemented). Simple delegation with default fuel.")
    (before "(define (expr-complexity-class expr)
  ;; TODO: classify expression complexity
  'unknown)")
    (test-file "lattice/fp/analysis/test-cost-analysis.ss")
    (test-forms (";; Atoms are constant
(assert-equal 'constant (expr-complexity-class 42))
(assert-equal 'constant (expr-complexity-class 'x))
(assert-equal 'constant (expr-complexity-class '(+ 1 2)))"
                 ";; map is linear
(assert-equal 'linear (expr-complexity-class '(map f xs)))"
                 ";; filter is linear
(assert-equal 'linear (expr-complexity-class '(filter p xs)))"
                 ";; fold is linear
(assert-equal 'linear (expr-complexity-class '(fold-left + 0 xs)))"
                 ";; letrec implies recursion
(assert-equal 'recursive
  (expr-complexity-class '(letrec ([f (lambda (x) (f x))]) (f 1))))"))
    (available-modules (prelude sort))
    (hints ("This is a one-line function: (classify-complexity expr 100)"
            "The fuel parameter bounds recursion depth for safety"
            "classify-complexity does all the actual work"))
    (reference-solution "(define (expr-complexity-class expr)
  (classify-complexity expr 100))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement parallel-beneficial?
  ;; ──────────────────────────────────────────────
  ;; Decide if parallelizing two expressions is worthwhile.

  (task
    (id "fp/cost-analysis/parallel-beneficial-impl")
    (module cost-analysis)
    (tier 0)
    (type implement)
    (description "Implement parallel-beneficial?: determine if running two expressions in parallel is worthwhile. Estimate the work for each expression using estimate-work, then check three conditions (all must hold): (1) Combined work (w1 + w2) exceeds *min-parallel-work* threshold. (2) w1 exceeds half the parallel overhead — prevents one-sided parallelism where one task is trivial. (3) w2 exceeds half the parallel overhead — same check. Use (quotient overhead 2) for integer half. This embodies the principle that parallelization only helps when both tasks have substantial work and the total justifies the spawning/synchronization overhead.")
    (context "Available: estimate-work, +, >, quotient, and, let*, *parallel-overhead*, *min-parallel-work*. Work estimation + threshold comparison.")
    (before "(define (parallel-beneficial? e1 e2)
  ;; TODO: is parallelizing e1 and e2 worthwhile?
  #f)")
    (test-file "lattice/fp/analysis/test-cost-analysis.ss")
    (test-forms (";; Trivial expressions: not worth parallelizing
(assert-false (parallel-beneficial? '(+ 1 2) '(+ 3 4)))"
                 ";; Substantial work on both sides: worthwhile
(let ([big '(fold-left + 0 (map (lambda (x) (* x x)) (iota 1000)))])
  (assert-true (parallel-beneficial? big big)))"
                 ";; Unbalanced: one trivial + one big -> not beneficial
(assert-false (parallel-beneficial? '(+ 1 2)
  '(fold-left + 0 (map sqrt (iota 100)))))"))
    (available-modules (prelude sort))
    (hints ("Estimate: (let* ([w1 (estimate-work e1)] [w2 (estimate-work e2)] ...)"
            "Combined: (+ w1 w2)"
            "Three conditions with and:"
            "(> combined threshold) — enough total work"
            "(> w1 (quotient overhead 2)) — first task substantial"
            "(> w2 (quotient overhead 2)) — second task substantial"))
    (reference-solution "(define (parallel-beneficial? e1 e2)
  (let* ([w1 (estimate-work e1)]
         [w2 (estimate-work e2)]
         [combined (+ w1 w2)]
         [overhead *parallel-overhead*]
         [threshold *min-parallel-work*])
    (and (> combined threshold)
         (> w1 (quotient overhead 2))
         (> w2 (quotient overhead 2)))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Implement work-analysis
  ;; ──────────────────────────────────────────────
  ;; Generate a detailed work analysis report.

  (task
    (id "fp/cost-analysis/work-analysis-impl")
    (module cost-analysis)
    (tier 0)
    (type implement)
    (description "Implement work-analysis: generate a structured report about an expression's computational cost. Compute four metrics: (1) total cost via estimate-work. (2) complexity class via expr-complexity-class. (3) hotspots via find-hotspots with n=3 (top 3 most expensive sub-expressions). (4) parallelizable? — true if total exceeds *min-parallel-work*. Return a quasiquoted alist tagged with 'work-analysis, containing five fields: total-cost, complexity-class, hotspots, parallelizable?, and suggested-splits (if parallelizable, min of 4 and total/threshold; else 1). The report structure is accessed by companion functions analysis-total-cost, analysis-complexity-class, etc.")
    (context "Available: estimate-work, expr-complexity-class, find-hotspots, >, /, min, quasiquote, unquote, let*, if, *min-parallel-work*. Compute metrics + assemble into tagged alist.")
    (before "(define (work-analysis expr)
  ;; TODO: generate work analysis report
  '(work-analysis))")
    (test-file "lattice/fp/analysis/test-cost-analysis.ss")
    (test-forms (";; Result is a work-analysis structure
(let ([wa (work-analysis '(+ 1 2))])
  (assert-true (work-analysis? wa))
  (assert-true (pair? (assq 'total-cost (cdr wa))))
  (assert-true (pair? (assq 'complexity-class (cdr wa))))
  (assert-true (pair? (assq 'parallelizable? (cdr wa)))))"
                 ";; Accessors return correct types
(let ([wa (work-analysis '(map (lambda (x) x) xs))])
  (assert-true (number? (analysis-total-cost wa)))
  (assert-true (symbol? (analysis-complexity-class wa)))
  (assert-true (boolean? (analysis-parallelizable? wa)))
  (assert-true (number? (analysis-suggested-splits wa))))"
                 ";; Hotspots are a list
(let* ([wa (work-analysis '(begin (sqrt 4) (map f xs) (+ 1 2)))]
       [hotspots (analysis-hotspots wa)])
  (assert-true (list? hotspots)))"
                 ";; Empty expression produces valid analysis
(let ([wa (work-analysis '())])
  (assert-true (work-analysis? wa))
  (assert-true (number? (analysis-total-cost wa))))"))
    (available-modules (prelude sort))
    (hints ("Compute: total via estimate-work, complexity via expr-complexity-class"
            "Hotspots: (find-hotspots expr 3)"
            "Parallelizable: (> total *min-parallel-work*)"
            "Suggested splits: (if parallelizable (min 4 (quotient total *min-parallel-work*)) 1)"
            "Return quasiquoted alist: `(work-analysis (total-cost . ,total) ...)"))
    (reference-solution "(define (work-analysis expr)
  (let* ([total (estimate-work expr)]
         [complexity (expr-complexity-class expr)]
         [hotspots (find-hotspots expr 3)]
         [parallelizable (> total *min-parallel-work*)])
    `(work-analysis
      (total-cost . ,total)
      (complexity-class . ,complexity)
      (hotspots . ,hotspots)
      (parallelizable? . ,parallelizable)
      (suggested-splits . ,(if parallelizable
                               (min 4 (quotient total *min-parallel-work*))
                               1)))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

) ;; end tasks
