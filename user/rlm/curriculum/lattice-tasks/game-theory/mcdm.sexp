;;; mcdm.sexp — Development tasks for lattice/game-theory/mcdm.ss
;;;
;;; Module: mcdm (tier 0, depends on voting)
;;; 481 lines, ~25 exported functions
;;; Multi-criteria decision making via social choice theory: treats evaluation
;;; criteria as "voters" ranking alternatives, then aggregates using voting rules.
;;; Pareto analysis, sensitivity/robustness, cycle detection, method agreement.
;;;
;;; Git history:
;;;   e68b9849 feat(module): migrate game-theory to require
;;;   c6dffccb refactor(lattice): reorganize skills, consolidate graph modules

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement dp-score
  ;; ──────────────────────────────────────────────
  ;; Look up score for an alternative on a criterion.

  (task
    (id "game-theory/mcdm/dp-score-impl")
    (module mcdm)
    (tier 0)
    (type implement)
    (description "Implement dp-score: look up the numeric score for a specific alternative on a specific criterion in a decision problem. The scores are stored as a list of triples (alternative criterion value). Search through the scores list for a matching (alt, crit) pair using find-score (a helper that walks the list checking car and cadar). If found, return the value (caddr of the triple). If not found, return 0 as the default.")
    (context "Available: dp-scores, find-score, caddr, if, let. One lookup + default.")
    (before "(define (dp-score dp alt crit)
  ;; TODO: look up score, default to 0
  0)")
    (test-file "lattice/game-theory/test-mcdm.ss")
    (test-forms (";; Found score
(let ([dp (make-decision-problem '(a b) '(x) '((a x 10) (b x 5)))])
  (assert-equal 10 (dp-score dp 'a 'x)))"
                 ";; Different alternative
(let ([dp (make-decision-problem '(a b) '(x) '((a x 10) (b x 5)))])
  (assert-equal 5 (dp-score dp 'b 'x)))"
                 ";; Missing score defaults to 0
(let ([dp (make-decision-problem '(a b) '(x) '((a x 10)))])
  (assert-equal 0 (dp-score dp 'a 'missing)))"))
    (available-modules (voting))
    (hints ("Get scores list: (dp-scores dp)"
            "Search: (let ([found (find-score (dp-scores dp) alt crit)]) ...)"
            "If found: (caddr found) extracts the value from the triple"
            "If not found (#f): return 0"))
    (reference-solution "(define (dp-score dp alt crit)
  (let ([found (find-score (dp-scores dp) alt crit)])
    (if found (caddr found) 0)))")
    (alternatives ())
    (source-commit "c6dffccb")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement criterion-ranking
  ;; ──────────────────────────────────────────────
  ;; Rank alternatives by a single criterion.

  (task
    (id "game-theory/mcdm/criterion-ranking-impl")
    (module mcdm)
    (tier 0)
    (type implement)
    (description "Implement criterion-ranking: rank all alternatives by their score on a single criterion, highest score first. This converts a numeric evaluation into a preference ordering — the key bridge between optimization and social choice. Get the alternatives list from the decision problem, then sort using sort-by with a comparator that checks dp-score. Higher scores should come first (use >).")
    (context "Available: dp-alternatives, dp-score, sort-by, >, lambda, let. One sort call.")
    (before "(define (criterion-ranking dp criterion)
  ;; TODO: rank alternatives by criterion score (best first)
  (dp-alternatives dp))")
    (test-file "lattice/game-theory/test-mcdm.ss")
    (test-forms (";; Sort by speed: b(200) > c(150) > a(100)
(let ([dp (make-decision-problem '(a b c) '(speed) '((a speed 100) (b speed 200) (c speed 150)))])
  (assert-equal '(b c a) (criterion-ranking dp 'speed)))"
                 ";; Single criterion with clear ordering
(let ([dp (make-decision-problem '(x y z) '(score) '((x score 1) (y score 3) (z score 2)))])
  (assert-equal '(y z x) (criterion-ranking dp 'score)))"))
    (available-modules (voting))
    (hints ("Get alternatives: (let ([alts (dp-alternatives dp)]) ...)"
            "Sort descending by score: (sort-by comparator alts)"
            "Comparator: (lambda (a b) (> (dp-score dp a criterion) (dp-score dp b criterion)))"))
    (reference-solution "(define (criterion-ranking dp criterion)
  (let ([alts (dp-alternatives dp)])
    (sort-by (lambda (a b)
                (> (dp-score dp a criterion)
                   (dp-score dp b criterion)))
              alts)))")
    (alternatives ())
    (source-commit "c6dffccb")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement dominates?
  ;; ──────────────────────────────────────────────
  ;; Pareto dominance check.

  (task
    (id "game-theory/mcdm/dominates-impl")
    (module mcdm)
    (tier 0)
    (type implement)
    (description "Implement dominates?: check if alternative a Pareto-dominates alternative b. Pareto dominance means a is at least as good as b on ALL criteria (>= on every score) AND strictly better on at least one criterion (> on at least one score). Use for-all? to check the first condition and any? to check the second. Both predicates iterate over the criteria list, comparing dp-score values. This is O(k) where k is the number of criteria.")
    (context "Available: dp-criteria, dp-score, >=, >, for-all?, any?, and, lambda, let. Two predicate checks combined with and.")
    (before "(define (dominates? dp a b)
  ;; TODO: does a Pareto-dominate b?
  #f)")
    (test-file "lattice/game-theory/test-mcdm.ss")
    (test-forms (";; Clear dominance: a beats b on all criteria
(let ([dp (make-decision-problem '(a b) '(x y) '((a x 10) (a y 10) (b x 5) (b y 5)))])
  (assert-true (dominates? dp 'a 'b))
  (assert-false (dominates? dp 'b 'a)))"
                 ";; Incomparable: each wins on one criterion
(let ([dp (make-decision-problem '(a b) '(x y) '((a x 10) (a y 5) (b x 5) (b y 10)))])
  (assert-false (dominates? dp 'a 'b))
  (assert-false (dominates? dp 'b 'a)))"
                 ";; Equal scores: not domination (needs strict on at least one)
(let ([dp (make-decision-problem '(a b) '(x y) '((a x 5) (a y 5) (b x 5) (b y 5)))])
  (assert-false (dominates? dp 'a 'b)))"))
    (available-modules (voting))
    (hints ("Get criteria: (let ([criteria (dp-criteria dp)]) ...)"
            "All at least as good: (for-all? (lambda (c) (>= (dp-score dp a c) (dp-score dp b c))) criteria)"
            "Strictly better on one: (any? (lambda (c) (> (dp-score dp a c) (dp-score dp b c))) criteria)"
            "Both must hold: (and condition1 condition2)"))
    (reference-solution "(define (dominates? dp a b)
  (let ([criteria (dp-criteria dp)])
    (and (for-all? (lambda (c)
                     (>= (dp-score dp a c) (dp-score dp b c)))
                   criteria)
         (any? (lambda (c)
                 (> (dp-score dp a c) (dp-score dp b c)))
               criteria))))")
    (alternatives ())
    (source-commit "c6dffccb")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement criterion-index
  ;; ──────────────────────────────────────────────
  ;; Find the 0-indexed position of a criterion.

  (task
    (id "game-theory/mcdm/criterion-index-impl")
    (module mcdm)
    (tier 0)
    (type implement)
    (description "Implement criterion-index: find the 0-indexed position of a criterion symbol in a list of criteria. Return #f if the criterion is not found. Use a named let loop with an index counter: walk the list, comparing each element with eq?. If found, return the current index. If the list is exhausted (null?), return #f. This is used by sensitivity analysis to locate which weight to perturb.")
    (context "Available: null?, car, cdr, eq?, +, let, cond. Named let with index counter.")
    (before "(define (criterion-index c criteria)
  ;; TODO: 0-indexed position of c in criteria, or #f
  #f)")
    (test-file "lattice/game-theory/test-mcdm.ss")
    (test-forms (";; First element
(assert-equal 0 (criterion-index 'x '(x y z)))"
                 ";; Middle element
(assert-equal 1 (criterion-index 'y '(x y z)))"
                 ";; Last element
(assert-equal 2 (criterion-index 'z '(x y z)))"
                 ";; Not found
(assert-false (criterion-index 'w '(x y z)))"))
    (available-modules (voting))
    (hints ("Named let: (let loop ([cs criteria] [i 0]) ...)"
            "Base: (null? cs) -> #f"
            "Match: (eq? (car cs) c) -> i"
            "Recurse: (loop (cdr cs) (+ i 1))"))
    (reference-solution "(define (criterion-index c criteria)
  (let loop ([cs criteria] [i 0])
    (cond
      [(null? cs) #f]
      [(eq? (car cs) c) i]
      [else (loop (cdr cs) (+ i 1))])))")
    (alternatives ())
    (source-commit "c6dffccb")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Implement list-update
  ;; ──────────────────────────────────────────────
  ;; Functional update of a list element by index.

  (task
    (id "game-theory/mcdm/list-update-impl")
    (module mcdm)
    (tier 0)
    (type implement)
    (description "Implement list-update: return a new list with the element at index idx replaced by val, leaving all other elements unchanged. This is a pure functional update — the original list is not modified. If idx is 0, cons val onto the cdr of the list. Otherwise, cons the car onto the result of recursively updating the cdr with idx decremented. This is used in sensitivity analysis to perturb individual criterion weights.")
    (context "Available: =, -, cons, car, cdr, if. Simple recursion on index.")
    (before "(define (list-update lst idx val)
  ;; TODO: replace element at idx with val
  lst)")
    (test-file "lattice/game-theory/test-mcdm.ss")
    (test-forms (";; Update first element
(assert-equal '(99 20 30) (list-update '(10 20 30) 0 99))"
                 ";; Update middle element
(assert-equal '(10 99 30) (list-update '(10 20 30) 1 99))"
                 ";; Update last element
(assert-equal '(10 20 99) (list-update '(10 20 30) 2 99))"
                 ";; Single-element list
(assert-equal '(42) (list-update '(0) 0 42))"))
    (available-modules (voting))
    (hints ("Base: idx = 0 -> (cons val (cdr lst))"
            "Recurse: (cons (car lst) (list-update (cdr lst) (- idx 1) val))"
            "This is pure: original list is unchanged"))
    (reference-solution "(define (list-update lst idx val)
  (if (= idx 0)
      (cons val (cdr lst))
      (cons (car lst) (list-update (cdr lst) (- idx 1) val))))")
    (alternatives ())
    (source-commit "c6dffccb")
    (difficulty 1))

) ;; end tasks
