;;; affine.sexp — Development tasks for lattice/numeric/affine.ss
;;;
;;; Module: affine (tier 0, depends on prelude, sort, interval)
;;; 634 lines, ~25 exported functions
;;; Affine arithmetic: correlation-tracking alternative to interval
;;; arithmetic. Affine forms represent quantities as x₀ + Σxᵢεᵢ where
;;; noise symbols track correlation. Solves the dependency problem:
;;; x - x = 0 exactly (not [-width, width] like intervals).
;;;
;;; Git history:
;;;   a3abed54 refactor: Reorganize into core/lattice/shell three-layer architecture
;;;   710fc341 feat(module): migrate lattice/numeric/ to require

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement affine->interval
  ;; ──────────────────────────────────────────────
  ;; Convert affine form to bounding interval.

  (task
    (id "numeric/affine/affine-to-interval-impl")
    (module affine)
    (tier 0)
    (type implement)
    (description "Implement affine->interval: convert an affine form to its bounding interval. An affine form x̂ = x₀ + x₁ε₁ + x₂ε₂ + ... has each εᵢ ∈ [-1,1], so the range of x̂ is [x₀ - Σ|xᵢ|, x₀ + Σ|xᵢ|]. Extract center x₀ via affine-center and terms via affine-terms. Compute total-deviation = sum of absolute values of all coefficients using fold-left + 0 over (map abs ...). Return (make-interval (- x₀ total) (+ x₀ total)).")
    (context "Available: affine-center, affine-terms, make-interval, fold-left, map, abs, cdr, +, -, let. One fold over terms + one make-interval.")
    (before "(define (affine->interval af)
  ;; TODO: [x0 - sum|xi|, x0 + sum|xi|]
  (make-interval 0 0))")
    (test-file "lattice/numeric/test-affine.ss")
    (test-forms (";; Constant affine: interval is a point
(let ([af (affine-constant 5)])
  (let ([iv (affine->interval af)])
    (assert-equal 5 (interval-lo iv))
    (assert-equal 5 (interval-hi iv))))"
                 ";; Affine with terms: x0=5, terms=((0 . 2) (1 . 3)) → [5-5, 5+5] = [0, 10]
(let ([af (make-affine 5 '((0 . 2) (1 . 3)))])
  (let ([iv (affine->interval af)])
    (assert-equal 0 (interval-lo iv))
    (assert-equal 10 (interval-hi iv))))"
                 ";; Negative coefficients: absolute values used
(let ([af (make-affine 0 '((0 . -3) (1 . 4)))])
  (let ([iv (affine->interval af)])
    (assert-equal -7 (interval-lo iv))
    (assert-equal 7 (interval-hi iv))))"))
    (available-modules (prelude sort interval))
    (hints ("Get center: (let ([x0 (affine-center af)] [terms (affine-terms af)]) ...)"
            "Sum absolute coefficients: (fold-left + 0 (map (lambda (t) (abs (cdr t))) terms))"
            "Return: (make-interval (- x0 total-deviation) (+ x0 total-deviation))"))
    (reference-solution "(define (affine->interval af)
  (let ([x0 (affine-center af)]
        [terms (affine-terms af)])
    (let ([total-deviation (fold-left + 0 (map (lambda (t) (abs (cdr t))) terms))])
      (make-interval (- x0 total-deviation)
                     (+ x0 total-deviation)))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement affine-neg
  ;; ──────────────────────────────────────────────
  ;; Negate: flip center and all coefficients.

  (task
    (id "numeric/affine/affine-neg-impl")
    (module affine)
    (tier 0)
    (type implement)
    (description "Implement affine-neg: negate an affine form. Negating x̂ = x₀ + Σxᵢεᵢ gives -x̂ = -x₀ + Σ(-xᵢ)εᵢ. Negate the center value and negate every coefficient in the terms list. Use map over the terms, where each term is a (noise-id . coefficient) pair — keep the car (noise-id), negate the cdr (coefficient). Return via make-affine.")
    (context "Available: affine-center, affine-terms, make-affine, -, map, cons, car, cdr, lambda. One negate + one map.")
    (before "(define (affine-neg af)
  ;; TODO: negate center and all coefficients
  af)")
    (test-file "lattice/numeric/test-affine.ss")
    (test-forms (";; Negate constant
(let ([af (affine-neg (affine-constant 3))])
  (assert-equal -3 (affine-center af)))"
                 ";; Negate with terms: center and all coefficients flip
(let ([af (affine-neg (make-affine 3 '((0 . 1) (1 . -2))))])
  (assert-equal -3 (affine-center af))
  (assert-equal '((0 . -1) (1 . 2)) (affine-terms af)))"
                 ";; Double negation is identity
(let* ([original (make-affine 5 '((0 . 3) (1 . -7)))]
       [doubled (affine-neg (affine-neg original))])
  (assert-equal (affine-center original) (affine-center doubled))
  (assert-equal (affine-terms original) (affine-terms doubled)))"))
    (available-modules (prelude sort interval))
    (hints ("Negate center: (- (affine-center af))"
            "Negate terms: (map (lambda (t) (cons (car t) (- (cdr t)))) (affine-terms af))"
            "Combine: (make-affine negated-center negated-terms)"))
    (reference-solution "(define (affine-neg af)
  (make-affine (- (affine-center af))
               (map (lambda (t) (cons (car t) (- (cdr t))))
                    (affine-terms af))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement affine-scale
  ;; ──────────────────────────────────────────────
  ;; Scalar multiplication: exact for affine operations.

  (task
    (id "numeric/affine/affine-scale-impl")
    (module affine)
    (tier 0)
    (type implement)
    (description "Implement affine-scale: multiply an affine form by a scalar k. For k*(x₀ + Σxᵢεᵢ) = k*x₀ + Σ(k*xᵢ)εᵢ. This operation is exact (no new noise introduced) because scalar multiplication is an affine function. Multiply the center by k and multiply each coefficient in the terms by k. Use map over the terms, keeping noise-ids and scaling coefficients.")
    (context "Available: affine-center, affine-terms, make-affine, *, map, cons, car, cdr, lambda. One multiply + one map.")
    (before "(define (affine-scale af k)
  ;; TODO: k * x0 + sum(k * xi * ei)
  af)")
    (test-file "lattice/numeric/test-affine.ss")
    (test-forms (";; Scale by 2
(let ([af (affine-scale (make-affine 3 '((0 . 1) (1 . -2))) 2)])
  (assert-equal 6 (affine-center af))
  (assert-equal '((0 . 2) (1 . -4)) (affine-terms af)))"
                 ";; Scale by 0: everything becomes zero (constant 0)
(let ([af (affine-scale (make-affine 5 '((0 . 3))) 0)])
  (assert-equal 0 (affine-center af))
  (assert-equal '() (affine-terms af)))"
                 ";; Scale by -1: same as negate
(let* ([original (make-affine 3 '((0 . 1) (1 . -2)))]
       [scaled (affine-scale original -1)]
       [negated (affine-neg original)])
  (assert-equal (affine-center scaled) (affine-center negated))
  (assert-equal (affine-terms scaled) (affine-terms negated)))"))
    (available-modules (prelude sort interval))
    (hints ("Scale center: (* k (affine-center af))"
            "Scale terms: (map (lambda (t) (cons (car t) (* k (cdr t)))) (affine-terms af))"
            "Combine: (make-affine scaled-center scaled-terms)"))
    (reference-solution "(define (affine-scale af k)
  (make-affine (* k (affine-center af))
               (map (lambda (t) (cons (car t) (* k (cdr t))))
                    (affine-terms af))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement merge-terms
  ;; ──────────────────────────────────────────────
  ;; The key operation that preserves correlations.

  (task
    (id "numeric/affine/merge-terms-impl")
    (module affine)
    (tier 0)
    (type implement)
    (description "Implement merge-terms: merge two sorted term lists, combining coefficients of shared noise symbols using a given binary operation. This is the heart of affine arithmetic's correlation tracking. Terms are association lists sorted by noise-id: ((id1 . c1) (id2 . c2) ...). The merge is like merge-sort's merge phase: compare the noise-ids of the heads of both lists. If t1's id < t2's id, emit t1's pair with (op coef 0). If t1's id > t2's id, emit t2's pair with (op 0 coef). If ids are equal, combine coefficients with (op c1 c2). Use a named let loop with three accumulators (t1, t2, acc), building result in reverse.")
    (context "Available: null?, <, >, =, caar, cdar, cdr, car, cons, reverse, append, map, lambda, let, cond. Named let merge loop.")
    (before "(define (merge-terms terms1 terms2 op)
  ;; TODO: merge sorted term lists, combining shared noise IDs
  '())")
    (test-file "lattice/numeric/test-affine.ss")
    (test-forms (";; Disjoint noise IDs: just concatenate (with op applied)
(assert-equal '((0 . 3) (1 . 4))
              (merge-terms '((0 . 3)) '((1 . 4)) +))"
                 ";; Shared noise ID: combine coefficients
(assert-equal '((0 . 3) (1 . 4) (2 . 3))
              (merge-terms '((0 . 3) (2 . 5)) '((1 . 4) (2 . -2)) +))"
                 ";; Subtraction cancels shared noise → x - x = 0
(assert-equal '((0 . 0) (1 . 0))
              (merge-terms '((0 . 3) (1 . -2)) '((0 . 3) (1 . -2)) -))"
                 ";; Empty lists
(assert-equal '() (merge-terms '() '() +))
(assert-equal '((0 . 5)) (merge-terms '((0 . 5)) '() +))"))
    (available-modules (prelude sort interval))
    (hints ("Base case t1 empty: apply (op 0 coef) to remaining t2 entries, append reversed"
            "Base case t2 empty: apply (op coef 0) to remaining t1 entries, append reversed"
            "Compare: (caar t1) vs (caar t2)"
            "Same ID: (cons (cons id (op c1 c2)) acc)"
            "Build in reverse, return (reverse acc) at base case"))
    (reference-solution "(define (merge-terms terms1 terms2 op)
  (let loop ([t1 terms1] [t2 terms2] [acc '()])
    (cond
      [(null? t1)
       (reverse (append (map (lambda (t) (cons (car t) (op 0 (cdr t)))) t2) acc))]
      [(null? t2)
       (reverse (append (map (lambda (t) (cons (car t) (op (cdr t) 0))) t1) acc))]
      [(< (caar t1) (caar t2))
       (loop (cdr t1) t2 (cons (cons (caar t1) (op (cdar t1) 0)) acc))]
      [(> (caar t1) (caar t2))
       (loop t1 (cdr t2) (cons (cons (caar t2) (op 0 (cdar t2))) acc))]
      [else
       (loop (cdr t1) (cdr t2) (cons (cons (caar t1) (op (cdar t1) (cdar t2))) acc))])))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Implement affine-add
  ;; ──────────────────────────────────────────────
  ;; Addition that preserves correlations.

  (task
    (id "numeric/affine/affine-add-impl")
    (module affine)
    (tier 0)
    (type implement)
    (description "Implement affine-add: add two affine forms while preserving correlations. For (x₀ + Σxᵢεᵢ) + (y₀ + Σyᵢεᵢ) = (x₀+y₀) + Σ(xᵢ+yᵢ)εᵢ. The key insight: when two affine forms share noise symbols (from the same original variable), addition automatically combines their coefficients. This is what makes affine arithmetic superior to interval arithmetic for correlated quantities. Add the centers and merge the terms using + as the combining operation.")
    (context "Available: affine-center, affine-terms, make-affine, merge-terms, +. One addition + one merge-terms call.")
    (before "(define (affine-add af1 af2)
  ;; TODO: add centers, merge terms with +
  (affine-constant 0))")
    (test-file "lattice/numeric/test-affine.ss")
    (test-forms (";; Add constants: just adds centers
(let ([af (affine-add (affine-constant 3) (affine-constant 7))])
  (assert-equal 10 (affine-center af))
  (assert-equal '() (affine-terms af)))"
                 ";; Add with shared noise: coefficients combine
(let ([af (affine-add (make-affine 3 '((0 . 1) (1 . -2)))
                      (make-affine 3 '((0 . 1) (1 . -2))))])
  (assert-equal 6 (affine-center af))
  ;; Coefficients double: ((0 . 2) (1 . -4))
  (assert-equal '((0 . 2) (1 . -4)) (affine-terms af)))"
                 ";; Add with disjoint noise: terms concatenated
(let ([af (affine-add (make-affine 1 '((0 . 5)))
                      (make-affine 2 '((1 . 3))))])
  (assert-equal 3 (affine-center af))
  (assert-equal '((0 . 5) (1 . 3)) (affine-terms af)))"))
    (available-modules (prelude sort interval))
    (hints ("New center: (+ (affine-center af1) (affine-center af2))"
            "New terms: (merge-terms (affine-terms af1) (affine-terms af2) +)"
            "Return: (make-affine new-center new-terms)"))
    (reference-solution "(define (affine-add af1 af2)
  (make-affine (+ (affine-center af1) (affine-center af2))
               (merge-terms (affine-terms af1) (affine-terms af2) +)))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

) ;; end tasks
