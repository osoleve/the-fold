;;; statistical-measures.sexp — Development tasks for lattice/info/statistical-measures.ss
;;;
;;; Module: statistical-measures (tier 0, depends on prelude, entropy, transcendental)
;;; 291 lines, ~20 exported functions
;;; Statistical divergence measures: Bhattacharyya coefficient/distance,
;;; Hellinger distance, total variation, chi-squared divergence, Jeffreys
;;; divergence, alpha-divergence, triangular discrimination, K-divergence,
;;; squared loss, Euclidean distance, Matusita distance, fidelity.
;;;
;;; Git history:
;;;   a3abed54 refactor: Reorganize into core/lattice/shell three-layer architecture
;;;   2696481c feat(module): migrate info/, automata/, number-theory/ to require

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement total-variation-distance
  ;; ──────────────────────────────────────────────
  ;; The simplest divergence measure — half the L1 distance.

  (task
    (id "info/statistical-measures/tv-distance-impl")
    (module statistical-measures)
    (tier 0)
    (type implement)
    (description "Implement total-variation-distance: compute the total variation distance between two probability distributions P and Q. TV(P,Q) = 0.5 * sum(|p_i - q_i|). This is half the L1 distance, giving a value in [0,1]. TV=0 means identical distributions, TV=1 means completely disjoint support. The factor of 0.5 normalizes so that TV never exceeds 1. Handle the empty list case by returning 1 (maximally different). Use fold-left with map to sum |p_i - q_i|.")
    (context "Available: fold-left, map, abs, -, +, *, null?, or. Pure arithmetic on lists of reals.")
    (before "(define (total-variation-distance p q)
  (doc 'export #t)
  (doc 'type '(-> (List Real) (List Real) Real))
  (doc 'description \"Total variation distance TV(P,Q) = 1/2 * sum(|p_i - q_i|), range [0, 1]\")
  ;; TODO: half the sum of absolute differences
  0)")
    (test-file "lattice/info/test-statistical-measures.ss")
    (test-forms (";; Identical distributions → 0
(assert-true (< (abs (total-variation-distance '(0.5 0.5) '(0.5 0.5))) 0.001))"
                 ";; Disjoint support → 1
(assert-true (< (abs (- (total-variation-distance '(0.5 0.5 0.0 0.0) '(0.0 0.0 0.5 0.5)) 1.0)) 0.001))"
                 ";; Known value: TV((0.5,0.5), (0.8,0.2)) = 0.3
(assert-true (< (abs (- (total-variation-distance '(0.5 0.5) '(0.8 0.2)) 0.3)) 0.001))"
                 ";; Symmetric: TV(P,Q) = TV(Q,P)
(let ([tv1 (total-variation-distance '(0.5 0.3 0.2) '(0.4 0.35 0.25))]
      [tv2 (total-variation-distance '(0.4 0.35 0.25) '(0.5 0.3 0.2))])
  (assert-true (< (abs (- tv1 tv2)) 0.0001)))"))
    (available-modules (prelude))
    (hints ("Check for empty: (if (or (null? p) (null? q)) 1 ...)"
            "Map absolute differences: (map (lambda (pi qi) (abs (- pi qi))) p q)"
            "Sum: (fold-left + 0 ...)"
            "Multiply by 0.5: (* 0.5 sum)"))
    (reference-solution "(define (total-variation-distance p q)
  (if (or (null? p) (null? q))
      1
      (* 0.5 (fold-left + 0
                        (map (lambda (pi qi)
                                     (abs (- pi qi)))
                             p q)))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement bhattacharyya-coefficient
  ;; ──────────────────────────────────────────────
  ;; Overlap measure via geometric mean — foundation for Hellinger.

  (task
    (id "info/statistical-measures/bhattacharyya-coef-impl")
    (module statistical-measures)
    (tier 0)
    (type implement)
    (description "Implement bhattacharyya-coefficient: compute BC(P,Q) = sum(sqrt(p_i * q_i)), which measures the overlap between two probability distributions. The result is in [0,1]: BC=1 means identical distributions, BC=0 means completely disjoint support. Each term sqrt(p_i * q_i) is the geometric mean of the corresponding probabilities. Guard against negative probabilities by treating them as zero. The Bhattacharyya coefficient relates to both the Bhattacharyya distance (D_B = -ln(BC)) and Hellinger distance (H^2 = 1 - BC).")
    (context "Available: fold-left, map, sqrt, *, +, <, or, null?. Pure arithmetic on lists of reals.")
    (before "(define (bhattacharyya-coefficient p q)
  (doc 'export #t)
  (doc 'type '(-> (List Real) (List Real) Real))
  (doc 'description \"Bhattacharyya coefficient BC(P,Q) = sum(sqrt(p_i * q_i)), range [0, 1]\")
  ;; TODO: sum of sqrt(p_i * q_i) for each pair
  0)")
    (test-file "lattice/info/test-statistical-measures.ss")
    (test-forms (";; Identical distributions → 1.0
(assert-true (< (abs (- (bhattacharyya-coefficient '(0.5 0.5) '(0.5 0.5)) 1.0)) 0.001))"
                 ";; Disjoint support → 0.0
(assert-true (< (abs (bhattacharyya-coefficient '(1.0 0.0) '(0.0 1.0))) 0.001))"
                 ";; Symmetric: BC(P,Q) = BC(Q,P)
(let ([bc1 (bhattacharyya-coefficient '(0.5 0.3 0.2) '(0.4 0.35 0.25))]
      [bc2 (bhattacharyya-coefficient '(0.4 0.35 0.25) '(0.5 0.3 0.2))])
  (assert-true (< (abs (- bc1 bc2)) 0.0001)))"
                 ";; Result in [0, 1]
(let ([bc (bhattacharyya-coefficient '(0.5 0.3 0.2) '(0.4 0.35 0.25))])
  (assert-true (and (>= bc 0) (<= bc 1))))"))
    (available-modules (prelude))
    (hints ("Guard: if either is null, return 0"
            "Map: (map (lambda (pi qi) (if (or (< pi 0) (< qi 0)) 0 (sqrt (* pi qi)))) p q)"
            "Sum: (fold-left + 0 mapped-values)"
            "The sqrt(p*q) is the geometric mean of each pair of probabilities"))
    (reference-solution "(define (bhattacharyya-coefficient p q)
  (if (or (null? p) (null? q))
      0
      (fold-left + 0
                 (map (lambda (pi qi)
                              (if (or (< pi 0) (< qi 0))
                                  0
                                  (sqrt (* pi qi))))
                      p q))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement hellinger-distance
  ;; ──────────────────────────────────────────────
  ;; A proper metric on distributions — satisfies triangle inequality.

  (task
    (id "info/statistical-measures/hellinger-impl")
    (module statistical-measures)
    (tier 0)
    (type implement)
    (description "Implement hellinger-distance: compute the Hellinger distance between distributions P and Q. The formula is: H(P,Q) = sqrt(0.5 * sum((sqrt(p_i) - sqrt(q_i))^2)). This is a proper metric (symmetric, satisfies triangle inequality) with range [0,1]. H=0 means identical, H=1 means completely disjoint. It relates to the Bhattacharyya coefficient via H^2 = 1 - BC. Guard against negative probabilities using max(0, x) before taking square roots.")
    (context "Available: fold-left, map, sqrt, max, -, *, +, null?, or. Pure arithmetic on lists of reals.")
    (before "(define (hellinger-distance p q)
  (doc 'export #t)
  (doc 'type '(-> (List Real) (List Real) Real))
  (doc 'description \"Hellinger distance, symmetric measure in [0, 1], H^2 = 1 - BC\")
  ;; TODO: sqrt(0.5 * sum((sqrt(p_i) - sqrt(q_i))^2))
  0)")
    (test-file "lattice/info/test-statistical-measures.ss")
    (test-forms (";; Identical distributions → 0
(assert-true (< (abs (hellinger-distance '(0.5 0.5) '(0.5 0.5))) 0.001))"
                 ";; Disjoint support → 1
(assert-true (< (abs (- (hellinger-distance '(1.0 0.0) '(0.0 1.0)) 1.0)) 0.001))"
                 ";; Symmetric
(let ([h1 (hellinger-distance '(0.5 0.3 0.2) '(0.4 0.35 0.25))]
      [h2 (hellinger-distance '(0.4 0.35 0.25) '(0.5 0.3 0.2))])
  (assert-true (< (abs (- h1 h2)) 0.0001)))"
                 ";; Relates to Bhattacharyya: H^2 = 1 - BC
(let ([h (hellinger-distance '(0.5 0.3 0.2) '(0.4 0.35 0.25))]
      [bc (bhattacharyya-coefficient '(0.5 0.3 0.2) '(0.4 0.35 0.25))])
  (assert-true (< (abs (- (* h h) (- 1 bc))) 0.001)))"))
    (available-modules (prelude))
    (hints ("Guard: if either is null, return 1"
            "For each pair: diff = sqrt(max(0, p_i)) - sqrt(max(0, q_i)), then diff^2"
            "Sum all squared differences with fold-left"
            "Result = sqrt(0.5 * sum)"
            "The max(0, ...) guards against negative probabilities"))
    (reference-solution "(define (hellinger-distance p q)
  (if (or (null? p) (null? q))
      1
      (let ([sum-sq (fold-left + 0
                               (map (lambda (pi qi)
                                            (let ([diff (- (sqrt (max 0 pi))
                                                           (sqrt (max 0 qi)))])
                                                 (* diff diff)))
                                    p q))])
           (sqrt (* 0.5 sum-sq)))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement chi-squared-divergence
  ;; ──────────────────────────────────────────────
  ;; The Pearson divergence — relates to the chi-squared test statistic.

  (task
    (id "info/statistical-measures/chi-squared-impl")
    (module statistical-measures)
    (tier 0)
    (type implement)
    (description "Implement chi-squared-divergence: compute chi^2(P||Q) = sum((p_i - q_i)^2 / q_i). This is the Pearson chi-squared divergence, related to the chi-squared goodness-of-fit test. Unlike Hellinger and TV distance, this is asymmetric: chi^2(P||Q) != chi^2(Q||P). Handle the case where q_i = 0: if p_i > 0 and q_i = 0, the divergence is +inf (P puts mass where Q doesn't); if both are 0, contribute 0. Use +inf.0 for infinity in Scheme.")
    (context "Available: fold-left, map, abs, -, *, /, <=, >, cond, null?, or, +inf.0. Pure arithmetic on lists of reals.")
    (before "(define (chi-squared-divergence p q)
  (doc 'export #t)
  (doc 'type '(-> (List Real) (List Real) Real))
  (doc 'description \"Chi-squared divergence chi^2(P||Q) = sum((p_i - q_i)^2 / q_i)\")
  ;; TODO: sum of (p_i - q_i)^2 / q_i with zero handling
  0)")
    (test-file "lattice/info/test-statistical-measures.ss")
    (test-forms (";; Identical distributions → 0
(assert-true (< (abs (chi-squared-divergence '(0.5 0.5) '(0.5 0.5))) 0.001))"
                 ";; Non-negative
(assert-true (>= (chi-squared-divergence '(0.5 0.3 0.2) '(0.4 0.35 0.25)) 0))"
                 ";; Asymmetric: chi^2(P||Q) != chi^2(Q||P) in general
(let ([pq (chi-squared-divergence '(0.5 0.3 0.2) '(0.4 0.35 0.25))]
      [qp (chi-squared-divergence '(0.4 0.35 0.25) '(0.5 0.3 0.2))])
  (assert-true (> (abs (- pq qp)) 0.001)))"
                 ";; Disjoint support → +inf
(assert-equal +inf.0 (chi-squared-divergence '(0.5 0.5 0.0 0.0) '(0.0 0.0 0.5 0.5)))"))
    (available-modules (prelude))
    (hints ("Guard empty lists → return 0"
            "For each pair, three cases via cond:"
            "  qi <= 0 and pi > 0 → +inf.0"
            "  qi <= 0 and pi <= 0 → 0"
            "  else → (/ (* diff diff) qi) where diff = (- pi qi)"
            "Sum with fold-left"))
    (reference-solution "(define (chi-squared-divergence p q)
  (if (or (null? p) (null? q))
      0
      (fold-left + 0
                 (map (lambda (pi qi)
                              (cond
                               [(<= qi 0)
                                (if (> pi 0) +inf.0 0)]
                               [else
                                (let ([diff (- pi qi)])
                                     (/ (* diff diff) qi))]))
                      p q))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 3))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Implement triangular-discrimination
  ;; ──────────────────────────────────────────────
  ;; Symmetric chi-squared variant bounded in [0,1].

  (task
    (id "info/statistical-measures/triangular-impl")
    (module statistical-measures)
    (tier 0)
    (type implement)
    (description "Implement triangular-discrimination: compute Delta(P,Q) = sum((p_i - q_i)^2 / (p_i + q_i)). This is a symmetric divergence measure (unlike chi-squared), bounded in [0,1] for probability distributions. Each term divides the squared difference by the sum of probabilities at that position. When both p_i and q_i are zero, the term contributes 0 (skip via sum <= 0 check). It is sometimes called the Vincze-Le Cam divergence and provides a symmetric alternative to the asymmetric chi-squared divergence.")
    (context "Available: fold-left, map, -, +, *, /, <=, null?, or. Pure arithmetic on lists of reals.")
    (before "(define (triangular-discrimination p q)
  (doc 'export #t)
  (doc 'type '(-> (List Real) (List Real) Real))
  (doc 'description \"Triangular discrimination Delta(P,Q) = sum((p_i - q_i)^2 / (p_i + q_i))\")
  ;; TODO: sum of squared differences divided by sums
  0)")
    (test-file "lattice/info/test-statistical-measures.ss")
    (test-forms (";; Identical distributions → 0
(assert-true (< (abs (triangular-discrimination '(0.5 0.5) '(0.5 0.5))) 0.001))"
                 ";; Symmetric: Delta(P,Q) = Delta(Q,P)
(let ([d1 (triangular-discrimination '(0.5 0.3 0.2) '(0.4 0.35 0.25))]
      [d2 (triangular-discrimination '(0.4 0.35 0.25) '(0.5 0.3 0.2))])
  (assert-true (< (abs (- d1 d2)) 0.0001)))"
                 ";; Non-negative
(assert-true (>= (triangular-discrimination '(0.5 0.3 0.2) '(0.4 0.35 0.25)) 0))"
                 ";; Bounded by 1
(assert-true (<= (triangular-discrimination '(0.5 0.3 0.2) '(0.4 0.35 0.25)) 1))"))
    (available-modules (prelude))
    (hints ("Guard empty lists → return 0"
            "For each pair: compute sum-pq = (+ pi qi)"
            "If sum-pq <= 0, contribute 0"
            "Else: diff = (- pi qi), contribute (/ (* diff diff) sum-pq)"
            "Sum with fold-left"))
    (reference-solution "(define (triangular-discrimination p q)
  (if (or (null? p) (null? q))
      0
      (fold-left + 0
                 (map (lambda (pi qi)
                              (let ([sum-pq (+ pi qi)])
                                   (if (<= sum-pq 0)
                                       0
                                       (let ([diff (- pi qi)])
                                            (/ (* diff diff) sum-pq)))))
                      p q))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

) ;; end tasks
