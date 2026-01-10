;;; core/info-theory/statistical-measures.ss — Statistical Divergence Measures
;;;
;;; Information-theoretic and statistical measures for comparing probability distributions:
;;;   - KL divergence D_KL(P||Q)
;;;   - Jensen-Shannon divergence JSD(P||Q)
;;;   - Cross-entropy H(P,Q)
;;;   - Mutual information I(X;Y)
;;;   - Bhattacharyya distance/coefficient
;;;   - Hellinger distance
;;;   - Total variation distance
;;;   - Chi-squared divergence
;;;   - Jeffreys divergence (symmetric KL)
;;;   - Alpha-divergence (generalized family)
;;;
;;; This module consolidates statistical measures for distribution comparison
;;; and extends the entropy.ss module with additional divergence measures.
;;;
;;; This is Core code: pure, total, assumes valid probability distributions.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - entropy.ss (for base measures)
;;;   - fp/numeric/transcendental.ss

(load "core/base/prelude.ss")
(load "lattice/info/entropy.ss")
(load "lattice/fp/numeric/transcendental.ss")

;;; ============================================================
;;; Re-exported from entropy.ss
;;; ============================================================

;;; These functions are already implemented in entropy.ss and are
;;; re-exported here for convenience.

;;; kl-divergence : (List Real) × (List Real) → Real
;;; Kullback-Leibler divergence D_KL(P||Q) = sum(p_i * log2(p_i/q_i))
;;; (Already defined in entropy.ss)

;;; jensen-shannon-divergence : (List Real) × (List Real) → Real
;;; Jensen-Shannon divergence: JSD(P||Q) = (D_KL(P||M) + D_KL(Q||M)) / 2
;;; where M = (P + Q) / 2.
;;; (Already defined in entropy.ss)

;;; cross-entropy : (List Real) × (List Real) → Real
;;; Cross entropy H(P,Q) = -sum(p_i * log2(q_i))
;;; (Already defined in entropy.ss)

;;; mutual-information : (List (List Real)) × (List Real) × (List Real) → Real
;;; Mutual information I(X;Y) = H(X) + H(Y) - H(X,Y)
;;; (Already defined in entropy.ss)

;;; ============================================================
;;; Bhattacharyya Distance and Coefficient
;;; ============================================================

;;; bhattacharyya-coefficient : (List Real) × (List Real) → Real
;;; Bhattacharyya coefficient BC(P,Q) = sum(sqrt(p_i * q_i))
;;; Measures overlap between two distributions, range [0, 1].
;;; BC = 1 when distributions are identical, BC = 0 when disjoint.
(define (bhattacharyya-coefficient p q)
  (if (or (null? p) (null? q))
      0
      (fold-left + 0
                 (map (lambda (pi qi)
                              (if (or (< pi 0) (< qi 0))
                                  0
                                  (sqrt (* pi qi))))
                      p q))))

;;; bhattacharyya-distance : (List Real) × (List Real) → Real
;;; Bhattacharyya distance D_B(P,Q) = -ln(BC(P,Q))
;;; Measures divergence, range [0, +inf).
;;; D_B = 0 when distributions are identical.
(define (bhattacharyya-distance p q)
  (let ([bc (bhattacharyya-coefficient p q)])
       (if (<= bc 0)
           +inf.0
           (- (log-num bc)))))

;;; ============================================================
;;; Hellinger Distance
;;; ============================================================

;;; hellinger-distance : (List Real) × (List Real) → Real
;;; Hellinger distance H(P,Q) = sqrt(1/2 * sum((sqrt(p_i) - sqrt(q_i))^2))
;;; Symmetric measure of distribution similarity, range [0, 1].
;;; H = 0 when distributions are identical, H = 1 when disjoint.
;;; Related to Bhattacharyya: H^2 = 1 - BC
(define (hellinger-distance p q)
  (if (or (null? p) (null? q))
      1
      (let ([sum-sq (fold-left + 0
                               (map (lambda (pi qi)
                                            (let ([diff (- (sqrt (max 0 pi))
                                                           (sqrt (max 0 qi)))])
                                                 (* diff diff)))
                                    p q))])
           (sqrt (* 0.5 sum-sq)))))

;;; hellinger-distance-from-bc : Real → Real
;;; Compute Hellinger distance from Bhattacharyya coefficient.
;;; H = sqrt(1 - BC)
(define (hellinger-distance-from-bc bc)
  (sqrt (- 1 (max 0 (min 1 bc)))))

;;; ============================================================
;;; Total Variation Distance
;;; ============================================================

;;; total-variation-distance : (List Real) × (List Real) → Real
;;; Total variation distance TV(P,Q) = 1/2 * sum(|p_i - q_i|)
;;; L1 distance between distributions, range [0, 1].
;;; TV = 0 when distributions are identical, TV = 1 when disjoint.
(define (total-variation-distance p q)
  (if (or (null? p) (null? q))
      1
      (* 0.5 (fold-left + 0
                        (map (lambda (pi qi)
                                     (abs (- pi qi)))
                             p q)))))

;;; ============================================================
;;; Chi-Squared Divergence
;;; ============================================================

;;; chi-squared-divergence : (List Real) × (List Real) → Real
;;; Chi-squared divergence χ²(P||Q) = sum((p_i - q_i)^2 / q_i)
;;; Measures divergence, range [0, +inf).
;;; Returns +inf.0 if Q has zero probability where P is non-zero.
;;; Not symmetric: χ²(P||Q) ≠ χ²(Q||P)
(define (chi-squared-divergence p q)
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
                      p q))))

;;; symmetric-chi-squared : (List Real) × (List Real) → Real
;;; Symmetric chi-squared divergence: (χ²(P||Q) + χ²(Q||P)) / 2
(define (symmetric-chi-squared p q)
  (let ([pq (chi-squared-divergence p q)]
        [qp (chi-squared-divergence q p)])
       (if (or (= pq +inf.0) (= qp +inf.0))
           +inf.0
           (/ (+ pq qp) 2))))

;;; ============================================================
;;; Jeffreys Divergence (Symmetric KL)
;;; ============================================================

;;; jeffreys-divergence : (List Real) × (List Real) → Real
;;; Jeffreys divergence J(P,Q) = D_KL(P||Q) + D_KL(Q||P)
;;; Symmetric version of KL divergence.
;;; Range: [0, +inf), J = 0 when P = Q.
;;; Note: This is also called J-divergence or information divergence.
(define (jeffreys-divergence p q)
  (let ([kl-pq (kl-divergence p q)]
        [kl-qp (kl-divergence q p)])
       (+ kl-pq kl-qp)))

;;; ============================================================
;;; Alpha-Divergence (Generalized Divergence Family)
;;; ============================================================

;;; alpha-divergence : Real × (List Real) × (List Real) → Real
;;; Alpha-divergence (Rényi divergence): D_α(P||Q) = 1/(α(α-1)) * log(sum(p_i^α * q_i^(1-α)))
;;; Generalizes several divergences:
;;;   α → 0: D_0 = -log(sum q_i * I[p_i > 0])
;;;   α = 0.5: Hellinger distance (squared and scaled)
;;;   α → 1: KL divergence (limit)
;;;   α = 2: Chi-squared divergence (scaled)
;;; Range: [0, +inf) for α > 0
(define (alpha-divergence alpha p q)
  (cond
   [(or (null? p) (null? q)) 0]
   [(= alpha 1)
    ;; Limit case: KL divergence
    (kl-divergence p q)]
   [(= alpha 0)
    ;; Limit case: related to support mismatch
    (- (log-num (fold-left + 0
                           (map (lambda (pi qi)
                                        (if (and (> pi 0) (> qi 0))
                                            qi
                                            0))
                                p q))))]
   [else
    ;; General case
    (let ([sum-term (fold-left + 0
                               (map (lambda (pi qi)
                                            (cond
                                             [(and (<= pi 0) (<= qi 0)) 0]
                                             [(or (<= pi 0) (<= qi 0)) 0]
                                             [else
                                              (* (expt pi alpha)
                                                 (expt qi (- 1 alpha)))]))
                                    p q))])
         (if (<= sum-term 0)
             +inf.0
             (/ (log-num sum-term)
                (* alpha (- alpha 1)))))]))

;;; ============================================================
;;; Squared Hellinger Distance (via alpha-divergence)
;;; ============================================================

;;; squared-hellinger-distance : (List Real) × (List Real) → Real
;;; Squared Hellinger distance via alpha-divergence with α = 0.5.
;;; H²(P,Q) = 1 - sum(sqrt(p_i * q_i)) = 1 - BC(P,Q)
(define (squared-hellinger-distance p q)
  (let ([bc (bhattacharyya-coefficient p q)])
       (- 1 bc)))

;;; ============================================================
;;; Triangular Discrimination
;;; ============================================================

;;; triangular-discrimination : (List Real) × (List Real) → Real
;;; Triangular discrimination Δ(P,Q) = sum((p_i - q_i)^2 / (p_i + q_i))
;;; Symmetric divergence measure, range [0, 1].
;;; Also known as triangular distance.
(define (triangular-discrimination p q)
  (if (or (null? p) (null? q))
      0
      (fold-left + 0
                 (map (lambda (pi qi)
                              (let ([sum-pq (+ pi qi)])
                                   (if (<= sum-pq 0)
                                       0
                                       (let ([diff (- pi qi)])
                                            (/ (* diff diff) sum-pq)))))
                      p q))))

;;; ============================================================
;;; Pearson Chi-Squared Divergence
;;; ============================================================

;;; pearson-divergence : (List Real) × (List Real) → Real
;;; Pearson chi-squared divergence: sum((p_i - q_i)^2 / q_i)
;;; This is identical to chi-squared-divergence above, included for naming clarity.
(define pearson-divergence chi-squared-divergence)

;;; neyman-divergence : (List Real) × (List Real) → Real
;;; Neyman chi-squared divergence (reverse Pearson): sum((p_i - q_i)^2 / p_i)
;;; Asymmetric, reverses the role of P and Q compared to Pearson.
(define (neyman-divergence p q)
  (chi-squared-divergence q p))

;;; ============================================================
;;; K-Divergence (Harmonic Mean Based)
;;; ============================================================

;;; k-divergence : (List Real) × (List Real) → Real
;;; K-divergence: sum(p_i * log(2*p_i / (p_i + q_i)))
;;; Symmetric measure based on harmonic mean.
(define (k-divergence p q)
  (if (or (null? p) (null? q))
      0
      (fold-left + 0
                 (map (lambda (pi qi)
                              (let ([sum-pq (+ pi qi)])
                                   (cond
                                    [(<= pi 0) 0]
                                    [(<= sum-pq 0) +inf.0]
                                    [else
                                     (* pi (log2 (/ (* 2 pi) sum-pq)))])))
                      p q))))

;;; ============================================================
;;; Squared-Loss Divergence
;;; ============================================================

;;; squared-loss : (List Real) × (List Real) → Real
;;; Squared loss (squared L2 distance): sum((p_i - q_i)^2)
;;; Simple quadratic divergence, range [0, 2].
(define (squared-loss p q)
  (if (or (null? p) (null? q))
      0
      (fold-left + 0
                 (map (lambda (pi qi)
                              (let ([diff (- pi qi)])
                                   (* diff diff)))
                      p q))))

;;; euclidean-distance : (List Real) × (List Real) → Real
;;; Euclidean distance (L2 norm): sqrt(sum((p_i - q_i)^2))
(define (euclidean-distance p q)
  (sqrt (squared-loss p q)))

;;; ============================================================
;;; Matusita Distance
;;; ============================================================

;;; matusita-distance : (List Real) × (List Real) → Real
;;; Matusita distance: sqrt(sum((sqrt(p_i) - sqrt(q_i))^2))
;;; Related to Hellinger distance: Matusita = sqrt(2) * Hellinger
(define (matusita-distance p q)
  (if (or (null? p) (null? q))
      0
      (let ([sum-sq (fold-left + 0
                               (map (lambda (pi qi)
                                            (let ([diff (- (sqrt (max 0 pi))
                                                           (sqrt (max 0 qi)))])
                                                 (* diff diff)))
                                    p q))])
           (sqrt sum-sq))))

;;; ============================================================
;;; Fidelity (Quantum Fidelity for Classical Distributions)
;;; ============================================================

;;; fidelity : (List Real) × (List Real) → Real
;;; Fidelity F(P,Q) = sum(sqrt(p_i * q_i))
;;; This is identical to Bhattacharyya coefficient, included for quantum analogy.
;;; Range: [0, 1], F = 1 for identical distributions.
(define fidelity bhattacharyya-coefficient)

;;; ============================================================
;;; Summary Statistics for Comparison
;;; ============================================================

;;; all-divergences : (List Real) × (List Real) → (List (Symbol × Real))
;;; Compute multiple divergence measures for comparison.
;;; Returns association list of (measure-name . value).
(define (all-divergences p q)
  (list
   (cons 'kl-divergence (kl-divergence p q))
   (cons 'reverse-kl (kl-divergence q p))
   (cons 'jeffreys (jeffreys-divergence p q))
   (cons 'jensen-shannon (jensen-shannon-divergence p q))
   (cons 'bhattacharyya-coef (bhattacharyya-coefficient p q))
   (cons 'bhattacharyya-dist (bhattacharyya-distance p q))
   (cons 'hellinger (hellinger-distance p q))
   (cons 'total-variation (total-variation-distance p q))
   (cons 'chi-squared (chi-squared-divergence p q))
   (cons 'squared-loss (squared-loss p q))
   (cons 'euclidean (euclidean-distance p q))
   (cons 'cross-entropy (cross-entropy p q))
   (cons 'triangular (triangular-discrimination p q))))

;;; ============================================================
;;; Utility: Validate Probability Distribution
;;; ============================================================

;;; valid-distribution? : (List Real) → Boolean
;;; Check if list is a valid probability distribution:
;;;   - All elements non-negative
;;;   - Sum approximately equals 1 (within epsilon)
(define (valid-distribution? p)
  (and (not (null? p))
       (andmap (lambda (pi) (>= pi 0)) p)
       (let ([sum (fold-left + 0 p)])
            (< (abs (- sum 1.0)) 1e-6))))

;;; normalize-distribution : (List Real) → (List Real)
;;; Normalize non-negative weights to sum to 1.
;;; Returns uniform distribution if all weights are zero.
(define (normalize-distribution weights)
  (let ([total (fold-left + 0 weights)])
       (if (<= total 0)
           (let ([n (length weights)])
                (map (lambda (_) (/ 1.0 n)) weights))
           (map (lambda (w) (/ w total)) weights))))

;;; andmap : (α → Boolean) × (List α) → Boolean
;;; Check if predicate holds for all elements.
(define (andmap pred lst)
  (or (null? lst)
      (and (pred (car lst))
           (andmap pred (cdr lst)))))
