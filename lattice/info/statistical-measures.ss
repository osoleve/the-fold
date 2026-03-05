;;; @module statistical-measures
;;; @requires prelude entropy transcendental
;;; @description Probability distribution divergence measures: KL, JS, Hellinger, Bhattacharyya, chi-squared, total variation, alpha-divergence, and many more.
;;; @purity total
;;; @stability stable

(require 'prelude)
(require 'entropy)
(require 'transcendental)

(doc 'module 'statistical-measures)
(doc 'description "Statistical divergence measures for comparing probability distributions")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'section 're-exported-from-entropy)

(doc 'note "kl-divergence, jensen-shannon-divergence, cross-entropy, and mutual-information are re-exported from entropy.ss")

(doc 'section 'bhattacharyya-distance)

(define (bhattacharyya-coefficient p q)
  (doc 'export #t)
  (doc 'type '(-> (List Real) (List Real) Real))
  (doc 'description "Bhattacharyya coefficient BC(P,Q) = sum(sqrt(p_i * q_i)), range [0, 1]")
  (if (or (null? p) (null? q))
      0
      (fold-left + 0
                 (map (lambda (pi qi)
                              (if (or (< pi 0) (< qi 0))
                                  0
                                  (sqrt (* pi qi))))
                      p q))))

(define (bhattacharyya-distance p q)
  (doc 'export #t)
  (doc 'type '(-> (List Real) (List Real) Real))
  (doc 'description "Bhattacharyya distance D_B(P,Q) = -ln(BC(P,Q)), range [0, +inf)")
  (let ([bc (bhattacharyya-coefficient p q)])
       (if (<= bc 0)
           +inf.0
           (- (log-num bc)))))

(doc 'section 'hellinger-distance)

(define (hellinger-distance p q)
  (doc 'export #t)
  (doc 'type '(-> (List Real) (List Real) Real))
  (doc 'description "Hellinger distance, symmetric measure in [0, 1], H^2 = 1 - BC")
  (if (or (null? p) (null? q))
      1
      (let ([sum-sq (fold-left + 0
                               (map (lambda (pi qi)
                                            (let ([diff (- (sqrt (max 0 pi))
                                                           (sqrt (max 0 qi)))])
                                                 (* diff diff)))
                                    p q))])
           (sqrt (* 0.5 sum-sq)))))

(define (hellinger-distance-from-bc bc)
  (doc 'export #t)
  (doc 'type '(-> Real Real))
  (doc 'description "Compute Hellinger distance from Bhattacharyya coefficient: H = sqrt(1 - BC)")
  (sqrt (- 1 (max 0 (min 1 bc)))))

(doc 'section 'total-variation-distance)

(define (total-variation-distance p q)
  (doc 'export #t)
  (doc 'type '(-> (List Real) (List Real) Real))
  (doc 'description "Total variation distance TV(P,Q) = 1/2 * sum(|p_i - q_i|), range [0, 1]")
  (if (or (null? p) (null? q))
      1
      (* 0.5 (fold-left + 0
                        (map (lambda (pi qi)
                                     (abs (- pi qi)))
                             p q)))))

(doc 'section 'chi-squared-divergence)

(define (chi-squared-divergence p q)
  (doc 'export #t)
  (doc 'type '(-> (List Real) (List Real) Real))
  (doc 'description "Chi-squared divergence χ²(P||Q) = sum((p_i - q_i)^2 / q_i)")
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

(define (symmetric-chi-squared p q)
  (doc 'export #t)
  (doc 'type '(-> (List Real) (List Real) Real))
  (doc 'description "Symmetric chi-squared divergence: (χ²(P||Q) + χ²(Q||P)) / 2")
  (let ([pq (chi-squared-divergence p q)]
        [qp (chi-squared-divergence q p)])
       (if (or (= pq +inf.0) (= qp +inf.0))
           +inf.0
           (/ (+ pq qp) 2))))

(doc 'section 'jeffreys-divergence)

(define (jeffreys-divergence p q)
  (doc 'export #t)
  (doc 'type '(-> (List Real) (List Real) Real))
  (doc 'description "Jeffreys divergence J(P,Q) = D_KL(P||Q) + D_KL(Q||P), symmetric KL")
  (let ([kl-pq (kl-divergence p q)]
        [kl-qp (kl-divergence q p)])
       (+ kl-pq kl-qp)))

(doc 'section 'alpha-divergence)

(define (alpha-divergence alpha p q)
  (doc 'export #t)
  (doc 'type '(-> Real (List Real) (List Real) Real))
  (doc 'description "Alpha-divergence (Rényi divergence), generalizes KL, Hellinger, chi-squared")
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

(doc 'section 'squared-hellinger-distance)

(define (squared-hellinger-distance p q)
  (doc 'export #t)
  (doc 'type '(-> (List Real) (List Real) Real))
  (doc 'description "Squared Hellinger distance H²(P,Q) = 1 - BC(P,Q)")
  (let ([bc (bhattacharyya-coefficient p q)])
       (- 1 bc)))

(doc 'section 'triangular-discrimination)

(define (triangular-discrimination p q)
  (doc 'export #t)
  (doc 'type '(-> (List Real) (List Real) Real))
  (doc 'description "Triangular discrimination Δ(P,Q) = sum((p_i - q_i)^2 / (p_i + q_i))")
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

(doc 'section 'pearson-and-neyman)

(doc pearson-divergence 'export #t)
(define pearson-divergence chi-squared-divergence)
(doc pearson-divergence 'type '(-> (List Real) (List Real) Real))
(doc pearson-divergence 'description "Pearson chi-squared divergence (alias for chi-squared-divergence)")

(define (neyman-divergence p q)
  (doc 'export #t)
  (doc 'type '(-> (List Real) (List Real) Real))
  (doc 'description "Neyman chi-squared divergence (reverse Pearson)")
  (chi-squared-divergence q p))

(doc 'section 'k-divergence)

(define (k-divergence p q)
  (doc 'export #t)
  (doc 'type '(-> (List Real) (List Real) Real))
  (doc 'description "K-divergence: symmetric measure based on harmonic mean")
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

(doc 'section 'squared-loss-divergence)

(define (squared-loss p q)
  (doc 'export #t)
  (doc 'type '(-> (List Real) (List Real) Real))
  (doc 'description "Squared loss (squared L2 distance): sum((p_i - q_i)^2)")
  (if (or (null? p) (null? q))
      0
      (fold-left + 0
                 (map (lambda (pi qi)
                              (let ([diff (- pi qi)])
                                   (* diff diff)))
                      p q))))

(define (euclidean-distance p q)
  (doc 'export #t)
  (doc 'type '(-> (List Real) (List Real) Real))
  (doc 'description "Euclidean distance (L2 norm): sqrt(sum((p_i - q_i)^2))")
  (sqrt (squared-loss p q)))

(doc 'section 'matusita-distance)

(define (matusita-distance p q)
  (doc 'export #t)
  (doc 'type '(-> (List Real) (List Real) Real))
  (doc 'description "Matusita distance: sqrt(sum((sqrt(p_i) - sqrt(q_i))^2))")
  (if (or (null? p) (null? q))
      0
      (let ([sum-sq (fold-left + 0
                               (map (lambda (pi qi)
                                            (let ([diff (- (sqrt (max 0 pi))
                                                           (sqrt (max 0 qi)))])
                                                 (* diff diff)))
                                    p q))])
           (sqrt sum-sq))))

(doc 'section 'fidelity)

(doc fidelity 'export #t)
(define fidelity bhattacharyya-coefficient)
(doc fidelity 'type '(-> (List Real) (List Real) Real))
(doc fidelity 'description "Fidelity F(P,Q) = sum(sqrt(p_i * q_i)), quantum analogy")

(doc 'section 'summary-statistics)

(define (all-divergences p q)
  (doc 'export #t)
  (doc 'type '(-> (List Real) (List Real) (List (Pair Symbol Real))))
  (doc 'description "Compute multiple divergence measures for comparison")
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

(doc 'section 'utility-functions)

(define (valid-distribution? p)
  (doc 'export #t)
  (doc 'type '(-> (List Real) Boolean))
  (doc 'description "Check if list is a valid probability distribution")
  (and (not (null? p))
       (andmap (lambda (pi) (>= pi 0)) p)
       (let ([sum (fold-left + 0 p)])
            (< (abs (- sum 1.0)) 1e-6))))

(define (normalize-distribution weights)
  (doc 'export #t)
  (doc 'type '(-> (List Real) (List Real)))
  (doc 'description "Normalize non-negative weights to sum to 1")
  (let ([total (fold-left + 0 weights)])
       (if (<= total 0)
           (let ([n (length weights)])
                (map (lambda (_) (/ 1.0 n)) weights))
           (map (lambda (w) (/ w total)) weights))))

