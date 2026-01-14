;;; core/info-theory/entropy.ss — Entropy and Information Measures
;;;
;;; Information-theoretic quantities for discrete and continuous distributions:
;;;   - Shannon entropy H(X)
;;;   - Joint entropy H(X,Y)
;;;   - Conditional entropy H(X|Y)
;;;   - Mutual information I(X;Y)
;;;   - Cross entropy H(P,Q)
;;;   - KL divergence D_KL(P||Q)
;;;   - Relative entropy
;;;
;;; This is Core code: pure, total, assumes valid probability distributions.
;;;
;;; Dependencies:
;;;   - prelude.ss

(load "core/base/prelude.ss")
(load "lattice/fp/numeric/transcendental.ss")

;;; ====
;;; Constants
;;; ====

(define pi 3.14159265358979323846)

;;; ====
;;; Utility Functions
;;; ====

;;; entropy-log2 : Number → Number
;;; Logarithm base 2 (uses transcendental.ss log2 if not already defined).
(define (entropy-log2 x)
  (if (<= x 0)
      0  ; Convention: 0 * log(0) = 0
      (log2 x)))

;;; safe-log2 : Number → Number
;;; Alias for entropy-log2; used in cross-entropy for readability.
(define safe-log2 entropy-log2)

;;; any2 : (α × β → Bool) × (List α) × (List β) → Bool
;;; Check if predicate holds for any pair of corresponding elements.
(define (any2 pred xs ys)
  (cond
   [(null? xs) #f]
   [(null? ys) #f]
   [(pred (car xs) (car ys)) #t]
   [else (any2 pred (cdr xs) (cdr ys))]))

;;; plogp : Number → Number
;;; Compute p * log2(p) with convention 0 * log(0) = 0.
(define (plogp p)
  (if (<= p 0)
      0
      (* p (entropy-log2 p))))

;;; ====
;;; Shannon Entropy
;;; ====

;;; entropy : (List Number) → Number
;;; Shannon entropy H(X) = -sum(p_i * log2(p_i))
;;; Input: list of probabilities (should sum to 1).
(define (entropy probs)
  (- (fold-left + 0 (map plogp probs))))

;;; entropy-normalized : (List Number) → Number
;;; Shannon entropy with automatic normalization.
;;; Accepts any non-negative weights and normalizes them.
(define (entropy-normalized weights)
  (let* ([total (fold-left + 0 weights)]
         [probs (if (= total 0)
                    weights
                    (map (lambda (w) (/ w total)) weights))])
        (entropy probs)))

;;; max-entropy : Nat → Number
;;; Maximum entropy for n equally likely outcomes.
;;; H_max = log2(n)
(define (max-entropy n)
  (if (<= n 0)
      0
      (log2 n)))

;;; entropy-ratio : (List Number) → Number
;;; Normalized entropy H(X)/H_max, between 0 and 1.
;;; Also known as efficiency or redundancy complement.
(define (entropy-ratio probs)
  (let ([n (length probs)]
        [h (entropy probs)])
       (if (<= n 1)
           0
           (/ h (max-entropy n)))))

;;; ====
;;; Binary Entropy
;;; ====

;;; binary-entropy : Number → Number
;;; Binary entropy function H(p) = -p*log2(p) - (1-p)*log2(1-p)
;;; For a Bernoulli random variable with success probability p.
(define (binary-entropy p)
  (cond
   [(<= p 0) 0]
   [(>= p 1) 0]
   [else (- (+ (plogp p) (plogp (- 1 p))))]))

;;; ====
;;; Joint Entropy
;;; ====

;;; joint-entropy : (List (List Number)) → Number
;;; Joint entropy H(X,Y) from joint probability matrix.
;;; Input: 2D list where entry (i,j) is P(X=i, Y=j).
(define (joint-entropy joint-probs)
  (let ([flat (apply append joint-probs)])
       (entropy flat)))

;;; ====
;;; Conditional Entropy
;;; ====

;;; conditional-entropy : (List (List Number)) × (List Number) → Number
;;; Conditional entropy H(X|Y) = H(X,Y) - H(Y)
;;; Input: joint probability matrix and marginal P(Y).
(define (conditional-entropy joint-probs marginal-y)
  (let ([h-joint (joint-entropy joint-probs)]
        [h-y (entropy marginal-y)])
       (- h-joint h-y)))

;;; conditional-entropy-direct : (List (List Number)) × (List Number) → Number
;;; Compute H(X|Y) directly from conditional probability distributions.
;;; Input: list of P(X|Y=y) for each y, weighted by P(Y=y).
;;; Each row is a conditional distribution P(X|Y=y).
;;; Returns 0 when total weight is zero (no conditioning events).
(define (conditional-entropy-direct conditional-probs weights-y)
  (let ([total-weight (fold-left + 0 weights-y)])
       (if (<= total-weight 0)
           0  ; No conditioning events; return 0 entropy
           (let* ([normalized-weights (map (lambda (w) (/ w total-weight)) weights-y)]
                  [conditional-entropies (map entropy conditional-probs)])
                 (fold-left + 0
                            (map * normalized-weights conditional-entropies))))))

;;; ====
;;; Mutual Information
;;; ====

;;; mutual-information : (List (List Number)) × (List Number) × (List Number) → Number
;;; Mutual information I(X;Y) = H(X) + H(Y) - H(X,Y)
;;;                           = H(X) - H(X|Y)
;;;                           = H(Y) - H(Y|X)
(define (mutual-information joint-probs marginal-x marginal-y)
  (let ([h-x (entropy marginal-x)]
        [h-y (entropy marginal-y)]
        [h-joint (joint-entropy joint-probs)])
       (+ h-x (- h-y h-joint))))

;;; mutual-information-from-joint : (List (List Number)) → Number
;;; Compute I(X;Y) from joint distribution only.
;;; Computes marginals automatically.
(define (mutual-information-from-joint joint-probs)
  (let* ([marginal-x (compute-marginal-x joint-probs)]
         [marginal-y (compute-marginal-y joint-probs)])
        (mutual-information joint-probs marginal-x marginal-y)))

;;; compute-marginal-x : (List (List Number)) → (List Number)
;;; For joint matrix where entry (i,j) = P(X=i, Y=j),
;;; sum each row over all columns to get P(X=i).
(define (compute-marginal-x joint-probs)
  (map (lambda (row) (fold-left + 0 row)) joint-probs))

;;; compute-marginal-y : (List (List Number)) → (List Number)
;;; For joint matrix where entry (i,j) = P(X=i, Y=j),
;;; sum each column over all rows to get P(Y=j).
(define (compute-marginal-y joint-probs)
  (if (null? joint-probs)
      '()
      (let ([n-cols (length (car joint-probs))])
           (map (lambda (j)
                        (fold-left + 0
                                   (map (lambda (row) (list-ref row j))
                                        joint-probs)))
                (iota n-cols)))))

;;; iota : Nat → (List Nat)
;;; Generate list (0 1 2 ... n-1).
(define (iota n)
  (if (<= n 0)
      '()
      (let loop ([i 0] [acc '()])
           (if (= i n)
               (reverse acc)
               (loop (+ i 1) (cons i acc))))))

;;; ====
;;; Cross Entropy and KL Divergence
;;; ====

;;; cross-entropy : (List Number) × (List Number) → Number
;;; Cross entropy H(P,Q) = -sum(p_i * log2(q_i))
;;; Measures the average number of bits needed to encode samples
;;; from P using a code optimized for Q.
;;; Returns +inf.0 when Q has zero probability for events where P > 0.
(define (cross-entropy p q)
  (if (null? p)
      0
      ;; Check for support mismatch: if Q(x)=0 but P(x)>0, return +inf.0
      (if (any2 (lambda (pi qi) (and (> pi 0) (<= qi 0))) p q)
          +inf.0
          (- (fold-left + 0
                        (map (lambda (pi qi)
                                     (if (<= pi 0)
                                         0
                                         (* pi (safe-log2 qi))))
                             p q))))))

;;; kl-divergence : (List Number) × (List Number) → Number
;;; Kullback-Leibler divergence D_KL(P||Q) = sum(p_i * log2(p_i/q_i))
;;; Measures how much P diverges from Q.
;;; Note: Not symmetric! D_KL(P||Q) ≠ D_KL(Q||P)
(define (kl-divergence p q)
  (if (null? p)
      0
      (fold-left + 0
                 (map (lambda (pi qi)
                              (cond
                               [(<= pi 0) 0]  ; 0 * log(0/q) = 0
                               [(<= qi 0) +inf.0]  ; p * log(p/0) = inf
                               [else (* pi (log2 (/ pi qi)))]))
                      p q))))

;;; symmetric-kl : (List Number) × (List Number) → Number
;;; Symmetric KL divergence: (D_KL(P||Q) + D_KL(Q||P)) / 2
(define (symmetric-kl p q)
  (/ (+ (kl-divergence p q) (kl-divergence q p)) 2))

;;; jensen-shannon-divergence : (List Number) × (List Number) → Number
;;; Jensen-Shannon divergence: JSD(P||Q) = (D_KL(P||M) + D_KL(Q||M)) / 2
;;; where M = (P + Q) / 2.
;;; This is symmetric and bounded [0, 1] in bits.
(define (jensen-shannon-divergence p q)
  (let ([m (map (lambda (pi qi) (/ (+ pi qi) 2)) p q)])
       (/ (+ (kl-divergence p m) (kl-divergence q m)) 2)))

;;; ====
;;; Continuous Entropy (Differential Entropy)
;;; ====

;;; For continuous distributions, we compute differential entropy
;;; using numerical integration or closed-form expressions.

;;; gaussian-entropy : Number → Number
;;; Differential entropy of Gaussian with standard deviation sigma.
;;; h(X) = (1/2) * log2(2 * pi * e * sigma^2)
(define (gaussian-entropy sigma)
  (let ([e-val (exp-num 1)])
       (* 0.5 (log2 (* 2 pi e-val (* sigma sigma))))))

;;; uniform-entropy : Number × Number → Number
;;; Differential entropy of uniform distribution on [a, b].
;;; h(X) = log2(b - a)
(define (uniform-entropy a b)
  (let ([width (- b a)])
       (if (<= width 0)
           -inf.0
           (log2 width))))

;;; exponential-entropy : Number → Number
;;; Differential entropy of exponential with rate lambda (in bits).
;;; h(X) = log2(e/lambda) = log2(e) - log2(lambda)
(define (exponential-entropy lambda)
  (if (<= lambda 0)
      +inf.0
      (- (log2 (exp-num 1)) (log2 lambda))))

;;; laplace-entropy : Number → Number
;;; Differential entropy of Laplace with scale b (in bits).
;;; h(X) = log2(2*b*e) = log2(e) + log2(2b)
(define (laplace-entropy b)
  (if (<= b 0)
      -inf.0
      (+ (log2 (exp-num 1)) (log2 (* 2 b)))))

;;; ====
;;; Entropy Estimation from Samples
;;; ====

;;; sample-entropy : (List α) → Number
;;; Estimate entropy from empirical frequency counts.
;;; Input: list of observed values (any type supporting eq?).
(define (sample-entropy samples)
  (if (null? samples)
      0
      (let* ([counts (count-occurrences samples)]
             [total (length samples)]
             [probs (map (lambda (c) (/ c total)) (map cdr counts))])
            (entropy probs))))

;;; count-occurrences : (List α) → (List (Pair α Nat))
;;; Count frequency of each unique element.
(define (count-occurrences lst)
  (let loop ([remaining lst] [counts '()])
       (if (null? remaining)
           counts
           (let* ([item (car remaining)]
                  [existing (assoc-helper item counts)])
                 (if existing
                     (loop (cdr remaining)
                           (update-count item counts))
                     (loop (cdr remaining)
                           (cons (cons item 1) counts)))))))

;;; assoc-helper : α × (List (Pair α β)) → (Pair α β) + Bool
(define (assoc-helper key alist)
  (cond
   [(null? alist) #f]
   [(equal? key (caar alist)) (car alist)]
   [else (assoc-helper key (cdr alist))]))

;;; update-count : α × (List (Pair α Nat)) → (List (Pair α Nat))
(define (update-count key alist)
  (map (lambda (pair)
               (if (equal? key (car pair))
                   (cons key (+ 1 (cdr pair)))
                   pair))
       alist))

;;; ====
;;; Variation of Information
;;; ====

;;; variation-of-information : (List (List Number)) × (List Number) × (List Number) → Number
;;; VI(X,Y) = H(X|Y) + H(Y|X) = H(X,Y) - I(X;Y)
;;; A true metric on clusterings/partitions.
(define (variation-of-information joint-probs marginal-x marginal-y)
  (let ([h-joint (joint-entropy joint-probs)]
        [mi (mutual-information joint-probs marginal-x marginal-y)])
       (- h-joint mi)))

;;; normalized-mutual-information : (List (List Number)) × (List Number) × (List Number) → Number
;;; NMI(X,Y) = I(X;Y) / sqrt(H(X) * H(Y))
;;; Normalized to [0, 1].
(define (normalized-mutual-information joint-probs marginal-x marginal-y)
  (let ([mi (mutual-information joint-probs marginal-x marginal-y)]
        [h-x (entropy marginal-x)]
        [h-y (entropy marginal-y)])
       (if (or (<= h-x 0) (<= h-y 0))
           0
           (/ mi (sqrt (* h-x h-y))))))

;;; ====
;;; Renyi Entropy
;;; ====

;;; renyi-entropy : Number × (List Number) → Number
;;; Renyi entropy of order alpha: H_alpha(X) = (1/(1-alpha)) * log2(sum(p_i^alpha))
;;; Special cases:
;;;   alpha -> 0: Hartley entropy (log of support size)
;;;   alpha -> 1: Shannon entropy (limit)
;;;   alpha = 2: Collision entropy
;;;   alpha -> inf: Min-entropy
;;; Returns +inf.0 for invalid alpha (negative values).
(define (renyi-entropy alpha probs)
  (cond
   [(< alpha 0) +inf.0]  ; Undefined for negative alpha
   [(= alpha 1) (entropy probs)]  ; Shannon entropy as limit
   [(= alpha 0) (log2 (length (filter (lambda (p) (> p 0)) probs)))]  ; Hartley
   [else
    (let ([sum-p-alpha (fold-left + 0 (map (lambda (p) (expt p alpha)) probs))])
         (* (/ 1 (- 1 alpha)) (log2 sum-p-alpha)))]))

;;; collision-entropy : (List Number) → Number
;;; Renyi entropy of order 2: H_2(X) = -log2(sum(p_i^2))
(define (collision-entropy probs)
  (renyi-entropy 2 probs))

;;; min-entropy : (List Number) → Number
;;; Renyi entropy of order infinity: H_inf(X) = -log2(max(p_i))
(define (min-entropy probs)
  (if (null? probs)
      0
      (- (log2 (apply max probs)))))

;;; hartley-entropy : (List Number) → Number
;;; Renyi entropy of order 0: H_0(X) = log2(|support|)
(define (hartley-entropy probs)
  (renyi-entropy 0 probs))

;;; ====
;;; Information Content
;;; ====

;;; surprisal : Number → Number
;;; Self-information / surprisal of an event with probability p.
;;; I(p) = -log2(p)
(define (surprisal p)
  (if (<= p 0)
      +inf.0
      (- (log2 p))))

;;; pointwise-mutual-information : Number × Number × Number → Number
;;; PMI(x,y) = log2(P(x,y) / (P(x) * P(y)))
(define (pointwise-mutual-information p-xy p-x p-y)
  (cond
   [(<= p-xy 0) -inf.0]
   [(or (<= p-x 0) (<= p-y 0)) +inf.0]
   [else (log2 (/ p-xy (* p-x p-y)))]))

;;; normalized-pmi : Number × Number × Number → Number
;;; NPMI(x,y) = PMI(x,y) / (-log2(P(x,y)))
;;; Normalized to [-1, 1].
(define (normalized-pmi p-xy p-x p-y)
  (let ([pmi (pointwise-mutual-information p-xy p-x p-y)]
        [denom (surprisal p-xy)])
       (if (or (= denom +inf.0) (= denom 0))
           0
           (/ pmi denom))))

;;; ====
;;; Entropy Powers and Relations
;;; ====

;;; entropy-power : Number → Number
;;; Entropy power of a distribution with differential entropy h.
;;; N(X) = (1/(2*pi*e)) * 2^(2h)
(define (entropy-power h)
  (let ([e-val (exp-num 1)])
       (* (/ 1 (* 2 pi e-val))
          (expt 2 (* 2 h)))))

