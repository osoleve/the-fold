(load "core/base/prelude.ss")
(load "lattice/fp/numeric/transcendental.ss")

(doc 'module 'entropy)
(doc 'description "Information-theoretic entropy and divergence measures")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'section 'constants)

(define pi 3.14159265358979323846)

(doc 'section 'utility-functions)

(define (entropy-log2 x)
  (doc 'type '(-> Number Number))
  (doc 'description "Logarithm base 2 with convention 0*log(0) = 0")
  (if (<= x 0)
      0  ; Convention: 0 * log(0) = 0
      (log2 x)))

(define safe-log2 entropy-log2)
(doc safe-log2 'type '(-> Number Number))
(doc safe-log2 'description "Alias for entropy-log2")

(define (any2 pred xs ys)
  (doc 'type '(-> (-> α β Bool) (List α) (List β) Bool))
  (doc 'description "Check if predicate holds for any pair of corresponding elements")
  (cond
   [(null? xs) #f]
   [(null? ys) #f]
   [(pred (car xs) (car ys)) #t]
   [else (any2 pred (cdr xs) (cdr ys))]))

(define (plogp p)
  (doc 'type '(-> Number Number))
  (doc 'description "Compute p * log2(p) with convention 0 * log(0) = 0")
  (if (<= p 0)
      0
      (* p (entropy-log2 p))))

(doc 'section 'shannon-entropy)

(define (entropy probs)
  (doc 'type '(-> (List Number) Number))
  (doc 'description "Shannon entropy H(X) = -sum(p_i * log2(p_i))")
  (- (fold-left + 0 (map plogp probs))))

(define (entropy-normalized weights)
  (doc 'type '(-> (List Number) Number))
  (doc 'description "Shannon entropy with automatic normalization")
  (let* ([total (fold-left + 0 weights)]
         [probs (if (= total 0)
                    weights
                    (map (lambda (w) (/ w total)) weights))])
        (entropy probs)))

(define (max-entropy n)
  (doc 'type '(-> Nat Number))
  (doc 'description "Maximum entropy for n equally likely outcomes: H_max = log2(n)")
  (if (<= n 0)
      0
      (log2 n)))

(define (entropy-ratio probs)
  (doc 'type '(-> (List Number) Number))
  (doc 'description "Normalized entropy H(X)/H_max, between 0 and 1")
  (let ([n (length probs)]
        [h (entropy probs)])
       (if (<= n 1)
           0
           (/ h (max-entropy n)))))

(doc 'section 'binary-entropy)

(define (binary-entropy p)
  (doc 'type '(-> Number Number))
  (doc 'description "Binary entropy function H(p) = -p*log2(p) - (1-p)*log2(1-p)")
  (cond
   [(<= p 0) 0]
   [(>= p 1) 0]
   [else (- (+ (plogp p) (plogp (- 1 p))))]))

(doc 'section 'joint-entropy)

(define (joint-entropy joint-probs)
  (doc 'type '(-> (List (List Number)) Number))
  (doc 'description "Joint entropy H(X,Y) from joint probability matrix")
  (let ([flat (apply append joint-probs)])
       (entropy flat)))

(doc 'section 'conditional-entropy)

(define (conditional-entropy joint-probs marginal-y)
  (doc 'type '(-> (List (List Number)) (List Number) Number))
  (doc 'description "Conditional entropy H(X|Y) = H(X,Y) - H(Y)")
  (let ([h-joint (joint-entropy joint-probs)]
        [h-y (entropy marginal-y)])
       (- h-joint h-y)))

(define (conditional-entropy-direct conditional-probs weights-y)
  (doc 'type '(-> (List (List Number)) (List Number) Number))
  (doc 'description "Compute H(X|Y) directly from conditional probability distributions")
  (let ([total-weight (fold-left + 0 weights-y)])
       (if (<= total-weight 0)
           0  ; No conditioning events; return 0 entropy
           (let* ([normalized-weights (map (lambda (w) (/ w total-weight)) weights-y)]
                  [conditional-entropies (map entropy conditional-probs)])
                 (fold-left + 0
                            (map * normalized-weights conditional-entropies))))))

(doc 'section 'mutual-information)

(define (mutual-information joint-probs marginal-x marginal-y)
  (doc 'type '(-> (List (List Number)) (List Number) (List Number) Number))
  (doc 'description "Mutual information I(X;Y) = H(X) + H(Y) - H(X,Y)")
  (let ([h-x (entropy marginal-x)]
        [h-y (entropy marginal-y)]
        [h-joint (joint-entropy joint-probs)])
       (+ h-x (- h-y h-joint))))

(define (mutual-information-from-joint joint-probs)
  (doc 'type '(-> (List (List Number)) Number))
  (doc 'description "Compute I(X;Y) from joint distribution only (computes marginals automatically)")
  (let* ([marginal-x (compute-marginal-x joint-probs)]
         [marginal-y (compute-marginal-y joint-probs)])
        (mutual-information joint-probs marginal-x marginal-y)))

(define (compute-marginal-x joint-probs)
  (doc 'type '(-> (List (List Number)) (List Number)))
  (doc 'description "Compute marginal P(X) by summing rows")
  (map (lambda (row) (fold-left + 0 row)) joint-probs))

(define (compute-marginal-y joint-probs)
  (doc 'type '(-> (List (List Number)) (List Number)))
  (doc 'description "Compute marginal P(Y) by summing columns")
  (if (null? joint-probs)
      '()
      (let ([n-cols (length (car joint-probs))])
           (map (lambda (j)
                        (fold-left + 0
                                   (map (lambda (row) (list-ref row j))
                                        joint-probs)))
                (iota n-cols)))))

(define (iota n)
  (doc 'type '(-> Nat (List Nat)))
  (doc 'description "Generate list (0 1 2 ... n-1)")
  (if (<= n 0)
      '()
      (let loop ([i 0] [acc '()])
           (if (= i n)
               (reverse acc)
               (loop (+ i 1) (cons i acc))))))

(doc 'section 'cross-entropy-and-kl)

(define (cross-entropy p q)
  (doc 'type '(-> (List Number) (List Number) Number))
  (doc 'description "Cross entropy H(P,Q) = -sum(p_i * log2(q_i))")
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

(define (kl-divergence p q)
  (doc 'type '(-> (List Number) (List Number) Number))
  (doc 'description "Kullback-Leibler divergence D_KL(P||Q) = sum(p_i * log2(p_i/q_i))")
  (if (null? p)
      0
      (fold-left + 0
                 (map (lambda (pi qi)
                              (cond
                               [(<= pi 0) 0]  ; 0 * log(0/q) = 0
                               [(<= qi 0) +inf.0]  ; p * log(p/0) = inf
                               [else (* pi (log2 (/ pi qi)))]))
                      p q))))

(define (symmetric-kl p q)
  (doc 'type '(-> (List Number) (List Number) Number))
  (doc 'description "Symmetric KL divergence: (D_KL(P||Q) + D_KL(Q||P)) / 2")
  (/ (+ (kl-divergence p q) (kl-divergence q p)) 2))

(define (jensen-shannon-divergence p q)
  (doc 'type '(-> (List Number) (List Number) Number))
  (doc 'description "Jensen-Shannon divergence: symmetric and bounded [0, 1] in bits")
  (let ([m (map (lambda (pi qi) (/ (+ pi qi) 2)) p q)])
       (/ (+ (kl-divergence p m) (kl-divergence q m)) 2)))

(doc 'section 'differential-entropy)

(define (gaussian-entropy sigma)
  (doc 'type '(-> Number Number))
  (doc 'description "Differential entropy of Gaussian with standard deviation sigma")
  (let ([e-val (exp-num 1)])
       (* 0.5 (log2 (* 2 pi e-val (* sigma sigma))))))

(define (uniform-entropy a b)
  (doc 'type '(-> Number Number Number))
  (doc 'description "Differential entropy of uniform distribution on [a, b]")
  (let ([width (- b a)])
       (if (<= width 0)
           -inf.0
           (log2 width))))

(define (exponential-entropy lambda)
  (doc 'type '(-> Number Number))
  (doc 'description "Differential entropy of exponential with rate lambda")
  (if (<= lambda 0)
      +inf.0
      (- (log2 (exp-num 1)) (log2 lambda))))

(define (laplace-entropy b)
  (doc 'type '(-> Number Number))
  (doc 'description "Differential entropy of Laplace with scale b")
  (if (<= b 0)
      -inf.0
      (+ (log2 (exp-num 1)) (log2 (* 2 b)))))

(doc 'section 'entropy-estimation)

(define (sample-entropy samples)
  (doc 'type '(-> (List α) Number))
  (doc 'description "Estimate entropy from empirical frequency counts")
  (if (null? samples)
      0
      (let* ([counts (count-occurrences samples)]
             [total (length samples)]
             [probs (map (lambda (c) (/ c total)) (map cdr counts))])
            (entropy probs))))

(define (count-occurrences lst)
  (doc 'type '(-> (List α) (List (Pair α Nat))))
  (doc 'description "Count frequency of each unique element")
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

(define (assoc-helper key alist)
  (doc 'type '(-> α (List (Pair α β)) (Sum (Pair α β) Bool)))
  (cond
   [(null? alist) #f]
   [(equal? key (caar alist)) (car alist)]
   [else (assoc-helper key (cdr alist))]))

(define (update-count key alist)
  (doc 'type '(-> α (List (Pair α Nat)) (List (Pair α Nat))))
  (map (lambda (pair)
               (if (equal? key (car pair))
                   (cons key (+ 1 (cdr pair)))
                   pair))
       alist))

(doc 'section 'variation-of-information)

(define (variation-of-information joint-probs marginal-x marginal-y)
  (doc 'type '(-> (List (List Number)) (List Number) (List Number) Number))
  (doc 'description "VI(X,Y) = H(X|Y) + H(Y|X), a true metric on clusterings")
  (let ([h-joint (joint-entropy joint-probs)]
        [mi (mutual-information joint-probs marginal-x marginal-y)])
       (- h-joint mi)))

(define (normalized-mutual-information joint-probs marginal-x marginal-y)
  (doc 'type '(-> (List (List Number)) (List Number) (List Number) Number))
  (doc 'description "NMI(X,Y) = I(X;Y) / sqrt(H(X) * H(Y)), normalized to [0, 1]")
  (let ([mi (mutual-information joint-probs marginal-x marginal-y)]
        [h-x (entropy marginal-x)]
        [h-y (entropy marginal-y)])
       (if (or (<= h-x 0) (<= h-y 0))
           0
           (/ mi (sqrt (* h-x h-y))))))

(doc 'section 'renyi-entropy)

(define (renyi-entropy alpha probs)
  (doc 'type '(-> Number (List Number) Number))
  (doc 'description "Renyi entropy of order alpha, generalizes Shannon entropy")
  (cond
   [(< alpha 0) +inf.0]  ; Undefined for negative alpha
   [(= alpha 1) (entropy probs)]  ; Shannon entropy as limit
   [(= alpha 0) (log2 (length (filter (lambda (p) (> p 0)) probs)))]  ; Hartley
   [else
    (let ([sum-p-alpha (fold-left + 0 (map (lambda (p) (expt p alpha)) probs))])
         (* (/ 1 (- 1 alpha)) (log2 sum-p-alpha)))]))

(define (collision-entropy probs)
  (doc 'type '(-> (List Number) Number))
  (doc 'description "Renyi entropy of order 2: H_2(X) = -log2(sum(p_i^2))")
  (renyi-entropy 2 probs))

(define (min-entropy probs)
  (doc 'type '(-> (List Number) Number))
  (doc 'description "Renyi entropy of order infinity: H_inf(X) = -log2(max(p_i))")
  (if (null? probs)
      0
      (- (log2 (apply max probs)))))

(define (hartley-entropy probs)
  (doc 'type '(-> (List Number) Number))
  (doc 'description "Renyi entropy of order 0: H_0(X) = log2(|support|)")
  (renyi-entropy 0 probs))

(doc 'section 'information-content)

(define (surprisal p)
  (doc 'type '(-> Number Number))
  (doc 'description "Self-information / surprisal: I(p) = -log2(p)")
  (if (<= p 0)
      +inf.0
      (- (log2 p))))

(define (pointwise-mutual-information p-xy p-x p-y)
  (doc 'type '(-> Number Number Number Number))
  (doc 'description "PMI(x,y) = log2(P(x,y) / (P(x) * P(y)))")
  (cond
   [(<= p-xy 0) -inf.0]
   [(or (<= p-x 0) (<= p-y 0)) +inf.0]
   [else (log2 (/ p-xy (* p-x p-y)))]))

(define (normalized-pmi p-xy p-x p-y)
  (doc 'type '(-> Number Number Number Number))
  (doc 'description "NPMI(x,y) = PMI(x,y) / (-log2(P(x,y))), normalized to [-1, 1]")
  (let ([pmi (pointwise-mutual-information p-xy p-x p-y)]
        [denom (surprisal p-xy)])
       (if (or (= denom +inf.0) (= denom 0))
           0
           (/ pmi denom))))

(doc 'section 'entropy-powers)

(define (entropy-power h)
  (doc 'type '(-> Number Number))
  (doc 'description "Entropy power: N(X) = (1/(2*pi*e)) * 2^(2h)")
  (let ([e-val (exp-num 1)])
       (* (/ 1 (* 2 pi e-val))
          (expt 2 (* 2 h)))))

