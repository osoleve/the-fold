;;; entropy.sexp — Development tasks for lattice/info/entropy.ss
;;;
;;; Module: entropy (tier 0, depends on prelude, transcendental)
;;; 375 lines, ~29 exported functions
;;; Shannon entropy, binary entropy, cross entropy, KL divergence,
;;; Jensen-Shannon divergence, Renyi entropy, mutual information,
;;; surprisal, PMI, differential entropy.
;;;
;;; Git history:
;;;   c70955ec fix(entropy,bbs,repl): resolve 3 BBS issues
;;;   2696481c feat(module): migrate info/, automata/, number-theory/ to require

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement Shannon entropy
  ;; ──────────────────────────────────────────────
  ;; The foundation of information theory.

  (task
    (id "info/entropy/shannon-impl")
    (module entropy)
    (tier 0)
    (type implement)
    (description "Implement Shannon entropy: H(X) = -sum(p_i * log2(p_i)) for a probability distribution given as a list of probabilities. Use the convention 0 * log2(0) = 0 (since lim p→0 of p*log(p) = 0). For each probability p: if p <= 0, contribute 0; otherwise contribute p * log2(p). Sum these and negate. Use log2 from the transcendental module.")
    (context "Available: prelude (map, fold-left, +, *, -, <=), transcendental (log2). The input is a list of non-negative numbers summing to 1. The result is in bits (base-2 logarithm).")
    (before "(define (entropy probs)
  (doc 'type '(-> (List Number) Number))
  (doc 'description \"Shannon entropy H(X) = -sum(p_i * log2(p_i))\")
  ;; TODO: sum p*log2(p) with 0*log(0)=0 convention, then negate
  0)")
    (test-file "lattice/info/test-entropy.ss")
    (test-forms (";; Fair coin: H = 1 bit
(assert-true (< (abs (- (entropy '(0.5 0.5)) 1.0)) 1e-10))"
                 ";; Certain outcome: H = 0
(assert-true (< (abs (entropy '(1.0 0.0 0.0))) 1e-10))"
                 ";; Fair die: H = log2(6) ≈ 2.585
(let ([h (entropy '(1/6 1/6 1/6 1/6 1/6 1/6))])
  (assert-true (< (abs (- h (log2 6))) 1e-10)))"
                 ";; Biased coin: 0 < H < 1
(let ([h (entropy '(0.9 0.1))])
  (assert-true (> h 0))
  (assert-true (< h 1)))"))
    (available-modules (prelude transcendental))
    (hints ("Define a helper plogp: if p <= 0, return 0; else return (* p (log2 p))"
            "Map plogp over the probability list"
            "Sum with fold-left"
            "Negate the sum (entropy is non-negative)"))
    (reference-solution "(define (entropy probs)
  (let ([plogp (lambda (p) (if (<= p 0) 0 (* p (log2 p))))])
    (- (fold-left + 0 (map plogp probs)))))")
    (alternatives ())
    (source-commit "2696481c")
    (difficulty 3))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement binary-entropy
  ;; ──────────────────────────────────────────────
  ;; Special case for binary events.

  (task
    (id "info/entropy/binary-impl")
    (module entropy)
    (tier 0)
    (type implement)
    (description "Implement binary-entropy: compute H(p) = -p*log2(p) - (1-p)*log2(1-p) for a single probability p in [0,1]. This is the entropy of a Bernoulli random variable. Handle edge cases: H(0) = 0 and H(1) = 0 (since 0*log(0) = 0 by convention). The maximum H(0.5) = 1 bit. Use the same plogp helper pattern: if p <= 0, contribute 0; if p >= 1, contribute 0.")
    (context "Available: prelude (+, -, *, <=, >=), transcendental (log2). p is a number between 0 and 1 inclusive.")
    (before "(define (binary-entropy p)
  (doc 'type '(-> Number Number))
  (doc 'description \"Binary entropy: H(p) = -p*log2(p) - (1-p)*log2(1-p)\")
  ;; TODO: handle edge cases, compute formula
  0)")
    (test-file "lattice/info/test-entropy.ss")
    (test-forms ("(assert-equal 0 (binary-entropy 0))"
                 "(assert-equal 0 (binary-entropy 1))"
                 "(assert-true (< (abs (- (binary-entropy 0.5) 1.0)) 1e-10))"
                 "(assert-true (> (binary-entropy 0.3) 0))"
                 "(assert-true (< (binary-entropy 0.3) 1))"))
    (available-modules (prelude transcendental))
    (hints ("If p <= 0 or p >= 1, return 0"
            "Otherwise: -(p*log2(p) + (1-p)*log2(1-p))"
            "This is equivalent to (entropy (list p (- 1 p)))"))
    (reference-solution "(define (binary-entropy p)
  (cond
   [(<= p 0) 0]
   [(>= p 1) 0]
   [else (let ([plogp (lambda (x) (* x (log2 x)))])
           (- (+ (plogp p) (plogp (- 1 p)))))]))")
    (alternatives ())
    (source-commit "2696481c")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement KL divergence
  ;; ──────────────────────────────────────────────
  ;; Asymmetric divergence with support mismatch handling.

  (task
    (id "info/entropy/kl-divergence-impl")
    (module entropy)
    (tier 0)
    (type implement)
    (description "Implement KL divergence: D_KL(P||Q) = sum(p_i * log2(p_i / q_i)). For each pair (p_i, q_i): if p_i <= 0, contribute 0 (0*log(anything) = 0). If p_i > 0 but q_i <= 0, contribute +inf.0 (P has support where Q doesn't — infinite divergence). Otherwise contribute p_i * log2(p_i / q_i). Sum all contributions. Use map with two lists and fold-left to sum.")
    (context "Available: prelude (map, fold-left, +, *, /, <=, cond), transcendental (log2). P and Q are lists of the same length. +inf.0 is Scheme's positive infinity literal.")
    (before "(define (kl-divergence p q)
  (doc 'type '(-> (List Number) (List Number) Number))
  (doc 'description \"KL divergence D_KL(P||Q) = sum(p_i * log2(p_i/q_i))\")
  ;; TODO: handle 0/0, p>0/q=0, and normal cases
  0)")
    (test-file "lattice/info/test-entropy.ss")
    (test-forms (";; KL(P||P) = 0 for any P
(assert-true (< (abs (kl-divergence '(0.5 0.5) '(0.5 0.5))) 1e-10))"
                 ";; KL is non-negative
(assert-true (>= (kl-divergence '(0.9 0.1) '(0.5 0.5)) 0))"
                 ";; KL(P||Q) != KL(Q||P) in general (asymmetric)
(let ([d1 (kl-divergence '(0.9 0.1) '(0.5 0.5))]
      [d2 (kl-divergence '(0.5 0.5) '(0.9 0.1))])
  (assert-false (< (abs (- d1 d2)) 1e-10)))"
                 ";; Support mismatch: P has weight where Q = 0 → +inf
(assert-true (= +inf.0 (kl-divergence '(0.5 0.5) '(1.0 0.0))))"))
    (available-modules (prelude transcendental))
    (hints ("Use map with a lambda taking (pi qi) to compute per-element contributions"
            "Three cases: p<=0 → 0, q<=0 → +inf.0, else → p * log2(p/q)"
            "Sum with fold-left"
            "Empty P → return 0"))
    (reference-solution "(define (kl-divergence p q)
  (if (null? p)
      0
      (fold-left + 0
                 (map (lambda (pi qi)
                              (cond
                               [(<= pi 0) 0]
                               [(<= qi 0) +inf.0]
                               [else (* pi (log2 (/ pi qi)))]))
                      p q))))")
    (alternatives ())
    (source-commit "2696481c")
    (difficulty 4))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement Jensen-Shannon divergence
  ;; ──────────────────────────────────────────────
  ;; Symmetric, bounded divergence using KL.

  (task
    (id "info/entropy/js-divergence-impl")
    (module entropy)
    (tier 0)
    (type implement)
    (description "Implement Jensen-Shannon divergence: JSD(P||Q) = (D_KL(P||M) + D_KL(Q||M)) / 2, where M = (P + Q) / 2 is the mixture distribution. For each i: m_i = (p_i + q_i) / 2. Then compute both KL(P||M) and KL(Q||M) and average them. JSD is symmetric (JSD(P||Q) = JSD(Q||P)) and bounded in [0, 1] bits. Use the existing kl-divergence function.")
    (context "Available: prelude (map, +, /), kl-divergence (computes KL divergence between two distributions).")
    (before "(define (jensen-shannon-divergence p q)
  (doc 'type '(-> (List Number) (List Number) Number))
  (doc 'description \"Jensen-Shannon divergence: symmetric, bounded [0,1]\")
  ;; TODO: compute mixture M, then average KL(P||M) and KL(Q||M)
  0)")
    (test-file "lattice/info/test-entropy.ss")
    (test-forms (";; JSD(P||P) = 0
(assert-true (< (abs (jensen-shannon-divergence '(0.5 0.5) '(0.5 0.5))) 1e-10))"
                 ";; JSD is symmetric
(let ([d1 (jensen-shannon-divergence '(0.9 0.1) '(0.5 0.5))]
      [d2 (jensen-shannon-divergence '(0.5 0.5) '(0.9 0.1))])
  (assert-true (< (abs (- d1 d2)) 1e-10)))"
                 ";; JSD is bounded [0, 1]
(let ([d (jensen-shannon-divergence '(1.0 0.0) '(0.0 1.0))])
  (assert-true (>= d 0))
  (assert-true (<= d 1.0001)))"))
    (available-modules (prelude transcendental))
    (hints ("Compute mixture: M = (map (lambda (pi qi) (/ (+ pi qi) 2)) p q)"
            "Compute KL(P||M) and KL(Q||M) using kl-divergence"
            "Return (/ (+ kl-pm kl-qm) 2)"))
    (reference-solution "(define (jensen-shannon-divergence p q)
  (let ([m (map (lambda (pi qi) (/ (+ pi qi) 2)) p q)])
       (/ (+ (kl-divergence p m) (kl-divergence q m)) 2)))")
    (alternatives ())
    (source-commit "2696481c")
    (difficulty 4))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Implement Renyi entropy
  ;; ──────────────────────────────────────────────
  ;; Generalization with special cases at alpha = 0, 1, infinity.

  (task
    (id "info/entropy/renyi-impl")
    (module entropy)
    (tier 0)
    (type implement)
    (description "Implement Renyi entropy of order alpha: H_alpha(X) = (1/(1-alpha)) * log2(sum(p_i^alpha)). Handle special cases: alpha = 1 reduces to Shannon entropy (use the entropy function), alpha = 0 gives Hartley entropy H_0 = log2(|support|) where |support| counts probabilities > 0, alpha < 0 returns +inf.0 (undefined). For all other alpha: compute sum of p_i^alpha using expt, then apply the formula.")
    (context "Available: prelude (fold-left, map, filter, length, +, *, /, =, <, expt), transcendental (log2), entropy (Shannon entropy). expt raises a number to a power: (expt base exp).")
    (before "(define (renyi-entropy alpha probs)
  (doc 'type '(-> Number (List Number) Number))
  (doc 'description \"Renyi entropy of order alpha, generalizes Shannon entropy\")
  ;; TODO: handle special cases (alpha=0, alpha=1, alpha<0), then general formula
  0)")
    (test-file "lattice/info/test-entropy.ss")
    (test-forms (";; Alpha=1 → Shannon entropy
(assert-true (< (abs (- (renyi-entropy 1 '(0.5 0.5)) 1.0)) 1e-10))"
                 ";; Alpha=0 → Hartley entropy = log2(|support|)
(assert-true (< (abs (- (renyi-entropy 0 '(0.5 0.5 0.0)) (log2 2))) 1e-10))"
                 ";; Alpha=2 → collision entropy
(let ([h2 (renyi-entropy 2 '(0.5 0.5))])
  (assert-true (< (abs (- h2 1.0)) 1e-10)))"
                 ";; For uniform distribution, all Renyi entropies are equal
(let ([h1 (renyi-entropy 1 '(0.25 0.25 0.25 0.25))]
      [h2 (renyi-entropy 2 '(0.25 0.25 0.25 0.25))])
  (assert-true (< (abs (- h1 h2)) 1e-10)))"))
    (available-modules (prelude transcendental))
    (hints ("If alpha < 0: return +inf.0"
            "If alpha = 1: return (entropy probs) — Shannon as limit"
            "If alpha = 0: count non-zero probabilities with (filter (lambda (p) (> p 0)) probs), return log2 of that count"
            "Otherwise: sum = fold-left + 0 (map (lambda (p) (expt p alpha)) probs), return (* (/ 1 (- 1 alpha)) (log2 sum))"))
    (reference-solution "(define (renyi-entropy alpha probs)
  (cond
   [(< alpha 0) +inf.0]
   [(= alpha 1) (entropy probs)]
   [(= alpha 0) (log2 (length (filter (lambda (p) (> p 0)) probs)))]
   [else
    (let ([sum-p-alpha (fold-left + 0 (map (lambda (p) (expt p alpha)) probs))])
         (* (/ 1 (- 1 alpha)) (log2 sum-p-alpha)))]))")
    (alternatives ())
    (source-commit "2696481c")
    (difficulty 4))

) ;; end tasks
