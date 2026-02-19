;;; channel-capacity.sexp — Development tasks for lattice/info/channel-capacity.ss
;;;
;;; Module: channel-capacity (tier 0, depends on entropy)
;;; 346 lines, ~25 exported functions
;;; Channel capacity for BSC, BEC, AWGN, Z-channel, q-ary symmetric channels.
;;; Includes Blahut-Arimoto algorithm, DMC mutual information, error exponents,
;;; and dB/linear conversions.
;;;
;;; Git history:
;;;   a3abed54 refactor: Reorganize into core/lattice/shell three-layer architecture
;;;   2696481c feat(module): migrate info/, automata/, number-theory/ to require

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement bec-capacity
  ;; ──────────────────────────────────────────────
  ;; Simplest channel capacity formula — linear in erasure probability.

  (task
    (id "info/channel-capacity/bec-capacity-impl")
    (module channel-capacity)
    (tier 0)
    (type implement)
    (description "Implement bec-capacity: compute the channel capacity of a Binary Erasure Channel (BEC) with erasure probability epsilon. In a BEC, each transmitted bit is either received correctly (with probability 1-epsilon) or erased (with probability epsilon). The erased bits carry no information, so capacity = 1 - epsilon. Handle edge cases: epsilon <= 0 means perfect channel (capacity 1.0), epsilon >= 1 means all bits erased (capacity 0.0). This is the simplest non-trivial channel capacity formula.")
    (context "Available: <=, >=, -, cond. Pure arithmetic. No dependencies beyond prelude.")
    (before "(define (bec-capacity epsilon)
  (doc 'export #t)
  (doc 'type '(-> Real Real))
  (doc 'description \"Channel capacity of BEC with erasure probability epsilon\")
  ;; TODO: capacity = 1 - epsilon with edge case handling
  0)")
    (test-file "lattice/info/test-channel-capacity.ss")
    (test-forms (";; Perfect channel
(assert-true (< (abs (- (bec-capacity 0) 1.0)) 0.001))"
                 ";; All erased
(assert-true (< (abs (bec-capacity 1)) 0.001))"
                 ";; Half erasure
(assert-true (< (abs (- (bec-capacity 0.5) 0.5)) 0.001))"
                 ";; Linear: epsilon=0.3 -> capacity=0.7
(assert-true (< (abs (- (bec-capacity 0.3) 0.7)) 0.001))"))
    (available-modules (prelude))
    (hints ("Three cases with cond: epsilon <= 0, epsilon >= 1, else"
            "The else case is simply (- 1 epsilon)"
            "This is the simplest capacity formula — it's linear"))
    (reference-solution "(define (bec-capacity epsilon)
  (cond
   [(<= epsilon 0) 1.0]
   [(>= epsilon 1) 0.0]
   [else (- 1 epsilon)]))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement bsc-capacity
  ;; ──────────────────────────────────────────────
  ;; Uses binary entropy — the canonical example of Shannon's theorem.

  (task
    (id "info/channel-capacity/bsc-capacity-impl")
    (module channel-capacity)
    (tier 0)
    (type implement)
    (description "Implement bsc-capacity: compute the channel capacity of a Binary Symmetric Channel (BSC) with crossover probability p. In a BSC, each transmitted bit is flipped with probability p. The capacity is C = 1 - H(p), where H(p) is the binary entropy function. Edge cases: p <= 0 or p >= 1 means perfect channel (capacity 1.0, since all-flip is just an inversion). p = 0.5 means random output (capacity 0.0). The function binary-entropy is available from the entropy module.")
    (context "Available: binary-entropy (from entropy module), <=, >=, =, -, cond. The binary-entropy function computes H(p) = -p*log2(p) - (1-p)*log2(1-p).")
    (before "(define (bsc-capacity p)
  (doc 'export #t)
  (doc 'type '(-> Real Real))
  (doc 'description \"Channel capacity of BSC with crossover probability p\")
  ;; TODO: C = 1 - H(p) with edge cases
  0)")
    (test-file "lattice/info/test-channel-capacity.ss")
    (test-forms (";; Perfect channel (no errors)
(assert-true (< (abs (- (bsc-capacity 0) 1.0)) 0.001))"
                 ";; Random channel (p=0.5) — no information
(assert-true (< (abs (bsc-capacity 0.5)) 0.001))"
                 ";; Symmetry: C(p) = C(1-p)
(assert-true (< (abs (- (bsc-capacity 0.1) (bsc-capacity 0.9))) 0.001))"
                 ";; Known value: C(0.11) ≈ 0.5
(assert-true (< (abs (- (bsc-capacity 0.11) 0.5)) 0.02))"))
    (available-modules (prelude entropy))
    (hints ("Four cases: p <= 0 → 1.0, p >= 1 → 1.0, p = 0.5 → 0.0, else → 1 - H(p)"
            "Use (binary-entropy p) for H(p)"
            "The p=0.5 case returns 0.0 because H(0.5) = 1.0 exactly"
            "Both p=0 and p=1 give capacity 1.0 — all-flip is still a perfect channel"))
    (reference-solution "(define (bsc-capacity p)
  (cond
   [(<= p 0) 1.0]
   [(>= p 1) 1.0]
   [(= p 0.5) 0.0]
   [else (- 1 (binary-entropy p))]))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement awgn-capacity
  ;; ──────────────────────────────────────────────
  ;; Shannon-Hartley theorem — the most important result in communication theory.

  (task
    (id "info/channel-capacity/awgn-capacity-impl")
    (module channel-capacity)
    (tier 0)
    (type implement)
    (description "Implement awgn-capacity: compute the channel capacity of an Additive White Gaussian Noise (AWGN) channel with signal-to-noise ratio snr (in linear scale, not dB). The Shannon-Hartley theorem gives: C = 0.5 * log2(1 + SNR) bits per channel use. This is the single most important result in communication theory — it sets the fundamental limit on reliable communication rate. Handle edge case: snr <= 0 gives capacity 0.0. The function log2 is available from the entropy module.")
    (context "Available: log2 (base-2 logarithm from entropy module), *, +, <=, cond. Pure arithmetic.")
    (before "(define (awgn-capacity snr)
  (doc 'export #t)
  (doc 'type '(-> Real Real))
  (doc 'description \"AWGN channel capacity with signal-to-noise ratio (linear)\")
  ;; TODO: C = 0.5 * log2(1 + SNR)
  0)")
    (test-file "lattice/info/test-channel-capacity.ss")
    (test-forms (";; Zero SNR — no capacity
(assert-true (< (abs (awgn-capacity 0)) 0.001))"
                 ";; SNR=1 (0 dB) — C = 0.5 bits
(assert-true (< (abs (- (awgn-capacity 1) 0.5)) 0.001))"
                 ";; High SNR gives high capacity
(assert-true (> (awgn-capacity 100) 3))"
                 ";; Negative SNR treated as zero
(assert-true (< (abs (awgn-capacity -1)) 0.001))"))
    (available-modules (prelude entropy))
    (hints ("Two cases: snr <= 0 → 0.0, else → (* 0.5 (log2 (+ 1 snr)))"
            "The 0.5 factor is for a single real-valued channel"
            "log2(1 + SNR) is the Shannon formula in its most basic form"
            "This is a two-line function"))
    (reference-solution "(define (awgn-capacity snr)
  (cond
   [(<= snr 0) 0.0]
   [else (* 0.5 (log2 (+ 1 snr)))]))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement dmc-output-distribution
  ;; ──────────────────────────────────────────────
  ;; Matrix-vector product for probability distributions.

  (task
    (id "info/channel-capacity/dmc-output-impl")
    (module channel-capacity)
    (tier 0)
    (type implement)
    (description "Implement dmc-output-distribution: compute the output distribution P(Y) for a Discrete Memoryless Channel (DMC) given the transition matrix P(Y|X) and input distribution P(X). This is the matrix-vector product: P(Y=j) = sum over all x of P(Y=j|X=x) * P(X=x). The transition matrix is a list of lists where row i gives P(Y|X=i). The input distribution is a list of P(X=i). Return a list of P(Y=j) values. Use map over output indices, with an inner sum over input indices using fold-left.")
    (context "Available: map, fold-left, length, list-ref, car, null?, iota, +, *. The transition matrix is (List (List Real)), input-dist is (List Real).")
    (before "(define (dmc-output-distribution transition input-dist)
  (doc 'export #t)
  (doc 'type '(-> (List (List Real)) (List Real) (List Real)))
  (doc 'description \"Compute P(Y) = sum_x P(Y|X=x) * P(X=x)\")
  ;; TODO: for each output j, sum over inputs: P(Y|X=i,Y=j) * P(X=i)
  '())")
    (test-file "lattice/info/test-channel-capacity.ss")
    (test-forms (";; BSC-like matrix with uniform input
(let ([out (dmc-output-distribution '((0.9 0.1) (0.2 0.8)) '(0.5 0.5))])
  (assert-true (< (abs (- (car out) 0.55)) 0.001))
  (assert-true (< (abs (- (cadr out) 0.45)) 0.001)))"
                 ";; Output sums to 1.0
(let ([out (dmc-output-distribution '((0.9 0.1) (0.2 0.8)) '(0.5 0.5))])
  (assert-true (< (abs (- (fold-left + 0 out) 1.0)) 0.001)))"
                 ";; Correct number of outputs
(assert-equal 2 (length (dmc-output-distribution '((0.9 0.1) (0.2 0.8)) '(0.5 0.5))))"))
    (available-modules (prelude entropy))
    (hints ("Get n-outputs from (length (car transition))"
            "Get n-inputs from (length transition)"
            "For each output j: sum over i of P(Y=j|X=i) * P(X=i)"
            "P(Y=j|X=i) = (list-ref (list-ref transition i) j)"
            "P(X=i) = (list-ref input-dist i)"
            "Outer map over (iota n-outputs), inner fold-left over (iota n-inputs)"))
    (reference-solution "(define (dmc-output-distribution transition input-dist)
  (let* ([n-outputs (if (null? transition) 0 (length (car transition)))]
         [n-inputs (length transition)])
    (map (lambda (j)
           (fold-left + 0
                      (map (lambda (i)
                             (* (list-ref input-dist i)
                                (list-ref (list-ref transition i) j)))
                           (iota n-inputs))))
         (iota n-outputs))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 3))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Implement qary-symmetric-capacity
  ;; ──────────────────────────────────────────────
  ;; Generalization of BSC to q-ary alphabets.

  (task
    (id "info/channel-capacity/qary-symmetric-impl")
    (module channel-capacity)
    (tier 0)
    (type implement)
    (description "Implement qary-symmetric-capacity: compute the channel capacity of a q-ary symmetric channel. In this channel, each symbol from a q-symbol alphabet is either received correctly (probability 1-p) or replaced by one of the other q-1 symbols uniformly at random (total error probability p). The capacity formula is: C = log2(q) - H(p) - p*log2(q-1), where H(p) is the binary entropy. This generalizes the BSC: when q=2, it reduces to 1 - H(p). Edge cases: q <= 1 → 0, p <= 0 → log2(q), p >= 1 → 0.")
    (context "Available: log2 (from entropy), binary-entropy (from entropy), <=, >=, -, *, +, cond. Pure arithmetic.")
    (before "(define (qary-symmetric-capacity q p)
  (doc 'export #t)
  ;; TODO: log2(q) - H(p) - p*log2(q-1)
  0)")
    (test-file "lattice/info/test-channel-capacity.ss")
    (test-forms (";; q=2 reduces to BSC
(assert-true (< (abs (- (qary-symmetric-capacity 2 0.1) (bsc-capacity 0.1))) 0.01))"
                 ";; Perfect channel: p=0 → log2(q) bits
(assert-true (< (abs (- (qary-symmetric-capacity 4 0) (log2 4))) 0.001))"
                 ";; Completely random: p=1 → 0 bits
(assert-true (< (abs (qary-symmetric-capacity 4 1)) 0.001))"
                 ";; Degenerate alphabet: q=1 → 0 bits
(assert-equal 0 (qary-symmetric-capacity 1 0.5))"))
    (available-modules (prelude entropy))
    (hints ("Handle edge cases first: q <= 1, p <= 0, p >= 1"
            "Main formula: (- (log2 q) (+ (binary-entropy p) (* p (log2 (- q 1)))))"
            "When q=2: log2(2)=1, log2(q-1)=log2(1)=0, so it becomes 1 - H(p) = BSC formula"
            "The p*log2(q-1) term accounts for the additional confusion from having more symbols"))
    (reference-solution "(define (qary-symmetric-capacity q p)
  (cond
   [(<= q 1) 0]
   [(<= p 0) (log2 q)]
   [(>= p 1) 0]
   [else
    (- (log2 q)
       (+ (binary-entropy p)
          (* p (log2 (- q 1)))))]))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

) ;; end tasks
