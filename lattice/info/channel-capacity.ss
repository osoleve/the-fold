;;; core/info-theory/channel-capacity.ss — Channel Capacity Calculations
;;;
;;; Information-theoretic channel capacity for various channel models:
;;;   - Binary Symmetric Channel (BSC)
;;;   - Binary Erasure Channel (BEC)
;;;   - Z-Channel (asymmetric binary)
;;;   - Additive White Gaussian Noise (AWGN) Channel
;;;   - Discrete Memoryless Channels (DMC)
;;;
;;; Channel capacity C = max_{p(x)} I(X;Y) is the theoretical maximum
;;; rate at which information can be reliably transmitted.
;;;
;;; This is Core code: pure, total, assumes valid parameters.
;;;
;;; Dependencies:
;;;   - entropy.ss (Shannon entropy, binary entropy, mutual information)

(load "lattice/info/entropy.ss")

;;; ====
;;; Binary Symmetric Channel (BSC)
;;; ====
;;;
;;; The BSC flips each bit with probability p:
;;;   P(Y=0|X=0) = 1-p,  P(Y=1|X=0) = p
;;;   P(Y=0|X=1) = p,    P(Y=1|X=1) = 1-p
;;;
;;; Capacity: C = 1 - H(p) where H is binary entropy

;;; bsc-capacity : Real → Real
;;; Channel capacity of BSC with crossover probability p.
;;; Returns capacity in bits per channel use.
(define (bsc-capacity p)
  (cond
   [(<= p 0) 1.0]  ; No errors: perfect channel
   [(>= p 1) 1.0]  ; All flipped: still perfect (just inverted)
   [(= p 0.5) 0.0] ; Random output: no information
   [else (- 1 (binary-entropy p))]))

;;; bsc-transition-matrix : Real → (List (List Real))
;;; Transition probability matrix P(Y|X) for BSC.
(define (bsc-transition-matrix p)
  (list (list (- 1 p) p)
        (list p (- 1 p))))

;;; bsc-mutual-information : Real × Real → Real
;;; Mutual information I(X;Y) for BSC with input probability p-x (P(X=1)).
(define (bsc-mutual-information p-crossover p-x)
  (let* ([p-y0 (+ (* (- 1 p-x) (- 1 p-crossover))
                  (* p-x p-crossover))]
         [p-y1 (- 1 p-y0)]
         [h-y (binary-entropy p-y1)]
         [h-y-given-x (binary-entropy p-crossover)])
        (- h-y h-y-given-x)))

;;; ====
;;; Binary Erasure Channel (BEC)
;;; ====
;;;
;;; The BEC either transmits correctly or erases with probability epsilon:
;;;   P(Y=0|X=0) = 1-epsilon,  P(Y='?'|X=0) = epsilon
;;;   P(Y=1|X=1) = 1-epsilon,  P(Y='?'|X=1) = epsilon
;;;
;;; Capacity: C = 1 - epsilon

;;; bec-capacity : Real → Real
;;; Channel capacity of BEC with erasure probability epsilon.
;;; Returns capacity in bits per channel use.
(define (bec-capacity epsilon)
  (cond
   [(<= epsilon 0) 1.0]  ; No erasures: perfect channel
   [(>= epsilon 1) 0.0]  ; All erased: no information
   [else (- 1 epsilon)]))

;;; ====
;;; Z-Channel (Asymmetric Binary Channel)
;;; ====
;;;
;;; The Z-channel only corrupts 1->0, not 0->1:
;;;   P(Y=0|X=0) = 1,      P(Y=1|X=0) = 0
;;;   P(Y=0|X=1) = p,      P(Y=1|X=1) = 1-p
;;;
;;; Capacity: C = log2(1 + (1-p) * p^(p/(1-p)))  for p > 0
;;;           C = 1 for p = 0

;;; z-channel-capacity : Real → Real
;;; Channel capacity of Z-channel with crossover probability p (1->0 only).
(define (z-channel-capacity p)
  (cond
   [(<= p 0) 1.0]  ; No errors
   [(>= p 1) 0.0]  ; All 1s become 0s
   [else
    (let* ([exponent (/ p (- 1 p))]
           [term (expt p exponent)])
          (log2 (+ 1 (* (- 1 p) term))))]))

;;; z-channel-transition-matrix : Real → (List (List Real))
;;; Transition probability matrix P(Y|X) for Z-channel.
(define (z-channel-transition-matrix p)
  (list (list 1 0)
        (list p (- 1 p))))

;;; ====
;;; Additive White Gaussian Noise (AWGN) Channel
;;; ====
;;;
;;; The AWGN channel: Y = X + N where N ~ N(0, sigma^2)
;;; With power constraint E[X^2] <= P
;;;
;;; Capacity (Shannon-Hartley): C = (1/2) * log2(1 + SNR)
;;; where SNR = P / sigma^2 (signal-to-noise ratio)
;;;
;;; For bandwidth W: C = W * log2(1 + P/(N0*W))
;;; where N0 is noise spectral density

;;; awgn-capacity : Real → Real
;;; Channel capacity of AWGN channel with signal-to-noise ratio (linear).
;;; Returns capacity in bits per channel use.
(define (awgn-capacity snr)
  (cond
   [(<= snr 0) 0.0]  ; No signal power
   [else (* 0.5 (log2 (+ 1 snr)))]))

;;; awgn-capacity-db : Real → Real
;;; Channel capacity of AWGN channel with SNR in decibels.
(define (awgn-capacity-db snr-db)
  (awgn-capacity (db-to-linear snr-db)))

;;; awgn-capacity-bandwidth : Real × Real × Real → Real
;;; Shannon-Hartley theorem: C = W * log2(1 + P/(N0*W))
;;; Parameters:
;;;   bandwidth: channel bandwidth in Hz
;;;   signal-power: average signal power
;;;   noise-density: noise power spectral density (N0)
(define (awgn-capacity-bandwidth bandwidth signal-power noise-density)
  (let ([noise-power (* noise-density bandwidth)])
       (if (<= noise-power 0)
           +inf.0  ; Noiseless channel
           (* bandwidth (log2 (+ 1 (/ signal-power noise-power)))))))

;;; snr-for-capacity : Real → Real
;;; Required SNR (linear) to achieve target capacity in bits/use.
;;; Inverse of awgn-capacity: SNR = 2^(2C) - 1
(define (snr-for-capacity target-capacity)
  (- (expt 2 (* 2 target-capacity)) 1))

;;; ====
;;; Discrete Memoryless Channel (DMC)
;;; ====
;;;
;;; A DMC is defined by transition matrix P(Y|X).
;;; Capacity: C = max_{p(x)} I(X;Y)
;;;
;;; For symmetric channels, uniform input is optimal.
;;; For general DMC, use Blahut-Arimoto algorithm.

;;; dmc-mutual-information : (List (List Real)) × (List Real) → Real
;;; Compute I(X;Y) for DMC with transition matrix and input distribution.
;;; transition: P(Y|X) matrix where entry (i,j) = P(Y=j|X=i)
;;; input-dist: P(X) distribution
(define (dmc-mutual-information transition input-dist)
  (let* ([output-dist (dmc-output-distribution transition input-dist)]
         [joint (dmc-joint-distribution transition input-dist)])
        (mutual-information-from-joint joint)))

;;; dmc-output-distribution : (List (List Real)) × (List Real) → (List Real)
;;; Compute P(Y) = sum_x P(Y|X=x) * P(X=x)
(define (dmc-output-distribution transition input-dist)
  (let* ([n-outputs (if (null? transition) 0 (length (car transition)))]
         [n-inputs (length transition)])
        (map (lambda (j)
                     (fold-left + 0
                                (map (lambda (i)
                                             (* (list-ref input-dist i)
                                                (list-ref (list-ref transition i) j)))
                                     (iota n-inputs))))
             (iota n-outputs))))

;;; dmc-joint-distribution : (List (List Real)) × (List Real) → (List (List Real))
;;; Compute joint distribution P(X,Y) = P(Y|X) * P(X)
(define (dmc-joint-distribution transition input-dist)
  (map (lambda (i)
               (let ([p-x (list-ref input-dist i)]
                     [row (list-ref transition i)])
                    (map (lambda (p-y-given-x) (* p-x p-y-given-x)) row)))
       (iota (length transition))))

;;; dmc-capacity-symmetric : (List (List Real)) → Real
;;; Capacity of symmetric DMC (uniform input is optimal).
;;; A channel is symmetric if all rows are permutations of each other
;;; and all columns are permutations of each other.
(define (dmc-capacity-symmetric transition)
  (let* ([n-inputs (length transition)]
         [uniform (make-uniform-dist n-inputs)])
        (dmc-mutual-information transition uniform)))

;;; make-uniform-dist : Nat → (List Real)
;;; Create uniform distribution over n outcomes.
(define (make-uniform-dist n)
  (if (<= n 0)
      '()
      (let ([p (/ 1.0 n)])
           (map (lambda (_) p) (iota n)))))

;;; ====
;;; Blahut-Arimoto Algorithm
;;; ====
;;;
;;; Iterative algorithm to compute capacity of general DMC.
;;; Converges to C = max I(X;Y) from any initial distribution.

;;; blahut-arimoto : (List (List Real)) × Nat × Real → (List Real) × Real
;;; Compute channel capacity using Blahut-Arimoto algorithm.
;;; Returns: (optimal-input-dist . capacity)
;;; Parameters:
;;;   transition: P(Y|X) matrix
;;;   max-iter: maximum iterations
;;;   tolerance: convergence threshold
(define (blahut-arimoto transition max-iter tolerance)
  (let* ([n-inputs (length transition)]
         [init-dist (make-uniform-dist n-inputs)])
        (ba-iterate transition init-dist max-iter tolerance 0 0.0)))

;;; ba-iterate : (List (List Real)) × (List Real) × Nat × Real × Nat × Real → (List Real) × Real
;;; Internal iteration for Blahut-Arimoto.
(define (ba-iterate transition dist max-iter tolerance iter prev-capacity)
  (let* ([output-dist (dmc-output-distribution transition dist)]
         [phi (ba-compute-phi transition dist output-dist)]
         [new-dist (ba-update-dist phi)]
         [capacity (dmc-mutual-information transition new-dist)])
        (if (or (>= iter max-iter)
                (< (abs (- capacity prev-capacity)) tolerance))
            (cons new-dist capacity)
            (ba-iterate transition new-dist max-iter tolerance
                        (+ iter 1) capacity))))

;;; ba-compute-phi : (List (List Real)) × (List Real) × (List Real) → (List Real)
;;; Compute phi_i = exp(sum_j p(y_j|x_i) * log(p(y_j|x_i)/p(y_j)))
(define (ba-compute-phi transition input-dist output-dist)
  (map (lambda (row)
               (let ([terms (map (lambda (p-y-given-x p-y)
                                         (if (or (<= p-y-given-x 0) (<= p-y 0))
                                             0
                                             (* p-y-given-x (log2 (/ p-y-given-x p-y)))))
                                 row output-dist)])
                    (expt 2 (fold-left + 0 terms))))
       transition))

;;; ba-update-dist : (List Real) → (List Real)
;;; Update input distribution: p'(x) proportional to p(x) * phi(x)
(define (ba-update-dist phi)
  (let ([total (fold-left + 0 phi)])
       (if (<= total 0)
           phi  ; Avoid division by zero
           (map (lambda (p) (/ p total)) phi))))

;;; ====
;;; Capacity Bounds
;;; ====

;;; dmc-capacity-upper-bound : (List (List Real)) → Real
;;; Upper bound on capacity: log2(number of outputs)
(define (dmc-capacity-upper-bound transition)
  (if (null? transition)
      0
      (log2 (length (car transition)))))

;;; dmc-capacity-lower-bound : (List (List Real)) → Real
;;; Lower bound: capacity with uniform input.
(define (dmc-capacity-lower-bound transition)
  (dmc-capacity-symmetric transition))

;;; ====
;;; Special Channel Models
;;; ====

;;; qary-symmetric-capacity : Nat × Real → Real
;;; Capacity of q-ary symmetric channel.
;;; Each symbol is replaced by a uniformly random different symbol with prob p.
(define (qary-symmetric-capacity q p)
  (cond
   [(<= q 1) 0]
   [(<= p 0) (log2 q)]  ; Perfect channel
   [(>= p 1) 0]  ; Completely random output
   [else
    (- (log2 q)
       (+ (binary-entropy p)
          (* p (log2 (- q 1)))))]))

;;; parallel-channel-capacity : (List Real) → Real
;;; Capacity of parallel channels with individual capacities.
;;; Total capacity is sum of individual capacities.
(define (parallel-channel-capacity capacities)
  (fold-left + 0 capacities))

;;; cascaded-channel-capacity-bound : Real × Real → Real
;;; Upper bound on capacity of cascaded (serial) channels.
;;; C_total <= min(C1, C2) by data processing inequality.
(define (cascaded-channel-capacity-bound c1 c2)
  (min c1 c2))

;;; ====
;;; Rate-Capacity Relationships
;;; ====

;;; achievable-rate? : Real × Real → Boolean
;;; Check if rate R is achievable for channel with capacity C.
;;; By Shannon's theorem: R <= C is achievable with vanishing error.
(define (achievable-rate? rate capacity)
  (<= rate capacity))

;;; max-reliable-rate : Real → Real
;;; Maximum rate for reliable communication = capacity.
(define (max-reliable-rate capacity)
  capacity)

;;; spectral-efficiency : Real × Real → Real
;;; Spectral efficiency = Rate / Bandwidth (bits/s/Hz)
(define (spectral-efficiency rate bandwidth)
  (if (<= bandwidth 0)
      +inf.0
      (/ rate bandwidth)))

;;; ====
;;; Error Exponent (Reliability Function)
;;; ====
;;;
;;; For rates R < C, error probability decays as exp(-n * E(R))
;;; where E(R) is the error exponent and n is block length.

;;; bsc-random-coding-exponent : Real × Real → Real
;;; Random coding exponent for BSC at rate R.
;;; E_r(R) = max_p [I(X;Y) - R] for uniform distribution.
(define (bsc-random-coding-exponent p-crossover rate)
  (let ([capacity (bsc-capacity p-crossover)])
       (if (>= rate capacity)
           0.0  ; No positive exponent above capacity
           (- capacity rate))))

;;; awgn-sphere-packing-exponent : Real × Real → Real
;;; Sphere-packing (upper) bound exponent for AWGN.
;;; E_sp(R) for rates below capacity.
(define (awgn-sphere-packing-exponent snr rate)
  (let ([capacity (awgn-capacity snr)])
       (if (>= rate capacity)
           0.0
           ;; Simplified: for AWGN, E_sp approximates (C-R)/2 near capacity
           (* 0.5 (- capacity rate)))))

;;; ====
;;; Utility Functions
;;; ====

;;; db-to-linear : Real → Real
;;; Convert decibels to linear scale: 10^(dB/10)
(define (db-to-linear db)
  (expt 10 (/ db 10)))

;;; linear-to-db : Real → Real
;;; Convert linear to decibels: 10 * log10(x)
(define (linear-to-db x)
  (if (<= x 0)
      -inf.0
      (* 10 (log10 x))))

;;; log10 : Real → Real
;;; Logarithm base 10 (derived from log2).
(define (log10 x)
  (/ (log2 x) (log2 10)))

;;; ====
;;; Channel Capacity Summary
;;; ====

;;; channel-summary : Symbol × Real → String
;;; Generate human-readable capacity summary for common channels.
(define (channel-summary channel-type param)
  (case channel-type
        [(bsc)
         (format "BSC (p=~a): C = ~a bits/use" param (bsc-capacity param))]
        [(bec)
         (format "BEC (epsilon=~a): C = ~a bits/use" param (bec-capacity param))]
        [(z-channel)
         (format "Z-Channel (p=~a): C = ~a bits/use" param (z-channel-capacity param))]
        [(awgn)
         (format "AWGN (SNR=~a): C = ~a bits/use" param (awgn-capacity param))]
        [else
         (format "Unknown channel type: ~a" channel-type)]))
