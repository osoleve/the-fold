;;; lattice/random/variational-inference.ss — Variational Inference Engine
;;;
;;; Gradient-based approximate Bayesian inference using the Evidence Lower Bound.
;;; Connects The Fold's autodiff system to probabilistic programming for scalable
;;; inference over arbitrary generative models.
;;;
;;; This is Lattice code: pure, Tier 2, depends on autodiff and random.
;;;
;;; Features:
;;;   - Reparameterization trick for gradient flow through random samples
;;;   - Variational families (mean-field Gaussian, full covariance)
;;;   - ELBO computation with Monte Carlo estimation
;;;   - Automatic gradient-based optimization
;;;   - Convergence diagnostics and ELBO tracking
;;;
;;; Key insight: Variational inference transforms integration (hard) into
;;; optimization (tractable). Instead of computing p(z|x) directly, we find
;;; q*(z) = argmin_q KL(q(z) || p(z|x)) = argmax_q ELBO(q).
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - autodiff/reverse-diff.ss (gradient computation)
;;;   - random/prng.ss (sampling)
;;;   - random/distributions.ss (density functions)
;;;   - random/bayesian.ss (log-pdfs, KL divergences)

(load "core/base/prelude.ss")
(load "core/autodiff/reverse-diff.ss")
(load "lattice/random/prng.ss")
(load "lattice/random/distributions.ss")
(load "lattice/random/bayesian.ss")

;;; ====
;;; Local Utilities
;;; ====

;;; iota-vi : Nat → (List Nat)
;;; Generate (0 1 2 ... n-1). Using local name to avoid conflicts.
(define (iota-vi n)
  (let loop ([i 0] [acc '()])
       (if (>= i n)
           (reverse acc)
           (loop (+ i 1) (cons i acc)))))

;;; ====
;;; Variational Families
;;; ====
;;;
;;; A variational family is a parameterized distribution q_φ(z).
;;; We optimize φ to make q close to the true posterior.
;;;
;;; Each family provides:
;;;   - sample-reparam: Sample using reparameterization trick (gradients flow)
;;;   - log-prob: Log probability q(z|φ)
;;;   - entropy: Analytical entropy H[q] when available
;;;   - params: Current parameters φ

;;; ====
;;; Mean-Field Gaussian
;;; ====
;;;
;;; q(z) = N(z; μ, diag(σ²))
;;; Parameters: μ (mean vector), log-σ (log std-dev vector)
;;; Reparameterization: z = μ + σ ⊙ ε, where ε ~ N(0, I)

;;; make-mf-gaussian : (List Number) × (List Number) → VFamily
;;; Create mean-field Gaussian with given means and log-stds.
(define (make-mf-gaussian means log-stds)
  (list 'mf-gaussian means log-stds))

;;; mf-gaussian? : α → Boolean
(define (mf-gaussian? v)
  (and (pair? v) (eq? (car v) 'mf-gaussian)))

;;; mf-gaussian-means : VFamily → (List Number)
(define (mf-gaussian-means v) (cadr v))

;;; mf-gaussian-log-stds : VFamily → (List Number)
(define (mf-gaussian-log-stds v) (caddr v))

;;; mf-gaussian-stds : VFamily → (List Number)
(define (mf-gaussian-stds v)
  (map exp (mf-gaussian-log-stds v)))

;;; mf-gaussian-dim : VFamily → Nat
(define (mf-gaussian-dim v)
  (length (mf-gaussian-means v)))

;;; ====
;;; Reparameterization Trick
;;; ====
;;;
;;; The key insight: instead of sampling z ~ q(z|φ), we sample ε ~ p(ε)
;;; and compute z = g(ε, φ). This makes z a deterministic function of φ,
;;; allowing gradients to flow through the sample.
;;;
;;; For Gaussian: z = μ + σ * ε, where ε ~ N(0, 1)
;;;
;;; This is critical for variational inference — without it, we cannot
;;; compute ∇_φ E_q[f(z)] via backpropagation.

;;; sample-standard-normals : Nat × PRNG → (List Number) × PRNG
;;; Sample n independent standard normal variables.
(define (sample-standard-normals n prng)
  (let loop ([k n] [rng prng] [acc '()])
       (if (= k 0)
           (values (reverse acc) rng)
           (let* ([result (run-state (random-normal 0 1) rng)]
                  [sample (car result)]
                  [new-rng (cdr result)])
                 (loop (- k 1) new-rng (cons sample acc))))))

;;; mf-gaussian-reparam : VFamily × (List Number) → (List Number)
;;; Apply reparameterization: z = μ + σ * ε
;;; Works with both regular numbers and traced values for autodiff.
(define (mf-gaussian-reparam vfamily epsilons)
  (let ([means (mf-gaussian-means vfamily)]
        [log-stds (mf-gaussian-log-stds vfamily)])
       (map (lambda (mu log-s eps)
                    (+ mu (* (exp log-s) eps)))
            means log-stds epsilons)))

;;; mf-gaussian-reparam-traced : (List Traced) × (List Traced) × (List Number) → (List Traced)
;;; Reparameterization with traced values for gradient computation.
(define (mf-gaussian-reparam-traced t-means t-log-stds epsilons)
  (map (lambda (t-mu t-log-s eps)
               (traced-add t-mu
                           (traced-mul (traced-exp t-log-s)
                                       (make-traced-const eps #f))))
       t-means t-log-stds epsilons))

;;; sample-mf-gaussian : VFamily × PRNG → (List Number) × PRNG
;;; Sample from mean-field Gaussian (for evaluation, not training).
(define (sample-mf-gaussian vfamily prng)
  (let-values ([(epsilons new-prng) (sample-standard-normals (mf-gaussian-dim vfamily) prng)])
              (values (mf-gaussian-reparam vfamily epsilons) new-prng)))

;;; ====
;;; Log Probabilities for Variational Families
;;; ====

;;; mf-gaussian-log-prob : VFamily × (List Number) → Number
;;; Log probability under mean-field Gaussian: log q(z|μ, σ).
(define (mf-gaussian-log-prob vfamily z)
  (let ([means (mf-gaussian-means vfamily)]
        [log-stds (mf-gaussian-log-stds vfamily)])
       (fold-left + 0
                  (map (lambda (zi mui log-si)
                               (log-normal-pdf zi mui (exp (* 2 log-si))))
                       z means log-stds))))

;;; mf-gaussian-entropy : VFamily → Number
;;; Analytical entropy of mean-field Gaussian: H[q] = Σ (0.5 * log(2πe σ²))
(define (mf-gaussian-entropy vfamily)
  (let ([log-stds (mf-gaussian-log-stds vfamily)])
       (* (length log-stds)
          (+ 0.5 (* 0.5 (log (* 2 3.141592653589793)))
             (fold-left + 0 log-stds)))))

;;; ====
;;; Full Covariance Gaussian
;;; ====
;;;
;;; q(z) = N(z; μ, LL^T) where L is lower triangular (Cholesky factor)
;;; Parameters: μ (mean), L (Cholesky factor, stored as vector)
;;; Reparameterization: z = μ + L ε, where ε ~ N(0, I)

;;; make-full-gaussian : (List Number) × (List Number) → VFamily
;;; Create full covariance Gaussian.
;;; chol-flat is the lower triangular Cholesky factor stored row-major.
(define (make-full-gaussian means chol-flat)
  (list 'full-gaussian means chol-flat))

;;; full-gaussian? : α → Boolean
(define (full-gaussian? v)
  (and (pair? v) (eq? (car v) 'full-gaussian)))

;;; full-gaussian-means : VFamily → (List Number)
(define (full-gaussian-means v) (cadr v))

;;; full-gaussian-chol : VFamily → (List Number)
(define (full-gaussian-chol v) (caddr v))

;;; full-gaussian-dim : VFamily → Nat
(define (full-gaussian-dim v)
  (length (full-gaussian-means v)))

;;; chol-flat-to-matrix : (List Number) × Nat → (List (List Number))
;;; Convert flat Cholesky storage to lower triangular matrix.
(define (chol-flat-to-matrix flat n)
  (let loop ([i 0] [row 0] [col 0] [current-row '()] [rows '()] [remaining flat])
       (cond
        [(= row n) (reverse rows)]
        [(> col row)
         ;; Move to next row
         (loop i (+ row 1) 0 '() (cons (reverse current-row) rows) remaining)]
        [else
         ;; Add element from flat storage
         (loop (+ i 1) row (+ col 1)
               (cons (car remaining) current-row)
               rows
               (cdr remaining))])))

;;; chol-num-elements : Nat → Nat
;;; Number of elements in n×n lower triangular matrix.
(define (chol-num-elements n)
  (/ (* n (+ n 1)) 2))

;;; full-gaussian-reparam : VFamily × (List Number) → (List Number)
;;; z = μ + L * ε (matrix-vector multiplication with lower triangular L)
(define (full-gaussian-reparam vfamily epsilons)
  (let* ([means (full-gaussian-means vfamily)]
         [flat (full-gaussian-chol vfamily)]
         [n (length means)]
         [L (chol-flat-to-matrix flat n)])
        (map (lambda (mu-i L-row)
                     (+ mu-i
                        (fold-left + 0
                                   (map * L-row (take epsilons (length L-row))))))
             means L)))

;;; take : (List α) × Nat → (List α)
(define (take lst n)
  (if (or (null? lst) (<= n 0))
      '()
      (cons (car lst) (take (cdr lst) (- n 1)))))

;;; sample-full-gaussian : VFamily × PRNG → (List Number) × PRNG
(define (sample-full-gaussian vfamily prng)
  (let-values ([(epsilons new-prng) (sample-standard-normals (full-gaussian-dim vfamily) prng)])
              (values (full-gaussian-reparam vfamily epsilons) new-prng)))

;;; full-gaussian-log-prob : VFamily × (List Number) → Number
;;; Log probability under full covariance Gaussian.
;;; Uses: log N(z|μ,Σ) = -0.5 * (d*log(2π) + log|Σ| + (z-μ)^T Σ^{-1} (z-μ))
;;; With Σ = LL^T: log|Σ| = 2*Σ log(L_ii) and Σ^{-1} easy via triangular solve.
(define (full-gaussian-log-prob vfamily z)
  (let* ([means (full-gaussian-means vfamily)]
         [flat (full-gaussian-chol vfamily)]
         [n (length means)]
         [L (chol-flat-to-matrix flat n)]
         ;; Extract diagonal elements of L
         [diag (map (lambda (i row) (list-ref row i))
                    (iota-vi n)
                    L)]
         ;; log|Σ| = 2 * Σ log(L_ii)
         [log-det (* 2 (fold-left + 0 (map log diag)))]
         ;; Solve L*y = (z - μ), then ||y||² = (z-μ)^T Σ^{-1} (z-μ)
         [diff (map - z means)]
         [y (forward-solve L diff)]
         [mahal (fold-left + 0 (map (lambda (yi) (* yi yi)) y))])
        (- (- (* 0.5 n (log (* 2 3.141592653589793))))
           (* 0.5 log-det)
           (* 0.5 mahal))))

;;; forward-solve : (List (List Number)) × (List Number) → (List Number)
;;; Solve L*y = b for y, where L is lower triangular.
(define (forward-solve L b)
  (let loop ([rows L] [b-remaining b] [y-acc '()])
       (if (null? rows)
           (reverse y-acc)
           (let* ([row (car rows)]
                  [bi (car b-remaining)]
                  ;; y_i = (b_i - Σ_{j<i} L_ij y_j) / L_ii
                  [sum (fold-left + 0 (map * (butlast row) (reverse y-acc)))]
                  [L-ii (last row)]
                  [yi (/ (- bi sum) L-ii)])
                 (loop (cdr rows) (cdr b-remaining) (cons yi y-acc))))))

;;; butlast : (List α) → (List α)
(define (butlast lst)
  (if (or (null? lst) (null? (cdr lst)))
      '()
      (cons (car lst) (butlast (cdr lst)))))

;;; last : (List α) → α
(define (last lst)
  (if (null? (cdr lst))
      (car lst)
      (last (cdr lst))))

;;; full-gaussian-entropy : VFamily → Number
;;; H[N(μ, LL^T)] = 0.5 * d * (1 + log(2π)) + Σ log(L_ii)
(define (full-gaussian-entropy vfamily)
  (let* ([n (full-gaussian-dim vfamily)]
         [flat (full-gaussian-chol vfamily)]
         [L (chol-flat-to-matrix flat n)]
         [diag (map (lambda (i row) (list-ref row i)) (iota-vi n) L)]
         [log-diag-sum (fold-left + 0 (map log diag))])
        (+ (* 0.5 n (+ 1 (log (* 2 3.141592653589793))))
           log-diag-sum)))

;;; ====
;;; Generic Variational Family Interface
;;; ====

;;; vfamily-dim : VFamily → Nat
(define (vfamily-dim v)
  (cond [(mf-gaussian? v) (mf-gaussian-dim v)]
        [(full-gaussian? v) (full-gaussian-dim v)]
        [else (error 'vfamily-dim "Unknown variational family" v)]))

;;; vfamily-sample : VFamily × PRNG → (List Number) × PRNG
(define (vfamily-sample v prng)
  (cond [(mf-gaussian? v) (sample-mf-gaussian v prng)]
        [(full-gaussian? v) (sample-full-gaussian v prng)]
        [else (error 'vfamily-sample "Unknown variational family" v)]))

;;; vfamily-log-prob : VFamily × (List Number) → Number
(define (vfamily-log-prob v z)
  (cond [(mf-gaussian? v) (mf-gaussian-log-prob v z)]
        [(full-gaussian? v) (full-gaussian-log-prob v z)]
        [else (error 'vfamily-log-prob "Unknown variational family" v)]))

;;; vfamily-entropy : VFamily → Number
(define (vfamily-entropy v)
  (cond [(mf-gaussian? v) (mf-gaussian-entropy v)]
        [(full-gaussian? v) (full-gaussian-entropy v)]
        [else (error 'vfamily-entropy "Unknown variational family" v)]))

;;; vfamily-params : VFamily → (List Number)
;;; Get flat parameter vector.
(define (vfamily-params v)
  (cond [(mf-gaussian? v)
         (append (mf-gaussian-means v) (mf-gaussian-log-stds v))]
        [(full-gaussian? v)
         (append (full-gaussian-means v) (full-gaussian-chol v))]
        [else (error 'vfamily-params "Unknown variational family" v)]))

;;; vfamily-from-params : Symbol × Nat × (List Number) → VFamily
;;; Reconstruct variational family from flat parameters.
(define (vfamily-from-params type dim params)
  (case type
    [(mf-gaussian)
     (let ([means (take params dim)]
           [log-stds (drop params dim)])
          (make-mf-gaussian means log-stds))]
    [(full-gaussian)
     (let ([means (take params dim)]
           [chol (drop params dim)])
          (make-full-gaussian means chol))]
    [else (error 'vfamily-from-params "Unknown type" type)]))

;;; drop : (List α) × Nat → (List α)
(define (drop lst n)
  (if (or (null? lst) (<= n 0))
      lst
      (drop (cdr lst) (- n 1))))

;;; ====
;;; ELBO Computation
;;; ====
;;;
;;; The Evidence Lower Bound (ELBO) is:
;;;   L(φ) = E_q(z;φ)[log p(x,z)] - E_q(z;φ)[log q(z;φ)]
;;;        = E_q[log p(x,z)] + H[q]
;;;
;;; For stochastic optimization, we estimate the expectation with Monte Carlo:
;;;   L̂(φ) ≈ (1/K) Σ_k [log p(x, z_k) - log q(z_k; φ)]
;;;
;;; where z_k ~ q(z; φ) using the reparameterization trick.

;;; elbo-estimate : (z → Number) × VFamily × PRNG × Nat → Number × PRNG
;;; Estimate ELBO using K Monte Carlo samples.
;;; log-joint: function z → log p(x, z) (joint log probability)
(define (elbo-estimate log-joint vfamily prng K)
  (let loop ([k K] [rng prng] [total 0])
       (if (= k 0)
           (values (/ total K) rng)
           (let*-values ([(z new-rng) (vfamily-sample vfamily rng)]
                         [(log-p) (log-joint z)]
                         [(log-q) (vfamily-log-prob vfamily z)])
                        (loop (- k 1) new-rng (+ total (- log-p log-q)))))))

;;; elbo-with-entropy : (z → Number) × VFamily × PRNG × Nat → Number × PRNG
;;; Alternative ELBO computation using analytical entropy when available.
;;; L(φ) = E_q[log p(x,z)] + H[q]
(define (elbo-with-entropy log-joint vfamily prng K)
  (let* ([H (vfamily-entropy vfamily)])
        (let loop ([k K] [rng prng] [total 0])
             (if (= k 0)
                 (values (+ (/ total K) H) rng)
                 (let*-values ([(z new-rng) (vfamily-sample vfamily rng)])
                              (loop (- k 1) new-rng (+ total (log-joint z))))))))

;;; ====
;;; ELBO Gradient via Reparameterization
;;; ====
;;;
;;; Using the reparameterization trick, we can compute:
;;;   ∇_φ L = ∇_φ E_ε[log p(x, g(ε,φ)) - log q(g(ε,φ); φ)]
;;;
;;; Since g(ε,φ) is differentiable in φ, we use reverse-mode AD.

;;; elbo-gradient-mf : ((List Traced) → Traced) × (List Number) × (List Number) × (List Number) → (List Number)
;;; Compute gradient of ELBO w.r.t. mean-field Gaussian parameters.
;;; Input: traced-log-joint function (takes traced z, returns traced log p(x,z)),
;;;        means, log-stds, pre-sampled epsilons
;;; Output: gradients [∂L/∂μ_1, ..., ∂L/∂μ_d, ∂L/∂log_σ_1, ..., ∂L/∂log_σ_d]
;;;
;;; IMPORTANT: traced-log-joint must use traced arithmetic (traced-add, traced-mul, etc.)
;;; to allow gradients to flow through log p(x,z).
(define (elbo-gradient-mf traced-log-joint means log-stds epsilons)
  (let* ([d (length means)]
         [params (append means log-stds)])
        ;; gradient's f receives individual traced arguments via (apply f traced-args)
        ;; So we use a variadic lambda to collect them back into a list
        (gradient
         (lambda traced-params-spread  ; Variadic: collects all args into list
                 ;; Split traced params into means and log-stds
                 (let* ([t-means (take-traced traced-params-spread d)]
                        [t-log-stds (drop-traced traced-params-spread d)]
                        ;; Reparameterize: z = μ + exp(log_σ) * ε
                        [t-z (map (lambda (t-mu t-log-s eps)
                                          (traced-add t-mu
                                                      (traced-mul (traced-exp t-log-s)
                                                                  (make-traced-const eps #f))))
                                  t-means t-log-stds epsilons)]
                        ;; Compute log p(x, z) using TRACED z values
                        [t-log-p (traced-log-joint t-z)]
                        ;; Compute log q(z; μ, σ) with traced parameters
                        [z-vals (map traced-value t-z)]  ; Need primal for log q
                        [t-log-q (sum-traced
                                  (map (lambda (zi t-mu t-log-s)
                                               (traced-log-normal-pdf zi t-mu t-log-s))
                                       z-vals t-means t-log-stds))])
                       ;; ELBO contribution: log p - log q
                       (traced-sub t-log-p t-log-q)))
         params)))

;;; ====
;;; Traced Log-Joint Construction
;;; ====
;;;
;;; For gradient-based VI, the log-joint must use traced arithmetic.
;;; These helpers make it easy to build traced log-joints.

;;; traced-log-normal-pdf-sum : (List Number) × Traced × Number → Traced
;;; Sum of log N(x_i | mu, variance) for observations x_i
;;; log N(x|mu,var) = -0.5*log(2*pi*var) - 0.5*(x-mu)^2/var
(define (traced-log-normal-pdf-sum observations t-mu variance)
  (let ([log-norm-const (make-traced-const (* -0.5 (log (* 2 3.141592653589793 variance))) #f)]
        [half (make-traced-const 0.5 #f)]
        [var-const (make-traced-const variance #f)])
       (let loop ([obs observations] [acc (make-traced-const 0.0 #f)])
            (if (null? obs)
                acc
                (let* ([xi (car obs)]
                       [xi-const (make-traced-const xi #f)]
                       [diff (traced-sub xi-const t-mu)]
                       [diff-sq (traced-mul diff diff)]
                       [scaled (traced-div diff-sq var-const)]
                       ;; log N(x|mu,var) = const - 0.5*(x-mu)^2/var
                       [term (traced-sub log-norm-const (traced-mul half scaled))])
                      (loop (cdr obs) (traced-add acc term)))))))

;;; make-traced-log-joint-normal-mean : (List Number) × Number × Number → ((List Traced) → Traced)
;;; Create a traced log-joint for inferring the mean of a normal distribution.
;;; Model: mu ~ N(prior-mean, prior-var), x_i ~ N(mu, known-var)
(define (make-traced-log-joint-normal-mean observations known-var prior-mean prior-var)
  (lambda (t-z)
    (let* ([t-mu (car t-z)]
           ;; Prior: log N(mu | prior-mean, prior-var)
           [prior-mean-const (make-traced-const prior-mean #f)]
           [t-prior (traced-log-normal-pdf (traced-value t-mu) prior-mean-const
                                           (make-traced-const (log (sqrt prior-var)) #f))]
           ;; Likelihood: sum of log N(x_i | mu, known-var)
           [t-likelihood (traced-log-normal-pdf-sum observations t-mu known-var)])
          (traced-add t-prior t-likelihood))))

;;; take-traced : (List Traced) × Nat → (List Traced)
(define (take-traced lst n)
  (if (or (null? lst) (<= n 0))
      '()
      (cons (car lst) (take-traced (cdr lst) (- n 1)))))

;;; drop-traced : (List Traced) × Nat → (List Traced)
(define (drop-traced lst n)
  (if (or (null? lst) (<= n 0))
      lst
      (drop-traced (cdr lst) (- n 1))))

;;; sum-traced : (List Traced) → Traced
(define (sum-traced lst)
  (if (null? lst)
      (make-traced-const 0 #f)
      (fold-left traced-add (car lst) (cdr lst))))

;;; traced-log-normal-pdf : Number × Traced × Traced → Traced
;;; Log normal PDF with traced mean and log-std parameters.
;;; log N(z|μ,σ²) = -0.5 * log(2πσ²) - (z-μ)²/(2σ²)
;;;              = -log(σ) - 0.5*log(2π) - (z-μ)²/(2σ²)
;;;              = -log_σ - 0.5*log(2π) - 0.5*(z-μ)²/σ²
(define (traced-log-normal-pdf z t-mu t-log-sigma)
  (let* ([const-term (make-traced-const (* -0.5 (log (* 2 3.141592653589793))) #f)]
         [z-const (make-traced-const z #f)]
         [diff (traced-sub z-const t-mu)]
         [diff-sq (traced-mul diff diff)]
         [var (traced-exp (traced-mul t-log-sigma (make-traced-const 2 #f)))]
         [mahal (traced-div diff-sq var)])
        (traced-sub
         (traced-sub const-term t-log-sigma)
         (traced-mul (make-traced-const 0.5 #f) mahal))))

;;; ====
;;; Stochastic Gradient Descent for VI
;;; ====

;;; vi-step-mf : (z → Number) × VFamily × PRNG × Number → VFamily × PRNG
;;; Single SGD step on mean-field Gaussian.
(define (vi-step-mf log-joint vfamily prng learning-rate)
  (let* ([d (mf-gaussian-dim vfamily)]
         [means (mf-gaussian-means vfamily)]
         [log-stds (mf-gaussian-log-stds vfamily)])
        ;; Sample noise for reparameterization
        (let-values ([(epsilons new-prng) (sample-standard-normals d prng)])
                    ;; Compute gradient
                    (let* ([grad (elbo-gradient-mf log-joint means log-stds epsilons)]
                           [grad-means (take grad d)]
                           [grad-log-stds (drop grad d)]
                           ;; Gradient ascent (maximize ELBO)
                           [new-means (map (lambda (m g) (+ m (* learning-rate g)))
                                           means grad-means)]
                           [new-log-stds (map (lambda (s g) (+ s (* learning-rate g)))
                                              log-stds grad-log-stds)])
                          (values (make-mf-gaussian new-means new-log-stds)
                                  new-prng)))))

;;; ====
;;; Adam Optimizer for VI
;;; ====
;;;
;;; Adam is the workhorse optimizer for variational inference.
;;; Adaptive learning rates help navigate the optimization landscape.

;;; make-adam-state : Nat → AdamState
;;; Initialize Adam optimizer state.
(define (make-adam-state num-params)
  (list 'adam-state
        (make-list num-params 0)    ; First moment (m)
        (make-list num-params 0)    ; Second moment (v)
        1))                          ; Time step t

;;; adam-state? : α → Boolean
(define (adam-state? x)
  (and (pair? x) (eq? (car x) 'adam-state)))

;;; adam-state-m : AdamState → (List Number)
(define (adam-state-m s) (cadr s))

;;; adam-state-v : AdamState → (List Number)
(define (adam-state-v s) (caddr s))

;;; adam-state-t : AdamState → Nat
(define (adam-state-t s) (cadddr s))

;;; make-list : Nat × α → (List α)
(define (make-list n val)
  (if (<= n 0) '() (cons val (make-list (- n 1) val))))

;;; adam-update : AdamState × (List Number) × Number × Number × Number → AdamState × (List Number)
;;; Compute Adam parameter updates.
;;; Returns: (new-state, update-deltas)
(define (adam-update state grads lr beta1 beta2 epsilon)
  (let* ([m (adam-state-m state)]
         [v (adam-state-v state)]
         [t (adam-state-t state)]
         ;; Update biased first moment estimate
         [new-m (map (lambda (mi gi)
                             (+ (* beta1 mi) (* (- 1 beta1) gi)))
                     m grads)]
         ;; Update biased second moment estimate
         [new-v (map (lambda (vi gi)
                             (+ (* beta2 vi) (* (- 1 beta2) (* gi gi))))
                     v grads)]
         ;; Bias correction
         [m-hat (map (lambda (mi) (/ mi (- 1 (expt beta1 t)))) new-m)]
         [v-hat (map (lambda (vi) (/ vi (- 1 (expt beta2 t)))) new-v)]
         ;; Compute update
         [deltas (map (lambda (mh vh)
                              (* lr (/ mh (+ (sqrt vh) epsilon))))
                      m-hat v-hat)]
         [new-state (list 'adam-state new-m new-v (+ t 1))])
        (values new-state deltas)))

;;; vi-step-adam : (z → Number) × VFamily × PRNG × AdamState × Number → VFamily × PRNG × AdamState
;;; Single Adam step for variational inference.
;;; Optional: beta1 (default 0.9), beta2 (default 0.999), epsilon (default 1e-8)
(define vi-step-adam
  (case-lambda
    [(log-joint vfamily prng adam-state lr)
     (vi-step-adam log-joint vfamily prng adam-state lr 0.9 0.999 1e-8)]
    [(log-joint vfamily prng adam-state lr beta1)
     (vi-step-adam log-joint vfamily prng adam-state lr beta1 0.999 1e-8)]
    [(log-joint vfamily prng adam-state lr beta1 beta2)
     (vi-step-adam log-joint vfamily prng adam-state lr beta1 beta2 1e-8)]
    [(log-joint vfamily prng adam-state lr beta1 beta2 eps)
     (let* ([d (vfamily-dim vfamily)]
            [params (vfamily-params vfamily)]
            [type (cond [(mf-gaussian? vfamily) 'mf-gaussian]
                        [(full-gaussian? vfamily) 'full-gaussian])])
           ;; Sample noise for reparameterization
           (let-values ([(epsilons new-prng) (sample-standard-normals d prng)])
                       ;; Compute gradient (only mean-field for now)
                       (let* ([grad (cond [(mf-gaussian? vfamily)
                                           (elbo-gradient-mf log-joint
                                                             (mf-gaussian-means vfamily)
                                                             (mf-gaussian-log-stds vfamily)
                                                             epsilons)]
                                          [else (error 'vi-step-adam
                                                       "Only mean-field Gaussian supported")])])
                             ;; Adam update (gradient ascent)
                             (let-values ([(new-adam deltas) (adam-update adam-state grad lr beta1 beta2 eps)])
                                         (let* ([new-params (map + params deltas)]
                                               [new-vfamily (vfamily-from-params type d new-params)])
                                              (values new-vfamily new-prng new-adam))))))]))

;;; vi-step-adam-traced : ((List Traced) → Traced) × VFamily × PRNG × AdamState × Number → VFamily × PRNG × AdamState
;;; Single Adam step using a traced log-joint for proper gradient flow.
(define vi-step-adam-traced
  (case-lambda
    [(traced-log-joint vfamily prng adam-state lr)
     (vi-step-adam-traced traced-log-joint vfamily prng adam-state lr 0.9 0.999 1e-8)]
    [(traced-log-joint vfamily prng adam-state lr beta1)
     (vi-step-adam-traced traced-log-joint vfamily prng adam-state lr beta1 0.999 1e-8)]
    [(traced-log-joint vfamily prng adam-state lr beta1 beta2)
     (vi-step-adam-traced traced-log-joint vfamily prng adam-state lr beta1 beta2 1e-8)]
    [(traced-log-joint vfamily prng adam-state lr beta1 beta2 eps)
     (let* ([d (vfamily-dim vfamily)]
            [params (vfamily-params vfamily)]
            [type (cond [(mf-gaussian? vfamily) 'mf-gaussian]
                        [(full-gaussian? vfamily) 'full-gaussian])])
           (let-values ([(epsilons new-prng) (sample-standard-normals d prng)])
                       (let* ([grad (cond [(mf-gaussian? vfamily)
                                           (elbo-gradient-mf traced-log-joint
                                                             (mf-gaussian-means vfamily)
                                                             (mf-gaussian-log-stds vfamily)
                                                             epsilons)]
                                          [else (error 'vi-step-adam-traced
                                                       "Only mean-field Gaussian supported")])])
                             (let-values ([(new-adam deltas) (adam-update adam-state grad lr beta1 beta2 eps)])
                                         (let* ([new-params (map + params deltas)]
                                                [new-vfamily (vfamily-from-params type d new-params)])
                                               (values new-vfamily new-prng new-adam))))))]))

;;; ====
;;; Variational Inference Main Loop
;;; ====

;;; vi-fit : (z → Number) × VFamily × Nat × Number [× Nat × Nat × Nat] → VIResult
;;; Fit variational approximation to posterior.
;;; NOTE: This version uses the log-joint for both ELBO estimation and gradients.
;;; For proper gradient flow, use vi-fit-traced with a traced log-joint.
;;; Returns the optimized variational family and ELBO history.
;;; Optional args: seed (default 42), elbo-samples (default 10), track-every (default 100)
(define vi-fit
  (case-lambda
    [(log-joint initial-vfamily num-iters learning-rate)
     (vi-fit log-joint initial-vfamily num-iters learning-rate 42 10 100)]
    [(log-joint initial-vfamily num-iters learning-rate seed)
     (vi-fit log-joint initial-vfamily num-iters learning-rate seed 10 100)]
    [(log-joint initial-vfamily num-iters learning-rate seed elbo-samples)
     (vi-fit log-joint initial-vfamily num-iters learning-rate seed elbo-samples 100)]
    [(log-joint initial-vfamily num-iters learning-rate seed elbo-samples track-every)
     (let* ([prng (make-pcg seed 1)]
            [d (vfamily-dim initial-vfamily)]
            [adam (make-adam-state (* 2 d))])  ; 2d params for mean-field
           (let loop ([iter 0]
                      [vfamily initial-vfamily]
                      [rng prng]
                      [adam-state adam]
                      [elbo-history '()])
                (if (>= iter num-iters)
                    ;; Return result
                    (list 'vi-result
                          vfamily
                          (reverse elbo-history)
                          iter)
                    ;; Take a step
                    (let*-values ([(new-vfamily new-rng new-adam)
                                   (vi-step-adam log-joint vfamily rng adam-state learning-rate)])
                                 ;; Track ELBO periodically
                                 (let-values ([(elbo final-rng)
                                               (if (= 0 (modulo iter track-every))
                                                   (elbo-estimate log-joint new-vfamily new-rng elbo-samples)
                                                   (values #f new-rng))])
                                             (loop (+ iter 1)
                                                   new-vfamily
                                                   final-rng
                                                   new-adam
                                                   (if elbo
                                                       (cons (cons iter elbo) elbo-history)
                                                       elbo-history)))))))]))

;;; vi-fit-traced : (z → Number) × ((List Traced) → Traced) × VFamily × Nat × Number [× Nat × Nat × Nat] → VIResult
;;; Fit variational approximation using separate functions for ELBO estimation and gradients.
;;; log-joint: Non-traced function for ELBO estimation
;;; traced-log-joint: Traced function for gradient computation (must use traced arithmetic)
(define vi-fit-traced
  (case-lambda
    [(log-joint traced-log-joint initial-vfamily num-iters learning-rate)
     (vi-fit-traced log-joint traced-log-joint initial-vfamily num-iters learning-rate 42 10 100)]
    [(log-joint traced-log-joint initial-vfamily num-iters learning-rate seed)
     (vi-fit-traced log-joint traced-log-joint initial-vfamily num-iters learning-rate seed 10 100)]
    [(log-joint traced-log-joint initial-vfamily num-iters learning-rate seed elbo-samples)
     (vi-fit-traced log-joint traced-log-joint initial-vfamily num-iters learning-rate seed elbo-samples 100)]
    [(log-joint traced-log-joint initial-vfamily num-iters learning-rate seed elbo-samples track-every)
     (let* ([prng (make-pcg seed 1)]
            [d (vfamily-dim initial-vfamily)]
            [adam (make-adam-state (* 2 d))])
           (let loop ([iter 0]
                      [vfamily initial-vfamily]
                      [rng prng]
                      [adam-state adam]
                      [elbo-history '()])
                (if (>= iter num-iters)
                    (list 'vi-result
                          vfamily
                          (reverse elbo-history)
                          iter)
                    ;; Take a step using traced-log-joint for gradients
                    (let*-values ([(new-vfamily new-rng new-adam)
                                   (vi-step-adam-traced traced-log-joint vfamily rng adam-state learning-rate)])
                                 ;; Track ELBO using non-traced log-joint
                                 (let-values ([(elbo final-rng)
                                               (if (= 0 (modulo iter track-every))
                                                   (elbo-estimate log-joint new-vfamily new-rng elbo-samples)
                                                   (values #f new-rng))])
                                             (loop (+ iter 1)
                                                   new-vfamily
                                                   final-rng
                                                   new-adam
                                                   (if elbo
                                                       (cons (cons iter elbo) elbo-history)
                                                       elbo-history)))))))]))

;;; vi-result? : α → Boolean
(define (vi-result? x)
  (and (pair? x) (eq? (car x) 'vi-result)))

;;; vi-result-vfamily : VIResult → VFamily
(define (vi-result-vfamily r) (cadr r))

;;; vi-result-elbo-history : VIResult → (List (Nat . Number))
(define (vi-result-elbo-history r) (caddr r))

;;; vi-result-iterations : VIResult → Nat
(define (vi-result-iterations r) (cadddr r))

;;; ====
;;; Convenience: Common Model Fitting
;;; ====

;;; vi-fit-normal-mean : (List Number) × Number [× Nat × Number] → VIResult
;;; Variational inference for normal mean with known variance.
;;; Model: μ ~ N(0, 100), x_i ~ N(μ, known-variance)
;;; Optional: num-iters (default 1000), learning-rate (default 0.01)
(define vi-fit-normal-mean
  (case-lambda
    [(observations known-variance)
     (vi-fit-normal-mean observations known-variance 1000 0.01)]
    [(observations known-variance num-iters)
     (vi-fit-normal-mean observations known-variance num-iters 0.01)]
    [(observations known-variance num-iters lr)
     (let* (;; Non-traced log joint for ELBO estimation
            [log-joint (lambda (z)
                               (let ([mu (car z)])
                                    (+ (log-normal-pdf mu 0 100)  ; Prior N(0, 100)
                                       (fold-left + 0
                                                  (map (lambda (xi)
                                                               (log-normal-pdf xi mu known-variance))
                                                       observations)))))]
            ;; Traced log joint for gradient computation
            [traced-log-joint (make-traced-log-joint-normal-mean
                               observations known-variance 0 100)]
            ;; Initial variational approximation (start near sample mean)
            [sample-mean (/ (fold-left + 0 observations) (length observations))]
            [init-vfamily (make-mf-gaussian (list 0.0) (list 0.0))])
           (vi-fit-traced log-joint traced-log-joint init-vfamily num-iters lr))]))

;;; vi-fit-linear-regression : (List (List Number)) × (List Number) [× Number × Nat × Number] → VIResult
;;; Variational inference for Bayesian linear regression.
;;; Model: β ~ N(0, 10*I), y_i ~ N(X_i · β, σ²)
;;; Optional: noise-variance (default 1.0), num-iters (default 2000), learning-rate (default 0.01)
(define vi-fit-linear-regression
  (case-lambda
    [(X y)
     (vi-fit-linear-regression X y 1.0 2000 0.01)]
    [(X y sigma2)
     (vi-fit-linear-regression X y sigma2 2000 0.01)]
    [(X y sigma2 num-iters)
     (vi-fit-linear-regression X y sigma2 num-iters 0.01)]
    [(X y sigma2 num-iters lr)
     (let* ([n (length y)]
            [d (length (car X))]  ; Number of features
            ;; Log joint
            [log-joint (lambda (beta)
                               (+ ;; Prior: β ~ N(0, 10*I)
                                (fold-left + 0
                                           (map (lambda (bi) (log-normal-pdf bi 0 10))
                                                beta))
                                ;; Likelihood: y ~ N(Xβ, σ²I)
                                (fold-left + 0
                                           (map (lambda (xi yi)
                                                        (let ([pred (fold-left + 0 (map * xi beta))])
                                                             (log-normal-pdf yi pred sigma2)))
                                                X y))))]
            ;; Initial variational approximation
            [init-means (make-list d 0.0)]
            [init-log-stds (make-list d 0.0)]
            [init-vfamily (make-mf-gaussian init-means init-log-stds)])
           (vi-fit log-joint init-vfamily num-iters lr))]))

;;; ====
;;; Diagnostics
;;; ====

;;; vi-summary : VIResult → Alist
;;; Generate summary of variational inference result.
(define (vi-summary result)
  (let* ([vfamily (vi-result-vfamily result)]
         [history (vi-result-elbo-history result)]
         [final-elbo (if (null? history) #f (cdar (last-pair history)))]
         [first-elbo (if (null? history) #f (cdar history))])
        `((type . ,(cond [(mf-gaussian? vfamily) 'mean-field-gaussian]
                         [(full-gaussian? vfamily) 'full-gaussian]
                         [else 'unknown]))
          (dimension . ,(vfamily-dim vfamily))
          (iterations . ,(vi-result-iterations result))
          (final-elbo . ,final-elbo)
          (elbo-improvement . ,(if (and first-elbo final-elbo)
                                   (- final-elbo first-elbo)
                                   #f))
          (posterior-mean . ,(cond [(mf-gaussian? vfamily) (mf-gaussian-means vfamily)]
                                   [(full-gaussian? vfamily) (full-gaussian-means vfamily)]
                                   [else #f]))
          (posterior-std . ,(cond [(mf-gaussian? vfamily) (mf-gaussian-stds vfamily)]
                                  [else #f])))))

;;; last-pair : (List α) → (α . α)
(define (last-pair lst)
  (if (null? (cdr lst))
      lst
      (last-pair (cdr lst))))

;;; vi-check-convergence : VIResult × Number → Boolean
;;; Check if ELBO has converged (relative change below threshold).
(define (vi-check-convergence result threshold)
  (let ([history (vi-result-elbo-history result)])
       (if (< (length history) 2)
           #f
           (let* ([recent (take history 2)]
                  [elbo1 (cdar recent)]
                  [elbo0 (cdadr recent)]
                  [rel-change (abs (/ (- elbo1 elbo0) (+ (abs elbo0) 1e-8)))])
                 (< rel-change threshold)))))

;;; ====
;;; Module Summary
;;; ====
;;;
;;; Variational Families:
;;;   make-mf-gaussian, mf-gaussian-means, mf-gaussian-log-stds
;;;   make-full-gaussian, full-gaussian-means, full-gaussian-chol
;;;   vfamily-dim, vfamily-sample, vfamily-log-prob, vfamily-entropy
;;;
;;; ELBO:
;;;   elbo-estimate, elbo-with-entropy
;;;   elbo-gradient-mf
;;;
;;; Optimization:
;;;   vi-step-mf, vi-step-adam
;;;   vi-fit
;;;
;;; Convenience:
;;;   vi-fit-normal-mean, vi-fit-linear-regression
;;;
;;; Diagnostics:
;;;   vi-summary, vi-check-convergence, vi-result-elbo-history
