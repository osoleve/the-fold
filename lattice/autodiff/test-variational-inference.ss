;;; lattice/autodiff/test-variational-inference.ss — Tests for Variational Inference
;;;
;;; Comprehensive test suite for the variational inference module.
;;; Tests reparameterization, ELBO computation, gradient flow, and optimization.

(load "core/lang/module.ss")
(load "core/testing/test-framework.ss")
(load "lattice/autodiff/variational-inference.ss")

;;; ====
;;; Test Utilities
;;; ====

;;; approx-equal? : Number × Number × Number → Boolean
;;; Check if two numbers are approximately equal within tolerance.
(define (approx-equal? a b tol)
  (< (abs (- a b)) tol))

;;; list-approx-equal? : (List Number) × (List Number) × Number → Boolean
;;; Note: Use explicit recursion to avoid andmap shadowing from distributions.ss
(define (list-approx-equal? xs ys tol)
  (cond
   [(and (null? xs) (null? ys)) #t]
   [(or (null? xs) (null? ys)) #f]
   [(not (approx-equal? (car xs) (car ys) tol)) #f]
   [else (list-approx-equal? (cdr xs) (cdr ys) tol)]))

;;; ====
;;; Mean-Field Gaussian Tests
;;; ====

(test-group "mean-field-gaussian"

  (define-test "mf-gaussian construction"
    (let ([v (make-mf-gaussian '(1.0 2.0) '(0.0 0.5))])
         (assert-true (mf-gaussian? v))
         (assert-equal (mf-gaussian-dim v) 2)
         (assert-equal (mf-gaussian-means v) '(1.0 2.0))
         (assert-equal (mf-gaussian-log-stds v) '(0.0 0.5))))

  (define-test "mf-gaussian standard deviations"
    (let ([v (make-mf-gaussian '(0.0) '(0.0))])
         ;; log-std = 0 → std = exp(0) = 1
         (assert-true (approx-equal? (car (mf-gaussian-stds v)) 1.0 1e-10))))

  (define-test "mf-gaussian reparameterization"
    (let* ([v (make-mf-gaussian '(5.0) '(0.0))]  ; mean=5, std=1
           [epsilons '(1.0)]                       ; epsilon = 1
           [z (mf-gaussian-reparam v epsilons)])
          ;; z = μ + σ * ε = 5 + 1*1 = 6
          (assert-true (approx-equal? (car z) 6.0 1e-10))))

  (define-test "mf-gaussian reparameterization with scaling"
    (let* ([v (make-mf-gaussian '(0.0) '(1.0))]  ; mean=0, log-std=1 → std=e
           [epsilons '(2.0)]
           [z (mf-gaussian-reparam v epsilons)])
          ;; z = 0 + e * 2 ≈ 5.436
          (assert-true (approx-equal? (car z) (* (exp 1) 2) 1e-8))))

  (define-test "mf-gaussian sampling"
    (let* ([v (make-mf-gaussian '(0.0) '(0.0))]
           [prng (make-pcg 42 1)])
          (let-values ([(z new-prng) (sample-mf-gaussian v prng)])
                      ;; Should produce a number (actual value depends on PRNG)
                      (assert-true (number? (car z)))
                      ;; Should advance PRNG state
                      (assert-true (not (equal? prng new-prng))))))

  (define-test "mf-gaussian log probability at mean"
    (let* ([v (make-mf-gaussian '(0.0) '(0.0))]  ; Standard normal
           [z '(0.0)]
           [log-p (mf-gaussian-log-prob v z)])
          ;; log N(0|0,1) = -0.5 * log(2π) ≈ -0.9189
          (assert-true (approx-equal? log-p -0.9189385332 1e-6))))

  (define-test "mf-gaussian entropy"
    (let* ([v (make-mf-gaussian '(0.0) '(0.0))]  ; Standard normal
           [H (mf-gaussian-entropy v)])
          ;; H[N(0,1)] = 0.5 * log(2πe) ≈ 1.4189
          (assert-true (approx-equal? H 1.4189385332 1e-6))))

  (define-test "mf-gaussian params round-trip"
    (let* ([means '(1.0 2.0 3.0)]
           [log-stds '(-1.0 0.0 1.0)]
           [v (make-mf-gaussian means log-stds)]
           [params (vfamily-params v)]
           [v2 (vfamily-from-params 'mf-gaussian 3 params)])
          (assert-equal (mf-gaussian-means v2) means)
          (assert-equal (mf-gaussian-log-stds v2) log-stds))))

;;; ====
;;; Full Covariance Gaussian Tests
;;; ====

(test-group "full-gaussian"

  (define-test "full-gaussian construction"
    (let ([v (make-full-gaussian '(0.0 0.0) '(1.0 0.0 1.0))])
         (assert-true (full-gaussian? v))
         (assert-equal (full-gaussian-dim v) 2)
         (assert-equal (full-gaussian-means v) '(0.0 0.0))))

  (define-test "chol-flat-to-matrix"
    (let* ([flat '(1.0 0.5 2.0)]  ; L = [[1, 0], [0.5, 2]]
           [L (chol-flat-to-matrix flat 2)])
          (assert-equal L '((1.0) (0.5 2.0)))))

  (define-test "chol-num-elements"
    (assert-equal (chol-num-elements 2) 3)
    (assert-equal (chol-num-elements 3) 6)
    (assert-equal (chol-num-elements 4) 10))

  (define-test "full-gaussian reparameterization"
    (let* ([v (make-full-gaussian '(0.0 0.0) '(1.0 0.5 2.0))]
           [epsilons '(1.0 1.0)]
           [z (full-gaussian-reparam v epsilons)])
          ;; L = [[1,0],[0.5,2]] so z = μ + L*ε = [0,0] + [1*1, 0.5*1 + 2*1] = [1, 2.5]
          (assert-true (list-approx-equal? z '(1.0 2.5) 1e-10))))

  (define-test "forward-solve"
    (let* ([L '((2.0) (1.0 3.0))]  ; L = [[2,0],[1,3]]
           [b '(4.0 10.0)]
           [y (forward-solve L b)])
          ;; Ly = b: 2*y1 = 4 → y1 = 2, y1 + 3*y2 = 10 → y2 = 8/3
          (assert-true (list-approx-equal? y '(2.0 2.6666666667) 1e-6)))))

;;; ====
;;; Generic Interface Tests
;;; ====

(test-group "vfamily-interface"

  (define-test "vfamily-dim dispatches correctly"
    (let ([mf (make-mf-gaussian '(0.0 0.0) '(0.0 0.0))]
          [full (make-full-gaussian '(0.0 0.0) '(1.0 0.0 1.0))])
         (assert-equal (vfamily-dim mf) 2)
         (assert-equal (vfamily-dim full) 2)))

  (define-test "vfamily-entropy dispatches correctly"
    (let ([mf (make-mf-gaussian '(0.0) '(0.0))]
          [full (make-full-gaussian '(0.0) '(1.0))])
         (assert-true (number? (vfamily-entropy mf)))
         (assert-true (number? (vfamily-entropy full))))))

;;; ====
;;; ELBO Tests
;;; ====

(test-group "elbo"

  (define-test "elbo-estimate runs"
    (let* ([log-joint (lambda (z) (- (* -0.5 (car z) (car z))))]  ; -z²/2
           [v (make-mf-gaussian '(0.0) '(0.0))]
           [prng (make-pcg 42 1)])
          (let-values ([(elbo new-prng) (elbo-estimate log-joint v prng 100)])
                      (assert-true (number? elbo))
                      (assert-true (not (= elbo +nan.0))))))

  (define-test "elbo-with-entropy consistent"
    (let* ([log-joint (lambda (z) (log-normal-pdf (car z) 0 1))]
           [v (make-mf-gaussian '(0.0) '(0.0))]  ; q = N(0,1)
           [prng (make-pcg 42 1)])
          ;; When q = p, ELBO = log p(x) (evidence)
          ;; For this case: ELBO should be approximately 0 (log 1)
          (let-values ([(elbo1 prng1) (elbo-estimate log-joint v prng 1000)])
                      (let-values ([(elbo2 prng2) (elbo-with-entropy log-joint v prng 1000)])
                                  ;; Both methods should give similar results
                                  (assert-true (approx-equal? elbo1 elbo2 0.1)))))))

;;; ====
;;; Gradient Tests
;;; ====

(test-group "gradients"

  (define-test "traced-log-normal-pdf gradient correct"
    ;; Test that gradient of log-pdf w.r.t. mean is correct
    ;; d/dμ log N(z|μ,σ²) = (z-μ)/σ²
    (let* ([z 1.0]
           [mu 0.0]
           [log-sigma 0.0]  ; σ = 1
           ;; gradient uses (apply f traced-args), so f takes individual args
           [grad (gradient
                  (lambda (t-mu t-log-sigma)
                          (traced-log-normal-pdf z t-mu t-log-sigma))
                  (list mu log-sigma))]
           [grad-mu (car grad)])
          ;; d/dμ log N(1|0,1) = (1-0)/1 = 1
          (assert-true (approx-equal? grad-mu 1.0 0.01))))

  (define-test "elbo-gradient-mf produces gradient"
    ;; Use a traced log-joint (required for gradient flow)
    (let* ([traced-log-joint (lambda (t-z)
                                     ;; log N(z|0,1) using traced arithmetic
                                     (let ([t-mu (car t-z)])
                                          (traced-log-normal-pdf (traced-value t-mu)
                                                                 (make-traced-const 0 #f)
                                                                 (make-traced-const 0 #f))))]  ; log-sigma=0 means sigma=1
           [means '(0.5)]
           [log-stds '(0.1)]
           [epsilons '(0.5)]
           [grad (elbo-gradient-mf traced-log-joint means log-stds epsilons)])
          ;; Should produce 2 gradients (1 mean + 1 log-std)
          (assert-equal (length grad) 2)
          ;; Gradients should be finite numbers
          (assert-true (number? (car grad)))
          (assert-true (number? (cadr grad)))
          (assert-true (not (= (car grad) +nan.0)))
          (assert-true (not (= (cadr grad) +nan.0))))))

;;; ====
;;; Adam Optimizer Tests
;;; ====

(test-group "adam"

  (define-test "adam-state initialization"
    (let ([s (make-adam-state 3)])
         (assert-true (adam-state? s))
         (assert-equal (length (adam-state-m s)) 3)
         (assert-equal (length (adam-state-v s)) 3)
         (assert-equal (adam-state-t s) 1)))

  (define-test "adam-update produces updates"
    (let* ([s (make-adam-state 2)]
           [grads '(1.0 -1.0)])
          (let-values ([(new-s deltas) (adam-update s grads 0.01 0.9 0.999 1e-8)])
                      (assert-true (adam-state? new-s))
                      (assert-equal (adam-state-t new-s) 2)
                      (assert-equal (length deltas) 2)
                      ;; First delta should be positive (gradient ascent on positive grad)
                      (assert-true (> (car deltas) 0))
                      ;; Second delta should be negative
                      (assert-true (< (cadr deltas) 0))))))

;;; ====
;;; VI Step Tests
;;; ====

(test-group "vi-steps"

  (define-test "vi-step-mf runs"
    ;; vi-step-mf calls elbo-gradient-mf which requires a traced log-joint
    (let* ([traced-log-joint (lambda (t-z)
                                     ;; log N(z|0,1) using traced arithmetic
                                     (let ([t-mu (car t-z)])
                                          (traced-log-normal-pdf (traced-value t-mu)
                                                                 (make-traced-const 0 #f)
                                                                 (make-traced-const 0 #f))))]
           [v (make-mf-gaussian '(0.5) '(0.1))]
           [prng (make-pcg 42 1)])
          (let-values ([(new-v new-prng) (vi-step-mf traced-log-joint v prng 0.01)])
                      (assert-true (mf-gaussian? new-v))
                      (assert-true (not (equal? v new-v)))))))

;;; ====
;;; VI Fit Tests
;;; ====

(test-group "vi-fit"

  (define-test "vi-fit-normal-mean approximates posterior"
    ;; Test: observations from N(3, 1), fit variational approximation
    ;; True posterior mean should be close to sample mean
    (let* ([observations '(2.5 3.1 2.8 3.3 2.9)]  ; Mean ≈ 2.92
           [known-var 1.0]
           [result (vi-fit-normal-mean observations known-var 500 0.05)])
          (assert-true (vi-result? result))
          (let* ([v (vi-result-vfamily result)]
                 [post-mean (car (mf-gaussian-means v))])
                ;; Posterior mean should be close to sample mean
                (assert-true (approx-equal? post-mean 2.92 0.3)))))

  (define-test "vi-result accessors"
    (let* ([result (vi-fit-normal-mean '(1.0 1.5 0.8) 1.0 200 0.01)])
          (assert-true (mf-gaussian? (vi-result-vfamily result)))
          (assert-true (list? (vi-result-elbo-history result)))
          (assert-true (>= (vi-result-iterations result) 200))))

  (define-test "vi-summary produces report"
    (let* ([result (vi-fit-normal-mean '(2.0 2.5 1.8) 1.0 200)]
           [summary (vi-summary result)])
          ;; assq returns pair if found, #f if not - check for pair
          (assert-true (pair? (assq 'type summary)))
          (assert-true (pair? (assq 'dimension summary)))
          (assert-true (pair? (assq 'posterior-mean summary)))
          (assert-equal (cdr (assq 'type summary)) 'mean-field-gaussian))))

;;; ====
;;; Convergence Tests
;;; ====

(test-group "convergence"

  (define-test "elbo improves during training"
    ;; Use vi-fit-traced with both regular and traced log-joints
    (let* ([log-joint (lambda (z) (log-normal-pdf (car z) 2.0 1.0))]
           ;; Traced log-joint for gradient computation
           ;; Computes log N(z|2, 1) = -0.5*log(2π) - 0.5*(z-2)²
           ;; Gradients must flow through the latent variable t-z
           [traced-log-joint (lambda (t-z)
                                    (let* ([t-mu (car t-z)]  ; Traced latent variable
                                           [two (make-traced-const 2.0 #f)]
                                           [diff (traced-sub t-mu two)]
                                           [diff-sq (traced-mul diff diff)]
                                           [half (make-traced-const 0.5 #f)]
                                           [const (make-traced-const (* -0.5 (log (* 2 3.141592653589793))) #f)])
                                          (traced-sub const (traced-mul half diff-sq))))]
           [init-v (make-mf-gaussian '(0.0) '(1.0))]  ; Start far from optimum
           [prng (make-pcg 42 1)])
          ;; Track ELBO before and after
          (let-values ([(elbo-before _) (elbo-estimate log-joint init-v prng 100)])
                      ;; vi-fit-traced: log-joint traced-log-joint vfamily iters lr seed elbo-samples track-every
                      (let* ([result (vi-fit-traced log-joint traced-log-joint init-v 500 0.05 42 10 50)]
                             [final-v (vi-result-vfamily result)]
                             [prng2 (make-pcg 43 1)])
                            (let-values ([(elbo-after _) (elbo-estimate log-joint final-v prng2 100)])
                                        ;; ELBO should improve
                                        (assert-true (> elbo-after elbo-before))))))))

;;; ====
;;; Run All Tests
;;; ====

(run-all-tests)
