;;; lattice/random/variational-inference.ss — Variational Inference Engine
;;; @module variational-inference
;;; @requires prelude reverse-diff prng distributions bayesian

(require 'prelude)
(require 'reverse-diff)
(require 'prng)
(require 'distributions)
(require 'bayesian)

(doc 'module 'variational-inference)
(doc 'description "Variational Inference Engine")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'section 'local-utilities)

(doc "Local Utilities")







(define (iota-vi n)
  (let loop ([i 0] [acc '()])
       (if (>= i n)
           (reverse acc)
           (loop (+ i 1) (cons i acc)))))


(doc 'section 'variational-families)

(doc "Variational Families")




(doc 'section 'mean-field-gaussian)

(doc "Mean-Field Gaussian")



(define (make-mf-gaussian means log-stds)
  (doc 'export #t)
  (list 'mf-gaussian means log-stds))

(define (mf-gaussian? v)
  (doc 'export #t)
  (and (pair? v) (eq? (car v) 'mf-gaussian)))

(define (mf-gaussian-means v) (cadr v))
(doc 'export #t)

(define (mf-gaussian-log-stds v) (caddr v))
(doc 'export #t)

(define (mf-gaussian-stds v)
  (doc 'export #t)
  (map exp (mf-gaussian-log-stds v)))

(define (mf-gaussian-dim v)
  (doc 'export #t)
  (length (mf-gaussian-means v)))


(doc 'section 'reparameterization-trick)

(doc "Reparameterization Trick")



(define (sample-standard-normals n prng)
  (let loop ([k n] [rng prng] [acc '()])
       (if (= k 0)
           (values (reverse acc) rng)
           (let* ([result (run-state (random-normal 0 1) rng)]
                  [sample (car result)]
                  [new-rng (cdr result)])
                 (loop (- k 1) new-rng (cons sample acc))))))

(define (mf-gaussian-reparam vfamily epsilons)
  (doc 'export #t)
  (let ([means (mf-gaussian-means vfamily)]
        [log-stds (mf-gaussian-log-stds vfamily)])
       (map (lambda (mu log-s eps)
                    (+ mu (* (exp log-s) eps)))
            means log-stds epsilons)))

(define (mf-gaussian-reparam-traced t-means t-log-stds epsilons)
  (map (lambda (t-mu t-log-s eps)
               (traced-add t-mu
                           (traced-mul (traced-exp t-log-s)
                                       (make-traced-const eps #f))))
       t-means t-log-stds epsilons))

(define (sample-mf-gaussian vfamily prng)
  (doc 'export #t)
  (let-values ([(epsilons new-prng) (sample-standard-normals (mf-gaussian-dim vfamily) prng)])
              (values (mf-gaussian-reparam vfamily epsilons) new-prng)))


(doc 'section 'log-probabilities-for-variational-families)

(doc "Log Probabilities for Variational Families")



(define (mf-gaussian-log-prob vfamily z)
  (doc 'export #t)
  (let ([means (mf-gaussian-means vfamily)]
        [log-stds (mf-gaussian-log-stds vfamily)])
       (fold-left + 0
                  (map (lambda (zi mui log-si)
                               (log-normal-pdf zi mui (exp (* 2 log-si))))
                       z means log-stds))))

(define (mf-gaussian-entropy vfamily)
  (doc 'export #t)
  (let ([log-stds (mf-gaussian-log-stds vfamily)])
       (* (length log-stds)
          (+ 0.5 (* 0.5 (log (* 2 3.141592653589793)))
             (fold-left + 0 log-stds)))))


(doc 'section 'full-covariance-gaussian)

(doc "Full Covariance Gaussian")



(define (make-full-gaussian means chol-flat)
  (doc 'export #t)
  (list 'full-gaussian means chol-flat))

(define (full-gaussian? v)
  (doc 'export #t)
  (and (pair? v) (eq? (car v) 'full-gaussian)))

(define (full-gaussian-means v) (cadr v))
(doc 'export #t)

(define (full-gaussian-chol v) (caddr v))
(doc 'export #t)

(define (full-gaussian-dim v)
  (doc 'export #t)
  (length (full-gaussian-means v)))

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

(define (chol-num-elements n)
  (/ (* n (+ n 1)) 2))

(define (full-gaussian-reparam vfamily epsilons)
  (doc 'export #t)
  (let* ([means (full-gaussian-means vfamily)]
         [flat (full-gaussian-chol vfamily)]
         [n (length means)]
         [L (chol-flat-to-matrix flat n)])
        (map (lambda (mu-i L-row)
                     (+ mu-i
                        (fold-left + 0
                                   (map * L-row (take (length L-row) epsilons)))))
             means L)))

;; take provided by prelude

(define (sample-full-gaussian vfamily prng)
  (doc 'export #t)
  (let-values ([(epsilons new-prng) (sample-standard-normals (full-gaussian-dim vfamily) prng)])
              (values (full-gaussian-reparam vfamily epsilons) new-prng)))

(define (full-gaussian-log-prob vfamily z)
  (doc 'export #t)
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

(define (butlast lst)
  (if (or (null? lst) (null? (cdr lst)))
      '()
      (cons (car lst) (butlast (cdr lst)))))

;;; last provided by prelude

(define (full-gaussian-entropy vfamily)
  (doc 'export #t)
  (let* ([n (full-gaussian-dim vfamily)]
         [flat (full-gaussian-chol vfamily)]
         [L (chol-flat-to-matrix flat n)]
         [diag (map (lambda (i row) (list-ref row i)) (iota-vi n) L)]
         [log-diag-sum (fold-left + 0 (map log diag))])
        (+ (* 0.5 n (+ 1 (log (* 2 3.141592653589793))))
           log-diag-sum)))


(doc 'section 'generic-variational-family-interface)

(doc "Generic Variational Family Interface")



(define (vfamily-dim v)
  (doc 'export #t)
  (cond [(mf-gaussian? v) (mf-gaussian-dim v)]
        [(full-gaussian? v) (full-gaussian-dim v)]
        [else (error 'vfamily-dim "Unknown variational family" v)]))

(define (vfamily-sample v prng)
  (cond [(mf-gaussian? v) (sample-mf-gaussian v prng)]
        [(full-gaussian? v) (sample-full-gaussian v prng)]
        [else (error 'vfamily-sample "Unknown variational family" v)]))

(define (vfamily-log-prob v z)
  (cond [(mf-gaussian? v) (mf-gaussian-log-prob v z)]
        [(full-gaussian? v) (full-gaussian-log-prob v z)]
        [else (error 'vfamily-log-prob "Unknown variational family" v)]))

(define (vfamily-entropy v)
  (doc 'export #t)
  (cond [(mf-gaussian? v) (mf-gaussian-entropy v)]
        [(full-gaussian? v) (full-gaussian-entropy v)]
        [else (error 'vfamily-entropy "Unknown variational family" v)]))

(define (vfamily-params v)
  (doc 'export #t)
  (cond [(mf-gaussian? v)
         (append (mf-gaussian-means v) (mf-gaussian-log-stds v))]
        [(full-gaussian? v)
         (append (full-gaussian-means v) (full-gaussian-chol v))]
        [else (error 'vfamily-params "Unknown variational family" v)]))

(define (vfamily-from-params type dim params)
  (doc 'export #t)
  (case type
    [(mf-gaussian)
     (let ([means (take dim params)]
           [log-stds (drop dim params)])
          (make-mf-gaussian means log-stds))]
    [(full-gaussian)
     (let ([means (take dim params)]
           [chol (drop dim params)])
          (make-full-gaussian means chol))]
    [else (error 'vfamily-from-params "Unknown type" type)]))

;; drop provided by prelude

(doc 'section 'elbo-computation)

(doc "ELBO Computation")



(define (elbo-estimate log-joint vfamily prng K)
  (doc 'export #t)
  (let loop ([k K] [rng prng] [total 0])
       (if (= k 0)
           (values (/ total K) rng)
           (let*-values ([(z new-rng) (vfamily-sample vfamily rng)]
                         [(log-p) (log-joint z)]
                         [(log-q) (vfamily-log-prob vfamily z)])
                        (loop (- k 1) new-rng (+ total (- log-p log-q)))))))

(define (elbo-with-entropy log-joint vfamily prng K)
  (doc 'export #t)
  (let* ([H (vfamily-entropy vfamily)])
        (let loop ([k K] [rng prng] [total 0])
             (if (= k 0)
                 (values (+ (/ total K) H) rng)
                 (let*-values ([(z new-rng) (vfamily-sample vfamily rng)])
                              (loop (- k 1) new-rng (+ total (log-joint z))))))))


(doc 'section 'elbo-gradient-via-reparameterization)

(doc "ELBO Gradient via Reparameterization")



(define (elbo-gradient-mf traced-log-joint means log-stds epsilons)
  (doc 'export #t)
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
                        ;; Compute ANALYTICAL entropy: H[q] = 0.5*d*(1+log(2π)) + Σ log_σ
                        ;; Only the Σ log_σ term contributes gradient (∂H/∂log_σ = +1)
                        ;; The constant 0.5*d*(1+log(2π)) has zero gradient, so we omit it
                        [t-entropy (sum-traced t-log-stds)])
                       ;; ELBO = E[log p(x,z)] + H[q] (using analytical entropy)
                       (traced-add t-log-p t-entropy)))
         params)))


(doc 'section 'traced-log-joint-construction)

(doc "Traced Log-Joint Construction")



(define (traced-log-normal-pdf-sum observations t-mu variance)
  (doc 'export #t)
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

(define (make-traced-log-joint-normal-mean observations known-var prior-mean prior-var)
  (doc 'export #t)
  (lambda (t-z)
    (let* ([t-mu (car t-z)]
           ;; Prior: log N(mu | prior-mean, prior-var)
           [prior-mean-const (make-traced-const prior-mean #f)]
           [t-prior (traced-log-normal-pdf (traced-value t-mu) prior-mean-const
                                           (make-traced-const (log (sqrt prior-var)) #f))]
           ;; Likelihood: sum of log N(x_i | mu, known-var)
           [t-likelihood (traced-log-normal-pdf-sum observations t-mu known-var)])
          (traced-add t-prior t-likelihood))))

(define (make-traced-log-joint-linear-regression X y noise-var prior-var)
  (lambda (t-beta)
    ;; Compute prior: sum of log N(β_j | 0, prior-var)
    (let* ([t-prior (fold-left traced-add
                               (make-traced-const 0 #f)
                               (map (lambda (t-bj)
                                           ;; log N(β_j | 0, prior-var)
                                           (traced-log-normal-pdf (traced-value t-bj)
                                                                  (make-traced-const 0 #f)
                                                                  (make-traced-const (log (sqrt prior-var)) #f)))
                                    t-beta))]
           ;; Compute likelihood: sum of log N(y_i | X_i·β, noise-var)
           [t-likelihood (fold-left traced-add
                                    (make-traced-const 0 #f)
                                    (map (lambda (xi yi)
                                                 ;; Compute X_i · β using traced arithmetic
                                                 (let* ([t-pred (fold-left traced-add
                                                                           (make-traced-const 0 #f)
                                                                           (map (lambda (xij t-bj)
                                                                                        (traced-mul (make-traced-const xij #f) t-bj))
                                                                                xi t-beta))]
                                                        ;; log N(y_i | pred, noise-var)
                                                        [yi-const (make-traced-const yi #f)]
                                                        [diff (traced-sub yi-const t-pred)]
                                                        [diff-sq (traced-mul diff diff)]
                                                        [var-const (make-traced-const noise-var #f)]
                                                        [log-norm-const (make-traced-const
                                                                         (* -0.5 (log (* 2 3.141592653589793 noise-var))) #f)]
                                                        [half (make-traced-const 0.5 #f)]
                                                        [scaled (traced-div diff-sq var-const)])
                                                       (traced-sub log-norm-const (traced-mul half scaled))))
                                         X y))])
          (traced-add t-prior t-likelihood))))

(define (take-traced lst n)
  (if (or (null? lst) (<= n 0))
      '()
      (cons (car lst) (take-traced (cdr lst) (- n 1)))))

(define (drop-traced lst n)
  (if (or (null? lst) (<= n 0))
      lst
      (drop-traced (cdr lst) (- n 1))))

(define (sum-traced lst)
  (if (null? lst)
      (make-traced-const 0 #f)
      (fold-left traced-add (car lst) (cdr lst))))

(define (traced-log-normal-pdf z t-mu t-log-sigma)
  (doc 'export #t)
  (let* ([const-term (make-traced-const (* -0.5 (log (* 2 3.141592653589793))) #f)]
         [z-const (make-traced-const z #f)]
         [diff (traced-sub z-const t-mu)]
         [diff-sq (traced-mul diff diff)]
         [var (traced-exp (traced-mul t-log-sigma (make-traced-const 2 #f)))]
         [mahal (traced-div diff-sq var)])
        (traced-sub
         (traced-sub const-term t-log-sigma)
         (traced-mul (make-traced-const 0.5 #f) mahal))))


(doc 'section 'stochastic-gradient-descent-for-vi)

(doc "Stochastic Gradient Descent for VI")



(define (vi-step-mf log-joint vfamily prng learning-rate)
  (doc 'export #t)
  (let* ([d (mf-gaussian-dim vfamily)]
         [means (mf-gaussian-means vfamily)]
         [log-stds (mf-gaussian-log-stds vfamily)])
        ;; Sample noise for reparameterization
        (let-values ([(epsilons new-prng) (sample-standard-normals d prng)])
                    ;; Compute gradient
                    (let* ([grad (elbo-gradient-mf log-joint means log-stds epsilons)]
                           [grad-means (take d grad)]
                           [grad-log-stds (drop d grad)]
                           ;; Gradient ascent (maximize ELBO)
                           [new-means (map (lambda (m g) (+ m (* learning-rate g)))
                                           means grad-means)]
                           [new-log-stds (map (lambda (s g) (+ s (* learning-rate g)))
                                              log-stds grad-log-stds)])
                          (values (make-mf-gaussian new-means new-log-stds)
                                  new-prng)))))


(doc 'section 'adam-optimizer-for-vi)

(doc "Adam Optimizer for VI")



(define (make-adam-state num-params)
  (doc 'export #t)
  (list 'adam-state
        (make-list num-params 0)    ; First moment (m)
        (make-list num-params 0)    ; Second moment (v)
        1))                          ; Time step t

(define (adam-state? x)
  (doc 'export #t)
  (and (pair? x) (eq? (car x) 'adam-state)))

(define (adam-state-m s) (cadr s))
(doc 'export #t)

(define (adam-state-v s) (caddr s))
(doc 'export #t)

(define (adam-state-t s) (cadddr s))
(doc 'export #t)

;; make-list is provided by prelude (alias for replicate)

(define (adam-update state grads lr beta1 beta2 epsilon)
  (doc 'export #t)
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

(doc vi-step-adam 'export #t)
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

(doc vi-step-adam-traced 'export #t)
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


(doc 'section 'variational-inference-main-loop)

(doc "Variational Inference Main Loop")



(doc vi-fit 'export #t)
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

(doc vi-fit-traced 'export #t)
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

(define (vi-result? x)
  (doc 'export #t)
  (and (pair? x) (eq? (car x) 'vi-result)))

(define (vi-result-vfamily r) (cadr r))
(doc 'export #t)

(define (vi-result-elbo-history r) (caddr r))
(doc 'export #t)

(define (vi-result-iterations r) (cadddr r))
(doc 'export #t)


(doc 'section 'convenience-common-model-fitting)

(doc "Convenience: Common Model Fitting")



(doc vi-fit-normal-mean 'export #t)
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

(doc vi-fit-linear-regression 'export #t)
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
            ;; Non-traced log joint (for ELBO estimation)
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
            ;; Traced log joint (for gradient computation)
            [traced-log-joint (make-traced-log-joint-linear-regression X y sigma2 10)]
            ;; Initial variational approximation
            [init-means (make-list d 0.0)]
            [init-log-stds (make-list d 0.0)]
            [init-vfamily (make-mf-gaussian init-means init-log-stds)])
           (vi-fit-traced log-joint traced-log-joint init-vfamily num-iters lr))]))


(doc 'section 'diagnostics)

(doc "Diagnostics")



(define (vi-summary result)
  (doc 'export #t)
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

(define (last-pair lst)
  (if (null? (cdr lst))
      lst
      (last-pair (cdr lst))))

(define (vi-check-convergence result threshold)
  (doc 'export #t)
  (let ([history (vi-result-elbo-history result)])
       (if (< (length history) 2)
           #f
           (let* ([recent (take 2 history)]
                  [elbo1 (cdar recent)]
                  [elbo0 (cdadr recent)]
                  [rel-change (abs (/ (- elbo1 elbo0) (+ (abs elbo0) 1e-8)))])
                 (< rel-change threshold)))))


(doc 'section 'module-summary)

(doc "Module Summary")


