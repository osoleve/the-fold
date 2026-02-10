;;; lattice/random/probability.ss — Probability Monad
;;; @module probability
;;; @requires prelude state prng distributions

(require 'prelude)
(require 'state)
(require 'prng)
(require 'distributions)

(doc 'module 'probability)
(doc 'description "Probability Monad")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'section 'probability-monad-representation)

(doc "Probability Monad Representation")








(define (make-prob state-comp)
  (doc 'export #t)
  (cons 'prob state-comp))

(define (prob? x)
  (doc 'export #t)
  (and (pair? x) (eq? (car x) 'prob)))

(define (prob-state p)
  (doc 'export #t)
  (cdr p))


(doc 'section 'internal-state-manipulation)

(doc "Internal State Manipulation")



(doc prob-get-prng 'export #t)
(define prob-get-prng
  (state-gets car))

(define (prob-put-prng new-prng)
  (doc 'export #t)
  (state-modify (lambda (s) (cons new-prng (cdr s)))))

(doc prob-get-weight 'export #t)
(define prob-get-weight
  (state-gets cdr))

(define (prob-add-weight log-w)
  (doc 'export #t)
  (state-modify (lambda (s) (cons (car s) (+ (cdr s) log-w)))))


(doc 'section 'running-probability-computations)

(doc "Running Probability Computations")



(define (run-prob p prng)
  (doc 'export #t)
  (let ([result (run-state (prob-state p) (cons prng 0.0))])
       (let ([value (car result)]
             [final-state (cdr result)])
            (cons (cons value (cdr final-state))
                  (car final-state)))))

(define (sample-prob p prng)
  (doc 'export #t)
  (car (car (run-prob p prng))))

(define (weight-prob p prng)
  (doc 'export #t)
  (cdr (car (run-prob p prng))))


(doc 'section 'monad-operations)

(doc "Monad Operations")



(define (prob-pure x)
  (doc 'export #t)
  (make-prob (state-pure x)))

(define (prob-bind p f)
  (doc 'export #t)
  (make-prob
   (state-bind (prob-state p)
               (lambda (a)
                       (prob-state (f a))))))

(define (prob-then p1 p2)
  (prob-bind p1 (lambda (_) p2)))

(define (prob-map f p)
  (doc 'export #t)
  (prob-bind p (lambda (a) (prob-pure (f a)))))

(define (prob-ap pf pa)
  (prob-bind pf
             (lambda (f)
                     (prob-bind pa
                                (lambda (a)
                                        (prob-pure (f a)))))))


(doc 'section 'sampling-from-distributions)

(doc "Sampling from Distributions")



(define (sample dist)
  (doc 'export #t)
  (make-prob
   (make-state
    (lambda (ps)
            (let* ([prng (car ps)]
                   [weight (cdr ps)]
                   [result (run-state dist prng)]
                   [value (car result)]
                   [new-prng (cdr result)])
                  (cons value (cons new-prng weight)))))))


(define prob-uniform
  (sample random-float))

(define (prob-uniform-range lo hi)
  (sample (random-range lo hi)))

(define (prob-uniform-int lo hi)
  (sample (random-int-range lo hi)))

(define (prob-bernoulli p)
  (sample (random-bernoulli p)))

(define (prob-normal mean stddev)
  (sample (random-normal mean stddev)))

(define (prob-exponential rate)
  (sample (random-exponential rate)))

(define (prob-poisson rate)
  (sample (random-poisson rate)))

(define (prob-geometric p)
  (sample (random-geometric p)))

(define (prob-binomial n p)
  (sample (random-binomial n p)))

(define (prob-gamma shape rate)
  (sample (random-gamma shape rate)))

(define (prob-beta a b)
  (sample (random-beta a b)))

(define (prob-categorical weights)
  (sample (random-categorical weights)))


(doc 'section 'conditioning)

(doc "Conditioning")



(define (factor log-prob)
  (doc 'export #t)
  (make-prob (prob-add-weight log-prob)))

(define (score likelihood)
  (if (<= likelihood 0)
      (factor -inf.0)  ; Zero probability
      (factor (log-num likelihood))))

(define (observe likelihood-fn observed)
  (doc 'export #t)
  (score (likelihood-fn observed)))

(define (observe-dist likelihood-fn p)
  (prob-bind p
             (lambda (x)
                     (prob-then (score (likelihood-fn x))
                                (prob-pure x)))))

(define (condition pred p)
  (doc 'export #t)
  (prob-bind p
             (lambda (x)
                     (if (pred x)
                         (prob-pure x)
                         (prob-then (factor -inf.0)
                                    (prob-pure x))))))

(define (assume test)
  (if test
      (prob-pure '())
      (factor -inf.0)))


(doc 'section 'inference-primitives)

(doc "Inference Primitives")



(define (sample-once p seed)
  (sample-prob p (make-pcg seed 1)))

(define (sample-many p seed n)
  (doc 'export #t)
  (if (<= n 0)
      '()
      (let loop ([prng (make-pcg seed 1)] [count n] [acc '()])
           (if (= count 0)
               (reverse acc)
               (let* ([result (run-prob p prng)]
                      [value (car (car result))]
                      [new-prng (cdr result)])
                     (loop new-prng (- count 1) (cons value acc)))))))

(define (weighted-samples p seed n)
  (if (<= n 0)
      '()
      (let loop ([prng (make-pcg seed 1)] [count n] [acc '()])
           (if (= count 0)
               (reverse acc)
               (let* ([result (run-prob p prng)]
                      [value (car (car result))]
                      [log-w (cdr (car result))]
                      [new-prng (cdr result)])
                     (loop new-prng (- count 1) (cons (cons value log-w) acc)))))))


(doc 'section 'importance-sampling-inference)

(doc "Importance Sampling Inference")



(define (log-sum-exp xs)
  (if (null? xs)
      -inf.0
      (let ([max-x (apply max xs)])
           (if (= max-x -inf.0)
               -inf.0
               (+ max-x (log-num (fold-left + 0 (map (lambda (x) (exp-num (- x max-x))) xs))))))))

(define (normalize-log-weights log-ws)
  (let ([total (log-sum-exp log-ws)])
       (if (= total -inf.0)
           (map (lambda (_) (/ 1.0 (length log-ws))) log-ws)  ; Uniform if all -inf
           (map (lambda (lw) (- lw total)) log-ws))))

(define (importance-expectation f p seed n)
  (let* ([samples (weighted-samples p seed n)]
         [log-ws (map cdr samples)]
         [values (map car samples)]
         [norm-log-ws (normalize-log-weights log-ws)]
         [weights (map exp-num norm-log-ws)]
         [f-values (map f values)])
        (fold-left + 0 (map * f-values weights))))

(define (importance-mean p seed n)
  (importance-expectation (lambda (x) x) p seed n))

(define (importance-variance p seed n)
  (let ([mean (importance-mean p seed n)])
       (importance-expectation (lambda (x) (* (- x mean) (- x mean))) p seed n)))


(doc 'section 'rejection-sampling)

(doc "Rejection Sampling")



(define (rejection-sample p seed max-tries)
  (doc 'export #t)
  (let loop ([prng (make-pcg seed 1)] [tries max-tries])
       (if (<= tries 0)
           nothing
           (let* ([result (run-prob p prng)]
                  [value (car (car result))]
                  [log-w (cdr (car result))]
                  [new-prng (cdr result)])
                 (if (> log-w -inf.0)
                     (just value)
                     (loop new-prng (- tries 1)))))))

(define (rejection-samples p seed n max-tries-per)
  (let loop ([prng (make-pcg seed 1)] [count n] [acc '()])
       (if (= count 0)
           (reverse acc)
           (let* ([result (run-prob p prng)]
                  [value (car (car result))]
                  [log-w (cdr (car result))]
                  [new-prng (cdr result)])
                 (if (> log-w -inf.0)
                     (loop new-prng (- count 1) (cons value acc))
                     ;; Retry with updated PRNG
                     (let inner-loop ([tries max-tries-per] [g new-prng])
                          (if (<= tries 0)
                              ;; Give up on this sample, continue to next
                              (loop g (- count 1) acc)
                              (let* ([r (run-prob p g)]
                                     [v (car (car r))]
                                     [lw (cdr (car r))]
                                     [ng (cdr r)])
                                    (if (> lw -inf.0)
                                        (loop ng (- count 1) (cons v acc))
                                        (inner-loop (- tries 1) ng))))))))))


(doc 'section 'combinators)

(doc "Combinators")



(define (prob-sequence probs)
  (doc 'export #t)
  (if (null? probs)
      (prob-pure '())
      (prob-bind (car probs)
                 (lambda (x)
                         (prob-bind (prob-sequence (cdr probs))
                                    (lambda (xs)
                                            (prob-pure (cons x xs))))))))

;; make-list is provided by prelude (alias for replicate)

(define (prob-replicate n p)
  (if (<= n 0)
      (prob-pure '())
      (prob-sequence (make-list n p))))

(define (prob-map-m f xs)
  (prob-sequence (map f xs)))

(define (prob-filter pred xs)
  (let ([valid (filter pred xs)])
       (if (null? valid)
           (prob-then (factor -inf.0) (prob-pure (car xs)))  ; No valid element
           (sample (random-choice valid)))))


(doc 'section 'distributions-as-first-class-values)

(doc "Distributions as First-Class Values")



(define (make-distribution sampler pdf)
  (list 'distribution sampler pdf))

(define (distribution? x)
  (and (pair? x) (eq? (car x) 'distribution)))

(define (dist-sampler d)
  (cadr d))

(define (dist-pdf d)
  (caddr d))

(define (sample-dist d)
  (sample (dist-sampler d)))

(define (observe-from-dist d observed)
  (let ([pdf (dist-pdf d)])
       (if pdf
           (score (pdf observed))
           (error 'observe-from-dist "distribution has no PDF" d))))


(doc 'section 'common-first-class-distributions)

(doc "Common First-Class Distributions")



(define (normal-dist mean stddev)
  (make-distribution
   (random-normal mean stddev)
   (lambda (x) (normal-pdf x mean stddev))))

(define (exponential-dist rate)
  (make-distribution
   (random-exponential rate)
   (lambda (x) (exponential-pdf rate x))))

(define (uniform-dist lo hi)
  (make-distribution
   (random-range lo hi)
   (lambda (x) (uniform-pdf lo hi x))))

(define (bernoulli-dist p)
  (make-distribution
   (random-bernoulli p)
   (lambda (x) (bernoulli-pmf p x))))

(define (poisson-dist rate)
  (make-distribution
   (random-poisson rate)
   (lambda (k) (poisson-pmf rate k))))


(doc 'section 'bayesian-inference-pattern)

(doc "Bayesian Inference Pattern")




(doc 'section 'example-coin-flip-model)

(doc "Example: Coin Flip Model")


