(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
;;; @module differencing
;;; @requires prelude iteration
(require 'prelude)
(require 'iteration)

(doc 'module 'differencing)
(doc 'description "Time Series Differencing — Differencing and integration for time series")
(doc 'layer 'lattice)
(doc 'purity 'partial)

(doc 'description "difference: d-th order differencing")
(doc 'description "integrate: reverse differencing")
(doc 'description "seasonal-difference: seasonal differencing")

(doc 'section 'simple-differencing)
(define (vec-diff xs)
  (doc 'export #t)
  (vec-tabulate (- (vector-length xs) 1) i
    (- (vector-ref xs (+ i 1))
       (vector-ref xs i))))

;;; difference : Vec × Nat → Vec
;;; d-th order difference.
;;; difference(xs, 0) = xs
;;; difference(xs, d) = difference(vec-diff(xs), d-1)
(define (difference xs d)
  (doc 'export #t)
  (if (<= d 0)
      xs
      (difference (vec-diff xs) (- d 1))))

;;; ====
;;; Lag Differencing
;;; ====

;;; lag-diff : Vec × Nat → Vec
;;; Difference with lag k: x'[i] = x[i] - x[i-k]
;;; Result has length n-k.
(define (lag-diff xs k)
  (doc 'export #t)
  (vec-tabulate (- (vector-length xs) k) i
    (- (vector-ref xs (+ i k))
       (vector-ref xs i))))

;;; seasonal-difference : Vec × Nat × Nat → Vec
;;; Seasonal differencing of order D with period s.
;;; Applies lag-s difference D times.
(define (seasonal-difference xs s D)
  (doc 'export #t)
  (if (<= D 0)
      xs
      (seasonal-difference (lag-diff xs s) s (- D 1))))

;;; ====
;;; Integration (Inverse of Differencing)
;;; ====

;;; integrate : Vec × Num → Vec
;;; Cumulative sum (inverse of first difference).
;;; Given differences, reconstruct original series starting from init.
;;; If x' = diff(x), then integrate(x', x[0]) = x
(define (integrate diffs init)
  (doc 'export #t)
  (vec-scan (+ (vector-length diffs) 1) init t acc
    (+ acc (vector-ref diffs (- t 1)))))

;;; integrate-from-last : Vec × Num → Vec
;;; Integrate backward from last value.
(define (integrate-from-last diffs last-val)
  (let* ([n (vector-length diffs)]
         [result (make-vector (+ n 1))])
        (vector-set! result n last-val)
        (do ([i (- n 1) (- i 1)])
            [(< i 0) result]
            (vector-set! result i
                         (- (vector-ref result (+ i 1))
                            (vector-ref diffs i))))))

;;; integrate-d : Vec × (List Num) × Nat → Vec
;;; Integrate d times, using initial values from original series.
;;; inits should have d elements (the first d values of original).
(define (integrate-d diffs inits d)
  (if (<= d 0)
      diffs
      (let* ([init (car inits)]
             [integrated (integrate diffs init)])
            (integrate-d integrated (cdr inits) (- d 1)))))

;;; ====
;;; Stationarity Helpers
;;; ====

;;; is-stationary? : Vec → Bool
;;; Simple heuristic: check if variance is roughly constant.
;;; Split into halves and compare variances.
;;; Note: This is a quick check, not a proper unit root test.
(define (is-stationary? xs)
  (let* ([n (vector-length xs)]
         [half (quotient n 2)]
         [first-half (subvec xs 0 half)]
         [second-half (subvec xs half n)]
         [var1 (vec-variance-simple first-half)]
         [var2 (vec-variance-simple second-half)]
         [ratio (/ (max var1 var2) (max (min var1 var2) 1e-10))])
        ;; If variance ratio < 2, likely stationary
        (< ratio 2)))

;;; vec-variance-simple : Vec → Num
(define (vec-variance-simple v)
  (let* ([n (vector-length v)]
         [mean (/ (let loop ([i 0] [s 0])
                       (if (= i n)
                           s
                           (loop (+ i 1) (+ s (vector-ref v i)))))
                  n)])
        (/ (let loop ([i 0] [s 0])
                (if (= i n)
                    s
                    (loop (+ i 1)
                          (+ s (expt (- (vector-ref v i) mean) 2)))))
           (max (- n 1) 1))))

;;; subvec : Vec × Nat × Nat → Vec
;;; Extract subvector from start to end (exclusive).
(define (subvec v start end)
  (vec-tabulate (- end start) i
    (vector-ref v (+ start i))))

;;; ====
;;; Log Transform
;;; ====

;;; log-transform : Vec → Vec
;;; Apply log transform (for variance stabilization).
(define (log-transform xs)
  (doc 'export #t)
  (vec-tabulate (vector-length xs) i
    (log (max (vector-ref xs i) 1e-10))))

;;; exp-transform : Vec → Vec
;;; Inverse of log transform.
(define (exp-transform xs)
  (vec-tabulate (vector-length xs) i
    (exp (vector-ref xs i))))

;;; box-cox : Vec × Num → Vec
;;; Box-Cox transformation.
;;; lambda = 0: log(x)
;;; lambda != 0: (x^lambda - 1) / lambda
(define (box-cox xs lambda)
  (doc 'export #t)
  (if (< (abs lambda) 1e-10)
      (log-transform xs)
      (vec-tabulate (vector-length xs) i
        (let ([x (max (vector-ref xs i) 1e-10)])
          (/ (- (expt x lambda) 1) lambda)))))

;;; box-cox-inverse : Vec × Num → Vec
(define (box-cox-inverse ys lambda)
  (if (< (abs lambda) 1e-10)
      (exp-transform ys)
      (vec-tabulate (vector-length ys) i
        (let ([y (vector-ref ys i)])
          (expt (+ 1 (* lambda y)) (/ 1 lambda))))))
