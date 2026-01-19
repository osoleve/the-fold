;;; lattice/statistics/core/summary-stats.ss — Descriptive Statistics
;;;
;;; Basic statistical summary functions.
;;;
;;; This is Lattice code: pure, total, assumes reasonable input.
;;;
;;; Provides:
;;;   - Mean, variance, standard deviation
;;;   - Median, quantiles
;;;   - Covariance, correlation
;;;   - Vector-optimized versions
;;;
;;; Dependencies:
;;;   - prelude.ss

(load "core/base/prelude.ss")

;;; ====
;;; Basic Statistics (List versions)
;;; ====

;;; mean : (List Num) → Num
;;; Compute arithmetic mean.
(define (mean xs)
  (if (null? xs)
      (error 'mean "empty list")
      (/ (fold-left + 0 xs) (length xs))))

;;; variance : (List Num) → Num
;;; Compute sample variance (n-1 denominator).
(define (variance xs)
  (let* ([n (length xs)]
         [mu (mean xs)]
         [ss (fold-left + 0 (map (lambda (x) (expt (- x mu) 2)) xs))])
        (if (<= n 1)
            0
            (/ ss (- n 1)))))

;;; variance-population : (List Num) → Num
;;; Compute population variance (n denominator).
(define (variance-population xs)
  (let* ([n (length xs)]
         [mu (mean xs)]
         [ss (fold-left + 0 (map (lambda (x) (expt (- x mu) 2)) xs))])
        (if (= n 0)
            0
            (/ ss n))))

;;; std-dev : (List Num) → Num
;;; Compute sample standard deviation.
(define (std-dev xs)
  (sqrt (variance xs)))

;;; std-dev-population : (List Num) → Num
;;; Compute population standard deviation.
(define (std-dev-population xs)
  (sqrt (variance-population xs)))

;;; ====
;;; Order Statistics
;;; ====

;;; median : (List Num) → Num
;;; Compute median (middle value or average of two middle values).
(define (median xs)
  (let* ([sorted (list-sort < xs)]
         [n (length sorted)]
         [mid (quotient n 2)])
        (if (odd? n)
            (list-ref sorted mid)
            (/ (+ (list-ref sorted (- mid 1))
                  (list-ref sorted mid))
               2))))

;;; quantile : (List Num) × Num → Num
;;; Compute p-th quantile (0 <= p <= 1).
;;; Uses linear interpolation (type 7, R default).
(define (quantile xs p)
  (if (or (< p 0) (> p 1))
      (error 'quantile "p must be in [0, 1]" p)
      (let* ([sorted (list-sort < xs)]
             [n (length sorted)]
             [index (* p (- n 1))]
             [lo (inexact->exact (floor index))]
             [hi (inexact->exact (ceiling index))]
             [h (- index lo)])
            (if (= lo hi)
                (list-ref sorted lo)
                (+ (* (- 1 h) (list-ref sorted lo))
                   (* h (list-ref sorted hi)))))))

;;; quantile-from-sorted : (Vector Num) × Num → Num
;;; Compute p-th quantile from pre-sorted vector (internal helper).
(define (quantile-from-sorted sorted-vec p)
  (let* ([n (vector-length sorted-vec)]
         [index (* p (- n 1))]
         [lo (inexact->exact (floor index))]
         [hi (inexact->exact (ceiling index))]
         [h (- index lo)])
    (if (= lo hi)
        (vector-ref sorted-vec lo)
        (+ (* (- 1 h) (vector-ref sorted-vec lo))
           (* h (vector-ref sorted-vec hi))))))

;;; quantiles : (List Num) × (List Num) → (List Num)
;;; Compute multiple quantiles efficiently (sorts only once).
;;; ps is a list of probabilities in [0, 1].
;;; Returns list of quantile values in same order as ps.
;;;
;;; Example:
;;;   (quantiles '(1 2 3 4 5 6 7 8 9 10) '(0.25 0.5 0.75))
;;;   => (3.25 5.5 7.75)  ; Q1, median, Q3
(define (quantiles xs ps)
  (for-each
    (lambda (p)
      (when (or (< p 0) (> p 1))
        (error 'quantiles "all probabilities must be in [0, 1]" p)))
    ps)
  (let ([sorted-vec (list->vector (list-sort < xs))])
    (map (lambda (p) (quantile-from-sorted sorted-vec p)) ps)))

;;; iqr : (List Num) → Num
;;; Interquartile range (Q3 - Q1).
(define (iqr xs)
  (let ([qs (quantiles xs '(0.25 0.75))])
    (- (cadr qs) (car qs))))

;;; range-stat : (List Num) → Num
;;; Range (max - min).
(define (range-stat xs)
  (- (apply max xs) (apply min xs)))

;;; ====
;;; Covariance and Correlation
;;; ====

;;; covariance : (List Num) × (List Num) → Num
;;; Compute sample covariance.
(define (covariance xs ys)
  (if (not (= (length xs) (length ys)))
      (error 'covariance "lists must have same length")
      (let* ([n (length xs)]
             [mu-x (mean xs)]
             [mu-y (mean ys)]
             [prod-sum (fold-left + 0
                                  (map (lambda (x y) (* (- x mu-x) (- y mu-y)))
                                       xs ys))])
            (if (<= n 1)
                0
                (/ prod-sum (- n 1))))))

;;; correlation : (List Num) × (List Num) → Num
;;; Compute Pearson correlation coefficient.
(define (correlation xs ys)
  (let ([cov (covariance xs ys)]
        [sx (std-dev xs)]
        [sy (std-dev ys)])
       (if (or (= sx 0) (= sy 0))
           0  ; undefined, return 0
           (/ cov (* sx sy)))))

;;; ====
;;; Vector-Optimized Versions
;;; ====

;;; vec-sum : Vec → Num
;;; Sum of vector elements.
(define (vec-sum v)
  (let ([n (vector-length v)])
       (let loop ([i 0] [s 0])
            (if (= i n)
                s
                (loop (+ i 1) (+ s (vector-ref v i)))))))

;;; vec-mean : Vec → Num
;;; Mean of vector elements.
(define (vec-mean v)
  (let ([n (vector-length v)])
       (if (= n 0)
           (error 'vec-mean "empty vector")
           (/ (vec-sum v) n))))

;;; vec-variance : Vec → Num
;;; Sample variance of vector elements.
(define (vec-variance v)
  (let* ([n (vector-length v)]
         [mu (vec-mean v)])
        (if (<= n 1)
            0
            (let loop ([i 0] [ss 0])
                 (if (= i n)
                     (/ ss (- n 1))
                     (let ([d (- (vector-ref v i) mu)])
                          (loop (+ i 1) (+ ss (* d d)))))))))

;;; vec-std-dev : Vec → Num
;;; Sample standard deviation of vector elements.
(define (vec-std-dev v)
  (sqrt (vec-variance v)))

;;; vec-min : Vec → Num
;;; Minimum element.
(define (vec-min v)
  (let ([n (vector-length v)])
       (if (= n 0)
           (error 'vec-min "empty vector")
           (let loop ([i 1] [m (vector-ref v 0)])
                (if (= i n)
                    m
                    (loop (+ i 1) (min m (vector-ref v i))))))))

;;; vec-max : Vec → Num
;;; Maximum element.
(define (vec-max v)
  (let ([n (vector-length v)])
       (if (= n 0)
           (error 'vec-max "empty vector")
           (let loop ([i 1] [m (vector-ref v 0)])
                (if (= i n)
                    m
                    (loop (+ i 1) (max m (vector-ref v i))))))))

;;; vec-median : Vec → Num
;;; Median of vector elements.
(define (vec-median v)
  (median (vector->list v)))

;;; vec-quantile : Vec × Num → Num
;;; p-th quantile of vector elements.
(define (vec-quantile v p)
  (quantile (vector->list v) p))

;;; vec-quantiles : Vec × (List Num) → (List Num)
;;; Multiple quantiles of vector elements (sorts only once).
(define (vec-quantiles v ps)
  (quantiles (vector->list v) ps))

;;; vec-covariance : Vec × Vec → Num
;;; Sample covariance of two vectors.
(define (vec-covariance v1 v2)
  (let* ([n (vector-length v1)])
        (if (not (= n (vector-length v2)))
            (error 'vec-covariance "vectors must have same length")
            (let* ([mu1 (vec-mean v1)]
                   [mu2 (vec-mean v2)])
                  (if (<= n 1)
                      0
                      (let loop ([i 0] [s 0])
                           (if (= i n)
                               (/ s (- n 1))
                               (loop (+ i 1)
                                     (+ s (* (- (vector-ref v1 i) mu1)
                                             (- (vector-ref v2 i) mu2)))))))))))

;;; vec-correlation : Vec × Vec → Num
;;; Pearson correlation of two vectors.
(define (vec-correlation v1 v2)
  (let ([cov (vec-covariance v1 v2)]
        [s1 (vec-std-dev v1)]
        [s2 (vec-std-dev v2)])
       (if (or (= s1 0) (= s2 0))
           0
           (/ cov (* s1 s2)))))

;;; ====
;;; Additional Utilities
;;; ====

;;; sem : (List Num) → Num
;;; Standard error of the mean.
(define (sem xs)
  (/ (std-dev xs) (sqrt (length xs))))

;;; vec-sem : Vec → Num
;;; Standard error of the mean (vector version).
(define (vec-sem v)
  (/ (vec-std-dev v) (sqrt (vector-length v))))

;;; skewness : (List Num) → Num
;;; Sample skewness (Fisher's definition).
(define (skewness xs)
  (let* ([n (length xs)]
         [mu (mean xs)]
         [s (std-dev xs)]
         [m3 (/ (fold-left + 0 (map (lambda (x) (expt (- x mu) 3)) xs)) n)])
        (if (= s 0) 0 (/ m3 (expt s 3)))))

;;; kurtosis : (List Num) → Num
;;; Sample excess kurtosis.
(define (kurtosis xs)
  (let* ([n (length xs)]
         [mu (mean xs)]
         [s (std-dev xs)]
         [m4 (/ (fold-left + 0 (map (lambda (x) (expt (- x mu) 4)) xs)) n)])
        (if (= s 0) 0 (- (/ m4 (expt s 4)) 3))))
