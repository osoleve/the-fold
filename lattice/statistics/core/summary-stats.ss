(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
;;; @module summary-stats
;;; @requires prelude sort vec
(require 'prelude)
(require 'sort)
(require 'vec)

(doc 'module 'summary-stats)
(doc 'description "Descriptive Statistics — Basic statistical summary functions")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'description "Mean, variance, standard deviation")
(doc 'description "Median, quantiles")
(doc 'description "Covariance, correlation")
(doc 'description "Vector-optimized versions")

(doc 'section 'basic-statistics-list-versions)

(define (mean xs)
  (doc 'type '(-> (List Num) Num))
  (doc 'description "Compute arithmetic mean")
  (if (null? xs)
      (error 'mean "empty list")
      (/ (fold-left + 0 xs) (length xs))))

;;; variance : (List Num) → Num
;;; Compute sample variance (n-1 denominator).
;;; Returns 0 for single-element lists. Errors on empty lists.
(define (variance xs)
  (let ([n (length xs)])
    (if (= n 0)
        (error 'variance "empty list")
        (let* ([mu (mean xs)]
               [ss (fold-left + 0 (map (lambda (x) (expt (- x mu) 2)) xs))])
          (if (<= n 1)
              0
              (/ ss (- n 1)))))))

;;; variance-population : (List Num) → Num
;;; Compute population variance (n denominator).
;;; Errors on empty lists.
(define (variance-population xs)
  (let ([n (length xs)])
    (if (= n 0)
        (error 'variance-population "empty list")
        (let* ([mu (mean xs)]
               [ss (fold-left + 0 (map (lambda (x) (expt (- x mu) 2)) xs))])
          (/ ss n)))))

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
;;; Errors on empty lists.
(define (median xs)
  (if (null? xs)
      (error 'median "empty list")
      (let* ([sorted (sort-by < xs)]
             [n (length sorted)]
             [mid (quotient n 2)])
        (if (odd? n)
            (list-ref sorted mid)
            (/ (+ (list-ref sorted (- mid 1))
                  (list-ref sorted mid))
               2)))))

;;; quantile : (List Num) × Num → Num
;;; Compute p-th quantile (0 <= p <= 1).
;;; Uses linear interpolation (type 7, R default).
(define (quantile xs p)
  (if (null? xs)
      (error 'quantile "empty list")
      (if (or (< p 0) (> p 1))
          (error 'quantile "p must be in [0, 1]" p)
          (let* ([sorted (sort-by < xs)]
                 [n (length sorted)]
                 [index (* p (- n 1))]
                 [lo (inexact->exact (floor index))]
                 [hi (inexact->exact (ceiling index))]
                 [h (- index lo)])
            (if (= lo hi)
                (list-ref sorted lo)
                (+ (* (- 1 h) (list-ref sorted lo))
                   (* h (list-ref sorted hi))))))))

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
  (doc 'export #t)
  (if (null? xs)
      (error 'quantiles "empty list")
      (begin
        (for-each
          (lambda (p)
            (when (or (< p 0) (> p 1))
              (error 'quantiles "all probabilities must be in [0, 1]" p)))
          ps)
        (let ([sorted-vec (list->vector (sort-by < xs))])
          (map (lambda (p) (quantile-from-sorted sorted-vec p)) ps)))))

;;; iqr : (List Num) → Num
;;; Interquartile range (Q3 - Q1).
(define (iqr xs)
  (let ([qs (quantiles xs '(0.25 0.75))])
    (- (cadr qs) (car qs))))

;;; range-stat : (List Num) → Num
;;; Range (max - min). Errors on empty lists. Returns 0 for single-element lists.
(define (range-stat xs)
  (if (null? xs)
      (error 'range-stat "empty list")
      (- (apply max xs) (apply min xs))))

;;; ====
;;; Covariance and Correlation
;;; ====

;;; covariance : (List Num) × (List Num) → Num
;;; Compute sample covariance. Errors on empty lists.
(define (covariance xs ys)
  (if (null? xs)
      (error 'covariance "empty list")
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
                (/ prod-sum (- n 1)))))))

;;; correlation : (List Num) × (List Num) → Num
;;; Compute Pearson correlation coefficient.
;;; Returns 0 when either variable has zero variance (no information).
;;; Errors on empty lists.
(define (correlation xs ys)
  (if (null? xs)
      (error 'correlation "empty list")
      (let ([cov (covariance xs ys)]
            [sx (std-dev xs)]
            [sy (std-dev ys)])
        (if (or (= sx 0) (= sy 0))
            0  ; undefined, return 0
            (/ cov (* sx sy))))))

;;; ====
;;; Vector-Optimized Versions
;;; ====

;;; vec-sum : Vec → Num
;;; Sum of vector elements.
;;; vec-mean : Vec → Num
;;; Mean of vector elements.
(define (vec-mean v)
  (doc 'export #t)
  (let ([n (vector-length v)])
       (if (= n 0)
           (error 'vec-mean "empty vector")
           (/ (vec-sum v) n))))

;;; vec-variance : Vec → Num
;;; Sample variance using Bessel's correction (n-1 denominator).
;;; For population variance (n denominator), use (/ (* var (- n 1)) n).
(define (vec-variance v)
  (doc 'export #t)
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
;;; Sample standard deviation (sqrt of Bessel-corrected variance).
(define (vec-std-dev v)
  (doc 'export #t)
  (sqrt (vec-variance v)))

;;; vec-median : Vec → Num
;;; Median of vector elements. Errors on empty vectors.
(define (vec-median v)
  (doc 'export #t)
  (if (= (vector-length v) 0)
      (error 'vec-median "empty vector")
      (median (vector->list v))))

;;; vec-quantile : Vec × Num → Num
;;; p-th quantile of vector elements.
(define (vec-quantile v p)
  (doc 'export #t)
  (quantile (vector->list v) p))

;;; vec-quantiles : Vec × (List Num) → (List Num)
;;; Multiple quantiles of vector elements (sorts only once).
(define (vec-quantiles v ps)
  (doc 'export #t)
  (quantiles (vector->list v) ps))

;;; vec-covariance : Vec × Vec → Num
;;; Sample covariance of two vectors. Errors on empty vectors.
(define (vec-covariance v1 v2)
  (doc 'export #t)
  (let ([n (vector-length v1)])
    (if (= n 0)
        (error 'vec-covariance "empty vector")
        (if (not (= n (vector-length v2)))
            (error 'vec-covariance "vectors must have same length")
            (if (<= n 1)
                0
                (let* ([mu1 (vec-mean v1)]
                       [mu2 (vec-mean v2)])
                  (let loop ([i 0] [s 0])
                    (if (= i n)
                        (/ s (- n 1))
                        (loop (+ i 1)
                              (+ s (* (- (vector-ref v1 i) mu1)
                                      (- (vector-ref v2 i) mu2))))))))))))

;;; vec-correlation : Vec × Vec → Num
;;; Pearson correlation of two vectors.
;;; Returns 0 when either variable has zero variance. Errors on empty vectors.
(define (vec-correlation v1 v2)
  (doc 'export #t)
  (if (= (vector-length v1) 0)
      (error 'vec-correlation "empty vector")
      (let ([cov (vec-covariance v1 v2)]
            [s1 (vec-std-dev v1)]
            [s2 (vec-std-dev v2)])
        (if (or (= s1 0) (= s2 0))
            0
            (/ cov (* s1 s2))))))

;;; ====
;;; Additional Utilities
;;; ====

;;; sem : (List Num) → Num
;;; Standard error of the mean. Errors on empty lists.
(define (sem xs)
  (if (null? xs)
      (error 'sem "empty list")
      (/ (std-dev xs) (sqrt (length xs)))))

;;; vec-sem : Vec → Num
;;; Standard error of the mean (vector version). Errors on empty vectors.
(define (vec-sem v)
  (let ([n (vector-length v)])
    (if (= n 0)
        (error 'vec-sem "empty vector")
        (/ (vec-std-dev v) (sqrt n)))))

;;; skewness : (List Num) → Num
;;; Population skewness (biased estimator).
;;; Uses population std-dev (n denominator) for consistency:
;;; g1 = m3 / sigma^3 where m3 = (1/n) sum((xi - mu)^3), sigma = sqrt(m2).
;;; Symmetric distributions have skewness 0; right-skewed > 0, left-skewed < 0.
(define (skewness xs)
  (doc 'export #t)
  (doc 'type '(-> (List Num) Num))
  (doc 'description "Population skewness (third standardized moment)")
  (if (null? xs)
      (error 'skewness "empty list")
      (let* ([n (length xs)]
             [mu (mean xs)]
             [s (std-dev-population xs)]
             [m3 (/ (fold-left + 0 (map (lambda (x) (expt (- x mu) 3)) xs)) n)])
        (if (= s 0) 0 (/ m3 (expt s 3))))))

;;; kurtosis : (List Num) → Num
;;; Excess kurtosis (biased estimator).
;;; Uses population std-dev (n denominator) for consistency:
;;; g2 = m4 / sigma^4 - 3 where m4 = (1/n) sum((xi - mu)^4), sigma = sqrt(m2).
;;; Normal distribution has excess kurtosis 0; heavy-tailed > 0 (leptokurtic),
;;; light-tailed < 0 (platykurtic).
(define (kurtosis xs)
  (doc 'export #t)
  (doc 'type '(-> (List Num) Num))
  (doc 'description "Excess kurtosis (fourth standardized moment minus 3)")
  (if (null? xs)
      (error 'kurtosis "empty list")
      (let* ([n (length xs)]
             [mu (mean xs)]
             [s (std-dev-population xs)]
             [m4 (/ (fold-left + 0 (map (lambda (x) (expt (- x mu) 4)) xs)) n)])
        (if (= s 0) 0 (- (/ m4 (expt s 4)) 3)))))

;;; vec-skewness : Vec → Num
;;; Population skewness for vector data. Errors on empty vectors.
(define (vec-skewness v)
  (doc 'type '(-> Vec Num))
  (doc 'export #t)
  (doc 'description "Population skewness (vector version)")
  (let* ([n (vector-length v)]
         [_ (when (= n 0) (error 'vec-skewness "empty vector"))]
         [mu (vec-mean v)]
         [m2 (let loop ([i 0] [s 0])
               (if (= i n) (/ s n)
                   (let ([d (- (vector-ref v i) mu)])
                     (loop (+ i 1) (+ s (* d d))))))]
         [sigma (sqrt m2)]
         [m3 (let loop ([i 0] [s 0])
               (if (= i n) (/ s n)
                   (let ([d (- (vector-ref v i) mu)])
                     (loop (+ i 1) (+ s (* d d d))))))])
    (if (= sigma 0) 0 (/ m3 (* sigma sigma sigma)))))

;;; vec-kurtosis : Vec → Num
;;; Excess kurtosis for vector data (population, biased estimator).
;;; Errors on empty vectors.
(define (vec-kurtosis v)
  (doc 'type '(-> Vec Num))
  (doc 'export #t)
  (doc 'description "Excess kurtosis (vector version)")
  (let* ([n (vector-length v)]
         [_ (when (= n 0) (error 'vec-kurtosis "empty vector"))]
         [mu (vec-mean v)]
         [m2 (let loop ([i 0] [s 0])
               (if (= i n) (/ s n)
                   (let ([d (- (vector-ref v i) mu)])
                     (loop (+ i 1) (+ s (* d d))))))]
         [sigma (sqrt m2)]
         [m4 (let loop ([i 0] [s 0])
               (if (= i n) (/ s n)
                   (let ([d (- (vector-ref v i) mu)])
                     (loop (+ i 1) (+ s (* d d d d))))))])
    (if (= sigma 0) 0 (- (/ m4 (* sigma sigma sigma sigma)) 3))))
