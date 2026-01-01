; ============================================================
; Numeric - Statistics
; Statistical functions: mean, median, variance, correlation, etc.
; Part of numeric.ss module
; ============================================================

; -- Central Tendency --

; mean: Arithmetic mean (alias: average)
(mean (fn (lst)
          (if (null? lst)
              0
              (/ (sum-list lst) (length lst)))))

; median: Middle value of sorted list
(median (fn (lst)
            (if (null? lst)
                0
                (let ((sorted (sort lst)))
                     (let ((n (length sorted)))
                          (let ((mid (/ n 2)))
                               (if (even? n)
                                   (/ (+ (nth (- mid 1) sorted) (nth mid sorted)) 2)
                                   (nth mid sorted))))))))

; mode: Most frequent element
(mode (fn (lst)
          (let ((freqs (frequencies lst)))
               (car (max-by cadr freqs)))))

; -- Dispersion --

; variance: Sample variance (n-1 denominator)
(variance (fn (lst)
              (if (null? lst)
                  0
                  (let ((m (mean lst))
                        (n (length lst)))
                       (/ (sum-list (map (fn (x) (* (- x m) (- x m))) lst))
                          (- n 1))))))

; population-variance: Population variance (n denominator)
(population-variance (fn (lst)
                         (if (null? lst)
                             0
                             (let ((m (mean lst))
                                   (n (length lst)))
                                  (/ (foldl (fn (acc x) (+ acc (* (- x m) (- x m)))) 0 lst) n)))))

; std-dev: Standard deviation
(std-dev (fn (lst) (sqrt (variance lst))))

; -- Percentiles and Quartiles --

; percentile: Get nth percentile (0-100)
(percentile (fn (p lst)
                (if (null? lst)
                    0
                    (let ((sorted (sort lst))
                          (n (length lst))
                          (idx (/ (* p (- n 1)) 100)))
                         (nth (floor idx) sorted)))))

; quartiles: Get Q1, Q2 (median), Q3
(quartiles (fn (lst)
               (list (percentile 25 lst)
                     (percentile 50 lst)
                     (percentile 75 lst))))

; interquartile-range: Q3 - Q1
(interquartile-range (fn (lst)
                         (let ((qs (quartiles lst)))
                              (- (caddr qs) (car qs)))))

; -- Standardization --

; z-score: Calculate z-score of single value relative to list
(z-score (fn (x lst)
             (let ((m (mean lst)))
                  (let ((std (sqrt (variance lst))))
                       (if (= std 0) 0 (/ (- x m) std))))))

; z-scores: Convert entire list to z-scores (standard scores)
(z-scores (fn (lst)
              (let ((m (mean lst))
                    (sd (std-dev lst)))
                   (if (= sd 0)
                       (map (const 0) lst)
                       (map (fn (x) (/ (- x m) sd)) lst)))))

; normalize-list: Normalize list to [0, 1] range
(normalize-list (fn (lst)
                    (let ((mn (apply min lst))
                          (mx (apply max lst)))
                         (let ((range (- mx mn)))
                              (if (= range 0)
                                  (replicate (length lst) 0)
                                  (map (fn (x) (/ (- x mn) range)) lst))))))

; standardize: Standardize list (alias for z-scores)
(standardize z-scores)

; -- Correlation and Regression --

; covariance: Calculate covariance of two lists
(covariance (fn (xs ys)
                (let ((mx (mean xs))
                      (my (mean ys))
                      (n (length xs)))
                     (/ (sum-list (zip-with * (map (fn (x) (- x mx)) xs)
                                            (map (fn (y) (- y my)) ys)))
                        (- n 1)))))

; correlation: Pearson correlation coefficient
(correlation (fn (xs ys)
                 (let ((cov (covariance xs ys))
                       (sx (sqrt (variance xs)))
                       (sy (sqrt (variance ys))))
                      (if (or (= sx 0) (= sy 0))
                          0
                          (/ cov (* sx sy))))))

; linear-regression: Simple linear regression (returns (slope . intercept))
(linear-regression (fn (xs ys)
                       (let ((n (length xs))
                             (mean-x (mean xs))
                             (mean-y (mean ys)))
                            (let ((num (foldl + 0 (map (fn (xy)
                                                           (* (- (car xy) mean-x)
                                                              (- (cdr xy) mean-y)))
                                                       (zip xs ys))))
                                  (denom (foldl + 0 (map (fn (x) (* (- x mean-x) (- x mean-x))) xs))))
                                 (if (= denom 0)
                                     (cons 0 mean-y)
                                     (let ((slope (/ num denom)))
                                          (cons slope (- mean-y (* slope mean-x)))))))))

; -- Other Statistical Functions --

; majority: Element appearing more than n/2 times (or #f)
(majority (fn (lst)
              (let ((n (length lst))
                    (freqs (frequencies lst)))
                   (let ((candidates (filter (fn (p) (> (cadr p) (/ n 2))) freqs)))
                        (if (null? candidates) #f (caar candidates))))))

; --- Module Exports ---
; (see exports.ss for exported symbols)
