; ============================================================
; Statistical Functions
; Canonical versions - no duplicates
; ============================================================

; mean: Arithmetic mean
(mean (fn (lst)
          (if (null? lst)
              0
              (/ (sum-list lst) (length lst)))))

; variance: Sample variance
(variance (fn (lst)
              (if (null? lst)
                  0
                  (let ((m (mean lst))
                        (n (length lst)))
                       (/ (sum-list (map (fn (x) (square (- x m))) lst))
                          (- n 1))))))

; std-dev: Standard deviation
(std-dev (fn (lst)
             (sqrt (variance lst))))

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

; percentile: Get nth percentile of list (0-100)
(percentile (fn (p lst)
                (let ((sorted (sort lst)))
                     (let ((idx (/ (* p (- (length sorted) 1)) 100)))
                          (list-ref sorted idx)))))

; quartiles: Get Q1, Q2 (median), Q3
(quartiles (fn (lst)
               (list (percentile 25 lst)
                     (percentile 50 lst)
                     (percentile 75 lst))))

; interquartile-range: Q3 - Q1
(interquartile-range (fn (lst)
                         (let ((qs (quartiles lst)))
                              (- (caddr qs) (car qs)))))

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

; covariance: Calculate covariance of two lists
(covariance (fn (xs ys)
                (let ((mx (mean xs))
                      (my (mean ys))
                      (n (length xs)))
                     (/ (sum-list (zip-with * (map (fn (x) (- x mx)) xs)
                                            (map (fn (y) (- y my)) ys)))
                        (- n 1)))))

; correlation: Calculate Pearson correlation coefficient
(correlation (fn (xs ys)
                 (let ((cov (covariance xs ys))
                       (sx (sqrt (variance xs)))
                       (sy (sqrt (variance ys))))
                      (if (or (= sx 0) (= sy 0))
                          0
                          (/ cov (* sx sy))))))

; majority: Element appearing more than n/2 times (or #f)
(majority (fn (lst)
              (let ((n (length lst))
                    (freqs (frequencies lst)))
                   (let ((candidates (filter (fn (p) (> (cadr p) (/ n 2))) freqs)))
                        (if (null? candidates) #f (caar candidates))))))
