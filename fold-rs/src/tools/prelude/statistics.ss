; ============================================================
; Statistical Utility Functions
; Descriptive statistics and percentiles
; ============================================================

; percentile-val: Get value at given percentile (0-100)
(percentile-val (fn (p lst)
                   (if (null? lst)
                       0
                       (let ((sorted (sort lst)))
                            (let ((n (length lst)))
                                 (let ((idx (floor (/ (* p (- n 1)) 100))))
                                      (nth idx sorted)))))))

; quartiles: Get 25th, 50th, and 75th percentiles
(quartiles (fn (lst)
              (list (percentile-val 25 lst)
                    (percentile-val 50 lst)
                    (percentile-val 75 lst))))

; iqr: Interquartile range
(iqr (fn (lst)
        (let ((qs (quartiles lst)))
             (- (caddr qs) (car qs)))))

; median-val: Middle value (50th percentile)
(median-val (fn (lst)
               (percentile-val 50 lst)))
