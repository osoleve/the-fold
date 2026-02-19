;;; summary-stats.sexp — Development tasks for lattice/statistics/core/summary-stats.ss
;;;
;;; Module: summary-stats (tier 0, depends on prelude, sort)
;;; 302 lines, ~20 exported functions
;;; Descriptive statistics: mean, variance, standard deviation,
;;; median, quantiles, covariance, correlation, skewness, kurtosis.
;;; Both list and vector-optimized versions.
;;;
;;; Git history:
;;;   a3abed54 refactor: Reorganize into core/lattice/shell three-layer architecture
;;;   2696481c feat(module): migrate info/, automata/, number-theory/ to require

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement mean
  ;; ──────────────────────────────────────────────
  ;; Arithmetic mean — the most basic statistic.

  (task
    (id "statistics/summary-stats/mean-impl")
    (module summary-stats)
    (tier 0)
    (type implement)
    (description "Implement mean: compute the arithmetic mean of a list of numbers. Mean = sum(xs) / length(xs). Use fold-left to sum the elements and length for the count. Error on empty list. This is the first moment of the empirical distribution and the foundation for all other moments (variance, skewness, kurtosis).")
    (context "Available: null?, fold-left, length, +, /, error. Single-pass fold.")
    (before "(define (mean xs)
  ;; TODO: arithmetic mean
  0)")
    (test-file "lattice/statistics/core/test-summary-stats.ss")
    (test-forms (";; Simple mean
(assert-equal 4 (mean '(2 4 6)))"
                 ";; Single element
(assert-equal 7 (mean '(7)))"
                 ";; Float mean
(assert-true (< (abs (- (mean '(1.0 2.0 3.0)) 2.0)) 0.001))"))
    (available-modules (prelude sort))
    (hints ("Guard: (null? xs) → (error 'mean \"empty list\")"
            "Sum: (fold-left + 0 xs)"
            "Return: (/ sum (length xs))"))
    (reference-solution "(define (mean xs)
  (if (null? xs)
      (error 'mean \"empty list\")
      (/ (fold-left + 0 xs) (length xs))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement variance
  ;; ──────────────────────────────────────────────
  ;; Sample variance with Bessel's correction.

  (task
    (id "statistics/summary-stats/variance-impl")
    (module summary-stats)
    (tier 0)
    (type implement)
    (description "Implement variance: compute the sample variance of a list of numbers using Bessel's correction (n-1 denominator). Steps: (1) Compute mean mu. (2) Map each element to (x - mu)^2. (3) Sum the squared deviations. (4) Divide by (n-1), not n. The n-1 correction makes this an unbiased estimator of the population variance. Return 0 if n <= 1 (variance undefined for single element, convention is 0).")
    (context "Available: length, mean (computes arithmetic mean), fold-left, map, expt, -, +, /, <=, lambda. Two-pass: first mean, then deviation sum.")
    (before "(define (variance xs)
  ;; TODO: sample variance with Bessel's correction
  0)")
    (test-file "lattice/statistics/core/test-summary-stats.ss")
    (test-forms (";; Known variance: [2,4,4,4,5,5,7,9] → 32/7
(assert-true (< (abs (- (variance '(2 4 4 4 5 5 7 9)) 32/7)) 0.001))"
                 ";; Single element → 0
(assert-equal 0 (variance '(5)))"
                 ";; Constant list → 0
(assert-equal 0 (variance '(3 3 3 3)))"))
    (available-modules (prelude sort))
    (hints ("Get n = (length xs), mu = (mean xs)"
            "Squared deviations: (map (lambda (x) (expt (- x mu) 2)) xs)"
            "Sum: ss = (fold-left + 0 squared-devs)"
            "Guard: if n <= 1 return 0"
            "Return: (/ ss (- n 1))"))
    (reference-solution "(define (variance xs)
  (let* ([n (length xs)]
         [mu (mean xs)]
         [ss (fold-left + 0 (map (lambda (x) (expt (- x mu) 2)) xs))])
    (if (<= n 1)
        0
        (/ ss (- n 1)))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement median
  ;; ──────────────────────────────────────────────
  ;; The robust central tendency measure.

  (task
    (id "statistics/summary-stats/median-impl")
    (module summary-stats)
    (tier 0)
    (type implement)
    (description "Implement median: compute the median (middle value) of a list of numbers. Sort the list first. If the count is odd, return the middle element. If even, return the average of the two middle elements. Use sort-by to sort, quotient for integer division, list-ref for indexing. The median is robust to outliers unlike the mean — replacing the largest value with infinity doesn't change the median.")
    (context "Available: sort-by (sort with comparator), <, length, quotient, odd?, list-ref, +, -, /. Sort then index.")
    (before "(define (median xs)
  ;; TODO: middle value after sorting
  0)")
    (test-file "lattice/statistics/core/test-summary-stats.ss")
    (test-forms (";; Odd count: middle element
(assert-equal 5 (median '(1 3 5 7 9)))"
                 ";; Even count: average of two middle
(assert-equal 3 (median '(1 2 4 5)))"
                 ";; Single element
(assert-equal 7 (median '(7)))"
                 ";; Unsorted input
(assert-equal 3 (median '(5 1 3)))"))
    (available-modules (prelude sort))
    (hints ("Sort: (sort-by < xs)"
            "Get n and mid = (quotient n 2)"
            "If (odd? n): (list-ref sorted mid)"
            "If even: (/ (+ (list-ref sorted (- mid 1)) (list-ref sorted mid)) 2)"))
    (reference-solution "(define (median xs)
  (let* ([sorted (sort-by < xs)]
         [n (length sorted)]
         [mid (quotient n 2)])
    (if (odd? n)
        (list-ref sorted mid)
        (/ (+ (list-ref sorted (- mid 1))
              (list-ref sorted mid))
           2))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement covariance
  ;; ──────────────────────────────────────────────
  ;; How two variables move together.

  (task
    (id "statistics/summary-stats/covariance-impl")
    (module summary-stats)
    (tier 0)
    (type implement)
    (description "Implement covariance: compute the sample covariance between two lists of numbers. Cov(X,Y) = (1/(n-1)) * sum((xi - mu_x)(yi - mu_y)). Steps: (1) Verify equal lengths. (2) Compute means of both lists. (3) Map the product of deviations. (4) Sum and divide by (n-1). Return 0 if n <= 1. Positive covariance means the variables tend to increase together; negative means one increases as the other decreases.")
    (context "Available: length, =, not, mean, fold-left, map, *, -, +, /, <=, lambda, error. Two means then a paired map.")
    (before "(define (covariance xs ys)
  ;; TODO: sample covariance
  0)")
    (test-file "lattice/statistics/core/test-summary-stats.ss")
    (test-forms (";; Perfect positive correlation
(assert-true (> (covariance '(1 2 3 4 5) '(2 4 6 8 10)) 0))"
                 ";; Perfect negative correlation
(assert-true (< (covariance '(1 2 3 4 5) '(10 8 6 4 2)) 0))"
                 ";; Zero covariance (independent-like)
(assert-true (< (abs (covariance '(1 2 3 4) '(1 -1 1 -1))) 0.001))"))
    (available-modules (prelude sort))
    (hints ("Guard: (not (= (length xs) (length ys))) → error"
            "Compute mu-x = (mean xs), mu-y = (mean ys)"
            "Products: (map (lambda (x y) (* (- x mu-x) (- y mu-y))) xs ys)"
            "Sum: (fold-left + 0 products)"
            "Guard: n <= 1 → 0"
            "Return: (/ sum (- n 1))"))
    (reference-solution "(define (covariance xs ys)
  (if (not (= (length xs) (length ys)))
      (error 'covariance \"lists must have same length\")
      (let* ([n (length xs)]
             [mu-x (mean xs)]
             [mu-y (mean ys)]
             [prod-sum (fold-left + 0
                                  (map (lambda (x y) (* (- x mu-x) (- y mu-y)))
                                       xs ys))])
        (if (<= n 1)
            0
            (/ prod-sum (- n 1))))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Implement correlation
  ;; ──────────────────────────────────────────────
  ;; Normalized covariance — the dimensionless relationship.

  (task
    (id "statistics/summary-stats/correlation-impl")
    (module summary-stats)
    (tier 0)
    (type implement)
    (description "Implement correlation: compute the Pearson correlation coefficient between two lists. r = Cov(X,Y) / (std-dev(X) * std-dev(Y)). The result is always in [-1, 1]. r=1 means perfect positive linear relationship, r=-1 means perfect negative, r=0 means no linear relationship. If either standard deviation is 0 (constant data), return 0 (undefined, but 0 is the convention). Use the existing covariance and std-dev functions.")
    (context "Available: covariance (sample covariance), std-dev (sample standard deviation), =, or, /, *. Three function calls and a guard.")
    (before "(define (correlation xs ys)
  ;; TODO: Pearson correlation coefficient
  0)")
    (test-file "lattice/statistics/core/test-summary-stats.ss")
    (test-forms (";; Perfect positive correlation → 1
(assert-true (< (abs (- (correlation '(1 2 3 4 5) '(2 4 6 8 10)) 1.0)) 0.001))"
                 ";; Perfect negative → -1
(assert-true (< (abs (- (correlation '(1 2 3 4 5) '(10 8 6 4 2)) -1.0)) 0.001))"
                 ";; Constant data → 0
(assert-equal 0 (correlation '(5 5 5) '(1 2 3)))"))
    (available-modules (prelude sort))
    (hints ("Compute cov = (covariance xs ys)"
            "Compute sx = (std-dev xs), sy = (std-dev ys)"
            "Guard: if either sx or sy is 0, return 0"
            "Return: (/ cov (* sx sy))"))
    (reference-solution "(define (correlation xs ys)
  (let ([cov (covariance xs ys)]
        [sx (std-dev xs)]
        [sy (std-dev ys)])
    (if (or (= sx 0) (= sy 0))
        0
        (/ cov (* sx sy)))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

) ;; end tasks
