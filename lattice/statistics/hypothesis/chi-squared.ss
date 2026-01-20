(load "core/base/prelude.ss")
(load "lattice/linalg/matrix.ss")
(load "lattice/statistics/core/result-types.ss")
(load "lattice/statistics/hypothesis/distributions.ss")

(doc 'module 'chi-squared)
(doc 'description "Chi-Squared Tests — Goodness-of-fit and independence tests")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'description "chi-squared-test-goodness: Compare observed vs expected frequencies")
(doc 'description "chi-squared-test-independence: Test independence in contingency table")

(doc 'section 'goodness-of-fit-test)
(define (chi-squared-test-goodness observed expected)
  (doc 'export #t)
  (doc 'type '(-> Vec Vec TestResult))
  (doc 'description "Test H0: observed frequencies match expected frequencies")
  (doc 'note "chi-sq = sum((O - E)² / E)")
  (doc 'param 'observed "observed frequency counts")
  (doc 'param 'expected "expected frequency counts (must sum to same as observed)")
  (let* ([n (vector-length observed)])
        (if (not (= n (vector-length expected)))
            (error 'chi-squared-test-goodness "vectors must have equal length")
            (let* ([chi-sq (let loop ([i 0] [sum 0])
                                (if (= i n)
                                    sum
                                    (let* ([o (vector-ref observed i)]
                                           [e (vector-ref expected i)]
                                           [e-safe (max e 1e-10)]
                                           [contrib (/ (expt (- o e) 2) e-safe)])
                                          (loop (+ i 1) (+ sum contrib)))))]
                   [df (- n 1)]
                   [p-value (chi-squared-pvalue chi-sq df)])
                  (make-test-result 'chi-squared-goodness chi-sq df p-value #f #f)))))

;;; chi-squared-test-goodness-uniform : Vec → TestResult
;;; Test against uniform distribution.
(define (chi-squared-test-goodness-uniform observed)
  (let* ([n (vector-length observed)]
         [total (vec-sum observed)]
         [expected-val (/ total n)]
         [expected (make-vector n expected-val)])
        (chi-squared-test-goodness observed expected)))

;;; chi-squared-test-goodness-proportions : Vec × Vec → TestResult
;;; Test observed counts against expected proportions.
;;;   observed: observed counts
;;;   proportions: expected proportions (should sum to 1)
(define (chi-squared-test-goodness-proportions observed proportions)
  (let* ([n (vector-length observed)]
         [total (vec-sum observed)]
         [expected (make-vector n)])
        (do ([i 0 (+ i 1)])
            [(= i n)]
            (vector-set! expected i (* total (vector-ref proportions i))))
        (chi-squared-test-goodness observed expected)))

;;; ====
;;; Independence Test
;;; ====

;;; chi-squared-test-independence : Matrix → TestResult
;;; Test H0: rows and columns are independent in contingency table.
;;; chi-sq = sum((O_ij - E_ij)² / E_ij)
;;; where E_ij = (row_i_total * col_j_total) / grand_total
(define (chi-squared-test-independence table)
  (doc 'export #t)
  (let* ([nrows (matrix-rows table)]
         [ncols (matrix-cols table)]
         ;; Compute row and column totals
         [row-totals (make-vector nrows 0)]
         [col-totals (make-vector ncols 0)]
         [grand-total 0])
        ;; Sum rows and columns
        (do ([i 0 (+ i 1)])
            [(= i nrows)]
            (do ([j 0 (+ j 1)])
                [(= j ncols)]
                (let ([val (matrix-ref table i j)])
                     (vector-set! row-totals i (+ (vector-ref row-totals i) val))
                     (vector-set! col-totals j (+ (vector-ref col-totals j) val))
                     (set! grand-total (+ grand-total val)))))
        ;; Compute chi-squared statistic
        (let* ([chi-sq
                (let loop-i ([i 0] [sum 0])
                     (if (= i nrows)
                         sum
                         (loop-i
                          (+ i 1)
                          (let loop-j ([j 0] [s sum])
                               (if (= j ncols)
                                   s
                                   (let* ([o (matrix-ref table i j)]
                                          [e (/ (* (vector-ref row-totals i)
                                                   (vector-ref col-totals j))
                                                (max grand-total 1))]
                                          [e-safe (max e 1e-10)]
                                          [contrib (/ (expt (- o e) 2) e-safe)])
                                         (loop-j (+ j 1) (+ s contrib))))))))]
               [df (* (- nrows 1) (- ncols 1))]
               [p-value (chi-squared-pvalue chi-sq df)]
               ;; Cramér's V as effect size
               [n grand-total]
               [k (min nrows ncols)]
               [cramers-v (sqrt (/ chi-sq (* n (max (- k 1) 1))))])
              (make-test-result 'chi-squared-independence chi-sq df p-value
                                #f cramers-v))))

;;; ====
;;; Helper Functions
;;; ====

;;; vec-sum : Vec → Num
(define (vec-sum v)
  (let ([n (vector-length v)])
       (let loop ([i 0] [s 0])
            (if (= i n)
                s
                (loop (+ i 1) (+ s (vector-ref v i)))))))

;;; contingency-table : Vec × Vec → Matrix
;;; Create contingency table from two categorical vectors.
;;; Returns (table, row-levels, col-levels).
(define (contingency-table xs ys)
  (if (not (= (vector-length xs) (vector-length ys)))
      (error 'contingency-table "vectors must have equal length")
      (let* ([n (vector-length xs)]
             ;; Get unique values
             [x-levels (unique-vector xs)]
             [y-levels (unique-vector ys)]
             [nr (vector-length x-levels)]
             [nc (vector-length y-levels)]
             [table-data (make-vector (* nr nc) 0)]
             [table (list 'matrix nr nc table-data)])
            ;; Count occurrences
            (do ([k 0 (+ k 1)])
                [(= k n)]
                (let* ([x-val (vector-ref xs k)]
                       [y-val (vector-ref ys k)]
                       [i (vector-index x-levels x-val)]
                       [j (vector-index y-levels y-val)])
                      (when (and i j)
                            (let ([idx (+ (* i nc) j)])
                                 (vector-set! table-data idx
                                              (+ (vector-ref table-data idx) 1))))))
            (list table x-levels y-levels))))

;;; unique-vector : Vec → Vec
;;; Get unique values from vector, preserving order.
(define (unique-vector v)
  (let* ([n (vector-length v)]
         [seen '()])
        (do ([i 0 (+ i 1)])
            [(= i n)]
            (let ([val (vector-ref v i)])
                 (unless (member val seen)
                         (set! seen (cons val seen)))))
        (list->vector (reverse seen))))

;;; vector-index : Vec × Any → Nat | #f
;;; Find index of value in vector.
(define (vector-index v val)
  (let ([n (vector-length v)])
       (let loop ([i 0])
            (if (= i n)
                #f
                (if (equal? (vector-ref v i) val)
                    i
                    (loop (+ i 1)))))))
