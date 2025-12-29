;;; fabric/stitches/fp/test-interval.ss -- Tests for Interval Arithmetic Library

;;; NOTE: Run from fabric/stitches directory

(load "fabric/stitches/test-framework.ss")
(load "fabric/stitches/fp/interval.ss")

(display "
")
(display "==============================================================
")
(display "               INTERVAL ARITHMETIC TESTS
")
(display "==============================================================
")

;;; Helper for floating-point comparison
(define (assert-within tolerance expected actual)
  (unless (< (abs (- expected actual)) tolerance)
          (error 'assert-within
                 (format "Expected ~a +/- ~a, got ~a (diff: ~a)"
                         expected tolerance actual (abs (- expected actual))))))

;;; ============================================================
;;; Interval Creation Tests
;;; ============================================================

(test-group interval-creation
            (define-test make-interval-test
              (let ([iv (make-interval 1 3)])
                   (assert-equal 1 (interval-lo iv))
                   (assert-equal 3 (interval-hi iv))))
            
            (define-test make-interval-swaps-test
              ;; Constructor should swap if lo > hi
              (let ([iv (make-interval 5 2)])
                   (assert-equal 2 (interval-lo iv))
                   (assert-equal 5 (interval-hi iv))))
            
            (define-test interval-singleton-test
              (let ([iv (interval-singleton 42)])
                   (assert-equal 42 (interval-lo iv))
                   (assert-equal 42 (interval-hi iv))
                   (assert-true (interval-singleton? iv))))
            
            (define-test interval-predicate-test
              (assert-true (interval? (make-interval 1 2)))
              (assert-false (interval? 42))
              (assert-false (interval? '(1 2)))
              (assert-false (interval? (vector 'not-interval 1 2)))))

;;; ============================================================
;;; Interval Properties Tests
;;; ============================================================

(test-group interval-properties
            (define-test interval-width-test
              (assert-equal 4 (interval-width (make-interval 2 6)))
              (assert-equal 0 (interval-width (interval-singleton 5))))
            
            (define-test interval-midpoint-test
              (assert-equal 4 (interval-midpoint (make-interval 2 6)))
              (assert-equal 5 (interval-midpoint (interval-singleton 5))))
            
            (define-test interval-radius-test
              (assert-equal 2 (interval-radius (make-interval 2 6)))
              (assert-equal 0 (interval-radius (interval-singleton 5))))
            
            (define-test interval-singleton-predicate-test
              (assert-true (interval-singleton? (interval-singleton 3)))
              (assert-false (interval-singleton? (make-interval 1 2))))
            
            (define-test interval-positive-test
              (assert-true (interval-positive? (make-interval 1 5)))
              (assert-false (interval-positive? (make-interval -1 5)))
              (assert-false (interval-positive? (make-interval -5 -1))))
            
            (define-test interval-negative-test
              (assert-false (interval-negative? (make-interval 1 5)))
              (assert-false (interval-negative? (make-interval -1 5)))
              (assert-true (interval-negative? (make-interval -5 -1))))
            
            (define-test interval-contains-zero-test
              (assert-true (interval-contains-zero? (make-interval -1 1)))
              (assert-true (interval-contains-zero? (make-interval 0 5)))
              (assert-true (interval-contains-zero? (make-interval -5 0)))
              (assert-false (interval-contains-zero? (make-interval 1 5)))
              (assert-false (interval-contains-zero? (make-interval -5 -1)))))

;;; ============================================================
;;; Interval Predicates Tests
;;; ============================================================

(test-group interval-predicates
            (define-test interval-contains-point-test
              (let ([iv (make-interval 2 5)])
                   (assert-true (interval-contains? iv 3))
                   (assert-true (interval-contains? iv 2))   ; boundary
                   (assert-true (interval-contains? iv 5))   ; boundary
                   (assert-false (interval-contains? iv 1))
                   (assert-false (interval-contains? iv 6))))
            
            (define-test interval-contains-interval-test
              (assert-true (interval-contains-interval?
                            (make-interval 1 10)
                            (make-interval 2 5)))
              (assert-false (interval-contains-interval?
                             (make-interval 2 5)
                             (make-interval 1 10))))
            
            (define-test interval-overlaps-test
              (assert-true (interval-overlaps?
                            (make-interval 1 5)
                            (make-interval 3 7)))
              (assert-true (interval-overlaps?
                            (make-interval 1 5)
                            (make-interval 5 7)))  ; touch at boundary
              (assert-false (interval-overlaps?
                             (make-interval 1 5)
                             (make-interval 6 10))))
            
            (define-test interval-disjoint-test
              (assert-true (interval-disjoint?
                            (make-interval 1 3)
                            (make-interval 5 7)))
              (assert-false (interval-disjoint?
                             (make-interval 1 5)
                             (make-interval 3 7)))))

;;; ============================================================
;;; Interval Arithmetic Tests
;;; ============================================================

(test-group interval-arithmetic
            ;; [1, 3] + [2, 4] = [3, 7]
            (define-test interval-add-test
              (let ([result (interval-add (make-interval 1 3)
                                          (make-interval 2 4))])
                   (assert-equal 3 (interval-lo result))
                   (assert-equal 7 (interval-hi result))))
            
            ;; -[2, 5] = [-5, -2]
            (define-test interval-negate-test
              (let ([result (interval-negate (make-interval 2 5))])
                   (assert-equal -5 (interval-lo result))
                   (assert-equal -2 (interval-hi result))))
            
            ;; [5, 10] - [1, 3] = [2, 9]
            (define-test interval-sub-test
              (let ([result (interval-sub (make-interval 5 10)
                                          (make-interval 1 3))])
                   (assert-equal 2 (interval-lo result))
                   (assert-equal 9 (interval-hi result))))
            
            ;; [2, 3] * [4, 5] = [8, 15]
            (define-test interval-mul-positive-test
              (let ([result (interval-mul (make-interval 2 3)
                                          (make-interval 4 5))])
                   (assert-equal 8 (interval-lo result))
                   (assert-equal 15 (interval-hi result))))
            
            ;; [-2, 3] * [4, 5] = [-10, 15]
            (define-test interval-mul-mixed-test
              (let ([result (interval-mul (make-interval -2 3)
                                          (make-interval 4 5))])
                   (assert-equal -10 (interval-lo result))
                   (assert-equal 15 (interval-hi result))))
            
            ;; [-3, -2] * [-5, -4] = [8, 15]
            (define-test interval-mul-negative-test
              (let ([result (interval-mul (make-interval -3 -2)
                                          (make-interval -5 -4))])
                   (assert-equal 8 (interval-lo result))
                   (assert-equal 15 (interval-hi result))))
            
            ;; [-2, 3] * [-1, 4] = [-8, 12]
            (define-test interval-mul-both-mixed-test
              (let ([result (interval-mul (make-interval -2 3)
                                          (make-interval -1 4))])
                   (assert-equal -8 (interval-lo result))
                   (assert-equal 12 (interval-hi result))))
            
            ;; 1/[2, 4] = [0.25, 0.5]
            (define-test interval-reciprocal-test
              (let ([result (interval-reciprocal (make-interval 2 4))])
                   (assert-true (just? result))
                   (assert-within 0.001 0.25 (interval-lo (from-just result)))
                   (assert-within 0.001 0.5 (interval-hi (from-just result)))))
            
            ;; 1/[-1, 1] = nothing (contains zero)
            (define-test interval-reciprocal-zero-test
              (let ([result (interval-reciprocal (make-interval -1 1))])
                   (assert-true (nothing? result))))
            
            ;; [6, 12] / [2, 3] = [2, 6]
            (define-test interval-div-test
              (let ([result (interval-div (make-interval 6 12)
                                          (make-interval 2 3))])
                   (assert-true (just? result))
                   (assert-within 0.001 2.0 (interval-lo (from-just result)))
                   (assert-within 0.001 6.0 (interval-hi (from-just result)))))
            
            ;; Division by zero-containing interval
            (define-test interval-div-zero-test
              (let ([result (interval-div (make-interval 1 2)
                                          (make-interval -1 1))])
                   (assert-true (nothing? result)))))

;;; ============================================================
;;; Scalar Operations Tests
;;; ============================================================

(test-group scalar-operations
            ;; 3 * [2, 4] = [6, 12]
            (define-test interval-scale-positive-test
              (let ([result (interval-scale 3 (make-interval 2 4))])
                   (assert-equal 6 (interval-lo result))
                   (assert-equal 12 (interval-hi result))))
            
            ;; -2 * [3, 5] = [-10, -6]
            (define-test interval-scale-negative-test
              (let ([result (interval-scale -2 (make-interval 3 5))])
                   (assert-equal -10 (interval-lo result))
                   (assert-equal -6 (interval-hi result))))
            
            ;; [2, 5] + 3 = [5, 8]
            (define-test interval-add-scalar-test
              (let ([result (interval-add-scalar (make-interval 2 5) 3)])
                   (assert-equal 5 (interval-lo result))
                   (assert-equal 8 (interval-hi result))))
            
            ;; [5, 10] - 2 = [3, 8]
            (define-test interval-sub-scalar-test
              (let ([result (interval-sub-scalar (make-interval 5 10) 2)])
                   (assert-equal 3 (interval-lo result))
                   (assert-equal 8 (interval-hi result)))))

;;; ============================================================
;;; Set Operations Tests
;;; ============================================================

(test-group set-operations
            ;; Union: [1, 5] ∪ [3, 8] = [1, 8]
            (define-test interval-union-overlap-test
              (let ([result (interval-union (make-interval 1 5)
                                            (make-interval 3 8))])
                   (assert-equal 1 (interval-lo result))
                   (assert-equal 8 (interval-hi result))))
            
            ;; Union: [1, 3] ∪ [5, 7] = [1, 7] (fills gap)
            (define-test interval-union-gap-test
              (let ([result (interval-union (make-interval 1 3)
                                            (make-interval 5 7))])
                   (assert-equal 1 (interval-lo result))
                   (assert-equal 7 (interval-hi result))))
            
            ;; Hull of list
            (define-test interval-hull-test
              (let ([result (interval-hull (list (make-interval 2 4)
                                                 (make-interval 1 3)
                                                 (make-interval 5 6)))])
                   (assert-true (just? result))
                   (assert-equal 1 (interval-lo (from-just result)))
                   (assert-equal 6 (interval-hi (from-just result)))))
            
            (define-test interval-hull-empty-test
              (assert-true (nothing? (interval-hull '()))))
            
            ;; Intersection: [1, 5] ∩ [3, 8] = [3, 5]
            (define-test interval-intersection-test
              (let ([result (interval-intersection (make-interval 1 5)
                                                   (make-interval 3 8))])
                   (assert-true (just? result))
                   (assert-equal 3 (interval-lo (from-just result)))
                   (assert-equal 5 (interval-hi (from-just result)))))
            
            ;; Intersection: disjoint returns nothing
            (define-test interval-intersection-disjoint-test
              (let ([result (interval-intersection (make-interval 1 3)
                                                   (make-interval 5 7))])
                   (assert-true (nothing? result)))))

;;; ============================================================
;;; Power Operations Tests
;;; ============================================================

(test-group power-operations
            ;; [2, 3]^2 = [4, 9]
            (define-test interval-square-positive-test
              (let ([result (interval-square (make-interval 2 3))])
                   (assert-equal 4 (interval-lo result))
                   (assert-equal 9 (interval-hi result))))
            
            ;; [-3, -2]^2 = [4, 9]
            (define-test interval-square-negative-test
              (let ([result (interval-square (make-interval -3 -2))])
                   (assert-equal 4 (interval-lo result))
                   (assert-equal 9 (interval-hi result))))
            
            ;; [-2, 3]^2 = [0, 9] (zero crossing)
            (define-test interval-square-mixed-test
              (let ([result (interval-square (make-interval -2 3))])
                   (assert-equal 0 (interval-lo result))
                   (assert-equal 9 (interval-hi result))))
            
            ;; x^0 = 1
            (define-test interval-pow-zero-test
              (let ([result (interval-pow-nat (make-interval 2 5) 0)])
                   (assert-equal 1 (interval-lo result))
                   (assert-equal 1 (interval-hi result))))
            
            ;; x^1 = x
            (define-test interval-pow-one-test
              (let ([result (interval-pow-nat (make-interval 2 5) 1)])
                   (assert-equal 2 (interval-lo result))
                   (assert-equal 5 (interval-hi result))))
            
            ;; [2, 3]^3 = [8, 27]
            (define-test interval-pow-cubic-test
              (let ([result (interval-pow-nat (make-interval 2 3) 3)])
                   (assert-equal 8 (interval-lo result))
                   (assert-equal 27 (interval-hi result))))
            
            ;; |[-3, 5]| = [0, 5]
            (define-test interval-abs-mixed-test
              (let ([result (interval-abs (make-interval -3 5))])
                   (assert-equal 0 (interval-lo result))
                   (assert-equal 5 (interval-hi result))))
            
            ;; |[-5, -2]| = [2, 5]
            (define-test interval-abs-negative-test
              (let ([result (interval-abs (make-interval -5 -2))])
                   (assert-equal 2 (interval-lo result))
                   (assert-equal 5 (interval-hi result)))))

;;; ============================================================
;;; Comparison Tests
;;; ============================================================

(test-group comparisons
            ;; [1, 2] < [5, 7]
            (define-test interval-lt-test
              (assert-true (interval-lt? (make-interval 1 2)
                                         (make-interval 5 7)))
              (assert-false (interval-lt? (make-interval 1 5)
                                          (make-interval 3 7))))
            
            ;; [5, 7] > [1, 2]
            (define-test interval-gt-test
              (assert-true (interval-gt? (make-interval 5 7)
                                         (make-interval 1 2)))
              (assert-false (interval-gt? (make-interval 1 5)
                                          (make-interval 3 7))))
            
            ;; interval-min and interval-max
            (define-test interval-min-test
              (let ([result (interval-min (make-interval 1 5)
                                          (make-interval 3 7))])
                   (assert-equal 1 (interval-lo result))
                   (assert-equal 5 (interval-hi result))))
            
            (define-test interval-max-test
              (let ([result (interval-max (make-interval 1 5)
                                          (make-interval 3 7))])
                   (assert-equal 3 (interval-lo result))
                   (assert-equal 7 (interval-hi result))))
            
            (define-test interval-equal-test
              (assert-true (interval-equal? (make-interval 2 5)
                                            (make-interval 2 5)))
              (assert-false (interval-equal? (make-interval 2 5)
                                             (make-interval 2 6)))))

;;; ============================================================
;;; Splitting Tests
;;; ============================================================

(test-group splitting
            (define-test interval-split-test
              (let* ([iv (make-interval 0 10)]
                     [result (interval-split iv)]
                     [left (car result)]
                     [right (cdr result)])
                    (assert-equal 0 (interval-lo left))
                    (assert-equal 5 (interval-hi left))
                    (assert-equal 5 (interval-lo right))
                    (assert-equal 10 (interval-hi right))))
            
            (define-test interval-bisect-test
              (let ([result (interval-bisect (make-interval 0 10) 3)])
                   (assert-true (just? result))
                   (let ([parts (from-just result)])
                        (assert-equal 0 (interval-lo (car parts)))
                        (assert-equal 3 (interval-hi (car parts)))
                        (assert-equal 3 (interval-lo (cdr parts)))
                        (assert-equal 10 (interval-hi (cdr parts))))))
            
            (define-test interval-bisect-outside-test
              (let ([result (interval-bisect (make-interval 0 10) 15)])
                   (assert-true (nothing? result)))))

;;; ============================================================
;;; Conversion Tests
;;; ============================================================

(test-group conversions
            (define-test interval-to-list-test
              (assert-equal '(2 5) (interval->list (make-interval 2 5))))
            
            (define-test interval-to-string-test
              (assert-equal "[2, 5]" (interval->string (make-interval 2 5)))))

;;; ============================================================
;;; Num Type Class Tests
;;; ============================================================

(test-group num-typeclass
            (define-test interval-num-add-test
              (let ([result ((num-plus interval-num)
                             (make-interval 1 2)
                             (make-interval 3 4))])
                   (assert-equal 4 (interval-lo result))
                   (assert-equal 6 (interval-hi result))))
            
            (define-test interval-num-mul-test
              (let ([result ((num-times interval-num)
                             (make-interval 2 3)
                             (make-interval 4 5))])
                   (assert-equal 8 (interval-lo result))
                   (assert-equal 15 (interval-hi result))))
            
            (define-test interval-num-from-integer-test
              (let ([result ((num-from-integer interval-num) 42)])
                   (assert-true (interval? result))
                   (assert-true (interval-singleton? result))
                   (assert-equal 42 (interval-lo result)))))

;;; ============================================================
;;; Constants Tests
;;; ============================================================

(test-group constants
            (define-test interval-zero-test
              (assert-true (interval-contains? interval-zero 0))
              (assert-true (interval-singleton? interval-zero)))
            
            (define-test interval-one-test
              (assert-true (interval-contains? interval-one 1))
              (assert-true (interval-singleton? interval-one)))
            
            (define-test interval-unit-test
              (assert-true (interval-contains? interval-unit 0))
              (assert-true (interval-contains? interval-unit 0.5))
              (assert-true (interval-contains? interval-unit 1))))

;;; ============================================================
;;; Run Tests
;;; ============================================================

(display "
")
(display "==============================================================
")
(display (format "Tests passed: ~a
" *tests-passed*))
(display (format "Tests failed: ~a
" *tests-failed*))
(display (format "Total tests:  ~a
" *tests-run*))

(if (= *tests-failed* 0)
    (display "
[SUCCESS] All interval arithmetic tests passed.
")
    (display "
[FAILURE] Some tests failed.
"))
