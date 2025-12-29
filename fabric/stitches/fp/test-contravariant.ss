;;; fabric/stitches/fp/test-contravariant.ss — Tests for Contravariant Functor

(load "fabric/stitches/test-framework.ss")
(load "fabric/stitches/fp/contravariant.ss")

(display "
")
(display "==============================================================
")
(display "         CONTRAVARIANT FUNCTOR TESTS
")
(display "==============================================================
")

;;; ============================================================
;;; Predicate Tests
;;; ============================================================

(test-group predicate-tests
            (define-test make-predicate-test
              (let ([p (make-predicate (lambda (x) (> x 0)))])
                   (assert-true (predicate? p))
                   (assert-true (run-predicate p 5))
                   (assert-false (run-predicate p -3))))
            
            (define-test predicate-contramap-test
              (let* ([is-positive (make-predicate (lambda (x) (> x 0)))]
                     [has-positive-length (predicate-contramap string-length is-positive)])
                    (assert-true (run-predicate has-positive-length "hello"))
                    (assert-false (run-predicate has-positive-length ""))))
            
            (define-test predicate-and-test
              (let* ([gt-5 (make-predicate (lambda (x) (> x 5)))]
                     [lt-10 (make-predicate (lambda (x) (< x 10)))]
                     [between-5-10 (predicate-and gt-5 lt-10)])
                    (assert-true (run-predicate between-5-10 7))
                    (assert-false (run-predicate between-5-10 3))
                    (assert-false (run-predicate between-5-10 15))))
            
            (define-test predicate-or-test
              (let* ([lt-5 (make-predicate (lambda (x) (< x 5)))]
                     [gt-10 (make-predicate (lambda (x) (> x 10)))]
                     [outside-5-10 (predicate-or lt-5 gt-10)])
                    (assert-true (run-predicate outside-5-10 3))
                    (assert-true (run-predicate outside-5-10 15))
                    (assert-false (run-predicate outside-5-10 7))))
            
            (define-test predicate-not-test
              (let* ([is-positive (make-predicate (lambda (x) (> x 0)))]
                     [is-non-positive (predicate-not is-positive)])
                    (assert-true (run-predicate is-non-positive -5))
                    (assert-true (run-predicate is-non-positive 0))
                    (assert-false (run-predicate is-non-positive 5))))
            
            (define-test predicate-all-none-test
              (assert-true (run-predicate predicate-all 'anything))
              (assert-false (run-predicate predicate-none 'anything))))

;;; ============================================================
;;; Comparison Tests
;;; ============================================================

(test-group comparison-tests
            (define-test default-comparison-test
              (assert-equal 'lt (run-comparison default-comparison 1 5))
              (assert-equal 'gt (run-comparison default-comparison 5 1))
              (assert-equal 'eq (run-comparison default-comparison 3 3)))
            
            (define-test string-comparison-test
              (assert-equal 'lt (run-comparison string-comparison "apple" "banana"))
              (assert-equal 'gt (run-comparison string-comparison "zebra" "apple"))
              (assert-equal 'eq (run-comparison string-comparison "same" "same")))
            
            (define-test comparison-contramap-test
              (let ([by-length (comparison-contramap string-length default-comparison)])
                   (assert-equal 'lt (run-comparison by-length "hi" "hello"))
                   (assert-equal 'gt (run-comparison by-length "hello" "hi"))
                   (assert-equal 'eq (run-comparison by-length "hi" "no"))))
            
            (define-test reverse-comparison-test
              (let ([desc (reverse-comparison default-comparison)])
                   (assert-equal 'gt (run-comparison desc 1 5))
                   (assert-equal 'lt (run-comparison desc 5 1))
                   (assert-equal 'eq (run-comparison desc 3 3))))
            
            (define-test comparison-chain-test
              (let* ([by-length (comparing string-length default-comparison)]
                     [by-alpha string-comparison]
                     [combined (comparison-chain by-length by-alpha)])
                    ;; Same length, fall back to alphabetic
                    (assert-equal 'lt (run-comparison combined "abc" "abd"))
                    ;; Different lengths, use length
                    (assert-equal 'lt (run-comparison combined "ab" "abc")))))

;;; ============================================================
;;; Equivalence Tests
;;; ============================================================

(test-group equivalence-tests
            (define-test default-equivalence-test
              (assert-true (run-equivalence default-equivalence 1 1))
              (assert-false (run-equivalence default-equivalence 1 2))
              (assert-true (run-equivalence default-equivalence "a" "a")))
            
            (define-test equivalence-contramap-test
              (let ([by-length (equivalence-contramap string-length default-equivalence)])
                   (assert-true (run-equivalence by-length "hi" "no"))
                   (assert-false (run-equivalence by-length "hi" "hello"))))
            
            (define-test equating-test
              (let ([by-first-char (equating (lambda (s) (string-ref s 0)))])
                   (assert-true (run-equivalence by-first-char "apple" "ant"))
                   (assert-false (run-equivalence by-first-char "apple" "banana"))))
            
            (define-test equivalence-and-test
              (let* ([same-length (equating string-length)]
                     [same-first (equating (lambda (s) (string-ref s 0)))]
                     [both (equivalence-and same-length same-first)])
                    (assert-true (run-equivalence both "ab" "ac"))
                    (assert-false (run-equivalence both "ab" "abc"))
                    (assert-false (run-equivalence both "ab" "bb"))))
            
            (define-test equivalence-from-comparison-test
              (let ([eq-from-cmp (equivalence-from-comparison default-comparison)])
                   (assert-true (run-equivalence eq-from-cmp 5 5))
                   (assert-false (run-equivalence eq-from-cmp 5 10)))))

;;; ============================================================
;;; Op Tests
;;; ============================================================

(test-group op-tests
            (define-test make-op-test
              (let ([o (make-op add1)])
                   (assert-true (op? o))
                   (assert-equal 6 (run-op o 5))))
            
            (define-test op-contramap-test
              (let* ([o (make-op (lambda (n) (* n 2)))]
                     [o2 (cmap op-contravariant string-length o)])
                    (assert-equal 10 (run-op o2 "hello")))))

;;; ============================================================
;;; Practical Utilities Tests
;;; ============================================================

(test-group utility-tests
            (define-test sort-by-test
              (assert-equal '(1 1 2 3 4 5)
                            (sort-by default-comparison '(3 1 4 1 5 2))))
            
            (define-test sort-by-descending-test
              (assert-equal '(5 4 3 2 1)
                            (sort-by (reverse-comparison default-comparison) '(3 1 4 2 5))))
            
            (define-test sort-by-field-test
              (let* ([people '(("Alice" . 30) ("Bob" . 25) ("Carol" . 35))]
                     [by-age (comparing cdr default-comparison)])
                    (assert-equal '(("Bob" . 25) ("Alice" . 30) ("Carol" . 35))
                                  (sort-by by-age people))))
            
            (define-test filter-by-test
              (let ([is-even (make-predicate (lambda (x) (= 0 (modulo x 2))))])
                   (assert-equal '(2 4 6) (filter-by is-even '(1 2 3 4 5 6))))))

;;; ============================================================
;;; Divisible Tests
;;; ============================================================

(test-group divisible-tests
            (define-test predicate-divisible-conquer-test
              (assert-true (run-predicate (divisible-conquer predicate-divisible) 'anything)))
            
            (define-test predicate-divisible-divide-test
              (let* ([is-positive (make-predicate (lambda (x) (> x 0)))]
                     [is-even (make-predicate (lambda (x) (= 0 (modulo x 2))))]
                     [split-pair (lambda (p) p)]  ; identity since already a pair
                     [both-pred ((divisible-divide predicate-divisible) split-pair is-positive is-even)])
                    (assert-true (run-predicate both-pred (cons 5 4)))
                    (assert-false (run-predicate both-pred (cons -1 4)))
                    (assert-false (run-predicate both-pred (cons 5 3))))))

;;; ============================================================
;;; Law Verification Tests
;;; ============================================================

(test-group law-tests
            (define-test predicate-identity-law-test
              (let* ([p (make-predicate (lambda (x) (> x 0)))]
                     [equiv (lambda (p1 p2)
                                    (and (eq? (run-predicate p1 5) (run-predicate p2 5))
                                         (eq? (run-predicate p1 -5) (run-predicate p2 -5))))])
                    (assert-true (verify-contravariant-identity predicate-contravariant equiv p))))
            
            (define-test comparison-identity-law-test
              (let* ([c default-comparison]
                     [equiv (lambda (c1 c2)
                                    (and (eq? (run-comparison c1 1 2) (run-comparison c2 1 2))
                                         (eq? (run-comparison c1 2 1) (run-comparison c2 2 1))))])
                    (assert-true (verify-contravariant-identity comparison-contravariant equiv c)))))

;;; ============================================================
;;; Summary
;;; ============================================================

(newline)
(exit-with-summary)
