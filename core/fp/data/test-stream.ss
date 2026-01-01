;;; fabric/stitches/fp/test-stream.ss — Tests for Lazy Streams

;;; NOTE: Run from fabric/stitches directory

(load "core/test-framework.ss")
(load "core/fp/data/stream.ss")

(display "
")
(display "==============================================================
")
(display "         LAZY STREAM TESTS
")
(display "==============================================================
")

;;; ============================================================
;;; Stream Construction Tests
;;; ============================================================

(test-group stream-construction
            (define-test stream-nil-test
              (assert-true (stream-nil? stream-nil)))
            
            (define-test stream-cons-test
              (let ([s (stream-cons 1 (lambda () stream-nil))])
                   (assert-true (stream-cons? s))
                   (assert-false (stream-nil? s))))
            
            (define-test stream-head-test
              (let ([s (stream-cons 42 (lambda () stream-nil))])
                   (assert-equal 42 (stream-head s))))
            
            (define-test stream-tail-test
              (let ([s (stream-cons 1 (lambda () (stream-cons 2 (lambda () stream-nil))))])
                   (assert-equal 2 (stream-head (stream-tail s)))))
            
            (define-test list-to-stream-empty-test
              (assert-true (stream-nil? (list->stream '()))))
            
            (define-test list-to-stream-test
              (let ([s (list->stream '(1 2 3))])
                   (assert-equal 1 (stream-head s))
                   (assert-equal 2 (stream-head (stream-tail s)))
                   (assert-equal 3 (stream-head (stream-tail (stream-tail s))))))
            
            (define-test stream-to-list-test
              (let ([s (list->stream '(1 2 3 4 5))])
                   (assert-equal '(1 2 3) (stream->list 3 s))
                   (assert-equal '(1 2 3 4 5) (stream->list 10 s)))))

;;; ============================================================
;;; Stream Generator Tests
;;; ============================================================

(test-group stream-generators
            (define-test stream-iterate-test
              (let ([s (stream-iterate (lambda (x) (+ x 1)) 0)])
                   (assert-equal '(0 1 2 3 4) (stream->list 5 s))))
            
            (define-test stream-repeat-test
              (let ([s (stream-repeat 'x)])
                   (assert-equal '(x x x x x) (stream->list 5 s))))
            
            (define-test stream-cycle-test
              (let ([s (stream-cycle '(a b c))])
                   (assert-equal '(a b c a b c a) (stream->list 7 s))))
            
            (define-test stream-cycle-empty-test
              (assert-true (stream-nil? (stream-cycle '()))))
            
            (define-test stream-from-test
              (let ([s (stream-from 10)])
                   (assert-equal '(10 11 12 13 14) (stream->list 5 s))))
            
            (define-test stream-range-test
              (assert-equal '(0 1 2 3 4) (stream->list 10 (stream-range 0 5))))
            
            (define-test stream-range-empty-test
              (assert-true (stream-nil? (stream-range 5 5)))
              (assert-true (stream-nil? (stream-range 10 5))))
            
            (define-test naturals-test
              (assert-equal '(0 1 2 3 4 5) (stream->list 6 naturals)))
            
            (define-test stream-unfold-test
              (let ([s (stream-unfold
                        (lambda (seed)
                                (if (> seed 5)
                                    nothing
                                    (just (cons (* seed 2) (+ seed 1)))))
                        1)])
                   (assert-equal '(2 4 6 8 10) (stream->list 10 s)))))

;;; ============================================================
;;; Stream Transformer Tests
;;; ============================================================

(test-group stream-transformers
            (define-test stream-map-test
              (let ([s (stream-map (lambda (x) (* x 2)) (stream-from 1))])
                   (assert-equal '(2 4 6 8 10) (stream->list 5 s))))
            
            (define-test stream-map-empty-test
              (assert-true (stream-nil? (stream-map add1 stream-nil))))
            
            (define-test stream-filter-test
              (let ([s (stream-filter even? (stream-from 0))])
                   (assert-equal '(0 2 4 6 8) (stream->list 5 s))))
            
            (define-test stream-filter-all-test
              (let ([s (stream-filter (lambda (x) #t) (list->stream '(1 2 3)))])
                   (assert-equal '(1 2 3) (stream->list 10 s))))
            
            (define-test stream-take-test
              (let ([s (stream-take 3 (stream-from 0))])
                   (assert-equal '(0 1 2) (stream->list 10 s))))
            
            (define-test stream-take-zero-test
              (assert-true (stream-nil? (stream-take 0 (stream-from 0)))))
            
            (define-test stream-drop-test
              (let ([s (stream-drop 3 (stream-from 0))])
                   (assert-equal '(3 4 5) (stream->list 3 s))))
            
            (define-test stream-drop-all-test
              (assert-true (stream-nil? (stream-drop 100 (list->stream '(1 2 3))))))
            
            (define-test stream-take-while-test
              (let ([s (stream-take-while (lambda (x) (< x 5)) (stream-from 0))])
                   (assert-equal '(0 1 2 3 4) (stream->list 10 s))))
            
            (define-test stream-drop-while-test
              (let ([s (stream-drop-while (lambda (x) (< x 5)) (stream-from 0))])
                   (assert-equal '(5 6 7) (stream->list 3 s))))
            
            (define-test stream-nth-test
              (let ([s (stream-from 10)])
                   (assert-equal 13 (stream-nth 3 s))))
            
            (define-test stream-scan-test
              (let ([s (stream-scan + 0 (stream-from 1))])
                   (assert-equal '(0 1 3 6 10) (stream->list 5 s)))))

;;; ============================================================
;;; Stream Combinator Tests
;;; ============================================================

(test-group stream-combinators
            (define-test stream-zip-test
              (let ([s (stream-zip (stream-from 0) (stream-repeat 'x))])
                   (assert-equal '((0 . x) (1 . x) (2 . x)) (stream->list 3 s))))
            
            (define-test stream-zip-different-lengths-test
              (let ([s (stream-zip (list->stream '(a b)) (stream-from 1))])
                   (assert-equal '((a . 1) (b . 2)) (stream->list 10 s))))
            
            (define-test stream-zip-with-test
              (let ([s (stream-zip-with + (stream-from 1) (stream-from 10))])
                   (assert-equal '(11 13 15 17) (stream->list 4 s))))
            
            (define-test stream-interleave-test
              (let ([s (stream-interleave (list->stream '(a b c)) (list->stream '(1 2 3)))])
                   (assert-equal '(a 1 b 2 c 3) (stream->list 10 s))))
            
            (define-test stream-merge-test
              (let* ([odds (stream-filter odd? (stream-from 1))]
                     [evens (stream-filter even? (stream-from 2))]
                     [merged (stream-merge < odds evens)])
                    (assert-equal '(1 2 3 4 5 6 7 8) (stream->list 8 merged))))
            
            (define-test stream-concat-test
              (let ([s (stream-concat (list->stream '(1 2)) (list->stream '(3 4)))])
                   (assert-equal '(1 2 3 4) (stream->list 10 s))))
            
            (define-test stream-concat-first-empty-test
              (let ([s (stream-concat stream-nil (list->stream '(1 2)))])
                   (assert-equal '(1 2) (stream->list 10 s)))))

;;; ============================================================
;;; Stream Fold Tests
;;; ============================================================

(test-group stream-folds
            (define-test stream-fold-test
              (let ([result (stream-fold + 0 5 (stream-from 1))])
                   (assert-equal 15 result)))  ; 1+2+3+4+5 = 15
            
            (define-test stream-any-test
              (assert-true (stream-any (lambda (x) (> x 50)) 100 (stream-from 0)))
              (assert-false (stream-any (lambda (x) (> x 50)) 10 (stream-from 0))))
            
            (define-test stream-all-test
              (assert-true (stream-all (lambda (x) (< x 100)) 50 (stream-from 0)))
              (assert-false (stream-all even? 5 (stream-from 0))))
            
            (define-test stream-find-test
              (let ([result (stream-find (lambda (x) (> x 5)) 100 (stream-from 0))])
                   (assert-true (just? result))
                   (assert-equal 6 (from-just result))))
            
            (define-test stream-find-not-found-test
              (assert-true (nothing? (stream-find (lambda (x) (> x 100)) 50 (stream-from 0))))))

;;; ============================================================
;;; Classic Stream Tests
;;; ============================================================

(test-group classic-streams
            (define-test fibonacci-test
              (assert-equal '(0 1 1 2 3 5 8 13 21 34) (stream->list 10 fibonacci)))
            
            (define-test primes-test
              (assert-equal '(2 3 5 7 11 13 17 19 23 29) (stream->list 10 primes)))
            
            (define-test powers-of-test
              (assert-equal '(1 2 4 8 16 32) (stream->list 6 (powers-of 2))))
            
            (define-test factorials-test
              (assert-equal '(1 1 2 6 24 120) (stream->list 6 factorials)))
            
            (define-test triangular-test
              (assert-equal '(0 1 3 6 10 15) (stream->list 6 triangular))))

;;; ============================================================
;;; Generator Tests
;;; ============================================================

(test-group generators
            (define-test counter-generator-test
              (let* ([gen (counter-generator 5 10)]
                     [s (make-generator gen)])
                    (assert-equal '(5 6 7 8 9) (stream->list 10 s))))
            
            (define-test random-stream-test
              (let ([s (random-stream 1 5 13)])
                   ;; Just verify it produces numbers
                   (assert-equal 5 (length (stream->list 5 s)))))
            
            (define-test make-generator-exhausted-test
              (let* ([called 0]
                     [gen (lambda ()
                                  (set! called (+ called 1))
                                  (if (> called 3) nothing (just called)))]
                     [s (make-generator gen)])
                    (assert-equal '(1 2 3) (stream->list 10 s)))))

;;; ============================================================
;;; Utility Function Tests
;;; ============================================================

(test-group stream-utilities
            (define-test stream-partition-test
              (let* ([parts (stream-partition even? (stream-from 0))]
                     [evens (car parts)]
                     [odds (cdr parts)])
                    (assert-equal '(0 2 4) (stream->list 3 evens))
                    (assert-equal '(1 3 5) (stream->list 3 odds))))
            
            (define-test stream-group-test
              (let ([s (stream-group 3 (stream-from 0))])
                   (assert-equal '((0 1 2) (3 4 5) (6 7 8)) (stream->list 3 s))))
            
            (define-test stream-distinct-test
              (let ([s (stream-distinct (list->stream '(1 1 2 2 2 3 3 1 1)))])
                   (assert-equal '(1 2 3 1) (stream->list 10 s))))
            
            (define-test stream-enumerate-test
              (let ([s (stream-enumerate (list->stream '(a b c)))])
                   (assert-equal '((0 . a) (1 . b) (2 . c)) (stream->list 10 s)))))

;;; ============================================================
;;; Memoization Tests
;;; ============================================================

(test-group memoization
            (define-test memo-stream-test
              (let* ([count 0]
                     [s (memo-stream-cons 1
                                          (lambda ()
                                                  (set! count (+ count 1))
                                                  (memo-stream-cons 2 (lambda () stream-nil))))])
                    ;; Access tail multiple times
                    (stream-head (stream-tail s))
                    (stream-head (stream-tail s))
                    (stream-head (stream-tail s))
                    ;; Should only have computed once
                    (assert-equal 1 count)))
            
            (define-test stream-force-test
              (let ([s (stream-take 5 (stream-from 0))])
                   (let ([forced (stream-force 10 s)])
                        (assert-equal '(0 1 2 3 4) (stream->list 10 forced))))))

;;; ============================================================
;;; Edge Case Tests
;;; ============================================================

(test-group edge-cases
            (define-test empty-operations-test
              (assert-true (stream-nil? (stream-map add1 stream-nil)))
              (assert-true (stream-nil? (stream-filter even? stream-nil)))
              (assert-true (stream-nil? (stream-take 5 stream-nil)))
              (assert-true (stream-nil? (stream-drop 5 stream-nil))))
            
            (define-test large-stream-test
              ;; Just verify we can handle large lazy operations
              (let ([s (stream-take 1000 (stream-from 0))])
                   (assert-equal 999 (stream-nth 999 s))))
            
            (define-test nested-transforms-test
              (let* ([s (stream-from 0)]
                     [s1 (stream-filter even? s)]
                     [s2 (stream-map (lambda (x) (* x 2)) s1)]
                     [s3 (stream-take 5 s2)])
                    (assert-equal '(0 4 8 12 16) (stream->list 10 s3)))))

;;; ============================================================
;;; Practical Application Tests
;;; ============================================================

(test-group practical-applications
            (define-test running-sum-test
              (let ([sums (stream-scan + 0 (list->stream '(1 2 3 4 5)))])
                   (assert-equal '(0 1 3 6 10 15) (stream->list 10 sums))))
            
            (define-test sieve-correctness-test
              ;; Verify first 20 primes
              (let ([first-20 (stream->list 20 primes)])
                   (assert-equal 2 (list-ref first-20 0))
                   (assert-equal 71 (list-ref first-20 19))))
            
            (define-test fibonacci-property-test
              ;; F(n) = F(n-1) + F(n-2)
              (let ([fibs (stream->list 10 fibonacci)])
                   (assert-equal (list-ref fibs 9)
                                 (+ (list-ref fibs 8) (list-ref fibs 7))))))

;;; ============================================================
;;; Summary
;;; ============================================================

(display "
")
(display "==============================================================
")
(printf "Tests passed: ~a
" *tests-passed*)
(printf "Tests failed: ~a
" *tests-failed*)
(printf "Total tests:  ~a
" *tests-run*)

(if (= *tests-failed* 0)
    (display "
[SUCCESS] All stream tests passed.
")
    (display "
[FAILURE] Some stream tests failed.
"))
