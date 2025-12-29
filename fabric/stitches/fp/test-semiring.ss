;;; fabric/stitches/fp/test-semiring.ss — Tests for Semiring Library
;;;
;;; NOTE: Run from fabric/stitches directory

(load "fabric/stitches/test-framework.ss")
(load "fabric/stitches/fp/semiring.ss")

(display "
")
(display "==============================================================
")
(display "         SEMIRING TESTS
")
(display "==============================================================
")

;;; ============================================================
;;; Basic Semiring Tests
;;; ============================================================

(test-group semiring-basics
            (define-test semiring-construction-test
              (assert-true (semiring? boolean-semiring))
              (assert-true (semiring? natural-semiring))
              (assert-true (semiring? tropical-semiring)))
            
            (define-test semiring-name-test
              (assert-equal 'boolean (semiring-name boolean-semiring))
              (assert-equal 'natural (semiring-name natural-semiring))
              (assert-equal 'tropical (semiring-name tropical-semiring))))

;;; ============================================================
;;; Boolean Semiring Tests
;;; ============================================================

(test-group boolean-semiring-tests
            (define-test boolean-zero-one-test
              (assert-false (sr-zero boolean-semiring))
              (assert-true (sr-one boolean-semiring)))
            
            (define-test boolean-add-test
              (assert-false (sr-add boolean-semiring #f #f))
              (assert-true (sr-add boolean-semiring #f #t))
              (assert-true (sr-add boolean-semiring #t #f))
              (assert-true (sr-add boolean-semiring #t #t)))
            
            (define-test boolean-mul-test
              (assert-false (sr-mul boolean-semiring #f #f))
              (assert-false (sr-mul boolean-semiring #f #t))
              (assert-false (sr-mul boolean-semiring #t #f))
              (assert-true (sr-mul boolean-semiring #t #t)))
            
            (define-test boolean-sum-test
              (assert-true (sr-sum boolean-semiring '(#f #f #t #f))))
            
            (define-test boolean-product-test
              (assert-false (sr-product boolean-semiring '(#t #t #f #t)))))

;;; ============================================================
;;; Natural Semiring Tests
;;; ============================================================

(test-group natural-semiring-tests
            (define-test natural-zero-one-test
              (assert-equal 0 (sr-zero natural-semiring))
              (assert-equal 1 (sr-one natural-semiring)))
            
            (define-test natural-add-test
              (assert-equal 5 (sr-add natural-semiring 2 3)))
            
            (define-test natural-mul-test
              (assert-equal 6 (sr-mul natural-semiring 2 3)))
            
            (define-test natural-sum-test
              (assert-equal 15 (sr-sum natural-semiring '(1 2 3 4 5))))
            
            (define-test natural-product-test
              (assert-equal 120 (sr-product natural-semiring '(1 2 3 4 5)))))

;;; ============================================================
;;; Tropical Semiring Tests
;;; ============================================================

(test-group tropical-semiring-tests
            (define-test tropical-zero-one-test
              (assert-equal 'inf (sr-zero tropical-semiring))
              (assert-equal 0 (sr-one tropical-semiring)))
            
            (define-test tropical-add-test
              ;; Plus is min
              (assert-equal 2 (sr-add tropical-semiring 2 5))
              (assert-equal 3 (sr-add tropical-semiring 'inf 3))
              (assert-equal 3 (sr-add tropical-semiring 3 'inf)))
            
            (define-test tropical-mul-test
              ;; Times is +
              (assert-equal 7 (sr-mul tropical-semiring 2 5))
              (assert-equal 'inf (sr-mul tropical-semiring 'inf 3))
              (assert-equal 'inf (sr-mul tropical-semiring 3 'inf)))
            
            (define-test tropical-sum-test
              ;; Sum is min of all
              (assert-equal 1 (sr-sum tropical-semiring '(5 3 1 4 2)))))

;;; ============================================================
;;; Arctic Semiring Tests
;;; ============================================================

(test-group arctic-semiring-tests
            (define-test arctic-zero-one-test
              (assert-equal '-inf (sr-zero arctic-semiring))
              (assert-equal 0 (sr-one arctic-semiring)))
            
            (define-test arctic-add-test
              ;; Plus is max
              (assert-equal 5 (sr-add arctic-semiring 2 5))
              (assert-equal 3 (sr-add arctic-semiring '-inf 3)))
            
            (define-test arctic-mul-test
              ;; Times is +
              (assert-equal 7 (sr-mul arctic-semiring 2 5))
              (assert-equal '-inf (sr-mul arctic-semiring '-inf 3))))

;;; ============================================================
;;; Probability Semiring Tests
;;; ============================================================

(test-group probability-semiring-tests
            (define-test prob-zero-one-test
              (assert-equal 0 (sr-zero probability-semiring))
              (assert-equal 1 (sr-one probability-semiring)))
            
            (define-test prob-add-test
              ;; Plus is max
              (assert-equal 0.8 (sr-add probability-semiring 0.5 0.8)))
            
            (define-test prob-mul-test
              ;; Times is *
              (assert-equal 0.25 (sr-mul probability-semiring 0.5 0.5))))

;;; ============================================================
;;; Matrix Operations Tests
;;; ============================================================

(test-group matrix-operations
            (define-test identity-matrix-test
              (let ([id (sr-identity-matrix natural-semiring 3)])
                   (assert-equal '((1 0 0) (0 1 0) (0 0 1)) id)))
            
            (define-test zero-matrix-test
              (let ([z (sr-zero-matrix natural-semiring 2)])
                   (assert-equal '((0 0) (0 0)) z)))
            
            (define-test matrix-add-test
              (let* ([m1 '((1 2) (3 4))]
                     [m2 '((5 6) (7 8))]
                     [result (sr-matrix-add natural-semiring m1 m2)])
                    (assert-equal '((6 8) (10 12)) result)))
            
            (define-test matrix-mul-test
              (let* ([m1 '((1 2) (3 4))]
                     [m2 '((5 6) (7 8))]
                     [result (sr-matrix-mul natural-semiring m1 m2)])
                    (assert-equal '((19 22) (43 50)) result)))
            
            (define-test matrix-power-test
              (let* ([m '((1 1) (1 0))]  ; Fibonacci matrix
                     [m3 (sr-matrix-power natural-semiring m 3)])
                    (assert-equal '((3 2) (2 1)) m3))))

;;; ============================================================
;;; Shortest Paths Tests
;;; ============================================================

(test-group shortest-paths
            (define-test shortest-path-basic-test
              ;; Simple graph: 0 --2--> 1 --3--> 2
              ;;               0 ------10-----> 2
              (let* ([distances '((0   2   10)
                                  (inf 0   3)
                                  (inf inf 0))]
                     [result (shortest-paths distances)])
                    ;; Check that shortest path 0->2 is 5 (via 1), not 10 (direct)
                    (assert-equal 5 (matrix-ref result 0 2))))
            
            (define-test tropical-matrix-test
              (let* ([m '((0 1) (2 0))]
                     [result (sr-matrix-mul tropical-semiring m m)])
                    ;; (0,0): min(0+0, 1+2) = 0
                    ;; (0,1): min(0+1, 1+0) = 1
                    (assert-equal 0 (matrix-ref result 0 0))
                    (assert-equal 1 (matrix-ref result 0 1)))))

;;; ============================================================
;;; Path Counting Tests
;;; ============================================================

(test-group path-counting
            (define-test count-paths-test
              ;; Simple graph: 0 -> 1 -> 2, 0 -> 2
              (let* ([adj '((0 1 1)
                            (0 0 1)
                            (0 0 0))]
                     [paths-2 (count-paths adj 2)])
                    ;; Paths of length 2 from 0 to 2: 0->1->2
                    (assert-equal 1 (matrix-ref paths-2 0 2)))))

;;; ============================================================
;;; Polynomial Tests
;;; ============================================================

(test-group polynomials
            (define-test poly-add-test
              ;; (1 + 2x) + (3 + 4x) = 4 + 6x
              (let ([result (sr-poly-add natural-semiring '(1 2) '(3 4))])
                   (assert-equal '(4 6) result)))
            
            (define-test poly-mul-test
              ;; (1 + x) * (1 + x) = 1 + 2x + x^2
              (let ([result (sr-poly-mul natural-semiring '(1 1) '(1 1))])
                   (assert-equal '(1 2 1) result)))
            
            (define-test poly-eval-test
              ;; Evaluate 1 + 2x + 3x^2 at x=2
              ;; = 1 + 4 + 12 = 17
              (let ([result (sr-poly-eval natural-semiring '(1 2 3) 2)])
                   (assert-equal 17 result)))
            
            (define-test poly-scale-test
              (let ([result (sr-poly-scale natural-semiring 3 '(1 2 3))])
                   (assert-equal '(3 6 9) result))))

;;; ============================================================
;;; Regex Semiring Tests
;;; ============================================================

(test-group regex-semiring-tests
            (define-test regex-zero-one-test
              (assert-equal 'empty (sr-zero regex-semiring))
              (assert-equal 'epsilon (sr-one regex-semiring)))
            
            (define-test regex-add-empty-test
              (assert-equal 'a (sr-add regex-semiring 'empty 'a))
              (assert-equal 'a (sr-add regex-semiring 'a 'empty)))
            
            (define-test regex-mul-empty-test
              (assert-equal 'empty (sr-mul regex-semiring 'empty 'a))
              (assert-equal 'empty (sr-mul regex-semiring 'a 'empty)))
            
            (define-test regex-mul-epsilon-test
              (assert-equal 'a (sr-mul regex-semiring 'epsilon 'a))
              (assert-equal 'a (sr-mul regex-semiring 'a 'epsilon)))
            
            (define-test regex-alt-test
              (let ([result (sr-add regex-semiring 'a 'b)])
                   (assert-equal '(alt a b) result)))
            
            (define-test regex-seq-test
              (let ([result (sr-mul regex-semiring 'a 'b)])
                   (assert-equal '(seq a b) result))))

;;; ============================================================
;;; Set Semiring Tests
;;; ============================================================

(test-group set-semiring-tests
            (define-test set-semiring-basic-test
              (let ([sr (make-set-semiring '(a b c))])
                   (assert-equal '() (sr-zero sr))
                   (assert-equal '(a b c) (sr-one sr))))
            
            (define-test set-semiring-union-test
              (let ([sr (make-set-semiring '(a b c d))])
                   (let ([result (sr-add sr '(a b) '(b c))])
                        (assert-true (not (not (member 'a result))))
                        (assert-true (not (not (member 'b result))))
                        (assert-true (not (not (member 'c result)))))))
            
            (define-test set-semiring-intersect-test
              (let ([sr (make-set-semiring '(a b c d))])
                   (let ([result (sr-mul sr '(a b c) '(b c d))])
                        (assert-true (not (not (member 'b result))))
                        (assert-true (not (not (member 'c result))))
                        (assert-false (member 'a result))))))

;;; ============================================================
;;; Star Semiring Tests
;;; ============================================================

(test-group star-semiring-tests
            (define-test star-semiring-type-test
              (assert-true (star-semiring? boolean-star-semiring))
              (assert-true (star-semiring? tropical-star-semiring)))
            
            (define-test boolean-star-test
              (let ([star (star-semiring-star boolean-star-semiring)])
                   (assert-true (star #f))
                   (assert-true (star #t))))
            
            (define-test tropical-star-test
              (let ([star (star-semiring-star tropical-star-semiring)])
                   (assert-equal 0 (star 0))
                   (assert-equal 0 (star 5))
                   (assert-equal '-inf (star -1)))))  ; Negative cycle

;;; Helper for verification tests (defined before tests that use them)
(define (every-string-contains? strings substr)
  (fold-left (lambda (acc s)
                     (and acc (string-contains-simple? s substr)))
             #t
             strings))

(define (string-contains-simple? s substr)
  (let ([s-len (string-length s)]
        [sub-len (string-length substr)])
       (let loop ([i 0])
            (if (> (+ i sub-len) s-len)
                #f
                (if (string=? (substring s i (+ i sub-len)) substr)
                    #t
                    (loop (+ i 1)))))))

;;; ============================================================
;;; Verification Tests
;;; ============================================================

(test-group verification
            (define-test verify-natural-test
              (let ([results (sr-verify-laws natural-semiring '(0 1 2 3))])
                   (assert-true (every-string-contains? results "PASS"))))
            
            (define-test verify-boolean-test
              (let ([results (sr-verify-laws boolean-semiring '(#f #t))])
                   (assert-true (every-string-contains? results "PASS")))))

;;; ============================================================
;;; Display Tests
;;; ============================================================

(test-group display-tests
            (define-test semiring-to-string-test
              (let ([str (semiring->string boolean-semiring)])
                   (assert-true (string? str))
                   (assert-true (> (string-length str) 0))))
            
            (define-test matrix-to-string-test
              (let ([str (matrix->string '((1 2) (3 4)))])
                   (assert-true (string? str)))))

;;; ============================================================
;;; Edge Cases
;;; ============================================================

(test-group edge-cases
            (define-test empty-sum-test
              (assert-equal 0 (sr-sum natural-semiring '()))
              (assert-false (sr-sum boolean-semiring '())))
            
            (define-test empty-product-test
              (assert-equal 1 (sr-product natural-semiring '()))
              (assert-true (sr-product boolean-semiring '())))
            
            (define-test single-element-test
              (assert-equal 5 (sr-sum natural-semiring '(5)))
              (assert-equal 5 (sr-product natural-semiring '(5)))))

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
[SUCCESS] All semiring tests passed.
")
    (display "
[FAILURE] Some semiring tests failed.
"))
