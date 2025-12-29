;;; fabric/stitches/fp/test-map.ss — Tests for Map Library
;;;
;;; NOTE: Run from fabric/stitches directory

(load "fabric/stitches/test-framework.ss")
(load "fabric/stitches/fp/map.ss")

(display "
")
(display "==============================================================
")
(display "         MAP TESTS
")
(display "==============================================================
")

;;; ============================================================
;;; Basic Map Tests
;;; ============================================================

(test-group map-basics
            (define-test empty-map-test
              (let ([m (map-empty)])
                   (assert-true (map? m))
                   (assert-true (map-empty? m))
                   (assert-equal 0 (map-size m))))
            
            (define-test singleton-map-test
              (let ([m (map-singleton 'key 'value)])
                   (assert-false (map-empty? m))
                   (assert-equal 1 (map-size m))
                   (assert-equal 'value (from-just (map-lookup m 'key)))))
            
            (define-test insert-test
              (let* ([m (map-empty)]
                     [m (map-insert m 'a 1)]
                     [m (map-insert m 'b 2)]
                     [m (map-insert m 'c 3)])
                    (assert-equal 3 (map-size m))
                    (assert-equal 1 (from-just (map-lookup m 'a)))
                    (assert-equal 2 (from-just (map-lookup m 'b)))
                    (assert-equal 3 (from-just (map-lookup m 'c)))))
            
            (define-test update-test
              (let* ([m (map-empty)]
                     [m (map-insert m 'a 1)]
                     [m (map-insert m 'a 100)])  ; Update
                    (assert-equal 1 (map-size m))
                    (assert-equal 100 (from-just (map-lookup m 'a))))))

;;; ============================================================
;;; Lookup Tests
;;; ============================================================

(test-group lookup
            (define-test lookup-found-test
              (let ([m (alist->map '((a . 1) (b . 2) (c . 3)))])
                   (assert-equal 2 (from-just (map-lookup m 'b)))))
            
            (define-test lookup-not-found-test
              (let ([m (alist->map '((a . 1) (b . 2)))])
                   (assert-true (nothing? (map-lookup m 'z)))))
            
            (define-test contains-test
              (let ([m (alist->map '((a . 1) (b . 2)))])
                   (assert-true (map-contains? m 'a))
                   (assert-false (map-contains? m 'z))))
            
            (define-test get-default-test
              (let ([m (alist->map '((a . 1)))])
                   (assert-equal 1 (map-get m 'a 999))
                   (assert-equal 999 (map-get m 'z 999)))))

;;; ============================================================
;;; Delete Tests
;;; ============================================================

(test-group deletion
            (define-test delete-test
              (let* ([m (alist->map '((a . 1) (b . 2) (c . 3)))]
                     [m (map-delete m 'b)])
                    (assert-equal 2 (map-size m))
                    (assert-false (map-contains? m 'b))
                    (assert-true (map-contains? m 'a))
                    (assert-true (map-contains? m 'c))))
            
            (define-test delete-not-present-test
              (let* ([m (alist->map '((a . 1)))]
                     [m (map-delete m 'z)])
                    (assert-equal 1 (map-size m))))
            
            (define-test delete-all-test
              (let* ([m (alist->map '((a . 1) (b . 2)))]
                     [m (map-delete m 'a)]
                     [m (map-delete m 'b)])
                    (assert-true (map-empty? m)))))

;;; ============================================================
;;; Conversion Tests
;;; ============================================================

(test-group conversions
            (define-test alist-to-map-test
              (let ([m (alist->map '((c . 3) (a . 1) (b . 2)))])
                   (assert-equal 3 (map-size m))))
            
            (define-test map-to-alist-test
              (let* ([m (alist->map '((b . 2) (a . 1) (c . 3)))]
                     [alist (map->alist m)])
                    ;; Should be sorted by key
                    (assert-equal '((a . 1) (b . 2) (c . 3)) alist)))
            
            (define-test map-keys-test
              (let ([m (alist->map '((c . 3) (a . 1) (b . 2)))])
                   (assert-equal '(a b c) (map-keys m))))
            
            (define-test map-values-test
              (let ([m (alist->map '((c . 3) (a . 1) (b . 2)))])
                   (assert-equal '(1 2 3) (map-values m)))))

;;; ============================================================
;;; Transformation Tests
;;; ============================================================

(test-group transformations
            (define-test map-map-test
              (let* ([m (alist->map '((a . 1) (b . 2) (c . 3)))]
                     [result (map-map (lambda (v) (* v 10)) m)])
                    (assert-equal 10 (from-just (map-lookup result 'a)))
                    (assert-equal 20 (from-just (map-lookup result 'b)))))
            
            (define-test map-map-kv-test
              (let* ([m (alist->map '((a . 1) (b . 2)))]
                     [result (map-map-kv (lambda (k v) (cons k v)) m)])
                    (assert-equal '(a . 1) (from-just (map-lookup result 'a)))))
            
            (define-test filter-test
              (let* ([m (alist->map '((a . 1) (b . 2) (c . 3) (d . 4)))]
                     [result (map-filter even? m)])
                    (assert-equal 2 (map-size result))
                    (assert-true (map-contains? result 'b))
                    (assert-true (map-contains? result 'd))))
            
            (define-test filter-kv-test
              (let* ([m (alist->map '((a . 1) (b . 2) (c . 3)))]
                     [result (map-filter-kv (lambda (k v) (eq? k 'b)) m)])
                    (assert-equal 1 (map-size result))
                    (assert-true (map-contains? result 'b))))
            
            (define-test partition-test
              (let* ([m (alist->map '((a . 1) (b . 2) (c . 3) (d . 4)))]
                     [result (map-partition even? m)])
                    (assert-equal 2 (map-size (car result)))  ; evens
                    (assert-equal 2 (map-size (cdr result))))) ; odds
            
            (define-test fold-test
              (let* ([m (alist->map '((a . 1) (b . 2) (c . 3)))]
                     [sum (map-fold (lambda (acc k v) (+ acc v)) 0 m)])
                    (assert-equal 6 sum))))

;;; ============================================================
;;; Merge Tests
;;; ============================================================

(test-group merge-operations
            (define-test merge-test
              (let* ([m1 (alist->map '((a . 1) (b . 2)))]
                     [m2 (alist->map '((b . 20) (c . 3)))]
                     [result (map-merge m1 m2)])
                    (assert-equal 3 (map-size result))
                    (assert-equal 1 (from-just (map-lookup result 'a)))
                    (assert-equal 20 (from-just (map-lookup result 'b)))  ; m2 wins
                    (assert-equal 3 (from-just (map-lookup result 'c)))))
            
            (define-test merge-with-test
              (let* ([m1 (alist->map '((a . 1) (b . 2)))]
                     [m2 (alist->map '((b . 20) (c . 3)))]
                     [result (map-merge-with + m1 m2)])
                    (assert-equal 22 (from-just (map-lookup result 'b)))))  ; 2 + 20
            
            (define-test intersection-test
              (let* ([m1 (alist->map '((a . 1) (b . 2) (c . 3)))]
                     [m2 (alist->map '((b . 20) (c . 30) (d . 4)))]
                     [result (map-intersection m1 m2)])
                    (assert-equal 2 (map-size result))
                    (assert-true (map-contains? result 'b))
                    (assert-true (map-contains? result 'c))))
            
            (define-test difference-test
              (let* ([m1 (alist->map '((a . 1) (b . 2) (c . 3)))]
                     [m2 (alist->map '((b . 20) (c . 30)))]
                     [result (map-difference m1 m2)])
                    (assert-equal 1 (map-size result))
                    (assert-true (map-contains? result 'a)))))

;;; ============================================================
;;; Update Tests
;;; ============================================================

(test-group update-operations
            (define-test update-test
              (let* ([m (alist->map '((a . 1) (b . 2)))]
                     [result (map-update m 'a (lambda (v) (* v 10)))])
                    (assert-equal 10 (from-just (map-lookup result 'a)))))
            
            (define-test update-missing-test
              (let* ([m (alist->map '((a . 1)))]
                     [result (map-update m 'z (lambda (v) (* v 10)))])
                    (assert-false (map-contains? result 'z))))
            
            (define-test update-default-test
              (let* ([m (alist->map '((a . 1)))]
                     [result (map-update-default m 'z 0 (lambda (v) (+ v 1)))])
                    (assert-equal 1 (from-just (map-lookup result 'z)))))
            
            (define-test insert-if-absent-test
              (let* ([m (alist->map '((a . 1)))]
                     [m (map-insert-if-absent m 'a 999)]
                     [m (map-insert-if-absent m 'b 2)])
                    (assert-equal 1 (from-just (map-lookup m 'a)))  ; Not changed
                    (assert-equal 2 (from-just (map-lookup m 'b))))))

;;; ============================================================
;;; Min/Max Tests
;;; ============================================================

(test-group min-max
            (define-test min-test
              (let ([m (alist->map '((c . 3) (a . 1) (b . 2)))])
                   (assert-equal '(a . 1) (from-just (map-min m)))))
            
            (define-test max-test
              (let ([m (alist->map '((c . 3) (a . 1) (b . 2)))])
                   (assert-equal '(c . 3) (from-just (map-max m)))))
            
            (define-test min-max-empty-test
              (let ([m (map-empty)])
                   (assert-true (nothing? (map-min m)))
                   (assert-true (nothing? (map-max m)))))
            
            (define-test delete-min-test
              (let* ([m (alist->map '((a . 1) (b . 2) (c . 3)))]
                     [result (map-delete-min m)])
                    (assert-equal 2 (map-size result))
                    (assert-false (map-contains? result 'a))))
            
            (define-test delete-max-test
              (let* ([m (alist->map '((a . 1) (b . 2) (c . 3)))]
                     [result (map-delete-max m)])
                    (assert-equal 2 (map-size result))
                    (assert-false (map-contains? result 'c)))))

;;; ============================================================
;;; Quantifier Tests
;;; ============================================================

(test-group quantifiers
            (define-test any-test
              (let ([m (alist->map '((a . 1) (b . 2) (c . 3)))])
                   (assert-true (map-any? (lambda (k v) (> v 2)) m))
                   (assert-false (map-any? (lambda (k v) (> v 10)) m))))
            
            (define-test all-test
              (let ([m (alist->map '((a . 2) (b . 4) (c . 6)))])
                   (assert-true (map-all? (lambda (k v) (even? v)) m))
                   (assert-false (map-all? (lambda (k v) (> v 3)) m))))
            
            (define-test find-test
              (let ([m (alist->map '((a . 1) (b . 2) (c . 3)))])
                   (let ([found (map-find (lambda (k v) (> v 1)) m)])
                        (assert-true (just? found))
                        (assert-equal 'b (car (from-just found))))
                   (assert-true (nothing? (map-find (lambda (k v) (> v 10)) m))))))

;;; ============================================================
;;; Advanced Operation Tests
;;; ============================================================

(test-group advanced-operations
            (define-test invert-test
              (let* ([m (alist->map '((a . 1) (b . 2) (c . 3)))]
                     [inverted (map-invert m)])
                    (assert-equal 'a (from-just (map-lookup inverted 1)))
                    (assert-equal 'c (from-just (map-lookup inverted 3)))))
            
            (define-test group-by-test
              (let* ([words '("apple" "ant" "banana" "bear" "cat")]
                     [grouped (group-by (lambda (s) (string-ref s 0)) words)])
                    (assert-equal 3 (map-size grouped))))
            
            (define-test frequencies-test
              (let* ([items '(a b a c b a)]
                     [freqs (frequencies items)])
                    (assert-equal 3 (from-just (map-lookup freqs 'a)))
                    (assert-equal 2 (from-just (map-lookup freqs 'b)))
                    (assert-equal 1 (from-just (map-lookup freqs 'c))))))

;;; ============================================================
;;; Display Tests
;;; ============================================================

(test-group display-tests
            (define-test to-string-test
              (let* ([m (alist->map '((a . 1) (b . 2)))]
                     [str (map->string m)])
                    (assert-true (string? str))
                    (assert-true (> (string-length str) 0))))
            
            (define-test empty-to-string-test
              (let ([str (map->string (map-empty))])
                   (assert-equal "{}" str))))

;;; ============================================================
;;; Comparison Tests
;;; ============================================================

(test-group comparison
            (define-test equal-test
              (let* ([m1 (alist->map '((b . 2) (a . 1)))]
                     [m2 (alist->map '((a . 1) (b . 2)))]
                     [m3 (alist->map '((a . 1) (c . 3)))])
                    (assert-true (map-equal? m1 m2))
                    (assert-false (map-equal? m1 m3)))))

;;; ============================================================
;;; Monoid Tests
;;; ============================================================

(test-group monoid
            (define-test mempty-test
              (assert-true (map-empty? (map-mempty))))
            
            (define-test mappend-test
              (let* ([m1 (alist->map '((a . 1)))]
                     [m2 (alist->map '((b . 2)))]
                     [result (map-mappend m1 m2)])
                    (assert-equal 2 (map-size result))))
            
            (define-test mconcat-test
              (let* ([maps (list (alist->map '((a . 1)))
                                 (alist->map '((b . 2)))
                                 (alist->map '((c . 3))))]
                     [result (map-mconcat maps)])
                    (assert-equal 3 (map-size result)))))

;;; ============================================================
;;; Different Key Types Tests
;;; ============================================================

(test-group different-key-types
            (define-test number-keys-test
              (let ([m (alist->map '((1 . "one") (2 . "two") (3 . "three")))])
                   (assert-equal "two" (from-just (map-lookup m 2)))))
            
            (define-test string-keys-test
              (let ([m (alist->map '(("a" . 1) ("b" . 2) ("c" . 3)))])
                   (assert-equal 2 (from-just (map-lookup m "b"))))))

;;; ============================================================
;;; Large Map Tests
;;; ============================================================

(test-group large-maps
            (define-test large-insert-test
              (let loop ([i 0] [m (map-empty)])
                   (if (>= i 100)
                       (assert-equal 100 (map-size m))
                       (loop (+ i 1) (map-insert m i (* i i))))))
            
            (define-test large-lookup-test
              (let ([m (fold-left (lambda (m i) (map-insert m i (* i 2)))
                                  (map-empty)
                                  (iota 100))])
                   (assert-equal 100 (map-size m))
                   (assert-equal 50 (from-just (map-lookup m 25)))
                   (assert-equal 198 (from-just (map-lookup m 99))))))

;;; Helper
(define (iota n)
  (let loop ([i 0] [acc '()])
       (if (>= i n)
           (reverse acc)
           (loop (+ i 1) (cons i acc)))))

;;; ============================================================
;;; Edge Cases
;;; ============================================================

(test-group edge-cases
            (define-test single-element-operations-test
              (let* ([m (map-singleton 'x 42)]
                     [deleted (map-delete m 'x)])
                    (assert-true (map-empty? deleted))))
            
            (define-test self-merge-test
              (let* ([m (alist->map '((a . 1) (b . 2)))]
                     [result (map-merge m m)])
                    (assert-true (map-equal? m result))))
            
            (define-test empty-merge-test
              (let* ([m (alist->map '((a . 1)))]
                     [result (map-merge m (map-empty))])
                    (assert-true (map-equal? m result)))))

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
[SUCCESS] All map tests passed.
")
    (display "
[FAILURE] Some map tests failed.
"))
