;;; fabric/stitches/fp/test-set.ss — Tests for Set Library
;;;
;;; NOTE: Run from fabric/stitches directory

(load "test-framework.ss")
(load "fp/set.ss")

(display "\n")
(display "==============================================================\n")
(display "         SET TESTS\n")
(display "==============================================================\n")

;;; ============================================================
;;; Basic Set Tests
;;; ============================================================

(test-group set-basics
  (define-test empty-set-test
    (let ([s (set-empty)])
      (assert-true (set? s))
      (assert-true (set-empty? s))
      (assert-equal 0 (set-size s))))

  (define-test singleton-set-test
    (let ([s (set-singleton 42)])
      (assert-false (set-empty? s))
      (assert-equal 1 (set-size s))
      (assert-true (set-member? s 42))))

  (define-test insert-test
    (let* ([s (set-empty)]
           [s (set-insert s 1)]
           [s (set-insert s 2)]
           [s (set-insert s 3)])
      (assert-equal 3 (set-size s))
      (assert-true (set-member? s 1))
      (assert-true (set-member? s 2))
      (assert-true (set-member? s 3))))

  (define-test duplicate-insert-test
    (let* ([s (set-empty)]
           [s (set-insert s 1)]
           [s (set-insert s 1)]
           [s (set-insert s 1)])
      (assert-equal 1 (set-size s)))))

;;; ============================================================
;;; Membership Tests
;;; ============================================================

(test-group membership
  (define-test member-test
    (let ([s (list->set '(1 2 3 4 5))])
      (assert-true (set-member? s 1))
      (assert-true (set-member? s 3))
      (assert-true (set-member? s 5))
      (assert-false (set-member? s 0))
      (assert-false (set-member? s 6))))

  (define-test member-empty-test
    (let ([s (set-empty)])
      (assert-false (set-member? s 1)))))

;;; ============================================================
;;; Delete Tests
;;; ============================================================

(test-group deletion
  (define-test delete-test
    (let* ([s (list->set '(1 2 3 4 5))]
           [s (set-delete s 3)])
      (assert-equal 4 (set-size s))
      (assert-false (set-member? s 3))
      (assert-true (set-member? s 1))
      (assert-true (set-member? s 5))))

  (define-test delete-not-present-test
    (let* ([s (list->set '(1 2 3))]
           [s (set-delete s 99)])
      (assert-equal 3 (set-size s))))

  (define-test delete-all-test
    (let* ([s (list->set '(1 2 3))]
           [s (set-delete s 1)]
           [s (set-delete s 2)]
           [s (set-delete s 3)])
      (assert-true (set-empty? s)))))

;;; ============================================================
;;; Conversion Tests
;;; ============================================================

(test-group conversions
  (define-test list-to-set-test
    (let ([s (list->set '(3 1 4 1 5 9 2 6))])
      ;; Duplicates removed
      (assert-equal 7 (set-size s))))

  (define-test set-to-list-test
    (let* ([s (list->set '(3 1 4 1 5))]
           [lst (set->list s)])
      ;; Should be sorted
      (assert-equal '(1 3 4 5) lst)))

  (define-test empty-conversions-test
    (let ([s (list->set '())])
      (assert-true (set-empty? s))
      (assert-equal '() (set->list s)))))

;;; ============================================================
;;; Set Operations Tests
;;; ============================================================

(test-group set-operations
  (define-test union-test
    (let* ([s1 (list->set '(1 2 3))]
           [s2 (list->set '(3 4 5))]
           [result (set-union s1 s2)])
      (assert-equal '(1 2 3 4 5) (set->list result))))

  (define-test intersection-test
    (let* ([s1 (list->set '(1 2 3 4))]
           [s2 (list->set '(3 4 5 6))]
           [result (set-intersection s1 s2)])
      (assert-equal '(3 4) (set->list result))))

  (define-test difference-test
    (let* ([s1 (list->set '(1 2 3 4))]
           [s2 (list->set '(3 4 5 6))]
           [result (set-difference s1 s2)])
      (assert-equal '(1 2) (set->list result))))

  (define-test symmetric-difference-test
    (let* ([s1 (list->set '(1 2 3))]
           [s2 (list->set '(2 3 4))]
           [result (set-symmetric-difference s1 s2)])
      (assert-equal '(1 4) (set->list result))))

  (define-test union-empty-test
    (let* ([s (list->set '(1 2 3))]
           [result (set-union s (set-empty))])
      (assert-equal '(1 2 3) (set->list result))))

  (define-test intersection-empty-test
    (let* ([s (list->set '(1 2 3))]
           [result (set-intersection s (set-empty))])
      (assert-true (set-empty? result)))))

;;; ============================================================
;;; Subset Tests
;;; ============================================================

(test-group subset-tests
  (define-test subset-test
    (let* ([s1 (list->set '(1 2))]
           [s2 (list->set '(1 2 3 4))])
      (assert-true (set-subset? s1 s2))
      (assert-false (set-subset? s2 s1))))

  (define-test subset-equal-test
    (let* ([s1 (list->set '(1 2 3))]
           [s2 (list->set '(1 2 3))])
      (assert-true (set-subset? s1 s2))
      (assert-true (set-subset? s2 s1))))

  (define-test proper-subset-test
    (let* ([s1 (list->set '(1 2))]
           [s2 (list->set '(1 2 3))]
           [s3 (list->set '(1 2))])
      (assert-true (set-proper-subset? s1 s2))
      (assert-false (set-proper-subset? s1 s3))))

  (define-test empty-subset-test
    (let ([s (list->set '(1 2 3))])
      (assert-true (set-subset? (set-empty) s)))))

;;; ============================================================
;;; Equality and Disjoint Tests
;;; ============================================================

(test-group equality-disjoint
  (define-test equal-test
    (let* ([s1 (list->set '(3 1 2))]
           [s2 (list->set '(1 2 3))]
           [s3 (list->set '(1 2))])
      (assert-true (set-equal? s1 s2))
      (assert-false (set-equal? s1 s3))))

  (define-test disjoint-test
    (let* ([s1 (list->set '(1 2 3))]
           [s2 (list->set '(4 5 6))]
           [s3 (list->set '(3 4 5))])
      (assert-true (set-disjoint? s1 s2))
      (assert-false (set-disjoint? s1 s3)))))

;;; ============================================================
;;; Transformation Tests
;;; ============================================================

(test-group transformations
  (define-test map-test
    (let* ([s (list->set '(1 2 3))]
           [result (set-map (lambda (x) (* x 2)) s)])
      (assert-equal '(2 4 6) (set->list result))))

  (define-test map-collapse-test
    ;; Map that produces duplicates
    (let* ([s (list->set '(1 2 3 4))]
           [result (set-map (lambda (x) (modulo x 2)) s)])
      (assert-equal '(0 1) (set->list result))))

  (define-test filter-test
    (let* ([s (list->set '(1 2 3 4 5 6))]
           [result (set-filter even? s)])
      (assert-equal '(2 4 6) (set->list result))))

  (define-test partition-test
    (let* ([s (list->set '(1 2 3 4 5 6))]
           [result (set-partition even? s)])
      (assert-equal '(2 4 6) (set->list (car result)))
      (assert-equal '(1 3 5) (set->list (cdr result)))))

  (define-test fold-test
    (let* ([s (list->set '(1 2 3 4 5))]
           [sum (set-fold + 0 s)])
      (assert-equal 15 sum))))

;;; ============================================================
;;; Min/Max Tests
;;; ============================================================

(test-group min-max
  (define-test min-test
    (let ([s (list->set '(5 3 7 1 9))])
      (assert-equal 1 (from-just (set-min s)))))

  (define-test max-test
    (let ([s (list->set '(5 3 7 1 9))])
      (assert-equal 9 (from-just (set-max s)))))

  (define-test min-max-empty-test
    (let ([s (set-empty)])
      (assert-true (nothing? (set-min s)))
      (assert-true (nothing? (set-max s)))))

  (define-test delete-min-test
    (let* ([s (list->set '(1 2 3 4 5))]
           [result (set-delete-min s)])
      (assert-equal '(2 3 4 5) (set->list result))))

  (define-test delete-max-test
    (let* ([s (list->set '(1 2 3 4 5))]
           [result (set-delete-max s)])
      (assert-equal '(1 2 3 4) (set->list result)))))

;;; ============================================================
;;; Quantifier Tests
;;; ============================================================

(test-group quantifiers
  (define-test any-test
    (let ([s (list->set '(1 3 5 7 9))])
      (assert-true (set-any? (lambda (x) (> x 5)) s))
      (assert-false (set-any? (lambda (x) (> x 10)) s))))

  (define-test all-test
    (let ([s (list->set '(2 4 6 8))])
      (assert-true (set-all? even? s))
      (assert-false (set-all? (lambda (x) (< x 5)) s))))

  (define-test find-test
    (let ([s (list->set '(1 2 3 4 5))])
      (assert-equal 4 (from-just (set-find (lambda (x) (> x 3)) s)))
      (assert-true (nothing? (set-find (lambda (x) (> x 10)) s))))))

;;; ============================================================
;;; Builder Tests
;;; ============================================================

(test-group builders
  (define-test range-test
    (let ([s (set-range 1 5)])
      (assert-equal '(1 2 3 4) (set->list s))))

  (define-test range-empty-test
    (let ([s (set-range 5 5)])
      (assert-true (set-empty? s))))

  (define-test generate-test
    (let ([s (set-generate 5 (lambda (i) (* i i)))])
      (assert-equal '(0 1 4 9 16) (set->list s)))))

;;; ============================================================
;;; Power Set Tests
;;; ============================================================

(test-group power-set
  (define-test power-set-test
    (let* ([s (list->set '(1 2))]
           [ps (set-power-set s)])
      (assert-equal 4 (length ps))))  ; 2^2 = 4 subsets

  (define-test power-set-empty-test
    (let ([ps (set-power-set (set-empty))])
      (assert-equal 1 (length ps))))  ; Just the empty set

  (define-test power-set-singleton-test
    (let ([ps (set-power-set (set-singleton 1))])
      (assert-equal 2 (length ps)))))  ; {} and {1}

;;; ============================================================
;;; Cartesian Product Tests
;;; ============================================================

(test-group cartesian-product
  (define-test product-test
    (let* ([s1 (list->set '(1 2))]
           [s2 (list->set '(3 4))]
           [result (set-product s1 s2)])
      (assert-equal 4 (set-size result))
      (assert-true (set-member? result (cons 1 3)))
      (assert-true (set-member? result (cons 2 4)))))

  (define-test product-empty-test
    (let* ([s1 (list->set '(1 2))]
           [result (set-product s1 (set-empty))])
      (assert-true (set-empty? result)))))

;;; ============================================================
;;; Monoid Tests
;;; ============================================================

(test-group monoid
  (define-test mempty-test
    (assert-true (set-empty? (set-mempty))))

  (define-test mappend-test
    (let* ([s1 (list->set '(1 2))]
           [s2 (list->set '(3 4))]
           [result (set-mappend s1 s2)])
      (assert-equal '(1 2 3 4) (set->list result))))

  (define-test mconcat-test
    (let* ([sets (list (list->set '(1))
                       (list->set '(2))
                       (list->set '(3)))]
           [result (set-mconcat sets)])
      (assert-equal '(1 2 3) (set->list result)))))

;;; ============================================================
;;; Display Tests
;;; ============================================================

(test-group display-tests
  (define-test to-string-test
    (let* ([s (list->set '(1 2 3))]
           [str (set->string s)])
      (assert-true (string? str))
      (assert-true (> (string-length str) 0))))

  (define-test empty-to-string-test
    (let ([str (set->string (set-empty))])
      (assert-equal "{}" str))))

;;; ============================================================
;;; Different Types Tests
;;; ============================================================

(test-group different-types
  (define-test string-set-test
    (let ([s (list->set '("apple" "banana" "cherry"))])
      (assert-equal 3 (set-size s))
      (assert-true (set-member? s "banana"))))

  (define-test symbol-set-test
    (let ([s (list->set '(foo bar baz))])
      (assert-equal 3 (set-size s))
      (assert-true (set-member? s 'bar)))))

;;; ============================================================
;;; Large Set Tests
;;; ============================================================

(test-group large-sets
  (define-test large-insert-test
    (let loop ([i 0] [s (set-empty)])
      (if (>= i 100)
          (assert-equal 100 (set-size s))
          (loop (+ i 1) (set-insert s i)))))

  (define-test large-operations-test
    (let* ([s1 (set-range 0 50)]
           [s2 (set-range 25 75)]
           [union (set-union s1 s2)]
           [intersection (set-intersection s1 s2)])
      (assert-equal 75 (set-size union))
      (assert-equal 25 (set-size intersection)))))

;;; ============================================================
;;; Edge Cases
;;; ============================================================

(test-group edge-cases
  (define-test single-element-operations-test
    (let* ([s (set-singleton 1)]
           [deleted (set-delete s 1)])
      (assert-true (set-empty? deleted))))

  (define-test self-union-test
    (let* ([s (list->set '(1 2 3))]
           [result (set-union s s)])
      (assert-true (set-equal? s result))))

  (define-test self-intersection-test
    (let* ([s (list->set '(1 2 3))]
           [result (set-intersection s s)])
      (assert-true (set-equal? s result))))

  (define-test self-difference-test
    (let* ([s (list->set '(1 2 3))]
           [result (set-difference s s)])
      (assert-true (set-empty? result)))))

;;; ============================================================
;;; Summary
;;; ============================================================

(display "\n")
(display "==============================================================\n")
(printf "Tests passed: ~a\n" *tests-passed*)
(printf "Tests failed: ~a\n" *tests-failed*)
(printf "Total tests:  ~a\n" *tests-run*)

(if (= *tests-failed* 0)
    (display "\n[SUCCESS] All set tests passed.\n")
    (display "\n[FAILURE] Some set tests failed.\n"))
