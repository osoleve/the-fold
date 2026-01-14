;;; lattice/fp/clp/test-domain.ss — Tests for Finite Domain
;;;
;;; Tests domain representation, operations, and edge cases.

(load "core/testing/test-framework.ss")
(load "lattice/fp/clp/domain.ss")

;;; ====
;;; Constructor Tests
;;; ====

(test-group "domain-constructors"
            
            (define-test "make-domain creates single interval"
              (assert-equal '((1 . 5)) (make-domain 1 5)))
            
            (define-test "make-domain with lo = hi creates singleton"
              (assert-equal '((3 . 3)) (make-domain 3 3)))
            
            (define-test "make-domain with lo > hi creates empty"
              (assert-equal '() (make-domain 5 3)))
            
            (define-test "domain-singleton creates single-value domain"
              (assert-equal '((7 . 7)) (domain-singleton 7)))
            
            (define-test "domain-from-list handles empty list"
              (assert-equal '() (domain-from-list '())))
            
            (define-test "domain-from-list handles single value"
              (assert-equal '((5 . 5)) (domain-from-list '(5))))
            
            (define-test "domain-from-list merges consecutive values"
              (assert-equal '((1 . 5)) (domain-from-list '(1 2 3 4 5))))
            
            (define-test "domain-from-list handles gaps"
              (assert-equal '((1 . 3) (7 . 9)) (domain-from-list '(1 2 3 7 8 9))))
            
            (define-test "domain-from-list handles unsorted input"
              (assert-equal '((1 . 3)) (domain-from-list '(3 1 2))))
            
            (define-test "domain-from-list handles duplicates"
              (assert-equal '((1 . 3)) (domain-from-list '(1 2 2 3 1 3))))
            )

;;; ====
;;; Predicate Tests
;;; ====

(test-group "domain-predicates"
            
            (define-test "domain-empty? true for empty"
              (assert-true (domain-empty? '())))
            
            (define-test "domain-empty? false for non-empty"
              (assert-false (domain-empty? (make-domain 1 5))))
            
            (define-test "domain-singleton? true for singleton"
              (assert-true (domain-singleton? (domain-singleton 5))))
            
            (define-test "domain-singleton? false for range"
              (assert-false (domain-singleton? (make-domain 1 5))))
            
            (define-test "domain-singleton? false for empty"
              (assert-false (domain-singleton? '())))
            
            (define-test "domain-contains? finds value in single interval"
              (assert-true (domain-contains? (make-domain 1 10) 5)))
            
            (define-test "domain-contains? finds value at boundary"
              (assert-true (domain-contains? (make-domain 1 10) 1))
              (assert-true (domain-contains? (make-domain 1 10) 10)))
            
            (define-test "domain-contains? rejects value outside"
              (assert-false (domain-contains? (make-domain 1 10) 0))
              (assert-false (domain-contains? (make-domain 1 10) 11)))
            
            (define-test "domain-contains? works with multiple intervals"
              (let ([dom (domain-from-list '(1 2 3 10 11 12))])
                   (assert-true (domain-contains? dom 2))
                   (assert-true (domain-contains? dom 11))
                   (assert-false (domain-contains? dom 5))))
            )

;;; ====
;;; Accessor Tests
;;; ====

(test-group "domain-accessors"
            
            (define-test "domain-size counts single interval"
              (assert-equal 5 (domain-size (make-domain 1 5))))
            
            (define-test "domain-size counts multiple intervals"
              (assert-equal 6 (domain-size (domain-from-list '(1 2 3 10 11 12)))))
            
            (define-test "domain-size returns 0 for empty"
              (assert-equal 0 (domain-size '())))
            
            (define-test "domain-min returns minimum"
              (assert-equal 1 (domain-min (make-domain 1 10))))
            
            (define-test "domain-min returns #f for empty"
              (assert-false (domain-min '())))
            
            (define-test "domain-max returns maximum"
              (assert-equal 10 (domain-max (make-domain 1 10))))
            
            (define-test "domain-max returns #f for empty"
              (assert-false (domain-max '())))
            
            (define-test "domain-bounds returns (min . max)"
              (assert-equal (cons 1 10) (domain-bounds (make-domain 1 10))))
            )

;;; ====
;;; Operation Tests
;;; ====

(test-group "domain-operations"
            
            ;; Intersection
            (define-test "domain-intersect overlapping intervals"
              (assert-equal '((3 . 7))
                            (domain-intersect (make-domain 1 7) (make-domain 3 10))))
            
            (define-test "domain-intersect disjoint intervals"
              (assert-equal '()
                            (domain-intersect (make-domain 1 5) (make-domain 10 15))))
            
            (define-test "domain-intersect identical intervals"
              (assert-equal '((1 . 10))
                            (domain-intersect (make-domain 1 10) (make-domain 1 10))))
            
            (define-test "domain-intersect subset"
              (assert-equal '((3 . 5))
                            (domain-intersect (make-domain 1 10) (make-domain 3 5))))
            
            (define-test "domain-intersect with empty"
              (assert-equal '()
                            (domain-intersect (make-domain 1 10) '())))
            
            ;; Union
            (define-test "domain-union overlapping intervals"
              (assert-equal '((1 . 10))
                            (domain-union (make-domain 1 7) (make-domain 5 10))))
            
            (define-test "domain-union adjacent intervals"
              (assert-equal '((1 . 10))
                            (domain-union (make-domain 1 5) (make-domain 6 10))))
            
            (define-test "domain-union disjoint intervals"
              (assert-equal '((1 . 5) (10 . 15))
                            (domain-union (make-domain 1 5) (make-domain 10 15))))
            
            (define-test "domain-union with empty"
              (assert-equal '((1 . 10))
                            (domain-union (make-domain 1 10) '())))
            
            ;; Subtract value
            (define-test "domain-subtract-value from middle"
              (assert-equal '((1 . 4) (6 . 10))
                            (domain-subtract-value (make-domain 1 10) 5)))
            
            (define-test "domain-subtract-value from boundary"
              (assert-equal '((2 . 10))
                            (domain-subtract-value (make-domain 1 10) 1))
              (assert-equal '((1 . 9))
                            (domain-subtract-value (make-domain 1 10) 10)))
            
            (define-test "domain-subtract-value singleton"
              (assert-equal '()
                            (domain-subtract-value (domain-singleton 5) 5)))
            
            (define-test "domain-subtract-value not present"
              (assert-equal '((1 . 10))
                            (domain-subtract-value (make-domain 1 10) 15)))
            
            ;; Subtract domain
            (define-test "domain-subtract removes overlap"
              (assert-equal '((1 . 4))
                            (domain-subtract (make-domain 1 10) (make-domain 5 15))))
            
            (define-test "domain-subtract splits interval"
              (assert-equal '((1 . 4) (8 . 10))
                            (domain-subtract (make-domain 1 10) (make-domain 5 7))))
            
            ;; Restrict min/max
            (define-test "domain-restrict-min keeps values >= n"
              (assert-equal '((5 . 10))
                            (domain-restrict-min (make-domain 1 10) 5)))
            
            (define-test "domain-restrict-max keeps values <= n"
              (assert-equal '((1 . 5))
                            (domain-restrict-max (make-domain 1 10) 5)))
            )

;;; ====
;;; Enumeration Tests
;;; ====

(test-group "domain-enumeration"
            
            (define-test "domain->list converts single interval"
              (assert-equal '(1 2 3 4 5) (domain->list (make-domain 1 5))))
            
            (define-test "domain->list converts multiple intervals"
              (assert-equal '(1 2 3 7 8 9)
                            (domain->list (domain-from-list '(1 2 3 7 8 9)))))
            
            (define-test "domain->list handles empty"
              (assert-equal '() (domain->list '())))
            
            (define-test "domain-fold sums values"
              (assert-equal 15 (domain-fold (make-domain 1 5) 0 +)))
            )

;;; ====
;;; Comparison Tests
;;; ====

(test-group "domain-comparison"
            
            (define-test "domain=? true for equal"
              (assert-true (domain=? (make-domain 1 10) (make-domain 1 10))))
            
            (define-test "domain=? false for different"
              (assert-false (domain=? (make-domain 1 10) (make-domain 1 11))))
            
            (define-test "domain-subset? true for subset"
              (assert-true (domain-subset? (make-domain 3 7) (make-domain 1 10))))
            
            (define-test "domain-subset? true for equal"
              (assert-true (domain-subset? (make-domain 1 10) (make-domain 1 10))))
            
            (define-test "domain-subset? false for partial overlap"
              (assert-false (domain-subset? (make-domain 1 10) (make-domain 5 15))))
            )

;;; ====
;;; Display Tests
;;; ====

(test-group "domain-display"
            
            (define-test "domain->string empty"
              (assert-equal "{}" (domain->string '())))
            
            (define-test "domain->string singleton"
              (assert-equal "{5}" (domain->string (domain-singleton 5))))
            
            (define-test "domain->string range"
              (assert-equal "{1..10}" (domain->string (make-domain 1 10))))
            
            (define-test "domain->string multiple intervals"
              (assert-equal "{1..3, 7..9}"
                            (domain->string (domain-from-list '(1 2 3 7 8 9)))))
            )

;;; ====
;;; Run Tests
;;; ====

(run-all-tests)
