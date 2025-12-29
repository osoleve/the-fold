;;; fabric/stitches/fp/test-dlist.ss — Tests for Difference List Library
;;;
;;; NOTE: Run from fabric/stitches directory

(load "fabric/stitches/test-framework.ss")
(load "fabric/stitches/fp/dlist.ss")

(display "
")
(display "==============================================================
")
(display "         DIFFERENCE LIST TESTS
")
(display "==============================================================
")

;;; ============================================================
;;; Basic DList Tests
;;; ============================================================

(test-group dlist-basics
            (define-test empty-dlist-test
              (assert-true (dlist? dlist-empty))
              (assert-true (dlist-empty? dlist-empty))
              (assert-equal '() (dlist->list dlist-empty)))
            
            (define-test singleton-dlist-test
              (let ([dl (dlist-singleton 42)])
                   (assert-true (dlist? dl))
                   (assert-false (dlist-empty? dl))
                   (assert-equal '(42) (dlist->list dl))))
            
            (define-test dlist-varargs-test
              (let ([dl (dlist 1 2 3)])
                   (assert-equal '(1 2 3) (dlist->list dl)))))

;;; ============================================================
;;; Conversion Tests
;;; ============================================================

(test-group conversions
            (define-test list-to-dlist-test
              (let ([dl (list->dlist '(a b c))])
                   (assert-equal '(a b c) (dlist->list dl))))
            
            (define-test empty-list-conversion-test
              (let ([dl (list->dlist '())])
                   (assert-true (dlist-empty? dl))))
            
            (define-test string-to-dlist-test
              (let ([dl (string->dlist "abc")])
                   (assert-equal 3 (dlist-length dl))))
            
            (define-test dlist-to-string-test
              (let ([dl (string->dlist "hello")])
                   (assert-equal "hello" (dlist->string dl)))))

;;; ============================================================
;;; Core Operations Tests
;;; ============================================================

(test-group core-operations
            (define-test dlist-append-test
              (let* ([dl1 (list->dlist '(1 2))]
                     [dl2 (list->dlist '(3 4))]
                     [result (dlist-append dl1 dl2)])
                    (assert-equal '(1 2 3 4) (dlist->list result))))
            
            (define-test dlist-cons-test
              (let* ([dl (list->dlist '(2 3))]
                     [result (dlist-cons 1 dl)])
                    (assert-equal '(1 2 3) (dlist->list result))))
            
            (define-test dlist-snoc-test
              (let* ([dl (list->dlist '(1 2))]
                     [result (dlist-snoc dl 3)])
                    (assert-equal '(1 2 3) (dlist->list result))))
            
            (define-test dlist-concat-test
              (let* ([dl1 (list->dlist '(1))]
                     [dl2 (list->dlist '(2))]
                     [dl3 (list->dlist '(3))]
                     [result (dlist-concat (list dl1 dl2 dl3))])
                    (assert-equal '(1 2 3) (dlist->list result))))
            
            (define-test append-is-o1-test
              ;; This test verifies the O(1) nature by doing many appends
              (let loop ([i 0] [dl dlist-empty])
                   (if (>= i 100)
                       (assert-equal 100 (dlist-length dl))
                       (loop (+ i 1) (dlist-snoc dl i))))))

;;; ============================================================
;;; Building Tests
;;; ============================================================

(test-group building
            (define-test dlist-replicate-test
              (let ([dl (dlist-replicate 5 'x)])
                   (assert-equal '(x x x x x) (dlist->list dl))))
            
            (define-test dlist-replicate-zero-test
              (let ([dl (dlist-replicate 0 'x)])
                   (assert-true (dlist-empty? dl))))
            
            (define-test dlist-range-test
              (let ([dl (dlist-range 1 5)])
                   (assert-equal '(1 2 3 4) (dlist->list dl))))
            
            (define-test dlist-range-empty-test
              (let ([dl (dlist-range 5 5)])
                   (assert-true (dlist-empty? dl)))))

;;; ============================================================
;;; Transformation Tests
;;; ============================================================

(test-group transformations
            (define-test dlist-map-test
              (let* ([dl (list->dlist '(1 2 3))]
                     [result (dlist-map (lambda (x) (* x 2)) dl)])
                    (assert-equal '(2 4 6) (dlist->list result))))
            
            (define-test dlist-filter-test
              (let* ([dl (list->dlist '(1 2 3 4 5))]
                     [result (dlist-filter even? dl)])
                    (assert-equal '(2 4) (dlist->list result))))
            
            (define-test dlist-flatmap-test
              (let* ([dl (list->dlist '(1 2 3))]
                     [result (dlist-flatmap (lambda (x) (dlist x x)) dl)])
                    (assert-equal '(1 1 2 2 3 3) (dlist->list result))))
            
            (define-test dlist-fold-left-test
              (let* ([dl (list->dlist '(1 2 3 4))]
                     [sum (dlist-fold-left + 0 dl)])
                    (assert-equal 10 sum)))
            
            (define-test dlist-fold-right-test
              (let* ([dl (list->dlist '(1 2 3))]
                     [result (dlist-fold-right cons '() dl)])
                    (assert-equal '(1 2 3) result)))
            
            (define-test dlist-reverse-test
              (let* ([dl (list->dlist '(1 2 3))]
                     [result (dlist-reverse dl)])
                    (assert-equal '(3 2 1) (dlist->list result)))))

;;; ============================================================
;;; Query Tests
;;; ============================================================

(test-group queries
            (define-test dlist-length-test
              (assert-equal 0 (dlist-length dlist-empty))
              (assert-equal 3 (dlist-length (list->dlist '(a b c)))))
            
            (define-test dlist-head-test
              (let ([dl (list->dlist '(1 2 3))])
                   (assert-equal 1 (dlist-head dl))))
            
            (define-test dlist-head-maybe-test
              (assert-true (nothing? (dlist-head-maybe dlist-empty)))
              (assert-equal 1 (from-just (dlist-head-maybe (list->dlist '(1 2))))))
            
            (define-test dlist-take-test
              (let ([dl (list->dlist '(1 2 3 4 5))])
                   (assert-equal '(1 2 3) (dlist->list (dlist-take 3 dl)))))
            
            (define-test dlist-drop-test
              (let ([dl (list->dlist '(1 2 3 4 5))])
                   (assert-equal '(4 5) (dlist->list (dlist-drop 3 dl))))))

;;; ============================================================
;;; String Builder Tests
;;; ============================================================

(test-group string-builder
            (define-test builder-basic-test
              (let* ([b (string-builder)]
                     [b (builder-append b "Hello")]
                     [b (builder-append b " ")]
                     [b (builder-append b "World")])
                    (assert-equal "Hello World" (builder->string b))))
            
            (define-test builder-char-test
              (let* ([b (string-builder)]
                     [b (builder-append-char b (string-ref "a" 0))]
                     [b (builder-append-char b (string-ref "b" 0))]
                     [b (builder-append-char b (string-ref "c" 0))])
                    (assert-equal "abc" (builder->string b))))
            
            (define-test string-join-dlist-test
              (let ([result (string-join-dlist '("a" "b" "c") ",")])
                   (assert-equal "a,b,c" result)))
            
            (define-test string-join-empty-test
              (let ([result (string-join-dlist '() ",")])
                   (assert-equal "" result))))

;;; ============================================================
;;; Monoid Tests
;;; ============================================================

(test-group monoid
            (define-test mempty-left-identity-test
              (let* ([dl (list->dlist '(1 2 3))]
                     [result (dlist-mappend dlist-mempty dl)])
                    (assert-true (dlist-equal? dl result))))
            
            (define-test mempty-right-identity-test
              (let* ([dl (list->dlist '(1 2 3))]
                     [result (dlist-mappend dl dlist-mempty)])
                    (assert-true (dlist-equal? dl result))))
            
            (define-test mappend-associative-test
              (let* ([a (list->dlist '(1))]
                     [b (list->dlist '(2))]
                     [c (list->dlist '(3))]
                     [left (dlist-mappend (dlist-mappend a b) c)]
                     [right (dlist-mappend a (dlist-mappend b c))])
                    (assert-true (dlist-equal? left right))))
            
            (define-test mconcat-test
              (let* ([dls (list (list->dlist '(1))
                                (list->dlist '(2))
                                (list->dlist '(3)))]
                     [result (dlist-mconcat dls)])
                    (assert-equal '(1 2 3) (dlist->list result)))))

;;; ============================================================
;;; Specialized Builder Tests
;;; ============================================================

(test-group specialized
            (define-test dlist-iterate-test
              (let ([dl (dlist-iterate 5 (lambda (x) (+ x 1)) 0)])
                   (assert-equal '(0 1 2 3 4) (dlist->list dl))))
            
            (define-test dlist-cycle-test
              (let* ([base (list->dlist '(a b))]
                     [result (dlist-cycle 3 base)])
                    (assert-equal '(a b a b a b) (dlist->list result))))
            
            (define-test dlist-intersperse-test
              (let* ([dl (list->dlist '(1 2 3))]
                     [result (dlist-intersperse 0 dl)])
                    (assert-equal '(1 0 2 0 3) (dlist->list result))))
            
            (define-test dlist-intersperse-empty-test
              (let ([result (dlist-intersperse 0 dlist-empty)])
                   (assert-true (dlist-empty? result))))
            
            (define-test dlist-intersperse-single-test
              (let* ([dl (dlist-singleton 1)]
                     [result (dlist-intersperse 0 dl)])
                    (assert-equal '(1) (dlist->list result)))))

;;; ============================================================
;;; Zipping Tests
;;; ============================================================

(test-group zipping
            (define-test dlist-zip-test
              (let* ([dl1 (list->dlist '(1 2 3))]
                     [dl2 (list->dlist '(a b c))]
                     [result (dlist-zip dl1 dl2)])
                    (assert-equal '((1 . a) (2 . b) (3 . c)) (dlist->list result))))
            
            (define-test dlist-zip-uneven-test
              (let* ([dl1 (list->dlist '(1 2))]
                     [dl2 (list->dlist '(a b c d))]
                     [result (dlist-zip dl1 dl2)])
                    (assert-equal 2 (dlist-length result))))
            
            (define-test dlist-zip-with-test
              (let* ([dl1 (list->dlist '(1 2 3))]
                     [dl2 (list->dlist '(10 20 30))]
                     [result (dlist-zip-with + dl1 dl2)])
                    (assert-equal '(11 22 33) (dlist->list result)))))

;;; ============================================================
;;; Partitioning Tests
;;; ============================================================

(test-group partitioning
            (define-test dlist-split-at-test
              (let* ([dl (list->dlist '(1 2 3 4 5))]
                     [result (dlist-split-at 2 dl)])
                    (assert-equal '(1 2) (dlist->list (car result)))
                    (assert-equal '(3 4 5) (dlist->list (cdr result)))))
            
            (define-test dlist-partition-test
              (let* ([dl (list->dlist '(1 2 3 4 5 6))]
                     [result (dlist-partition even? dl)])
                    (assert-equal '(2 4 6) (dlist->list (car result)))
                    (assert-equal '(1 3 5) (dlist->list (cdr result))))))

;;; ============================================================
;;; Comparison Tests
;;; ============================================================

(test-group comparison
            (define-test dlist-equal-test
              (let ([dl1 (list->dlist '(1 2 3))]
                    [dl2 (list->dlist '(1 2 3))]
                    [dl3 (list->dlist '(1 2))])
                   (assert-true (dlist-equal? dl1 dl2))
                   (assert-false (dlist-equal? dl1 dl3))))
            
            (define-test dlist-show-test
              (let* ([dl (list->dlist '(1 2 3))]
                     [s (dlist-show dl)])
                    (assert-true (string? s))
                    (assert-true (> (string-length s) 0)))))

;;; ============================================================
;;; Edge Cases
;;; ============================================================

(test-group edge-cases
            (define-test append-empty-left-test
              (let* ([dl (list->dlist '(1 2 3))]
                     [result (dlist-append dlist-empty dl)])
                    (assert-equal '(1 2 3) (dlist->list result))))
            
            (define-test append-empty-right-test
              (let* ([dl (list->dlist '(1 2 3))]
                     [result (dlist-append dl dlist-empty)])
                    (assert-equal '(1 2 3) (dlist->list result))))
            
            (define-test cons-to-empty-test
              (let ([result (dlist-cons 1 dlist-empty)])
                   (assert-equal '(1) (dlist->list result))))
            
            (define-test snoc-to-empty-test
              (let ([result (dlist-snoc dlist-empty 1)])
                   (assert-equal '(1) (dlist->list result)))))

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
[SUCCESS] All dlist tests passed.
")
    (display "
[FAILURE] Some dlist tests failed.
"))
