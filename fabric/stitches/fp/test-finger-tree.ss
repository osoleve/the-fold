;;; fabric/stitches/fp/test-finger-tree.ss — Tests for Finger Tree Library
;;;
;;; NOTE: Run from fabric/stitches directory

(load "fabric/stitches/test-framework.ss")
(load "fabric/stitches/fp/finger-tree.ss")

(display "
")
(display "==============================================================
")
(display "         FINGER TREE TESTS
")
(display "==============================================================
")

;;; ============================================================
;;; Basic Finger Tree Tests
;;; ============================================================

(test-group finger-tree-basics
            (define-test empty-ft-test
              (assert-true (ft-empty? ft-empty))
              (assert-true (ft-null? ft-empty))
              (assert-equal 0 (ft-size ft-empty)))
            
            (define-test single-ft-test
              (let ([ft (make-ft-single 42)])
                   (assert-true (ft-single? ft))
                   (assert-false (ft-empty? ft))
                   (assert-equal 1 (ft-size ft))
                   (assert-equal 42 (from-just (ft-head-left ft)))
                   (assert-equal 42 (from-just (ft-head-right ft)))))
            
            (define-test cons-left-test
              (let* ([ft ft-empty]
                     [ft (ft-cons-left ft 1)]
                     [ft (ft-cons-left ft 2)]
                     [ft (ft-cons-left ft 3)])
                    (assert-equal 3 (ft-size ft))
                    (assert-equal '(3 2 1) (ft->list ft))))
            
            (define-test snoc-right-test
              (let* ([ft ft-empty]
                     [ft (ft-snoc-right ft 1)]
                     [ft (ft-snoc-right ft 2)]
                     [ft (ft-snoc-right ft 3)])
                    (assert-equal 3 (ft-size ft))
                    (assert-equal '(1 2 3) (ft->list ft)))))

;;; ============================================================
;;; Head/Tail Operations Tests
;;; ============================================================

(test-group head-tail-operations
            (define-test head-left-test
              (let ([ft (list->ft '(1 2 3 4 5))])
                   (assert-equal 1 (from-just (ft-head-left ft)))))
            
            (define-test head-right-test
              (let ([ft (list->ft '(1 2 3 4 5))])
                   (assert-equal 5 (from-just (ft-head-right ft)))))
            
            (define-test head-empty-test
              (assert-true (nothing? (ft-head-left ft-empty)))
              (assert-true (nothing? (ft-head-right ft-empty))))
            
            (define-test tail-left-test
              (let* ([ft (list->ft '(1 2 3 4 5))]
                     [tl (ft-tail-left ft)])
                    (assert-equal '(2 3 4 5) (ft->list tl))))
            
            (define-test tail-right-test
              (let* ([ft (list->ft '(1 2 3 4 5))]
                     [init (ft-tail-right ft)])
                    (assert-equal '(1 2 3 4) (ft->list init))))
            
            (define-test tail-single-test
              (let* ([ft (make-ft-single 42)]
                     [tl (ft-tail-left ft)])
                    (assert-true (ft-empty? tl)))))

;;; ============================================================
;;; Conversion Tests
;;; ============================================================

(test-group conversions
            (define-test list-to-ft-test
              (let ([ft (list->ft '(a b c d e))])
                   (assert-equal 5 (ft-size ft))
                   (assert-equal '(a b c d e) (ft->list ft))))
            
            (define-test empty-list-test
              (let ([ft (list->ft '())])
                   (assert-true (ft-empty? ft))))
            
            (define-test roundtrip-test
              (let* ([lst '(1 2 3 4 5 6 7 8 9 10)]
                     [ft (list->ft lst)]
                     [result (ft->list ft)])
                    (assert-equal lst result))))

;;; ============================================================
;;; Concatenation Tests
;;; ============================================================

(test-group concatenation
            (define-test append-simple-test
              (let* ([ft1 (list->ft '(1 2 3))]
                     [ft2 (list->ft '(4 5 6))]
                     [result (ft-append ft1 ft2)])
                    (assert-equal '(1 2 3 4 5 6) (ft->list result))))
            
            (define-test append-empty-left-test
              (let* ([ft (list->ft '(1 2 3))]
                     [result (ft-append ft-empty ft)])
                    (assert-equal '(1 2 3) (ft->list result))))
            
            (define-test append-empty-right-test
              (let* ([ft (list->ft '(1 2 3))]
                     [result (ft-append ft ft-empty)])
                    (assert-equal '(1 2 3) (ft->list result))))
            
            (define-test append-single-test
              (let* ([ft1 (make-ft-single 1)]
                     [ft2 (make-ft-single 2)]
                     [result (ft-append ft1 ft2)])
                    (assert-equal '(1 2) (ft->list result))))
            
            (define-test mconcat-test
              (let* ([fts (list (list->ft '(1 2))
                                (list->ft '(3 4))
                                (list->ft '(5 6)))]
                     [result (ft-mconcat fts)])
                    (assert-equal '(1 2 3 4 5 6) (ft->list result)))))

;;; ============================================================
;;; Transformation Tests
;;; ============================================================

(test-group transformations
            (define-test ft-map-test
              (let* ([ft (list->ft '(1 2 3 4 5))]
                     [result (ft-map (lambda (x) (* x 2)) ft)])
                    (assert-equal '(2 4 6 8 10) (ft->list result))))
            
            (define-test ft-filter-test
              (let* ([ft (list->ft '(1 2 3 4 5 6))]
                     [result (ft-filter even? ft)])
                    (assert-equal '(2 4 6) (ft->list result))))
            
            (define-test ft-fold-left-test
              (let* ([ft (list->ft '(1 2 3 4 5))]
                     [sum (ft-fold-left + 0 ft)])
                    (assert-equal 15 sum)))
            
            (define-test ft-fold-right-test
              (let* ([ft (list->ft '(1 2 3))]
                     [result (ft-fold-right cons '() ft)])
                    (assert-equal '(1 2 3) result)))
            
            (define-test ft-reverse-test
              (let* ([ft (list->ft '(1 2 3 4 5))]
                     [result (ft-reverse ft)])
                    (assert-equal '(5 4 3 2 1) (ft->list result)))))

;;; ============================================================
;;; Deque Interface Tests
;;; ============================================================

(test-group deque-interface
            (define-test deque-empty-test
              (let ([dq (deque-empty)])
                   (assert-true (deque-empty? dq))
                   (assert-equal 0 (deque-size dq))))
            
            (define-test deque-cons-front-test
              (let* ([dq (deque-empty)]
                     [dq (deque-cons-front dq 3)]
                     [dq (deque-cons-front dq 2)]
                     [dq (deque-cons-front dq 1)])
                    (assert-equal '(1 2 3) (deque->list dq))))
            
            (define-test deque-cons-back-test
              (let* ([dq (deque-empty)]
                     [dq (deque-cons-back dq 1)]
                     [dq (deque-cons-back dq 2)]
                     [dq (deque-cons-back dq 3)])
                    (assert-equal '(1 2 3) (deque->list dq))))
            
            (define-test deque-front-back-test
              (let ([dq (list->deque '(1 2 3 4 5))])
                   (assert-equal 1 (from-just (deque-front dq)))
                   (assert-equal 5 (from-just (deque-back dq)))))
            
            (define-test deque-pop-front-test
              (let* ([dq (list->deque '(1 2 3))]
                     [result (deque-pop-front dq)])
                    (assert-true (not (not result)))
                    (assert-equal 1 (car result))
                    (assert-equal '(2 3) (deque->list (cdr result)))))
            
            (define-test deque-pop-back-test
              (let* ([dq (list->deque '(1 2 3))]
                     [result (deque-pop-back dq)])
                    (assert-true (not (not result)))
                    (assert-equal 3 (car result))
                    (assert-equal '(1 2) (deque->list (cdr result)))))
            
            (define-test deque-reverse-test
              (let* ([dq (list->deque '(1 2 3 4 5))]
                     [result (deque-reverse dq)])
                    (assert-equal '(5 4 3 2 1) (deque->list result)))))

;;; ============================================================
;;; Sequence Interface Tests
;;; ============================================================

(test-group sequence-interface
            (define-test seq-basic-test
              (let ([s (seq-empty)])
                   (assert-true (seq-empty? s))
                   (assert-equal 0 (seq-length s))))
            
            (define-test seq-singleton-test
              (let ([s (seq-singleton 42)])
                   (assert-equal 1 (seq-length s))
                   (assert-equal 42 (from-just (seq-head s)))))
            
            (define-test seq-cons-snoc-test
              (let* ([s (seq-empty)]
                     [s (seq-cons 1 s)]
                     [s (seq-snoc s 2)])
                    (assert-equal '(1 2) (seq->list s))))
            
            (define-test seq-head-last-test
              (let ([s (list->seq '(1 2 3 4 5))])
                   (assert-equal 1 (from-just (seq-head s)))
                   (assert-equal 5 (from-just (seq-last s)))))
            
            (define-test seq-tail-init-test
              (let ([s (list->seq '(1 2 3 4 5))])
                   (assert-equal '(2 3 4 5) (seq->list (seq-tail s)))
                   (assert-equal '(1 2 3 4) (seq->list (seq-init s)))))
            
            (define-test seq-append-test
              (let* ([s1 (list->seq '(1 2))]
                     [s2 (list->seq '(3 4))]
                     [result (seq-append s1 s2)])
                    (assert-equal '(1 2 3 4) (seq->list result)))))

;;; ============================================================
;;; Split Operations Tests
;;; ============================================================

(test-group split-operations
            (define-test seq-split-at-test
              (let* ([s (list->seq '(1 2 3 4 5))]
                     [result (seq-split-at 2 s)])
                    (assert-equal '(1 2) (seq->list (car result)))
                    (assert-equal '(3 4 5) (seq->list (cdr result)))))
            
            (define-test seq-take-test
              (let* ([s (list->seq '(1 2 3 4 5))]
                     [result (seq-take 3 s)])
                    (assert-equal '(1 2 3) (seq->list result))))
            
            (define-test seq-drop-test
              (let* ([s (list->seq '(1 2 3 4 5))]
                     [result (seq-drop 2 s)])
                    (assert-equal '(3 4 5) (seq->list result))))
            
            (define-test seq-ref-test
              (let ([s (list->seq '(a b c d e))])
                   (assert-equal 'a (from-just (seq-ref s 0)))
                   (assert-equal 'c (from-just (seq-ref s 2)))
                   (assert-equal 'e (from-just (seq-ref s 4)))
                   (assert-true (nothing? (seq-ref s 10))))))

;;; ============================================================
;;; Utility Operations Tests
;;; ============================================================

(test-group utilities
            (define-test seq-replicate-test
              (let ([s (seq-replicate 5 'x)])
                   (assert-equal '(x x x x x) (seq->list s))))
            
            (define-test seq-range-test
              (let ([s (seq-range 1 5)])
                   (assert-equal '(1 2 3 4) (seq->list s))))
            
            (define-test seq-intersperse-test
              (let* ([s (list->seq '(1 2 3))]
                     [result (seq-intersperse 0 s)])
                    (assert-equal '(1 0 2 0 3) (seq->list result))))
            
            (define-test seq-intersperse-single-test
              (let* ([s (seq-singleton 1)]
                     [result (seq-intersperse 0 s)])
                    (assert-equal '(1) (seq->list result))))
            
            (define-test ft-equal-test
              (let ([ft1 (list->ft '(1 2 3))]
                    [ft2 (list->ft '(1 2 3))]
                    [ft3 (list->ft '(1 2))])
                   (assert-true (ft-equal? ft1 ft2))
                   (assert-false (ft-equal? ft1 ft3)))))

;;; Helper: iota-list (defined before tests that use it)
(define (iota-list n)
  (let loop ([i 0] [acc '()])
       (if (>= i n)
           (reverse acc)
           (loop (+ i 1) (cons i acc)))))

;;; ============================================================
;;; Large Sequence Tests
;;; ============================================================

(test-group large-sequences
            (define-test large-append-test
              (let* ([s1 (list->seq (iota-list 50))]
                     [s2 (list->seq (iota-list 50))]
                     [result (seq-append s1 s2)])
                    (assert-equal 100 (seq-length result))))
            
            (define-test many-cons-test
              (let loop ([i 0] [s (seq-empty)])
                   (if (>= i 100)
                       (assert-equal 100 (seq-length s))
                       (loop (+ i 1) (seq-cons i s)))))
            
            (define-test many-snoc-test
              (let loop ([i 0] [s (seq-empty)])
                   (if (>= i 100)
                       (assert-equal 100 (seq-length s))
                       (loop (+ i 1) (seq-snoc s i))))))

;;; ============================================================
;;; Monoid Tests
;;; ============================================================

(test-group monoid
            (define-test mempty-left-identity-test
              (let* ([ft (list->ft '(1 2 3))]
                     [result (ft-mappend ft-mempty ft)])
                    (assert-true (ft-equal? ft result))))
            
            (define-test mempty-right-identity-test
              (let* ([ft (list->ft '(1 2 3))]
                     [result (ft-mappend ft ft-mempty)])
                    (assert-true (ft-equal? ft result))))
            
            (define-test mappend-associative-test
              (let* ([a (list->ft '(1))]
                     [b (list->ft '(2))]
                     [c (list->ft '(3))]
                     [left (ft-mappend (ft-mappend a b) c)]
                     [right (ft-mappend a (ft-mappend b c))])
                    (assert-true (ft-equal? left right)))))

;;; ============================================================
;;; Display Tests
;;; ============================================================

(test-group display-tests
            (define-test ft-to-string-test
              (let* ([ft (list->ft '(1 2 3))]
                     [s (ft->string ft)])
                    (assert-true (string? s))
                    (assert-true (> (string-length s) 0))))
            
            (define-test empty-ft-to-string-test
              (let ([s (ft->string ft-empty)])
                   (assert-true (string? s)))))

;;; ============================================================
;;; Edge Cases
;;; ============================================================

(test-group edge-cases
            (define-test pop-empty-test
              (assert-false (deque-pop-front (deque-empty)))
              (assert-false (deque-pop-back (deque-empty))))
            
            (define-test tail-empty-test
              (assert-true (ft-empty? (ft-tail-left ft-empty)))
              (assert-true (ft-empty? (ft-tail-right ft-empty))))
            
            (define-test cons-to-single-test
              (let* ([ft (make-ft-single 2)]
                     [ft (ft-cons-left ft 1)])
                    (assert-equal '(1 2) (ft->list ft))))
            
            (define-test snoc-to-single-test
              (let* ([ft (make-ft-single 1)]
                     [ft (ft-snoc-right ft 2)])
                    (assert-equal '(1 2) (ft->list ft)))))

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
[SUCCESS] All finger tree tests passed.
")
    (display "
[FAILURE] Some finger tree tests failed.
"))
