;;; fabric/stitches/fp/test-quickcheck.ss --- Tests for QuickCheck Library
;;;
;;; Tests demonstrating:
;;; - Basic generators
;;; - Generator combinators
;;; - Shrinking behavior
;;; - Property-based testing with forall
;;; - Minimal counterexamples

;;; NOTE: Run from fabric/stitches directory

(load "core/test-framework.ss")
(load "core/fp/testing/quickcheck.ss")

(display "
")
(display "==============================================================
")
(display "         QUICKCHECK LIBRARY TESTS
")
(display "==============================================================
")

;;; ============================================================
;;; Random Number Generator Tests
;;; ============================================================

(test-group rng
            (define-test rng-next-produces-different-values
              ;; Two consecutive calls should produce different values
              (let* ([r1 (rng-next 42)]
                     [r2 (rng-next (cdr r1))])
                    (assert-false (= (car r1) (car r2)))))
            
            (define-test rng-int-range-stays-in-bounds
              ;; Generate 20 values in [10, 20] and verify all are in range
              (let loop ([seed 42] [i 0])
                   (if (>= i 20)
                       (assert-true #t)
                       (let* ([r (rng-int-range seed 10 20)]
                              [val (car r)])
                             (assert-true (>= val 10))
                             (assert-true (<= val 20))
                             (loop (cdr r) (+ i 1))))))
            
            (define-test rng-bool-produces-both
              ;; With enough iterations, should see both true and false
              (let loop ([seed 42] [i 0] [seen-true #f] [seen-false #f])
                   (if (>= i 100)
                       (begin
                        (assert-true seen-true)
                        (assert-true seen-false))
                       (let* ([r (rng-bool seed)]
                              [val (car r)])
                             (loop (cdr r) (+ i 1)
                                   (or seen-true val)
                                   (or seen-false (not val)))))))
            
            (define-test rng-double-in-unit-interval
              ;; All values should be in [0, 1)
              (let loop ([seed 123] [i 0])
                   (if (>= i 20)
                       (assert-true #t)
                       (let* ([r (rng-double seed)]
                              [val (car r)])
                             (assert-true (>= val 0))
                             (assert-true (< val 1))
                             (loop (cdr r) (+ i 1)))))))

;;; ============================================================
;;; Generator Monad Tests
;;; ============================================================

(test-group gen-monad
            (define-test gen-pure-returns-value
              ;; gen-pure should always return the same value
              (let ([gen (gen-pure 42)])
                   (assert-equal 42 (gen-sample gen 10 123))
                   (assert-equal 42 (gen-sample gen 50 999))))
            
            (define-test gen-map-applies-function
              ;; gen-map should transform generated values
              (let* ([gen (gen-map (lambda (x) (* x 2)) gen-nat)]
                     [val (gen-sample gen 10 42)])
                    (assert-true (even? val))))
            
            (define-test gen-bind-sequences-generators
              ;; gen-bind should thread through generators
              (let* ([gen (gen-bind gen-nat (lambda (n)
                                                    (gen-pure (* n 10))))]
                     [val (gen-sample gen 5 42)])
                    (assert-true (= (modulo val 10) 0))))
            
            (define-test gen-sequence-collects-list
              ;; gen-sequence should collect generator results
              (let* ([gen (gen-sequence (list (gen-pure 1) (gen-pure 2) (gen-pure 3)))]
                     [val (gen-sample gen 10 42)])
                    (assert-equal '(1 2 3) val)))
            
            (define-test gen-ap-applies-function
              ;; gen-ap should apply generated function to generated argument
              (let* ([gf (gen-pure (lambda (x) (+ x 1)))]
                     [ga (gen-pure 5)]
                     [gen (gen-ap gf ga)]
                     [val (gen-sample gen 10 42)])
                    (assert-equal 6 val))))

;;; ============================================================
;;; Basic Generator Tests
;;; ============================================================

(test-group basic-generators
            (define-test gen-int-respects-size
              ;; Values should be bounded by size
              (let* ([vals (sample-n gen-int 20)]
                     [max-abs (apply max (map abs vals))])
                    ;; With size growing to 20, max should be reasonable
                    (assert-true (<= max-abs 100))))
            
            (define-test gen-nat-is-non-negative
              ;; All generated naturals should be >= 0
              (let ([vals (sample-n gen-nat 50)])
                   (assert-true (andmap (lambda (x) (>= x 0)) vals))))
            
            (define-test gen-pos-is-positive
              ;; All generated positives should be > 0
              (let ([vals (sample-n gen-pos 50)])
                   (assert-true (andmap (lambda (x) (> x 0)) vals))))
            
            (define-test gen-bool-produces-both
              ;; Should see both true and false
              (let* ([vals (sample-n gen-bool 50)]
                     [has-true (ormap identity vals)]
                     [has-false (ormap not vals)])
                    (assert-true has-true)
                    (assert-true has-false)))
            
            (define-test gen-char-is-printable
              ;; All characters should be in printable ASCII range
              (let ([vals (sample-n gen-char 50)])
                   (assert-true (andmap (lambda (c)
                                                (and (char>=? c (integer->char 32))
                                                     (char<=? c (integer->char 126))))
                                        vals))))
            
            (define-test gen-int-range-stays-in-range
              ;; gen-int-range should respect bounds
              (let* ([gen (gen-int-range 5 10)]
                     [vals (sample-n gen 30)])
                    (assert-true (andmap (lambda (x) (and (>= x 5) (<= x 10))) vals)))))

;;; ============================================================
;;; Generator Combinator Tests
;;; ============================================================

(test-group gen-combinators
            (define-test gen-one-of-picks-from-list
              ;; Should pick from the provided generators
              (let* ([gen (gen-one-of (list (gen-pure 'a) (gen-pure 'b) (gen-pure 'c)))]
                     [vals (sample-n gen 30)]
                     [unique (let loop ([vs vals] [acc '()])
                                  (if (null? vs)
                                      acc
                                      (if (member (car vs) acc)
                                          (loop (cdr vs) acc)
                                          (loop (cdr vs) (cons (car vs) acc)))))])
                    ;; Should see at least 2 different values
                    (assert-true (> (length unique) 1))))
            
            (define-test gen-elements-picks-values
              ;; Should pick from the provided list
              (let* ([gen (gen-elements '(x y z))]
                     [vals (sample-n gen 50)])
                    (assert-true (ormap (lambda (v) (eq? v 'x)) vals))
                    (assert-true (ormap (lambda (v) (eq? v 'y)) vals))
                    (assert-true (ormap (lambda (v) (eq? v 'z)) vals))))
            
            (define-test gen-frequency-respects-weights
              ;; Higher weights should produce more frequent values
              (let* ([gen (gen-frequency
                           (list (cons 10 (gen-pure 'common))
                                 (cons 1 (gen-pure 'rare))))]
                     [vals (sample-n gen 100)]
                     [common-count (length (filter (lambda (v) (eq? v 'common)) vals))]
                     [rare-count (length (filter (lambda (v) (eq? v 'rare)) vals))])
                    ;; Common should appear more often than rare
                    (assert-true (> common-count rare-count))))
            
            (define-test gen-such-that-filters
              ;; Should only produce values satisfying predicate
              (let* ([gen (gen-such-that even? gen-nat)]
                     [vals (sample-n gen 20)])
                    (assert-true (andmap even? vals))))
            
            (define-test gen-sized-accesses-size
              ;; Should be able to access size parameter
              (let* ([gen (gen-sized (lambda (size) (gen-pure size)))]
                     [val-5 (gen-sample gen 5 42)]
                     [val-20 (gen-sample gen 20 42)])
                    (assert-equal 5 val-5)
                    (assert-equal 20 val-20)))
            
            (define-test gen-resize-overrides-size
              ;; gen-resize should override the size
              (let* ([gen (gen-resize 50 (gen-sized (lambda (s) (gen-pure s))))]
                     [val (gen-sample gen 5 42)])  ; Size 5 ignored
                    (assert-equal 50 val)))
            
            (define-test gen-scale-modifies-size
              ;; gen-scale should transform the size
              (let* ([gen (gen-scale (lambda (s) (* s 2))
                                     (gen-sized (lambda (s) (gen-pure s))))]
                     [val (gen-sample gen 10 42)])
                    (assert-equal 20 val))))

;;; ============================================================
;;; Collection Generator Tests
;;; ============================================================

(test-group collection-generators
            (define-test gen-list-produces-lists
              ;; Should produce valid lists
              (let* ([gen (gen-list gen-nat)]
                     [vals (sample-n gen 10)])
                    (assert-true (andmap list? vals))))
            
            (define-test gen-list-n-produces-exact-length
              ;; Should produce lists of exactly n elements
              (let* ([gen (gen-list-n 5 gen-nat)]
                     [vals (sample-n gen 10)])
                    (assert-true (andmap (lambda (xs) (= 5 (length xs))) vals))))
            
            (define-test gen-nonempty-list-is-nonempty
              ;; Should never produce empty lists
              (let* ([gen (gen-nonempty-list gen-nat)]
                     [vals (sample-n gen 20)])
                    (assert-true (andmap (lambda (xs) (> (length xs) 0)) vals))))
            
            (define-test gen-string-produces-strings
              ;; Should produce valid strings
              (let* ([vals (sample-n gen-string 10)])
                    (assert-true (andmap string? vals))))
            
            (define-test gen-pair-produces-pairs
              ;; Should produce cons cells
              (let* ([gen (gen-pair gen-nat gen-bool)]
                     [val (gen-sample gen 10 42)])
                    (assert-true (pair? val))
                    (assert-true (integer? (car val)))
                    (assert-true (boolean? (cdr val)))))
            
            (define-test gen-tuple-produces-2-lists
              ;; Should produce 2-element lists
              (let* ([gen (gen-tuple gen-nat gen-bool)]
                     [val (gen-sample gen 10 42)])
                    (assert-equal 2 (length val))
                    (assert-true (integer? (car val)))
                    (assert-true (boolean? (cadr val)))))
            
            (define-test gen-maybe-produces-both
              ;; Should produce both nothing and just
              ;; Use multiple starting seeds to ensure coverage
              (let* ([gen (gen-maybe gen-nat)]
                     [vals1 (sample-n gen 50)]
                     [vals2 (let loop ([i 0] [seed 3] [acc '()])  ; Seed 3 is known to produce #f
                                 (if (>= i 50)
                                     (reverse acc)
                                     (let* ([result (run-gen gen (+ 1 i) seed)]
                                            [val (car result)]
                                            [new-seed (cdr result)])
                                           (loop (+ i 1) new-seed (cons val acc)))))]
                     [vals (append vals1 vals2)]
                     [has-nothing (ormap (lambda (v) (eq? v #f)) vals)]
                     [has-just (ormap (lambda (v) (and (pair? v) (eq? (car v) 'just))) vals)])
                    (assert-true has-nothing)
                    (assert-true has-just))))

;;; ============================================================
;;; Shrinking Tests
;;; ============================================================

(test-group shrinking
            (define-test shrink-int-includes-zero
              ;; Shrinking positive should include 0
              (let ([shrinks (shrink-int 10)])
                   (assert-true (if (member 0 shrinks) #t #f))))
            
            (define-test shrink-int-zero-is-empty
              ;; 0 cannot shrink further
              (assert-equal '() (shrink-int 0)))
            
            (define-test shrink-int-produces-smaller
              ;; All shrinks should be smaller in absolute value
              (let* ([shrinks (shrink-int 100)]
                     [all-smaller (andmap (lambda (x) (< (abs x) 100)) shrinks)])
                    (assert-true all-smaller)))
            
            (define-test shrink-nat-non-negative
              ;; Shrunk naturals should stay non-negative
              (let ([shrinks (shrink-nat 50)])
                   (assert-true (andmap (lambda (x) (>= x 0)) shrinks))))
            
            (define-test shrink-list-removes-elements
              ;; Shrinking list should include sublists
              (let ([shrinks ((shrink-list no-shrink) '(1 2 3))])
                   (assert-true (if (member '(2 3) shrinks) #t #f))
                   (assert-true (if (member '(1 3) shrinks) #t #f))
                   (assert-true (if (member '(1 2) shrinks) #t #f))))
            
            (define-test shrink-list-empty-is-empty
              ;; Empty list cannot shrink
              (assert-equal '() ((shrink-list no-shrink) '())))
            
            (define-test shrink-list-shrinks-elements
              ;; Should also shrink individual elements
              (let ([shrinks ((shrink-list shrink-int) '(10))])
                   ;; Should include '() (removal) and lists with smaller elements
                   (assert-true (if (member '() shrinks) #t #f))
                   (assert-true (> (length shrinks) 1))))
            
            (define-test shrink-pair-shrinks-both
              ;; Should shrink both components
              (let ([shrinks ((shrink-pair shrink-int shrink-int) (cons 10 20))])
                   ;; Should include pairs with smaller first and smaller second
                   (assert-true (> (length shrinks) 0)))))

;;; ============================================================
;;; Property and forall Tests
;;; ============================================================

(test-group properties
            (define-test property-creation
              ;; forall should create a property
              (let ([prop (forall (x gen-int) (>= (* x x) 0))])
                   (assert-true (property? prop))))
            
            (define-test passing-property
              ;; Property that always holds
              (let* ([prop (forall (x gen-nat) (>= x 0))]
                     [result (check 50 prop)])
                    (assert-true (passed? result))))
            
            (define-test failing-property
              ;; Property that fails
              (let* ([prop (forall (x gen-nat) (< x 5))]
                     [result (check 100 prop)])
                    (assert-true (failed? result))))
            
            (define-test counterexample-exists
              ;; Failed property should have counterexample
              (let* ([prop (forall (x (gen-int-range 10 20)) (< x 15))]
                     [result (check 100 prop)])
                    (assert-true (failed? result))
                    (let ([ce (result-counterexample result)])
                         (assert-true (>= ce 15)))))
            
            (define-test multi-binding-property
              ;; forall with multiple bindings
              (let* ([prop (forall ((x gen-nat) (y gen-nat))
                                   (= (+ x y) (+ y x)))]
                     [result (check 50 prop)])
                    (assert-true (passed? result)))))

;;; ============================================================
;;; Shrinking Finds Minimal Counterexample Tests
;;; ============================================================

(test-group minimal-counterexamples
            (define-test shrinking-finds-minimal-int
              ;; Shrinking should find value close to boundary
              (let* ([prop (forall/shrink (x (gen-int-range 0 100) shrink-int)
                                          (< x 50))]
                     [result (check 200 prop)])
                    (assert-true (failed? result))
                    ;; Counterexample should be around 50 (minimal failing case)
                    (let ([ce (result-counterexample result)])
                         (assert-true (>= ce 50))
                         (assert-true (<= ce 55)))))  ; Allow some slack
            
            (define-test shrinking-finds-minimal-list
              ;; Should find minimal list that fails
              (let* ([prop (forall/shrink (xs gen-list-nat (shrink-list shrink-nat))
                                          (< (length xs) 3))]
                     [result (check 100 prop)])
                    (assert-true (failed? result))
                    (let ([ce (result-counterexample result)])
                         ;; Counterexample should be a list of length 3 with small elements
                         (assert-equal 3 (length ce)))))
            
            (define-test shrinking-with-pair
              ;; Should shrink pairs properly
              (let* ([prop (forall/shrink ((x gen-nat shrink-nat) (y gen-nat shrink-nat))
                                          (< (+ x y) 10))]
                     [result (check 100 prop)])
                    (assert-true (failed? result))
                    (let* ([ce (result-counterexample result)]
                           [x (car ce)]
                           [y (cadr ce)])
                          ;; Sum should be >= 10, but hopefully minimal
                          (assert-true (>= (+ x y) 10))))))

;;; ============================================================
;;; Implication Tests
;;; ============================================================

(test-group implication
            (define-test implication-true-passes
              ;; When precondition holds, conclusion must hold
              (let* ([prop (forall (x gen-nat)
                                   (==> (even? x) (even? (* x 2))))]
                     [result (check 50 prop)])
                    (assert-true (passed? result))))
            
            (define-test implication-false-discards
              ;; When precondition fails, test is discarded
              (let* ([prop (forall (x gen-nat)
                                   (==> (> x 1000000) #f))]  ; Rarely true
                     [result (check 100 prop)])
                    ;; Should pass (all failing preconditions are discarded)
                    (assert-true (or (passed? result) (gave-up? result))))))

;;; ============================================================
;;; Standard Properties Tests
;;; ============================================================

(test-group standard-properties
            (define-test prop-reverse-reverse-holds
              (let ([result (qc prop-reverse-reverse)])
                   (assert-true (passed? result))))
            
            (define-test prop-length-append-holds
              (let ([result (qc prop-length-append)])
                   (assert-true (passed? result))))
            
            (define-test prop-map-length-holds
              (let ([result (qc prop-map-length)])
                   (assert-true (passed? result))))
            
            (define-test prop-filter-length-holds
              (let ([result (qc prop-filter-length)])
                   (assert-true (passed? result))))
            
            (define-test prop-append-assoc-holds
              (let ([result (qc prop-append-assoc)])
                   (assert-true (passed? result))))
            
            (define-test reflexive-equal-holds
              (let* ([prop (prop-reflexive gen-int =)]
                     [result (qc prop)])
                    (assert-true (passed? result))))
            
            (define-test symmetric-less-equal-holds
              ;; Test with <= which is NOT symmetric (will give up or find counterexample)
              ;; We use a symmetric relation: same-parity
              (let* ([same-parity? (lambda (x y) (= (modulo (abs x) 2) (modulo (abs y) 2)))]
                     [prop (prop-symmetric gen-int same-parity?)]
                     [result (qc prop)])
                    (assert-true (passed? result))))
            
            (define-test commutative-plus-holds
              (let* ([prop (prop-commutative gen-int +)]
                     [result (qc prop)])
                    (assert-true (passed? result))))
            
            (define-test associative-plus-holds
              (let* ([prop (prop-associative gen-int +)]
                     [result (qc prop)])
                    (assert-true (passed? result))))
            
            (define-test identity-plus-holds
              (let* ([prop (prop-identity gen-int 0 +)]
                     [result (qc prop)])
                    (assert-true (passed? result)))))

;;; ============================================================
;;; Edge Cases and Special Scenarios
;;; ============================================================

(test-group edge-cases
            (define-test empty-list-handled
              ;; Should handle empty lists gracefully
              (let* ([prop (forall (xs (gen-pure '())) (null? xs))]
                     [result (qc prop)])
                    (assert-true (passed? result))))
            
            (define-test singleton-list
              ;; Single element list properties
              (let* ([prop (forall (xs (gen-list-n 1 gen-nat))
                                   (= 1 (length xs)))]
                     [result (qc prop)])
                    (assert-true (passed? result))))
            
            (define-test gen-char-alnum-is-alphanumeric
              ;; gen-char-alnum should produce alphanumeric chars
              (let* ([vals (sample-n gen-char-alnum 50)])
                    (assert-true (andmap (lambda (c)
                                                 (or (char-alphabetic? c)
                                                     (char-numeric? c)))
                                         vals))))
            
            (define-test sample-returns-list
              ;; sample should return a list of values
              (let ([vals (sample gen-nat)])
                   (assert-equal 10 (length vals))
                   (assert-true (andmap integer? vals))))
            
            (define-test sample-n-respects-count
              ;; sample-n should return exactly n values
              (let ([vals (sample-n gen-nat 7)])
                   (assert-equal 7 (length vals)))))

;;; ============================================================
;;; More Complex Properties
;;; ============================================================

(test-group complex-properties
            (define-test reverse-preserves-length
              (let* ([prop (forall/shrink (xs gen-list-int (shrink-list shrink-int))
                                          (= (length xs) (length (reverse xs))))]
                     [result (qc prop)])
                    (assert-true (passed? result))))
            
            (define-test append-nil-identity
              (let* ([prop (forall/shrink (xs gen-list-int (shrink-list shrink-int))
                                          (equal? (append xs '()) xs))]
                     [result (qc prop)])
                    (assert-true (passed? result))))
            
            (define-test map-compose
              ;; map f . map g = map (f . g)
              (let* ([f (lambda (x) (* x 2))]
                     [g (lambda (x) (+ x 1))]
                     [prop (forall/shrink (xs gen-list-int (shrink-list shrink-int))
                                          (equal? (map f (map g xs))
                                                  (map (lambda (x) (f (g x))) xs)))]
                     [result (qc prop)])
                    (assert-true (passed? result))))
            
            (define-test filter-idempotent
              ;; filter p . filter p = filter p
              (let* ([prop (forall/shrink (xs gen-list-int (shrink-list shrink-int))
                                          (equal? (filter even? (filter even? xs))
                                                  (filter even? xs)))]
                     [result (qc prop)])
                    (assert-true (passed? result)))))

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
[SUCCESS] All QuickCheck library tests passed.
")
    (display "
[FAILURE] Some QuickCheck library tests failed.
"))
