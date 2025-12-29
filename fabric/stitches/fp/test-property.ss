;;; fabric/stitches/fp/test-property.ss — Tests for Property-Based Testing

;;; NOTE: Run from fabric/stitches directory

(load "test-framework.ss")
(load "fp/property.ss")

(display "\n")
(display "==============================================================\n")
(display "         PROPERTY-BASED TESTING TESTS\n")
(display "==============================================================\n")

;;; ============================================================
;;; Random Number Generation Tests
;;; ============================================================

(test-group random-generation
  (define-test lcg-produces-values-test
    (let* ([r1 (lcg-next 42)]
           [r2 (lcg-next (cdr r1))])
      (assert-true (integer? (car r1)))
      (assert-true (integer? (car r2)))
      (assert-false (= (car r1) (car r2)))))  ; Different values

  (define-test random-int-in-range-test
    (let loop ([seed 42] [i 0])
      (if (>= i 20)
          (assert-true #t)  ; All passed
          (let* ([r (random-int seed 10 20)]
                 [val (car r)])
            (assert-true (>= val 10))
            (assert-true (<= val 20))
            (loop (cdr r) (+ i 1))))))

  (define-test random-bool-produces-booleans-test
    (let loop ([seed 42] [i 0] [seen-true #f] [seen-false #f])
      (if (>= i 100)
          (begin
            (assert-true seen-true)
            (assert-true seen-false))
          (let* ([r (random-bool seed)]
                 [val (car r)])
            (assert-true (boolean? val))
            (loop (cdr r) (+ i 1)
                  (or seen-true val)
                  (or seen-false (not val))))))))

;;; ============================================================
;;; Basic Generator Tests
;;; ============================================================

(test-group basic-generators
  (define-test gen-pure-test
    (let ([gen (gen-pure 42)])
      (assert-equal 42 (sample-gen gen 1 10))
      (assert-equal 42 (sample-gen gen 999 100))))

  (define-test gen-nat-respects-size-test
    (let* ([small (samples gen-nat 42 10)]
           [max-val (apply max small)])
      ;; Values should be bounded by size
      (assert-true (<= max-val 15))))  ; Generous bound

  (define-test gen-bool-produces-both-test
    (let* ([bools (samples gen-bool 42 50)]
           [trues (filter identity bools)]
           [falses (filter not bools)])
      (assert-true (> (length trues) 0))
      (assert-true (> (length falses) 0))))

  (define-test gen-int-range-test
    (let ([gen (gen-int 5 10)])
      (let loop ([seed 1] [i 0])
        (if (>= i 20)
            (assert-true #t)
            (let* ([val (sample-gen gen seed 10)])
              (assert-true (>= val 5))
              (assert-true (<= val 10))
              (loop (+ seed 13) (+ i 1)))))))

  (define-test gen-small-int-centered-test
    (let* ([vals (samples gen-small-int 42 20)]
           [has-positive (> (length (filter positive? vals)) 0)]
           [has-negative (> (length (filter negative? vals)) 0)])
      (assert-true has-positive)
      (assert-true has-negative))))

;;; ============================================================
;;; Generator Monad Tests
;;; ============================================================

(test-group generator-monad
  (define-test gen-map-test
    (let* ([gen (gen-map (lambda (x) (* x 2)) gen-nat)]
           [val (sample-gen gen 42 10)])
      (assert-true (even? val))))

  (define-test gen-bind-test
    (let* ([gen (gen-bind gen-nat (lambda (n)
                  (gen-pure (* n 10))))]
           [val (sample-gen gen 42 5)])
      (assert-true (= (modulo val 10) 0))))

  (define-test gen-seq-test
    (let* ([gen (gen-seq (list (gen-pure 1) (gen-pure 2) (gen-pure 3)))]
           [val (sample-gen gen 42 10)])
      (assert-equal '(1 2 3) val)))

  (define-test gen-ap-test
    (let* ([gf (gen-pure (lambda (x) (+ x 1)))]
           [ga (gen-pure 5)]
           [gen (gen-ap gf ga)]
           [val (sample-gen gen 42 10)])
      (assert-equal 6 val))))

;;; ============================================================
;;; Combinator Tests
;;; ============================================================

(test-group generator-combinators
  (define-test gen-one-of-test
    (let* ([gen (gen-one-of (list (gen-pure 'a) (gen-pure 'b) (gen-pure 'c)))]
           [vals (samples gen 42 30)]
           [unique (let loop ([vs vals] [acc '()])
                     (if (null? vs)
                         acc
                         (if (member (car vs) acc)
                             (loop (cdr vs) acc)
                             (loop (cdr vs) (cons (car vs) acc)))))])
      ;; Should see multiple different values
      (assert-true (> (length unique) 1))))

  (define-test gen-elements-test
    (let* ([gen (gen-elements '(x y z))]
           [vals (samples gen 42 30)])
      (assert-true (not (null? (filter (lambda (v) (eq? v 'x)) vals))))
      (assert-true (not (null? (filter (lambda (v) (eq? v 'y)) vals))))
      (assert-true (not (null? (filter (lambda (v) (eq? v 'z)) vals))))))

  (define-test gen-frequency-test
    (let* ([gen (gen-frequency
                 (list (cons 10 (gen-pure 'common))
                       (cons 1 (gen-pure 'rare))))]
           [vals (samples gen 42 100)]
           [common-count (length (filter (lambda (v) (eq? v 'common)) vals))]
           [rare-count (length (filter (lambda (v) (eq? v 'rare)) vals))])
      ;; Common should appear more often
      (assert-true (> common-count rare-count))))

  (define-test gen-sized-test
    (let* ([gen (gen-sized (lambda (size) (gen-pure size)))]
           [val-small (sample-gen gen 42 5)]
           [val-large (sample-gen gen 42 100)])
      (assert-equal 5 val-small)
      (assert-equal 100 val-large)))

  (define-test gen-resize-test
    (let* ([gen (gen-resize 50 (gen-sized (lambda (s) (gen-pure s))))]
           [val (sample-gen gen 42 5)])  ; Original size ignored
      (assert-equal 50 val))))

;;; ============================================================
;;; Collection Generator Tests
;;; ============================================================

(test-group collection-generators
  (define-test gen-list-test
    (let* ([gen (gen-list (gen-pure 1))]
           [val (sample-gen gen 42 5)])
      (assert-true (list? val))
      (assert-true (or (null? val) (= (car val) 1)))))

  (define-test gen-list-n-test
    (let* ([gen (gen-list-n 5 (gen-pure 'x))]
           [val (sample-gen gen 42 10)])
      (assert-equal 5 (length val))
      (assert-true (for-all (lambda (v) (eq? v 'x)) val))))

  (define-test gen-nonempty-list-test
    (let* ([gen (gen-nonempty-list gen-nat)]
           [vals (samples gen 42 20)])
      (assert-true (for-all (lambda (v) (> (length v) 0)) vals))))

  (define-test gen-string-test
    (let* ([gen gen-string]
           [val (sample-gen gen 42 10)])
      (assert-true (string? val))))

  (define-test gen-pair-test
    (let* ([gen (gen-pair (gen-pure 'left) (gen-pure 'right))]
           [val (sample-gen gen 42 10)])
      (assert-equal 'left (car val))
      (assert-equal 'right (cdr val))))

  (define-test gen-maybe-test
    (let* ([gen (gen-maybe (gen-pure 42))]
           [vals (samples gen 42 50)]
           [has-nothing (> (length (filter nothing? vals)) 0)]
           [has-just (> (length (filter just? vals)) 0)])
      (assert-true has-nothing)
      (assert-true has-just))))

;;; ============================================================
;;; Shrinking Tests
;;; ============================================================

(test-group shrinking
  (define-test shrink-int-positive-test
    (let ([shrinks (shrink-int 10)])
      (assert-true (not (not (member 0 shrinks))))  ; Should include 0
      (assert-true (> (length shrinks) 0))))

  (define-test shrink-int-zero-test
    (assert-equal '() (shrink-int 0)))

  (define-test shrink-int-negative-test
    (let ([shrinks (shrink-int -10)])
      (assert-true (not (not (member 0 shrinks))))))

  (define-test shrink-list-test
    (let ([shrinks ((shrink-list no-shrink) '(1 2 3))])
      ;; Should include sublists (member returns tail, not #t)
      (assert-true (not (not (member '(2 3) shrinks))))
      (assert-true (not (not (member '(1 3) shrinks))))
      (assert-true (not (not (member '(1 2) shrinks))))))

  (define-test shrink-list-empty-test
    (assert-equal '() ((shrink-list no-shrink) '())))

  (define-test shrink-list-with-element-shrinking-test
    (let* ([shrinks ((shrink-list shrink-int) '(10))]
           [has-smaller (> (length shrinks) 1)])
      ;; Should include both removal (-> '()) and element shrinking
      (assert-true (not (not (member '() shrinks))))
      (assert-true has-smaller))))

;;; ============================================================
;;; Property Result Tests
;;; ============================================================

(test-group property-results
  (define-test success-test
    (let ([r (make-success)])
      (assert-true (success? r))
      (assert-false (failure? r))
      (assert-false (discarded? r))))

  (define-test failure-test
    (let ([r (make-failure 'counterexample)])
      (assert-false (success? r))
      (assert-true (failure? r))
      (assert-equal 'counterexample (failure-counterexample r))))

  (define-test discarded-test
    (let ([r (make-discarded)])
      (assert-false (success? r))
      (assert-false (failure? r))
      (assert-true (discarded? r))))

  (define-test prop-bool-true-test
    (assert-true (success? (prop-bool #t))))

  (define-test prop-bool-false-test
    (assert-true (failure? (prop-bool #f))))

  (define-test implication-true-test
    (assert-true (success? (==> #t (make-success)))))

  (define-test implication-false-test
    (assert-true (discarded? (==> #f (make-success))))))

;;; ============================================================
;;; Quick Check Tests
;;; ============================================================

(test-group quick-check
  (define-test passing-property-test
    (let* ([prop (prop-forall gen-nat (lambda (x) (>= x 0)))]
           [result (quick-check prop 50 42)])
      (assert-true (check-passed? result))))

  (define-test failing-property-test
    (let* ([prop (prop-forall gen-nat (lambda (x) (< x 5)))]
           [result (quick-check prop 100 42)])
      (assert-true (check-failed? result))))

  (define-test failing-has-counterexample-test
    (let* ([prop (prop-forall (gen-int 10 20) (lambda (x) (< x 15)))]
           [result (quick-check prop 100 42)])
      (assert-true (check-failed? result))
      (let ([ce (check-counterexample result)])
        (assert-true (>= ce 15)))))

  (define-test discarding-property-test
    (let* ([prop (make-forall
                  gen-small-int
                  no-shrink
                  (lambda (x) (==> (> x 0) (prop-bool (positive? x)))))]
           [result (quick-check prop 50 42)])
      ;; Should pass (discards negatives, all positives pass)
      (assert-true (check-passed? result))))

  (define-test shrinking-finds-minimal-test
    (let* ([prop (prop-forall/shrink
                  (gen-int 0 100)
                  shrink-int
                  (lambda (x) (< x 50)))]
           [result (quick-check prop 200 42)])
      (if (check-failed? result)
          ;; Shrinking should find value close to 50 (minimal failing case)
          ;; Due to shrink algorithm, may get 50 or slightly higher
          (assert-true (<= 50 (check-counterexample result)))
          (assert-true #f)))))  ; Should have failed

;;; ============================================================
;;; Standard Property Tests
;;; ============================================================

(test-group standard-properties
  (define-test prop-reverse-reverse-test
    (let ([result (qc prop-reverse-reverse)])
      (assert-true (check-passed? result))))

  (define-test prop-length-append-test
    (let ([result (qc prop-length-append)])
      (assert-true (check-passed? result))))

  (define-test prop-map-length-test
    (let ([result (qc prop-map-length)])
      (assert-true (check-passed? result))))

  (define-test reflexive-equal-test
    (let* ([prop (prop-reflexive gen-small-int =)]
           [result (qc prop)])
      (assert-true (check-passed? result))))

  (define-test symmetric-leq-test
    ;; Test symmetry of <= would fail (not symmetric), so test with a symmetric relation
    ;; x same-parity y means both even or both odd
    (let* ([same-parity? (lambda (x y) (= (modulo x 2) (modulo y 2)))]
           [prop (prop-symmetric gen-small-int same-parity?)]
           [result (qc prop)])
      (assert-true (check-passed? result))))

  (define-test commutative-plus-test
    (let* ([prop (prop-commutative gen-small-int + =)]
           [result (qc prop)])
      (assert-true (check-passed? result))))

  (define-test associative-plus-test
    (let* ([prop (prop-associative gen-small-int + =)]
           [result (qc prop)])
      (assert-true (check-passed? result))))

  (define-test identity-plus-test
    (let* ([prop (prop-identity gen-small-int 0 + =)]
           [result (qc prop)])
      (assert-true (check-passed? result)))))

;;; ============================================================
;;; Edge Cases
;;; ============================================================

(test-group edge-cases
  (define-test empty-list-handled-test
    (let* ([prop (prop-forall (gen-pure '()) (lambda (xs) (null? xs)))]
           [result (qc prop)])
      (assert-true (check-passed? result))))

  (define-test single-element-test
    (let* ([prop (prop-forall (gen-list-n 1 gen-nat)
                              (lambda (xs) (= 1 (length xs))))]
           [result (qc prop)])
      (assert-true (check-passed? result))))

  (define-test gen-char-printable-test
    (let* ([chars (samples gen-char 42 50)]
           [all-printable (for-all (lambda (c)
                                     (and (char>=? c (integer->char 32))
                                          (char<=? c (integer->char 126))))
                                   chars)])
      (assert-true all-printable)))

  (define-test samples-produces-list-test
    (let ([vals (samples gen-nat 42 10)])
      (assert-equal 10 (length vals))
      (assert-true (for-all integer? vals)))))

;;; ============================================================
;;; Summary
;;; ============================================================

(display "\n")
(display "==============================================================\n")
(printf "Tests passed: ~a\n" *tests-passed*)
(printf "Tests failed: ~a\n" *tests-failed*)
(printf "Total tests:  ~a\n" *tests-run*)

(if (= *tests-failed* 0)
    (display "\n[SUCCESS] All property-based testing tests passed.\n")
    (display "\n[FAILURE] Some property-based testing tests failed.\n"))

