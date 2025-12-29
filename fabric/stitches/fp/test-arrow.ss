;;; fabric/stitches/fp/test-arrow.ss — Tests for Arrow Abstraction

;;; NOTE: Run from fabric/stitches directory

(load "test-framework.ss")
(load "fp/arrow.ss")

(display "\n")
(display "==============================================================\n")
(display "         ARROW TESTS\n")
(display "==============================================================\n")

;;; ============================================================
;;; Core Constructor Tests
;;; ============================================================

(test-group arrow-constructors
  (define-test fn-arrow-test
    (let ([f (fn-arrow add1)])
      (assert-true (arrow? f))
      (assert-equal 'function (arrow-type f))))

  (define-test arr-test
    (let ([f (arr add1)])
      (assert-true (arrow? f))
      (assert-equal 6 (run-arrow f 5))))

  (define-test arr-id-test
    (assert-equal 42 (run-arrow arr-id 42))
    (assert-equal "hello" (run-arrow arr-id "hello"))))

;;; ============================================================
;;; Arrow Composition Tests
;;; ============================================================

(test-group arrow-composition
  (define-test compose-test
    (let* ([double (arr (lambda (x) (* x 2)))]
           [add1-arr (arr add1)]
           [composed (arrow-compose double add1-arr)])
      (assert-equal 11 (run-arrow composed 5))))

  (define-test >>>-test
    (let* ([double (arr (lambda (x) (* x 2)))]
           [add1-arr (arr add1)]
           [pipeline (>>> double add1-arr)])
      (assert-equal 11 (run-arrow pipeline 5))))

  (define-test <<<-test
    (let* ([double (arr (lambda (x) (* x 2)))]
           [add1-arr (arr add1)]
           [pipeline (<<< add1-arr double)])
      (assert-equal 11 (run-arrow pipeline 5))))

  (define-test chain-test
    (let* ([a1 (arr add1)]
           [a2 (arr (lambda (x) (* x 2)))]
           [a3 (arr (lambda (x) (- x 3)))]
           [chain (>>> a1 (>>> a2 a3))])
      (assert-equal 9 (run-arrow chain 5)))))

;;; ============================================================
;;; Arrow Laws Tests
;;; ============================================================

(test-group arrow-laws
  ;; Identity laws: arr id >>> f = f = f >>> arr id
  (define-test left-identity-test
    (let ([f (arr add1)])
      (assert-equal (run-arrow (>>> arr-id f) 5)
                    (run-arrow f 5))))

  (define-test right-identity-test
    (let ([f (arr add1)])
      (assert-equal (run-arrow (>>> f arr-id) 5)
                    (run-arrow f 5))))

  ;; Associativity: (f >>> g) >>> h = f >>> (g >>> h)
  (define-test associativity-test
    (let* ([f (arr add1)]
           [g (arr (lambda (x) (* x 2)))]
           [h (arr (lambda (x) (- x 3)))]
           [lhs (>>> (>>> f g) h)]
           [rhs (>>> f (>>> g h))])
      (assert-equal (run-arrow lhs 5)
                    (run-arrow rhs 5))))

  ;; Composition: arr (g . f) = arr f >>> arr g
  (define-test arr-composition-test
    (let* ([f add1]
           [g (lambda (x) (* x 2))]
           [composed (lambda (x) (g (f x)))]
           [lhs (arr composed)]
           [rhs (>>> (arr f) (arr g))])
      (assert-equal (run-arrow lhs 5)
                    (run-arrow rhs 5)))))

;;; ============================================================
;;; First and Second Tests
;;; ============================================================

(test-group first-second
  (define-test first-test
    (let* ([double (arr (lambda (x) (* x 2)))]
           [f (first double)])
      (assert-equal (cons 10 'unchanged)
                    (run-arrow f (cons 5 'unchanged)))))

  (define-test second-test
    (let* ([double (arr (lambda (x) (* x 2)))]
           [f (second double)])
      (assert-equal (cons 'unchanged 10)
                    (run-arrow f (cons 'unchanged 5)))))

  (define-test first-preserves-second-test
    (let* ([f (first (arr add1))]
           [pair (cons 5 "keep")])
      (assert-equal "keep" (cdr (run-arrow f pair)))))

  (define-test second-preserves-first-test
    (let* ([f (second (arr add1))]
           [pair (cons "keep" 5)])
      (assert-equal "keep" (car (run-arrow f pair))))))

;;; ============================================================
;;; Split and Fanout Tests
;;; ============================================================

(test-group split-fanout
  (define-test split-test
    (let* ([double (arr (lambda (x) (* x 2)))]
           [add10 (arr (lambda (x) (+ x 10)))]
           [f (*** double add10)])
      (assert-equal (cons 10 15)
                    (run-arrow f (cons 5 5)))))

  (define-test fanout-test
    (let* ([double (arr (lambda (x) (* x 2)))]
           [add10 (arr (lambda (x) (+ x 10)))]
           [f (&&& double add10)])
      (assert-equal (cons 10 15)
                    (run-arrow f 5))))

  (define-test split-independent-test
    (let* ([f (*** (arr add1) (arr (lambda (x) (* x 2))))])
      (assert-equal (cons 6 10)
                    (run-arrow f (cons 5 5)))))

  (define-test fanout-same-input-test
    (let* ([f (&&& arr-id (arr (lambda (x) (* x x))))])
      (assert-equal (cons 5 25)
                    (run-arrow f 5)))))

;;; ============================================================
;;; Utility Arrow Tests
;;; ============================================================

(test-group arrow-utilities
  (define-test arr-const-test
    (let ([f (arr-const 42)])
      (assert-equal 42 (run-arrow f 'anything))
      (assert-equal 42 (run-arrow f 100))))

  (define-test arr-dup-test
    (assert-equal (cons 5 5) (run-arrow arr-dup 5)))

  (define-test arr-swap-test
    (assert-equal (cons 'b 'a) (run-arrow arr-swap (cons 'a 'b))))

  (define-test arr-fst-test
    (assert-equal 'a (run-arrow arr-fst (cons 'a 'b))))

  (define-test arr-snd-test
    (assert-equal 'b (run-arrow arr-snd (cons 'a 'b))))

  (define-test arr-assoc-test
    (let ([nested (cons (cons 1 2) 3)])
      (assert-equal (cons 1 (cons 2 3))
                    (run-arrow arr-assoc nested))))

  (define-test arr-unassoc-test
    (let ([nested (cons 1 (cons 2 3))])
      (assert-equal (cons (cons 1 2) 3)
                    (run-arrow arr-unassoc nested)))))

;;; ============================================================
;;; ArrowChoice Tests
;;; ============================================================

(test-group arrow-choice
  (define-test arr-left-test
    (let* ([f (arr-left (arr add1))]
           [l (left 5)]
           [r (right "unchanged")])
      ;; Left values get transformed
      (let ([result-l (run-arrow f l)])
        (assert-true (left? result-l))
        (assert-equal 6 (from-left result-l)))
      ;; Right values pass through
      (let ([result-r (run-arrow f r)])
        (assert-true (right? result-r))
        (assert-equal "unchanged" (from-right result-r)))))

  (define-test arr-right-test
    (let* ([f (arr-right (arr add1))]
           [l (left "unchanged")]
           [r (right 5)])
      ;; Left values pass through
      (let ([result-l (run-arrow f l)])
        (assert-true (left? result-l))
        (assert-equal "unchanged" (from-left result-l)))
      ;; Right values get transformed
      (let ([result-r (run-arrow f r)])
        (assert-true (right? result-r))
        (assert-equal 6 (from-right result-r)))))

  (define-test choice-left-test
    (let* ([f (arrow-fanin (arr add1) (arr (lambda (x) (* x 2))))])
      (assert-equal 6 (run-arrow f (left 5)))))

  (define-test choice-right-test
    (let* ([f (arrow-fanin (arr add1) (arr (lambda (x) (* x 2))))])
      (assert-equal 10 (run-arrow f (right 5)))))

  (define-test choose-test
    (let* ([f (arrow-sum (arr add1) (arr (lambda (x) (* x 2))))]
           [l (left 5)]
           [r (right 5)])
      (let ([result-l (run-arrow f l)]
            [result-r (run-arrow f r)])
        (assert-true (left? result-l))
        (assert-equal 6 (from-left result-l))
        (assert-true (right? result-r))
        (assert-equal 10 (from-right result-r))))))

;;; ============================================================
;;; Conditional Arrow Tests
;;; ============================================================

(test-group conditional-arrows
  (define-test arr-if-true-test
    (let ([f (arr-if even?
                     (arr (lambda (x) (* x 2)))
                     (arr add1))])
      (assert-equal 8 (run-arrow f 4))))

  (define-test arr-if-false-test
    (let ([f (arr-if even?
                     (arr (lambda (x) (* x 2)))
                     (arr add1))])
      (assert-equal 6 (run-arrow f 5))))

  (define-test arr-case-first-match-test
    (let ([f (arr-case
              (list (cons zero? (arr (lambda (x) 'zero)))
                    (cons even? (arr (lambda (x) 'even))))
              (arr (lambda (x) 'odd)))])
      (assert-equal 'zero (run-arrow f 0))))

  (define-test arr-case-second-match-test
    (let ([f (arr-case
              (list (cons zero? (arr (lambda (x) 'zero)))
                    (cons even? (arr (lambda (x) 'even))))
              (arr (lambda (x) 'odd)))])
      (assert-equal 'even (run-arrow f 4))))

  (define-test arr-case-default-test
    (let ([f (arr-case
              (list (cons zero? (arr (lambda (x) 'zero)))
                    (cons even? (arr (lambda (x) 'even))))
              (arr (lambda (x) 'odd)))])
      (assert-equal 'odd (run-arrow f 5)))))

;;; ============================================================
;;; ArrowApply Tests
;;; ============================================================

(test-group arrow-apply
  (define-test arr-apply-test
    (let* ([f (arr add1)]
           [pair (cons f 5)])
      (assert-equal 6 (run-arrow arr-apply pair))))

  (define-test arr-apply-different-arrows-test
    (let* ([add (arr add1)]
           [double (arr (lambda (x) (* x 2)))]
           [pair1 (cons add 5)]
           [pair2 (cons double 5)])
      (assert-equal 6 (run-arrow arr-apply pair1))
      (assert-equal 10 (run-arrow arr-apply pair2)))))

;;; ============================================================
;;; Arrow Combinator Tests
;;; ============================================================

(test-group arrow-combinators
  (define-test arrow-map-test
    (let* ([f (arr add1)]
           [mapped (arrow-map (lambda (x) (* x 2)) f)])
      (assert-equal 12 (run-arrow mapped 5))))

  (define-test arrow-pre-test
    (let* ([f (arr add1)]
           [pre (arrow-pre (lambda (x) (* x 2)) f)])
      (assert-equal 11 (run-arrow pre 5))))

  (define-test arrow-sequence-empty-test
    (assert-equal 42 (run-arrow (arrow-sequence '()) 42)))

  (define-test arrow-sequence-test
    (let* ([a1 (arr add1)]
           [a2 (arr (lambda (x) (* x 2)))]
           [a3 (arr (lambda (x) (- x 3)))]
           [seq (arrow-sequence (list a1 a2 a3))])
      (assert-equal 9 (run-arrow seq 5))))

  (define-test arrow-pipe-test
    (let* ([a1 (arr add1)]
           [a2 (arr (lambda (x) (* x 2)))])
      (assert-equal 12 (arrow-pipe 5 (list a1 a2)))))

  (define-test arrow-fold-test
    ;; Sum a list using arrows
    (let* ([f (lambda (acc)
                (arr (lambda (x) (+ acc x))))])
      (assert-equal 15 (arrow-fold f 0 '(1 2 3 4 5))))))

;;; ============================================================
;;; Static Arrow Tests
;;; ============================================================

(test-group static-arrows
  (define-test make-static-test
    (let ([s (make-static 'info (arr add1))])
      (assert-true (static? s))
      (assert-equal 'info (static-info s))))

  (define-test run-static-test
    (let ([s (make-static 'info (arr add1))])
      (assert-equal 6 (run-static s 5))))

  (define-test static-compose-test
    (let* ([s1 (make-static '(a) (arr add1))]
           [s2 (make-static '(b) (arr (lambda (x) (* x 2))))]
           [composed (static-compose s1 s2 append)])
      (assert-equal '(a b) (static-info composed))
      (assert-equal 12 (run-static composed 5)))))

;;; ============================================================
;;; Tracing Arrow Tests
;;; ============================================================

(test-group tracing
  (define-test traced-single-test
    (let* ([f (traced "step1" (arr add1))]
           [result (run-arrow f 5)])
      (assert-equal 6 (car result))
      ;; trace is in cdr
      (assert-equal "step1" (cadr result))))

  ;; Note: traced arrows don't compose cleanly because the output format changes
  ;; This test verifies basic tracing works
  (define-test traced-values-test
    (let* ([f (traced "double" (arr (lambda (x) (* x 2))))]
           [result (run-arrow f 5)])
      (assert-equal 10 (car result))
      (assert-equal "double" (cadr result)))))

;;; ============================================================
;;; Practical Examples Tests
;;; ============================================================

(test-group practical-examples
  ;; Validation pipeline
  (define-test validation-pipeline-test
    (let* ([check-positive (arr-if positive?
                                   arr-id
                                   (arr (lambda (x) 0)))]
           [double (arr (lambda (x) (* x 2)))]
           [pipeline (>>> check-positive double)])
      (assert-equal 10 (run-arrow pipeline 5))
      (assert-equal 0 (run-arrow pipeline -5))))

  ;; Data transformation
  (define-test transform-pair-test
    (let* ([transform (*** (arr string-upcase)
                           (arr add1))])
      (assert-equal (cons "HELLO" 6)
                    (run-arrow transform (cons "hello" 5)))))

  ;; Extract and process
  (define-test extract-process-test
    (let* ([extract (arr car)]
           [process (arr (lambda (x) (* x x)))]
           [pipeline (>>> extract process)])
      (assert-equal 25 (run-arrow pipeline (cons 5 'ignored)))))

  ;; Either-based error handling
  (define-test error-handling-test
    (let* ([parse (arr (lambda (s)
                         (let ([n (string->number s)])
                           (if n (right n) (left "Not a number")))))]
           [handle-result (arrow-fanin (arr (lambda (e) (cons 'error e)))
                                       (arr (lambda (n) (cons 'ok n))))]
           [pipeline (>>> parse handle-result)])
      (let ([good (run-arrow pipeline "42")])
        (assert-equal 'ok (car good))
        (assert-equal 42 (cdr good)))
      (let ([bad (run-arrow pipeline "abc")])
        (assert-equal 'error (car bad))
        (assert-equal "Not a number" (cdr bad)))))

  ;; Parallel computation
  (define-test parallel-computation-test
    (let* ([compute (&&& (arr (lambda (x) (* x x)))
                         (arr (lambda (x) (+ x x))))]
           [combine (arr (lambda (p) (+ (car p) (cdr p))))]
           [pipeline (>>> compute combine)])
      ;; x^2 + 2x for x=5 = 25 + 10 = 35
      (assert-equal 35 (run-arrow pipeline 5)))))

;;; ============================================================
;;; ArrowLoop Tests (Fixed Point)
;;; ============================================================

(test-group arrow-loop
  (define-test simple-loop-test
    ;; A trivial loop that converges immediately
    (let* ([f (arr (lambda (p)
                     (let ([a (car p)]
                           [c (cdr p)])
                       (cons (+ a c) c))))]
           [looped (arrow-loop f 10)])
      (assert-equal 15 (run-arrow looped 5))))

  (define-test convergent-loop-test
    ;; Loop that converges when c reaches target
    (let* ([f (arr (lambda (p)
                     (let ([target (car p)]
                           [current (cdr p)])
                       (if (= current target)
                           (cons current current)  ; converged
                           (cons current (+ current 1))))))]
           [looped (arrow-loop f 0)])
      (assert-equal 5 (run-arrow looped 5)))))

;;; ============================================================
;;; Summary
;;; ============================================================

(display "\n")
(display "==============================================================\n")
(printf "Tests passed: ~a\n" *tests-passed*)
(printf "Tests failed: ~a\n" *tests-failed*)
(printf "Total tests:  ~a\n" *tests-run*)

(if (= *tests-failed* 0)
    (display "\n[SUCCESS] All arrow tests passed.\n")
    (display "\n[FAILURE] Some arrow tests failed.\n"))
