;;; fabric/stitches/fp/test-logic.ss — Tests for Logic Programming

;;; NOTE: Run from fabric/stitches directory

(load "test-framework.ss")
(load "fp/logic.ss")

(display "\n")
(display "==============================================================\n")
(display "         LOGIC PROGRAMMING TESTS\n")
(display "==============================================================\n")

;;; ============================================================
;;; Logic Variable Tests
;;; ============================================================

(test-group logic-variables
  (define-test make-lvar-test
    (let ([v (make-lvar 'x)])
      (assert-true (lvar? v))
      (assert-equal 'x (lvar-name v))))

  (define-test lvar-unique-id-test
    (let ([v1 (make-lvar 'a)]
          [v2 (make-lvar 'b)])
      (assert-false (= (lvar-id v1) (lvar-id v2)))))

  (define-test lvar-equality-test
    (let ([v1 (make-lvar 'x)]
          [v2 (make-lvar 'x)])
      (assert-false (lvar=? v1 v2))  ; Different vars
      (assert-true (lvar=? v1 v1)))) ; Same var

  (define-test lvar-predicate-test
    (assert-true (lvar? (make-lvar 'q)))
    (assert-false (lvar? 42))
    (assert-false (lvar? '(a b c)))))

;;; ============================================================
;;; Substitution Tests
;;; ============================================================

(test-group substitutions
  (define-test empty-subst-test
    (let ([v (make-lvar 'x)])
      (assert-true (nothing? (lookup-subst v empty-subst)))))

  (define-test extend-subst-test
    (let* ([v (make-lvar 'x)]
           [s (extend-subst v 42 empty-subst)]
           [result (lookup-subst v s)])
      (assert-true (just? result))
      (assert-equal 42 (from-just result))))

  (define-test walk-unbound-test
    (let ([v (make-lvar 'x)])
      (assert-true (lvar? (walk v empty-subst)))))

  (define-test walk-bound-test
    (let* ([v (make-lvar 'x)]
           [s (extend-subst v 42 empty-subst)])
      (assert-equal 42 (walk v s))))

  (define-test walk-chain-test
    (let* ([v1 (make-lvar 'x)]
           [v2 (make-lvar 'y)]
           [s1 (extend-subst v1 v2 empty-subst)]
           [s2 (extend-subst v2 100 s1)])
      (assert-equal 100 (walk v1 s2))))

  (define-test walk*-deep-test
    (let* ([v (make-lvar 'x)]
           [s (extend-subst v 42 empty-subst)]
           [term (list 1 v 3)])
      (assert-equal '(1 42 3) (walk* term s)))))

;;; ============================================================
;;; Unification Tests
;;; ============================================================

(test-group unification
  (define-test unify-atoms-equal-test
    (let ([result (unify 42 42 empty-subst)])
      (assert-true (just? result))))

  (define-test unify-atoms-unequal-test
    (assert-true (nothing? (unify 1 2 empty-subst))))

  (define-test unify-var-value-test
    (let* ([v (make-lvar 'x)]
           [result (unify v 42 empty-subst)])
      (assert-true (just? result))
      (assert-equal 42 (walk v (from-just result)))))

  (define-test unify-value-var-test
    (let* ([v (make-lvar 'x)]
           [result (unify 42 v empty-subst)])
      (assert-true (just? result))
      (assert-equal 42 (walk v (from-just result)))))

  (define-test unify-vars-test
    (let* ([v1 (make-lvar 'x)]
           [v2 (make-lvar 'y)]
           [result (unify v1 v2 empty-subst)])
      (assert-true (just? result))))

  (define-test unify-pairs-test
    (let ([result (unify '(1 2) '(1 2) empty-subst)])
      (assert-true (just? result))))

  (define-test unify-pairs-fail-test
    (assert-true (nothing? (unify '(1 2) '(1 3) empty-subst))))

  (define-test unify-nested-test
    (let* ([v (make-lvar 'x)]
           [result (unify (list 1 v 3) (list 1 2 3) empty-subst)])
      (assert-true (just? result))
      (assert-equal 2 (walk v (from-just result)))))

  (define-test unify-complex-test
    (let* ([v1 (make-lvar 'a)]
           [v2 (make-lvar 'b)]
           [result (unify (list v1 v2) (list 1 2) empty-subst)])
      (assert-true (just? result))
      (let ([s (from-just result)])
        (assert-equal 1 (walk v1 s))
        (assert-equal 2 (walk v2 s))))))

;;; ============================================================
;;; Goal Tests
;;; ============================================================

(test-group basic-goals
  (define-test succeed-test
    (let ([results (stream->list 10 (succeed empty-subst))])
      (assert-equal 1 (length results))))

  (define-test fail-test
    (let ([results (stream->list 10 (fail empty-subst))])
      (assert-equal 0 (length results))))

  (define-test ==-succeed-test
    (let ([results (stream->list 10 ((== 1 1) empty-subst))])
      (assert-equal 1 (length results))))

  (define-test ==-fail-test
    (let ([results (stream->list 10 ((== 1 2) empty-subst))])
      (assert-equal 0 (length results))))

  (define-test ==-var-test
    (let* ([v (make-lvar 'x)]
           [results (stream->list 10 ((== v 42) empty-subst))])
      (assert-equal 1 (length results))
      (assert-equal 42 (walk v (car results)))))

  (define-test =/=-test
    (let ([results (stream->list 10 ((=/= 1 2) empty-subst))])
      (assert-equal 1 (length results)))
    (let ([results (stream->list 10 ((=/= 1 1) empty-subst))])
      (assert-equal 0 (length results)))))

;;; ============================================================
;;; Goal Combinator Tests
;;; ============================================================

(test-group goal-combinators
  (define-test conj-succeed-test
    (let* ([goal (conj succeed succeed)]
           [results (stream->list 10 (goal empty-subst))])
      (assert-equal 1 (length results))))

  (define-test conj-fail-test
    (let* ([goal (conj succeed fail)]
           [results (stream->list 10 (goal empty-subst))])
      (assert-equal 0 (length results))))

  (define-test disj-test
    (let* ([goal (disj (== 1 1) (== 2 2))]
           [results (stream->list 10 (goal empty-subst))])
      (assert-equal 2 (length results))))

  (define-test disj-one-fail-test
    (let* ([goal (disj (== 1 2) (== 1 1))]
           [results (stream->list 10 (goal empty-subst))])
      (assert-equal 1 (length results))))

  (define-test conj*-test
    (let* ([v1 (make-lvar 'x)]
           [v2 (make-lvar 'y)]
           [goal (conj* (list (== v1 1) (== v2 2)))]
           [results (stream->list 10 (goal empty-subst))])
      (assert-equal 1 (length results))
      (let ([s (car results)])
        (assert-equal 1 (walk v1 s))
        (assert-equal 2 (walk v2 s)))))

  (define-test conde-test
    (let* ([v (make-lvar 'x)]
           [goal (conde (list (list (== v 1))
                              (list (== v 2))
                              (list (== v 3))))]
           [results (stream->list 10 (goal empty-subst))])
      (assert-equal 3 (length results)))))

;;; ============================================================
;;; Fresh Variable Tests
;;; ============================================================

(test-group fresh-variables
  (define-test fresh1-test
    (let* ([goal (fresh1 (lambda (x) (== x 42)))]
           [results (stream->list 10 (goal empty-subst))])
      (assert-equal 1 (length results))))

  (define-test fresh2-test
    (let* ([goal (fresh2 (lambda (x y)
                           (conj (== x 1) (== y 2))))]
           [results (stream->list 10 (goal empty-subst))])
      (assert-equal 1 (length results))))

  (define-test fresh-multiple-solutions-test
    (let* ([goal (fresh1 (lambda (x)
                           (disj (== x 'a) (== x 'b))))]
           [results (stream->list 10 (goal empty-subst))])
      (assert-equal 2 (length results)))))

;;; ============================================================
;;; Run and Reify Tests
;;; ============================================================

(test-group run-and-reify
  (define-test run-goal-test
    (let ([results (run-goal 5 (== 1 1))])
      (assert-equal 1 (length results))))

  (define-test run*-single-test
    (let ([results (run* 5 (lambda (q) (== q 42)))])
      (assert-equal '(42) results)))

  (define-test run*-multiple-test
    (let ([results (run* 5 (lambda (q)
                             (disj (== q 1) (disj (== q 2) (== q 3)))))])
      (assert-equal 3 (length results))
      (assert-true (not (not (member 1 results))))
      (assert-true (not (not (member 2 results))))
      (assert-true (not (not (member 3 results))))))

  (define-test run*-limit-test
    (let ([results (run* 2 (lambda (q)
                             (disj* (list (== q 1) (== q 2) (== q 3) (== q 4)))))])
      (assert-equal 2 (length results)))))

;;; ============================================================
;;; List Relation Tests
;;; ============================================================

(test-group list-relations
  (define-test conso-forward-test
    (let ([results (run* 1 (lambda (q) (conso 1 '(2 3) q)))])
      (assert-equal '((1 2 3)) results)))

  (define-test conso-backward-head-test
    (let ([results (run* 1 (lambda (q) (conso q '(2 3) '(1 2 3))))])
      (assert-equal '(1) results)))

  (define-test conso-backward-tail-test
    (let ([results (run* 1 (lambda (q) (conso 1 q '(1 2 3))))])
      (assert-equal '((2 3)) results)))

  (define-test nullo-test
    (let ([results (run* 1 (lambda (q) (conj (nullo q) succeed)))])
      (assert-equal '(()) results)))

  (define-test caro-test
    (let ([results (run* 1 (lambda (q) (caro '(a b c) q)))])
      (assert-equal '(a) results)))

  (define-test cdro-test
    (let ([results (run* 1 (lambda (q) (cdro '(a b c) q)))])
      (assert-equal '((b c)) results)))

  (define-test appendo-forward-test
    (let ([results (run* 1 (lambda (q) (appendo '(1 2) '(3 4) q)))])
      (assert-equal '((1 2 3 4)) results)))

  (define-test appendo-backward-test
    (let ([results (run* 5 (lambda (q)
                             (fresh2 (lambda (x y)
                               (conj (appendo x y '(1 2 3))
                                     (== q (cons x y)))))))])
      (assert-equal 4 (length results))))  ; 4 ways to split

  (define-test membero-test
    (let ([results (run* 5 (lambda (q) (membero q '(a b c))))])
      (assert-equal 3 (length results))
      (assert-true (not (not (member 'a results))))
      (assert-true (not (not (member 'b results))))
      (assert-true (not (not (member 'c results)))))))

;;; ============================================================
;;; Peano Arithmetic Tests
;;; ============================================================

(test-group peano-arithmetic
  (define-test nat->peano-test
    (assert-equal 'zero (nat->peano 0))
    (assert-equal '(succ zero) (nat->peano 1))
    (assert-equal '(succ (succ zero)) (nat->peano 2)))

  (define-test peano->nat-test
    (assert-equal 0 (peano->nat 'zero))
    (assert-equal 3 (peano->nat (nat->peano 3))))

  (define-test pluso-forward-test
    (let ([results (run* 1 (lambda (q)
                             (pluso (nat->peano 2) (nat->peano 3) q)))])
      (assert-equal 1 (length results))
      (assert-equal 5 (peano->nat (car results)))))

  (define-test pluso-backward-test
    (let ([results (run* 1 (lambda (q)
                             (pluso q (nat->peano 2) (nat->peano 5))))])
      (assert-equal 1 (length results))
      (assert-equal 3 (peano->nat (car results)))))

  (define-test minuso-test
    (let ([results (run* 1 (lambda (q)
                             (minuso (nat->peano 5) (nat->peano 2) q)))])
      (assert-equal 1 (length results))
      (assert-equal 3 (peano->nat (car results))))))

;;; ============================================================
;;; Occurs Check Tests
;;; ============================================================

(test-group occurs-check
  (define-test occurs-simple-test
    (let ([v (make-lvar 'x)])
      (assert-true (occurs? v v empty-subst))
      (assert-false (occurs? v 42 empty-subst))))

  (define-test occurs-in-pair-test
    (let ([v (make-lvar 'x)])
      (assert-true (occurs? v (list 1 v 3) empty-subst))
      (assert-false (occurs? v (list 1 2 3) empty-subst))))

  (define-test unify-check-prevents-cycle-test
    (let ([v (make-lvar 'x)])
      ;; Without occurs check, this would create a cycle x = (1 . x)
      (assert-true (nothing? (unify-check v (cons 1 v) empty-subst)))))

  (define-test ==/check-test
    (let* ([v (make-lvar 'x)]
           [g (==/check v (cons 1 v))]
           [results (stream->list 1 (g empty-subst))])
      (assert-equal 0 (length results)))))

;;; ============================================================
;;; Reverse Relation Tests
;;; ============================================================

(test-group reverse-relation
  (define-test reverseo-forward-test
    (let ([results (run* 1 (lambda (q) (reverseo '(1 2 3) q)))])
      (assert-equal '((3 2 1)) results)))

  ;; Note: reverseo-backward is not supported with current implementation
  ;; as it requires infinite search (accumulator pattern doesn't work backward)

  (define-test reverseo-empty-test
    (let ([results (run* 1 (lambda (q) (reverseo '() q)))])
      (assert-equal '(()) results))))

;;; ============================================================
;;; Complex Query Tests
;;; ============================================================

(test-group complex-queries
  ;; Find all pairs (x, y) where x + y = 3
  (define-test addition-pairs-test
    (let ([results (run* 10 (lambda (q)
                              (fresh2 (lambda (x y)
                                (conj* (list
                                  (pluso x y (nat->peano 3))
                                  (== q (cons x y))))))))])
      (assert-equal 4 (length results))))  ; (0,3), (1,2), (2,1), (3,0)

  ;; Find element at position in list
  (define-test element-at-test
    (let ([results (run* 1 (lambda (q)
                             (fresh2 (lambda (head tail)
                               (conj* (list
                                 (cdro '(a b c d) tail)
                                 (caro tail q)))))))])
      (assert-equal '(b) results))))

;;; ============================================================
;;; Summary
;;; ============================================================

(display "\n")
(display "==============================================================\n")
(printf "Tests passed: ~a\n" *tests-passed*)
(printf "Tests failed: ~a\n" *tests-failed*)
(printf "Total tests:  ~a\n" *tests-run*)

(if (= *tests-failed* 0)
    (display "\n[SUCCESS] All logic programming tests passed.\n")
    (display "\n[FAILURE] Some logic programming tests failed.\n"))
