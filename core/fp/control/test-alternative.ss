;;; fabric/stitches/fp/test-alternative.ss — Tests for Alternative/MonadPlus

;;; NOTE: Run from fabric/stitches directory

(load "core/test-framework.ss")
(load "core/fp/control/alternative.ss")

(display "
")
(display "==============================================================
")
(display "         ALTERNATIVE / MONADPLUS TESTS
")
(display "==============================================================
")

;;; ============================================================
;;; Alternative Structure Tests
;;; ============================================================

(test-group alternative-structure
            (define-test make-alternative-test
              (assert-true (alternative? maybe-alternative)))
            
            (define-test alt-empty-maybe-test
              (assert-true (nothing? (alt-empty maybe-alternative))))
            
            (define-test alt-pure-maybe-test
              (let ([result (alt-pure maybe-alternative 42)])
                   (assert-true (just? result))
                   (assert-equal 42 (from-just result)))))

;;; ============================================================
;;; Maybe Alternative Tests
;;; ============================================================

(test-group maybe-alternative
            (define-test maybe-alt-both-nothing-test
              (let ([result (alt-or maybe-alternative nothing nothing)])
                   (assert-true (nothing? result))))
            
            (define-test maybe-alt-left-just-test
              (let ([result (alt-or maybe-alternative (just 1) nothing)])
                   (assert-true (just? result))
                   (assert-equal 1 (from-just result))))
            
            (define-test maybe-alt-right-just-test
              (let ([result (alt-or maybe-alternative nothing (just 2))])
                   (assert-true (just? result))
                   (assert-equal 2 (from-just result))))
            
            (define-test maybe-alt-both-just-test
              ;; First Just wins
              (let ([result (alt-or maybe-alternative (just 1) (just 2))])
                   (assert-true (just? result))
                   (assert-equal 1 (from-just result))))
            
            (define-test maybe-alt-ap-test
              (let ([result (alt-ap maybe-alternative
                                    (just add1)
                                    (just 41))])
                   (assert-true (just? result))
                   (assert-equal 42 (from-just result))))
            
            (define-test maybe-alt-ap-nothing-fn-test
              (let ([result (alt-ap maybe-alternative nothing (just 41))])
                   (assert-true (nothing? result))))
            
            (define-test maybe-alt-ap-nothing-arg-test
              (let ([result (alt-ap maybe-alternative (just add1) nothing)])
                   (assert-true (nothing? result)))))

;;; ============================================================
;;; Alternative Laws Tests (Maybe)
;;; ============================================================

(test-group alternative-laws-maybe
            ;; Left identity: empty <|> x = x
            (define-test left-identity-test
              (let ([x (just 42)])
                   (assert-equal (from-just x)
                                 (from-just (alt-or maybe-alternative
                                                    (alt-empty maybe-alternative)
                                                    x)))))
            
            ;; Right identity: x <|> empty = x
            (define-test right-identity-test
              (let ([x (just 42)])
                   (assert-equal (from-just x)
                                 (from-just (alt-or maybe-alternative
                                                    x
                                                    (alt-empty maybe-alternative))))))
            
            ;; Associativity: (x <|> y) <|> z = x <|> (y <|> z)
            (define-test associativity-test
              (let ([x (just 1)]
                    [y (just 2)]
                    [z (just 3)])
                   (let ([lhs (alt-or maybe-alternative
                                      (alt-or maybe-alternative x y)
                                      z)]
                         [rhs (alt-or maybe-alternative
                                      x
                                      (alt-or maybe-alternative y z))])
                        (assert-equal (from-just lhs) (from-just rhs))))))

;;; ============================================================
;;; List Alternative Tests
;;; ============================================================

(test-group list-alternative
            (define-test list-empty-test
              (assert-equal '() (alt-empty list-alternative)))
            
            (define-test list-alt-concat-test
              (let ([result (alt-or list-alternative '(1 2) '(3 4))])
                   (assert-equal '(1 2 3 4) result)))
            
            (define-test list-alt-left-empty-test
              (let ([result (alt-or list-alternative '() '(1 2))])
                   (assert-equal '(1 2) result)))
            
            (define-test list-alt-right-empty-test
              (let ([result (alt-or list-alternative '(1 2) '())])
                   (assert-equal '(1 2) result)))
            
            (define-test list-pure-test
              (assert-equal '(42) (alt-pure list-alternative 42)))
            
            (define-test list-ap-test
              (let ([result (alt-ap list-alternative
                                    (list add1 (lambda (x) (* x 2)))
                                    '(10 20))])
                   (assert-equal '(11 21 20 40) result))))

;;; ============================================================
;;; MonadPlus Structure Tests
;;; ============================================================

(test-group monad-plus-structure
            (define-test make-monad-plus-test
              (assert-true (monad-plus? maybe-monad-plus)))
            
            (define-test mp-zero-test
              (assert-true (nothing? (mp-zero maybe-monad-plus))))
            
            (define-test mp-return-test
              (let ([result (mp-return maybe-monad-plus 42)])
                   (assert-true (just? result))
                   (assert-equal 42 (from-just result)))))

;;; ============================================================
;;; Maybe MonadPlus Tests
;;; ============================================================

(test-group maybe-monad-plus
            (define-test maybe-mp-bind-just-test
              (let ([result (mp-bind maybe-monad-plus
                                     (just 21)
                                     (lambda (x) (just (* x 2))))])
                   (assert-true (just? result))
                   (assert-equal 42 (from-just result))))
            
            (define-test maybe-mp-bind-nothing-test
              (let ([result (mp-bind maybe-monad-plus
                                     nothing
                                     (lambda (x) (just (* x 2))))])
                   (assert-true (nothing? result))))
            
            (define-test maybe-mp-plus-test
              (let ([result (mp-plus maybe-monad-plus (just 1) (just 2))])
                   (assert-equal 1 (from-just result)))))

;;; ============================================================
;;; List MonadPlus Tests
;;; ============================================================

(test-group list-monad-plus
            (define-test list-mp-bind-test
              (let ([result (mp-bind list-monad-plus
                                     '(1 2 3)
                                     (lambda (x) (list x (* x 10))))])
                   (assert-equal '(1 10 2 20 3 30) result)))
            
            (define-test list-mp-bind-empty-test
              (let ([result (mp-bind list-monad-plus
                                     '()
                                     (lambda (x) (list x (* x 10))))])
                   (assert-equal '() result)))
            
            (define-test list-mp-return-bind-test
              (let ([result (mp-bind list-monad-plus
                                     (mp-return list-monad-plus 5)
                                     (lambda (x) (list (+ x 1) (+ x 2))))])
                   (assert-equal '(6 7) result))))

;;; ============================================================
;;; Guard Tests
;;; ============================================================

(test-group guard
            (define-test guard-true-maybe-test
              (let ([result (guard maybe-alternative #t)])
                   (assert-true (just? result))))
            
            (define-test guard-false-maybe-test
              (let ([result (guard maybe-alternative #f)])
                   (assert-true (nothing? result))))
            
            (define-test guard-true-list-test
              (let ([result (guard list-alternative #t)])
                   (assert-equal '(()) result)))
            
            (define-test guard-false-list-test
              (let ([result (guard list-alternative #f)])
                   (assert-equal '() result)))
            
            (define-test guard-mp-true-test
              (let ([result (guard-mp list-monad-plus #t)])
                   (assert-equal '(()) result)))
            
            (define-test guard-mp-false-test
              (let ([result (guard-mp list-monad-plus #f)])
                   (assert-equal '() result))))

;;; ============================================================
;;; asum / msum Tests
;;; ============================================================

(test-group asum-msum
            (define-test asum-empty-test
              (let ([result (asum maybe-alternative '())])
                   (assert-true (nothing? result))))
            
            (define-test asum-all-nothing-test
              (let ([result (asum maybe-alternative (list nothing nothing nothing))])
                   (assert-true (nothing? result))))
            
            (define-test asum-finds-first-test
              (let ([result (asum maybe-alternative
                                  (list nothing (just 1) (just 2)))])
                   (assert-true (just? result))
                   (assert-equal 1 (from-just result))))
            
            (define-test msum-empty-test
              (assert-equal '() (msum list-monad-plus '())))
            
            (define-test msum-concatenates-test
              (let ([result (msum list-monad-plus '((1 2) (3 4) (5)))])
                   (assert-equal '(1 2 3 4 5) result))))

;;; ============================================================
;;; Optional Tests
;;; ============================================================

(test-group optional
            (define-test optional-success-test
              (let ([result (optional maybe-alternative (just 42))])
                   (assert-true (just? result))
                   (let ([inner (from-just result)])
                        (assert-true (just? inner))
                        (assert-equal 42 (from-just inner)))))
            
            (define-test optional-failure-test
              (let ([result (optional maybe-alternative nothing)])
                   (assert-true (just? result))  ; optional always succeeds
                   (let ([inner (from-just result)])
                        (assert-true (nothing? inner))))))

;;; ============================================================
;;; many/some Tests
;;; ============================================================

(test-group many-some
            (define-test many-fuel-empty-test
              (let ([result (many-fuel list-alternative '() 10)])
                   (assert-equal '(()) result)))  ; One empty list solution
            
            (define-test some-fuel-empty-test
              (let ([result (some-fuel list-alternative '() 10)])
                   (assert-equal '() result)))  ; No solutions
            
            (define-test many-fuel-zero-test
              (let ([result (many-fuel list-alternative '(x) 0)])
                   (assert-equal '(()) result)))  ; Zero fuel = empty result
            
            (define-test some-fuel-zero-test
              (let ([result (some-fuel list-alternative '(x) 0)])
                   (assert-equal '() result))))  ; Zero fuel = no results

;;; ============================================================
;;; Backtracking Tests
;;; ============================================================

(test-group backtracking
            (define-test bt-return-test
              (assert-equal '(42) (bt-return 42)))
            
            (define-test bt-fail-test
              (assert-equal '() bt-fail))
            
            (define-test bt-choice-test
              (assert-equal '(1 2 3 4) (bt-choice '(1 2) '(3 4))))
            
            (define-test bt-guard-true-test
              (assert-equal '(()) (bt-guard #t)))
            
            (define-test bt-guard-false-test
              (assert-equal '() (bt-guard #f)))
            
            (define-test bt-bind-test
              (let ([result (bt-bind '(1 2 3)
                                     (lambda (x) (bt-return (* x 2))))])
                   (assert-equal '(2 4 6) result)))
            
            (define-test bt-bind-with-guard-test
              (let ([result (bt-bind (between 1 10)
                                     (lambda (x)
                                             (bt-bind (bt-guard (even? x))
                                                      (lambda (_) (bt-return x)))))])
                   (assert-equal '(2 4 6 8 10) result))))

;;; ============================================================
;;; amb / amb-list Tests
;;; ============================================================

(test-group amb
            (define-test amb-test
              (let ([result (amb list-monad-plus 1 2)])
                   (assert-equal '(1 2) result)))
            
            (define-test amb-list-test
              (let ([result (amb-list list-monad-plus '(a b c))])
                   (assert-equal '(a b c) result)))
            
            (define-test amb-list-empty-test
              (let ([result (amb-list list-monad-plus '())])
                   (assert-equal '() result))))

;;; ============================================================
;;; try-all / first-match Tests
;;; ============================================================

(test-group try-all-first-match
            (define-test try-all-find-test
              (let ([result (try-all maybe-monad-plus
                                     (lambda (x) (if (> x 5) (just x) nothing))
                                     '(1 2 3 10 20))])
                   (assert-true (just? result))
                   (assert-equal 10 (from-just result))))
            
            (define-test try-all-none-test
              (let ([result (try-all maybe-monad-plus
                                     (lambda (x) (if (> x 100) (just x) nothing))
                                     '(1 2 3))])
                   (assert-true (nothing? result))))
            
            (define-test first-match-found-test
              (let ([result (first-match maybe-monad-plus even? '(1 3 4 5 6))])
                   (assert-true (just? result))
                   (assert-equal 4 (from-just result))))
            
            (define-test first-match-not-found-test
              (let ([result (first-match maybe-monad-plus even? '(1 3 5 7))])
                   (assert-true (nothing? result)))))

;;; ============================================================
;;; once (cut) Tests
;;; ============================================================

(test-group once
            (define-test once-empty-test
              (assert-equal '() (once '())))
            
            (define-test once-nonempty-test
              (assert-equal '(1) (once '(1 2 3 4 5))))
            
            (define-test once-single-test
              (assert-equal '(x) (once '(x)))))

;;; ============================================================
;;; collect Tests
;;; ============================================================

(test-group collect
            (define-test collect-all-test
              (assert-equal '(1 2 3) (collect-all '(1 2 3))))
            
            (define-test collect-first-found-test
              (let ([result (collect-first '(1 2 3))])
                   (assert-true (just? result))
                   (assert-equal 1 (from-just result))))
            
            (define-test collect-first-empty-test
              (let ([result (collect-first '())])
                   (assert-true (nothing? result))))
            
            (define-test collect-n-test
              (assert-equal '(1 2 3) (collect-n 3 '(1 2 3 4 5))))
            
            (define-test collect-n-less-test
              (assert-equal '(1 2) (collect-n 5 '(1 2)))))

;;; ============================================================
;;; Utility Tests
;;; ============================================================

(test-group utilities
            (define-test between-test
              (assert-equal '(1 2 3 4 5) (between 1 5)))
            
            (define-test between-empty-test
              (assert-equal '() (between 5 3)))
            
            (define-test between-single-test
              (assert-equal '(3) (between 3 3)))
            
            (define-test remove-first-test
              (assert-equal '(1 3 2) (remove-first 2 '(1 2 3 2))))
            
            (define-test remove-first-not-found-test
              (assert-equal '(1 2 3) (remove-first 4 '(1 2 3))))
            
            (define-test subsets-empty-test
              (assert-equal '(()) (subsets '())))
            
            (define-test subsets-single-test
              (let ([result (subsets '(a))])
                   (assert-equal 2 (length result))
                   (assert-true (not (not (member '() result))))
                   (assert-true (not (not (member '(a) result))))))
            
            (define-test subsets-count-test
              ;; 2^n subsets
              (assert-equal 8 (length (subsets '(1 2 3))))))

;;; ============================================================
;;; Permutations Tests
;;; ============================================================

(test-group permutations
            (define-test permutations-empty-test
              (assert-equal '(()) (permutations '())))
            
            (define-test permutations-single-test
              (assert-equal '((a)) (permutations '(a))))
            
            (define-test permutations-count-test
              ;; n! permutations
              (assert-equal 6 (length (permutations '(1 2 3)))))
            
            (define-test permutations-contains-all-test
              (let ([result (permutations '(1 2))])
                   (assert-true (not (not (member '(1 2) result))))
                   (assert-true (not (not (member '(2 1) result)))))))

;;; ============================================================
;;; Interleave Tests
;;; ============================================================

(test-group interleave
            (define-test interleave-empty-left-test
              (assert-equal '(1 2 3) (interleave '() '(1 2 3))))
            
            (define-test interleave-empty-right-test
              (assert-equal '(1 2 3) (interleave '(1 2 3) '())))
            
            (define-test interleave-same-length-test
              (assert-equal '(1 a 2 b 3 c) (interleave '(1 2 3) '(a b c))))
            
            (define-test interleave-different-length-test
              (assert-equal '(1 a 2 b 3) (interleave '(1 2 3) '(a b)))))

;;; ============================================================
;;; N-Queens Tests
;;; ============================================================

(test-group n-queens
            (define-test safe-no-conflict-test
              (assert-true (safe? 3 '(1))))
            
            (define-test safe-same-column-test
              (assert-false (safe? 1 '(1))))
            
            (define-test safe-diagonal-test
              (assert-false (safe? 2 '(1))))
            
            (define-test safe-anti-diagonal-test
              (assert-false (safe? 0 '(1))))
            
            (define-test n-queens-1-test
              (let ([solutions (n-queens 1)])
                   (assert-equal 1 (length solutions))))
            
            (define-test n-queens-4-test
              (let ([solutions (n-queens 4)])
                   (assert-equal 2 (length solutions))))
            
            (define-test n-queens-4-valid-test
              (let ([solutions (n-queens 4)])
                   ;; Both solutions should be valid
                   (assert-true (not (not (member '(3 1 4 2) solutions))))
                   (assert-true (not (not (member '(2 4 1 3) solutions)))))))

;;; ============================================================
;;; Practical Example: Pythagorean Triples
;;; ============================================================

(test-group pythagorean-triples
            (define (pythagorean-triples n)
              (bt-bind (between 1 n)
                       (lambda (a)
                               (bt-bind (between a n)
                                        (lambda (b)
                                                (bt-bind (between b n)
                                                         (lambda (c)
                                                                 (bt-bind (bt-guard (= (+ (* a a) (* b b))
                                                                                       (* c c)))
                                                                          (lambda (_)
                                                                                  (bt-return (list a b c)))))))))))
            
            (define-test find-small-triples-test
              (let ([result (pythagorean-triples 15)])
                   (assert-true (not (not (member '(3 4 5) result))))
                   (assert-true (not (not (member '(5 12 13) result))))
                   (assert-true (not (not (member '(6 8 10) result)))))))

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
[SUCCESS] All alternative/monadplus tests passed.
")
    (display "
[FAILURE] Some alternative/monadplus tests failed.
"))
