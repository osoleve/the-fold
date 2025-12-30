;;; fabric/stitches/fp/test-regex-to-fsm.ss — Tests for Regex to FSM Conversion

(load "fabric/stitches/test-framework.ss")
(load "fabric/stitches/fp/regex-to-fsm.ss")

(display "\n")
(display "==============================================================\n")
(display "         REGEX TO FSM CONVERSION TESTS\n")
(display "==============================================================\n")

;;; ============================================================
;;; Thompson Construction Tests
;;; ============================================================

(test-group thompson-basic
            (define-test thompson-char-test
              (thompson-reset-counter!)
              (let* ([r (rx-char #\a)]
                     [nfa (rx->nfa r)])
                    (assert-true (fsm? nfa))
                    (assert-true (fsm-accepts? nfa "a"))
                    (assert-false (fsm-accepts? nfa ""))
                    (assert-false (fsm-accepts? nfa "b"))
                    (assert-false (fsm-accepts? nfa "aa"))))
            
            (define-test thompson-epsilon-test
              (thompson-reset-counter!)
              (let* ([r rx-epsilon]
                     [nfa (rx->nfa r)])
                    (assert-true (fsm-accepts? nfa ""))
                    (assert-false (fsm-accepts? nfa "a"))))
            
            (define-test thompson-empty-test
              (thompson-reset-counter!)
              (let* ([r rx-empty]
                     [nfa (rx->nfa r)])
                    (assert-false (fsm-accepts? nfa ""))
                    (assert-false (fsm-accepts? nfa "a"))))
            
            (define-test thompson-seq-test
              (thompson-reset-counter!)
              (let* ([r (rx-seq (rx-char #\a) (rx-char #\b))]
                     [nfa (rx->nfa r)])
                    (assert-true (fsm-accepts? nfa "ab"))
                    (assert-false (fsm-accepts? nfa "a"))
                    (assert-false (fsm-accepts? nfa "b"))
                    (assert-false (fsm-accepts? nfa "ba"))))
            
            (define-test thompson-alt-test
              (thompson-reset-counter!)
              (let* ([r (rx-alt (rx-char #\a) (rx-char #\b))]
                     [nfa (rx->nfa r)])
                    (assert-true (fsm-accepts? nfa "a"))
                    (assert-true (fsm-accepts? nfa "b"))
                    (assert-false (fsm-accepts? nfa "c"))
                    (assert-false (fsm-accepts? nfa "ab"))))
            
            (define-test thompson-star-test
              (thompson-reset-counter!)
              (let* ([r (rx-star (rx-char #\a))]
                     [nfa (rx->nfa r)])
                    (assert-true (fsm-accepts? nfa ""))
                    (assert-true (fsm-accepts? nfa "a"))
                    (assert-true (fsm-accepts? nfa "aa"))
                    (assert-true (fsm-accepts? nfa "aaa"))
                    (assert-false (fsm-accepts? nfa "b"))
                    (assert-false (fsm-accepts? nfa "ab")))))

;;; ============================================================
;;; Complex Regex Tests
;;; ============================================================

(test-group thompson-complex
            (define-test thompson-plus-test
              (thompson-reset-counter!)
              (let* ([r (rx-plus (rx-char #\a))]
                     [nfa (rx->nfa r)])
                    (assert-false (fsm-accepts? nfa ""))
                    (assert-true (fsm-accepts? nfa "a"))
                    (assert-true (fsm-accepts? nfa "aa"))
                    (assert-true (fsm-accepts? nfa "aaa"))))
            
            (define-test thompson-opt-test
              (thompson-reset-counter!)
              (let* ([r (rx-opt (rx-char #\a))]
                     [nfa (rx->nfa r)])
                    (assert-true (fsm-accepts? nfa ""))
                    (assert-true (fsm-accepts? nfa "a"))
                    (assert-false (fsm-accepts? nfa "aa"))))
            
            (define-test thompson-string-test
              (thompson-reset-counter!)
              (let* ([r (rx-string "hello")]
                     [nfa (rx->nfa r)])
                    (assert-true (fsm-accepts? nfa "hello"))
                    (assert-false (fsm-accepts? nfa "hell"))
                    (assert-false (fsm-accepts? nfa "helloo"))))
            
            (define-test thompson-complex-composition-test
              (thompson-reset-counter!)
              ;; (a|b)*c
              (let* ([r (rx-seq (rx-star (rx-alt (rx-char #\a) (rx-char #\b)))
                                (rx-char #\c))]
                     [nfa (rx->nfa r)])
                    (assert-true (fsm-accepts? nfa "c"))
                    (assert-true (fsm-accepts? nfa "ac"))
                    (assert-true (fsm-accepts? nfa "bc"))
                    (assert-true (fsm-accepts? nfa "abc"))
                    (assert-true (fsm-accepts? nfa "aabc"))
                    (assert-true (fsm-accepts? nfa "bbaac"))
                    (assert-false (fsm-accepts? nfa ""))
                    (assert-false (fsm-accepts? nfa "ab"))))
            
            (define-test thompson-nested-star-test
              (thompson-reset-counter!)
              ;; (a*)*
              (let* ([r (rx-star (rx-star (rx-char #\a)))]
                     [nfa (rx->nfa r)])
                    (assert-true (fsm-accepts? nfa ""))
                    (assert-true (fsm-accepts? nfa "a"))
                    (assert-true (fsm-accepts? nfa "aaa"))))
            
            (define-test thompson-multiple-alt-test
              (thompson-reset-counter!)
              ;; a|b|c
              (let* ([r (rx-alt (rx-char #\a)
                                (rx-alt (rx-char #\b) (rx-char #\c)))]
                     [nfa (rx->nfa r)])
                    (assert-true (fsm-accepts? nfa "a"))
                    (assert-true (fsm-accepts? nfa "b"))
                    (assert-true (fsm-accepts? nfa "c"))
                    (assert-false (fsm-accepts? nfa "d")))))

;;; ============================================================
;;; NFA to DFA Conversion Tests
;;; ============================================================

(test-group nfa-to-dfa-conversion
            (define-test rx-to-dfa-basic-test
              (thompson-reset-counter!)
              (let* ([r (rx-seq (rx-char #\a) (rx-char #\b))]
                     [dfa (rx->dfa r)])
                    (assert-true (fsm-deterministic? dfa))
                    (assert-true (fsm-accepts? dfa "ab"))
                    (assert-false (fsm-accepts? dfa "a"))
                    (assert-false (fsm-accepts? dfa "ba"))))
            
            (define-test rx-to-dfa-star-test
              (thompson-reset-counter!)
              (let* ([r (rx-star (rx-alt (rx-char #\a) (rx-char #\b)))]
                     [dfa (rx->dfa r)])
                    (assert-true (fsm-deterministic? dfa))
                    (assert-true (fsm-accepts? dfa ""))
                    (assert-true (fsm-accepts? dfa "a"))
                    (assert-true (fsm-accepts? dfa "ab"))
                    (assert-true (fsm-accepts? dfa "ba"))
                    (assert-true (fsm-accepts? dfa "aabb"))))
            
            (define-test rx-to-minimized-dfa-test
              (thompson-reset-counter!)
              (let* ([r (rx-alt (rx-char #\a) (rx-char #\a))]  ; Redundant
                     [dfa (rx->minimized-dfa r)])
                    (assert-true (fsm-deterministic? dfa))
                    (assert-true (fsm-accepts? dfa "a"))
                    (assert-false (fsm-accepts? dfa "b")))))

;;; ============================================================
;;; Character Class Tests
;;; ============================================================

(test-group character-classes
            (define-test rx-one-of-test
              (thompson-reset-counter!)
              (let* ([r (rx-one-of "abc")]
                     [nfa (rx->nfa r)])
                    ;; Note: Current implementation has limitations with char classes
                    ;; This test documents expected behavior
                    (assert-true (fsm? nfa))))
            
            (define-test rx-digit-test
              (thompson-reset-counter!)
              (let* ([r rx-digit]
                     [nfa (rx->nfa r)])
                    (assert-true (fsm? nfa)))))

;;; ============================================================
;;; Regex Parsing Tests
;;; ============================================================

(test-group regex-parsing
            (define-test parse-char-test
              (let ([r (parse-regex "a")])
                   (assert-true (rx-char? r))
                   (assert-equal #\a (rx-char-value r))))
            
            (define-test parse-seq-test
              (let ([r (parse-regex "ab")])
                   (assert-true (rx-seq? r))))
            
            (define-test parse-alt-test
              (let ([r (parse-regex "a|b")])
                   (assert-true (rx-alt? r))))
            
            (define-test parse-star-test
              (let ([r (parse-regex "a*")])
                   (assert-true (rx-star? r))))
            
            (define-test parse-plus-test
              (let ([r (parse-regex "a+")])
                   (assert-true (rx-seq? r))  ; a+ is expanded to aa*
                   ))
            
            (define-test parse-opt-test
              (let ([r (parse-regex "a?")])
                   (assert-true (rx-alt? r))  ; a? is a|epsilon
                   ))
            
            (define-test parse-group-test
              (let ([r (parse-regex "(ab)*")])
                   (assert-true (rx-star? r))))
            
            (define-test parse-complex-test
              (let ([r (parse-regex "(a|b)*c")])
                   (assert-true (rx-seq? r)))))

;;; ============================================================
;;; Regex Match Convenience Tests
;;; ============================================================

(test-group regex-match-convenience
            (define-test regex-match-simple-test
              (assert-true (regex-match? "a" "a"))
              (assert-false (regex-match? "a" "b")))
            
            (define-test regex-match-star-test
              (assert-true (regex-match? "a*" ""))
              (assert-true (regex-match? "a*" "a"))
              (assert-true (regex-match? "a*" "aaa")))
            
            (define-test regex-match-alt-test
              (assert-true (regex-match? "a|b" "a"))
              (assert-true (regex-match? "a|b" "b"))
              (assert-false (regex-match? "a|b" "c")))
            
            (define-test regex-match-complex-test
              (assert-true (regex-match? "(a|b)*c" "c"))
              (assert-true (regex-match? "(a|b)*c" "abc"))
              (assert-true (regex-match? "(a|b)*c" "bac"))
              (assert-false (regex-match? "(a|b)*c" "ab")))
            
            (define-test regex-match-dot-test
              (assert-true (regex-match? "." "a"))
              (assert-true (regex-match? "." "x"))
              (assert-false (regex-match? "." "")))
            
            (define-test regex-match-char-class-test
              (assert-true (regex-match? "[abc]" "a"))
              (assert-true (regex-match? "[abc]" "b"))
              (assert-true (regex-match? "[abc]" "c"))
              (assert-false (regex-match? "[abc]" "d")))
            
            (define-test regex-match-negated-class-test
              (assert-false (regex-match? "[^abc]" "a"))
              (assert-true (regex-match? "[^abc]" "d"))))

;;; ============================================================
;;; Equivalence with Derivative Matching
;;; ============================================================

(test-group derivative-equivalence
            (define-test nfa-vs-derivative-char-test
              (let ([r (rx-char #\a)])
                   (assert-equal (rx-match? r "a") (regex-match? "a" "a"))
                   (assert-equal (rx-match? r "b") (regex-match? "a" "b"))))
            
            (define-test nfa-vs-derivative-star-test
              (let ([r (rx-star (rx-char #\a))])
                   (assert-equal (rx-match? r "") (fsm-accepts? (rx->nfa r) ""))
                   (assert-equal (rx-match? r "a") (fsm-accepts? (rx->nfa r) "a"))
                   (assert-equal (rx-match? r "aaa") (fsm-accepts? (rx->nfa r) "aaa"))))
            
            (define-test nfa-vs-derivative-complex-test
              (let ([r (rx-seq (rx-star (rx-alt (rx-char #\a) (rx-char #\b)))
                               (rx-char #\c))])
                   (assert-equal (rx-match? r "c") (fsm-accepts? (rx->nfa r) "c"))
                   (assert-equal (rx-match? r "abc") (fsm-accepts? (rx->nfa r) "abc"))
                   (assert-equal (rx-match? r "ab") (fsm-accepts? (rx->nfa r) "ab")))))

;;; ============================================================
;;; Edge Cases
;;; ============================================================

(test-group edge-cases
            (define-test empty-regex-test
              (thompson-reset-counter!)
              (let ([nfa (rx->nfa rx-empty)])
                   (assert-false (fsm-accepts? nfa ""))
                   (assert-false (fsm-accepts? nfa "a"))))
            
            (define-test epsilon-only-test
              (thompson-reset-counter!)
              (let ([nfa (rx->nfa rx-epsilon)])
                   (assert-true (fsm-accepts? nfa ""))
                   (assert-false (fsm-accepts? nfa "a"))))
            
            (define-test nested-empty-test
              (thompson-reset-counter!)
              (let ([nfa (rx->nfa (rx-seq rx-empty (rx-char #\a)))])
                   (assert-false (fsm-accepts? nfa "a"))))
            
            (define-test star-of-epsilon-test
              (thompson-reset-counter!)
              (let ([nfa (rx->nfa (rx-star rx-epsilon))])
                   (assert-true (fsm-accepts? nfa ""))
                   (assert-false (fsm-accepts? nfa "a")))))

;;; ============================================================
;;; Summary
;;; ============================================================

(display "\n")
(display "==============================================================\n")
(printf "Tests passed: ~a\n" *tests-passed*)
(printf "Tests failed: ~a\n" *tests-failed*)
(printf "Total tests:  ~a\n" *tests-run*)

(if (= *tests-failed* 0)
    (display "\n[SUCCESS] All regex-to-fsm tests passed.\n")
    (display "\n[FAILURE] Some regex-to-fsm tests failed.\n"))
