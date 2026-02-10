;;; lattice/fp/parsing/test-regex.ss — Tests for Regex to NFA Compilation
;;;
;;; Run: scheme --script lattice/fp/parsing/test-regex.ss

(load "core/lang/module.ss")
(load "core/testing/test-framework.ss")
(load "lattice/fp/parsing/regex.ss")

(display "\n")
(display "====\n")
(display "         REGEX TO NFA TESTS\n")
(display "====\n")

;;; ============================================================
;;; AST Construction Tests
;;; ============================================================

(test-group ast-construction
  (define-test literal-ast
    (let ([ast (regex-lit #\a)])
      (assert-true (regex-lit? ast))
      (assert-equal #\a (regex-lit-char ast))))

  (define-test dot-ast
    (let ([ast (regex-dot)])
      (assert-true (regex-dot? ast))))

  (define-test class-ast
    (let ([ast (regex-class '(#\a #\b #\c) #f)])
      (assert-true (regex-class? ast))
      (assert-equal '(#\a #\b #\c) (regex-class-chars ast))
      (assert-false (regex-class-negated? ast))))

  (define-test negated-class-ast
    (let ([ast (regex-class '(#\a) #t)])
      (assert-true (regex-class-negated? ast))))

  (define-test seq-ast
    (let ([ast (regex-seq (list (regex-lit #\a) (regex-lit #\b)))])
      (assert-true (regex-seq? ast))
      (assert-equal 2 (length (regex-seq-exprs ast)))))

  (define-test alt-ast
    (let ([ast (regex-alt (list (regex-lit #\a) (regex-lit #\b)))])
      (assert-true (regex-alt? ast))
      (assert-equal 2 (length (regex-alt-exprs ast)))))

  (define-test star-ast
    (let ([ast (regex-star (regex-lit #\a))])
      (assert-true (regex-star? ast))
      (assert-true (regex-lit? (regex-star-expr ast))))))

;;; ============================================================
;;; Parser Tests
;;; ============================================================

(test-group regex-parser
  (define-test parse-literal
    (let ([result (regex-parse "a")])
      (assert-true (right? result))
      (assert-true (regex-lit? (from-right result)))
      (assert-equal #\a (regex-lit-char (from-right result)))))

  (define-test parse-dot
    (let ([result (regex-parse ".")])
      (assert-true (right? result))
      (assert-true (regex-dot? (from-right result)))))

  (define-test parse-concat
    (let ([result (regex-parse "ab")])
      (assert-true (right? result))
      (assert-true (regex-seq? (from-right result)))
      (assert-equal 2 (length (regex-seq-exprs (from-right result))))))

  (define-test parse-alt
    (let ([result (regex-parse "a|b")])
      (assert-true (right? result))
      (assert-true (regex-alt? (from-right result)))
      (assert-equal 2 (length (regex-alt-exprs (from-right result))))))

  (define-test parse-star
    (let ([result (regex-parse "a*")])
      (assert-true (right? result))
      (assert-true (regex-star? (from-right result)))))

  (define-test parse-plus
    (let ([result (regex-parse "a+")])
      (assert-true (right? result))
      (assert-true (regex-plus? (from-right result)))))

  (define-test parse-optional
    (let ([result (regex-parse "a?")])
      (assert-true (right? result))
      (assert-true (regex-opt? (from-right result)))))

  (define-test parse-group
    (let ([result (regex-parse "(ab)")])
      (assert-true (right? result))
      (assert-true (regex-group? (from-right result)))))

  (define-test parse-class-simple
    (let ([result (regex-parse "[abc]")])
      (assert-true (right? result))
      (assert-true (regex-class? (from-right result)))
      (assert-equal 3 (length (regex-class-chars (from-right result))))
      (assert-false (regex-class-negated? (from-right result)))))

  (define-test parse-class-negated
    (let ([result (regex-parse "[^abc]")])
      (assert-true (right? result))
      (assert-true (regex-class? (from-right result)))
      (assert-true (regex-class-negated? (from-right result)))))

  (define-test parse-class-range
    (let ([result (regex-parse "[a-z]")])
      (assert-true (right? result))
      (assert-true (regex-class? (from-right result)))
      ;; Should have 26 characters
      (assert-equal 26 (length (regex-class-chars (from-right result))))))

  (define-test parse-escape
    (let ([result (regex-parse "\\*")])
      (assert-true (right? result))
      (assert-true (regex-lit? (from-right result)))
      (assert-equal #\* (regex-lit-char (from-right result)))))

  (define-test parse-escape-newline
    (let ([result (regex-parse "\\n")])
      (assert-true (right? result))
      (assert-true (regex-lit? (from-right result)))
      (assert-equal #\newline (regex-lit-char (from-right result)))))

  (define-test parse-complex
    ;; a(b|c)*d
    (let ([result (regex-parse "a(b|c)*d")])
      (assert-true (right? result))
      (assert-true (regex-seq? (from-right result)))))

  (define-test parse-multiple-alt
    ;; a|b|c should produce n-ary alt
    (let ([result (regex-parse "a|b|c")])
      (assert-true (right? result))
      (assert-true (regex-alt? (from-right result)))
      (assert-equal 3 (length (regex-alt-exprs (from-right result)))))))

;;; ============================================================
;;; Compilation Tests
;;; ============================================================

(test-group regex-compilation
  (define-test compile-literal
    (let ([nfa (regex->nfa "a")])
      (assert-true (fsm? nfa))
      (assert-true (fsm-accepts? nfa "a"))
      (assert-false (fsm-accepts? nfa "b"))
      (assert-false (fsm-accepts? nfa ""))))

  (define-test compile-concat
    (let ([nfa (regex->nfa "ab")])
      (assert-true (fsm-accepts? nfa "ab"))
      (assert-false (fsm-accepts? nfa "a"))
      (assert-false (fsm-accepts? nfa "b"))
      (assert-false (fsm-accepts? nfa "ba"))))

  (define-test compile-alt
    (let ([nfa (regex->nfa "a|b")])
      (assert-true (fsm-accepts? nfa "a"))
      (assert-true (fsm-accepts? nfa "b"))
      (assert-false (fsm-accepts? nfa "ab"))
      (assert-false (fsm-accepts? nfa ""))))

  (define-test compile-star
    (let ([nfa (regex->nfa "a*")])
      (assert-true (fsm-accepts? nfa ""))
      (assert-true (fsm-accepts? nfa "a"))
      (assert-true (fsm-accepts? nfa "aaa"))
      (assert-false (fsm-accepts? nfa "b"))))

  (define-test compile-plus
    (let ([nfa (regex->nfa "a+")])
      (assert-false (fsm-accepts? nfa ""))
      (assert-true (fsm-accepts? nfa "a"))
      (assert-true (fsm-accepts? nfa "aaa"))
      (assert-false (fsm-accepts? nfa "b"))))

  (define-test compile-optional
    (let ([nfa (regex->nfa "a?")])
      (assert-true (fsm-accepts? nfa ""))
      (assert-true (fsm-accepts? nfa "a"))
      (assert-false (fsm-accepts? nfa "aa"))))

  (define-test compile-group
    (let ([nfa (regex->nfa "(ab)+")])
      (assert-false (fsm-accepts? nfa ""))
      (assert-true (fsm-accepts? nfa "ab"))
      (assert-true (fsm-accepts? nfa "abab"))
      (assert-false (fsm-accepts? nfa "a"))))

  (define-test compile-class
    (let ([nfa (regex->nfa "[abc]")])
      (assert-true (fsm-accepts? nfa "a"))
      (assert-true (fsm-accepts? nfa "b"))
      (assert-true (fsm-accepts? nfa "c"))
      (assert-false (fsm-accepts? nfa "d"))
      (assert-false (fsm-accepts? nfa ""))))

  (define-test compile-class-range
    (let ([nfa (regex->nfa "[a-c]")])
      (assert-true (fsm-accepts? nfa "a"))
      (assert-true (fsm-accepts? nfa "b"))
      (assert-true (fsm-accepts? nfa "c"))
      (assert-false (fsm-accepts? nfa "d"))))

  (define-test compile-dot
    (let ([nfa (regex->nfa "a.b")])
      (assert-true (fsm-accepts? nfa "aab"))
      (assert-true (fsm-accepts? nfa "axb"))
      (assert-true (fsm-accepts? nfa "a b"))
      (assert-false (fsm-accepts? nfa "ab"))
      (assert-false (fsm-accepts? nfa "axxb"))))

  (define-test compile-negated-class
    ;; Test with small universe for predictability
    (let* ([universe '(#\a #\b #\c #\d)]
           [nfa (regex->nfa "[^ab]" universe)])
      (assert-false (fsm-accepts? nfa "a"))
      (assert-false (fsm-accepts? nfa "b"))
      (assert-true (fsm-accepts? nfa "c"))
      (assert-true (fsm-accepts? nfa "d")))))

;;; ============================================================
;;; Integration Tests
;;; ============================================================

(test-group regex-integration
  (define-test email-like-pattern
    ;; Simplified email-like pattern: [a-z]+@[a-z]+
    (let ([nfa (regex->nfa "[a-z]+@[a-z]+")])
      (assert-true (fsm-accepts? nfa "foo@bar"))
      (assert-true (fsm-accepts? nfa "a@b"))
      (assert-false (fsm-accepts? nfa "@bar"))
      (assert-false (fsm-accepts? nfa "foo@"))
      (assert-false (fsm-accepts? nfa "foobar"))))

  (define-test phone-like-pattern
    ;; Simple pattern: [0-9][0-9][0-9]-[0-9][0-9][0-9][0-9]
    (let ([nfa (regex->nfa "[0-9][0-9][0-9]-[0-9][0-9][0-9][0-9]")])
      (assert-true (fsm-accepts? nfa "123-4567"))
      (assert-false (fsm-accepts? nfa "12-4567"))
      (assert-false (fsm-accepts? nfa "abc-defg"))))

  (define-test complex-pattern
    ;; (a|b)*abb - classic regex for strings ending in "abb"
    (let ([nfa (regex->nfa "(a|b)*abb")])
      (assert-true (fsm-accepts? nfa "abb"))
      (assert-true (fsm-accepts? nfa "aabb"))
      (assert-true (fsm-accepts? nfa "babb"))
      (assert-true (fsm-accepts? nfa "aababb"))
      (assert-false (fsm-accepts? nfa "ab"))
      (assert-false (fsm-accepts? nfa "abab"))))

  (define-test regex-to-dfa
    ;; Verify DFA conversion works
    (let ([dfa (regex->dfa "a*b")])
      (assert-true (fsm? dfa))
      (assert-true (fsm-deterministic? dfa))
      (assert-true (fsm-accepts? dfa "b"))
      (assert-true (fsm-accepts? dfa "ab"))
      (assert-true (fsm-accepts? dfa "aaab"))
      (assert-false (fsm-accepts? dfa "a"))))

  (define-test regex-accepts-convenience
    (assert-true (regex-accepts? "a+" "aaa"))
    (assert-false (regex-accepts? "a+" "bbb")))

  (define-test empty-regex
    ;; Empty pattern should match empty string
    (let ([nfa (regex->nfa "")])
      (assert-true (fsm-accepts? nfa ""))
      (assert-false (fsm-accepts? nfa "a")))))

;;; ============================================================
;;; Edge Case Tests
;;; ============================================================

(test-group regex-edge-cases
  (define-test multiple-postfix-ops
    ;; a*+ means (a*)+ which is same as a*
    (let ([nfa (regex->nfa "a*+")])
      (assert-true (fsm-accepts? nfa ""))
      (assert-true (fsm-accepts? nfa "a"))
      (assert-true (fsm-accepts? nfa "aaa"))))

  (define-test nested-groups
    (let ([nfa (regex->nfa "((a))")])
      (assert-true (fsm-accepts? nfa "a"))
      (assert-false (fsm-accepts? nfa ""))))

  (define-test alt-with-empty
    ;; a|b|c - multiple alternatives
    (let ([nfa (regex->nfa "a|b|c")])
      (assert-true (fsm-accepts? nfa "a"))
      (assert-true (fsm-accepts? nfa "b"))
      (assert-true (fsm-accepts? nfa "c"))))

  (define-test escaped-backslash
    ;; \\\\ in regex matches a single backslash
    (let ([nfa (regex->nfa "\\\\")])
      (assert-true (fsm-accepts? nfa "\\"))
      (assert-false (fsm-accepts? nfa ""))))

  (define-test dash-at-start-of-class
    ;; [-a] means literal dash or 'a'
    (let ([nfa (regex->nfa "[-a]")])
      (assert-true (fsm-accepts? nfa "-"))
      (assert-true (fsm-accepts? nfa "a"))))

  (define-test dash-at-end-of-class
    ;; [a-] means 'a' or literal dash
    (let ([nfa (regex->nfa "[a-]")])
      (assert-true (fsm-accepts? nfa "a"))
      (assert-true (fsm-accepts? nfa "-")))))

;;; ============================================================
;;; Malformed Input Tests (Parser Robustness)
;;; ============================================================

(test-group regex-malformed
  (define-test unclosed-group
    ;; (a should fail to parse
    (let ([result (regex-parse "(a")])
      (assert-true (left? result))))

  (define-test unclosed-class
    ;; [abc should fail to parse
    (let ([result (regex-parse "[abc")])
      (assert-true (left? result))))

  (define-test invalid-range-becomes-literals
    ;; [z-a] - when range is invalid (z > a), backtracking parses as 3 literals
    (let ([result (regex-parse "[z-a]")])
      (assert-true (right? result))
      (let ([ast (from-right result)])
        (assert-true (regex-class? ast))
        ;; Should have 3 chars: z, -, a
        (assert-equal 3 (length (regex-class-chars ast))))))

  (define-test unmatched-close-paren
    ;; a) should fail
    (let ([result (regex-parse "a)")])
      (assert-true (left? result))))

  (define-test close-bracket-needs-escape
    ;; a] fails because ] is a metachar outside class (use \] instead)
    (let ([result (regex-parse "a]")])
      (assert-true (left? result))))

  (define-test escaped-close-bracket
    ;; a\\] should match "a]"
    (let ([nfa (regex->nfa "a\\]")])
      (assert-true (fsm-accepts? nfa "a]"))))

  (define-test trailing-backslash
    ;; a\\ at end (just backslash, no escape char) should fail
    (let ([result (regex-parse "a\\")])
      (assert-true (left? result)))))

;;; ============================================================
;;; Run Tests
;;; ============================================================

(run-all-tests)
