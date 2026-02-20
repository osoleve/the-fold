(load "core/testing/test-framework.ss")
(load "lattice/fp/parsing/parser-compile.ss")

;;; ====
;;; DFA-backed Parser: Basic Matching
;;; ====

(test-group "dfa-parser-basic"

  (define-test "matches single character"
    (let* ([p (regex->parser "a")]
           [result (parser-parse p "abc")])
      (assert-true (right? result))
      (assert-equal "a" (from-right result))))

  (define-test "matches literal string"
    (let* ([p (regex->parser "hello")]
           [result (parser-parse p "hello world")])
      (assert-true (right? result))
      (assert-equal "hello" (from-right result))))

  (define-test "matches digit sequence"
    (let* ([p (regex->parser "[0-9]+")]
           [result (parser-parse p "42abc")])
      (assert-true (right? result))
      (assert-equal "42" (from-right result))))

  (define-test "matches character class"
    (let* ([p (regex->parser "[a-z]+")])
      (let ([r1 (parser-parse p "hello123")])
        (assert-true (right? r1))
        (assert-equal "hello" (from-right r1)))))

  (define-test "fails on non-matching input"
    (let* ([p (regex->parser "[0-9]+")]
           [result (parser-parse p "abc")])
      (assert-true (left? result))))

  (define-test "matches alternation"
    (let* ([p (regex->parser "cat|dog")])
      (let ([r1 (parser-parse p "cat!")])
        (assert-true (right? r1))
        (assert-equal "cat" (from-right r1)))
      (let ([r2 (parser-parse p "dog!")])
        (assert-true (right? r2))
        (assert-equal "dog" (from-right r2)))))

  (define-test "matches star"
    (let* ([p (regex->parser "a*")]
           [r1 (parser-parse p "aaa")]
           [r2 (parser-parse p "bbb")])
      (assert-true (right? r1))
      (assert-equal "aaa" (from-right r1))
      ;; Star matches empty string
      (assert-true (right? r2))
      (assert-equal "" (from-right r2))))

  (define-test "matches optional"
    (let* ([p (regex->parser "colou?r")]
           [r1 (parser-parse p "color")]
           [r2 (parser-parse p "colour")])
      (assert-true (right? r1))
      (assert-equal "color" (from-right r1))
      (assert-true (right? r2))
      (assert-equal "colour" (from-right r2)))))

;;; ====
;;; DFA-backed Parser: Composition
;;; ====

(test-group "dfa-parser-composition"

  (define-test "composes with parser-bind"
    (let* ([digits (regex->parser "[0-9]+")]
           [combined (parser-bind digits
                       (lambda (num-str)
                         (parser-bind (parser-char #\+)
                           (lambda (_)
                             (parser-map
                               (lambda (num-str2)
                                 (list num-str num-str2))
                               digits)))))]
           [result (parse-all combined "123+456")])
      (assert-true (right? result))
      (assert-equal '("123" "456") (from-right result))))

  (define-test "composes with parser-then"
    (let* ([word (regex->parser "[a-z]+")]
           [combined (parser-left word (parser-char #\!))]
           [result (parse-all combined "hello!")])
      (assert-true (right? result))
      (assert-equal "hello" (from-right result))))

  (define-test "composes with parser-or"
    (let* ([num (regex->parser "[0-9]+")]
           [word (regex->parser "[a-z]+")]
           [combined (parser-or (parser-try num) word)]
           [r1 (parser-parse combined "42")]
           [r2 (parser-parse combined "abc")])
      (assert-true (right? r1))
      (assert-equal "42" (from-right r1))
      (assert-true (right? r2))
      (assert-equal "abc" (from-right r2))))

  (define-test "composes with parser-many"
    (let* ([token (parser-left (regex->parser "[a-z]+") (parser-many (parser-char #\space)))]
           [result (parse-all (parser-many token) "hello world foo")])
      (assert-true (right? result))
      (assert-equal '("hello" "world" "foo") (from-right result)))))

;;; ====
;;; Regex AST → Parser
;;; ====

(test-group "ast-to-parser"

  (define-test "literal character"
    (let* ([ast (regex-lit #\x)]
           [p (regex-ast->parser ast)]
           [result (parser-parse p "xyz")])
      (assert-true (right? result))
      (assert-equal "x" (from-right result))))

  (define-test "sequence"
    (let* ([ast (regex-seq (list (regex-lit #\a) (regex-lit #\b)))]
           [p (regex-ast->parser ast)]
           [result (parser-parse p "abc")])
      (assert-true (right? result))
      (assert-equal "ab" (from-right result))))

  (define-test "alternation"
    (let* ([ast (regex-alt (list (regex-lit #\a) (regex-lit #\b)))]
           [p (regex-ast->parser ast)]
           [r1 (parser-parse p "a")]
           [r2 (parser-parse p "b")])
      (assert-true (right? r1))
      (assert-equal "a" (from-right r1))
      (assert-true (right? r2))
      (assert-equal "b" (from-right r2))))

  (define-test "star produces string"
    (let* ([ast (regex-star (regex-lit #\a))]
           [p (regex-ast->parser ast)]
           [result (parser-parse p "aaab")])
      (assert-true (right? result))
      (assert-equal "aaa" (from-right result))))

  (define-test "plus requires at least one"
    (let* ([ast (regex-plus (regex-lit #\a))]
           [p (regex-ast->parser ast)]
           [r1 (parser-parse p "aaa")]
           [r2 (parser-parse p "bbb")])
      (assert-true (right? r1))
      (assert-equal "aaa" (from-right r1))
      (assert-true (left? r2))))

  (define-test "character class"
    (let* ([ast (regex-class (list #\a #\b #\c) #f)]
           [p (regex-ast->parser ast)]
           [r1 (parser-parse p "b")]
           [r2 (parser-parse p "x")])
      (assert-true (right? r1))
      (assert-equal "b" (from-right r1))
      (assert-true (left? r2))))

  (define-test "negated character class"
    (let* ([ast (regex-class (list #\a #\b #\c) #t)]
           [p (regex-ast->parser ast)]
           [r1 (parser-parse p "x")]
           [r2 (parser-parse p "a")])
      (assert-true (right? r1))
      (assert-equal "x" (from-right r1))
      (assert-true (left? r2)))))

;;; ====
;;; Compiled Regex (Dual Form)
;;; ====

(test-group "compiled-regex"

  (define-test "compile-regex creates valid form"
    (let ([crx (compile-regex "[0-9]+")])
      (assert-true (compiled-regex? crx))
      (assert-equal "[0-9]+" (compiled-regex-pattern crx))))

  (define-test "DFA matching"
    (let ([crx (compile-regex "ab+")])
      (assert-true (compiled-regex-matches? crx "abbb"))
      (assert-false (compiled-regex-matches? crx "a"))
      (assert-false (compiled-regex-matches? crx "bb"))))

  (define-test "parser matching returns value"
    (let* ([crx (compile-regex "[a-z]+")]
           [result (compiled-regex-parse crx "hello world")])
      (assert-true (right? result))
      (assert-equal "hello" (from-right result))))

  (define-test "DFA and parser agree"
    (let ([crx (compile-regex "a(b|c)*d")])
      ;; DFA says yes for full match
      (assert-true (compiled-regex-matches? crx "abcbd"))
      ;; Parser returns the matched prefix
      (let ([result (compiled-regex-parse crx "abcbd rest")])
        (assert-true (right? result))
        (assert-equal "abcbd" (from-right result)))))

  (define-test "parser from compiled-regex is composable"
    (let* ([num-rx (compile-regex "[0-9]+")]
           [p (compiled-regex-parser num-rx)]
           [combined (parser-bind p
                       (lambda (n) (parser-pure (string-length n))))]
           [result (parser-parse combined "12345")])
      (assert-true (right? result))
      (assert-equal 5 (from-right result)))))

;;; ====
;;; Position Tracking
;;; ====

(test-group "position-tracking"

  (define-test "advances column correctly"
    (let* ([p (regex->parser "[a-z]+")]
           ;; Use run-parser to inspect final state
           [state (parser-initial-state "hello world")]
           [result (run-parser p state)])
      (assert-true (right? result))
      (let ([new-state (cdr (from-right result))])
        ;; After "hello" (5 chars), column should be 6
        (assert-equal 5 (parser-state-index new-state))
        (assert-equal 6 (pos-col (parser-state-pos new-state))))))

  (define-test "handles newlines in match"
    (let* ([p (regex->parser "a.b")]  ;; dot matches newline in our universe
           [state (parser-initial-state (string-append "a" (string #\newline) "b rest"))]
           [result (run-parser p state)])
      (assert-true (right? result))
      (let ([new-state (cdr (from-right result))])
        (assert-equal 3 (parser-state-index new-state))
        ;; After "a\nb", line should be 2
        (assert-equal 2 (pos-line (parser-state-pos new-state)))))))

;;; ====
;;; Edge Cases
;;; ====

(test-group "edge-cases"

  (define-test "empty pattern matches empty prefix"
    (let* ([p (dfa->parser (fsm-epsilon-lang) "empty")]
           [result (parser-parse p "anything")])
      (assert-true (right? result))
      (assert-equal "" (from-right result))))

  (define-test "full match via parse-all"
    (let* ([p (regex->parser "[0-9]+")]
           [r1 (parse-all p "123")]
           [r2 (parse-all p "123abc")])
      (assert-true (right? r1))
      (assert-equal "123" (from-right r1))
      ;; parse-all requires EOF — "abc" remains so this fails
      (assert-true (left? r2))))

  (define-test "regex->combinator-parser works"
    (let* ([p (regex->combinator-parser "[0-9]+")]
           [result (parser-parse p "42abc")])
      (assert-true (right? result))
      (assert-equal "42" (from-right result)))))

(run-all-tests)
