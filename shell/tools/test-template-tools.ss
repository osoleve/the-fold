;;; shell/tools/test-template-tools.ss — Tests for Template Shell Tools
;;;
;;; Tests for template-session.ss and template-parser.ss

(load "core/testing/test-framework.ss")
(load "shell/tools/template-parser.ss")

;;; ============================================================
;;; Tokenizer Tests
;;; ============================================================

(test-group tokenizer

  (define-test "tokenize basic symbols"
    (assert-equal '(foo bar baz) (tokenize "foo bar baz")))

  (define-test "tokenize with numbers"
    (assert-equal '(+ 1 2) (tokenize "+ 1 2")))

  (define-test "tokenize with holes"
    (assert-equal '($a $b $c) (tokenize "$a $b $c")))

  (define-test "tokenize strings with spaces"
    (assert-equal '(print "hello world") (tokenize "print \"hello world\"")))

  (define-test "tokenize booleans"
    (assert-equal '(if #t 1 0) (tokenize "if #t 1 0")))

  (define-test "tokenize quoted forms"
    (assert-equal '(x := 'foo) (tokenize "x := 'foo")))

  (define-test "tokenize nested parens"
    (assert-equal '(define (foo x) (+ x 1)) (tokenize "define (foo x) (+ x 1)")))

  (define-test "tokenize characters"
    (assert-equal '(char #\a) (tokenize "char #\\a"))))

;;; ============================================================
;;; Implicit Parens Tests
;;; ============================================================

(test-group implicit-parens

  (define-test "single token stays unwrapped"
    (assert-equal 'foo (apply-implicit-parens '(foo))))

  (define-test "multiple tokens get wrapped"
    (assert-equal '(a b c) (apply-implicit-parens '(a b c))))

  (define-test "empty list stays empty"
    (assert-equal '() (apply-implicit-parens '()))))

;;; ============================================================
;;; Session Tests
;;; ============================================================

(test-group session

  (define-test "ts-start creates session"
    (ts-reset)
    (ts-start '($a $b))
    (assert-true (ts-active?))
    (assert-equal '($a $b) (ts-holes)))

  (define-test "ts-fill replaces hole"
    (ts-reset)
    (ts-start '(+ $x $y))
    (ts-fill '$x 1)
    (assert-equal '($y) (ts-holes)))

  (define-test "ts-fill propagates new holes"
    (ts-reset)
    (ts-start '($outer))
    (ts-fill '$outer '($inner1 $inner2))
    (assert-equal '($inner1 $inner2) (ts-holes)))

  (define-test "ts-undo reverts fill"
    (ts-reset)
    (ts-start '($a $b))
    (ts-fill '$a 1)
    (assert-equal '($b) (ts-holes))
    (ts-undo)
    (assert-equal '($a $b) (ts-holes)))

  (define-test "ts-compile succeeds when complete"
    (ts-reset)
    (ts-start '(+ 1 2))
    (let ([result (ts-compile)])
      (assert-equal '(+ 1 2) result)))

  (define-test "ts-reset clears session"
    (ts-reset)
    (ts-start '($x))
    (assert-true (ts-active?))
    (ts-reset)
    (assert-false (ts-active?))))

;;; ============================================================
;;; Parser Integration Tests
;;; ============================================================

(test-group parser-integration

  (define-test "parse template definition"
    (ts-reset)
    (tp-parse "define $name $body")
    (assert-true (ts-active?))
    (assert-equal '($name $body) (ts-holes)))

  (define-test "parse assignment"
    (ts-reset)
    (tp-parse "print $msg")
    (tp-parse "$msg := \"hello\"")
    (assert-equal '() (ts-holes)))

  (define-test "parse with implicit parens"
    (ts-reset)
    (tp-parse "$x")
    (tp-parse "$x := + 1 2")
    (let ([result (ts-compile)])
      (assert-equal '(+ 1 2) result)))

  (define-test "full workflow"
    (ts-reset)
    (tp-parse "if $cond $then $else")
    (tp-parse "$cond := = x 0")
    (tp-parse "$then := 1")
    (tp-parse "$else := 2")
    (let ([result (ts-compile)])
      (assert-equal '(if (= x 0) 1 2) result))))

;;; ============================================================
;;; String Utilities Tests
;;; ============================================================

(test-group string-utils

  (define-test "string-find finds needle"
    (assert-equal 4 (string-find "hello world" "o" 0)))

  (define-test "string-find respects start position"
    (assert-equal 7 (string-find "hello world" "o" 5)))

  (define-test "string-find returns #f when not found"
    (assert-false (string-find "hello" "z" 0)))

  (define-test "string-split basic"
    (assert-equal '("a" "b" "c") (string-split "a---b---c" "---")))

  (define-test "string-split no delimiter"
    (assert-equal '("abc") (string-split "abc" "---")))

  (define-test "string-trim removes whitespace"
    (assert-equal "hello" (string-trim "  hello  "))))

;;; ============================================================
;;; Batch Mode Tests
;;; ============================================================

(test-group batch-mode

  (define-test "tp-batch single definition"
    (let ([result (tp-batch "define (f x) (+ x 1)")])
      (assert-equal '(define (f x) (+ x 1)) result)))

  (define-test "tp-batch multiple definitions"
    (let ([result (tp-batch "define (f x) (+ x 1) --- define (g x) (f x)")])
      (assert-equal '(begin (define (f x) (+ x 1)) (define (g x) (f x))) result)))

  (define-test "tp-batch with whitespace"
    (let ([result (tp-batch "  + 1 2  ---  * 3 4  ")])
      (assert-equal '(begin (+ 1 2) (* 3 4)) result))))

;;; ============================================================
;;; Run All Tests
;;; ============================================================

(display "\n=== Template Shell Tools Tests ===\n\n")
(run-all-tests)
