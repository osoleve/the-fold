;;; fabric/stitches/fp/test-parser-dsl.ss - Tests for DSL Toolkit

(load "test-framework.ss")
(load "fp/parser-dsl.ss")

(display "
")
(display "==============================================================
")
(display "         DSL TOOLKIT TESTS
")
(display "==============================================================
")

;;; ============================================================
;;; Higher-Order Combinator Tests
;;; ============================================================

(test-group higher-order-combinators
            ;; chainl1 - left associative
            (define-test chainl1-basic-test
              (let* ([num (parser-map string->number (parser-map list->string (some digit)))]
                     [add-op (parser-then (char #\+) (parser-pure (lambda (a b) (list '+ a b))))]
                     [p (chainl1 num add-op)]
                     [result (parse p "1+2+3")])
                    (assert-true (right? result))
                    (assert-equal '(+ (+ 1 2) 3) (from-right result))))
            
            (define-test chainl1-single-test
              (let* ([num (parser-map string->number (parser-map list->string (some digit)))]
                     [add-op (parser-then (char #\+) (parser-pure +))]
                     [p (chainl1 num add-op)]
                     [result (parse p "42")])
                    (assert-true (right? result))
                    (assert-equal 42 (from-right result))))
            
            ;; chainr1 - right associative
            (define-test chainr1-basic-test
              (let* ([num (parser-map string->number (parser-map list->string (some digit)))]
                     [exp-op (parser-then (char #\^) (parser-pure (lambda (a b) (list '^ a b))))]
                     [p (chainr1 num exp-op)]
                     [result (parse p "2^3^4")])
                    (assert-true (right? result))
                    (assert-equal '(^ 2 (^ 3 4)) (from-right result))))
            
            ;; chainl with default
            (define-test chainl-empty-test
              (let* ([num (parser-map string->number (parser-map list->string (some digit)))]
                     [add-op (parser-then (char #\+) (parser-pure +))]
                     [p (chainl (parser-fail "no number") add-op 0)]
                     [result (parse p "")])
                    (assert-true (right? result))
                    (assert-equal 0 (from-right result))))
            
            ;; skip-many
            (define-test skip-many-test
              (let* ([p (parser-then (skip-many space) (some letter))]
                     [result (parse p "   hello")])
                    (assert-true (right? result))
                    (assert-equal 5 (length (from-right result)))))
            
            ;; skip-some
            (define-test skip-some-test
              (let* ([p (parser-then (skip-some space) (some letter))]
                     [result (parse p "  hi")])
                    (assert-true (right? result))
                    (assert-equal 2 (length (from-right result)))))
            
            (define-test skip-some-fail-test
              (let* ([p (parser-then (skip-some space) (some letter))]
                     [result (parse p "hi")])
                    (assert-true (left? result))))
            
            ;; sep-end-by
            (define-test sep-end-by-test
              (let* ([num natural]
                     [p (sep-end-by num (char #\,))]
                     [result (parse p "1,2,3,")])
                    (assert-true (right? result))
                    (assert-equal '(1 2 3) (from-right result))))
            
            (define-test sep-end-by-no-trailing-test
              (let* ([num natural]
                     [p (sep-end-by num (char #\,))]
                     [result (parse p "1,2,3")])
                    (assert-true (right? result))
                    (assert-equal '(1 2 3) (from-right result))))
            
            ;; at-most - using letter-a defined via integer->char
            (define-test at-most-test
              (let* ([letter-a (char (integer->char 97))]  ; 'a'
                     [p (at-most 3 letter-a)]
                     [result (parse p "aaaaa")])
                    (assert-true (right? result))
                    (assert-equal 3 (length (from-right result)))))
            
            ;; at-least
            (define-test at-least-test
              (let* ([letter-a (char (integer->char 97))]  ; 'a'
                     [p (at-least 2 letter-a)]
                     [result (parse p "aaaa")])
                    (assert-true (right? result))
                    (assert-equal 4 (length (from-right result)))))
            
            (define-test at-least-fail-test
              (let* ([letter-a (char (integer->char 97))]  ; 'a'
                     [p (at-least 3 letter-a)]
                     [result (parse p "aa")])
                    (assert-true (left? result))))
            
            ;; range-of
            (define-test range-of-test
              (let* ([letter-x (char (integer->char 120))]  ; 'x'
                     [p (range-of 2 4 letter-x)]
                     [result (parse p "xxx")])
                    (assert-true (right? result))
                    (assert-equal 3 (length (from-right result)))))
            
            ;; with-pos
            (define-test with-pos-test
              (let* ([p (with-pos natural)]
                     [result (parse p "42")])
                    (assert-true (right? result))
                    (let ([val-pos (from-right result)])
                         (assert-equal 42 (car val-pos))
                         (assert-true (pos? (cdr val-pos))))))
            
            ;; with-span
            (define-test with-span-test
              (let* ([p (with-span (string-parser "hello"))]
                     [result (parse p "hello")])
                    (assert-true (right? result))
                    (let ([result-list (from-right result)])
                         (assert-equal "hello" (car result-list))
                         (assert-true (pos? (cadr result-list)))
                         (assert-true (pos? (caddr result-list)))))))

;;; ============================================================
;;; AST Node Tests
;;; ============================================================

(test-group ast-nodes
            (define-test make-ast-test
              (let ([node (make-ast 'number no-span 42)])
                   (assert-true (ast? node))
                   (assert-equal 'number (ast-tag node))
                   (assert-equal 42 (ast-ref node 0))))
            
            (define-test ast-span-test
              (let* ([start (make-pos 1 1 0)]
                     [end (make-pos 1 5 4)]
                     [span (make-source-span start end)]
                     [node (make-ast 'ident span "foo")])
                    (assert-true (source-span? (ast-span node)))
                    (assert-equal start (span-start (ast-span node)))
                    (assert-equal end (span-end (ast-span node)))))
            
            (define-test parse-ast-test
              (let* ([p (parse-ast 'num natural)]
                     [result (parse p "42")])
                    (assert-true (right? result))
                    (let ([node (from-right result)])
                         (assert-true (ast? node))
                         (assert-equal 'num (ast-tag node))
                         (assert-equal 42 (ast-ref node 0)))))
            
            (define-test merge-spans-test
              (let* ([s1 (make-source-span (make-pos 1 1 0) (make-pos 1 5 4))]
                     [s2 (make-source-span (make-pos 1 10 9) (make-pos 1 15 14))]
                     [merged (merge-spans s1 s2)])
                    (assert-equal (make-pos 1 1 0) (span-start merged))
                    (assert-equal (make-pos 1 15 14) (span-end merged)))))

;;; ============================================================
;;; Expression Parser Builder Tests
;;; ============================================================

(test-group expr-parser
            (define-test simple-expr-parser-test
              (let* ([num (lexeme natural)]
                     [add-op (infix-l (parser-then (symbol "+") (parser-pure '+))
                                      (lambda (a b) (list '+ a b)))]
                     [mul-op (infix-l (parser-then (symbol "*") (parser-pure '*))
                                      (lambda (a b) (list '* a b)))]
                     [table (list (list mul-op) (list add-op))]
                     [expr (build-expr-parser table num)]
                     [result (parse-all expr "1 + 2 * 3")])
                    (assert-true (right? result))
                    (assert-equal '(+ 1 (* 2 3)) (from-right result))))
            
            (define-test right-assoc-expr-test
              (let* ([num (lexeme natural)]
                     [exp-op (infix-r (parser-then (symbol "^") (parser-pure '^))
                                      (lambda (a b) (list '^ a b)))]
                     [table (list (list exp-op))]
                     [expr (build-expr-parser table num)]
                     [result (parse-all expr "2 ^ 3 ^ 4")])
                    (assert-true (right? result))
                    (assert-equal '(^ 2 (^ 3 4)) (from-right result)))))

;;; ============================================================
;;; Token Parser Tests
;;; ============================================================

(test-group token-parser
            (define test-lang
              (list (cons 'reserved-names '("if" "then" "else" "let" "in"))
                    (cons 'reserved-ops '("+" "-" "*" "/" "=" "=="))
                    (cons 'comment-line ";")))
            
            (define tp (make-token-parser test-lang))
            
            (define-test identifier-test
              (let* ([ident (tp-get tp 'identifier)]
                     [result (parse ident "foo")])
                    (assert-true (right? result))
                    (assert-equal "foo" (from-right result))))
            
            (define-test identifier-reserved-fail-test
              (let* ([ident (tp-get tp 'identifier)]
                     [result (parse ident "if")])
                    (assert-true (left? result))))
            
            (define-test reserved-test
              (let* ([reserved (tp-get tp 'reserved)]
                     [result (parse (reserved "if") "if")])
                    (assert-true (right? result))
                    (assert-equal "if" (from-right result))))
            
            (define-test number-test
              (let* ([num (tp-get tp 'number)]
                     [result (parse num "42")])
                    (assert-true (right? result))
                    (assert-equal 42 (from-right result))))
            
            (define-test parens-test
              (let* ([parens-p (tp-get tp 'parens)]
                     [num (tp-get tp 'number)]
                     [result (parse (parens-p num) "(42)")])
                    (assert-true (right? result))
                    (assert-equal 42 (from-right result))))
            
            (define-test comma-sep-test
              (let* ([comma-sep-p (tp-get tp 'comma-sep)]
                     [num (tp-get tp 'number)]
                     [result (parse (comma-sep-p num) "1, 2, 3")])
                    (assert-true (right? result))
                    (assert-equal '(1 2 3) (from-right result)))))

;;; ============================================================
;;; Keyword Alias Tests
;;; ============================================================

(test-group keyword-utilities
            (define-test kw-boundary-test
              (let* ([p (kw-boundary "north")]
                     [result (parse p "north ")])
                    (assert-true (right? result))
                    (assert-equal "north" (from-right result))))
            
            (define-test kw-boundary-fail-test
              (let* ([p (kw-boundary "north")]
                     [result (parse p "northern")])
                    (assert-true (left? result))))
            
            (define-test keyword-aliases-test
              (let* ([directions
                      (keyword-aliases
                       '((north "north" "n")
                         (south "south" "s"))
                       (lambda (d) (list 'move d)))]
                     [r1 (parse directions "north ")]
                     [r2 (parse directions "s ")])
                    (assert-true (right? r1))
                    (assert-equal '(move north) (from-right r1))
                    (assert-true (right? r2))
                    (assert-equal '(move south) (from-right r2))))
            
            (define-test reserved-words-test
              (let* ([p (reserved-words '("if" "then" "else"))]
                     [r1 (parse p "if ")]
                     [r2 (parse p "then ")])
                    (assert-true (right? r1))
                    (assert-true (right? r2)))))

;;; ============================================================
;;; String Escape Tests
;;; ============================================================

(test-group string-escapes
            (define-test double-quoted-simple-test
              (let ([result (parse double-quoted-string "\"hello\"")])
                   (assert-true (right? result))
                   (assert-equal "hello" (from-right result))))
            
            (define-test double-quoted-newline-test
              (let ([result (parse double-quoted-string "\"hello\nworld\"")])
                   (assert-true (right? result))
                   (assert-equal 11 (string-length (from-right result)))))
            
            (define-test double-quoted-tab-test
              (let ([result (parse double-quoted-string "\"hello\tworld\"")])
                   (assert-true (right? result))
                   (assert-equal 11 (string-length (from-right result)))))
            
            (define-test single-quoted-test
              (let ([result (parse single-quoted-string "'hello'")])
                   (assert-true (right? result))
                   (assert-equal "hello" (from-right result)))))

;;; ============================================================
;;; Command Parser Tests
;;; ============================================================

(test-group command-utilities
            (define-test command-test
              (let* ([p (command "look" (parser-pure 'look-intent))]
                     [result (parse p "look")])
                    (assert-true (right? result))
                    (assert-equal 'look-intent (from-right result))))
            
            (define-test command-with-arg-test
              (let* ([p (command "take" (lexeme identifier))]
                     [result (parse p "take sword")])
                    (assert-true (right? result))
                    (assert-equal "sword" (from-right result))))
            
            (define-test command-with-aliases-test
              (let* ([p (command-with-aliases '("quit" "exit" "q") (parser-pure 'quit))]
                     [r1 (parse-all p "quit")]
                     [r2 (parse-all p "exit")]
                     [r3 (parse-all p "q")])
                    (assert-true (right? r1))
                    (assert-true (right? r2))
                    (assert-true (right? r3))))
            
            (define-test rest-trimmed-test
              (let ([result (parse rest-trimmed "  hello world  ")])
                   (assert-true (right? result))
                   (assert-equal "hello world" (from-right result))))
            
            (define-test string-trim-both-test
              (assert-equal "hello" (string-trim-both "  hello  "))
              (assert-equal "hello world" (string-trim-both "hello world"))
              (assert-equal "" (string-trim-both "   "))))

;;; ============================================================
;;; Language Style Tests
;;; ============================================================

(test-group language-styles
            (define-test scheme-style-exists-test
              (assert-true (pair? scheme-style)))
            
            (define-test c-style-exists-test
              (assert-true (pair? c-style)))
            
            (define-test haskell-style-exists-test
              (assert-true (pair? haskell-style))))

;;; ============================================================
;;; Utility Tests
;;; ============================================================

(test-group utilities
            (define-test string-split-test
              (assert-equal '("a" "b" "c") (string-split "a,b,c" #\,)))
            
            (define-test string-split-no-delim-test
              (assert-equal '("hello") (string-split "hello" #\,)))
            
            (define-test make-string-test
              (assert-equal "xxxx" (make-string 4 #\x)))
            
            (define-test make-list-test
              (assert-equal '(1 1 1) (make-list 3 1))))

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
[SUCCESS] All DSL toolkit tests passed.
")
    (display "
[FAILURE] Some DSL toolkit tests failed.
"))
