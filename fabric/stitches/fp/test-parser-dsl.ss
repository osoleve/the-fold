;;; fabric/stitches/fp/test-parser-dsl.ss — Tests for DSL Toolkit

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
                    ;; Left associative: ((1 + 2) + 3)
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
                    ;; Right associative: (2 ^ (3 ^ 4))
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
                    (assert-equal '(#\h # #\l #\l #\o) (from-right result))))
            
            ;; skip-some
            (define-test skip-some-test
              (let* ([p (parser-then (skip-some space) (some letter))]
                     [result (parse p "  hi")])
                    (assert-true (right? result))
                    (assert-equal '(#\h #\i) (from-right result))))
            
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
            
            ;; at-most
            (define-test at-most-test
              (let* ([p (at-most 3 (char #))]
                     [result (parse p "aaaaa")])
                    (assert-true (right? result))
                    (assert-equal '(# # #) (from-right result))))
            
            (define-test at-most-fewer-test
              (let* ([p (at-most 5 (char #))]
                     [result (parse p "aa")])
                    (assert-true (right? result))
                    (assert-equal '(# #) (from-right result))))
            
            ;; at-least
            (define-test at-least-test
              (let* ([p (at-least 2 (char #))]
                     [result (parse p "aaaa")])
                    (assert-true (right? result))
                    (assert-equal '(# # # #) (from-right result))))
            
            (define-test at-least-fail-test
              (let* ([p (at-least 3 (char #))]
                     [result (parse p "aa")])
                    (assert-true (left? result))))
            
            ;; range-of
            (define-test range-of-test
              (let* ([p (range-of 2 4 (char #\x))]
                     [result (parse p "xxx")])
                    (assert-true (right? result))
                    (assert-equal '(#\x #\x #\x) (from-right result))))
            
            ;; fold-p
            (define-test fold-p-test
              (let* ([p (fold-p + 0 natural)]
                     [result (parse (parser-left p eof) "123")])
                    (assert-true (right? result))
                    (assert-equal 123 (from-right result))))
            
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
            ;; Simple arithmetic with precedence
            (define-test simple-expr-parser-test
              (let* ([num (lexeme natural)]
                     [add-op (infix-l (parser-then (symbol "+") (parser-pure '+))
                                      (lambda (a b) (list '+ a b)))]
                     [mul-op (infix-l (parser-then (symbol "*") (parser-pure '*))
                                      (lambda (a b) (list '* a b)))]
                     [table (list (list mul-op)   ; higher precedence
                                  (list add-op))]  ; lower precedence
                     [expr (build-expr-parser table num)]
                     [result (parse-all expr "1 + 2 * 3")])
                    (assert-true (right? result))
                    ;; * binds tighter: 1 + (2 * 3)
                    (assert-equal '(+ 1 (* 2 3)) (from-right result))))
            
            (define-test expr-parser-parens-test
              (let* ([num (lexeme natural)]
                     [expr-ref #f]
                     [atom (parser-or (parens (make-parser (lambda (s) (run-parser expr-ref s))))
                                      num)]
                     [add-op (infix-l (parser-then (symbol "+") (parser-pure '+))
                                      (lambda (a b) (list '+ a b)))]
                     [mul-op (infix-l (parser-then (symbol "*") (parser-pure '*))
                                      (lambda (a b) (list '* a b)))]
                     [table (list (list mul-op)
                                  (list add-op))]
                     [expr (build-expr-parser table atom)])
                    (set! expr-ref expr)
                    (let ([result (parse-all expr "(1 + 2) * 3")])
                         (assert-true (right? result))
                         ;; Parens override: (1 + 2) * 3
                         (assert-equal '(* (+ 1 2) 3) (from-right result)))))
            
            (define-test right-assoc-expr-test
              (let* ([num (lexeme natural)]
                     [exp-op (infix-r (parser-then (symbol "^") (parser-pure '^))
                                      (lambda (a b) (list '^ a b)))]
                     [table (list (list exp-op))]
                     [expr (build-expr-parser table num)]
                     [result (parse-all expr "2 ^ 3 ^ 4")])
                    (assert-true (right? result))
                    ;; Right associative: 2 ^ (3 ^ 4)
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
            
            (define-test reserved-not-prefix-test
              (let* ([reserved (tp-get tp 'reserved)]
                     [result (parse (reserved "if") "iff")])
                    (assert-true (left? result))))
            
            (define-test number-test
              (let* ([num (tp-get tp 'number)]
                     [result (parse num "42")])
                    (assert-true (right? result))
                    (assert-equal 42 (from-right result))))
            
            (define-test string-lit-test
              (let* ([str-p (tp-get tp 'string-lit)]
                     [result (parse str-p "\"hello\"")])
                    (assert-true (right? result))
                    (assert-equal "hello" (from-right result))))
            
            (define-test string-escape-test
              (let* ([str-p (tp-get tp 'string-lit)]
                     [result (parse str-p "\"hello\nworld\"")])
                    (assert-true (right? result))
                    (assert-equal "hello
world" (from-right result))))
            
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
