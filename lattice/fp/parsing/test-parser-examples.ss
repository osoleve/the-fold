;;; fabric/stitches/fp/test-parser-examples.ss — Tests for Example Parsers

;;; NOTE: Run from fabric/stitches directory

(load "core/test-framework.ss")
(load "lattice/fp/parsing/parser-examples.ss")

;;; Helper for checking parse success
(define (assert-parses-to parser input expected)
  (let ([result (parse-all parser input)])
       (unless (and (right? result)
                    (equal? (from-right result) expected))
               (set! *tests-failed* (+ *tests-failed* 1))
               (display "    X ")
               (display *current-test-name*)
               (newline)
               (display "      Input:    ")
               (write input)
               (newline)
               (display "      Expected: ")
               (write expected)
               (newline)
               (display "      Got:      ")
               (if (right? result)
                   (write (from-right result))
                   (display (format-error (from-left result))))
               (newline))))

;;; Helper for checking parse failure
(define (assert-parse-fails parser input)
  (let ([result (parse-all parser input)])
       (unless (left? result)
               (set! *tests-failed* (+ *tests-failed* 1))
               (display "    X ")
               (display *current-test-name*)
               (newline)
               (display "      Input:    ")
               (write input)
               (newline)
               (display "      Expected: parse failure")
               (newline)
               (display "      Got:      ")
               (write (from-right result))
               (newline))))

(display "
")
(display "====
")
(display "         PARSER EXAMPLES TESTS
")
(display "====
")

;;; ====
;;; JSON Parser Tests
;;; ====

(test-group json-parser
            (define-test json-null-test
              (let ([result (parse-json "null")])
                   (assert-true (right? result))
                   (assert-equal 'json-null (from-right result))))
            
            (define-test json-bool-test
              (let ([r1 (parse-json "true")]
                    [r2 (parse-json "false")])
                   (assert-true (right? r1))
                   (assert-true (from-right r1))
                   (assert-true (right? r2))
                   (assert-false (from-right r2))))
            
            (define-test json-number-test
              (let ([r1 (parse-json "42")]
                    [r2 (parse-json "-3.14")]
                    [r3 (parse-json "0")])
                   (assert-true (right? r1))
                   (assert-equal 42 (from-right r1))
                   (assert-true (right? r2))
                   (assert-equal -3.14 (from-right r2))
                   (assert-true (right? r3))
                   (assert-equal 0 (from-right r3))))
            
            (define-test json-string-test
              (let ([r1 (parse-json "\"hello\"")])
                   (assert-true (right? r1))
                   (assert-equal "hello" (from-right r1))))
            
            (define-test json-array-test
              (let ([r1 (parse-json "[1, 2, 3]")]
                    [r2 (parse-json "[]")]
                    [r3 (parse-json "[true, null, \"hello\"]")])
                   (assert-true (right? r1))
                   (assert-equal '(1 2 3) (from-right r1))
                   (assert-true (right? r2))
                   (assert-equal '() (from-right r2))
                   (assert-true (right? r3))
                   (assert-equal '(#t json-null "hello") (from-right r3))))
            
            (define-test json-object-test
              (let ([r1 (parse-json "{\"a\": 1, \"b\": 2}")]
                    [r2 (parse-json "{}")])
                   (assert-true (right? r1))
                   (assert-equal '(("a" . 1) ("b" . 2)) (from-right r1))
                   (assert-true (right? r2))
                   (assert-equal '() (from-right r2))))
            
            (define-test json-nested-test
              (let ([result (parse-json "{\"user\": {\"name\": \"Alice\", \"tags\": [1, 2]}}")])
                   (assert-true (right? result))
                   (let ([obj (from-right result)])
                        (assert-equal 1 (length obj))
                        (let* ([user-pair (car obj)]
                               [user (cdr user-pair)])
                              (assert-equal "user" (car user-pair))
                              (assert-true (pair? user)))))))

;;; ====
;;; S-Expression Parser Tests
;;; ====

(test-group sexp-parser
            (define-test sexp-number-test
              (let ([r1 (parse-sexp "42")]
                    [r2 (parse-sexp "-3.14")])
                   (assert-true (right? r1))
                   (assert-equal 42 (from-right r1))
                   (assert-true (right? r2))
                   (assert-equal -3.14 (from-right r2))))
            
            (define-test sexp-symbol-test
              (let ([r1 (parse-sexp "hello")]
                    [r2 (parse-sexp "foo-bar")]
                    [r3 (parse-sexp "+")])
                   (assert-true (right? r1))
                   (assert-equal 'hello (from-right r1))
                   (assert-true (right? r2))
                   (assert-equal 'foo-bar (from-right r2))
                   (assert-true (right? r3))
                   (assert-equal '+ (from-right r3))))
            
            (define-test sexp-string-test
              (let ([r1 (parse-sexp "\"hello\"")])
                   (assert-true (right? r1))
                   (assert-equal "hello" (from-right r1))))
            
            (define-test sexp-boolean-test
              (let ([r1 (parse-sexp "#t")]
                    [r2 (parse-sexp "#f")])
                   (assert-true (right? r1))
                   (assert-true (from-right r1))
                   (assert-true (right? r2))
                   (assert-false (from-right r2))))
            
            (define-test sexp-list-test
              (let ([r1 (parse-sexp "(a b c)")]
                    [r2 (parse-sexp "(1 2 3)")]
                    [r3 (parse-sexp "()")])
                   (assert-true (right? r1))
                   (assert-equal '(a b c) (from-right r1))
                   (assert-true (right? r2))
                   (assert-equal '(1 2 3) (from-right r2))
                   (assert-true (right? r3))
                   (assert-equal '() (from-right r3))))
            
            (define-test sexp-nested-test
              (let ([result (parse-sexp "(define (square x) (* x x))")])
                   (assert-true (right? result))
                   (assert-equal '(define (square x) (* x x)) (from-right result))))
            
            (define-test sexp-quote-test
              (let ([r1 (parse-sexp "'x")]
                    [r2 (parse-sexp "'(a b c)")])
                   (assert-true (right? r1))
                   (assert-equal '(quote x) (from-right r1))
                   (assert-true (right? r2))
                   (assert-equal '(quote (a b c)) (from-right r2))))
            
            (define-test sexp-quasiquote-test
              (let ([result (parse-sexp "`(a ,b ,@c)")])
                   (assert-true (right? result))
                   (assert-equal '(quasiquote (a (unquote b) (unquote-splicing c)))
                                 (from-right result))))
            
            (define-test sexp-comment-test
              (let ([result (parse-sexp "(a ; comment
b)")])
                   (assert-true (right? result))
                   (assert-equal '(a b) (from-right result))))
            
            (define-test sexp-multiple-test
              (let ([result (parse-sexps "(a b) (c d)")])
                   (assert-true (right? result))
                   (assert-equal '((a b) (c d)) (from-right result)))))

;;; ====
;;; Arithmetic Expression Parser Tests
;;; ====

(test-group arith-parser
            (define-test arith-number-test
              (let ([result (parse-arith "42")])
                   (assert-true (right? result))
                   (assert-equal 42 (from-right result))))
            
            (define-test arith-add-test
              (let ([result (parse-arith "1 + 2")])
                   (assert-true (right? result))
                   (assert-equal '(+ 1 2) (from-right result))))
            
            (define-test arith-sub-test
              (let ([result (parse-arith "5 - 3")])
                   (assert-true (right? result))
                   (assert-equal '(- 5 3) (from-right result))))
            
            (define-test arith-mul-test
              (let ([result (parse-arith "2 * 3")])
                   (assert-true (right? result))
                   (assert-equal '(* 2 3) (from-right result))))
            
            (define-test arith-div-test
              (let ([result (parse-arith "10 / 2")])
                   (assert-true (right? result))
                   (assert-equal '(/ 10 2) (from-right result))))
            
            (define-test arith-precedence-test
              ;; * binds tighter than +
              (let ([result (parse-arith "1 + 2 * 3")])
                   (assert-true (right? result))
                   (assert-equal '(+ 1 (* 2 3)) (from-right result))))
            
            (define-test arith-left-assoc-test
              ;; Left associative: 1 - 2 - 3 = (1 - 2) - 3
              (let ([result (parse-arith "1 - 2 - 3")])
                   (assert-true (right? result))
                   (assert-equal '(- (- 1 2) 3) (from-right result))))
            
            (define-test arith-parens-test
              (let ([result (parse-arith "(1 + 2) * 3")])
                   (assert-true (right? result))
                   (assert-equal '(* (+ 1 2) 3) (from-right result))))
            
            (define-test arith-unary-test
              (let ([result (parse-arith "-5")])
                   (assert-true (right? result))
                   (assert-equal '(- 5) (from-right result))))
            
            (define-test arith-complex-test
              (let ([result (parse-arith "1 + 2 * 3 - 4 / 2")])
                   (assert-true (right? result))
                   ;; Should be: (- (+ 1 (* 2 3)) (/ 4 2))
                   (assert-equal '(- (+ 1 (* 2 3)) (/ 4 2)) (from-right result))))
            
            (define-test arith-eval-test
              (let ([result (parse-arith "1 + 2 * 3 - 4 / 2")])
                   (assert-true (right? result))
                   ;; 1 + 6 - 2 = 5
                   (assert-equal 5 (eval-arith (from-right result))))))

;;; ====
;;; INI File Parser Tests
;;; ====

(test-group ini-parser
            (define-test ini-simple-test
              (let ([result (parse-ini "[section]
key=value
")])
                   (assert-true (right? result))
                   (let ([ini (from-right result)])
                        (assert-equal 1 (length ini))
                        (assert-equal "section" (caar ini))
                        (assert-equal '(("key" . "value")) (cdar ini)))))
            
            (define-test ini-multiple-sections-test
              (let ([result (parse-ini "[db]
host=localhost
port=5432

[app]
debug=true
")])
                   (assert-true (right? result))
                   (let ([ini (from-right result)])
                        (assert-equal 2 (length ini))
                        (let ([db (assoc "db" ini)]
                              [app (assoc "app" ini)])
                             (assert-true (pair? db))
                             (assert-true (pair? app))
                             (assert-equal "localhost" (cdr (assoc "host" (cdr db))))
                             (assert-equal "5432" (cdr (assoc "port" (cdr db))))
                             (assert-equal "true" (cdr (assoc "debug" (cdr app))))))))
            
            (define-test ini-comments-test
              (let ([result (parse-ini "; comment
[section]
# another comment
key=value
")])
                   (assert-true (right? result))
                   (let ([ini (from-right result)])
                        (assert-equal 1 (length ini))
                        (assert-equal "value" (cdr (assoc "key" (cdar ini)))))))
            
            (define-test ini-blank-lines-test
              (let ([result (parse-ini "

[section]

key=value

")])
                   (assert-true (right? result))
                   (let ([ini (from-right result)])
                        (assert-equal 1 (length ini)))))
            
            (define-test ini-get-test
              (let ([result (parse-ini "[section]
foo=bar
")])
                   (assert-true (right? result))
                   (let ([ini (from-right result)])
                        (let ([val (ini-get ini "section" "foo")]
                              [missing (ini-get ini "section" "baz")]
                              [no-section (ini-get ini "other" "foo")])
                             (assert-true (just? val))
                             (assert-equal "bar" (from-just val))
                             (assert-true (nothing? missing))
                             (assert-true (nothing? no-section)))))))

;;; ====
;;; Summary
;;; ====

(display "
")
(display "====
")
(printf "Tests passed: ~a
" *tests-passed*)
(printf "Tests failed: ~a
" *tests-failed*)
(printf "Total tests:  ~a
" *tests-run*)

(if (= *tests-failed* 0)
    (display "
[SUCCESS] All parser example tests passed.
")
    (display "
[FAILURE] Some parser example tests failed.
"))
