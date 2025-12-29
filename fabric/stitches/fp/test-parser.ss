;;; fabric/stitches/fp/test-parser.ss — Tests for Parser Combinators

;;; NOTE: Run from fabric/stitches directory

(load "fabric/stitches/test-framework.ss")
(load "fabric/stitches/fp/parser.ss")

(display "
")
(display "==============================================================
")
(display "         PARSER COMBINATORS TESTS
")
(display "==============================================================
")

;;; Define character constants to avoid formatter issues
(define %char-a (integer->char 97))   ; a
(define %char-b (integer->char 98))   ; b
(define %char-c (integer->char 99))   ; c
(define %char-h (integer->char 104))  ; h
(define %char-x (integer->char 120))  ; x
(define %char-0 (integer->char 48))   ; 0
(define %char-1 (integer->char 49))   ; 1
(define %char-5 (integer->char 53))   ; 5
(define %char-A (integer->char 65))   ; A
(define %newline (integer->char 10))
(define %space (integer->char 32))

;;; ============================================================
;;; Test Helpers
;;; ============================================================

(define (assert-parses parser input expected)
  (let ([result (parse parser input)])
       (cond
        [(left? result)
         (error 'assert-parses
                (string-append "Expected success but got error"))]
        [(not (equal? expected (from-right result)))
         (error 'assert-parses
                (format "Expected ~s but got ~s" expected (from-right result)))]
        [else #t])))

(define (assert-fails parser input)
  (let ([result (parse parser input)])
       (if (right? result)
           (error 'assert-fails
                  (format "Expected failure but got success: ~s" (from-right result)))
           #t)))

;;; ============================================================
;;; Position Tests
;;; ============================================================

(test-group position-tests
            (define-test make-pos-test
              (let ([p (make-pos 1 5 10)])
                   (assert-equal 1 (pos-line p))
                   (assert-equal 5 (pos-col p))
                   (assert-equal 10 (pos-offset p))))
            
            (define-test advance-pos-char-test
              (let* ([p1 (make-pos 1 1 0)]
                     [p2 (advance-pos p1 %char-a)])
                    (assert-equal 1 (pos-line p2))
                    (assert-equal 2 (pos-col p2))
                    (assert-equal 1 (pos-offset p2))))
            
            (define-test advance-pos-newline-test
              (let* ([p1 (make-pos 1 5 10)]
                     [p2 (advance-pos p1 %newline)])
                    (assert-equal 2 (pos-line p2))
                    (assert-equal 1 (pos-col p2))
                    (assert-equal 11 (pos-offset p2)))))

;;; ============================================================
;;; State Tests
;;; ============================================================

(test-group state-tests
            (define-test make-state-test
              (let ([s (make-state "abc" (make-pos 1 1 0))])
                   (assert-equal "abc" (state-input s))))
            
            (define-test initial-state-test
              (let ([s (initial-state "hello")])
                   (assert-equal "hello" (state-input s))
                   (assert-equal 1 (pos-line (state-pos s)))
                   (assert-equal 1 (pos-col (state-pos s))))))

;;; ============================================================
;;; Error Tests
;;; ============================================================

(test-group error-tests
            (define-test make-error-test
              (let ([e (make-parse-error (make-pos 1 1 0) "msg" '("expected x"))])
                   (assert-true (parse-error? e))
                   (assert-equal 1 (length (error-expected e)))))
            
            (define-test merge-errors-test
              (let* ([e1 (make-parse-error (make-pos 1 1 0) "msg1" '("expected a"))]
                     [e2 (make-parse-error (make-pos 1 2 1) "msg2" '("expected b"))]
                     [merged (merge-errors e1 e2)])
                    ;; Later position should win
                    (assert-equal 2 (pos-col (error-pos merged)))))
            
            (define-test merge-errors-same-pos-test
              (let* ([e1 (make-parse-error (make-pos 1 1 0) "msg1" '("expected a"))]
                     [e2 (make-parse-error (make-pos 1 1 0) "msg2" '("expected b"))]
                     [merged (merge-errors e1 e2)])
                    ;; Same position, expectations should be combined
                    (assert-equal 2 (length (error-expected merged))))))

;;; ============================================================
;;; Basic Parsers Tests
;;; ============================================================

(test-group basic-parsers
            (define-test pure-parser-test
              (assert-parses (parser-pure 42) "anything" 42)
              (assert-parses (parser-pure "hello") "" "hello"))
            
            (define-test fail-parser-test
              (assert-fails (parser-fail "expected nothing") "input"))
            
            (define-test any-char-test
              (assert-parses any-char "hello" %char-h)
              (assert-parses any-char "x" %char-x))
            
            (define-test any-char-empty-test
              (assert-fails any-char ""))
            
            (define-test satisfy-test
              (assert-parses (satisfy char-numeric? "digit") "5" %char-5)
              (assert-fails (satisfy char-numeric? "digit") "a"))
            
            (define-test char-test
              (assert-parses (char %char-x) "xyz" %char-x)
              (assert-fails (char %char-x) "abc"))
            
            (define-test string-parser-test
              (assert-parses (string-parser "hello") "hello world" "hello")
              (assert-fails (string-parser "hello") "hel")))

;;; ============================================================
;;; Character Class Tests
;;; ============================================================

(test-group character-classes
            (define-test digit-test
              (assert-parses digit "5abc" %char-5)
              (assert-parses digit "0" %char-0)
              (assert-fails digit "abc"))
            
            (define-test letter-test
              (assert-parses letter "abc" %char-a)
              (assert-fails letter "123"))
            
            (define-test alpha-num-test
              (assert-parses alpha-num "a1" %char-a)
              (assert-parses alpha-num "1a" %char-1)
              (assert-fails alpha-num " x")))

;;; ============================================================
;;; Combinator Tests
;;; ============================================================

(test-group combinators
            (define-test parser-map-test
              (let ([p (parser-map char-upcase letter)])
                   (assert-parses p "abc" %char-A)))
            
            (define-test parser-bind-test
              (let ([p (parser-bind letter
                                    (lambda (c)
                                            (parser-pure (char-upcase c))))])
                   (assert-parses p "abc" %char-A)))
            
            (define-test parser-then-test
              (let ([p (parser-then (char %char-a) (char %char-b))])
                   (assert-parses p "abc" %char-b)))
            
            (define-test parser-left-test
              (let ([p (parser-left (char %char-a) (char %char-b))])
                   (assert-parses p "abc" %char-a)))
            
            (define-test parser-or-a-test
              (let ([p (parser-or (char %char-a) (char %char-b))])
                   (assert-parses p "abc" %char-a)))
            
            (define-test parser-or-b-test
              (let ([p (parser-or (char %char-a) (char %char-b))])
                   (assert-parses p "bcd" %char-b)))
            
            (define-test parser-or-fail-test
              (let ([p (parser-or (char %char-a) (char %char-b))])
                   (assert-fails p "cde")))
            
            (define-test choice-test
              (let ([p (choice (list (char %char-a) (char %char-b) (char %char-c)))])
                   (assert-parses p "abc" %char-a)
                   (assert-parses p "bcd" %char-b)
                   (assert-parses p "cde" %char-c))))

;;; ============================================================
;;; Repetition Tests
;;; ============================================================

(test-group repetition
            (define-test many-test
              (assert-parses (many digit) "123abc" (list %char-1 (integer->char 50) (integer->char 51)))
              (assert-parses (many digit) "abc" '()))
            
            (define-test some-test
              (assert-parses (some digit) "123" (list %char-1 (integer->char 50) (integer->char 51)))
              (assert-fails (some digit) "abc"))
            
            (define-test count-test
              (assert-parses (count 3 letter) "abcdef"
                             (list %char-a %char-b %char-c))
              (assert-fails (count 3 letter) "ab"))
            
            (define-test optional-test
              (assert-parses (optional digit %char-0) "5" %char-5)
              (assert-parses (optional digit %char-0) "a" %char-0)))

;;; ============================================================
;;; Sequence Tests
;;; ============================================================

(test-group sequences
            (define-test between-test
              (let ([p (between (char (integer->char 40))  ; '('
                                (char (integer->char 41))  ; ')'
                                letter)])
                   (assert-parses p "(a)" %char-a)))
            
            (define-test sep-by-test
              (let ([p (sep-by digit (char (integer->char 44)))])  ; ','
                   (assert-parses p "1,2,3" (list %char-1 (integer->char 50) (integer->char 51)))
                   (assert-parses p "" '()))))

;;; ============================================================
;;; Token Tests
;;; ============================================================

(test-group tokens
            (define-test lexeme-test
              (let ([p (lexeme (string-parser "hello"))])
                   (assert-parses p "hello   world" "hello")))
            
            (define-test integer-test
              (assert-parses integer "123abc" 123)
              (assert-parses integer "-42xyz" -42)))

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
[SUCCESS] All parser combinator tests passed.
")
    (display "
[FAILURE] Some parser combinator tests failed.
"))
