;;; core/lang/test-span.ss — Tests for Position-Aware Parser Combinators
;;; Standardized to use test-framework.ss

(load "core/testing/test-framework.ss")
(load "core/lang/span.ss")

;;; ============================================================================
;;; Tests
;;; ============================================================================

;;; ====
;;; Span Tests
;;; ====

(test-group span-construction
            (define-test make-span-basic
              (let ([s (make-span "test.scm" 10 5 10 20)])
                   (assert-true (span? s))
                   (assert-equal "test.scm" (span-file s))
                   (assert-equal 10 (span-line s))
                   (assert-equal 5 (span-column s))
                   (assert-equal 10 (span-end-line s))
                   (assert-equal 20 (span-end-column s))))
            
            (define-test no-span-test
              (assert-true (span? no-span))
              (assert-equal "<unknown>" (span-file no-span)))
            
            (define-test merge-spans-same-line
              (let* ([s1 (make-span "file.scm" 5 10 5 15)]
                     [s2 (make-span "file.scm" 5 20 5 30)]
                     [merged (merge-spans s1 s2)])
                    (assert-equal "file.scm" (span-file merged))
                    (assert-equal 5 (span-line merged))
                    (assert-equal 10 (span-column merged))
                    (assert-equal 5 (span-end-line merged))
                    (assert-equal 30 (span-end-column merged))))
            
            (define-test merge-spans-multi-line
              (let* ([s1 (make-span "file.scm" 5 10 5 15)]
                     [s2 (make-span "file.scm" 8 1 8 10)]
                     [merged (merge-spans s1 s2)])
                    (assert-equal 5 (span-line merged))
                    (assert-equal 10 (span-column merged))
                    (assert-equal 8 (span-end-line merged))
                    (assert-equal 10 (span-end-column merged))))
            
            (define-test format-span-test
              (let ([s (make-span "test.scm" 42 17 42 25)])
                   (assert-equal "test.scm:42:17" (format-span s)))))

;;; ====
;;; Parser State Tests
;;; ====

(test-group parser-state
            (define-test initial-state-basic
              (let ([st (initial-state "hello")])
                   (assert-true (parse-state? st))
                   (assert-equal "hello" (state-input st))
                   (assert-equal 0 (state-offset st))
                   (assert-equal 1 (state-line st))
                   (assert-equal 1 (state-column st))
                   (assert-equal "<input>" (state-file st))))
            
            (define-test initial-state-with-file
              (let ([st (initial-state "hello" "test.scm")])
                   (assert-equal "test.scm" (state-file st))))
            
            (define-test state-span-test
              (let* ([st (initial-state "hello" "test.scm")]
                     [sp (state-span st)])
                    (assert-equal "test.scm" (span-file sp))
                    (assert-equal 1 (span-line sp))
                    (assert-equal 1 (span-column sp))))
            
            (define-test advance-state-regular-char
              (let* ([st (initial-state "hello")]
                     [st2 (advance-state st #\h)])
                    (assert-equal "ello" (state-remaining-input st2))
                    (assert-equal 1 (state-offset st2))
                    (assert-equal 1 (state-line st2))
                    (assert-equal 2 (state-column st2))))
            
            (define-test advance-state-newline
              (let* ([st (initial-state "a\nb")]
                     [st2 (advance-state st #\a)]
                     [st3 (advance-state st2 #\newline)])
                    (assert-equal "b" (state-remaining-input st3))
                    (assert-equal 2 (state-offset st3))
                    (assert-equal 2 (state-line st3))
                    (assert-equal 1 (state-column st3))))
            
            (define-test advance-state-sequence
              (let* ([st (initial-state "abc")]
                     [st2 (advance-state st #\a)]
                     [st3 (advance-state st2 #\b)]
                     [st4 (advance-state st3 #\c)])
                    (assert-equal "" (state-remaining-input st4))
                    (assert-equal 3 (state-offset st4))
                    (assert-equal 1 (state-line st4))
                    (assert-equal 4 (state-column st4)))))

;;; ====
;;; Spanned Result Tests
;;; ====

(test-group spanned-results
            (define-test success-construction
              (let* ([st (initial-state "test")]
                     [sp (state-span st)]
                     [result (spanned-success 42 st sp)])
                    (assert-true (spanned-ok? result))
                    (assert-false (spanned-err? result))
                    (assert-equal 42 (spanned-value result))
                    (assert-true (parse-state? (spanned-state result)))
                    (assert-true (span? (spanned-span result)))))
            
            (define-test failure-construction
              (let* ([st (initial-state "test")]
                     [result (spanned-failure "identifier" st)])
                    (assert-false (spanned-ok? result))
                    (assert-true (spanned-err? result))
                    (assert-equal "identifier" (spanned-expected result))
                    (assert-true (parse-state? (spanned-error-state result))))))

;;; ====
;;; Primitive Parser Tests
;;; ====

(test-group primitive-parsers
            (define-test s-pure-test
              (let ([result (run-spanned (s-pure 42) "anything")])
                   (assert-true (spanned-ok? result))
                   (assert-equal 42 (spanned-value result))))
            
            (define-test s-fail-test
              (let ([result (run-spanned (s-fail "error") "anything")])
                   (assert-true (spanned-err? result))
                   (assert-equal "error" (spanned-expected result))))
            
            (define-test s-item-success
              (let ([result (run-spanned s-item "hello")])
                   (assert-true (spanned-ok? result))
                   (assert-equal #\h (spanned-value result))
                   (assert-equal "ello" (state-remaining-input (spanned-state result)))))
            
            (define-test s-item-failure
              (let ([result (run-spanned s-item "")])
                   (assert-true (spanned-err? result))
                   (assert-equal "any character" (spanned-expected result))))
            
            (define-test s-eof-success
              (let ([result (run-spanned s-eof "")])
                   (assert-true (spanned-ok? result))))
            
            (define-test s-eof-failure
              (let ([result (run-spanned s-eof "x")])
                   (assert-true (spanned-err? result))
                   (assert-equal "end of input" (spanned-expected result))))
            
            (define-test s-satisfy-success
              (let* ([parser (s-satisfy char-numeric? "digit")]
                     [result (run-spanned parser "5abc")])
                    (assert-true (spanned-ok? result))
                    (assert-equal #\5 (spanned-value result))))
            
            (define-test s-satisfy-failure
              (let* ([parser (s-satisfy char-numeric? "digit")]
                     [result (run-spanned parser "abc")])
                    (assert-true (spanned-err? result))
                    (assert-equal "digit" (spanned-expected result)))))

;;; ====
;;; Character Parser Tests
;;; ====

(test-group character-parsers
            (define-test s-char-success
              (let ([result (run-spanned (s-char #\x) "xyz")])
                   (assert-true (spanned-ok? result))
                   (assert-equal #\x (spanned-value result))))
            
            (define-test s-char-failure
              (let ([result (run-spanned (s-char #\x) "abc")])
                   (assert-true (spanned-err? result))))
            
            (define-test s-digit-success
              (let ([result (run-spanned s-digit "7abc")])
                   (assert-true (spanned-ok? result))
                   (assert-equal #\7 (spanned-value result))))
            
            (define-test s-alpha-success
              (let ([result (run-spanned s-alpha "abc")])
                   (assert-true (spanned-ok? result))
                   (assert-equal #\a (spanned-value result))))
            
            (define-test s-space-success
              (let ([result (run-spanned s-space " abc")])
                   (assert-true (spanned-ok? result))
                   (assert-equal #\space (spanned-value result))))
            
            (define-test s-alphanum-success-letter
              (let ([result (run-spanned s-alphanum "a5")])
                   (assert-true (spanned-ok? result))
                   (assert-equal #\a (spanned-value result))))
            
            (define-test s-alphanum-success-digit
              (let ([result (run-spanned s-alphanum "5a")])
                   (assert-true (spanned-ok? result))
                   (assert-equal #\5 (spanned-value result))))
            
            (define-test s-string-success
              (let ([result (run-spanned (s-string "hello") "hello world")])
                   (assert-true (spanned-ok? result))
                   (assert-equal "hello" (spanned-value result))))
            
            (define-test s-string-failure
              (let ([result (run-spanned (s-string "hello") "hi there")])
                   (assert-true (spanned-err? result)))))

;;; ====
;;; Combinator Tests
;;; ====

(test-group combinators
            (define-test s-bind-success
              (let* ([parser (s-bind s-item (lambda (c) (s-pure (char-upcase c))))]
                     [result (run-spanned parser "abc")])
                    (assert-true (spanned-ok? result))
                    (assert-equal #\A (spanned-value result))))
            
            (define-test s-seq-test
              (let* ([parser (s-seq (s-char #\a) (s-char #\b))]
                     [result (run-spanned parser "abc")])
                    (assert-true (spanned-ok? result))
                    (assert-equal #\b (spanned-value result))))
            
            (define-test s-seq-left-test
              (let* ([parser (s-seq-left (s-char #\a) (s-char #\b))]
                     [result (run-spanned parser "abc")])
                    (assert-true (spanned-ok? result))
                    (assert-equal #\a (spanned-value result))))
            
            (define-test s-alt-first-succeeds
              (let* ([parser (s-alt (s-char #\a) (s-char #\b))]
                     [result (run-spanned parser "abc")])
                    (assert-true (spanned-ok? result))
                    (assert-equal #\a (spanned-value result))))
            
            (define-test s-alt-second-succeeds
              (let* ([parser (s-alt (s-char #\b) (s-char #\a))]
                     [result (run-spanned parser "abc")])
                    (assert-true (spanned-ok? result))
                    (assert-equal #\a (spanned-value result))))
            
            (define-test s-choice-test
              (let* ([parser (s-choice (list (s-char #\x) (s-char #\y) (s-char #\a)))]
                     [result (run-spanned parser "abc")])
                    (assert-true (spanned-ok? result))
                    (assert-equal #\a (spanned-value result))))
            
            (define-test s-map-test
              (let* ([parser (s-map (lambda (c) (char->integer c)) (s-char #\A))]
                     [result (run-spanned parser "ABC")])
                    (assert-true (spanned-ok? result))
                    (assert-equal (char->integer #\A) (spanned-value result)))))

;;; ====
;;; Repetition Tests
;;; ====

(test-group repetition
            (define-test s-many-success
              (let* ([parser (s-many s-digit)]
                     [result (run-spanned parser "123abc")])
                    (assert-true (spanned-ok? result))
                    (assert-equal 3 (length (spanned-value result)))
                    (assert-equal #\1 (car (spanned-value result)))))
            
            (define-test s-many-zero-matches
              (let* ([parser (s-many s-digit)]
                     [result (run-spanned parser "abc")])
                    (assert-true (spanned-ok? result))
                    (assert-equal 0 (length (spanned-value result)))))
            
            (define-test s-many1-success
              (let* ([parser (s-many1 s-digit)]
                     [result (run-spanned parser "42abc")])
                    (assert-true (spanned-ok? result))
                    (assert-equal 2 (length (spanned-value result)))))
            
            (define-test s-many1-failure
              (let* ([parser (s-many1 s-digit)]
                     [result (run-spanned parser "abc")])
                    (assert-true (spanned-err? result))))
            
            (define-test s-optional-present
              (let* ([parser (s-optional (s-char #\a))]
                     [result (run-spanned parser "abc")])
                    (assert-true (spanned-ok? result))
                    (assert-equal 1 (length (spanned-value result)))))
            
            (define-test s-optional-absent
              (let* ([parser (s-optional (s-char #\a))]
                     [result (run-spanned parser "xyz")])
                    (assert-true (spanned-ok? result))
                    (assert-equal 0 (length (spanned-value result)))))
            
            (define-test s-sep-by-multiple
              (let* ([parser (s-sep-by s-digit (s-char #\,))]
                     [result (run-spanned parser "1,2,3")])
                    (assert-true (spanned-ok? result))
                    (assert-equal 3 (length (spanned-value result)))))
            
            (define-test s-sep-by-single
              (let* ([parser (s-sep-by s-digit (s-char #\,))]
                     [result (run-spanned parser "5")])
                    (assert-true (spanned-ok? result))
                    (assert-equal 1 (length (spanned-value result)))))
            
            (define-test s-sep-by-empty
              (let* ([parser (s-sep-by s-digit (s-char #\,))]
                     [result (run-spanned parser "abc")])
                    (assert-true (spanned-ok? result))
                    (assert-equal 0 (length (spanned-value result))))))

;;; ====
;;; Utility Parser Tests
;;; ====

(test-group utility-parsers
            (define-test s-spaces-test
              (let ([result (run-spanned s-spaces "   abc")])
                   (assert-true (spanned-ok? result))
                   (assert-equal 3 (length (spanned-value result)))))
            
            (define-test s-spaces1-success
              (let ([result (run-spanned s-spaces1 "  x")])
                   (assert-true (spanned-ok? result))))
            
            (define-test s-spaces1-failure
              (let ([result (run-spanned s-spaces1 "x")])
                   (assert-true (spanned-err? result))))
            
            (define-test s-lexeme-test
              (let* ([parser (s-lexeme (s-string "hello"))]
                     [result (run-spanned parser "hello   world")])
                    (assert-true (spanned-ok? result))
                    (assert-equal "hello" (spanned-value result))))
            
            (define-test s-symbol-test
              (let ([result (run-spanned (s-symbol "foo") "foo  bar")])
                   (assert-true (spanned-ok? result))
                   (assert-equal "foo" (spanned-value result))))
            
            (define-test s-nat-test
              (let ([result (run-spanned s-nat "42abc")])
                   (assert-true (spanned-ok? result))
                   (assert-equal 42 (spanned-value result))))
            
            (define-test s-int-positive
              (let ([result (run-spanned s-int "42")])
                   (assert-true (spanned-ok? result))
                   (assert-equal 42 (spanned-value result))))
            
            (define-test s-int-negative
              (let ([result (run-spanned s-int "-17")])
                   (assert-true (spanned-ok? result))
                   (assert-equal -17 (spanned-value result))))
            
            (define-test s-identifier-test
              (let ([result (run-spanned s-identifier "foo_bar123 ")])
                   (assert-true (spanned-ok? result))
                   (assert-equal "foo_bar123" (spanned-value result))))
            
            (define-test s-identifier-no-digit-start
              (let ([result (run-spanned s-identifier "5foo")])
                   (assert-true (spanned-err? result)))))

;;; ====
;;; Spanned Value Wrapper Tests
;;; ====

(test-group spanned-values
            (define-test spanned-construction
              (let* ([sp (make-span "test.scm" 1 1 1 5)]
                     [sv (spanned 42 sp)])
                    (assert-true (spanned-value? sv))
                    (assert-equal 42 (get-spanned-value sv))
                    (assert-true (span? (get-value-span sv)))))
            
            (define-test get-spanned-value-plain
              (assert-equal 42 (get-spanned-value 42)))
            
            (define-test get-value-span-plain
              (assert-true (span? (get-value-span 42))))
            
            (define-test s-spanned-parser
              (let* ([parser (s-spanned (s-string "test"))]
                     [result (run-spanned parser "test" "file.scm")])
                    (assert-true (spanned-ok? result))
                    (let ([sv (spanned-value result)])
                         (assert-true (spanned-value? sv))
                         (assert-equal "test" (get-spanned-value sv))
                         (let ([sp (get-value-span sv)])
                              (assert-equal "file.scm" (span-file sp))
                              (assert-equal 1 (span-line sp))
                              (assert-equal 1 (span-column sp)))))))

;;; ====
;;; Error Formatting Tests
;;; ====

(test-group error-formatting
            (define-test format-error-test
              (let* ([parser (s-char #\x)]
                     [result (run-spanned parser "abc" "test.scm")]
                     [msg (format-parse-error result)])
                    (assert-true (string? msg))
                    ;; Should contain file location and expected
                    (assert-true (> (string-length msg) 0))))
            
            (define-test format-no-error
              (let ([msg (format-parse-error (run-spanned (s-pure 1) "x"))])
                   (assert-equal "no error" msg))))

;;; ====
;;; Integration Tests
;;; ====

(test-group integration
            (define-test parse-list-of-numbers
              (let* ([num-parser (s-lexeme s-nat)]
                     [list-parser (s-many num-parser)]
                     [result (run-spanned list-parser "1 2 3 4 5")])
                    (assert-true (spanned-ok? result))
                    (assert-equal 5 (length (spanned-value result)))
                    (assert-equal 1 (car (spanned-value result)))))
            
            (define-test parse-csv-numbers
              (let* ([parser (s-sep-by s-nat (s-symbol ","))]
                     [result (run-spanned parser "10, 20, 30")])
                    (assert-true (spanned-ok? result))
                    (assert-equal 3 (length (spanned-value result)))
                    (assert-equal 10 (car (spanned-value result)))))
            
            (define-test parse-with-spanning
              (let* ([parser (s-spanned (s-many1 s-digit))]
                     [result (run-spanned parser "123abc" "test.scm")])
                    (assert-true (spanned-ok? result))
                    (let* ([sv (spanned-value result)]
                           [sp (get-value-span sv)])
                          (assert-equal 1 (span-line sp))
                          (assert-equal 1 (span-column sp))
                          (assert-equal 1 (span-end-line sp))
                          (assert-equal 4 (span-end-column sp))))))

;;; ============================================================================
;;; Run all tests
;;; ============================================================================

(run-all-tests-and-exit)
