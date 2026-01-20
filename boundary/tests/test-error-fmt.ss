(load "boundary/debug/error-fmt.ss")

(doc 'module 'test-error-fmt)
(doc 'description "Tests for Error Formatter")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'note "Comprehensive test suite for boundary/debug/error-fmt.ss")
(doc 'note "Validates error formatting, colorization, suggestions, and placeholder fixes")

(define test-count 0)
(define pass-count 0)
(define fail-count 0)

(define (assert-equal name actual expected)
(doc assert-equal 'description "Test assertion helper")
  (set! test-count (+ test-count 1))
  (if (equal? actual expected)
      (begin
       (set! pass-count (+ pass-count 1))
       (printf "  ✓ ~a\n" name))
      (begin
       (set! fail-count (+ fail-count 1))
       (printf "  ✗ ~a\n" name)
       (printf "    Expected: ~s\n" expected)
       (printf "    Actual:   ~s\n" actual))))

(define (assert-contains name actual substring)
(doc assert-contains 'description "Test helper for string contains")
  (set! test-count (+ test-count 1))
  (if (string-contains? actual substring)
      (begin
       (set! pass-count (+ pass-count 1))
       (printf "  ✓ ~a\n" name))
      (begin
       (set! fail-count (+ fail-count 1))
       (printf "  ✗ ~a\n" name)
       (printf "    Expected substring: ~s\n" substring)
       (printf "    Actual string:      ~s\n" actual))))

(define (assert-no-error name thunk)
(doc assert-no-error 'description "Test helper for verifying no error thrown")
  (set! test-count (+ test-count 1))
  (guard (e [else
             (set! fail-count (+ fail-count 1))
             (printf "  ✗ ~a\n" name)
             (printf "    Unexpected error: ~a\n" (if (condition? e) (condition-message e) e))])
         (thunk)
         (set! pass-count (+ pass-count 1))
         (printf "  ✓ ~a\n" name)))

(doc 'section 'errorf-tests)

(printf "\n=== Testing errorf ===\n")

(assert-no-error "errorf substitutes ~a placeholder"
                 (lambda ()
                         (guard (e [else
                                    (let ([msg (if (condition? e) (condition-message e) "")])
                                         (when (not (string-contains? msg "test-value"))
                                               (error 'test "Expected substitution in message")))])
                                (errorf 'test "Value is: ~a" "test-value"))))

(assert-no-error "errorf substitutes ~s placeholder"
                 (lambda ()
                         (guard (e [else
                                    (let ([msg (if (condition? e) (condition-message e) "")])
                                         (when (not (string-contains? msg "42"))
                                               (error 'test "Expected substitution in message")))])
                                (errorf 'test "Number is: ~s" 42))))

(assert-no-error "errorf substitutes multiple placeholders"
                 (lambda ()
                         (guard (e [else
                                    (let ([msg (if (condition? e) (condition-message e) "")])
                                         (when (not (and (string-contains? msg "foo")
                                                         (string-contains? msg "bar")))
                                               (error 'test "Expected both substitutions")))])
                                (errorf 'test "Values: ~a and ~s" "foo" "bar"))))

(doc 'section 'string-utility-tests)

(printf "\n=== Testing string utilities ===\n")

(assert-equal "string-find basic"
              (string-find "hello world" "world")
              6)

(assert-equal "string-find not found"
              (string-find "hello" "xyz")
              #f)

(assert-equal "string-find at start"
              (string-find "hello" "he")
              0)

(assert-equal "string-find empty pattern"
              (string-find "test" "")
              0)

(assert-equal "string-replace-once first occurrence"
              (string-replace-once "hello hello" "hello" "hi")
              "hi hello")

(assert-equal "string-replace-once not found"
              (string-replace-once "test" "xyz" "abc")
              "test")

(assert-equal "string-replace-once with empty pattern"
              (string-replace-once "test" "" "x")
              "xtest")

(assert-equal "make-string creates string of chars"
              (make-string 5 #\*)
              "*****")

(assert-equal "make-string zero length"
              (make-string 0 #\a)
              "")

(assert-equal "list-slice basic"
              (list-slice '(a b c d e) 1 4)
              '(b c d))

(assert-equal "list-slice from start"
              (list-slice '(a b c) 0 2)
              '(a b))

(assert-equal "list-slice out of bounds"
              (list-slice '(a b c) 0 10)
              '(a b c))

(doc 'section 'placeholder-fixing-tests)

(printf "\n=== Testing placeholder fixing ===\n")

(assert-equal "fix-unfilled-placeholders ~s"
              (fix-unfilled-placeholders "Value is ~s")
              "Value is <value>")

(assert-equal "fix-unfilled-placeholders ~a"
              (fix-unfilled-placeholders "Item ~a found")
              "Item <value> found")

(assert-equal "fix-unfilled-placeholders ~d"
              (fix-unfilled-placeholders "Count: ~d")
              "Count: <number>")

(assert-equal "fix-unfilled-placeholders multiple"
              (fix-unfilled-placeholders "~s is not ~a")
              "<value> is not <value>")

(assert-equal "fix-unfilled-placeholders no placeholders"
              (fix-unfilled-placeholders "Plain text")
              "Plain text")

(doc 'section 'format-message-tests)

(printf "\n=== Testing format-message ===\n")

(assert-equal "format-message no irritants"
              (format-message "Plain error message" '())
              "Plain error message")

(assert-equal "format-message with unfilled placeholder"
              (format-message "Value is ~s" '())
              "Value is <value>")

(assert-no-error "format-message with irritants"
                 (lambda ()
                         (let ([result (format-message "Value: ~a" '("test"))])
                              (when (not (string-contains? result "test"))
                                    (error 'test "Expected formatted value")))))

(doc 'section 'suggestion-tests)

(printf "\n=== Testing suggest-fixes ===\n")

(assert-equal "suggest-fixes for unbound variable"
              (length (suggest-fixes "variable x is not bound"))
              3)

(assert-equal "suggest-fixes for wrong number of args"
              (length (suggest-fixes "wrong number of arguments to foo"))
              2)

(assert-equal "suggest-fixes for type error"
              (length (suggest-fixes "type mismatch in expression"))
              2)

(assert-equal "suggest-fixes for file error"
              (length (suggest-fixes "file not found"))
              2)

(assert-equal "suggest-fixes for unknown error"
              (suggest-fixes "unknown error occurred")
              '())

(doc 'section 'colorization-tests)

(printf "\n=== Testing colorization ===\n")

;; Test with colors enabled
(set! *use-colors* #t)

(assert-contains "colorize with colors enabled"
                 (colorize "test" *color-red* *color-bold*)
                 "test")

(assert-contains "colorize includes color code"
                 (colorize "test" *color-red* "")
                 "\x1b;")

;; Test with colors disabled
(set! *use-colors* #f)

(assert-equal "colorize with colors disabled"
              (colorize "test" *color-red* *color-bold*)
              "test")

;; Re-enable for remaining tests
(set! *use-colors* #t)

(doc 'section 'error-header-formatting-tests)

(printf "\n=== Testing error header formatting ===\n")

(assert-contains "format-error-header with who"
                 (format-error-header *level-error* 'test-func #f #f #f)
                 "[test-func]")

(assert-contains "format-error-header with location"
                 (format-error-header *level-error* #f "test.ss" 42 10)
                 "test.ss:42")

(assert-contains "format-error-header error level"
                 (format-error-header *level-error* #f #f #f #f)
                 "ERROR")

(assert-contains "format-error-header warning level"
                 (format-error-header *level-warning* #f #f #f #f)
                 "WARN")

(doc 'section 'format-error-simple-tests)

(printf "\n=== Testing format-error-simple ===\n")

(assert-contains "format-error-simple includes message"
                 (format-error-simple "Test error message")
                 "Test error message")

(assert-contains "format-error-simple includes ERROR"
                 (format-error-simple "Test error")
                 "ERROR")

(doc 'section 'format-error-with-location-tests)

(printf "\n=== Testing format-error-with-location ===\n")

(assert-contains "format-error-with-location includes file"
                 (format-error-with-location "Test error" "test.ss" 10 5)
                 "test.ss")

(assert-contains "format-error-with-location includes line"
                 (format-error-with-location "Test error" "test.ss" 10 5)
                 "10")

(assert-contains "format-error-with-location includes message"
                 (format-error-with-location "Test error" "test.ss" 10 5)
                 "Test error")

(doc 'section 'format-suggestions-tests)

(printf "\n=== Testing format-suggestions ===\n")

;; Enable suggestions
(set! *show-suggestions* #t)

(assert-contains "format-suggestions with matches"
                 (format-suggestions "variable not bound")
                 "Suggestions")

(assert-contains "format-suggestions includes suggestion text"
                 (format-suggestions "variable not bound")
                 "Check for typos")

;; Note: format-suggestions doesn't check *show-suggestions* flag
;; The flag is checked in format-error-full instead
(set! *show-suggestions* #f)

(assert-contains "format-suggestions always formats (flag checked elsewhere)"
                 (format-suggestions "variable not bound")
                 "Suggestions")

;; Re-enable
(set! *show-suggestions* #t)

(doc 'section 'integration-tests)

(printf "\n=== Testing integration scenarios ===\n")

(assert-no-error "format-error handles simple condition"
                 (lambda ()
                         (guard (e [else
                                    (let ([formatted (format-error e)])
                                         (when (not (string? formatted))
                                               (error 'test "Expected string result")))])
                                (error 'test "test error"))))

(assert-no-error "format-error-simple doesn't crash"
                 (lambda ()
                         (let ([result (format-error-simple "test message")])
                              (when (not (string? result))
                                    (error 'test "Expected string result")))))

(assert-no-error "format-error-with-location doesn't crash"
                 (lambda ()
                         (let ([result (format-error-with-location "msg" "file.ss" 1 1)])
                              (when (not (string? result))
                                    (error 'test "Expected string result")))))

(assert-no-error "errorf works with no format arguments"
                 (lambda ()
                         (guard (e [else #t])
                                (errorf 'test "plain message"))))

(assert-no-error "format-message handles empty string"
                 (lambda ()
                         (let ([result (format-message "" '())])
                              (when (not (string? result))
                                    (error 'test "Expected string result")))))

(doc 'section 'edge-cases)

(printf "\n=== Testing edge cases ===\n")

(assert-equal "string-find with equal length pattern"
              (string-find "test" "test")
              0)

(assert-equal "string-find pattern longer than haystack"
              (string-find "hi" "hello")
              #f)

(assert-equal "make-string negative length"
              (make-string -5 #\a)
              "")

(assert-equal "list-slice with start beyond end"
              (list-slice '(a b c) 5 10)
              '())

(assert-equal "list-slice with same start and end"
              (list-slice '(a b c) 1 1)
              '())

(assert-no-error "suggest-fixes with empty string"
                 (lambda ()
                         (let ([result (suggest-fixes "")])
                              (when (not (list? result))
                                    (error 'test "Expected list result")))))

(assert-no-error "colorize with empty string"
                 (lambda ()
                         (let ([result (colorize "" *color-red* "")])
                              (when (not (string? result))
                                    (error 'test "Expected string result")))))

(assert-equal "fix-unfilled-placeholders with no text"
              (fix-unfilled-placeholders "")
              "")

(doc 'section 'test-summary)

(printf "\n====\n")
(printf "Test Results:\n")
(printf "  Total:  ~a\n" test-count)
(printf "  Passed: ~a\n" pass-count)
(printf "  Failed: ~a\n" fail-count)

(if (= fail-count 0)
    (printf "\n✓ All tests passed!\n\n")
    (printf "\n✗ Some tests failed\n\n"))

(= fail-count 0)
