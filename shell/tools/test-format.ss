;;; shell/tools/test-format.ss — Tests for Code Formatter
;;;
;;; Run from project root: scheme --script shell/tools/test-format.ss

(load "shell/tools/format.ss")

(define test-count 0)
(define pass-count 0)
(define fail-count 0)

;;; Test assertion helper
(define (assert-equal name actual expected)
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

(define (assert-true name condition)
  (set! test-count (+ test-count 1))
  (if condition
      (begin
       (set! pass-count (+ pass-count 1))
       (printf "  ✓ ~a\n" name))
      (begin
       (set! fail-count (+ fail-count 1))
       (printf "  ✗ ~a\n" name)
       (printf "    Expected: #t\n")
       (printf "    Actual:   #f\n"))))

(define (assert-false name condition)
  (assert-true name (not condition)))

;;; ====
;;; Test 1: Comment Detection
;;; ====

(printf "\n=== Test 1: Comment Detection ===\n")

(assert-true "semicolon is comment"
             (comment-line? "; comment"))
(assert-true "triple semicolon is comment"
             (comment-line? ";;; header"))
(assert-false "code not comment"
              (comment-line? "(define x 10)"))
(assert-false "empty not comment"
              (comment-line? ""))

;;; ====
;;; Test 2: Paren Counting
;;; ====

(printf "\n=== Test 2: Paren Counting ===\n")

(assert-equal "count open parens"
              (count-open-parens "(define (foo x) (+ x 1))")
              3)
(assert-equal "count close parens"
              (count-close-parens "(define (foo x) (+ x 1))")
              3)
(assert-equal "count open brackets"
              (count-open-parens "[1 2 3]")
              1)
(assert-equal "count close brackets"
              (count-close-parens "[1 2 3]")
              1)
(assert-equal "no parens"
              (count-open-parens "hello")
              0)

;;; ====
;;; Test 3: Make Spaces
;;; ====

(printf "\n=== Test 3: Make Spaces ===\n")

(assert-equal "zero spaces" (make-spaces 0) "")
(assert-equal "one space" (make-spaces 1) " ")
(assert-equal "five spaces" (make-spaces 5) "     ")

;;; ====
;;; Test 4: Empty Line Detection
;;; ====

(printf "\n=== Test 4: Empty Line Detection ===\n")

(assert-true "empty string" (empty-line? ""))
(assert-false "whitespace not empty" (empty-line? "  "))
(assert-false "code not empty" (empty-line? "(+ 1 2)"))

;;; ====
;;; Test 5: Close Paren Detection
;;; ====

(printf "\n=== Test 5: Close Paren Detection ===\n")

(assert-true "starts with close paren"
             (starts-with-close-paren? ")"))
(assert-true "starts with close bracket"
             (starts-with-close-paren? "]"))
(assert-false "starts with open paren"
              (starts-with-close-paren? "("))
(assert-false "starts with letter"
              (starts-with-close-paren? "define"))

;;; ====
;;; Test 6: Format Expression (Simple)
;;; ====

(printf "\n=== Test 6: Format Expression (Simple) ===\n")

(assert-equal "format number"
              (format-expr 42)
              "42")
(assert-equal "format symbol"
              (format-expr 'foo)
              "foo")
(assert-equal "format string"
              (format-expr "hello")
              "\"hello\"")
(assert-equal "format boolean true"
              (format-expr #t)
              "#t")
(assert-equal "format boolean false"
              (format-expr #f)
              "#f")
(assert-equal "format empty list"
              (format-expr '())
              "()")

;;; ====
;;; Test 7: Format Expression (Lists)
;;; ====

(printf "\n=== Test 7: Format Expression (Lists) ===\n")

(let ([result (format-expr '(+ 1 2))])
     (assert-true "format simple list contains +"
                  (string-contains? result "+"))
     (assert-true "format simple list contains 1"
                  (string-contains? result "1"))
     (assert-true "format simple list contains 2"
                  (string-contains? result "2")))

;;; ====
;;; Test 8: Format Expression (Vectors)
;;; ====

(printf "\n=== Test 8: Format Expression (Vectors) ===\n")

(let ([result (format-expr '#(1 2 3))])
     (assert-true "format vector starts with #("
                  (string-contains? result "#("))
     (assert-true "format vector contains elements"
                  (and (string-contains? result "1")
                       (string-contains? result "2")
                       (string-contains? result "3"))))

;;; ====
;;; Test 9: Format String (No Change)
;;; ====

(printf "\n=== Test 9: Format String (No Change) ===\n")

(let* ([code "(+ 1 2)"]
       [formatted (format-string code)])
      (assert-true "simple code unchanged"
                   (string-contains? formatted "+"))
      (assert-true "formatted is string"
                   (string? formatted)))

;;; ====
;;; Test 10: Format Check
;;; ====

(printf "\n=== Test 10: Format Check ===\n")

;; Create a temporary file
(let ([temp-file "/tmp/test-format-check.ss"])
     (call-with-output-file temp-file
                            (lambda (port)
                                    (display "(+ 1 2)\n" port))
                            'replace)
     
     ;; Check if formatting needed
     (let ([needs-format (format-check temp-file)])
          (assert-true "format-check returns boolean"
                       (boolean? needs-format)))
     
     ;; Clean up
     (when (file-exists? temp-file)
           (delete-file temp-file)))

;;; ====
;;; Test 11: Format File
;;; ====

(printf "\n=== Test 11: Format File ===\n")

;; Create a temporary file
(let ([temp-file "/tmp/test-format-file.ss"])
     (call-with-output-file temp-file
                            (lambda (port)
                                    (display "(define x 10)\n(+ x 1)\n" port))
                            'replace)
     
     ;; Format the file
     (guard (e [else
                (printf "    Format error: ~a\n"
                        (if (condition? e) (condition-message e) e))
                (assert-true "format-file failed" #f)])
            (let ([modified (format-file temp-file)])
                 (assert-true "format-file returns boolean" (boolean? modified))))
     
     ;; Clean up
     (when (file-exists? temp-file)
           (delete-file temp-file)))

;;; ====
;;; Test 12: Format Diff
;;; ====

(printf "\n=== Test 12: Format Diff ===\n")

;; Create a temporary file
(let ([temp-file "/tmp/test-format-diff.ss"])
     (call-with-output-file temp-file
                            (lambda (port)
                                    (display "(+ 1 2)\n" port))
                            'replace)
     
     ;; Get diff
     (guard (e [else
                (printf "    Diff error: ~a\n"
                        (if (condition? e) (condition-message e) e))
                (assert-true "format-diff failed" #f)])
            (let ([diff (format-diff temp-file)])
                 (assert-true "diff is string" (string? diff))
                 (assert-true "diff not empty" (> (string-length diff) 0))))
     
     ;; Clean up
     (when (file-exists? temp-file)
           (delete-file temp-file)))

;;; ====
;;; Test 13: Configuration Variables
;;; ====

(printf "\n=== Test 13: Configuration Variables ===\n")

(assert-equal "default indent width" *indent-width* 2)
(assert-equal "default max line length" *max-line-length* 80)
(assert-equal "default format style" *format-style* 'compact)

;; Test modifying configuration
(set! *indent-width* 4)
(assert-equal "modified indent width" *indent-width* 4)
(set! *indent-width* 2)  ; Restore

;;; ====
;;; Test 14: Special Forms Configuration
;;; ====

(printf "\n=== Test 14: Special Forms Configuration ===\n")

(assert-true "special forms is list" (list? *special-forms*))
(assert-true "has define" (assq 'define *special-forms*))
(assert-true "has lambda" (assq 'lambda *special-forms*))
(assert-true "has if" (assq 'if *special-forms*))

;;; ====
;;; Test 15: Format Styles
;;; ====

(printf "\n=== Test 15: Format Styles ===\n")

;; Test compact style
(set! *format-style* 'compact)
(let ([result (format-expr '(+ 1 2))])
     (assert-true "compact style produces output" (string? result)))

;; Test expanded style
(set! *format-style* 'expanded)
(let ([result (format-expr '(+ 1 2))])
     (assert-true "expanded style produces output" (string? result)))

;; Test canonical style
(set! *format-style* 'canonical)
(let ([result (format-expr '(+ 1 2))])
     (assert-true "canonical style produces output" (string? result)))

;; Restore default
(set! *format-style* 'compact)

;;; ====
;;; Test 16: Edge Cases
;;; ====

(printf "\n=== Test 16: Edge Cases ===\n")

;; Empty string formatting
(let ([result (format-string "")])
     (assert-equal "empty string formats to empty" result ""))

;; Single character
(let ([result (format-string "x")])
     (assert-true "single char formats" (string? result)))

;; Very nested expression
(let ([result (format-expr '(+ (+ (+ 1 2) 3) 4))])
     (assert-true "nested expr formats" (string? result)))

;;; ====
;;; Test 17: File I/O Functions
;;; ====

(printf "\n=== Test 17: File I/O Functions ===\n")

(let ([temp-file "/tmp/test-format-io.ss"])
     ;; Test write
     (write-file-as-string temp-file "(define x 10)")
     (assert-true "file written" (file-exists? temp-file))
     
     ;; Test read
     (let ([content (read-file-as-string temp-file)])
          (assert-equal "file read correctly" content "(define x 10)"))
     
     ;; Clean up
     (when (file-exists? temp-file)
           (delete-file temp-file)))

;;; ====
;;; Test Summary
;;; ====

(printf "\n====\n")
(printf "Test Results:\n")
(printf "  Total:  ~a\n" test-count)
(printf "  Passed: ~a\n" pass-count)
(printf "  Failed: ~a\n" fail-count)

(if (= fail-count 0)
    (printf "\n✓ All tests passed!\n\n")
    (printf "\n✗ Some tests failed\n\n"))

;;; Return success/failure
(= fail-count 0)
