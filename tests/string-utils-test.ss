;;; string-utils-test.ss — Comprehensive tests for string utilities

(load "shell/string-utils.ss")

(printf "\n╔════════════════════════════════════════════════════════════╗\n")
(printf "║            STRING UTILITIES TEST SUITE                     ║\n")
(printf "╚════════════════════════════════════════════════════════════╝\n\n")

(define tests-passed 0)
(define tests-failed 0)

(define (test name expected actual)
  (if (equal? expected actual)
      (begin
       (set! tests-passed (+ tests-passed 1))
       (printf "  ✓ ~a\n" name))
      (begin
       (set! tests-failed (+ tests-failed 1))
       (printf "  ✗ ~a\n" name)
       (printf "      Expected: ~s\n" expected)
       (printf "      Got:      ~s\n" actual))))

;;; ============================================================
;;; Test String Search Functions
;;; ============================================================

(printf "Testing String Search:\n")
(printf "─────────────────────────────────────\n")

(test "string-contains? - basic"
      #t
      (string-contains? "hello world" "world"))

(test "string-contains? - not found"
      #f
      (string-contains? "hello" "xyz"))

(test "string-contains? - empty needle"
      #t
      (string-contains? "hello" ""))

(test "string-starts-with? - true"
      #t
      (string-starts-with? "hello world" "hello"))

(test "string-starts-with? - false"
      #f
      (string-starts-with? "hello" "world"))

(test "string-ends-with? - true"
      #t
      (string-ends-with? "hello world" "world"))

(test "string-ends-with? - false"
      #f
      (string-ends-with? "hello" "xyz"))

(test "string-index-of - found"
      6
      (string-index-of "hello world" "world"))

(test "string-index-of - not found"
      #f
      (string-index-of "hello" "xyz"))

(test "string-last-index-of - found"
      9
      (string-last-index-of "hello hello" "lo"))

(printf "\n")

;;; ============================================================
;;; Test String Splitting and Joining
;;; ============================================================

(printf "Testing String Split & Join:\n")
(printf "─────────────────────────────────────\n")

(test "string-split - comma delimiter"
      '("a" "b" "c")
      (string-split "a,b,c" ","))

(test "string-split - empty delimiter"
      '("h" "i")
      (string-split "hi" ""))

(test "string-split - no delimiter found"
      '("hello")
      (string-split "hello" ","))

(test "string-join - basic"
      "a,b,c"
      (string-join '("a" "b" "c") ","))

(test "string-join - single element"
      "hello"
      (string-join '("hello") ","))

(test "string-join - empty list"
      ""
      (string-join '() ","))

(printf "\n")

;;; ============================================================
;;; Test String Trimming
;;; ============================================================

(printf "Testing String Trim:\n")
(printf "─────────────────────────────────────\n")

(test "string-trim-left"
      "hello  "
      (string-trim-left "  hello  "))

(test "string-trim-right"
      "  hello"
      (string-trim-right "  hello  "))

(test "string-trim"
      "hello"
      (string-trim "  hello  "))

(test "string-trim - tabs and newlines"
      "hello"
      (string-trim "\t\nhello\n\t"))

(printf "\n")

;;; ============================================================
;;; Test String Transformation
;;; ============================================================

(printf "Testing String Transformation:\n")
(printf "─────────────────────────────────────\n")

(test "string-replace - single"
      "hello there"
      (string-replace "hello world" "world" "there"))

(test "string-replace - multiple"
      "bbb"
      (string-replace "aaa" "a" "b"))

(test "string-replace - not found"
      "hello"
      (string-replace "hello" "xyz" "abc"))

(test "string-reverse"
      "olleh"
      (string-reverse "hello"))

(printf "\n")

;;; ============================================================
;;; Test String Predicates
;;; ============================================================

(printf "Testing String Predicates:\n")
(printf "─────────────────────────────────────\n")

(test "string-empty? - true"
      #t
      (string-empty? ""))

(test "string-empty? - false"
      #f
      (string-empty? "hello"))

(test "string-blank? - empty"
      #t
      (string-blank? ""))

(test "string-blank? - whitespace only"
      #t
      (string-blank? "  \t\n  "))

(test "string-blank? - has content"
      #f
      (string-blank? "  hello  "))

(test "string-all-match? - all digits"
      #t
      (string-all-match? "12345" char-numeric?))

(test "string-all-match? - not all digits"
      #f
      (string-all-match? "123a5" char-numeric?))

(printf "\n")

;;; ============================================================
;;; Test String Padding
;;; ============================================================

(printf "Testing String Padding:\n")
(printf "─────────────────────────────────────\n")

(test "string-pad-left - pad with zeros"
      "00042"
      (string-pad-left "42" 5 #\0))

(test "string-pad-left - already long enough"
      "hello"
      (string-pad-left "hello" 3 #\space))

(test "string-pad-right - pad with spaces"
      "hi   "
      (string-pad-right "hi" 5 #\space))

(printf "\n")

;;; ============================================================
;;; Summary
;;; ============================================================

(printf "╔════════════════════════════════════════════════════════════╗\n")
(printf "║                    TEST RESULTS                             ║\n")
(printf "╚════════════════════════════════════════════════════════════╝\n\n")

(printf "Tests passed: ~a\n" tests-passed)
(printf "Tests failed: ~a\n" tests-failed)
(printf "Total tests:  ~a\n" (+ tests-passed tests-failed))

(if (= tests-failed 0)
    (printf "\n✓ All tests passed! String utilities are working correctly.\n\n")
    (printf "\n✗ Some tests failed. Please review.\n\n"))
