;;; thimble/test-string-utils.ss — Tests for String Utilities
;;;
;;; Comprehensive test suite for thimble/string-utils.ss
;;; Validates all string operations with edge cases and Unicode handling.

(load "shell/string-utils.ss")

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

;;; ============================================================
;;; String Splitting Tests
;;; ============================================================

(printf "\n=== Testing string-split ===\n")

(assert-equal "split simple comma-separated"
              (string-split "foo,bar,baz" #\,)
              '("foo" "bar" "baz"))

(assert-equal "split with empty segments"
              (string-split "a,,b" #\,)
              '("a" "" "b"))

(assert-equal "split empty string"
              (string-split "" #\,)
              '(""))

(assert-equal "split with no delimiter"
              (string-split "hello" #\,)
              '("hello"))

(assert-equal "split at start"
              (string-split ",foo" #\,)
              '("" "foo"))

(assert-equal "split at end"
              (string-split "foo," #\,)
              '("foo" ""))

(assert-equal "split all delimiters"
              (string-split ",,," #\,)
              '("" "" "" ""))

;;; ============================================================
;;; String Lines Tests
;;; ============================================================

(printf "\n=== Testing string-split-lines ===\n")

(assert-equal "split Unix line endings"
              (string-split-lines "foo\nbar\nbaz")
              '("foo" "bar" "baz"))

(assert-equal "split Windows line endings"
              (string-split-lines "foo\r\nbar\r\nbaz")
              '("foo" "bar" "baz"))

(assert-equal "split mixed line endings"
              (string-split-lines "foo\nbar\r\nbaz")
              '("foo" "bar" "baz"))

(assert-equal "split single line"
              (string-split-lines "hello")
              '("hello"))

;;; ============================================================
;;; String Join Tests
;;; ============================================================

(printf "\n=== Testing string-join ===\n")

(assert-equal "join simple list"
              (string-join '("foo" "bar" "baz") ", ")
              "foo, bar, baz")

(assert-equal "join single element"
              (string-join '("a") ",")
              "a")

(assert-equal "join empty list"
              (string-join '() ",")
              "")

(assert-equal "join with empty separator"
              (string-join '("a" "b" "c") "")
              "abc")

(assert-equal "join with space"
              (string-join '("hello" "world") " ")
              "hello world")

;;; ============================================================
;;; String Contains Tests
;;; ============================================================

(printf "\n=== Testing string-contains? ===\n")

(assert-equal "contains at end"
              (string-contains? "hello world" "world")
              #t)

(assert-equal "contains at start"
              (string-contains? "hello world" "hello")
              #t)

(assert-equal "contains in middle"
              (string-contains? "hello world" "lo wo")
              #t)

(assert-equal "does not contain"
              (string-contains? "hello" "bye")
              #f)

(assert-equal "contains empty string"
              (string-contains? "test" "")
              #t)

(assert-equal "empty haystack"
              (string-contains? "" "test")
              #f)

(assert-equal "exact match"
              (string-contains? "hello" "hello")
              #t)

;;; ============================================================
;;; String Starts/Ends With Tests
;;; ============================================================

(printf "\n=== Testing string-starts-with? ===\n")

(assert-equal "starts with prefix"
              (string-starts-with? "hello world" "hello")
              #t)

(assert-equal "does not start with prefix"
              (string-starts-with? "hello world" "world")
              #f)

(assert-equal "starts with empty string"
              (string-starts-with? "test" "")
              #t)

(assert-equal "starts with self"
              (string-starts-with? "hello" "hello")
              #t)

(printf "\n=== Testing string-ends-with? ===\n")

(assert-equal "ends with suffix"
              (string-ends-with? "hello world" "world")
              #t)

(assert-equal "does not end with suffix"
              (string-ends-with? "hello world" "hello")
              #f)

(assert-equal "ends with empty string"
              (string-ends-with? "test" "")
              #t)

(assert-equal "ends with self"
              (string-ends-with? "hello" "hello")
              #t)

;;; ============================================================
;;; String Index Tests
;;; ============================================================

(printf "\n=== Testing string-index ===\n")

(assert-equal "find first occurrence"
              (string-index "hello" #\l)
              2)

(assert-equal "character not found"
              (string-index "hello" #\x)
              #f)

(assert-equal "find at start"
              (string-index "hello" #\h)
              0)

(assert-equal "find at end"
              (string-index "hello" #\o)
              4)

(printf "\n=== Testing string-index-of ===\n")

(assert-equal "find substring"
              (string-index-of "hello world" "world")
              6)

(assert-equal "substring not found"
              (string-index-of "test" "best")
              #f)

(assert-equal "find at start"
              (string-index-of "hello world" "hello")
              0)

(assert-equal "find empty string"
              (string-index-of "test" "")
              0)

;;; ============================================================
;;; String Replace Tests
;;; ============================================================

(printf "\n=== Testing string-replace ===\n")

(assert-equal "replace single occurrence"
              (string-replace "hello world" "world" "universe")
              "hello universe")

(assert-equal "replace multiple occurrences"
              (string-replace "aaa" "a" "b")
              "bbb")

(assert-equal "replace non-existent"
              (string-replace "test" "x" "y")
              "test")

(assert-equal "replace with empty string"
              (string-replace "hello world" " " "")
              "helloworld")

(assert-equal "replace empty old (no-op)"
              (string-replace "test" "" "x")
              "test")

(printf "\n=== Testing string-replace-first ===\n")

(assert-equal "replace first occurrence only"
              (string-replace-first "hello hello" "hello" "hi")
              "hi hello")

(assert-equal "replace first of many"
              (string-replace-first "aaa" "a" "b")
              "baa")

(assert-equal "replace non-existent"
              (string-replace-first "test" "x" "y")
              "test")

;;; ============================================================
;;; String Trim Tests
;;; ============================================================

(printf "\n=== Testing string-trim ===\n")

(assert-equal "trim both sides"
              (string-trim "  hello  ")
              "hello")

(assert-equal "trim newlines and tabs"
              (string-trim "\n\ttest\n")
              "test")

(assert-equal "trim all whitespace"
              (string-trim "   ")
              "")

(assert-equal "trim nothing"
              (string-trim "hello")
              "hello")

(assert-equal "trim mixed whitespace"
              (string-trim "  \t\n  hello world  \r\n  ")
              "hello world")

(printf "\n=== Testing string-trim-left ===\n")

(assert-equal "trim left only"
              (string-trim-left "  hello  ")
              "hello  ")

(assert-equal "trim left nothing"
              (string-trim-left "hello  ")
              "hello  ")

(printf "\n=== Testing string-trim-right ===\n")

(assert-equal "trim right only"
              (string-trim-right "  hello  ")
              "  hello")

(assert-equal "trim right nothing"
              (string-trim-right "  hello")
              "  hello")

;;; ============================================================
;;; String Predicate Tests
;;; ============================================================

(printf "\n=== Testing string-empty? ===\n")

(assert-equal "empty string"
              (string-empty? "")
              #t)

(assert-equal "non-empty string"
              (string-empty? "hello")
              #f)

(assert-equal "whitespace is not empty"
              (string-empty? "  ")
              #f)

(printf "\n=== Testing string-blank? ===\n")

(assert-equal "blank: empty string"
              (string-blank? "")
              #t)

(assert-equal "blank: whitespace only"
              (string-blank? "   ")
              #t)

(assert-equal "blank: mixed whitespace"
              (string-blank? " \t\n\r ")
              #t)

(assert-equal "not blank: has content"
              (string-blank? "  a  ")
              #f)

;;; ============================================================
;;; Unicode and Edge Cases
;;; ============================================================

(printf "\n=== Testing Unicode handling ===\n")

(assert-equal "unicode split"
              (string-split "α,β,γ" #\,)
              '("α" "β" "γ"))

(assert-equal "unicode contains"
              (string-contains? "Hello 世界" "世界")
              #t)

(assert-equal "unicode replace"
              (string-replace "Hello 世界" "世界" "World")
              "Hello World")

(assert-equal "emoji split"
              (string-split "🔥,💧,🌍" #\,)
              '("🔥" "💧" "🌍"))

;;; ============================================================
;;; Additional String Search Tests
;;; ============================================================

(printf "\n=== Testing string-last-index-of ===\n")

(assert-equal "find last occurrence"
              (string-last-index-of "hello hello" "lo")
              9)

(assert-equal "last occurrence at start"
              (string-last-index-of "hello" "he")
              0)

(assert-equal "last occurrence not found"
              (string-last-index-of "hello" "xyz")
              #f)

(printf "\n=== Testing string-index-right ===\n")

(assert-equal "find last char"
              (string-index-right "hello" #\l)
              3)

(assert-equal "char not found from right"
              (string-index-right "hello" #\x)
              #f)

;;; ============================================================
;;; String Transformation Tests
;;; ============================================================

(printf "\n=== Testing string-reverse ===\n")

(assert-equal "reverse basic string"
              (string-reverse "hello")
              "olleh")

(assert-equal "reverse empty string"
              (string-reverse "")
              "")

(printf "\n=== Testing string-upcase/downcase ===\n")

(assert-equal "upcase"
              (string-upcase "hello")
              "HELLO")

(assert-equal "downcase"
              (string-downcase "HELLO")
              "hello")

;;; ============================================================
;;; String Padding Tests
;;; ============================================================

(printf "\n=== Testing string-pad ===\n")

(assert-equal "pad-left"
              (string-pad-left "42" 5 #\0)
              "00042")

(assert-equal "pad-left already long"
              (string-pad-left "hello" 3 #\space)
              "hello")

(assert-equal "pad-right"
              (string-pad-right "hi" 5 #\space)
              "hi   ")

;;; ============================================================
;;; Additional Predicate Tests
;;; ============================================================

(printf "\n=== Testing string-all-match? ===\n")

(assert-equal "all match digits"
              (string-all-match? "12345" char-numeric?)
              #t)

(assert-equal "not all match digits"
              (string-all-match? "123a5" char-numeric?)
              #f)

;;; ============================================================
;;; Integration and Edge Case Tests
;;; ============================================================

(printf "\n=== Testing Integration Cases ===\n")

(assert-equal "split with string delimiter"
              (string-split "a<sep>b<sep>c" "<sep>")
              '("a" "b" "c"))

(assert-equal "split with empty string delimiter"
              (string-split "hi" "")
              '("h" "i"))

(assert-equal "CSV pipeline"
              (map string-trim (string-split "foo, bar, baz" #\,))
              '("foo" "bar" "baz"))

(assert-equal "word extraction"
              (string-join
               (filter (lambda (s) (not (string-empty? s)))
                       (string-split "one  two  three" #\space))
               " ")
              "one two three")

(assert-equal "trim-check-empty"
              (string-empty? (string-trim "   "))
              #t)

;;; ============================================================
;;; Test Summary
;;; ============================================================

(printf "\n========================================\n")
(printf "Test Results:\n")
(printf "  Total:  ~a\n" test-count)
(printf "  Passed: ~a\n" pass-count)
(printf "  Failed: ~a\n" fail-count)

(if (= fail-count 0)
    (printf "\n✓ All tests passed!\n\n")
    (printf "\n✗ Some tests failed\n\n"))

;;; Return success/failure
(= fail-count 0)
