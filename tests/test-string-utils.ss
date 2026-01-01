;;; test-string-utils.ss — Comprehensive Tests for String Utilities
;;;
;;; Tests for thimble/string-utils.ss
;;; Wishlist Item #3 (High Priority)

(define *quiet* #t)
(load "shell/repl.ss")
(load "shell/string-utils.ss")

(display "\n╔════════════════════════════════════════════════════════════╗\n")
(display "║  STRING UTILITIES TEST SUITE                               ║\n")
(display "║  Testing thimble/string-utils.ss                           ║\n")
(display "╚════════════════════════════════════════════════════════════╝\n\n")

(define test-count 0)
(define pass-count 0)
(define fail-count 0)

(define (test name expected actual)
  (set! test-count (+ test-count 1))
  (if (equal? expected actual)
      (begin
       (set! pass-count (+ pass-count 1))
       (printf "✓ ~a\n" name))
      (begin
       (set! fail-count (+ fail-count 1))
       (printf "✗ ~a\n  Expected: ~s\n  Got: ~s\n" name expected actual))))

(display "═══ STRING SPLITTING ═══\n")
(test "string-split: basic"
      '("foo" "bar" "baz")
      (string-split "foo,bar,baz" #\,))
(test "string-split: empty parts"
      '("a" "" "b")
      (string-split "a,,b" #\,))
(test "string-split: empty string"
      '("")
      (string-split "" #\,))
(test "string-split: no delimiter"
      '("hello")
      (string-split "hello" #\,))
(test "string-split: newlines"
      '("line1" "line2" "line3")
      (string-split "line1\nline2\nline3" #\newline))

(display "\n═══ STRING JOINING ═══\n")
(test "string-join: basic"
      "foo, bar, baz"
      (string-join '("foo" "bar" "baz") ", "))
(test "string-join: single element"
      "hello"
      (string-join '("hello") ", "))
(test "string-join: empty list"
      ""
      (string-join '() ", "))
(test "string-join: empty separator"
      "abc"
      (string-join '("a" "b" "c") ""))

(display "\n═══ STRING SEARCHING ═══\n")
(test "string-contains?: found"
      #t
      (string-contains? "hello world" "world"))
(test "string-contains?: not found"
      #f
      (string-contains? "hello" "bye"))
(test "string-contains?: empty needle"
      #t
      (string-contains? "test" ""))
(test "string-contains?: same string"
      #t
      (string-contains? "hello" "hello"))

(test "string-starts-with?: match"
      #t
      (string-starts-with? "hello world" "hello"))
(test "string-starts-with?: no match"
      #f
      (string-starts-with? "test" "best"))
(test "string-starts-with?: empty prefix"
      #t
      (string-starts-with? "foo" ""))
(test "string-starts-with?: exact match"
      #t
      (string-starts-with? "hello" "hello"))

(test "string-ends-with?: match"
      #t
      (string-ends-with? "hello world" "world"))
(test "string-ends-with?: no match"
      #f
      (string-ends-with? "test" "best"))
(test "string-ends-with?: empty suffix"
      #t
      (string-ends-with? "foo" ""))
(test "string-ends-with?: exact match"
      #t
      (string-ends-with? "hello" "hello"))

(test "string-index: found"
      2
      (string-index "hello" #\l))
(test "string-index: not found"
      #f
      (string-index "hello" #\x))
(test "string-index: first char"
      0
      (string-index "hello" #\h))

(test "string-index-of: found"
      6
      (string-index-of "hello world" "world"))
(test "string-index-of: not found"
      #f
      (string-index-of "test" "best"))
(test "string-index-of: empty needle"
      0
      (string-index-of "test" ""))

(display "\n═══ STRING REPLACEMENT ═══\n")
(test "string-replace: basic"
      "hello universe"
      (string-replace "hello world" "world" "universe"))
(test "string-replace: all occurrences"
      "bbb"
      (string-replace "aaa" "a" "b"))
(test "string-replace: not found"
      "test"
      (string-replace "test" "x" "y"))
(test "string-replace: empty old"
      "test"
      (string-replace "test" "" "x"))

(test "string-replace-first: first only"
      "hi hello"
      (string-replace-first "hello hello" "hello" "hi"))
(test "string-replace-first: single"
      "baa"
      (string-replace-first "aaa" "a" "b"))
(test "string-replace-first: not found"
      "test"
      (string-replace-first "test" "x" "y"))

(display "\n═══ STRING TRIMMING ═══\n")
(test "string-trim: both sides"
      "hello"
      (string-trim "  hello  "))
(test "string-trim: tabs and newlines"
      "test"
      (string-trim "\n\ttest\n"))
(test "string-trim: all whitespace"
      ""
      (string-trim "   "))
(test "string-trim: no whitespace"
      "hello"
      (string-trim "hello"))

(test "string-trim-left: left only"
      "hello  "
      (string-trim-left "  hello  "))
(test "string-trim-left: no leading"
      "hello  "
      (string-trim-left "hello  "))

(test "string-trim-right: right only"
      "  hello"
      (string-trim-right "  hello  "))
(test "string-trim-right: no trailing"
      "  hello"
      (string-trim-right "  hello"))

(display "\n═══ STRING PREDICATES ═══\n")
(test "string-empty?: yes"
      #t
      (string-empty? ""))
(test "string-empty?: no"
      #f
      (string-empty? "hello"))

(test "string-blank?: empty"
      #t
      (string-blank? ""))
(test "string-blank?: whitespace only"
      #t
      (string-blank? "   "))
(test "string-blank?: has content"
      #f
      (string-blank? "  a  "))
(test "string-blank?: tabs"
      #t
      (string-blank? "\t\t"))

(display "\n═══ EDGE CASES ═══\n")
(test "string-split: single delimiter"
      '("" "")
      (string-split "," #\,))
(test "string-contains?: substring at end"
      #t
      (string-contains? "hello" "lo"))
(test "string-contains?: substring at start"
      #t
      (string-contains? "hello" "he"))
(test "string-trim: newlines and tabs"
      "mixed\twhitespace"
      (string-trim "\n  mixed\twhitespace  \n"))
(test "string-replace: overlapping pattern"
      "xc"
      (string-replace "abc" "ab" "x"))

(display "\n═══ INTEGRATION TESTS ═══\n")
(test "split-trim-join: CSV processing"
      '("foo" "bar" "baz")
      (map string-trim (string-split "foo, bar, baz" #\,)))

(test "trim-check-empty: pipeline"
      #t
      (string-empty? (string-trim "   ")))

(test "split-filter-join: word extraction"
      "one two three"
      (string-join
       (filter (lambda (s) (not (string-empty? s)))
               (string-split "one  two  three" #\space))
       " "))

(display "\n╔════════════════════════════════════════════════════════════╗\n")
(printf "║  TEST RESULTS                                              ║\n")
(printf "║  Total:  ~a tests                                           ║\n" test-count)
(printf "║  Passed: ~a (✓)                                             ║\n" pass-count)
(printf "║  Failed: ~a (✗)                                             ║\n" fail-count)
(display "╚════════════════════════════════════════════════════════════╝\n\n")

(if (= fail-count 0)
    (display "✓ All tests passed! String utilities are working correctly.\n")
    (display "✗ Some tests failed. Please review the failures above.\n"))
