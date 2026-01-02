;;; fabric/stitches/test-parse.ss — Test Vectors for Parser Combinators
;;;
;;; Comprehensive tests for the parser combinator library.
;;; Tests cover primitives, combinators, and example parsers.
;;;
;;; This is Core test code: verifies pure functionality.

;;; Load the parser library
(load "core/lang/parse.ss")

;;; ============================================================
;;; Test Helpers
;;; ============================================================

(define (assert-true condition msg)
  (if condition
      #t
      (begin
        (display "FAIL: ")
        (display msg)
        (newline)
        #f)))

(define (assert-equal actual expected msg)
  (if (equal? actual expected)
      #t
      (begin
        (display "FAIL: ")
        (display msg)
        (newline)
        (display "  Expected: ")
        (display expected)
        (newline)
        (display "  Actual:   ")
        (display actual)
        (newline)
        #f)))

(define (assert-parse-ok result msg)
  (assert-true (parse-ok? result) msg))

(define (assert-parse-err result msg)
  (assert-true (parse-err? result) msg))

;;; ============================================================
;;; ParseResult Tests
;;; ============================================================

(define (test-parse-result)
  (display "Testing ParseResult construction...")
  (newline)

  ;; Success result
  (let ([result (success 42 "remaining")])
    (assert-parse-ok result "success creates ok result")
    (assert-equal (result-value result) 42 "success value")
    (assert-equal (result-remaining result) "remaining" "success remaining"))

  ;; Failure result
  (let ([result (failure "expected char" 5)])
    (assert-parse-err result "failure creates err result")
    (assert-equal (result-expected result) "expected char" "failure expected")
    (assert-equal (result-position result) 5 "failure position"))

  (display "ParseResult tests complete.")
  (newline))

;;; ============================================================
;;; Primitive Parser Tests
;;; ============================================================

(define (test-primitives)
  (display "Testing primitive parsers...")
  (newline)

  ;; pure always succeeds
  (let ([result (run-parser (pure 42) "hello")])
    (assert-parse-ok result "pure succeeds")
    (assert-equal (result-value result) 42 "pure returns value")
    (assert-equal (result-remaining result) "hello" "pure consumes nothing"))

  ;; fail always fails
  (let ([result (run-parser (fail "error message") "hello")])
    (assert-parse-err result "fail fails")
    (assert-equal (result-expected result) "error message" "fail message"))

  ;; item consumes one character
  (let ([result (run-parser item "abc")])
    (assert-parse-ok result "item succeeds on non-empty")
    (assert-equal (result-value result) #\a "item returns first char")
    (assert-equal (result-remaining result) "bc" "item consumes one char"))

  ;; item fails on empty
  (let ([result (run-parser item "")])
    (assert-parse-err result "item fails on empty"))

  ;; eof succeeds on empty
  (let ([result (run-parser eof "")])
    (assert-parse-ok result "eof succeeds on empty"))

  ;; eof fails on non-empty
  (let ([result (run-parser eof "abc")])
    (assert-parse-err result "eof fails on non-empty"))

  ;; satisfy with matching predicate
  (let ([result (run-parser (satisfy char-numeric? "digit") "5abc")])
    (assert-parse-ok result "satisfy succeeds on match")
    (assert-equal (result-value result) #\5 "satisfy returns matching char"))

  ;; satisfy with non-matching predicate
  (let ([result (run-parser (satisfy char-numeric? "digit") "abc")])
    (assert-parse-err result "satisfy fails on no match"))

  (display "Primitive parser tests complete.")
  (newline))

;;; ============================================================
;;; Character Parser Tests
;;; ============================================================

(define (test-char-parsers)
  (display "Testing character parsers...")
  (newline)

  ;; char matches specific character
  (let ([result (run-parser (char #\a) "abc")])
    (assert-parse-ok result "char succeeds on match")
    (assert-equal (result-value result) #\a "char returns matched char")
    (assert-equal (result-remaining result) "bc" "char consumes one char"))

  ;; char fails on mismatch
  (let ([result (run-parser (char #\a) "xyz")])
    (assert-parse-err result "char fails on mismatch"))

  ;; digit matches digit
  (let ([result (run-parser digit "3abc")])
    (assert-parse-ok result "digit matches digit")
    (assert-equal (result-value result) #\3 "digit value"))

  ;; digit fails on non-digit
  (let ([result (run-parser digit "abc")])
    (assert-parse-err result "digit fails on non-digit"))

  ;; alpha matches letter
  (let ([result (run-parser alpha "abc")])
    (assert-parse-ok result "alpha matches letter")
    (assert-equal (result-value result) #\a "alpha value"))

  ;; space matches whitespace
  (let ([result (run-parser space " abc")])
    (assert-parse-ok result "space matches whitespace")
    (assert-equal (result-value result) #\space "space value"))

  ;; string-p matches exact string
  (let ([result (run-parser (string-p "hello") "hello world")])
    (assert-parse-ok result "string-p matches exact string")
    (assert-equal (result-value result) "hello" "string-p value")
    (assert-equal (result-remaining result) " world" "string-p remaining"))

  ;; string-p fails on mismatch
  (let ([result (run-parser (string-p "hello") "goodbye")])
    (assert-parse-err result "string-p fails on mismatch"))

  ;; string-p fails on partial match
  (let ([result (run-parser (string-p "hello") "hel")])
    (assert-parse-err result "string-p fails on too-short input"))

  (display "Character parser tests complete.")
  (newline))

;;; ============================================================
;;; Combinator Tests — Sequencing
;;; ============================================================

(define (test-sequencing)
  (display "Testing sequencing combinators...")
  (newline)

  ;; bind threads results
  (let ([parser (bind (char #\a) (lambda (c) (pure (char-upcase c))))])
    (let ([result (run-parser parser "abc")])
      (assert-parse-ok result "bind succeeds")
      (assert-equal (result-value result) #\A "bind threads value")))

  ;; bind propagates failure
  (let ([parser (bind (char #\a) (lambda (c) (fail "error")))])
    (let ([result (run-parser parser "abc")])
      (assert-parse-err result "bind propagates failure from second parser")))

  ;; seq keeps second result
  (let ([parser (seq (char #\a) (char #\b))])
    (let ([result (run-parser parser "abc")])
      (assert-parse-ok result "seq succeeds")
      (assert-equal (result-value result) #\b "seq keeps second value")))

  ;; seq-left keeps first result
  (let ([parser (seq-left (char #\a) (char #\b))])
    (let ([result (run-parser parser "abc")])
      (assert-parse-ok result "seq-left succeeds")
      (assert-equal (result-value result) #\a "seq-left keeps first value")))

  (display "Sequencing combinator tests complete.")
  (newline))

;;; ============================================================
;;; Combinator Tests — Choice
;;; ============================================================

(define (test-choice)
  (display "Testing choice combinators...")
  (newline)

  ;; alt tries first, succeeds
  (let ([parser (alt (char #\a) (char #\b))])
    (let ([result (run-parser parser "abc")])
      (assert-parse-ok result "alt succeeds on first parser")
      (assert-equal (result-value result) #\a "alt value from first")))

  ;; alt tries second on first failure
  (let ([parser (alt (char #\a) (char #\b))])
    (let ([result (run-parser parser "bac")])
      (assert-parse-ok result "alt succeeds on second parser")
      (assert-equal (result-value result) #\b "alt value from second")))

  ;; alt fails if both fail
  (let ([parser (alt (char #\a) (char #\b))])
    (let ([result (run-parser parser "xyz")])
      (assert-parse-err result "alt fails when both fail")))

  ;; choice tries multiple alternatives
  (let ([parser (choice (list (char #\a) (char #\b) (char #\c)))])
    (let ([result (run-parser parser "bac")])
      (assert-parse-ok result "choice succeeds on matching alternative")
      (assert-equal (result-value result) #\b "choice value")))

  (display "Choice combinator tests complete.")
  (newline))

;;; ============================================================
;;; Combinator Tests — Transformation
;;; ============================================================

(define (test-transformation)
  (display "Testing transformation combinators...")
  (newline)

  ;; map-p transforms result
  (let ([parser (map-p char-upcase (char #\a))])
    (let ([result (run-parser parser "abc")])
      (assert-parse-ok result "map-p succeeds")
      (assert-equal (result-value result) #\A "map-p transforms value")))

  ;; map-p with list->string
  (let ([parser (map-p list->string (many alpha))])
    (let ([result (run-parser parser "hello123")])
      (assert-parse-ok result "map-p with list->string")
      (assert-equal (result-value result) "hello" "map-p result")))

  (display "Transformation combinator tests complete.")
  (newline))

;;; ============================================================
;;; Combinator Tests — Repetition
;;; ============================================================

(define (test-repetition)
  (display "Testing repetition combinators...")
  (newline)

  ;; many succeeds with zero matches
  (let ([parser (many (char #\a))])
    (let ([result (run-parser parser "bbb")])
      (assert-parse-ok result "many succeeds with zero")
      (assert-equal (result-value result) '() "many zero value")))

  ;; many collects multiple matches
  (let ([parser (many (char #\a))])
    (let ([result (run-parser parser "aaabbb")])
      (assert-parse-ok result "many succeeds with multiple")
      (assert-equal (result-value result) '(#\a #\a #\a) "many multiple values")
      (assert-equal (result-remaining result) "bbb" "many remaining")))

  ;; many1 requires at least one
  (let ([parser (many1 (char #\a))])
    (let ([result (run-parser parser "bbb")])
      (assert-parse-err result "many1 fails with zero matches")))

  ;; many1 succeeds with one or more
  (let ([parser (many1 (char #\a))])
    (let ([result (run-parser parser "aaabbb")])
      (assert-parse-ok result "many1 succeeds with multiple")
      (assert-equal (result-value result) '(#\a #\a #\a) "many1 values")))

  ;; sep-by with zero items
  (let ([parser (sep-by digit (char #\,))])
    (let ([result (run-parser parser "abc")])
      (assert-parse-ok result "sep-by succeeds with zero")
      (assert-equal (result-value result) '() "sep-by zero value")))

  ;; sep-by with multiple items
  (let ([parser (sep-by digit (char #\,))])
    (let ([result (run-parser parser "1,2,3abc")])
      (assert-parse-ok result "sep-by succeeds with multiple")
      (assert-equal (result-value result) '(#\1 #\2 #\3) "sep-by values")
      (assert-equal (result-remaining result) "abc" "sep-by remaining")))

  ;; sep-by with one item (no separator)
  (let ([parser (sep-by digit (char #\,))])
    (let ([result (run-parser parser "5abc")])
      (assert-parse-ok result "sep-by succeeds with one")
      (assert-equal (result-value result) '(#\5) "sep-by one value")))

  ;; optional succeeds with match
  (let ([parser (optional (char #\a))])
    (let ([result (run-parser parser "abc")])
      (assert-parse-ok result "optional succeeds with match")
      (assert-equal (result-value result) '(#\a) "optional value with match")))

  ;; optional succeeds without match
  (let ([parser (optional (char #\a))])
    (let ([result (run-parser parser "bbb")])
      (assert-parse-ok result "optional succeeds without match")
      (assert-equal (result-value result) '() "optional value without match")))

  (display "Repetition combinator tests complete.")
  (newline))

;;; ============================================================
;;; Combinator Tests — Delimiters
;;; ============================================================

(define (test-delimiters)
  (display "Testing delimiter combinators...")
  (newline)

  ;; between parses content between delimiters
  (let ([parser (between (char #\() (char #\)) alpha)])
    (let ([result (run-parser parser "(a)bc")])
      (assert-parse-ok result "between succeeds")
      (assert-equal (result-value result) #\a "between value")
      (assert-equal (result-remaining result) "bc" "between remaining")))

  ;; between fails if open missing
  (let ([parser (between (char #\() (char #\)) alpha)])
    (let ([result (run-parser parser "a)bc")])
      (assert-parse-err result "between fails without open delimiter")))

  ;; between fails if close missing
  (let ([parser (between (char #\() (char #\)) alpha)])
    (let ([result (run-parser parser "(abc")])
      (assert-parse-err result "between fails without close delimiter")))

  (display "Delimiter combinator tests complete.")
  (newline))

;;; ============================================================
;;; Number Parser Tests
;;; ============================================================

(define (test-number-parsers)
  (display "Testing number parsers...")
  (newline)

  ;; nat parses natural numbers
  (let ([result (run-parser nat "123abc")])
    (assert-parse-ok result "nat succeeds")
    (assert-equal (result-value result) 123 "nat value")
    (assert-equal (result-remaining result) "abc" "nat remaining"))

  ;; nat fails on non-digit
  (let ([result (run-parser nat "abc")])
    (assert-parse-err result "nat fails on non-digit"))

  ;; int parses positive integers
  (let ([result (run-parser int "123abc")])
    (assert-parse-ok result "int parses positive")
    (assert-equal (result-value result) 123 "int positive value"))

  ;; int parses negative integers
  (let ([result (run-parser int "-456abc")])
    (assert-parse-ok result "int parses negative")
    (assert-equal (result-value result) -456 "int negative value"))

  (display "Number parser tests complete.")
  (newline))

;;; ============================================================
;;; Word Parser Tests
;;; ============================================================

(define (test-word-parsers)
  (display "Testing word parsers...")
  (newline)

  ;; word parses alphabetic sequence
  (let ([result (run-parser word "hello123")])
    (assert-parse-ok result "word succeeds")
    (assert-equal (result-value result) "hello" "word value")
    (assert-equal (result-remaining result) "123" "word remaining"))

  ;; identifier parses valid identifier
  (let ([result (run-parser identifier "hello_world123 ")])
    (assert-parse-ok result "identifier succeeds")
    (assert-equal (result-value result) "hello_world123" "identifier value"))

  ;; identifier requires letter start
  (let ([result (run-parser identifier "123abc")])
    (assert-parse-err result "identifier fails on digit start"))

  (display "Word parser tests complete.")
  (newline))

;;; ============================================================
;;; Utility Parser Tests
;;; ============================================================

(define (test-utilities)
  (display "Testing utility parsers...")
  (newline)

  ;; spaces parses whitespace
  (let ([result (run-parser spaces "   abc")])
    (assert-parse-ok result "spaces succeeds")
    (assert-equal (length (result-value result)) 3 "spaces count")
    (assert-equal (result-remaining result) "abc" "spaces remaining"))

  ;; spaces succeeds on no whitespace
  (let ([result (run-parser spaces "abc")])
    (assert-parse-ok result "spaces succeeds on no whitespace")
    (assert-equal (result-value result) '() "spaces zero value"))

  ;; lexeme skips trailing whitespace
  (let ([parser (lexeme (string-p "hello"))])
    (let ([result (run-parser parser "hello   world")])
      (assert-parse-ok result "lexeme succeeds")
      (assert-equal (result-value result) "hello" "lexeme value")
      (assert-equal (result-remaining result) "world" "lexeme skips trailing space")))

  ;; symbol parses string and skips whitespace
  (let ([result (run-parser (symbol "hello") "hello   world")])
    (assert-parse-ok result "symbol succeeds")
    (assert-equal (result-remaining result) "world" "symbol remaining"))

  ;; one-of matches any char in string
  (let ([parser (one-of "abc")])
    (let ([result (run-parser parser "bxyz")])
      (assert-parse-ok result "one-of succeeds")
      (assert-equal (result-value result) #\b "one-of value")))

  ;; one-of fails on no match
  (let ([parser (one-of "abc")])
    (let ([result (run-parser parser "xyz")])
      (assert-parse-err result "one-of fails on no match")))

  ;; count parses exactly n items
  (let ([parser (count 3 digit)])
    (let ([result (run-parser parser "123abc")])
      (assert-parse-ok result "count succeeds")
      (assert-equal (result-value result) '(#\1 #\2 #\3) "count values")
      (assert-equal (result-remaining result) "abc" "count remaining")))

  ;; count fails on too few items
  (let ([parser (count 5 digit)])
    (let ([result (run-parser parser "123abc")])
      (assert-parse-err result "count fails on too few")))

  (display "Utility parser tests complete.")
  (newline))

;;; ============================================================
;;; Complete Example Parser — Simple Arithmetic
;;; ============================================================

(define (test-arithmetic-parser)
  ;; Simple expression parser: parses "num op num" like "3 + 5"
  (define addop
    (alt (map-p (lambda (_) +) (char #\+))
         (map-p (lambda (_) -) (char #\-))))

  (define mulop
    (alt (map-p (lambda (_) *) (char #\*))
         (map-p (lambda (_) /) (char #\/))))

  ;; Simple expression: number operator number
  (define simple-expr
    (bind nat (lambda (left)
      (bind spaces (lambda (_)
        (bind addop (lambda (op)
          (bind spaces (lambda (_)
            (bind nat (lambda (right)
              (pure (op left right)))))))))))))

  (display "Testing complete arithmetic parser...")
  (newline)

  ;; Test addition
  (let ([result (run-parser simple-expr "3 + 5")])
    (assert-parse-ok result "arithmetic parser succeeds on addition")
    (assert-equal (result-value result) 8 "addition result"))

  ;; Test subtraction
  (let ([result (run-parser simple-expr "10 - 3")])
    (assert-parse-ok result "arithmetic parser succeeds on subtraction")
    (assert-equal (result-value result) 7 "subtraction result"))

  (display "Arithmetic parser tests complete.")
  (newline))

;;; ============================================================
;;; Complete Example Parser — CSV
;;; ============================================================

(define (test-csv-parser)
  ;; CSV field: sequence of non-comma characters
  (define csv-field
    (map-p list->string (many (none-of ",\n"))))

  ;; CSV line: comma-separated fields
  (define csv-line
    (sep-by csv-field (char #\,)))

  (display "Testing CSV parser...")
  (newline)

  ;; Test single field
  (let ([result (run-parser csv-line "hello")])
    (assert-parse-ok result "csv single field")
    (assert-equal (result-value result) '("hello") "csv single value"))

  ;; Test multiple fields
  (let ([result (run-parser csv-line "one,two,three")])
    (assert-parse-ok result "csv multiple fields")
    (assert-equal (result-value result) '("one" "two" "three") "csv values"))

  ;; Test empty fields
  (let ([result (run-parser csv-line "a,,c")])
    (assert-parse-ok result "csv with empty fields")
    (assert-equal (result-value result) '("a" "" "c") "csv with empty"))

  (display "CSV parser tests complete.")
  (newline))

;;; ============================================================
;;; Complete Example Parser — S-expressions (simple)
;;; ============================================================

(define (test-sexpr-parser)
  ;; Atom: identifier or number
  (define atom
    (alt (map-p (lambda (s) (string->symbol s)) identifier)
         nat))

  ;; Simple S-expr: atom or (atom atom ...)
  ;; Note: This is a simplified version, not handling nesting
  (define simple-sexpr
    (alt atom
         (between (char #\()
                  (char #\))
                  (sep-by atom space))))

  (display "Testing simple S-expression parser...")
  (newline)

  ;; Test atom
  (let ([result (run-parser simple-sexpr "hello")])
    (assert-parse-ok result "sexpr atom")
    (assert-equal (result-value result) 'hello "sexpr atom value"))

  ;; Test number
  (let ([result (run-parser simple-sexpr "42")])
    (assert-parse-ok result "sexpr number")
    (assert-equal (result-value result) 42 "sexpr number value"))

  ;; Test list (using identifiers that start with letters)
  (let ([result (run-parser simple-sexpr "(add 1 2)")])
    (assert-parse-ok result "sexpr list")
    (assert-equal (result-value result) '(add 1 2) "sexpr list value"))

  (display "S-expression parser tests complete.")
  (newline))

;;; ============================================================
;;; Run All Tests
;;; ============================================================

(define (run-all-tests)
  (display "========================================")
  (newline)
  (display "Parser Combinator Test Suite")
  (newline)
  (display "========================================")
  (newline)
  (newline)

  (test-parse-result)
  (newline)

  (test-primitives)
  (newline)

  (test-char-parsers)
  (newline)

  (test-sequencing)
  (newline)

  (test-choice)
  (newline)

  (test-transformation)
  (newline)

  (test-repetition)
  (newline)

  (test-delimiters)
  (newline)

  (test-number-parsers)
  (newline)

  (test-word-parsers)
  (newline)

  (test-utilities)
  (newline)

  (test-arithmetic-parser)
  (newline)

  (test-csv-parser)
  (newline)

  (test-sexpr-parser)
  (newline)

  (display "========================================")
  (newline)
  (display "All tests complete!")
  (newline)
  (display "========================================")
  (newline))

;;; Run tests:
(run-all-tests)
