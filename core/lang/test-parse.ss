;;; fabric/stitches/test-parse.ss — Test Vectors for Parser Combinators
;;; Standardized to use test-framework.ss

(load "core/testing/test-framework.ss")
(load "core/lang/parse.ss")

;;; ============================================================================
;;; Tests
;;; ============================================================================

(test-group parse-result
  (define-test "success creates ok result"
    (let ([result (success 42 "remaining")])
      (assert-true (parse-ok? result))))

  (define-test "success value"
    (let ([result (success 42 "remaining")])
      (assert-equal 42 (result-value result))))

  (define-test "success remaining"
    (let ([result (success 42 "remaining")])
      (assert-equal "remaining" (result-remaining result))))

  (define-test "failure creates err result"
    (let ([result (failure "expected char" 5)])
      (assert-true (parse-err? result))))

  (define-test "failure expected"
    (let ([result (failure "expected char" 5)])
      (assert-equal "expected char" (result-expected result))))

  (define-test "failure position"
    (let ([result (failure "expected char" 5)])
      (assert-equal 5 (result-position result)))))

(test-group primitive-parsers
  (define-test "pure succeeds"
    (let ([result (run-parser (pure 42) "hello")])
      (assert-true (parse-ok? result))))

  (define-test "pure returns value"
    (let ([result (run-parser (pure 42) "hello")])
      (assert-equal 42 (result-value result))))

  (define-test "pure consumes nothing"
    (let ([result (run-parser (pure 42) "hello")])
      (assert-equal "hello" (result-remaining result))))

  (define-test "fail fails"
    (let ([result (run-parser (fail "error message") "hello")])
      (assert-true (parse-err? result))))

  (define-test "fail message"
    (let ([result (run-parser (fail "error message") "hello")])
      (assert-equal "error message" (result-expected result))))

  (define-test "item succeeds on non-empty"
    (let ([result (run-parser item "abc")])
      (assert-true (parse-ok? result))))

  (define-test "item returns first char"
    (let ([result (run-parser item "abc")])
      (assert-equal #\a (result-value result))))

  (define-test "item consumes one char"
    (let ([result (run-parser item "abc")])
      (assert-equal "bc" (result-remaining result))))

  (define-test "item fails on empty"
    (let ([result (run-parser item "")])
      (assert-true (parse-err? result))))

  (define-test "eof succeeds on empty"
    (let ([result (run-parser eof "")])
      (assert-true (parse-ok? result))))

  (define-test "eof fails on non-empty"
    (let ([result (run-parser eof "abc")])
      (assert-true (parse-err? result))))

  (define-test "satisfy succeeds on match"
    (let ([result (run-parser (satisfy char-numeric? "digit") "5abc")])
      (assert-true (parse-ok? result))))

  (define-test "satisfy returns matching char"
    (let ([result (run-parser (satisfy char-numeric? "digit") "5abc")])
      (assert-equal #\5 (result-value result))))

  (define-test "satisfy fails on no match"
    (let ([result (run-parser (satisfy char-numeric? "digit") "abc")])
      (assert-true (parse-err? result)))))

(test-group character-parsers
  (define-test "char succeeds on match"
    (let ([result (run-parser (char #\a) "abc")])
      (assert-true (parse-ok? result))))

  (define-test "char returns matched char"
    (let ([result (run-parser (char #\a) "abc")])
      (assert-equal #\a (result-value result))))

  (define-test "char consumes one char"
    (let ([result (run-parser (char #\a) "abc")])
      (assert-equal "bc" (result-remaining result))))

  (define-test "char fails on mismatch"
    (let ([result (run-parser (char #\a) "xyz")])
      (assert-true (parse-err? result))))

  (define-test "digit matches digit"
    (let ([result (run-parser digit "3abc")])
      (assert-true (parse-ok? result))))

  (define-test "digit value"
    (let ([result (run-parser digit "3abc")])
      (assert-equal #\3 (result-value result))))

  (define-test "digit fails on non-digit"
    (let ([result (run-parser digit "abc")])
      (assert-true (parse-err? result))))

  (define-test "alpha matches letter"
    (let ([result (run-parser alpha "abc")])
      (assert-true (parse-ok? result))))

  (define-test "alpha value"
    (let ([result (run-parser alpha "abc")])
      (assert-equal #\a (result-value result))))

  (define-test "space matches whitespace"
    (let ([result (run-parser space " abc")])
      (assert-true (parse-ok? result))))

  (define-test "space value"
    (let ([result (run-parser space " abc")])
      (assert-equal #\space (result-value result))))

  (define-test "string-p matches exact string"
    (let ([result (run-parser (string-p "hello") "hello world")])
      (assert-true (parse-ok? result))))

  (define-test "string-p value"
    (let ([result (run-parser (string-p "hello") "hello world")])
      (assert-equal "hello" (result-value result))))

  (define-test "string-p remaining"
    (let ([result (run-parser (string-p "hello") "hello world")])
      (assert-equal " world" (result-remaining result))))

  (define-test "string-p fails on mismatch"
    (let ([result (run-parser (string-p "hello") "goodbye")])
      (assert-true (parse-err? result))))

  (define-test "string-p fails on too-short input"
    (let ([result (run-parser (string-p "hello") "hel")])
      (assert-true (parse-err? result)))))

(test-group sequencing-combinators
  (define-test "bind succeeds"
    (let ([parser (bind (char #\a) (lambda (c) (pure (char-upcase c))))])
      (let ([result (run-parser parser "abc")])
        (assert-true (parse-ok? result)))))

  (define-test "bind threads value"
    (let ([parser (bind (char #\a) (lambda (c) (pure (char-upcase c))))])
      (let ([result (run-parser parser "abc")])
        (assert-equal #\A (result-value result)))))

  (define-test "bind propagates failure from second parser"
    (let ([parser (bind (char #\a) (lambda (c) (fail "error")))])
      (let ([result (run-parser parser "abc")])
        (assert-true (parse-err? result)))))

  (define-test "seq succeeds"
    (let ([parser (seq (char #\a) (char #\b))])
      (let ([result (run-parser parser "abc")])
        (assert-true (parse-ok? result)))))

  (define-test "seq keeps second value"
    (let ([parser (seq (char #\a) (char #\b))])
      (let ([result (run-parser parser "abc")])
        (assert-equal #\b (result-value result)))))

  (define-test "seq-left succeeds"
    (let ([parser (seq-left (char #\a) (char #\b))])
      (let ([result (run-parser parser "abc")])
        (assert-true (parse-ok? result)))))

  (define-test "seq-left keeps first value"
    (let ([parser (seq-left (char #\a) (char #\b))])
      (let ([result (run-parser parser "abc")])
        (assert-equal #\a (result-value result))))))

(test-group choice-combinators
  (define-test "alt succeeds on first parser"
    (let ([parser (alt (char #\a) (char #\b))])
      (let ([result (run-parser parser "abc")])
        (assert-true (parse-ok? result)))))

  (define-test "alt value from first"
    (let ([parser (alt (char #\a) (char #\b))])
      (let ([result (run-parser parser "abc")])
        (assert-equal #\a (result-value result)))))

  (define-test "alt succeeds on second parser"
    (let ([parser (alt (char #\a) (char #\b))])
      (let ([result (run-parser parser "bac")])
        (assert-true (parse-ok? result)))))

  (define-test "alt value from second"
    (let ([parser (alt (char #\a) (char #\b))])
      (let ([result (run-parser parser "bac")])
        (assert-equal #\b (result-value result)))))

  (define-test "alt fails when both fail"
    (let ([parser (alt (char #\a) (char #\b))])
      (let ([result (run-parser parser "xyz")])
        (assert-true (parse-err? result)))))

  (define-test "choice succeeds on matching alternative"
    (let ([parser (choice (list (char #\a) (char #\b) (char #\c)))])
      (let ([result (run-parser parser "bac")])
        (assert-true (parse-ok? result)))))

  (define-test "choice value"
    (let ([parser (choice (list (char #\a) (char #\b) (char #\c)))])
      (let ([result (run-parser parser "bac")])
        (assert-equal #\b (result-value result))))))

(test-group transformation-combinators
  (define-test "map-p succeeds"
    (let ([parser (map-p char-upcase (char #\a))])
      (let ([result (run-parser parser "abc")])
        (assert-true (parse-ok? result)))))

  (define-test "map-p transforms value"
    (let ([parser (map-p char-upcase (char #\a))])
      (let ([result (run-parser parser "abc")])
        (assert-equal #\A (result-value result)))))

  (define-test "map-p with list->string"
    (let ([parser (map-p list->string (many alpha))])
      (let ([result (run-parser parser "hello123")])
        (assert-true (parse-ok? result)))))

  (define-test "map-p result"
    (let ([parser (map-p list->string (many alpha))])
      (let ([result (run-parser parser "hello123")])
        (assert-equal "hello" (result-value result))))))

(test-group repetition-combinators
  (define-test "many succeeds with zero"
    (let ([parser (many (char #\a))])
      (let ([result (run-parser parser "bbb")])
        (assert-true (parse-ok? result)))))

  (define-test "many zero value"
    (let ([parser (many (char #\a))])
      (let ([result (run-parser parser "bbb")])
        (assert-equal '() (result-value result)))))

  (define-test "many succeeds with multiple"
    (let ([parser (many (char #\a))])
      (let ([result (run-parser parser "aaabbb")])
        (assert-true (parse-ok? result)))))

  (define-test "many multiple values"
    (let ([parser (many (char #\a))])
      (let ([result (run-parser parser "aaabbb")])
        (assert-equal '(#\a #\a #\a) (result-value result)))))

  (define-test "many remaining"
    (let ([parser (many (char #\a))])
      (let ([result (run-parser parser "aaabbb")])
        (assert-equal "bbb" (result-remaining result)))))

  (define-test "many1 fails with zero matches"
    (let ([parser (many1 (char #\a))])
      (let ([result (run-parser parser "bbb")])
        (assert-true (parse-err? result)))))

  (define-test "many1 succeeds with multiple"
    (let ([parser (many1 (char #\a))])
      (let ([result (run-parser parser "aaabbb")])
        (assert-true (parse-ok? result)))))

  (define-test "many1 values"
    (let ([parser (many1 (char #\a))])
      (let ([result (run-parser parser "aaabbb")])
        (assert-equal '(#\a #\a #\a) (result-value result)))))

  (define-test "sep-by succeeds with zero"
    (let ([parser (sep-by digit (char #\,))])
      (let ([result (run-parser parser "abc")])
        (assert-true (parse-ok? result)))))

  (define-test "sep-by zero value"
    (let ([parser (sep-by digit (char #\,))])
      (let ([result (run-parser parser "abc")])
        (assert-equal '() (result-value result)))))

  (define-test "sep-by succeeds with multiple"
    (let ([parser (sep-by digit (char #\,))])
      (let ([result (run-parser parser "1,2,3abc")])
        (assert-true (parse-ok? result)))))

  (define-test "sep-by values"
    (let ([parser (sep-by digit (char #\,))])
      (let ([result (run-parser parser "1,2,3abc")])
        (assert-equal '(#\1 #\2 #\3) (result-value result)))))

  (define-test "sep-by remaining"
    (let ([parser (sep-by digit (char #\,))])
      (let ([result (run-parser parser "1,2,3abc")])
        (assert-equal "abc" (result-remaining result)))))

  (define-test "sep-by succeeds with one"
    (let ([parser (sep-by digit (char #\,))])
      (let ([result (run-parser parser "5abc")])
        (assert-true (parse-ok? result)))))

  (define-test "sep-by one value"
    (let ([parser (sep-by digit (char #\,))])
      (let ([result (run-parser parser "5abc")])
        (assert-equal '(#\5) (result-value result)))))

  (define-test "optional succeeds with match"
    (let ([parser (optional (char #\a))])
      (let ([result (run-parser parser "abc")])
        (assert-true (parse-ok? result)))))

  (define-test "optional value with match"
    (let ([parser (optional (char #\a))])
      (let ([result (run-parser parser "abc")])
        (assert-equal '(#\a) (result-value result)))))

  (define-test "optional succeeds without match"
    (let ([parser (optional (char #\a))])
      (let ([result (run-parser parser "bbb")])
        (assert-true (parse-ok? result)))))

  (define-test "optional value without match"
    (let ([parser (optional (char #\a))])
      (let ([result (run-parser parser "bbb")])
        (assert-equal '() (result-value result))))))

(test-group delimiter-combinators
  (define-test "between succeeds"
    (let ([parser (between (char #\() (char #\)) alpha)])
      (let ([result (run-parser parser "(a)bc")])
        (assert-true (parse-ok? result)))))

  (define-test "between value"
    (let ([parser (between (char #\() (char #\)) alpha)])
      (let ([result (run-parser parser "(a)bc")])
        (assert-equal #\a (result-value result)))))

  (define-test "between remaining"
    (let ([parser (between (char #\() (char #\)) alpha)])
      (let ([result (run-parser parser "(a)bc")])
        (assert-equal "bc" (result-remaining result)))))

  (define-test "between fails without open delimiter"
    (let ([parser (between (char #\() (char #\)) alpha)])
      (let ([result (run-parser parser "a)bc")])
        (assert-true (parse-err? result)))))

  (define-test "between fails without close delimiter"
    (let ([parser (between (char #\() (char #\)) alpha)])
      (let ([result (run-parser parser "(abc")])
        (assert-true (parse-err? result))))))

(test-group number-parsers
  (define-test "nat succeeds"
    (let ([result (run-parser nat "123abc")])
      (assert-true (parse-ok? result))))

  (define-test "nat value"
    (let ([result (run-parser nat "123abc")])
      (assert-equal 123 (result-value result))))

  (define-test "nat remaining"
    (let ([result (run-parser nat "123abc")])
      (assert-equal "abc" (result-remaining result))))

  (define-test "nat fails on non-digit"
    (let ([result (run-parser nat "abc")])
      (assert-true (parse-err? result))))

  (define-test "int parses positive"
    (let ([result (run-parser int "123abc")])
      (assert-true (parse-ok? result))))

  (define-test "int positive value"
    (let ([result (run-parser int "123abc")])
      (assert-equal 123 (result-value result))))

  (define-test "int parses negative"
    (let ([result (run-parser int "-456abc")])
      (assert-true (parse-ok? result))))

  (define-test "int negative value"
    (let ([result (run-parser int "-456abc")])
      (assert-equal -456 (result-value result)))))

(test-group word-parsers
  (define-test "word succeeds"
    (let ([result (run-parser word "hello123")])
      (assert-true (parse-ok? result))))

  (define-test "word value"
    (let ([result (run-parser word "hello123")])
      (assert-equal "hello" (result-value result))))

  (define-test "word remaining"
    (let ([result (run-parser word "hello123")])
      (assert-equal "123" (result-remaining result))))

  (define-test "identifier succeeds"
    (let ([result (run-parser identifier "hello_world123 ")])
      (assert-true (parse-ok? result))))

  (define-test "identifier value"
    (let ([result (run-parser identifier "hello_world123 ")])
      (assert-equal "hello_world123" (result-value result))))

  (define-test "identifier fails on digit start"
    (let ([result (run-parser identifier "123abc")])
      (assert-true (parse-err? result)))))

(test-group utility-parsers
  (define-test "spaces succeeds"
    (let ([result (run-parser spaces "   abc")])
      (assert-true (parse-ok? result))))

  (define-test "spaces count"
    (let ([result (run-parser spaces "   abc")])
      (assert-equal 3 (length (result-value result)))))

  (define-test "spaces remaining"
    (let ([result (run-parser spaces "   abc")])
      (assert-equal "abc" (result-remaining result))))

  (define-test "spaces succeeds on no whitespace"
    (let ([result (run-parser spaces "abc")])
      (assert-true (parse-ok? result))))

  (define-test "spaces zero value"
    (let ([result (run-parser spaces "abc")])
      (assert-equal '() (result-value result))))

  (define-test "lexeme succeeds"
    (let ([parser (lexeme (string-p "hello"))])
      (let ([result (run-parser parser "hello   world")])
        (assert-true (parse-ok? result)))))

  (define-test "lexeme value"
    (let ([parser (lexeme (string-p "hello"))])
      (let ([result (run-parser parser "hello   world")])
        (assert-equal "hello" (result-value result)))))

  (define-test "lexeme skips trailing space"
    (let ([parser (lexeme (string-p "hello"))])
      (let ([result (run-parser parser "hello   world")])
        (assert-equal "world" (result-remaining result)))))

  (define-test "symbol succeeds"
    (let ([result (run-parser (symbol "hello") "hello   world")])
      (assert-true (parse-ok? result))))

  (define-test "symbol remaining"
    (let ([result (run-parser (symbol "hello") "hello   world")])
      (assert-equal "world" (result-remaining result))))

  (define-test "one-of succeeds"
    (let ([parser (one-of "abc")])
      (let ([result (run-parser parser "bxyz")])
        (assert-true (parse-ok? result)))))

  (define-test "one-of value"
    (let ([parser (one-of "abc")])
      (let ([result (run-parser parser "bxyz")])
        (assert-equal #\b (result-value result)))))

  (define-test "one-of fails on no match"
    (let ([parser (one-of "abc")])
      (let ([result (run-parser parser "xyz")])
        (assert-true (parse-err? result)))))

  (define-test "count succeeds"
    (let ([parser (count 3 digit)])
      (let ([result (run-parser parser "123abc")])
        (assert-true (parse-ok? result)))))

  (define-test "count values"
    (let ([parser (count 3 digit)])
      (let ([result (run-parser parser "123abc")])
        (assert-equal '(#\1 #\2 #\3) (result-value result)))))

  (define-test "count remaining"
    (let ([parser (count 3 digit)])
      (let ([result (run-parser parser "123abc")])
        (assert-equal "abc" (result-remaining result)))))

  (define-test "count fails on too few"
    (let ([parser (count 5 digit)])
      (let ([result (run-parser parser "123abc")])
        (assert-true (parse-err? result))))))

(test-group arithmetic-example
  ;; Simple expression parser: parses "num op num" like "3 + 5"
  (define addop
    (alt (map-p (lambda (_) +) (char #\+))
         (map-p (lambda (_) -) (char #\-))))

  (define simple-expr
    (bind nat (lambda (left)
      (bind spaces (lambda (_)
        (bind addop (lambda (op)
          (bind spaces (lambda (_)
            (bind nat (lambda (right)
              (pure (op left right)))))))))))))

  (define-test "arithmetic parser succeeds on addition"
    (let ([result (run-parser simple-expr "3 + 5")])
      (assert-true (parse-ok? result))))

  (define-test "addition result"
    (let ([result (run-parser simple-expr "3 + 5")])
      (assert-equal 8 (result-value result))))

  (define-test "arithmetic parser succeeds on subtraction"
    (let ([result (run-parser simple-expr "10 - 3")])
      (assert-true (parse-ok? result))))

  (define-test "subtraction result"
    (let ([result (run-parser simple-expr "10 - 3")])
      (assert-equal 7 (result-value result)))))

(test-group csv-example
  ;; CSV field: sequence of non-comma characters
  (define csv-field
    (map-p list->string (many (none-of ",\n"))))

  ;; CSV line: comma-separated fields
  (define csv-line
    (sep-by csv-field (char #\,)))

  (define-test "csv single field"
    (let ([result (run-parser csv-line "hello")])
      (assert-true (parse-ok? result))))

  (define-test "csv single value"
    (let ([result (run-parser csv-line "hello")])
      (assert-equal '("hello") (result-value result))))

  (define-test "csv multiple fields"
    (let ([result (run-parser csv-line "one,two,three")])
      (assert-true (parse-ok? result))))

  (define-test "csv values"
    (let ([result (run-parser csv-line "one,two,three")])
      (assert-equal '("one" "two" "three") (result-value result))))

  (define-test "csv with empty fields"
    (let ([result (run-parser csv-line "a,,c")])
      (assert-true (parse-ok? result))))

  (define-test "csv with empty"
    (let ([result (run-parser csv-line "a,,c")])
      (assert-equal '("a" "" "c") (result-value result)))))

(test-group sexpr-example
  ;; Atom: identifier or number
  (define atom
    (alt (map-p (lambda (s) (string->symbol s)) identifier)
         nat))

  ;; Simple S-expr: atom or (atom atom ...)
  (define simple-sexpr
    (alt atom
         (between (char #\()
                  (char #\))
                  (sep-by atom space))))

  (define-test "sexpr atom"
    (let ([result (run-parser simple-sexpr "hello")])
      (assert-true (parse-ok? result))))

  (define-test "sexpr atom value"
    (let ([result (run-parser simple-sexpr "hello")])
      (assert-equal 'hello (result-value result))))

  (define-test "sexpr number"
    (let ([result (run-parser simple-sexpr "42")])
      (assert-true (parse-ok? result))))

  (define-test "sexpr number value"
    (let ([result (run-parser simple-sexpr "42")])
      (assert-equal 42 (result-value result))))

  (define-test "sexpr list"
    (let ([result (run-parser simple-sexpr "(add 1 2)")])
      (assert-true (parse-ok? result))))

  (define-test "sexpr list value"
    (let ([result (run-parser simple-sexpr "(add 1 2)")])
      (assert-equal '(add 1 2) (result-value result)))))

;;; ============================================================================
;;; Run all tests
;;; ============================================================================

(run-all-tests-and-exit)
