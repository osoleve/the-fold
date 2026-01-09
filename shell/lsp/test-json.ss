;;; core/lsp/test-json.ss — Tests for JSON Parser

(load "shell/lsp/json.ss")

(define tests-passed 0)
(define tests-failed 0)

(define (test name expected actual)
  (if (equal? expected actual)
      (begin
       (set! tests-passed (+ tests-passed 1))
       (display "  ✓ ") (display name) (newline))
      (begin
       (set! tests-failed (+ tests-failed 1))
       (display "  ✗ ") (display name)
       (display " — expected ") (write expected)
       (display ", got ") (write actual)
       (newline))))

(display "Testing json.ss\n")
(display "================\n\n")

;;; Primitives
(display "Primitives:\n")
(test "parse null" '(ok null) (json-read "null"))
(test "parse true" '(ok #t) (json-read "true"))
(test "parse false" '(ok #f) (json-read "false"))
(test "parse integer" '(ok 42) (json-read "42"))
(test "parse negative" '(ok -123) (json-read "-123"))
(test "parse float" 3.14 (cadr (json-read "3.14")))
(test "parse exponent" 1e10 (cadr (json-read "1e10")))

;;; Strings
(display "\nStrings:\n")
(test "parse simple string" '(ok "hello") (json-read "\"hello\""))
(test "parse empty string" '(ok "") (json-read "\"\""))
(test "parse escape sequences"
      `(ok ,(string-append "a" (string #\newline) "b" (string #\tab) "c"))
      (json-read "\"a\\nb\\tc\""))
(test "parse unicode escape" '(ok "A") (json-read "\"\\u0041\""))
(test "parse escaped quotes" '(ok "say \"hello\"") (json-read "\"say \\\"hello\\\"\""))

;;; Surrogate pairs (non-BMP characters)
(display "\nSurrogate Pairs:\n")
;; U+1F600 (😀) = \uD83D\uDE00
(test "parse surrogate pair emoji"
      `(ok ,(string (integer->char #x1F600)))
      (json-read "\"\\uD83D\\uDE00\""))
;; U+1D11E (𝄞 - musical G clef) = \uD834\uDD1E
(test "parse surrogate pair music symbol"
      `(ok ,(string (integer->char #x1D11E)))
      (json-read "\"\\uD834\\uDD1E\""))
;; Mixed: BMP + surrogate pair + BMP
(test "parse mixed BMP and surrogate"
      `(ok ,(string-append "a" (string (integer->char #x1F600)) "b"))
      (json-read "\"a\\uD83D\\uDE00b\""))
;; Lone high surrogate (invalid - replaced with U+FFFD)
(test "parse lone high surrogate"
      `(ok ,(string (integer->char #xFFFD)))  ; Replacement char
      (json-read "\"\\uD83D\""))
;; High surrogate followed by non-surrogate escape (not a valid pair)
(test "parse high surrogate + regular escape"
      `(ok ,(string-append (string (integer->char #xFFFD)) "A"))
      (json-read "\"\\uD83D\\u0041\""))
;; Lone low surrogate (invalid - replaced with U+FFFD)
(test "parse lone low surrogate"
      `(ok ,(string (integer->char #xFFFD)))
      (json-read "\"\\uDC00\""))

;;; Arrays
(display "\nArrays:\n")
(test "parse empty array" '(ok (json-array)) (json-read "[]"))
(test "parse array with values" '(ok (json-array 1 2 3)) (json-read "[1, 2, 3]"))
(test "parse nested array" '(ok (json-array (json-array 1 2) (json-array 3 4)))
      (json-read "[[1, 2], [3, 4]]"))
(test "parse mixed array" '(ok (json-array 1 "two" #t null))
      (json-read "[1, \"two\", true, null]"))

;;; Objects
(display "\nObjects:\n")
(test "parse empty object" '(ok (json-object)) (json-read "{}"))
(test "parse simple object" '(ok (json-object ("a" . 1))) (json-read "{\"a\": 1}"))
(test "parse multi-key object" '(ok (json-object ("a" . 1) ("b" . 2)))
      (json-read "{\"a\": 1, \"b\": 2}"))
(test "parse nested object" '(ok (json-object ("outer" . (json-object ("inner" . 42)))))
      (json-read "{\"outer\": {\"inner\": 42}}"))

;;; Whitespace
(display "\nWhitespace:\n")
(test "leading whitespace" '(ok 42) (json-read "  42"))
(test "trailing whitespace" '(ok 42) (json-read "42  "))
(test "whitespace in object" '(ok (json-object ("a" . 1))) (json-read "{ \"a\" : 1 }"))

;;; Serializer
(display "\nSerializer:\n")
(test "write null" "null" (json-write 'null))
(test "write true" "true" (json-write #t))
(test "write false" "false" (json-write #f))
(test "write integer" "42" (json-write 42))
(test "write string" "\"hello\"" (json-write "hello"))
(test "write string with escapes" "\"a\\nb\"" (json-write "a\nb"))
(test "write array" "[1,2,3]" (json-write '(json-array 1 2 3)))
(test "write object" "{\"a\":1}" (json-write '(json-object ("a" . 1))))

;;; Round-trip
(display "\nRound-trip:\n")
(test "round-trip object"
      '(json-object ("name" . "test") ("value" . 42))
      (let* ([original '(json-object ("name" . "test") ("value" . 42))]
             [serialized (json-write original)]
             [parsed (json-read serialized)])
            (cadr parsed)))

(test "round-trip nested"
      '(json-object ("data" . (json-array 1 2 3)) ("meta" . (json-object ("id" . "abc"))))
      (let* ([original '(json-object ("data" . (json-array 1 2 3))
                         ("meta" . (json-object ("id" . "abc"))))]
             [serialized (json-write original)]
             [parsed (json-read serialized)])
            (cadr parsed)))

;;; Object Access
(display "\nObject Access:\n")
(test "json-get existing key" 1 (json-get '(json-object ("a" . 1) ("b" . 2)) "a"))
(test "json-get missing key" #f (json-get '(json-object ("a" . 1)) "x"))
(test "json-get-path" 42
      (json-get-path '(json-object ("outer" . (json-object ("inner" . 42))))
                     '("outer" "inner")))

;;; Errors
(display "\nErrors:\n")
(test "trailing garbage" 'error (car (json-read "42 extra")))
(test "unterminated string" 'error (car (json-read "\"hello")))
(test "unterminated array" 'error (car (json-read "[1, 2")))

;;; Summary
(display "\n================\n")
(printf "Passed: ~a, Failed: ~a\n" tests-passed tests-failed)
(when (> tests-failed 0)
      (exit 1))
