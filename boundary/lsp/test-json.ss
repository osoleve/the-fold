(load "core/testing/test-framework.ss")
(load "boundary/lsp/json.ss")

(doc 'module 'lsp/test-json)
(doc 'description "Tests for JSON Parser")
(doc 'layer 'boundary)
(doc 'purity 'partial)

(display "Testing json.ss\n")
(display "====\n\n")

;;; ====
;;; Primitives Tests
;;; ====

(test-group primitives
  (define-test parse-null
    (assert-equal '(ok null) (json-read "null")))

  (define-test parse-true
    (assert-equal '(ok #t) (json-read "true")))

  (define-test parse-false
    (assert-equal '(ok #f) (json-read "false")))

  (define-test parse-integer
    (assert-equal '(ok 42) (json-read "42")))

  (define-test parse-negative
    (assert-equal '(ok -123) (json-read "-123")))

  (define-test parse-float
    (assert-equal 3.14 (cadr (json-read "3.14"))))

  (define-test parse-exponent
    (assert-equal 1e10 (cadr (json-read "1e10")))))

;;; ====
;;; Strings Tests
;;; ====

(test-group strings
  (define-test parse-simple-string
    (assert-equal '(ok "hello") (json-read "\"hello\"")))

  (define-test parse-empty-string
    (assert-equal '(ok "") (json-read "\"\"")))

  (define-test parse-escape-sequences
    (assert-equal `(ok ,(string-append "a" (string #\newline) "b" (string #\tab) "c"))
                  (json-read "\"a\\nb\\tc\"")))

  (define-test parse-unicode-escape
    (assert-equal '(ok "A") (json-read "\"\\u0041\"")))

  (define-test parse-escaped-quotes
    (assert-equal '(ok "say \"hello\"") (json-read "\"say \\\"hello\\\"\""))))

;;; ====
;;; Surrogate Pairs Tests
;;; ====

(test-group surrogate-pairs
  ;; U+1F600 (😀) = \uD83D\uDE00
  (define-test parse-surrogate-pair-emoji
    (assert-equal `(ok ,(string (integer->char #x1F600)))
                  (json-read "\"\\uD83D\\uDE00\"")))

  ;; U+1D11E (𝄞 - musical G clef) = \uD834\uDD1E
  (define-test parse-surrogate-pair-music-symbol
    (assert-equal `(ok ,(string (integer->char #x1D11E)))
                  (json-read "\"\\uD834\\uDD1E\"")))

  ;; Mixed: BMP + surrogate pair + BMP
  (define-test parse-mixed-bmp-and-surrogate
    (assert-equal `(ok ,(string-append "a" (string (integer->char #x1F600)) "b"))
                  (json-read "\"a\\uD83D\\uDE00b\"")))

  ;; Lone high surrogate (invalid - replaced with U+FFFD)
  (define-test parse-lone-high-surrogate
    (assert-equal `(ok ,(string (integer->char #xFFFD)))
                  (json-read "\"\\uD83D\"")))

  ;; High surrogate followed by non-surrogate escape
  (define-test parse-high-surrogate-plus-regular-escape
    (assert-equal `(ok ,(string-append (string (integer->char #xFFFD)) "A"))
                  (json-read "\"\\uD83D\\u0041\"")))

  ;; Lone low surrogate (invalid - replaced with U+FFFD)
  (define-test parse-lone-low-surrogate
    (assert-equal `(ok ,(string (integer->char #xFFFD)))
                  (json-read "\"\\uDC00\""))))

;;; ====
;;; Arrays Tests
;;; ====

(test-group arrays
  (define-test parse-empty-array
    (assert-equal '(ok (json-array)) (json-read "[]")))

  (define-test parse-array-with-values
    (assert-equal '(ok (json-array 1 2 3)) (json-read "[1, 2, 3]")))

  (define-test parse-nested-array
    (assert-equal '(ok (json-array (json-array 1 2) (json-array 3 4)))
                  (json-read "[[1, 2], [3, 4]]")))

  (define-test parse-mixed-array
    (assert-equal '(ok (json-array 1 "two" #t null))
                  (json-read "[1, \"two\", true, null]"))))

;;; ====
;;; Objects Tests
;;; ====

(test-group objects
  (define-test parse-empty-object
    (assert-equal '(ok (json-object)) (json-read "{}")))

  (define-test parse-simple-object
    (assert-equal '(ok (json-object ("a" . 1))) (json-read "{\"a\": 1}")))

  (define-test parse-multi-key-object
    (assert-equal '(ok (json-object ("a" . 1) ("b" . 2)))
                  (json-read "{\"a\": 1, \"b\": 2}")))

  (define-test parse-nested-object
    (assert-equal '(ok (json-object ("outer" . (json-object ("inner" . 42)))))
                  (json-read "{\"outer\": {\"inner\": 42}}"))))

;;; ====
;;; Whitespace Tests
;;; ====

(test-group whitespace
  (define-test leading-whitespace
    (assert-equal '(ok 42) (json-read "  42")))

  (define-test trailing-whitespace
    (assert-equal '(ok 42) (json-read "42  ")))

  (define-test whitespace-in-object
    (assert-equal '(ok (json-object ("a" . 1))) (json-read "{ \"a\" : 1 }"))))

;;; ====
;;; Serializer Tests
;;; ====

(test-group serializer
  (define-test write-null
    (assert-equal "null" (json-write 'null)))

  (define-test write-true
    (assert-equal "true" (json-write #t)))

  (define-test write-false
    (assert-equal "false" (json-write #f)))

  (define-test write-integer
    (assert-equal "42" (json-write 42)))

  (define-test write-string
    (assert-equal "\"hello\"" (json-write "hello")))

  (define-test write-string-with-escapes
    (assert-equal "\"a\\nb\"" (json-write "a\nb")))

  (define-test write-array
    (assert-equal "[1,2,3]" (json-write '(json-array 1 2 3))))

  (define-test write-object
    (assert-equal "{\"a\":1}" (json-write '(json-object ("a" . 1))))))

;;; ====
;;; Round-trip Tests
;;; ====

(test-group round-trip
  (define-test round-trip-object
    (assert-equal '(json-object ("name" . "test") ("value" . 42))
                  (let* ([original '(json-object ("name" . "test") ("value" . 42))]
                         [serialized (json-write original)]
                         [parsed (json-read serialized)])
                    (cadr parsed))))

  (define-test round-trip-nested
    (assert-equal '(json-object ("data" . (json-array 1 2 3)) ("meta" . (json-object ("id" . "abc"))))
                  (let* ([original '(json-object ("data" . (json-array 1 2 3))
                                      ("meta" . (json-object ("id" . "abc"))))]
                         [serialized (json-write original)]
                         [parsed (json-read serialized)])
                    (cadr parsed)))))

;;; ====
;;; Object Access Tests
;;; ====

(test-group object-access
  (define-test json-get-existing-key
    (assert-equal 1 (json-get '(json-object ("a" . 1) ("b" . 2)) "a")))

  (define-test json-get-missing-key
    (assert-equal #f (json-get '(json-object ("a" . 1)) "x")))

  (define-test json-get-path
    (assert-equal 42
                  (json-get-path '(json-object ("outer" . (json-object ("inner" . 42))))
                                 '("outer" "inner")))))

;;; ====
;;; Errors Tests
;;; ====

(test-group errors
  (define-test trailing-garbage
    (assert-equal 'error (car (json-read "42 extra"))))

  (define-test unterminated-string
    (assert-equal 'error (car (json-read "\"hello"))))

  (define-test unterminated-array
    (assert-equal 'error (car (json-read "[1, 2")))))

;;; ====
;;; Run Tests
;;; ====

(print-summary)
(when (> *tests-failed* 0)
  (exit 1))
