;;; fabric/stitches/fp/test-regex.ss — Tests for Regular Expressions

;;; NOTE: Run from fabric/stitches directory

(load "test-framework.ss")
(load "fp/regex.ss")

(display "\n")
(display "==============================================================\n")
(display "         REGULAR EXPRESSION TESTS\n")
(display "==============================================================\n")

;;; ============================================================
;;; Basic Regex Tests
;;; ============================================================

(test-group basic-regex
  (define-test rx-empty-test
    (assert-true (rx-empty? rx-empty))
    (assert-false (rx-match? rx-empty "")))

  (define-test rx-epsilon-test
    (assert-true (rx-epsilon? rx-epsilon))
    (assert-true (rx-match? rx-epsilon ""))
    (assert-false (rx-match? rx-epsilon "a")))

  (define-test rx-char-test
    (let ([r (rx-char #\a)])
      (assert-true (rx-match? r "a"))
      (assert-false (rx-match? r "b"))
      (assert-false (rx-match? r ""))
      (assert-false (rx-match? r "aa"))))

  (define-test rx-char-class-test
    (let ([r (rx-char-class char-numeric? "digit")])
      (assert-true (rx-match? r "5"))
      (assert-false (rx-match? r "x"))
      (assert-false (rx-match? r ""))))

  (define-test rx-seq-test
    (let ([r (rx-seq (rx-char #\a) (rx-char #\b))])
      (assert-true (rx-match? r "ab"))
      (assert-false (rx-match? r "a"))
      (assert-false (rx-match? r "ba"))
      (assert-false (rx-match? r "abc"))))

  (define-test rx-alt-test
    (let ([r (rx-alt (rx-char #\a) (rx-char #\b))])
      (assert-true (rx-match? r "a"))
      (assert-true (rx-match? r "b"))
      (assert-false (rx-match? r "c"))
      (assert-false (rx-match? r "ab"))))

  (define-test rx-star-test
    (let ([r (rx-star (rx-char #\a))])
      (assert-true (rx-match? r ""))
      (assert-true (rx-match? r "a"))
      (assert-true (rx-match? r "aaa"))
      (assert-false (rx-match? r "b"))
      (assert-false (rx-match? r "aab")))))

;;; ============================================================
;;; Extended Constructor Tests
;;; ============================================================

(test-group extended-constructors
  (define-test rx-plus-test
    (let ([r (rx-plus (rx-char #\a))])
      (assert-false (rx-match? r ""))
      (assert-true (rx-match? r "a"))
      (assert-true (rx-match? r "aaaa"))))

  (define-test rx-opt-test
    (let ([r (rx-opt (rx-char #\a))])
      (assert-true (rx-match? r ""))
      (assert-true (rx-match? r "a"))
      (assert-false (rx-match? r "aa"))))

  (define-test rx-count-test
    (let ([r (rx-count 3 (rx-char #\a))])
      (assert-false (rx-match? r "aa"))
      (assert-true (rx-match? r "aaa"))
      (assert-false (rx-match? r "aaaa"))))

  (define-test rx-range-test
    (let ([r (rx-range 2 4 (rx-char #\a))])
      (assert-false (rx-match? r "a"))
      (assert-true (rx-match? r "aa"))
      (assert-true (rx-match? r "aaa"))
      (assert-true (rx-match? r "aaaa"))
      (assert-false (rx-match? r "aaaaa"))))

  (define-test rx-string-test
    (let ([r (rx-string "hello")])
      (assert-true (rx-match? r "hello"))
      (assert-false (rx-match? r "hell"))
      (assert-false (rx-match? r "hello!"))))

  (define-test rx-one-of-test
    (let ([r (rx-one-of "aeiou")])
      (assert-true (rx-match? r "a"))
      (assert-true (rx-match? r "e"))
      (assert-false (rx-match? r "b"))
      (assert-false (rx-match? r "ae"))))

  (define-test rx-none-of-test
    (let ([r (rx-none-of "aeiou")])
      (assert-false (rx-match? r "a"))
      (assert-true (rx-match? r "b"))
      (assert-true (rx-match? r "z"))))

  (define-test rx-char-range-test
    (let ([r (rx-char-range #\a #\z)])
      (assert-true (rx-match? r "a"))
      (assert-true (rx-match? r "m"))
      (assert-true (rx-match? r "z"))
      (assert-false (rx-match? r "A"))
      (assert-false (rx-match? r "0")))))

;;; ============================================================
;;; Character Class Tests
;;; ============================================================

(test-group character-classes
  (define-test rx-dot-test
    (assert-true (rx-match? rx-dot "a"))
    (assert-true (rx-match? rx-dot "9"))
    (assert-true (rx-match? rx-dot " "))
    (assert-false (rx-match? rx-dot "")))

  (define-test rx-digit-test
    (assert-true (rx-match? rx-digit "5"))
    (assert-false (rx-match? rx-digit "x")))

  (define-test rx-alpha-test
    (assert-true (rx-match? rx-alpha "a"))
    (assert-true (rx-match? rx-alpha "Z"))
    (assert-false (rx-match? rx-alpha "5")))

  (define-test rx-word-test
    (assert-true (rx-match? rx-word "a"))
    (assert-true (rx-match? rx-word "5"))
    (assert-true (rx-match? rx-word "_"))
    (assert-false (rx-match? rx-word " ")))

  (define-test rx-space-test
    (assert-true (rx-match? rx-space " "))
    (assert-true (rx-match? rx-space "\t"))
    (assert-false (rx-match? rx-space "a"))))

;;; ============================================================
;;; Nullability Tests
;;; ============================================================

(test-group nullability
  (define-test nullable-empty-test
    (assert-false (nullable? rx-empty)))

  (define-test nullable-epsilon-test
    (assert-true (nullable? rx-epsilon)))

  (define-test nullable-char-test
    (assert-false (nullable? (rx-char #\a))))

  (define-test nullable-seq-test
    (assert-false (nullable? (rx-seq (rx-char #\a) rx-epsilon)))
    (assert-true (nullable? (rx-seq rx-epsilon rx-epsilon))))

  (define-test nullable-alt-test
    (assert-true (nullable? (rx-alt rx-epsilon (rx-char #\a))))
    (assert-false (nullable? (rx-alt (rx-char #\a) (rx-char #\b)))))

  (define-test nullable-star-test
    (assert-true (nullable? (rx-star (rx-char #\a))))))

;;; ============================================================
;;; Derivative Tests
;;; ============================================================

(test-group derivatives
  (define-test deriv-char-match-test
    (let ([d (derivative (rx-char #\a) #\a)])
      (assert-true (nullable? d))))

  (define-test deriv-char-no-match-test
    (let ([d (derivative (rx-char #\a) #\b)])
      (assert-true (rx-empty? d))))

  (define-test deriv-seq-test
    (let* ([r (rx-seq (rx-char #\a) (rx-char #\b))]
           [d (derivative r #\a)])
      (assert-true (rx-match? d "b"))))

  (define-test deriv-star-test
    (let* ([r (rx-star (rx-char #\a))]
           [d (derivative r #\a)])
      ;; After consuming one 'a', can match zero or more more
      (assert-true (rx-match? d ""))
      (assert-true (rx-match? d "aa")))))

;;; ============================================================
;;; Pattern Matching Tests
;;; ============================================================

(test-group pattern-matching
  (define-test complex-pattern-test
    ;; (a|b)*c
    (let ([r (rx-seq (rx-star (rx-alt (rx-char #\a) (rx-char #\b)))
                     (rx-char #\c))])
      (assert-true (rx-match? r "c"))
      (assert-true (rx-match? r "ac"))
      (assert-true (rx-match? r "bc"))
      (assert-true (rx-match? r "abababc"))
      (assert-false (rx-match? r "ab"))))

  (define-test identifier-pattern-test
    (let ([r rx-identifier])
      (assert-true (rx-match? r "foo"))
      (assert-true (rx-match? r "foo_bar"))
      (assert-true (rx-match? r "x123"))
      (assert-false (rx-match? r "123x"))
      (assert-false (rx-match? r "_foo"))))

  (define-test integer-pattern-test
    (let ([r rx-integer])
      (assert-true (rx-match? r "42"))
      (assert-true (rx-match? r "-123"))
      (assert-true (rx-match? r "+7"))
      (assert-false (rx-match? r "abc"))))

  (define-test decimal-pattern-test
    (let ([r rx-decimal])
      (assert-true (rx-match? r "3.14"))
      (assert-true (rx-match? r "42"))
      (assert-true (rx-match? r "-123.456"))
      (assert-false (rx-match? r ".5")))))  ; Requires leading digit

;;; ============================================================
;;; Find Tests
;;; ============================================================

(test-group finding
  (define-test find-first-test
    (let* ([r (rx-plus rx-digit)]
           [result (rx-find-first r "abc123def")])
      (assert-true (just? result))
      (assert-equal 3 (car (from-just result)))
      (assert-equal 6 (cdr (from-just result)))))

  (define-test find-first-at-start-test
    (let* ([r (rx-plus rx-alpha)]
           [result (rx-find-first r "hello123")])
      (assert-true (just? result))
      (assert-equal 0 (car (from-just result)))))

  (define-test find-first-not-found-test
    (let ([result (rx-find-first rx-digit "abc")])
      (assert-true (nothing? result))))

  (define-test find-all-test
    (let* ([r (rx-plus rx-digit)]
           [results (rx-find-all r "a1b22c333")])
      (assert-equal 3 (length results))))

  (define-test extract-test
    (let ([result (rx-extract rx-integer "The answer is 42!")])
      (assert-true (just? result))
      (assert-equal "42" (from-just result))))

  (define-test extract-all-test
    (let ([results (rx-extract-all rx-integer "1 plus 2 equals 3")])
      (assert-equal 3 (length results))
      (assert-equal "1" (car results))
      (assert-equal "2" (cadr results))
      (assert-equal "3" (caddr results)))))

;;; ============================================================
;;; String Operation Tests
;;; ============================================================

(test-group string-operations
  (define-test split-test
    (let* ([r (rx-char #\,)]
           [result (rx-split r "a,b,c")])
      (assert-equal 3 (length result))
      (assert-equal "a" (car result))
      (assert-equal "b" (cadr result))
      (assert-equal "c" (caddr result))))

  (define-test split-whitespace-test
    (let ([result (rx-split rx-whitespace "hello   world")])
      (assert-equal 2 (length result))
      (assert-equal "hello" (car result))
      (assert-equal "world" (cadr result))))

  (define-test replace-test
    (let ([result (rx-replace rx-digit "X" "a1b2c3")])
      (assert-equal "aXb2c3" result)))

  (define-test replace-all-test
    (let ([result (rx-replace-all rx-digit "X" "a1b2c3")])
      (assert-equal "aXbXcX" result)))

  (define-test replace-word-test
    (let ([result (rx-replace-all (rx-string "foo") "bar" "foo and foo")])
      (assert-equal "bar and bar" result))))

;;; ============================================================
;;; Simplification Tests
;;; ============================================================

(test-group simplification
  (define-test simplify-empty-seq-test
    ;; empty . r = empty
    (let ([r (rx-seq rx-empty (rx-char #\a))])
      (assert-true (rx-empty? r))))

  (define-test simplify-epsilon-seq-test
    ;; epsilon . r = r
    (let ([r (rx-seq rx-epsilon (rx-char #\a))])
      (assert-true (rx-char? r))))

  (define-test simplify-empty-alt-test
    ;; empty | r = r
    (let ([r (rx-alt rx-empty (rx-char #\a))])
      (assert-true (rx-char? r))))

  (define-test simplify-star-star-test
    ;; (r*)* = r*
    (let ([r (rx-star (rx-star (rx-char #\a)))])
      (assert-true (rx-star? r))
      (assert-true (rx-char? (rx-star-inner r)))))

  (define-test simplify-empty-star-test
    ;; empty* = epsilon
    (assert-true (rx-epsilon? (rx-star rx-empty)))))

;;; ============================================================
;;; Edge Cases
;;; ============================================================

(test-group edge-cases
  (define-test empty-string-match-test
    (assert-true (rx-match? rx-epsilon ""))
    (assert-true (rx-match? (rx-star rx-digit) "")))

  (define-test nested-star-test
    ;; (a*)* should still work
    (let ([r (rx-star (rx-star (rx-char #\a)))])
      (assert-true (rx-match? r ""))
      (assert-true (rx-match? r "aaa"))))

  (define-test deep-alternation-test
    (let ([r (rx-any-of (list (rx-char #\a)
                              (rx-char #\b)
                              (rx-char #\c)
                              (rx-char #\d)))])
      (assert-true (rx-match? r "a"))
      (assert-true (rx-match? r "d"))
      (assert-false (rx-match? r "e"))))

  (define-test rx-to-string-test
    (let ([s (rx->string (rx-seq (rx-char #\a) (rx-star (rx-char #\b))))])
      (assert-true (string? s))
      (assert-true (> (string-length s) 0)))))

;;; ============================================================
;;; Complex Pattern Tests
;;; ============================================================

(test-group complex-patterns
  (define-test email-simple-test
    (assert-true (rx-match? rx-email-simple "user@example.com"))
    (assert-true (rx-match? rx-email-simple "a@b.co"))
    (assert-false (rx-match? rx-email-simple "@invalid.com"))
    (assert-false (rx-match? rx-email-simple "no-at-sign")))

  (define-test phone-like-test
    ;; Simple phone pattern: 3 digits, dash, 3 digits, dash, 4 digits
    (let ([r (rx-seq-of (list (rx-count 3 rx-digit)
                              (rx-char #\-)
                              (rx-count 3 rx-digit)
                              (rx-char #\-)
                              (rx-count 4 rx-digit)))])
      (assert-true (rx-match? r "123-456-7890"))
      (assert-false (rx-match? r "12-456-7890"))
      (assert-false (rx-match? r "1234567890"))))

  (define-test ip-octet-test
    ;; Match 1-3 digits (simple, doesn't validate 0-255)
    (let* ([octet (rx-range 1 3 rx-digit)]
           [dot (rx-char #\.)]
           [ip (rx-seq-of (list octet dot octet dot octet dot octet))])
      (assert-true (rx-match? ip "192.168.1.1"))
      (assert-true (rx-match? ip "1.2.3.4"))
      (assert-false (rx-match? ip "192.168.1")))))

;;; ============================================================
;;; Summary
;;; ============================================================

(display "\n")
(display "==============================================================\n")
(printf "Tests passed: ~a\n" *tests-passed*)
(printf "Tests failed: ~a\n" *tests-failed*)
(printf "Total tests:  ~a\n" *tests-run*)

(if (= *tests-failed* 0)
    (display "\n[SUCCESS] All regex tests passed.\n")
    (display "\n[FAILURE] Some regex tests failed.\n"))

