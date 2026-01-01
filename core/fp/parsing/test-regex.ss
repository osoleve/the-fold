;;; fabric/stitches/fp/test-regex.ss — Tests for Regular Expressions

;;; NOTE: Run from fabric/stitches directory

(load "core/test-framework.ss")
(load "core/fp/parsing/regex.ss")

(display "
")
(display "==============================================================
")
(display "         REGULAR EXPRESSION TESTS
")
(display "==============================================================
")

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
              (let ([r (rx-char #)])
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
              (let ([r (rx-seq (rx-char #) (rx-char #))])
                   (assert-true (rx-match? r "ab"))
                   (assert-false (rx-match? r "a"))
                   (assert-false (rx-match? r "ba"))
                   (assert-false (rx-match? r "abc"))))
            
            (define-test rx-alt-test
              (let ([r (rx-alt (rx-char #) (rx-char #))])
                   (assert-true (rx-match? r "a"))
                   (assert-true (rx-match? r "b"))
                   (assert-false (rx-match? r "c"))
                   (assert-false (rx-match? r "ab"))))
            
            (define-test rx-star-test
              (let ([r (rx-star (rx-char #))])
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
              (let ([r (rx-plus (rx-char #))])
                   (assert-false (rx-match? r ""))
                   (assert-true (rx-match? r "a"))
                   (assert-true (rx-match? r "aaaa"))))
            
            (define-test rx-opt-test
              (let ([r (rx-opt (rx-char #))])
                   (assert-true (rx-match? r ""))
                   (assert-true (rx-match? r "a"))
                   (assert-false (rx-match? r "aa"))))
            
            (define-test rx-count-test
              (let ([r (rx-count 3 (rx-char #))])
                   (assert-false (rx-match? r "aa"))
                   (assert-true (rx-match? r "aaa"))
                   (assert-false (rx-match? r "aaaa"))))
            
            (define-test rx-range-test
              (let ([r (rx-range 2 4 (rx-char #))])
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
              (let ([r (rx-char-range # #\z)])
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
              (assert-true (rx-match? rx-space "	"))
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
              (assert-false (nullable? (rx-char #))))
            
            (define-test nullable-seq-test
              (assert-false (nullable? (rx-seq (rx-char #) rx-epsilon)))
              (assert-true (nullable? (rx-seq rx-epsilon rx-epsilon))))
            
            (define-test nullable-alt-test
              (assert-true (nullable? (rx-alt rx-epsilon (rx-char #))))
              (assert-false (nullable? (rx-alt (rx-char #) (rx-char #)))))
            
            (define-test nullable-star-test
              (assert-true (nullable? (rx-star (rx-char #))))))

;;; ============================================================
;;; Derivative Tests
;;; ============================================================

(test-group derivatives
            (define-test deriv-char-match-test
              (let ([d (derivative (rx-char #) #)])
                   (assert-true (nullable? d))))
            
            (define-test deriv-char-no-match-test
              (let ([d (derivative (rx-char #) #)])
                   (assert-true (rx-empty? d))))
            
            (define-test deriv-seq-test
              (let* ([r (rx-seq (rx-char #) (rx-char #))]
                     [d (derivative r #)])
                    (assert-true (rx-match? d "b"))))
            
            (define-test deriv-star-test
              (let* ([r (rx-star (rx-char #))]
                     [d (derivative r #)])
                    ;; After consuming one 'a', can match zero or more more
                    (assert-true (rx-match? d ""))
                    (assert-true (rx-match? d "aa")))))

;;; ============================================================
;;; Pattern Matching Tests
;;; ============================================================

(test-group pattern-matching
            (define-test complex-pattern-test
              ;; (a|b)*c
              (let ([r (rx-seq (rx-star (rx-alt (rx-char #) (rx-char #)))
                               (rx-char #