; ============================================================
; Validation and Testing Utilities
; Assertions, predicates, and test framework
; ============================================================

; -- Type predicates (compound) --

; list-of?: Check if all elements satisfy predicate
(list-of? (fn (pred lst)
              (and (list? lst) (all pred lst))))

; non-empty-list?: Check if list is non-empty
(non-empty-list? (fn (lst)
                     (and (list? lst) (not (null? lst)))))

; non-empty-string?: Check if string is non-empty
(non-empty-string? (fn (s)
                       (and (string? s) (> (string-length s) 0))))

; natural?: Check if number is non-negative integer
(natural? (fn (n)
              (and (integer? n) (>= n 0))))

; positive-integer?: Check if number is positive integer
(positive-integer? (fn (n)
                       (and (integer? n) (> n 0))))

; -- Assertions --

; assert-eq: Assert two values are equal
(assert-eq (fn (expected actual msg)
               (if (equal? expected actual)
                   #t
                   (list 'assertion-failed msg 'expected expected 'actual actual))))

; assert-true: Assert value is truthy
(assert-true (fn (val msg)
                 (if val
                     #t
                     (list 'assertion-failed msg 'expected #t 'actual val))))

; assert-false: Assert value is falsy
(assert-false (fn (val msg)
                  (if (not val)
                      #t
                      (list 'assertion-failed msg 'expected #f 'actual val))))

; assert-throws: Assert that expression produces error
; Note: This is a stub - real implementation needs error handling
(assert-throws (fn (thunk msg)
                   (list 'assertion-skipped msg 'reason "no error handling")))

; assert-pred: Assert predicate holds on value
(assert-pred (fn (pred val msg)
                 (if (pred val)
                     #t
                     (list 'assertion-failed msg 'predicate-failed-on val))))

; -- Test framework --

; test-case: Create a test case
(test-case (fn (name assertions)
               (list 'test name assertions)))

; run-tests: Run a list of test cases, return results
(run-tests (fn (tests)
               (map (fn (test)
                        (let ((name (cadr test))
                              (assertions (caddr test)))
                             (let ((results (map (fn (a) (a)) assertions)))
                                  (list name
                                        (all (fn (r) (eq? r #t)) results)
                                        results))))
                    tests)))

; test-suite: Create a test suite
(test-suite (fn (name tests)
                (list 'suite name tests)))

; run-suite: Run a test suite
(run-suite (fn (suite)
               (let ((name (cadr suite))
                     (tests (caddr suite)))
                    (list name (run-tests tests)))))

; -- Validation combinators --

; validate: Apply validators, collect errors
(validate (fn (validators value)
              (let ((errors (filter (fn (r) (not (eq? r #t)))
                                    (map (fn (v) (v value)) validators))))
                   (if (null? errors)
                       (right value)
                       (left errors)))))

; validator: Create a validator from predicate and message
(validator (fn (pred msg)
               (fn (value)
                   (if (pred value) #t (list 'invalid msg value)))))

; required: Validator that value is truthy
(required (validator id "value is required"))

; min-length: Validator for minimum length
(min-length (fn (n)
                (validator (fn (s) (>= (string-length s) n))
                           (string-append "minimum length is " (number->string n)))))

; max-length: Validator for maximum length
(max-length (fn (n)
                (validator (fn (s) (<= (string-length s) n))
                           (string-append "maximum length is " (number->string n)))))

; in-range: Validator for numeric range
(in-range (fn (lo hi)
              (validator (fn (n) (and (>= n lo) (<= n hi)))
                         (string-append "must be between " (number->string lo)
                                        " and " (number->string hi)))))

; matches-pattern: Validator for pattern match
(matches-pattern (fn (pattern)
                     (validator (fn (s) (string-contains? pattern s))
                                (string-append "must contain " pattern))))

; -- Safe operations --

; safe-car: Car that returns default on non-pair
(safe-car (fn (default lst)
              (if (pair? lst) (car lst) default)))

; safe-cdr: Cdr that returns default on non-pair
(safe-cdr (fn (default lst)
              (if (pair? lst) (cdr lst) default)))

; safe-nth: Nth that returns default on out of bounds
(safe-nth (fn (default n lst)
              (if (or (< n 0) (>= n (length lst)))
                  default
                  (nth lst n))))

; safe-div: Division that returns default on divide by zero
(safe-div (fn (default a b)
              (if (= b 0) default (/ a b))))

; safe-head: First element or default
(safe-head (fn (default lst)
               (if (null? lst) default (car lst))))

; safe-tail: All but first or default
(safe-tail (fn (default lst)
               (if (null? lst) default (cdr lst))))

; safe-last: Last element or default
(safe-last (fn (default lst)
               (if (null? lst) default (last lst))))

; -- Input sanitization --

; sanitize-string: Remove potentially dangerous characters
(sanitize-string (fn (s)
                     (string-filter (fn (c)
                                        (or (char-alphabetic? c)
                                            (char-numeric? c)
                                            (eq? c #\space)
                                            (eq? c #\-)
                                            (eq? c #\_)))
                                    s)))

; clamp-string: Limit string length
(clamp-string (fn (max-len s)
                  (if (<= (string-length s) max-len)
                      s
                      (substring s 0 max-len))))

; normalize-whitespace: Collapse whitespace
(normalize-whitespace string-normalize)

; -- Debug utilities --

; trace: Print value and return it
(trace (fn (label value)
           (begin
            (display label)
            (display ": ")
            (write value)
            (newline)
            value)))

; tap: Apply function for side effect, return original value
(tap (fn (f x)
         (begin (f x) x)))

; spy: Log and return value (alias for trace without label)
(spy (fn (x)
         (begin (write x) (newline) x)))
