; ============================================================
; Validation - Assertions and Testing
; Test framework, assertions, debug utilities
; Part of validation.ss module
; ============================================================

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

; --- Module Exports ---
; (see exports.ss for exported symbols)
