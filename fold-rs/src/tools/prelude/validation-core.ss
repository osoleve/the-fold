; ============================================================
; Validation - Core Operations
; Type predicates, validation combinators, safe operations
; Part of validation.ss module
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

; -- Extended Validation Functions --

; validate-all: Check all predicates pass
(validate-all (fn (preds val)
                  (if (all (fn (p) (p val)) preds) val #f)))

; validate-type: Validate value has expected type
(validate-type (fn (type-pred value msg)
                   (if (type-pred value)
                       (right value)
                       (left msg))))

; validate-range: Validate number is in range
(validate-range (fn (lo hi value)
                    (if (and (>= value lo) (<= value hi))
                        (right value)
                        (left (list 'out-of-range lo hi value)))))

; validate-length: Validate list has expected length
(validate-length (fn (n lst)
                     (if (= (length lst) n)
                         (right lst)
                         (left (list 'wrong-length n (length lst))))))

; validate-not-empty: Validate list/string is not empty
(validate-not-empty (fn (value)
                        (if (if (string? value)
                                (not (string-empty? value))
                                (not (null? value)))
                            (right value)
                            (left 'empty-value))))

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

; --- Module Exports ---
; (see exports.ss for exported symbols)
