;;; fabric/stitches/fp/validation.ss — Validation with Error Accumulation
;;;
;;; Validation is like Either but accumulates errors instead of short-circuiting.
;;; This enables collecting ALL validation errors rather than stopping at the first.
;;;
;;; Validation e a = Success a | Failure (List e)
;;;
;;; Key difference from Either:
;;;   - Either: First error stops computation (Monad)
;;;   - Validation: All errors collected (Applicative, not Monad)
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Features:
;;;   - Validation type (success, failure)
;;;   - Applicative operations (pure, ap, map)
;;;   - Error accumulation
;;;   - Common validators
;;;   - Validation combinators
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - fp/combinators.ss

(load "fabric/stitches/prelude.ss")
(load "fabric/stitches/fp/combinators.ss")

;;; ============================================================
;;; Validation Type
;;; ============================================================

;;; success : a -> Validation e a
;;; Create a successful validation.
(define (validation-success x)
  (list 'success x))

;;; failure : e -> Validation e a
;;; Create a failed validation with a single error.
(define (validation-failure err)
  (list 'failure (list err)))

;;; failures : (List e) -> Validation e a
;;; Create a failed validation with multiple errors.
(define (validation-failures errs)
  (list 'failure errs))

;;; success? : Validation e a -> Boolean
(define (validation-success? v)
  (and (pair? v) (eq? (car v) 'success)))

;;; failure? : Validation e a -> Boolean
(define (validation-failure? v)
  (and (pair? v) (eq? (car v) 'failure)))

;;; from-success : Validation e a -> a
;;; Extract value from Success (partial).
(define (from-success v)
  (cadr v))

;;; from-failure : Validation e a -> (List e)
;;; Extract errors from Failure (partial).
(define (from-failure v)
  (cadr v))

;;; ============================================================
;;; Functor Operations
;;; ============================================================

;;; validation-map : (a -> b) -> Validation e a -> Validation e b
;;; Map over successful value.
(define (validation-map f v)
  (if (validation-success? v)
      (validation-success (f (from-success v)))
      v))

;;; ============================================================
;;; Applicative Operations
;;; ============================================================

;;; validation-pure : a -> Validation e a
;;; Lift a value into Validation.
(define validation-pure validation-success)

;;; validation-ap : Validation e (a -> b) -> Validation e a -> Validation e b
;;; Apply a validated function to a validated value.
;;; If both fail, errors are accumulated.
(define (validation-ap vf va)
  (cond
   [(and (validation-success? vf) (validation-success? va))
    (validation-success ((from-success vf) (from-success va)))]
   [(and (validation-failure? vf) (validation-failure? va))
    (validation-failures (append (from-failure vf) (from-failure va)))]
   [(validation-failure? vf) vf]
   [else va]))

;;; validation-ap2 : (a -> b -> c) -> Validation e a -> Validation e b -> Validation e c
;;; Apply a 2-argument function to two validations.
(define (validation-ap2 f va vb)
  (validation-ap (validation-map f va) vb))

;;; validation-ap3 : (a -> b -> c -> d) -> Validation e a -> Validation e b -> Validation e c -> Validation e d
;;; Apply a 3-argument function to three validations.
(define (validation-ap3 f va vb vc)
  (validation-ap (validation-ap (validation-map f va) vb) vc))

;;; validation-ap4 : Apply a 4-argument function.
(define (validation-ap4 f va vb vc vd)
  (validation-ap (validation-ap3 f va vb vc) vd))

;;; ============================================================
;;; Combining Validations
;;; ============================================================

;;; validation-sequence : (List (Validation e a)) -> Validation e (List a)
;;; Sequence validations, accumulating all errors.
(define (validation-sequence vs)
  (if (null? vs)
      (validation-success '())
      (validation-ap2 (lambda (x) (lambda (xs) (cons x xs)))
                      (car vs)
                      (validation-sequence (cdr vs)))))

;;; validation-traverse : (a -> Validation e b) -> (List a) -> Validation e (List b)
;;; Map and sequence in one pass.
(define (validation-traverse f lst)
  (validation-sequence (map f lst)))

;;; validation-concat : Validation e a -> Validation e b -> Validation e b
;;; Run both validations, keep second result if both succeed, accumulate errors.
(define (validation-concat va vb)
  (validation-ap2 (lambda (a) (lambda (b) b)) va vb))

;;; validation-all : (List (Validation e ())) -> Validation e ()
;;; Check that all validations pass, accumulating errors.
(define (validation-all vs)
  (validation-map (lambda (_) '()) (validation-sequence vs)))

;;; ============================================================
;;; Converting to/from Either
;;; ============================================================

;;; validation->either : Validation e a -> Either (List e) a
(define (validation->either v)
  (if (validation-success? v)
      (right (from-success v))
      (left (from-failure v))))

;;; either->validation : Either e a -> Validation e a
(define (either->validation e)
  (if (right? e)
      (validation-success (from-right e))
      (validation-failure (from-left e))))

;;; ============================================================
;;; Validator Type
;;; ============================================================
;;;
;;; A Validator is a function that takes a value and returns
;;; a Validation. This allows composing validators.
;;;
;;; Validator e a = a -> Validation e a

;;; make-validator : (a -> Boolean) -> e -> Validator e a
;;; Create a validator from a predicate and error message.
(define (make-validator pred error)
  (lambda (x)
          (if (pred x)
              (validation-success x)
              (validation-failure error))))

;;; run-validator : Validator e a -> a -> Validation e a
;;; Run a validator on a value.
(define (run-validator validator value)
  (validator value))

;;; ============================================================
;;; Validator Combinators
;;; ============================================================

;;; validator-and : Validator e a -> Validator e a -> Validator e a
;;; Combine validators, running both and accumulating errors.
(define (validator-and v1 v2)
  (lambda (x)
          (validation-ap2 (lambda (a) (lambda (b) a)) (v1 x) (v2 x))))

;;; validator-all : (List (Validator e a)) -> Validator e a
;;; Combine multiple validators.
(define (validator-all validators)
  (if (null? validators)
      (lambda (x) (validation-success x))
      (validator-and (car validators) (validator-all (cdr validators)))))

;;; validator-or : Validator e a -> Validator e a -> Validator e a
;;; Try first validator, if fails try second (no accumulation).
(define (validator-or v1 v2)
  (lambda (x)
          (let ([result (v1 x)])
               (if (validation-success? result)
                   result
                   (v2 x)))))

;;; validator-map-error : (e -> e2) -> Validator e a -> Validator e2 a
;;; Transform error messages.
(define (validator-map-error f validator)
  (lambda (x)
          (let ([result (validator x)])
               (if (validation-success? result)
                   result
                   (validation-failures (map f (from-failure result)))))))

;;; validator-focus : (b -> a) -> Validator e a -> Validator e b
;;; Apply validator to a part of the input.
(define (validator-focus getter validator)
  (lambda (x)
          (validation-map (lambda (_) x) (validator (getter x)))))

;;; ============================================================
;;; Common Validators
;;; ============================================================

;;; required : e -> Validator e a
;;; Validates that value is not #f or null.
(define (required error)
  (make-validator (lambda (x) (and x (not (null? x)))) error))

;;; not-empty : e -> Validator e String
;;; Validates that string is not empty.
(define (not-empty error)
  (make-validator (lambda (s) (and (string? s) (> (string-length s) 0))) error))

;;; min-length : Nat -> e -> Validator e String
;;; Validates minimum string length.
(define (min-length n error)
  (make-validator (lambda (s) (and (string? s) (>= (string-length s) n))) error))

;;; max-length : Nat -> e -> Validator e String
;;; Validates maximum string length.
(define (max-length n error)
  (make-validator (lambda (s) (and (string? s) (<= (string-length s) n))) error))

;;; in-range : Nat -> Nat -> e -> Validator e Number
;;; Validates number is in range [min, max].
(define (in-range min-val max-val error)
  (make-validator (lambda (n) (and (number? n) (>= n min-val) (<= n max-val))) error))

;;; positive : e -> Validator e Number
;;; Validates number is positive.
(define (positive error)
  (make-validator (lambda (n) (and (number? n) (> n 0))) error))

;;; non-negative : e -> Validator e Number
;;; Validates number is non-negative.
(define (non-negative error)
  (make-validator (lambda (n) (and (number? n) (>= n 0))) error))

;;; is-type : (a -> Boolean) -> e -> Validator e a
;;; Validates value satisfies type predicate.
(define (is-type pred error)
  (make-validator pred error))

;;; is-string : e -> Validator e a
(define (is-string error)
  (is-type string? error))

;;; is-number : e -> Validator e a
(define (is-number error)
  (is-type number? error))

;;; is-list : e -> Validator e a
(define (is-list error)
  (is-type list? error))

;;; is-symbol : e -> Validator e a
(define (is-symbol error)
  (is-type symbol? error))

;;; one-of : (List a) -> e -> Validator e a
;;; Validates value is in a list of allowed values.
(define (one-of allowed error)
  (make-validator (lambda (x) (member x allowed)) error))

;;; ============================================================
;;; Field Validation (for Records/Alists)
;;; ============================================================

;;; validate-field : Symbol -> Validator e a -> Validator e (Alist Symbol a)
;;; Validate a specific field in an alist.
(define (validate-field field-name validator)
  (lambda (alist)
          (let ([pair (assoc field-name alist)])
               (if pair
                   (let ([result (validator (cdr pair))])
                        (validation-map (lambda (_) alist) result))
                   (validation-failure (list 'missing-field field-name))))))

;;; validate-optional-field : Symbol -> Validator e a -> Validator e (Alist Symbol a)
;;; Validate a field only if it exists.
(define (validate-optional-field field-name validator)
  (lambda (alist)
          (let ([pair (assoc field-name alist)])
               (if pair
                   (let ([result (validator (cdr pair))])
                        (validation-map (lambda (_) alist) result))
                   (validation-success alist)))))

;;; validate-fields : (List (Symbol . Validator e a)) -> Validator e (Alist Symbol a)
;;; Validate multiple fields.
(define (validate-fields field-validators)
  (validator-all
   (map (lambda (fv)
                (validate-field (car fv) (cdr fv)))
        field-validators)))

;;; ============================================================
;;; List Validation
;;; ============================================================

;;; validate-each : Validator e a -> Validator e (List a)
;;; Validate each element of a list.
(define (validate-each validator)
  (lambda (lst)
          (validation-traverse validator lst)))

;;; validate-at : Nat -> Validator e a -> Validator e (List a)
;;; Validate element at specific index.
(define (validate-at index validator)
  (lambda (lst)
          (if (< index (length lst))
              (let ([result (validator (list-ref lst index))])
                   (validation-map (lambda (_) lst) result))
              (validation-failure (list 'index-out-of-bounds index)))))

;;; ============================================================
;;; Conditional Validation
;;; ============================================================

;;; validate-when : (a -> Boolean) -> Validator e a -> Validator e a
;;; Only validate when predicate is true.
(define (validate-when pred validator)
  (lambda (x)
          (if (pred x)
              (validator x)
              (validation-success x))))

;;; validate-unless : (a -> Boolean) -> Validator e a -> Validator e a
;;; Only validate when predicate is false.
(define (validate-unless pred validator)
  (validate-when (lambda (x) (not (pred x))) validator))

;;; ============================================================
;;; Error Formatting
;;; ============================================================

;;; format-errors : Validation e a -> String
;;; Format errors as a string.
(define (format-errors v)
  (if (validation-success? v)
      ""
      (let ([errors (from-failure v)])
           (apply string-append
                  (map (lambda (e)
                               (string-append "- " (if (string? e) e (format "~a" e)) "
"))
                       errors)))))

;;; with-field-prefix : Symbol -> Validation e a -> Validation (Symbol . e) a
;;; Prefix errors with field name.
(define (with-field-prefix field-name v)
  (if (validation-success? v)
      v
      (validation-failures
       (map (lambda (e) (cons field-name e))
            (from-failure v)))))

;;; ============================================================
;;; Example Usage (for documentation)
;;; ============================================================
;;;
;;; ;; Define validators
;;; (define username-validator
;;;   (validator-all
;;;    (list (not-empty "Username required")
;;;          (min-length 3 "Username too short")
;;;          (max-length 20 "Username too long"))))
;;;
;;; (define age-validator
;;;   (validator-all
;;;    (list (is-number "Age must be a number")
;;;          (in-range 0 150 "Age must be between 0 and 150"))))
;;;
;;; ;; Validate a user record
;;; (define (validate-user user)
;;;   (validation-ap2
;;;    (lambda (name age) user)
;;;    (with-field-prefix 'username (username-validator (cdr (assoc 'username user))))
;;;    (with-field-prefix 'age (age-validator (cdr (assoc 'age user))))))
;;;
;;; ;; Run validation
;;; (validate-user '((username . "ab") (age . -5)))
;;; ; => (failure ((username . "Username too short") (age . "Age must be between 0 and 150")))
;;;
;;; (validate-user '((username . "alice") (age . 25)))
;;; ; => (success ((username . "alice") (age . 25)))
