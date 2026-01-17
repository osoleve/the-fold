;;; lattice/fp/meta/result.ss — Result Type Operations
;;;
;;; Consolidates the Result type pattern used throughout The Fold.
;;; Re-exports core operations from prelude.ss and adds utilities.
;;;
;;; Result α ε = (ok α) | (error ε)
;;;
;;; This is lattice code: pure, assumes reasonable input.
;;; Core Result operations are in core/base/prelude.ss.
;;;
;;; Features:
;;;   - Constructors: ok, error (from prelude)
;;;   - Predicates: ok?, error? (from prelude)
;;;   - Accessors: unwrap-ok, unwrap-error (from prelude)
;;;   - Monadic: result-bind, result-map, result-sequence (from prelude)
;;;   - New: result-catch, result-fold, result-default, result-or
;;;   - New: result-map-error, result-bimap
;;;   - New: assert-ok!, result->maybe, maybe->result
;;;
;;; Dependencies:
;;;   - core/base/prelude.ss (provides core Result ops)
;;;   - lattice/fp/meta/combinators.ss (provides Maybe, Either)

(load "core/base/prelude.ss")
(load "lattice/fp/meta/combinators.ss")

;;; ====
;;; Re-exports from prelude (documented here for discoverability)
;;; ====
;;;
;;; Constructors:
;;;   (list 'ok value)     — success result
;;;   (list 'error . rest) — failure result
;;;
;;; Predicates:
;;;   ok?           : Any → Bool
;;;   error?        : Any → Bool
;;;
;;; Accessors:
;;;   unwrap-ok     : (Result α ε) → α        (assumes ok)
;;;   unwrap-error  : (Result α ε) → ε        (assumes error)
;;;
;;; Monadic operations:
;;;   result-map      : (α → β) × (Result α ε) → (Result β ε)
;;;   result-bind     : (Result α ε) × (α → Result β ε) → (Result β ε)
;;;   result-sequence : (List (Result α ε)) → (Result (List α) ε)

;;; ====
;;; Result Constructors (explicit)
;;; ====

;;; ok : α → (Result α ε)
;;; Wrap a value as success.
(define (ok value)
  (list 'ok value))

;;; err : ε → (Result α ε)
;;; Wrap an error value. Alternative to bare (error ...) for clarity.
(define (err e)
  (list 'error e))

;;; ====
;;; Additional Result Operations
;;; ====

;;; result-fold : (α → β) → (ε → β) → (Result α ε) → β
;;; Pattern match on Result, applying appropriate function.
;;; Note: on-error receives (cdr result) - the full error payload after 'error tag.
;;; For (error 'tag "msg"), on-error receives ('tag "msg").
(define (result-fold on-ok on-error result)
  (if (ok? result)
      (on-ok (unwrap-ok result))
      (on-error (cdr result))))

;;; result-default : α → (Result α ε) → α
;;; Extract value or return default on error.
(define (result-default default result)
  (if (ok? result)
      (unwrap-ok result)
      default))

;;; result-or : (Result α ε) → (Result α ε) → (Result α ε)
;;; Return first ok result, or last error if both fail.
(define (result-or r1 r2)
  (if (ok? r1) r1 r2))

;;; result-and : (Result α ε) → (Result β ε) → (Result β ε)
;;; Return second if first is ok, otherwise first error.
(define (result-and r1 r2)
  (if (ok? r1) r2 r1))

;;; result-map-error : (ε₁ → ε₂) → (Result α ε₁) → (Result α ε₂)
;;; Transform the error value, leaving ok unchanged.
;;; Note: f receives and should return (cdr result) - the full error payload.
;;; Uses cons to preserve flat error structure: (error tag details...).
(define (result-map-error f result)
  (if (ok? result)
      result
      (cons 'error (f (cdr result)))))

;;; result-bimap : (α → β) → (ε₁ → ε₂) → (Result α ε₁) → (Result β ε₂)
;;; Transform both success and error cases.
;;; Note: f-err receives and should return (cdr result) - the full error payload.
;;; Uses cons to preserve flat error structure: (error tag details...).
(define (result-bimap f-ok f-err result)
  (if (ok? result)
      (ok (f-ok (unwrap-ok result)))
      (cons 'error (f-err (cdr result)))))

;;; ====
;;; Exception Integration
;;; ====

;;; result-catch : (→ α) → (Result α Condition)
;;; Execute thunk, catching exceptions as error results.
;;; Returns (ok value) on success, (error condition) on exception.
(define (result-catch thunk)
  (guard (e [else (list 'error e)])
         (ok (thunk))))

;;; result-catch/handler : (→ α) → (Condition → ε) → (Result α ε)
;;; Execute thunk with custom error transformation.
(define (result-catch/handler thunk handler)
  (guard (e [else (list 'error (handler e))])
         (ok (thunk))))

;;; result-try : (→ α) → α → α
;;; Execute thunk, returning default on any exception.
;;; Simpler than result-catch when you just want a fallback.
(define (result-try thunk default)
  (guard (e [else default])
         (thunk)))

;;; ====
;;; Assertions
;;; ====

;;; assert-ok! : (Result α ε) → α
;;; Unwrap ok value or raise error with message.
;;; Use sparingly - prefer result-bind for chaining.
(define (assert-ok! result)
  (if (ok? result)
      (unwrap-ok result)
      (error 'assert-ok! "Expected ok result" (cdr result))))

;;; assert-ok/msg! : (Result α ε) → String → α
;;; Unwrap with custom error message.
(define (assert-ok/msg! result msg)
  (if (ok? result)
      (unwrap-ok result)
      (error 'assert-ok! msg (cdr result))))

;;; ====
;;; Conversions
;;; ====

;;; result->maybe : (Result α ε) → (Maybe α)
;;; Convert Result to Maybe, discarding error info.
(define (result->maybe result)
  (if (ok? result)
      (just (unwrap-ok result))
      nothing))

;;; maybe->result : (Maybe α) → ε → (Result α ε)
;;; Convert Maybe to Result, using provided error for Nothing.
(define (maybe->result m err-val)
  (if (just? m)
      (ok (from-just m))
      (list 'error err-val)))

;;; result->either : (Result α ε) → (Either ε α)
;;; Convert Result to Either (error on left, value on right).
(define (result->either result)
  (if (ok? result)
      (right (unwrap-ok result))
      (left (cdr result))))

;;; either->result : (Either ε α) → (Result α ε)
;;; Convert Either to Result.
(define (either->result e)
  (if (right? e)
      (ok (from-right e))
      (list 'error (from-left e))))

;;; ====
;;; Applicative Operations
;;; ====

;;; result-ap : (Result (α → β) ε) → (Result α ε) → (Result β ε)
;;; Applicative apply for Result.
(define (result-ap rf ra)
  (result-bind rf
               (lambda (f)
                 (result-map f ra))))

;;; result-lift2 : (α → β → γ) → (Result α ε) → (Result β ε) → (Result γ ε)
;;; Lift a binary function to work on Results.
(define (result-lift2 f ra rb)
  (result-bind ra
               (lambda (a)
                 (result-map (lambda (b) (f a b)) rb))))

;;; ====
;;; Traversal
;;; ====

;;; result-traverse : (α → Result β ε) → (List α) → (Result (List β) ε)
;;; Map a result-producing function over a list, collecting results.
(define (result-traverse f xs)
  (result-sequence (map f xs)))

;;; result-filter : (α → Result Bool ε) → (List α) → (Result (List α) ε)
;;; Filter list using result-producing predicate.
(define (result-filter pred xs)
  (if (null? xs)
      (ok '())
      (result-bind (pred (car xs))
                   (lambda (keep?)
                     (result-bind (result-filter pred (cdr xs))
                                  (lambda (rest)
                                    (ok (if keep?
                                            (cons (car xs) rest)
                                            rest))))))))

;;; ====
;;; Validation (Applicative Error Accumulation)
;;; ====
;;;
;;; Unlike result-bind which short-circuits on first error,
;;; validation accumulates ALL errors.

;;; validation-ok : α → (Validation (List ε) α)
;;; Success case for validation.
(define (validation-ok x) (list 'validation-ok x))

;;; validation-err : ε → (Validation (List ε) α)
;;; Single error case for validation.
(define (validation-err e) (list 'validation-err (list e)))

;;; validation-errs : (List ε) → (Validation (List ε) α)
;;; Multiple errors case for validation.
(define (validation-errs es) (list 'validation-err es))

;;; validation-ok? : (Validation ε α) → Bool
(define (validation-ok? v)
  (and (pair? v) (eq? (car v) 'validation-ok)))

;;; validation-err? : (Validation ε α) → Bool
(define (validation-err? v)
  (and (pair? v) (eq? (car v) 'validation-err)))

;;; validation-value : (Validation ε α) → α
(define (validation-value v) (cadr v))

;;; validation-errors : (Validation ε α) → (List ε)
(define (validation-errors v) (cadr v))

;;; validation-ap : (Validation ε (α → β)) → (Validation ε α) → (Validation ε β)
;;; Applicative that accumulates errors from both sides.
(define (validation-ap vf va)
  (cond
    [(and (validation-ok? vf) (validation-ok? va))
     (validation-ok ((validation-value vf) (validation-value va)))]
    [(and (validation-err? vf) (validation-err? va))
     (validation-errs (append (validation-errors vf) (validation-errors va)))]
    [(validation-err? vf) vf]
    [else va]))

;;; validation-map : (α → β) → (Validation ε α) → (Validation ε β)
(define (validation-map f v)
  (if (validation-ok? v)
      (validation-ok (f (validation-value v)))
      v))

;;; validation->result : (Validation ε α) → (Result α (List ε))
;;; Convert validation to result.
(define (validation->result v)
  (if (validation-ok? v)
      (ok (validation-value v))
      (list 'error (validation-errors v))))

;;; result->validation : (Result α ε) → (Validation (List ε) α)
;;; Convert result to validation.
(define (result->validation r)
  (if (ok? r)
      (validation-ok (unwrap-ok r))
      (validation-err (cdr r))))
