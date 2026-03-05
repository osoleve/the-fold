;;; lattice/fp/meta/result.ss — Result Type Operations
;;; @module result
;;; @requires prelude combinators

(require 'prelude)
(require 'combinators)

(doc 'module 'result)
(doc 'description "Result Type Operations — Consolidates the Result type pattern used throughout The Fold. Re-exports core operations from prelude.ss and adds utilities. Result α ε = (ok α) | (error ε). Core Result operations are in core/base/prelude.ss.")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'section 're-exports)
(doc 'note "Constructors: (list 'ok value) — success result, (list 'error . rest) — failure result. Predicates: ok?, error?. Accessors: unwrap-ok, unwrap-error. Monadic: result-map, result-bind, result-sequence (all from prelude)")

(doc 'section 'result-constructors)

(define (ok value)
  (doc 'export #t)
  (doc 'type '(-> α Result-α-ε))
  (doc 'description "Wrap a value as success")
  (list 'ok value))

(define (err e)
  (doc 'export #t)
  (doc 'type '(-> ε Result-α-ε))
  (doc 'description "Wrap an error value - alternative to bare (error ...) for clarity")
  (list 'error e))

(doc 'section 'result-operations)

(define (result-fold on-ok on-error result)
  (doc 'export #t)
  (doc 'type '(-> (-> α β) (-> ε β) Result-α-ε β))
  (doc 'description "Pattern match on Result, applying appropriate function")
  (doc 'note "on-error receives (cdr result) - the full error payload after 'error tag. For (error 'tag \"msg\"), on-error receives ('tag \"msg\")")
  (if (ok? result)
      (on-ok (unwrap-ok result))
      (on-error (cdr result))))

(define (result-default default result)
  (doc 'export #t)
  (doc 'type '(-> α Result-α-ε α))
  (doc 'description "Extract value or return default on error")
  (if (ok? result)
      (unwrap-ok result)
      default))

(define (result-or r1 r2)
  (doc 'export #t)
  (doc 'type '(-> Result-α-ε Result-α-ε Result-α-ε))
  (doc 'description "Return first ok result, or last error if both fail")
  (if (ok? r1) r1 r2))

(define (result-and r1 r2)
  (doc 'export #t)
  (doc 'type '(-> Result-α-ε Result-β-ε Result-β-ε))
  (doc 'description "Return second if first is ok, otherwise first error")
  (if (ok? r1) r2 r1))

(define (result-map-error f result)
  (doc 'export #t)
  (doc 'type '(-> (-> ε₁ ε₂) Result-α-ε₁ Result-α-ε₂))
  (doc 'description "Transform the error value, leaving ok unchanged")
  (doc 'note "f receives and should return (cdr result) - the full error payload. Uses cons to preserve flat error structure: (error tag details...)")
  (if (ok? result)
      result
      (cons 'error (f (cdr result)))))

(define (result-bimap f-ok f-err result)
  (doc 'export #t)
  (doc 'type '(-> (-> α β) (-> ε₁ ε₂) Result-α-ε₁ Result-β-ε₂))
  (doc 'description "Transform both success and error cases")
  (doc 'note "f-err receives and should return (cdr result) - the full error payload. Uses cons to preserve flat error structure: (error tag details...)")
  (if (ok? result)
      (ok (f-ok (unwrap-ok result)))
      (cons 'error (f-err (cdr result)))))

(doc 'section 'exception-integration)

(define (result-catch thunk)
  (doc 'export #t)
  (doc 'type '(-> (-> α) Result-α-Condition))
  (doc 'description "Execute thunk, catching exceptions as error results")
  (doc 'returns "(ok value) on success, (error condition) on exception")
  (guard (e [else (list 'error e)])
         (ok (thunk))))

(define (result-catch/handler thunk handler)
  (doc 'export #t)
  (doc 'type '(-> (-> α) (-> Condition ε) Result-α-ε))
  (doc 'description "Execute thunk with custom error transformation")
  (guard (e [else (list 'error (handler e))])
         (ok (thunk))))

(define (result-try thunk default)
  (doc 'export #t)
  (doc 'type '(-> (-> α) α α))
  (doc 'description "Execute thunk, returning default on any exception")
  (doc 'note "Simpler than result-catch when you just want a fallback")
  (guard (e [else default])
         (thunk)))

(doc 'section 'assertions)

(define (assert-ok! result)
  (doc 'export #t)
  (doc 'type '(-> Result-α-ε α))
  (doc 'description "Unwrap ok value or raise error with message")
  (doc 'note "Use sparingly - prefer result-bind for chaining")
  (if (ok? result)
      (unwrap-ok result)
      (error 'assert-ok! "Expected ok result" (cdr result))))

(define (assert-ok/msg! result msg)
  (doc 'export #t)
  (doc 'type '(-> Result-α-ε String α))
  (doc 'description "Unwrap with custom error message")
  (if (ok? result)
      (unwrap-ok result)
      (error 'assert-ok! msg (cdr result))))

(doc 'section 'conversions)

(define (result->maybe result)
  (doc 'export #t)
  (doc 'type '(-> Result-α-ε Maybe-α))
  (doc 'description "Convert Result to Maybe, discarding error info")
  (if (ok? result)
      (just (unwrap-ok result))
      nothing))

(define (maybe->result m err-val)
  (doc 'export #t)
  (doc 'type '(-> Maybe-α ε Result-α-ε))
  (doc 'description "Convert Maybe to Result, using provided error for Nothing")
  (if (just? m)
      (ok (from-just m))
      (list 'error err-val)))

(define (result->either result)
  (doc 'export #t)
  (doc 'type '(-> Result-α-ε Either-ε-α))
  (doc 'description "Convert Result to Either (error on left, value on right)")
  (if (ok? result)
      (right (unwrap-ok result))
      (left (cdr result))))

(define (either->result e)
  (doc 'export #t)
  (doc 'type '(-> Either-ε-α Result-α-ε))
  (doc 'description "Convert Either to Result")
  (if (right? e)
      (ok (from-right e))
      (list 'error (from-left e))))

(doc 'section 'applicative-operations)

(define (result-ap rf ra)
  (doc 'export #t)
  (doc 'type '(-> Result-(-> α β)-ε Result-α-ε Result-β-ε))
  (doc 'description "Applicative apply for Result")
  (result-bind rf
               (lambda (f)
                 (result-map f ra))))

(define (result-lift2 f ra rb)
  (doc 'export #t)
  (doc 'type '(-> (-> α β γ) Result-α-ε Result-β-ε Result-γ-ε))
  (doc 'description "Lift a binary function to work on Results")
  (result-bind ra
               (lambda (a)
                 (result-map (lambda (b) (f a b)) rb))))

(doc 'section 'traversal)

(define (result-traverse f xs)
  (doc 'export #t)
  (doc 'type '(-> (-> α Result-β-ε) (List α) Result-(List β)-ε))
  (doc 'description "Map a result-producing function over a list, collecting results")
  (result-sequence (map f xs)))

(define (result-filter pred xs)
  (doc 'export #t)
  (doc 'type '(-> (-> α Result-Bool-ε) (List α) Result-(List α)-ε))
  (doc 'description "Filter list using result-producing predicate")
  (if (null? xs)
      (ok '())
      (result-bind (pred (car xs))
                   (lambda (keep?)
                     (result-bind (result-filter pred (cdr xs))
                                  (lambda (rest)
                                    (ok (if keep?
                                            (cons (car xs) rest)
                                            rest))))))))

(doc 'section 'validation)
(doc 'note "Unlike result-bind which short-circuits on first error, validation accumulates ALL errors")

(define (validation-ok x)
  (doc 'export #t)
  (doc 'type '(-> α Validation-(List ε)-α))
  (doc 'description "Success case for validation")
  (list 'validation-ok x))

(define (validation-err e)
  (doc 'export #t)
  (doc 'type '(-> ε Validation-(List ε)-α))
  (doc 'description "Single error case for validation")
  (list 'validation-err (list e)))

(define (validation-errs es)
  (doc 'export #t)
  (doc 'type '(-> (List ε) Validation-(List ε)-α))
  (doc 'description "Multiple errors case for validation")
  (list 'validation-err es))

(define (validation-ok? v)
  (doc 'export #t)
  (doc 'type '(-> Validation-ε-α Bool))
  (and (pair? v) (eq? (car v) 'validation-ok)))

(define (validation-err? v)
  (doc 'export #t)
  (doc 'type '(-> Validation-ε-α Bool))
  (and (pair? v) (eq? (car v) 'validation-err)))

(define (validation-value v)
  (doc 'export #t)
  (doc 'type '(-> Validation-ε-α α))
  (cadr v))

(define (validation-errors v)
  (doc 'export #t)
  (doc 'type '(-> Validation-ε-α (List ε)))
  (cadr v))

(define (validation-ap vf va)
  (doc 'export #t)
  (doc 'type '(-> Validation-ε-(-> α β) Validation-ε-α Validation-ε-β))
  (doc 'description "Applicative that accumulates errors from both sides")
  (cond
    [(and (validation-ok? vf) (validation-ok? va))
     (validation-ok ((validation-value vf) (validation-value va)))]
    [(and (validation-err? vf) (validation-err? va))
     (validation-errs (append (validation-errors vf) (validation-errors va)))]
    [(validation-err? vf) vf]
    [else va]))

(define (validation-map f v)
  (doc 'export #t)
  (doc 'type '(-> (-> α β) Validation-ε-α Validation-ε-β))
  (if (validation-ok? v)
      (validation-ok (f (validation-value v)))
      v))

(define (validation->result v)
  (doc 'export #t)
  (doc 'type '(-> Validation-ε-α Result-α-(List ε)))
  (doc 'description "Convert validation to result")
  (if (validation-ok? v)
      (ok (validation-value v))
      (list 'error (validation-errors v))))

(define (result->validation r)
  (doc 'export #t)
  (doc 'type '(-> Result-α-ε Validation-(List ε)-α))
  (doc 'description "Convert result to validation")
  (if (ok? r)
      (validation-ok (unwrap-ok r))
      (validation-err (cdr r))))
