(load "lattice/fp/category/monad-derivation.ss")

(doc 'module 'common-monads)
(doc 'description "Common Monads - Direct Construction

While monads can be derived from adjunctions (see monad-derivation.ss),
some monads are more naturally defined directly. This module provides:

  - Maybe monad: computations that may fail
  - Either monad: computations with typed errors
  - Reader monad: computations with read-only environment

Each monad is provided as a MonadOps record for use with the
Kleisli category and law verification machinery.

Representation conventions (matching lattice/fp/meta/combinators.ss):
  - Maybe: #f for Nothing, (list 'just x) for Just x
  - Either: (list 'left e) for Left, (list 'right x) for Right
  - Reader: functions E → A")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'section 'maybe-monad)
(doc 'description "Maybe Monad

The Maybe monad models computations that may fail silently.
  - Just x: successful computation with value x
  - Nothing: failed computation

Representation: #f for Nothing, (list 'just x) for Just x
This tagged representation correctly distinguishes (just #f) from nothing.

Monad operations:
  - return x = Just x
  - bind (Just x) f = f x
  - bind Nothing f = Nothing
  - join (Just (Just x)) = Just x
  - join Nothing = Nothing
  - join (Just Nothing) = Nothing")

;;; maybe-nothing : Maybe a
;;; The Nothing value (failure)
(define maybe-nothing #f)

;;; maybe-just : a → Maybe a
;;; Wrap a value in Just
(define (maybe-just x)
  (doc 'type '(-> a (Maybe a)))
  (list 'just x))

;;; maybe-just? : Maybe a → Boolean
;;; Is this a Just value?
(define (maybe-just? ma)
  (doc 'type '(-> (Maybe a) Boolean))
  (and (pair? ma) (eq? (car ma) 'just)))

;;; maybe-nothing? : Maybe a → Boolean
;;; Is this Nothing?
(define (maybe-nothing? ma)
  (doc 'type '(-> (Maybe a) Boolean))
  (not ma))

;;; maybe-from-just : Maybe a → a
;;; Extract value (undefined for Nothing)
(define (maybe-from-just ma)
  (doc 'type '(-> (Maybe a) a))
  (cadr ma))

;;; maybe-return : a → Maybe a
(define (maybe-return x)
  (doc 'type '(-> a (Maybe a)))
  (maybe-just x))

;;; maybe-bind : Maybe a → (a → Maybe b) → Maybe b
(define (maybe-bind ma f)
  (doc 'type '(-> (Maybe a) (-> a (Maybe b)) (Maybe b)))
  (if (maybe-just? ma)
      (f (maybe-from-just ma))
      maybe-nothing))

;;; maybe-fmap : (a → b) → Maybe a → Maybe b
(define (maybe-fmap f ma)
  (doc 'type '(-> (-> a b) (Maybe a) (Maybe b)))
  (if (maybe-just? ma)
      (maybe-just (f (maybe-from-just ma)))
      maybe-nothing))

;;; maybe-join : Maybe (Maybe a) → Maybe a
(define (maybe-join mma)
  (doc 'type '(-> (Maybe (Maybe a)) (Maybe a)))
  (if (maybe-just? mma)
      (maybe-from-just mma)  ; unwrap outer Just, inner is already Maybe a
      maybe-nothing))

;;; monad-maybe : MonadOps
;;; The Maybe monad packaged as MonadOps
(define monad-maybe
  (make-monad-ops 'maybe
                  maybe-return
                  maybe-fmap
                  maybe-join
                  maybe-bind))

;;; maybe? : Any → Boolean
;;; Test if value is a valid Maybe
(define (maybe? x)
  (doc 'type '(-> Any Boolean))
  (or (not x) (maybe-just? x)))

;;; maybe-from-default : a → Maybe a → a
;;; Extract value with default fallback
(define (maybe-from-default default ma)
  (doc 'type '(-> a (Maybe a) a))
  (if (maybe-just? ma)
      (maybe-from-just ma)
      default))

;;; maybe-or-else : Maybe a → (-> Maybe a) → Maybe a
;;; Try first, if Nothing then try alternative
(define (maybe-or-else ma alt)
  (doc 'type '(-> (Maybe a) (-> (Maybe a)) (Maybe a)))
  (if (maybe-just? ma) ma (alt)))

;;; maybe-map : (a → b) → Maybe a → Maybe b
;;; Alias for maybe-fmap (consistency with combinators.ss)
(define maybe-map maybe-fmap)

(doc 'section 'either-monad)
(doc 'description "Either Monad (Result)

The Either monad models computations that may fail with an error value.
  - Right x: successful computation with value x
  - Left e: failed computation with error e

This is the classic 'Result' type from Rust/ML traditions.

Representation: (list 'left e) or (list 'right x) - matching combinators.ss

Monad operations (right-biased):
  - return x = Right x
  - bind (Right x) f = f x
  - bind (Left e) f = Left e
  - The Left case short-circuits (propagates errors)")

;;; make-left : e → Either e a
(define (make-left err)
  (doc 'type '(-> e (Either e a)))
  (list 'left err))

;;; make-right : a → Either e a
(define (make-right val)
  (doc 'type '(-> a (Either e a)))
  (list 'right val))

;;; either? : Any → Boolean
(define (either? x)
  (doc 'type '(-> Any Boolean))
  (and (pair? x)
       (or (eq? (car x) 'left)
           (eq? (car x) 'right))))

;;; either-left? : Either e a → Boolean
(define (either-left? ea)
  (doc 'type '(-> (Either e a) Boolean))
  (and (pair? ea) (eq? (car ea) 'left)))

;;; either-right? : Either e a → Boolean
(define (either-right? ea)
  (doc 'type '(-> (Either e a) Boolean))
  (and (pair? ea) (eq? (car ea) 'right)))

;;; either-from-left : Either e a → e
;;; Extract Left value (undefined for Right)
(define (either-from-left ea)
  (doc 'type '(-> (Either e a) e))
  (cadr ea))

;;; either-from-right : Either e a → a
;;; Extract Right value (undefined for Left)
(define (either-from-right ea)
  (doc 'type '(-> (Either e a) a))
  (cadr ea))

;;; either-value : Either e a → (Union e a)
;;; Extract the wrapped value (either error or success)
(define (either-value ea)
  (doc 'type '(-> (Either e a) (Union e a)))
  (if (either? ea) (cadr ea) ea))

;;; either-return : a → Either e a
(define (either-return x)
  (doc 'type '(-> a (Either e a)))
  (make-right x))

;;; either-bind : Either e a → (a → Either e b) → Either e b
(define (either-bind ea f)
  (doc 'type '(-> (Either e a) (-> a (Either e b)) (Either e b)))
  (cond
    [(either-right? ea) (f (either-from-right ea))]
    [(either-left? ea) ea]  ; propagate error
    [else (error 'either-bind "Invalid Either value")]))

;;; either-fmap : (a → b) → Either e a → Either e b
(define (either-fmap f ea)
  (doc 'type '(-> (-> a b) (Either e a) (Either e b)))
  (cond
    [(either-right? ea) (make-right (f (either-from-right ea)))]
    [(either-left? ea) ea]
    [else (error 'either-fmap "Invalid Either value")]))

;;; either-join : Either e (Either e a) → Either e a
(define (either-join eea)
  (doc 'type '(-> (Either e (Either e a)) (Either e a)))
  (cond
    [(either-right? eea) (either-from-right eea)]  ; unwrap outer Right
    [(either-left? eea) eea]                        ; propagate error
    [else (error 'either-join "Invalid Either value")]))

;;; monad-either : MonadOps
;;; The Either monad packaged as MonadOps (right-biased)
(define monad-either
  (make-monad-ops 'either
                  either-return
                  either-fmap
                  either-join
                  either-bind))

;;; either-from-default : a → Either e a → a
;;; Extract Right value with default for Left
(define (either-from-default default ea)
  (doc 'type '(-> a (Either e a) a))
  (if (either-right? ea)
      (either-from-right ea)
      default))

;;; either-map-left : (e → e') → Either e a → Either e' a
;;; Transform the error type
(define (either-map-left f ea)
  (doc 'type '(-> (-> e e2) (Either e a) (Either e2 a)))
  (cond
    [(either-left? ea) (make-left (f (either-from-left ea)))]
    [(either-right? ea) ea]
    [else (error 'either-map-left "Invalid Either value")]))

;;; either-fold : (e → r) → (a → r) → Either e a → r
;;; Eliminate Either by providing handlers for both cases
(define (either-fold on-left on-right ea)
  (doc 'type '(-> (-> e r) (-> a r) (Either e a) r))
  (cond
    [(either-left? ea) (on-left (either-from-left ea))]
    [(either-right? ea) (on-right (either-from-right ea))]
    [else (error 'either-fold "Invalid Either value")]))

(doc 'section 'reader-monad)
(doc 'description "Reader Monad

The Reader monad models computations with a read-only environment.
  - Reader E A = functions E → A
  - Pure computations that can read from a shared environment

Representation: functions E → A (same as adjunction-derived)

Monad operations:
  - return x = λe. x (constant function)
  - bind m f = λe. f (m e) e (thread environment through)
  - ask = λe. e (read the environment)

Unlike State, the environment is immutable - no 'put' operation.")

;;; reader-return : a → Reader e a
(define (reader-return x)
  (doc 'type '(-> a (-> e a)))
  (lambda (env) x))

;;; reader-bind : Reader e a → (a → Reader e b) → Reader e b
(define (reader-bind ra f)
  (doc 'type '(-> (-> e a) (-> a (-> e b)) (-> e b)))
  (lambda (env)
    ((f (ra env)) env)))

;;; reader-fmap : (a → b) → Reader e a → Reader e b
(define (reader-fmap f ra)
  (doc 'type '(-> (-> a b) (-> e a) (-> e b)))
  (lambda (env)
    (f (ra env))))

;;; reader-join : Reader e (Reader e a) → Reader e a
(define (reader-join rra)
  (doc 'type '(-> (-> e (-> e a)) (-> e a)))
  (lambda (env)
    ((rra env) env)))

;;; monad-reader : MonadOps
;;; The Reader monad packaged as MonadOps
;;; Note: Reader E A = E → A, parametric in E
(define monad-reader
  (make-monad-ops 'reader
                  reader-return
                  reader-fmap
                  reader-join
                  reader-bind))

;;; reader-ask : Reader e e
;;; Read the entire environment
(define reader-ask
  (lambda (env) env))

;;; reader-asks : (e → a) → Reader e a
;;; Read a computed projection of the environment
(define (reader-asks f)
  (doc 'type '(-> (-> e a) (-> e a)))
  (lambda (env) (f env)))

;;; reader-local : (e → e) → Reader e a → Reader e a
;;; Run a computation with a modified environment
(define (reader-local modify ra)
  (doc 'type '(-> (-> e e) (-> e a) (-> e a)))
  (lambda (env)
    (ra (modify env))))

;;; run-reader : Reader e a → e → a
;;; Execute a Reader computation with an environment
(define (run-reader ra env)
  (doc 'type '(-> (-> e a) e a))
  (ra env))

(doc 'section 'exports)
(doc 'description "Exports

Maybe Monad:
  maybe-return, maybe-bind, maybe-fmap, maybe-join
  monad-maybe (MonadOps)
  maybe-nothing, maybe-just, maybe-from-just
  maybe?, maybe-just?, maybe-nothing?
  maybe-from-default, maybe-or-else, maybe-map

Either Monad:
  make-left, make-right
  either?, either-left?, either-right?
  either-from-left, either-from-right, either-value
  either-return, either-bind, either-fmap, either-join
  monad-either (MonadOps)
  either-from-default, either-map-left, either-fold

Reader Monad:
  reader-return, reader-bind, reader-fmap, reader-join
  monad-reader (MonadOps)
  reader-ask, reader-asks, reader-local, run-reader")
