;;; fabric/stitches/fp/reader.ss — Reader Monad
;;;
;;; The Reader monad threads a read-only environment through computation.
;;; Useful for dependency injection, configuration access, and context passing.
;;;
;;; Reader r a = r -> a
;;;
;;; A Reader computation takes an environment and produces a value.
;;; Unlike State, Reader cannot modify the environment.
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Features:
;;;   - Core Reader operations (run-reader, ask, asks)
;;;   - Monad operations (reader-pure, reader-bind, reader-map)
;;;   - Local environment modification (local)
;;;   - Reader combinators
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - fp/combinators.ss

(load "core/prelude.ss")
(load "core/fp/meta/combinators.ss")

;;; ============================================================
;;; Reader Representation
;;; ============================================================
;;;
;;; Reader r a is represented as a tagged function:
;;;   ('reader . (r -> a))
;;;
;;; The function takes an environment and returns a value.

;;; make-reader : (r -> a) -> Reader r a
;;; Create a Reader computation from a function.
(define (make-reader run-fn)
  (cons 'reader run-fn))

;;; reader? : Any -> Boolean
(define (reader? x)
  (and (pair? x) (eq? (car x) 'reader)))

;;; reader-fn : Reader r a -> (r -> a)
;;; Extract the underlying function.
(define (reader-fn rd)
  (cdr rd))

;;; ============================================================
;;; Running Reader Computations
;;; ============================================================

;;; run-reader : Reader r a -> r -> a
;;; Run a Reader computation with an environment.
(define (run-reader rd env)
  ((reader-fn rd) env))

;;; ============================================================
;;; Core Reader Operations
;;; ============================================================

;;; ask : Reader r r
;;; Get the current environment.
(define reader-ask
  (make-reader (lambda (env) env)))

;;; asks : (r -> a) -> Reader r a
;;; Get a value derived from the environment.
(define (reader-asks f)
  (make-reader (lambda (env) (f env))))

;;; local : (r -> r) -> Reader r a -> Reader r a
;;; Run a computation with a modified environment.
(define (reader-local f rd)
  (make-reader (lambda (env) (run-reader rd (f env)))))

;;; ============================================================
;;; Monad Operations
;;; ============================================================

;;; reader-pure : a -> Reader r a
;;; Lift a value into Reader (return/pure).
(define (reader-pure x)
  (make-reader (lambda (env) x)))

;;; reader-bind : Reader r a -> (a -> Reader r b) -> Reader r b
;;; Sequence two Reader computations (>>=).
(define (reader-bind rd f)
  (make-reader
   (lambda (env)
           (let ([a (run-reader rd env)])
                (run-reader (f a) env)))))

;;; reader-then : Reader r a -> Reader r b -> Reader r b
;;; Sequence two Reader computations, discarding the first result (>>).
(define (reader-then rd1 rd2)
  (reader-bind rd1 (lambda (_) rd2)))

;;; reader-map : (a -> b) -> Reader r a -> Reader r b
;;; Map a function over the result of a Reader computation.
(define (reader-map f rd)
  (reader-bind rd (lambda (a) (reader-pure (f a)))))

;;; reader-ap : Reader r (a -> b) -> Reader r a -> Reader r b
;;; Apply a Reader-wrapped function to a Reader-wrapped value.
(define (reader-ap rd-f rd-a)
  (reader-bind rd-f
               (lambda (f)
                       (reader-bind rd-a
                                    (lambda (a)
                                            (reader-pure (f a)))))))

;;; ============================================================
;;; Reader Combinators
;;; ============================================================

;;; reader-sequence : (List (Reader r a)) -> Reader r (List a)
;;; Sequence a list of Reader computations, collecting results.
(define (reader-sequence readers)
  (if (null? readers)
      (reader-pure '())
      (reader-bind (car readers)
                   (lambda (x)
                           (reader-bind (reader-sequence (cdr readers))
                                        (lambda (xs)
                                                (reader-pure (cons x xs))))))))

;;; reader-map-m : (a -> Reader r b) -> (List a) -> Reader r (List b)
;;; Map a Reader-returning function over a list.
(define (reader-map-m f lst)
  (reader-sequence (map f lst)))

;;; reader-for-each : (List a) -> (a -> Reader r ()) -> Reader r ()
;;; Execute a Reader action for each element (for side effects).
(define (reader-for-each lst f)
  (if (null? lst)
      (reader-pure '())
      (reader-then (f (car lst))
                   (reader-for-each (cdr lst) f))))

;;; reader-when : Boolean -> Reader r () -> Reader r ()
;;; Execute Reader action only when condition is true.
(define (reader-when condition action)
  (if condition
      action
      (reader-pure '())))

;;; reader-unless : Boolean -> Reader r () -> Reader r ()
;;; Execute Reader action only when condition is false.
(define (reader-unless condition action)
  (reader-when (not condition) action))

;;; ============================================================
;;; Alist Environment Helpers
;;; ============================================================
;;;
;;; Common pattern: environment is an alist with named dependencies.

;;; reader-get-key : Symbol -> Reader (Alist Symbol a) (Maybe a)
;;; Get a value by key from an alist environment.
(define (reader-get-key key)
  (reader-asks (lambda (env)
                       (let ([pair (assoc key env)])
                            (if pair (just (cdr pair)) nothing)))))

;;; reader-get-key-default : Symbol -> a -> Reader (Alist Symbol a) a
;;; Get a value by key with a default.
(define (reader-get-key-default key default)
  (reader-asks (lambda (env)
                       (let ([pair (assoc key env)])
                            (if pair (cdr pair) default)))))

;;; reader-require-key : Symbol -> Reader (Alist Symbol a) a
;;; Get a required value by key (partial - errors if missing).
(define (reader-require-key key)
  (reader-asks (lambda (env)
                       (let ([pair (assoc key env)])
                            (if pair
                                (cdr pair)
                                (error 'reader-require-key
                                       (format "Missing required key: ~a" key)))))))

;;; with-key : Symbol -> a -> Reader (Alist Symbol a) b -> Reader (Alist Symbol a) b
;;; Run a Reader with an additional key-value pair in the environment.
(define (reader-with-key key val rd)
  (reader-local (lambda (env) (cons (cons key val) env)) rd))

;;; with-keys : (Alist Symbol a) -> Reader (Alist Symbol a) b -> Reader (Alist Symbol a) b
;;; Run a Reader with additional key-value pairs in the environment.
(define (reader-with-keys kvs rd)
  (reader-local (lambda (env) (append kvs env)) rd))

;;; ============================================================
;;; Scope Helpers
;;; ============================================================

;;; reader-scope : Symbol -> (r -> r2) -> Reader r2 a -> Reader r a
;;; Focus on a sub-part of the environment.
(define (reader-scope name extractor rd)
  (make-reader
   (lambda (env)
           (run-reader rd (extractor env)))))

;;; reader-with-scope : Symbol -> Reader r a -> Reader (Alist Symbol r) a
;;; Extract a sub-environment by key and run Reader in that scope.
(define (reader-with-scope scope-key rd)
  (reader-bind (reader-require-key scope-key)
               (lambda (sub-env)
                       (make-reader (lambda (_) (run-reader rd sub-env))))))

;;; ============================================================
;;; Lifting Functions
;;; ============================================================

;;; reader-lift : (a -> b) -> (Reader r a -> Reader r b)
;;; Lift a unary function to work with Reader.
(define (reader-lift f)
  (lambda (rd) (reader-map f rd)))

;;; reader-lift2 : (a -> b -> c) -> (Reader r a -> Reader r b -> Reader r c)
;;; Lift a binary function to work with Reader.
(define (reader-lift2 f)
  (lambda (rd1 rd2)
          (reader-bind rd1
                       (lambda (a)
                               (reader-bind rd2
                                            (lambda (b)
                                                    (reader-pure (f a b))))))))

;;; reader-lift3 : (a -> b -> c -> d) -> ...
;;; Lift a ternary function to work with Reader.
(define (reader-lift3 f)
  (lambda (rd1 rd2 rd3)
          (reader-bind rd1
                       (lambda (a)
                               (reader-bind rd2
                                            (lambda (b)
                                                    (reader-bind rd3
                                                                 (lambda (c)
                                                                         (reader-pure (f a b c))))))))))

;;; ============================================================
;;; Combining with Other Monads
;;; ============================================================

;;; reader-maybe : Reader r (Maybe a) -> b -> (a -> b) -> Reader r b
;;; Pattern match on Reader of Maybe.
(define (reader-maybe rd on-nothing on-just)
  (reader-map (lambda (m)
                      (if (nothing? m)
                          on-nothing
                          (on-just (from-just m))))
              rd))

;;; reader-either : Reader r (Either e a) -> (e -> b) -> (a -> b) -> Reader r b
;;; Pattern match on Reader of Either.
(define (reader-either rd on-left on-right)
  (reader-map (lambda (e)
                      (if (left? e)
                          (on-left (from-left e))
                          (on-right (from-right e))))
              rd))

;;; ============================================================
;;; Dependency Injection Pattern
;;; ============================================================
;;;
;;; Common pattern for dependency injection:
;;;
;;; 1. Define an environment type (alist or record)
;;; 2. Create accessor readers with reader-asks
;;; 3. Compose computations with reader-bind
;;; 4. Run with run-reader providing dependencies

;;; Example environment accessors:
;;; (define get-database (reader-get-key 'database))
;;; (define get-config (reader-get-key 'config))
;;; (define get-logger (reader-get-key 'logger))

;;; ============================================================
;;; Example Usage (for documentation)
;;; ============================================================
;;;
;;; ;; Define environment accessors
;;; (define get-host (reader-asks (lambda (env) (cdr (assoc 'host env)))))
;;; (define get-port (reader-asks (lambda (env) (cdr (assoc 'port env)))))
;;;
;;; ;; Compose to build URL
;;; (define make-url
;;;   (reader-bind get-host
;;;                (lambda (host)
;;;                  (reader-bind get-port
;;;                               (lambda (port)
;;;                                 (reader-pure
;;;                                  (string-append "http://" host ":" (number->string port))))))))
;;;
;;; ;; Run with environment
;;; (run-reader make-url '((host . "localhost") (port . 8080)))
;;; ; => "http://localhost:8080"
;;;
;;; ;; Using local to modify environment temporarily
;;; (run-reader (reader-local (lambda (env) (cons '(port . 3000) env))
;;;                           make-url)
;;;             '((host . "localhost") (port . 8080)))
;;; ; => "http://localhost:3000"
