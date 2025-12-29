;;; fabric/stitches/fp/writer.ss — Writer Monad
;;;
;;; The Writer monad threads a write-only accumulator through computation.
;;; Useful for logging, audit trails, and accumulating output alongside values.
;;;
;;; Writer w a = (a, w)
;;;
;;; A Writer computation produces a value along with accumulated output.
;;; The output must form a monoid (has identity and associative combine).
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Features:
;;;   - Core Writer operations (run-writer, tell, listen, pass)
;;;   - Monad operations (writer-pure, writer-bind, writer-map)
;;;   - Censor for transforming output
;;;   - Writer combinators
;;;   - Common monoids (list, string, sum, product)
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - fp/combinators.ss

(load "fabric/stitches/prelude.ss")
(load "fabric/stitches/fp/combinators.ss")

;;; ============================================================
;;; Monoid Concept
;;; ============================================================
;;;
;;; A monoid has:
;;;   - mempty: identity element
;;;   - mappend: associative binary operation
;;;
;;; We represent monoids as a pair: (mempty . mappend)

;;; make-monoid : w -> (w -> w -> w) -> Monoid w
(define (make-monoid empty append-fn)
  (cons empty append-fn))

;;; monoid-empty : Monoid w -> w
(define (monoid-empty m) (car m))

;;; monoid-append : Monoid w -> w -> w -> w
(define (monoid-append m) (cdr m))

;;; ============================================================
;;; Common Monoids
;;; ============================================================

;;; list-monoid : Monoid (List a)
;;; Identity: '(), Operation: append
(define list-monoid
  (make-monoid '() append))

;;; string-monoid : Monoid String
;;; Identity: "", Operation: string-append
(define string-monoid
  (make-monoid "" string-append))

;;; sum-monoid : Monoid Number
;;; Identity: 0, Operation: +
(define sum-monoid
  (make-monoid 0 +))

;;; product-monoid : Monoid Number
;;; Identity: 1, Operation: *
(define product-monoid
  (make-monoid 1 *))

;;; and-monoid : Monoid Boolean
;;; Identity: #t, Operation: and
(define and-monoid
  (make-monoid #t (lambda (a b) (and a b))))

;;; or-monoid : Monoid Boolean
;;; Identity: #f, Operation: or
(define or-monoid
  (make-monoid #f (lambda (a b) (or a b))))

;;; first-monoid : Monoid (Maybe a)
;;; Keep first Just value
(define first-monoid
  (make-monoid nothing
               (lambda (a b)
                       (if (just? a) a b))))

;;; last-monoid : Monoid (Maybe a)
;;; Keep last Just value
(define last-monoid
  (make-monoid nothing
               (lambda (a b)
                       (if (just? b) b a))))

;;; ============================================================
;;; Writer Representation
;;; ============================================================
;;;
;;; Writer w a is represented as:
;;;   ('writer monoid value output)
;;;
;;; Where:
;;;   - monoid: the monoid used for output
;;;   - value: the computed value (a)
;;;   - output: the accumulated output (w)

;;; make-writer : Monoid w -> a -> w -> Writer w a
;;; Create a Writer computation.
(define (make-writer monoid value output)
  (list 'writer monoid value output))

;;; writer? : Any -> Boolean
(define (writer? x)
  (and (pair? x) (eq? (car x) 'writer)))

;;; writer-monoid : Writer w a -> Monoid w
(define (writer-monoid wr) (list-ref wr 1))

;;; writer-value : Writer w a -> a
(define (writer-value wr) (list-ref wr 2))

;;; writer-output : Writer w a -> w
(define (writer-output wr) (list-ref wr 3))

;;; ============================================================
;;; Running Writer Computations
;;; ============================================================

;;; run-writer : Writer w a -> (a . w)
;;; Extract value and output as a pair.
(define (run-writer wr)
  (cons (writer-value wr) (writer-output wr)))

;;; exec-writer : Writer w a -> w
;;; Extract only the output.
(define (exec-writer wr)
  (writer-output wr))

;;; eval-writer : Writer w a -> a
;;; Extract only the value.
(define (eval-writer wr)
  (writer-value wr))

;;; ============================================================
;;; Core Writer Operations
;;; ============================================================

;;; writer-pure : Monoid w -> a -> Writer w a
;;; Lift a value into Writer with empty output.
(define (writer-pure monoid x)
  (make-writer monoid x (monoid-empty monoid)))

;;; tell : Monoid w -> w -> Writer w ()
;;; Output a value without producing a result.
(define (writer-tell monoid output)
  (make-writer monoid '() output))

;;; listen : Writer w a -> Writer w (a . w)
;;; Expose the output alongside the value.
(define (writer-listen wr)
  (make-writer (writer-monoid wr)
               (cons (writer-value wr) (writer-output wr))
               (writer-output wr)))

;;; listens : (w -> b) -> Writer w a -> Writer w (a . b)
;;; Expose transformed output alongside the value.
(define (writer-listens f wr)
  (make-writer (writer-monoid wr)
               (cons (writer-value wr) (f (writer-output wr)))
               (writer-output wr)))

;;; pass : Writer w (a . (w -> w)) -> Writer w a
;;; Apply a function to the output.
(define (writer-pass wr)
  (let* ([pair (writer-value wr)]
         [a (car pair)]
         [f (cdr pair)]
         [w (writer-output wr)])
        (make-writer (writer-monoid wr) a (f w))))

;;; censor : (w -> w) -> Writer w a -> Writer w a
;;; Transform the output.
(define (writer-censor f wr)
  (make-writer (writer-monoid wr)
               (writer-value wr)
               (f (writer-output wr))))

;;; ============================================================
;;; Monad Operations
;;; ============================================================

;;; writer-bind : Writer w a -> (a -> Writer w b) -> Writer w b
;;; Sequence two Writer computations, combining output.
(define (writer-bind wr f)
  (let* ([monoid (writer-monoid wr)]
         [a (writer-value wr)]
         [w1 (writer-output wr)]
         [wr2 (f a)]
         [b (writer-value wr2)]
         [w2 (writer-output wr2)]
         [combined ((monoid-append monoid) w1 w2)])
        (make-writer monoid b combined)))

;;; writer-then : Writer w a -> Writer w b -> Writer w b
;;; Sequence, discarding first result.
(define (writer-then wr1 wr2)
  (writer-bind wr1 (lambda (_) wr2)))

;;; writer-map : (a -> b) -> Writer w a -> Writer w b
;;; Map a function over the value.
(define (writer-map f wr)
  (make-writer (writer-monoid wr)
               (f (writer-value wr))
               (writer-output wr)))

;;; writer-ap : Writer w (a -> b) -> Writer w a -> Writer w b
;;; Apply a Writer-wrapped function to a Writer-wrapped value.
(define (writer-ap wr-f wr-a)
  (let* ([monoid (writer-monoid wr-f)]
         [f (writer-value wr-f)]
         [w1 (writer-output wr-f)]
         [a (writer-value wr-a)]
         [w2 (writer-output wr-a)]
         [combined ((monoid-append monoid) w1 w2)])
        (make-writer monoid (f a) combined)))

;;; ============================================================
;;; Writer Combinators
;;; ============================================================

;;; writer-sequence : Monoid w -> (List (Writer w a)) -> Writer w (List a)
;;; Sequence a list of Writer computations.
(define (writer-sequence monoid writers)
  (if (null? writers)
      (writer-pure monoid '())
      (writer-bind (car writers)
                   (lambda (x)
                           (writer-bind (writer-sequence monoid (cdr writers))
                                        (lambda (xs)
                                                (writer-pure monoid (cons x xs))))))))

;;; writer-map-m : Monoid w -> (a -> Writer w b) -> (List a) -> Writer w (List b)
;;; Map a Writer-returning function over a list.
(define (writer-map-m monoid f lst)
  (writer-sequence monoid (map f lst)))

;;; writer-when : Monoid w -> Boolean -> Writer w () -> Writer w ()
;;; Execute Writer action only when condition is true.
(define (writer-when monoid condition action)
  (if condition
      action
      (writer-pure monoid '())))

;;; writer-unless : Monoid w -> Boolean -> Writer w () -> Writer w ()
;;; Execute Writer action only when condition is false.
(define (writer-unless monoid condition action)
  (writer-when monoid (not condition) action))

;;; ============================================================
;;; Logging Helpers (List Monoid)
;;; ============================================================

;;; log-msg : String -> Writer (List String) ()
;;; Log a message.
(define (log-msg msg)
  (writer-tell list-monoid (list msg)))

;;; log-value : String -> a -> Writer (List String) a
;;; Log a value with a label, returning the value.
(define (log-value label val)
  (make-writer list-monoid
               val
               (list (string-append label ": " (format "~s" val)))))

;;; with-logging : (a -> Writer (List String) b) -> a -> Writer (List String) b
;;; Wrapper for functions that should log entry/exit.
(define (with-logging name f)
  (lambda (x)
          (writer-bind (log-msg (string-append "Entering " name))
                       (lambda (_)
                               (writer-bind (f x)
                                            (lambda (result)
                                                    (writer-bind (log-msg (string-append "Exiting " name))
                                                                 (lambda (_)
                                                                         (writer-pure list-monoid result)))))))))

;;; ============================================================
;;; Counting Helpers (Sum Monoid)
;;; ============================================================

;;; count-one : Writer Number ()
;;; Increment counter by 1.
(define count-one
  (writer-tell sum-monoid 1))

;;; count-n : Number -> Writer Number ()
;;; Add n to counter.
(define (count-n n)
  (writer-tell sum-monoid n))

;;; with-count : (a -> Writer Number b) -> a -> Writer Number (b . Number)
;;; Wrap a computation and expose its count.
(define (with-count f)
  (lambda (x)
          (writer-listens identity (f x))))

;;; ============================================================
;;; String Building Helpers (String Monoid)
;;; ============================================================

;;; emit : String -> Writer String ()
;;; Emit a string.
(define (emit s)
  (writer-tell string-monoid s))

;;; emit-line : String -> Writer String ()
;;; Emit a string with newline.
(define (emit-line s)
  (writer-tell string-monoid (string-append s "
")))

;;; emit-indent : Number -> String -> Writer String ()
;;; Emit an indented line.
(define (emit-indent level s)
  (let ([indent (make-string (* level 2) (integer->char 32))])
       (emit-line (string-append indent s))))

;;; ============================================================
;;; Lifting Functions
;;; ============================================================

;;; writer-lift : Monoid w -> (a -> b) -> (Writer w a -> Writer w b)
;;; Lift a unary function.
(define (writer-lift monoid f)
  (lambda (wr) (writer-map f wr)))

;;; writer-lift2 : Monoid w -> (a -> b -> c) -> Writer w a -> Writer w b -> Writer w c
;;; Lift a binary function.
(define (writer-lift2 monoid f)
  (lambda (wr1 wr2)
          (writer-bind wr1
                       (lambda (a)
                               (writer-bind wr2
                                            (lambda (b)
                                                    (writer-pure monoid (f a b))))))))

;;; ============================================================
;;; Example Usage (for documentation)
;;; ============================================================
;;;
;;; ;; Simple logging
;;; (define logged-computation
;;;   (writer-bind (log-msg "Starting")
;;;                (lambda (_)
;;;                  (writer-bind (log-value "input" 5)
;;;                               (lambda (x)
;;;                                 (writer-bind (log-value "doubled" (* x 2))
;;;                                              (lambda (y)
;;;                                                (writer-bind (log-msg "Done")
;;;                                                             (lambda (_)
;;;                                                               (writer-pure list-monoid y))))))))))
;;; (run-writer logged-computation)
;;; ; => (10 . ("Starting" "input: 5" "doubled: 10" "Done"))
;;;
;;; ;; Counting operations
;;; (define count-ops
;;;   (writer-bind count-one
;;;                (lambda (_)
;;;                  (writer-bind count-one
;;;                               (lambda (_)
;;;                                 (writer-bind count-one
;;;                                              (lambda (_)
;;;                                                (writer-pure sum-monoid 'done))))))))
;;; (run-writer count-ops)
;;; ; => (done . 3)
;;;
;;; ;; Building output
;;; (define build-html
;;;   (writer-bind (emit-line "<html>")
;;;                (lambda (_)
;;;                  (writer-bind (emit-indent 1 "<body>")
;;;                               (lambda (_)
;;;                                 (writer-bind (emit-indent 2 "<p>Hello</p>")
;;;                                              (lambda (_)
;;;                                                (writer-bind (emit-indent 1 "</body>")
;;;                                                             (lambda (_)
;;;                                                               (writer-bind (emit-line "</html>")
;;;                                                                            (lambda (_)
;;;                                                                              (writer-pure string-monoid 'done))))))))))))
;;; (exec-writer build-html)
;;; ; => "<html>
  <body>
    <p>Hello</p>
  </body>
</html>
"
