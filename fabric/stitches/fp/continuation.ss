;;; fabric/stitches/fp/continuation.ss — Continuation Monad
;;;
;;; The Continuation monad captures the "rest of the computation"
;;; as a first-class value, enabling powerful control flow patterns.
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Features:
;;;   - Cont monad: Continuation-passing style computation
;;;   - ContT transformer: Continuation transformer
;;;   - callCC: Call with current continuation
;;;   - shift/reset: Delimited continuations
;;;   - Coroutines: Cooperative multitasking
;;;   - Backtracking: Non-deterministic search
;;;   - Exception-like control flow
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - fp/combinators.ss
;;;
;;; Type:
;;;   Cont r a = (a -> r) -> r
;;;
;;; The 'r' is the final result type of the computation.
;;;
;;; Monad Laws:
;;;   return a >>= f    = f a
;;;   m >>= return      = m
;;;   (m >>= f) >>= g   = m >>= (\x -> f x >>= g)

(load "fabric/stitches/prelude.ss")
(load "fabric/stitches/fp/combinators.ss")

;;; ============================================================
;;; Cont Monad
;;; ============================================================

;;; make-cont : ((a -> r) -> r) -> Cont r a
(define (make-cont run-fn)
  (list 'cont run-fn))

;;; cont? : Any -> Boolean
(define (cont? x)
  (and (pair? x) (eq? (car x) 'cont)))

;;; run-cont : Cont r a -> (a -> r) -> r
;;; Run the continuation with a final handler.
(define (run-cont c k)
  ((list-ref c 1) k))

;;; eval-cont : Cont a a -> a
;;; Evaluate a continuation that returns the same type.
(define (eval-cont c)
  (run-cont c identity))

;;; cont-return : a -> Cont r a
;;; Inject a value into a continuation.
(define (cont-return a)
  (make-cont (lambda (k) (k a))))

;;; cont-bind : Cont r a -> (a -> Cont r b) -> Cont r b
;;; Sequence continuations.
(define (cont-bind ma f)
  (make-cont
   (lambda (k)
           (run-cont ma (lambda (a)
                                (run-cont (f a) k))))))

;;; cont-map : (a -> b) -> Cont r a -> Cont r b
;;; Functor map for Cont.
(define (cont-map f ca)
  (cont-bind ca (lambda (a) (cont-return (f a)))))

;;; cont-ap : Cont r (a -> b) -> Cont r a -> Cont r b
;;; Applicative ap for Cont.
(define (cont-ap cf ca)
  (cont-bind cf (lambda (f)
                        (cont-bind ca (lambda (a)
                                              (cont-return (f a)))))))

;;; cont-join : Cont r (Cont r a) -> Cont r a
;;; Flatten nested continuations.
(define (cont-join cca)
  (cont-bind cca identity))

;;; ============================================================
;;; callCC - Call with Current Continuation
;;; ============================================================
;;;
;;; callCC gives you access to the current continuation,
;;; which you can invoke to escape from anywhere.

;;; callCC : ((a -> Cont r b) -> Cont r a) -> Cont r a
;;; The escape continuation, when called, aborts and returns its argument.
(define (callCC f)
  (make-cont
   (lambda (k)
           (run-cont (f (lambda (a)
                                (make-cont (lambda (_) (k a)))))
                     k))))

;;; ============================================================
;;; Control Operators
;;; ============================================================

;;; cont-abort : a -> Cont a b
;;; Abort with a final value, ignoring the rest of computation.
(define (cont-abort a)
  (make-cont (lambda (_) a)))

;;; cont-shift : ((a -> r) -> Cont r r) -> Cont r a
;;; Delimited shift: captures continuation up to nearest reset.
(define (cont-shift f)
  (callCC (lambda (k)
                  (cont-bind (f (lambda (a) (run-cont (k a) identity)))
                             cont-abort))))

;;; cont-reset : Cont r r -> Cont s r
;;; Delimited reset: delimits the extent of shift.
(define (cont-reset c)
  (cont-return (eval-cont c)))

;;; ============================================================
;;; Exception-like Control Flow
;;; ============================================================

;;; with-escape : ((a -> Cont r b) -> Cont r a) -> Cont r a
;;; Same as callCC but named more descriptively.
(define with-escape callCC)

;;; try-catch : Cont r a -> (exn -> Cont r a) -> Cont r a
;;; Simulated exception handling using continuations.
;;; Note: This uses a protocol where exceptions are (left exn).
(define (make-try-handler handler normal-cont)
  (lambda (result)
          (if (and (pair? result) (eq? (car result) 'exception))
              (run-cont (handler (cadr result)) normal-cont)
              (normal-cont result))))

;;; throw : exn -> Cont r a
;;; Throw an exception value.
(define (cont-throw exn)
  (make-cont (lambda (k)
                     (k (list 'exception exn)))))

;;; ============================================================
;;; Loop Control
;;; ============================================================

;;; cont-loop : a -> (a -> Cont r (Either a b)) -> Cont r b
;;; Loop until Right is returned.
(define (cont-loop init body)
  (cont-bind (body init)
             (lambda (either)
                     (if (left? either)
                         (cont-loop (from-left either) body)
                         (cont-return (from-right either))))))

;;; cont-for-each : (a -> Cont r ()) -> List a -> Cont r ()
;;; Execute continuation for each element.
(define (cont-for-each f lst)
  (if (null? lst)
      (cont-return '())
      (cont-bind (f (car lst))
                 (lambda (_)
                         (cont-for-each f (cdr lst))))))

;;; cont-fold : (b -> a -> Cont r b) -> b -> List a -> Cont r b
;;; Fold with continuation effects.
(define (cont-fold f init lst)
  (if (null? lst)
      (cont-return init)
      (cont-bind (f init (car lst))
                 (lambda (acc)
                         (cont-fold f acc (cdr lst))))))

;;; ============================================================
;;; Early Return Pattern
;;; ============================================================

;;; with-early-return : ((a -> Cont a b) -> Cont a a) -> a
;;; Execute with ability to return early.
(define (with-early-return body)
  (eval-cont (callCC body)))

;;; cont-when : Bool -> Cont r () -> Cont r ()
;;; Conditionally execute a continuation.
(define (cont-when pred action)
  (if pred action (cont-return '())))

;;; cont-unless : Bool -> Cont r () -> Cont r ()
;;; Execute unless predicate is true.
(define (cont-unless pred action)
  (cont-when (not pred) action))

;;; ============================================================
;;; Backtracking / Non-determinism
;;; ============================================================
;;;
;;; Using continuations for backtracking search.

;;; fail : Cont (Maybe a) a
;;; Failure in non-deterministic computation.
(define fail
  (make-cont (lambda (_) nothing)))

;;; choose : List a -> Cont (Maybe a) a
;;; Non-deterministically choose from a list.
(define (choose lst)
  (callCC (lambda (k)
                  (let loop ([remaining lst])
                       (if (null? remaining)
                           fail
                           (let ([result (run-cont (k (car remaining)) identity)])
                                (if (just? result)
                                    (cont-return (car remaining))
                                    (loop (cdr remaining)))))))))

;;; amb : a -> a -> Cont (Maybe a) a
;;; Binary choice.
(define (amb a b)
  (choose (list a b)))

;;; require : Bool -> Cont (Maybe a) ()
;;; Require a condition, fail if false.
(define (require pred)
  (if pred
      (cont-return '())
      fail))

;;; ============================================================
;;; Coroutines
;;; ============================================================
;;;
;;; Cooperative multitasking using continuations.

;;; make-coroutine : ((a -> Cont r ()) -> Cont r b) -> Coroutine r a
;;; Create a coroutine that can yield values.
(define (make-coroutine body)
  (list 'coroutine body #f))  ; (tag body done?)

;;; coroutine? : Any -> Boolean
(define (coroutine? x)
  (and (pair? x) (eq? (car x) 'coroutine)))

;;; yield : a -> Cont (Pair a (Cont r b)) ()
;;; Yield a value and suspend.
(define (yield val)
  (callCC (lambda (k)
                  (make-cont (lambda (outer-k)
                                     (outer-k (cons val (make-cont (lambda (inner-k)
                                                                           (run-cont (k '()) inner-k))))))))))

;;; ============================================================
;;; Generator Pattern
;;; ============================================================

;;; make-generator : (yield-fn -> Cont r ()) -> Generator
;;; Create a generator from a body that calls yield.
(define (make-generator body)
  (list 'generator body))

;;; generator-to-list : Generator -> List a
;;; Collect all yielded values into a list.
(define (generator-to-list gen)
  (let ([body (list-ref gen 1)])
       (let loop ([acc '()])
            (callCC (lambda (k)
                            ;; This is simplified; full impl needs more machinery
                            acc)))))

;;; ============================================================
;;; Trampolined Recursion
;;; ============================================================
;;;
;;; Use continuations to avoid stack overflow.

;;; make-bounce : (() -> Trampoline a) -> Trampoline a
(define (make-bounce thunk)
  (list 'bounce thunk))

;;; bounce? : Trampoline a -> Boolean
(define (bounce? x)
  (and (pair? x) (eq? (car x) 'bounce)))

;;; make-done : a -> Trampoline a
(define (make-done val)
  (list 'done val))

;;; done? : Trampoline a -> Boolean
(define (done? x)
  (and (pair? x) (eq? (car x) 'done)))

;;; done-value : Trampoline a -> a
(define (done-value d) (list-ref d 1))

;;; bounce-thunk : Trampoline a -> (() -> Trampoline a)
(define (bounce-thunk b) (list-ref b 1))

;;; trampoline : Trampoline a -> a
;;; Run a trampolined computation.
(define (trampoline t)
  (if (done? t)
      (done-value t)
      (trampoline ((bounce-thunk t)))))

;;; cont-trampoline : Cont (Trampoline a) a -> a
;;; Run a continuation in trampolined style.
(define (cont-trampoline c)
  (trampoline (run-cont c make-done)))

;;; ============================================================
;;; ContT Monad Transformer
;;; ============================================================
;;;
;;; ContT r m a = (a -> m r) -> m r

;;; make-cont-t : ((a -> m r) -> m r) -> ContT r m a
(define (make-cont-t run-fn)
  (list 'cont-t run-fn))

;;; cont-t? : Any -> Boolean
(define (cont-t? x)
  (and (pair? x) (eq? (car x) 'cont-t)))

;;; run-cont-t : ContT r m a -> (a -> m r) -> m r
(define (run-cont-t ct k)
  ((list-ref ct 1) k))

;;; cont-t-return : a -> ContT r m a
(define (cont-t-return a)
  (make-cont-t (lambda (k) (k a))))

;;; cont-t-bind : ContT r m a -> (a -> ContT r m b) -> ContT r m b
(define (cont-t-bind ma f)
  (make-cont-t
   (lambda (k)
           (run-cont-t ma (lambda (a)
                                  (run-cont-t (f a) k))))))

;;; cont-t-lift : m a -> ContT r m a
;;; Lift an underlying monad action.
(define (cont-t-lift m-bind m-action)
  (make-cont-t
   (lambda (k)
           (m-bind m-action k))))

;;; ============================================================
;;; Practical Helpers
;;; ============================================================

;;; sequence-cont : List (Cont r a) -> Cont r (List a)
;;; Sequence a list of continuations.
(define (sequence-cont conts)
  (if (null? conts)
      (cont-return '())
      (cont-bind (car conts)
                 (lambda (x)
                         (cont-bind (sequence-cont (cdr conts))
                                    (lambda (xs)
                                            (cont-return (cons x xs))))))))

;;; map-cont : (a -> Cont r b) -> List a -> Cont r (List b)
;;; Map a continuation-producing function over a list.
(define (map-cont f lst)
  (sequence-cont (map f lst)))

;;; filter-cont : (a -> Cont r Bool) -> List a -> Cont r (List a)
;;; Filter with a continuation predicate.
(define (filter-cont pred lst)
  (if (null? lst)
      (cont-return '())
      (cont-bind (pred (car lst))
                 (lambda (keep?)
                         (cont-bind (filter-cont pred (cdr lst))
                                    (lambda (rest)
                                            (cont-return (if keep?
                                                             (cons (car lst) rest)
                                                             rest))))))))

;;; ============================================================
;;; Do-Notation Helpers
;;; ============================================================

;;; cont-let* : Helper for sequential binding (manual do-notation)
;;; Usage: (cont-let* ([x (cont1)] [y (cont2 x)]) body)
;;; Implemented as a transformation to nested cont-bind.

;;; cont>> : Cont r a -> Cont r b -> Cont r b
;;; Sequence, discarding first result.
(define (cont>> ca cb)
  (cont-bind ca (lambda (_) cb)))

;;; cont-void : Cont r ()
;;; Return unit.
(define cont-void
  (cont-return '()))

;;; ============================================================
;;; Example Usage (for documentation)
;;; ============================================================
;;;
;;; ;; Basic continuation usage
;;; (eval-cont (cont-return 42))  ; => 42
;;;
;;; ;; Sequencing
;;; (eval-cont
;;;   (cont-bind (cont-return 5)
;;;              (lambda (x) (cont-return (* x 2)))))  ; => 10
;;;
;;; ;; callCC for early exit
;;; (eval-cont
;;;   (callCC (lambda (exit)
;;;             (cont-bind (exit 42)
;;;                        (lambda (x) (cont-return 0))))))  ; => 42
;;;
;;; ;; Loop with early exit
;;; (with-early-return
;;;   (lambda (return)
;;;     (cont-fold (lambda (acc x)
;;;                  (if (= x 0)
;;;                      (return acc)
;;;                      (cont-return (+ acc x))))
;;;                0
;;;                '(1 2 3 0 4 5))))  ; => 6 (stops at 0)
;;;
;;; ;; Backtracking search
;;; (eval-cont
;;;   (cont-bind (choose '(1 2 3))
;;;              (lambda (x)
;;;                (cont-bind (require (> x 1))
;;;                           (lambda (_) (cont-return x))))))
;;; ; Finds 2 (first > 1)
