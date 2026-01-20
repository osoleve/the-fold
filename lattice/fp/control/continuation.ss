(load "core/base/prelude.ss")
(load "lattice/fp/meta/combinators.ss")

(doc 'module 'continuation)
(doc 'description "The Continuation monad captures the rest of the computation as a first-class value, enabling powerful control flow patterns.")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'features "Cont monad, ContT transformer, callCC, shift/reset, coroutines, backtracking, exception-like control flow")
(doc 'type "Cont r a = (a -> r) -> r")
(doc 'note "The r is the final result type of the computation")
(doc 'note "Monad Laws: return a >>= f = f a; m >>= return = m; (m >>= f) >>= g = m >>= (\\x -> f x >>= g)")

(doc 'section 'cont-monad)

(define (make-cont run-fn)
  (doc 'type '(-> (-> (-> a r) r) (Cont r a)))
  (doc 'description "Create a Cont computation from a CPS function")
  (list 'cont run-fn))

(define (cont? x)
  (doc 'type '(-> Any Boolean))
  (doc 'description "Check if value is a Cont")
  (and (pair? x) (eq? (car x) 'cont)))

(define (run-cont c k)
  (doc 'type '(-> (Cont r a) (-> a r) r))
  (doc 'description "Run the continuation with a final handler")
  ((list-ref c 1) k))

(define (eval-cont c)
  (doc 'type '(-> (Cont a a) a))
  (doc 'description "Evaluate a continuation that returns the same type")
  (run-cont c identity))

(define (cont-return a)
  (doc 'type '(-> a (Cont r a)))
  (doc 'description "Inject a value into a continuation")
  (make-cont (lambda (k) (k a))))

(define (cont-bind ma f)
  (doc 'type '(-> (Cont r a) (-> a (Cont r b)) (Cont r b)))
  (doc 'description "Sequence continuations")
  (make-cont
   (lambda (k)
           (run-cont ma (lambda (a)
                                (run-cont (f a) k))))))

(define (cont-map f ca)
  (doc 'type '(-> (-> a b) (Cont r a) (Cont r b)))
  (doc 'description "Functor map for Cont")
  (cont-bind ca (lambda (a) (cont-return (f a)))))

(define (cont-ap cf ca)
  (doc 'type '(-> (Cont r (-> a b)) (Cont r a) (Cont r b)))
  (doc 'description "Applicative ap for Cont")
  (cont-bind cf (lambda (f)
                        (cont-bind ca (lambda (a)
                                              (cont-return (f a)))))))

(define (cont-join cca)
  (doc 'type '(-> (Cont r (Cont r a)) (Cont r a)))
  (doc 'description "Flatten nested continuations")
  (cont-bind cca identity))

(doc 'section 'callcc)
(doc 'note "callCC gives you access to the current continuation which you can invoke to escape from anywhere")

(define (callCC f)
  (doc 'type '(-> (-> (-> a (Cont r b)) (Cont r a)) (Cont r a)))
  (doc 'description "Call with current continuation - the escape continuation when called aborts and returns its argument")
  (make-cont
   (lambda (k)
           (run-cont (f (lambda (a)
                                (make-cont (lambda (_) (k a)))))
                     k))))

(doc 'section 'control-operators)

(define (cont-abort a)
  (doc 'type '(-> a (Cont a b)))
  (doc 'description "Abort with a final value, ignoring the rest of computation")
  (make-cont (lambda (_) a)))

(define (cont-shift f)
  (doc 'type '(-> (-> (-> a r) (Cont r r)) (Cont r a)))
  (doc 'description "Delimited shift - captures continuation up to nearest reset")
  (callCC (lambda (k)
                  (cont-bind (f (lambda (a) (run-cont (k a) identity)))
                             cont-abort))))

(define (cont-reset c)
  (doc 'type '(-> (Cont r r) (Cont s r)))
  (doc 'description "Delimited reset - delimits the extent of shift")
  (cont-return (eval-cont c)))

(doc 'section 'exception-like-control)
(doc 'note "Exception handling using continuations - exceptions are (left exn)")

(define with-escape callCC)
(doc with-escape 'type '(-> (-> (-> a (Cont r b)) (Cont r a)) (Cont r a)))
(doc with-escape 'description "Same as callCC but named more descriptively")

(define (make-try-handler handler normal-cont)
  (doc 'type '(-> (-> a (Cont r b)) (-> c r) (-> c r)))
  (doc 'description "Create a try/catch handler that intercepts exceptions")
  (lambda (result)
          (if (and (pair? result) (eq? (car result) 'exception))
              (run-cont (handler (cadr result)) normal-cont)
              (normal-cont result))))

(define (cont-throw exn)
  (doc 'type '(-> exn (Cont r a)))
  (doc 'description "Throw an exception value")
  (make-cont (lambda (k)
                     (k (list 'exception exn)))))

(doc 'section 'loop-control)

(define (cont-loop init body)
  (doc 'type '(-> a (-> a (Cont r (Either a b))) (Cont r b)))
  (doc 'description "Loop until Right is returned")
  (cont-bind (body init)
             (lambda (either)
                     (if (left? either)
                         (cont-loop (from-left either) body)
                         (cont-return (from-right either))))))

(define (cont-for-each f lst)
  (doc 'type '(-> (-> a (Cont r ())) (List a) (Cont r ())))
  (doc 'description "Execute continuation for each element")
  (if (null? lst)
      (cont-return '())
      (cont-bind (f (car lst))
                 (lambda (_)
                         (cont-for-each f (cdr lst))))))

(define (cont-fold f init lst)
  (doc 'type '(-> (-> b a (Cont r b)) b (List a) (Cont r b)))
  (doc 'description "Fold with continuation effects")
  (if (null? lst)
      (cont-return init)
      (cont-bind (f init (car lst))
                 (lambda (acc)
                         (cont-fold f acc (cdr lst))))))

(doc 'section 'early-return)

(define (with-early-return body)
  (doc 'type '(-> (-> (-> a (Cont a b)) (Cont a a)) a))
  (doc 'description "Execute with ability to return early")
  (eval-cont (callCC body)))

(define (cont-when pred action)
  (doc 'type '(-> Bool (Cont r ()) (Cont r ())))
  (doc 'description "Conditionally execute a continuation")
  (if pred action (cont-return '())))

(define (cont-unless pred action)
  (doc 'type '(-> Bool (Cont r ()) (Cont r ())))
  (doc 'description "Execute unless predicate is true")
  (cont-when (not pred) action))

(doc 'section 'backtracking)
(doc 'note "Using continuations for backtracking search")

(define fail
  (make-cont (lambda (_) nothing)))
(doc fail 'type '(Cont (Maybe a) a))
(doc fail 'description "Failure in non-deterministic computation")

(define (choose lst)
  (doc 'type '(-> (List a) (Cont (Maybe a) a)))
  (doc 'description "Non-deterministically choose from a list")
  (callCC (lambda (k)
                  (let loop ([remaining lst])
                       (if (null? remaining)
                           fail
                           (let ([result (run-cont (k (car remaining)) identity)])
                                (if (just? result)
                                    (cont-return (car remaining))
                                    (loop (cdr remaining)))))))))

(define (amb a b)
  (doc 'type '(-> a a (Cont (Maybe a) a)))
  (doc 'description "Binary choice")
  (choose (list a b)))

(define (require pred)
  (doc 'type '(-> Bool (Cont (Maybe a) ())))
  (doc 'description "Require a condition, fail if false")
  (if pred
      (cont-return '())
      fail))

(doc 'section 'coroutines)
(doc 'note "Cooperative multitasking using continuations")

(define (make-coroutine body)
  (doc 'type '(-> (-> (-> a (Cont r ())) (Cont r b)) (Coroutine r a)))
  (doc 'description "Create a coroutine that can yield values")
  (list 'coroutine body #f))

(define (coroutine? x)
  (doc 'type '(-> Any Boolean))
  (and (pair? x) (eq? (car x) 'coroutine)))

(define (yield val)
  (doc 'type '(-> a (Cont (Pair a (Cont r b)) ())))
  (doc 'description "Yield a value and suspend")
  (callCC (lambda (k)
                  (make-cont (lambda (outer-k)
                                     (outer-k (cons val (make-cont (lambda (inner-k)
                                                                           (run-cont (k '()) inner-k))))))))))

(doc 'section 'generators)

(define (make-generator body)
  (doc 'type '(-> (-> yield-fn (Cont r ())) Generator))
  (doc 'description "Create a generator from a body that calls yield")
  (list 'generator body))

(define (generator-to-list gen)
  (doc 'type '(-> Generator (List a)))
  (doc 'description "Collect all yielded values into a list")
  (let ([body (list-ref gen 1)])
       (let* ([values '()]
              [custom-yield
               (lambda (val)
                       (callCC (lambda (k)
                                       (set! values (append values (list val)))
                                       (k (cont-return val)))))])
             (eval-cont (body custom-yield))
             values)))

(doc 'section 'trampolined-recursion)
(doc 'note "Use continuations to avoid stack overflow")

(define (make-bounce thunk)
  (doc 'type '(-> (-> () (Trampoline a)) (Trampoline a)))
  (list 'bounce thunk))

(define (bounce? x)
  (doc 'type '(-> (Trampoline a) Boolean))
  (and (pair? x) (eq? (car x) 'bounce)))

(define (make-done val)
  (doc 'type '(-> a (Trampoline a)))
  (list 'done val))

(define (done? x)
  (doc 'type '(-> (Trampoline a) Boolean))
  (and (pair? x) (eq? (car x) 'done)))

(define (done-value d)
  (doc 'type '(-> (Trampoline a) a))
  (list-ref d 1))

(define (bounce-thunk b)
  (doc 'type '(-> (Trampoline a) (-> () (Trampoline a))))
  (list-ref b 1))

(define (trampoline t)
  (doc 'type '(-> (Trampoline a) a))
  (doc 'description "Run a trampolined computation")
  (if (done? t)
      (done-value t)
      (trampoline ((bounce-thunk t)))))

(define (cont-trampoline c)
  (doc 'type '(-> (Cont (Trampoline a) a) a))
  (doc 'description "Run a continuation in trampolined style")
  (trampoline (run-cont c make-done)))

(doc 'section 'cont-t-transformer)
(doc 'note "ContT r m a = (a -> m r) -> m r")

(define (make-cont-t run-fn)
  (doc 'type '(-> (-> (-> a (m r)) (m r)) (ContT r m a)))
  (list 'cont-t run-fn))

(define (cont-t? x)
  (doc 'type '(-> Any Boolean))
  (and (pair? x) (eq? (car x) 'cont-t)))

(define (run-cont-t ct k)
  (doc 'type '(-> (ContT r m a) (-> a (m r)) (m r)))
  ((list-ref ct 1) k))

(define (cont-t-return a)
  (doc 'type '(-> a (ContT r m a)))
  (make-cont-t (lambda (k) (k a))))

(define (cont-t-bind ma f)
  (doc 'type '(-> (ContT r m a) (-> a (ContT r m b)) (ContT r m b)))
  (make-cont-t
   (lambda (k)
           (run-cont-t ma (lambda (a)
                                  (run-cont-t (f a) k))))))

(define (cont-t-lift m-bind m-action)
  (doc 'type '(-> m-bind (m a) (ContT r m a)))
  (doc 'description "Lift an underlying monad action")
  (make-cont-t
   (lambda (k)
           (m-bind m-action k))))

(doc 'section 'practical-helpers)

(define (sequence-cont conts)
  (doc 'type '(-> (List (Cont r a)) (Cont r (List a))))
  (doc 'description "Sequence a list of continuations")
  (if (null? conts)
      (cont-return '())
      (cont-bind (car conts)
                 (lambda (x)
                         (cont-bind (sequence-cont (cdr conts))
                                    (lambda (xs)
                                            (cont-return (cons x xs))))))))

(define (map-cont f lst)
  (doc 'type '(-> (-> a (Cont r b)) (List a) (Cont r (List b))))
  (doc 'description "Map a continuation-producing function over a list")
  (sequence-cont (map f lst)))

(define (filter-cont pred lst)
  (doc 'type '(-> (-> a (Cont r Bool)) (List a) (Cont r (List a))))
  (doc 'description "Filter with a continuation predicate")
  (if (null? lst)
      (cont-return '())
      (cont-bind (pred (car lst))
                 (lambda (keep?)
                         (cont-bind (filter-cont pred (cdr lst))
                                    (lambda (rest)
                                            (cont-return (if keep?
                                                             (cons (car lst) rest)
                                                             rest))))))))

(doc 'section 'do-notation-helpers)

(define (cont>> ca cb)
  (doc 'type '(-> (Cont r a) (Cont r b) (Cont r b)))
  (doc 'description "Sequence, discarding first result")
  (cont-bind ca (lambda (_) cb)))

(define cont-void
  (cont-return '()))
(doc cont-void 'type '(Cont r ()))
(doc cont-void 'description "Return unit")
