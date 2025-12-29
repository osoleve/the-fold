;;; fabric/stitches/fp/transformers.ss — Monad Transformers
;;;
;;; Monad transformers allow combining monadic effects. Each transformer
;;; adds its effect to an underlying monad.
;;;
;;; ReaderT r m a = r -> m a         (adds environment to any monad)
;;; StateT s m a = s -> m (a, s)     (adds state to any monad)
;;; WriterT w m a = m (a, w)         (adds logging to any monad)
;;; MaybeT m a = m (Maybe a)         (adds failure to any monad)
;;; EitherT e m a = m (Either e a)   (adds errors to any monad)
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Note: Without typeclasses, we pass monad operations explicitly.
;;; Each transformer takes the underlying monad's pure and bind.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - fp/combinators.ss

(load "fabric/stitches/prelude.ss")
(load "fabric/stitches/fp/combinators.ss")

;;; ============================================================
;;; Identity Monad (Base Case)
;;; ============================================================

;;; identity-pure : a -> Identity a
(define (identity-pure x) x)

;;; identity-bind : Identity a -> (a -> Identity b) -> Identity b
(define (identity-bind m f) (f m))

;;; identity-map : (a -> b) -> Identity a -> Identity b
(define (identity-map f m) (f m))

;;; ============================================================
;;; MaybeT Transformer
;;; ============================================================
;;;
;;; MaybeT m a = m (Maybe a)
;;; Adds optional failure to any monad.

;;; make-maybe-t : m (Maybe a) -> MaybeT m a
(define (make-maybe-t inner)
  (list 'maybe-t inner))

;;; maybe-t? : Any -> Boolean
(define (maybe-t? x)
  (and (pair? x) (eq? (car x) 'maybe-t)))

;;; run-maybe-t : MaybeT m a -> m (Maybe a)
(define (run-maybe-t mt)
  (list-ref mt 1))

;;; maybe-t-pure : (a -> m a) -> a -> MaybeT m a
;;; Lift a value into MaybeT.
(define (maybe-t-pure m-pure x)
  (make-maybe-t (m-pure (just x))))

;;; maybe-t-bind : (m a -> (a -> m b) -> m b) -> (a -> m a) -> MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
;;; Sequence MaybeT computations.
(define (maybe-t-bind m-bind m-pure mt f)
  (make-maybe-t
   (m-bind (run-maybe-t mt)
           (lambda (maybe-a)
                   (if (nothing? maybe-a)
                       (m-pure nothing)
                       (run-maybe-t (f (from-just maybe-a))))))))

;;; maybe-t-fail : (a -> m a) -> MaybeT m a
;;; Fail in MaybeT.
(define (maybe-t-fail m-pure)
  (make-maybe-t (m-pure nothing)))

;;; maybe-t-lift : (m a -> (a -> m b) -> m b) -> (a -> m a) -> m a -> MaybeT m a
;;; Lift an action from the underlying monad.
(define (maybe-t-lift m-bind m-pure m-action)
  (make-maybe-t
   (m-bind m-action
           (lambda (a)
                   (m-pure (just a))))))

;;; ============================================================
;;; EitherT Transformer
;;; ============================================================
;;;
;;; EitherT e m a = m (Either e a)
;;; Adds error handling to any monad.

;;; make-either-t : m (Either e a) -> EitherT e m a
(define (make-either-t inner)
  (list 'either-t inner))

;;; either-t? : Any -> Boolean
(define (either-t? x)
  (and (pair? x) (eq? (car x) 'either-t)))

;;; run-either-t : EitherT e m a -> m (Either e a)
(define (run-either-t et)
  (list-ref et 1))

;;; either-t-pure : (a -> m a) -> a -> EitherT e m a
;;; Lift a value into EitherT.
(define (either-t-pure m-pure x)
  (make-either-t (m-pure (right x))))

;;; either-t-bind : (m a -> (a -> m b) -> m b) -> (a -> m a) -> EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
;;; Sequence EitherT computations.
(define (either-t-bind m-bind m-pure et f)
  (make-either-t
   (m-bind (run-either-t et)
           (lambda (either-a)
                   (if (left? either-a)
                       (m-pure either-a)
                       (run-either-t (f (from-right either-a))))))))

;;; either-t-throw : (a -> m a) -> e -> EitherT e m a
;;; Throw an error in EitherT.
(define (either-t-throw m-pure err)
  (make-either-t (m-pure (left err))))

;;; either-t-catch : (m a -> (a -> m b) -> m b) -> (a -> m a) -> EitherT e m a -> (e -> EitherT e m a) -> EitherT e m a
;;; Catch and handle errors.
(define (either-t-catch m-bind m-pure et handler)
  (make-either-t
   (m-bind (run-either-t et)
           (lambda (either-a)
                   (if (left? either-a)
                       (run-either-t (handler (from-left either-a)))
                       (m-pure either-a))))))

;;; either-t-lift : (m a -> (a -> m b) -> m b) -> (a -> m a) -> m a -> EitherT e m a
;;; Lift an action from the underlying monad.
(define (either-t-lift m-bind m-pure m-action)
  (make-either-t
   (m-bind m-action
           (lambda (a)
                   (m-pure (right a))))))

;;; ============================================================
;;; ReaderT Transformer
;;; ============================================================
;;;
;;; ReaderT r m a = r -> m a
;;; Adds read-only environment to any monad.

;;; make-reader-t : (r -> m a) -> ReaderT r m a
(define (make-reader-t run-fn)
  (list 'reader-t run-fn))

;;; reader-t? : Any -> Boolean
(define (reader-t? x)
  (and (pair? x) (eq? (car x) 'reader-t)))

;;; reader-t-fn : ReaderT r m a -> (r -> m a)
(define (reader-t-fn rt)
  (list-ref rt 1))

;;; run-reader-t : ReaderT r m a -> r -> m a
(define (run-reader-t rt env)
  ((reader-t-fn rt) env))

;;; reader-t-pure : (a -> m a) -> a -> ReaderT r m a
;;; Lift a value into ReaderT.
(define (reader-t-pure m-pure x)
  (make-reader-t (lambda (env) (m-pure x))))

;;; reader-t-bind : (m a -> (a -> m b) -> m b) -> ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
;;; Sequence ReaderT computations.
(define (reader-t-bind m-bind rt f)
  (make-reader-t
   (lambda (env)
           (m-bind (run-reader-t rt env)
                   (lambda (a)
                           (run-reader-t (f a) env))))))

;;; reader-t-ask : (a -> m a) -> ReaderT r m r
;;; Get the environment.
(define (reader-t-ask m-pure)
  (make-reader-t (lambda (env) (m-pure env))))

;;; reader-t-asks : (a -> m a) -> (r -> a) -> ReaderT r m a
;;; Get a value derived from environment.
(define (reader-t-asks m-pure f)
  (make-reader-t (lambda (env) (m-pure (f env)))))

;;; reader-t-local : (r -> r) -> ReaderT r m a -> ReaderT r m a
;;; Run with modified environment.
(define (reader-t-local f rt)
  (make-reader-t (lambda (env) (run-reader-t rt (f env)))))

;;; reader-t-lift : m a -> ReaderT r m a
;;; Lift an action from the underlying monad.
(define (reader-t-lift m-action)
  (make-reader-t (lambda (env) m-action)))

;;; ============================================================
;;; StateT Transformer
;;; ============================================================
;;;
;;; StateT s m a = s -> m (a, s)
;;; Adds mutable state to any monad.

;;; make-state-t : (s -> m (a . s)) -> StateT s m a
(define (make-state-t run-fn)
  (list 'state-t run-fn))

;;; state-t? : Any -> Boolean
(define (state-t? x)
  (and (pair? x) (eq? (car x) 'state-t)))

;;; state-t-fn : StateT s m a -> (s -> m (a . s))
(define (state-t-fn st)
  (list-ref st 1))

;;; run-state-t : StateT s m a -> s -> m (a . s)
(define (run-state-t st initial-state)
  ((state-t-fn st) initial-state))

;;; eval-state-t : (m a -> (a -> m b) -> m b) -> (a -> m a) -> StateT s m a -> s -> m a
;;; Run StateT and extract value.
(define (eval-state-t m-bind m-pure st initial-state)
  (m-bind (run-state-t st initial-state)
          (lambda (pair)
                  (m-pure (car pair)))))

;;; exec-state-t : (m a -> (a -> m b) -> m b) -> (a -> m a) -> StateT s m a -> s -> m s
;;; Run StateT and extract final state.
(define (exec-state-t m-bind m-pure st initial-state)
  (m-bind (run-state-t st initial-state)
          (lambda (pair)
                  (m-pure (cdr pair)))))

;;; state-t-pure : (a -> m a) -> a -> StateT s m a
;;; Lift a value into StateT.
(define (state-t-pure m-pure x)
  (make-state-t (lambda (s) (m-pure (cons x s)))))

;;; state-t-bind : (m a -> (a -> m b) -> m b) -> StateT s m a -> (a -> StateT s m b) -> StateT s m b
;;; Sequence StateT computations.
(define (state-t-bind m-bind st f)
  (make-state-t
   (lambda (s)
           (m-bind (run-state-t st s)
                   (lambda (pair)
                           (let ([a (car pair)]
                                 [s2 (cdr pair)])
                                (run-state-t (f a) s2)))))))

;;; state-t-get : (a -> m a) -> StateT s m s
;;; Get the current state.
(define (state-t-get m-pure)
  (make-state-t (lambda (s) (m-pure (cons s s)))))

;;; state-t-put : (a -> m a) -> s -> StateT s m ()
;;; Set the state.
(define (state-t-put m-pure new-state)
  (make-state-t (lambda (s) (m-pure (cons '() new-state)))))

;;; state-t-modify : (a -> m a) -> (s -> s) -> StateT s m ()
;;; Modify the state.
(define (state-t-modify m-pure f)
  (make-state-t (lambda (s) (m-pure (cons '() (f s))))))

;;; state-t-gets : (a -> m a) -> (s -> a) -> StateT s m a
;;; Get a value derived from state.
(define (state-t-gets m-pure f)
  (make-state-t (lambda (s) (m-pure (cons (f s) s)))))

;;; state-t-lift : (m a -> (a -> m b) -> m b) -> (a -> m a) -> m a -> StateT s m a
;;; Lift an action from the underlying monad.
(define (state-t-lift m-bind m-pure m-action)
  (make-state-t
   (lambda (s)
           (m-bind m-action
                   (lambda (a)
                           (m-pure (cons a s)))))))

;;; ============================================================
;;; WriterT Transformer
;;; ============================================================
;;;
;;; WriterT w m a = m (a, w)
;;; Adds logging/accumulation to any monad.

;;; make-writer-t : Monoid w -> m (a . w) -> WriterT w m a
(define (make-writer-t monoid inner)
  (list 'writer-t monoid inner))

;;; writer-t? : Any -> Boolean
(define (writer-t? x)
  (and (pair? x) (eq? (car x) 'writer-t)))

;;; writer-t-monoid : WriterT w m a -> Monoid w
(define (writer-t-monoid wt)
  (list-ref wt 1))

;;; writer-t-inner : WriterT w m a -> m (a . w)
(define (writer-t-inner wt)
  (list-ref wt 2))

;;; run-writer-t : WriterT w m a -> m (a . w)
(define (run-writer-t wt)
  (writer-t-inner wt))

;;; writer-t-pure : (a -> m a) -> Monoid w -> a -> WriterT w m a
;;; Lift a value into WriterT.
(define (writer-t-pure m-pure monoid x)
  (make-writer-t monoid (m-pure (cons x (monoid-empty monoid)))))

;;; Monoid helpers (from writer.ss)
(define (make-monoid empty append-fn)
  (cons empty append-fn))
(define (monoid-empty m) (car m))
(define (monoid-append m) (cdr m))

;;; writer-t-bind : (m a -> (a -> m b) -> m b) -> (a -> m a) -> WriterT w m a -> (a -> WriterT w m b) -> WriterT w m b
;;; Sequence WriterT computations.
(define (writer-t-bind m-bind m-pure wt f)
  (let ([monoid (writer-t-monoid wt)])
       (make-writer-t
        monoid
        (m-bind (run-writer-t wt)
                (lambda (pair1)
                        (let* ([a (car pair1)]
                               [w1 (cdr pair1)]
                               [wt2 (f a)])
                              (m-bind (run-writer-t wt2)
                                      (lambda (pair2)
                                              (let* ([b (car pair2)]
                                                     [w2 (cdr pair2)]
                                                     [combined ((monoid-append monoid) w1 w2)])
                                                    (m-pure (cons b combined)))))))))))

;;; writer-t-tell : (a -> m a) -> Monoid w -> w -> WriterT w m ()
;;; Output a value.
(define (writer-t-tell m-pure monoid output)
  (make-writer-t monoid (m-pure (cons '() output))))

;;; writer-t-listen : (m a -> (a -> m b) -> m b) -> (a -> m a) -> WriterT w m a -> WriterT w m (a . w)
;;; Expose the output alongside the value.
(define (writer-t-listen m-bind m-pure wt)
  (let ([monoid (writer-t-monoid wt)])
       (make-writer-t
        monoid
        (m-bind (run-writer-t wt)
                (lambda (pair)
                        (let ([a (car pair)]
                              [w (cdr pair)])
                             (m-pure (cons (cons a w) w))))))))

;;; writer-t-lift : (m a -> (a -> m b) -> m b) -> (a -> m a) -> Monoid w -> m a -> WriterT w m a
;;; Lift an action from the underlying monad.
(define (writer-t-lift m-bind m-pure monoid m-action)
  (make-writer-t
   monoid
   (m-bind m-action
           (lambda (a)
                   (m-pure (cons a (monoid-empty monoid)))))))

;;; ============================================================
;;; Common Monad Stacks
;;; ============================================================

;;; List monoid (for WriterT)
(define transformer-list-monoid
  (make-monoid '() append))

;;; ReaderT r (Either e) a — Reader with errors
;;; Useful for configuration + validation

;;; reader-either-pure : r -> ReaderT r (Either e) a
(define (reader-either-pure x)
  (reader-t-pure (lambda (y) (right y)) x))

;;; reader-either-bind : ReaderT r (Either e) a -> (a -> ReaderT r (Either e) b) -> ReaderT r (Either e) b
(define (reader-either-bind rt f)
  (reader-t-bind either-bind rt f))

;;; Helper for Either monad
(define (either-bind e f)
  (if (left? e)
      e
      (f (from-right e))))

;;; reader-either-ask : ReaderT r (Either e) r
(define reader-either-ask
  (reader-t-ask (lambda (x) (right x))))

;;; reader-either-throw : e -> ReaderT r (Either e) a
(define (reader-either-throw err)
  (reader-t-lift (left err)))

;;; ============================================================
;;; Example Usage (for documentation)
;;; ============================================================
;;;
;;; ;; ReaderT with Either for config + validation
;;; (define validate-config
;;;   (reader-either-bind reader-either-ask
;;;                       (lambda (env)
;;;                         (let ([port (cdr (assoc 'port env))])
;;;                           (if (and (number? port) (> port 0))
;;;                               (reader-either-pure port)
;;;                               (reader-either-throw "Invalid port"))))))
;;;
;;; (run-reader-t validate-config '((port . 8080)))
;;; ; => (right 8080)
;;;
;;; (run-reader-t validate-config '((port . -1)))
;;; ; => (left "Invalid port")
;;;
;;; ;; StateT with Maybe for stateful computations that can fail
;;; (define state-maybe-pop
;;;   (state-t-bind (lambda (m f) (if (nothing? m) nothing (f (from-just m))))
;;;                 (state-t-get (lambda (x) (just x)))
;;;                 (lambda (stack)
;;;                   (if (null? stack)
;;;                       (make-state-t (lambda (s) nothing))
;;;                       (state-t-bind (lambda (m f) (if (nothing? m) nothing (f (from-just m))))
;;;                                     (state-t-put (lambda (x) (just x)) (cdr stack))
;;;                                     (lambda (_)
;;;                                       (state-t-pure (lambda (x) (just x)) (car stack))))))))
