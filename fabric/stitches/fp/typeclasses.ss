;;; fabric/stitches/fp/typeclasses.ss - Unified Typeclass Hierarchy
;;;
;;; This module provides a unified interface for the standard FP typeclass
;;; hierarchy: Functor, Applicative, Monad, and their duals.
;;;
;;; Since Scheme lacks typeclasses, we represent them as data structures
;;; containing the required operations. This allows:
;;;   - Generic programming over any type implementing a typeclass
;;;   - Instance derivation (e.g., derive Applicative from Monad)
;;;   - Unified law verification
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - fp/combinators.ss

(load "fabric/stitches/prelude.ss")
(load "fabric/stitches/fp/combinators.ss")

;;; ============================================================
;;; Functor
;;; ============================================================
;;;
;;; class Functor f where
;;;   fmap :: (a -> b) -> f a -> f b
;;;
;;; Laws:
;;;   fmap id = id                           (identity)
;;;   fmap (f . g) = fmap f . fmap g         (composition)

;;; make-functor : ((a -> b) -> f a -> f b) -> Functor f
(define (make-functor fmap-fn)
  (list 'functor fmap-fn))

;;; functor? : Any -> Boolean
(define (functor? x)
  (and (pair? x) (eq? (car x) 'functor)))

;;; functor-fmap : Functor f -> (a -> b) -> f a -> f b
(define (functor-fmap f)
  (list-ref f 1))

;;; fmap : Functor f -> (a -> b) -> f a -> f b
(define (fmap functor f fa)
  ((functor-fmap functor) f fa))

;;; (<$>) : Functor f -> (a -> b) -> f a -> f b
;;; Operator alias for fmap.
(define <$> fmap)

;;; (<$) : Functor f -> a -> f b -> f a
;;; Replace all values with a constant.
(define (const-fmap functor a fb)
  (fmap functor (const a) fb))

;;; void-fmap : Functor f -> f a -> f ()
;;; Discard the value.
(define (void-fmap functor fa)
  (fmap functor (const '()) fa))

;;; ============================================================
;;; Standard Functor Instances
;;; ============================================================

;;; list-functor : Functor List
(define list-functor
  (make-functor map))

;;; maybe-functor : Functor Maybe
(define maybe-functor
  (make-functor
   (lambda (f m)
           (if (nothing? m)
               nothing
               (just (f (from-just m)))))))

;;; either-functor : Functor (Either e)
;;; Maps over the Right value.
(define either-functor
  (make-functor
   (lambda (f e)
           (if (left? e)
               e
               (right (f (from-right e)))))))

;;; pair-functor : Functor ((,) a)
;;; Maps over the second element.
(define pair-functor
  (make-functor
   (lambda (f p)
           (cons (car p) (f (cdr p))))))

;;; function-functor : Functor ((->) r)
;;; Maps over the result of functions.
(define function-functor
  (make-functor compose))

;;; ============================================================
;;; Applicative
;;; ============================================================
;;;
;;; class Functor f => Applicative f where
;;;   pure :: a -> f a
;;;   (<*>) :: f (a -> b) -> f a -> f b
;;;
;;; Laws:
;;;   pure id <*> v = v                                   (identity)
;;;   pure (.) <*> u <*> v <*> w = u <*> (v <*> w)       (composition)
;;;   pure f <*> pure x = pure (f x)                      (homomorphism)
;;;   u <*> pure y = pure ($ y) <*> u                    (interchange)

;;; make-applicative-full : Functor f -> (a -> f a) -> (f (a->b) -> f a -> f b) -> Applicative f
(define (make-applicative-full functor pure-fn ap-fn)
  (list 'applicative functor pure-fn ap-fn))

;;; applicative? : Any -> Boolean
(define (applicative? x)
  (and (pair? x) (eq? (car x) 'applicative)))

;;; applicative-functor : Applicative f -> Functor f
(define (applicative-functor a)
  (list-ref a 1))

;;; applicative-pure : Applicative f -> a -> f a
(define (applicative-pure a)
  (list-ref a 2))

;;; applicative-ap : Applicative f -> f (a -> b) -> f a -> f b
(define (applicative-ap a)
  (list-ref a 3))

;;; pure : Applicative f -> a -> f a
(define (pure app a)
  ((applicative-pure app) a))

;;; ap : Applicative f -> f (a -> b) -> f a -> f b
(define (ap app ff fa)
  ((applicative-ap app) ff fa))

;;; lift2 : Applicative f -> (a -> b -> c) -> f a -> f b -> f c
;;; Note: function must be curried.
(define (lift2 app f fa fb)
  (ap app (fmap (applicative-functor app) f fa) fb))

;;; lift3 : Applicative f -> (a -> b -> c -> d) -> f a -> f b -> f c -> f d
(define (lift3 app f fa fb fc)
  (ap app (lift2 app f fa fb) fc))

;;; (*>) : Applicative f -> f a -> f b -> f b
;;; Sequence, discarding first result.
(define (seq-right app fa fb)
  (lift2 app (lambda (a) (lambda (b) b)) fa fb))

;;; (<*) : Applicative f -> f a -> f b -> f a
;;; Sequence, discarding second result.
(define (seq-left app fa fb)
  (lift2 app (lambda (a) (lambda (b) a)) fa fb))

;;; ============================================================
;;; Standard Applicative Instances
;;; ============================================================

;;; list-applicative : Applicative List
(define list-applicative
  (make-applicative-full
   list-functor
   list
   (lambda (fs as)
           (apply append (map (lambda (f) (map f as)) fs)))))

;;; maybe-applicative : Applicative Maybe
(define maybe-applicative
  (make-applicative-full
   maybe-functor
   just
   (lambda (mf ma)
           (if (nothing? mf)
               nothing
               (if (nothing? ma)
                   nothing
                   (just ((from-just mf) (from-just ma))))))))

;;; Alias for backwards compatibility
(define maybe-applicative-full maybe-applicative)

;;; either-applicative : Applicative (Either e)
(define either-applicative
  (make-applicative-full
   either-functor
   right
   (lambda (ef ea)
           (if (left? ef)
               ef
               (if (left? ea)
                   ea
                   (right ((from-right ef) (from-right ea))))))))

;;; function-applicative : Applicative ((->) r)
;;; The Reader applicative.
(define function-applicative
  (make-applicative-full
   function-functor
   const                                          ; pure: a -> (r -> a)
   (lambda (ff fa)                                ; ap: (r -> a -> b) -> (r -> a) -> (r -> b)
           (lambda (r)
                   ((ff r) (fa r))))))

;;; ============================================================
;;; Monad
;;; ============================================================
;;;
;;; class Applicative m => Monad m where
;;;   (>>=) :: m a -> (a -> m b) -> m b
;;;   return :: a -> m a  -- same as pure
;;;
;;; Laws:
;;;   return a >>= f = f a                         (left identity)
;;;   m >>= return = m                             (right identity)
;;;   (m >>= f) >>= g = m >>= (\x -> f x >>= g)   (associativity)

;;; make-monad : Applicative m -> (m a -> (a -> m b) -> m b) -> Monad m
(define (make-monad applicative bind-fn)
  (list 'monad applicative bind-fn))

;;; monad? : Any -> Boolean
(define (monad? x)
  (and (pair? x) (eq? (car x) 'monad)))

;;; monad-applicative : Monad m -> Applicative m
(define (monad-applicative m)
  (list-ref m 1))

;;; monad-bind : Monad m -> m a -> (a -> m b) -> m b
(define (monad-bind m)
  (list-ref m 2))

;;; return : Monad m -> a -> m a
(define (return m a)
  (pure (monad-applicative m) a))

;;; bind : Monad m -> m a -> (a -> m b) -> m b
(define (bind m ma f)
  ((monad-bind m) ma f))

;;; (>>=) : Monad m -> m a -> (a -> m b) -> m b
(define >>= bind)

;;; (>>) : Monad m -> m a -> m b -> m b
;;; Sequence, discarding first result.
(define (then m ma mb)
  (bind m ma (lambda (_) mb)))

;;; join : Monad m -> m (m a) -> m a
(define (join m mma)
  (bind m mma identity))

;;; ============================================================
;;; Deriving Applicative from Monad
;;; ============================================================

;;; monad->applicative : Monad m -> Applicative m
;;; Derive applicative instance from monad.
(define (monad->applicative m)
  (let* ([app (monad-applicative m)]
         [functor (applicative-functor app)]
         [pure-fn (applicative-pure app)])
        (make-applicative-full
         functor
         pure-fn
         (lambda (mf ma)
                 (bind m mf (lambda (f)
                                    (bind m ma (lambda (a)
                                                       (pure-fn (f a))))))))))

;;; ============================================================
;;; Standard Monad Instances
;;; ============================================================

;;; list-monad : Monad List
(define list-monad
  (make-monad
   list-applicative
   (lambda (ma f)
           (apply append (map f ma)))))

;;; maybe-monad : Monad Maybe
(define maybe-monad
  (make-monad
   maybe-applicative
   (lambda (ma f)
           (if (nothing? ma)
               nothing
               (f (from-just ma))))))

;;; Alias for backwards compatibility
(define maybe-monad-full maybe-monad)

;;; either-monad : Monad (Either e)
(define either-monad
  (make-monad
   either-applicative
   (lambda (ea f)
           (if (left? ea)
               ea
               (f (from-right ea))))))

;;; ============================================================
;;; Comonad
;;; ============================================================
;;;
;;; class Functor w => Comonad w where
;;;   extract :: w a -> a
;;;   extend :: (w a -> b) -> w a -> w b
;;;   duplicate :: w a -> w (w a)  -- derived: extend id
;;;
;;; Laws:
;;;   extract . extend f = f
;;;   extend extract = id
;;;   extend f . extend g = extend (f . extend g)

;;; make-comonad : Functor w -> (w a -> a) -> ((w a -> b) -> w a -> w b) -> Comonad w
(define (make-comonad functor extract-fn extend-fn)
  (list 'comonad functor extract-fn extend-fn))

;;; comonad? : Any -> Boolean
(define (comonad? x)
  (and (pair? x) (eq? (car x) 'comonad)))

;;; comonad-functor : Comonad w -> Functor w
(define (comonad-functor c)
  (list-ref c 1))

;;; comonad-extract : Comonad w -> w a -> a
(define (comonad-extract c)
  (list-ref c 2))

;;; comonad-extend : Comonad w -> (w a -> b) -> w a -> w b
(define (comonad-extend c)
  (list-ref c 3))

;;; extract : Comonad w -> w a -> a
(define (extract comonad wa)
  ((comonad-extract comonad) wa))

;;; extend : Comonad w -> (w a -> b) -> w a -> w b
(define (extend comonad f wa)
  ((comonad-extend comonad) f wa))

;;; duplicate : Comonad w -> w a -> w (w a)
(define (duplicate comonad wa)
  (extend comonad identity wa))

;;; (=>>) : Comonad w -> w a -> (w a -> b) -> w b
;;; Flipped extend.
(define (=>> comonad wa f)
  (extend comonad f wa))

;;; ============================================================
;;; Standard Comonad Instances
;;; ============================================================

;;; pair-comonad : Comonad ((,) e)
;;; The Env comonad - a value with an environment.
(define pair-comonad
  (make-comonad
   pair-functor
   cdr
   (lambda (f p)
           (cons (car p) (f p)))))

;;; function-comonad : Comonad ((->) e) for Monoid e
;;; The Traced comonad - functions from a monoid.
;;; Note: Requires monoid operations for extend.
(define (function-comonad-from-monoid mempty mappend)
  (make-comonad
   function-functor
   (lambda (f) (f mempty))                    ; extract: apply at identity
   (lambda (wab f)                            ; extend: shift the argument
           (lambda (m1)
                   (f (lambda (m2) (wab (mappend m1 m2))))))))

;;; ============================================================
;;; Law Verification
;;; ============================================================

;;; verify-functor-identity : Functor f -> f a -> (f a -> f a -> Boolean) -> Boolean
(define (verify-functor-identity functor fa eq?)
  (eq? fa (fmap functor identity fa)))

;;; verify-functor-composition : Functor f -> (b -> c) -> (a -> b) -> f a -> (f c -> f c -> Boolean) -> Boolean
(define (verify-functor-composition functor f g fa eq?)
  (eq? (fmap functor (compose f g) fa)
       (fmap functor f (fmap functor g fa))))

;;; verify-monad-left-identity : Monad m -> a -> (a -> m b) -> (m b -> m b -> Boolean) -> Boolean
(define (verify-monad-left-identity m a f eq?)
  (eq? (bind m (return m a) f)
       (f a)))

;;; verify-monad-right-identity : Monad m -> m a -> (m a -> m a -> Boolean) -> Boolean
(define (verify-monad-right-identity m ma eq?)
  (eq? (bind m ma (lambda (a) (return m a)))
       ma))

;;; verify-monad-associativity : Monad m -> m a -> (a -> m b) -> (b -> m c) -> (m c -> m c -> Boolean) -> Boolean
(define (verify-monad-associativity m ma f g eq?)
  (eq? (bind m (bind m ma f) g)
       (bind m ma (lambda (x) (bind m (f x) g)))))

;;; verify-comonad-left-identity : Comonad w -> w a -> (w a -> w a -> Boolean) -> Boolean
(define (verify-comonad-left-identity c wa eq?)
  (eq? (extend c (lambda (w) (extract c w)) wa)
       wa))

;;; verify-comonad-right-identity : Comonad w -> (w a -> b) -> w a -> Boolean
(define (verify-comonad-right-identity c f wa)
  (equal? (extract c (extend c f wa))
          (f wa)))

;;; ============================================================
;;; Utility Combinators
;;; ============================================================

;;; sequence-monad : Monad m -> (List (m a)) -> m (List a)
;;; Sequence a list of monadic values.
(define (sequence-monad m mas)
  (if (null? mas)
      (return m '())
      (bind m (car mas)
            (lambda (a)
                    (bind m (sequence-monad m (cdr mas))
                          (lambda (as)
                                  (return m (cons a as))))))))

;;; map-monad : Monad m -> (a -> m b) -> (List a) -> m (List b)
;;; Map a monadic function over a list.
(define (map-monad m f as)
  (sequence-monad m (map f as)))

;;; filter-monad : Monad m -> (a -> m Boolean) -> (List a) -> m (List a)
;;; Monadic filter.
(define (filter-monad m pred as)
  (if (null? as)
      (return m '())
      (bind m (pred (car as))
            (lambda (keep?)
                    (bind m (filter-monad m pred (cdr as))
                          (lambda (rest)
                                  (return m (if keep? (cons (car as) rest) rest))))))))

;;; fold-monad : Monad m -> (b -> a -> m b) -> b -> (List a) -> m b
;;; Monadic fold.
(define (fold-monad m f init as)
  (if (null? as)
      (return m init)
      (bind m (f init (car as))
            (lambda (acc)
                    (fold-monad m f acc (cdr as))))))

;;; when-monad : Monad m -> Boolean -> m () -> m ()
;;; Conditional execution.
(define (when-monad m cond action)
  (if cond action (return m '())))

;;; unless-monad : Monad m -> Boolean -> m () -> m ()
(define (unless-monad m cond action)
  (when-monad m (not cond) action))

;;; replicate-monad : Monad m -> Int -> m a -> m (List a)
;;; Repeat an action n times.
(define (replicate-monad m n action)
  (sequence-monad m (make-list n action)))

;;; make-list : Int -> a -> List a
;;; Helper to create a list of n copies.
(define (make-list n x)
  (if (<= n 0)
      '()
      (cons x (make-list (- n 1) x))))

;;; ============================================================
;;; Semigroupoid
;;; ============================================================
;;;
;;; A Semigroupoid is a Category without identity - just composition.
;;; Useful as the base for Arrow composition.
;;;
;;; class Semigroupoid c where
;;;   (.) :: c j k -> c i j -> c i k
;;;
;;; Laws:
;;;   (f . g) . h = f . (g . h)   (associativity)

;;; make-semigroupoid : ((c j k) -> (c i j) -> (c i k)) -> Semigroupoid c
(define (make-semigroupoid compose-fn)
  (list 'semigroupoid compose-fn))

;;; semigroupoid? : Any -> Boolean
(define (semigroupoid? x)
  (and (pair? x) (eq? (car x) 'semigroupoid)))

;;; semigroupoid-compose : Semigroupoid c -> c j k -> c i j -> c i k
(define (semigroupoid-compose sg)
  (list-ref sg 1))

;;; (.) : Semigroupoid c -> c j k -> c i j -> c i k
;;; Compose two morphisms.
(define (semigroup-compose sg f g)
  ((semigroupoid-compose sg) f g))

;;; ============================================================
;;; Semigroupoid Instances
;;; ============================================================

;;; function-semigroupoid : Semigroupoid (->)
(define function-semigroupoid
  (make-semigroupoid compose))

;;; ============================================================
;;; Apply (Applicative without pure)
;;; ============================================================
;;;
;;; Apply is Applicative without pure. Useful for types that can
;;; combine but not create from nothing (e.g., NonEmpty lists).
;;;
;;; class Functor f => Apply f where
;;;   (<*>) :: f (a -> b) -> f a -> f b
;;;
;;; Laws:
;;;   (.) <$> u <*> v <*> w = u <*> (v <*> w)   (composition)

;;; make-apply : Functor f -> (f (a -> b) -> f a -> f b) -> Apply f
(define (make-apply functor ap-fn)
  (list 'apply functor ap-fn))

;;; apply? : Any -> Boolean
(define (apply? x)
  (and (pair? x) (eq? (car x) 'apply)))

;;; apply-functor : Apply f -> Functor f
(define (apply-functor a)
  (list-ref a 1))

;;; apply-ap : Apply f -> f (a -> b) -> f a -> f b
(define (apply-ap a)
  (list-ref a 2))

;;; (<*>) : Apply f -> f (a -> b) -> f a -> f b
(define (ap-apply ap ff fa)
  ((apply-ap ap) ff fa))

;;; (<*) : Apply f -> f a -> f b -> f a
;;; Sequence, keep left.
(define (ap-left ap fa fb)
  (ap-apply ap (fmap (apply-functor ap) const fa) fb))

;;; (*>) : Apply f -> f a -> f b -> f b
;;; Sequence, keep right.
(define (ap-right ap fa fb)
  (ap-apply ap (fmap (apply-functor ap) (lambda (a) identity) fa) fb))

;;; lift-apply2 : Apply f -> (a -> b -> c) -> f a -> f b -> f c
;;; Lift a binary function.
(define (lift-apply2 ap f fa fb)
  (ap-apply ap (fmap (apply-functor ap) f fa) fb))

;;; ============================================================
;;; Apply Instances
;;; ============================================================

;;; list-apply : Apply List
(define list-apply
  (make-apply
   list-functor
   (lambda (fs as)
           (apply append (map (lambda (f) (map f as)) fs)))))

;;; maybe-apply : Apply Maybe
(define maybe-apply
  (make-apply
   maybe-functor
   (lambda (mf ma)
           (if (or (nothing? mf) (nothing? ma))
               nothing
               (just ((from-just mf) (from-just ma)))))))

;;; either-apply : Apply (Either e)
(define either-apply
  (make-apply
   either-functor
   (lambda (ef ea)
           (cond
            [(left? ef) ef]
            [(left? ea) ea]
            [else (right ((from-right ef) (from-right ea)))]))))

;;; ============================================================
;;; Bind (Monad without return)
;;; ============================================================
;;;
;;; Bind is Monad without return. Useful for types with sequencing
;;; but no trivial construction.
;;;
;;; class Apply m => Bind m where
;;;   (>>=) :: m a -> (a -> m b) -> m b
;;;
;;; Laws:
;;;   (m >>= f) >>= g = m >>= (\x -> f x >>= g)   (associativity)

;;; make-bind : Apply m -> (m a -> (a -> m b) -> m b) -> Bind m
(define (make-bind apply-inst bind-fn)
  (list 'bind apply-inst bind-fn))

;;; bind? : Any -> Boolean
(define (bind? x)
  (and (pair? x) (eq? (car x) 'bind)))

;;; bind-apply : Bind m -> Apply m
(define (bind-apply b)
  (list-ref b 1))

;;; bind-bind : Bind m -> m a -> (a -> m b) -> m b
(define (bind-bind b)
  (list-ref b 2))

;;; (>>=) : Bind m -> m a -> (a -> m b) -> m b
(define (bind-seq b ma f)
  ((bind-bind b) ma f))

;;; (>>) : Bind m -> m a -> m b -> m b
;;; Sequence, discarding first result.
(define (bind-then b ma mb)
  (bind-seq b ma (lambda (_) mb)))

;;; join-bind : Bind m -> m (m a) -> m a
;;; Flatten nested bind.
(define (join-bind b mma)
  (bind-seq b mma identity))

;;; ============================================================
;;; Bind Instances
;;; ============================================================

;;; list-bind : Bind List
(define list-bind
  (make-bind
   list-apply
   (lambda (as f)
           (apply append (map f as)))))

;;; maybe-bind : Bind Maybe
(define maybe-bind
  (make-bind
   maybe-apply
   (lambda (ma f)
           (if (nothing? ma)
               nothing
               (f (from-just ma))))))

;;; either-bind : Bind (Either e)
(define either-bind
  (make-bind
   either-apply
   (lambda (ea f)
           (if (left? ea)
               ea
               (f (from-right ea))))))

;;; ============================================================
;;; Selective (between Applicative and Monad)
;;; ============================================================
;;;
;;; Selective functors allow conditional execution of effects based
;;; on runtime values, without requiring full Monad power.
;;;
;;; class Applicative f => Selective f where
;;;   select :: f (Either a b) -> f (a -> b) -> f b
;;;
;;; Laws:
;;;   select (pure (Right b)) f = pure b                    (identity)
;;;   select (pure (Left a)) (pure f) = pure (f a)          (distributivity)
;;;   select x (pure id) = either id id <$> x               (idempotence)
;;;
;;; Reference: https://hackage.haskell.org/package/selective

;;; make-selective : Applicative f -> (f (Either a b) -> f (a -> b) -> f b) -> Selective f
(define (make-selective applicative select-fn)
  (list 'selective applicative select-fn))

;;; selective? : Any -> Boolean
(define (selective? x)
  (and (pair? x) (eq? (car x) 'selective)))

;;; selective-applicative : Selective f -> Applicative f
(define (selective-applicative s)
  (list-ref s 1))

;;; selective-select : Selective f -> f (Either a b) -> f (a -> b) -> f b
(define (selective-select s)
  (list-ref s 2))

;;; select : Selective f -> f (Either a b) -> f (a -> b) -> f b
;;; The core selective operation.
(define (select sel feab fab)
  ((selective-select sel) feab fab))

;;; branch : Selective f -> f (Either a b) -> f (a -> c) -> f (b -> c) -> f c
;;; Branch on Either, applying the appropriate function.
(define (branch sel feab fac fbc)
  (let* ([app (selective-applicative sel)]
         [func (applicative-functor app)]
         ;; Transform Either a b to Either a (Either () b)
         [feab* (fmap func (lambda (eab)
                                   (either (lambda (a) (left a))
                                           (lambda (b) (right (right b)))
                                           eab))
                      feab)]
         ;; First select: handle Left a
         [fac* (fmap func (lambda (f) (lambda (a) (right (f a)))) fac)]
         ;; Result is f (Either () c) or f (Either (Either () b) c)
         [intermediate (select sel feab* fac*)])
        ;; Second select: handle Right b -> c
        (select sel intermediate (fmap func (lambda (f) (lambda (u) (f '()))) fbc))))

;;; ifS : Selective f -> f Boolean -> f a -> f a -> f a
;;; Selective if-then-else.
(define (ifS sel fcond fthen felse)
  (let* ([app (selective-applicative sel)]
         [func (applicative-functor app)]
         ;; Convert Bool to Either () ()
         [feither (fmap func (lambda (b) (if b (left '()) (right '()))) fcond)])
        (branch sel feither
                (fmap func (lambda (a) (lambda (_) a)) fthen)
                (fmap func (lambda (a) (lambda (_) a)) felse))))

;;; whenS : Selective f -> f Boolean -> f () -> f ()
;;; Selective when.
(define (whenS sel fcond faction)
  (let* ([app (selective-applicative sel)]
         [func (applicative-functor app)])
        (ifS sel fcond faction (pure app '()))))

;;; (<*?) : Selective f -> f (Either a b) -> f (a -> b) -> f b
;;; Operator alias for select.
(define <*? select)

;;; ============================================================
;;; Selective Instances
;;; ============================================================

;;; maybe-selective : Selective Maybe
;;; Rigid selective - always evaluates both branches.
(define maybe-selective
  (make-selective
   maybe-applicative
   (lambda (meab mf)
           (cond
            [(nothing? meab) nothing]
            [(right? (from-just meab)) (just (from-right (from-just meab)))]
            [(nothing? mf) nothing]
            [else (just ((from-just mf) (from-left (from-just meab))))]))))

;;; either-selective : Selective (Either e)
;;; Rigid selective for Either.
(define either-selective
  (make-selective
   either-applicative
   (lambda (eeab ef)
           (cond
            [(left? eeab) eeab]
            [(right? (from-right eeab)) (right (from-right (from-right eeab)))]
            [(left? ef) ef]
            [else (right ((from-right ef) (from-left (from-right eeab))))]))))

;;; function-selective : Selective ((->) r)
;;; Functions have a selective instance.
(define function-selective
  (make-selective
   function-applicative
   (lambda (feab fab)
           (lambda (r)
                   (let ([eab (feab r)])
                        (if (right? eab)
                            (from-right eab)
                            ((fab r) (from-left eab))))))))
