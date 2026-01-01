;;; fabric/stitches/fp/bifunctor.ss — Bifunctor Typeclass
;;;
;;; A Bifunctor is a type constructor with two type parameters that is
;;; a functor in both arguments. Common examples: Either, Pair, Validation.
;;;
;;; class Bifunctor p where
;;;   bimap  :: (a -> b) -> (c -> d) -> p a c -> p b d
;;;   first  :: (a -> b) -> p a c -> p b c
;;;   second :: (c -> d) -> p a c -> p a d
;;;
;;; Laws:
;;;   bimap id id = id                              (identity)
;;;   bimap f g . bimap h i = bimap (f . h) (g . i) (composition)
;;;   first id = id
;;;   second id = id
;;;   first f = bimap f id
;;;   second g = bimap id g
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - fp/combinators.ss
;;;   - fp/algebraic.ss (for mappend in Bifoldable)
;;;   - fp/traversable.ss (for app-fmap, app-lift2-uncurried in Bitraversable)

(load "core/prelude.ss")
(load "core/fp/meta/combinators.ss")
(load "core/fp/typeclasses/algebraic.ss")
(load "core/fp/functors/traversable.ss")

;;; ============================================================
;;; Bifunctor Type
;;; ============================================================

;;; make-bifunctor : (a -> b -> p a b) -> ((a -> c) -> p a b -> p c b) -> ((b -> d) -> p a b -> p a d) -> Bifunctor p
;;; Create a bifunctor from first-map, second-map operations.
;;; bimap is derived from first and second.
(define (make-bifunctor bf-first bf-second)
  (list 'bifunctor bf-first bf-second))

;;; bifunctor? : Any -> Boolean
(define (bifunctor? x)
  (and (pair? x) (eq? (car x) 'bifunctor)))

;;; bifunctor-first : Bifunctor p -> (a -> b) -> p a c -> p b c
;;; Map over the first type parameter.
(define (bifunctor-first bf)
  (list-ref bf 1))

;;; bifunctor-second : Bifunctor p -> (c -> d) -> p a c -> p a d
;;; Map over the second type parameter.
(define (bifunctor-second bf)
  (list-ref bf 2))

;;; bf-bimap : Bifunctor p -> (a -> b) -> (c -> d) -> p a c -> p b d
;;; Map over both type parameters.
(define (bf-bimap bf f g x)
  ((bifunctor-second bf) g ((bifunctor-first bf) f x)))

;;; bf-first : Bifunctor p -> (a -> b) -> p a c -> p b c
;;; Convenience wrapper for bifunctor-first.
(define (bf-first bf f x)
  ((bifunctor-first bf) f x))

;;; bf-second : Bifunctor p -> (c -> d) -> p a c -> p a d
;;; Convenience wrapper for bifunctor-second.
(define (bf-second bf g x)
  ((bifunctor-second bf) g x))

;;; ============================================================
;;; Pair Bifunctor
;;; ============================================================
;;;
;;; Pair (a, b) is a bifunctor where:
;;;   first f (a, b) = (f a, b)
;;;   second g (a, b) = (a, g b)

(define pair-bifunctor
  (make-bifunctor
   ;; first
   (lambda (f p) (cons (f (car p)) (cdr p)))
   ;; second
   (lambda (g p) (cons (car p) (g (cdr p))))))

;;; pair-bimap : (a -> b) -> (c -> d) -> (a . c) -> (b . d)
(define (pair-bimap f g p)
  (bf-bimap pair-bifunctor f g p))

;;; pair-first : (a -> b) -> (a . c) -> (b . c)
(define (pair-map-first f p)
  (bf-first pair-bifunctor f p))

;;; pair-second : (c -> d) -> (a . c) -> (a . d)
(define (pair-map-second g p)
  (bf-second pair-bifunctor g p))

;;; ============================================================
;;; Either Bifunctor
;;; ============================================================
;;;
;;; Either e a is a bifunctor where:
;;;   first f (Left e) = Left (f e)
;;;   first f (Right a) = Right a
;;;   second g (Left e) = Left e
;;;   second g (Right a) = Right (g a)

(define either-bifunctor
  (make-bifunctor
   ;; first (maps over Left)
   (lambda (f e)
           (if (left? e)
               (left (f (from-left e)))
               e))
   ;; second (maps over Right)
   (lambda (g e)
           (if (right? e)
               (right (g (from-right e)))
               e))))

;;; either-bimap : (e -> e') -> (a -> a') -> Either e a -> Either e' a'
(define (either-bimap f g e)
  (bf-bimap either-bifunctor f g e))

;;; Note: combinators.ss provides either-fmap and either-fmap-left as direct implementations.
;;; These bifunctor-based versions use the bifunctor machinery explicitly.

;;; either-map-left : (e -> e') -> Either e a -> Either e' a
;;; Bifunctor-based left mapping (equivalent to either-fmap-left in combinators.ss).
(define (either-map-left f e)
  (bf-first either-bifunctor f e))

;;; either-map-right : (a -> a') -> Either e a -> Either e a'
;;; Bifunctor-based right mapping (equivalent to either-fmap in combinators.ss).
(define (either-map-right g e)
  (bf-second either-bifunctor g e))

;;; ============================================================
;;; Validation Bifunctor
;;; ============================================================
;;;
;;; Validation e a is a bifunctor similar to Either:
;;;   first f (Failure es) = Failure (map f es)
;;;   first f (Success a) = Success a
;;;   second g (Failure es) = Failure es
;;;   second g (Success a) = Success (g a)

;;; validation-success : a -> Validation e a
(define (validation-mk-success x)
  (list 'success x))

;;; validation-failure : (List e) -> Validation e a
(define (validation-mk-failure errs)
  (list 'failure errs))

;;; validation-success? : Validation e a -> Boolean
(define (validation-is-success? v)
  (and (pair? v) (eq? (car v) 'success)))

;;; validation-failure? : Validation e a -> Boolean
(define (validation-is-failure? v)
  (and (pair? v) (eq? (car v) 'failure)))

;;; from-validation-success : Validation e a -> a
(define (from-validation-success v)
  (cadr v))

;;; from-validation-failure : Validation e a -> (List e)
(define (from-validation-failure v)
  (cadr v))

(define validation-bifunctor
  (make-bifunctor
   ;; first (maps over each error in Failure)
   (lambda (f v)
           (if (validation-is-failure? v)
               (validation-mk-failure (map f (from-validation-failure v)))
               v))
   ;; second (maps over Success)
   (lambda (g v)
           (if (validation-is-success? v)
               (validation-mk-success (g (from-validation-success v)))
               v))))

;;; validation-bimap : (e -> e') -> (a -> a') -> Validation e a -> Validation e' a'
(define (validation-bimap f g v)
  (bf-bimap validation-bifunctor f g v))

;;; validation-map-errors : (e -> e') -> Validation e a -> Validation e' a
(define (validation-map-errors f v)
  (bf-first validation-bifunctor f v))

;;; validation-map-success : (a -> a') -> Validation e a -> Validation e a'
(define (validation-map-success g v)
  (bf-second validation-bifunctor g v))

;;; ============================================================
;;; Validation Applicative (Error-Accumulating)
;;; ============================================================
;;;
;;; Unlike Either which short-circuits on the first error,
;;; Validation accumulates all errors. This is the key feature
;;; that distinguishes Validation from Either.
;;;
;;; Note: Validation cannot be a Monad because bind would need
;;; to short-circuit (the continuation needs the success value).

;;; validation-fmap : (a -> b) -> Validation e a -> Validation e b
;;; Functor instance for Validation.
(define (validation-fmap f v)
  (if (validation-is-success? v)
      (validation-mk-success (f (from-validation-success v)))
      v))

;;; validation-pure : a -> Validation e a
;;; Wrap a value in Success.
(define (validation-pure x)
  (validation-mk-success x))

;;; validation-ap : Validation e (a -> b) -> Validation e a -> Validation e b
;;; Apply function wrapped in Validation, accumulating errors.
;;; Key difference from Either: both sides are evaluated and errors combined.
(define (validation-ap vf va)
  (cond
   ;; Both Success: apply the function
   [(and (validation-is-success? vf) (validation-is-success? va))
    (validation-mk-success ((from-validation-success vf)
                            (from-validation-success va)))]
   ;; Both Failure: combine error lists
   [(and (validation-is-failure? vf) (validation-is-failure? va))
    (validation-mk-failure (append (from-validation-failure vf)
                                   (from-validation-failure va)))]
   ;; One Success, one Failure: return the Failure
   [(validation-is-failure? vf) vf]
   [else va]))

;;; validation-lift2 : (a -> b -> c) -> Validation e a -> Validation e b -> Validation e c
;;; Lift a binary function over Validations.
(define (validation-lift2 f va vb)
  (validation-ap (validation-fmap (lambda (a) (lambda (b) (f a b))) va) vb))

;;; validation-lift3 : (a -> b -> c -> d) -> Validation e a -> Validation e b -> Validation e c -> Validation e d
(define (validation-lift3 f va vb vc)
  (validation-ap (validation-lift2 f va vb) vc))

;;; validation-sequence : (List (Validation e a)) -> Validation e (List a)
;;; Sequence a list of validations, accumulating all errors.
(define (validation-sequence vs)
  (if (null? vs)
      (validation-pure '())
      (validation-lift2 cons (car vs) (validation-sequence (cdr vs)))))

;;; validation-traverse : (a -> Validation e b) -> (List a) -> Validation e (List b)
;;; Map a validation-producing function over a list.
(define (validation-traverse f as)
  (validation-sequence (map f as)))

;;; validation-from-either : Either e a -> Validation e a
;;; Convert Either to Validation (wrapping error in list).
(define (validation-from-either e)
  (if (left? e)
      (validation-mk-failure (list (from-left e)))
      (validation-mk-success (from-right e))))

;;; validation-to-either : Validation e a -> Either (List e) a
;;; Convert Validation to Either.
(define (validation-to-either v)
  (if (validation-is-success? v)
      (right (from-validation-success v))
      (left (from-validation-failure v))))

;;; validate : (a -> Boolean) -> e -> a -> Validation e a
;;; Validate a value against a predicate.
(define (validate pred err x)
  (if (pred x)
      (validation-mk-success x)
      (validation-mk-failure (list err))))

;;; validate-all : (List (Pair (a -> Boolean) e)) -> a -> Validation e a
;;; Run multiple validations, accumulating all errors.
(define (validate-all validators x)
  (let* ([results (map (lambda (v) (validate (car v) (cdr v) x)) validators)]
         [failures (filter validation-is-failure? results)])
        (if (null? failures)
            (validation-mk-success x)
            (validation-mk-failure (apply append (map from-validation-failure failures))))))

;;; ============================================================
;;; These (Const/Tagged) Bifunctor
;;; ============================================================
;;;
;;; These a b = This a | That b | These a b
;;; A bifunctor that holds one, the other, or both values.

;;; make-this : a -> These a b
(define (make-this x)
  (list 'this x))

;;; make-that : b -> These a b
(define (make-that y)
  (list 'that y))

;;; make-these : a -> b -> These a b
(define (make-these x y)
  (list 'these x y))

;;; this? : These a b -> Boolean
(define (this? t)
  (and (pair? t) (eq? (car t) 'this)))

;;; that? : These a b -> Boolean
(define (that? t)
  (and (pair? t) (eq? (car t) 'that)))

;;; these? : These a b -> Boolean
(define (these? t)
  (and (pair? t) (eq? (car t) 'these)))

;;; from-this : These a b -> a
(define (from-this t)
  (cadr t))

;;; from-that : These a b -> b
(define (from-that t)
  (cadr t))

;;; from-these-fst : These a b -> a
(define (from-these-fst t)
  (cadr t))

;;; from-these-snd : These a b -> b
(define (from-these-snd t)
  (caddr t))

(define these-bifunctor
  (make-bifunctor
   ;; first
   (lambda (f t)
           (cond
            [(this? t) (make-this (f (from-this t)))]
            [(that? t) t]
            [(these? t) (make-these (f (from-these-fst t)) (from-these-snd t))]))
   ;; second
   (lambda (g t)
           (cond
            [(this? t) t]
            [(that? t) (make-that (g (from-that t)))]
            [(these? t) (make-these (from-these-fst t) (g (from-these-snd t)))]))))

;;; these-bimap : (a -> a') -> (b -> b') -> These a b -> These a' b'
(define (these-bimap f g t)
  (bf-bimap these-bifunctor f g t))

;;; ============================================================
;;; Law Verification
;;; ============================================================

;;; verify-bifunctor-identity : Bifunctor p -> p a b -> Boolean
;;; Check that bimap id id = id
(define (verify-bifunctor-identity bf x)
  (equal? x (bf-bimap bf identity identity x)))

;;; verify-bifunctor-first-identity : Bifunctor p -> p a b -> Boolean
;;; Check that first id = id
(define (verify-bifunctor-first-identity bf x)
  (equal? x (bf-first bf identity x)))

;;; verify-bifunctor-second-identity : Bifunctor p -> p a b -> Boolean
;;; Check that second id = id
(define (verify-bifunctor-second-identity bf x)
  (equal? x (bf-second bf identity x)))

;;; verify-bifunctor-first-second : Bifunctor p -> (a -> b) -> (c -> d) -> p a c -> Boolean
;;; Check that bimap f g = first f . second g
(define (verify-bifunctor-composition bf f g x)
  (equal? (bf-bimap bf f g x)
          (bf-first bf f (bf-second bf g x))))

;;; ============================================================
;;; Derived Operations
;;; ============================================================

;;; bf-void-left : Bifunctor p -> a -> p b c -> p a c
;;; Replace left contents with a constant value.
(define (bf-void-left bf x p)
  (bf-first bf (const x) p))

;;; bf-void-right : Bifunctor p -> c -> p a b -> p a c
;;; Replace right contents with a constant value.
(define (bf-void-right bf x p)
  (bf-second bf (const x) p))

;;; ============================================================
;;; Bifoldable
;;; ============================================================
;;;
;;; class Bifoldable p where
;;;   bifoldMap :: Monoid m => (a -> m) -> (b -> m) -> p a b -> m
;;;
;;; Bifoldable allows folding over both type parameters of a bifunctor.

;;; make-bifoldable : ((a -> m) -> (b -> m) -> Monoid m -> p a b -> m) -> Bifoldable p
(define (make-bifoldable bifold-map-fn)
  (list 'bifoldable bifold-map-fn))

;;; bifoldable? : Any -> Boolean
(define (bifoldable? x)
  (and (pair? x) (eq? (car x) 'bifoldable)))

;;; bifoldable-bifold-map : Bifoldable p -> ((a -> m) -> (b -> m) -> Monoid m -> p a b -> m)
(define (bifoldable-bifold-map bf)
  (list-ref bf 1))

;;; bifold-map : Bifoldable p -> (a -> m) -> (b -> m) -> Monoid m -> p a b -> m
(define (bifold-map bf f g monoid pab)
  ((bifoldable-bifold-map bf) f g monoid pab))

;;; bifold : Bifoldable p -> Monoid m -> p m m -> m
;;; Fold both sides using their own values.
(define (bifold bf monoid pmm)
  (bifold-map bf identity identity monoid pmm))

;;; bifold-left : Bifoldable p -> (a -> m) -> Monoid m -> p a b -> m
;;; Fold only the left side.
(define (bifold-left bf f monoid pab)
  (bifold-map bf f (const (monoid-empty monoid)) monoid pab))

;;; bifold-right : Bifoldable p -> (b -> m) -> Monoid m -> p a b -> m
;;; Fold only the right side.
(define (bifold-right bf g monoid pab)
  (bifold-map bf (const (monoid-empty monoid)) g monoid pab))

;;; bitoList : Bifoldable p -> p a a -> List a
;;; Collect all values into a list.
(define (bitoList bf paa)
  (bifold-map bf list list list-monoid paa))

;;; bilength : Bifoldable p -> p a b -> Int
;;; Count the number of elements.
(define (bilength bf pab)
  (bifold-map bf (const 1) (const 1) sum-monoid pab))

;;; binull : Bifoldable p -> p a b -> Boolean
;;; Check if both sides are "empty" (no values).
(define (binull bf pab)
  (bifold-map bf (const #f) (const #f) all-monoid pab))

;;; ============================================================
;;; Bifoldable Instances
;;; ============================================================

;;; pair-bifoldable : Bifoldable Pair
(define pair-bifoldable
  (make-bifoldable
   (lambda (f g monoid p)
           (mappend monoid (f (car p)) (g (cdr p))))))

;;; either-bifoldable : Bifoldable Either
(define either-bifoldable
  (make-bifoldable
   (lambda (f g monoid e)
           (if (left? e)
               (f (from-left e))
               (g (from-right e))))))

;;; these-bifoldable : Bifoldable These
(define these-bifoldable
  (make-bifoldable
   (lambda (f g monoid t)
           (cond
            [(this? t) (f (from-this t))]
            [(that? t) (g (from-that t))]
            [else (mappend monoid (f (from-these-fst t)) (g (from-these-snd t)))]))))

;;; ============================================================
;;; Bitraversable
;;; ============================================================
;;;
;;; class (Bifunctor p, Bifoldable p) => Bitraversable p where
;;;   bitraverse :: Applicative f => (a -> f c) -> (b -> f d) -> p a b -> f (p c d)
;;;
;;; Bitraversable allows effectful traversal over both type parameters.

;;; make-bitraversable : Bifunctor p -> Bifoldable p -> ((a -> f c) -> (b -> f d) -> Applicative f -> p a b -> f (p c d)) -> Bitraversable p
(define (make-bitraversable bifunctor bifoldable bitraverse-fn)
  (list 'bitraversable bifunctor bifoldable bitraverse-fn))

;;; bitraversable? : Any -> Boolean
(define (bitraversable? x)
  (and (pair? x) (eq? (car x) 'bitraversable)))

;;; bitraversable-bifunctor : Bitraversable p -> Bifunctor p
(define (bitraversable-bifunctor bt)
  (list-ref bt 1))

;;; bitraversable-bifoldable : Bitraversable p -> Bifoldable p
(define (bitraversable-bifoldable bt)
  (list-ref bt 2))

;;; bitraversable-bitraverse : Bitraversable p -> ((a -> f c) -> (b -> f d) -> Applicative f -> p a b -> f (p c d))
(define (bitraversable-bitraverse bt)
  (list-ref bt 3))

;;; bitraverse : Bitraversable p -> (a -> f c) -> (b -> f d) -> Applicative f -> p a b -> f (p c d)
(define (bitraverse bt f g app pab)
  ((bitraversable-bitraverse bt) f g app pab))

;;; bisequence : Bitraversable p -> Applicative f -> p (f a) (f b) -> f (p a b)
;;; Sequence both sides.
(define (bisequence bt app pff)
  (bitraverse bt identity identity app pff))

;;; bifor : Bitraversable p -> Applicative f -> p a b -> (a -> f c) -> (b -> f d) -> f (p c d)
;;; Flipped bitraverse.
(define (bifor bt app pab f g)
  (bitraverse bt f g app pab))

;;; ============================================================
;;; Bitraversable Instances
;;; ============================================================

;;; pair-bitraversable : Bitraversable Pair
(define pair-bitraversable
  (make-bitraversable
   pair-bifunctor
   pair-bifoldable
   (lambda (f g app p)
           ;; Traverse both elements, combine with pair constructor
           (app-lift2-uncurried app cons (f (car p)) (g (cdr p))))))

;;; either-bitraversable : Bitraversable Either
(define either-bitraversable
  (make-bitraversable
   either-bifunctor
   either-bifoldable
   (lambda (f g app e)
           (if (left? e)
               (app-fmap app left (f (from-left e)))
               (app-fmap app right (g (from-right e)))))))

;;; these-bitraversable : Bitraversable These
(define these-bitraversable
  (make-bitraversable
   these-bifunctor
   these-bifoldable
   (lambda (f g app t)
           (cond
            [(this? t) (app-fmap app make-this (f (from-this t)))]
            [(that? t) (app-fmap app make-that (g (from-that t)))]
            [else (app-lift2-uncurried app make-these
                                       (f (from-these-fst t))
                                       (g (from-these-snd t)))]))))

;;; ============================================================
;;; Example Usage (for documentation)
;;; ============================================================
;;;
;;; ;; Pair bifunctor
;;; (pair-bimap add1 string-length (cons 1 "hello"))
;;; ; => (2 . 5)
;;;
;;; ;; Either bifunctor
;;; (either-bimap string-upcase add1 (left "error"))
;;; ; => (left "ERROR")
;;; (either-bimap string-upcase add1 (right 5))
;;; ; => (right 6)
;;;
;;; ;; Validation bifunctor
;;; (validation-bimap string-upcase add1 (validation-mk-failure '("bad input")))
;;; ; => (failure ("BAD INPUT"))
;;;
;;; ;; These bifunctor
;;; (these-bimap add1 string-upcase (make-these 1 "hi"))
;;; ; => (these 2 "HI")
