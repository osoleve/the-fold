;;; fabric/stitches/fp/traversable.ss — Traversable and Foldable
;;;
;;; Traversable structures can be traversed while performing effects.
;;; Foldable structures can be reduced to a summary value.
;;;
;;; Foldable has:
;;;   fold-map : Monoid m => (a -> m) -> t a -> m
;;;   foldr    : (a -> b -> b) -> b -> t a -> b
;;;
;;; Traversable has (extends Foldable):
;;;   traverse : Applicative f => (a -> f b) -> t a -> f (t b)
;;;   sequence : Applicative f => t (f a) -> f (t a)
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Features:
;;;   - Foldable operations (foldr, foldl, fold-map)
;;;   - Traversable operations (traverse, sequence)
;;;   - Derived operations (for_, traverse_, mapM, etc.)
;;;   - Instances for List, Maybe, Either, Pair
;;;   - Generic traversal utilities
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - fp/combinators.ss
;;;   - fp/algebraic.ss (for Monoid)

(load "fabric/stitches/prelude.ss")
(load "fabric/stitches/fp/combinators.ss")
(load "fabric/stitches/fp/algebraic.ss")

;;; ============================================================
;;; Monoid Compatibility Layer
;;; ============================================================
;;;
;;; Monoids are defined in algebraic.ss with full structure:
;;;   (list 'monoid name mempty op)
;;;
;;; We provide monoid-append as an alias for monoid-op for backward
;;; compatibility with existing code that uses the append terminology.

;;; monoid-append : Monoid m -> (a -> a -> a)
;;; Alias for monoid-op (the binary operation).
(define monoid-append monoid-op)

;;; Additional monoids not in algebraic.ss
;;; Note: list-monoid, sum-monoid, product-monoid, string-monoid,
;;;       first-monoid, last-monoid, endo-monoid, dual-monoid
;;;       are all defined in algebraic.ss

;;; and-monoid : Monoid Bool (conjunction)
;;; Same as all-monoid from algebraic.ss
(define and-monoid all-monoid)

;;; or-monoid : Monoid Bool (disjunction)
;;; Same as any-monoid from algebraic.ss
(define or-monoid any-monoid)

;;; ============================================================
;;; Simple Monoids (unwrapped versions for convenience)
;;; ============================================================
;;;
;;; The algebraic.ss versions use newtypes (First, Last, Dual, Endo).
;;; These simple versions work directly with raw values.

;;; simple-first-monoid : Monoid (Maybe a)
;;; Keeps the first non-Nothing value (no First wrapper).
(define simple-first-monoid
  (make-monoid
   'simple-first
   nothing
   (lambda (a b) (if (just? a) a b))))

;;; simple-last-monoid : Monoid (Maybe a)
;;; Keeps the last non-Nothing value (no Last wrapper).
(define simple-last-monoid
  (make-monoid
   'simple-last
   nothing
   (lambda (a b) (if (just? b) b a))))

;;; simple-dual-monoid : Monoid a -> Monoid a
;;; Reverses append order without Dual wrapper.
(define (simple-dual-monoid m)
  (make-monoid
   (string->symbol (string-append "simple-dual-" (symbol->string (monoid-name m))))
   (monoid-empty m)
   (lambda (a b) ((monoid-op m) b a))))

;;; simple-endo-monoid : Monoid (a -> a)
;;; Endomorphisms under composition (no Endo wrapper).
(define simple-endo-monoid
  (make-monoid
   'simple-endo
   identity
   (lambda (f g) (lambda (x) (f (g x))))))

;;; ============================================================
;;; Applicative Interface (for Traversable)
;;; ============================================================
;;;
;;; Applicatives are represented as tagged lists for consistency
;;; with other typeclass dictionaries in the FP toolkit:
;;;   (list 'applicative pure ap)
;;;
;;; pure : a -> f a
;;; ap   : f (a -> b) -> f a -> f b

;;; make-applicative : (a -> f a) -> (f (a -> b) -> f a -> f b) -> Applicative f
(define (make-applicative pure ap)
  (list 'applicative pure ap))

;;; applicative? : Any -> Boolean
(define (applicative? x)
  (and (pair? x) (eq? (car x) 'applicative)))

;;; app-pure : Applicative f -> a -> f a
(define (app-pure app) (list-ref app 1))

;;; app-ap : Applicative f -> f (a -> b) -> f a -> f b
(define (app-ap app) (list-ref app 2))

;;; app-fmap : Applicative f -> (a -> b) -> f a -> f b
;;; Derived from pure and ap.
(define (app-fmap app f fa)
  ((app-ap app) ((app-pure app) f) fa))

;;; app-lift2 : Applicative f -> (a -> b -> c) -> f a -> f b -> f c
;;; Note: f must be curried (a -> b -> c), not (a b -> c)
(define (app-lift2 app f fa fb)
  ((app-ap app) (app-fmap app f fa) fb))

;;; app-lift2-uncurried : Applicative f -> (a b -> c) -> f a -> f b -> f c
;;; For use with Scheme's multi-arg functions.
(define (app-lift2-uncurried app f fa fb)
  (app-lift2 app (lambda (a) (lambda (b) (f a b))) fa fb))

;;; ============================================================
;;; Common Applicatives
;;; ============================================================

;;; identity-applicative : Applicative Identity
(define identity-applicative
  (make-applicative identity (lambda (f a) (f a))))

;;; maybe-applicative : Applicative Maybe
(define maybe-applicative
  (make-applicative
   just
   (lambda (mf ma)
           (if (nothing? mf)
               nothing
               (if (nothing? ma)
                   nothing
                   (just ((from-just mf) (from-just ma))))))))

;;; list-applicative : Applicative List
;;; Cartesian product style.
(define list-applicative
  (make-applicative
   list
   (lambda (fs as)
           (apply append (map (lambda (f) (map f as)) fs)))))

;;; either-applicative : Applicative (Either e)
(define either-applicative
  (make-applicative
   right
   (lambda (ef ea)
           (if (left? ef)
               ef
               (if (left? ea)
                   ea
                   (right ((from-right ef) (from-right ea))))))))

;;; const-applicative : Monoid m -> Applicative (Const m)
;;; Const m a = m (ignores a).
(define (const-applicative monoid)
  (make-applicative
   (lambda (a) (monoid-empty monoid))
   (lambda (cf ca) ((monoid-append monoid) cf ca))))

;;; ============================================================
;;; Foldable: List
;;; ============================================================

;;; list-foldr : (a -> b -> b) -> b -> List a -> b
(define (list-foldr f init lst)
  (if (null? lst)
      init
      (f (car lst) (list-foldr f init (cdr lst)))))

;;; list-foldl : (b -> a -> b) -> b -> List a -> b
(define (list-foldl f init lst)
  (if (null? lst)
      init
      (list-foldl f (f init (car lst)) (cdr lst))))

;;; list-fold-map : Monoid m -> (a -> m) -> List a -> m
(define (list-fold-map monoid f lst)
  (list-foldr (lambda (a acc) ((monoid-append monoid) (f a) acc))
              (monoid-empty monoid)
              lst))

;;; list-fold : Monoid m -> List m -> m
;;; Fold a list of monoid values.
(define (list-fold monoid lst)
  (list-fold-map monoid identity lst))

;;; ============================================================
;;; Foldable: Maybe
;;; ============================================================

;;; maybe-foldr : (a -> b -> b) -> b -> Maybe a -> b
(define (maybe-foldr f init m)
  (if (nothing? m)
      init
      (f (from-just m) init)))

;;; maybe-foldl : (b -> a -> b) -> b -> Maybe a -> b
(define (maybe-foldl f init m)
  (if (nothing? m)
      init
      (f init (from-just m))))

;;; maybe-fold-map : Monoid m -> (a -> m) -> Maybe a -> m
(define (maybe-fold-map monoid f m)
  (if (nothing? m)
      (monoid-empty monoid)
      (f (from-just m))))

;;; ============================================================
;;; Foldable: Either
;;; ============================================================

;;; either-foldr : (a -> b -> b) -> b -> Either e a -> b
(define (either-foldr f init e)
  (if (left? e)
      init
      (f (from-right e) init)))

;;; either-foldl : (b -> a -> b) -> b -> Either e a -> b
(define (either-foldl f init e)
  (if (left? e)
      init
      (f init (from-right e))))

;;; either-fold-map : Monoid m -> (a -> m) -> Either e a -> m
(define (either-fold-map monoid f e)
  (if (left? e)
      (monoid-empty monoid)
      (f (from-right e))))

;;; ============================================================
;;; Foldable: Pair (folds over second element)
;;; ============================================================

;;; pair-foldr : (a -> b -> b) -> b -> (c, a) -> b
(define (pair-foldr f init p)
  (f (cdr p) init))

;;; pair-foldl : (b -> a -> b) -> b -> (c, a) -> b
(define (pair-foldl f init p)
  (f init (cdr p)))

;;; pair-fold-map : Monoid m -> (a -> m) -> (c, a) -> m
(define (pair-fold-map monoid f p)
  (f (cdr p)))

;;; ============================================================
;;; Traversable: List
;;; ============================================================

;;; list-traverse : Applicative f -> (a -> f b) -> List a -> f (List b)
(define (list-traverse app f lst)
  (if (null? lst)
      ((app-pure app) '())
      (app-lift2-uncurried app cons
                           (f (car lst))
                           (list-traverse app f (cdr lst)))))

;;; list-sequence : Applicative f -> List (f a) -> f (List a)
(define (list-sequence app lst)
  (list-traverse app identity lst))

;;; list-for : Applicative f -> List a -> (a -> f b) -> f (List b)
;;; Flipped traverse.
(define (list-for app lst f)
  (list-traverse app f lst))

;;; ============================================================
;;; Traversable: Maybe
;;; ============================================================

;;; maybe-traverse : Applicative f -> (a -> f b) -> Maybe a -> f (Maybe b)
(define (maybe-traverse app f m)
  (if (nothing? m)
      ((app-pure app) nothing)
      (app-fmap app just (f (from-just m)))))

;;; maybe-sequence : Applicative f -> Maybe (f a) -> f (Maybe a)
(define (maybe-sequence app m)
  (maybe-traverse app identity m))

;;; ============================================================
;;; Traversable: Either
;;; ============================================================

;;; either-traverse : Applicative f -> (a -> f b) -> Either e a -> f (Either e b)
(define (either-traverse app f e)
  (if (left? e)
      ((app-pure app) e)
      (app-fmap app right (f (from-right e)))))

;;; either-sequence : Applicative f -> Either e (f a) -> f (Either e a)
(define (either-sequence app e)
  (either-traverse app identity e))

;;; ============================================================
;;; Traversable: Pair
;;; ============================================================

;;; pair-traverse : Applicative f -> (a -> f b) -> (c, a) -> f (c, b)
(define (pair-traverse app f p)
  (app-fmap app (lambda (b) (cons (car p) b))
            (f (cdr p))))

;;; pair-sequence : Applicative f -> (c, f a) -> f (c, a)
(define (pair-sequence app p)
  (pair-traverse app identity p))

;;; ============================================================
;;; Derived Foldable Operations
;;; ============================================================

;;; to-list : Foldable t => t a -> List a
;;; Generic conversion using foldr.
(define (to-list-with foldr-fn ta)
  (foldr-fn cons '() ta))

;;; list-to-list : List a -> List a
(define (list-to-list lst) (to-list-with list-foldr lst))

;;; maybe-to-list : Maybe a -> List a
(define (maybe-to-list m) (to-list-with maybe-foldr m))

;;; either-to-list : Either e a -> List a
(define (either-to-list e) (to-list-with either-foldr e))

;;; null? for foldables
(define (foldable-null? foldr-fn ta)
  (foldr-fn (lambda (a b) #f) #t ta))

;;; length for foldables
(define (foldable-length foldr-fn ta)
  (foldr-fn (lambda (a acc) (+ 1 acc)) 0 ta))

;;; elem? : Eq a => a -> t a -> Bool
(define (foldable-elem? foldr-fn x ta)
  (foldr-fn (lambda (a acc) (or (equal? a x) acc)) #f ta))

;;; find : (a -> Bool) -> t a -> Maybe a
(define (foldable-find foldr-fn pred ta)
  (foldr-fn (lambda (a acc)
                    (if (pred a) (just a) acc))
            nothing
            ta))

;;; all : (a -> Bool) -> t a -> Bool
(define (foldable-all foldr-fn pred ta)
  (foldr-fn (lambda (a acc) (and (pred a) acc)) #t ta))

;;; any : (a -> Bool) -> t a -> Bool
(define (foldable-any foldr-fn pred ta)
  (foldr-fn (lambda (a acc) (or (pred a) acc)) #f ta))

;;; sum : Num a => t a -> a
(define (foldable-sum foldr-fn ta)
  (foldr-fn + 0 ta))

;;; product : Num a => t a -> a
(define (foldable-product foldr-fn ta)
  (foldr-fn * 1 ta))

;;; maximum : Ord a => t a -> Maybe a
(define (foldable-maximum foldr-fn ta)
  (foldr-fn (lambda (a acc)
                    (if (nothing? acc)
                        (just a)
                        (just (max a (from-just acc)))))
            nothing
            ta))

;;; minimum : Ord a => t a -> Maybe a
(define (foldable-minimum foldr-fn ta)
  (foldr-fn (lambda (a acc)
                    (if (nothing? acc)
                        (just a)
                        (just (min a (from-just acc)))))
            nothing
            ta))

;;; ============================================================
;;; Derived Traversable Operations
;;; ============================================================

;;; traverse_ : Applicative f -> (a -> f b) -> t a -> f ()
;;; Traverse for effects only, discarding results.
(define (list-traverse_ app f lst)
  (if (null? lst)
      ((app-pure app) '())
      ((app-ap app)
       (app-fmap app (lambda (x) (lambda (y) '())) (f (car lst)))
       (list-traverse_ app f (cdr lst)))))

;;; for_ : Applicative f -> t a -> (a -> f b) -> f ()
(define (list-for_ app lst f)
  (list-traverse_ app f lst))

;;; mapM : Monad m -> (a -> m b) -> List a -> m (List b)
;;; Same as traverse with monad.
(define list-mapM list-traverse)

;;; forM : Monad m -> List a -> (a -> m b) -> m (List b)
(define (list-forM app lst f)
  (list-traverse app f lst))

;;; filter-with-effect : Applicative f -> (a -> f Bool) -> List a -> f (List a)
;;; Filter a list with an effectful predicate.
(define (list-filter-with-effect app pred lst)
  (if (null? lst)
      ((app-pure app) '())
      (app-lift2 app
                 (lambda (keep?) (lambda (rest)
                                         (if keep? (cons (car lst) rest) rest)))
                 (pred (car lst))
                 (list-filter-with-effect app pred (cdr lst)))))

;;; ============================================================
;;; Zip With Applicative
;;; ============================================================

;;; The ZipList applicative applies functions element-wise.
;;;
;;; For ZipList, pure x needs to return an infinite stream of x values.
;;; We use a lazy stream representation: (cons head tail-thunk)
;;; where tail-thunk is a procedure that returns the rest of the stream.

;;; make-lazy-stream : a -> (() -> LazyStream a) -> LazyStream a
(define (make-lazy-stream head tail-thunk)
  (cons head tail-thunk))

;;; lazy-stream-head : LazyStream a -> a
(define lazy-stream-head car)

;;; lazy-stream-tail : LazyStream a -> LazyStream a
(define (lazy-stream-tail s)
  ((cdr s)))

;;; lazy-stream-repeat : a -> LazyStream a
;;; Create an infinite stream repeating the same value.
(define (lazy-stream-repeat x)
  (make-lazy-stream x (lambda () (lazy-stream-repeat x))))

;;; lazy-stream-take : Number -> LazyStream a -> List a
;;; Take first n elements as a finite list.
(define (lazy-stream-take n s)
  (if (= n 0)
      '()
      (cons (lazy-stream-head s)
            (lazy-stream-take (- n 1) (lazy-stream-tail s)))))

;;; zip-list-applicative : Applicative ZipList
;;; ZipList can work with either finite lists or lazy streams.
;;; When mixing, the result is finite (length of shorter).
(define zip-list-applicative
  (make-applicative
   ;; pure returns an infinite lazy stream
   lazy-stream-repeat
   ;; ap zips element-wise, handling both lazy streams and lists
   (lambda (fs as)
           (cond
            ;; Null/empty terminates
            [(null? fs) '()]
            [(null? as) '()]
            ;; Both are lazy streams (pair with procedure cdr)
            [(and (pair? fs) (procedure? (cdr fs))
                  (pair? as) (procedure? (cdr as)))
             (make-lazy-stream
              ((lazy-stream-head fs) (lazy-stream-head as))
              (lambda () ((app-ap zip-list-applicative)
                          (lazy-stream-tail fs)
                          (lazy-stream-tail as))))]
            ;; fs is lazy, as is list
            [(and (pair? fs) (procedure? (cdr fs)))
             (cons ((lazy-stream-head fs) (car as))
                   ((app-ap zip-list-applicative) (lazy-stream-tail fs) (cdr as)))]
            ;; fs is list, as is lazy
            [(and (pair? as) (procedure? (cdr as)))
             (cons ((car fs) (lazy-stream-head as))
                   ((app-ap zip-list-applicative) (cdr fs) (lazy-stream-tail as)))]
            ;; Both are regular lists
            [else
             (cons ((car fs) (car as))
                   ((app-ap zip-list-applicative) (cdr fs) (cdr as)))]))))

;;; zip-list-to-list : Number -> ZipList a -> List a
;;; Convert a (possibly lazy) ZipList to a finite list.
(define (zip-list-to-list n zl)
  (cond
   [(null? zl) '()]
   [(= n 0) '()]
   [(procedure? (cdr zl))
    ;; Lazy stream
    (cons (lazy-stream-head zl)
          (zip-list-to-list (- n 1) (lazy-stream-tail zl)))]
   [else
    ;; Regular list
    (cons (car zl)
          (zip-list-to-list (- n 1) (cdr zl)))]))

;;; ============================================================
;;; Accumulating Traversals
;;; ============================================================

;;; scan-left : (b -> a -> b) -> b -> List a -> List b
;;; Like foldl but keeps intermediate results.
(define (scan-left f init lst)
  (if (null? lst)
      (list init)
      (cons init (scan-left f (f init (car lst)) (cdr lst)))))

;;; scan-right : (a -> b -> b) -> b -> List a -> List b
;;; Like foldr but keeps intermediate results.
(define (scan-right f init lst)
  (if (null? lst)
      (list init)
      (let ([rest (scan-right f init (cdr lst))])
           (cons (f (car lst) (car rest)) rest))))

;;; ============================================================
;;; Indexed Traversals
;;; ============================================================

;;; itraverse : Applicative f -> (Int -> a -> f b) -> List a -> f (List b)
;;; Traverse with index.
(define (list-itraverse app f lst)
  (define (go i lst)
    (if (null? lst)
        ((app-pure app) '())
        (app-lift2-uncurried app cons
                             (f i (car lst))
                             (go (+ i 1) (cdr lst)))))
  (go 0 lst))

;;; ifoldr : (Int -> a -> b -> b) -> b -> List a -> b
;;; Fold with index.
(define (list-ifoldr f init lst)
  (define (go i lst)
    (if (null? lst)
        init
        (f i (car lst) (go (+ i 1) (cdr lst)))))
  (go 0 lst))

;;; ifoldl : (Int -> b -> a -> b) -> b -> List a -> b
;;; Left fold with index.
(define (list-ifoldl f init lst)
  (define (go i acc lst)
    (if (null? lst)
        acc
        (go (+ i 1) (f i acc (car lst)) (cdr lst))))
  (go 0 init lst))

;;; ============================================================
;;; Example Usage (for documentation)
;;; ============================================================
;;;
;;; ;; Traverse a list with Maybe (stops on Nothing)
;;; (define (safe-sqrt x)
;;;   (if (< x 0) nothing (just (sqrt x))))
;;; (list-traverse maybe-applicative safe-sqrt '(4 9 16))
;;; ; => (just (2 3 4))
;;; (list-traverse maybe-applicative safe-sqrt '(4 -1 16))
;;; ; => nothing
;;;
;;; ;; Fold a list to sum
;;; (list-fold-map sum-monoid identity '(1 2 3 4 5))
;;; ; => 15
;;;
;;; ;; Sequence a list of Maybes
;;; (list-sequence maybe-applicative (list (just 1) (just 2) (just 3)))
;;; ; => (just (1 2 3))
;;; (list-sequence maybe-applicative (list (just 1) nothing (just 3)))
;;; ; => nothing
;;;
;;; ;; Use traverse to validate and transform
;;; (define (validate-positive x)
;;;   (if (> x 0) (right x) (left "must be positive")))
;;; (list-traverse either-applicative validate-positive '(1 2 3))
;;; ; => (right (1 2 3))
;;; (list-traverse either-applicative validate-positive '(1 -2 3))
;;; ; => (left "must be positive")
