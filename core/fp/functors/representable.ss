;;; fabric/stitches/fp/representable.ss — Representable and Distributive Functors
;;;
;;; Representable functors are those isomorphic to ((->) r) for some type r.
;;; This enables powerful operations like tabulate (memoization) and index
;;; (efficient lookup).
;;;
;;; Distributive functors are the dual of Traversable. They allow "flipping"
;;; the order of nested functors. Every Representable functor is Distributive.
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Features:
;;;   - Representable typeclass (tabulate, index)
;;;   - Distributive typeclass (distribute, collect)
;;;   - Instances for Identity, Pair, Function, Stream
;;;   - Derived operations (tabulated, indexedMap, etc.)
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - fp/typeclasses.ss
;;;   - fp/stream.ss

(load "core/prelude.ss")
(load "core/fp/typeclasses/typeclasses.ss")
(load "core/fp/data/stream.ss")

;;; ============================================================
;;; Representable Functor
;;; ============================================================
;;;
;;; class Functor f => Representable f where
;;;   type Rep f :: *                    -- the representation type (key)
;;;   tabulate :: (Rep f -> a) -> f a    -- build from function
;;;   index    :: f a -> Rep f -> a      -- lookup by key
;;;
;;; Laws:
;;;   tabulate . index = id
;;;   index . tabulate = id
;;;   fmap f = tabulate . fmap f . index
;;;
;;; Intuition: A Representable functor is a "container" where each position
;;; has a fixed address (Rep f). You can build one by providing a function
;;; from addresses to values (tabulate), or extract values by address (index).

;;; make-representable : Functor f -> Symbol -> (f a -> key -> a) -> ((key -> a) -> f a) -> Representable f
;;; Create a Representable instance.
;;;   functor  : the underlying Functor
;;;   rep-name : name describing the representation type
;;;   index-fn : f a -> key -> a
;;;   tabulate-fn : (key -> a) -> f a
(define (make-representable functor rep-name index-fn tabulate-fn)
  (list 'representable functor rep-name index-fn tabulate-fn))

;;; representable? : Any -> Boolean
(define (representable? x)
  (and (pair? x) (eq? (car x) 'representable)))

;;; representable-functor : Representable f -> Functor f
(define (representable-functor r)
  (list-ref r 1))

;;; representable-rep-name : Representable f -> Symbol
(define (representable-rep-name r)
  (list-ref r 2))

;;; representable-index : Representable f -> (f a -> key -> a)
(define (representable-index r)
  (list-ref r 3))

;;; representable-tabulate : Representable f -> ((key -> a) -> f a)
(define (representable-tabulate r)
  (list-ref r 4))

;;; index : Representable f -> f a -> key -> a
;;; Extract the value at a given key.
(define (rep-index r fa key)
  ((representable-index r) fa key))

;;; tabulate : Representable f -> (key -> a) -> f a
;;; Build a structure by providing a function from keys to values.
(define (rep-tabulate r f)
  ((representable-tabulate r) f))

;;; ============================================================
;;; Representable Derived Operations
;;; ============================================================

;;; tabulated : Representable f -> f key
;;; A structure where each position contains its own key.
;;; Equivalent to: tabulate id
(define (rep-tabulated r)
  (rep-tabulate r identity))

;;; indexed-map : Representable f -> (key -> a -> b) -> f a -> f b
;;; Map with access to the key at each position.
(define (rep-indexed-map r f fa)
  (rep-tabulate r (lambda (key) (f key (rep-index r fa key)))))

;;; ap-rep : Representable f -> f (a -> b) -> f a -> f b
;;; Applicative 'ap' derived from Representable.
;;; Every Representable has Applicative.
(define (rep-ap r ff fa)
  (rep-tabulate r (lambda (key) ((rep-index r ff key) (rep-index r fa key)))))

;;; pure-rep : Representable f -> a -> f a
;;; Applicative 'pure' derived from Representable.
(define (rep-pure r a)
  (rep-tabulate r (const a)))

;;; bind-rep : Representable f -> f a -> (a -> f b) -> f b
;;; Monadic bind derived from Representable.
;;; Every Representable has Monad (specifically, the Reader monad structure).
(define (rep-bind r fa f)
  (rep-tabulate r (lambda (key) (rep-index r (f (rep-index r fa key)) key))))

;;; ask-rep : Representable f -> f key
;;; Reader's 'ask' derived from Representable.
;;; Returns the key at each position.
(define rep-ask rep-tabulated)

;;; local-rep : Representable f -> (key -> key) -> f a -> f a
;;; Reader's 'local' derived from Representable.
;;; Modify the key before indexing.
(define (rep-local r f fa)
  (rep-tabulate r (lambda (key) (rep-index r fa (f key)))))

;;; ============================================================
;;; Distributive Functor
;;; ============================================================
;;;
;;; class Functor g => Distributive g where
;;;   distribute :: Functor f => f (g a) -> g (f a)
;;;   collect    :: Functor f => (a -> g b) -> f a -> g (f b)
;;;
;;; Laws:
;;;   distribute . distribute = id
;;;   distribute . fmap (fmap f) = fmap (fmap f) . distribute
;;;
;;; The dual of Traversable:
;;;   - Traversable: t (f a) -> f (t a) (sequence with effects)
;;;   - Distributive: f (g a) -> g (f a) (flip structure, no effects needed)
;;;
;;; Key insight: Distributive functors have a fixed "shape" known statically.
;;; This is why streams and functions are distributive but lists are not.

;;; make-distributive : Functor g -> (Functor f -> f (g a) -> g (f a)) -> Distributive g
;;; Create a Distributive instance.
;;;   functor      : the underlying Functor
;;;   distribute-fn : takes a Functor dict and returns the distribute function
(define (make-distributive functor distribute-fn)
  (list 'distributive functor distribute-fn))

;;; distributive? : Any -> Boolean
(define (distributive? x)
  (and (pair? x) (eq? (car x) 'distributive)))

;;; distributive-functor : Distributive g -> Functor g
(define (distributive-functor d)
  (list-ref d 1))

;;; distributive-distribute-fn : Distributive g -> (Functor f -> f (g a) -> g (f a))
(define (distributive-distribute-fn d)
  (list-ref d 2))

;;; distribute : Distributive g -> Functor f -> f (g a) -> g (f a)
;;; Flip the nesting order of functors.
(define (dist-distribute d f-functor fga)
  ((distributive-distribute-fn d) f-functor fga))

;;; collect : Distributive g -> Functor f -> (a -> g b) -> f a -> g (f b)
;;; Map and then distribute.
(define (dist-collect d f-functor agb fa)
  (dist-distribute d f-functor (fmap f-functor agb fa)))

;;; cotraverse : Distributive g -> Functor f -> (f a -> b) -> f (g a) -> g b
;;; Cotraverse extracts from distributed structure.
(define (dist-cotraverse d f-functor fab fga)
  (fmap (distributive-functor d) fab (dist-distribute d f-functor fga)))

;;; ============================================================
;;; Representable => Distributive
;;; ============================================================
;;;
;;; Every Representable functor is Distributive.
;;; distribute = tabulate . flip (fmap . index)
;;;
;;; This is the fundamental insight: if you know the structure's shape
;;; (via Rep), you can always flip the nesting.

;;; representable->distributive : Representable f -> Distributive f
;;; Derive Distributive from Representable.
(define (representable->distributive r)
  (make-distributive
   (representable-functor r)
   (lambda (f-functor fga)
           ;; distribute fga = tabulate (\k -> fmap (\ga -> index ga k) fga)
           (rep-tabulate r
                         (lambda (key)
                                 (fmap f-functor
                                       (lambda (ga) (rep-index r ga key))
                                       fga))))))

;;; ============================================================
;;; Identity Functor (local definition)
;;; ============================================================
;;;
;;; Identity wraps a value with no additional structure.
;;; Identity a = a (we represent it as just the value itself)

;;; identity-functor : Functor Identity
;;; Since Identity a = a, fmap f x = f x
(define identity-functor
  (make-functor (lambda (f x) (f x))))

;;; ============================================================
;;; Identity Representable
;;; ============================================================
;;;
;;; Identity is representable with Rep = () (unit type).
;;; Only one position, so one key.

;;; identity-representable : Representable Identity
;;; Identity a ≅ (() -> a)
(define identity-representable
  (make-representable
   identity-functor
   'unit
   ;; index : Identity a -> () -> a
   (lambda (ia _unit) ia)
   ;; tabulate : (() -> a) -> Identity a
   (lambda (f) (f '()))))

;;; identity-distributive : Distributive Identity
(define identity-distributive
  (representable->distributive identity-representable))

;;; ============================================================
;;; Pair Representable
;;; ============================================================
;;;
;;; Pair a = (a, a) is representable with Rep = Bool.
;;; Two positions: #t for first, #f for second.

;;; make-pair-rep : a -> a -> Pair a
(define (make-pair-rep fst snd)
  (cons fst snd))

;;; pair-rep-fst : Pair a -> a
(define pair-rep-fst car)

;;; pair-rep-snd : Pair a -> a
(define pair-rep-snd cdr)

;;; pair-rep-functor : Functor Pair
(define pair-rep-functor
  (make-functor
   (lambda (f p)
           (make-pair-rep (f (pair-rep-fst p)) (f (pair-rep-snd p))))))

;;; pair-representable : Representable Pair
;;; Pair a ≅ (Bool -> a)
(define pair-representable
  (make-representable
   pair-rep-functor
   'bool
   ;; index : Pair a -> Bool -> a
   (lambda (p b) (if b (pair-rep-fst p) (pair-rep-snd p)))
   ;; tabulate : (Bool -> a) -> Pair a
   (lambda (f) (make-pair-rep (f #t) (f #f)))))

;;; pair-distributive : Distributive Pair
(define pair-distributive
  (representable->distributive pair-representable))

;;; ============================================================
;;; Function Representable
;;; ============================================================
;;;
;;; ((->) r) is trivially representable with Rep = r.
;;; Functions are the "canonical" representable functor.

;;; function-representable : r -> Representable ((->) r)
;;; Note: We parameterize by a dummy value since we can't create
;;; a proper polymorphic representation in Scheme.
(define (make-function-representable)
  (make-representable
   function-functor
   'key
   ;; index : (r -> a) -> r -> a
   (lambda (f key) (f key))
   ;; tabulate : (r -> a) -> (r -> a)
   identity))

;;; function-distributive : Distributive ((->) r)
(define function-distributive
  (representable->distributive (make-function-representable)))

;;; ============================================================
;;; Stream Representable
;;; ============================================================
;;;
;;; Infinite streams are representable with Rep = Nat.
;;; Each natural number indexes a position in the stream.

;;; stream-functor : Functor Stream
;;; Local definition using stream-map from stream.ss
(define stream-functor
  (make-functor stream-map))

;;; stream-tabulate : (Nat -> a) -> Stream a
;;; Build a stream from a function on natural numbers.
(define (stream-tabulate f)
  (letrec ([s (stream-cons (f 0)
                           (lambda () (stream-map (lambda (x) (f (+ (stream-nth 0 s) 1)))
                                                  (stream-tail s))))])
          ;; Simpler approach: iterate with index
          (let loop ([n 0])
               (stream-cons (f n) (lambda () (loop (+ n 1)))))))

;;; stream-representable : Representable Stream
;;; Stream a ≅ (Nat -> a)
(define stream-representable
  (make-representable
   stream-functor
   'nat
   ;; index : Stream a -> Nat -> a
   ;; Note: stream-nth is (n s), we need (s n), so flip
   (lambda (s n) (stream-nth n s))
   ;; tabulate : (Nat -> a) -> Stream a
   stream-tabulate))

;;; stream-distributive : Distributive Stream
(define stream-distributive
  (representable->distributive stream-representable))

;;; ============================================================
;;; Utility Functions
;;; ============================================================

;;; memoize-via-rep : Representable f -> (key -> a) -> f a
;;; Memoize a function by tabulating it into a representable structure.
;;; The result structure can then be indexed efficiently.
(define memoize-via-rep rep-tabulate)

;;; zipWith-rep : Representable f -> (a -> b -> c) -> f a -> f b -> f c
;;; Zip two representable structures with a binary function.
(define (rep-zip-with r f fa fb)
  (rep-tabulate r (lambda (key) (f (rep-index r fa key) (rep-index r fb key)))))

;;; duplicate-rep : Representable f -> f a -> f (f a)
;;; Comonadic duplicate for Representable.
(define (rep-duplicate r fa)
  (rep-tabulate r (lambda (key) (rep-local r (const key) fa))))

;;; extend-rep : Representable f -> (f a -> b) -> f a -> f b
;;; Comonadic extend for Representable.
(define (rep-extend r f fa)
  (rep-tabulate r (lambda (key) (f (rep-local r (const key) fa)))))

;;; extract-rep : Representable f -> key -> f a -> a
;;; Comonadic extract for Representable (needs a default key).
(define (rep-extract r default-key fa)
  (rep-index r fa default-key))
