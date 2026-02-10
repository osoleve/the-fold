;;; lattice/fp/category/kan-extension.ss — Kan Extensions
;;; @module kan-extension
;;; @requires prelude combinators templates

(require 'prelude)
(require 'combinators)
(require 'templates)

(doc 'module 'kan-extension)
(doc 'description "Kan Extensions

Kan extensions are the most universal constructions in category theory.
Every concept in category theory can be expressed as a Kan extension.

This module implements:
  - Right Kan Extension (Ran_K F): universal approximation from above
  - Left Kan Extension (Lan_K F): universal approximation from below
  - Codensity monad: Ran_Id M, which gives O(1) bind for any monad M

Key Insight: The O(1) bind optimizations in free.ss and effects.ss
ARE the Codensity transformation. This module makes that explicit.")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'section 'theory)
(doc 'description "Theory

Given functors K : C -> D and F : C -> E, the Right Kan Extension
Ran_K F : D -> E satisfies:

  Nat(G . K, F) ~ Nat(G, Ran_K F)

In Haskell-like notation:
  (Ran K F) a = forall b. (a -> K b) -> F b

The Left Kan Extension satisfies the dual:
  Nat(F, G . K) ~ Nat(Lan_K F, G)

In Haskell-like notation:
  (Lan K F) a = exists b. (K b -> a, F b)

The Codensity Monad:
When K = Id, we get Codensity M = Ran_Id M:
  Codensity M a = forall r. (a -> M r) -> M r

This is EXACTLY the continuation-passing style representation that
gives O(1) amortized bind. The existing free-queue and eff-queue
structures are implementations of this pattern.")

(doc 'section 'right-kan-extension)
(doc 'description "Right Kan Extension: Ran_K F

(Ran K F) a = forall b. (a -> K b) -> F b

In Scheme, we represent this as a function that takes a continuation
(a function a -> K b) and produces F b.

Structure:
  ('ran k-functor f-functor computation)

Where computation : (a -> K b) -> F b for all b")

;;; make-ran : Functor -> Functor -> ((a -> K b) -> F b) -> Ran K F a
;;; Create a Right Kan extension value.
(define (make-ran k-functor f-functor computation)
  (list 'ran k-functor f-functor computation))

;;; ran? : Any -> Boolean
(define (ran? x)
  (and (pair? x) (eq? (car x) 'ran)))

;;; ran-k : Ran K F a -> Functor
;;; Get the K functor.
(define (ran-k ran)
  (list-ref ran 1))

;;; ran-f : Ran K F a -> Functor
;;; Get the F functor.
(define (ran-f ran)
  (list-ref ran 2))

;;; ran-computation : Ran K F a -> ((a -> K b) -> F b)
;;; Get the underlying computation.
(define (ran-computation ran)
  (list-ref ran 3))

;;; ran-apply : Ran K F a -> (a -> K b) -> F b
;;; Apply the Ran to a K-morphism.
;;; This is the fundamental operation: given how to go from a to K b,
;;; produce F b.
(define (ran-apply ran k-morphism)
  ((ran-computation ran) k-morphism))

;;; ====
;;; Ran Functor Instance
;;; ====
;;;
;;; (Ran K F) is a functor in 'a':
;;;   fmap f ran = ran { \k -> computation (k . f) }

;;; ran-fmap : (a -> b) -> Ran K F a -> Ran K F b
;;; Functor instance for Ran K F.
(define (ran-fmap f ran)
  (make-ran (ran-k ran)
            (ran-f ran)
            (lambda (k)
              ((ran-computation ran) (compose2 k f)))))

;;; functor-ran : Functor -> Functor -> Functor
;;; Create the functor instance for Ran K F.
(define (functor-ran k-functor f-functor)
  (make-functor
   (lambda (f ran)
     (ran-fmap f ran))))

;;; ====
;;; Universal Property of Ran
;;; ====
;;;
;;; The right Kan extension satisfies:
;;;   Nat(G . K, F) ~ Nat(G, Ran_K F)
;;;
;;; This means any natural transformation G . K => F factors uniquely
;;; through Ran_K F.

;;; ====
;;; Ran Lifting (Requires Additional Structure)
;;; ====
;;;
;;; Lifting F a into Ran K F (K a) is NOT generally possible!
;;;
;;; The type: (Ran K F) (K a) = forall b. (K a -> K b) -> F b
;;; Given fa : F a and k : K a -> K b, we need to produce F b.
;;; Without knowing how F relates to K, this is impossible.
;;;
;;; Three valid approaches:
;;;
;;; 1. K = Id (Codensity case):
;;;    Codensity M a = forall r. (a -> M r) -> M r
;;;    Given ma : M a and k : a -> M r, we can use m-bind: ma >>= k
;;;    Use: codensity-lift (below)
;;;
;;; 2. With F's fmap over K's action (when K has a right adjoint):
;;;    If we have transform : (a -> K b) -> F a -> F b
;;;    Then ran-lift-with-transform transform fa works
;;;    Use: ran-lift-with-transform
;;;
;;; 3. Trivial embedding (K a = a, immediate application):
;;;    Only valid when you'll immediately apply with identity
;;;    DEPRECATED - use codensity-lift instead
;;;    Use: ran-lift-identity

;;; ran-lift-with-transform : ((a -> K b) -> F a -> F b) -> Functor -> Functor -> F a -> Ran K F a
;;; Lift an F-value into Ran given a transformation function.
;;;
;;; The transform function captures how F can be mapped given a K-morphism.
;;; This is typically derived from an adjunction K ⊣ R where:
;;;   transform k fa = fmap-F (counit . fmap-K k) fa
;;;
;;; Example: If K is a comonad with extract : K a -> a, then:
;;;   transform = (lambda (k fa) (fmap-F (compose extract k) fa))
(define (ran-lift-with-transform transform k-functor f-functor fa)
  (make-ran k-functor
            f-functor
            (lambda (k)
              (transform k fa))))

;;; ran-lift-identity : Functor -> Functor -> F a -> Ran K F a
;;; DEPRECATED: Only use for K = Id case. Prefer codensity-lift.
;;;
;;; This creates a Ran value that ignores the K-morphism argument,
;;; which is only valid when K is the identity functor (or when
;;; you immediately apply with identity continuation).
;;;
;;; For the common case of Codensity (K = Id, F = M a monad),
;;; use codensity-lift which correctly handles the bind operation.
(define (ran-lift-identity k-functor f-functor fa)
  (make-ran k-functor
            f-functor
            (lambda (k)
              ;; WARNING: Ignores k. Only valid for K = Id.
              fa)))

;;; ran-lift : F a -> Ran K F (K a)
;;; DEPRECATED: This function has incorrect semantics for K ≠ Id.
;;;
;;; For Codensity (the common case), use codensity-lift instead.
;;; For general Kan extensions, use ran-lift-with-transform.
;;;
;;; This function is preserved for backward compatibility but will
;;; raise an error to prevent silent type violations.
(define (ran-lift k-functor f-functor fa)
  (error 'ran-lift
         "ran-lift is deprecated due to incorrect semantics for K ≠ Id. \
Use codensity-lift for the Codensity monad (K = Id), or \
ran-lift-with-transform if you have the required transformation function. \
For a quick fix, use ran-lift-identity (understanding it only works for K = Id)."))

(doc 'section 'left-kan-extension)
(doc 'description "Left Kan Extension: Lan_K F

(Lan K F) a = exists b. (K b -> a, F b)

In Scheme, we represent this as a pair:
  - A function K b -> a
  - An F b value

Structure:
  ('lan k-functor f-functor k-morphism f-value)

Where k-morphism : K b -> a and f-value : F b")

;;; make-lan : Functor -> Functor -> (K b -> a) -> F b -> Lan K F a
;;; Create a Left Kan extension value.
(define (make-lan k-functor f-functor k-morphism f-value)
  (list 'lan k-functor f-functor k-morphism f-value))

;;; lan? : Any -> Boolean
(define (lan? x)
  (and (pair? x) (eq? (car x) 'lan)))

;;; lan-k : Lan K F a -> Functor
(define (lan-k lan)
  (list-ref lan 1))

;;; lan-f : Lan K F a -> Functor
(define (lan-f lan)
  (list-ref lan 2))

;;; lan-morphism : Lan K F a -> (K b -> a)
;;; Get the K-morphism.
(define (lan-morphism lan)
  (list-ref lan 3))

;;; lan-value : Lan K F a -> F b
;;; Get the F-value.
(define (lan-value lan)
  (list-ref lan 4))

;;; ====
;;; Lan Functor Instance
;;; ====
;;;
;;; (Lan K F) is a functor in 'a':
;;;   fmap f lan = lan { (f . k-morphism, f-value) }

;;; lan-fmap : (a -> b) -> Lan K F a -> Lan K F b
;;; Functor instance for Lan K F.
(define (lan-fmap f lan)
  (make-lan (lan-k lan)
            (lan-f lan)
            (compose2 f (lan-morphism lan))
            (lan-value lan)))

;;; functor-lan : Functor -> Functor -> Functor
;;; Create the functor instance for Lan K F.
(define (functor-lan k-functor f-functor)
  (make-functor
   (lambda (f lan)
     (lan-fmap f lan))))

;;; ====
;;; Universal Property of Lan
;;; ====
;;;
;;; The left Kan extension satisfies:
;;;   Nat(F, G . K) ~ Nat(Lan_K F, G)
;;;
;;; This means any natural transformation F => G . K factors uniquely
;;; through Lan_K F.

;;; lan-inject : F b -> (K b -> a) -> Lan K F a
;;; Inject an F-value with a K-morphism.
(define (lan-inject k-functor f-functor f-value k-morphism)
  (make-lan k-functor f-functor k-morphism f-value))

;;; lan-lower : ((F b -> G (K b)) for all b) -> Lan K F a -> G a
;;; Given a natural transformation F => G . K, lower Lan to G.
;;; This is the universal property in action.
(define (lan-lower nat-transform lan g-fmap)
  (let* ([fb (lan-value lan)]
         [k-morph (lan-morphism lan)]
         ;; Apply nat-transform to get G (K b)
         [gkb (nat-transform fb)]
         ;; fmap k-morph over G to get G a
         )
    (g-fmap k-morph gkb)))

(doc 'section 'codensity-monad)
(doc 'description "Codensity Monad: Ran_Id M

The Codensity monad is the right Kan extension of M along Id:

  Codensity M a = Ran_Id M a
                = forall r. (a -> Id r) -> M r
                = forall r. (a -> r) -> M r
                = forall r. (a -> M r) -> M r

The standard Codensity encoding uses the (a -> M r) -> M r form.

KEY INSIGHT:
This is EXACTLY what free-queue and eff-queue implement!

free-queue: ('free-queue base-free fmap continuation-queue)
  where continuation-queue is List (a -> Free f b)
  This accumulates continuations instead of rebuilding structure.

eff-queue: ('eff-queue base-eff continuation-queue)
  where continuation-queue is List (a -> Eff e b)
  Same pattern: defer traversal by queuing continuations.

Both represent: forall r. (a -> M r) -> M r
where the continuation queue represents the composition of all
the (a -> M r) functions waiting to be applied.")

;;; ====
;;; Codensity Representation
;;; ====
;;;
;;; We represent Codensity M a as:
;;;   ('codensity m-return run-computation)
;;;
;;; Where:
;;;   - m-return : a -> M a (the monad's return)
;;;   - run-computation : (a -> M r) -> M r

;;; make-codensity : (a -> M a) -> ((a -> M r) -> M r) -> Codensity M a
(define (make-codensity m-return run-computation)
  (list 'codensity m-return run-computation))

;;; codensity? : Any -> Boolean
(define (codensity? x)
  (and (pair? x) (eq? (car x) 'codensity)))

;;; codensity-return-fn : Codensity M a -> (a -> M a)
;;; Get the underlying monad's return.
(define (codensity-return-fn c)
  (list-ref c 1))

;;; codensity-run : Codensity M a -> ((a -> M r) -> M r)
;;; Get the run function.
(define (codensity-run c)
  (list-ref c 2))

;;; ====
;;; Codensity Monad Operations
;;; ====

;;; codensity-return : (a -> M a) -> a -> Codensity M a
;;; Monadic return for Codensity.
;;; return x = \k -> k x
(define (codensity-return m-return x)
  (make-codensity m-return
                  (lambda (k) (k x))))

;;; codensity-bind : Codensity M a -> (a -> Codensity M b) -> Codensity M b
;;; Monadic bind for Codensity.
;;; O(1) operation! This is the magic.
;;;
;;; (m >>= f) = \k -> run_m (\a -> run_{f a} k)
;;;
;;; Instead of traversing structure, we compose continuations.
(define (codensity-bind ca f)
  (let ([m-return (codensity-return-fn ca)]
        [run-a (codensity-run ca)])
    (make-codensity
     m-return
     (lambda (k)
       ;; Run the first computation with a continuation that:
       ;; 1. Applies f to get Codensity M b
       ;; 2. Runs that with the final continuation k
       (run-a (lambda (a)
                ((codensity-run (f a)) k)))))))

;;; codensity-map : (a -> b) -> Codensity M a -> Codensity M b
;;; Functor instance.
(define (codensity-map f ca)
  (codensity-bind ca
                  (lambda (a)
                    (codensity-return (codensity-return-fn ca) (f a)))))

;;; ====
;;; Codensity Lifting and Lowering
;;; ====

;;; codensity-lift : (a -> M a) -> (M a -> (a -> M r) -> M r) -> M a -> Codensity M a
;;; Lift a monadic value into Codensity.
;;; This requires knowing how to bind in M.
;;;
;;; lift m = \k -> m >>= k
(define (codensity-lift m-return m-bind ma)
  (make-codensity m-return
                  (lambda (k)
                    (m-bind ma k))))

;;; codensity-lower : Codensity M a -> M a
;;; Lower Codensity back to the underlying monad.
;;; This is where all the accumulated work happens.
;;;
;;; lower c = run_c return
(define (codensity-lower ca)
  (let ([m-return (codensity-return-fn ca)]
        [run (codensity-run ca)])
    (run m-return)))

;;; ====
;;; Connection to Existing Code
;;; ====
;;;
;;; The free-queue and eff-queue ARE Codensity in disguise:
;;;
;;; free-queue translates to:
;;;   - base-free: the underlying Free f computation
;;;   - conts: the accumulated continuations (a -> Free f b)
;;;
;;; In Codensity terms:
;;;   codensity-run = \k -> normalize(apply-queue(base, conts ++ [k]))
;;;
;;; eff-queue translates to:
;;;   - base-eff: the underlying Eff computation
;;;   - conts: the accumulated continuations
;;;
;;; Same pattern!

(doc 'section 'codensity-list)
(doc 'description "Codensity for List

IMPORTANT: Codensity provides O(1) BIND, not O(1) append.

If you want O(1) append (difference lists), use the dlist-* functions below.
Codensity List is useful when you have many monadic binds (>>=) but don't
need efficient concatenation.

Common confusion: 'Codensity List ~ difference lists' is about representation
isomorphism, not operational equivalence. The O(1) append comes from the
dlist encoding ([a] -> [a] endomorphisms), not from Codensity's CPS encoding.")

;;; codensity-list-singleton : a -> Codensity List a
;;; Create a single-element Codensity List.
;;; Use codensity-bind for monadic composition (O(1)).
;;; For list concatenation with O(1) append, use dlist-* instead.
(define (codensity-list-singleton x)
  (make-codensity list
                  (lambda (k) (k x))))

;;; codensity-list-lower : Codensity List a -> List a
;;; Lower Codensity back to a regular list.
(define (codensity-list-lower c)
  ((codensity-run c) list))

;;; ====
;;; True Difference Lists (O(1) append)
;;; ====
;;;
;;; A DList is a function [a] -> [a] that prepends elements.
;;; - Composition is O(1)
;;; - Lowering (to-list) is O(n)
;;;
;;; This is the proper "difference list" pattern from functional programming.

;;; make-dlist : ([a] -> [a]) -> DList a
;;; Create a difference list from a prepend function.
(define (make-dlist prepend-fn)
  (list 'dlist prepend-fn))

;;; dlist? : Any -> Boolean
(define (dlist? x)
  (and (pair? x) (eq? (car x) 'dlist)))

;;; dlist-prepend : DList a -> ([a] -> [a])
(define (dlist-prepend dl)
  (cadr dl))

;;; dlist-empty : DList a
;;; The empty difference list.
(define dlist-empty
  (make-dlist id))

;;; dlist-singleton : a -> DList a
;;; O(1) - create a single-element difference list.
(define (dlist-singleton x)
  (make-dlist (lambda (tail) (cons x tail))))

;;; dlist-from-list : [a] -> DList a
;;; O(n) - convert a list to a difference list.
(define (dlist-from-list xs)
  (make-dlist (lambda (tail) (append xs tail))))

;;; dlist-append : DList a -> DList a -> DList a
;;; O(1)! - append two difference lists via function composition.
;;; This is the key operation that makes difference lists useful.
(define (dlist-append dl1 dl2)
  (make-dlist (lambda (tail)
                ((dlist-prepend dl1)
                 ((dlist-prepend dl2) tail)))))

;;; dlist-cons : a -> DList a -> DList a
;;; O(1) - prepend an element.
(define (dlist-cons x dl)
  (make-dlist (lambda (tail)
                (cons x ((dlist-prepend dl) tail)))))

;;; dlist-snoc : DList a -> a -> DList a
;;; O(1) - append an element to the end.
(define (dlist-snoc dl x)
  (dlist-append dl (dlist-singleton x)))

;;; dlist-to-list : DList a -> [a]
;;; O(n) - convert back to a regular list.
;;; This is where all the deferred work happens.
(define (dlist-to-list dl)
  ((dlist-prepend dl) '()))

;;; dlist-concat : [DList a] -> DList a
;;; O(k) where k is the number of difference lists.
;;; The total to-list is still O(n) where n is total elements.
(define (dlist-concat dls)
  (fold-right dlist-append dlist-empty dls))

;;; ====
;;; Codensity for Maybe
;;; ====

;;; codensity-maybe-return : a -> Codensity Maybe a
(define (codensity-maybe-return x)
  (make-codensity just
                  (lambda (k) (k x))))

;;; codensity-maybe-bind : Codensity Maybe a -> (a -> Codensity Maybe b) -> Codensity Maybe b
;;; O(1) bind for Maybe via Codensity.
(define (codensity-maybe-bind ca f)
  (codensity-bind ca f))

;;; codensity-maybe-fail : Codensity Maybe a
(define codensity-maybe-fail
  (make-codensity just
                  (lambda (k) nothing)))

;;; ====
;;; Generic Codensity Builder
;;; ====

;;; make-codensity-monad : (a -> M a) -> (M a -> (a -> M b) -> M b) -> CodensityMonad M
;;; Create a Codensity monad instance for any monad M.
;;; Returns a record of operations.
(define (make-codensity-monad m-return m-bind)
  (list
   'codensity-monad
   ;; return : a -> Codensity M a
   (lambda (x) (codensity-return m-return x))
   ;; bind : Codensity M a -> (a -> Codensity M b) -> Codensity M b
   codensity-bind
   ;; lift : M a -> Codensity M a
   (lambda (ma) (codensity-lift m-return m-bind ma))
   ;; lower : Codensity M a -> M a
   codensity-lower))

;;; codensity-monad-return : CodensityMonad M -> (a -> Codensity M a)
(define (codensity-monad-return cm)
  (list-ref cm 1))

;;; codensity-monad-bind : CodensityMonad M -> (Codensity M a -> (a -> Codensity M b) -> Codensity M b)
(define (codensity-monad-bind cm)
  (list-ref cm 2))

;;; codensity-monad-lift : CodensityMonad M -> (M a -> Codensity M a)
(define (codensity-monad-lift cm)
  (list-ref cm 3))

;;; codensity-monad-lower : CodensityMonad M -> (Codensity M a -> M a)
(define (codensity-monad-lower cm)
  (list-ref cm 4))

(doc 'section 'yoneda-relationship)
(doc 'description "Relationship to Yoneda

The Yoneda lemma states:
  Nat(Hom(A, -), F) ~ F A

Codensity is related to Yoneda:
  Codensity M a = forall r. (a -> M r) -> M r
                ~ Nat(Hom(a, M -), M)

When M = Id, Codensity Id a ~ a (by Yoneda).
This is why Codensity preserves the monad structure.")

(doc 'section 'key-theorem)
(doc 'description "The Key Theorem

For any monad M:
  1. Codensity M is a monad
  2. lift : M ~> Codensity M is a monad morphism
  3. lower : Codensity M ~> M is a monad morphism
  4. lower . lift = id (retraction)

This means Codensity M is always at least as good as M,
and the composition lower . lift is the identity.

The O(1) bind of Codensity comes from the fact that:
  (m >>= f) >>= g = m >>= (\\x -> f x >>= g)

Instead of nested binds (left-associative, O(n^2)),
Codensity reassociates to right-associative (O(n)).")

;;; ============================================================
;;; Comparison with Existing Implementation
;;; ============================================================
;;;
;;; free.ss uses:
;;;   ('free-queue base-free fmap (list cont1 cont2 ...))
;;;
;;; This is equivalent to:
;;;   Codensity (Free f) where:
;;;     run k = free-apply-queue fmap base (conts ++ [k])
;;;
;;; effects.ss uses:
;;;   ('eff-queue base-eff (list cont1 cont2 ...))
;;;
;;; This is equivalent to:
;;;   Codensity Eff where:
;;;     run k = eff-apply-queue base (conts ++ [k])
;;;
;;; The "queue" representation is a defunctionalized continuation.
;;; Instead of nested lambda closures, we store a list of functions
;;; that will be composed at run time.

;;; ============================================================
;;; Performance Analysis
;;; ============================================================
;;;
;;; Without Codensity (naive bind):
;;;   ((a >>= b) >>= c) >>= d
;;;
;;;   Each >>= traverses all the way down to modify the deepest
;;;   continuation. For n binds, this is O(n^2) total.
;;;
;;; With Codensity:
;;;   Each >>= is O(1) - just compose continuations.
;;;   Final lower is O(n) - apply all continuations once.
;;;   Total: O(n)
;;;
;;; The continuation queue in free-queue/eff-queue achieves this:
;;;   - append to queue: O(1) (using cons + reverse at end, or list append)
;;;   - normalize at run time: O(n)
;;;
;;; Note: The current implementation uses (append queue (list f)) which
;;; is O(n) per append. A proper difference list or finger tree would
;;; give true O(1) append, but the amortized complexity is still good
;;; for typical use patterns.

;;; ============================================================
;;; Exports
;;; ============================================================
;;;
;;; Right Kan Extension:
;;;   make-ran, ran?, ran-k, ran-f, ran-computation, ran-apply
;;;   ran-fmap, functor-ran
;;;   ran-lift-with-transform, ran-lift-identity
;;;   ran-lift [DEPRECATED - errors, use codensity-lift or ran-lift-identity]
;;;
;;; Left Kan Extension:
;;;   make-lan, lan?, lan-k, lan-f, lan-morphism, lan-value
;;;   lan-fmap, functor-lan, lan-inject, lan-lower
;;;
;;; Codensity Monad:
;;;   make-codensity, codensity?, codensity-return-fn, codensity-run
;;;   codensity-return, codensity-bind, codensity-map
;;;   codensity-lift, codensity-lower
;;;   codensity-list-singleton, codensity-list-lower
;;;   codensity-maybe-return, codensity-maybe-bind, codensity-maybe-fail
;;;   make-codensity-monad, codensity-monad-return, codensity-monad-bind
;;;   codensity-monad-lift, codensity-monad-lower
;;;
;;; Difference Lists (True O(1) append):
;;;   make-dlist, dlist?, dlist-prepend, dlist-empty
;;;   dlist-singleton, dlist-from-list, dlist-append
;;;   dlist-cons, dlist-snoc, dlist-to-list, dlist-concat
