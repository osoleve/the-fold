;;; @module monad-derivation
;;; @requires adjunction
;;; @description Monads from adjunctions
;;; @purity total
;;; @stability stable

(require 'adjunction)

(doc 'module 'monad-derivation)
(doc 'description "Unified Monad Derivation from Adjunctions

Every adjunction F ⊣ G gives rise to a monad on the composite G∘F:
  - return = η (unit of the adjunction)
  - join = G(ε_F) where ε is the counit
  - bind m f = join (fmap f m)

This module provides:
  - monad-from-adjunction: derive monad operations from any adjunction
  - MonadOps record: return, bind, join, fmap
  - Example derivations: List monad from Free monoid adjunction
  - State adjunction and its derived monad")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'section 'monad-ops-record)
(doc 'description "MonadOps Record

A MonadOps bundles the four core monad operations:
  - return : a → M a
  - fmap : (a → b) → M a → M b
  - join : M (M a) → M a
  - bind : M a → (a → M b) → M b")

(define (make-monad-ops name return-fn fmap-fn join-fn bind-fn)
  (doc 'export #t)
  (doc 'type '(-> Symbol (-> a (M a)) (-> (-> a b) (M a) (M b)) (-> (M (M a)) (M a)) (-> (M a) (-> a (M b)) (M b)) MonadOps))
  (list 'monad-ops name return-fn fmap-fn join-fn bind-fn))

(define (monad-ops? x)
  (doc 'export #t)
  (doc 'type '(-> Any Boolean))
  (and (pair? x)
       (eq? (car x) 'monad-ops)
       (= (length x) 6)))

(define (monad-ops-name m)
  (doc 'export #t)
  (doc 'type '(-> MonadOps Symbol))
  (if (monad-ops? m) (cadr m) 'unknown))

(define (monad-ops-return m)
  (doc 'export #t)
  (doc 'type '(-> MonadOps (-> a (M a))))
  (if (monad-ops? m) (caddr m) #f))

(define (monad-ops-fmap m)
  (doc 'export #t)
  (doc 'type '(-> MonadOps (-> (-> a b) (M a) (M b))))
  (if (monad-ops? m) (cadddr m) #f))

(define (monad-ops-join m)
  (doc 'export #t)
  (doc 'type '(-> MonadOps (-> (M (M a)) (M a))))
  (if (monad-ops? m) (car (cddddr m)) #f))

(define (monad-ops-bind m)
  (doc 'export #t)
  (doc 'type '(-> MonadOps (-> (M a) (-> a (M b)) (M b))))
  (if (monad-ops? m) (cadr (cddddr m)) #f))

;;; monad-ops-ap : MonadOps → (M (a → b) → M a → M b)
;;; Applicative ap derived from bind and fmap.
;;; ap mf ma = bind mf (λf. fmap f ma)
(define (monad-ops-ap m)
  (doc 'export #t)
  (doc 'type '(-> MonadOps (-> (M (-> a b)) (M a) (M b))))
  (doc 'description "Applicative <*> derived from monad operations")
  (let ([bind-fn (monad-ops-bind m)]
        [fmap-fn (monad-ops-fmap m)])
    (lambda (mf ma)
      (bind-fn mf (lambda (f) (fmap-fn f ma))))))

;;; monad-ap : MonadOps × M (a → b) × M a → M b
;;; Convenience function for applicative application.
(define (monad-ap ops mf ma)
  (doc 'export #t)
  (doc 'type '(-> MonadOps (M (-> a b)) (M a) (M b)))
  ((monad-ops-ap ops) mf ma))

(doc 'section 'monad-derivation)
(doc 'description "Monad Derivation from Adjunction

Given an adjunction F ⊣ G, the composite G∘F forms a monad where:
  - The carrier functor is G∘F
  - return = η : Id → G∘F (unit of the adjunction)
  - join = G(ε_F) : G∘F∘G∘F → G∘F (applying G to counit at F)

The key insight: ε : F∘G → Id, so ε_F(A) : F(G(F(A))) → F(A)
Applying G: G(ε_F(A)) : G(F(G(F(A)))) → G(F(A))
This gives us join: (G∘F)∘(G∘F) → G∘F")

;;; monad-from-adjunction : Adjunction → MonadOps
;;; Derive monad operations from an adjunction F ⊣ G.
;;; Returns MonadOps for the monad on G∘F.
(define (monad-from-adjunction adj)
  (doc 'export #t)
  (let* ([F (adjunction-left adj)]
         [G (adjunction-right adj)]
         [η (adjunction-unit adj)]
         [ε (adjunction-counit adj)]
         [η-comp (nat-transform-component η)]
         [ε-comp (nat-transform-component ε)]
         [F-fmap (functor-fmap F)]
         [G-fmap (functor-fmap G)]
         [name (string->symbol
                (format "monad-~a" (adjunction-name adj)))])

    ;; return = η : a → G(F(a))
    (let ([return-fn η-comp]

          ;; fmap for G∘F: (a → b) → G(F(a)) → G(F(b))
          ;; (G∘F)(f) = G(F(f))
          [fmap-fn (lambda (f gfa)
                     (G-fmap (lambda (fa) (F-fmap f fa)) gfa))]

          ;; join = G(ε_F) : G(F(G(F(a)))) → G(F(a))
          ;; ε at F(a) gives: F(G(F(a))) → F(a)
          ;; G applied: G(F(G(F(a)))) → G(F(a))
          [join-fn (lambda (gfgfa)
                     (G-fmap ε-comp gfgfa))])

      ;; bind m f = join (fmap f m)
      (let ([bind-fn (lambda (m f)
                       (join-fn (fmap-fn f m)))])

        (make-monad-ops name return-fn fmap-fn join-fn bind-fn)))))

(doc 'section 'list-monad-example)
(doc 'description "Example: List Monad from Free Monoid Adjunction

The free monoid adjunction List ⊣ Id (or more precisely, Free ⊣ Forgetful)
gives rise to the List monad:
  - return x = [x] (singleton list)
  - join = concat (flatten nested lists)
  - bind xs f = concat (map f xs)")

(doc monad-list-derived 'type 'MonadOps)
(doc monad-list-derived 'description "List monad derived from adj-free-list")
(define monad-list-derived
  (monad-from-adjunction adj-free-list))

(doc 'section 'state-adjunction)
(doc 'description "State Adjunction and Monad

The State monad arises from the adjunction (- × S) ⊣ (- ^ S):
  - Left adjoint F: A ↦ A × S (product with state)
  - Right adjoint G: B ↦ S → B (functions from state)
  - Unit η_A : A → (S → A × S), η_A(a) = λs. (a, s)
  - Counit ε_B : (S → B) × S → B, ε_B(f, s) = f(s)

The resulting monad on G∘F is:
  M A = S → (A × S)

This is exactly the State monad, where computations thread state through.
Note: Reader monad (S → A) requires a different construction.")

;;; make-state-adjunction : Type → Adjunction
;;; Create the product/exponential adjunction for a fixed state type S.
;;; Note: In our untyped setting, S is just a tag to distinguish the adjunction.
;;;
;;; We use the standard formulation where:
;;;   F(A) = (A, S)  -- pair with state slot
;;;   G(B) = S → B   -- function from state
;;;   η : A → (S → (A, S))  -- η(a) = λs. (a, s)
;;;   ε : (S → B, S) → B    -- ε(f, s) = f(s)
(define (make-state-adjunction state-tag)
  (doc 'export #t)
  (let* (;; F(A) = A × S represented as (a . state-slot)
         ;; F(f)(a . s) = (f a . s)
         [F (make-functor
             (lambda (f as)
               (cons (f (car as)) (cdr as))))]

         ;; G(B) = S → B, represented as functions
         ;; G(f) : (S → B) → (S → C) = λg. λs. f(g(s))
         [G (make-functor
             (lambda (f g)
               (lambda (s) (f (g s)))))]

         ;; η : Id → G∘F
         ;; η_A : A → (S → A × S)
         ;; η_A(a) = λs. (a, s)
         [η (make-nat-transform
             'η-state
             functor-id
             G  ; Simplified: target is G (applied to F result)
             (lambda (a)
               (lambda (s) (cons a s))))]

         ;; ε : F∘G → Id
         ;; ε_B : (S → B) × S → B
         ;; ε_B(f, s) = f(s)
         [ε (make-nat-transform
             'ε-state
             F  ; Simplified: source is F (applied to G result)
             functor-id
             (lambda (fs)
               (let ([f (car fs)]
                     [s (cdr fs)])
                 (f s))))])

    (make-adjunction
     (string->symbol (format "state-~a" state-tag))
     F
     G
     η
     ε)))

;;; adj-state-example : Adjunction
;;; Example State adjunction with state tag 'state.
(define adj-state-example
  (make-state-adjunction 'state))

;;; monad-state-derived : MonadOps
;;; State monad derived from the State adjunction.
;;; M A = S → (A × S)
(define monad-state-derived
  (monad-from-adjunction adj-state-example))

(doc 'section 'state-utilities)
(doc 'description "State Monad Operations

The monad derived from the State adjunction is M A = S → (A × S).
Provide convenience functions for working with this pattern.")

(define (run-state m s)
  (doc 'export #t)
  (doc 'type '(-> (-> S (* A S)) S (* A S)))
  (doc 'description "Run a state computation with initial state")
  (m s))

(define (eval-state m s)
  (doc 'export #t)
  (doc 'type '(-> (-> S (* A S)) S A))
  (doc 'description "Run computation and return just the value")
  (car (run-state m s)))

(define (exec-state m s)
  (doc 'export #t)
  (doc 'type '(-> (-> S (* A S)) S S))
  (doc 'description "Run computation and return just the final state")
  (cdr (run-state m s)))

(doc 'section 'monad-law-verification)
(doc 'description "Monad Law Verification

A monad must satisfy three laws:
  1. Left identity:  bind (return a) f = f a
  2. Right identity: bind m return = m
  3. Associativity:  bind (bind m f) g = bind m (λx. bind (f x) g)

NOTE: The basic verify-* functions use Scheme's equal? for comparison.
This works for data-based monads (List, Maybe, Either) but FAILS for
function-based monads (State, Reader, Continuation) because functions
are compared by reference, not extensionally.

For function-based monads, use the verify-*-with-eq variants that accept
a custom equality predicate. Example for State monad:

  (define (state-eq s m1 m2)
    (equal? (run-state m1 s) (run-state m2 s)))

  (verify-left-identity-with-eq ops a f (lambda (x y) (state-eq 0 x y)))")

;;; verify-left-identity : MonadOps × a × (a → M b) → Boolean
;;; Check: bind (return a) f = f a
;;; Uses equal? - only works for data-based monads.
(define (verify-left-identity ops a f)
  (doc 'export #t)
  (verify-left-identity-with-eq ops a f equal?))

;;; verify-left-identity-with-eq : MonadOps × a × (a → M b) × (M b × M b → Boolean) → Boolean
;;; Check: bind (return a) f = f a, using custom equality.
(define (verify-left-identity-with-eq ops a f eq?)
  (doc 'export #t)
  (let ([return (monad-ops-return ops)]
        [bind (monad-ops-bind ops)])
    (eq? (bind (return a) f)
         (f a))))

;;; verify-right-identity : MonadOps × M a → Boolean
;;; Check: bind m return = m
;;; Uses equal? - only works for data-based monads.
(define (verify-right-identity ops m)
  (doc 'export #t)
  (verify-right-identity-with-eq ops m equal?))

;;; verify-right-identity-with-eq : MonadOps × M a × (M a × M a → Boolean) → Boolean
;;; Check: bind m return = m, using custom equality.
(define (verify-right-identity-with-eq ops m eq?)
  (doc 'export #t)
  (let ([return (monad-ops-return ops)]
        [bind (monad-ops-bind ops)])
    (eq? (bind m return) m)))

;;; verify-associativity : MonadOps × M a × (a → M b) × (b → M c) → Boolean
;;; Check: bind (bind m f) g = bind m (λx. bind (f x) g)
;;; Uses equal? - only works for data-based monads.
(define (verify-associativity ops m f g)
  (doc 'export #t)
  (verify-associativity-with-eq ops m f g equal?))

;;; verify-associativity-with-eq : MonadOps × M a × (a → M b) × (b → M c) × (M c × M c → Boolean) → Boolean
;;; Check: bind (bind m f) g = bind m (λx. bind (f x) g), using custom equality.
(define (verify-associativity-with-eq ops m f g eq?)
  (doc 'export #t)
  (let ([bind (monad-ops-bind ops)])
    (eq? (bind (bind m f) g)
         (bind m (lambda (x) (bind (f x) g))))))

;;; verify-monad-laws : MonadOps × a × M a × (a → M a) × (a → M a) → Boolean
;;; Verify all three monad laws for given test values.
;;; Uses equal? - only works for data-based monads.
(define (verify-monad-laws ops a m f g)
  (doc 'export #t)
  (verify-monad-laws-with-eq ops a m f g equal?))

;;; verify-monad-laws-with-eq : MonadOps × a × M a × (a → M a) × (a → M a) × (M a × M a → Boolean) → Boolean
;;; Verify all three monad laws using custom equality.
;;; Use this for function-based monads (State, Reader, Continuation).
(define (verify-monad-laws-with-eq ops a m f g eq?)
  (doc 'export #t)
  (and (verify-left-identity-with-eq ops a f eq?)
       (verify-right-identity-with-eq ops m eq?)
       (verify-associativity-with-eq ops m f g eq?)))

;;; ====
;;; Functor Law Verification
;;; ====
;;;
;;; A functor must satisfy:
;;;   1. Identity:    fmap id = id
;;;   2. Composition: fmap (g . f) = fmap g . fmap f
;;;
;;; Same caveat as monad laws: use -with-eq variants for function-based monads.

;;; verify-functor-identity : MonadOps × M a → Boolean
;;; Check: fmap id m = m
;;; Uses equal? - only works for data-based monads.
(define (verify-functor-identity ops m)
  (doc 'export #t)
  (verify-functor-identity-with-eq ops m equal?))

;;; verify-functor-identity-with-eq : MonadOps × M a × (M a × M a → Boolean) → Boolean
;;; Check: fmap id m = m, using custom equality.
(define (verify-functor-identity-with-eq ops m eq?)
  (doc 'export #t)
  (let ([fmap (monad-ops-fmap ops)])
    (eq? (fmap id m) m)))

;;; verify-functor-composition : MonadOps × (b → c) × (a → b) × M a → Boolean
;;; Check: fmap (g . f) m = fmap g (fmap f m)
;;; Uses equal? - only works for data-based monads.
(define (verify-functor-composition ops g f m)
  (doc 'export #t)
  (verify-functor-composition-with-eq ops g f m equal?))

;;; verify-functor-composition-with-eq : MonadOps × (b → c) × (a → b) × M a × (M c × M c → Boolean) → Boolean
;;; Check: fmap (g . f) m = fmap g (fmap f m), using custom equality.
(define (verify-functor-composition-with-eq ops g f m eq?)
  (doc 'export #t)
  (let ([fmap (monad-ops-fmap ops)])
    (eq? (fmap (compose2 g f) m)
         (fmap g (fmap f m)))))

;;; ====
;;; Display
;;; ====

;;; monad-ops->string : MonadOps → String
(define (monad-ops->string ops)
  (doc 'export #t)
  (if (monad-ops? ops)
      (format "MonadOps<~a>" (monad-ops-name ops))
      "Not a MonadOps"))

;;; ====
;;; Exports
;;; ====
;;;
;;; MonadOps:
;;;   - make-monad-ops, monad-ops?
;;;   - monad-ops-name, monad-ops-return, monad-ops-fmap
;;;   - monad-ops-join, monad-ops-bind
;;;   - monad-ops-ap, monad-ap (Applicative <*> derived from bind)
;;;
;;; Derivation:
;;;   - monad-from-adjunction
;;;
;;; Examples:
;;;   - monad-list-derived (from adj-free-list)
;;;   - make-state-adjunction, adj-state-example
;;;   - monad-state-derived
;;;
;;; State utilities:
;;;   - run-state, eval-state, exec-state
;;;
;;; Verification (data-based monads - use equal?):
;;;   - verify-left-identity, verify-right-identity, verify-associativity
;;;   - verify-monad-laws
;;;   - verify-functor-identity, verify-functor-composition
;;;
;;; Verification (function-based monads - custom equality):
;;;   - verify-left-identity-with-eq, verify-right-identity-with-eq
;;;   - verify-associativity-with-eq, verify-monad-laws-with-eq
;;;   - verify-functor-identity-with-eq, verify-functor-composition-with-eq
;;;
;;; Display:
;;;   - monad-ops->string
