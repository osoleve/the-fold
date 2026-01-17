;;; lattice/fp/category/monad-derivation.ss — Unified Monad Derivation from Adjunctions
;;;
;;; Every adjunction F ⊣ G gives rise to a monad on the composite G∘F:
;;;   - return = η (unit of the adjunction)
;;;   - join = G(ε_F) where ε is the counit
;;;   - bind m f = join (fmap f m)
;;;
;;; This module provides:
;;;   - monad-from-adjunction: derive monad operations from any adjunction
;;;   - MonadOps record: return, bind, join, fmap
;;;   - Example derivations: List monad from Free monoid adjunction
;;;   - State adjunction and its derived monad
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - adjunction.ss
;;;   - natural-transform.ss
;;;   - templates.ss (for functor definitions)

(load "lattice/fp/category/adjunction.ss")

;;; ====
;;; MonadOps Record
;;; ====
;;;
;;; A MonadOps bundles the four core monad operations:
;;;   - return : a → M a
;;;   - fmap : (a → b) → M a → M b
;;;   - join : M (M a) → M a
;;;   - bind : M a → (a → M b) → M b

;;; make-monad-ops : Symbol × (a → M a) × ((a → b) → M a → M b) × (M (M a) → M a) × (M a → (a → M b) → M b) → MonadOps
(define (make-monad-ops name return-fn fmap-fn join-fn bind-fn)
  (list 'monad-ops name return-fn fmap-fn join-fn bind-fn))

;;; monad-ops? : Any → Boolean
(define (monad-ops? x)
  (and (pair? x)
       (eq? (car x) 'monad-ops)
       (= (length x) 6)))

;;; monad-ops-name : MonadOps → Symbol
(define (monad-ops-name m)
  (if (monad-ops? m) (cadr m) 'unknown))

;;; monad-ops-return : MonadOps → (a → M a)
(define (monad-ops-return m)
  (if (monad-ops? m) (caddr m) #f))

;;; monad-ops-fmap : MonadOps → ((a → b) → M a → M b)
(define (monad-ops-fmap m)
  (if (monad-ops? m) (cadddr m) #f))

;;; monad-ops-join : MonadOps → (M (M a) → M a)
(define (monad-ops-join m)
  (if (monad-ops? m) (car (cddddr m)) #f))

;;; monad-ops-bind : MonadOps → (M a → (a → M b) → M b)
(define (monad-ops-bind m)
  (if (monad-ops? m) (cadr (cddddr m)) #f))

;;; ====
;;; Monad Derivation from Adjunction
;;; ====
;;;
;;; Given an adjunction F ⊣ G, the composite G∘F forms a monad where:
;;;   - The carrier functor is G∘F
;;;   - return = η : Id → G∘F (unit of the adjunction)
;;;   - join = G(ε_F) : G∘F∘G∘F → G∘F (applying G to counit at F)
;;;
;;; The key insight: ε : F∘G → Id, so ε_F(A) : F(G(F(A))) → F(A)
;;; Applying G: G(ε_F(A)) : G(F(G(F(A)))) → G(F(A))
;;; This gives us join: (G∘F)∘(G∘F) → G∘F

;;; monad-from-adjunction : Adjunction → MonadOps
;;; Derive monad operations from an adjunction F ⊣ G.
;;; Returns MonadOps for the monad on G∘F.
(define (monad-from-adjunction adj)
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

;;; ====
;;; Example: List Monad from Free Monoid Adjunction
;;; ====
;;;
;;; The free monoid adjunction List ⊣ Id (or more precisely, Free ⊣ Forgetful)
;;; gives rise to the List monad:
;;;   - return x = [x] (singleton list)
;;;   - join = concat (flatten nested lists)
;;;   - bind xs f = concat (map f xs)

;;; monad-list-derived : MonadOps
;;; List monad derived from adj-free-list.
(define monad-list-derived
  (monad-from-adjunction adj-free-list))

;;; ====
;;; State Adjunction and Monad
;;; ====
;;;
;;; The State monad arises from the adjunction (- × S) ⊣ (- ^ S):
;;;   - Left adjoint F: A ↦ A × S (product with state)
;;;   - Right adjoint G: B ↦ S → B (functions from state)
;;;   - Unit η_A : A → (S → A × S), η_A(a) = λs. (a, s)
;;;   - Counit ε_B : (S → B) × S → B, ε_B(f, s) = f(s)
;;;
;;; The resulting monad on G∘F is:
;;;   M A = S → (A × S)
;;;
;;; This is exactly the State monad, where computations thread state through.
;;; Note: Reader monad (S → A) requires a different construction.

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

;;; ====
;;; State Monad Operations
;;; ====
;;;
;;; The monad derived from the State adjunction is M A = S → (A × S).
;;; Provide convenience functions for working with this pattern.

;;; run-state : (S → (A × S)) × S → (A × S)
;;; Run a state computation with initial state.
(define (run-state m s)
  (m s))

;;; eval-state : (S → (A × S)) × S → A
;;; Run computation and return just the value.
(define (eval-state m s)
  (car (run-state m s)))

;;; exec-state : (S → (A × S)) × S → S
;;; Run computation and return just the final state.
(define (exec-state m s)
  (cdr (run-state m s)))

;;; ====
;;; Monad Law Verification
;;; ====
;;;
;;; A monad must satisfy three laws:
;;;   1. Left identity:  bind (return a) f = f a
;;;   2. Right identity: bind m return = m
;;;   3. Associativity:  bind (bind m f) g = bind m (λx. bind (f x) g)

;;; verify-left-identity : MonadOps × a × (a → M b) → Boolean
;;; Check: bind (return a) f = f a
(define (verify-left-identity ops a f)
  (let ([return (monad-ops-return ops)]
        [bind (monad-ops-bind ops)])
    (equal? (bind (return a) f)
            (f a))))

;;; verify-right-identity : MonadOps × M a → Boolean
;;; Check: bind m return = m
(define (verify-right-identity ops m)
  (let ([return (monad-ops-return ops)]
        [bind (monad-ops-bind ops)])
    (equal? (bind m return) m)))

;;; verify-associativity : MonadOps × M a × (a → M b) × (b → M c) → Boolean
;;; Check: bind (bind m f) g = bind m (λx. bind (f x) g)
(define (verify-associativity ops m f g)
  (let ([bind (monad-ops-bind ops)])
    (equal? (bind (bind m f) g)
            (bind m (lambda (x) (bind (f x) g))))))

;;; verify-monad-laws : MonadOps × a × M a × (a → M a) × (a → M a) → Boolean
;;; Verify all three monad laws for given test values.
(define (verify-monad-laws ops a m f g)
  (and (verify-left-identity ops a f)
       (verify-right-identity ops m)
       (verify-associativity ops m f g)))

;;; ====
;;; Functor Law Verification
;;; ====
;;;
;;; A functor must satisfy:
;;;   1. Identity:    fmap id = id
;;;   2. Composition: fmap (g . f) = fmap g . fmap f

;;; verify-functor-identity : MonadOps × M a → Boolean
;;; Check: fmap id m = m
(define (verify-functor-identity ops m)
  (let ([fmap (monad-ops-fmap ops)])
    (equal? (fmap id m) m)))

;;; verify-functor-composition : MonadOps × (b → c) × (a → b) × M a → Boolean
;;; Check: fmap (g . f) m = fmap g (fmap f m)
(define (verify-functor-composition ops g f m)
  (let ([fmap (monad-ops-fmap ops)])
    (equal? (fmap (compose2 g f) m)
            (fmap g (fmap f m)))))

;;; ====
;;; Display
;;; ====

;;; monad-ops->string : MonadOps → String
(define (monad-ops->string ops)
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
;;; Verification:
;;;   - verify-left-identity, verify-right-identity, verify-associativity
;;;   - verify-monad-laws
;;;   - verify-functor-identity, verify-functor-composition
;;;
;;; Display:
;;;   - monad-ops->string
