;;; lattice/fp/category/state-store-adjunction.ss — State/Store Adjunction
;;;
;;; The product-exponential adjunction is the canonical source of State and Store:
;;;
;;;   (−) × S  ⊣  (−)^S
;;;   Product     Exponential (function type)
;;;
;;; Where:
;;;   - Left adjoint F = (−) × S: adds state component
;;;   - Right adjoint G = (−)^S: takes function from state
;;;
;;; This yields:
;;;   - Monad G∘F = (−)^S ∘ ((−) × S) = S → (S × −) = State S
;;;   - Comonad F∘G = ((−) × S) ∘ (−)^S = (S → −) × S = Store S
;;;
;;; The adjunction's:
;;;   - Unit η : a → (S → S × a) given by η(a) = λs. (s, a)
;;;   - Counit ε : (S → a) × S → a given by ε(f, s) = f(s)
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Features:
;;;   - Product functor (−) × S
;;;   - Exponential functor (−)^S
;;;   - The adjunction with unit/counit
;;;   - Derived State monad operations
;;;   - Derived Store comonad operations
;;;   - Verification that derived operations match hand-coded ones
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - fp/category/adjunction.ss
;;;   - fp/category/comonad.ss

(load "core/base/prelude.ss")
(load "lattice/fp/meta/combinators.ss")
(load "lattice/fp/templates.ss")
(load "lattice/fp/category/adjunction.ss")
(load "lattice/fp/category/comonad.ss")

;;; ====
;;; Product Functor: (−) × S
;;; ====
;;;
;;; (−) × S maps:
;;;   - Objects: a ↦ a × S = (a, S)
;;;   - Morphisms: f : a → b ↦ f × id_S : (a, S) → (b, S)

;;; make-product-functor : S → Functor
;;; Create the product functor (−) × S for a fixed state type S.
;;; In our untyped setting, S is implicit in the structure.
(define (make-product-functor)
  ;; fmap f (a, s) = (f a, s)
  (make-functor
   (lambda (f pair)
     (cons (f (car pair)) (cdr pair)))))

;;; product-functor : Functor
;;; The product functor (−) × S.
(define product-functor (make-product-functor))

;;; ====
;;; Exponential Functor: (−)^S
;;; ====
;;;
;;; (−)^S maps:
;;;   - Objects: a ↦ (S → a)
;;;   - Morphisms: f : a → b ↦ (f ∘ −) : (S → a) → (S → b)

;;; make-exponential-functor : → Functor
;;; Create the exponential functor (−)^S.
(define (make-exponential-functor)
  ;; fmap f g = f ∘ g for g : S → a
  (make-functor
   (lambda (f g)
     (lambda (s) (f (g s))))))

;;; exponential-functor : Functor
;;; The exponential functor (−)^S.
(define exponential-functor (make-exponential-functor))

;;; ====
;;; The Adjunction: (−) × S ⊣ (−)^S
;;; ====

;;; make-state-store-unit : → NatTransform
;;; Unit η : Id ⟹ (−)^S ∘ ((−) × S)
;;; η_a : a → (S → (a × S))
;;; η_a(x) = λs. (x, s)
(define (make-state-store-unit)
  (make-nat-transform
   'state-store-unit
   functor-id
   ;; Target: (−)^S ∘ ((−) × S) - the State functor
   (make-functor
    (lambda (f m)
      ;; m : S → (a × S), we need S → (b × S)
      ;; fmap f m = λs. let (a, s') = m(s) in (f a, s')
      (lambda (s)
        (let ([result (m s)])
          (cons (f (car result)) (cdr result))))))
   ;; Component: a ↦ (S → (a × S))
   (lambda (a)
     (lambda (s) (cons a s)))))

;;; make-state-store-counit : → NatTransform
;;; Counit ε : ((−) × S) ∘ (−)^S ⟹ Id
;;; ε_a : ((S → a) × S) → a
;;; ε_a(f, s) = f(s)
(define (make-state-store-counit)
  (make-nat-transform
   'state-store-counit
   ;; Source: ((−) × S) ∘ (−)^S - the Store functor
   (make-functor
    (lambda (f store)
      ;; store = ((S → a), S), we need ((S → b), S)
      ;; fmap f store = (f ∘ accessor, position)
      (cons (lambda (s) (f ((car store) s)))
            (cdr store))))
   functor-id
   ;; Component: ((S → a) × S) → a
   (lambda (store)
     ((car store) (cdr store)))))

;;; adj-state-store : Adjunction
;;; The product-exponential adjunction (−) × S ⊣ (−)^S.
(define adj-state-store
  (make-adjunction
   'state-store
   product-functor
   exponential-functor
   (make-state-store-unit)
   (make-state-store-counit)))

;;; ====
;;; Derived State Monad
;;; ====
;;;
;;; The monad G∘F = (−)^S ∘ ((−) × S) is the State monad.
;;; State S a = S → (a × S)

;;; state-from-adjunction-return : a → State S a
;;; Derived: return = η (unit of adjunction)
(define (state-from-adjunction-return a)
  ((nat-transform-component (make-state-store-unit)) a))

;;; state-from-adjunction-join : State S (State S a) → State S a
;;; Derived: join = G(ε_F)
;;; For m : S → ((S → (a × S)) × S), join flattens to S → (a × S)
(define (state-from-adjunction-join mm)
  (lambda (s)
    (let* ([outer-result (mm s)]           ; ((S → (a × S)), S)
           [inner-state (car outer-result)] ; S → (a × S)
           [new-s (cdr outer-result)])      ; S
      (inner-state new-s))))

;;; state-from-adjunction-bind : State S a × (a → State S b) → State S b
;;; Derived: m >>= f = join (fmap f m)
(define (state-from-adjunction-bind m f)
  (state-from-adjunction-join
   (lambda (s)
     (let ([result (m s)])
       (cons (f (car result)) (cdr result))))))

;;; ====
;;; Derived Store Comonad
;;; ====
;;;
;;; The comonad F∘G = ((−) × S) ∘ (−)^S is the Store comonad.
;;; Store S a = (S → a) × S

;;; store-from-adjunction-extract : Store S a → a
;;; Derived: extract = ε (counit of adjunction)
(define (store-from-adjunction-extract store)
  ((nat-transform-component (make-state-store-counit)) store))

;;; store-from-adjunction-duplicate : Store S a → Store S (Store S a)
;;; Derived: duplicate = F(η_G)
;;; For (f, s) : (S → a) × S, duplicate = (λs'. (f, s'), s)
(define (store-from-adjunction-duplicate store)
  (let ([f (car store)]
        [s (cdr store)])
    ;; New accessor: given position s', return store focused at s'
    (cons (lambda (s-prime) (cons f s-prime))
          s)))

;;; store-from-adjunction-extend : (Store S a → b) × Store S a → Store S b
;;; Derived: extend g w = fmap g (duplicate w)
(define (store-from-adjunction-extend g store)
  (let ([f (car store)]
        [s (cdr store)])
    ;; New accessor: at each position s', apply g to store focused there
    (cons (lambda (s-prime)
            (g (cons f s-prime)))
          s)))

;;; ====
;;; Verification: Adjunction Laws
;;; ====

;;; verify-state-store-triangle-left : (a × S) → Boolean
;;; Verify left triangle: ε_{F(a)} ∘ F(η_a) = id_{F(a)}
(define (verify-state-store-triangle-left pair)
  (verify-triangle-left adj-state-store pair))

;;; verify-state-store-triangle-right : (S → a) → Boolean
;;; Verify right triangle: G(ε_a) ∘ η_{G(a)} = id_{G(a)}
(define (verify-state-store-triangle-right func s-test)
  ;; We need to check at a specific input
  (let* ([η (nat-transform-component (make-state-store-unit))]
         [ε (nat-transform-component (make-state-store-counit))]
         ;; Step 1: η_{G(a)}(func) : S → ((S → a) × S)
         [step1 (η func)]
         ;; Step 2: G(ε)(step1) : S → a
         ;; G(ε) = ε ∘ -, so G(ε)(step1) = λs. ε(step1(s))
         [step2 (lambda (s)
                  (ε (step1 s)))])  ; Apply ε to the pair at s
    ;; Should equal func
    (equal? (func s-test) (step2 s-test))))

;;; ====
;;; Verification: Derived Operations Match Hand-coded
;;; ====

;;; verify-state-return-matches : a × S → Boolean
;;; Check that derived return matches standard State return.
(define (verify-state-return-matches a s)
  (let* ([derived ((state-from-adjunction-return a) s)]
         [standard (cons a s)])  ; Standard: λs. (a, s)
    (equal? derived standard)))

;;; verify-store-extract-matches : (S → a) × S → Boolean
;;; Check that derived extract matches standard Store extract.
(define (verify-store-extract-matches accessor pos)
  (let* ([store (cons accessor pos)]
         [derived (store-from-adjunction-extract store)]
         [standard (accessor pos)])  ; Standard: accessor(pos)
    (equal? derived standard)))

;;; ====
;;; Convenience: State Monad from Adjunction
;;; ====

;;; state-adjunction-monad : MonadOps
;;; Package the State monad operations derived from the adjunction.
(define state-adjunction-monad
  (list 'monad
        'state-from-adjunction
        state-from-adjunction-return
        state-from-adjunction-bind))

;;; ====
;;; Convenience: Store Comonad from Adjunction
;;; ====

;;; store-adjunction-comonad : Comonad
;;; The Store comonad derived from the adjunction.
;;; Note: This matches store-comonad from comonad.ss
(define store-adjunction-comonad
  (make-comonad
   ;; Functor for Store
   (make-functor
    (lambda (f store)
      (cons (lambda (s) (f ((car store) s)))
            (cdr store))))
   store-from-adjunction-extract
   store-from-adjunction-extend))

;;; ====
;;; Categorical Interpretation
;;; ====
;;;
;;; The adjunction (−) × S ⊣ (−)^S can be understood as:
;;;
;;; Hom(A × S, B) ≅ Hom(A, S → B)
;;;
;;; This is currying! The isomorphism is:
;;;   - Left to right: f : A × S → B ↦ curry(f) : A → S → B
;;;   - Right to left: g : A → S → B ↦ uncurry(g) : A × S → B
;;;
;;; The adjunction encodes the curry/uncurry isomorphism categorically.

;;; curry-via-adjunction : ((a × S) → b) → (a → S → b)
;;; Curry using adjunction transpose.
(define (curry-via-adjunction f)
  (adjunction-transpose-left adj-state-store f))

;;; uncurry-via-adjunction : (a → S → b) → ((a × S) → b)
;;; Uncurry using adjunction transpose.
(define (uncurry-via-adjunction g)
  (adjunction-transpose-right adj-state-store g))

;;; ====
;;; Display
;;; ====

;;; adj-state-store->string : → String
(define (adj-state-store->string)
  "(−) × S ⊣ (−)^S : Product-Exponential Adjunction")
