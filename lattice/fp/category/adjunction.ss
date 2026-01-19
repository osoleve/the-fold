;;; lattice/fp/category/adjunction.ss — Adjoint Functors
;;;
;;; An adjunction F ⊣ G consists of:
;;;   - Left adjoint F : C → D
;;;   - Right adjoint G : D → C  
;;;   - Unit η : Id_C ⟹ G∘F
;;;   - Counit ε : F∘G ⟹ Id_D
;;;
;;; Triangle identities:
;;;   (ε ◁ F) ∘ (F ▷ η) = id_F
;;;   (G ▷ ε) ∘ (η ◁ G) = id_G

(load "lattice/fp/category/natural-transform.ss")

;;; ====
;;; Adjunction Definition
;;; ====

;;; make-adjunction : Symbol × Functor × Functor × NatTransform × NatTransform → Adjunction
(define (make-adjunction name left right unit counit)
  (list 'adjunction name left right unit counit))

;;; adjunction? : Any → Boolean
(define (adjunction? x)
  (and (pair? x)
       (eq? (car x) 'adjunction)
       (= (length x) 6)))

;;; adjunction-name : Adjunction → Symbol
(define (adjunction-name adj)
  (if (adjunction? adj) (cadr adj) 'unknown))

;;; adjunction-left : Adjunction → Functor
(define (adjunction-left adj)
  (if (adjunction? adj) (caddr adj) #f))

;;; adjunction-right : Adjunction → Functor
(define (adjunction-right adj)
  (if (adjunction? adj) (cadddr adj) #f))

;;; adjunction-unit : Adjunction → NatTransform
(define (adjunction-unit adj)
  (if (adjunction? adj) (car (cddddr adj)) #f))

;;; adjunction-counit : Adjunction → NatTransform
(define (adjunction-counit adj)
  (if (adjunction? adj) (cadr (cddddr adj)) #f))

;;; ====
;;; Utilities
;;; ====

;;; functor-id : Functor
;;; Identity functor: Id(f) = f, Id(x) = x
(define functor-id
  (make-functor (lambda (f x) (f x))))

;;; ====
;;; Triangle Identities
;;; ====

;;; verify-triangle-left : Adjunction × F(A) → Boolean
;;; Verify (ε ◁ F) ∘ (F ▷ η) = id_F at a specific value.
;;; Left triangle: ε_{F(A)} ∘ F(η_A) = id_{F(A)}
(define (verify-triangle-left adj val)
  (let* ([F (adjunction-left adj)]
         [η (adjunction-unit adj)]
         [ε (adjunction-counit adj)]
         [η-comp (nat-transform-component η)]
         [ε-comp (nat-transform-component ε)]
         [F-fmap (functor-fmap F)])
    ;; val is in F(A)
    ;; Step 1: F(η_A)(val)
    ;; η_A : A -> G(F(A))
    ;; F-fmap applies η_A to elements in val
    (let* ([step1 (F-fmap η-comp val)]
           ;; Step 2: ε_{F(A)}(step1)
           ;; ε : F∘G -> Id
           [result (ε-comp step1)])
      (equal? result val))))

;;; verify-triangle-right : Adjunction × G(B) → Boolean
;;; Verify (G ▷ ε) ∘ (η ◁ G) = id_G
;;; Right triangle: G(ε_B) ∘ η_{G(B)} = id_{G(B)}
(define (verify-triangle-right adj val)
  (let* ([G (adjunction-right adj)]
         [η (adjunction-unit adj)]
         [ε (adjunction-counit adj)]
         [η-comp (nat-transform-component η)]
         [ε-comp (nat-transform-component ε)]
         [G-fmap (functor-fmap G)])
    ;; val is in G(B)
    ;; Step 1: η_{G(B)}(val)
    (let* ([step1 (η-comp val)]
           ;; Step 2: G(ε_B)(step1)
           [result (G-fmap ε-comp step1)])
      (equal? result val))))

;;; verify-adjunction : Adjunction × F(A) × G(B) → Boolean
(define (verify-adjunction adj val-in-left val-in-right)
  (and (verify-triangle-left adj val-in-left)
       (verify-triangle-right adj val-in-right)))

;;; ====
;;; Hom-Set Bijection
;;; ====

;;; adjunction-transpose-left : Adjunction × (F(A) → B) → (A → G(B))
;;; Left-to-Right: f ↦ G(f) ∘ η_A
(define (adjunction-transpose-left adj f)
  (let* ([G (adjunction-right adj)]
         [η (adjunction-unit adj)]
         [G-fmap (functor-fmap G)]
         [η-comp (nat-transform-component η)])
    (lambda (x)
      (G-fmap f (η-comp x)))))

;;; adjunction-transpose-right : Adjunction × (A → G(B)) → (F(A) → B)
;;; Right-to-Left: g ↦ ε_B ∘ F(g)
(define (adjunction-transpose-right adj g)
  (let* ([F (adjunction-left adj)]
         [ε (adjunction-counit adj)]
         [F-fmap (functor-fmap F)]
         [ε-comp (nat-transform-component ε)])
    (lambda (x)
      (ε-comp (F-fmap g x)))))

;;; ====
;;; Adjunction Composition
;;; ====

;;; adjunction-compose : Adjunction × Adjunction → Adjunction
;;; Compose F ⊣ G and F' ⊣ G' to get F'∘F ⊣ G∘G'
;;; Precondition: both arguments are valid adjunctions.
;;; This is pure lattice code; use boundary/fp/category.ss for validated entry points.
(define (adjunction-compose adj2 adj1)
  (let* ([F (adjunction-left adj1)]
         [G (adjunction-right adj1)]
         [η (adjunction-unit adj1)]
         [ε (adjunction-counit adj1)]
         [F-prime (adjunction-left adj2)]
         [G-prime (adjunction-right adj2)]
         [η-prime (adjunction-unit adj2)]
         [ε-prime (adjunction-counit adj2)]
         ;; Hoist functor-fmap lookups out of hot path
         [F-fmap (functor-fmap F)]
         [G-fmap (functor-fmap G)]
         [F-prime-fmap (functor-fmap F-prime)]
         [G-prime-fmap (functor-fmap G-prime)])
    (let ([new-left (make-functor (lambda (f x) (F-prime-fmap (lambda (y) (F-fmap f y)) x)))]
          [new-right (make-functor (lambda (f x) (G-fmap (lambda (y) (G-prime-fmap f y)) x)))])
      (make-adjunction
       (string->symbol (format "~a∘~a" (adjunction-name adj2) (adjunction-name adj1)))
       new-left
       new-right
       (nat-compose (nat-whisker-right (nat-whisker-left G η-prime) F) η)
       (nat-compose ε-prime (nat-whisker-left F-prime (nat-whisker-right ε G-prime)))))))

;;; ====
;;; Common Adjunctions
;;; ====

;;; adj-free-list : Adjunction
;;; Free monoid adjunction: List ⊣ Id
;;; This works for "List as Monoids", where F=List, G=Id.
(define adj-free-list
  (make-adjunction
   'free-list
   functor-list
   functor-id
   nat-pure-list ; η : Id ⟹ List (singleton)
   nat-concat    ; ε : List∘List ⟹ List (flatten)
   ))

;;; ====
;;; Galois Connections
;;; ====

;;; make-galois : Symbol × (P → Q) × (Q → P) → Galois
;;; Create a Galois connection (adjunction between preorders).
;;; Lower adjoint (left) and Upper adjoint (right).
(define (make-galois name lower upper)
  (list 'galois name lower upper))

;;; galois? : Any → Boolean
(define (galois? x)
  (and (pair? x)
       (eq? (car x) 'galois)
       (= (length x) 4)))

;;; galois-lower : Galois → (P → Q)
(define (galois-lower g) (caddr g))

;;; galois-upper : Galois → (Q → P)
(define (galois-upper g) (cadddr g))

;;; galois-closure : Galois × P → P
;;; Closure operator: u ∘ l
(define (galois-closure g x)
  ((galois-upper g) ((galois-lower g) x)))

;;; galois-kernel : Galois × Q → Q
;;; Kernel/Interior operator: l ∘ u
(define (galois-kernel g x)
  ((galois-lower g) ((galois-upper g) x)))

;;; galois-floor-ceil : Galois Connection
;;; Example: Ceiling ⊣ Inclusion (Integer ⊆ Real)
;;; ceil : Real → Int (Lower)
;;; inclusion : Int → Real (Upper)
;;; ceil(r) ≤ i ⟺ r ≤ inclusion(i)
(define galois-floor-ceil
  (make-galois
   'ceil-inclusion
   ceiling
   (lambda (x) x)))

;;; ====
;;; Display
;;; ====

;;; adjunction->string : Adjunction → String
(define (adjunction->string adj)
  (if (adjunction? adj)
      (format "~a : ~a ⊣ ~a"
              (adjunction-name adj)
              "L" ; Functor names not readily available
              "R")
      "Not an adjunction"))
