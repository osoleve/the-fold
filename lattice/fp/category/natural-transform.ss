;;; lattice/fp/category/natural-transform.ss — Natural Transformations
;;;
;;; Natural transformations are morphisms between functors.
;;; Given functors F, G : C → D, a natural transformation η : F ⟹ G
;;; assigns to each object A a morphism η_A : F(A) → G(A) such that
;;; for every morphism f : A → B, the naturality square commutes:
;;;
;;;        F(A) ----η_A----> G(A)
;;;         |                 |
;;;       F(f)              G(f)
;;;         |                 |
;;;         v                 v
;;;        F(B) ----η_B----> G(B)
;;;
;;; That is: G(f) ∘ η_A = η_B ∘ F(f)
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Features:
;;;   - Natural transformation definition
;;;   - Vertical composition (η ∘ ε)
;;;   - Horizontal composition (η * ε)
;;;   - Whiskering operations (F ▷ η and η ◁ G)
;;;   - Natural isomorphism detection
;;;   - Naturality condition verification
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - fp/meta/combinators.ss
;;;   - fp/templates.ss (for functor definitions)

(load "core/base/prelude.ss")
(load "lattice/fp/meta/combinators.ss")
(load "lattice/fp/templates.ss")

;;; ====
;;; Natural Transformation Definition
;;; ====
;;;
;;; A natural transformation is represented as:
;;;   (nat-transform name source-functor target-functor component-fn)
;;;
;;; Where component-fn : ∀A. F(A) → G(A)
;;; In our untyped setting, component-fn is a single polymorphic function.

;;; make-nat-transform : Symbol × Functor × Functor × (F(A) → G(A)) → NatTransform
;;; Create a natural transformation from F to G.
(define (make-nat-transform name source target component)
  (list 'nat-transform name source target component))

;;; nat-transform? : Any → Boolean
(define (nat-transform? x)
  (and (pair? x)
       (eq? (car x) 'nat-transform)
       (= (length x) 5)))

;;; nat-transform-name : NatTransform → Symbol
(define (nat-transform-name η)
  (if (nat-transform? η) (cadr η) 'unknown))

;;; nat-transform-source : NatTransform → Functor
;;; The source functor F in η : F ⟹ G
(define (nat-transform-source η)
  (if (nat-transform? η) (caddr η) #f))

;;; nat-transform-target : NatTransform → Functor
;;; The target functor G in η : F ⟹ G
(define (nat-transform-target η)
  (if (nat-transform? η) (cadddr η) #f))

;;; nat-transform-component : NatTransform → (F(A) → G(A))
;;; The component function η_A
;;; Errors if η is not a valid nat-transform (fail-fast, fold-zxnw fix)
(define (nat-transform-component η)
  (if (nat-transform? η)
      (car (cddddr η))
      (error 'nat-transform-component "expected nat-transform" η)))

;;; nat-apply : NatTransform × F(A) → G(A)
;;; Apply the natural transformation at a value.
;;; η_A(x) for x : F(A)
(define (nat-apply η x)
  ((nat-transform-component η) x))

;;; ====
;;; Identity Natural Transformation
;;; ====

;;; nat-id : Functor → NatTransform
;;; The identity natural transformation id_F : F ⟹ F
;;; Components are all identity functions: (id_F)_A = id_{F(A)}
(define (nat-id functor)
  (make-nat-transform 'id functor functor id))

;;; ====
;;; Vertical Composition
;;; ====
;;;
;;; Given η : F ⟹ G and ε : G ⟹ H, their vertical composition
;;; (ε ∘ η) : F ⟹ H has components (ε ∘ η)_A = ε_A ∘ η_A
;;;
;;;        F(A) ---η_A---> G(A) ---ε_A---> H(A)

;;; nat-compose : NatTransform × NatTransform → NatTransform
;;; Vertical composition: (ε ∘ η) where η : F ⟹ G and ε : G ⟹ H
;;; Precondition: both arguments are valid nat-transforms,
;;;               target of η equals source of ε.
;;; This is pure lattice code; use shell/fp/category.ss for validated entry points.
(define (nat-compose ε η)
  (let ([source (nat-transform-source η)]
        [target (nat-transform-target ε)]
        [η-comp (nat-transform-component η)]
        [ε-comp (nat-transform-component ε)])
    (make-nat-transform
     (string->symbol
      (format "~a∘~a" (nat-transform-name ε) (nat-transform-name η)))
     source
     target
     (lambda (x) (ε-comp (η-comp x))))))

;;; nat-∘ : Alias for nat-compose
(define nat-∘ nat-compose)

;;; ====
;;; Horizontal Composition
;;; ====
;;;
;;; Given η : F ⟹ G and ε : H ⟹ K (where these are functors between
;;; appropriate categories), their horizontal composition (η * ε)
;;; is defined as:
;;;
;;; If F, G : C → D and H, K : D → E, then
;;; (η * ε) : H∘F ⟹ K∘G with components:
;;; (η * ε)_A = K(η_A) ∘ ε_{F(A)} = ε_{G(A)} ∘ H(η_A)
;;;
;;; In our setting with endofunctors on the category of Scheme values,
;;; this simplifies to composing the functors and transformations.

;;; nat-horizontal : NatTransform × NatTransform → NatTransform
;;; Horizontal composition: (η * ε)
;;; Given η : F ⟹ G and ε : H ⟹ K, produces (η * ε) : H∘F ⟹ K∘G
(define (nat-horizontal η ε)
  (let* ([F (nat-transform-source η)]
         [G (nat-transform-target η)]
         [H (nat-transform-source ε)]
         [K (nat-transform-target ε)]
         [η-comp (nat-transform-component η)]
         [ε-comp (nat-transform-component ε)]
         [H-fmap (functor-fmap H)]
         [K-fmap (functor-fmap K)]
         [F-fmap (functor-fmap F)]
         [G-fmap (functor-fmap G)])
    ;; Compose functors: H∘F and K∘G
    ;; (H∘F)(f) = H(F(f)) applied to container
    (let ([HF (make-functor (lambda (f x) (H-fmap (lambda (y) (F-fmap f y)) x)))]
          [KG (make-functor (lambda (f x) (K-fmap (lambda (y) (G-fmap f y)) x)))])
      (make-nat-transform
       (string->symbol
        (format "~a*~a" (nat-transform-name η) (nat-transform-name ε)))
       HF
       KG
       ;; (η * ε)_A = K(η_A) ∘ ε_{F(A)} (Godement product)
       ;; In our setting: apply ε-comp, then map η-comp via K
       (lambda (x)
         (K-fmap η-comp (ε-comp x)))))))

;;; nat-* : Alias for nat-horizontal
(define nat-* nat-horizontal)

;;; ====
;;; Whiskering Operations
;;; ====
;;;
;;; Whiskering composes a natural transformation with a functor.
;;;
;;; Right whiskering (η ◁ H): Given η : F ⟹ G and H : D → C,
;;; produces (η ◁ H) : F∘H ⟹ G∘H with (η ◁ H)_A = η_{H(A)}
;;;
;;; Left whiskering (H ▷ η): Given H : D → E and η : F ⟹ G,
;;; produces (H ▷ η) : H∘F ⟹ H∘G with (H ▷ η)_A = H(η_A)

;;; nat-whisker-right : NatTransform × Functor → NatTransform
;;; Right whiskering: (η ◁ H) : F∘H ⟹ G∘H
;;; Given η : F ⟹ G and functor H, precompose with H
(define (nat-whisker-right η H)
  (let* ([F (nat-transform-source η)]
         [G (nat-transform-target η)]
         [η-comp (nat-transform-component η)]
         [H-fmap (functor-fmap H)]
         [F-fmap (functor-fmap F)]
         [G-fmap (functor-fmap G)])
    ;; F∘H and G∘H: (F∘H)(f) = F(H(f)), lifting H(f) through F
    ;; For x : F(H(A)), we map (λy. H(f)(y)) over the F-container
    (let ([FH (make-functor (lambda (f x) (F-fmap (lambda (y) (H-fmap f y)) x)))]
          [GH (make-functor (lambda (f x) (G-fmap (lambda (y) (H-fmap f y)) x)))])
      (make-nat-transform
       (string->symbol (format "~a◁H" (nat-transform-name η)))
       FH
       GH
       ;; (η ◁ H)_A = η_{H(A)}, but since η-comp is polymorphic, just η-comp
       η-comp))))

;;; nat-◁ : Alias for nat-whisker-right
(define nat-◁ nat-whisker-right)

;;; nat-whisker-left : Functor × NatTransform → NatTransform
;;; Left whiskering: (H ▷ η) : H∘F ⟹ H∘G
;;; Given functor H and η : F ⟹ G, postcompose with H
(define (nat-whisker-left H η)
  (let* ([F (nat-transform-source η)]
         [G (nat-transform-target η)]
         [η-comp (nat-transform-component η)]
         [H-fmap (functor-fmap H)]
         [F-fmap (functor-fmap F)]
         [G-fmap (functor-fmap G)])
    ;; H∘F and H∘G: (H∘F)(f) = H(F(f)), lifting F(f) through H
    ;; For x : H(F(A)), we map (λy. F(f)(y)) over the H-container
    (let ([HF (make-functor (lambda (f x) (H-fmap (lambda (y) (F-fmap f y)) x)))]
          [HG (make-functor (lambda (f x) (H-fmap (lambda (y) (G-fmap f y)) x)))])
      (make-nat-transform
       (string->symbol (format "H▷~a" (nat-transform-name η)))
       HF
       HG
       ;; (H ▷ η)_A = H(η_A): map η-comp over H-container
       (lambda (x) (H-fmap η-comp x))))))

;;; nat-▷ : Alias for nat-whisker-left
(define nat-▷ nat-whisker-left)

;;; ====
;;; Natural Isomorphisms
;;; ====
;;;
;;; A natural isomorphism is a natural transformation where each
;;; component is an isomorphism. η : F ⟹ G is a natural iso if
;;; there exists ε : G ⟹ F such that ε ∘ η = id_F and η ∘ ε = id_G.

;;; make-nat-iso : Symbol × Functor × Functor × (F(A) → G(A)) × (G(A) → F(A)) → NatIso
;;; Create a natural isomorphism with explicit inverse.
(define (make-nat-iso name source target forward inverse)
  (list 'nat-iso name source target forward inverse))

;;; nat-iso? : Any → Boolean
(define (nat-iso? x)
  (and (pair? x)
       (eq? (car x) 'nat-iso)
       (= (length x) 6)))

;;; nat-iso-forward : NatIso → NatTransform
;;; The forward direction η : F ⟹ G
(define (nat-iso-forward iso)
  (if (nat-iso? iso)
      (make-nat-transform
       (cadr iso)
       (caddr iso)
       (cadddr iso)
       (car (cddddr iso)))
      #f))

;;; nat-iso-inverse : NatIso → NatTransform
;;; The inverse direction ε : G ⟹ F
(define (nat-iso-inverse iso)
  (if (nat-iso? iso)
      (make-nat-transform
       (string->symbol (format "~a⁻¹" (cadr iso)))
       (cadddr iso)    ; G is now source
       (caddr iso)     ; F is now target
       (cadr (cddddr iso)))
      #f))

;;; ====
;;; Naturality Verification
;;; ====
;;;
;;; Given η : F ⟹ G, verify the naturality condition:
;;; For all f : A → B, G(f) ∘ η_A = η_B ∘ F(f)
;;;
;;; Since we can't quantify over all morphisms, we test with
;;; specific values and functions.

;;; check-naturality : NatTransform × (A → B) × F(A) → Boolean
;;; Check that naturality holds for a specific morphism and value.
;;; Tests: G(f)(η_A(x)) = η_B(F(f)(x))
(define (check-naturality η f x)
  (let* ([F (nat-transform-source η)]
         [G (nat-transform-target η)]
         [η-comp (nat-transform-component η)]
         [F-fmap (functor-fmap F)]
         [G-fmap (functor-fmap G)]
         ;; Left side: G(f) ∘ η_A applied to x
         ;; Note: fmap takes (f, container), not curried
         [left (G-fmap f (η-comp x))]
         ;; Right side: η_B ∘ F(f) applied to x
         [right (η-comp (F-fmap f x))])
    (equal? left right)))

;;; verify-naturality : NatTransform × (List (Pair Fn Value)) → Boolean
;;; Verify naturality for a list of (function, value) test cases.
(define (verify-naturality η test-cases)
  (andmap (lambda (tc)
            (check-naturality η (car tc) (cdr tc)))
          test-cases))

;;; ====
;;; Common Natural Transformations
;;; ====

;;; nat-head : NatTransform from List to Maybe
;;; η_A : [A] → Maybe A, returns Just (head) or Nothing
(define nat-head
  (make-nat-transform
   'head
   functor-list
   functor-maybe
   (lambda (xs)
     (if (null? xs)
         nothing
         (just (car xs))))))

;;; nat-singleton : NatTransform from Maybe to List
;;; η_A : Maybe A → [A], Nothing → [], Just x → [x]
(define nat-singleton
  (make-nat-transform
   'singleton
   functor-maybe
   functor-list
   (lambda (m)
     (if (just? m)
         (list (from-just m))
         '()))))

;;; nat-maybe-to-either : (E → NatTransform from Maybe to Either E)
;;; Given a default error value, transform Maybe to Either
(define (nat-maybe-to-either default-error)
  (make-nat-transform
   'maybe->either
   functor-maybe
   functor-either
   (lambda (m)
     (if (just? m)
         (right (from-just m))
         (left default-error)))))

;;; nat-either-to-maybe : NatTransform from Either to Maybe
;;; Forgets the error, keeping only success
(define nat-either-to-maybe
  (make-nat-transform
   'either->maybe
   functor-either
   functor-maybe
   (lambda (e)
     (if (right? e)
         (just (from-right e))
         nothing))))

;;; nat-concat : NatTransform from (List ∘ List) to List
;;; The join/flatten operation: [[A]] → [A]
;;; This is the multiplication of the List monad
(define nat-concat
  (let ([list-list (make-functor (lambda (f xss) (map (lambda (xs) (map f xs)) xss)))])
    (make-nat-transform
     'concat
     list-list
     functor-list
     (lambda (xss) (apply append xss)))))

;;; nat-pure-list : NatTransform from Identity to List
;;; η_A : A → [A], the unit of the List monad
(define nat-pure-list
  (let ([functor-id (make-functor (lambda (f x) (f x)))])  ; Identity functor
    (make-nat-transform
     'pure-list
     functor-id
     functor-list
     list)))

;;; nat-pure-maybe : NatTransform from Identity to Maybe
;;; η_A : A → Maybe A, wraps value in Just
(define nat-pure-maybe
  (let ([functor-id (make-functor (lambda (f x) (f x)))])
    (make-nat-transform
     'pure-maybe
     functor-id
     functor-maybe
     just)))

;;; ====
;;; Display
;;; ====

;;; nat-transform->string : NatTransform → String
(define (nat-transform->string η)
  (if (nat-transform? η)
      (format "~a : ~a ⟹ ~a"
              (nat-transform-name η)
              (if (functor? (nat-transform-source η)) "F" "?")
              (if (functor? (nat-transform-target η)) "G" "?"))
      "Not a natural transformation"))
