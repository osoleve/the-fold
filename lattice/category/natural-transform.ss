;;; lattice/category/natural-transform.ss — Natural Transformations
;;; @module natural-transform
;;; @requires prelude combinators templates

(require 'prelude)
(require 'combinators)
(require 'templates)

(doc 'module 'natural-transform)
(doc 'description "Natural Transformations

Natural transformations are morphisms between functors.
Given functors F, G : C → D, a natural transformation η : F ⟹ G
assigns to each object A a morphism η_A : F(A) → G(A) such that
for every morphism f : A → B, the naturality square commutes:

       F(A) ----η_A----> G(A)
        |                 |
      F(f)              G(f)
        |                 |
        v                 v
       F(B) ----η_B----> G(B)

That is: G(f) ∘ η_A = η_B ∘ F(f)")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'note "Features:
  - Natural transformation definition
  - Vertical composition (η ∘ ε)
  - Horizontal composition (η * ε)
  - Whiskering operations (F ▷ η and η ◁ G)
  - Natural isomorphism detection
  - Naturality condition verification")

(doc 'section 'definition)
(doc 'description "Natural Transformation Definition

A natural transformation is represented as:
  (nat-transform name source-functor target-functor component-fn)

Where component-fn : ∀A. F(A) → G(A)
In our untyped setting, component-fn is a single polymorphic function.")

(define (make-nat-transform name source target component)
  (doc 'type '(-> Symbol Functor Functor (-> (F A) (G A)) NatTransform))
  (doc 'description "Create a natural transformation from F to G")
  (list 'nat-transform name source target component))

(define (nat-transform? x)
  (doc 'type '(-> Any Boolean))
  (and (pair? x)
       (eq? (car x) 'nat-transform)
       (= (length x) 5)))

(define (nat-transform-name η)
  (doc 'type '(-> NatTransform Symbol))
  (doc 'description "Get the name of a natural transformation.
Errors if η is not a valid nat-transform (fail-fast)")
  (if (nat-transform? η)
      (cadr η)
      (error 'nat-transform-name "expected nat-transform" η)))

(define (nat-transform-source η)
  (doc 'type '(-> NatTransform Functor))
  (doc 'description "The source functor F in η : F ⟹ G.
Errors if η is not a valid nat-transform (fail-fast)")
  (if (nat-transform? η)
      (caddr η)
      (error 'nat-transform-source "expected nat-transform" η)))

(define (nat-transform-target η)
  (doc 'type '(-> NatTransform Functor))
  (doc 'description "The target functor G in η : F ⟹ G.
Errors if η is not a valid nat-transform (fail-fast)")
  (if (nat-transform? η)
      (cadddr η)
      (error 'nat-transform-target "expected nat-transform" η)))

(define (nat-transform-component η)
  (doc 'type '(-> NatTransform (-> (F A) (G A))))
  (doc 'description "The component function η_A.
Errors if η is not a valid nat-transform (fail-fast)")
  (if (nat-transform? η)
      (car (cddddr η))
      (error 'nat-transform-component "expected nat-transform" η)))

(define (nat-apply η x)
  (doc 'type '(-> NatTransform (F A) (G A)))
  (doc 'description "Apply the natural transformation at a value.
η_A(x) for x : F(A)")
  ((nat-transform-component η) x))

(doc 'section 'identity)

(define (nat-id functor)
  (doc 'type '(-> Functor NatTransform))
  (doc 'description "The identity natural transformation id_F : F ⟹ F
Components are all identity functions: (id_F)_A = id_{F(A)}")
  (make-nat-transform 'id functor functor id))

(doc 'section 'vertical-composition)
(doc 'description "Vertical Composition

Given η : F ⟹ G and ε : G ⟹ H, their vertical composition
(ε ∘ η) : F ⟹ H has components (ε ∘ η)_A = ε_A ∘ η_A

       F(A) ---η_A---> G(A) ---ε_A---> H(A)")

(define (nat-compose ε η)
  (doc 'type '(-> NatTransform NatTransform NatTransform))
  (doc 'description "Vertical composition: (ε ∘ η) where η : F ⟹ G and ε : G ⟹ H
Precondition: both arguments are valid nat-transforms,
              target of η equals source of ε.
This is pure lattice code; use boundary/fp/category.ss for validated entry points.")
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

(doc nat-∘ 'type '(-> NatTransform NatTransform NatTransform))
(doc nat-∘ 'description "Alias for nat-compose")
(define nat-∘ nat-compose)

(doc 'section 'horizontal-composition)
(doc 'description "Horizontal Composition

Given η : F ⟹ G and ε : H ⟹ K (where these are functors between
appropriate categories), their horizontal composition (η * ε)
is defined as:

If F, G : C → D and H, K : D → E, then
(η * ε) : H∘F ⟹ K∘G with components:
(η * ε)_A = K(η_A) ∘ ε_{F(A)} = ε_{G(A)} ∘ H(η_A)

In our setting with endofunctors on the category of Scheme values,
this simplifies to composing the functors and transformations.")

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
    (let ([HF (make-named-functor
               (string->symbol (format "~a∘~a" (functor-name H) (functor-name F)))
               (lambda (f x) (H-fmap (lambda (y) (F-fmap f y)) x)))]
          [KG (make-named-functor
               (string->symbol (format "~a∘~a" (functor-name K) (functor-name G)))
               (lambda (f x) (K-fmap (lambda (y) (G-fmap f y)) x)))])
      (make-nat-transform
       (string->symbol
        (format "~a*~a" (nat-transform-name η) (nat-transform-name ε)))
       HF
       KG
       ;; (η * ε)_A = K(η_A) ∘ ε_{F(A)} (Godement product)
       ;; In our setting: apply ε-comp, then map η-comp via K
       (lambda (x)
         (K-fmap η-comp (ε-comp x)))))))

(doc nat-* 'type '(-> NatTransform NatTransform NatTransform))
(doc nat-* 'description "Alias for nat-horizontal")
(define nat-* nat-horizontal)

(doc 'section 'whiskering)
(doc 'description "Whiskering Operations

Whiskering composes a natural transformation with a functor.

Right whiskering (η ◁ H): Given η : F ⟹ G and H : D → C,
produces (η ◁ H) : F∘H ⟹ G∘H with (η ◁ H)_A = η_{H(A)}

Left whiskering (H ▷ η): Given H : D → E and η : F ⟹ G,
produces (H ▷ η) : H∘F ⟹ H∘G with (H ▷ η)_A = H(η_A)")

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
    (let ([FH (make-named-functor
               (string->symbol (format "~a∘~a" (functor-name F) (functor-name H)))
               (lambda (f x) (F-fmap (lambda (y) (H-fmap f y)) x)))]
          [GH (make-named-functor
               (string->symbol (format "~a∘~a" (functor-name G) (functor-name H)))
               (lambda (f x) (G-fmap (lambda (y) (H-fmap f y)) x)))])
      (make-nat-transform
       (string->symbol (format "~a◁H" (nat-transform-name η)))
       FH
       GH
       ;; (η ◁ H)_A = η_{H(A)}, but since η-comp is polymorphic, just η-comp
       η-comp))))

(doc nat-◁ 'type '(-> NatTransform Functor NatTransform))
(doc nat-◁ 'description "Alias for nat-whisker-right")
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
    (let ([HF (make-named-functor
               (string->symbol (format "~a∘~a" (functor-name H) (functor-name F)))
               (lambda (f x) (H-fmap (lambda (y) (F-fmap f y)) x)))]
          [HG (make-named-functor
               (string->symbol (format "~a∘~a" (functor-name H) (functor-name G)))
               (lambda (f x) (H-fmap (lambda (y) (G-fmap f y)) x)))])
      (make-nat-transform
       (string->symbol (format "H▷~a" (nat-transform-name η)))
       HF
       HG
       ;; (H ▷ η)_A = H(η_A): map η-comp over H-container
       (lambda (x) (H-fmap η-comp x))))))

(doc nat-▷ 'type '(-> Functor NatTransform NatTransform))
(doc nat-▷ 'description "Alias for nat-whisker-left")
(define nat-▷ nat-whisker-left)

(doc 'section 'natural-isomorphisms)
(doc 'description "Natural Isomorphisms

A natural isomorphism is a natural transformation where each
component is an isomorphism. η : F ⟹ G is a natural iso if
there exists ε : G ⟹ F such that ε ∘ η = id_F and η ∘ ε = id_G.")

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
;;; Errors if iso is not a valid nat-iso (fail-fast)
(define (nat-iso-forward iso)
  (if (nat-iso? iso)
      (make-nat-transform
       (cadr iso)
       (caddr iso)
       (cadddr iso)
       (car (cddddr iso)))
      (error 'nat-iso-forward "expected nat-iso" iso)))

;;; nat-iso-inverse : NatIso → NatTransform
;;; The inverse direction ε : G ⟹ F
;;; Errors if iso is not a valid nat-iso (fail-fast)
(define (nat-iso-inverse iso)
  (if (nat-iso? iso)
      (make-nat-transform
       (string->symbol (format "~a⁻¹" (cadr iso)))
       (cadddr iso)    ; G is now source
       (caddr iso)     ; F is now target
       (cadr (cddddr iso)))
      (error 'nat-iso-inverse "expected nat-iso" iso)))

(doc 'section 'naturality-verification)
(doc 'description "Naturality Verification

Given η : F ⟹ G, verify the naturality condition:
For all f : A → B, G(f) ∘ η_A = η_B ∘ F(f)

Since we can't quantify over all morphisms, we test with
specific values and functions.")

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

(doc 'section 'common-transformations)

(doc nat-head 'type 'NatTransform)
(doc nat-head 'description "NatTransform from List to Maybe
η_A : [A] → Maybe A, returns Just (head) or Nothing")
(define nat-head
  (make-nat-transform
   'head
   functor-list
   functor-maybe
   (lambda (xs)
     (if (null? xs)
         nothing
         (just (car xs))))))

(doc nat-singleton 'type 'NatTransform)
(doc nat-singleton 'description "NatTransform from Maybe to List
η_A : Maybe A → [A], Nothing → [], Just x → [x]")
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

(doc nat-either-to-maybe 'type 'NatTransform)
(doc nat-either-to-maybe 'description "NatTransform from Either to Maybe.
Forgets the error, keeping only success")
(define nat-either-to-maybe
  (make-nat-transform
   'either->maybe
   functor-either
   functor-maybe
   (lambda (e)
     (if (right? e)
         (just (from-right e))
         nothing))))

(doc nat-concat 'type 'NatTransform)
(doc nat-concat 'description "NatTransform from (List ∘ List) to List.
The join/flatten operation: [[A]] → [A]
This is the multiplication of the List monad")
(define nat-concat
  (let ([list-list (make-named-functor 'list∘list
                     (lambda (f xss) (map (lambda (xs) (map f xs)) xss)))])
    (make-nat-transform
     'concat
     list-list
     functor-list
     (lambda (xss) (flatten xss)))))

(doc nat-pure-list 'type 'NatTransform)
(doc nat-pure-list 'description "NatTransform from Identity to List.
η_A : A → [A], the unit of the List monad")
(define nat-pure-list
  (make-nat-transform
   'pure-list
   functor-id
   functor-list
   list))

(doc nat-pure-maybe 'type 'NatTransform)
(doc nat-pure-maybe 'description "NatTransform from Identity to Maybe.
η_A : A → Maybe A, wraps value in Just")
(define nat-pure-maybe
  (make-nat-transform
   'pure-maybe
   functor-id
   functor-maybe
   just))

;;; ====
;;; Display
;;; ====

;;; nat-transform->string : NatTransform → String
(define (nat-transform->string η)
  (if (nat-transform? η)
      (format "~a : ~a ⟹ ~a"
              (nat-transform-name η)
              (if (functor? (nat-transform-source η))
                  (functor-name (nat-transform-source η) 'F)
                  "?")
              (if (functor? (nat-transform-target η))
                  (functor-name (nat-transform-target η) 'G)
                  "?"))
      "Not a natural transformation"))
