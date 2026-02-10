;;; lattice/fp/category/test-natural-transform.ss — Natural Transformation Tests
;;;
;;; Tests for natural transformations including composition, whiskering,
;;; naturality verification, and common transformations.
;;;
;;; Run from project root: scheme --script lattice/fp/category/test-natural-transform.ss

(load "core/lang/module.ss")
(load "lattice/fp/category/natural-transform.ss")

;;; ====
;;; Test Helpers
;;; ====

(define (test name expected actual)
  (display "  ")
  (display name)
  (display ": ")
  (if (equal? expected actual)
      (display "✓")
      (begin
       (display "✗\n    expected: ")
       (display expected)
       (display "\n    got: ")
       (display actual)))
  (newline))

(define (test-true name actual)
  (test name #t (if actual #t #f)))

(define (test-false name actual)
  (test name #f actual))

(define (section name)
  (newline)
  (display name)
  (newline))

;;; ====
;;; Test: Natural Transformation Basics
;;; ====

(section "Natural Transformation Basics")

(test-true "make-nat-transform creates nat-transform"
           (nat-transform? (make-nat-transform 'test functor-list functor-maybe id)))

(test "nat-transform-name extracts name"
      'test
      (nat-transform-name (make-nat-transform 'test functor-list functor-maybe id)))

(test-true "nat-transform-source extracts functor"
           (functor? (nat-transform-source nat-head)))

(test-true "nat-transform-target extracts functor"
           (functor? (nat-transform-target nat-head)))

(test-true "nat-transform-component extracts function"
           (procedure? (nat-transform-component nat-head)))

;;; ====
;;; Test: nat-head (List ⟹ Maybe)
;;; ====

(section "nat-head: List ⟹ Maybe")

(test "nat-head on empty list" nothing (nat-apply nat-head '()))
(test "nat-head on singleton" (just 1) (nat-apply nat-head '(1)))
(test "nat-head on longer list" (just 'a) (nat-apply nat-head '(a b c)))

;;; ====
;;; Test: nat-singleton (Maybe ⟹ List)
;;; ====

(section "nat-singleton: Maybe ⟹ List")

(test "nat-singleton on nothing" '() (nat-apply nat-singleton nothing))
(test "nat-singleton on just" '(42) (nat-apply nat-singleton (just 42)))

;;; ====
;;; Test: nat-either-to-maybe
;;; ====

(section "nat-either-to-maybe: Either ⟹ Maybe")

(test "either-to-maybe on left" nothing (nat-apply nat-either-to-maybe (left "error")))
(test "either-to-maybe on right" (just 42) (nat-apply nat-either-to-maybe (right 42)))

;;; ====
;;; Test: nat-maybe-to-either
;;; ====

(section "nat-maybe-to-either: Maybe ⟹ Either")

(let ([η (nat-maybe-to-either 'default-error)])
  (test "maybe-to-either on nothing" (left 'default-error) (nat-apply η nothing))
  (test "maybe-to-either on just" (right 42) (nat-apply η (just 42))))

;;; ====
;;; Test: Identity Natural Transformation
;;; ====

(section "Identity Natural Transformation")

(let ([id-list (nat-id functor-list)])
  (test-true "nat-id creates nat-transform" (nat-transform? id-list))
  (test "nat-id is identity on lists" '(1 2 3) (nat-apply id-list '(1 2 3)))
  (test "nat-id is identity on empty" '() (nat-apply id-list '())))

;;; ====
;;; Test: Vertical Composition
;;; ====

(section "Vertical Composition")

;; Compose nat-head : List ⟹ Maybe with nat-maybe-to-either
(let* ([η (nat-maybe-to-either 'empty)]
       [composed (nat-compose η nat-head)])
  (test-true "nat-compose creates nat-transform" (nat-transform? composed))
  (test "composed on empty list" (left 'empty) (nat-apply composed '()))
  (test "composed on non-empty" (right 'a) (nat-apply composed '(a b c))))

;; Compose nat-singleton : Maybe ⟹ List with nat-head : List ⟹ Maybe
;; This should be approximately identity (Maybe ⟹ List ⟹ Maybe)
(let ([round-trip (nat-compose nat-head nat-singleton)])
  (test "round-trip on nothing" nothing (nat-apply round-trip nothing))
  (test "round-trip on just" (just 42) (nat-apply round-trip (just 42))))

;;; ====
;;; Test: Naturality Condition
;;; ====

(section "Naturality Condition")

;; Test nat-head satisfies naturality: Maybe(f) ∘ head = head ∘ List(f)
;; For f = add1 and x = '(1 2 3)
(test-true "nat-head naturality with add1"
           (check-naturality nat-head add1 '(1 2 3)))

(test-true "nat-head naturality with symbol->string"
           (check-naturality nat-head symbol->string '(a b c)))

(test-true "nat-head naturality on empty"
           (check-naturality nat-head add1 '()))

;; Test nat-singleton satisfies naturality
(test-true "nat-singleton naturality on just"
           (check-naturality nat-singleton add1 (just 5)))

(test-true "nat-singleton naturality on nothing"
           (check-naturality nat-singleton add1 nothing))

;; Test nat-either-to-maybe satisfies naturality
(test-true "nat-either-to-maybe naturality on right"
           (check-naturality nat-either-to-maybe add1 (right 5)))

(test-true "nat-either-to-maybe naturality on left"
           (check-naturality nat-either-to-maybe add1 (left "error")))

;; Verify with multiple test cases
(test-true "verify-naturality with multiple cases"
           (verify-naturality nat-head
                              (list (cons add1 '(1 2 3))
                                    (cons (lambda (x) (* x 2)) '(5 10))
                                    (cons symbol->string '(a b)))))

;;; ====
;;; Test: Whiskering
;;; ====

(section "Whiskering Operations")

;; Right whiskering: (nat-head ◁ functor-maybe)
;; Precompose with Maybe: List∘Maybe ⟹ Maybe∘Maybe
(let ([whiskered (nat-whisker-right nat-head functor-maybe)])
  (test-true "right whiskering creates nat-transform"
             (nat-transform? whiskered)))

;; Left whiskering: (functor-list ▷ nat-head)
;; Postcompose with List: List∘List ⟹ List∘Maybe
(let ([whiskered (nat-whisker-left functor-list nat-head)])
  (test-true "left whiskering creates nat-transform"
             (nat-transform? whiskered)))

;; Substantive whiskering tests using nat-head
;; nat-head : List ⟹ Maybe takes a list and returns Just (head) or Nothing

;; Right whiskering: (nat-head ◁ Maybe) : List∘Maybe ⟹ Maybe∘Maybe
;; Input is List(Maybe(A)), output is Maybe(Maybe(A))
;; The component η_{Maybe(A)} = head : List(Maybe(A)) → Maybe(Maybe(A))
(let ([right-whiskered (nat-whisker-right nat-head functor-maybe)])
  (test "right whisker (η ◁ H) applies η to H-wrapped values"
        (just (just 1))  ; head of [(Just 1), (Just 2), Nothing]
        (nat-apply right-whiskered (list (just 1) (just 2) nothing)))

  (test "right whisker on empty list"
        nothing
        (nat-apply right-whiskered '())))

;; Left whiskering: (List ▷ nat-head) : List∘List ⟹ List∘Maybe
;; Input is List(List(A)), output is List(Maybe(A))
;; Component (H ▷ η)_A = H(η_A) = List(head) - map head over the outer list
(let ([left-whiskered (nat-whisker-left functor-list nat-head)])
  (test "left whisker (H ▷ η) maps η over H-container"
        (list (just 'a) (just 'd) nothing)  ; head of each inner list
        (nat-apply left-whiskered '((a b c) (d e) ())))

  (test "left whisker on empty outer list"
        '()
        (nat-apply left-whiskered '())))

;; Test that the composed functor's fmap works correctly
;; This was the bug: the composed functor was doing F(f)(H(f)(x)) instead of F(H(f))(x)
(section "Whiskering Functor Composition")

(let* ([η (make-nat-transform
           'id-list
           functor-list
           functor-list
           (lambda (xs) xs))]  ; identity on lists
       [whiskered (nat-whisker-right η functor-maybe)]
       [FH (nat-transform-source whiskered)]
       [FH-fmap (functor-fmap FH)])

  ;; For List∘Maybe, fmap f should apply f to the innermost values
  ;; Input: [(Just 1), (Just 2), Nothing]
  ;; f = add1
  ;; Output: [(Just 2), (Just 3), Nothing]
  (test "composed functor (F∘H) fmap works correctly"
        (list (just 2) (just 3) nothing)
        (FH-fmap add1 (list (just 1) (just 2) nothing))))

;;; ====
;;; Test: Natural Isomorphisms
;;; ====

(section "Natural Isomorphisms")

;; Create an isomorphism between Maybe A and Either () A
(let* ([to-either (lambda (m)
                    (if (just? m)
                        (right (from-just m))
                        (left '())))]
       [from-either (lambda (e)
                      (if (right? e)
                          (just (from-right e))
                          nothing))]
       [functor-either-unit (make-functor either-fmap)]
       [iso (make-nat-iso 'maybe≅either
                          functor-maybe
                          functor-either-unit
                          to-either
                          from-either)])
  (test-true "make-nat-iso creates nat-iso" (nat-iso? iso))
  (test-true "nat-iso-forward extracts forward transform"
             (nat-transform? (nat-iso-forward iso)))
  (test-true "nat-iso-inverse extracts inverse transform"
             (nat-transform? (nat-iso-inverse iso)))

  ;; Test round-trip
  (let* ([fwd (nat-iso-forward iso)]
         [inv (nat-iso-inverse iso)]
         [round-trip (nat-compose inv fwd)])
    (test "iso round-trip on just" (just 42)
          (nat-apply round-trip (just 42)))
    (test "iso round-trip on nothing" nothing
          (nat-apply round-trip nothing))))

;;; ====
;;; Test: nat-concat (List∘List ⟹ List)
;;; ====

(section "nat-concat: List∘List ⟹ List")

(test "nat-concat flattens" '(1 2 3 4 5)
      (nat-apply nat-concat '((1 2) (3) (4 5))))

(test "nat-concat on empty outer" '()
      (nat-apply nat-concat '()))

(test "nat-concat with empty inners" '(1 2)
      (nat-apply nat-concat '((1) () (2) ())))

;;; ====
;;; Test: nat-pure transformations
;;; ====

(section "nat-pure Transformations (Monad Units)")

(test "nat-pure-list wraps in list" '(42)
      (nat-apply nat-pure-list 42))

(test "nat-pure-maybe wraps in just" (just 42)
      (nat-apply nat-pure-maybe 42))

;;; ====
;;; Test: Display
;;; ====

(section "Display")

(test-true "nat-transform->string produces string"
           (string? (nat-transform->string nat-head)))

;;; ====
;;; Test: Fail-Fast on Invalid Input (fold-zxnw fix)
;;; ====

(section "Fail-Fast Behavior (fold-zxnw)")

;; nat-transform-component should error on invalid input, not return id
(let ([error-caught? #f])
  (guard (e [else (set! error-caught? #t)])
    (nat-transform-component 'not-a-nat-transform))
  (test-true "nat-transform-component errors on invalid input"
             error-caught?))

;; Verify the error includes useful context
(let ([error-message #f])
  (guard (e [else (set! error-message (condition-message e))])
    (nat-transform-component 42))
  (test-true "error message mentions expected type"
             (and error-message
                  (string? error-message)
                  (string-contains? error-message "nat-transform"))))

;; Verify that nat-apply also fails fast (since it uses nat-transform-component)
(let ([error-caught? #f])
  (guard (e [else (set! error-caught? #t)])
    (nat-apply 'oops-just-a-symbol 'some-value))
  (test-true "nat-apply errors on invalid nat-transform"
             error-caught?))

;;; ====
;;; Test: Algebraic Laws (fold-zxnv)
;;; ====

(section "Algebraic Laws: Vertical Composition Associativity")

;; Associativity: (α ∘ β) ∘ γ = α ∘ (β ∘ γ)
;; Using: γ = nat-head : List → Maybe
;;        β = nat-singleton : Maybe → List
;;        α = nat-head : List → Maybe
;; Both sides give List → Maybe
(let* ([γ nat-head]           ; List → Maybe
       [β nat-singleton]       ; Maybe → List
       [α nat-head]           ; List → Maybe
       ;; Left association: (α ∘ β) ∘ γ
       [α∘β (nat-compose α β)]               ; Maybe → Maybe
       [left-assoc (nat-compose α∘β γ)]      ; List → Maybe
       ;; Right association: α ∘ (β ∘ γ)
       [β∘γ (nat-compose β γ)]               ; List → List
       [right-assoc (nat-compose α β∘γ)])    ; List → Maybe

  (test-true "associativity creates nat-transforms"
             (and (nat-transform? left-assoc)
                  (nat-transform? right-assoc)))

  ;; Test on various inputs
  (test "associativity: (α ∘ β) ∘ γ = α ∘ (β ∘ γ) on '(1 2 3)"
        (nat-apply left-assoc '(1 2 3))
        (nat-apply right-assoc '(1 2 3)))

  (test "associativity: (α ∘ β) ∘ γ = α ∘ (β ∘ γ) on '()"
        (nat-apply left-assoc '())
        (nat-apply right-assoc '()))

  (test "associativity: (α ∘ β) ∘ γ = α ∘ (β ∘ γ) on '(a)"
        (nat-apply left-assoc '(a))
        (nat-apply right-assoc '(a))))

(section "Algebraic Laws: Identity Laws")

;; Left identity: nat-compose (nat-id G) η = η
;; For η = nat-head : List → Maybe, G = Maybe
(let* ([η nat-head]
       [id-maybe (nat-id functor-maybe)]
       [left-id-composed (nat-compose id-maybe η)])

  (test "left identity: nat-compose (nat-id G) η = η on '(1 2 3)"
        (nat-apply η '(1 2 3))
        (nat-apply left-id-composed '(1 2 3)))

  (test "left identity: nat-compose (nat-id G) η = η on '()"
        (nat-apply η '())
        (nat-apply left-id-composed '()))

  (test "left identity: nat-compose (nat-id G) η = η on '(x)"
        (nat-apply η '(x))
        (nat-apply left-id-composed '(x))))

;; Right identity: nat-compose η (nat-id F) = η
;; For η = nat-head : List → Maybe, F = List
(let* ([η nat-head]
       [id-list (nat-id functor-list)]
       [right-id-composed (nat-compose η id-list)])

  (test "right identity: nat-compose η (nat-id F) = η on '(1 2 3)"
        (nat-apply η '(1 2 3))
        (nat-apply right-id-composed '(1 2 3)))

  (test "right identity: nat-compose η (nat-id F) = η on '()"
        (nat-apply η '())
        (nat-apply right-id-composed '()))

  (test "right identity: nat-compose η (nat-id F) = η on '(a b)"
        (nat-apply η '(a b))
        (nat-apply right-id-composed '(a b))))

;; Test with different transformation: nat-singleton : Maybe → List
(let* ([η nat-singleton]
       [id-list (nat-id functor-list)]
       [id-maybe (nat-id functor-maybe)]
       [left-composed (nat-compose id-list η)]
       [right-composed (nat-compose η id-maybe)])

  (test "left identity for nat-singleton on (just 42)"
        (nat-apply η (just 42))
        (nat-apply left-composed (just 42)))

  (test "right identity for nat-singleton on nothing"
        (nat-apply η nothing)
        (nat-apply right-composed nothing)))

(section "Algebraic Laws: Interchange Law")

;; Interchange law: (α ∘ β) * (γ ∘ δ) = (α * γ) ∘ (β * δ)
;; where * is horizontal composition (nat-horizontal)
;;
;; Setup: We need transformations that can be horizontally composed.
;; Using nat-id transformations to test the structure:
;;   α, β : List → List (identity on List)
;;   γ, δ : Maybe → Maybe (identity on Maybe)
;;
;; Then (α ∘ β) : List → List, (γ ∘ δ) : Maybe → Maybe
;; (α ∘ β) * (γ ∘ δ) : List∘Maybe → List∘Maybe
;; (α * γ) ∘ (β * δ) : List∘Maybe → List∘Maybe
(let* ([α (nat-id functor-list)]     ; List → List
       [β (nat-id functor-list)]     ; List → List
       [γ (nat-id functor-maybe)]    ; Maybe → Maybe
       [δ (nat-id functor-maybe)]    ; Maybe → Maybe
       ;; Left side: (α ∘ β) * (γ ∘ δ)
       [α∘β (nat-compose α β)]
       [γ∘δ (nat-compose γ δ)]
       [left-side (nat-horizontal α∘β γ∘δ)]
       ;; Right side: (α * γ) ∘ (β * δ)
       [α*γ (nat-horizontal α γ)]
       [β*δ (nat-horizontal β δ)]
       [right-side (nat-compose α*γ β*δ)]
       ;; Test value: List(Maybe(A)) = [(Just 1), Nothing, (Just 2)]
       [test-val (list (just 1) nothing (just 2))])

  (test-true "interchange creates nat-transforms"
             (and (nat-transform? left-side)
                  (nat-transform? right-side)))

  (test "interchange law: (α ∘ β) * (γ ∘ δ) = (α * γ) ∘ (β * δ)"
        (nat-apply left-side test-val)
        (nat-apply right-side test-val))

  ;; Test on empty list of maybes
  (test "interchange law on '()"
        (nat-apply left-side '())
        (nat-apply right-side '()))

  ;; Test on list with all nothings
  (test "interchange law on '(nothing nothing)"
        (nat-apply left-side (list nothing nothing))
        (nat-apply right-side (list nothing nothing))))

;;; Note: Input validation for nat-compose is tested in boundary/fp/test-category.ss
;;; The lattice code is pure and assumes valid input; validation lives at the boundary layer.

;;; ====
;;; Summary
;;; ====

(newline)
(display "Natural transformation tests complete.")
(newline)
