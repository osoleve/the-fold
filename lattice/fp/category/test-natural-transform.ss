;;; lattice/fp/category/test-natural-transform.ss — Natural Transformation Tests
;;;
;;; Tests for natural transformations including composition, whiskering,
;;; naturality verification, and common transformations.
;;;
;;; Run from project root: scheme --script lattice/fp/category/test-natural-transform.ss

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
;;; Summary
;;; ====

(newline)
(display "Natural transformation tests complete.")
(newline)
