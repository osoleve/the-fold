;;; Test harness for hole constraint tracking in type inference
;;;
;;; Tests the gradual typing support added to the unifier:
;;;   - Named holes (? x) get consistent constraints across uses
;;;   - Anonymous holes (?) get fresh hole variables
;;;   - Hole constraints are recorded in the substitution

(load "core/blocks/block.ss")
(load "core/types/types.ss")
(load "core/types/kinds.ss")
(load "core/types/infer.ss")

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
       (display "✗
    expected: ")
       (display expected)
       (display "
    got: ")
       (display actual)))
  (newline))

(define (test-true name actual)
  (test name #t actual))

(define (test-false name actual)
  (test name #f actual))

(define (section name)
  (newline)
  (display name)
  (newline))

;;; ====
;;; Hole Variable Basics
;;; ====

(section "Hole Variable Predicates")

(test-true "?x is a hole var" (hole-var? '?x))
(test-true "?1 is a hole var" (hole-var? '?1))
(test-true "?foo is a hole var" (hole-var? '?foo))
(test-false "? is not a hole var" (hole-var? '?))
(test-false "x is not a hole var" (hole-var? 'x))
(test-false "Int is not a hole var" (hole-var? 'Int))

;;; ====
;;; Hole Conversion
;;; ====

(section "Hole Conversion")

(reset-fresh!)
(test "named hole (? x) → ?x" '?x (hole->var '(? x)))
(test "named hole (? foo) → ?foo" '?foo (hole->var '(? foo)))

(reset-fresh!)
(test "anonymous hole → ?1" '?1 (hole->var '?))
(test "second anonymous → ?2" '?2 (hole->var '?))
(test "third anonymous → ?3" '?3 (hole->var '?))

;;; ====
;;; Unification with Holes
;;; ====

(section "Unification with Holes")

;; Anonymous hole unifies with Int, records constraint
(reset-fresh!)
(let ([result (unify '? 'Int)])
  (test "anon hole unifies with Int" 'ok (car result))
  (let ([constraints (hole-constraints (cadr result))])
    (test "records ?1 → Int" '((?1 . Int)) constraints)))

;; Named hole unifies with Int
(reset-fresh!)
(let ([result (unify '(? x) 'Int)])
  (test "named hole unifies with Int" 'ok (car result))
  (let ([constraints (hole-constraints (cadr result))])
    (test "records ?x → Int" '((?x . Int)) constraints)))

;; Two anonymous holes get different variables
(reset-fresh!)
(let ([result (unify '(-> ? ?) '(-> Int Bool))])
  (test "function with holes unifies" 'ok (car result))
  (let ([constraints (hole-constraints (cadr result))])
    (test-true "two constraints recorded" (= (length constraints) 2))
    ;; Should have ?1 → Int and ?2 → Bool (or reverse order)
    (test-true "contains Int binding"
               (ormap (lambda (c) (eq? (cdr c) 'Int)) constraints))
    (test-true "contains Bool binding"
               (ormap (lambda (c) (eq? (cdr c) 'Bool)) constraints))))

;;; ====
;;; Named Hole Consistency
;;; ====

(section "Named Hole Consistency")

;; Same named hole used twice must be consistent
(reset-fresh!)
(let ([result (unify '(-> (? t) (? t)) '(-> Int Int))])
  (test "same named hole twice (consistent)" 'ok (car result)))

;; Same named hole with inconsistent types should fail
(reset-fresh!)
(let ([result (unify '(-> (? t) (? t)) '(-> Int Bool))])
  (test "same named hole twice (inconsistent)" 'error (car result)))

;; Different named holes can have different types
(reset-fresh!)
(let ([result (unify '(-> (? a) (? b)) '(-> Int Bool))])
  (test "different named holes" 'ok (car result))
  (let ([constraints (hole-constraints (cadr result))])
    (test-true "?a bound" (if (assq '?a constraints) #t #f))
    (test-true "?b bound" (if (assq '?b constraints) #t #f))))

;;; ====
;;; Apply-Subst with Holes
;;; ====

(section "Apply-Subst with Holes")

;; Substitution replaces named holes
(let ([s '((?x . Int))])
  (test "apply-subst replaces named hole"
        'Int
        (apply-subst s '(? x))))

;; Substitution replaces hole variables
(let ([s '((?1 . Bool))])
  (test "apply-subst replaces hole var"
        'Bool
        (apply-subst s '?1)))

;; Nested substitution in function type
(let ([s '((?x . Int) (?y . Bool))])
  (test "apply-subst in function"
        '(-> Int Bool)
        (apply-subst s '(-> (? x) (? y)))))

;; Chained substitution
(let ([s '((?x . ?y) (?y . Int))])
  (test "apply-subst chases chains"
        'Int
        (apply-subst s '(? x))))

;;; ====
;;; Type Inference with Holes
;;; ====

(section "Type Inference with Holes")

;; Annotated expression with hole infers from usage
(reset-fresh!)
(let ([result (infer '(prim 'add (: x (? t)) 1) (tenv-extend empty-tenv 'x 'Int))])
  (test "add with hole annotation" 'ok (car result))
  (let ([constraints (hole-constraints (caddr result))])
    ;; The hole should have been unified with Int
    (test-true "hole constraint recorded"
               (ormap (lambda (c) (eq? (cdr c) 'Int)) constraints))))

;;; ====
;;; Hole Constraint Extraction
;;; ====

(section "Hole Constraint Extraction")

(let ([s '((?x . Int) (τ1 . Bool) (?y . String) (a . Unit))])
  (let ([hc (hole-constraints s)]
        [tc (type-var-constraints s)])
    (test "hole-constraints extracts ?x and ?y"
          2
          (length hc))
    (test "type-var-constraints extracts τ1 and a"
          2
          (length tc))))

;;; ====
;;; Occurs Check with Holes
;;; ====

(section "Occurs Check with Holes")

;; Occurs check should detect cycles through hole vars
(reset-fresh!)
(let ([result (unify '?x '(List ?x))])
  (test "occurs check detects cycle" 'error (car result)))

;; Named hole in occurs check
(test-true "occurs? finds named hole"
           (occurs? '?x '(-> (? x) Int)))

(test-false "occurs? misses different named hole"
            (occurs? '?x '(-> (? y) Int)))

;;; ====
;;; Summary
;;; ====

(newline)
(display "Hole constraint tests complete.")
(newline)
