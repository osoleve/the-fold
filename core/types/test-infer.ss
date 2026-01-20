;;; Test harness for core/infer.ss — Type Inference

(load "core/blocks/block.ss")
(load "core/types/types.ss")
(load "core/types/kinds.ss")
(load "core/types/infer.ss")

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

(define (test-type name expected expr)
  (display "  ")
  (display name)
  (display ": ")
  (reset-fresh!)
  (let ([result (typeof expr)])
       (if (and (pair? result) (eq? (car result) 'error))
           (begin
            (display "✗ Error: ")
            (display result))
           (if (type=? expected result)
               (display "✓")
               (begin
                (display "✗
    expected: ")
                (display (type->string expected))
                (display "
    got: ")
                (display (type->string result)))))
       (newline)))

(define (test-error name expr)
  (display "  ")
  (display name)
  (display ": ")
  (reset-fresh!)
  (let ([result (typeof expr)])
       (if (and (pair? result) (eq? (car result) 'error))
           (display "✓")
           (begin
            (display "✗ expected error, got: ")
            (display result)))
       (newline)))

(define (test-section name)
  (newline)
  (display name)
  (newline))

;;; ====
;;; Substitution
;;; ====
(test-section "Substitution")
(test "apply-subst simple" 'Int (apply-subst '((a . Int)) 'a))
(test "apply-subst no match" 'b (apply-subst '((a . Int)) 'b))
(test "apply-subst in function" '(-> Int Int)
      (apply-subst '((a . Int)) '(-> a a)))
(test "apply-subst chain" 'Bool
      (apply-subst '((a . b) (b . Bool)) 'a))

;;; ====
;;; Unification
;;; ====
(test-section "Unification")
(let ([r (unify 'Int 'Int)])
     (test "same base type" 'ok (car r)))
(let ([r (unify 'Int 'Bool)])
     (test "different base types" 'error (car r)))
(let ([r (unify 'a 'Int)])
     (test "var with type" 'ok (car r))
     (test "var bound to Int" 'Int (apply-subst (cadr r) 'a)))
(let ([r (unify '(-> a a) '(-> Int Int))])
     (test "function unification" 'ok (car r))
     (test "unified type var" 'Int (apply-subst (cadr r) 'a)))
(let ([r (unify 'a '(List a))])
     (test "occurs check" 'error (car r)))

;;; ====
;;; Instantiation
;;; ====
(test-section "Instantiation")
(reset-fresh!)
(let ([inst (instantiate '(∀ (a) (-> a a)))])
     (test "instantiate structure" '-> (car inst))
     (test "instantiate same var" (cadr inst) (caddr inst)))

;;; ====
;;; Literal Inference
;;; ====
(test-section "Literal Inference")
(test-type "integer" 'Int 42)
(test-type "boolean true" 'Bool #t)
(test-type "boolean false" 'Bool #f)
(test-type "string" 'String "hello")
(test-type "quoted symbol" 'Symbol '(quote foo))

;;; ====
;;; Lambda Inference
;;; ====
(test-section "Lambda Inference")
;; Identity function — polymorphic
(reset-fresh!)
(let ([result (typeof '(fn (x) x))])
     (test "identity is polymorphic" '∀ (car result)))

;; Lambda with primitive use
(test-type "neg function" '(-> Int Int) '(fn (x) (prim 'neg x)))

;;; ====
;;; Application Inference
;;; ====
(test-section "Application Inference")
;; Apply identity to int
(reset-fresh!)
(let ([result (typeof '((fn (x) x) 42))])
     (test "apply identity to int" 'Int result))

;; Nested application
(reset-fresh!)
(let ([result (typeof '((fn (f) (fn (x) (f x))) (fn (y) y)))])
     ;; Should be polymorphic: ∀ a. a → a
     (test "nested app is polymorphic" '∀ (if (pair? result) (car result) 'not-pair)))

;;; ====
;;; Let Inference
;;; ====
(test-section "Let Inference")
(test-type "let simple" 'Int '(let ((x 42)) x))

;; Let with polymorphism
(reset-fresh!)
(let ([result (typeof '(let ((id (fn (x) x))) (id 42)))])
     (test "let-polymorphism" 'Int result))

;; Use same let-bound var at different types
(reset-fresh!)
(let ([result (typeof '(let ((id (fn (x) x)))
                        (let ((a (id 42)))
                             (id #t))))])
     (test "let-polymorphism different types" 'Bool result))

;;; ====
;;; Primitive Inference
;;; ====
(test-section "Primitive Inference")
(test-type "add" 'Int '(prim 'add 1 2))
(test-type "sub" 'Int '(prim 'sub 10 5))
(test-type "neg" 'Int '(prim 'neg 42))
(test-type "eq?" 'Bool '(prim 'eq? 1 1))
(test-type "lt?" 'Bool '(prim 'lt? 1 2))
(test-type "not" 'Bool '(prim 'not #t))

;; List primitives
(reset-fresh!)
(let ([result (typeof '(prim 'null? (quote ())))])
     (test "null? returns Bool" 'Bool result))

;;; ====
;;; Type Annotation
;;; ====
(test-section "Type Annotation")
(test-type "annotated int" 'Int '(: 42 Int))
(test-type "annotated id" '(-> Int Int) '(: (fn (x) x) (-> Int Int)))

;;; ====
;;; Type Errors
;;; ====
(test-section "Type Errors")
(test-error "unbound variable" 'undefined-var)
(test-error "applying non-function" '(42 1 2))
(test-error "wrong arity" '((fn (x) x) 1 2))

;;; ====
;;; Fix (Recursion)
;;; ====
(test-section "Fix")
;; Recursive identity (silly but tests fix)
(reset-fresh!)
(let ([result (typeof '(fix (f) (fn (x) x)))])
     ;; Should be polymorphic function
     (test "fix returns function" '∀ (if (pair? result) (car result) 'not-pair)))

;;; ====
;;; Generalization
;;; ====
(test-section "Generalization")
(let ([gen (generalize '() '(-> a a))])
     (test "generalize free vars" '∀ (car gen))
     (test "generalize binds a" '(a) (cadr gen)))
(let ([gen (generalize '((x . a)) '(-> a b))])
     ;; Only b should be generalized (a is in env)
     (test "generalize respects env" '∀ (car gen))
     (test "generalize only free" '(b) (cadr gen)))

;;; ====
;;; Complex Expressions
;;; ====
(test-section "Complex Expressions")

;; Compose-like
(reset-fresh!)
(let ([result (typeof '(fn (f) (fn (g) (fn (x) (f (g x))))))])
     (test "compose is polymorphic" '∀ (if (pair? result) (car result) 'not-pair)))

;; K combinator
(reset-fresh!)
(let ([result (typeof '(fn (x) (fn (y) x)))])
     (test "K is polymorphic" '∀ (if (pair? result) (car result) 'not-pair)))

;; S combinator (partial — full S needs application)
(reset-fresh!)
(let ([result (typeof '(fn (x) (fn (y) (fn (z) ((x z) (y z))))))])
     (test "S is polymorphic" '∀ (if (pair? result) (car result) 'not-pair)))

;;; ====
;;; Extended Primitive Inference
;;; ====
(test-section "Extended Primitives")

;; Bitwise
(test-type "bitand" 'Int '(prim 'bitand 7 3))
(test-type "bitor" 'Int '(prim 'bitor 4 2))
(test-type "bitnot" 'Int '(prim 'bitnot 5))
(test-type "shl" 'Int '(prim 'shl 1 4))

;; Boolean
(test-type "and" 'Bool '(prim 'and #t #f))
(test-type "or" 'Bool '(prim 'or #t #f))

;; String operations
(test-type "string-length" 'Int '(prim 'string-length "hello"))
(test-type "string-append" 'String '(prim 'string-append "a" "b"))
(test-type "string=?" 'Bool '(prim 'string=? "a" "b"))

;; Character operations
;; Character tests skipped - require literal chars
;; (test-type "char->integer" ...) - deferred

;; List operations
;; reverse xs returns a list, and the fn wraps it
(reset-fresh!)
(let ([result (typeof '(fn (xs) (prim 'reverse xs)))])
     (test "reverse is polymorphic" '∀ (if (pair? result) (car result) 'not-pair)))
;; append takes two lists and returns one
(reset-fresh!)
(let ([result (typeof '(fn (xs) (prim 'append xs xs)))])
     (test "append is polymorphic" '∀ (if (pair? result) (car result) 'not-pair)))
(test-type "list-ref" 'Int '(prim 'list-ref (quote (1 2 3)) 0))

;; Type predicates
(test-type "boolean?" 'Bool '(prim 'boolean? #t))
(test-type "char?" 'Bool '(prim 'char? (quote #\x)))

;;; ====
;;; Higher-Order FP Primitives
;;; ====
(test-section "FP Primitives")

;; map : (a → b) → [a] → [b]
(reset-fresh!)
(let ([result (typeof '(fn (f) (fn (xs) (prim 'map f xs))))])
     (test "map is polymorphic" '∀ (if (pair? result) (car result) 'not-pair)))

;; filter : (a → Bool) → [a] → [a]
(reset-fresh!)
(let ([result (typeof '(fn (p) (fn (xs) (prim 'filter p xs))))])
     (test "filter is polymorphic" '∀ (if (pair? result) (car result) 'not-pair)))

;; foldr : (a → b → b) → b → [a] → b
(reset-fresh!)
(let ([result (typeof '(fn (f) (fn (z) (fn (xs) (prim 'foldr f z xs)))))])
     (test "foldr is polymorphic" '∀ (if (pair? result) (car result) 'not-pair)))

;; foldl : (b → a → b) → b → [a] → b
(reset-fresh!)
(let ([result (typeof '(fn (f) (fn (z) (fn (xs) (prim 'foldl f z xs)))))])
     (test "foldl is polymorphic" '∀ (if (pair? result) (car result) 'not-pair)))

;; andmap/ormap : (a → Bool) → [a] → Bool
(test-type "andmap" 'Bool '(prim 'andmap (fn (x) #t) (quote (1 2 3))))
(test-type "ormap" 'Bool '(prim 'ormap (fn (x) #f) (quote (1 2 3))))

;; take/drop : Int → [a] → [a]
(reset-fresh!)
(let ([result (typeof '(fn (xs) (prim 'take 5 xs)))])
     (test "take is polymorphic" '∀ (if (pair? result) (car result) 'not-pair)))

;; zip : [a] → [b] → [(a, b)]
(reset-fresh!)
(let ([result (typeof '(fn (xs) (fn (ys) (prim 'zip xs ys))))])
     (test "zip is polymorphic" '∀ (if (pair? result) (car result) 'not-pair)))

;;; ====
;;; Case Expression Inference
;;; ====
(test-section "Case Expressions")

;; Simple case with one clause
(reset-fresh!)
(let ([result (typeof '(fn (blk) (case blk ((Foo x) 42))))])
     (test "case returns Int" '∀ (if (pair? result) (car result) 'not-pair)))

;; Case with multiple clauses (same type)
(reset-fresh!)
(let ([result (typeof '(fn (blk)
                        (case blk
                              ((True) 1)
                              ((False) 0))))])
     (test "multi-clause case" '∀ (if (pair? result) (car result) 'not-pair)))

;; Case using bound variable
(reset-fresh!)
(let ([result (typeof '(fn (blk)
                        (case blk
                              ((Wrap ref) ref))))])
     (test "case binds ref" '∀ (if (pair? result) (car result) 'not-pair)))

;;; ====
;;; Declared Types (from doc annotations)
;;; ====
(test-section "Declared Types")

;; Clear any existing declared types
(clear-declared-types!)

;; Register a declared type manually
(register-declared-type! 'my-add '(-> Int Int Int))

;; Lookup should find it
(test "lookup-declared-type finds registered" '(-> Int Int Int) (lookup-declared-type 'my-add))
(test "lookup-declared-type returns #f for unknown" #f (lookup-declared-type 'unknown-fn))

;; Infer should use declared type for unbound variable
(reset-fresh!)
(let ([result (infer 'my-add empty-tenv)])
  (test "infer uses declared type" 'ok (car result))
  (test "infer declared type correct" '(-> Int Int Int) (cadr result)))

;; Register via register-doc-types!
(register-doc-types! '((my-sub . (-> Int Int Int))
                       (my-mul . (-> Int Int Int))))
(test "register-doc-types! works" '(-> Int Int Int) (lookup-declared-type 'my-sub))

;; Build tenv from declared types
(let ([env (build-tenv-from-declared-types '(my-add my-sub my-unknown))])
  (test "build-tenv includes my-add" '(-> Int Int Int) (tenv-lookup env 'my-add))
  (test "build-tenv includes my-sub" '(-> Int Int Int) (tenv-lookup env 'my-sub))
  (test "build-tenv skips unknown" #f (tenv-lookup env 'my-unknown)))

;; Clean up
(clear-declared-types!)

(newline)
(display "✓ All type inference tests complete.
")
