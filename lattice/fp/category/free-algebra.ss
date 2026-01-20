;;; lattice/fp/category/free-algebra.ss — Free Algebras and Generalized Free ⊣ Forgetful
;;;
;;; Generalizes the Free ⊣ Forgetful adjunction pattern (seen in adj-free-list)
;;; to arbitrary algebraic signatures: Monoid, Group, Magma, Ring, etc.
;;;
;;; Key abstraction: A Signature describes an algebraic structure
;;;   - operations: list of (op-name . arity)
;;;   - laws: list of rewrite rules for normalization
;;;
;;; An Algebra bundles:
;;;   - signature: the algebraic theory
;;;   - carrier: the underlying set (represented as type tag)
;;;   - ops: alist of (op-name . implementation)
;;;
;;; The Free ⊣ Forgetful adjunction for signature Σ:
;;;   - Free(X) = term algebra over generators from X
;;;   - Forgetful(A) = underlying carrier set of algebra A
;;;   - Unit η: x ↦ (gen x) — embed as generator
;;;   - Counit ε: term ↦ evaluate term in algebra (extracting ops from algebra)
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - adjunction.ss (for adjunction infrastructure)
;;;   - rewrite/rule.ss (for rewrite rules)
;;;   - rewrite/engine.ss (for pattern matching and normalization)

(load "lattice/fp/category/adjunction.ss")
(load "lattice/fp/rewrite/rule.ss")
(load "lattice/fp/rewrite/engine.ss")

;;; ====
;;; Signature Definition
;;; ====
;;;
;;; A signature describes an algebraic theory:
;;;   - name: identifying symbol
;;;   - operations: ((op-name . arity) ...)  e.g., ((e . 0) (* . 2))
;;;   - laws: list of rewrite rules for normalization

;;; make-signature : Symbol × OpList × RuleList → Signature
;;; Create a signature for an algebraic theory.
;;; ops: list of (op-name . arity) pairs
;;; laws: list of rewrite rules for term normalization
(define (make-signature name ops laws)
  (list 'signature name ops laws))

;;; signature? : Any → Boolean
(define (signature? x)
  (and (pair? x)
       (eq? (car x) 'signature)
       (= (length x) 4)))

;;; signature-name : Signature → Symbol
(define (signature-name sig)
  (if (signature? sig) (cadr sig) 'unknown))

;;; signature-operations : Signature → OpList
(define (signature-operations sig)
  (if (signature? sig) (caddr sig) '()))

;;; signature-laws : Signature → RuleList
(define (signature-laws sig)
  (if (signature? sig) (cadddr sig) '()))

;;; signature-op-arity : Signature × Symbol → Nat | #f
;;; Get the arity of an operation in the signature.
(define (signature-op-arity sig op-name)
  (let ([entry (assq op-name (signature-operations sig))])
    (and entry (cdr entry))))

;;; signature-has-op? : Signature × Symbol → Boolean
(define (signature-has-op? sig op-name)
  (and (assq op-name (signature-operations sig)) #t))

;;; ====
;;; Algebra Definition
;;; ====
;;;
;;; An Algebra bundles a signature with a concrete implementation.
;;; The ops alist maps operation names to procedures.

;;; make-algebra : Signature × Symbol × OpImplList → Algebra
;;; Create an algebra over a signature.
;;; carrier: a symbol identifying the carrier type
;;; ops: alist of (op-name . procedure)
(define (make-algebra sig carrier ops)
  (list 'algebra sig carrier ops))

;;; algebra? : Any → Boolean
(define (algebra? x)
  (and (pair? x)
       (eq? (car x) 'algebra)
       (= (length x) 4)))

;;; algebra-signature : Algebra → Signature
(define (algebra-signature alg)
  (if (algebra? alg) (cadr alg) #f))

;;; algebra-carrier : Algebra → Symbol
(define (algebra-carrier alg)
  (if (algebra? alg) (caddr alg) 'unknown))

;;; algebra-ops : Algebra → OpImplList
(define (algebra-ops alg)
  (if (algebra? alg) (cadddr alg) '()))

;;; algebra-op : Algebra × Symbol → Procedure | #f
;;; Get the implementation of an operation.
(define (algebra-op alg op-name)
  (let ([entry (assq op-name (algebra-ops alg))])
    (and entry (cdr entry))))

;;; ====
;;; Term Representation
;;; ====
;;;
;;; Terms in the free algebra are built from:
;;;   - Generators: (gen x) — elements from the carrier set
;;;   - Operations: (op-name term ...) — operation applied to subterms
;;;   - Constants: just the op-name for 0-arity operations

;;; make-gen : Any → Generator
;;; Create a generator (embedding of a carrier element into the free algebra).
(define (make-gen x)
  (list 'gen x))

;;; gen? : Any → Boolean
(define (gen? t)
  (and (pair? t)
       (eq? (car t) 'gen)
       (= (length t) 2)))

;;; gen-value : Generator → Any
(define (gen-value g)
  (if (gen? g) (cadr g) g))

;;; term-op? : Signature × Any → Boolean
;;; Check if a term is an operation application.
;;; Either a symbol (0-arity) or a list starting with an op-name.
(define (term-op? sig t)
  (cond
    ;; Symbol: 0-arity constant
    [(symbol? t)
     (let ([arity (signature-op-arity sig t)])
       (and arity (= arity 0)))]
    ;; List: n-arity operation application
    [(and (pair? t) (symbol? (car t)))
     (signature-has-op? sig (car t))]
    [else #f]))

;;; term? : Signature × Any → Boolean
;;; Check if something is a valid term in the free algebra.
(define (term? sig t)
  (or (gen? t)
      (term-op? sig t)))

;;; ====
;;; Term Operations
;;; ====

;;; free-fmap : Signature × (a → b) × Term → Term
;;; Apply a function to all generators in a term (functorial action).
(define (free-fmap sig f term)
  (cond
    ;; Generator: apply f to the value
    [(gen? term)
     (make-gen (f (gen-value term)))]
    ;; 0-arity constant (symbol): unchanged
    [(and (symbol? term) (term-op? sig term))
     term]
    ;; n-arity operation: recursively map over arguments
    [(and (pair? term) (term-op? sig term))
     (cons (car term)
           (map (lambda (arg) (free-fmap sig f arg)) (cdr term)))]
    ;; Fallback: unchanged (shouldn't happen with valid terms)
    [else term]))

;;; normalize-term : Signature × Term → Term
;;; Normalize a term using the signature's laws.
;;; Applies rewrite rules exhaustively in bottomup fashion.
(define (normalize-term sig term)
  (let ([laws (signature-laws sig)])
    (if (null? laws)
        term  ; No laws: term is already normalized
        (let ([strategy (innermost (rules->strategy laws))])
          (strategy term)))))

;;; eval-term : Algebra × Term → Value
;;; Evaluate a term in an algebra.
;;; Generators evaluate to their embedded values.
;;; Operations look up implementations in the algebra.
(define (eval-term alg term)
  (let ([sig (algebra-signature alg)]
        [ops (algebra-ops alg)])
    (cond
      ;; Generator: extract the value
      [(gen? term)
       (gen-value term)]
      ;; 0-arity constant: call the implementation with no args
      [(and (symbol? term) (term-op? sig term))
       (let ([op-fn (algebra-op alg term)])
         (if op-fn
             (op-fn)  ; 0-arity: call with no arguments
             (error 'eval-term (format "Unknown operation: ~a" term))))]
      ;; n-arity operation: evaluate args then apply
      [(and (pair? term) (term-op? sig term))
       (let ([op-name (car term)]
             [args (cdr term)])
         (let ([op-fn (algebra-op alg op-name)])
           (if op-fn
               (apply op-fn (map (lambda (arg) (eval-term alg arg)) args))
               (error 'eval-term (format "Unknown operation: ~a" op-name)))))]
      ;; Literal value: pass through (for generators that aren't wrapped)
      [else term])))

;;; ====
;;; Functors for Free ⊣ Forgetful
;;; ====

;;; make-free-functor : Signature → Functor
;;; The Free functor: Set → Alg(Σ)
;;; F(X) = term algebra over X (as generators)
;;; F(f) = map f over generators in terms
(define (make-free-functor sig)
  (make-named-functor
   (string->symbol (format "Free-~a" (signature-name sig)))
   (lambda (f term)
     (free-fmap sig f term))))

;;; make-forgetful-functor : Signature → Functor
;;; The Forgetful functor: Alg(Σ) → Set
;;; G(A) = carrier of A
;;; G(h) = underlying function of algebra homomorphism
;;; In our setting, we represent this as identity on values since
;;; algebras are first-class and we can always extract carriers.
(define (make-forgetful-functor sig)
  (make-named-functor
   (string->symbol (format "Forget-~a" (signature-name sig)))
   (lambda (f x) (f x))))  ; Identity-like: just apply f

;;; ====
;;; Unit and Counit
;;; ====

;;; make-free-unit : Signature → NatTransform
;;; Unit η : Id → G∘F
;;; η_X(x) = (gen x) — embed element as generator in free algebra
(define (make-free-unit sig)
  (make-nat-transform
   (string->symbol (format "eta-~a" (signature-name sig)))
   functor-id
   (make-forgetful-functor sig)
   make-gen))

;;; make-free-counit : Signature → NatTransform
;;; Counit ε : F∘G → Id
;;; ε_A : Free(Forget(A)) → A
;;; ε_A(term) = recursively extract generators from the term
;;;
;;; For triangle identities: After F(η) wraps generators in extra gen layers,
;;; the counit must recursively unwrap them to restore the original term.
;;;
;;; Example: F(η)((* (gen a) (gen b))) = (* (gen (gen a)) (gen (gen b)))
;;;          ε((* (gen (gen a)) (gen (gen b)))) = (* (gen a) (gen b))
(define (make-free-counit sig)
  ;; Helper: recursively extract one layer of generators
  (define (extract-gen term)
    (cond
      ;; Generator: unwrap one layer
      [(gen? term) (gen-value term)]
      ;; 0-arity constant: unchanged
      [(symbol? term) term]
      ;; n-arity operation: recursively extract from arguments
      [(pair? term)
       (cons (car term)
             (map extract-gen (cdr term)))]
      ;; Fallback
      [else term]))

  (make-nat-transform
   (string->symbol (format "eps-~a" (signature-name sig)))
   (make-free-functor sig)
   functor-id
   extract-gen))

;;; ====
;;; Free Adjunction Constructor
;;; ====

;;; make-free-adjunction : Signature → Adjunction
;;; Construct the Free ⊣ Forgetful adjunction for an algebraic signature.
;;; F = Free functor (builds term algebra)
;;; G = Forgetful functor (extracts carrier)
;;; η = unit (embed as generator)
;;; ε = counit (evaluate terms)
(define (make-free-adjunction sig)
  (make-adjunction
   (string->symbol (format "free-~a" (signature-name sig)))
   (make-free-functor sig)
   (make-forgetful-functor sig)
   (make-free-unit sig)
   (make-free-counit sig)))

;;; ====
;;; Evaluation in Algebra Context
;;; ====

;;; eval-in-algebra : Algebra × Term → Value
;;; Evaluate a (possibly normalized) term in an algebra.
(define (eval-in-algebra alg term)
  (let ([sig (algebra-signature alg)])
    (eval-term alg (normalize-term sig term))))

;;; make-algebra-evaluator : Algebra → (Term → Value)
;;; Create a term evaluator for a specific algebra.
(define (make-algebra-evaluator alg)
  (lambda (term)
    (eval-in-algebra alg term)))

;;; ====
;;; Pre-built Signatures
;;; ====

;;; sig-magma : Signature for Magma (one binary operation, no laws)
(define sig-magma
  (make-signature 'magma
    '((* . 2))
    '()))

;;; sig-semigroup : Signature for Semigroup (associative binary operation)
(define sig-semigroup
  (make-signature 'semigroup
    '((* . 2))
    (list
      ;; Associativity: (* (* x y) z) → (* x (* y z))
      (make-rule 'assoc
                 '(* (* (?x) (?y)) (?z))
                 '(* (?x) (* (?y) (?z)))))))

;;; sig-monoid : Signature for Monoid (identity + associativity)
(define sig-monoid
  (make-signature 'monoid
    '((e . 0) (* . 2))
    (list
      ;; Left identity: (* e x) → x
      (make-rule 'left-id
                 '(* e (?x))
                 '(?x))
      ;; Right identity: (* x e) → x
      (make-rule 'right-id
                 '(* (?x) e)
                 '(?x))
      ;; Associativity: (* (* x y) z) → (* x (* y z))
      (make-rule 'assoc
                 '(* (* (?x) (?y)) (?z))
                 '(* (?x) (* (?y) (?z)))))))

;;; sig-commutative-monoid : Signature for Commutative Monoid
(define sig-commutative-monoid
  (make-signature 'comm-monoid
    '((e . 0) (* . 2))
    (append
      (signature-laws sig-monoid)
      (list
        ;; Commutativity: (* x y) → (* y x) when x > y (canonical ordering)
        ;; Note: Full commutativity normalization requires ordered rewriting.
        ;; For simplicity, we don't add commutativity to avoid non-confluence.
        ))))

;;; sig-group : Signature for Group (monoid + inverses)
(define sig-group
  (make-signature 'group
    '((e . 0) (* . 2) (inv . 1))
    (append
      (signature-laws sig-monoid)
      (list
        ;; Left inverse: (* (inv x) x) → e
        (make-rule 'left-inv
                   '(* (inv (?x)) (?x))
                   'e)
        ;; Right inverse: (* x (inv x)) → e
        (make-rule 'right-inv
                   '(* (?x) (inv (?x)))
                   'e)
        ;; Double inverse: (inv (inv x)) → x
        (make-rule 'inv-inv
                   '(inv (inv (?x)))
                   '(?x))
        ;; Inverse of identity: (inv e) → e
        (make-rule 'inv-id
                   '(inv e)
                   'e)))))

;;; sig-abelian-group : Signature for Abelian Group
(define sig-abelian-group
  (make-signature 'abelian-group
    '((e . 0) (* . 2) (inv . 1))
    (signature-laws sig-group)))  ; Commutativity omitted for confluence

;;; ====
;;; Pre-built Algebras
;;; ====

;;; alg-list-monoid : Algebra for List monoid
(define alg-list-monoid
  (make-algebra sig-monoid 'list
    `((e . ,(lambda () '()))
      (* . ,append))))

;;; alg-sum-monoid : Algebra for Integer sum monoid
(define alg-sum-monoid
  (make-algebra sig-monoid 'number
    `((e . ,(lambda () 0))
      (* . ,+))))

;;; alg-product-monoid : Algebra for Integer product monoid
(define alg-product-monoid
  (make-algebra sig-monoid 'number
    `((e . ,(lambda () 1))
      (* . ,*))))

;;; alg-integer-group : Algebra for Integer addition group
(define alg-integer-group
  (make-algebra sig-group 'integer
    `((e . ,(lambda () 0))
      (* . ,+)
      (inv . ,-))))

;;; ====
;;; Adjunction Instances
;;; ====

;;; adj-free-magma : Free ⊣ Forgetful for Magma
(define adj-free-magma
  (make-free-adjunction sig-magma))

;;; adj-free-semigroup : Free ⊣ Forgetful for Semigroup
(define adj-free-semigroup
  (make-free-adjunction sig-semigroup))

;;; adj-free-monoid : Free ⊣ Forgetful for Monoid
(define adj-free-monoid
  (make-free-adjunction sig-monoid))

;;; adj-free-group : Free ⊣ Forgetful for Group
(define adj-free-group
  (make-free-adjunction sig-group))

;;; ====
;;; Display
;;; ====

;;; signature->string : Signature → String
(define (signature->string sig)
  (if (signature? sig)
      (format "Signature<~a: ~a ops, ~a laws>"
              (signature-name sig)
              (length (signature-operations sig))
              (length (signature-laws sig)))
      "Not a signature"))

;;; algebra->string : Algebra → String
(define (algebra->string alg)
  (if (algebra? alg)
      (format "Algebra<~a over ~a>"
              (signature-name (algebra-signature alg))
              (algebra-carrier alg))
      "Not an algebra"))

;;; term->string : Signature × Term → String
(define (term->string sig term)
  (cond
    [(gen? term)
     (format "(gen ~a)" (gen-value term))]
    [(and (symbol? term) (term-op? sig term))
     (symbol->string term)]
    [(and (pair? term) (term-op? sig term))
     (format "(~a ~a)"
             (car term)
             (apply string-append
                    (map (lambda (arg)
                           (string-append (term->string sig arg) " "))
                         (cdr term))))]
    [else (format "~a" term)]))

;;; ====
;;; Exports
;;; ====
;;;
;;; Signature:
;;;   make-signature, signature?, signature-name
;;;   signature-operations, signature-laws
;;;   signature-op-arity, signature-has-op?
;;;
;;; Algebra:
;;;   make-algebra, algebra?, algebra-signature
;;;   algebra-carrier, algebra-ops, algebra-op
;;;
;;; Terms:
;;;   make-gen, gen?, gen-value
;;;   term-op?, term?
;;;
;;; Term Operations:
;;;   free-fmap, normalize-term, eval-term
;;;   eval-in-algebra, make-algebra-evaluator
;;;
;;; Functors and Adjunction:
;;;   make-free-functor, make-forgetful-functor
;;;   make-free-unit, make-free-counit
;;;   make-free-adjunction
;;;
;;; Pre-built Signatures:
;;;   sig-magma, sig-semigroup, sig-monoid
;;;   sig-commutative-monoid, sig-group, sig-abelian-group
;;;
;;; Pre-built Algebras:
;;;   alg-list-monoid, alg-sum-monoid, alg-product-monoid
;;;   alg-integer-group
;;;
;;; Pre-built Adjunctions:
;;;   adj-free-magma, adj-free-semigroup
;;;   adj-free-monoid, adj-free-group
;;;
;;; Display:
;;;   signature->string, algebra->string, term->string
