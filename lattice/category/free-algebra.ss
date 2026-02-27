;;; @module free-algebra
;;; @requires adjunction rule engine

(require 'adjunction)
(require 'rule)
(require 'engine)

(doc 'module 'free-algebra)
(doc 'description "Free Algebras and Generalized Free ⊣ Forgetful

Generalizes the Free ⊣ Forgetful adjunction pattern (seen in adj-free-list)
to arbitrary algebraic signatures: Monoid, Group, Magma, Ring, etc.

Key abstraction: A Signature describes an algebraic structure
  - operations: list of (op-name . arity)
  - laws: list of rewrite rules for normalization

An Algebra bundles:
  - signature: the algebraic theory
  - carrier: the underlying set (represented as type tag)
  - ops: alist of (op-name . implementation)

The Free ⊣ Forgetful adjunction for signature Σ:
  - Free(X) = term algebra over generators from X
  - Forgetful(A) = underlying carrier set of algebra A
  - Unit η: x ↦ (gen x) — embed as generator
  - Counit ε: term ↦ evaluate term in algebra (extracting ops from algebra)")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'section 'confluence)
(doc 'description "Confluence of Rewrite Rules

When defining custom signatures with rewrite rules (laws), confluence
determines whether normalization produces consistent results.

WHAT IS CONFLUENCE?
A rule set is confluent if every term reaches the same normal form
regardless of which rules are applied first. Non-confluent rules can
give different answers depending on reduction order.

Example of non-confluence:
  Rule 1: (f (g x)) → (h x)
  Rule 2: (g x) → x
  Term: (f (g a))
  Path A: Apply rule 1 first → (h a)
  Path B: Apply rule 2 first → (f a) — stuck, different result

WHEN RULES ARE LIKELY CONFLUENT:
- Non-overlapping left-hand sides: patterns don't match the same terms
- Terminating + locally confluent: all critical pairs rejoin
- Oriented toward simpler terms: each rule reduces complexity
- Orthogonal systems: no critical overlaps between rule patterns

WARNING SIGNS OF NON-CONFLUENCE:
- Overlapping patterns: two rules can both match a term
- Circular rewrites: A → B and B → A (or indirect cycles)
- Missing cases: some overlap combinations lack a joining rule
- Commutativity without ordering: (f x y) ↔ (f y x) loops forever

PRE-BUILT SIGNATURES:
The signatures defined below (sig-monoid, sig-group, etc.) are designed
to be confluent. They use standard orientations:
- Identity laws reduce toward the non-identity term
- Associativity normalizes to right-association
- Inverse laws reduce toward the identity
- Commutativity is intentionally omitted (would cause non-termination)

CUSTOM SIGNATURES:
When creating your own signature, verify:
1. Each rule strictly reduces term size or complexity
2. Overlapping patterns have consistent outcomes (critical pair analysis)
3. No rule can undo another rule's effect

For advanced users: the Knuth-Bendix completion procedure can sometimes
transform non-confluent rules into confluent ones, but this is not
automated here.")

(doc 'section 'signature-definition)
(doc 'description "Signature Definition

A signature describes an algebraic theory:
  - name: identifying symbol
  - operations: ((op-name . arity) ...)  e.g., ((e . 0) (* . 2))
  - laws: list of rewrite rules for normalization")

(define (make-signature name ops laws)
  (doc 'type '(-> Symbol OpList RuleList Signature))
  (doc 'description "Create a signature for an algebraic theory
ops: list of (op-name . arity) pairs
laws: list of rewrite rules for term normalization")
  (list 'signature name ops laws))

(define (signature? x)
  (doc 'type '(-> Any Boolean))
  (doc 'description "Test if value is a signature")
  (and (pair? x)
       (eq? (car x) 'signature)
       (= (length x) 4)))

(define (signature-name sig)
  (doc 'type '(-> Signature Symbol))
  (doc 'description "Get the name of a signature")
  (if (signature? sig) (cadr sig) 'unknown))

(define (signature-operations sig)
  (doc 'type '(-> Signature OpList))
  (doc 'description "Get the operations list from a signature")
  (if (signature? sig) (caddr sig) '()))

(define (signature-laws sig)
  (doc 'type '(-> Signature RuleList))
  (doc 'description "Get the rewrite laws from a signature")
  (if (signature? sig) (cadddr sig) '()))

(define (signature-op-arity sig op-name)
  (doc 'type '(-> Signature Symbol (Maybe Nat)))
  (doc 'description "Get the arity of an operation in the signature")
  (let ([entry (assq op-name (signature-operations sig))])
    (and entry (cdr entry))))

(define (signature-has-op? sig op-name)
  (doc 'type '(-> Signature Symbol Boolean))
  (doc 'description "Test if signature has an operation")
  (and (assq op-name (signature-operations sig)) #t))

(doc 'section 'algebra-definition)
(doc 'description "Algebra Definition

An Algebra bundles a signature with a concrete implementation.
The ops alist maps operation names to procedures.")

(define (make-algebra sig carrier ops)
  (doc 'type '(-> Signature Symbol OpImplList Algebra))
  (doc 'description "Create an algebra over a signature
carrier: a symbol identifying the carrier type
ops: alist of (op-name . procedure)")
  (list 'algebra sig carrier ops))

(define (algebra? x)
  (doc 'type '(-> Any Boolean))
  (doc 'description "Test if value is an algebra")
  (and (pair? x)
       (eq? (car x) 'algebra)
       (= (length x) 4)))

(define (algebra-signature alg)
  (doc 'type '(-> Algebra Signature))
  (doc 'description "Get the signature from an algebra")
  (if (algebra? alg) (cadr alg) #f))

(define (algebra-carrier alg)
  (doc 'type '(-> Algebra Symbol))
  (doc 'description "Get the carrier type symbol from an algebra")
  (if (algebra? alg) (caddr alg) 'unknown))

(define (algebra-ops alg)
  (doc 'type '(-> Algebra OpImplList))
  (doc 'description "Get the operations implementation list")
  (if (algebra? alg) (cadddr alg) '()))

(define (algebra-op alg op-name)
  (doc 'type '(-> Algebra Symbol (Maybe Procedure)))
  (doc 'description "Get the implementation of an operation")
  (let ([entry (assq op-name (algebra-ops alg))])
    (and entry (cdr entry))))

(define (validate-algebra alg)
  (doc 'type '(-> Algebra (Result Algebra String)))
  (doc 'description "Validate that an algebra correctly implements its signature
Checks:
  1. All signature operations have implementations
  2. All implementations are procedures
  3. No extra operations beyond the signature")
  (if (not (algebra? alg))
      (list 'err "Not an algebra")
      (let* ([sig (algebra-signature alg)]
             [sig-ops (signature-operations sig)]
             [alg-ops (algebra-ops alg)]
             [sig-names (map car sig-ops)]
             [alg-names (map car alg-ops)])
        ;; Check for missing operations
        (let ([missing (filter (lambda (name) (not (assq name alg-ops))) sig-names)])
          (if (pair? missing)
              (list 'err (format "Missing operations: ~a" missing))
              ;; Check all implementations are procedures
              (let ([non-procs (filter (lambda (entry)
                                         (not (procedure? (cdr entry))))
                                       alg-ops)])
                (if (pair? non-procs)
                    (list 'err (format "Non-procedure implementations: ~a"
                                       (map car non-procs)))
                    ;; Check for extra operations (warning, not error)
                    (let ([extra (filter (lambda (name) (not (assq name sig-ops))) alg-names)])
                      (if (pair? extra)
                          (list 'ok alg extra)  ; ok with warning
                          (list 'ok alg))))))))))

(define (algebra-valid? alg)
  (doc 'type '(-> Algebra Boolean))
  (doc 'description "Quick predicate for algebra validity")
  (let ([result (validate-algebra alg)])
    (and (pair? result) (eq? (car result) 'ok))))

(define (make-validated-algebra sig carrier ops)
  (doc 'type '(-> Signature Symbol OpImplList Algebra))
  (doc 'description "Like make-algebra but validates and raises error on failure")
  (let* ([alg (make-algebra sig carrier ops)]
         [result (validate-algebra alg)])
    (if (eq? (car result) 'err)
        (error 'make-validated-algebra (cadr result))
        alg)))

(doc 'section 'algebra-homomorphisms)
(doc 'description "Algebra Homomorphisms

An algebra homomorphism h : A → B is a function that preserves structure:
  For each n-ary operation σ:  h(σ_A(a₁,...,aₙ)) = σ_B(h(a₁),...,h(aₙ))
  For 0-ary operations (constants):  h(e_A) = e_B")

(define (make-algebra-hom source target f)
  (doc 'type '(-> Algebra Algebra (-> Any Any) AlgebraHom))
  (doc 'description "Create an algebra homomorphism from source to target.
The function f should preserve the algebraic structure.")
  (list 'algebra-hom source target f))

(define (algebra-hom? x)
  (doc 'type '(-> Any Boolean))
  (and (pair? x)
       (eq? (car x) 'algebra-hom)
       (= (length x) 4)))

(define (algebra-hom-source h)
  (doc 'type '(-> AlgebraHom Algebra))
  (if (algebra-hom? h) (cadr h) #f))

(define (algebra-hom-target h)
  (doc 'type '(-> AlgebraHom Algebra))
  (if (algebra-hom? h) (caddr h) #f))

(define (algebra-hom-function h)
  (doc 'type '(-> AlgebraHom (-> Any Any)))
  (if (algebra-hom? h) (cadddr h) #f))

(define (algebra-hom-apply h x)
  (doc 'type '(-> AlgebraHom Any Any))
  (doc 'description "Apply a homomorphism to a value")
  ((algebra-hom-function h) x))

;;; verify-homomorphism : AlgebraHom × TestValues → Boolean
;;; Verify the homomorphism law for each operation on test values.
;;; test-values: alist of (arity . values-list) for generating test cases
;;; Returns #t if all tests pass.
(define (verify-homomorphism hom test-values)
  (let* ([source (algebra-hom-source hom)]
         [target (algebra-hom-target hom)]
         [f (algebra-hom-function hom)]
         [sig (algebra-signature source)]
         [ops (signature-operations sig)])
    ;; For each operation, verify h(σ(args)) = σ(h(args))
    (let loop ([ops ops])
      (if (null? ops)
          #t
          (let* ([op-entry (car ops)]
                 [op-name (car op-entry)]
                 [arity (cdr op-entry)]
                 [source-op (algebra-op source op-name)]
                 [target-op (algebra-op target op-name)])
            (cond
              ;; 0-arity: h(e_A) = e_B
              [(= arity 0)
               (if (equal? (f (source-op)) (target-op))
                   (loop (cdr ops))
                   #f)]
              ;; n-arity: check with provided test values
              [else
               (let ([entry (assv arity test-values)])
                 (if (not entry)
                     (loop (cdr ops))  ; No test values for this arity, skip
                     (let check-vals ([vals (cdr entry)])
                       (if (null? vals)
                           (loop (cdr ops))
                           (let* ([args (car vals)]
                                  [lhs (f (apply source-op args))]
                                  [rhs (apply target-op (map f args))])
                             (if (equal? lhs rhs)
                                 (check-vals (cdr vals))
                                 #f))))))]))))))

;;; compose-algebra-hom : AlgebraHom × AlgebraHom → AlgebraHom
;;; Compose two homomorphisms: (g ∘ h)(x) = g(h(x))
;;; Precondition: target of h = source of g (structurally equal)
(define (compose-algebra-hom g h)
  (let ([h-target (algebra-hom-target h)]
        [g-source (algebra-hom-source g)])
    ;; Use equal? for structural equality, not eq? for pointer equality.
    ;; This allows composition of homomorphisms from different call sites
    ;; that produce structurally identical algebras.
    (if (not (equal? h-target g-source))
        (error 'compose-algebra-hom "Target of h must equal source of g")
        (make-algebra-hom
         (algebra-hom-source h)
         (algebra-hom-target g)
         (lambda (x) (algebra-hom-apply g (algebra-hom-apply h x)))))))

;;; identity-algebra-hom : Algebra → AlgebraHom
;;; The identity homomorphism on an algebra.
(define (identity-algebra-hom alg)
  (make-algebra-hom alg alg (lambda (x) x)))

(doc 'section 'term-representation)
(doc 'description "Term Representation

Terms in the free algebra are built from:
  - Generators: (gen x) — elements from the carrier set
  - Operations: (op-name term ...) — operation applied to subterms
  - Constants: just the op-name for 0-arity operations")

(define (make-gen x)
  (doc 'type '(-> Any Generator))
  (doc 'description "Create a generator (embedding of a carrier element into the free algebra)")
  (list 'gen x))

(define (gen? t)
  (doc 'type '(-> Any Boolean))
  (and (pair? t)
       (eq? (car t) 'gen)
       (= (length t) 2)))

(define (gen-value g)
  (doc 'type '(-> Generator Any))
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

(doc 'section 'free-forgetful-functors)

(define (make-free-functor sig)
  (doc 'type '(-> Signature Functor))
  (doc 'description "The Free functor: Set → Alg(Σ)
F(X) = term algebra over X (as generators)
F(f) = map f over generators in terms

Note: The functor's fmap directly transforms terms for efficiency.
For categorical composition in Alg(Σ), use free-morphism to get
first-class algebra-hom objects instead.")
  (make-named-functor
   (string->symbol (format "Free-~a" (signature-name sig)))
   (lambda (f term)
     (free-fmap sig f term))))

(define (make-free-algebra sig generators)
  (doc 'type '(-> Signature (List Any) Algebra))
  (doc 'description "Create the free algebra over a set of generators.
The carrier includes the generators to distinguish Free(A) from Free(B).
Operations build term ASTs. This is an explicit algebra object for cat-Alg.")
  (let* ([ops (signature-operations sig)]
         [op-impls (map (lambda (op-spec)
                          (let ([name (car op-spec)]
                                [arity (cdr op-spec)])
                            (cons name
                                  (if (= arity 0)
                                      (lambda () name)
                                      (lambda args (cons name args))))))
                        ops)]
         ;; Include generators in carrier so Free(A) ≠ Free(B)
         [carrier (list 'free-term generators)])
    (list 'algebra sig carrier op-impls)))

(define (free-morphism sig source-gens target-gens f)
  (doc 'type '(-> Signature (List Any) (List Any) (-> Any Any) AlgebraHom))
  (doc 'description "Construct Free(f) as a first-class algebra homomorphism.
Given f : A → B (a function on generator sets), returns the algebra-hom
Free(f) : Free(A) → Free(B) for use in categorical composition.

Use this when you need to compose morphisms in cat-Alg.
For simple term transformation, use (functor-fmap (make-free-functor sig)) instead.")
  (let ([source-alg (make-free-algebra sig source-gens)]
        [target-alg (make-free-algebra sig target-gens)])
    (make-algebra-hom
     source-alg
     target-alg
     (lambda (term) (free-fmap sig f term)))))

(define (make-forgetful-functor sig)
  (doc 'type '(-> Signature Functor))
  (doc 'description "The Forgetful functor: Alg(Σ) → Set

Categorical semantics:
  On objects:    G(A) = carrier set of algebra A
  On morphisms:  G(h : A → B) = underlying function h : |A| → |B|

In our Scheme encoding:
  - Algebras are first-class values containing their carriers
  - Algebra homomorphisms are represented as functions
  - Therefore G(h)(x) = h(x), which is just function application

This is correct categorically: the forgetful functor forgets the
algebraic structure and retains only the underlying set/function.
The apparent simplicity reflects that Set is the base category.")
  (make-named-functor
   (string->symbol (format "Forget-~a" (signature-name sig)))
   ;; fmap for forgetful: given h : A → B (underlying function),
   ;; produce Forget(h) : Forget(A) → Forget(B)
   ;; Since Forget(A) = carrier elements, this is just h itself.
   (lambda (h x) (h x))))

;;; forget-carrier : Algebra → Symbol
;;; Extract the carrier type from an algebra (forgetful functor on objects).
(define (forget-carrier alg)
  (algebra-carrier alg))

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

(doc 'section 'prebuilt-signatures)

(doc sig-magma 'type 'Signature)
(doc sig-magma 'description "Signature for Magma (one binary operation, no laws)")
(define sig-magma
  (make-signature 'magma
    '((* . 2))
    '()))

(doc sig-semigroup 'type 'Signature)
(doc sig-semigroup 'description "Signature for Semigroup (associative binary operation)")
(define sig-semigroup
  (make-signature 'semigroup
    '((* . 2))
    (list
      (make-rule 'assoc
                 '(* (* (?x) (?y)) (?z))
                 '(* (?x) (* (?y) (?z)))))))

(doc sig-monoid 'type 'Signature)
(doc sig-monoid 'description "Signature for Monoid (identity + associativity)")
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

(doc sig-commutative-monoid 'type 'Signature)
(doc sig-commutative-monoid 'description "Signature for Commutative Monoid.
Note: Full commutativity normalization requires ordered rewriting.
For simplicity, we don't add commutativity to avoid non-confluence.")
(define sig-commutative-monoid
  (make-signature 'comm-monoid
    '((e . 0) (* . 2))
    (append
      (signature-laws sig-monoid)
      (list))))

(doc sig-group 'type 'Signature)
(doc sig-group 'description "Signature for Group (monoid + inverses)")
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

(doc sig-abelian-group 'type 'Signature)
(doc sig-abelian-group 'description "Signature for Abelian Group.
Commutativity omitted for confluence")
(define sig-abelian-group
  (make-signature 'abelian-group
    '((e . 0) (* . 2) (inv . 1))
    (signature-laws sig-group)))

(doc 'section 'prebuilt-algebras)

(doc alg-list-monoid 'type 'Algebra)
(doc alg-list-monoid 'description "Algebra for List monoid")
(define alg-list-monoid
  (make-algebra sig-monoid 'list
    `((e . ,(lambda () '()))
      (* . ,append))))

(doc alg-sum-monoid 'type 'Algebra)
(doc alg-sum-monoid 'description "Algebra for Integer sum monoid")
(define alg-sum-monoid
  (make-algebra sig-monoid 'number
    `((e . ,(lambda () 0))
      (* . ,+))))

(doc alg-product-monoid 'type 'Algebra)
(doc alg-product-monoid 'description "Algebra for Integer product monoid")
(define alg-product-monoid
  (make-algebra sig-monoid 'number
    `((e . ,(lambda () 1))
      (* . ,*))))

(doc alg-integer-group 'type 'Algebra)
(doc alg-integer-group 'description "Algebra for Integer addition group")
(define alg-integer-group
  (make-algebra sig-group 'integer
    `((e . ,(lambda () 0))
      (* . ,+)
      (inv . ,-))))

(doc 'section 'adjunction-instances)

(doc adj-free-magma 'type 'Adjunction)
(doc adj-free-magma 'description "Free ⊣ Forgetful for Magma")
(define adj-free-magma
  (make-free-adjunction sig-magma))

(doc adj-free-semigroup 'type 'Adjunction)
(doc adj-free-semigroup 'description "Free ⊣ Forgetful for Semigroup")
(define adj-free-semigroup
  (make-free-adjunction sig-semigroup))

(doc adj-free-monoid 'type 'Adjunction)
(doc adj-free-monoid 'description "Free ⊣ Forgetful for Monoid")
(define adj-free-monoid
  (make-free-adjunction sig-monoid))

(doc adj-free-group 'type 'Adjunction)
(doc adj-free-group 'description "Free ⊣ Forgetful for Group")
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

(doc 'section 'exports)
(doc 'description "Exports

Signature:
  make-signature, signature?, signature-name
  signature-operations, signature-laws
  signature-op-arity, signature-has-op?

Algebra:
  make-algebra, algebra?, algebra-signature
  algebra-carrier, algebra-ops, algebra-op
  validate-algebra, algebra-valid?, make-validated-algebra

Algebra Homomorphisms:
  make-algebra-hom, algebra-hom?, algebra-hom-source
  algebra-hom-target, algebra-hom-function, algebra-hom-apply
  verify-homomorphism, compose-algebra-hom, identity-algebra-hom

Terms:
  make-gen, gen?, gen-value
  term-op?, term?

Term Operations:
  free-fmap, normalize-term, eval-term
  eval-in-algebra, make-algebra-evaluator

Functors and Adjunction:
  make-free-functor, make-forgetful-functor
  make-free-algebra, free-morphism
  make-free-unit, make-free-counit
  make-free-adjunction, forget-carrier

Pre-built Signatures:
  sig-magma, sig-semigroup, sig-monoid
  sig-commutative-monoid, sig-group, sig-abelian-group

Pre-built Algebras:
  alg-list-monoid, alg-sum-monoid, alg-product-monoid
  alg-integer-group

Pre-built Adjunctions:
  adj-free-magma, adj-free-semigroup
  adj-free-monoid, adj-free-group

Display:
  signature->string, algebra->string, term->string")
