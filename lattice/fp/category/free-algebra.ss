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
;;;
;;; ====
;;; Confluence of Rewrite Rules
;;; ====
;;;
;;; When defining custom signatures with rewrite rules (laws), confluence
;;; determines whether normalization produces consistent results.
;;;
;;; WHAT IS CONFLUENCE?
;;; A rule set is confluent if every term reaches the same normal form
;;; regardless of which rules are applied first. Non-confluent rules can
;;; give different answers depending on reduction order.
;;;
;;; Example of non-confluence:
;;;   Rule 1: (f (g x)) → (h x)
;;;   Rule 2: (g x) → x
;;;   Term: (f (g a))
;;;   Path A: Apply rule 1 first → (h a)
;;;   Path B: Apply rule 2 first → (f a) — stuck, different result
;;;
;;; WHEN RULES ARE LIKELY CONFLUENT:
;;; - Non-overlapping left-hand sides: patterns don't match the same terms
;;; - Terminating + locally confluent: all critical pairs rejoin
;;; - Oriented toward simpler terms: each rule reduces complexity
;;; - Orthogonal systems: no critical overlaps between rule patterns
;;;
;;; WARNING SIGNS OF NON-CONFLUENCE:
;;; - Overlapping patterns: two rules can both match a term
;;; - Circular rewrites: A → B and B → A (or indirect cycles)
;;; - Missing cases: some overlap combinations lack a joining rule
;;; - Commutativity without ordering: (f x y) ↔ (f y x) loops forever
;;;
;;; PRE-BUILT SIGNATURES:
;;; The signatures defined below (sig-monoid, sig-group, etc.) are designed
;;; to be confluent. They use standard orientations:
;;; - Identity laws reduce toward the non-identity term
;;; - Associativity normalizes to right-association
;;; - Inverse laws reduce toward the identity
;;; - Commutativity is intentionally omitted (would cause non-termination)
;;;
;;; CUSTOM SIGNATURES:
;;; When creating your own signature, verify:
;;; 1. Each rule strictly reduces term size or complexity
;;; 2. Overlapping patterns have consistent outcomes (critical pair analysis)
;;; 3. No rule can undo another rule's effect
;;;
;;; For advanced users: the Knuth-Bendix completion procedure can sometimes
;;; transform non-confluent rules into confluent ones, but this is not
;;; automated here.

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

;;; validate-algebra : Algebra → (ok Algebra) | (err String)
;;; Validate that an algebra correctly implements its signature.
;;; Checks:
;;;   1. All signature operations have implementations
;;;   2. All implementations are procedures
;;;   3. No extra operations beyond the signature
(define (validate-algebra alg)
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

;;; algebra-valid? : Algebra → Boolean
;;; Quick predicate for algebra validity.
(define (algebra-valid? alg)
  (let ([result (validate-algebra alg)])
    (and (pair? result) (eq? (car result) 'ok))))

;;; make-validated-algebra : Signature × Symbol × OpImplList → Algebra
;;; Like make-algebra but validates and raises error on failure.
(define (make-validated-algebra sig carrier ops)
  (let* ([alg (make-algebra sig carrier ops)]
         [result (validate-algebra alg)])
    (if (eq? (car result) 'err)
        (error 'make-validated-algebra (cadr result))
        alg)))

;;; ====
;;; Algebra Homomorphisms
;;; ====
;;;
;;; An algebra homomorphism h : A → B is a function that preserves structure:
;;;   For each n-ary operation σ:  h(σ_A(a₁,...,aₙ)) = σ_B(h(a₁),...,h(aₙ))
;;;   For 0-ary operations (constants):  h(e_A) = e_B

;;; make-algebra-hom : Algebra × Algebra × (Any → Any) → AlgebraHom
;;; Create an algebra homomorphism from source to target.
;;; The function f should preserve the algebraic structure.
(define (make-algebra-hom source target f)
  (list 'algebra-hom source target f))

;;; algebra-hom? : Any → Boolean
(define (algebra-hom? x)
  (and (pair? x)
       (eq? (car x) 'algebra-hom)
       (= (length x) 4)))

;;; algebra-hom-source : AlgebraHom → Algebra
(define (algebra-hom-source h)
  (if (algebra-hom? h) (cadr h) #f))

;;; algebra-hom-target : AlgebraHom → Algebra
(define (algebra-hom-target h)
  (if (algebra-hom? h) (caddr h) #f))

;;; algebra-hom-function : AlgebraHom → (Any → Any)
(define (algebra-hom-function h)
  (if (algebra-hom? h) (cadddr h) #f))

;;; algebra-hom-apply : AlgebraHom × Any → Any
;;; Apply a homomorphism to a value.
(define (algebra-hom-apply h x)
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
               (let ([vals (cdr (assv arity test-values))])
                 (if (not vals)
                     (loop (cdr ops))  ; No test values for this arity, skip
                     (let check-vals ([vals vals])
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
;;; Precondition: target of h = source of g
(define (compose-algebra-hom g h)
  (let ([h-target (algebra-hom-target h)]
        [g-source (algebra-hom-source g)])
    (if (not (eq? h-target g-source))
        (error 'compose-algebra-hom "Target of h must equal source of g")
        (make-algebra-hom
         (algebra-hom-source h)
         (algebra-hom-target g)
         (lambda (x) (algebra-hom-apply g (algebra-hom-apply h x)))))))

;;; identity-algebra-hom : Algebra → AlgebraHom
;;; The identity homomorphism on an algebra.
(define (identity-algebra-hom alg)
  (make-algebra-hom alg alg (lambda (x) x)))

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
;;;
;;; Categorical semantics:
;;;   On objects:    G(A) = carrier set of algebra A
;;;   On morphisms:  G(h : A → B) = underlying function h : |A| → |B|
;;;
;;; In our Scheme encoding:
;;;   - Algebras are first-class values containing their carriers
;;;   - Algebra homomorphisms are represented as functions
;;;   - Therefore G(h)(x) = h(x), which is just function application
;;;
;;; This is correct categorically: the forgetful functor "forgets" the
;;; algebraic structure and retains only the underlying set/function.
;;; The apparent simplicity reflects that Set is the "base" category.
(define (make-forgetful-functor sig)
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
