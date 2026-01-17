## 5. The Type Theory


The Fold implements a gradual dependent type system combining multiple features into a coherent whole.

### 5.1 Core Type Language

**Base Types**:
```
BaseType ::= Nat | Int | Bool | Char | Symbol
           | String | Bytes | Unit | Void | Hash
```

**Compound Types**:
```
Type ::= BaseType
       | (→ Type Type)           ; Function
       | (× Type Type)           ; Product (pair)
       | (+ (Tag Type) ...)      ; Sum (tagged union)
       | (List Type)             ; Homogeneous list
       | (Vector Type)           ; Homogeneous vector
       | (Block Symbol Type)     ; Block with tag and payload type
       | (Ref Type)              ; Content-addressed reference
       | (∀ (α ...) Type)        ; Universal quantification
       | (μ α. Type)             ; Recursive type
       | (Cap Name Type)         ; Capability type
       | ?                       ; Hole (gradual typing)
       | (? name)                ; Named hole
       | α                       ; Type variable
```

**Type Grammar** (BNF):
```bnf
<type>      ::= <base> | <compound> | <var> | <hole>
<base>      ::= "Nat" | "Int" | "Bool" | "Char" | "Symbol"
              | "String" | "Bytes" | "Unit" | "Void" | "Hash"
<compound>  ::= "(" "→" <type> <type> ")"
              | "(" "×" <type>+ ")"
              | "(" "+" <variant>+ ")"
              | "(" "List" <type> ")"
              | "(" "Vector" <type> ")"
              | "(" "Block" <symbol> <type> ")"
              | "(" "∀" "(" <var>+ ")" <type> ")"
              | "(" "μ" <var> <type> ")"
              | "(" "Cap" <name> <type> ")"
<variant>   ::= "(" <tag> <type> ")"
<hole>      ::= "?" | "(" "?" <name> ")"
<var>       ::= <identifier>
```

### 5.2 Bidirectional Type Checking

Following Dunfield & Krishnaswami (2013), we use bidirectional type checking with two judgments:

**Synthesis** (↑): `Γ ⊢ e ⇒ A` — infer type A for expression e
**Checking** (↓): `Γ ⊢ e ⇐ A` — check that e has type A

**Selected Rules**:

```
─────────────────────────────  [Var]
Γ, x : A ⊢ x ⇒ A


Γ ⊢ e ⇐ A
─────────────────────────────  [Anno]
Γ ⊢ (e : A) ⇒ A


Γ ⊢ e₁ ⇒ (→ A B)    Γ ⊢ e₂ ⇐ A
───────────────────────────────────  [App]
Γ ⊢ (e₁ e₂) ⇒ B


Γ, x : A ⊢ e ⇐ B
─────────────────────────────────  [Lam⇐]
Γ ⊢ (λ x. e) ⇐ (→ A B)


Γ ⊢ e ⇒ A    A ≤ B
───────────────────────────────  [Sub]
Γ ⊢ e ⇐ B
```

**Unification**: Type inference uses Robinson unification with occurs check:

```scheme
(define (unify t1 t2)
  (cond
    [(type-var? t1) (bind t1 t2)]
    [(type-var? t2) (bind t2 t1)]
    [(and (function-type? t1) (function-type? t2))
     (compose (unify (param t1) (param t2))
              (unify (return t1) (return t2)))]
    [(type=? t1 t2) empty-subst]
    [else (error 'unification-failed t1 t2)]))
```

### 5.3 Dependent Types

**Pi Types** (Dependent Functions):
```
(Π ((x : A)) B)
```

The type of functions where the return type B may depend on the argument value x.

```scheme
;; Length-indexed vectors
(Π ((n : Nat)) (→ (Vec n Int) Int))

;; head requires non-empty vector
(Π ((n : Nat)) (→ (Vec (succ n) A) A))
```

**Sigma Types** (Dependent Pairs):
```
(Σ ((x : A)) B)
```

Pairs where the type of the second component depends on the first component's value.

```scheme
;; Existential length
(Σ ((n : Nat)) (Vec n Int))
```

**Universe Hierarchy**:
```
Type₀ : Type₁ : Type₂ : ...
```

`Type₀` (often written just `Type`) classifies ordinary types. `Type₁` classifies `Type₀`, and so on. This hierarchy prevents Russell's paradox.

**Propositional Equality**:
```
(= A x y)    ; x equals y at type A
```

With introduction and elimination:
```scheme
refl : (Π ((A : Type)) (Π ((x : A)) (= A x x)))

J : (Π ((A : Type))
     (Π ((P : (Π ((x : A)) (Π ((y : A)) (→ (= A x y) Type)))))
      (Π ((x : A))
       (Π ((p : (P x x refl)))
        (Π ((y : A))
         (Π ((eq : (= A x y)))
          (P x y eq)))))))
```

### 5.4 Inductive Data Types

Data declarations introduce inductive types with constructors:

```scheme
(data Nat
  [zero : Nat]
  [succ : (→ Nat Nat)])

(data (List A)
  [nil  : (List A)]
  [cons : (→ A (→ (List A) (List A)))])

(data (Vec A (n : Nat))
  [vnil  : (Vec A zero)]
  [vcons : (Π ((m : Nat)) (→ A (→ (Vec A m) (Vec A (succ m)))))])
```

**Eliminator Generation**: Each inductive type generates an eliminator (recursor):

```scheme
;; For Nat:
elim-Nat : (Π ((P : (→ Nat Type)))
            (→ (P zero)
             (→ (Π ((n : Nat)) (→ (P n) (P (succ n))))
              (Π ((n : Nat)) (P n)))))
```

### 5.5 Normalization by Evaluation (NbE)

Type checking with dependent types requires comparing types for equality. We use Normalization by Evaluation:

**Semantic Domain**:
```scheme
V ::= (V-lam param body env)     ; Closure
    | (V-pi domain codomain)     ; Pi value
    | (V-sigma fst snd)          ; Sigma value
    | (V-type level)             ; Universe
    | (V-neutral var elims)      ; Stuck computation
    | (V-base tag)               ; Base type/value
```

**Evaluation**: Syntax → Semantics
```scheme
(define (eval expr env)
  (match expr
    [(λ x body) (V-lam x body env)]
    [(app f a) (apply-value (eval f env) (eval a env))]
    ...))
```

**Quotation**: Semantics → Syntax (normalized)
```scheme
(define (quote-value val)
  (match val
    [(V-lam x body env)
     (let ([v (fresh-var)])
       `(λ ,v ,(quote-value (eval body (extend env x (V-neutral v '()))))))]
    ...))
```

**Definitional Equality**:
```scheme
(define (conv-eq? v1 v2)
  (equal? (quote-value v1) (quote-value v2)))
```

### 5.6 Higher-Kinded Types and Kind System

**Kind Grammar**:
```
Kind ::= *                    ; Type
       | (⇒ Kind Kind)        ; Kind arrow
       | Constraint           ; Type class constraint
       | Row                  ; Row kind
       | (κ∀ (κ ...) Kind)    ; Kind polymorphism
       | (Πκ ((v : K)) K')    ; Dependent kind
       | □                    ; Sort (kind of kinds)
```

**Built-in Kinds**:
```scheme
List   : * ⇒ *
Either : * ⇒ * ⇒ *
(→)    : * ⇒ * ⇒ *
Monad  : (* ⇒ *) ⇒ Constraint
```

**Kind Inference**:
```scheme
(define (infer-kind type kenv)
  (match type
    [(@ f arg)
     (let ([fk (infer-kind f kenv)]
           [ak (infer-kind arg kenv)])
       (match fk
         [(⇒ dom cod)
          (if (kind=? dom ak) cod
              (error 'kind-mismatch))]))]
    ...))
```

### 5.7 Type Classes via Dictionary-Passing

Type classes are implemented as *explicit dictionary values*, not implicit resolution:

```scheme
;; Monoid dictionary type
(define-record-type monoid
  (fields mempty mappend))

;; Instances are values
(define monoid-sum
  (make-monoid 0 +))

(define monoid-list
  (make-monoid '() append))

;; Functions take dictionaries explicitly
(define (mconcat dict xs)
  (fold-right (monoid-mappend dict)
              (monoid-mempty dict)
              xs))

;; Usage
(mconcat monoid-sum '(1 2 3 4))   ; → 10
(mconcat monoid-list '((a) (b)))  ; → (a b)
```

**Benefits**:
- Maintains purity (no global instance database)
- Enables local instances (pass different dictionary)
- Simple, predictable semantics
- Compatible with Core's call-by-value model

**Functional Dependencies**: Multi-parameter classes with type-level functions:

```scheme
(class (Collection c e) | c -> e
  (empty : c)
  (insert : (→ e (→ c c))))
```

The `| c -> e` declares that `c` determines `e`—given a concrete collection type, the element type is fixed.

### 5.8 GADTs with Pattern Refinement

Generalized Algebraic Data Types allow constructors to return refined types:

```scheme
(data (Expr A)
  [Lit  : (→ Int (Expr Int))]
  [Bool : (→ Bool (Expr Bool))]
  [Add  : (→ (Expr Int) (→ (Expr Int) (Expr Int)))]
  [Eq   : (Π ((B : Type)) (→ (Expr B) (→ (Expr B) (Expr Bool))))]
  [If   : (Π ((B : Type))
           (→ (Expr Bool) (→ (Expr B) (→ (Expr B) (Expr B)))))])
```

**Pattern Refinement**: Matching on a GADT constructor refines the type variable:

```scheme
(define (eval-expr [e : (Expr A)]) : A
  (match e
    [(Lit n) n]                    ; A refined to Int
    [(Bool b) b]                   ; A refined to Bool
    [(Add x y) (+ (eval-expr x) (eval-expr y))]
    [(Eq B x y) (equal? (eval-expr x) (eval-expr y))]
    [(If B c t f) (if (eval-expr c) (eval-expr t) (eval-expr f))]))
```

### 5.9 Gradual Typing Integration

Holes (`?`) enable partial type specifications:

```scheme
;; Fully specified
(define (add [x : Int] [y : Int]) : Int (+ x y))

;; Partially specified
(define (process [x : ?]) : ? (complex-operation x))

;; Named holes for documentation
(define (transform [x : (? input)]) : (? output) ...)
```

**Semantics**: Holes unify with any type during inference. The system is *sound where types are known*—type errors are caught at typed boundaries, while untyped regions defer checking to runtime.

#### 5.9.1 Hole Constraint Tracking

Rather than simply ignoring holes during unification (which loses information), The Fold converts holes to *hole variables* and records what types they unify with:

**Anonymous Holes** (`?`): Each occurrence generates a fresh hole variable (`?1`, `?2`, etc.). Multiple anonymous holes are independent:

```scheme
;; (→ ? ?) unifying with (→ Int Bool) produces:
;;   ?1 → Int
;;   ?2 → Bool
;; The two holes are independent constraints
```

**Named Holes** (`(? name)`): All occurrences of the same named hole share a single hole variable (`?name`). This enforces consistency:

```scheme
;; (→ (? t) (? t)) unifying with (→ Int Int) succeeds:
;;   ?t → Int

;; (→ (? t) (? t)) unifying with (→ Int Bool) fails:
;;   Cannot unify Int with Bool (inconsistent use of ?t)
```

**Constraint Extraction**: After inference, hole constraints can be extracted from the substitution:

```scheme
(hole-constraints subst)      ; → ((?x . Int) (?y . Bool) ...)
(type-var-constraints subst)  ; → ((τ1 . String) ...)
```

This enables:
- Better error messages ("hole ?x was inferred as Int")
- Potential runtime cast generation for full gradual typing
- IDE tooling showing inferred types for holes

#### 5.9.2 Interaction with Dependent Types

Combining gradual and dependent types is a known hard problem (Eremondi et al., 2019). The core difficulty: in `(Π ((x : A)) B)`, the type `B` may mention `x`. If `A` is a hole, what is `x`? If we don't know `x`'s type, we cannot normalize `B`.

**Example of the problem:**
```scheme
;; What does this mean?
(Π ((x : ?)) (Vec x Int))

;; If x : Nat, this is a length-indexed vector
;; If x : String, this is nonsense
;; With x : ?, we cannot evaluate (Vec x Int)
```

**The Fold's approach: Strict separation**

Rather than approximate normalization (Eremondi et al.) or elaborate runtime checks, we enforce separation:

1. **Holes cannot appear in dependent positions**:
   ```scheme
   ;; Allowed: hole in simple function type
   (→ ? Int)

   ;; Rejected: hole as Pi domain when codomain depends on it
   (Π ((x : ?)) (Vec x Int))  ; ERROR: x used dependently

   ;; Allowed: hole in Pi domain when codomain is independent
   (Π ((x : ?)) Int)  ; OK: degenerates to (→ ? Int)
   ```

2. **Dependent elimination requires concrete types**:
   ```scheme
   ;; Pattern matching on (Vec n A) requires n to be concrete
   (match vec
     [(vnil) ...]
     [(vcons x xs) ...])  ; n must be known to refine types
   ```

3. **Gradual and dependent regions don't mix**:
   - A module is either "dependently typed" (no holes in signatures) or "gradually typed" (holes allowed, no dependent types)
   - Cross-boundary calls insert runtime checks

**What we sacrifice:**
- Cannot incrementally add dependent types to untyped code
- Cannot have "partially dependent" functions
- Less flexibility than full gradual dependent types

**What we gain:**
- Simple, predictable semantics
- No approximate normalization complexity
- Clear separation of concerns
- Type checking remains decidable

**Future direction**: We may explore restricted approximate normalization for specific patterns (e.g., length-indexed vectors with unknown but bounded length).

### 5.10 Contract System

Contracts provide runtime verification with precise blame tracking. They complement the static type system, especially for gradual typing boundaries.

#### 5.10.1 Contract Grammar

```
Contract ::= (Flat Predicate)              ; Simple predicate
           | (→ (Contract ...) Contract)   ; Function contract
           | (Dep (Var ...) Contract)      ; Dependent contract
           | (And Contract ...)            ; Conjunction
           | (Or Contract ...)             ; Disjunction
           | (Not Contract)                ; Negation
           | Any                           ; Top (always satisfied)
           | None                          ; Bottom (never satisfied)
```

**Flat contracts** check a predicate immediately:
```scheme
nat/c   = (Flat (λ (x) (and (integer? x) (>= x 0))))
pos/c   = (Flat (λ (x) (and (number? x) (> x 0))))
```

**Function contracts** specify domain and range:
```scheme
(->c (list nat/c nat/c) pos/c)  ; (Nat × Nat) → Pos
```

#### 5.10.2 Higher-Order Contract Wrapping

For higher-order functions, contracts must wrap function values to check them at each call site. The key challenge is **blame assignment**—who is at fault when a contract is violated?

**First-Order Blame**:
- **Domain violation** → blame the **caller** (they passed bad arguments)
- **Range violation** → blame the **callee** (the function returned a bad result)

**Higher-Order Blame Flip**:
When a function is passed as an argument, blame must "flip" across the boundary:

```scheme
;; Contract: ((Nat → Nat) → Nat)
;; A function that takes a callback and returns a Nat

(define (apply-twice f) (f (f 5)))
(define wrapped (apply-contract ho-contract apply-twice 'apply-twice))

;; Case 1: Caller provides bad callback (returns wrong type)
(wrapped (λ (x) "bad"))  ; Callback violates range → blame CALLER
                         ; (caller provided a faulty callback)

;; Case 2: Callee misuses the callback
(define (misuse f) (f "not a number"))  ; Calls f with string
(define wrapped-misuse (apply-contract ho-contract misuse 'misuse))
(wrapped-misuse (λ (x) (+ x 1)))  ; Domain violation → blame CALLEE
                                  ; (callee misused the callback)
```

The blame flip rule: when wrapping a higher-order argument, swap caller↔callee. This ensures:
- If the callback itself is broken → caller's fault (they provided it)
- If the callback is used incorrectly → callee's fault (they misused it)

#### 5.10.3 Blame Tracking

Blame records capture violation context:

```scheme
(blame party location message value)

party    : 'caller | 'callee
location : symbol (function name or source location)
message  : human-readable description
value    : the offending value
```

`flip-blame` swaps caller↔callee when crossing contract boundaries with higher-order values.

#### 5.10.4 Contract Combinators

**Container contracts**:
```scheme
(listof nat/c)      ; List of natural numbers
(vectorof string/c) ; Vector of strings
```

**Range contracts**:
```scheme
(between/c 0 100)   ; Numbers in [0, 100]
```

**Enumeration**:
```scheme
(one-of/c 'red 'green 'blue)  ; Must be one of these symbols
```

**Boolean combinators**:
```scheme
(and/c nat/c pos/c)           ; Both must hold
(or/c nat/c string/c)         ; At least one must hold
(not/c nat/c)                 ; Must NOT be a natural number
```

### 5.11 Category Theory Foundations

The Fold provides category-theoretic abstractions as first-class values, supporting compositional reasoning about functors and transformations.

#### 5.11.1 Functors

A functor F : C → D maps objects and morphisms while preserving identity and composition. In The Fold, functors over Scheme values are represented as records containing an `fmap` function:

```scheme
(define functor-list (make-functor map))
(define functor-maybe (make-functor maybe-fmap))

;; fmap : (a → b) → F a → F b
(fmap-with functor-list add1 '(1 2 3))  ; → (2 3 4)
```

#### 5.11.2 Natural Transformations

A **natural transformation** η : F ⟹ G between functors assigns to each type A a morphism η_A : F(A) → G(A) such that the naturality square commutes:

```
     F(A) ───η_A──→ G(A)
      │              │
    F(f)           G(f)
      │              │
      ↓              ↓
     F(B) ───η_B──→ G(B)
```

That is: `G(f) ∘ η_A = η_B ∘ F(f)` for all morphisms f : A → B.

**Definition**:
```scheme
(define nat-head
  (make-nat-transform
   'head
   functor-list      ; source: List
   functor-maybe     ; target: Maybe
   (lambda (xs)      ; component: [A] → Maybe A
     (if (null? xs) nothing (just (car xs))))))

(nat-apply nat-head '(1 2 3))  ; → (just 1)
(nat-apply nat-head '())       ; → nothing
```

**Vertical Composition** (η ∘ ε): Chain transformations F ⟹ G ⟹ H:
```scheme
(define η (nat-maybe-to-either 'empty))  ; Maybe ⟹ Either
(define composed (nat-compose η nat-head)) ; List ⟹ Maybe ⟹ Either
(nat-apply composed '(a b))  ; → (right a)
(nat-apply composed '())     ; → (left 'empty)
```

**Horizontal Composition** (η * ε): The Godement product composes transformations on composed functors. Given η : F ⟹ G and ε : H ⟹ K, produces (η * ε) : H∘F ⟹ K∘G.

**Whiskering**: Extend a transformation by composing with a functor:
- Right whiskering (η ◁ H): Precompose with H, producing F∘H ⟹ G∘H
- Left whiskering (H ▷ η): Postcompose with H, producing H∘F ⟹ H∘G

#### 5.11.3 Naturality Verification

The system can verify the naturality condition for specific test cases:

```scheme
;; Test: Maybe(f) ∘ head = head ∘ List(f)
(check-naturality nat-head add1 '(1 2 3))  ; → #t

;; Verify with multiple morphisms and values
(verify-naturality nat-head
  (list (cons add1 '(1 2 3))
        (cons symbol->string '(a b c))))  ; → #t
```

#### 5.11.4 Natural Isomorphisms

A natural isomorphism is a natural transformation where each component is invertible. This captures when two functors are "essentially the same":

```scheme
(define maybe≅either
  (make-nat-iso 'maybe≅either
    functor-maybe
    functor-either-unit
    (lambda (m) (if (just? m) (right (from-just m)) (left '())))
    (lambda (e) (if (right? e) (just (from-right e)) nothing))))

;; Round-trip is identity
(nat-apply (nat-compose (nat-iso-inverse maybe≅either)
                        (nat-iso-forward maybe≅either))
           (just 42))  ; → (just 42)
```

#### 5.11.5 Common Natural Transformations

| Transformation | Type | Description |
|---------------|------|-------------|
| `nat-head` | List ⟹ Maybe | First element or nothing |
| `nat-singleton` | Maybe ⟹ List | Wrap in singleton or empty |
| `nat-concat` | List∘List ⟹ List | Flatten (monad join) |
| `nat-pure-list` | Id ⟹ List | Wrap in singleton (monad unit) |
| `nat-pure-maybe` | Id ⟹ Maybe | Wrap in Just |
| `nat-either-to-maybe` | Either ⟹ Maybe | Forget error |
| `nat-maybe-to-either` | Maybe ⟹ Either | Add default error |

These transformations satisfy the naturality condition and compose correctly, enabling equational reasoning about data flow through functor pipelines.

### 5.12 Adjoint Functors

An **adjunction** F ⊣ G between categories C and D is the fundamental structure-preserving relationship in category theory—more fundamental than equivalence and more general than isomorphism. It consists of:

- **Left adjoint** F : C → D
- **Right adjoint** G : D → C
- **Unit** η : Id_C ⟹ G∘F (a natural transformation)
- **Counit** ε : F∘G ⟹ Id_D (a natural transformation)

```scheme
(define adj-free-list
  (make-adjunction
   'free-list
   functor-list    ; Left: Free (lifts to list)
   functor-id      ; Right: Forgetful (underlying set)
   nat-pure-list   ; Unit: η (singleton wrapping)
   nat-concat))    ; Counit: ε (flatten/join)
```

#### 5.12.1 Triangle Identities

The unit and counit must satisfy the **triangle identities** (also called zig-zag laws), which ensure coherence:

```
         η_F              F▷η            ε◁F
    F ═══════════▶ GFG ═════════▶ GFG ═══════════▶ F
                        should equal id_F

         η◁G              G▷ε
    G ═══════════▶ GFG ═══════════▶ G
                  should equal id_G
```

Formally:
- **Left triangle**: (ε ◁ F) ∘ (F ▷ η) = id_F
- **Right triangle**: (G ▷ ε) ∘ (η ◁ G) = id_G

Where ▷ denotes left whiskering and ◁ denotes right whiskering.

```scheme
;; Verify triangle identities for specific test values
(verify-triangle-left adj-free-list '(1 2 3))   ; → #t
(verify-triangle-right adj-free-list '(a b c))  ; → #t

;; Combined verification
(verify-adjunction adj-free-list '(1 2 3) '(a b c))  ; → #t
```

#### 5.12.2 Hom-Set Bijection

An equivalent characterization: an adjunction F ⊣ G yields a natural isomorphism between hom-sets:

```
Hom_D(F(A), B) ≅ Hom_C(A, G(B))
```

This bijection is implemented by the **transpose** operations:

```scheme
;; Left transpose: (F(A) → B) → (A → G(B))
;; Given f : F(A) → B, produce G(f) ∘ η_A
(define g (adjunction-transpose-left adj f))

;; Right transpose: (A → G(B)) → (F(A) → B)
;; Given g : A → G(B), produce ε_B ∘ F(g)
(define f (adjunction-transpose-right adj g))
```

Example with the free monoid adjunction:

```scheme
;; f: List(Int) → Int (sum the list)
(define f (lambda (xs) (apply + xs)))

;; Transpose to g: Int → Int
;; g(x) = f([x]) = x
(define g (adjunction-transpose-left adj-free-list f))
(g 10)  ; → 10

;; Round-trip: transpose-right(transpose-left(f)) = f
```

#### 5.12.3 Common Adjunctions

| Adjunction | Left (F) | Right (G) | Description |
|------------|----------|-----------|-------------|
| `adj-free-list` | List | Id | Free monoid: singleton ⊣ underlying set |

The **free-forgetful** pattern is ubiquitous:
- Free monoid: F(S) = lists over S, G forgets the monoid structure
- Free group: F(S) = group generated by S, G forgets group structure
- Free vector space: F(S) = formal linear combinations over S

```scheme
;; Free monoid adjunction
;; η_A : A → List(A)  wraps in singleton (unit/pure)
;; ε_B : List(List(B)) → List(B)  flattens (join/concat)

(nat-apply nat-pure-list 42)           ; → (42)
(nat-apply nat-concat '((1 2) (3 4)))  ; → (1 2 3 4)
```

#### 5.12.4 Adjunction Composition

Adjunctions compose: given F ⊣ G and F' ⊣ G', we get (F' ∘ F) ⊣ (G ∘ G'):

```scheme
(define adj-id
  (make-adjunction 'id functor-id functor-id
                   (nat-id functor-id)
                   (nat-id functor-id)))

(define composed (adjunction-compose adj-free-list adj-id))
(adjunction-name composed)  ; → 'free-list∘id
```

The composed unit and counit are constructed using whiskering and natural transformation composition.

#### 5.12.5 Galois Connections

A **Galois connection** is an adjunction between preorders (categories where hom-sets have at most one element). Given posets (P, ≤) and (Q, ≤), a Galois connection consists of monotone functions:

- Lower adjoint f : P → Q
- Upper adjoint g : Q → P

Such that: f(p) ≤ q ⟺ p ≤ g(q)

```scheme
(define galois-floor-ceil
  (make-galois 'ceil-inclusion ceiling (lambda (x) x)))

;; Closure operator: g ∘ f (always ≥ original)
(galois-closure galois-floor-ceil 3.1)  ; → 4.0

;; Kernel operator: f ∘ g
(galois-kernel galois-floor-ceil 5)     ; → 5
```

**Properties of Galois closures**:
- **Extensive**: x ≤ closure(x)
- **Monotone**: x ≤ y implies closure(x) ≤ closure(y)
- **Idempotent**: closure(closure(x)) = closure(x)

Examples of Galois connections:
- Ceiling ⊣ Inclusion (ℤ ⊆ ℝ)
- Interior ⊣ Closure (topology)
- Abstraction ⊣ Concretization (abstract interpretation)

### 5.13 Comonads

A **comonad** is the categorical dual of a monad. Where monads encode effects and sequencing, comonads encode contexts and decomposition. A comonad W on a category C consists of:

- A functor W : C → C
- **Extract** ε : W ⟹ Id (counit, dual of return)
- **Duplicate** δ : W ⟹ W∘W (comultiplication, dual of join)

Or equivalently, via **extend**:
- **Extend** : (W A → B) → W A → W B

These satisfy the dual of monad laws:

```
Law 1: extend extract = id
Law 2: extract ∘ extend f = f
Law 3: extend f ∘ extend g = extend (f ∘ extend g)
```

#### 5.13.1 Comonad Type Class

```scheme
(define (make-comonad functor extract-fn extend-fn)
  (list 'comonad functor extract-fn extend-fn))

;; Extract a value from context
(define (extract-with comonad wa)
  ((comonad-extract comonad) wa))

;; Extend a function over all positions
(define (extend-with comonad f wa)
  ((comonad-extend comonad) f wa))

;; Derive duplicate from extend
(define (duplicate-with comonad wa)
  (extend-with comonad (lambda (x) x) wa))
```

#### 5.13.2 Store Comonad

The **Store comonad** `Store S A = (S → A) × S` represents a position in a space with the ability to access any other position. It's ideal for:
- Cellular automata (each cell can see neighbors)
- Zippers and cursors
- Image processing (convolution kernels)

```scheme
(define (make-store accessor position)
  (list 'store accessor position))

;; Peek at another position
(define (store-peek store pos)
  ((store-accessor store) pos))

;; Move to a new position
(define (store-seek store new-pos)
  (make-store (store-accessor store) new-pos))

;; Extract: get value at current position
(define (store-extract store)
  ((store-accessor store) (store-position store)))

;; Extend: apply function at every position
(define (store-extend f store)
  (make-store
   (lambda (pos) (f (store-seek store pos)))
   (store-position store)))
```

**Example: Cellular Automaton Rule**

```scheme
;; Count live neighbors and apply rule
(define (rule store)
  (let* ([pos (store-position store)]
         [left (store-peek store (- pos 1))]
         [right (store-peek store (+ pos 1))]
         [neighbors (+ left right)])
    (if (= neighbors 1) 1 0)))  ; Rule 30-ish

;; One generation step
(define (step world)
  (store-extend rule world))
```

#### 5.13.3 Env Comonad

The **Env comonad** `Env E A = (E, A)` represents a value with an immutable environment (dual of Reader monad):

```scheme
(define (make-env environment value)
  (list 'env environment value))

(define (env-extract env) (env-value env))

(define (env-extend f env)
  (make-env (env-environment env) (f env)))

;; Access environment without modifying
(define (env-ask env) (env-environment env))

;; Transform environment locally
(define (env-local f env)
  (make-env (f (env-environment env)) (env-value env)))
```

#### 5.13.4 Traced Comonad

The **Traced comonad** `Traced M A = M → A` for monoid M represents a computation that depends on an accumulated monoidal context:

```scheme
(define (make-traced run-fn monoid)
  (list 'traced run-fn monoid))

(define (traced-extract traced)
  ((traced-run traced) (monoid-identity (traced-monoid traced))))

(define (traced-extend f traced)
  (let ([monoid (traced-monoid traced)])
    (make-traced
     (lambda (m)
       (f (make-traced
           (lambda (m2)
             ((traced-run traced)
              ((monoid-op monoid) m m2)))
           monoid)))
     monoid)))
```

#### 5.13.5 Comonad from Adjunction

Every adjunction F ⊣ G yields a comonad on the domain category via F∘G:

```scheme
;; Given F ⊣ G with unit η and counit ε:
;; W = F ∘ G
;; extract = ε (counit)
;; duplicate = F(η_G) (whisker unit through F)

(define (comonad-from-adjunction adj)
  (let* ([F (adjunction-left adj)]
         [G (adjunction-right adj)]
         [η (adjunction-unit adj)]
         [ε (adjunction-counit adj)])
    (make-comonad
     (functor-compose F G)
     (nat-transform-component ε)
     (lambda (f wa)
       ;; extend f = fmap f ∘ duplicate
       ;; = F∘G(f) ∘ F(η_G)
       ((functor-fmap F)
        (lambda (ga) (f ((functor-fmap F) (nat-transform-component η) ga)))
        wa)))))
```

The **Store comonad** arises from the product-exponential adjunction (−)×S ⊣ (−)^S.

#### 5.13.6 Comonad Composition

Unlike monads, **comonads always compose**. Given comonads W₁ and W₂:

```scheme
(define (compose-comonads w1 w2)
  (make-comonad
   (functor-compose (comonad-functor w1) (comonad-functor w2))
   (lambda (w1w2a)
     ;; extract₁ ∘ fmap₁(extract₂)
     (extract-with w1 ((functor-fmap (comonad-functor w1))
                       (lambda (w2a) (extract-with w2 w2a))
                       w1w2a)))
   (lambda (f w1w2a)
     ;; extend using both comonads
     (extend-with w1
       (lambda (w1x)
         ((functor-fmap (comonad-functor w1))
          (lambda (w2a) (extend-with w2 (lambda (w2y) (f ???)) w2a))
          w1x))
       w1w2a))))
```

This is because the extract/duplicate operations distribute coherently without needing a distributive law (which monads require).

### 5.14 Monad Derivation from Adjunctions

Every adjunction F ⊣ G gives rise to a monad on the codomain of G (= domain of F). This provides a principled, unified derivation of all standard monads.

#### 5.14.1 The Derivation

Given adjunction F ⊣ G with unit η and counit ε:

```
Monad M = G ∘ F

return : A → M A = η_A (unit)
join : M(M A) → M A = G(ε_{F(A)}) (apply counit under G)
```

In code:

```scheme
(define (monad-from-adjunction adj)
  (let* ([F (adjunction-left adj)]
         [G (adjunction-right adj)]
         [η (adjunction-unit adj)]
         [ε (adjunction-counit adj)]
         [M (functor-compose G F)])
    (make-monad-ops
     (string->symbol (format "monad-~a" (adjunction-name adj)))
     (nat-transform-component η)              ; return = η
     (functor-fmap M)                          ; fmap from composed functor
     (lambda (mma)                             ; join = G(ε_F)
       ((functor-fmap G)
        (nat-transform-component ε)
        mma))
     (lambda (ma f)                            ; bind via join and fmap
       (join (fmap f ma))))))
```

#### 5.14.2 Example: List Monad from Free Monoid

The List monad arises from the free-forgetful adjunction for monoids:

```scheme
;; F: Set → Mon (free monoid = lists)
;; G: Mon → Set (forget monoid structure)
;; F ⊣ G

(define adj-free-list
  (make-adjunction
   'free-list
   functor-list     ; F: wraps in lists
   functor-id       ; G: identity (forgets structure)
   nat-pure-list    ; η: singleton wrapping
   nat-concat))     ; ε: concatenation (join)

(define monad-list-derived
  (monad-from-adjunction adj-free-list))

;; Verify it works
(define return (monad-ops-return monad-list-derived))
(define bind (monad-ops-bind monad-list-derived))

(return 42)  ; → '(42)
(bind '(1 2 3) (lambda (x) (list x x)))  ; → '(1 1 2 2 3 3)
```

#### 5.14.3 Example: State Monad

The State monad arises from the product-exponential adjunction:

```scheme
;; For fixed state type S:
;; F(A) = A × S  (product functor)
;; G(B) = S → B  (exponential functor)
;; F ⊣ G (currying adjunction)

(define (make-state-adjunction state-type)
  (make-adjunction
   'state
   (make-product-functor state-type)
   (make-exponential-functor state-type)
   (make-state-unit)
   (make-state-counit)))

;; Derived: State S A = S → (A × S)
;; return a = λs. (a, s)
;; m >>= f = λs. let (a, s') = m s in f a s'
```

#### 5.14.4 MonadOps Record

Derived monads are packaged in a record containing all operations:

```scheme
(define-record-type monad-ops
  (fields name return fmap join bind))

;; Usage
(monad-ops-name monad-list-derived)    ; → 'monad-free-list
(monad-ops-return monad-list-derived)  ; → singleton procedure
(monad-ops-bind monad-list-derived)    ; → concatMap procedure
```

#### 5.14.5 Law Verification

The derivation automatically satisfies monad laws (by the triangle identities of the adjunction). Verification functions confirm this:

```scheme
;; Left identity: return a >>= f = f a
(verify-left-identity monad-list-derived 5 (lambda (x) (list x x)))  ; → #t

;; Right identity: m >>= return = m
(verify-right-identity monad-list-derived '(1 2 3))  ; → #t

;; Associativity: (m >>= f) >>= g = m >>= (λx. f x >>= g)
(verify-associativity monad-list-derived '(1 2)
  (lambda (x) (list x (+ x 1)))
  (lambda (y) (list y y)))  ; → #t

;; All laws at once
(verify-monad-laws monad-list-derived ...)  ; → #t
```

### 5.15 Kan Extensions and the Codensity Monad

**Kan extensions** are "the most universal construction" in category theory—every other concept (limits, colimits, adjunctions, ends) can be expressed as a Kan extension.

#### 5.15.1 Right Kan Extension

The **Right Kan extension** of F : C → E along K : C → D is a functor Ran_K F : D → E together with a universal natural transformation:

```
(Ran_K F) A = ∀B. (A → K B) → F B
```

Universal property: Any natural transformation G∘K ⟹ F factors uniquely through Ran_K F.

```scheme
(define (make-ran k f computation)
  (list 'ran k f computation))

;; Apply the Ran to a K-morphism
(define (ran-apply ran k-morphism)
  ((ran-computation ran) k-morphism))

;; Ran is a functor
(define (ran-fmap f ran)
  (make-ran (ran-k ran) (ran-f ran)
    (lambda (k)
      ((ran-computation ran) (compose k f)))))
```

#### 5.15.2 Left Kan Extension

The **Left Kan extension** is dual:

```
(Lan_K F) A = ∃B. (K B → A, F B)
```

It's a coend: Lan_K F = ∫^B (K B → A) ⊗ F B

```scheme
(define (make-lan k f morphism value)
  (list 'lan k f morphism value))

;; Inject a value into Lan
(define (lan-inject k f fb morphism)
  (make-lan k f morphism fb))

;; Lan is a functor
(define (lan-fmap f lan)
  (make-lan (lan-k lan) (lan-f lan)
    (compose f (lan-morphism lan))
    (lan-value lan)))
```

#### 5.15.3 The Codensity Monad

The **Codensity monad** is the Right Kan extension of a monad M along itself:

```
Codensity M A = Ran_M M A = ∀R. (A → M R) → M R
```

This is exactly **continuation-passing style** made categorical. The key insight:

```
Standard bind: m >>= f     Rebuilds structure each time → O(n²) for left-nested binds
Codensity bind: ca >>= f   Composes continuations → O(1) per bind, O(n) at lower
```

```scheme
(define (codensity-return m-return a)
  (make-codensity m-return (lambda (k) (k a))))

(define (codensity-bind ca f)
  (make-codensity
   (codensity-return-fn ca)
   (lambda (k)
     ((codensity-run ca)
      (lambda (a) ((codensity-run (f a)) k))))))

;; Lower back to base monad
(define (codensity-lower ca)
  ((codensity-run ca) (codensity-return-fn ca)))

;; Lift from base monad
(define (codensity-lift m-return m-bind ma)
  (make-codensity m-return (lambda (k) (m-bind ma k))))
```

#### 5.15.4 Connection to The Fold's Effect System

The Codensity monad explains the O(1) bind optimization used in `lattice/fp/free.ss` and `lattice/fp/effects.ss`:

```scheme
;; In free.ss, the 'free-queue variant:
('free-queue base-free fmap continuation-queue)

;; In effects.ss, the 'eff-queue variant:
('eff-queue base-eff continuation-queue)
```

These **are Codensity monad implementations**. The queue represents accumulated continuations `(A → M R)` waiting to be applied. Instead of nested lambda closures, the queue defunctionalizes the continuation:

```
Naive bind chain: ((((m >>= f) >>= g) >>= h) >>= i)
  Each >>= traverses the accumulated structure → O(n²)

Codensity/queue: m with conts = [f, g, h, i]
  Each >>= just appends to queue → O(1)
  Final lower applies all at once → O(n)
```

#### 5.15.5 Difference Lists via Codensity

The classic "difference list" pattern is Codensity applied to the List monad:

```scheme
;; Normal list append: [1,2] ++ [3,4]
;; Must traverse [1,2] to find end → O(n)

;; Codensity List: λxs. 1:2:xs
;; Composition is function composition → O(1)

(define (codensity-list-singleton x)
  (codensity-return list x))

(define (codensity-list-append c1 c2)
  (codensity-bind c1
    (lambda (x)
      (codensity-bind c2
        (lambda (y)
          (codensity-return list (cons x y)))))))

;; Lower to regular list
(define (codensity-list-lower c)
  (codensity-lower c))
```

This transforms O(n²) left-associative appends into O(n):

```scheme
;; Left-associative: (((a ++ b) ++ c) ++ d)
;;   Step 1: traverse a          → O(|a|)
;;   Step 2: traverse a ++ b     → O(|a| + |b|)
;;   Step 3: traverse all so far → O(|a| + |b| + |c|)
;;   Total: O(n²) where n = total elements

;; Codensity: lower (c_a ∘ c_b ∘ c_c ∘ c_d)
;;   All compositions: O(1) each → O(n) total
;;   Final lower: single traversal → O(n)
;;   Total: O(n)
```

#### 5.15.6 Generic Codensity Monad Builder

A utility constructs Codensity for any monad:

```scheme
(define (make-codensity-monad m-return m-bind)
  (list
   (lambda (a) (codensity-return m-return a))           ; return
   (lambda (ca f) (codensity-bind ca f))                ; bind
   (lambda (ma) (codensity-lift m-return m-bind ma))    ; lift
   (lambda (ca) (codensity-lower ca))))                 ; lower
```

**Usage pattern**:

```scheme
;; Build optimized Maybe monad
(define codensity-maybe (make-codensity-monad just maybe-bind))
(define cm-return (car codensity-maybe))
(define cm-bind (cadr codensity-maybe))
(define cm-lower (cadddr codensity-maybe))

;; Use Codensity for computation
(define result
  (cm-lower
    (cm-bind (cm-return 5)
      (lambda (x)
        (cm-bind (cm-return (* x 2))
          (lambda (y)
            (cm-return (+ y 1))))))))
;; → (just 11), but with O(1) binds
```

---
