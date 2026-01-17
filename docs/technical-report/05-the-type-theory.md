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

---
