## 4. The Block Calculus


This section formalizes the computational model underlying The Fold.

### 4.1 Syntax

The core calculus is a lambda calculus extended with blocks and primitives:

```
e ::= x                          ; Variable
    | (λ x. e)                   ; Abstraction
    | (e₁ e₂)                    ; Application
    | (let x = e₁ in e₂)         ; Let binding
    | (fix x. e)                 ; Recursive binding
    | c                          ; Constant (numbers, strings, ...)
    | (prim op e₁ ... eₙ)        ; Primitive operation
    | (make-block tag payload refs)  ; Block construction
    | (block-tag e)              ; Block tag accessor
    | (block-payload e)          ; Block payload accessor
    | (block-refs e)             ; Block refs accessor
    | (quote e)                  ; Quotation (code as data)
    | (eval e)                   ; Evaluation (data as code)
```

**Normalized syntax** (after de Bruijn transformation):

```
e ::= (dv n)                     ; De Bruijn variable (index n)
    | (λ e)                      ; Abstraction (no binder name)
    | (e₁ e₂)                    ; Application
    | (let e₁ e₂)                ; Let (no binder name)
    | (fix e)                    ; Fix (no binder name)
    | c | (prim op e*) | ...     ; As above
```

### 4.2 Operational Semantics

We define a small-step reduction relation `e →ᶠ e'` parameterized by fuel `f`:

**Values**:
```
v ::= (λ x. e)           ; Abstractions
    | c                  ; Constants
    | (block t p r)      ; Fully evaluated blocks
```

**Reduction Rules** (selected):

```
                              f > 0
─────────────────────────────────────────────────  [β-reduce]
((λ x. e) v) →ᶠ e[v/x]     (fuel: f - 1)


                e₁ →ᶠ e₁'
────────────────────────────────────────────────  [app-left]
(e₁ e₂) →ᶠ (e₁' e₂)


               e₂ →ᶠ e₂'        v₁ is a value
────────────────────────────────────────────────  [app-right]
(v₁ e₂) →ᶠ (v₁ e₂')


                              f > 0
─────────────────────────────────────────────────  [let]
(let x = v in e) →ᶠ e[v/x]     (fuel: f - 1)


                              f > 0
─────────────────────────────────────────────────  [fix]
(fix x. e) →ᶠ e[(fix x. e)/x]     (fuel: f - 1)


                              f = 0
─────────────────────────────────────────────────  [out-of-fuel]
e →⁰ (error 'out-of-fuel)
```

**Fuel Semantics**:
- Each reduction step consumes fuel
- When fuel reaches 0, evaluation stops with `out-of-fuel`
- This guarantees termination: every evaluation completes in bounded steps

### 4.3 Call-by-Value Evaluation

The Fold uses *call-by-value* (strict) evaluation:

1. Arguments are evaluated before function application
2. Let bindings evaluate the bound expression before the body
3. No lazy evaluation or thunks in Core

**Rationale**: Call-by-value interacts predictably with effects (even though Core is pure, Boundary is not) and simplifies reasoning about resource consumption.

### 4.4 The Homoiconic Mechanism

Homoiconicity means code can be manipulated as data. In The Fold:

**Quotation**: `(quote e)` suspends evaluation, yielding `e` as a data structure:

```scheme
(quote (+ 1 2))     ; → the list '(+ 1 2), not 3
(quote (λ x. x))    ; → the list '(λ x. x)
```

**Evaluation**: `(eval e)` interprets data as code:

```scheme
(eval '(+ 1 2))     ; → 3
(eval (quote (λ x. x)))  ; → the identity function
```

**Code↔Block Bijection**:

```scheme
;; S-expression → Block
(define (sexpr->block tag sexpr)
  (make-block tag (string->utf8 (format "~s" sexpr)) #()))

;; Block → S-expression
(define (block->sexpr blk)
  (read (open-string-input-port
          (utf8->string (block-payload blk)))))
```

This bijection enables:
- Storing code in the CAS
- Content-addressing programs
- Metaprogramming via block manipulation
- Serialization/deserialization of any value

### 4.5 Effects and the Boundary

Core is *effect-free*. The Boundary provides effects through a capability-based system:

**Capability Types**:
```scheme
(Cap FS T)    ; Filesystem capability producing T
(Cap Net T)   ; Network capability producing T
(Cap Time T)  ; Time/randomness capability producing T
```

**Effect Boundary**: A capability is a token authorizing specific operations. The Boundary mints capabilities; Core code that needs effects must receive them as arguments:

```scheme
;; Boundary mints a filesystem capability
(define fs-cap (mint-capability 'filesystem))

;; Core function requires capability as argument
(define (read-file cap path)
  (with-capability cap
    (boundary-read-file path)))
```

**Monadic IO**: The FP toolkit (`lattice/fp/control/`) provides monadic abstractions:

```scheme
(>>= (read-line fs-cap)
     (lambda (line)
       (>>= (write-line fs-cap (string-upcase line))
            (lambda (_) (pure 'done)))))
```

This keeps Core pure while enabling practical programs.

### 4.6 Boundary Implementation Details

The Boundary ("thimble") is the verification boundary—code below is trusted, code above is verified. This section details Boundary's invariants and implementation.

#### 4.6.1 Boundary Invariants

The Boundary maintains these invariants before invoking Core:

**I1. Well-formed S-expressions**: All input is syntactically valid. Malformed UTF-8, unbalanced parentheses, and invalid tokens are rejected before reaching Core.

```scheme
;; Boundary validation pipeline
(define (validate-input raw-bytes)
  (let ([utf8-result (validate-utf8 raw-bytes)])
    (if (err? utf8-result)
        (error 'invalid-utf8 (err-msg utf8-result))
        (let ([sexpr-result (try-read (utf8->string raw-bytes))])
          (if (err? sexpr-result)
              (error 'malformed-sexpr (err-msg sexpr-result))
              (ok-val sexpr-result))))))
```

**I2. Type-compatible arguments**: Values passed to typed Core functions satisfy their declared types. Boundary performs runtime type checks at the interface.

```scheme
;; Boundary check before Core call
(define (call-core-function f args expected-types)
  (for-each
    (lambda (arg type)
      (unless (runtime-type-check arg type)
        (error 'type-mismatch arg type)))
    args expected-types)
  (apply f args))
```

**I3. Capability presence**: Effectful operations receive valid capability tokens. No capability = no effect.

**I4. Fuel budget**: Every Core invocation receives a finite fuel budget. Boundary chooses the budget based on operation type and user configuration.

#### 4.6.2 Capability Implementation

Capabilities are unforgeable tokens authorizing specific effects. Implementation:

```scheme
;; Capability is a record with a unique, unguessable ID
(define-record-type capability
  (fields
    id          ; Cryptographically random 128-bit identifier
    kind        ; Symbol: 'filesystem, 'network, 'time, etc.
    scope       ; Restrictions: paths, hosts, etc.
    revoked?))  ; Mutable: can be revoked

;; Capability minting (Boundary only)
(define (mint-capability kind scope)
  (make-capability
    (crypto-random-bytes 16)
    kind
    scope
    #f))

;; Capability checking
(define (check-capability cap required-kind operation)
  (cond
    [(capability-revoked? cap)
     (error 'revoked-capability cap)]
    [(not (eq? (capability-kind cap) required-kind))
     (error 'wrong-capability-kind required-kind (capability-kind cap))]
    [(not (scope-permits? (capability-scope cap) operation))
     (error 'scope-violation operation (capability-scope cap))]
    [else #t]))

;; Usage in Boundary
(define (read-file cap path)
  (check-capability cap 'filesystem `(read ,path))
  (call-with-input-file path get-string-all))
```

**Capability hierarchy**:
```
(Cap-Root)                    ; Superuser, mints other capabilities
├── (Cap-FS scope)            ; Filesystem (scope: paths)
├── (Cap-Net scope)           ; Network (scope: hosts/ports)
├── (Cap-Time)                ; Current time, sleep
├── (Cap-Random)              ; Cryptographic randomness
└── (Cap-Subprocess scope)    ; Spawn processes (scope: allowed commands)
```

**Capability attenuation**: Capabilities can be narrowed but not widened:

```scheme
;; Attenuate filesystem cap to single directory
(define (attenuate-fs-cap cap allowed-path)
  (unless (path-prefix? allowed-path (capability-scope cap))
    (error 'cannot-widen-capability))
  (make-capability
    (crypto-random-bytes 16)  ; New ID
    'filesystem
    allowed-path              ; Narrower scope
    #f))
```

#### 4.6.3 Error Handling

Boundary catches all errors from Core and presents them to users:

```scheme
(define (boundary-eval expr fuel)
  (guard (exn
          [(out-of-fuel? exn)
           (format-error "Computation exceeded fuel budget (~a)"
                        (out-of-fuel-consumed exn))]
          [(type-error? exn)
           (format-type-error exn)]
          [(eval-error? exn)
           (format-eval-error exn)]
          [else
           (format-error "Internal error: ~a" exn)])
    (core-eval expr fuel)))
```

**Error categories**:

| Category | Source | User Message |
|----|----|----|
| `parse-error` | Boundary | "Syntax error at line N: ..." |
| `type-error` | Core | "Type mismatch: expected T₁, got T₂" |
| `out-of-fuel` | Core | "Computation exceeded budget" |
| `unbound-var` | Core | "Undefined variable: x" |
| `capability-error` | Boundary | "Operation requires capability C" |
| `io-error` | Boundary | "Cannot read file: ..." |

#### 4.6.4 Boundary/Core Protocol

Communication follows a strict protocol:

```
Boundary                        Core
  │                               │
  ├─── validate(input) ──────────►│
  │                               │
  │◄── ok | parse-error ──────────┤
  │                               │
  ├─── infer-type(expr) ─────────►│
  │                               │
  │◄── type | type-error ─────────┤
  │                               │
  ├─── eval(expr, fuel, caps) ───►│
  │                               │
  │◄── value | error ─────────────┤
  │                               │
```

Core never initiates communication. Core never performs IO directly. All external interaction flows through Boundary.

### 4.7 Metaprogramming and the Type System

The homoiconic mechanism (`quote`/`eval`) operates outside the type system. This section clarifies the interaction.

#### 4.7.1 Quotation is Untyped

`quote` produces an S-expression value, not a typed term:

```scheme
(quote (+ 1 2))        ; → '(+ 1 2), type: Sexpr
(quote (lambda (x) x)) ; → '(lambda (x) x), type: Sexpr
```

The type of `quote` is:
```
quote : (→ <syntax> Sexpr)
```

Where `<syntax>` is the syntactic category of expressions, not a type. This is a *macro* operation, not a function.

#### 4.7.2 Evaluation is Dynamically Typed

`eval` interprets an S-expression as code:

```scheme
(eval '(+ 1 2))        ; → 3
(eval '(lambda (x) x)) ; → <procedure>
```

The type of `eval`:
```
eval : (→ Sexpr ?)
```

The result type is unknown statically. `eval` may:
- Return any type
- Fail with a type error at runtime
- Fail with a syntax error

#### 4.7.3 Safe Metaprogramming Patterns

**Pattern 1: Generate, then type-check**

```scheme
;; Generate code
(define generated-code
  `(define (add-n n)
     (lambda (x) (+ x ,n))))

;; Type-check before use
(define checked-code
  (type-check-sexpr generated-code))

;; Only use if well-typed
(when (ok? checked-code)
  (eval generated-code))
```

**Pattern 2: Typed wrappers**

```scheme
;; Wrap eval with expected type
(define (eval-expecting type sexpr)
  (let ([result (eval sexpr)])
    (if (runtime-type-check result type)
        (ok result)
        (err 'type-mismatch type result))))

;; Usage
(eval-expecting '(→ Int Int) '(lambda (x) (+ x 1)))
```

**Pattern 3: Quasiquotation with typed holes**

```scheme
;; Typed value spliced into untyped template
(define (make-adder [n : Int])
  (eval `(lambda (x) (+ x ,n))))
;; n is type-checked; the template is not
```

#### 4.7.4 Why Not Typed Quotation?

Typed quotation (as in MetaML) would give:
```
quote : (∀ (A) (→ A (Code A)))
eval  : (∀ (A) (→ (Code A) A))
```

Where `Code A` represents code that, when evaluated, produces type `A`.

We don't implement this because:
1. **Complexity**: Requires staging levels, environment classifiers
2. **Homoiconicity tension**: S-expressions don't carry types
3. **Practical sufficiency**: Untyped metaprogramming + runtime checks works for our use cases

**Future direction**: A typed quotation sublanguage for specific patterns (e.g., SQL query generation) may be added.

#### 4.7.5 Content Addressing of Generated Code

Generated code participates in content addressing:

```scheme
;; Two generators produce the same code
(define code1 (generate-identity 'x))  ; '(lambda (x) x)
(define code2 (generate-identity 'y))  ; '(lambda (y) y)

;; After normalization, same hash
(equal? (hash-sexpr code1) (hash-sexpr code2))  ; → #t
```

Even metaprogrammed code benefits from semantic identity.

---
