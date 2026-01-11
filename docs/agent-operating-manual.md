# Agent Operating Manual

This document provides algorithmic steps for agents operating within The Fold. No hand-waving. Each section is a concrete procedure.

---

## 1. Capability Discovery

### 1.1 Discovering Available Skills (Lattice Level)

**Prerequisite:** Initialize the knowledge graph once per session.

```scheme
(load "lattice/meta/meta.ss")
(lattice-init!)  ; Builds KG + search indices (~1400 exports)
```

**Algorithm: Find a capability by need**

```
PROCEDURE find-capability(need: String) -> List<Skill>
  1. Tokenize `need` into search terms
  2. Query BM25 index: (lf "need")
  3. Receive ranked list: ((skill-name . score) ...)
  4. For each result with score > 0.1:
     a. Inspect skill: (li 'skill-name)
     b. Check exports: (le 'skill-name)
     c. Check modules: (lm 'skill-name)
  5. Return matching skills with their exports
```

**Concrete example:**

```scheme
;; Need: matrix operations
(lf "matrix decomposition")
;; → ((linalg . 2.34) (autodiff . 0.89) ...)

(li 'linalg)
;; → Shows: tier 0, pure, stable, exports vec3/mat4/lu-decompose/...

(le 'linalg)
;; → ((vec "vec.ss" vec3 vec4 vec-add vec-dot ...)
;;    (mat "mat.ss" mat4 mat-mul lu-decompose ...))
```

### 1.2 Discovering Shell Capabilities

**Algorithm: Scan codebase for capability definitions**

```
PROCEDURE discover-shell-capabilities(paths: List<Path>) -> List<Capability>
  1. Initialize caps = []
  2. For each path in paths:
     a. Read all S-expressions from file
     b. For each expr:
        i.  If (define-record-type X-capability ...):
            Extract X, add to caps as {name: X, type: 'record}
        ii. If (define (mint-X-capability ...) ...):
            Extract X, add to caps as {name: X, type: 'mint}
        iii. If (define (make-X-capability ...) ...):
            Extract X, add to caps as {name: X, type: 'make}
  3. Return unique(caps)
```

**Detection patterns (from `shell/capability-lens.ss`):**

| Pattern | Extracts |
|---------|----------|
| `(define-record-type fs-capability ...)` | `"fs"` |
| `(define (mint-io-capability ...) ...)` | `"io"` |
| `(define (make-net-capability ...) ...)` | `"net"` |

**Shell entry point:**

```scheme
(load "shell/capability-lens.ss")
(define all-caps (discover-capabilities (fs) (glob "shell/**/*.ss")))
;; → ("fs" "io" "net" "time" "random" ...)
```

### 1.3 Checking What a Function Requires

**Algorithm: Analyze function dependencies**

```
PROCEDURE analyze-requirements(fn-name: Symbol, file: Path) -> Requirements
  1. Parse file into S-expressions
  2. Find definition of fn-name
  3. Walk the body, collecting:
     mints = []   ; Capabilities this function creates
     uses  = []   ; Capabilities this function consumes
  4. For each operator in body:
     a. If operator matches mint-X-capability → add X to mints
     b. If operator matches X-read, X-write, X-open → add X to uses
  5. requires = uses - mints  ; External dependencies
  6. Return {mints, uses, requires}
```

**Concrete usage:**

```scheme
(define analysis (analyze-file (fs) "shell/io/fs.ss" known-caps))
;; → ((file . "shell/io/fs.ss")
;;    (mints . (fs))
;;    (uses . (fs))
;;    (requires . ()))  ; fs.ss is self-contained
```

---

## 2. Trust and Verification

### 2.1 The Trust Model

```
TRUST HIERARCHY:

  Core (pure)     ← Trusts nothing, assumes perfect input
       ↑
  Shell (fallen)  ← Validates everything, mints capabilities
       ↑
  Outside         ← Untrusted by definition
```

**Axioms:**
1. Core code NEVER constructs capability records directly
2. Only Shell's `mint-X-capability` functions create capabilities
3. Capability records are **opaque** — Core cannot forge them
4. Shell validates ALL input before passing to Core

### 2.2 Verifying a Capability is Valid

**Algorithm: Capability verification**

```
PROCEDURE verify-capability(cap: Any, expected-type: Symbol) -> Boolean
  1. Check cap is a record: (record? cap)
  2. Check record type matches expected:
     (eq? (record-type-descriptor cap) expected-type-rtd)
  3. If capability has fields, verify field invariants:
     - fs-capability: (directory-exists? (fs-capability-store-path cap))
     - net-capability: (valid-endpoint? (net-capability-endpoint cap))
  4. Return #t if all checks pass, #f otherwise
```

**Why this works:**

```scheme
;; Only Shell can create these (Scheme record system enforces this)
(define-record-type fs-capability
  (fields store-path))

;; Core receives capability as opaque value
;; Cannot construct it, cannot inspect internals without accessor
;; Can only pass it to functions that accept fs-capability
```

### 2.3 Static Verification: Capability Audit

**Algorithm: Audit a module for capability safety**

```
PROCEDURE audit-module(path: Path) -> AuditResult
  1. caps = discover-capabilities(fs, [path])
  2. analysis = analyze-file(fs, path, caps)
  3. violations = []
  4. For each definition in analysis.definitions:
     a. If def.mints is non-empty AND path not in shell/:
        Add violation: "Core code minting capability"
     b. If def.requires has capability not in explicit-imports:
        Add violation: "Undeclared capability dependency"
  5. Return {analysis, violations, safe: violations.empty?}
```

**Running an audit:**

```scheme
(capability-report (fs) "shell")
;; Prints:
;; === Capability Report ===
;; Files analyzed: 47
;; Capabilities found: 12
;;
;; MINTING (only shell should do this):
;;   fs: shell/io/fs.ss
;;   net: shell/io/net.ss
;;   ...
;;
;; USAGE BY MODULE:
;;   shell/repl/repl-daemon.ss: uses (fs io)
;;   ...
```

---

## 3. Fuel Risk Prediction

### 3.1 The Fuel Model

```
FUEL RULES:
  - Each eval step costs 1 fuel
  - Primitives cost 0 fuel (add, mul, cons, car, cdr, ...)
  - When fuel = 0, evaluation suspends (not crashes)
  - Suspended computation can resume with fresh fuel
```

**Core implementation (from `core/lang/eval.ss`):**

```scheme
(define (eval-core expr env fuel ctx)
  (cond
   [(out-of-fuel? fuel)
    (make-suspended expr env fuel ctx)]  ; Suspend, don't crash
   [else
    (let ([remaining (- fuel 1)])
         ;; ... continue evaluation with remaining fuel
         )]))
```

### 3.2 Estimating Fuel Requirements

**Algorithm: Static fuel estimation**

```
PROCEDURE estimate-fuel(expr: S-expr) -> FuelEstimate
  1. count = 0
  2. Walk expr recursively:
     a. For each (fn ...) or (lambda ...): count += 1
     b. For each (let ...) binding: count += 1 per binding
     c. For each (if ...) or (cond ...): count += 1 + max(branches)
     d. For each function application: count += 1 + sum(arg-costs)
     e. For each (fix ...) recursive call: count += estimated-iterations
  3. Return {minimum: count, complexity: inferred-big-O}
```

**Complexity classes (from manifests):**

| Pattern | Fuel Bound | Example |
|---------|------------|---------|
| Simple accessor | O(1) | `(vec-x v)` |
| Linear traversal | O(n) | `(map f lst)` |
| Matrix multiply | O(n³) | `(mat-mul a b)` |
| Recursive descent | O(2^n) worst | Parser combinators |

### 3.3 Safe Fuel Budgeting

**Algorithm: Budget fuel for unknown computation**

```
PROCEDURE budget-fuel(expr: S-expr, max-allowed: Nat) -> Nat
  1. estimate = estimate-fuel(expr)
  2. If estimate.complexity is polynomial:
     budget = min(estimate.minimum * 10, max-allowed)
  3. Else if estimate.complexity is exponential:
     budget = min(1000, max-allowed)  ; Hard cap
  4. Return budget
```

**Runtime monitoring:**

```scheme
;; Evaluate with fuel budget
(define result (eval-core expr env 10000 (make-ctx)))

;; Check result
(cond
 [(ok? result)
  (printf "Success, fuel remaining: ~a~n" (ok-fuel result))]
 [(suspended? result)
  (printf "Suspended at: ~a~n" (suspended-expr result))
  ;; Can resume: (eval-core (suspended-expr result)
  ;;                        (suspended-env result)
  ;;                        fresh-fuel
  ;;                        (suspended-ctx result))
  ]
 [(error? result)
  (printf "Error: ~a~n" (error-message result))])
```

### 3.4 Parallel Fuel Hazards

**Warning:** Parallel evaluation (`par`) has different fuel semantics:

```
PARALLEL FUEL RULE:
  (par a b) evaluates:
    - a in background thread (fixed fuel, cannot suspend)
    - b in main thread (can suspend normally)

  If background thread exhausts fuel → ERROR (not suspension)
  Background computations must be bounded.
```

---

## 4. Shell Boundary Protocol

### 4.1 Request/Response Protocol

**File-based IPC layout:**

```
.fold-repl/
├── ready              # Exists when daemon is running
├── requests/
│   └── {session}.ss   # Request file (agent writes here)
├── responses/
│   └── {session}.txt  # Response file (daemon writes here)
├── daemon.log         # Daemon log
└── error.txt          # Error output
```

### 4.2 Requesting Evaluation

**Algorithm: Send request to Shell**

```
PROCEDURE shell-eval(session: String, expr: S-expr) -> Result
  1. Verify daemon running: (file-exists? ".fold-repl/ready")
  2. Clear stale response: (delete-file-if-exists response-path)
  3. Write request:
     (write-to-file request-path
       `((session-id . ,session)
         (expression . ,expr)
         (timestamp . ,(current-time))))
  4. Poll for response (100ms intervals, max 300 attempts):
     LOOP:
       If (file-exists? response-path):
         content = (read-file response-path)
         (delete-file response-path)
         Return (parse-response content)
       Else:
         (sleep 100ms)
  5. If timeout: Return (error 'timeout)
```

**Using fold-agent.py (recommended):**

```bash
# Simple evaluation
./fold-agent.py "(+ 1 2)"
# → {"status": "ok", "result": "3", "output": ""}

# With session
./fold-agent.py --session my-agent "(define x 10)"
# → {"status": "ok", "result": "", "output": ""}

# JSON input (for programmatic use)
echo '{"code": "(* x 2)", "session": "my-agent"}' | ./fold-agent.py --json
# → {"status": "ok", "result": "20", "output": ""}
```

### 4.3 Requesting a Capability

**Algorithm: Request capability from Shell**

```
PROCEDURE request-capability(cap-type: Symbol, params: Alist) -> Capability
  1. Construct request expression:
     expr = `(mint-{cap-type}-capability ,@params)
  2. Send to Shell: (shell-eval session expr)
  3. Receive capability handle (opaque to agent)
  4. Store handle for future use
  5. Return handle
```

**Example: Requesting filesystem capability**

```scheme
;; Agent sends:
(shell-eval "agent-1" '(mint-fs-capability ".store"))

;; Shell validates:
;;   - Is ".store" a valid path?
;;   - Does agent have permission?
;;   - Create capability record

;; Agent receives:
;;   #<fs-capability store-path=".store">
;;   (Opaque handle — agent cannot inspect internals)

;; Agent uses:
(shell-eval "agent-1" '(fs-write cap "data.txt" "hello"))
```

### 4.4 Validation at Boundary

**What Shell validates before passing to Core:**

| Check | Failure Mode |
|-------|--------------|
| S-expression parses | Syntax error |
| Types check (if typed mode) | Type error |
| Fuel budget reasonable | Budget error |
| Capabilities present | Missing capability |
| No forbidden operations | Security violation |

---

## 5. Recording Provenance

### 5.1 The Block Model

**Everything is a Block:**

```
Block = {
  tag:     Symbol      # Block type identifier
  payload: Bytevector  # Raw data (literals, encoded S-exprs)
  refs:    [Address]   # Ordered list of 33-byte addresses
}

Address = VersionByte (1) + SHA256Hash (32) = 33 bytes
```

### 5.2 Computing a Hash

**Algorithm: Hash an S-expression**

```
PROCEDURE hash-expr(expr: S-expr) -> Address
  1. Normalize expr (α-conversion to de Bruijn indices):
     normalized = (normalize expr)
  2. Serialize to canonical bytes:
     bytes = (expr->bytes normalized)
  3. Compute SHA-256:
     hash = (sha256 bytes)
  4. Prefix with version byte:
     address = [0x00] ++ hash
  5. Return address (33 bytes)
```

**Normalization examples:**

```scheme
;; These produce the SAME hash:
(normalize '(fn (x) x))       ;; → (fn (dv 0))
(normalize '(fn (y) y))       ;; → (fn (dv 0))

;; These produce DIFFERENT hashes:
(normalize '(fn (x) (fn (y) x)))  ;; → (fn (fn (dv 1)))
(normalize '(fn (x) (fn (y) y)))  ;; → (fn (fn (dv 0)))
```

### 5.3 Storing a Block

**Algorithm: Store with provenance**

```
PROCEDURE store-with-provenance(content: S-expr, refs: [Address]) -> Address
  1. Infer tag from content structure:
     tag = (infer-tag content)
  2. Serialize content to payload:
     payload = (expr->bytes content)
  3. Create block:
     block = (make-block tag payload refs)
  4. Compute address:
     address = (hash-block block)
  5. Store in CAS:
     (hashtable-set! *store* address block)
  6. Return address
```

**Concrete usage:**

```scheme
;; Store a simple value
(define hash1 (store! (make-block 'literal (string->utf8 "hello") #())))

;; Store something that references the first
(define hash2 (store! (make-block 'pair
                                  #vu8()
                                  (vector hash1 hash1))))

;; Retrieve
(fetch hash1)  ;; → #<block tag=literal payload="hello" refs=[]>
(fetch hash2)  ;; → #<block tag=pair payload=() refs=[hash1, hash1]>
```

### 5.4 Building with Provenance

**Algorithm: Build a derived value with full provenance**

```
PROCEDURE build-derived(inputs: [Address], transform: Procedure) -> Address
  1. Fetch all input blocks:
     input-blocks = (map fetch inputs)
  2. Extract values from blocks:
     input-values = (map block->value input-blocks)
  3. Apply transformation:
     result = (transform input-values)
  4. Create result block with refs to inputs:
     result-block = (make-block
                      (infer-tag result)
                      (value->bytes result)
                      (list->vector inputs))  ; Provenance!
  5. Store and return:
     Return (store! result-block)
```

**Example: Building a compound structure**

```scheme
;; Build a function definition with provenance
(define (store-function name params body)
  ;; Store each component
  (let* ([name-hash (store! (make-block 'symbol (symbol->utf8 name) #()))]
         [params-hash (store! (make-block 'list (params->bytes params) #()))]
         [body-hash (store! (make-block 'expr (expr->bytes body) #()))])
    ;; Store the function, referencing all components
    (store! (make-block 'function
                        #vu8()  ; No direct payload
                        (vector name-hash params-hash body-hash)))))

;; Later: retrieve with full provenance
(define fn-block (fetch fn-hash))
(block-refs fn-block)  ;; → #(name-hash params-hash body-hash)
```

### 5.5 Verifying Provenance

**Algorithm: Verify a block's integrity**

```
PROCEDURE verify-integrity(address: Address) -> Boolean
  1. Fetch block: block = (fetch address)
  2. If block is #f: Return #f (not found)
  3. Recompute hash: computed = (hash-block block)
  4. Compare: Return (bytevector=? address computed)
```

**Algorithm: Verify transitive provenance**

```
PROCEDURE verify-provenance-tree(address: Address) -> VerificationResult
  1. Initialize: visited = {}, queue = [address], errors = []
  2. While queue not empty:
     a. current = (pop queue)
     b. If current in visited: continue
     c. Add current to visited
     d. If not (verify-integrity current):
        Add to errors: {address: current, error: 'hash-mismatch}
     e. block = (fetch current)
     f. For each ref in (block-refs block):
        Add ref to queue
  3. Return {valid: errors.empty?, errors: errors, nodes-checked: |visited|}
```

### 5.6 Pinning for Persistence

**Algorithm: Pin a block tree**

```
PROCEDURE pin-tree(root: Address) -> Nat
  1. refs = (collect-all-refs root)  ; BFS traversal
  2. count = 0
  3. For each ref in refs:
     If not (pinned? ref):
       (pin! ref)
       count += 1
  4. Return count
```

**Why pinning matters:**

```
Without pinning:
  - Blocks may be garbage collected
  - Provenance chain can break

With pinning:
  - Block guaranteed to persist
  - Entire DAG preserved
  - Can always reconstruct derivation
```

---

## Quick Reference

### Discovery Commands

```scheme
(lf "query")           ; Search skills by text
(lfe 'symbol)          ; Find skill exporting symbol
(li 'skill)            ; Inspect skill details
(le 'skill)            ; List skill exports
(ld 'skill)            ; Dependencies of skill
(lu 'skill)            ; What uses this skill
```

### Verification Commands

```scheme
(capability-scan (fs) "path")     ; Scan for capabilities
(capability-report (fs) "path")   ; Print capability audit
(analyze-file (fs) "file" caps)   ; Analyze single file
```

### Fuel Commands

```scheme
(eval-core expr env fuel ctx)     ; Evaluate with fuel
(out-of-fuel? n)                  ; Check if exhausted
(suspended? result)               ; Check if suspended
(ok-fuel result)                  ; Remaining fuel from ok result
```

### Store Commands

```scheme
(store! block)                    ; Store, return hash
(fetch hash)                      ; Retrieve by hash
(stored? hash)                    ; Check existence
(hash-block block)                ; Compute hash without storing
(normalize expr)                  ; α-normalize expression
```

### Shell Commands

```scheme
(shell-eval session expr)         ; Evaluate via daemon
(mint-X-capability ...)           ; Request capability (Shell only)
```

---

## Invariants to Maintain

1. **Never construct capabilities in Core** — always request from Shell
2. **Always budget fuel** — unbounded computation will suspend
3. **Always record provenance** — store refs to inputs in output blocks
4. **Normalize before hashing** — ensures α-equivalent = same hash
5. **Pin important blocks** — prevents accidental garbage collection
6. **Validate at boundaries** — Shell checks everything, Core trusts input
