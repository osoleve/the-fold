# Agent Training Problems

Problem set for training models to navigate The Fold and use its tools effectively. Designed to resist brute-forcing in chain-of-thought — each problem requires genuinely running computation through the REPL.

---

## Design Principles

Problems that frontier models brute-force share a common trait: the answer is derivable from general knowledge or moderate arithmetic. Effective training problems need **computational irreducibility** — the answer genuinely requires running the computation.

Five properties that resist brute-forcing:

1. **Numerical precision beyond mental arithmetic** — exact rationals from matrix decompositions, floating-point results from AD at 6+ decimal places
2. **State-dependent answers** — queries against the CAS, BBS, or block store that have no "correct" answer without querying the live system
3. **Combinatorial search spaces** — SAT problems, graph coloring, where enumeration in CoT is infeasible
4. **Multi-step tool composition** — problems requiring loading multiple modules and chaining results
5. **Discovery as prerequisite** — the model must find the right capability before it can use it

Every problem has a **scale knob** — a parameter (matrix size, game depth, graph order, expression complexity) that can be turned up if models start brute-forcing smaller instances.

---

## Difficulty Tiers

### Tier 1: Tool Discovery + Single Module Use

These test: Can the model find the right tool and call it correctly?

#### P1 — Lattice Navigation

> What module in The Fold provides Cholesky decomposition? Load it and decompose the matrix `[[4, 12, -16], [12, 37, -43], [-16, -43, 98]]`. Return the lower-triangular factor L.

**Skills tested:** `(modules)`, `(module-exports 'name)`, `(require 'name)`, calling the function with correct arguments.

**Why it resists brute-force:** 3x3 Cholesky is possible mentally but tedious. Scale to 5x5 to make it impractical. The exact entries matter — no rounding.

**Scale knob:** Matrix size. At 5x5 with non-integer entries, mental computation is infeasible.

**Solution sketch:**
```scheme
(require 'matrix)
(require 'matrix-decomp)
(matrix-cholesky (matrix-from-lists '((4 12 -16) (12 37 -43) (-16 -43 98))))
```

**Expected output:**
```
((matrix 3 3 #(2 0 0 6 1 0 -8 5 3)))
```
L = `[[2, 0, 0], [6, 1, 0], [-8, 5, 3]]`. Verify: L·Lᵀ reconstructs the original matrix.

#### P2 — BBS State Query

> How many BBS issues are currently in 'open' status? What are the IDs and titles of all P2-priority ready issues?

**Skills tested:** `(require 'boundary/bbs)`, `(bbs-init!)`, `(bbs-list)`, `(bbs-ready)`, filtering output.

**Why it resists brute-force:** The answer is entirely runtime-dependent. No amount of reasoning derives it.

**Scale knob:** N/A — inherently state-dependent. Can vary the query: "which issues block fold-XXXX?", "how many issues were created in 2026?", etc.

**Solution sketch:**
```scheme
(require 'boundary/bbs)
(bbs-ready)  ; Filter output for P2 priority
```

**Expected output:** Runtime-dependent. The model's answer must match the live system state. Verify by running the same commands.

#### P3 — Normalization Phase Inspection

> Run `normalize-v3-phases` on `(fn (y) (+ (* 1 (+ y 0)) (- y y)))`. Report the output at each phase.

**Skills tested:** Loading the normalizer, calling `normalize-v3-phases`, reading structured output.

**Why it resists brute-force:** A model can simplify mentally (`(* 1 ...)` → identity, `(+ y 0)` → `y`, `(- y y)` → `0`, so `(+ y 0)` → `y`) but cannot predict the exact intermediate representations at each phase. The NbE phase may produce forms the model can't anticipate.

**Scale knob:** Expression complexity. Add nested lets, fix points, multiple identity operations.

**Solution sketch:**
```scheme
(load "core/blocks/normalize.ss")
(normalize-v3-phases '(fn (y) (+ (* 1 (+ y 0)) (- y y))))
```

**Expected output:**
```
((input      fn (y) (+ (* 1 (+ y 0)) (- y y)))
 (after-nbe  fn (x0) (+ (* 1 (+ x0 0)) (- x0 x0)))
 (after-algebraic  fn (x0) x0)
 (after-alpha      fn (dv 0))
 (final            fn (dv 0)))
```
The algebraic phase does all the heavy lifting: identity elimination, zero elimination, and the entire expression collapses to just the bound variable.

---

### Tier 2: Precise Numerical Results

These test: Can the model get exact answers that require computation?

#### P4 — Exact Rational Linear Algebra

> Compute the LU decomposition of `[[3, 1, 4], [1, 5, 9], [2, 6, 5]]`. Report the exact rational entries of L and U.

**Skills tested:** Module discovery, `matrix-from-lists`, `matrix-lu`, reading structured output with exact rationals.

**Why it resists brute-force:** 3x3 LU with pivoting produces rationals like `14/3` or `-47/14` that are essentially impossible to compute correctly by mental arithmetic. The specific entries are the answer.

**Scale knob:** Matrix size. At 4x4 and above, brute-forcing is out of reach.

**Solution sketch:**
```scheme
(require 'matrix)
(require 'matrix-decomp)
(matrix-lu (matrix-from-lists '((3 1 4) (1 5 9) (2 6 5))))
```

**Expected output:**
```
((matrix 3 3 #(1 0 0 2/3 1 0 1/3 7/8 1))
 (matrix 3 3 #(3 1 4 0 16/3 7/3 0 0 45/8))
 #(0 2 1))
```
L has rationals `2/3`, `1/3`, `7/8`. U has `16/3`, `7/3`, `45/8`. Permutation vector is `#(0 2 1)`.

#### P5 — Reverse-Mode AD on Composed Functions

> Using The Fold's reverse-mode AD, compute the gradient of `f(x,y,z) = sin(x·y) · exp(-z) + log(1 + x·z)` at the point (1.0, 2.0, 0.5). Report all three partial derivatives.

**Skills tested:** `(require 'reverse-diff)`, composing `traced-*` operations, calling `gradient`, interpreting result.

**Why it resists brute-force:** The chain rule involves sin, cos, exp, log composed with products. Getting numerical results right by mental computation is infeasible.

**Scale knob:** Function complexity and number of variables. Add more terms, deeper nesting, more transcendentals.

**Solution sketch:**
```scheme
(require 'reverse-diff)
(gradient
  (lambda (x y z)
    (traced-add
      (traced-mul (traced-sin (traced-mul x y))
                  (traced-exp (traced-neg z)))
      (traced-log (traced-add (make-traced-const 1 (traced-tape x))
                              (traced-mul x z)))))
  '(1.0 2.0 0.5))
```

**Expected output:**
```
(-0.17147829728319414 -0.2524058153082637 0.11514989849908586)
```
Verified against analytical partial derivatives. E.g., ∂f/∂x = cos(xy)·y·exp(-z) + z/(1+xz) = cos(2)·2·exp(-0.5) + 0.5/1.5 ≈ -0.1715.

**Note:** The model must figure out that Fold AD uses `traced-*` operations, not bare Scheme `+`/`*`. This is itself a discovery step. Also: use `gradient` (takes a function over a list), not `gradient-at` (has a wrapping bug with apply).

#### P6 — Physics Trajectory

> A rigid body starts at position (0, 50) with velocity (8, 0), mass 2.0, under gravity (0, -9.81). Using `integrate-rigid-body` with dt=0.05, at what simulation step does the y-coordinate first go negative? What is the exact position at that step?

**Skills tested:** `(require 'rigid-body)`, `(require 'vec2)`, simulation loop, reading results.

**Why it resists brute-force:** The analytical answer (continuous projectile: t ≈ 3.19s, step ~64) won't match Euler integration exactly. Accumulated numerical drift means the model must actually simulate to get the right step count and position.

**Scale knob:** Time step (smaller dt = more steps = harder to track mentally), initial conditions, addition of angular velocity.

**Solution sketch:**
```scheme
(require 'rigid-body)
(let loop ((b (make-rigid-body (vec2 0.0 50.0) (vec2 8.0 0.0)
                               0.0 0.0 2.0 1.0))
           (step 0))
  (if (< (vec2-y (rigid-body-pos b)) 0)
      (list step (rigid-body-pos b))
      (loop (integrate-rigid-body b (vec2 0.0 -9.81) 0.0 0.05)
            (+ step 1))))
```

**Expected output:**
```
(64 (vec2 25.599999999999973 -1.0120000000000489))
```
Step 64, position ≈ (25.6, -1.012). The analytical continuous-time answer is t ≈ 3.194s (step ~63.9), but Euler integration drift pushes the crossing to step 64. This is the point — the model can't shortcut the simulation.

---

### Tier 3: Combinatorial / Structural

These test: Can the model encode problems for solvers and interpret results?

#### P7 — SAT Graph Coloring

> Using The Fold's SAT solver, determine: is the Petersen graph 3-colorable? Vertices 0-9, edges: {0-1, 0-4, 0-5, 1-2, 1-6, 2-3, 2-7, 3-4, 3-8, 4-9, 5-7, 5-8, 6-8, 6-9, 7-9}. If satisfiable, return a valid coloring.

**Skills tested:** `(require 'sat)`, `graph-coloring-solve`, constructing edge lists, interpreting SAT models.

**Why it resists brute-force:** The Petersen graph has chromatic number 3 (3-colorable but not 2). Verifying this requires actual search over 3^10 = 59049 possible assignments.

**Scale knob:** Graph size and structure. Use larger graphs, ask about k-colorability for different k.

**Solution sketch:**
```scheme
(require 'sat)
;; Edges must be dotted pairs (car/cdr), not lists
(graph-coloring-solve
  '((0 . 1) (0 . 4) (0 . 5) (1 . 2) (1 . 6) (2 . 3) (2 . 7)
    (3 . 4) (3 . 8) (4 . 9) (5 . 7) (5 . 8) (6 . 8) (6 . 9) (7 . 9))
  10 3)
```

**Expected output:** A satisfying assignment (list of `(var . #t/#f)` pairs). The Petersen graph IS 3-colorable (chromatic number = 3). The specific assignment varies by solver run, but will always be satisfiable. To decode: variable `(node * 3 + color + 1)` represents "node has color". True variables indicate the assigned colors.

#### P8 — Custom Simplicial Complex Homology

> Construct a simplicial complex from:
> - Vertices: {0, 1, 2, 3, 4, 5}
> - Edges: {01, 12, 23, 34, 45, 50, 02, 24, 03}
> - Triangles: {012, 023, 024}
>
> Compute its Betti numbers over Z₂.

**Skills tested:** `(require 'simplicial-complex)`, `(require 'homology)`, constructing a custom complex, calling `sc-betti-numbers`.

**Why it resists brute-force:** This is a non-standard space. The Betti numbers depend on the specific topology — the model can't look up the answer because this complex isn't named. Computing boundary matrices and ranks over Z₂ by hand is painful.

**Scale knob:** Number of simplices. Add more vertices, edges, and faces to create complex topologies with non-trivial H₁ and H₂.

**Solution sketch:**
```scheme
(require 'simplicial-complex)
(require 'homology)
;; sc-from-simplices auto-adds all faces of each simplex.
;; Only specify maximal simplices + extra edges not covered by triangles.
(let ((sc (sc-from-simplices
            (list (triangle 0 1 2)
                  (triangle 0 2 3)
                  (triangle 0 2 4)
                  (edge 3 4) (edge 4 5) (edge 5 0) (edge 0 3)))))
  (list (sc-betti-numbers sc) (sc-f-vector sc) (sc-euler sc)))
```

**Expected output:**
```
((1 2 0) (6 10 3) -1)
```
Betti numbers (1, 2, 0): one connected component, two independent 1-cycles, no 2-holes. f-vector: 6 vertices, 10 edges, 3 triangles. Euler characteristic: 6 - 10 + 3 = -1, consistent with 1 - 2 + 0 = -1.

#### P9 — Extensive-Form Game Solving

> Construct and solve by backward induction: Player 0 chooses {A, B, C}. If A: Player 1 chooses {X, Y} → payoffs X:(3,1), Y:(1,4). If B: chance node with equal probability between outcomes (5,0) and (0,5). If C: Player 1 chooses {X, Y}; if X, Player 0 chooses {L, R} → payoffs L:(2,3), R:(4,2); if Y, payoff (1,1). What is the SPE?

**Skills tested:** `(require 'extensive-form)`, `make-decision`, `make-chance`, `make-terminal`, `solve-spe`.

**Why it resists brute-force:** Small enough that a strong model might solve it, but the game tree construction itself tests whether the model can use the API. Scale by adding more players, deeper trees, and chance nodes.

**Scale knob:** Tree depth, number of players, chance nodes. At depth 5+ with chance nodes, backward induction in CoT becomes unreliable.

**Solution sketch:**
```scheme
(require 'extensive-form)
(let* ((a-tree (make-decision 1 '(X Y)
                (list (make-terminal '(3 1))
                      (make-terminal '(1 4)))))
       (b-tree (make-chance '(win lose) '(1/2 1/2)
                (list (make-terminal '(5 0))
                      (make-terminal '(0 5)))))
       (cx-tree (make-decision 0 '(L R)
                 (list (make-terminal '(2 3))
                       (make-terminal '(4 2)))))
       (c-tree (make-decision 1 '(X Y)
                (list cx-tree (make-terminal '(1 1)))))
       (game (make-extensive-game 2
               (make-decision 0 '(A B C)
                (list a-tree b-tree c-tree)))))
  (solve-spe game))
```

**Expected output:**
```
(spe-result (4 2) ((0 . C) (1 . X) (0 . R)))
```
SPE: Player 0 chooses C, Player 1 chooses X, Player 0 chooses R → payoffs (4,2). Reasoning: A gives P0 at most 3 (if P1 picks X) or 1 (if P1 picks Y; P1 prefers Y for payoff 4). B has expected value (2.5, 2.5). C→X→R gives P0 payoff 4, which dominates.

---

### Tier 4: Multi-Module Composition

These test: Can the model compose capabilities across lattice boundaries?

#### P10 — Normalization Version Comparison

> Consider:
> - A = `(fn (x) (+ (* x 1) 0))`
> - B = `(fn (y) y)`
>
> A has an identity multiplication and an identity addition wrapping a simple variable reference. B is just the identity function.
>
> Are A and B equivalent under α-only hashing (`hash-sexpr`)? Under algebraic hashing (`hash-sexpr-algebraic`)? Under v2? Under v3?

**Skills tested:** Loading the normalizer and CAS, understanding the four hashing modes and what each normalizes, calling the hash functions, comparing bytevectors.

**Why it resists brute-force:** While a model can mentally simplify `(+ (* x 1) 0)` → `x`, it cannot predict (a) which normalization versions catch this equivalence, and (b) the actual hex hashes. The CAS version bytes (`0x00`, `0x01`, `0x02`, `0x03`) and SHA-256 digests are unknowable without the tool.

**Scale knob:** Expression complexity. Add more algebraic identities, nested eta-wrappers, polynomial rearrangements, deeper nesting.

**Solution sketch:**
```scheme
(load "core/blocks/normalize.ss")
(require 'cas)
(let ((A '(fn (x) (+ (* x 1) 0)))
      (B '(fn (y) y)))
  (list
    (list 'alpha-only
      (bytevector=? (hash-sexpr 'fn A) (hash-sexpr 'fn B)))
    (list 'algebraic
      (bytevector=? (hash-sexpr-algebraic 'fn A) (hash-sexpr-algebraic 'fn B)))
    (list 'v2
      (hash->hex (hash-sexpr-v2 'fn A))
      (hash->hex (hash-sexpr-v2 'fn B))
      (bytevector=? (hash-sexpr-v2 'fn A) (hash-sexpr-v2 'fn B)))
    (list 'v3
      (hash->hex (hash-sexpr-v3 'fn A))
      (hash->hex (hash-sexpr-v3 'fn B))
      (bytevector=? (hash-sexpr-v3 'fn A) (hash-sexpr-v3 'fn B)))))
```

**Expected output:**
```
((alpha-only #f)
 (algebraic #f)
 (v2 "<hex>" "<hex>" #t)
 (v3 "<hex>" "<hex>" #t))
```
α-only hashing: NOT equal (it only renames variables, doesn't simplify arithmetic). Algebraic hashing: NOT equal (sorts commutative ops but doesn't eliminate identities). v2: EQUAL (identity elimination collapses `(* x 1)` → `x` and `(+ x 0)` → `x`). v3: EQUAL (same, with NbE adding nothing here). This cleanly demonstrates the progression of normalization power across versions.

#### P11 — Gradient Verification Pipeline

> Define f(x) = det([[x, 1], [2, x+1]]) = x(x+1) - 2 = x² + x - 2. Compute f'(3) two ways:
> 1. Analytically (by expanding and differentiating)
> 2. Numerically via The Fold's reverse-mode AD
>
> Do the results agree?

**Skills tested:** Matrix module, autodiff, the ability to express a parameterized matrix determinant using traced operations, comparing approaches.

**Why it resists brute-force:** The analytical part is easy (f'(x) = 2x+1, f'(3) = 7). The interesting part is whether the model can wire traced values through the determinant computation — this tests API fluency and understanding that `matrix-det` operates on Scheme numbers, not traced values, so the determinant formula must be re-expressed as traced arithmetic.

**Solution sketch:**
```scheme
(require 'reverse-diff)
;; det([[x, 1], [2, x+1]]) = x*(x+1) - 2*1 = x^2 + x - 2
;; Use gradient (not gradient-at — the latter has a wrapping bug with single args)
(gradient
  (lambda (x)
    (traced-sub
      (traced-mul x (traced-add x (make-traced-const 1 (traced-tape x))))
      (make-traced-const 2 (traced-tape x))))
  '(3.0))
;; Analytical: f'(x) = 2x + 1, so f'(3) = 7
```

**Expected output:**
```
(7.0)
```
Matches `f'(x) = 2x + 1` at `x = 3`. The key challenge is that `matrix-det` can't operate on traced values directly — the model must re-derive the determinant formula as traced arithmetic. This tests whether the model understands the boundary between the matrix module (Scheme numbers) and the AD module (traced values).

#### P12 — Topology of a Parameterized Family

> The triangulated n-gon prism has 2n vertices, 3n quad-diagonal edges + 2n polygon edges, and 2n triangular faces. Construct the prism for n=5 (pentagonal prism) as a simplicial complex and compute its Betti numbers. Then do n=6 (hexagonal prism). Do the Betti numbers change?

**Skills tested:** Understanding simplicial complexes, constructing parameterized families, homology computation, comparing results.

**Why it resists brute-force:** The model must triangulate the prism correctly (each rectangular face becomes two triangles), construct the complex, and compute. A hollow prism has β₀=1, β₁=1, β₂=0 (homotopy equivalent to S¹), but only if triangulated correctly as a surface without caps. With caps it's contractible (β = (1,0,0)). The model must make and verify the right choice.

**Scale knob:** n (polygon order), whether to cap the ends, whether to add interior structure.

**Solution sketch:**
```scheme
(require 'simplicial-complex)
(require 'homology)

;; Build an open n-gon prism: vertices 0..n-1 on top, n..2n-1 on bottom.
;; Each rectangular side face is split into two triangles.
(define (make-prism-surface n)
  (let* ((top (map (lambda (i) i) (iota n)))
         (bot (map (lambda (i) (+ i n)) (iota n))))
    (sc-from-simplices
      (append
        ;; Upper triangles of each quad face
        (map (lambda (i)
               (let ((j (mod (+ i 1) n)))
                 (triangle (list-ref top i) (list-ref top j) (list-ref bot i))))
             (iota n))
        ;; Lower triangles of each quad face
        (map (lambda (i)
               (let ((j (mod (+ i 1) n)))
                 (triangle (list-ref top j) (list-ref bot i) (list-ref bot j))))
             (iota n))))))

(list
  (sc-betti-numbers (make-prism-surface 5))
  (sc-betti-numbers (make-prism-surface 6)))
```

**Expected output:**
```
((1 1 0) (1 1 0))
```
Both give (1, 1, 0): one connected component, one independent cycle (the prism is homotopy equivalent to S¹), no 2-holes (open ends). The Betti numbers are the same regardless of n — the prism is always a cylinder. If one end is capped with a triangulation fan, the result becomes (1, 0, 0) (contractible).

---

## Generating New Problems

### Template: Exact Numerical

```
Given [specific input data], compute [specific operation] using The Fold.
Report the exact result.
```

The input data should be chosen so the output involves exact rationals or enough decimal places that mental computation fails. Good operations: matrix decomposition, determinant, gradient, solving linear systems.

### Template: State Query

```
Query The Fold's [BBS / block store / module registry] and report [specific statistic or filtered list].
```

Inherently tool-dependent. Can be parameterized infinitely by varying the query.

### Template: Encode-and-Solve

```
[Problem description in natural language]. Encode this as a [SAT / constraint / game theory] problem
using The Fold and solve it.
```

Tests both the encoding step (translating English to formal constraints) and the tool-use step.

### Template: Multi-Module Pipeline

```
Using modules [A] and [B], compute [result that requires output of A as input to B].
```

Tests composition and data flow between modules.

### Anti-Patterns (Problems That Don't Work)

- **Standard mathematical facts:** "What are the Betti numbers of a torus?" — memorizable.
- **Small arithmetic:** "What is 7 * 13?" — trivially brute-forced.
- **Yes/no with obvious structure:** "Is a complete graph on 3 vertices 2-colorable?" — K₃ is textbook.
- **Problems solvable by pattern matching:** "Simplify (+ x 0)" — every model knows this.

The test is: **if you can imagine a human solving it on a napkin in 30 seconds, the model will brute-force it.**

---

## Curriculum Structure

Recommended training order:

1. **Module discovery** (P1, P2) — Learn to navigate the lattice
2. **Single-tool computation** (P3, P4) — Learn to call functions correctly
3. **Numerical precision** (P5, P6) — Learn to trust the tool over CoT arithmetic
4. **Solver encoding** (P7, P8, P9) — Learn to translate problems into tool inputs
5. **Multi-module composition** (P10, P11, P12) — Learn to chain capabilities

Within each stage, vary parameters to prevent memorization. A model that memorizes the Petersen graph answer learns nothing; a model that learns to call `graph-coloring-solve` with arbitrary inputs has acquired a generalizable skill.
