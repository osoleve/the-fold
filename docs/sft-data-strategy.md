# SFT Data Strategy: Teaching The Fold to a Base Model

## Goal

Full SFT on Qwen3.5 base (4B or 9B) to produce a model that knows The Fold
intrinsically — Scheme fluency, lattice structure, architectural reasoning —
while retaining general STEM and coding capabilities.

Target: **~100M Fold-specific tokens**, mixed with **~200-400M retention tokens**,
for a total training corpus of **300-500M tokens**.

## Evidence So Far

Early sweep results (2026-03-03) show full SFT on 0.8B-Base already beats
LoRA on 4B-Base (eval_loss 0.372 vs 0.454) with only ~20M tokens of mixed
training data. This validates the approach — full weight updates on curated
data outperform parameter-efficient methods on more weights.

## Fold-Specific Data Types

### 1. Tree Edit Sequences (highest priority)

**Idea:** Base models can learn *processes*, not just input/output pairs. For
each target S-expression, generate the minimum tree edit path from empty to
complete, unrolled as a sequential text stream.

**Construction mode:**
```
;; Goal: (define (factorial n) (if (= n 0) 1 (* n (factorial (- n 1)))))

()
(define)
(define (factorial n))
(define (factorial n) (if))
(define (factorial n) (if (= n 0)))
(define (factorial n) (if (= n 0) 1))
(define (factorial n) (if (= n 0) 1 (* n (factorial (- n 1)))))
```

The model learns to build S-expressions hierarchically — outside-in, skeleton
then leaves. Each intermediate state is a coherent partial program.

**Maintenance/editing mode:**
```
;; Change: add error handling for negative input

;; Before:
(define (factorial n)
  (if (= n 0) 1 (* n (factorial (- n 1)))))

;; Edit 1: wrap body in cond
(define (factorial n)
  (cond))

;; Edit 2: add negative guard
(define (factorial n)
  (cond
    [(< n 0) (error 'factorial "negative input")]))

;; Edit 3: restore base case
(define (factorial n)
  (cond
    [(< n 0) (error 'factorial "negative input")]
    [(= n 0) 1]))

;; Edit 4: restore recursive case
(define (factorial n)
  (cond
    [(< n 0) (error 'factorial "negative input")]
    [(= n 0) 1]
    [else (* n (factorial (- n 1)))]))
```

**Implementation:** Tree diff in Scheme (Zhang-Shasha or similar on parsed
S-expressions) to compute minimum edit paths. The diff is mechanical — no LLM
needed for the edit sequence itself. LLM generates source/target pairs; the
tree-diff produces training sequences deterministically.

**Volume:** Each function produces a multi-step sequence (5-20 steps). With
~10K functions from synthgen + lattice source, this could yield 50-100K
training sequences.

### 2. Tutorials and Worked Examples

**Idea:** Pair natural language explanation with code, showing not just *what*
to write but *why*. Use existing tutorials (diffgeo, physics, symbolic
computation) as seeds, generate more with Qwen3.5-27B.

**Format:**
```
## Working with Optics

A Lens focuses on exactly one part of a data structure. In The Fold,
lenses compose left-to-right with `lens-compose`:

(require 'optics)

;; A lens into the second element of a pair
(define snd-lens (make-lens cdr set-cdr!))

;; Compose to reach nested structure
(define deep-lens (lens-compose fst-lens snd-lens))

(lens-view deep-lens '((1 . 2) . 3))  ; => 2
```

**Source material:** Existing tutorials (~4K tokens), tech report chapters,
lattice skill documentation, CLAUDE.md architectural sections.

**Verification:** Execute all code blocks against the Fold REPL. Only include
samples that produce correct output.

### 3. Technical Report Expansions

**Idea:** Use the 20 chapters + appendices (~70K tokens) as seeds. For each
section, generate expanded explanations, alternative presentations, concrete
code examples illustrating the concepts.

**Examples:**
- Block calculus chapter → worked examples of normalization, content addressing
- Type theory chapter → concrete type inference traces on real Fold code
- Module system chapter → walkthrough of require chains, collision resolution
- Agent substrate chapter → annotated RLM interaction traces

**Volume:** 3-5x expansion of each chapter → ~200-350K tokens.

### 4. Lattice Navigation Sessions

**Idea:** Record or synthesize realistic REPL sessions showing how to discover
and compose lattice skills.

**Format:**
```
;; Task: Find if there's a shortest-path algorithm available

(lf "shortest path")
;; => graph-algorithms: dijkstra, bellman-ford, floyd-warshall

(li 'data)
;; => skill: data, modules: graph, graph-algorithms, ...

(le 'data/graph-algorithms)
;; => dijkstra, bellman-ford, a-star, topological-sort, ...

(require 'data/graph-algorithms)

;; Build a weighted graph and find shortest path
(define g (make-directed-graph))
(graph-add-edge! g 'a 'b 4)
(graph-add-edge! g 'a 'c 2)
(graph-add-edge! g 'c 'b 1)
(dijkstra g 'a 'b)  ; => (path: (a c b) cost: 3)
```

**Source:** Replay searches against the actual lattice meta-tooling to
guarantee correctness. Each of the ~47 skills can generate multiple
discovery sessions.

**Verification:** Fully reproducible — run the session against the REPL.

### 5. Curated Source with Annotations

**Idea:** Not raw file dumps (which we already have ~2,666 of), but selected
code with interleaved commentary explaining design decisions.

**Format:**
```
;; lattice/optics/lens.ss — Core lens implementation
;;
;; A Lens s t a b represents a functional reference from structure s to
;; focus a, with the ability to replace a with b to produce t.
;; In the simple case (Lens' s a), s=t and a=b.
;;
;; The Fold's optics are van Laarhoven style: a lens is a function
;; (a -> f b) -> s -> f t, polymorphic in the functor f.

(define (make-lens getter setter)
  ;; Wraps get/set into van Laarhoven representation.
  ;; The functor constraint is implicit — callers provide
  ;; the appropriate fmap at the use site.
  (lambda (f)
    (lambda (s)
      (fmap (lambda (b) (setter s b))
            (f (getter s))))))
```

**Source material:** Best-written lattice modules, selected by hand or by
quality score. Focus on modules that demonstrate important patterns:
protocols, optics, parsers, test conventions.

### 6. Git History Edit Traces

**Idea:** Mine the Fold's own git history for real edit sequences. For each
commit touching `.ss` files: old version + commit message → tree edit
sequence → new version.

**Advantage:** Real edits with real intent. The commit message provides
natural language description of the change.

**Volume:** Depends on git history depth. Each qualifying commit produces
one training sequence.

### 7. NotebookLM-Generated Content

**Idea:** Use Google's NotebookLM to ingest the technical report, source code,
and design docs, then produce diverse derivative content:

- **Podcast-style discussions** — two-voice audio exploring concepts like the
  block machine, fuel semantics, content addressing. Transcribed to text, these
  produce training data where concepts are explained multiple ways, objections
  are raised and addressed, and analogies emerge naturally. The dialogic format
  covers angles that single-voice exposition misses.
- **Quizzes and Q&A** — "What happens when two different S-expressions normalize
  to the same form?" / "They produce the same content hash, so the CAS stores
  them as one block." Teaches the model to reason about Fold concepts, not just
  recite code.
- **Reports and summaries** — different-length treatments of the same topic.
  The model sees the block calculus explained in 200 words, 2000 words, and
  10000 words, learning to compress and expand appropriately.
- **Chat-style exchanges** — natural Q&A about implementation details,
  architectural tradeoffs, debugging approaches.

**Advantage:** Produces genuinely diverse perspectives on the same material
without hand-crafting prompts. The podcast transcriptions especially are a
format that's hard to synthesize with standard LLM prompting — the emergent
back-and-forth produces more natural reasoning chains.

**Volume:** Each source document can generate multiple derivative formats.
The tech report alone (20 chapters) could yield 50+ distinct samples across
formats.

## Retention Data

To prevent catastrophic forgetting, mix Fold-specific data with:

### High Priority (closely related to Fold capabilities)
- **Other Lisps** — Racket, Common Lisp, Clojure, Emacs Lisp from The Stack v2
- **Haskell / ML family** — functional patterns, type classes, ADTs
- **Math and formal reasoning** — competition math, proofs, OpenWebMath
- **CS theory** — PLT, type theory, algorithms (arXiv cs.PL, cs.LO)

### Medium Priority (general capability retention)
- **Technical documentation** — manuals, API docs, RFCs
- **arXiv STEM** — math, cs.AI, physics
- **General code** — Python, Rust, C (small amount for breadth)

### Lower Priority
- **Wikipedia STEM subset** — factual grounding
- **STEM textbooks** — FineWeb-Edu filtered for math/CS

### What to Skip
- Fiction, social media, news, chat logs
- Non-STEM humanities
- Low-quality web text

### Suggested Ratio
- 30-40% Fold/Scheme specific
- 20-30% adjacent code (Lisps, Haskell, functional languages)
- 20-30% math/CS text (arXiv, textbooks, competition math)
- 10-20% technical prose (docs, Wikipedia STEM)

## Generation Infrastructure

### Hardware
- 2x DGX Spark (GB10), 128GB unified memory each
- Qwen3.5-27B NVFP4 via vLLM: ~20 tok/s per box serial, much higher batched
- Estimated throughput with concurrency: 100M tokens in 1-2 days

### Pipeline
1. **Prompt templates** per data type (tree edits, tutorials, expansions, etc.)
2. **Batch generation** via vLLM with continuous batching
3. **Verification** — syntax check via `./fold`, execution test where possible
   (existing subprocess-isolated verifier runs ~500 samples/s)
4. **Quality filtering** — generate 3x target, score with second-pass LLM prompt,
   keep top third
5. **Deduplication** — MinHash or exact-match on normalized S-expressions

### Priority Order
1. Tree edit sequences (mechanically verifiable, highest signal)
2. Tutorials / worked examples (execution-verified)
3. NotebookLM derivatives (podcasts, quizzes, Q&A — diverse, low-effort)
4. Tech report expansions (seeded from existing high-quality text)
5. Lattice navigation sessions (reproducible against real REPL)
6. Annotated source selections (hand-curated seeds, LLM-expanded)
7. Git history traces (mechanical extraction)

## Training Plan

Once dataset is assembled:
1. Full SFT sweep on 0.8B to validate data mix and find hyperparameters
2. Scale to 4B or 9B (may require rented hardware — 8xH100 node for 1-2 days)
3. Evaluate: Scheme generation quality, lattice task completion, general
   capability retention (GSM8K, coding benchmarks)
4. Iterate on data mix if retention degrades

## Open Questions

- Optimal ratio of construction vs editing tree-edit sequences?
- How much retention data is "enough"? Need ablation studies.
- Should tree edits show the diff description at each step or just the states?
- Is Zhang-Shasha the right tree edit algorithm or do we want something
  S-expression-aware that respects form boundaries?
- Token budget for the final model: 4B (faster, cheaper to train and serve)
  vs 9B (more capacity, better retention)?
