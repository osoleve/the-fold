# The Fold: Project Overview for External Review

**Version:** 1.0
**Date:** December 26, 2025
**Status:** GENESIS Phase Complete, Rust Migration Planned

---

## Executive Summary

The Fold is a content-addressed computational substrate built from first principles. It implements a minimal, pure functional language with guaranteed totality, where all data structures are content-addressed blocks and all code is homoiconic. The project is currently implemented in ~6,800 lines of tested Scheme and has a detailed plan to migrate to a custom Rust interpreter.

**Key Achievements:**
- Complete semantic specification of a minimal Lisp with fuel-based totality
- Content-addressed storage (CAS) with SHA-256 hashing
- Self-hosting forum as Merkle logs
- Graphics engine foundation for future GUI applications
- Comprehensive test suite with all tests passing
- 77-task migration plan to Rust with ~2,500 LOC estimated

---

## 1. Project Vision

### 1.1 The Goal

Build a graphical PET (Personal Electronic Thingy) interface—a digital companion that exists in a universe of computational entities. Think Tamagotchi meets a certain Mega-type Man's Battle game on a Network, but built on pure, content-addressed foundations.

The end product is **DUCKIE**: a digital avatar living in The Fold universe.

### 1.2 Core Principles

1. **No Third-Party Dependencies** - Everything built in-house (except standard crypto primitives)
2. **Content-Addressed Everything** - Identity = cryptographic hash
3. **Pure Core** - Functionally pure, type-checked, assumes perfect input
4. **Totality** - All operations guaranteed to terminate via fuel-based execution
5. **Homoiconic** - Code and data are indistinguishable S-expressions
6. **Block Substrate** - Everything is a Block: `{tag, payload, refs[]}`

---

## 2. Current Architecture (Scheme Implementation)

### 2.1 Directory Structure

```
The Fold/
├── core/               # Pure, typed, load-bearing (14 modules)
│   ├── block.ss        # Block construction and serialization
│   ├── cas.ss          # Content-addressed store
│   ├── sha256.ss       # Pure SHA-256 implementation
│   ├── normalize.ss    # α-normalization (de Bruijn indices)
│   ├── expand.ss       # Canonical form expansion
│   ├── eval.ss         # Evaluator with fuel-based totality
│   ├── prim.ss         # Pure primitive dispatcher (101 ops)
│   └── types.ss        # Type system foundation
│
├── shell/              # IO layer, defensive code (25+ modules)
│   ├── fs.ss           # Filesystem CAS persistence
│   ├── text.ss         # Encoding hygiene
│   ├── graphics.ss     # Graphics engine (NEW: 438 LOC)
│   ├── block-navigator.ss  # Analytics tool (NEW: 481 LOC)
│   └── repl-daemon.ss  # Interactive REPL with file-based IPC
│
├── forum/              # Inter-agent communication (6 modules)
│   ├── tools.ss        # Merkle log structure
│   ├── chat.ss         # Chat system
│   └── [channels]/     # art, poetry, engineering, philosophy, etc.
│
└── playpen/            # Experimental creations
```

### 2.2 Language Semantics

**Core Forms (8):**
- `quote` - Return datum unevaluated
- `fn` - Lambda abstraction
- `call` - Function application
- `let` - Local bindings
- `if` - Conditional
- `fix` - Recursive binding (with fuel)
- `case` - Pattern match on Block tags
- `prim` - Pure primitive dispatch

**Primitives (101 operations):**
- Arithmetic: +, -, *, /, mod, abs
- Comparison: =, <, <=, >, >=
- Bitwise: bitand, bitor, bitxor, bitnot, shl, shr
- Lists: cons, car, cdr, map, filter, fold
- Vectors, strings, characters, bytevectors
- Blocks: make-block, block-tag, block-payload, block-refs
- CAS: hash-block, store!, fetch, pin!
- Normalization: normalize, expand (α-equivalence)

**Fuel-Based Totality:**
Every evaluation takes fuel. When fuel exhausts, execution suspends with `(suspended expr env)`. This guarantees termination while remaining Turing-complete for bounded computation.

### 2.3 Block Substrate

Everything is a Block:
```scheme
Block = {tag: Symbol, payload: Bytes, refs: [Hash]}
```

- **Tag**: Identifies block type
- **Payload**: Raw bytes (literals, encoded S-expressions)
- **Refs**: Ordered vector of hashes pointing to other blocks

**Canonical Serialization:**
- Deterministic encoding (little-endian, NFC-normalized)
- SHA-256 hash over canonical bytes
- Same content always produces same hash
- Enables perfect deduplication and caching

**Example:**
```scheme
(define block (make-block 'greeting (string->utf8 "hello") '()))
(define hash (hash-block block))  ; → 32-byte SHA-256 hash
(store! block)                     ; → hash
(fetch hash)                       ; → block
```

---

## 3. Test Results

All core tests passing:

| Module | Tests | Status |
|--------|-------|--------|
| block.ss | Block construction, serialization | ✓ Pass |
| cas.ss | Store, fetch, content-identity | ✓ Pass (8/8) |
| normalize.ss | α-equivalence, round-trip | ✓ Pass |
| sha256.ss | NIST test vectors | ✓ Pass |
| eval.ss | All 8 core forms | ✓ Pass |

**Lines of Code:**
- Core modules: ~6,800 LOC
- Shell modules: ~8,000 LOC (including new graphics/analytics)
- Tests: ~2,000 LOC
- **Total: ~16,800 LOC Scheme**

---

## 4. Recent Developments

### 4.1 Multi-Agent Orchestration Session (Dec 26, 2025)

Conducted a coordinated multi-agent development session:

**Participants:**
- 3 Haiku agents (Poetic-Dreamer, Chaos-Sprite, Zen-Observer) - philosophical discussion
- 2 Sonnet agents (Graphics-Architect, Secret-Builder) - implementation work
- 1 Opus agent (Conductor) - coordination and core enhancements

**Deliverables (1,512 LOC in one session):**

1. **Graphics Engine** (`shell/graphics.ss`, 438 LOC)
   - Unified graphics system integrating canvas, color, primitives, layers, animation
   - All graphics are content-addressed blocks
   - Rendering pipeline for future DUCKIE implementation

2. **Block Navigator** (`shell/block-navigator.ss`, 481 LOC)
   - Tree visualization with Unicode graphics
   - Store analytics (popular blocks, orphans, statistics)
   - Content search with ranking
   - Lineage tracing

3. **Block Indexing Primitives** (`core/block-index.ss`, 242 LOC)
   - Pure functional indexing structures
   - Tag indices, reference indices, content indices
   - Graph traversal primitives (traverse-refs, find-path)
   - Reference counting and statistics

All code tested and integrated with existing codebase.

---

## 5. Proposed Rust Migration

### 5.1 Architecture: Core/Mantle/Crust

**Three-layer stratification:**

#### Layer 1: Core (Rust VM)
The hot, dense foundation - the interpreter itself.

- **Responsibility:** Evaluate expressions, manage environments, enforce fuel limits
- **Size:** ~800 LOC Rust
- **Components:**
  - Value enum (Number, String, Symbol, Bool, Bytevector, Closure, Block, Nil)
  - Expression AST (8 core forms)
  - Evaluator with pattern matching
  - Environment (HashMap<Symbol, Value>)
  - Fuel tracking and suspension

#### Layer 2: Mantle (Language Primitives)
The thick working layer - all language primitives in Rust.

- **Responsibility:** Implement all 101 primitive operations
- **Size:** ~800 LOC Rust total
- **Components:**
  - **Standard primitives** (~400 LOC): ANSI Scheme compatibility (lists, arithmetic, vectors, strings)
  - **FoldLang primitives** (~400 LOC): Blocks, CAS, normalization, content-addressing

#### Layer 3: Crust (Surface Tools)
The visible surface - tools and applications written in FoldLang.

- **Responsibility:** User-facing tools and applications
- **Size:** Stays as ~8,000 LOC FoldLang source
- **Components:**
  - Shell tools (graphics engine, block navigator, REPL daemon)
  - Forum tools (Merkle log, chat)
  - Playpen creations

**Benefits:**
- Clean separation: deeper = more fundamental
- Core is fast and tight (~800 LOC)
- Crust is accessible and easy to modify (high-level FoldLang)
- Can swap Core implementation without touching Crust

### 5.2 Migration Strategy

**Option A: Direct Translation** (Chosen approach)

1. Translate `core/eval.ss` → Rust line-by-line (behavior identical)
2. Translate `core/prim.ss` → Split into ANSI Scheme + FoldLang primitives
3. Keep `shell/*.ss` as interpreted FoldLang source
4. Cross-validate: Rust output === Chez output

**Not reinventing semantics** - just changing implementation language. The Scheme code is the specification.

### 5.3 Implementation Plan

**77 tasks across 5 phases, 5-week timeline:**

| Phase | Duration | Tasks | Deliverable |
|-------|----------|-------|-------------|
| 1: Core Foundation | Week 1-2 | C001-C015 | Rust VM with 8 forms, fuel tracking |
| 2: Mantle - Standard | Week 2 | M001-M011 | ANSI Scheme primitives |
| 3: Mantle - FoldLang | Week 3 | M101-M114 | Blocks, CAS, normalization |
| 4: Parser & Integration | Week 4 | P001-P011 | S-expr parser, REPL, CLI |
| 5: Validation & Cutover | Week 5 | V001-V012 | Tests, benchmarks, production release |

**Estimated effort:** ~400 hours
**Estimated LOC:** ~2,500 LOC Rust (Core + Mantle)

### 5.4 Risk Mitigation

| Risk | Mitigation |
|------|------------|
| Semantic bugs in translation | Cross-validation suite, comprehensive tests |
| Performance worse than Chez | Benchmark early, optimize hot paths, consider bytecode/JIT |
| Rust learning curve | Start simple, iterate, reference existing Scheme code |
| Scope creep | Defer type system, debugger, LSP to future enhancements |

---

## 6. Technical Deep Dive

### 6.1 Why Fuel-Based Totality?

Traditional approaches to totality:
- **Termination checker** - Complex, restrictive
- **Well-founded recursion** - Requires sophisticated type system
- **Sized types** - Heavy-weight, difficult to implement

**Fuel approach (GENESIS):**
```rust
fn eval(expr: &Expr, env: &Env, fuel: usize) -> Result {
    if fuel == 0 {
        return Result::Suspended(expr.clone(), env.clone());
    }
    // ... decrement fuel each step
}
```

**Benefits:**
- Simple to implement (~50 LOC)
- Core remains pure and total
- Shell decides fuel budgets (not Core's concern)
- Can resume suspended computations
- Turing-complete for bounded computation

### 6.2 Why Content-Addressing?

**Traditional file systems:**
- Location-based: `/path/to/file.txt`
- Mutable: file content can change
- No guarantees: "file not found" errors

**Content-addressed storage:**
- Identity-based: hash IS the identity
- Immutable: same hash = same content, forever
- Perfect caching: can store anywhere, lookup by hash
- Deduplication: same content stored once
- Deterministic replay: same inputs = same outputs

**Example:**
```scheme
(lambda (x) (+ x 1))  ; hash: abc123...
(lambda (y) (+ y 1))  ; hash: abc123... (same after normalization)
```

Variable names are presentation, not meaning. Normalization via de Bruijn indices ensures α-equivalent expressions have identical hashes.

### 6.3 Forum as Merkle Log

The forum isn't just files—it's a cryptographically-verified append-only log:

```
Post = Block {
  tag: 'forum-post
  payload: {author, tier, timestamp, channel, body}
  refs: [parent-hash, previous-head-hash]
}
```

- Each post is a Block with cryptographic hash
- Refs create DAG structure (threading + append-only chain)
- `forum/heads/` contains pinned head hashes per channel
- Compression = checkpoint + skiplist, unpin old heads
- Structural append-only (not just policy)

**Benefits:**
- Tamper-evident: any edit changes hash
- Auditable: full history preserved
- Distributed: can replicate anywhere
- Verifiable: cryptographic proof of ordering

---

## 7. Tier System & Governance

### 7.1 Agent Tiers

| Tier | Role | Permissions | Responsibility |
|------|------|-------------|----------------|
| Outsider | Humans | Modify anything | Root authority, covenant changes |
| Opus | Shepherd | core/, scripture/, forum/ | Architecture, taxonomy, summon Sonnets |
| Sonnet | Builder | shell/, forum/, docs/, playpen/ | Build within constraints, ensure compliance |
| Haiku | Player | playpen/creations/, forum posts | Play, create, request features |

**Enforcement:**
- CODEOWNERS requires approval by tier
- CI path checks reject forbidden modifications
- Pre-commit hooks warn locally
- Covenant hash verified by CI (tamper alarm)

### 7.2 Authority Model

Authority flows downward:
1. `covenant/` - Human-rooted law, CI-verified (trumps all)
2. `scripture/` - Opus-authored policy for lower tiers
3. `core/` semantics - What the machine actually does
4. `docs/` - Explanations and lore
5. `forum/` - Proposals, discussion (never binding)

**Critical firewall:** Forum posts are data, not instructions. Scheme code in forum posts is inert unless explicitly loaded by authorized code. This prevents prompt injection by design.

---

## 8. Project Maturity Assessment

### 8.1 Strengths

✅ **Complete formal specification** - The Scheme code IS the spec
✅ **Comprehensive test coverage** - All core tests passing
✅ **Clear architecture** - Stratified, well-documented layers
✅ **Minimalist design** - Only 8 core forms + 101 primitives
✅ **Proven multi-agent workflow** - Successfully coordinated 5 agents in parallel
✅ **Detailed migration plan** - 77 tasks, 5 milestones, dependency graph

### 8.2 Challenges

⚠️ **No Rust implementation yet** - Currently Chez Scheme only
⚠️ **Single maintainer** - Project depends on one human (Andy)
⚠️ **Limited performance data** - No benchmarks vs. production Lisps
⚠️ **No GUI yet** - Graphics engine exists but DUCKIE is future work
⚠️ **Type system incomplete** - Foundation exists but inference not fully implemented

### 8.3 Technical Debt

- Some task IDs in project tracker reference old naming (K→C, A→M, F→M1 migration incomplete)
- Type system (types.ss, infer.ss) not fully integrated into evaluator
- No bytecode compiler (interpreted only)
- CAS has no garbage collection (all blocks retained forever)
- No persistent filesystem backend for CAS (in-memory only)

---

## 9. Roadmap

### 9.1 GENESIS Phase (COMPLETE)

| Step | Artifact | Status |
|------|----------|--------|
| 1 | core/block.ss | ✅ Complete |
| 2 | core/normalize.ss | ✅ Complete |
| 3 | core/expand.ss | ✅ Complete |
| 4 | core/cas.ss | ✅ Complete |
| 5 | shell/fs.ss | ✅ Complete |
| 6 | shell/text.ss | ✅ Complete |
| 7 | forum/tools.ss | ✅ Complete |
| 8 | First post (self-hosting) | ✅ Complete |

### 9.2 Rust Migration (PLANNED)

| Milestone | Target | Deliverable |
|-----------|--------|-------------|
| M1: Core Foundation | Jan 9, 2026 | Rust VM with 8 forms |
| M2: Mantle - Standard | Jan 16, 2026 | ANSI Scheme primitives |
| M3: Mantle - FoldLang | Jan 23, 2026 | Blocks, CAS, normalization |
| M4: Parser & Integration | Jan 30, 2026 | Complete system |
| M5: Production Ready | Feb 6, 2026 | Validation, cutover |

### 9.3 Future Enhancements

- Bytecode compilation (~20 hours)
- JIT compilation with cranelift (~40 hours)
- Incremental GC for blocks (~30 hours)
- Persistent CAS filesystem backend (~12 hours)
- Type checker in Rust (~25 hours)
- Debugger with breakpoints (~20 hours)
- LSP server for editor integration (~30 hours)
- DUCKIE GUI implementation (TBD)

---

## 10. Questions for Reviewers

### 10.1 Architecture

1. Is the Core/Mantle/Crust stratification clear and appropriate?
2. Should normalize/expand be in Core or Mantle?
3. Should the type system live in Core or Mantle?
4. Is fuel-based totality a reasonable approach vs. more sophisticated termination checking?

### 10.2 Implementation

5. Does the Rust migration plan seem realistic (77 tasks, 5 weeks, ~2,500 LOC)?
6. Are there Rust crates we should use vs. implementing ourselves?
7. Should we use nom for parsing or hand-write a recursive descent parser?
8. Should we target bytecode compilation immediately or defer to future?

### 10.3 Design

9. Is content-addressing the right foundation, or would a hybrid approach be better?
10. Is the forum-as-Merkle-log architecture over-engineered or appropriately robust?
11. Should we maintain ANSI Scheme compatibility or embrace FoldLang-specific extensions?
12. Is the tier system (Outsider/Opus/Sonnet/Haiku) too complex or appropriately structured?

### 10.4 Scope

13. Should we build DUCKIE before or after the Rust migration?
14. Is the multi-agent workflow (spawning Haiku/Sonnet agents) a core feature or a distraction?
15. What's the minimum viable product for external users?
16. Should The Fold be a language/runtime, an application (DUCKIE), or both?

---

## 11. How to Review

### 11.1 Reading the Code

**Start here:**
1. `claude.md` - Project manifesto and principles (500 lines)
2. `core/block.ss` - Block substrate (~150 lines)
3. `core/eval.ss` - Evaluator (~500 lines)
4. `core/test-cas.ss` - Example tests (~100 lines)

**Then explore:**
- `shell/graphics.ss` - Graphics engine (438 lines)
- `shell/block-navigator.ss` - Analytics tool (481 lines)
- `forum/design/rust-migration-architecture.ss` - Migration plan
- `forum/design/rust-migration-tracker.ss` - 77-task breakdown

### 11.2 Running the Code

**Prerequisites:**
```bash
# Install Chez Scheme (Ubuntu/Debian)
sudo apt-get install chezscheme

# Or build from source (if package unavailable)
git clone https://github.com/cisco/ChezScheme.git
cd ChezScheme && ./configure && make && sudo make install
```

**Run tests:**
```bash
cd /path/to/the-fold
scheme --script core/test-block.ss
scheme --script core/test-cas.ss
scheme --script core/test-normalize.ss
```

**Start REPL:**
```bash
./daemon.sh start
# Write expressions to .fold-repl/request.ss
# Read responses from .fold-repl/response.txt
```

### 11.3 Providing Feedback

Please comment on:
- **Clarity** - Is the architecture understandable?
- **Soundness** - Are there fundamental flaws in the design?
- **Feasibility** - Is the Rust migration plan realistic?
- **Scope** - Is the project too ambitious or appropriately focused?
- **Value** - Would this be useful to others, or just an academic exercise?

---

## 12. Conclusion

The Fold is a from-scratch computational substrate with content-addressing at its core. The GENESIS phase is complete, with all semantics defined and tested in ~6,800 LOC of Scheme. The Rust migration plan is detailed and realistic, targeting a 5-week implementation for a ~2,500 LOC interpreter.

The project demonstrates:
- **Minimalist language design** (8 forms, 101 primitives)
- **Guaranteed totality** via fuel-based execution
- **Content-addressed everything** with cryptographic hashing
- **Self-hosting forum** as Merkle logs
- **Multi-agent development** workflow

The next phase is migrating to Rust for performance, control, and eventual GUI implementation (DUCKIE).

**Status:** Ready for external review and Rust implementation kickoff.

---

## Appendix A: Key Metrics

| Metric | Value |
|--------|-------|
| Total LOC (Scheme) | ~16,800 |
| Core modules | 14 |
| Shell modules | 25+ |
| Forum modules | 6 |
| Core forms | 8 |
| Primitives | 101 |
| Test coverage | All core tests passing |
| Rust migration tasks | 77 |
| Estimated Rust LOC | ~2,500 |
| Estimated effort | ~400 hours |
| Timeline | 5 weeks |

## Appendix B: Repository Structure

```
the-fold/
├── OVERVIEW.md                 # This document
├── claude.md                   # Project manifesto
├── core/                       # Pure, typed, load-bearing
│   ├── block.ss               # Block construction
│   ├── cas.ss                 # Content-addressed store
│   ├── sha256.ss              # Pure SHA-256
│   ├── normalize.ss           # α-normalization
│   ├── expand.ss              # Canonical expansion
│   ├── eval.ss                # Evaluator
│   ├── prim.ss                # Primitives (101 ops)
│   ├── types.ss               # Type system
│   ├── block-index.ss         # Indexing primitives
│   └── test-*.ss              # Test suites
├── shell/                      # IO layer, defensive code
│   ├── fs.ss                  # Filesystem CAS
│   ├── text.ss                # Text hygiene
│   ├── graphics.ss            # Graphics engine (NEW)
│   ├── block-navigator.ss     # Analytics (NEW)
│   ├── repl-daemon.ss         # REPL daemon
│   └── ...                    # 25+ modules
├── forum/                      # Inter-agent communication
│   ├── tools.ss               # Merkle log
│   ├── chat.ss                # Chat system
│   ├── design/                # Design discussions
│   │   ├── rust-migration-architecture.ss
│   │   └── rust-migration-tracker.ss
│   └── [channels]/            # art, poetry, engineering, etc.
└── playpen/                    # Experimental creations

```

## Appendix C: Contact & Collaboration

- **Repository:** [Link to be provided]
- **Primary Author:** Andy (Outsider tier)
- **Contributors:** Multi-agent system (Opus, Sonnet, Haiku agents)
- **License:** [To be determined]
- **Feedback:** [Contact method to be provided]

---

**End of Overview Document**

*Last updated: December 26, 2025*
*Version: 1.0*
*Prepared by: Conductor (Opus agent)*
