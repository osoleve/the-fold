# RLM v2: HUD-Based State Machine Architecture

**Status:** Draft — revised after peer review (Gemini 3 Pro, OpenCode big-pickle, Codex gpt-5.3)
**BBS:** fold-zxvu
**Authors:** Andy + Claude Opus
**Date:** 2026-02-06

---

## Motivation

The current RLM implementation (`boundary/pipeline/rlm-loop.ss`) follows the original RLM paper's formulation: a chat-based agent loop where the model generates markdown-fenced code blocks within a conversation, receives execution results as user messages, and iterates. This works but inherits assumptions from the paper that fight The Fold's substrate:

1. **Chat framing** — `user`/`assistant`/`system` roles, message lists, sliding windows. The model is treated as a chatbot, not a reasoner.
2. **Markdown fences** — Code wrapped in ` ```scheme ` blocks, parsed with regex. A text display format used as a protocol boundary in a homoiconic system.
3. **Repeated tool documentation** — 80 lines of tool docs pasted into every step prompt. Static information repeated N times.
4. **Manual memory management** — The model explicitly calls `(rlm-env-put! 'key value)` and `(rlm-env-get 'key)`. Bookkeeping disguised as reasoning.
5. **Sliding window context** — "Keep first 2 messages + last 12" — a lossy heuristic that erases early reasoning.
6. **Magic-key completion** — `(rlm-env-put! 'answer value)` signals termination. A protocol convention hidden inside a data operation.

These all exist because the paper assumes a text-in/text-out model behind a chat API. The Fold doesn't need that assumption.

---

## Core Design: Agent as Pure Function

The agent is a function from state to action. The driver calls it, executes the action, then calls it again with updated state.

```
agent : State → Action
```

The agent has no memory, no conversation history, no hidden state. It receives the world, it makes a move. Like a chess engine given a board position.

### Two Calls Per Step: Act, Then Reflect

Each logical step involves two LLM calls:

1. **Act** — the agent receives the HUD and emits `think` + action(s)
2. **Reflect** — the same model (for now; a cheaper model later) receives the raw thinking + action results and produces a one-line **note** summarizing what was learned

The raw chain-of-thought is working memory — it helps the model reason in the moment but is discarded after the reflection pass. What persists is the distilled note, added to a running `notes` log in the state. This gives the agent access to the *conclusions* of its prior reasoning without the token cost of full CoT rollouts.

Why separate calls? If the model could perfectly summarize its own reasoning in-band while also acting, we wouldn't need CoT in the first place. Summarization is a distinct cognitive act from reasoning — the model needs to meander to get somewhere useful, then a separate pass extracts the insight.

### The Driver

```scheme
(define (rlm-drive task input agent summarizer)
  (let loop ([state (make-initial-state task input)])
    (cond
      [(state-complete? state) (state-result state)]
      [(state-exhausted? state) (state-error state)]
      [else
       (let* ([;; Act: agent thinks + emits action
              (act-result (agent state))
              (raw-thought (act-result-thought act-result))
              (action (act-result-action act-result))
              ;; Execute: driver runs the action
              (observation (execute-action state action))
              ;; Reflect: summarize what was learned
              (note (summarizer raw-thought action observation))
              ;; Update: build new state
              (state* (update-state state action observation note))])
         (loop (record-step! state*)))])))
```

The `summarizer` is a parameter — same model for now, swappable to a cheaper model later. The reflection call is cheap: short input (raw thought + observation), short output (one line).

---

## The State (The HUD)

The state is a structured S-expression — a heads-up display of everything the agent needs to make its next decision. Not a chat log. Not a narrative. A projection of current reality.

```scheme
(rlm-state
  ;; Always visible — the objective
  (task "Find eigenvalues of the input matrix and return them as a sorted list")

  ;; Agent-created plan — the todo list.
  ;; Agent proposes and updates status. Driver records outcomes.
  (plan
    ((check-input     . done)
     (search-tools    . done)
     (load-module     . pending)
     (compute-eigen   . pending)
     (verify-result   . pending)))

  ;; Inventory — what's available, not the content itself
  (env
    ((input       matrix  2400)
     (search-hits list    340)))

  ;; Which lattice modules are loaded and available
  (loaded (linalg/vec linalg/matrix))

  ;; Semantic memory — distilled insights from the reflection pass.
  ;; Compact, agent-readable. Subject to adaptive compaction when
  ;; notes exceed the context budget (see Memory Management below).
  (notes
    ("Input is 50x50 symmetric real-valued matrix"
     "linalg skill has matrix-decomp module with matrix-eigen — direct decomposition"))

  ;; Episodic memory — immutable pointers to full step records in CAS.
  ;; The ground truth. If notes were compacted and the agent needs
  ;; the original detail, (recall-step N) retrieves it from here.
  (episodic
    ((0 . "cas-hash-abc123")   ;; step 0 full record
     (1 . "cas-hash-def456"))) ;; step 1 full record

  ;; Most recent action result (full detail). Cleared each step.
  (last-result
    (1 search "eigenvalue decomposition"
       "linalg (0.87): decomposition, solvers\noptimization (0.42): gradient methods"))

  ;; Resource gauges
  (fuel 48500)
  (step 2))
```

### HUD Design Principles

1. **The state replaces the conversation.** No message history. The HUD is the complete context. The agent reads the HUD, makes a move. The driver updates the HUD with the result.

2. **The plan is emergent.** The agent's first action is typically to examine the input and propose a plan. Subsequent invocations see the plan with items checked off. The agent communicates with its future self through the state. **The agent owns plan status** — the driver records what happened, the agent decides whether the plan item is satisfied.

3. **Dual memory: semantic + episodic.** After each action, a reflection pass distills the raw chain-of-thought + observation into a one-line **note** (semantic memory). The full step record is also stored in the CAS with its hash recorded in the **episodic log** (immutable ground truth). Notes are the working understanding; the episodic log is what actually happened. If a note was wrong or compacted away, `(recall-step N)` retrieves the original.

4. **Adaptive compaction.** Notes grow one per step. When their total token count exceeds a configurable fraction of the context budget, the driver triggers a **compaction pass**: a separate LLM call that receives the current notes, plan, and task, and produces a reduced set — keeping notes relevant to pending plan items, folding completed-item notes into a brief context summary. Compaction is soft: nothing is permanently lost, just moved from the HUD to the episodic log. Short tasks never trigger it. Long tasks trigger it when they need to.

5. **Truth-maintenance for notes.** Notes are beliefs, not facts. Later steps can invalidate earlier conclusions (e.g., "linalg has SVD" → turns out it doesn't). The agent handles this the same way it handles any new information: the most recent note takes precedence, and the plan adjusts. During compaction, the compactor sees the full note sequence and can drop contradicted notes. No separate retraction mechanism — temporal ordering + compaction is sufficient.

6. **Env shows metadata, not content.** The agent sees `(input matrix 2400)` — key, type, size. If it needs the actual content, it uses `(retrieve 'input)`. Large values support chunked access via `(peek key n)`, `(grep key pattern k)`, and `(slice key start end)` — no full materialization needed.

7. **Rendering is parameterized.** `render-state : State × ContextBudget × ModelProfile → String`. A weaker model gets a sparser HUD. A stronger model gets richer detail. Same state, different projections.

8. **Last-result is ephemeral.** Only the most recent action's full result is shown. Everything older lives in the notes as distilled insights. This keeps the HUD bounded regardless of run length.

---

## The Action Language

Fourteen forms. The agent interface.

```
action ::= (search query)          ;; Search the lattice by keyword
         | (inspect skill)         ;; Get skill description, deps, modules
         | (exports skill)         ;; List a skill's exported functions
         | (load module)           ;; Load a lattice module into session
         | (eval expr)             ;; Evaluate arbitrary Scheme (escape hatch)
         | (store key expr)        ;; Store a computed value in env
         | (retrieve key)          ;; Fetch a value from env (full content)
         | (peek key n)            ;; Preview first n chars of an env value
         | (grep key pattern k)    ;; Search chunks of a value by keyword, top-k
         | (slice key start end)   ;; Extract a range from a chunked value
         | (recall-step n)         ;; Retrieve full record of step N from episodic log
         | (submit expr)           ;; Signal completion with final answer
         | (think text)            ;; Raw reasoning (ephemeral — fed to reflection pass)
         | (plan! items)           ;; Propose or update the plan
```

Plus `(begin action ...)` to compose multiple actions in one turn.

### Action Semantics

- **`search`**: Runs `lf` against the lattice BM25 index. Returns ranked skill/module/export matches.
- **`inspect`/`exports`**: Runs `li`/`le`. Returns skill metadata or export list.
- **`load`**: Runs `(require 'module)` in the IPC worker. Updates `loaded` in state.
- **`eval`**: Executes arbitrary Scheme in the fuel-bounded sandbox. The escape hatch. Result is automatically stored in env with a generated key (e.g., `step-3-result`). No manual `env-put!` needed.
- **`store`**: Explicitly name a value for later reference. For when auto-naming isn't enough.
- **`retrieve`**: Fetch full content of an env entry. Returns the actual value, not just metadata. For large values, prefer `peek`/`grep`/`slice` instead.
- **`peek`**: Preview the first N characters of an env value. For large inputs where full retrieval would explode the context.
- **`grep`**: Search chunks of a value by keyword pattern, return top-k matching chunks. Ported from v1's `rlm-env-grep`.
- **`slice`**: Extract a contiguous range from a chunked value by index. Ported from v1's `rlm-env-read-chunk`.
- **`recall-step`**: Retrieve the full observation from a previous step via the episodic log. The safety net for compacted notes — if the agent needs something that was summarized away, it can go back to the source.
- **`submit`**: Evaluate the expression and terminate the run with its value as the answer. Supports an optional **verifier hook** — a predicate registered at task creation that checks the answer before accepting it. If the verifier returns `#f`, the submit becomes a failed observation ("answer rejected: [reason]") and the run continues. This enables tasks with machine-checkable correctness criteria (e.g., "result must be a sorted list of numbers") without requiring the model to self-verify. Tasks without a verifier accept any non-error result.
- **`think`**: Raw chain-of-thought reasoning. **Ephemeral** — fed to the reflection pass after the action executes, then discarded. The reflection pass distills it into a note that persists. The model doesn't need to be disciplined about what it thinks; the substrate handles compression.
- **`plan!`**: Propose or update the plan (list of `(item . status)` pairs). The agent owns plan status — it decides when items are done, not the driver.

### `begin` Semantics

`(begin action ...)` executes actions sequentially, left to right. **On failure, execution stops** — remaining actions are not attempted. The partial state (successfully executed actions) is committed, and the failed action's error becomes `last-result`. This avoids both silent partial failures and unnecessary rollbacks.

Example: `(begin (load 'linalg/matrix-decomp) (eval (matrix-eigen M)))` — if the load fails, the eval is skipped and the agent sees the load error next step.

### Reflection Failure

If the reflection call fails (API error, timeout, garbage output), the driver falls back to a mechanical note: `"Step N: [action-type] on [target] → [ok/error]"`. The run continues. Reflection is best-effort — a failed summary is worse than a terse one, but not a fatal error.

### The eval Escape Hatch

A concern raised in review: models may default to `(eval (begin ...))` for everything, making the structured actions decorative. This is mitigated by **SFT before RL** — the model is fine-tuned on training data (fold-zxvt) that demonstrates structured action patterns. The training distribution IS the structured actions. `eval` stays available for genuinely novel computation, but the model's default behavior is to search → inspect → load → use.

### Why Not Raw Scheme?

The action language is a restricted DSL, not full Scheme. This is deliberate:

- **Parseable with `read` + one `case` dispatch.** No markdown fence detection, no regex.
- **Each action type has its own telemetry.** We know exactly what the model is doing at every step.
- **Desire paths become visible.** If the model uses `eval` for something that could be a structured action, that's a signal to add a new action type.
- **Bracket mismatches are isolated.** Only `eval` expressions can have complex nesting. The other actions are flat.

---

## Step Lifecycle

Each logical step has two phases: **act** and **reflect**.

### Typical Flow

**Step 0:** State has no plan, just task + input.

*Act:*
```scheme
(begin
  (think "Let me look at the input to understand what I'm working with.")
  (retrieve 'input))
```

*Reflect:* Driver sends raw thought + retrieval result to summarizer.
→ Note: `"Input is 50x50 symmetric real-valued matrix"`

**Step 1:** Agent sees note about input, no plan yet.

*Act:*
```scheme
(begin
  (think "Symmetric matrix — eigendecomposition is the right approach.
          The lattice probably has tools for this.")
  (plan! ((search-eigen    . pending)
          (load-decomp     . pending)
          (compute-eigen   . pending)
          (sort-and-submit . pending)))
  (search "eigenvalue decomposition symmetric"))
```

*Reflect:* → Note: `"linalg skill (0.87) has matrix-decomp with matrix-eigen — direct decomposition"`

**Step 2:** Agent sees search note, `search-eigen` still pending (agent owns status).

*Act:*
```scheme
(begin
  (plan! ((search-eigen . done)
          (load-decomp  . pending)
          (compute-eigen . pending)
          (sort-and-submit . pending)))
  (load 'linalg/matrix-decomp))
```

*Reflect:* → Note: `"matrix-decomp loaded successfully, matrix-eigen now available"`

**Step 3:** Agent sees module loaded, `compute-eigen` is next.

*Act:*
```scheme
(begin
  (eval (sort < (matrix-eigen (retrieve 'input))))
  (plan! ((search-eigen . done) (load-decomp . done)
          (compute-eigen . done) (sort-and-submit . pending))))
```

*Reflect:* → Note: `"Eigenvalues computed: 50 values, all positive, sorted ascending"`

**Step 4:** Agent sees computation succeeded, last item.

*Act:* `(submit (retrieve 'step-3-result))`

---

## Driver Responsibilities

The driver (`rlm-drive`) is simple but does real work:

1. **Execute actions** — dispatch on action type, handle errors
2. **Update state** — write observations, update plan status, manage env
3. **Render HUD** — serialize state for the model's context window
4. **Record trajectory** — store each (state, action, observation) triple in CAS
5. **Enforce limits** — fuel accounting, max steps, timeout
6. **Select agent** — the agent is a parameter; driver can swap models per-step

### Swappable Agents

Because the agent is a pure function `State → Action`, different models can handle different steps:

- **Haiku-class**: search, inspect, plan creation (cheap, structured)
- **Sonnet-class**: eval, computation, complex reasoning
- **Opus-class**: recovery from errors, replanning, multi-skill composition

The driver picks based on the current state and what the plan calls for.

### Error Handling

Errors are observations, not exceptions. If `(load 'linalg/nonexistent)` fails, the driver writes:

```scheme
(observations
  ...
  (3 load 'linalg/nonexistent → error "Module not found"))
```

The plan item stays `pending`. Next invocation, the agent sees the failure and adjusts. No special error-handling machinery — the state just tells the truth.

---

## Two Transports

### MCP (for Claude, tool-use models)

Each action type becomes an MCP tool:

```json
{"name": "lattice_search", "parameters": {"query": "string"}}
{"name": "lattice_inspect", "parameters": {"skill": "string"}}
{"name": "fold_eval", "parameters": {"code": "string"}}
{"name": "submit_answer", "parameters": {"value": "string"}}
```

The model uses native tool-calling. No parsing needed.

### S-expression text (for local vLLM models)

The model outputs `(search "eigenvalue")` as plain text. Parsed with `read`. One function call. Falls back gracefully — if `read` fails, the raw text becomes a `(think ...)` observation and the agent can retry.

Same semantics, different wire format. The driver doesn't care which transport delivered the action.

### Parse Failure Safety

When the S-expression reader fails on model output, the fuzzy fallback wraps the text as `(think "...")`. This is safe — the reflection pass distills it into a note, and the agent retries next step. But the fuzzy parser also attempts to extract the first balanced `(...)` from noisy output. This extraction has a risk: partially matched output could produce a syntactically valid but semantically wrong action (e.g., extracting `(load 'linalg)` from a sentence like "I should (load 'linalg) next").

Mitigation: the fuzzy extractor runs `read` on the candidate, then validates it against the action grammar (known action form, correct arity). If validation fails, the entire output falls back to `(think "...")`. The parser never guesses at action intent — it either confidently parses a well-formed action or treats the whole thing as thinking.

All parse failures are logged to telemetry with the raw output, parsed candidate, and failure reason. This feeds desire-path analysis — persistent parse failures on a specific model indicate a prompt or format issue.

---

## Desire Paths

Every action is logged with the model ID. Over time, patterns emerge:

- **Frequent `eval` patterns** → candidates for new structured actions
- **Common errors** → candidates for rewrite rules (per-model, stored in CAS)
- **Name mismatches** → `(eval (matrix-eigenvalues M))` fails → fuzzy-match suggests `matrix-eigen` → pave a rewrite
- **Arg-order swaps** → `(eval (sort list pred))` → known Chez gotcha → transparent rewrite to `(sort pred list)`

The substrate adapts to the model. Different models get different rewrite profiles. The profiles are content-addressed and diffable.

### Rewrite Rule Governance

Desire-path rewrites are powerful but can silently change semantics. A rewrite that "helpfully" swaps argument order could mask a genuine bug in the model's understanding. Governance rules:

1. **Rewrites are transparent.** Every rewrite is logged in telemetry: original expression, rewrite rule applied, result. The agent doesn't see the rewrite (it would confuse the model), but the trajectory record preserves both versions.
2. **Rewrites are reversible.** Stored in CAS with model ID, creation date, and activation count. Unused rewrites can be pruned. High-activation rewrites are candidates for SFT integration (teach the model the correct form).
3. **Rewrites are conservative.** Only apply to well-understood patterns: known arg-order swaps, common name mismatches (verified by fuzzy match score > threshold), and known Chez idiom translations. No semantic rewrites (e.g., replacing one algorithm with another). When in doubt, let the error propagate — the agent learns more from seeing the failure.
4. **Periodic audit.** Rewrites accumulate. Periodically review the active set per model — are they still needed? Has the model learned the correct form via SFT? Deactivate rewrites that are no longer triggered.

---

## What We Keep From the Paper

- **Iterative refinement** — observe results, adjust approach. This is genuinely valuable.
- **Code as reasoning** — computation, not just natural language. Aligned with the Fold.
- **Bounded execution** — fuel semantics, max steps. Good containment for Fold-native models.
- **CAS trajectory recording** — every step is a block. Provenance is free.

## What We Drop

| Paper artifact | Replacement |
|---|---|
| Chat message roles | Structured state (HUD) |
| Markdown code fences | S-expression action language |
| Sliding window context | CAS-backed state with smart compression |
| Repeated tool docs per step | Action grammar is the interface |
| `rlm-env-put!`/`get` | Auto-stored eval results + `store`/`retrieve` |
| `DONE` markers / magic keys | `(submit value)` action |
| Single model identity | Swappable agent parameter |
| Fingerprint loop detection | Semantic loop detection (plan-status + action-type fingerprint) |

---

## Relation to Training Data (fold-zxvt)

Training examples should be (state, action) pairs in this format — not chat transcripts. Each example teaches: "given this HUD, here's the right move." This is simpler to learn than "given this conversation history, here's a markdown response containing code."

The desire-path telemetry generates training data automatically: every real run produces (state, action, outcome) triples stored in CAS.

---

## Open Questions

1. **Plan granularity** — Should `plan!` items be free-form strings or structured (action-type + target)? Free-form is more flexible; structured enables programmatic validation. Current lean: free-form, since the agent owns plan status anyway.

2. **Retrieve semantics inside eval** — When the model writes `(eval (matrix-eigen (retrieve 'input)))`, does `retrieve` resolve before sending to IPC, or is it a runtime call? Current `expand-env-gets` does pre-expansion; v2 should keep this pattern. Large values need pass-by-reference into the IPC worker (bind to a variable in worker scope, not inline into the code string).

3. **Namespace clashes in lazy-loading** — If we ever pursue auto-loading modules on undefined reference, clashes between skills (e.g., `polynomial` exists in both `algebra` and `numeric`) need a resolution strategy. Tabled for now.

4. **Sub-agent spawning** — Current `rlm-spawn` creates a child loop with sliced env. In v2, a sub-agent is a nested `rlm-drive` with a sub-state. The child's final result becomes the parent's `last-result`, and the reflection pass summarizes it into a note.

5. **Semantic loop detection** — State technically changes every step (step counter, new notes). Need to detect when the agent is *logically* stuck — e.g., repeated search queries, same plan items staying pending. Fingerprint the (plan-status, last-action-type) pair, not just the raw state.

### Resolved Questions

- **Observation compression** → Replaced by the notes model. No separate observation history to compress. `last-result` shows the most recent action's full result; everything older is captured in the notes log via the reflection pass. Notes are one line each — they don't need compression.

- **Chain-of-thought persistence** → Raw CoT is ephemeral. The reflection pass (separate LLM call, same model for now, cheaper model later) distills each step's thinking into a note. Notes persist. This gives CoT's reasoning benefit without its token cost.

- **eval escape hatch dominance** → Mitigated by SFT. The model is trained on structured action traces before RL. The training distribution is the structured actions, so `eval` stays an escape hatch rather than the default.

- **Notes accumulation vs bounded context** → Adaptive compaction. Notes grow freely during short tasks. When they exceed a configurable fraction of context budget, a plan-aware compaction pass runs. Nothing is permanently lost — compacted notes move to the episodic log (CAS). See HUD Design Principle #4.

- **Truth-maintenance for stale notes** → Temporal ordering + compaction. Later notes take precedence. The compactor sees the full note sequence and can drop contradicted notes. No separate retraction mechanism needed. See HUD Design Principle #5.

- **Parse failure safety** → Fuzzy parser validates extracted S-expressions against the action grammar (known form, correct arity) before accepting. Falls back to `(think "...")` on any uncertainty. All failures logged to telemetry. See Parse Failure Safety section.

- **Rewrite rule governance** → Rewrites are transparent, reversible, conservative, and periodically audited. See Rewrite Rule Governance section.

- **Trajectory quality for SFT** → Not all trajectories are training data. Filtering on: success + verifier, efficiency threshold (2× expected steps), action diversity (flag >70% eval), no loop steps, reflection quality. See Training Data Pipeline section.

- **Submit verification** → Optional verifier hook registered at task creation. Machine-checkable correctness criteria. Rejected submits become failed observations, run continues. Tasks without verifiers accept any non-error result.

---

## Implementation Path

1. **Define state and action types** in `lattice/pipeline/rlm2.ss` (pure)
2. **Build the HUD renderer** in `lattice/pipeline/rlm2-hud.ss` (pure — state → string)
3. **Build the driver** in `boundary/pipeline/rlm2-drive.ss` (impure — act/reflect loop, action execution)
4. **Build the reflection pass** — same model, separate call, distills (thought + observation) → note
5. **Port action execution** from existing `execute-code` handlers
6. **Telemetry + desire-path logging** from day one
7. **Benchmark** on OOLONG / OOLONG-Pairs against v1 to validate
8. **Add MCP transport** alongside S-expression text transport (future)
9. **Swap summarizer to cheaper model** once note quality is validated (future)

### Training Data Pipeline (fold-zxvt)

Training examples are (state, action) pairs for the act phase AND (thought + observation, note) pairs for the reflection phase. Two skills, same training run. The SFT teaches both "given this HUD, what's the right move" and "given this reasoning + result, what's the key insight."

### Trajectory Quality Filtering

Not all trajectories make good training data. A run that succeeds in 4 clean steps is high-signal. A run that flails for 30 steps and stumbles into an answer teaches bad habits. Quality criteria for trajectory inclusion in SFT:

- **Success required.** Only completed runs with verified answers (via submit verifier). Failed runs go to a separate error-analysis corpus.
- **Efficiency threshold.** Runs exceeding 2× the expected step count for a task class are downweighted or excluded. Expected counts are derived from the task benchmark suite.
- **Action diversity.** Runs dominated by `eval` (>70% of actions) are flagged — they teach eval-heavy behavior. Ideal training runs demonstrate the structured action vocabulary.
- **No loop steps.** Steps where the driver detected semantic looping are excluded from the training pairs (the state-before-loop is useful, but the repeated action is noise).
- **Reflection quality.** Notes that are just "Step N succeeded" (mechanical fallback) don't teach good summarization. Training pairs for the reflection skill require substantive notes.
