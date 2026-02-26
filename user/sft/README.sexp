((name . "sft")
 (description . "Generative grammar DSL for SFT prompt diversity.
  Replaces data/sft_prompt_diversity.py with composable, seed-deterministic
  grammar expansion using The Fold's PCG PRNG infrastructure.")

 (files
  (("grammar.ss"      . "Data types: make-grammar, grammar-merge, grammar-ref, grammar-rules")
   ("expand.ss"       . "Interpreter: expand, expand-body, expand-to-string, expand-n, collapse-blank-lines")
   ("base-grammar.ss" . "Shared SFT grammar (*sft-base-grammar*), make-sft-context, verify decomposition")
   ("analyze.ss"      . "Static analysis: grammar-reachable, grammar-dead-rules, grammar-coverage")
   ("generate.ss"     . "Batch JSONL generation: reads pre-diversification records, emits diversified output")
   ("test-grammar.ss" . "76 tests covering all combinators, inheritance, analysis, edge cases")))

 (architecture
  . "Grammar → Expand → Base-Grammar form a pipeline:
     grammar.ss defines the data type (tagged alist),
     expand.ss interprets it with explicit RNG threading,
     base-grammar.ss provides the shared prompt template.
     analyze.ss does static analysis (dead rules, coverage).
     generate.ss is the I/O harness for batch processing.")

 (combinators
  ((text      "(\"literal\")"          "String literals expand to themselves")
   (ref       "(ref rule-name)"        "Expand a named rule from the grammar")
   (slot      "(slot key)"             "Look up key in context alist, expand to its value")
   (seq       "(seq a b c ...)"        "Concatenate expansions left to right")
   (alt       "(alt a b c ...)"        "Uniform random choice among alternatives")
   (weighted  "(weighted (w1 a) ...)"  "Weighted random choice; weights must sum positive")
   (maybe     "(maybe prob body)"      "Include body with probability prob, else empty string")
   (when      "(when key body)"        "Include body only if key is truthy in context")
   (case-on   "(case-on key (v1 b1) (v2 b2) ... (else b))"
                                       "Switch on context value; supports string/symbol matching")
   (join      "(join sep items-key)"   "Join context list with separator string")))

 (rng-model
  . "Explicit pair-passing: (expand grammar rule ctx rng) → (string . rng').
     No hidden state. Uses lattice/random/prng PCG via run-state.
     expand-to-string wraps this with (make-pcg seed 0) for convenience.")

 (usage
  ((repl
    . "(load \"user/sft/expand.ss\")
       (load \"user/sft/base-grammar.ss\")

       (define ctx (make-sft-context \"spec_to_code\" \"implementation\"
                     \"vec-add\" \"Implement vec-add.\"))
       (expand-to-string *sft-base-grammar* 'prompt ctx 42)
       ;; → deterministic diversified prompt string

       ;; Multiple variants:
       (expand-n *sft-base-grammar* 'prompt ctx 0 5)
       ;; → list of 5 prompts at seeds 0..4")

   (batch
    . "scheme --script user/sft/generate.ss input.jsonl output-dir/

       Input: JSONL with prompt_body (raw task text) + context fields
       Output: all.jsonl, train.jsonl, eval.jsonl with diversified prompt field

       Environment variables:
         SFT_EVAL_RATIO  — eval split fraction (default: 0.05)
         SFT_GRAMMAR     — path to grammar overlay file (optional)")

   (analysis
    . "(load \"user/sft/analyze.ss\")

       ;; Check for unreachable rules
       (grammar-dead-rules *sft-base-grammar* 'prompt)  ;; → ()

       ;; Coverage sampling — which rules get hit over N expansions
       (grammar-coverage *sft-base-grammar* 'prompt ctx 42 100)
       ;; → ((prompt . 100) (task-prefix . 100) ...)")

   (custom-grammar
    . ";; Define a module-specific overlay
       (define *linalg-overlay*
         (make-grammar
           '((task-suffix . (alt
               \"Ensure numerical stability in edge cases.\"
               \"Vectorize where the API permits.\")))))

       ;; Merge: child rules shadow parent
       (define *linalg-grammar*
         (grammar-merge *sft-base-grammar* *linalg-overlay*))

       (expand-to-string *linalg-grammar* 'prompt ctx 42)")))

 (input-schema
  . "JSONL records with fields:
       id              — unique sample identifier (used for seed derivation)
       family          — spec_to_code | translation | bugfix | composition | cloze | type_inhabit | type_sig | meta_template | meta_protocol
       category        — implementation | transpile | translation | repair | debugging | usage | analysis
       source_function — target function name
       prompt_body     — raw task description (pre-diversification)
       ground_truth    — expected correct output
     Optional:
       difficulty, source_module, source_test, verify_expr,
       tags, available_functions, known_issue, split, prompt
     Notes:
       - If prompt_body is missing, generate.ss falls back to prompt.
       - If split is already train/eval, generate.ss preserves it;
         otherwise it derives split from seed/eval-ratio.")

 (output-schema
  . "JSONL records matching data/*/all.jsonl:
       id, family, category, difficulty, source_module, source_test,
       source_function, prompt_body, prompt (diversified), ground_truth,
       verify_expr, tags, split (train|eval)")

 (seed-derivation
  . "id->seed: SHA256(id) → first 8 hex chars → 32-bit integer.
     Same ID always produces same prompt. Fully reproducible.
     Split derivation is only used when input split is absent.")

 (migration
  . "Replaces data/sft_prompt_diversity.py.
     Key differences:
       - Grammar is data (s-expressions), not Python string interpolation
       - Probabilistic gates (maybe, weighted) instead of flat selection
       - Grammar inheritance via grammar-merge for per-module overlays
       - Verify-expr auto-decomposition extracts individual checks
       - Blank-line collapse prevents phantom paragraphs from maybe gates
     The grammar encodes the same variation dimensions as the Python system
     (task-prefix × category-hint × verify-frame × family-overlay × task-suffix)
     but with richer combinatorics and extensibility.
     Composition prompts intentionally avoid inlining verify_expr snippets
     to reduce answer-oracle leakage.")

 (dependencies
  (("core/lang/module.ss"   . "Module system bootstrap (via grammar.ss)")
   ("lattice/random/prng"   . "PCG PRNG: make-pcg, random-int-range, random-float")
   ("lattice/fp/state"      . "State monad: run-state")
   ("core/base/sha256.ss"   . "SHA256 for seed derivation (generate.ss only)")
   ("boundary/io/json.ss"   . "JSON parse/serialize (generate.ss only)"))))
