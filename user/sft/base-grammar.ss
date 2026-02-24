;;; user/sft/base-grammar.ss — Shared SFT prompt grammar
;;;
;;; Encodes the same variation dimensions as data/sft_prompt_diversity.py
;;; but as composable grammar rules.

(load "user/sft/expand.ss")

;;; ============================================================
;;; Base grammar — 4 task families, ~5 variation dimensions
;;; ============================================================

(define *sft-base-grammar*
  (make-grammar
    `(;; --- Entry point ---
      (prompt . (seq (ref task-prefix) "\n\n"
                     (slot prompt-body) "\n\n"
                     (ref category-hint)
                     (maybe 0.64 (seq "\n\n" (ref verify-frame)))
                     (maybe 0.50 (seq "\n\n" (ref family-overlay)))
                     "\n\n" (ref task-suffix)))

      ;; --- Task mode prefixes (3 per family) ---
      (task-prefix . (case-on family
        ("spec_to_code" (alt
          "Task mode: implement the API contract in canonical Fold Scheme."
          "Task mode: behavior-first function implementation."
          "Task mode: complete the target function to match module semantics."))
        ("translation" (alt
          "Task mode: semantic translation into idiomatic Fold Scheme."
          "Task mode: preserve behavior while translating syntax and naming."
          "Task mode: convert source-language logic to Fold-native form."))
        ("bugfix" (alt
          "Task mode: minimal patch bug repair."
          "Task mode: localize and fix the behavioral defect."
          "Task mode: surgical correction with unchanged intended API."))
        ("composition" (alt
          "Task mode: compose existing APIs into one expression."
          "Task mode: small integration task across module primitives."
          "Task mode: solve by expression synthesis over available functions."))
        (else "")))

      ;; --- Category hints (2 per category) ---
      (category-hint . (case-on category
        ("implementation" (alt
          "Focus on correctness first; keep structure straightforward."
          "Match the stated contract exactly, including edge cases."))
        ("transpile" (alt
          "Translate semantics faithfully; adapt names to the requested target."
          "Keep behavior exact while adopting Fold syntax."))
        ("translation" (alt
          "Translate semantics faithfully; adapt names to the requested target."
          "Keep behavior exact while adopting Fold syntax."))
        ("repair" (alt
          "Apply the smallest coherent fix that restores expected behavior."
          "Repair the defect without broad refactoring."))
        ("debugging" (alt
          "Apply the smallest coherent fix that restores expected behavior."
          "Repair the defect without broad refactoring."))
        ("usage" (alt
          "Compose from existing module functions where appropriate."
          "Solve with an expression that can be evaluated directly."))
        (else "")))

      ;; --- Verify frame (included ~64% of the time via maybe gate) ---
      (verify-frame . (case-on family
        ("spec_to_code" (alt
          (when has-verify (seq "Behavior examples your implementation must satisfy:\n```scheme\n" (slot verify-expr) "\n```"))
          (when has-verify (seq "Acceptance checks to pass:\n```scheme\n" (slot verify-expr) "\n```"))))
        ("translation" (alt
          (when has-verify (seq "Preserve these observable behaviors in translation:\n```scheme\n" (slot verify-expr) "\n```"))
          (when has-verify (seq "Semantic checks for the translated function:\n```scheme\n" (slot verify-expr) "\n```"))))
        ("bugfix" (alt
          (when has-verify (seq "Regression checks after fixing the bug:\n```scheme\n" (slot verify-expr) "\n```"))
          (when has-verify (seq "Keep the original function signature unchanged.\n\nRegression checks after fixing the bug:\n```scheme\n" (slot verify-expr) "\n```"))))
        ("composition" (alt
          (when has-verify (seq "Target properties for your expression:\n```scheme\n" (slot verify-expr) "\n```"))
          (when has-verify (seq "Expression-only output is required (no helper definitions).\n\nTarget properties for your expression:\n```scheme\n" (slot verify-expr) "\n```"))))
        (else "")))

      ;; --- Family-specific overlays ---
      (family-overlay . (case-on family
        ("bugfix" (ref bugfix-overlay))
        ("composition" (ref composition-overlay))
        (else "")))

      (bugfix-overlay . (when has-verify
        (seq (ref bug-report-line) "\n\n"
             "Expected behavior after patch:\n```scheme\n"
             (slot verify-expr) "\n```\n\n"
             "Actual behavior: the provided implementation fails the expectation above.")))

      (bug-report-line . (alt
        (when known-issue (seq "Bug report summary: " (slot known-issue)))
        "Bug report summary: the current implementation violates required behavior."))

      (composition-overlay . (when has-available-functions
        (seq "Available functions you may compose:\n"
             (slot available-functions-formatted) "\n\n"
             "Prefer using this API inventory directly instead of re-implementing behavior.")))

      ;; --- Task suffixes (3 per family) ---
      (task-suffix . (case-on family
        ("spec_to_code" (alt
          "Prioritize edge-case behavior when specified by the contract."
          "Keep the implementation idiomatic and dependency-aware."
          "Ensure the definition is production-ready for module integration."))
        ("translation" (alt
          "Semantic equivalence is more important than token-level similarity."
          "Preserve boundary conditions and error behavior from the source snippet."
          "Prefer Fold conventions while keeping the same observable behavior."))
        ("bugfix" (alt
          "Keep unrelated logic unchanged unless needed for correctness."
          "Fix root-cause behavior rather than masking symptoms."
          "Retain public contract while repairing the implementation fault."))
        ("composition" (alt
          "Favor direct API use over ad-hoc reimplementation."
          "Keep the answer as a concise executable expression."
          "Use provided module operations to satisfy the requested behavior."))
        (else ""))))))

;;; --- Context builder ---

(define (make-sft-context family category function-name prompt-body
                          . kwargs)
  (doc 'description "Build context alist for SFT prompt expansion")
  (let* ([verify-expr (if (>= (length kwargs) 1) (car kwargs) #f)]
         [available-fns (if (>= (length kwargs) 2) (cadr kwargs) #f)]
         [known-issue (if (>= (length kwargs) 3) (caddr kwargs) #f)]
         [ctx `((family . ,family)
                (category . ,category)
                (function-name . ,function-name)
                (prompt-body . ,prompt-body)
                (has-verify . ,(if verify-expr #t #f))
                (verify-expr . ,(or verify-expr ""))
                (has-available-functions . ,(if available-fns #t #f))
                (available-functions-formatted
                 . ,(if available-fns
                        (apply string-append
                          (map (lambda (f) (string-append "- `" f "`\n"))
                               available-fns))
                        ""))
                (known-issue . ,(or known-issue ""))
                (has-known-issue . ,(if known-issue #t #f)))])
    ctx))
