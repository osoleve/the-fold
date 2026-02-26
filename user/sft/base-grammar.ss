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
        ("cloze" (alt
          "Task mode: fill in the missing expression or function."
          "Task mode: structural completion from surrounding context."
          "Task mode: infer the missing piece from module and type context."))
        ("type_inhabit" (alt
          "Task mode: implement a function satisfying the given type signature."
          "Task mode: type-driven implementation — the type IS the spec."
          "Task mode: produce a well-typed inhabitant of the specified signature."))
        ("type_sig" (alt
          "Task mode: predict the type signature from the implementation."
          "Task mode: type inference — read the code, write the contract."
          "Task mode: derive the type annotation matching this function body."))
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
        ("analysis" (alt
          "Read the implementation carefully before writing the type."
          "Consider all code paths and edge cases when inferring the type."))
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
          "Expression-only output is required (no helper definitions)."
          "Use module APIs directly; avoid re-implementing internal logic."))
        ("cloze" (alt
          (when has-verify (seq "Your completion must satisfy:\n```scheme\n" (slot verify-expr) "\n```"))
          (when has-verify (seq "Checks the filled version must pass:\n```scheme\n" (slot verify-expr) "\n```"))))
        ("type_inhabit" (alt
          (when has-verify (seq "Your implementation must pass:\n```scheme\n" (slot verify-expr) "\n```"))
          (when has-verify (seq "Type-level checks:\n```scheme\n" (slot verify-expr) "\n```"))))
        ("type_sig" (alt
          "The type annotation should capture all input and output types."
          "Include parametric type variables where the function is polymorphic."))
        (else "")))

      ;; --- Family-specific overlays ---
      (family-overlay . (case-on family
        ("bugfix" (ref bugfix-overlay))
        ("composition" (ref composition-overlay))
        ("cloze" "")
        ("type_inhabit" "The type signature is your only specification. No natural language hints.")
        ("type_sig" "")
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
        ("cloze" (alt
          "The completion should be consistent with the surrounding module context."
          "Consider types, naming conventions, and peer functions for inference."
          "Fill the gap naturally — the result should look like it was never missing."))
        ("type_inhabit" (alt
          "The implementation must be well-typed and handle edge cases implied by the signature."
          "Produce the simplest correct implementation consistent with the type."
          "Focus on type-directed design — let the signature guide the structure."))
        ("type_sig" (alt
          "Capture the full polymorphic structure of the function."
          "Include all input types, output type, and any type class constraints."
          "The type should be as precise as possible without being overly restrictive."))
        (else ""))))))

;;; --- Verify-expr decomposition ---

(define (decompose-verify-checks verify-str . opts)
  (doc 'description
    "Extract individual check expressions from compound verify-expr.
     Handles (and c1 c2 ...) and (let () defs... (and c1 c2 ...)).
     Returns newline-separated string of up to max-checks assertions.")
  (let ([max-checks (if (pair? opts) (car opts) 2)])
    (guard (ex [else verify-str])
      (let ([expr (read (open-input-string verify-str))])
        (cond
          ;; (and check1 check2 ...)
          [(and (pair? expr) (eq? (car expr) 'and))
           (format-checks (take-up-to max-checks (cdr expr)))]
          ;; (let () body...) or (let* () body...) — extract from last form
          [(and (pair? expr) (memq (car expr) '(let let*)))
           (let ([last-form (last-body-form expr)])
             (if (and (pair? last-form) (eq? (car last-form) 'and))
                 (format-checks (take-up-to max-checks (cdr last-form)))
                 verify-str))]
          [else verify-str])))))

(define (last-body-form let-expr)
  (doc 'description "Extract the last body form from a let/let* expression")
  ;; (let <maybe-name> <bindings> body1 body2 ... bodyN) → bodyN
  (let* ([after-head (cdr let-expr)]  ; skip let/let*
         ;; Skip optional name (named let)
         [rest (if (and (pair? after-head)
                        (symbol? (car after-head)))
                   (cdr after-head)
                   after-head)]
         ;; Skip bindings
         [body (if (pair? rest) (cdr rest) '())])
    (if (pair? body)
        (let loop ([forms body])
          (if (null? (cdr forms))
              (car forms)
              (loop (cdr forms))))
        #f)))

(define (take-up-to n lst)
  (doc 'description "Take up to n elements from lst")
  (let loop ([i 0] [l lst] [acc '()])
    (if (or (>= i n) (null? l))
        (reverse acc)
        (loop (+ i 1) (cdr l) (cons (car l) acc)))))

(define (sexp->string expr)
  (doc 'description "Format s-expression preserving quote shorthand and string quotes")
  (let ([port (open-output-string)])
    (pretty-print expr port)
    ;; pretty-print adds trailing newline; strip it
    (let* ([s (get-output-string port)]
           [n (string-length s)])
      (if (and (> n 0) (char=? (string-ref s (- n 1)) #\newline))
          (substring s 0 (- n 1))
          s))))

(define (format-checks checks)
  (doc 'description "Format check s-expressions as newline-separated strings")
  (let loop ([cs checks] [acc ""])
    (if (null? cs)
        acc
        (let ([s (sexp->string (car cs))])
          (loop (cdr cs)
                (if (string=? acc "")
                    s
                    (string-append acc "\n" s)))))))

;;; --- Context builder ---

(define (make-sft-context family category function-name prompt-body
                          . kwargs)
  (doc 'description "Build context alist for SFT prompt expansion")
  (let* ([verify-expr-raw (if (>= (length kwargs) 1) (car kwargs) #f)]
         [available-fns (if (>= (length kwargs) 2) (cadr kwargs) #f)]
         [known-issue (if (>= (length kwargs) 3) (caddr kwargs) #f)]
         [verify-expr (if (and verify-expr-raw
                               (string? verify-expr-raw)
                               (not (string=? verify-expr-raw "")))
                          (decompose-verify-checks verify-expr-raw)
                          #f)]
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
