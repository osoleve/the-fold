;;; lattice/dsl/template/README.sexp — Grammar-Driven Code Construction

(readme
 (title "Template DSL")
 (purpose
  "Build S-expressions via EBNF-like production statements without
   tracking parentheses. Designed for AI-assisted code generation.")

 (concept
  "The AI writes linear statements where holes ($name) are non-terminals.
   Filling a hole with a value containing holes propagates those holes.
   Once all holes are filled, the template compiles to an S-expression.

   Multi-token statements get implicit parentheses:
     define $sig $body → (define $sig $body)
     $cond := = n 0    → (= n 0)")

 (usage
  (example "Batch Mode (Recommended)"
   "Chain template + fills in one command with ---:

    (tp-batch \"
      define (qs lst) $body
      --- $body := if $cond $then $else
      --- $cond := null? lst
      --- $then := '()
      --- $else := append (qs (filter $pred (cdr lst)))
                          (cons (car lst) (qs (filter $pred2 (cdr lst))))
      --- $pred := lambda (x) (< x (car lst))
      --- $pred2 := lambda (x) (>= x (car lst))
    \")
    ;; → (define (qs lst) (if (null? lst) '() (append ...)))")

  (example "Interactive Mode"
   "Build incrementally with hole propagation:

    (tp-parse \"define $sig $body\")
    (tp-parse \"$sig := factorial n\")
    (tp-parse \"$body := if $cond $then $else\")
    (tp-parse \"$cond := = n 0\")
    (tp-parse \"$then := 1\")
    (tp-parse \"$else := * n (factorial (- n 1))\")
    (ts-compile)
    ;; → (define (factorial n) (if (= n 0) 1 (* n (factorial (- n 1)))))"))

 (files
  (file "template.ss" "Core engine: holes, templates, filling, compilation")
  (file "test-template.ss" "25 unit tests")
  (file "manifest.sexp" "Skill metadata"))

 (shell-tools
  (file "boundary/tools/template-session.ss"
        "Stateful session manager with undo support")
  (file "boundary/tools/template-parser.ss"
        "Linear syntax parser for EBNF-like statements"))

 (api
  (section "Hole Detection"
   (fn "hole?" "Any → Bool" "Check if value is a $-prefixed symbol")
   (fn "hole-name" "Symbol → Symbol" "Extract name: $foo → foo")
   (fn "find-holes" "Expr → (List Symbol)" "Find all holes in expression"))

  (section "Templates"
   (fn "new-template" "Expr → Template" "Create template from expression")
   (fn "template?" "Any → Bool" "Predicate")
   (fn "template-expr" "Template → Expr" "Get underlying expression")
   (fn "template-holes" "Template → (List Symbol)" "Get unfilled holes")
   (fn "template-complete?" "Template → Bool" "No holes remaining?"))

  (section "Filling"
   (fn "fill-hole" "Template × Symbol × Expr → Template"
       "Fill a hole, propagating new holes from value")
   (fn "fill-holes" "Template × Alist → Template"
       "Fill multiple holes from association list"))

  (section "Compilation"
   (fn "compile-template" "Template → Expr"
       "Extract S-expr if complete, else error")
   (fn "try-compile-template" "Template → (Ok Expr) | (Err Holes)"
       "Safe compilation returning result type"))

  (section "Session (boundary/tools/template-session.ss)"
   (fn "ts-start" "Expr → Unit" "Start new session")
   (fn "ts-fill" "Symbol × Expr → Unit" "Fill a hole")
   (fn "ts-holes" "→ (List Symbol)" "Get current holes")
   (fn "ts-undo" "→ Unit" "Revert last fill")
   (fn "ts-compile" "→ Expr" "Compile if complete")
   (fn "ts-show" "→ Unit" "Pretty-print current template")
   (fn "ts-status" "→ String" "Get status string"))

  (section "Parser (boundary/tools/template-parser.ss)"
   (fn "tp-batch" "String → Expr"
       "Parse chained definitions separated by ---. Recommended for AI use.")
   (fn "tp-parse" "String → Unit"
       "Parse line and execute template operation")
   (fn "tp-repl" "→ Unit" "Interactive REPL for template construction")))

 (design-notes
  (note "Batch Chaining"
   "Use tp-batch with --- separators for best AI workflow.
    Each section is a complete definition or $hole := fill.
    The batch parses all sections and returns a begin block.
    Example: 'define f $b --- $b := + 1 2' → (define f (+ 1 2))")

  (note "Implicit Parens Rule"
   "Every multi-token statement gets wrapped in parentheses.
    Single tokens stay as-is. This eliminates outer parentheses
    while preserving explicit nesting within expressions.")

  (note "Hole Propagation"
   "When filling a hole, if the value contains holes, those
    become new holes in the template. This allows incremental
    refinement of structure.")

  (note "AI-First Design"
   "Optimized for AI code generation where tracking parenthesis
    depth across tokens is error-prone. Linear statements are
    easier to generate correctly.")))
