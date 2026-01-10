((name "lang")
 (purpose "Language core - parsing, evaluation and compilation")
 (description "The parsing, evaluation and compilation pipeline for The Fold.
Includes parser combinators, spanned parsing, Fold syntax parser,
primitive dispatch, pure evaluation, type-directed evaluation,
normalization by evaluation (NbE), and the module system.")
 (modules
  ((parse.ss "Parsec-style parser combinators")
   (span.ss "Position-aware parser combinators with source spans")
   (fold-parse.ss "Fold S-expression syntax parser")
   (prim.ss "Primitive function dispatcher")
   (eval.ss "Pure evaluator")
   (typed-eval.ss "Type-directed evaluation")
   (compile.ss "Compilation pipeline")
   (nbe.ss "Normalization by evaluation")
   (module.ss "Module system")
   (index.ss "Module index and discovery")))
 (tests
  ((test-parse.ss "Parser combinator tests")
   (test-span.ss "Position-aware parsing tests")
   (test-fold-parse.ss "Fold parser tests")
   (test-par-pseq.ss "Parallel/sequential evaluation tests")))
 (dependencies (base types))
 (evaluation-strategy "call-by-value")
 (documentation "docs/language-reference.md"))
