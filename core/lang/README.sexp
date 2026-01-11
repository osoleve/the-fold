((name "lang")
 (purpose "Language core - parsing, evaluation, compilation, and Rust codegen")
 (description "The parsing, evaluation and compilation pipeline for The Fold.
Includes parser combinators, spanned parsing, Fold syntax parser,
primitive dispatch, pure evaluation, type-directed evaluation,
normalization by evaluation (NbE), module system, and Rust code generation
for accelerating Layer 1 primitives via FFI.")
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
   (index.ss "Module index and discovery")
   ;; Rust codegen subsystem
   (rust-mapping.ss "Scheme to Rust type mapping")
   (rust-codegen.ss "Rust IR serialization and Scheme->IR translation")
   (rust-compile.ss "Rust compilation bridge (rustc invocation)")))
 (tests
  ((test-parse.ss "Parser combinator tests")
   (test-span.ss "Position-aware parsing tests")
   (test-fold-parse.ss "Fold parser tests")
   (test-par-pseq.ss "Parallel/sequential evaluation tests")
   (test-rust-codegen.ss "Rust codegen tests (40 tests)")))
 (dependencies (base types))
 (evaluation-strategy "call-by-value")
 (documentation "docs/language-reference.md")
 (rust-codegen
  (status "Layer 1 complete")
  (supported-primitives
   (arithmetic add sub mul div mod neg abs)
   (comparison lt? le? gt? ge? eq?)
   (logical and or not)
   (bitwise bitand bitor bitxor bitnot shl shr)
   (math sqrt sin cos tan log floor ceiling round expt))
  (remaining-work
   (fold-49ht "Full closure/recursion support in translator")
   (fold-4s4q "Type-aware result emission (bool->f64)")
   (fold-id7k "Crate integration for TestResult")
   (fold-jppr "Division-by-zero protection")
   (fold-ulzh "Variadic primitive support"))))
