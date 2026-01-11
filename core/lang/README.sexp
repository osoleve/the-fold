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
   ;; Rust codegen subsystem (v1.0)
   (rust-mapping.ss "Scheme to Rust type mapping")
   (rust-codegen.ss "IR nodes, serialization, fuel costs, autodiff gradients")
   (rust-compile.ss "Crate integration, cargo build commands")))
 (tests
  ((test-parse.ss "Parser combinator tests")
   (test-span.ss "Position-aware parsing tests")
   (test-fold-parse.ss "Fold parser tests")
   (test-par-pseq.ss "Parallel/sequential evaluation tests")
   (test-rust-codegen.ss "Rust codegen tests (235 tests)")))
 (dependencies (base types))
 (evaluation-strategy "call-by-value")
 (documentation "docs/rust-codegen.md")
 (rust-codegen
  (status "v1.0 complete")
  (version "1.0.0")
  (supported-primitives
   (arithmetic add sub mul div mod neg abs)
   (comparison lt le gt ge eq ne)
   (logical and or not)
   (bitwise bitand bitor bitxor bitnot shl shr)
   (math sqrt sin cos tan log floor ceil round min max))
  (supported-types i64 f64 bool u64)
  (features
   (type-safety "Type-specific result structs (I64Result, F64Result, BoolResult, U64Result)")
   (fuel-accounting "Automatic fuel cost computation from IR")
   (error-handling "Division-by-zero protection with runtime error status")
   (variadic-ops "N-ary add, mul, and, or, bitand, bitor, bitxor")
   (crate-integration "Generated modules in shell/ffi/rust-accel/src/generated/")
   (ffi-loader "Dynamic function loading via shell/ffi/rust-loader.ss")
   (autodiff-compat "Gradient formulas documented for reverse-mode AD"))
  (future-work
   (fold-49ht "Full closure/recursion support")
   (eval-integration "Automatic hot-path acceleration")
   (layer-2 "Bytevectors, strings")
   (layer-3 "Crypto primitives"))))
