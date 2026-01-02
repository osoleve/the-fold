((name "lang")
 (purpose "Language core - evaluation and compilation")
 (description "The evaluation and compilation pipeline for The Fold.
Includes primitive dispatch, pure evaluation, type-directed evaluation,
normalization by evaluation (NbE), and the module system.")
 (modules
  ((prim.ss "Primitive function dispatcher")
   (eval.ss "Pure evaluator")
   (typed-eval.ss "Type-directed evaluation")
   (compile.ss "Compilation pipeline")
   (nbe.ss "Normalization by evaluation")
   (module.ss "Module system")
   (index.ss "Module index and discovery")))
 (dependencies (base types))
 (evaluation-strategy "call-by-value"))
