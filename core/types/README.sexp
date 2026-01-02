((name "types")
 (purpose "Type system implementation")
 (description "Complete type system for The Fold including base types,
higher-kinded types, dependent types (Pi and Sigma), and bidirectional
type inference. The type system ensures code correctness at compile time.")
 (modules
  ((types.ss "Base and compound types (Int, Bool, ->, x, +, List)")
   (kinds.ss "Higher-kinded types (* -> *, etc.)")
   (dep-types.ss "Dependent types: Pi (forall), Sigma (exists), Vec, Matrix")
   (infer.ss "Bidirectional type inference")
   (dep-infer.ss "Dependent type inference")
   (resolve.ss "Type class resolution")
   (annotate.ss "AST type annotation")))
 (dependencies (base))
 (key-concepts
  ((bidirectional "Types flow both up (inference) and down (checking)")
   (dependent-types "Types that depend on values (e.g., Vec n a)")
   (universes "Type : Type1 : Type2 : ... hierarchy"))))
