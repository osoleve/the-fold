((name "types")
 (purpose "Type system implementation")
 (description "Complete type system for The Fold including base types,
higher-kinded types, dependent types (Pi and Sigma), type classes with
functional dependencies, full Rank-N polymorphism with impredicativity,
and bidirectional type inference.")
 (modules
  ((types.ss "Base and compound types (Int, Bool, ->, x, +, List)")
   (kinds.ss "Higher-kinded types, type classes, functional dependencies")
   (dep-types.ss "Dependent types: Pi (forall), Sigma (exists), Vec, Matrix")
   (infer.ss "Bidirectional type inference (Hindley-Milner)")
   (dep-infer.ss "Dependent type inference")
   (rank-n.ss "Rank-N polymorphism: subsumption, skolemization, impredicative unification")
   (rank-n-infer.ss "Full Rank-N inference with Quick Look guided instantiation")
   (resolve.ss "Type class resolution")
   (annotate.ss "AST type annotation")))
 (dependencies (base))
 (key-concepts
  ((bidirectional "Types flow both up (inference) and down (checking)")
   (dependent-types "Types that depend on values (e.g., Vec n a)")
   (universes "Type : Type1 : Type2 : ... hierarchy")
   (type-classes "Ad-hoc polymorphism via dictionary passing")
   (rank-n-polymorphism "First-class polymorphic functions, runST-style types")
   (impredicativity "Type variables can unify with polymorphic types")
   (quick-look "Guided instantiation based on argument structure")))
 (type-classes
  ((single-param
    "Standard type classes with one type parameter"
    (Functor "fmap : (a -> b) -> f a -> f b")
    (Applicative "pure, <*> : extends Functor")
    (Monad ">>= : m a -> (a -> m b) -> m b")
    (Contravariant "contramap : (a -> b) -> f b -> f a")
    (Eq "==, /= : a -> a -> Bool")
    (Ord "compare, <, <=, >, >= : extends Eq")
    (Show "show : a -> String")
    (Semigroup "<> : a -> a -> a")
    (Monoid "mempty : extends Semigroup"))
   (multi-param
    "Type classes with multiple parameters (* -> * -> *)"
    (Bifunctor "bimap, first, second")
    (Category "id, compose for morphism types")
    (Profunctor "dimap, lmap, rmap")
    (Arrow "arr, first*, second*, ***, &&& : extends Category"))
   (with-fundeps
    "Multi-param classes with functional dependencies"
    (Convertible "convert : a -> b")
    (Collection "c -> e : empty, insert, member?")
    (MonadReader "m -> r : ask, local")
    (MonadState "m -> s : get, put, modify")
    (MonadWriter "m -> w : tell, listen, pass")))))
