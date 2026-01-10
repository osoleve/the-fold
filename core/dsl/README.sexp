((name "dsl")
 (purpose "Domain-specific language infrastructure")
 (description "Tools for building and embedding domain-specific languages
in The Fold. Provides quasiquotation for syntax templates, the
Tagless Final pattern for extensible DSL interpreters, and modular
composition strategies for combining independent DSLs.")
 (modules
  ((quasi.ss "Quasiquotation and syntax templates (`expr, ,expr, ,@expr)")
   (tagless.ss "Tagless Final DSL pattern for embedded languages")
   (compose.ss "Modular DSL composition: Data Types à la Carte, effect composition, tagless composition")))
 (dependencies (base fp/control))
 (key-concepts
  ((quasiquotation "Pattern-based code generation with holes")
   (tagless-final "DSL as type class - programs parameterized by algebra")
   (dictionary-passing "Interpreters are algebra dictionaries")
   (data-types-a-la-carte "Combine functors via coproduct for modular DSLs")
   (effect-composition "Stack and compose effect handlers")
   (functor-rows "Type-safe injection into coproducts")
   (interface-constraints "Verify dictionaries satisfy required operations"))))
