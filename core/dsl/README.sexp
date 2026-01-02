((name "dsl")
 (purpose "Domain-specific language infrastructure")
 (description "Tools for building and embedding domain-specific languages
in The Fold. Provides quasiquotation for syntax templates and the
Tagless Final pattern for extensible, efficient DSL interpreters.")
 (modules
  ((quasi.ss "Quasiquotation and syntax templates (`expr, ,expr, ,@expr)")
   (tagless.ss "Tagless Final DSL pattern for embedded languages")))
 (dependencies (base lang))
 (key-concepts
  ((quasiquotation "Pattern-based code generation with holes")
   (tagless-final "DSL as type class - programs parameterized by algebra")
   (dictionary-passing "Interpreters are algebra dictionaries"))))
