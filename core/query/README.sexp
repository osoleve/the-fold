((name "query")
 (purpose "Query system and pattern matching DSL")
 (description "Declarative query combinators and pattern matching for
searching and transforming S-expressions. Includes the Aho-Corasick
algorithm for efficient multi-pattern string matching.")
 (modules
  ((query.ss "Query combinators")
   (query-dsl.ss "Query domain-specific language")
   (query-patterns.ss "Pattern definitions")
   (aho-corasick.ss "Efficient multi-pattern string matching")))
 (dependencies (base)))
