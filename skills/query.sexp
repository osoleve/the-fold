;;; skills/query.sexp — Skill curation file

(skill query
  (description
    "Query DSL for searching and filtering blocks in the content-addressed store.\n    Includes tag extraction, pattern matching, relational joins, Aho-Corasick\n    multi-pattern string matching, and optic-based declarative queries.\n\n    The optic query language uses optics as typed path expressions for navigating\n    data structures, combined with predicate filtering, projection, and aggregation\n    for building composable, declarative queries.\n\n    The declarative query-macro DSL provides SQL-like syntax:\n      (from world world-each-body\n        (where (>? body-vel-y 0))\n        (select body-pos-lens)\n        (order-by (pluck body-pos-y))\n        (limit 10))")

  (keywords (query dsl search filter tags pattern-matching relational join projection aho-corasick string-search optics optic-query declarative traversal))

  (aliases (search find filter block-query optic-query))

  (concepts (query-infrastructure))

  (modules
    patterns-parse
    query-dsl
    aho-corasick
    query-patterns
    optic-query
    query-macro
  )

  ;; Curated Q&A pairs for LLM context
  (prompts)

  ;; KG/search queries an agent should try
  (suggested-queries)
)
