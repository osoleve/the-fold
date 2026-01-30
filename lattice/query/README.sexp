((name "query")
(purpose "Query system for content-addressed blocks")
(description "Query infrastructure for The Fold's content-addressed block\nsystem. Provides multiple query paradigms: tag-based querying, Datalog-style\nDSL with predicates/joins/projections, pattern matching with variable binding,\nmulti-pattern string matching via Aho-Corasick, and inline metadata parsing.")
(modules 
  ((query-dsl.ss "Datalog-style declarative query language for blocks")
  (query-patterns.ss "Pattern matching with variable binding and joins")
  (patterns-parse.ss "Inline @-tag parsing for metadata extraction")
  (aho-corasick.ss "Multi-pattern string matching algorithm")))
(tests 
  ((test-query-dsl.ss "100+ tests for query-dsl.ss")
  (test-query-patterns.ss "~30 tests for query-patterns.ss")
  (test-patterns-parse.ss "~30 tests for patterns-parse.ss")
  (test-aho-corasick.ss "~20 tests for aho-corasick.ss")))
(dependencies (base queue set dict)))
