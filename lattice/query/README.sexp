((name "query")
 (purpose "Query system for content-addressed blocks")
 (description "Query infrastructure for The Fold's content-addressed block
system. Provides multiple query paradigms: tag-based querying, Datalog-style
DSL with predicates/joins/projections, pattern matching with variable binding,
multi-pattern string matching via Aho-Corasick, and inline metadata parsing.")
 (modules
  ((query.ss "Tag-based forum post querying"
   (dependencies "patterns-parse.ss" "forum/tools.ss" "boundary/fs.ss")
   (exports find-tagged list-all-tags tag-histogram find-by-tag))
   (query-dsl.ss "Datalog-style declarative query language for blocks"
    (dependencies "core/blocks/block.ss" "boundary/store-api.ss")
    (exports query query-count query-group-by find-entities find-relations))
   (query-patterns.ss "Pattern matching with variable binding and joins"
    (dependencies "query-dsl.ss")
    (exports pattern-query match-pattern join-patterns project-vars))
   (patterns-parse.ss "Inline @-tag parsing for metadata extraction"
    (exports extract-tags has-tag? get-tag safe-extract-tags))
   (aho-corasick.ss "Multi-pattern string matching algorithm"
    (dependencies "lattice/data/queue.ss" "lattice/data/set.ss" "lattice/data/dict.ss")
    (exports make-automaton search))))
 (tests
  ((test-query-dsl.ss "100+ tests for query-dsl.ss")
   (test-query-patterns.ss "~30 tests for query-patterns.ss")
   (test-patterns-parse.ss "~30 tests for patterns-parse.ss")
   (test-aho-corasick.ss "~20 tests for aho-corasick.ss")))
 (dependencies (base queue set dict)))
