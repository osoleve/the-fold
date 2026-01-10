;;; lattice/query/manifest.sexp — Query DSL Skill Manifest

(skill query
  (version "0.1.0")
  (tier 1)
  (path "lattice/query")
  (purity partial)
  (stability stable)
  (fuel-bound "O(n) for linear scans, O(n*m) for joins, O(k*n) for multi-pattern Aho-Corasick")
  (deps (data))

  (description
   "Query DSL for searching and filtering blocks in the content-addressed store.
    Includes tag extraction, pattern matching, relational joins, and Aho-Corasick
    multi-pattern string matching.")

  (keywords (query dsl search filter tags pattern-matching
             relational join projection aho-corasick string-search))
  (aliases (search find filter block-query))

  (exports
   (patterns-parse extract-tags extract-tag-positions parse-tag-at
                   format-tag tags->string has-tag? get-tag filter-tags-by-key
                   safe-extract-tags)
   (query-dsl query interpret-query project refs-to-query refs-from-query
              refs-transitive query-count query-group-by find-entities
              find-relations find-collections find-by-content find-with-refs
              find-orphans make-tag-query make-content-query make-and-query
              make-or-query make-not-query make-select-query)
   (aho-corasick make-automaton search build-trie compute-failures)
   (query find-tagged find-tagged-any list-all-tags tag-histogram
          tag-values tag-report)
   (query-patterns pattern-query join-patterns match-pattern project-vars
                   find-pattern count-pattern))

  (modules
   (patterns-parse "patterns-parse.ss" "Tag parsing and extraction from text")
   (query-dsl "query-dsl.ss" "Core query DSL with predicates and combinators")
   (aho-corasick "aho-corasick.ss" "Aho-Corasick multi-pattern string matching")
   (query "query.ss" "High-level query interface for tagged blocks")
   (query-patterns "query-patterns.ss" "Relational pattern matching with joins")))
