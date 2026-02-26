;;; lattice/query/manifest.sexp — Query DSL Skill Manifest

(skill query
  (version "0.3.0")
  (path "lattice/query")
  (purity partial)
  (stability stable)
  (fuel-bound "O(n) for linear scans, O(n*m) for joins, O(k*n) for multi-pattern Aho-Corasick")
  (deps (data optics))

  (description
   "Query DSL for searching and filtering blocks in the content-addressed store.
    Includes tag extraction, pattern matching, relational joins, Aho-Corasick
    multi-pattern string matching, and optic-based declarative queries.

    The optic query language uses optics as typed path expressions for navigating
    data structures, combined with predicate filtering, projection, and aggregation
    for building composable, declarative queries.

    The declarative query-macro DSL provides SQL-like syntax:
      (from world world-each-body
        (where (>? body-vel-y 0))
        (select body-pos-lens)
        (order-by (pluck body-pos-y))
        (limit 10))")

  (keywords (query dsl search filter tags pattern-matching
             relational join projection aho-corasick string-search
             optics optic-query declarative traversal))
  (aliases (search find filter block-query optic-query))

  (concepts
    (concept query-infrastructure
      (description "Block query DSL, optic-based declarative queries, Aho-Corasick multi-pattern search, and SQL-like syntax.")
      (parent system-design)
      (synonyms optic-query declarative-query sql-like-dsl block-query block-query-dsl)))

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
   (optic-query oquery oquery-where oquery-select oquery-pipe
                oquery-first oquery-first-where
                optic-where optic-having optic-select optic-at-index
                optic-limit optic-skip
                oquery-count oquery-count-where oquery-sum oquery-sum-by
                oquery-any oquery-all oquery-min oquery-max
                oquery-min-by oquery-max-by
                oquery-group-by oquery-partition
                oquery-join oquery-zip oquery-union oquery-intersect
                oquery-sort-by oquery-sort-by-desc
                make-query q-where q-map q-run q-count q-first
                optic-eq? optic-matches? optic-exists?
                optic-gt? optic-lt? optic-gte? optic-lte? optic-between?
                key-lens)
   ;; Declarative query-macro DSL
   (query-macro from where-clause select-clause order-by-clause
                limit-clause offset-clause run-query
                =? /=? >? <? >=? <=? between? in? like?
                null-at? exists-at? and? or? not?
                @ @?
                count-query sum-query avg-query min-query max-query group-query
                first-result any-result? all-match?
                pluck select-fields))

  (modules
   (patterns-parse "patterns-parse.ss" "Tag parsing and extraction from text")
   (query-dsl "query-dsl.ss" "Core query DSL with predicates and combinators")
   (aho-corasick "aho-corasick.ss" "Aho-Corasick multi-pattern string matching")
   (query-patterns "query-patterns.ss" "Relational pattern matching with joins")
   (optic-query "optic-query.ss" "Optic-based declarative query language")
   (query-macro "query-macro.ss" "SQL-like declarative query DSL with predicate combinators")))
