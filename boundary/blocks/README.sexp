;;; boundary/blocks/README.sexp — Block Navigation and Query Modules
;;;
;;; Tools for exploring and querying the content-addressed block store.

((name . blocks)
 (purpose . "Block navigation, querying, and exploration tools")
 (tier-access . shell)
 (purity . impure)

 (description . "
Interactive tools for navigating, querying, and analyzing the
content-addressed block store. Supports both programmatic queries
and REPL-based exploration.

Key features:
- Query language for block searches
- Interactive navigator with analytics
- Block comparison and diffing
- Index-based search optimization
")

 (modules
  ((file . block-navigator.ss)
   (purpose . "Interactive block store navigator")
   (api . (explore-blocks navigate-to block-tree block-graph
           nav-forward nav-back nav-stats)))

  ((file . block-explorer.ss)
   (purpose . "Session-based block exploration")
   (api . (explore-block block-info block-refs block-history)))

  ((file . block-query.ss)
   (purpose . "Query language for blocks")
   (api . (query-blocks block-match? compile-query)))

  ((file . block-query-advanced.ss)
   (purpose . "Advanced query capabilities")
   (api . (query-aggregate query-join query-window)))

  ((file . block-index.ss)
   (purpose . "Block indexing for fast lookup")
   (api . (build-index index-lookup index-stats rebuild-index)))

  ((file . block-diff.ss)
   (purpose . "Block comparison and diffing")
   (api . (diff-blocks block-changes common-ancestor))))

 (dependencies
  (internal . (core/blocks/block.ss
               core/blocks/cas.ss
               boundary/storage/store-api.ss)))

 (usage . "
Navigate blocks interactively:
  (load \"boundary/blocks/block-navigator.ss\")
  (explore-blocks)

Query blocks:
  (load \"boundary/blocks/block-query.ss\")
  (query-blocks '(tag = post))
")

 (see-also . (
   "boundary/storage/README.sexp"
   "core/blocks/README.sexp")))
