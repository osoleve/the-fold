;;; shell/storage/README.sexp — Storage and Persistence Modules
;;;
;;; Content-addressed storage and identity management.

((name . storage)
 (purpose . "Content-addressed storage and identity persistence")
 (tier-access . shell)
 (purity . impure)

 (description . "
Storage layer for the content-addressed block system.
Provides disk persistence for the in-memory CAS, store analysis,
and identity management with immutable history.

Key design: All blocks are content-addressed (hash = identity).
Identity updates create new blocks, enabling time-travel queries.
")

 (modules
  ((file . cas-persist.ss)
   (purpose . "Filesystem persistence for content-addressed store")
   (api . (persist-block! load-block hydrate-cas! cas-path)))

  ((file . store-api.ss)
   (purpose . "High-level block storage API")
   (api . (store-put! store-get store-list store-search
           store-filter store-count store-stats)))

  ((file . store-analyze.ss)
   (purpose . "Storage health and usage analysis")
   (api . (store-stats store-distribution store-health-check
           store-report store-growth-analysis)))

  ((file . identity.ss)
   (purpose . "Agent identity and audit trail")
   (api . (make-identity identity-update! identity-history
           identity-at-time current-identity))))

 (dependencies
  (internal . (core/base/prelude.ss
               core/blocks/block.ss
               core/blocks/cas.ss
               shell/io/fs.ss)))

 (usage . "
Load storage API:
  (load \"shell/storage/store-api.ss\")
  (store-put! fs block)
  (store-get fs hash)

Identity management:
  (load \"shell/storage/identity.ss\")
  (make-identity 'agent-name 'shepherd)
  (identity-history identity-hash)
")

 (see-also . (
   "shell/io/README.sexp"
   "core/blocks/README.sexp")))
