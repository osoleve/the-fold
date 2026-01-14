;;; shell/bbs/README.sexp — BBS Issue Tracker Module
;;;
;;; CAS-native bulletin board system for issue tracking.
;;; Replaces external beads tool with native Scheme implementation.

((directory "shell/bbs")
 (purpose "Issue tracking using The Fold's CAS primitives")
 (authority builder)  ; Shell code - may be modified by Builders+

 (modules
  ((name "bbs.ss")
   (description "Entry point - loads all modules, provides display functions")
   (exports (bbs-init! bbs-show bbs-list bbs-ready bbs-blocked
             bbs-history bbs-find bbs-find-exact)))

  ((name "ops.ss")
   (description "Issue operations - create, update, close, dependencies")
   (exports (bbs-create bbs-update bbs-close bbs-reopen
             bbs-dep bbs-undep bbs-comment)))

  ((name "index.ss")
   (description "In-memory indices for fast lookups")
   (exports (bbs-rebuild-indices! bbs-all-issues bbs-issues-by-status
             bbs-issues-by-priority bbs-issue-count bbs-issue-exists?
             bbs-issue-hash bbs-blockers bbs-blocking bbs-is-blocked?
             bbs-blocked-issues bbs-ready-issues bbs-stats
             bbs-add-dep! bbs-remove-dep!)))

  ((name "store.ss")
   (description "Block storage and retrieval via CAS")
   (exports (bbs-store! bbs-fetch bbs-fetch-issue bbs-fetch-issue-data
             bbs-issue-history-data)))

  ((name "heads.ss")
   (description "Head file management for mutable references")
   (exports (bbs-head-path bbs-read-head bbs-write-head! bbs-list-heads)))

  ((name "blocks.ss")
   (description "Block creation helpers for issue/comment/dep blocks")
   (exports (make-issue-block make-comment-block make-dep-block
             issue-block-data)))

  ((name "counter.ss")
   (description "Base36 ID generation with persistence")
   (exports (bbs-next-id! bbs-sync-counter-from-heads!))))

 (storage
  ((path ".store/heads/bbs/*.head")
   (description "Current hash for each issue ID"))
  ((path ".store/objects/")
   (description "Block storage (shared with rest of Fold)"))
  ((path ".bbs/counter")
   (description "Next ID number"))
  ((path ".bbs/deps")
   (description "Dependency list persistence")))

 (pipeline-integration
  "BBS effects are available in agent pipelines via lattice/pipeline/effects.ss:
   - (bbs-create title) - Create issue, return ID
   - (bbs-create-full title desc type priority) - Create with full details
   - (bbs-update id updates) - Update issue fields
   - (bbs-close id) - Close issue
   - (bbs-ready) - Get unblocked issues
   - (bbs-show id) - Get issue details

   Effect type: 'bbs (with 'beads alias for backwards compatibility)")

 (notes
  "Migrated from external beads tool to native implementation (2026-01-14)."
  "All data is content-addressed - history preserved via block refs."
  "Dependencies stored in .bbs/deps for persistence across sessions."
  "IDs use base36 encoding: fold-001, fold-00a, fold-010, etc."))
