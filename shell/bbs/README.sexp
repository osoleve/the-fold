;;; shell/bbs/README.sexp — BBS Bulletin Board System
;;;
;;; CAS-native bulletin board system for issue tracking and posts.
;;; Replaces external beads tool with native Scheme implementation.

((directory "shell/bbs")
 (purpose "Issue tracking and posts using The Fold's CAS primitives")
 (authority builder)  ; Shell code - may be modified by Builders+

 (modules
  ((name "bbs.ss")
   (description "Entry point - loads all modules, provides display functions")
   (exports (bbs-init! bbs-show bbs-list bbs-ready bbs-blocked
             bbs-history bbs-find bbs-find-exact
             ;; Re-exported from posts.ss
             post-create post-show post-list post-update)))

  ((name "ops.ss")
   (description "Issue operations - create, update, close, dependencies")
   (exports (bbs-create bbs-update bbs-close bbs-reopen
             bbs-dep bbs-undep bbs-comment)))

  ((name "posts.ss")
   (description "Post operations - changelogs, notes, announcements")
   (exports (post-create post-show post-list post-update post-fetch-data)))

  ((name "index.ss")
   (description "In-memory issue indices with disk cache for fast lookups")
   (exports (bbs-rebuild-indices! bbs-all-issues bbs-issues-by-status
             bbs-issues-by-priority bbs-issue-count bbs-issue-exists?
             bbs-issue-hash bbs-blockers bbs-blocking bbs-is-blocked?
             bbs-blocked-issues bbs-ready-issues bbs-stats
             bbs-add-dep! bbs-remove-dep!
             ;; Cache functions
             bbs-save-index-cache! bbs-load-index-cache!)))

  ((name "post-index.ss")
   (description "In-memory post indices with disk cache for fast lookups")
   (exports (post-rebuild-indices! post-all-ids post-all-posts
             post-ids-by-type post-index-count post-index-exists?
             post-index-hash post-index-stats
             ;; Cache functions
             post-save-index-cache! post-load-index-cache!)))

  ((name "store.ss")
   (description "Block storage and retrieval via CAS")
   (exports (bbs-store! bbs-fetch bbs-fetch-issue bbs-fetch-issue-data
             bbs-issue-history-data)))

  ((name "heads.ss")
   (description "Head file management for mutable references")
   (exports (bbs-head-path bbs-read-head bbs-write-head! bbs-list-heads)))

  ((name "blocks.ss")
   (description "Block creation helpers for issue/comment/dep/post blocks")
   (exports (make-issue-block make-comment-block make-dep-block make-post-block
             issue-block-data post-block-data)))

  ((name "counter.ss")
   (description "Base36 ID generation with persistence")
   (exports (bbs-next-id! bbs-sync-counter-from-heads!))))

 (post-types
  ((changelog "Release notes, what changed in a version or session")
   (note "General notes, documentation, thoughts")
   (announcement "Important announcements for the community")
   (session-summary "Summary of a work session")))

 (storage
  ((path ".store/heads/bbs/fold-*.head")
   (description "Current hash for each issue ID"))
  ((path ".store/heads/bbs/post-*.head")
   (description "Current hash for each post ID"))
  ((path ".store/objects/")
   (description "Block storage (shared with rest of Fold)"))
  ((path ".bbs/counter")
   (description "Next issue ID number"))
  ((path ".bbs/post-counter")
   (description "Next post ID number"))
  ((path ".bbs/deps")
   (description "Dependency list persistence"))
  ((path ".bbs/index.cache")
   (description "Serialized issue index cache for fast session startup"))
  ((path ".bbs/post-index.cache")
   (description "Serialized post index cache for fast session startup")))

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
  "Issue IDs use base36 encoding: fold-001, fold-00a, fold-010, etc."
  "Post IDs use base36 encoding: post-1, post-2, post-a, etc."
  "Index cache added 2026-01-15 for fast session startup (26x speedup)."
  "Cache validation: count-based (if head count differs, rebuild from scratch)."
  "Individual issue updates auto-refresh via bbs-issue-hash on cache miss."
  "Posts added 2026-01-16 for changelogs, notes, announcements, session summaries."
  "Post index added 2026-01-17 mirroring issue index pattern for O(1) lookups."))
