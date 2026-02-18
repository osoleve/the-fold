;;; boundary/bbs/README.sexp — BBS Bulletin Board System
;;;
;;; CAS-native bulletin board system with multi-board support.
;;; Board types: tracker (issues), feed (posts), forum (threaded topics).
;;; All boards share the CAS. Board context via dynamic parameter.

((directory "boundary/bbs")
 (purpose "Multi-board bulletin board system using The Fold's CAS primitives")
 (authority builder)  ; Shell code - may be modified by Builders+

 (modules
  ((name "bbs.ss")
   (description "Entry point - loads all modules, board init, display functions")
   (exports (bbs-init! bbs-init-quiet! bbs-show bbs-list bbs-ready bbs-blocked
             bbs-history bbs-find bbs-find-exact boards board-switch
             ;; Re-exported from posts.ss
             post-create post-show post-list post-update)))

  ((name "board.ss")
   (description "Board record type, registry, persistence, creation")
   (exports (make-board board? board-slug board-type board-id-prefix
             board-items-ht board-by-status-ht board-by-priority-ht
             board-by-type-ht board-reply-map-ht
             board-dir board-heads-dir board-counter-file board-deps-file
             board-index-cache-file board-descriptor-file board-ensure-dirs!
             board-register! board-ref board-exists? board-list board-slugs
             board-save-registry! board-load-registry! board-create! board-info
             make-tracker-board make-feed-board make-forum-board)))

  ((name "context.ss")
   (description "Board context parameter for scoping operations")
   (exports (*current-board* current-board current-board-slug
             current-board-type current-board-id-prefix
             board-context? with-board)))

  ((name "item.ss")
   (description "Polymorphic item API - dispatches by board type")
   (exports (item-create! item-fetch item-fetch-data item-count
             item-all-ids item-show)))

  ((name "ops.ss")
   (description "Issue operations - create, update, close, dependencies")
   (exports (bbs-create bbs-update bbs-close bbs-reopen
             bbs-dep bbs-undep bbs-comment)))

  ((name "posts.ss")
   (description "Post operations - changelogs, notes, announcements")
   (exports (post-create post-show post-list post-update post-fetch-data)))

  ((name "forum.ss")
   (description "Forum board operations - threaded topics with replies")
   (exports (topic-create! topic-reply! topic-lock! topic-archive!
             topic-replies topic-reply-count topic-show
             board-next-topic-id! board-next-reply-id!)))

  ((name "forum-index.ss")
   (description "Per-board forum index for topics and replies")
   (exports (forum-index-add-topic! forum-index-add-reply!
             forum-rebuild-index! reply-id->topic-id
             forum-save-index-cache! forum-load-index-cache!)))

  ((name "index.ss")
   (description "In-memory issue indices with disk cache, board-aware")
   (exports (bbs-rebuild-indices! bbs-all-issues bbs-issues-by-status
             bbs-issues-by-priority bbs-issue-count bbs-issue-exists?
             bbs-issue-hash bbs-blockers bbs-blocking bbs-is-blocked?
             bbs-blocked-issues bbs-ready-issues bbs-stats
             bbs-add-dep! bbs-remove-dep!
             bbs-current-items-ht bbs-current-status-ht bbs-current-priority-ht
             bbs-save-index-cache! bbs-load-index-cache!)))

  ((name "post-index.ss")
   (description "In-memory post indices with disk cache, board-aware")
   (exports (post-rebuild-indices! post-all-ids post-all-posts
             post-ids-by-type post-index-count post-index-exists?
             post-index-hash post-index-stats
             post-save-index-cache! post-load-index-cache!)))

  ((name "store.ss")
   (description "Block storage and retrieval via CAS")
   (exports (bbs-store! bbs-fetch bbs-fetch-issue bbs-fetch-issue-data
             bbs-issue-history-data)))

  ((name "heads.ss")
   (description "Head file management, board-aware path resolution")
   (exports (bbs-head-path bbs-read-head bbs-write-head! bbs-list-heads
             bbs-current-heads-dir bbs-list-heads-in)))

  ((name "blocks.ss")
   (description "Block creation helpers for all entity types")
   (exports (make-issue-block make-comment-block make-dep-block make-post-block
             make-topic-block make-reply-block make-board-block
             issue-block-data post-block-data topic-block-data reply-block-data
             board-block-data
             BBS-ISSUE BBS-EVENT BBS-COMMENT BBS-DEP BBS-INDEX BBS-POST
             BBS-BOARD BBS-TOPIC BBS-REPLY)))

  ((name "counter.ss")
   (description "Base36 ID generation with persistence, board-aware")
   (exports (bbs-next-id! bbs-sync-counter-from-heads!
             bbs-current-counter-file bbs-current-id-prefix)))

  ((name "migrate.ss")
   (description "One-time migration from flat to multi-board layout")
   (exports (bbs-needs-migration? bbs-migrate-to-boards!))))

 (board-types
  ((tracker "Issue tracking with status, priority, dependencies, comments"
            (block-tags bbs/issue bbs/event bbs/comment bbs/dep)
            (default-board "issues" "fold-"))
   (feed "Flat posts for changelogs, notes, announcements"
         (block-tags bbs/post)
         (default-board "changelog" "post-"))
   (forum "Threaded discussion with topics and replies"
          (block-tags bbs/topic bbs/reply)
          (id-format "<prefix>NNN for topics, reply-<topic-id>-NNN for replies"))))

 (post-types
  ((changelog "Release notes, what changed in a version or session")
   (note "General notes, documentation, thoughts")
   (announcement "Important announcements for the community")
   (session-summary "Summary of a work session")))

 (storage
  ((path ".bbs/boards.sexp")
   (description "Board registry - list of (slug type id-prefix description)"))
  ((path ".bbs/boards/<slug>/board.sexp")
   (description "Board descriptor cache"))
  ((path ".bbs/boards/<slug>/counter")
   (description "Next ID number for the board"))
  ((path ".bbs/boards/<slug>/deps")
   (description "Dependency list (tracker boards only)"))
  ((path ".bbs/boards/<slug>/index.cache")
   (description "Serialized index cache for fast startup"))
  ((path ".store/heads/bbs/<slug>/*.head")
   (description "Current hash for each item in the board"))
  ((path ".store/objects/")
   (description "Block storage (shared across all boards)")))

 (pipeline-integration
  "BBS effects are available in agent pipelines via lattice/pipeline/effects.ss:
   - (bbs-create title ['board slug]) - Create item, return ID
   - (bbs-create-full title desc type priority ['board slug]) - Create with full details
   - (bbs-update id updates ['board slug]) - Update item fields
   - (bbs-close id ['board slug]) - Close item
   - (bbs-ready ['board slug]) - Get unblocked items
   - (bbs-show id ['board slug]) - Get item details

   Effect type: 'bbs (with 'beads alias for backwards compatibility)
   Optional 'board keyword routes to specific board; defaults to 'issues'.")

 (notes
  "Migrated from external beads tool to native implementation (2026-01-14)."
  "Multi-board architecture added 2026-02-18."
  "All data is content-addressed - history preserved via block refs."
  "Board context via dynamic parameter (*current-board*) with (with-board slug ...)."
  "Auto-migration: if boards.sexp absent but .bbs/counter exists, migrate on init."
  "Issue IDs use base36 encoding: fold-001, fold-00a, fold-010, etc."
  "Post IDs use base36 encoding: post-1, post-2, post-a, etc."
  "Forum reply IDs: reply-<topic-id>-001, reply-<topic-id>-002, etc."
  "Index cache added 2026-01-15 for fast session startup (26x speedup)."
  "Cache validation: count-based (if head count differs, rebuild from scratch)."
  "Global hashtables aliased to default board records for backward compatibility."))
