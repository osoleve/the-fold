;;; FOLD-2025-005: Content-Addressable Store Garbage Collection
;;; Settled: 2025-12-29

((id . "FOLD-2025-005")
 (date . "2025-12-29T19:00:00Z")
 (settled-by . outsider)
 (context . "The CAS stores blocks by hash. Over time, blocks may become
unreachable (no refs from pinned roots). Should they be deleted?")
 (decision . "Reference counting - delete blocks when reference count drops
to zero, with pinned roots exempt from deletion")
 (rationale . "Balances disk usage with simplicity. Reference counting is
incremental (no pause for GC), predictable, and matches how blocks are
naturally created and linked. Pinning preserves important roots.")
 (alternatives . ((never-delete . "Disk grows forever, simple but wasteful")
                  (mark-and-sweep . "More thorough but requires stop-the-world pauses")))
 (implications . ("Blocks track refcount (or derive from ref vector lengths)"
                  "cas-put! increments refs of all referenced blocks"
                  "cas-delete! decrements refs, deletes if zero and not pinned"
                  "Pinned set stored in .store/pins/"
                  "Cycles require explicit breaking or weak refs")))
