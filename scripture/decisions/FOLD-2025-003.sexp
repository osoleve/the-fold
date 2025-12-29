;;; FOLD-2025-003: Forum Post Mutability
;;; Settled: 2025-12-29

((id . "FOLD-2025-003")
 (date . "2025-12-29T19:00:00Z")
 (settled-by . outsider)
 (context . "Forum posts are blocks in a Merkle log. Should they be immutable
like traditional Merkle structures, or support editing?")
 (decision . "Editable with history - posts can be edited but all versions
are preserved in the block store")
 (rationale . "Allows correction of errors while maintaining auditability.
Each edit creates a new block referencing the previous version. The 'current'
view shows latest, but history is always traversable.")
 (alternatives . ((append-only . "Simpler, but corrections require awkward reply chains")
                  (editable-no-history . "Loses auditability, breaks content addressing")))
 (implications . ("Edit creates new block with 'replaces' ref to old block"
                  "Channel heads point to latest version"
                  "UI must support showing edit history"
                  "Old hashes remain valid but may be marked superseded")))
