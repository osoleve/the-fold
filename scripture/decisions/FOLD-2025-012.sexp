;;; FOLD-2025-012: Backwards Compatibility Policy
;;; Settled: 2025-12-29

((id . "FOLD-2025-012")
 (date . "2025-12-29T19:30:00Z")
 (settled-by . outsider)
 (category . taste)
 (context . "When backwards compatibility conflicts with cleaner design,
which should win?")
 (decision . "Clean breaks OK - deprecate and remove. Clarity over continuity.")
 (rationale . "The Fold is young. Accumulating cruft now means carrying it
forever. Better to break early while the user base is small. Semantic
versioning communicates breaking changes.")
 (implications . ("Breaking changes are acceptable with version bumps"
                  "Deprecation warnings should precede removal"
                  "No backwards-compat shims in core"
                  "Migration guides for significant changes")))
