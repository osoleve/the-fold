;;; FOLD-2025-011: Standard Library Philosophy
;;; Settled: 2025-12-29

((id . "FOLD-2025-011")
 (date . "2025-12-29T19:30:00Z")
 (settled-by . outsider)
 (category . taste)
 (context . "Should the system feel minimal/austere (small surface area) or
batteries-included (rich standard library)?")
 (decision . "Comfortable standard library with extensive additional libraries
and a fluent mechanism for discovering them")
 (rationale . "Minimalism is elegant but frustrating in practice. A good
standard library accelerates everyone. Discovery mechanisms (search, browse,
docgen) make large libraries navigable.")
 (implications . ("Prelude should include common operations"
                  "Additional libraries in core/fp/, etc."
                  "docgen-search and similar discovery tools are first-class"
                  "Library growth is encouraged, not feared")))
