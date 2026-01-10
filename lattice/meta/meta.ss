;;; lattice/meta/meta.ss — Unified Entry Point
;;;
;;; Loads all lattice meta-tooling modules.
;;; Use this as a single import for full lattice navigation.
;;;
;;; Usage:
;;;   (load "lattice/meta/meta.ss")
;;;   (lattice-init!)                    ; Build KG and search indices
;;;   (lf "query")                       ; Search
;;;   (li 'skill)                        ; Inspect
;;;   (ls)                               ; Stats
;;;
;;; Dependencies:
;;;   All lattice/meta modules

(load "lattice/meta/search.ss")
(load "lattice/meta/analytics.ss")
(load "lattice/meta/inspect.ss")

;;; ============================================================
;;; Initialization
;;; ============================================================

;;; lattice-init! : -> void
;;; Initialize the lattice tooling (build KG and search indices)
(define (lattice-init!)
  (kg-build!)
  (lattice-index!)
  (printf "\nLattice tooling initialized!\n")
  (printf "  Use (lf \"query\") to search\n")
  (printf "  Use (li 'skill) to inspect a skill\n")
  (printf "  Use (ls) for statistics\n")
  (printf "  Use (lh) for health check\n"))

;;; ============================================================
;;; Quick Reference
;;; ============================================================

(define (lattice-help)
  (printf "\nLattice Meta-Tooling Quick Reference\n")
  (printf "=====================================\n\n")
  (printf "INITIALIZATION:\n")
  (printf "  (lattice-init!)           - Build KG and indices (run first!)\n\n")
  (printf "SEARCH:\n")
  (printf "  (lf \"query\")              - Full-text search\n")
  (printf "  (lfe 'symbol)             - Exact symbol lookup\n")
  (printf "  (lattice-complete \"pre\")  - Autocomplete\n\n")
  (printf "DEPENDENCIES:\n")
  (printf "  (ld 'skill)               - Show what skill depends on\n")
  (printf "  (lu 'skill)               - Show what uses this skill\n")
  (printf "  (lattice-path 'a 'b)      - Find path between skills\n")
  (printf "  (lattice-graph)           - Print full DAG\n\n")
  (printf "INSPECTION:\n")
  (printf "  (li 'skill)               - Full skill description\n")
  (printf "  (le 'skill)               - List exports\n")
  (printf "  (lm 'skill)               - List modules\n")
  (printf "  (lattice-summary)         - One-line summary of all skills\n\n")
  (printf "ANALYTICS:\n")
  (printf "  (ls)                      - Lattice statistics\n")
  (printf "  (lh)                      - Health check\n")
  (printf "  (lattice-coverage-pretty) - Coverage report\n")
  (printf "  (lattice-hubs)            - Most-used skills\n\n")
  (printf "STRUCTURAL:\n")
  (printf "  (lattice-roots)           - Tier 0 skills\n")
  (printf "  (lattice-leaves)          - Skills with no dependents\n")
  (printf "  (lattice-tiers)           - Skills grouped by tier\n"))

;;; ============================================================
;;; REPL Interface
;;; ============================================================

(printf "\nmeta.ss loaded — Unified lattice tooling\n")
(printf "  (lattice-init!)  - Initialize (build KG + indices)\n")
(printf "  (lattice-help)   - Show quick reference\n")
