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
(load "lattice/meta/persist.ss")
(load "lattice/meta/source-loc.ss")

;;; ====
;;; Initialization
;;; ====

;;; lattice-init! : -> void
;;; Initialize the lattice tooling (build KG and search indices)
;;; Uses cache if valid, otherwise rebuilds and caches
(define (lattice-init!)
  (if (lattice-load-cache!)
      ;; Cache loaded successfully, just build search indices
      (begin
        (lattice-index!)
        (build-source-location-cache!)
        (printf "\nLattice tooling initialized (from cache)!\n"))
      ;; No valid cache, full rebuild
      (begin
        (kg-build!)
        (lattice-index!)
        (build-source-location-cache!)
        (lattice-save-cache!)))
  (printf "  Use (lf \"query\") to search\n")
  (printf "  Use (li 'skill) to inspect a skill\n")
  (printf "  Use (lsrc 'fn) for source location\n")
  (printf "  Use (ls) for statistics\n")
  (printf "  Use (lh) for health check\n"))

;;; lattice-init-quiet! : -> void
;;; Initialize silently (for REPL startup)
(define (lattice-init-quiet!)
  (if (lattice-load-cache!)
      (begin
        (lattice-index!)
        (build-source-location-cache!))
      (begin
        (kg-build!)
        (lattice-index!)
        (build-source-location-cache!)
        (lattice-save-cache!))))

;;; lattice-init-fresh! : -> void
;;; Force full rebuild, ignoring cache
(define (lattice-init-fresh!)
  (kg-build!)
  (lattice-index!)
  (build-source-location-cache!)
  (lattice-save-cache!)
  (printf "\nLattice tooling initialized (fresh build)!\n")
  (printf "  Use (lf \"query\") to search\n")
  (printf "  Use (li 'skill) to inspect a skill\n")
  (printf "  Use (lsrc 'fn) for source location\n")
  (printf "  Use (ls) for statistics\n")
  (printf "  Use (lh) for health check\n"))

;;; ====
;;; Source Location Convenience
;;; ====

;;; lsrc : Symbol -> String | void
;;; Quick source location lookup - prints "file:line" format
(define (lsrc sym)
  (let ([loc (format-source-location sym)])
       (if loc
           loc
           (printf "Symbol not found: ~a\n" sym))))

;;; ====
;;; Quick Reference
;;; ====

(define (lattice-help)
  (printf "\nLattice Meta-Tooling Quick Reference\n")
  (printf "====\n\n")
  (printf "INITIALIZATION:\n")
  (printf "  (lattice-init!)           - Init with caching (fast if unchanged)\n")
  (printf "  (lattice-init-fresh!)     - Force full rebuild\n\n")
  (printf "SEARCH:\n")
  (printf "  (lf \"query\")              - Full-text search\n")
  (printf "  (lfe 'symbol)             - Exact lookup (falls back to substring)\n")
  (printf "  (lfp 'prefix)             - Prefix search\n")
  (printf "  (lfs 'substr)             - Substring search\n")
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
  (printf "  (lsrc 'fn)                - Source location (file:line)\n")
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

;;; ====
;;; REPL Interface
;;; ====

(printf "\nmeta.ss loaded — Unified lattice tooling\n")
(printf "  (lattice-init!)  - Initialize (build KG + indices)\n")
(printf "  (lattice-help)   - Show quick reference\n")
