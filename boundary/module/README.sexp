;;; boundary/module/README.sexp — Module Index Documentation
;;;
;;; This directory contains the manifest-based module discovery system.
;;; It replaces hardcoded module search directories with auto-discovery
;;; from manifest.sexp files in the lattice.

(directory boundary/module
  (description
   "Manifest-based module index for auto-discovery of lattice modules.
    Provides O(1) lookup for module paths instead of directory scanning.")

  (architecture
   "Three-layer design following The Fold architecture:

    ┌─────────────────────────────────────────────────────────────┐
    │  boundary/module/                                              │
    │  ├── manifest-scanner.ss   ← I/O: find + read manifests    │
    │  └── module-index.ss       ← Build + inject hashtable       │
    ├─────────────────────────────────────────────────────────────┤
    │  lattice/meta/manifest.ss  ← PURE parsing (no I/O)          │
    │  └── parse-manifest, manifest->module-index                 │
    ├─────────────────────────────────────────────────────────────┤
    │  core/lang/module.ss                                        │
    │  └── Uses manifest index for O(1) lookup                    │
    └─────────────────────────────────────────────────────────────┘")

  (modules
   ((manifest-scanner.ss
     "I/O operations for discovering manifest.sexp files.
      - find-manifests: Recursive directory scan
      - read-manifest-file: Parse manifest S-expression
      - scan-all-manifests: Collect all manifests")

    (module-index.ss
     "Builds and manages the module lookup hashtable.
      - module-index-init!: Full initialization at startup
      - module-index-lookup: O(1) path lookup
      - module-index-inject!: Inject into core/lang/module.ss")))

  (usage
   "The module index is automatically initialized during REPL startup.

    Manual usage:
      (module-index-lookup 'vec)           ; → \"lattice/linalg/vec.ss\"
      (module-index-lookup 'linalg/vec)    ; → \"lattice/linalg/vec.ss\"
      (module-index-lookup 'state)         ; → \"lattice/fp/control/state.ss\"
      (module-index-initialized?)          ; → #t

    The index supports both simple names and namespaced names:
      Simple:     (require 'vec)           ; First-match-wins for collisions
      Namespaced: (require 'linalg/vec)    ; Always unambiguous")

  (statistics
   "As of this implementation:
    - 28 manifests scanned
    - 248 simple modules indexed
    - 256 namespaced modules indexed
    - 8 collisions handled (first-match-wins)")

  (see-also
   (lattice/meta/manifest.ss "Pure manifest parsing")
   (core/lang/module.ss "Core module system")))
