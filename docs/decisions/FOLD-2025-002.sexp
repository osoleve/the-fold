;;; FOLD-2025-002: SDK Patch Policy
;;;
;;; Defines conventions for SDK patches in The Fold.

((id . "FOLD-2025-002")
 (date . "2025-12-29T14:00:00Z")
 (settled-by . shepherd)
 (title . "SDK Patch Policy: Naming, Versioning, and Provides")

 (context . "The Fold has three SDKs (Loom, Quill, Satin) that need
  to be loadable via the patch system. Need consistent conventions
  for naming, versioning, dependency declaration, and exports.")

 (decision . "SDK patches follow these conventions:

  1. NAMING: sdk-<name> format (e.g., sdk-loom, sdk-quill, sdk-satin)
     - Main entrypoint patch: sdk-<name>
     - Sub-modules: sdk-<name>-<module> (e.g., sdk-loom-combat)

  2. VERSIONING: Semantic versioning (MAJOR.MINOR.PATCH)
     - MAJOR: Breaking API changes
     - MINOR: New features, backward compatible
     - PATCH: Bug fixes, backward compatible
     - SDK versions track their main module version

  3. PROVIDES: Minimal entrypoint exports only
     - Export only user-facing API functions
     - Internal helpers are NOT in provides
     - Group by category in comments for documentation
     - Example: (provides . (make-game run-game ...))

  4. REQUIRES: Mirror SDK internal dependencies
     - If SDK-A loads SDK-B internally, requires must list sdk-b
     - Core patches (turtle, graphics) can be required
     - Avoid circular dependencies

  5. FILES: Load order matters
     - List files in dependency order
     - Main facade file loads last
     - Use relative paths from project root")

 (rationale . "Consistent naming allows discovery via (patches).
  Semantic versioning enables safe updates. Minimal provides
  keeps the namespace clean. Mirrored requires ensures patches
  work standalone.")

 (alternatives .
  ((flat-names . "Using just 'loom', 'quill' - rejected as too generic")
   (auto-export . "Export everything - rejected as pollutes namespace")
   (date-versioning . "YYYY.MM.DD - rejected as non-semantic")))

 (implications .
  ("SDK patches go in patches/sdk-<name>.ss"
   "Sub-module patches are optional granular loading"
   "Users can (apply-patch 'sdk-loom) for full SDK"
   "Or (apply-patch 'sdk-loom-combat) for just combat system")))
