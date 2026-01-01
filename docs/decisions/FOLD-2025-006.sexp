;;; FOLD-2025-006: Module Name Conflict Resolution
;;; Settled: 2025-12-29

((id . "FOLD-2025-006")
 (date . "2025-12-29T19:00:00Z")
 (settled-by . outsider)
 (context . "When loading modules, name conflicts can occur. How should
the system handle a definition that shadows an existing binding?")
 (decision . "Last wins - later definitions shadow earlier ones silently")
 (rationale . "Matches Scheme's natural behavior and REPL workflow. Allows
incremental redefinition during development. Users who want protection
can use explicit prefixing conventions.")
 (alternatives . ((error-on-conflict . "Safer but breaks iterative REPL workflow")
                  (require-prefix . "Verbose, fights Scheme conventions")))
 (implications . ("(load ...) overwrites existing bindings"
                  "No automatic namespacing"
                  "Convention: modules can define module-name:export for explicit namespacing"
                  "Future module system may add opt-in conflict detection")))
