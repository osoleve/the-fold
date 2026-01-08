;;; shell/repl/repl.ss — The Fold System REPL
;;;
;;; THIS FILE MUST BE LOADED FIRST BY ALL CLAUDES.
;;;
;;; The System REPL is the mandatory entry point to The Fold.
;;; It loads all necessary dependencies and displays the welcome screen.
;;;
;;; Usage:
;;;   (load "shell/repl.ss")  ; First and ONLY thing you do
;;;
;;; This is Shell code: uses IO, manages system state.
;;;
;;; After loading, the REPL will:
;;;   1. Load all dependencies
;;;   2. Display the welcome screen
;;;   3. Make all CAS and exploration functions available

;;; ============================================================
;;; Dependency Loading
;;; ============================================================

;;; Load order matters — dependencies first

;; Set up source-directories so core modules can find prelude.ss
(source-directories (cons "core" (source-directories)))

;; Core dependencies
(load "core/blocks/block.ss")
(load "core/base/sha256.ss")

;; Module system (provides (require), (modules), (module-info))
(load "core/lang/module.ss")

;; Shell dependencies
(load "shell/fs.ss")
(load "shell/ui/text.ss")
(load "shell/tools/string-utils.ss")  ; Wishlist #3: Foundational string utilities
(load "shell/edit.ss")
(load "shell/git/git.ss")
(load "shell/session-manager.ss")

;; DUCKIE interaction
(load "shell/duckie-interact.ss")

;; Block navigation and exploration
(load "shell/block-navigator.ss")
(load "shell/block-explorer.ss")

;; Metadata tagging system
(load "core/lang/parse.ss")
(load "core/query/patterns-parse.ss")  ; Tag extraction (extract-tags, has-tag?, get-tag)
(load "core/query/query.ss")

;; Standard library: Store API and Collection Utilities
(load "shell/store-api.ss")
(load "core/data/collection-utils.ss")
(load "core/query/query-dsl.ss")  ; Query DSL (depends on store-api)

;; Command system
(load "shell/commands.ss")

;; Typed evaluation commands (fold-parse, fold-type, fold-eval, fold-compile)
(load "shell/eval-repl.ss")

;; Patch system
(load "shell/patches.ss")

;; Lens navigation system
(load "shell/lens/navigator.ss")


;;; ============================================================
;;; Quiet Mode
;;; ============================================================

;;; Set *quiet* to #t before loading to suppress startup output.
;;; Usage: (define *quiet* #t) (load "shell/repl.ss")
(define *quiet* (if (top-level-bound? '*quiet*) *quiet* #f))

;;; ============================================================
;;; Startup Display
;;; ============================================================

(define *fold-version* "GENESIS")

;;; display-startup : → void
;;; Minimal startup: version and commands
(define (display-startup)
  (display (format "The Fold ~a — Content-Addressed Storage System\n" *fold-version*))
  (display "Commands: (blocks) (explore-block hash) (help)\n")
  (display "New to The Fold? Try (start-tutorial) for an interactive guide!\n")
  (display "Type (commands) to see all registered commands.\n"))

;;; ============================================================
;;; Help and Command Reference
;;; ============================================================

(define (display-help)
  (display "\n")
  (display "  ┌────────────────────────────────────────────────────────────────────┐\n")
  (display "  │                       AVAILABLE COMMANDS                           │\n")
  (display "  └────────────────────────────────────────────────────────────────────┘\n")
  (display "\n")
  (display "  TUTORIALS:\n")
  (display "    (start-tutorial)       Start interactive tutorial\n")
  (display "    (tutorial-next)        Next tutorial step\n")
  (display "    (tutorial-do)          Do tutorial exercise\n")
  (display "    (tutorial-skip)        Skip tutorial step\n")
  (display "    (tutorial-help)        Get tutorial help\n")
  (display "    (tutorial-status)      Show tutorial progress\n")
  (display "    (list-tutorials)       List available tutorials\n")
  (display "\n")
  (display "  GIT:\n")
  (display "    (git-status)           Show git status\n")
  (display "    (git-diff)             Show uncommitted changes\n")
  (display "    (git-log [n])          Show recent commits\n")
  (display "    (commit! msg)          Stage and commit\n")
  (display "    (push!)                Push to origin\n")
  (display "    (commit-and-push! msg) Commit and push\n")
  (display "\n")
  (display "  EDITING:\n")
  (display "    (read-text-file (fs) path)     Read file as string\n")
  (display "    (write-text-file! (fs) p str)  Write string to file\n")
  (display "    (edit-file! (fs) path fn)      Transform file contents\n")
  (display "\n")
  (display "  DUCKIE:\n")
  (display "    (to-duckie msg)        Talk to DUCKIE\n")
  (display "    (duckie-greet)         Get a greeting from DUCKIE\n")
  (display "    (duckie-farewell)      Say goodbye to DUCKIE\n")
  (display "    (duckie-mood)          Check DUCKIE's mood\n")
  (display "    (set-duckie-mood! m)   Change DUCKIE's mood\n")
  (display "\n")
  (display "  BLOCK EXPLORER:\n")
  (display "    (blocks)               Show CAS statistics and overview\n")
  (display "    (explore-block hash)   Explore a block by hash prefix\n")
  (display "    (tree hash [depth])    Visualize block reference tree\n")
  (display "    (popular [n])          Show N most-referenced blocks\n")
  (display "    (orphans)              Find blocks with no inbound refs\n")
  (display "    (search query)         Search blocks with ranking\n")
  (display "\n")
  (display "  INTERACTIVE BLOCK EXPLORER:\n")
  (display "    (bx)                   Start interactive explorer\n")
  (display "    (bx-popular)           View popular blocks (numbered)\n")
  (display "    (bx-orphans)           View orphans (numbered)\n")
  (display "    (bx-search \"query\")   Search blocks (numbered)\n")
  (display "    (bx-view N)            Explore block number N\n")
  (display "    (bx-back)              Go back to previous block\n")
  (display "    (bx-home)              Return to home screen\n")
  (display "    (bx-help)              Show all explorer commands\n")
  (display "\n")
  (display "  METADATA TAGS:\n")
  (display "    (tags)                 Show all @tags in use\n")
  (display "    (tag-report)           Tag frequency histogram\n")
  (display "    (find-tagged (fs) k v) Find blocks with @key:value\n")
  (display "    (extract-tags text)    Parse @tags from text\n")
  (display "\n")
  (display "  TYPED EVALUATION:\n")
  (display "    (fold-eval expr)       Evaluate a Fold expression\n")
  (display "    (fold-eval expr fuel)  Evaluate with custom fuel limit\n")
  (display "    (fold-type expr)       Type-check, show type\n")
  (display "    (fold-parse str)       Parse string, show AST\n")
  (display "    (fold-compile expr)    Full pipeline with diagnostics\n")
  (display "    (fold-eval-help)       Detailed help for these commands\n")
  (display "\n")
  (display "  PATCHES:\n")
  (display "    (patches)              List available patches\n")
  (display "    (apply-patch 'name)    Load a patch (e.g., 'turtle)\n")
  (display "    (patch-info 'name)     Show patch details\n")
  (display "    (applied-patches)      List currently loaded patches\n")
  (display "\n")
  (display "  LENS NAVIGATION:\n")
  (display "    (lens-jump 'sym)       Jump to symbol definition\n")
  (display "    (lens-callers 'sym)    Who calls this symbol?\n")
  (display "    (lens-callees 'sym)    What does this symbol call?\n")
  (display "    (lens-test 'sym)       Find related tests\n")
  (display "    (lens-slice-up 'sym)   Transitive dependents\n")
  (display "    (lens-slice-down 'sym) Transitive dependencies\n")
  (display "    (lens-path 'a 'b)      Find call path from a to b\n")
  (display "    (lens-stats)           Show navigation statistics\n")
  (display "    (lens-rebuild!)        Rebuild all indices\n")
  (display "    (lens-help)            Lens command reference\n")
  (display "\n")
  (display "  UTILITIES:\n")
  (display "    (help)                 Show this help\n")
  (display "    (fs)                   Get filesystem capability\n")
  (display "    (load-core)            Load Core modules + playground\n")
  (display "    (playground-help)      Playground commands (after load-core)\n")
  (display "    (playground-demo)      Try the playground (after load-core)\n")
  (display "\n")
  (display "  MODULE SYSTEM:\n")
  (display "    (modules)              List all available modules\n")
  (display "    (module-info 'name)    Show module details (path, deps, status)\n")
  (display "    (require 'name)        Load a module with its dependencies\n")
  (display "    (module-stats)         Show loaded modules and timings\n")
  (display "    (module-graph)         Show dependency graph\n")
  (display "\n")
  (display "  DEVELOPMENT TOOLKIT:\n")
  (display "    (run-tests)            Run all tests (scheme --script test-all.ss)\n")
  (display "    (deps-check dir)       Check module dependencies\n")
  (display "    (find-uses 'sym)       Find symbol usage across codebase\n")
  (display "    (check-circular-deps (fs) dir) Find circular dependencies\n")
  (display "    See toolkit.ss for more: block-diff, type-inspect, fuel-profile\n")
  (display "\n"))

;; New command-based help (integrates with command registry)
(define (help . args)
  (if (null? args)
      ;; Show traditional comprehensive help for now
      (display-help)
      ;; Show command-specific help using command registry
      (apply cmd-help args)))

;;; ============================================================
;;; Convenience Functions
;;; ============================================================

;;; fs : → FS
;;; Convenience function to get a filesystem capability.
(define (fs)
  (mint-fs-capability ".store"))

;;; Block Explorer Convenience Functions

;;; blocks : → void
;;; Show all blocks in the content-addressed store.
(define (blocks)
  (block-stats (fs)))

;;; explore : String → void
;;; Explore a block by hash prefix (interactive drilldown).
(define (explore-block hash-prefix)
  (explore (fs) hash-prefix))

;;; popular : [Nat] → void
;;; Show the N most-referenced blocks (default 10).
(define (popular . args)
  (let ([n (if (null? args) 10 (car args))])
       (find-popular (fs) n)))

;;; orphans : → void
;;; Find blocks with no inbound references.
(define (orphans)
  (find-orphans (fs)))

;;; tree : String [Nat] → void
;;; Visualize block reference tree (default depth 3).
(define (tree hash-prefix . args)
  (let ([depth (if (null? args) 3 (car args))])
       (visualize-tree (fs) hash-prefix depth)))

;;; search : String → void
;;; Search blocks with relevance ranking.
(define (search query)
  (search-ranked (fs) query))

;;; ============================================================
;;; Interactive Block Explorer (session-based)
;;; ============================================================
;;; Functions loaded from shell/block-explorer.ss
;;; Available: bx, bx-view, bx-back, bx-home, bx-popular,
;;;            bx-orphans, bx-search, bx-recent, bx-by-tag,
;;;            bx-stats, bx-help

;;; bx : → void
;;; Convenience wrapper to start the interactive block explorer.
(define (bx)
  (block-explorer (fs)))

;;; ============================================================

;;; resume-session : → void
;;; Resume an existing session without re-logging in.
(define (resume-session)
  (if (session-exists?)
      (who)
      (display "No session. Use (hi tier name msg) to login.\n")))

;;; clear : → void
;;; Clear the REPL screen.
(define (clear)
  (display "\x1b;[2J\x1b;[H"))

;;; version : → void
;;; Display system version information.
(define (version)
  (display (format "The Fold ~a\n" *fold-version*))
  (display "Content-Addressed Storage System\n"))

;;; ============================================================
;;; Core Development Utilities
;;; ============================================================

;;; load-core : → void
;;; Load Core modules for development experimentation.
;;; Provides: normalize, expand, eval-expr, run, infer, etc.
;;;
;;; Note: block.ss and sha256.ss are loaded by repl.ss.
;;; Other core modules may have internal load statements that assume
;;; they're loaded from the core directory. We load the standalone modules.
(define *core-loaded* #f)

(define (load-core)
  (if *core-loaded*
      (display "Core modules already loaded.\n")
      (begin
       (display "Loading Core modules...\n")
       ;; These modules are standalone (no internal loads)
       (load "core/blocks/normalize.ss")
       (load "core/blocks/expand.ss")
       (load "core/lang/prim.ss")
       (load "core/lang/eval.ss")
       (load "core/types/types.ss")
       (load "core/types/kinds.ss")
       (load "core/types/infer.ss")
       (load "core/types/annotate.ss")
       (load "core/typed-eval.ss")
       ;; Note: cas.ss, parse.ss, validate.ss have internal loads
       ;; that conflict with repl.ss. Hash/block functions are already
       ;; available from block.ss and sha256.ss loaded by repl.ss.
       
       ;; Load Core Playground (depends on normalize/expand)
       (load "shell/examples/core-playground.ss")
       
       (set! *core-loaded* #t)
       (display "Core loaded. Available: normalize, expand, run, infer, etc.\n")
       (display "Core Playground loaded. Use (playground-help) for commands.\n")
       (display "\nExamples:\n")
       (display "  (run '((fn (x) x) 42) 100)   → (ok 42)\n")
       (display "  (infer '(fn (x) x) '())      → (ok (-> τ1 τ1) ())\n")
       (display "  (t '(prim 'add 1 2))         → 3 : Int\n")
       (display "  (:type '(fn (x) x))          → (∀ (τ1) (τ1 → τ1))\n")
       (display "\nPlayground:\n")
       (display "  (try-normalize '(lambda (x) x))  → Show de Bruijn form\n")
       (display "  (try-hash '(lambda (x) x))       → Show expression hash\n")
       (display "  (playground-demo)                → See all features\n"))))

;;; ============================================================
;;; Typed Evaluation Convenience Functions
;;; ============================================================

;;; These functions are available after (load-core)

;;; print-typed : TypedValue → void
;;; Display a typed value with its type annotation.
(define (print-typed tv)
  (display (show-typed tv))
  (newline))

;;; typed-repl-eval : Expr → void
;;; Evaluate and display with types (for REPL use).
(define (typed-repl-eval expr)
  (let ([result (typed-run expr 1000)])
       (cond
        [(and (pair? result) (eq? (car result) 'ok))
         (print-typed (cadr result))]
        [(and (pair? result) (eq? (car result) 'error))
         (display "Type error: ")
         (display (format-type-error result))
         (newline)]
        [else
         (display "Error: ")
         (display result)
         (newline)])))

;;; t : Expr → void
;;; Typed evaluation: type-check and evaluate, display result with type.
(define (t expr)
  (unless *core-loaded*
          (error 't "Core not loaded. Run (load-core) first."))
  (typed-repl-eval expr))

;;; :type : Expr → void
;;; Show the type of an expression without evaluating.
(define (:type expr)
  (unless *core-loaded*
          (error ':type "Core not loaded. Run (load-core) first."))
  (reset-fresh!)
  (let ([result (typeof expr)])
       (if (and (pair? result) (eq? (car result) 'error))
           (begin
            (display "Type error: ")
            (display (format-type-error result))
            (newline))
           (begin
            (display (type->string result))
            (newline)))))

;;; :ann : Expr → void
;;; Show an expression with type annotations at every node.
(define (:ann expr)
  (unless *core-loaded*
          (error ':ann "Core not loaded. Run (load-core) first."))
  (show-annotated expr))

;;; ============================================================
;;; REPL Initialization
;;; ============================================================

(define (fold-repl-init)
  (unless *quiet*
          (display-startup))
  (when *quiet*
        (display "The Fold loaded.\n")))

;;; ============================================================
;;; Auto-initialize on load
;;; ============================================================

(fold-repl-init)
