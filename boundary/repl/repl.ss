(source-directories (cons "core" (source-directories)))

(doc 'module 'repl)
(doc 'description "The Fold System REPL — The mandatory entry point to The Fold")
(doc 'layer 'boundary)
(doc 'purity 'partial)

(doc 'critical "THIS FILE MUST BE LOADED FIRST BY ALL CLAUDES")

(doc 'usage "(load \"boundary/repl/repl.ss\")  ; First and ONLY thing you do")

(doc 'initialization "
After loading, the REPL will:
  1. Load all dependencies
  2. Display the welcome screen
  3. Make all CAS and exploration functions available
")

(doc 'section 'dependency-loading)
(doc 'note "Load order matters — dependencies first")

;; Core dependencies
(load "core/blocks/block.ss")
(load "core/base/sha256.ss")

;; Module system (provides (require), (modules), (module-info))
(load "core/lang/module.ss")

;; Module index initialization (manifest-based auto-discovery)
;; Must happen BEFORE any lattice modules are required
(load "boundary/module/manifest-scanner.ss")
(load "lattice/meta/manifest.ss")
(load "boundary/module/module-index.ss")
(module-index-init!)

;; Shell dependencies
(load "boundary/io/fs.ss")
(load "boundary/ui/text.ss")
(load "boundary/tools/string-utils.ss")  ; Wishlist #3: Foundational string utilities
(load "boundary/tools/edit.ss")
(load "boundary/git/git.ss")
(load "boundary/repl/session-manager.ss")

;; Block navigation and exploration
(load "boundary/blocks/block-navigator.ss")
(load "boundary/blocks/block-explorer.ss")

;; Metadata tagging system
(load "core/lang/parse.ss")
(load "lattice/query/patterns-parse.ss")  ; Tag extraction (extract-tags, has-tag?, get-tag)

;; Standard library: Store API and Collection Utilities
(load "boundary/storage/store-api.ss")
(load "lattice/data/collection-utils.ss")
(load "lattice/query/query-dsl.ss")  ; Query DSL (depends on store-api)

;; Command system
(load "boundary/commands.ss")

;; Typed evaluation commands (fold-parse, fold-type, fold-eval, fold-compile)
(load "boundary/repl/eval-repl.ss")

;; Patch system
(load "boundary/repl/patches.ss")

;; Lens navigation system
(load "boundary/lens/navigator.ss")

;; BBS issue tracker
(load "boundary/bbs/bbs.ss")
(bbs-init-quiet!)

;; Lattice meta-tooling (auto-initialized with cached docstrings + source-locs)
(load "lattice/meta/meta.ss")
(lattice-init-quiet!)

;; Tracked loader (load! for development workflow)
(load "boundary/repl/loader.ss")

;; Test runner (test-module for quick testing)
(load "boundary/repl/test-runner.ss")

;; Error context capture (last-error, with-context)
(load "boundary/repl/error-context.ss")

;; History module (undo/redo, branching)
;; Note: Loaded by repl-worker.ss after session initialization
;; Commands: (undo) (redo) (history) (branch 'name) (branches) (checkout 'name)


(doc 'section 'quiet-mode)

(doc '*quiet* 'description "Set to #t before loading to suppress startup output")
(doc '*quiet* 'usage "(define *quiet* #t) (load \"boundary/repl/repl.ss\")")
(define *quiet* (if (top-level-bound? '*quiet*) *quiet* #f))

(doc 'section 'startup-display)

(define *fold-version* "GENESIS")

(doc display-startup 'type '(-> Void))
(doc display-startup 'description "Minimal startup: version and commands")
(define (display-startup)
  (display (format "The Fold ~a — Content-Addressed Storage System\n" *fold-version*))
  (display "Commands: (blocks) (explore-block hash) (help)\n"))

(doc 'section 'help-and-command-reference)

(define (display-help)
  (doc 'type '(-> Void))
  (doc 'description "Display comprehensive help text with all commands")
  (display "\n")
  (display "  ┌────────────────────────────────────────────────────────────────────┐\n")
  (display "  │                       AVAILABLE COMMANDS                           │\n")
  (display "  └────────────────────────────────────────────────────────────────────┘\n")
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
  (display "    (extract-tags text)    Parse @tags from text\n")
  (display "    (has-tag? tags key)    Check if tag exists\n")
  (display "    (get-tag tags key)     Get tag value\n")
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
  (display "  DEVELOPMENT:\n")
  (display "    (load! \"path\")        Load and track file for reload\n")
  (display "    (load!)                Reload all tracked files\n")
  (display "    (loaded)               Show tracked files\n")
  (display "    (test-module \"path\")  Run tests for a module\n")
  (display "    (test-dir \"path\")     Run all tests in directory\n")
  (display "\n")
  (display "  ERROR DEBUGGING:\n")
  (display "    (last-error)           Show last error with full details\n")
  (display "    (clear-error!)         Clear captured error\n")
  (display "    (with-context lbl e)   Evaluate with context label\n")
  (display "\n")
  (when (top-level-bound? 'history)
    (display "  HISTORY (undo/redo):\n")
    (display "    (undo)                 Undo last command\n")
    (display "    (redo)                 Redo undone command\n")
    (display "    (history)              Show command history\n")
    (display "    (history n)            Show last n commands\n")
    (display "    (jump n)               Jump to history index n\n")
    (display "    (branch 'name)         Create branch at current point\n")
    (display "    (branches)             List all branches\n")
    (display "    (checkout 'name)       Switch to branch\n")
    (display "    (history-help)         Full history command reference\n")
    (display "\n"))
  (display "  UTILITIES:\n")
  (display "    (help)                 Show this help\n")
  (display "    (fs)                   Get filesystem capability\n")
  (display "    (load-core)            Load Core modules (eval, types, compile)\n")
  (display "\n")
  (display "  MODULE SYSTEM:\n")
  (display "    (modules)              List all available modules\n")
  (display "    (module-info 'name)    Show module details (path, deps, status)\n")
  (display "    (module-exports 'name) Show all defines exported by module\n")
  (display "    (require 'name)        Load a module with its dependencies\n")
  (display "    (module-stats)         Show loaded modules and timings\n")
  (display "    (module-graph)         Show dependency graph\n")
  (display "\n")
  (display "  MORE:\n")
  (display "    (load \"boundary/toolkit.ss\")           Dev tools (find-uses, deps-check)\n")
  (display "    (load \"boundary/tutorial/tutorial.ss\") Interactive tutorials\n")
  (display "\n"))

(doc help 'description "Command-based help integrating with command registry")
(define (help . args)
  (if (null? args)
      (display-help)
      (apply cmd-help args)))

(doc 'section 'convenience-functions)

(doc fs 'type '(-> FS))
(doc fs 'description "Convenience function to get a filesystem capability")
(define (fs)
  (mint-fs-capability ".store"))

(doc 'section 'block-explorer-convenience)

(doc blocks 'type '(-> Void))
(doc blocks 'description "Show all blocks in the content-addressed store")
(define (blocks)
  (block-stats (fs)))

(doc explore-block 'type '(-> String Void))
(doc explore-block 'description "Explore a block by hash prefix (interactive drilldown)")
(define (explore-block hash-prefix)
  (explore (fs) hash-prefix))

(doc popular 'type '(-> (Option Nat) Void))
(doc popular 'description "Show the N most-referenced blocks (default 10)")
(define (popular . args)
  (let ([n (if (null? args) 10 (car args))])
       (find-popular (fs) n)))

(doc orphans 'type '(-> Void))
(doc orphans 'description "Find blocks with no inbound references")
(define (orphans)
  (find-orphans (fs)))

(doc tree 'type '(-> String (Option Nat) Void))
(doc tree 'description "Visualize block reference tree (default depth 3)")
(define (tree hash-prefix . args)
  (let ([depth (if (null? args) 3 (car args))])
       (visualize-tree (fs) hash-prefix depth)))

(doc search 'type '(-> String Void))
(doc search 'description "Search blocks with relevance ranking. Optional second arg: result limit (default 50, #f for all).")
(define (search query . args)
  (apply search-ranked (fs) query args))

(doc 'section 'interactive-block-explorer)
(doc 'note "Functions loaded from boundary/blocks/block-explorer.ss")
(doc 'functions '(bx bx-view bx-back bx-home bx-popular bx-orphans bx-search bx-recent bx-by-tag bx-stats bx-help))

(doc bx 'type '(-> Void))
(doc bx 'description "Convenience wrapper to start the interactive block explorer")
(define (bx)
  (block-explorer (fs)))

(doc 'section 'session-management)

(doc resume-session 'type '(-> Void))
(doc resume-session 'description "Resume an existing session without re-logging in")
(define (resume-session)
  (if (session-exists?)
      (who)
      (display "No session. Use fold_login to authenticate.\n")))

(doc clear 'type '(-> Void))
(doc clear 'description "Clear the REPL screen")
(define (clear)
  (display "\x1b;[2J\x1b;[H"))

(doc version 'type '(-> Void))
(doc version 'description "Display system version information")
(define (version)
  (display (format "The Fold ~a\n" *fold-version*))
  (display "Content-Addressed Storage System\n"))

(doc 'section 'core-development-utilities)

(doc load-core 'type '(-> Void))
(doc load-core 'description "Load Core modules for development experimentation. Provides: normalize, expand, eval-expr, run, infer, etc.")
(doc load-core 'note "block.ss and sha256.ss are loaded by repl.ss. Other core modules may have internal load statements that assume they're loaded from the core directory. We load the standalone modules.")
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
       (load "core/lang/typed-eval.ss")
       ;; Note: cas.ss, parse.ss, validate.ss have internal loads
       ;; that conflict with repl.ss. Hash/block functions are already
       ;; available from block.ss and sha256.ss loaded by repl.ss.
       
       ;; Load Core Playground (depends on normalize/expand)
       (load "boundary/examples/core-playground.ss")
       
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

(doc 'section 'typed-evaluation-convenience)
(doc 'note "These functions are available after (load-core)")

(doc print-typed 'type '(-> TypedValue Void))
(doc print-typed 'description "Display a typed value with its type annotation")
(define (print-typed tv)
  (display (show-typed tv))
  (newline))

(doc typed-repl-eval 'type '(-> Expr Void))
(doc typed-repl-eval 'description "Evaluate and display with types (for REPL use)")
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

(doc t 'type '(-> Expr Void))
(doc t 'description "Typed evaluation: type-check and evaluate, display result with type")
(define (t expr)
  (unless *core-loaded*
          (error 't "Core not loaded. Run (load-core) first."))
  (typed-repl-eval expr))

(doc :type 'type '(-> Expr Void))
(doc :type 'description "Show the type of an expression without evaluating")
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

(doc :ann 'type '(-> Expr Void))
(doc :ann 'description "Show an expression with type annotations at every node")
(define (:ann expr)
  (unless *core-loaded*
          (error ':ann "Core not loaded. Run (load-core) first."))
  (show-annotated expr))

(doc 'section 'repl-initialization)

(define (fold-repl-init)
  (doc 'type '(-> Void))
  (doc 'description "Initialize the REPL (display startup or quiet confirmation)")
  (unless *quiet*
          (display-startup))
  (when *quiet*
        (display "The Fold loaded.\n")))

(doc 'section 'auto-initialize)

(fold-repl-init)
