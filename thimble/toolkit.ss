;;; shell/toolkit.ss — Development Toolkit Index
;;;
;;; Central registry and documentation for all development tools.
;;; Load this file to access the complete toolkit.
;;;
;;; This is Shell code: coordinates multiple tool modules.
;;;
;;; Usage:
;;;   (load "thimble/toolkit.ss")
;;;   (toolkit-help)           ; Show all available tools
;;;   (toolkit-help 'category) ; Show tools in a category
;;;
;;; Categories:
;;;   - building:      Tools for building and maintaining code
;;;   - workflow:      Development workflow and productivity tools
;;;   - introspection: Tools for examining and analyzing the system
;;;   - debugging:     Tools for finding and fixing issues
;;;   - analysis:      Tools for performance and quality analysis

;;; Set up source-directories
(source-directories (cons "shell" (source-directories)))
(source-directories (cons "core" (source-directories)))

;;; ============================================================
;;; Toolkit Registry
;;; ============================================================

;;; Tool categories and their descriptions
(define *toolkit-categories*
  '((building . "Code building and maintenance tools")
    (workflow . "Development workflow and productivity tools")
    (introspection . "System examination and analysis tools")
    (debugging . "Debugging and troubleshooting tools")
    (analysis . "Performance and quality analysis tools")))

;;; Tool registry: ((name category description file) ...)
(define *toolkit-tools*
  '(;; Building Tools
    (module-deps building "Analyze module dependencies and detect cycles" "module-deps.ss")
    (xref building "Find all uses and definitions of symbols" "xref.ss")
    (commands building "Structured command registry system" "commands.ss")
    (edit building "Text file editing utilities" "edit.ss")
    (validate building "Validation utilities for blocks and data" "validate.ss")
    (meta building "Inline metadata tag parser" "meta.ss")
    (scaffold building "Code scaffolding and templating system" "scaffold.ss")
    (format building "Code formatter and pretty-printer" "format.ss")
    (init-project building "Project initialization wizard" "init-project.ss")

    ;; Workflow Tools
    (test-runner workflow "Comprehensive test automation and discovery" "test-runner.ss")
    (watch workflow "File watching with auto-reload and auto-test" "watch.ss")
    (history workflow "Persistent REPL command history" "history.ss")
    (docgen workflow "Documentation generator for Scheme code" "docgen.ss")
    (git-workflow workflow "Git workflow helpers and shortcuts" "git-workflow.ss")

    ;; Introspection Tools
    (block-diff introspection "Compare and diff blocks structurally" "block-diff.ss")
    (block-explorer introspection "Interactive block navigation" "block-explorer.ss")
    (block-navigator introspection "Block navigation utilities" "block-navigator.ss")
    (block-query introspection "Query blocks by various criteria" "block-query.ss")
    (block-index introspection "Build and query block indices" "block-index.ss")
    (store-analyze introspection "Analyze store usage and health" "store-analyze.ss")
    (universe-dump introspection "Dump universe to single file" "universe-dump.ss")

    ;; Debugging Tools
    (type-inspect debugging "Examine and explain inferred types" "type-inspect.ss")
    (fuel-profile debugging "Profile fuel consumption" "fuel-profile.ss")
    (debug debugging "Interactive debugger (core)" "../core/debug.ss")
    (error-fmt debugging "Enhanced error formatter with color and context" "error-fmt.ss")

    ;; Analysis Tools
    (project-status analysis "Show project status and metrics" "project-status.ss")
    (concept-map analysis "Generate concept maps" "concept-map.ss")
    (perf-monitor analysis "Real-time performance monitoring dashboard" "perf-monitor.ss")
    (benchmark analysis "Benchmarking harness with statistics" "benchmark.ss")
    (coverage analysis "Code coverage analyzer" "coverage.ss")))
    (benchmark analysis "Benchmarking harness with statistics" "benchmark.ss")))

;;; ============================================================
;;; Help System
;;; ============================================================

;;; toolkit-help : [Symbol] → void
;;; Display help for the toolkit or a specific category.
(define (toolkit-help . args)
  (if (null? args)
      (toolkit-help-all)
      (toolkit-help-category (car args))))

;;; toolkit-help-all : → void
;;; Display all tools organized by category.
(define (toolkit-help-all)
  (display "\n")
  (display "╔══════════════════════════════════════════════════════════════════════════╗\n")
  (display "║                    THE FOLD DEVELOPMENT TOOLKIT                         ║\n")
  (display "╚══════════════════════════════════════════════════════════════════════════╝\n")
  (display "\n")
  (display "A comprehensive set of tools for building on and introspecting The Fold.\n")
  (display "\n")

  (for-each
    (lambda (cat-entry)
      (let ([category (car cat-entry)]
            [description (cdr cat-entry)])
        (display "─────────────────────────────────────────────────────────────────────────\n")
        (display (format "  ~a: ~a\n" (category-name category) description))
        (display "─────────────────────────────────────────────────────────────────────────\n")

        (let ([tools (filter (lambda (tool) (eq? (cadr tool) category))
                            *toolkit-tools*)])
          (for-each
            (lambda (tool)
              (let ([name (car tool)]
                    [desc (caddr tool)])
                (display (format "    ~a~a~a\n"
                                name
                                (make-string (max 1 (- 20 (string-length (symbol->string name)))) #\space)
                                desc))))
            tools))
        (display "\n")))
    *toolkit-categories*)

  (display "Usage:\n")
  (display "  (toolkit-help)                  - Show this help\n")
  (display "  (toolkit-help 'category)        - Show tools in a category\n")
  (display "  (toolkit-load 'tool-name)       - Load a specific tool\n")
  (display "  (toolkit-load-category 'cat)    - Load all tools in a category\n")
  (display "\n"))

;;; toolkit-help-category : Symbol → void
;;; Display help for a specific category.
(define (toolkit-help-category category)
  (let ([cat-entry (assq category *toolkit-categories*)])
    (if (not cat-entry)
        (begin
          (display (format "Unknown category: ~a\n" category))
          (display "Available categories: ")
          (display (map car *toolkit-categories*))
          (display "\n"))
        (begin
          (display "\n")
          (display (format "Category: ~a\n" (category-name category)))
          (display (format "~a\n\n" (cdr cat-entry)))

          (let ([tools (filter (lambda (tool) (eq? (cadr tool) category))
                              *toolkit-tools*)])
            (if (null? tools)
                (display "No tools in this category.\n")
                (for-each
                  (lambda (tool)
                    (let ([name (car tool)]
                          [desc (caddr tool)]
                          [file (cadddr tool)])
                      (display (format "  ~a\n" name))
                      (display (format "    Description: ~a\n" desc))
                      (display (format "    File: ~a\n" file))
                      (display (format "    Load: (toolkit-load '~a)\n\n" name))))
                  tools)))))))

;;; category-name : Symbol → String
;;; Convert category symbol to display name.
(define (category-name sym)
  (string-upcase (symbol->string sym)))

;;; string-upcase : String → String
;;; Convert string to uppercase.
(define (string-upcase str)
  (list->string (map char-upcase (string->list str))))

;;; ============================================================
;;; Loading Tools
;;; ============================================================

;;; toolkit-load : Symbol → void
;;; Load a specific tool by name.
(define (toolkit-load tool-name)
  (let ([tool (assq tool-name *toolkit-tools*)])
    (if (not tool)
        (begin
          (display (format "Unknown tool: ~a\n" tool-name))
          (display "Use (toolkit-help) to see available tools.\n"))
        (let ([file (cadddr tool)])
          (display (format "Loading ~a...\n" tool-name))
          (guard (e [else
                     (display (format "Error loading ~a: ~a\n"
                                     tool-name
                                     (if (condition? e)
                                         (condition-message e)
                                         e)))])
            (load file)
            (display (format "✓ ~a loaded.\n" tool-name)))))))

;;; toolkit-load-category : Symbol → void
;;; Load all tools in a category.
(define (toolkit-load-category category)
  (let ([tools (filter (lambda (tool) (eq? (cadr tool) category))
                      *toolkit-tools*)])
    (if (null? tools)
        (display (format "No tools in category: ~a\n" category))
        (begin
          (display (format "Loading ~a tools...\n\n" (category-name category)))
          (for-each
            (lambda (tool)
              (toolkit-load (car tool)))
            tools)
          (display (format "\n✓ All ~a tools loaded.\n" (category-name category)))))))

;;; toolkit-load-all : → void
;;; Load all tools in the toolkit.
(define (toolkit-load-all)
  (display "Loading all toolkit components...\n\n")
  (for-each
    (lambda (cat-entry)
      (toolkit-load-category (car cat-entry)))
    *toolkit-categories*)
  (display "\n✓ Complete toolkit loaded.\n"))

;;; ============================================================
;;; Quick Access Functions
;;; ============================================================

;;; Convenience functions for commonly used tools

(define (deps-check dir)
  "Quick dependency check for a directory"
  (toolkit-load 'module-deps)
  (check-circular-deps (fs) dir))

(define (find-uses symbol)
  "Quick cross-reference lookup"
  (toolkit-load 'xref)
  (xref-find-uses (fs) symbol))

(define (compare-blocks h1 h2)
  "Quick block comparison"
  (toolkit-load 'block-diff)
  (block-diff (fs) h1 h2))

(define (check-types file)
  "Quick type check for a file"
  (toolkit-load 'type-inspect)
  (type-check-file (fs) file))

(define (profile-fuel expr)
  "Quick fuel profiling"
  (toolkit-load 'fuel-profile)
  (fuel-profile expr 10000))

(define (analyze-store)
  "Quick store analysis"
  (toolkit-load 'store-analyze)
  (store-stats (fs)))

;;; ============================================================
;;; Utility Functions
;;; ============================================================

;;; make-string : Nat × Char → String
;;; Create string of n copies of character.
(define (make-string n c)
  (list->string (make-list-of n c)))

;;; make-list-of : Nat × α → (List α)
(define (make-list-of n x)
  (if (= n 0)
      '()
      (cons x (make-list-of (- n 1) x))))

;;; fs : → FS
;;; Get filesystem capability (placeholder - should be provided by environment)
(define (fs)
  (error 'toolkit "Filesystem capability not available. Load shell/fs.ss first."))

;;; ============================================================
;;; Initialization
;;; ============================================================

(display "\n")
(display "╔══════════════════════════════════════════════════════════════════════════╗\n")
(display "║                    THE FOLD DEVELOPMENT TOOLKIT                         ║\n")
(display "╚══════════════════════════════════════════════════════════════════════════╝\n")
(display "\n")
(display "Toolkit index loaded.\n")
(display "\n")
(display "Quick Start:\n")
(display "  (toolkit-help)              - Show all available tools\n")
(display "  (toolkit-load 'tool-name)   - Load a specific tool\n")
(display "  (toolkit-load-all)          - Load everything\n")
(display "\n")
(display "Quick Access:\n")
(display "  (deps-check \"dir\")          - Check dependencies\n")
(display "  (find-uses 'symbol)         - Find symbol uses\n")
(display "  (compare-blocks h1 h2)      - Compare blocks\n")
(display "  (check-types \"file.ss\")     - Type check file\n")
(display "  (profile-fuel expr)         - Profile fuel usage\n")
(display "  (analyze-store)             - Analyze store\n")
(display "\n")

;; Auto-load essential tools
(guard (e [else (void)])
  (load "fs.ss"))
