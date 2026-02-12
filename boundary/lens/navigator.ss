(doc 'module 'navigator)
(doc 'description "REPL integration for lens navigation - main entry point")
(doc 'layer 'boundary)
(doc 'purity 'partial)

(doc 'section 'module-loading)

(load "boundary/tools/index.ss")
(load "boundary/lens/call-graph.ss")
(load "boundary/lens/test-finder.ss")
(load "boundary/lens/dep-slice.ss")
(load "boundary/lens/jump.ss")

;;; ====
;;; Navigation Commands
;;; ====

;;; lens-jump : Symbol -> void
;;; Jump to the definition of a symbol.
(define (lens-jump sym)
  (jump-to-def sym))

;;; lens-callers : Symbol [Nat] -> void
;;; Show all functions that call this symbol.
(define (lens-callers sym . opts)
  (unless (call-graph-built?)
          (call-graph-refresh!))
  
  (let* ([limit (if (null? opts) 20 (car opts))]
         [callers (call-graph-callers sym)]
         [shown (if (> (length callers) limit)
                    (take limit callers)
                    callers)]
         [more (- (length callers) (length shown))])
        (display "\n")
        (printf "  Callers of '~a':\n" sym)
        (display "  ────────────────────────────────\n")
        (if (null? callers)
            (display "    (no callers found)\n")
            (begin
             (for-each
              (lambda (caller)
                      (let ([loc (jump-location caller)])
                           (if loc
                               (printf "    ~a  (~a)\n" caller (location->string loc))
                               (printf "    ~a\n" caller))))
              shown)
             (when (> more 0)
                   (printf "\n    ... and ~a more\n" more))))
        (display "\n")))

;;; lens-callees : Symbol [Nat] -> void
;;; Show all functions that this symbol calls.
(define (lens-callees sym . opts)
  (unless (call-graph-built?)
          (call-graph-refresh!))
  
  (let* ([limit (if (null? opts) 20 (car opts))]
         [callees (call-graph-callees sym)]
         [shown (if (> (length callees) limit)
                    (take limit callees)
                    callees)]
         [more (- (length callees) (length shown))])
        (display "\n")
        (printf "  Callees of '~a':\n" sym)
        (display "  ────────────────────────────────\n")
        (if (null? callees)
            (display "    (no callees found)\n")
            (begin
             (for-each
              (lambda (callee)
                      (let ([loc (jump-location callee)])
                           (if loc
                               (printf "    ~a  (~a)\n" callee (location->string loc))
                               (printf "    ~a\n" callee))))
              shown)
             (when (> more 0)
                   (printf "\n    ... and ~a more\n" more))))
        (display "\n")))

;;; take is provided by core/prelude.ss with signature (n lst)

;;; ====
;;; Test Navigation
;;; ====

;;; lens-test : Symbol -> void
;;; Find tests related to a symbol.
(define (lens-test sym)
  (let ([tests (find-tests-for sym)])
       (display "\n")
       (printf "  Tests for '~a':\n" sym)
       (display "  ────────────────────────────────\n")
       (if (null? tests)
           (display "    (no tests found)\n")
           (for-each
            (lambda (test-info)
                    (let ([file (car test-info)]
                          [line (cadr test-info)]
                          [names (caddr test-info)])
                         (printf "    ~a\n" file)
                         (when (pair? names)
                               (for-each
                                (lambda (name)
                                        (printf "      • ~a\n" name))
                                (take names 5)))
                         (when (> (length names) 5)
                               (printf "      ... and ~a more tests\n"
                                       (- (length names) 5)))))
            tests))
       (display "\n")))

;;; ====
;;; Dependency Slicing Commands
;;; ====

;;; lens-slice-up : Symbol [Nat] -> void
;;; Show transitive dependents (what depends on this?).
(define (lens-slice-up sym . opts)
  (unless (call-graph-built?)
          (call-graph-refresh!))
  
  (let* ([max-depth (if (null? opts) 5 (car opts))]
         [layers (dep-slice-layers-up sym max-depth)])
        (display "\n")
        (printf "  Transitive Dependents of '~a':\n" sym)
        (display "  ────────────────────────────────\n")
        (if (null? layers)
            (display "    (no dependents found)\n")
            (for-each
             (lambda (layer)
                     (let ([depth (car layer)]
                           [syms (cdr layer)])
                          (printf "    Level ~a (~a):\n" depth (length syms))
                          (for-each
                           (lambda (s)
                                   (printf "      ~a\n" s))
                           (take syms 10))
                          (when (> (length syms) 10)
                                (printf "      ... and ~a more\n" (- (length syms) 10)))))
             layers))
        (display "\n")))

;;; lens-slice-down : Symbol [Nat] -> void
;;; Show transitive dependencies (what does this need?).
(define (lens-slice-down sym . opts)
  (unless (call-graph-built?)
          (call-graph-refresh!))
  
  (let* ([max-depth (if (null? opts) 5 (car opts))]
         [layers (dep-slice-layers-down sym max-depth)])
        (display "\n")
        (printf "  Transitive Dependencies of '~a':\n" sym)
        (display "  ────────────────────────────────\n")
        (if (null? layers)
            (display "    (no dependencies found)\n")
            (for-each
             (lambda (layer)
                     (let ([depth (car layer)]
                           [syms (cdr layer)])
                          (printf "    Level ~a (~a):\n" depth (length syms))
                          (for-each
                           (lambda (s)
                                   (printf "      ~a\n" s))
                           (take syms 10))
                          (when (> (length syms) 10)
                                (printf "      ... and ~a more\n" (- (length syms) 10)))))
             layers))
        (display "\n")))

;;; ====
;;; Path Finding
;;; ====

;;; lens-path : Symbol × Symbol [Nat] -> void
;;; Find call paths between two symbols.
(define (lens-path from to . opts)
  (unless (call-graph-built?)
          (call-graph-refresh!))
  
  (let* ([max-depth (if (null? opts) 6 (car opts))]
         [paths (call-graph-path from to max-depth)])
        (display "\n")
        (printf "  Call Paths from '~a' to '~a':\n" from to)
        (display "  ────────────────────────────────\n")
        (if (not paths)
            (printf "    (no path found within ~a steps)\n" max-depth)
            (let ([shown-paths (take paths 5)])
                 (for-each
                  (lambda (path)
                          (display "    ")
                          (let loop ([p path])
                               (display (car p))
                               (when (pair? (cdr p))
                                     (display " → ")
                                     (loop (cdr p))))
                          (display "\n"))
                  shown-paths)
                 (when (> (length paths) 5)
                       (printf "\n    ... and ~a more paths\n"
                               (- (length paths) 5)))))
        (display "\n")))

;;; ====
;;; Statistics and Maintenance
;;; ====

;;; lens-stats : -> void
;;; Show overall lens navigation statistics.
(define (lens-stats)
  (display "\n")
  (display "  ┌────────────────────────────────────────────────────────────────────┐\n")
  (display "  │                    LENS NAVIGATION STATISTICS                      │\n")
  (display "  └────────────────────────────────────────────────────────────────────┘\n")
  (display "\n")
  
  ;; Call graph stats
  (if (call-graph-built?)
      (call-graph-stats)
      (display "  Call graph not built. Use (lens-rebuild!) to build.\n\n"))
  
  ;; Symbol index stats (from index.ss)
  (guard (e [else (void)])
         (when (and (top-level-bound? 'index-stats)
                    (procedure? index-stats))
               (index-stats)))
  
  ;; Test file count
  (let ([tests (find-all-test-files)])
       (printf "  Test files found: ~a\n\n" (length tests))))

;;; lens-rebuild! : -> void
;;; Rebuild all lens indices.
(define (lens-rebuild!)
  (display "\n")
  (display "  Rebuilding Lens Navigation Indices...\n")
  (display "  ────────────────────────────────\n\n")
  
  ;; Rebuild symbol index if available
  (guard (e [else (void)])
         (when (and (top-level-bound? 'index-refresh!)
                    (procedure? index-refresh!))
               (display "  Step 1: Rebuilding symbol index\n")
               (index-refresh!)))
  
  ;; Rebuild call graph
  (display "  Step 2: Rebuilding call graph\n")
  (call-graph-refresh!)
  
  (display "\n  Lens navigation ready!\n\n"))

;;; ====
;;; Quick Reference
;;; ====

;;; lens-help : -> void
;;; Show lens navigation quick reference.
(define (lens-help)
  (display "\n")
  (display "  ┌────────────────────────────────────────────────────────────────────┐\n")
  (display "  │                   LENS NAVIGATION COMMANDS                         │\n")
  (display "  └────────────────────────────────────────────────────────────────────┘\n")
  (display "\n")
  (display "  Navigation:\n")
  (display "    (lens-jump 'symbol)          Jump to definition\n")
  (display "    (lens-callers 'symbol)       Who calls this?\n")
  (display "    (lens-callees 'symbol)       What does this call?\n")
  (display "    (lens-test 'symbol)          Find related tests\n")
  (display "\n")
  (display "  Dependency Analysis:\n")
  (display "    (lens-slice-up 'symbol)      Transitive dependents\n")
  (display "    (lens-slice-down 'symbol)    Transitive dependencies\n")
  (display "    (lens-path 'from 'to)        Find call path\n")
  (display "\n")
  (display "  Maintenance:\n")
  (display "    (lens-stats)                 Show statistics\n")
  (display "    (lens-rebuild!)              Rebuild all indices\n")
  (display "    (lens-help)                  Show this help\n")
  (display "\n"))

;;; ====
;;; Structured Output (for programmatic use)
;;; ====

;;; lens-query : Symbol × Symbol -> S-expr
;;; Generic query interface returning structured data.
;;; query-type: 'location | 'callers | 'callees | 'tests | 'slice-up | 'slice-down
(define (lens-query sym query-type)
  (case query-type
        [(location)
         (let ([loc (jump-location sym)])
              (if loc `(ok ,loc) '(error not-found)))]
        [(callers)
         `(ok ,(call-graph-callers sym))]
        [(callees)
         `(ok ,(call-graph-callees sym))]
        [(tests)
         `(ok ,(find-tests-for sym))]
        [(slice-up)
         `(ok ,(dep-slice-up sym))]
        [(slice-down)
         `(ok ,(dep-slice-down sym))]
        [else
         `(error unknown-query ,query-type)]))

;;; ====
;;; Auto-initialization
;;; ====

(display "\n")
(display "  Lens Navigation System Loaded\n")
(display "  ────────────────────────────────\n")
(display "  Use (lens-help) for available commands.\n")
(display "  Use (lens-rebuild!) to build indices.\n")
(display "\n")
