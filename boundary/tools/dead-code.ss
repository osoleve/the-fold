;;; boundary/tools/dead-code.ss — Dead Code and Unused Binding Detection
;;;
;;; Finds unused definitions across the codebase using the symbol index
;;; and call graph. Provides safe-delete suggestions.
;;;
;;; Features:
;;;   - Find globally unused definitions (defined but never called)
;;;   - Find locally unused bindings in let/let*/letrec forms
;;;   - Identify dead branches in conditionals
;;;   - Safe-delete suggestions with impact analysis
;;;
;;; This is Shell code: uses index and lens systems for analysis.
;;;
;;; Usage:
;;;   (dead-code-scan)                 - Scan entire codebase
;;;   (dead-code-scan "dir")           - Scan specific directory
;;;   (dead-code-file "file.ss")       - Analyze single file
;;;   (dead-code-suggest-delete 'sym)  - Safe-delete analysis
;;;   (dead-code-stats)                - Show statistics
;;;
;;; Dependencies:
;;;   boundary/tools/index.ss (symbol index)
;;;   boundary/lens/call-graph.ss (call graph)

;;; ====
;;; Dependencies
;;; ====

(load "core/base/prelude.ss")

;;; ====
;;; Analysis State
;;; ====

;;; All defined symbols found
(define *all-definitions* '())

;;; All referenced symbols found - now stores (count . source-files)
(define *all-references* (make-eq-hashtable))

;;; Unused symbols detected
(define *unused-symbols* '())

;;; Confidence levels for dead code:
;;;   - definitely-dead: Zero references anywhere
;;;   - probably-dead: Only self-references or only in test files
;;;   - possibly-dead: Only 1-2 callers (candidates for inlining)
;;;   - low-usage: Referenced but in limited contexts
(define *dead-code-by-confidence* '())

;;; Built-in/standard symbols to ignore
(define *ignore-symbols*
  '(;; R6RS/Chez standard
    define define-syntax lambda let let* letrec letrec* if cond case
    and or not when unless begin set! quote quasiquote unquote
    car cdr cons list vector string null? pair? list? symbol? string?
    number? boolean? procedure? vector? eq? eqv? equal? + - * /
    < > <= >= = map filter fold-left fold-right for-each apply
    display newline format printf error guard call/cc call-with-values
    values with-exception-handler raise raise-continuable
    ;; Common test/framework symbols
    test-case check assert expect run-tests
    ;; Module system
    library import export))

;;; ====
;;; Symbol Collection
;;; ====

;;; collect-definitions : String -> (List (symbol file line))
;;; Collect all definitions from a file.
(define (collect-definitions file)
  (guard (e [else '()])
         (call-with-input-file file
                               (lambda (port)
                                       (let loop ([line-num 1] [results '()])
                                            (let ([expr (guard (e [else #f]) (read port))])
                                                 (cond
                                                  [(eof-object? expr) (reverse results)]
                                                  [(not expr) (reverse results)]
                                                  [else
                                                   (let ([defs (extract-defs expr file line-num)])
                                                        (loop (+ line-num 1)
                                                              (append defs results)))])))))))

;;; extract-defs : S-expr × String × Nat -> (List (symbol file line))
;;; Extract defined names from an expression.
(define (extract-defs expr file line)
  (cond
   [(not (pair? expr)) '()]
   [(eq? (car expr) 'define)
    (cond
     [(and (pair? (cdr expr)) (symbol? (cadr expr)))
      (list (list (cadr expr) file line))]
     [(and (pair? (cdr expr)) (pair? (cadr expr)))
      (list (list (caadr expr) file line))]
     [else '()])]
   [(eq? (car expr) 'define-syntax)
    (if (and (pair? (cdr expr)) (symbol? (cadr expr)))
        (list (list (cadr expr) file line))
        '())]
   [else '()]))

;;; collect-references : String -> void
;;; Collect all symbol references from a file into *all-references*.
;;; Now tracks count and source files for each symbol.
(define (collect-references file)
  (guard (e [else (void)])
         (call-with-input-file file
                               (lambda (port)
                                       (let loop ()
                                            (let ([expr (guard (e [else #f]) (read port))])
                                                 (cond
                                                  [(eof-object? expr) (void)]
                                                  [(not expr) (void)]
                                                  [else
                                                   (walk-for-refs expr file)
                                                   (loop)])))))))

;;; walk-for-refs : S-expr × String -> void
;;; Walk expression tree and record all symbol references with source file.
;;; Entry format: (count . files-hashtable) for O(1) file membership check.
(define (walk-for-refs expr file)
  (cond
   [(symbol? expr)
    (let ([existing (hashtable-ref *all-references* expr #f)])
         (if existing
             ;; Update count and add file if not already there
             ;; BUGFIX: Use hashtable for files to avoid O(N) member check
             (let ([count (car existing)]
                   [files-ht (cdr existing)])
                  (hashtable-set! files-ht file #t)
                  (hashtable-set! *all-references* expr
                                  (cons (+ count 1) files-ht)))
             ;; New entry - use hashtable for files
             (let ([files-ht (make-hashtable string-hash string=?)])
                  (hashtable-set! files-ht file #t)
                  (hashtable-set! *all-references* expr (cons 1 files-ht)))))]
   [(pair? expr)
    (walk-for-refs (car expr) file)
    (walk-for-refs (cdr expr) file)]
   [else (void)]))

;;; get-ref-count : Symbol -> Nat
;;; Get reference count for a symbol.
(define (get-ref-count sym)
  (let ([entry (hashtable-ref *all-references* sym #f)])
       (if entry (car entry) 0)))

;;; get-ref-files : Symbol -> (List String)
;;; Get files that reference a symbol.
(define (get-ref-files sym)
  (let ([entry (hashtable-ref *all-references* sym #f)])
       (if entry
           ;; Convert hashtable keys (vector) to list
           (vector->list (hashtable-keys (cdr entry)))
           '())))

;;; ====
;;; Confidence Level Classification
;;; ====

;;; classify-definition : (sym file line) -> (sym file line confidence reason)
;;; Classify a definition by how likely it is to be dead code.
(define (classify-definition def)
  (let* ([sym (car def)]
         [def-file (cadr def)]
         [line (caddr def)]
         [ref-count (get-ref-count sym)]
         [ref-files (get-ref-files sym)])
        (cond
         ;; Skip built-ins and test-related
         [(or (memq sym *ignore-symbols*)
              (test-related? sym)
              (exported? sym def-file))
          #f]
         
         ;; Definitely dead: zero references anywhere
         [(= ref-count 0)
          (list sym def-file line 'definitely-dead "no references")]
         
         ;; Probably dead: only referenced from test files (not from main code)
         [(all-test-files? ref-files)
          (list sym def-file line 'probably-dead
                (format "only ~a refs in test files" ref-count))]
         
         ;; Internal helpers: only self-references from same file
         ;; This is NORMAL for internal/private helpers - low priority
         [(and (= (length ref-files) 1)
               (string=? (car ref-files) def-file))
          ;; Only flag if very few references (might be dead code within the module)
          (if (<= ref-count 2)
              (list sym def-file line 'low-usage
                    (format "internal helper with ~a refs" ref-count))
              #f)]  ; Internal helper with many refs is fine
         
         ;; Possibly dead: very few callers from OTHER files (candidates for inlining)
         [(and (<= ref-count 2) (<= (length ref-files) 2))
          (list sym def-file line 'possibly-dead
                (format "only ~a refs in ~a files" ref-count (length ref-files)))]
         
         ;; Low usage: few callers but not dead
         [(<= ref-count 5)
          (list sym def-file line 'low-usage
                (format "~a refs in ~a files" ref-count (length ref-files)))]
         
         ;; Well-used
         [else #f])))

;;; all-test-files? : (List String) -> Boolean
;;; Check if all files are test files.
(define (all-test-files? files)
  (and (not (null? files))
       (andmap test-file? files)))

;;; test-file? : String -> Boolean
(define (test-file? file)
  (or (string-contains? file "test-")
      (string-contains? file "-test")
      (string-contains? file "/tests/")
      (string-contains? file "/test/")))

;;; string-contains? : String × String -> Boolean
(define (string-contains? str sub)
  (let ([slen (string-length str)]
        [sublen (string-length sub)])
       (let loop ([i 0])
            (cond
             [(> (+ i sublen) slen) #f]
             [(string-prefix-at? str sub i) #t]
             [else (loop (+ i 1))]))))

;;; string-prefix-at? : String × String × Nat -> Boolean
(define (string-prefix-at? str prefix start)
  (let ([plen (string-length prefix)]
        [slen (string-length str)])
       (and (<= (+ start plen) slen)
            (let loop ([i 0])
                 (or (>= i plen)
                     (and (char=? (string-ref str (+ start i))
                                  (string-ref prefix i))
                          (loop (+ i 1))))))))

;;; ====
;;; Dead Code Detection
;;; ====

;;; find-scheme-files : String -> (List String)
(define (find-scheme-files dir)
  (define (skip-dir? name)
    (or (string=? name ".")
        (string=? name "..")
        (string=? name ".git")
        (string=? name ".store")
        (string=? name ".fold-repl")
        (string=? name ".fold-sessions")
        (string=? name "node_modules")))
  
  (guard (e [else '()])
         (let loop ([pending (list dir)] [results '()])
              (if (null? pending)
                  results
                  (let ([current (car pending)]
                        [rest (cdr pending)])
                       (cond
                        [(not (file-exists? current))
                         (loop rest results)]
                        [(file-directory? current)
                         (let ([entries (guard (e [else '()])
                                               (directory-list current))])
                              (let ([full-paths
                                     (filter
                                      (lambda (p) p)
                                      (map (lambda (name)
                                                   (if (skip-dir? name)
                                                       #f
                                                       (string-append current "/" name)))
                                           entries))])
                                   (loop (append full-paths rest) results)))]
                        [(string-ends-with? current ".ss")
                         (loop rest (cons current results))]
                        [else
                         (loop rest results)]))))))

;;; dead-code-scan : [String] -> (List classified-def)
;;; Scan for unused/low-usage definitions with confidence levels.
(define (dead-code-scan . args)
  (let ([dir (if (null? args) "." (car args))])
       
       (display "\n")
       (printf "  Dead Code Analysis: ~a\n" dir)
       (display "  ────────────────────────────────\n\n")
       
       ;; Reset state
       (set! *all-definitions* '())
       (hashtable-clear! *all-references*)
       (set! *unused-symbols* '())
       (set! *dead-code-by-confidence* '())
       
       ;; Find all files
       (let ([files (find-scheme-files dir)])
            (printf "  Scanning ~a files...\n" (length files))
            
            ;; Phase 1: Collect all definitions
            (display "  Phase 1: Collecting definitions...\n")
            (for-each
             (lambda (file)
                     (let ([defs (collect-definitions file)])
                          (set! *all-definitions*
                                (append defs *all-definitions*))))
             files)
            (printf "    Found ~a definitions\n" (length *all-definitions*))
            
            ;; Phase 2: Collect all references
            (display "  Phase 2: Collecting references...\n")
            (for-each collect-references files)
            (printf "    Found ~a unique references\n"
                    (hashtable-size *all-references*))
            
            ;; Phase 3: Classify all definitions
            (display "  Phase 3: Classifying definitions...\n")
            (let ([classified (filter identity
                                      (map classify-definition *all-definitions*))])
                 
                 ;; Group by confidence level
                 (let ([definitely-dead (filter (lambda (c) (eq? (list-ref c 3) 'definitely-dead)) classified)]
                       [probably-dead (filter (lambda (c) (eq? (list-ref c 3) 'probably-dead)) classified)]
                       [possibly-dead (filter (lambda (c) (eq? (list-ref c 3) 'possibly-dead)) classified)]
                       [low-usage (filter (lambda (c) (eq? (list-ref c 3) 'low-usage)) classified)])
                      
                      (set! *dead-code-by-confidence*
                            (list (cons 'definitely-dead definitely-dead)
                                  (cons 'probably-dead probably-dead)
                                  (cons 'possibly-dead possibly-dead)
                                  (cons 'low-usage low-usage)))
                      
                      ;; Also set *unused-symbols* for backwards compatibility
                      (set! *unused-symbols* definitely-dead)
                      
                      ;; Report results
                      (display "\n")
                      (display "  ════════════════════════════════════════════════════════════\n")
                      (display "                     DEAD CODE ANALYSIS RESULTS               \n")
                      (display "  ════════════════════════════════════════════════════════════\n\n")
                      
                      ;; Report definitely dead
                      (report-confidence-level 'definitely-dead "DEFINITELY DEAD" "🔴"
                                               "Zero references - safe to delete"
                                               definitely-dead)
                      
                      ;; Report probably dead
                      (report-confidence-level 'probably-dead "PROBABLY DEAD" "🟠"
                                               "Only self-refs or only in test files"
                                               probably-dead)
                      
                      ;; Report possibly dead
                      (report-confidence-level 'possibly-dead "POSSIBLY DEAD" "🟡"
                                               "Very few callers - inline candidates"
                                               possibly-dead)
                      
                      ;; Report low usage (optionally)
                      (when (> (length low-usage) 0)
                            (printf "\n  Low Usage (~a): Use (dead-code-show 'low-usage) to see.\n"
                                    (length low-usage)))
                      
                      ;; Summary
                      (display "\n")
                      (display "  ────────────────────────────────────────────────────────────\n")
                      (printf "  Summary: ~a definitely, ~a probably, ~a possibly dead\n"
                              (length definitely-dead)
                              (length probably-dead)
                              (length possibly-dead))
                      (display "  Use (dead-code-show 'level) to see details for a level.\n\n")
                      
                      classified)))))

;;; report-confidence-level : Symbol × String × String × String × List -> void
(define (report-confidence-level level label icon desc items)
  (printf "\n  ~a ~a (~a)\n" icon label (length items))
  (printf "  ~a\n" desc)
  (display "  ────────────────────────────────\n")
  (if (null? items)
      (display "    (none)\n")
      (for-each
       (lambda (item)
               (let ([sym (car item)]
                     [file (cadr item)]
                     [line (caddr item)]
                     [reason (list-ref item 4)])
                    (printf "    ~a (~a:~a)\n      ~a\n"
                            sym file line reason)))
       (take 15 items)))
  (when (> (length items) 15)
        (printf "    ... and ~a more\n" (- (length items) 15))))

;;; dead-code-show : Symbol -> void
;;; Show details for a confidence level.
(define (dead-code-show level)
  (let ([group (assq level *dead-code-by-confidence*)])
       (if (not group)
           (begin
            (display "\n  Unknown level. Valid levels:\n")
            (display "    'definitely-dead, 'probably-dead, 'possibly-dead, 'low-usage\n\n"))
           (let ([items (cdr group)])
                (printf "\n  ~a (~a items):\n" level (length items))
                (display "  ────────────────────────────────\n")
                (for-each
                 (lambda (item)
                         (printf "    ~a (~a:~a)\n      ~a\n"
                                 (car item) (cadr item) (caddr item)
                                 (list-ref item 4)))
                 items)
                (display "\n")))))

;;; group-by-file : (List (sym file line)) -> (List (file . defs))
(define (group-by-file defs)
  (let ([groups (make-hashtable string-hash string=?)])
       (for-each
        (lambda (def)
                (let* ([file (cadr def)]
                       [existing (hashtable-ref groups file '())])
                      (hashtable-set! groups file (cons def existing))))
        defs)
       (let ([result '()])
            (vector-for-each
             (lambda (key)
                     (set! result (cons (cons key (hashtable-ref groups key '()))
                                        result)))
             (hashtable-keys groups))
            (sort-groups result))))

;;; sort-groups : (List (file . defs)) -> (List (file . defs))
(define (sort-groups groups)
  (list-sort (lambda (a b) (string<? (car a) (car b))) groups))

;;; test-related? : Symbol -> Boolean
;;; Check if symbol is likely test-related (should not be flagged).
(define (test-related? sym)
  (let ([name (symbol->string sym)])
       (or (string-starts-with? name "test-")
           (string-ends-with? name "-test")
           (string-starts-with? name "check-")
           (string-starts-with? name "assert-")
           (string-starts-with? name "expect-"))))

;;; exported? : Symbol × String -> Boolean
;;; Check if symbol might be exported from the module.
;;; More conservative: only considers explicitly exported if file has export form.
(define (exported? sym file)
  (let ([name (symbol->string sym)])
       (or
        ;; Private by convention (underscore prefix)
        ;; These are NEVER exported, so return #f to include them in analysis
        ;; (Actually we want to include them, so this should be negated below)
        
        ;; Check if it's a public API by name pattern
        ;; Entry points typically start with the module name or are common API patterns
        (and (or (string-starts-with? name "dead-code-")
                 (string-starts-with? name "refactor-")
                 (string-starts-with? name "type-search-")
                 (string-starts-with? name "lens-")
                 (string-starts-with? name "index-"))
             ;; But only if it doesn't start with underscore
             (not (string-starts-with? name "_"))))))

;;; ====
;;; Unused Local Bindings
;;; ====

;;; find-unused-locals : String -> (List (name file line binding-form))
;;; Find unused local bindings in let/let*/letrec forms.
(define (find-unused-locals file)
  (guard (e [else '()])
         (let ([content (call-with-input-file file get-string-all)]
               [port (open-input-file file)]
               [unused '()])
              (let loop ([line-num 1])
                   (let ([expr (guard (e [else #f]) (read port))])
                        (cond
                         [(eof-object? expr) (close-input-port port)]
                         [(not expr) (close-input-port port)]
                         [else
                          (let ([found (find-unused-in-expr expr file line-num)])
                               (set! unused (append found unused)))
                          (loop (+ line-num 1))])))
              (reverse unused))))

;;; find-unused-in-expr : S-expr × String × Nat -> (List unused)
;;; Recursively find unused bindings in an expression.
(define (find-unused-in-expr expr file line)
  (cond
   [(not (pair? expr)) '()]
   [(memq (car expr) '(let let* letrec letrec*))
    (if (and (pair? (cdr expr))
             (or (pair? (cadr expr))     ; Regular let
                 (symbol? (cadr expr)))) ; Named let
        (let* ([is-named? (symbol? (cadr expr))]
               [bindings (if is-named? (caddr expr) (cadr expr))]
               [body (if is-named? (cdddr expr) (cddr expr))])
              (if (list? bindings)
                  (let ([bound-names (map car bindings)]
                        [body-refs (collect-body-refs body)])
                       ;; Find which bound names aren't used in body
                       (let ([unused-here
                              (filter
                               (lambda (name)
                                       (not (memq name body-refs)))
                               bound-names)])
                            (append
                             (map (lambda (n) (list n file line (car expr)))
                                  unused-here)
                             ;; Recurse into binding expressions and body
                             (apply append
                                    (map (lambda (b)
                                                 (if (pair? (cdr b))
                                                     (find-unused-in-expr (cadr b) file line)
                                                     '()))
                                         bindings))
                             (apply append
                                    (map (lambda (e) (find-unused-in-expr e file line))
                                         body)))))
                  '()))
        '())]
   [(eq? (car expr) 'lambda)
    ;; Check for unused lambda parameters
    (if (and (pair? (cdr expr)) (pair? (cddr expr)))
        (let ([params (cadr expr)]
              [body (cddr expr)])
             (if (list? params)
                 (let ([body-refs (collect-body-refs body)]
                       [unused-params
                        (filter
                         (lambda (p)
                                 (and (symbol? p)
                                      (not (memq p body-refs))))
                         params)])
                      (append
                       (map (lambda (p) (list p file line 'lambda)) unused-params)
                       (apply append
                              (map (lambda (e) (find-unused-in-expr e file line))
                                   body))))
                 '()))
        '())]
   [else
    ;; Recurse into subexpressions
    (apply append
           (map (lambda (e) (find-unused-in-expr e file line))
                expr))]))

;;; collect-body-refs : S-expr -> (List Symbol)
;;; Collect all symbol references in body expressions.
(define (collect-body-refs body)
  (let ([refs '()])
       (letrec ([walk (lambda (expr)
                              (cond
                               [(symbol? expr) (set! refs (cons expr refs))]
                               [(pair? expr)
                                (walk (car expr))
                                (walk (cdr expr))]
                               [else (void)]))])
               (walk body)
               refs)))

;;; ====
;;; Safe Delete Analysis
;;; ====

;;; dead-code-suggest-delete : Symbol -> void
;;; Analyze safety of deleting a symbol.
(define (dead-code-suggest-delete sym)
  (display "\n")
  (printf "  Safe-Delete Analysis: '~a'\n" sym)
  (display "  ────────────────────────────────\n\n")
  
  ;; Find definition
  (let ([def (find-def-in-list sym *all-definitions*)])
       (if (not def)
           (printf "  Symbol '~a' not found in current analysis.\n" sym)
           (let ([file (cadr def)]
                 [line (caddr def)])
                
                (printf "  Definition: ~a:~a\n\n" file line)
                
                ;; Check for references
                (let ([is-referenced (hashtable-ref *all-references* sym #f)])
                     (if is-referenced
                         (begin
                          (display "  WARNING: Symbol is referenced in codebase!\n")
                          (display "  Deletion would cause errors.\n")
                          (display "\n  Callers:\n")
                          ;; Try to find callers using call graph
                          (guard (e [else (display "    (call graph not loaded)\n")])
                                 (when (and (top-level-bound? 'call-graph-callers)
                                            (procedure? call-graph-callers))
                                       (let ([callers (call-graph-callers sym)])
                                            (if (null? callers)
                                                (display "    (no direct callers found)\n")
                                                (for-each
                                                 (lambda (c) (printf "    ~a\n" c))
                                                 (take 10 callers)))))))
                         (begin
                          (display "  SAFE TO DELETE: No references found.\n\n")
                          (display "  To delete, remove the definition from:\n")
                          (printf "    ~a:~a\n" file line)))))))
  (display "\n"))

;;; find-def-in-list : Symbol × (List def) -> def | #f
(define (find-def-in-list sym defs)
  (let loop ([defs defs])
       (cond
        [(null? defs) #f]
        [(eq? (caar defs) sym) (car defs)]
        [else (loop (cdr defs))])))

;;; ====
;;; Statistics and Reporting
;;; ====

;;; dead-code-stats : -> void
;;; Show analysis statistics.
(define (dead-code-stats)
  (display "\n")
  (display "  Dead Code Analysis Statistics\n")
  (display "  ────────────────────────────────\n\n")
  (printf "  Total definitions analyzed: ~a\n" (length *all-definitions*))
  (printf "  Unique references found:    ~a\n" (hashtable-size *all-references*))
  (printf "  Unused definitions:         ~a\n" (length *unused-symbols*))
  
  (when (> (length *unused-symbols*) 0)
        (display "\n  Top unused symbols:\n")
        (for-each
         (lambda (def)
                 (printf "    ~a (~a:~a)\n" (car def) (cadr def) (caddr def)))
         (take 10 *unused-symbols*))
        (when (> (length *unused-symbols*) 10)
              (printf "    ... and ~a more\n" (- (length *unused-symbols*) 10))))
  (display "\n"))

;;; dead-code-file : String -> void
;;; Analyze a single file for unused locals.
(define (dead-code-file file)
  (display "\n")
  (printf "  Unused Local Bindings: ~a\n" file)
  (display "  ────────────────────────────────\n\n")
  
  (let ([unused (find-unused-locals file)])
       (if (null? unused)
           (display "    No unused local bindings found.\n")
           (for-each
            (lambda (u)
                    (printf "    Line ~a: ~a (in ~a form)\n"
                            (caddr u) (car u) (cadddr u)))
            unused)))
  (display "\n"))

;;; ====
;;; REPL Commands
;;; ====

;;; dead-code-help : -> void
(define (dead-code-help)
  (display "\n")
  (display "  ┌────────────────────────────────────────────────────────────────────┐\n")
  (display "  │                  DEAD CODE DETECTION COMMANDS                      │\n")
  (display "  └────────────────────────────────────────────────────────────────────┘\n")
  (display "\n")
  (display "  Global Analysis:\n")
  (display "    (dead-code-scan)              Scan entire codebase\n")
  (display "    (dead-code-scan \"dir\")        Scan specific directory\n")
  (display "    (dead-code-stats)             Show analysis statistics\n")
  (display "\n")
  (display "  Confidence Levels:\n")
  (display "    (dead-code-show 'definitely-dead)  Zero references\n")
  (display "    (dead-code-show 'probably-dead)    Only self/test refs\n")
  (display "    (dead-code-show 'possibly-dead)    1-2 callers only\n")
  (display "    (dead-code-show 'low-usage)        Few callers (<6)\n")
  (display "\n")
  (display "  File Analysis:\n")
  (display "    (dead-code-file \"file.ss\")    Find unused locals in file\n")
  (display "\n")
  (display "  Safe Delete:\n")
  (display "    (dead-code-suggest-delete 'sym)   Analyze deletion safety\n")
  (display "\n")
  (display "  Help:\n")
  (display "    (dead-code-help)              Show this help\n")
  (display "\n"))

;;; ====
;;; Initialization
;;; ====

(display "\n")
(display "  Dead Code Detection Loaded\n")
(display "  ────────────────────────────────\n")
(display "  Use (dead-code-help) for available commands.\n")
(display "  Use (dead-code-scan) to analyze the codebase.\n")
(display "\n")
