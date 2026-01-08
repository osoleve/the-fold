;;; shell/module-deps.ss — Module Dependency Analyzer
;;;
;;; Analyzes Scheme source files to extract and visualize dependencies.
;;; Helps understand the module structure and identify circular dependencies.
;;;
;;; Features:
;;;   - Extract (load ...) dependencies from source files
;;;   - Build dependency graph
;;;   - Detect circular dependencies
;;;   - Generate dependency reports
;;;   - Find reverse dependencies (what depends on X?)
;;;
;;; This is Shell code: reads files, handles IO.
;;;
;;; Usage:
;;;   (analyze-module-deps fs "shell/repl.ss")
;;;   (find-module-deps fs "core")               ; analyze entire directory
;;;   (find-reverse-deps fs "core/blocks/block.ss")     ; what depends on this?
;;;   (check-circular-deps fs "shell")           ; find cycles
;;;   (deps-report fs "." "dependency-report.txt") ; generate report
;;;
;;; Dependencies:
;;;   shell/fs.ss
;;;   shell/edit.ss

;;; Set up source-directories to find shell modules
(source-directories (cons "shell" (source-directories)))

(load "fs.ss")
(load "edit.ss")
(load "core/base/prelude.ss")

;;; NOTE: string-contains?, string-starts-with?, string-last-index-of, string-join
;;;       are provided by core/prelude.ss

;;; ============================================================
;;; Dependency Extraction
;;; ============================================================

;;; extract-loads : String → (List String)
;;; Extract all (load "...") expressions from source code.
;;; Returns list of file paths that are loaded.
(define (extract-loads source-code)
  (let ([port (open-input-string source-code)])
       (let loop ([loads '()])
            (let ([expr (guard (e [else #f])
                               (read port))])
                 (cond
                  [(eof-object? expr) (reverse loads)]
                  [(not expr) (reverse loads)]  ; parse error
                  [(and (pair? expr)
                        (eq? (car expr) 'load)
                        (pair? (cdr expr))
                        (string? (cadr expr)))
                   (loop (cons (cadr expr) loads))]
                  [else (loop loads)])))))

;;; normalize-path : String × String → String
;;; Normalize a load path relative to a source file.
;;; E.g., if "shell/repl.ss" loads "fs.ss", result is "shell/fs.ss"
(define (normalize-path base-file load-path)
  (cond
   ;; Absolute or rooted path
   [(or (string-starts-with? load-path "/")
        (string-starts-with? load-path "../"))
    load-path]
   ;; Relative to base file's directory
   [else
    (let* ([base-dir (path-directory base-file)]
           [normalized (if (string=? base-dir "")
                           load-path
                           (string-append base-dir "/" load-path))])
          ;; Normalize ".." components
          (normalize-path-components normalized))]))

;;; path-directory : String → String
;;; Extract directory part of a file path.
;;; E.g., "shell/repl.ss" → "shell", "foo.ss" → ""
(define (path-directory path)
  (let ([idx (string-last-index-of path "/")])
       (if idx
           (substring path 0 idx)
           "")))

;;; normalize-path-components : String → String
;;; Normalize ".." components in a path.
;;; E.g., "shell/../core/block.ss" → "core/blocks/block.ss"
(define (normalize-path-components path)
  (let* ([parts (string-split-path path)]
         [normalized (normalize-parts parts)])
        (string-join-path normalized)))

;;; string-split-path : String → (List String)
;;; Split path by "/" into components.
;;; NOTE: This differs from (string-split path #\/) because it filters out
;;;       empty components (handling consecutive slashes correctly).
(define (string-split-path path)
  (let loop ([chars (string->list path)]
             [current '()]
             [result '()])
       (cond
        [(null? chars)
         (if (null? current)
             (reverse result)
             (reverse (cons (list->string (reverse current)) result)))]
        [(char=? (car chars) #\/)
         (if (null? current)
             (loop (cdr chars) '() result)  ; skip empty components
             (loop (cdr chars) '() (cons (list->string (reverse current)) result)))]
        [else
         (loop (cdr chars) (cons (car chars) current) result)])))

;;; normalize-parts : (List String) → (List String)
;;; Remove ".." and "." components from path parts.
(define (normalize-parts parts)
  (let loop ([remaining parts]
             [result '()])
       (cond
        [(null? remaining) (reverse result)]
        [(string=? (car remaining) "..")
         (if (null? result)
             (loop (cdr remaining) result)  ; Can't go up from root
             (loop (cdr remaining) (cdr result)))]  ; Go up one level
        [(string=? (car remaining) ".")
         (loop (cdr remaining) result)]  ; Skip current directory
        [else
         (loop (cdr remaining) (cons (car remaining) result))])))

;;; string-join-path : (List String) → String
;;; Join path components with "/".
;;; NOTE: Uses string-join from core/base/prelude.ss
(define (string-join-path parts)
  (string-join parts "/"))

;;; analyze-file-deps : FS × String → (List String)
;;; Analyze a single file and return normalized dependency paths.
(define (analyze-file-deps fs file-path)
  (guard (e [else '()])  ; Return empty list on error
         (let* ([source (read-text-file fs file-path)]
                [loads (extract-loads source)])
               ;; Normalize each load path relative to this file
               (map (lambda (load-path)
                            (normalize-path file-path load-path))
                    loads))))

;;; ============================================================
;;; Dependency Graph Construction
;;; ============================================================

;;; build-dep-graph : FS × (List String) → Alist
;;; Build dependency graph for a list of files.
;;; Returns alist: ((file . (list of deps)) ...)
(define (build-dep-graph fs files)
  (map (lambda (file)
               (cons file (analyze-file-deps fs file)))
       files))

;;; find-all-scheme-files : FS × String → (List String)
;;; Recursively find all .ss files in a directory.
(define (find-all-scheme-files fs dir-path)
  (define (scheme-file? path)
    (and (>= (string-length path) 3)
         (string=? (substring path (- (string-length path) 3) (string-length path)) ".ss")))
  
  (let loop ([to-process (list dir-path)]
             [result '()])
       (if (null? to-process)
           result
           (let ([current (car to-process)]
                 [rest (cdr to-process)])
                (cond
                 [(not (file-exists? current))
                  (loop rest result)]
                 [(and (file-exists? current)
                       (guard (e [else #f])
                              (directory? (directory-list current))))
                  ;; It's a directory - add its contents
                  (let ([entries (guard (e [else '()])
                                        (directory-list current))])
                       (let ([full-paths (map (lambda (e)
                                                      (string-append current "/" e))
                                              entries)])
                            (loop (append full-paths rest) result)))]
                 [(scheme-file? current)
                  (loop rest (cons current result))]
                 [else
                  (loop rest result)])))))

;;; ============================================================
;;; Reverse Dependencies
;;; ============================================================

;;; find-reverse-deps : FS × String × Alist → (List String)
;;; Find all files that depend on the given file.
;;; dep-graph is the output of build-dep-graph.
(define (find-reverse-deps-from-graph target-file dep-graph)
  (filter (lambda (entry)
                  (let ([file (car entry)]
                        [deps (cdr entry)])
                       (member target-file deps)))
          dep-graph))

;;; ============================================================
;;; Circular Dependency Detection
;;; ============================================================

;;; find-circular-deps : Alist → (List (List String))
;;; Detect circular dependencies in the dependency graph.
;;; Returns list of cycles, each cycle is a list of file paths.
(define (find-circular-deps dep-graph)
  (let ([cycles '()])
       (for-each
        (lambda (entry)
                (let ([file (car entry)])
                     (let ([cycle (detect-cycle file dep-graph '())])
                          (when cycle
                                (set! cycles (cons cycle cycles))))))
        dep-graph)
       (remove-duplicate-cycles cycles)))

;;; detect-cycle : String × Alist × (List String) → (List String) | #f
;;; DFS to detect if there's a cycle starting from file.
;;; visited is the path we've taken so far.
(define (detect-cycle file dep-graph visited)
  (cond
   [(member file visited)
    ;; Found a cycle - return the cycle path
    (let loop ([path visited])
         (if (string=? (car path) file)
             (reverse (cons file path))
             (loop (cdr path))))]
   [else
    (let ([entry (assoc file dep-graph)])
         (if (not entry)
             #f
             (let ([deps (cdr entry)])
                  (let check-deps ([remaining deps])
                       (if (null? remaining)
                           #f
                           (or (detect-cycle (car remaining) dep-graph (cons file visited))
                               (check-deps (cdr remaining))))))))]))

;;; remove-duplicate-cycles : (List (List String)) → (List (List String))
;;; Remove duplicate cycle representations.
;;; Two cycles are the same if they contain the same files in the same order
;;; (possibly rotated).
(define (remove-duplicate-cycles cycles)
  (let loop ([remaining cycles]
             [seen '()])
       (if (null? remaining)
           seen
           (let ([cycle (car remaining)])
                (if (any (lambda (seen-cycle)
                                 (cycle-equal? cycle seen-cycle))
                         seen)
                    (loop (cdr remaining) seen)
                    (loop (cdr remaining) (cons cycle seen)))))))

;;; cycle-equal? : (List String) × (List String) → Boolean
;;; Check if two cycles are equal (considering rotation).
(define (cycle-equal? c1 c2)
  (and (= (length c1) (length c2))
       (or (equal? c1 c2)
           (any (lambda (rotation)
                        (equal? c1 rotation))
                (rotations c2)))))

;;; rotations : (List α) → (List (List α))
;;; Generate all rotations of a list.
(define (rotations lst)
  (if (null? lst)
      '(())
      (let loop ([n (length lst)]
                 [current lst]
                 [result '()])
           (if (= n 0)
               result
               (loop (- n 1)
                     (append (cdr current) (list (car current)))
                     (cons current result))))))

;;; any : (α → Boolean) × (List α) → Boolean
;;; Check if predicate holds for any element.
(define (any pred lst)
  (and (not (null? lst))
       (or (pred (car lst))
           (any pred (cdr lst)))))

;;; ============================================================
;;; Reporting
;;; ============================================================

;;; deps-report : FS × String × String → void
;;; Generate a comprehensive dependency report for a directory.
;;; Writes report to output-file.
(define (deps-report fs root-dir output-file)
  (let* ([files (find-all-scheme-files fs root-dir)]
         [dep-graph (build-dep-graph fs files)]
         [cycles (find-circular-deps dep-graph)])
        
        (call-with-output-file output-file
                               (lambda (port)
                                       (display "MODULE DEPENDENCY REPORT\n" port)
                                       (display "========================\n\n" port)
                                       
                                       (display (format "Root directory: ~a\n" root-dir) port)
                                       (display (format "Total files analyzed: ~a\n\n" (length files)) port)
                                       
                                       ;; Circular dependencies section
                                       (display "CIRCULAR DEPENDENCIES\n" port)
                                       (display "---------------------\n" port)
                                       (if (null? cycles)
                                           (display "None detected.\n\n" port)
                                           (begin
                                            (for-each
                                             (lambda (cycle)
                                                     (display "  Cycle: " port)
                                                     (display (string-join (map (lambda (f) f) cycle) " -> ") port)
                                                     (display "\n" port))
                                             cycles)
                                            (display "\n" port)))
                                       
                                       ;; Per-file dependencies
                                       (display "FILE DEPENDENCIES\n" port)
                                       (display "-----------------\n\n" port)
                                       (for-each
                                        (lambda (entry)
                                                (let ([file (car entry)]
                                                      [deps (cdr entry)])
                                                     (display (format "~a:\n" file) port)
                                                     (if (null? deps)
                                                         (display "  (no dependencies)\n" port)
                                                         (for-each
                                                          (lambda (dep)
                                                                  (display (format "  -> ~a\n" dep) port))
                                                          deps))
                                                     (display "\n" port)))
                                        dep-graph)
                                       
                                       ;; Reverse dependencies
                                       (display "REVERSE DEPENDENCIES\n" port)
                                       (display "--------------------\n\n" port)
                                       (for-each
                                        (lambda (entry)
                                                (let* ([file (car entry)]
                                                       [rdeps (find-reverse-deps-from-graph file dep-graph)])
                                                      (when (not (null? rdeps))
                                                            (display (format "~a is used by:\n" file) port)
                                                            (for-each
                                                             (lambda (rdep)
                                                                     (display (format "  <- ~a\n" (car rdep)) port))
                                                             rdeps)
                                                            (display "\n" port))))
                                        dep-graph)))
        
        (display (format "Report written to ~a\n" output-file))))

;;; ============================================================
;;; Public API
;;; ============================================================

;;; analyze-module-deps : FS × String → void
;;; Analyze and display dependencies for a single module.
(define (analyze-module-deps fs file-path)
  (let ([deps (analyze-file-deps fs file-path)])
       (display (format "\nDependencies for ~a:\n" file-path))
       (if (null? deps)
           (display "  (no dependencies)\n")
           (for-each
            (lambda (dep)
                    (display (format "  -> ~a\n" dep)))
            deps))
       (display "\n")))

;;; find-module-deps : FS × String → void
;;; Analyze all modules in a directory and display summary.
(define (find-module-deps fs dir-path)
  (let* ([files (find-all-scheme-files fs dir-path)]
         [dep-graph (build-dep-graph fs files)])
        (display (format "\nModule dependency analysis for: ~a\n" dir-path))
        (display (format "Total files: ~a\n\n" (length files)))
        
        (for-each
         (lambda (entry)
                 (let ([file (car entry)]
                       [deps (cdr entry)])
                      (display (format "~a (~a deps)\n" file (length deps)))))
         dep-graph)
        (display "\n")))

;;; find-reverse-deps : FS × String → void
;;; Find and display what depends on a given file.
(define (find-reverse-deps fs target-file)
  (let* ([dir (if (string-contains? target-file "/")
                  (path-directory target-file)
                  ".")]
         [files (find-all-scheme-files fs dir)]
         [dep-graph (build-dep-graph fs files)]
         [rdeps (find-reverse-deps-from-graph target-file dep-graph)])
        (display (format "\nReverse dependencies for ~a:\n" target-file))
        (if (null? rdeps)
            (display "  (no files depend on this)\n")
            (for-each
             (lambda (entry)
                     (display (format "  <- ~a\n" (car entry))))
             rdeps))
        (display "\n")))

;;; check-circular-deps : FS × String → void
;;; Check for circular dependencies in a directory.
(define (check-circular-deps fs dir-path)
  (let* ([files (find-all-scheme-files fs dir-path)]
         [dep-graph (build-dep-graph fs files)]
         [cycles (find-circular-deps dep-graph)])
        (display (format "\nCircular dependency check for: ~a\n" dir-path))
        (if (null? cycles)
            (display "  ✓ No circular dependencies detected.\n")
            (begin
             (display (format "  ✗ Found ~a circular dependencies:\n\n" (length cycles)))
             (for-each
              (lambda (cycle)
                      (display "    ")
                      (display (string-join (map (lambda (f) f) cycle) " -> "))
                      (display "\n"))
              cycles)))
        (display "\n")))

(display "Module dependency analyzer loaded.\n")
(display "Usage:\n")
(display "  (analyze-module-deps fs \"path/to/file.ss\")\n")
(display "  (find-module-deps fs \"directory\")\n")
(display "  (find-reverse-deps fs \"path/to/file.ss\")\n")
(display "  (check-circular-deps fs \"directory\")\n")
(display "  (deps-report fs \"directory\" \"output.txt\")\n")
