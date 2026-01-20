(load "core/base/prelude.ss")

(doc 'module 'archextract)
(doc 'description "ArchExtract: Seeing Structure in Code. Analyzes code structure and generates architecture visualizations. Reads Scheme source files, extracts dependencies, and builds a dependency graph.")
(doc 'layer 'boundary)
(doc 'purity 'partial)

(doc 'section 'path-utilities)

(doc arch-find-last-sep 'type "(-> String (Maybe Nat))")
(doc arch-find-last-sep 'description "Find the last path separator (/ or \\) in a path")
(define (arch-find-last-sep path)
  (let loop ([i (- (string-length path) 1)])
       (cond
        [(< i 0) #f]
        [(or (char=? (string-ref path i) #\/)
             (char=? (string-ref path i) #\\)) i]
        [else (loop (- i 1))])))

;;; path-basename : String -> String
;;; Get filename from path (after last separator).
(define (arch-path-basename path)
  (let ([sep-pos (arch-find-last-sep path)])
       (if sep-pos
           (substring path (+ sep-pos 1) (string-length path))
           path)))

;;; path-dirname : String -> String
;;; Get directory from path (before last separator).
(define (arch-path-dirname path)
  (let ([sep-pos (arch-find-last-sep path)])
       (if sep-pos
           (substring path 0 sep-pos)
           ".")))

;;; string-suffix? : String x String -> Boolean
;;; Check if str ends with suffix.
(define (arch-string-suffix? suffix str)
  (let ([slen (string-length suffix)]
        [len (string-length str)])
       (and (>= len slen)
            (string=? suffix (substring str (- len slen) len)))))

;;; normalize-path-sep : String -> String
;;; Convert backslashes to forward slashes for consistency.
(define (normalize-path-sep path)
  (list->string
   (map (lambda (c) (if (char=? c #\\) #\/ c))
        (string->list path))))

;;; resolve-relative-path : String x String -> String
;;; Resolve a relative path from a base directory.
;;; Handles ../ by removing path components.
(define (resolve-relative-path base-dir rel-path)
  (let* ([normalized-base (normalize-path-sep base-dir)]
         [normalized-rel (normalize-path-sep rel-path)]
         [base-parts (split-path normalized-base)]
         [rel-parts (split-path normalized-rel)])
        (join-path (resolve-parts base-parts rel-parts))))

;;; split-path : String -> (List String)
;;; Split path by forward slash.
(define (split-path path)
  (let loop ([chars (string->list path)] [current '()] [parts '()])
       (cond
        [(null? chars)
         (reverse (if (null? current)
                      parts
                      (cons (list->string (reverse current)) parts)))]
        [(char=? (car chars) #\/)
         (loop (cdr chars) '()
               (if (null? current)
                   parts
                   (cons (list->string (reverse current)) parts)))]
        [else
         (loop (cdr chars) (cons (car chars) current) parts)])))

;;; join-path : (List String) -> String
;;; Join path parts with forward slash.
(define (join-path parts)
  (if (null? parts)
      ""
      (fold-left (lambda (acc part)
                         (if (string=? acc "")
                             part
                             (string-append acc "/" part)))
                 ""
                 parts)))

;;; resolve-parts : (List String) x (List String) -> (List String)
;;; Resolve relative path parts against base parts.
(define (resolve-parts base-parts rel-parts)
  (if (null? rel-parts)
      base-parts
      (let ([part (car rel-parts)])
           (cond
            [(string=? part "..")
             (resolve-parts (if (null? base-parts)
                                '()
                                (reverse (cdr (reverse base-parts))))
                            (cdr rel-parts))]
            [(string=? part ".")
             (resolve-parts base-parts (cdr rel-parts))]
            [else
             (resolve-parts (append base-parts (list part)) (cdr rel-parts))]))))

;;; ====
;;; File Reading
;;; ====

;;; read-file-contents : String -> String | #f
;;; Read entire file as string. Returns #f if file doesn't exist.
(define (read-file-contents path)
  (if (file-exists? path)
      (call-with-input-file path
                            (lambda (port)
                                    (let loop ([chars '()])
                                         (let ([c (read-char port)])
                                              (if (eof-object? c)
                                                  (list->string (reverse chars))
                                                  (loop (cons c chars)))))))
      #f))

;;; read-file-sexprs : String -> (List Sexpr) | #f
;;; Read all s-expressions from a file.
(define (read-file-sexprs path)
  (if (file-exists? path)
      (call-with-input-file path
                            (lambda (port)
                                    (let loop ([sexprs '()])
                                         (let ([sexpr (read port)])
                                              (if (eof-object? sexpr)
                                                  (reverse sexprs)
                                                  (loop (cons sexpr sexprs)))))))
      #f))

;;; ====
;;; Module Analysis
;;; ====

;;; A Module is represented as an alist:
;;;   ((path . <string>)           ; absolute or relative path to file
;;;    (name . <string>)           ; basename without extension
;;;    (loads . <list-of-strings>) ; paths from (load ...) expressions
;;;    (defines . <list-of-symbols>)) ; top-level definitions (exports)

;;; extract-loads : (List Sexpr) -> (List String)
;;; Extract all (load "path") expressions from s-expressions.
(define (extract-loads sexprs)
  (let loop ([sexprs sexprs] [loads '()])
       (cond
        [(null? sexprs) (reverse loads)]
        [(and (pair? (car sexprs))
              (eq? (caar sexprs) 'load)
              (pair? (cdar sexprs))
              (string? (cadar sexprs)))
         (loop (cdr sexprs) (cons (cadar sexprs) loads))]
        [(pair? (car sexprs))
         ;; Recurse into nested expressions (e.g., inside when, unless, let)
         (loop (cdr sexprs) (append (reverse (extract-loads (car sexprs))) loads))]
        [else
         (loop (cdr sexprs) loads)])))

;;; extract-defines : (List Sexpr) -> (List Symbol)
;;; Extract top-level (define name ...) symbols.
(define (extract-defines sexprs)
  (let loop ([sexprs sexprs] [defines '()])
       (cond
        [(null? sexprs) (reverse defines)]
        [(and (pair? (car sexprs))
              (eq? (caar sexprs) 'define)
              (pair? (cdar sexprs)))
         (let ([name-part (cadar sexprs)])
              (cond
               ;; (define name ...)
               [(symbol? name-part)
                (loop (cdr sexprs) (cons name-part defines))]
               ;; (define (name args...) ...)
               [(and (pair? name-part) (symbol? (car name-part)))
                (loop (cdr sexprs) (cons (car name-part) defines))]
               [else
                (loop (cdr sexprs) defines)]))]
        ;; (define-record-type name ...)
        [(and (pair? (car sexprs))
              (eq? (caar sexprs) 'define-record-type)
              (pair? (cdar sexprs))
              (symbol? (cadar sexprs)))
         (loop (cdr sexprs) (cons (cadar sexprs) defines))]
        [else
         (loop (cdr sexprs) defines)])))

;;; path-stem : String -> String
;;; Remove extension from filename.
(define (arch-path-stem path)
  (let* ([base (arch-path-basename path)]
         [len (string-length base)])
        (let loop ([i (- len 1)])
             (cond
              [(< i 0) base]
              [(char=? (string-ref base i) #\.)
               (substring base 0 i)]
              [else (loop (- i 1))]))))

;;; analyze-module : String -> Module | #f
;;; Analyze a single .ss file and return module info.
;;; Returns #f if file cannot be read.
(define (analyze-module path)
  (let ([sexprs (read-file-sexprs path)])
       (if (not sexprs)
           #f
           (let* ([loads (extract-loads sexprs)]
                  [defines (extract-defines sexprs)]
                  [name (arch-path-stem (arch-path-basename path))])
                 `((path . ,path)
                   (name . ,name)
                   (loads . ,loads)
                   (defines . ,defines))))))

;;; ====
;;; Directory Analysis
;;; ====

;;; list-ss-files : String -> (List String)
;;; List all .ss files in a directory (non-recursive).
(define (list-ss-files dir)
  (if (file-exists? dir)
      (filter (lambda (f) (arch-string-suffix? ".ss" f))
              (map (lambda (f) (string-append dir "/" f))
                   (directory-list dir)))
      '()))

;;; analyze-directory : String -> (List Module)
;;; Analyze all .ss files in a directory.
(define (analyze-directory dir)
  (let ([files (list-ss-files dir)])
       (filter (lambda (m) m)  ; Remove #f entries
               (map analyze-module files))))

;;; ====
;;; Dependency Graph
;;; ====

;;; A Graph is represented as an alist:
;;;   ((nodes . <list-of-strings>)     ; module names
;;;    (edges . <list-of-pairs>))      ; (from . to) pairs
;;;
;;; Edges represent "from depends on to" relationships.

;;; get-module-name : String x (List Module) -> String | #f
;;; Find module name by matching load path to module paths.
(define (get-module-name load-path modules)
  (let* ([load-stem (arch-path-stem (arch-path-basename load-path))])
        ;; Try to find a module with matching stem
        (let loop ([mods modules])
             (cond
              [(null? mods) #f]
              [(string=? (cdr (assq 'name (car mods))) load-stem)
               load-stem]
              [else (loop (cdr mods))]))))

;;; build-dependency-graph : (List Module) -> Graph
;;; Build a dependency graph from analyzed modules.
(define (build-dependency-graph modules)
  (let* ([nodes (map (lambda (m) (cdr (assq 'name m))) modules)]
         [edges (fold-left
                 (lambda (acc mod)
                         (let* ([mod-name (cdr (assq 'name mod))]
                                [loads (cdr (assq 'loads mod))])
                               (append acc
                                       (filter-map
                                        (lambda (load-path)
                                                (let ([target (get-module-name load-path modules)])
                                                     (if target
                                                         (cons mod-name target)
                                                         #f)))
                                        loads))))
                 '()
                 modules)])
        `((nodes . ,nodes)
          (edges . ,edges))))

;;; filter-map : (a -> b | #f) x (List a) -> (List b)
;;; Map and filter out #f results.
(define (filter-map f lst)
  (let loop ([lst lst] [acc '()])
       (cond
        [(null? lst) (reverse acc)]
        [else
         (let ([result (f (car lst))])
              (if result
                  (loop (cdr lst) (cons result acc))
                  (loop (cdr lst) acc)))])))

;;; ====
;;; Cycle Detection
;;; ====

;;; find-cycles : Graph -> (List (List String))
;;; Find all cycles in the dependency graph using DFS.
;;; Returns a list of cycles, where each cycle is a list of node names.
(define (find-cycles graph)
  (let* ([nodes (cdr (assq 'nodes graph))]
         [edges (cdr (assq 'edges graph))]
         [adj (build-adjacency-list nodes edges)])
        (find-all-cycles nodes adj)))

;;; build-adjacency-list : (List String) x (List Pair) -> Alist
;;; Build adjacency list representation from edges.
(define (build-adjacency-list nodes edges)
  (map (lambda (node)
               (cons node
                     (map cdr (filter (lambda (e) (string=? (car e) node)) edges))))
       nodes))

;;; get-neighbors : String x Alist -> (List String)
;;; Get neighbors of a node from adjacency list.
(define (get-neighbors node adj)
  (let ([entry (assoc node adj)])
       (if entry (cdr entry) '())))

;;; find-all-cycles : (List String) x Alist -> (List (List String))
;;; Find all cycles in the graph using DFS.
;;; Uses a simple list-based approach for portability.
(define (find-all-cycles nodes adj)
  (let ([all-cycles '()])
       ;; For each node, try to find cycles starting from it
       (for-each
        (lambda (start)
                (let ([found (find-cycles-from start adj '() '())])
                     (set! all-cycles (append all-cycles found))))
        nodes)
       (remove-duplicate-cycles all-cycles)))

;;; find-cycles-from : String x Alist x (List String) x (List String) -> (List (List String))
;;; Find cycles starting from a node.
;;; path: current DFS path
;;; visited: nodes already fully explored
(define (find-cycles-from node adj path visited)
  (cond
   ;; If node is in current path, we found a cycle
   [(member node path)
    (list (extract-cycle node path))]
   ;; If already visited, no cycles from here
   [(member node visited)
    '()]
   ;; Otherwise, explore neighbors
   [else
    (let ([new-path (cons node path)])
         (fold-left
          (lambda (cycles neighbor)
                  (append cycles (find-cycles-from neighbor adj new-path visited)))
          '()
          (get-neighbors node adj)))]))

;;; extract-cycle : String x (List String) -> (List String)
;;; Extract the cycle portion from a path.
(define (extract-cycle target path)
  (let loop ([p path] [cycle (list target)])
       (cond
        [(null? p) cycle]
        [(string=? (car p) target) cycle]
        [else (loop (cdr p) (cons (car p) cycle))])))

;;; remove-duplicate-cycles : (List (List String)) -> (List (List String))
;;; Remove duplicate cycles (same nodes, different starting points).
(define (remove-duplicate-cycles cycles)
  (let loop ([remaining cycles] [seen '()] [result '()])
       (cond
        [(null? remaining) (reverse result)]
        [else
         (let* ([cycle (car remaining)]
                [sorted (list-sort string<? cycle)]
                [key (fold-left string-append "" sorted)])
               (if (member key seen)
                   (loop (cdr remaining) seen result)
                   (loop (cdr remaining) (cons key seen) (cons cycle result))))])))

;;; ====
;;; Critical Module Detection
;;; ====

;;; critical-modules : Graph -> (List (String . Nat))
;;; Find modules sorted by number of dependents (in-degree).
;;; Returns list of (module-name . dependent-count) pairs.
(define (critical-modules graph)
  (let* ([nodes (cdr (assq 'nodes graph))]
         [edges (cdr (assq 'edges graph))]
         [in-degrees (map (lambda (node)
                                  (cons node
                                        (length (filter (lambda (e)
                                                                (string=? (cdr e) node))
                                                        edges))))
                          nodes)])
        ;; Sort by in-degree descending
        (list-sort (lambda (a b) (> (cdr a) (cdr b))) in-degrees)))

;;; ====
;;; ASCII Visualization
;;; ====

;;; render-graph-ascii : Graph -> String
;;; Render dependency graph as ASCII art.
(define (render-graph-ascii graph)
  (let* ([nodes (cdr (assq 'nodes graph))]
         [edges (cdr (assq 'edges graph))]
         [adj (build-adjacency-list nodes edges)])
        (string-append
         (render-header "DEPENDENCY GRAPH")
         "\n"
         (render-nodes-with-deps nodes adj)
         "\n"
         (render-legend))))

;;; render-header : String -> String
(define (render-header title)
  (let* ([line (make-string 60 #\-)])
        (string-append line "\n  " title "\n" line "\n")))

;;; render-nodes-with-deps : (List String) x Alist -> String
;;; Render each node with its dependencies.
(define (render-nodes-with-deps nodes adj)
  (fold-left
   (lambda (acc node)
           (let ([deps (get-neighbors node adj)])
                (string-append
                 acc
                 (if (null? deps)
                     (string-append "  [" node "] (no dependencies)\n")
                     (string-append
                      "  [" node "]\n"
                      (fold-left
                       (lambda (a d)
                               (string-append a "    --> " d "\n"))
                       ""
                       deps))))))
   ""
   nodes))

;;; render-legend : -> String
(define (render-legend)
  (string-append
   "----\n"
   "  Legend: [module] --> dependency\n"
   "----\n"))

;;; ====
;;; Summary Report
;;; ====

;;; archextract-report : String -> String
;;; Generate a full architecture report for a directory.
(define (archextract-report dir)
  (let* ([modules (analyze-directory dir)]
         [graph (build-dependency-graph modules)]
         [cycles (find-cycles graph)]
         [critical (critical-modules graph)])
        (string-append
         (render-header (string-append "ARCHITECTURE REPORT: " dir))
         "\n"
         "MODULES ANALYZED: " (number->string (length modules)) "\n\n"
         
         ;; Module list
         "MODULES:\n"
         (fold-left
          (lambda (acc m)
                  (string-append acc "  - " (cdr (assq 'name m))
                                 " (" (number->string (length (cdr (assq 'defines m)))) " definitions)\n"))
          ""
          modules)
         "\n"
         
         ;; Dependency graph
         (render-graph-ascii graph)
         "\n"
         
         ;; Cycles
         (render-header "CYCLES")
         (if (null? cycles)
             "  No cycles detected.\n"
             (fold-left
              (lambda (acc cycle)
                      (string-append acc "  CYCLE: "
                                     (fold-left (lambda (a n) (string-append a " -> " n))
                                                (car cycle)
                                                (cdr cycle))
                                     "\n"))
              ""
              cycles))
         "\n"
         
         ;; Critical modules
         (render-header "CRITICAL MODULES (by dependents)")
         (fold-left
          (lambda (acc pair)
                  (if (> (cdr pair) 0)
                      (string-append acc "  " (car pair) ": "
                                     (number->string (cdr pair)) " dependent(s)\n")
                      acc))
          ""
          (take-up-to 10 critical))
         "\n")))

;;; take-up-to : Nat x (List a) -> (List a)
;;; Take up to n elements from list.
(define (take-up-to n lst)
  (if (or (= n 0) (null? lst))
      '()
      (cons (car lst) (take-up-to (- n 1) (cdr lst)))))

;;; NOTE: string-contains? provided by core/prelude.ss

;;; ====
;;; Quick Analysis Functions
;;; ====

;;; archextract : String -> void
;;; Analyze a directory and print the report.
(define (archextract dir)
  (display (archextract-report dir)))

;;; show-module : String -> void
;;; Show analysis of a single module.
(define (show-module path)
  (let ([mod (analyze-module path)])
       (if mod
           (begin
            (display "Module: ") (display (cdr (assq 'name mod))) (newline)
            (display "Path: ") (display (cdr (assq 'path mod))) (newline)
            (display "Loads:\n")
            (for-each (lambda (l) (display "  ") (display l) (newline))
                      (cdr (assq 'loads mod)))
            (display "Defines: ")
            (display (length (cdr (assq 'defines mod))))
            (display " symbols\n"))
           (display "Could not analyze module.\n"))))
