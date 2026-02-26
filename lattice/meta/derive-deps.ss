(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
(require 'prelude)

;;; @module meta/derive-deps
;;; @requires prelude manifest meta/skill-map

(unless (top-level-bound? 'parse-manifest)
  (load "lattice/meta/manifest.ss"))
(unless (top-level-bound? 'build-skill-map)
  (load "lattice/meta/skill-map.ss"))

(doc 'module 'meta/derive-deps)
(doc 'description "Derive skill-level dependencies from the require graph.
For each skill, walks its modules' @requires annotations, maps required
modules to skills via the skill map, and produces the honest dependency
set. No exclusions — the derived deps reflect reality.")
(doc 'layer 'lattice)
(doc 'purity 'total)

;;; ====
;;; Header Parsing (lightweight, no module.ss dependency graph needed)
;;; ====

(doc 'section 'header-parsing)

;;; parse-requires-from-file-lines : (List String) -> (List Symbol)
;;; Extract required module names from @requires annotations in file header.
;;; Handles multiple @requires lines. Space-separated, no commas.
(define (parse-requires-from-file-lines lines)
  (append-map
   (lambda (line)
     (let ([trimmed (string-trim-dd line)])
       (if (string-prefix-dd? ";;; @requires " trimmed)
           (let ([rest (substring trimmed 14 (string-length trimmed))])
             (filter-map
              (lambda (tok)
                (and (> (string-length tok) 0)
                     (string->symbol tok)))
              (string-split-dd rest #\space)))
           '())))
   lines))

;;; read-header-lines-dd : String Nat -> (List String)
;;; Read first n lines of a file.
(define (read-header-lines-dd filepath n)
  (guard (e [else '()])
    (call-with-input-file filepath
      (lambda (port)
        (let loop ([i 0] [acc '()])
          (if (>= i n)
              (reverse acc)
              (let ([line (get-line port)])
                (if (eof-object? line)
                    (reverse acc)
                    (loop (+ i 1) (cons line acc))))))))))

;;; file-requires : String -> (List Symbol)
;;; Get required module names from a source file's header.
(doc file-requires 'type '(-> String (List Symbol)))
(doc file-requires 'export #t)
(define (file-requires filepath)
  (parse-requires-from-file-lines (read-header-lines-dd filepath 60)))

;;; ====
;;; String Helpers
;;; ====

(define (string-trim-dd s)
  (let* ([len (string-length s)]
         [start (let loop ([i 0])
                  (if (and (< i len) (char-whitespace? (string-ref s i)))
                      (loop (+ i 1))
                      i))]
         [end (let loop ([i len])
                (if (and (> i start) (char-whitespace? (string-ref s (- i 1))))
                    (loop (- i 1))
                    i))])
    (substring s start end)))

(define (string-prefix-dd? prefix str)
  (let ([plen (string-length prefix)]
        [slen (string-length str)])
    (and (>= slen plen)
         (string=? prefix (substring str 0 plen)))))

(define (string-split-dd str delim)
  (let loop ([chars (string->list str)]
             [current '()]
             [result '()])
    (cond
      [(null? chars)
       (reverse (if (null? current)
                    result
                    (cons (list->string (reverse current)) result)))]
      [(char=? (car chars) delim)
       (loop (cdr chars) '()
             (if (null? current) result
                 (cons (list->string (reverse current)) result)))]
      [else
       (loop (cdr chars) (cons (car chars) current) result)])))

;;; ====
;;; Deps Derivation
;;; ====

(doc 'section 'derivation)

;;; derive-skill-deps : ManifestData (Alist Symbol Symbol) -> (List Symbol)
;;; Derive the dependency set for a single skill from its modules' requires.
;;;
;;; Algorithm:
;;;   1. Get all modules for this skill from the manifest
;;;   2. For each module, parse @requires from the source file header
;;;   3. Map each required module to its owning skill via the skill map
;;;   4. Collect unique skill names, subtract self
;;;   5. Return sorted
(doc derive-skill-deps 'type '(-> ManifestData (Alist Symbol Symbol) (List Symbol)))
(doc derive-skill-deps 'export #t)
(define (derive-skill-deps manifest smap)
  (let* ([skill-name (cdr (assq 'name manifest))]
         [skill-path (cdr (assq 'path manifest))]
         [module-index (manifest->module-index manifest)]
         [all-required-modules
          (append-map
           (lambda (mod-entry)
             ;; mod-entry = (module-name . file-path)
             (let ([path (cdr mod-entry)])
               (if (and (string? path) (> (string-length path) 0))
                   (file-requires path)
                   '())))
           module-index)]
         ;; Map modules to skills
         [dep-skills
          (filter-map
           (lambda (mod)
             (skill-map-lookup smap mod))
           all-required-modules)]
         ;; Remove self, deduplicate, sort
         [unique-deps
          (sort-symbols
           (deduplicate-dd
            (filter (lambda (s) (not (eq? s skill-name)))
                    dep-skills)))])
    unique-deps))

;;; derive-all-deps : (List ManifestData) -> (Alist Symbol (List Symbol))
;;; Derive deps for all skills. Returns: ((skill-name . (dep1 dep2 ...)) ...)
(doc derive-all-deps 'type '(-> (List ManifestData) (Alist Symbol (List Symbol))))
(doc derive-all-deps 'export #t)
(define (derive-all-deps manifests)
  (let ([smap (build-skill-map manifests)])
    (map (lambda (manifest)
           (let ([name (cdr (assq 'name manifest))])
             (cons name (derive-skill-deps manifest smap))))
         manifests)))

;;; ====
;;; Bridge-Aware Cycle Breaking
;;; ====

(doc 'section 'cycle-breaking)

;;; count-cross-requires : ManifestData Symbol (Alist Symbol Symbol) -> Nat
;;; Count how many module-level requires from skill A point into skill B.
;;; Each unique (module-in-A, required-module-in-B) pair counts as 1.
(define (count-cross-requires manifest target-skill smap)
  (let* ([module-index (manifest->module-index manifest)]
         [count 0])
    (for-each
     (lambda (mod-entry)
       (let ([path (cdr mod-entry)])
         (when (and (string? path) (> (string-length path) 0))
           (let ([reqs (file-requires path)])
             (for-each
              (lambda (req)
                (let ([owner (skill-map-lookup smap req)])
                  (when (eq? owner target-skill)
                    (set! count (+ count 1)))))
              reqs)))))
     module-index)
    count))

;;; find-cycle-dfs : Symbol (Alist Symbol (List Symbol)) -> (Maybe (List Symbol))
;;; Find a cycle reachable from start node using DFS.
;;; Returns a list of nodes forming the cycle as (A B C ... A),
;;; where each consecutive pair is an edge. Returns #f if no cycle found.
(define (find-cycle-dfs start all-deps)
  (let ([visiting '()]   ;; Currently on DFS stack (grey nodes)
        [visited '()]    ;; Fully explored (black nodes)
        [found #f])      ;; The cycle path once found
    (let dfs ([node start] [path '()])
      (cond
        [found (void)]  ;; Already found a cycle, stop
        [(memq node visited) (void)]  ;; Already fully explored
        [(memq node visiting)
         ;; Back-edge: extract cycle from path
         ;; path is (most-recent ... oldest) — DFS ancestors in stack order
         ;; Scan from head collecting nodes until we find `node` (the cycle start)
         ;; cycle accumulates in DFS-forward order
         (let loop ([p path] [cycle '()])
           (cond
             [(null? p)
              ;; Shouldn't happen for a real back-edge, but safety fallback
              (set! found #f)]
             [(eq? (car p) node)
              ;; Close the cycle: (start body... start)
              (set! found (cons node (append cycle (list node))))]
             [else (loop (cdr p) (cons (car p) cycle))]))]
        [else
         (set! visiting (cons node visiting))
         (let ([entry (assq node all-deps)])
           (when entry
             (for-each
              (lambda (dep) (dfs dep (cons node path)))
              (cdr entry))))
         (set! visiting (filter (lambda (x) (not (eq? x node))) visiting))
         (set! visited (cons node visited))]))
    found))

;;; find-any-cycle : (Alist Symbol (List Symbol)) -> (Maybe (List Symbol))
;;; Find any cycle in the dependency graph. Returns cycle nodes or #f.
(define (find-any-cycle all-deps)
  (let loop ([nodes (map car all-deps)])
    (if (null? nodes)
        #f
        (or (find-cycle-dfs (car nodes) all-deps)
            (loop (cdr nodes))))))

;;; weakest-edge-in-cycle : (List Symbol) (List ManifestData) (Alist Symbol Symbol) -> (Pair Symbol Symbol)
;;; Given a cycle (A B C ... A), find the edge with the fewest cross-requires.
;;; Returns (from . to) for the weakest edge.
(define (weakest-edge-in-cycle cycle manifests smap)
  (let loop ([nodes cycle]
             [weakest-from #f]
             [weakest-to #f]
             [weakest-count +inf.0])
    (if (or (null? nodes) (null? (cdr nodes)))
        (cons weakest-from weakest-to)
        (let* ([from (car nodes)]
               [to (cadr nodes)]
               [manifest-from (find-manifest-by-name manifests from)]
               [count (if manifest-from
                         (count-cross-requires manifest-from to smap)
                         0)])
          (if (< count weakest-count)
              (loop (cdr nodes) from to count)
              (loop (cdr nodes) weakest-from weakest-to weakest-count))))))

;;; remove-dep-edge : (Alist Symbol (List Symbol)) Symbol Symbol -> (Alist Symbol (List Symbol))
;;; Remove a single edge from->to in the dependency alist.
(define (remove-dep-edge all-deps from to)
  (map (lambda (entry)
         (if (eq? (car entry) from)
             (cons from (filter (lambda (d) (not (eq? d to))) (cdr entry)))
             entry))
       all-deps))

;;; break-dep-cycles : (Alist Symbol (List Symbol)) (List ManifestData) (Alist Symbol Symbol) -> (Values (Alist Symbol (List Symbol)) (List BridgeReport))
;;; Iteratively detect and break cycles in derived deps.
;;; Uses DFS to find cycles, then removes the weakest edge (fewest cross-requires)
;;; in each cycle. Repeats until the graph is acyclic.
;;;
;;; Bridge report entries: ((from . to) (bridge-count . N))
(doc break-dep-cycles 'type '(-> (Alist Symbol (List Symbol)) (List ManifestData) (Alist Symbol Symbol) (Values (Alist Symbol (List Symbol)) (List Any))))
(doc break-dep-cycles 'export #t)
(define (break-dep-cycles all-deps manifests smap)
  (let loop ([deps all-deps] [report '()] [iterations 0])
    (if (> iterations 50)  ;; Safety limit
        (values deps (reverse report))
        (let ([cycle (find-any-cycle deps)])
          (if (not cycle)
              (values deps (reverse report))
              (let* ([edge (weakest-edge-in-cycle cycle manifests smap)]
                     [from (car edge)]
                     [to (cdr edge)]
                     [manifest-from (find-manifest-by-name manifests from)]
                     [count (if manifest-from
                                (count-cross-requires manifest-from to smap)
                                0)]
                     [cleaned (remove-dep-edge deps from to)])
                (loop cleaned
                      (cons `((from . ,from) (to . ,to) (bridge-count . ,count))
                            report)
                      (+ iterations 1))))))))

;;; find-manifest-by-name : (List ManifestData) Symbol -> (Maybe ManifestData)
(define (find-manifest-by-name manifests name)
  (let loop ([remaining manifests])
    (if (null? remaining)
        #f
        (let ([m (car remaining)])
          (if (eq? (cdr (assq 'name m)) name)
              m
              (loop (cdr remaining)))))))

;;; find-mutual-deps : (Alist Symbol (List Symbol)) -> (List (Pair Symbol Symbol))
;;; Find all (A . B) pairs where A depends on B and B depends on A.
;;; Useful for diagnostics. Returns only one direction per pair.
(define (find-mutual-deps all-deps)
  (let ([result '()])
    (for-each
     (lambda (entry)
       (let ([skill-a (car entry)]
             [deps-a (cdr entry)])
         (for-each
          (lambda (dep)
            (let ([dep-entry (assq dep all-deps)])
              (when (and dep-entry (memq skill-a (cdr dep-entry)))
                (when (string<? (symbol->string skill-a) (symbol->string dep))
                  (set! result (cons (cons skill-a dep) result))))))
          deps-a)))
     all-deps)
    result))

;;; derive-all-deps-acyclic : (List ManifestData) -> (Values (Alist Symbol (List Symbol)) (List BridgeReport))
;;; Derive deps for all skills, then iteratively break all cycles.
;;; Returns two values: the acyclic dep alist and a bridge report.
(doc derive-all-deps-acyclic 'type '(-> (List ManifestData) (Values (Alist Symbol (List Symbol)) (List Any))))
(doc derive-all-deps-acyclic 'export #t)
(define (derive-all-deps-acyclic manifests)
  (let* ([smap (build-skill-map manifests)]
         [raw-deps (map (lambda (manifest)
                          (let ([name (cdr (assq 'name manifest))])
                            (cons name (derive-skill-deps manifest smap))))
                        manifests)])
    (break-dep-cycles raw-deps manifests smap)))

;;; ====
;;; Diff: Derived vs Declared
;;; ====

(doc 'section 'diff)

;;; deps-diff : (List Symbol) (List Symbol) -> DiffReport
;;; Compare derived deps against declared deps.
;;; Returns: ((added . (syms))     ; in derived but not declared
;;;           (removed . (syms))   ; in declared but not derived
;;;           (same . (syms)))     ; in both
(doc deps-diff 'type '(-> (List Symbol) (List Symbol) DiffReport))
(doc deps-diff 'export #t)
(define (deps-diff derived declared)
  (let ([added   (filter (lambda (s) (not (memq s declared))) derived)]
        [removed (filter (lambda (s) (not (memq s derived))) declared)]
        [same    (filter (lambda (s) (memq s derived)) declared)])
    `((added   . ,added)
      (removed . ,removed)
      (same    . ,same))))

;;; print-deps-diff : Symbol DiffReport -> Void
(define (print-deps-diff skill-name diff)
  (let ([added   (cdr (assq 'added diff))]
        [removed (cdr (assq 'removed diff))]
        [same    (cdr (assq 'same diff))])
    (printf "~a:" skill-name)
    (cond
      [(and (null? added) (null? removed))
       (printf " deps match (~a)\n" (length same))]
      [else
       (printf "\n")
       (for-each (lambda (s) (printf "  + ~a (derived, not declared)\n" s)) added)
       (for-each (lambda (s) (printf "  - ~a (declared, not derived)\n" s)) removed)
       (when (pair? same)
         (printf "  = ~a (both)\n" same))])))

;;; ====
;;; Full Report
;;; ====

(doc 'section 'report)

;;; derive-and-compare-all : (List ManifestData) -> Void
;;; Derive deps for all skills and print diff against declared deps.
(doc derive-and-compare-all 'type '(-> (List ManifestData) Void))
(doc derive-and-compare-all 'export #t)
(define (derive-and-compare-all manifests)
  (let ([smap (build-skill-map manifests)]
        [total-added 0]
        [total-removed 0]
        [total-match 0])
    (for-each
     (lambda (manifest)
       (let* ([name (cdr (assq 'name manifest))]
              [derived (derive-skill-deps manifest smap)]
              [declared (manifest-deps manifest)]
              [diff (deps-diff derived declared)]
              [added (cdr (assq 'added diff))]
              [removed (cdr (assq 'removed diff))])
         (print-deps-diff name diff)
         (set! total-added (+ total-added (length added)))
         (set! total-removed (+ total-removed (length removed)))
         (when (and (null? added) (null? removed))
           (set! total-match (+ total-match 1)))))
     manifests)
    (printf "\nSummary: ~a skills match, ~a undeclared deps, ~a phantom deps\n"
            total-match total-added total-removed)))

;;; ====
;;; Helpers
;;; ====

(define (deduplicate-dd lst)
  (let loop ([remaining lst] [seen '()] [acc '()])
    (if (null? remaining)
        (reverse acc)
        (let ([x (car remaining)])
          (if (memq x seen)
              (loop (cdr remaining) seen acc)
              (loop (cdr remaining) (cons x seen) (cons x acc)))))))

(define (sort-symbols syms)
  ;; Sort symbols alphabetically
  (map string->symbol
       (list-sort string<? (map symbol->string syms))))
