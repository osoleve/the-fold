(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
(require 'prelude)
(require 'sort)
(require 'meta/kg)

;;; @module meta/dag
;;; @requires prelude sort meta/kg
(doc 'module 'dag)
(doc 'description "Tools for navigating the skill dependency DAG. Find dependencies, dependents, paths, and structural queries.")
(doc 'layer 'lattice)
(doc 'purity 'partial)

(doc 'section 'direct-dependencies)

(doc lattice-deps 'type (-> Symbol (List Symbol)))
(doc lattice-deps 'description "Get direct dependencies of a skill")
(define (lattice-deps skill-name)
  (kg-deps skill-name))

(doc lattice-uses 'type (-> Symbol (List Symbol)))
(doc lattice-uses 'description "Get skills that directly depend on this skill")
(define (lattice-uses skill-name)
  (kg-uses skill-name))

(doc 'section 'transitive-closure)

(doc lattice-deps-transitive 'type (-> Symbol (List Symbol)))
(doc lattice-deps-transitive 'description "Get all dependencies (transitive closure)")
(define (lattice-deps-transitive skill-name)
  (let loop ([to-visit (kg-deps skill-name)]
             [visited '()])
       (if (null? to-visit)
           (reverse visited)
           (let ([current (car to-visit)]
                 [rest (cdr to-visit)])
                (if (memq current visited)
                    (loop rest visited)
                    (loop (append (kg-deps current) rest)
                          (cons current visited)))))))

(doc lattice-uses-transitive 'type (-> Symbol (List Symbol)))
(doc lattice-uses-transitive 'description "Get all dependents (reverse transitive closure)")
(define (lattice-uses-transitive skill-name)
  (let loop ([to-visit (kg-uses skill-name)]
             [visited '()])
       (if (null? to-visit)
           (reverse visited)
           (let ([current (car to-visit)]
                 [rest (cdr to-visit)])
                (if (memq current visited)
                    (loop rest visited)
                    (loop (append (kg-uses current) rest)
                          (cons current visited)))))))

(doc 'section 'path-finding)

(doc lattice-path 'type (-> Symbol Symbol (Maybe (List Symbol))))
(doc lattice-path 'description "Find a dependency path from one skill to another. Returns path including both endpoints, or #f if no path exists")
(define (lattice-path from to)
  (if (eq? from to)
      (list from)
      (let loop ([queue (list (list from))]
                 [visited '()])
           (if (null? queue)
               #f  ; No path found
               (let* ([path (car queue)]
                      [current (car path)]
                      [rest (cdr queue)])
                     (if (memq current visited)
                         (loop rest visited)
                         (let ([deps (kg-deps current)])
                              (if (memq to deps)
                                  (reverse (cons to path))  ; Found!
                                  (loop (append rest
                                                (map (lambda (d) (cons d path))
                                                     (filter (lambda (d) (not (memq d visited)))
                                                             deps)))
                                        (cons current visited))))))))))

(doc lattice-distance 'type (-> Symbol Symbol (Maybe Int)))
(doc lattice-distance 'description "Get the shortest dependency distance between two skills")
(define (lattice-distance from to)
  (let ([path (lattice-path from to)])
       (if path
           (- (length path) 1)
           #f)))

(doc 'section 'structural-queries)

(doc lattice-roots 'type (-> (List Symbol)))
(doc lattice-roots 'description "Get dependency-free skills (no deps in the DAG)")
(define (lattice-roots)
  (filter
   (lambda (skill-name)
           (null? (kg-deps skill-name)))
   (kg-skills)))

(doc lattice-tier-0 'type (-> (List Symbol)))
(doc lattice-tier-0 'description "Get tier 0 foundational skills (no dependencies)")
(define (lattice-tier-0)
  (filter
   (lambda (skill-name)
           (= 0 (lattice-depth skill-name)))
   (kg-skills)))

(doc lattice-leaves 'type (-> (List Symbol)))
(doc lattice-leaves 'description "Get skills with no dependents")
(define (lattice-leaves)
  (filter
   (lambda (skill-name)
           (null? (kg-uses skill-name)))
   (kg-skills)))

;;; lattice-orphans : -> (List Symbol)
;;; Get exports that are not used anywhere
;;; (This is a placeholder - would need usage tracking)
(define (lattice-orphans)
  '())  ; TODO: Implement when usage tracking is added

;;; lattice-cycles : -> (List (List Symbol))
;;; Detect dependency cycles (should be empty in a valid DAG)
(define (lattice-cycles)
  (let ([cycles '()])
       (for-each
        (lambda (skill-name)
                (let ([trans-deps (lattice-deps-transitive skill-name)])
                     (when (memq skill-name trans-deps)
                           (set! cycles (cons skill-name cycles)))))
        (kg-skills))
       cycles))

(doc 'section 'cycle-detection)

(doc lattice-would-cycle? 'type (-> Symbol Symbol Boolean))
(doc lattice-would-cycle? 'description "Check if adding a dependency from->to would create a cycle. Returns #t if the edge would create a cycle, #f if safe. Algorithm: A cycle would form if 'to' can already reach 'from'.")
(define (lattice-would-cycle? from to)
  (or (eq? from to)  ; Self-loop is a cycle
      (and (memq from (lattice-deps-transitive to)) #t)))

(doc lattice-find-cycle-path 'type (-> Symbol Symbol (Maybe (List Symbol))))
(doc lattice-find-cycle-path 'description "If adding from->to would create a cycle, return the cycle path. Returns #f if no cycle would be created.")
(define (lattice-find-cycle-path from to)
  (if (eq? from to)
      (list from from)  ; Self-loop
      (let ([path (lattice-path to from)])
           (if path
               (append path (list to))  ; Complete the cycle
               #f))))

(doc lattice-check-deps 'type (-> Symbol Result))
(doc lattice-check-deps 'description "Validate that a skill's dependencies don't create cycles. Returns (ok #t) if valid, (err (cycle-from to path)) if cycle found.")
(define (lattice-check-deps skill-name)
  (let ([deps (kg-deps skill-name)])
       (let loop ([deps deps])
            (if (null? deps)
                '(ok #t)
                (let* ([dep (car deps)]
                       [cycle-path (lattice-find-cycle-path skill-name dep)])
                      (if cycle-path
                          `(err (cycle ,skill-name ,dep ,cycle-path))
                          (loop (cdr deps))))))))

(doc lattice-validate-all 'type (-> (List Result)))
(doc lattice-validate-all 'description "Validate all skills, returning list of cycle errors.")
(define (lattice-validate-all)
  (filter-map
   (lambda (skill-name)
           (let ([result (lattice-check-deps skill-name)])
                (if (and (pair? result) (eq? (car result) 'err))
                    result
                    #f)))
   (kg-skills)))

(doc lc 'type (-> Symbol Void))
(doc lc 'description "Quick cycle check for a skill (REPL convenience)")
(define (lc skill-name)
  (let ([result (lattice-check-deps skill-name)])
       (if (and (pair? result) (eq? (car result) 'ok))
           (printf "~a: No cycles detected\n" skill-name)
           (let* ([err-data (cadr result)]
                  [from (cadr err-data)]
                  [to (caddr err-data)]
                  [path (cadddr err-data)])
                 (printf "~a: CYCLE DETECTED!\n" skill-name)
                 (printf "  Adding ~a -> ~a creates cycle:\n" from to)
                 (printf "  ~a\n" (format-cycle-path path)))))
  (void))

;;; format-cycle-path : (List Symbol) -> String
;;; Format a cycle path for display
(define (format-cycle-path path)
  (if (null? path)
      ""
      (let loop ([rest (cdr path)]
                 [acc (symbol->string (car path))])
           (if (null? rest)
               acc
               (loop (cdr rest)
                     (string-append acc " → " (symbol->string (car rest))))))))

(doc 'section 'tier-analysis)

(doc *tier-cache* 'description "Memoization cache for lattice-depth. Alist of (skill . depth).")
(define *tier-cache* '())

(doc tier-cache-clear! 'type (-> Void))
(doc tier-cache-clear! 'description "Clear the tier depth cache. Call when KG is reloaded.")
(define (tier-cache-clear!) (set! *tier-cache* '()))

(doc lattice-depth 'type (-> Symbol Int))
(doc lattice-depth 'description "Get the maximum dependency depth of a skill (memoized). Depth 0 = no dependencies, depth N = longest path to a root. Handles cycles gracefully by treating back-edges as depth 0.")
(define (lattice-depth skill-name)
  (let ([cached (assq skill-name *tier-cache*)])
    (cond
      [(and cached (eq? (cdr cached) 'in-progress))
       ;; Cycle detected — break it by treating back-edge as depth 0
       0]
      [cached (cdr cached)]
      [else
       ;; Mark in-progress to detect cycles
       (set! *tier-cache* (cons (cons skill-name 'in-progress) *tier-cache*))
       (let* ([deps (kg-deps skill-name)]
              [d (if (null? deps) 0 (+ 1 (apply max (map lattice-depth deps))))])
         ;; Replace the in-progress entry with the final value
         (set-cdr! (assq skill-name *tier-cache*) d)
         d)])))

(doc lattice-tiers 'type (-> (List (Pair Int (List Symbol)))))
(doc lattice-tiers 'description "Group skills by their computed DAG depth")
(define (lattice-tiers)
  (let ([tier-map '()])
       (for-each
        (lambda (skill-name)
                (let* ([tier (lattice-depth skill-name)]
                       [entry (assq tier tier-map)])
                      (if entry
                          (set-cdr! entry (cons skill-name (cdr entry)))
                          (set! tier-map (cons (cons tier (list skill-name)) tier-map)))))
        (kg-skills))
       (sort-by (lambda (a b) (< (car a) (car b))) tier-map)))

;;; lattice-max-depth : -> Int
;;; Get the maximum depth of the entire DAG
(define (lattice-max-depth)
  (if (null? (kg-skills))
      0
      (apply max (map lattice-depth (kg-skills)))))

(doc 'section 'impact-analysis)

(doc lattice-impact 'type (-> Symbol Int))
(doc lattice-impact 'description "Score based on how many skills depend on this (transitive)")
(define (lattice-impact skill-name)
  (length (lattice-uses-transitive skill-name)))

;;; lattice-hubs : [Int] -> (List (Symbol . Int))
;;; Get most-depended-on skills (hubs)
(define (lattice-hubs . options)
  (let* ([k (if (pair? options) (car options) 10)]
         [impacts (map (lambda (s) (cons s (lattice-impact s))) (kg-skills))]
         [sorted (sort-by (lambda (a b) (> (cdr a) (cdr b))) impacts)])
        (take k sorted)))

;;; lattice-breaking-change? : Symbol Symbol -> Bool
;;; Would removing an export from a skill break anything?
;;; (Placeholder - would need export-level dependency tracking)
(define (lattice-breaking-change? skill-name export-name)
  ;; For now, assume any exported symbol might be used
  (> (lattice-impact skill-name) 0))

(doc 'section 'visualization)

(doc lattice-tree 'type (-> Symbol Void))
(doc lattice-tree 'description "Print dependency tree")
(define (lattice-tree skill-name)
  (define (print-tree name indent)
    (printf "~a~a\n" (make-string (* indent 2) #\space) name)
    (for-each (lambda (dep) (print-tree dep (+ indent 1)))
              (kg-deps name)))
  (print-tree skill-name 0))

;;; lattice-reverse-tree : Symbol -> void
;;; Print dependent tree (what uses this)
(define (lattice-reverse-tree skill-name)
  (define (print-tree name indent)
    (printf "~a~a\n" (make-string (* indent 2) #\space) name)
    (for-each (lambda (dep) (print-tree dep (+ indent 1)))
              (kg-uses name)))
  (print-tree skill-name 0))

;;; lattice-graph : -> void
;;; Print entire DAG structure
(define (lattice-graph)
  (printf "Lattice Skill DAG\n")
  (printf "====\n\n")
  (let ([tiers (lattice-tiers)])
       (for-each
        (lambda (tier-entry)
                (let ([tier (car tier-entry)]
                      [skills (cdr tier-entry)])
                     (printf "Tier ~a:\n" tier)
                     (for-each
                      (lambda (skill)
                              (let ([deps (kg-deps skill)])
                                   (if (null? deps)
                                       (printf "  ~a\n" skill)
                                       (printf "  ~a -> ~a\n" skill deps))))
                      skills)
                     (printf "\n")))
        tiers)))


(doc 'section 'module-level-dag)

;;; ====
;;; Module-Level DAG
;;; ====
;;; Computes module depth from @requires annotations.
;;; Skill tier is derived as max depth of constituent modules.

(doc *module-dag* 'type '(Alist Symbol (List Symbol)))
(doc *module-dag* 'description "Module dependency graph from @requires annotations.
Populated by module-dag-init! from scan-all-module-metadata.")
(define *module-dag* '())

(doc *module-depth-cache* 'type '(Alist Symbol (U Int Symbol)))
(doc *module-depth-cache* 'description "Memoization cache for module-depth.")
(define *module-depth-cache* '())

(doc module-dag-init! 'type (-> Void))
(doc module-dag-init! 'description "Initialize module-level DAG from scan-all-module-metadata.
Must be called after module-meta.ss is loaded.")
(doc module-dag-init! 'export #t)
(define (module-dag-init!)
  (when (top-level-bound? 'scan-all-module-metadata)
    (let ([all-meta (scan-all-module-metadata)])
      (set! *module-dag*
            (map (lambda (meta)
                   (cons (cdr (assq 'name meta))
                         (cdr (assq 'requires meta))))
                 all-meta))
      (set! *module-depth-cache* '())
      (printf "Module DAG initialized: ~a modules\n" (length *module-dag*)))))

(doc module-deps 'type (-> Symbol (List Symbol)))
(doc module-deps 'description "Get direct module dependencies from @requires.")
(doc module-deps 'export #t)
(define (module-deps mod-name)
  (let ([entry (assq mod-name *module-dag*)])
    (if entry (cdr entry) '())))

(doc module-uses 'type (-> Symbol (List Symbol)))
(doc module-uses 'description "Get modules that directly depend on this module.")
(doc module-uses 'export #t)
(define (module-uses mod-name)
  (filter-map
   (lambda (entry)
     (and (memq mod-name (cdr entry))
          (car entry)))
   *module-dag*))

(doc module-depth 'type (-> Symbol Int))
(doc module-depth 'description "Compute maximum dependency depth of a module (memoized).
Depth 0 = no dependencies, depth N = 1 + max(depth(deps)).
Handles cycles by treating back-edges as depth 0.")
(doc module-depth 'export #t)
(define (module-depth mod-name)
  (let ([cached (assq mod-name *module-depth-cache*)])
    (cond
      [(and cached (eq? (cdr cached) 'in-progress)) 0]
      [cached (cdr cached)]
      [else
       (set! *module-depth-cache*
             (cons (cons mod-name 'in-progress) *module-depth-cache*))
       (let* ([deps (module-deps mod-name)]
              [d (if (null? deps)
                     0
                     (+ 1 (apply max (map module-depth deps))))])
         (set-cdr! (assq mod-name *module-depth-cache*) d)
         d)])))

(doc module-tiers 'type (-> (List (Pair Int (List Symbol)))))
(doc module-tiers 'description "Group modules by their computed DAG depth.")
(doc module-tiers 'export #t)
(define (module-tiers)
  (let ([tier-map '()])
    (for-each
     (lambda (entry)
       (let* ([mod-name (car entry)]
              [tier (module-depth mod-name)]
              [existing (assq tier tier-map)])
         (if existing
             (set-cdr! existing (cons mod-name (cdr existing)))
             (set! tier-map (cons (cons tier (list mod-name)) tier-map)))))
     *module-dag*)
    (sort-by (lambda (a b) (< (car a) (car b))) tier-map)))

(doc module-max-depth 'type (-> Int))
(doc module-max-depth 'description "Maximum depth of the module DAG.")
(doc module-max-depth 'export #t)
(define (module-max-depth)
  (if (null? *module-dag*)
      0
      (apply max (map (lambda (e) (module-depth (car e))) *module-dag*))))

(doc derive-skill-tier 'type (-> Symbol Int))
(doc derive-skill-tier 'description "Derive a skill's tier as max depth of its constituent modules.
Uses *module-paths* to find which modules belong to a skill directory.")
(doc derive-skill-tier 'export #t)
(define (derive-skill-tier skill-name)
  (let* ([skill-prefix (string-append "lattice/" (symbol->string skill-name) "/")]
         [all-paths (hashtable->alist *module-paths*)]
         [skill-modules
          (filter-map
           (lambda (entry)
             (let ([mod-name (car entry)]
                   [path (cdr entry)])
               (and (>= (string-length path) (string-length skill-prefix))
                    (string=? skill-prefix
                              (substring path 0 (string-length skill-prefix)))
                    mod-name)))
           all-paths)])
    (if (null? skill-modules)
        0
        (apply max (map module-depth skill-modules)))))

(doc 'section 'convenience-functions)

(doc ld 'type (-> Symbol Void))
(doc ld 'description "Quick show dependencies")
(define (ld skill-name)
  (let ([deps (kg-deps skill-name)])
       (if (null? deps)
           (printf "~a has no dependencies (tier 0)\n" skill-name)
           (printf "~a depends on: ~a\n" skill-name deps))))

(doc lu 'type (-> Symbol Void))
(doc lu 'description "Quick show dependents")
(define (lu skill-name)
  (let ([uses (kg-uses skill-name)])
       (if (null? uses)
           (printf "Nothing depends on ~a (leaf)\n" skill-name)
           (printf "~a is used by: ~a\n" skill-name uses))))

(doc 'section 'repl-interface)

(unless (top-level-bound? '*dag-banner-shown*)
  (meta-printf "dag.ss loaded.\n")
  (meta-printf "  (lattice-deps 'skill)            - Direct dependencies\n")
  (meta-printf "  (lattice-deps-transitive 'skill) - Full dep closure\n")
  (meta-printf "  (lattice-uses 'skill)            - Direct dependents\n")
  (meta-printf "  (lattice-uses-transitive 'skill) - Full dep closure\n")
  (meta-printf "  (lattice-path 'from 'to)         - Find path\n")
  (meta-printf "  (lattice-roots)                  - Dependency-free skills\n")
  (meta-printf "  (lattice-tier-0)                 - Tier 0 foundational skills\n")
  (meta-printf "  (lattice-leaves)                 - No dependents\n")
  (meta-printf "  (lattice-hubs)                   - Most used skills\n")
  (meta-printf "  (lattice-graph)                  - Print full DAG\n")
  (meta-printf "  (lattice-would-cycle? 'a 'b)     - Would a->b create cycle?\n")
  (meta-printf "  (lattice-check-deps 'skill)      - Validate skill's deps\n")
  (meta-printf "  (lattice-validate-all)           - Validate all skills\n")
  (meta-printf "  (ld 'skill), (lu 'skill)         - Quick lookup\n")
  (meta-printf "  (lc 'skill)                      - Quick cycle check\n"))
(set-top-level-value! '*dag-banner-shown* #t)
