;;; user/sft/llm/meta-lattice.ss — Lattice navigation SFT task generator
;;;
;;; Generates SFT samples teaching lattice navigation and discovery:
;;;   - Search: find functions by keyword (lattice-find-data)
;;;   - Inspect: examine skill structure (lattice-info, lattice-exports-data)
;;;   - Dependency chain: navigate DAG (lattice-deps, lattice-uses)
;;;   - Concept bridge: discover cross-domain connections (kg-concept-bridge)
;;;   - Discovery workflow: search → inspect → require → use (multi-step)
;;;
;;; Seeds use real lattice data for verifiable results.
;;; Verification: structured APIs return expected data shapes.
;;;
;;; Usage:
;;;   (load "user/sft/llm/meta-lattice.ss")
;;;   (define seeds (meta-lattice/generate-seeds))

(load "user/sft/extract/emit.ss")

;;; ====
;;; Skill Archetypes
;;; ====
;;; Real lattice skills with known structure. Each archetype provides
;;; enough metadata for multiple task types without needing lattice-init!.

(define *ml-skills*
  `(;; --- Core math skills ---
    ((name . linalg)
     (description . "Vectors, matrices, decomposition, solvers")
     (tier . 1)
     (purity . pure)
     (deps . (prelude))
     (used-by . (geometry autodiff physics/classical physics/diff statistics))
     (modules . ((vec . "lattice/linalg/vec.ss")
                 (matrix . "lattice/linalg/matrix.ss")
                 (decompose . "lattice/linalg/decompose.ss")
                 (solve . "lattice/linalg/solve.ss")))
     (key-exports . (make-vec vec-add vec-dot matrix-multiply lu-decompose
                     solve-linear vec-scale vec-normalize matrix-transpose))
     (concepts . (linear-algebra vector-spaces matrix-operations)))

    ((name . algebra)
     (description . "Groups, rings, polynomial algebra, Gröbner bases")
     (tier . 1)
     (purity . pure)
     (deps . (prelude))
     (used-by . (number-theory crypto))
     (modules . ((group . "lattice/algebra/group.ss")
                 (ring . "lattice/algebra/ring.ss")
                 (polynomial . "lattice/algebra/polynomial.ss")))
     (key-exports . (make-group group-op group-identity make-ring ring-add
                     ring-mul poly-add poly-mul poly-eval grobner-basis))
     (concepts . (abstract-algebra group-theory ring-theory polynomials)))

    ((name . optics)
     (description . "Complete optics tower: Iso, Lens, Prism, Affine, Traversal, Fold, Getter, Setter")
     (tier . 2)
     (purity . pure)
     (deps . (prelude fp))
     (used-by . (autodiff physics/lenses))
     (modules . ((lens . "lattice/optics/lens.ss")
                 (prism . "lattice/optics/prism.ss")
                 (traversal . "lattice/optics/traversal.ss")
                 (iso . "lattice/optics/iso.ss")
                 (optics-compose . "lattice/optics/compose.ss")))
     (key-exports . (make-lens lens-view lens-set lens-modify make-prism
                     prism-match prism-review make-traversal traversal-to-list
                     make-iso iso-forward iso-backward optic-compose))
     (concepts . (optics lenses prisms functional-updates)))

    ((name . autodiff)
     (description . "Reverse-mode AD, computational graphs, interval gradients")
     (tier . 2)
     (purity . pure)
     (deps . (prelude linalg))
     (used-by . (optimization physics/diff))
     (modules . ((comp-graph . "lattice/autodiff/comp-graph.ss")
                 (reverse-diff . "lattice/autodiff/reverse-diff.ss")
                 (traced-optics . "lattice/autodiff/traced-optics.ss")))
     (key-exports . (make-traced traced-value gradient reverse-gradient
                     make-comp-graph comp-graph-forward comp-graph-backward
                     lift-at-optic))
     (concepts . (automatic-differentiation gradient-computation backpropagation)))

    ((name . fp)
     (description . "Monads, parsers, streams, protocols, game theory, control systems")
     (tier . 1)
     (purity . pure)
     (deps . (prelude))
     (used-by . (optics query dsl autodiff))
     (modules . ((maybe . "lattice/fp/maybe.ss")
                 (either . "lattice/fp/either.ss")
                 (parser . "lattice/fp/parser.ss")
                 (stream . "lattice/fp/stream.ss")
                 (protocol . "lattice/fp/protocol.ss")
                 (state . "lattice/fp/state.ss")))
     (key-exports . (maybe-just maybe-nothing maybe-bind either-left
                     either-right either-bind make-parser parser-run
                     parser-many parser-choice stream-cons stream-take
                     define-protocol implement-protocol! run-state))
     (concepts . (monads functors applicatives parser-combinators)))

    ((name . data)
     (description . "Data structures, graphs, collections, community detection")
     (tier . 1)
     (purity . pure)
     (deps . (prelude))
     (used-by . (fp query egraph))
     (modules . ((hamt . "lattice/data/hamt.ss")
                 (graph . "lattice/data/graph.ss")
                 (heap . "lattice/data/heap.ss")
                 (sort . "lattice/data/sort.ss")))
     (key-exports . (hamt-empty hamt-set hamt-ref hamt-remove make-graph
                     graph-add-edge graph-neighbors graph-bfs graph-dfs
                     make-heap heap-insert heap-min sort-by))
     (concepts . (data-structures hash-maps graphs heaps)))

    ((name . statistics)
     (description . "Regression, GLM, time series, hypothesis testing")
     (tier . 2)
     (purity . pure)
     (deps . (prelude linalg))
     (used-by . ())
     (modules . ((regression . "lattice/statistics/regression.ss")
                 (hypothesis . "lattice/statistics/hypothesis.ss")
                 (descriptive . "lattice/statistics/descriptive.ss")))
     (key-exports . (linear-regression fit-glm ols-residuals t-test
                     chi-squared-test mean variance std-deviation))
     (concepts . (statistics regression hypothesis-testing)))

    ((name . geometry)
     (description . "Shapes, transforms, raymarching, SDFs, mesh topology")
     (tier . 2)
     (purity . pure)
     (deps . (prelude linalg))
     (used-by . (physics/classical physics/diff))
     (modules . ((shapes . "lattice/geometry/shapes.ss")
                 (transform . "lattice/geometry/transform.ss")
                 (sdf . "lattice/geometry/sdf.ss")))
     (key-exports . (make-circle make-rect make-sphere sdf-sphere sdf-box
                     sdf-union sdf-intersect sdf-smooth-union translate-2d
                     rotate-2d scale-2d))
     (concepts . (computational-geometry signed-distance-fields raymarching)))))

;;; ====
;;; Concept Bridge Pairs
;;; ====
;;; Known cross-domain connections for concept bridge tasks.

(define *ml-bridges*
  `(((skill-a . optics)
     (skill-b . autodiff)
     (shared . (functional-updates gradient-computation))
     (explanation . "Optics specify *what* to differentiate; autodiff computes *how*.
The traced-optics module bridges them: lift-at-optic traces only the
optic's focus, enabling efficient gradient computation through composed lenses."))

    ((skill-a . linalg)
     (skill-b . statistics)
     (shared . (matrix-operations linear-algebra))
     (explanation . "Statistics builds on linear algebra: OLS regression is
matrix inversion (X'X)^-1 X'y, PCA is eigendecomposition, and GLMs use
iteratively reweighted least squares — all matrix operations."))

    ((skill-a . fp)
     (skill-b . optics)
     (shared . (functional-updates monads functors))
     (explanation . "Optics are profunctor-encoded, making them inherently
functorial. The lens laws mirror monad laws: get-put ≈ left identity,
put-get ≈ right identity. Traversals are applicative functors over containers."))

    ((skill-a . data)
     (skill-b . autodiff)
     (shared . (data-structures gradient-computation))
     (explanation . "The computational graph in autodiff IS a directed acyclic
graph from lattice/data. Reverse-mode AD is topological sort + backward
traversal — fundamental graph algorithms applied to gradient flow."))

    ((skill-a . geometry)
     (skill-b . optics)
     (shared . (functional-updates computational-geometry))
     (explanation . "Physics lenses compose geometric accessors with the
optics tower. A lens into a rigid body's position composes with a lens
into a world's body list, enabling deep functional updates of physics state."))))

;;; ====
;;; Task Type Generators
;;; ====

(define *ml-counter* 0)

(define (ml/next-id)
  (set! *ml-counter* (+ *ml-counter* 1))
  (format "meta_lattice_~3,'0d" *ml-counter*))

(define (ml/skill-ref skill key)
  (cdr (assq key skill)))

(define (ml/generate-search skill)
  (doc 'description "Generate: search the lattice for functions by keyword")
  (let* ([name (ml/skill-ref skill 'name)]
         [desc (ml/skill-ref skill 'description)]
         [exports (ml/skill-ref skill 'key-exports)]
         [first-export (car exports)]
         ;; Pick a search-friendly keyword from the description
         [query-word (symbol->string name)])
    (let* ([prompt-body
            (format "You need to find functions related to \"~a\" in The Fold's lattice.

Use the lattice search API to:
1. Search for functions matching the query
2. Examine the top results
3. Identify which skill and module they belong to

Show the search command and interpret the results."
                    query-word)]
           [ground-truth
            (format ";; Initialize lattice tooling
(load \"lattice/meta/meta.ss\")
(lattice-init!)

;; Search for ~a-related functions
(define results (lattice-find-data \"~a\"))

;; results is a list of alists:
;; ((id . ~a) (score . 0.95) (type . export)
;;  (module . ~a) (require . ~a) ...)
;;
;; Top results should include exports from the ~a skill:
;; ~a"
                    query-word query-word
                    first-export
                    (cdar (ml/skill-ref skill 'modules))
                    (caar (ml/skill-ref skill 'modules))
                    name
                    (let loop ([es (list-head exports (min 5 (length exports)))]
                               [acc ""])
                      (if (null? es) acc
                          (loop (cdr es)
                                (string-append acc (format ";;   ~a\n" (car es)))))))]
           [verify-sexp
            `(begin
               (lattice-ensure!)
               (let ([results (lattice-find-data ,(symbol->string name))])
                 (and (pair? results)
                      (assq 'id (car results)))))]
           [verify-str (let ([p (open-output-string)])
                         (write verify-sexp p)
                         (get-output-string p))]
           [id (ml/next-id)]
           [tags (list (symbol->string name) "meta_lattice" "search")])
      (make-sample id "meta_lattice" "usage" "easy"
                   "" ""
                   (symbol->string name)
                   prompt-body ground-truth verify-str tags))))

(define (ml/generate-inspect skill)
  (doc 'description "Generate: inspect skill structure and exports")
  (let* ([name (ml/skill-ref skill 'name)]
         [desc (ml/skill-ref skill 'description)]
         [tier (ml/skill-ref skill 'tier)]
         [purity (ml/skill-ref skill 'purity)]
         [deps (ml/skill-ref skill 'deps)]
         [modules (ml/skill-ref skill 'modules)]
         [exports (ml/skill-ref skill 'key-exports)])
    (let* ([prompt-body
            (format "Examine the `~a` skill in The Fold's lattice.

Get its metadata (tier, purity, dependencies, module count, export count),
then list its exports grouped by module. Explain what the skill provides
and how its modules are organized."
                    name)]
           [ground-truth
            (format ";; Initialize lattice tooling
(load \"lattice/meta/meta.ss\")
(lattice-init!)

;; Get structured skill info
(define info (lattice-info '~a))
;; => ((name . ~a) (version . \"...\") (tier . ~a) (purity . ~a)
;;     (dependencies . ~a) (dependents . ~a)
;;     (module-count . ~a) (export-count . ~a) ...)

;; Get grouped exports
(define exports (lattice-exports-data '~a))
;; => ((skill . ~a) (total . ~a)
;;     (groups . (...)))
;;
;; Key exports include:
~a"
                    name name tier purity
                    deps (ml/skill-ref skill 'used-by)
                    (length modules) (length exports)
                    name name (length exports)
                    (let loop ([es (list-head exports (min 6 (length exports)))]
                               [acc ""])
                      (if (null? es) acc
                          (loop (cdr es)
                                (string-append acc (format ";;   ~a\n" (car es)))))))]
           [verify-sexp
            `(begin
               (lattice-ensure!)
               (let ([info (lattice-info ',name)])
                 (and info
                      (assq 'name info)
                      (assq 'tier info)
                      (assq 'dependencies info))))]
           [verify-str (let ([p (open-output-string)])
                         (write verify-sexp p)
                         (get-output-string p))]
           [id (ml/next-id)]
           [tags (list (symbol->string name) "meta_lattice" "inspect")])
      (make-sample id "meta_lattice" "analysis" "easy"
                   "" ""
                   (symbol->string name)
                   prompt-body ground-truth verify-str tags))))

(define (ml/generate-dep-chain skill)
  (doc 'description "Generate: navigate dependency chain from skill")
  (let* ([name (ml/skill-ref skill 'name)]
         [deps (ml/skill-ref skill 'deps)]
         [used-by (ml/skill-ref skill 'used-by)])
    ;; Need at least one dep and one user for a meaningful chain
    (if (or (null? deps) (null? used-by))
        #f
        (let* ([dep (car deps)]
               [user (car used-by)]
               [prompt-body
                (format "Explore the dependency chain around the `~a` skill.

1. Find what `~a` depends on (its upstream skills)
2. Find what depends on `~a` (its downstream skills)
3. Check if there's a path from `~a` to `~a` through the dependency graph
4. Show the full transitive dependency closure

Explain the skill's position in the lattice DAG."
                        name name name dep user)]
               [ground-truth
                (format ";; Initialize lattice tooling
(load \"lattice/meta/meta.ss\")
(lattice-init!)

;; Direct dependencies (upstream)
(lattice-deps '~a)
;; => ~a

;; Direct dependents (downstream)
(lattice-uses '~a)
;; => ~a

;; Full transitive dependency closure
(lattice-deps-transitive '~a)
;; => ... (all skills ~a transitively depends on)

;; Path finding: is there a path from ~a to ~a?
(lattice-path '~a '~a)
;; => ~a (if connected) or #f (if not)"
                        name deps
                        name used-by
                        name name
                        dep user
                        dep user
                        (if (eq? dep name) (format "(~a)" dep)
                            (format "(~a ~a)" dep name)))]
               [verify-sexp
                `(begin
                   (lattice-ensure!)
                   (and (pair? (lattice-deps ',name))
                        (list? (lattice-uses ',name))
                        (list? (lattice-deps-transitive ',name))))]
               [verify-str (let ([p (open-output-string)])
                             (write verify-sexp p)
                             (get-output-string p))]
               [id (ml/next-id)]
               [tags (list (symbol->string name) (symbol->string dep)
                           "meta_lattice" "dep_chain")])
          (make-sample id "meta_lattice" "analysis" "medium"
                       "" ""
                       (symbol->string name)
                       prompt-body ground-truth verify-str tags)))))

(define (ml/generate-concept-bridge bridge)
  (doc 'description "Generate: discover concept bridge between two skills")
  (let* ([skill-a (cdr (assq 'skill-a bridge))]
         [skill-b (cdr (assq 'skill-b bridge))]
         [shared (cdr (assq 'shared bridge))]
         [explanation (cdr (assq 'explanation bridge))])
    (let* ([prompt-body
            (format "Discover the conceptual bridge between `~a` and `~a` in The Fold's lattice.

These skills come from different domains but share deep connections.
Use the knowledge graph concept bridge API to:
1. Find shared concepts between the two skills
2. Check if they're dependency-connected or independent
3. Explain why the connection is surprising or useful

What cross-domain insight does this bridge reveal?"
                    skill-a skill-b)]
           [ground-truth
            (format ";; Initialize lattice tooling (with KG)
(load \"lattice/meta/meta.ss\")
(lattice-init!)

;; Find concept bridge
(define bridge (kg-concept-bridge '~a '~a))
;; => ~a

;; Check for transitive bridge (via concept hierarchy)
(define trans (kg-shared-concepts-transitive '~a '~a))

;; What each skill provides
(kg-skill-concepts '~a)  ;; => ...
(kg-skill-concepts '~a)  ;; => ...

;; The connection:
;; ~a"
                    skill-a skill-b shared
                    skill-a skill-b
                    skill-a skill-b
                    (let ([lines (string-split-newline explanation)])
                      (apply string-append
                        (map (lambda (l) (string-append ";; " l "\n")) lines))))]
           [verify-sexp
            `(begin
               (lattice-ensure!)
               ;; Both skills should be known to the KG
               (and (pair? (kg-skill-concepts ',skill-a))
                    (pair? (kg-skill-concepts ',skill-b))))]
           [verify-str (let ([p (open-output-string)])
                         (write verify-sexp p)
                         (get-output-string p))]
           [id (ml/next-id)]
           [tags (list (symbol->string skill-a) (symbol->string skill-b)
                       "meta_lattice" "concept_bridge")])
      (make-sample id "meta_lattice" "analysis" "medium"
                   "" ""
                   (format "~a-~a" skill-a skill-b)
                   prompt-body ground-truth verify-str tags))))

(define (ml/generate-discovery skill)
  (doc 'description
    "Generate: full discovery workflow — search → inspect → require → use")
  (let* ([name (ml/skill-ref skill 'name)]
         [desc (ml/skill-ref skill 'description)]
         [modules (ml/skill-ref skill 'modules)]
         [exports (ml/skill-ref skill 'key-exports)]
         [first-mod (car modules)]
         [mod-name (car first-mod)]
         [mod-path (cdr first-mod)]
         ;; Pick two exports to demonstrate
         [fn1 (car exports)]
         [fn2 (cadr exports)])
    (let* ([prompt-body
            (format "You need to work with ~a in The Fold but don't know where to start.

Walk through the complete discovery workflow:
1. **Search**: Find relevant functions using lattice search
2. **Inspect**: Examine the skill's structure, modules, and exports
3. **Require**: Load the appropriate module(s)
4. **Use**: Demonstrate using the discovered functions

Show each step with commands and expected results. Use the structured
data APIs (lattice-find-data, lattice-info, lattice-exports-data) so
results can be programmatically processed."
                    desc)]
           [ground-truth
            (format ";; === Step 1: Search ===
(load \"lattice/meta/meta.ss\")
(lattice-init!)

(define results (lattice-find-data \"~a\"))
;; Top result: ((id . ~a) (score . ...) (type . export)
;;              (module . ~a) ...)

;; === Step 2: Inspect ===
;; Found skill '~a — examine it
(define info (lattice-info '~a))
;; tier ~a, ~a, deps: ~a

(define exports (lattice-exports-data '~a))
;; ~a modules, ~a+ exports

;; === Step 3: Require ===
(require '~a)
;; Loads ~a and its dependencies

;; === Step 4: Use ===
;; Now we can use the exported functions:
;; (~a ...) — ~a
;; (~a ...) — ~a"
                    (symbol->string name)
                    fn1 mod-name
                    name name
                    (ml/skill-ref skill 'tier)
                    (ml/skill-ref skill 'purity)
                    (ml/skill-ref skill 'deps)
                    name (length modules) (length exports)
                    mod-name mod-path
                    fn1 desc fn2 desc)]
           [verify-sexp
            `(begin
               (lattice-ensure!)
               (and (pair? (lattice-find-data ,(symbol->string name)))
                    (lattice-info ',name)
                    (lattice-exports-data ',name)))]
           [verify-str (let ([p (open-output-string)])
                         (write verify-sexp p)
                         (get-output-string p))]
           [id (ml/next-id)]
           [tags (list (symbol->string name) "meta_lattice" "discovery")])
      (make-sample id "meta_lattice" "usage" "hard"
                   "" ""
                   (symbol->string name)
                   prompt-body ground-truth verify-str tags))))

;;; ====
;;; Utility
;;; ====

(define (string-split-newline str)
  (let loop ([i 0] [start 0] [acc '()])
    (cond
      [(= i (string-length str))
       (reverse (cons (substring str start i) acc))]
      [(char=? (string-ref str i) #\newline)
       (loop (+ i 1) (+ i 1) (cons (substring str start i) acc))]
      [else (loop (+ i 1) start acc)])))

;;; ====
;;; Main Generator
;;; ====

(define (meta-lattice/generate-seeds)
  (doc 'description
    "Generate lattice navigation SFT samples from archetypes.
     Returns list of sample alists in standard schema.")
  (set! *ml-counter* 0)
  (printf "Generating lattice navigation seeds...\\n")

  ;; Generate skill-based tasks
  (let* ([skill-samples
          (apply append
            (map (lambda (skill)
                   (let* ([s1 (guard (ex [else #f])
                                (ml/generate-search skill))]
                          [s2 (guard (ex [else #f])
                                (ml/generate-inspect skill))]
                          [s3 (guard (ex [else #f])
                                (ml/generate-dep-chain skill))]
                          [s4 (guard (ex [else #f])
                                (ml/generate-discovery skill))])
                     (filter (lambda (x) x) (list s1 s2 s3 s4))))
                 *ml-skills*))]

         ;; Generate concept bridge tasks
         [bridge-samples
          (filter-map
            (lambda (bridge)
              (guard (ex [else #f])
                (ml/generate-concept-bridge bridge)))
            *ml-bridges*)]

         [all-samples (append skill-samples bridge-samples)])

    (printf "Generated ~a lattice navigation seeds (~a skill, ~a bridge)\\n"
            (length all-samples) (length skill-samples) (length bridge-samples))
    all-samples))

;;; ====
;;; REPL interface
;;; ====

(printf "meta-lattice.ss loaded.\\n")
(printf "  (meta-lattice/generate-seeds) - Generate lattice navigation seeds\\n")
