(load "lattice/meta/search.ss")
(load "lattice/meta/serendipity.ss")
(load "lattice/meta/analytics.ss")
(load "lattice/meta/inspect.ss")
(load "boundary/meta/persist-io.ss")
(load "boundary/meta/source-loc-io.ss")
(load "boundary/meta/docstrings-io.ss")
(load "boundary/meta/kg-io.ss")
(load "boundary/meta/exports-io.ss")
(load "boundary/meta/inspect-io.ss")
(load "boundary/introspect/exports.ss")

(doc 'module 'meta)
(doc 'description "Unified entry point for lattice meta-tooling modules. Loads all lattice navigation capabilities.")
(doc 'layer 'lattice)
(doc 'purity 'partial)

(doc 'section 'initialization)

(doc lattice-init! 'type (-> Void))
(doc lattice-init! 'description "Initialize the lattice tooling.
Priority chain: CAS root → sexp cache → manifest build.
The CAS path is the KG-first fast path — if a root hash exists and
all blocks are in the store, we skip both cache and manifest parsing.")
(define (lattice-init!)
  (tier-cache-clear!)
  (cond
   ;; Priority 1: CAS-first — load from content-addressed store
   [(kg-load-from-root!)
    (kg-load-concept-ontology!)
    (lattice-index!)
    (build-source-location-cache!)
    (printf "\nLattice tooling initialized (from CAS)!\n")]
   ;; Priority 2: sexp cache — legacy fast path
   [(lattice-load-cache!)
    (lattice-index!)
    (printf "\nLattice tooling initialized (from cache)!\n")]
   ;; Priority 3: full manifest build
   [else
    (kg-build!)
    (lattice-index!)
    (build-source-location-cache!)
    (lattice-save-cache!)
    (printf "\nLattice tooling initialized!\n")])
  (printf "  Use (lf \"query\") to search\n")
  (printf "  Use (li 'skill) to inspect a skill\n")
  (printf "  Use (lef \"file.ss\") to list exports from any file\n")
  (printf "  Use (lt 'skill) to list tests\n")
  (printf "  Use (lsrc 'fn) for source location\n")
  (printf "  Use (lr 'sym) to browse related\n")
  (printf "  Use (ls) for statistics\n")
  (printf "  Use (lh) for health check\n"))

(doc lattice-init-quiet! 'type (-> Void))
(doc lattice-init-quiet! 'description "Initialize silently (for REPL startup).
Priority chain: CAS root → sexp cache → full manifest build.")
(define (lattice-init-quiet!)
  (tier-cache-clear!)
  (cond
   ;; Priority 1: CAS-first
   [(kg-load-from-root!)
    (kg-load-concept-ontology!)
    (lattice-index!)
    (build-source-location-cache!)]
   ;; Priority 2: sexp cache
   [(lattice-load-cache!)
    (lattice-index!)]
   ;; Priority 3: full rebuild
   [else
    (kg-build!)
    (lattice-index!)
    (build-source-location-cache!)
    (lattice-save-cache!)]))

(doc lattice-ensure! 'type (-> Void))
(doc lattice-ensure! 'description "Initialize only if not already ready. Use this in tests.")
(define (lattice-ensure!)
  (unless *search-ready*
          (lattice-init!)))

(doc lattice-init-fresh! 'type (-> Void))
(doc lattice-init-fresh! 'description "Force full rebuild from manifests, ignoring cache")
(define (lattice-init-fresh!)
  (tier-cache-clear!)
  (kg-build!)
  (lattice-index!)
  (build-source-location-cache!)
  (lattice-save-cache!)
  (printf "\nLattice tooling initialized (fresh build)!\n")
  (printf "  Use (lf \"query\") to search\n")
  (printf "  Use (li 'skill) to inspect a skill\n")
  (printf "  Use (lef \"file.ss\") to list exports from any file\n")
  (printf "  Use (lt 'skill) to list tests\n")
  (printf "  Use (lsrc 'fn) for source location\n")
  (printf "  Use (lr 'sym) to browse related\n")
  (printf "  Use (ls) for statistics\n")
  (printf "  Use (lh) for health check\n"))

(doc lattice-init-source! 'type (-> Void))
(doc lattice-init-source! 'description "Build KG from module source annotations + curation files only.
No manifest.sexp files are consulted. Useful for testing the manifest-free path.
Note: export coverage may be lower than manifest build (~75%) until all modules
have complete (doc sym 'export #t) annotations.")
(define (lattice-init-source!)
  (tier-cache-clear!)
  (kg-build-from-source!)
  (lattice-index!)
  (build-source-location-cache!)
  (printf "\nLattice tooling initialized (source-only build)!\n")
  (printf "  Use (lf \"query\") to search\n")
  (printf "  Use (li 'skill) to inspect a skill or module\n")
  (printf "  Use (lef \"file.ss\") to list exports from any file\n")
  (printf "  Use (lsrc 'fn) for source location\n")
  (printf "  Use (ls) for statistics\n"))

(doc 'section 'source-location)

(doc lsrc 'type (-> Symbol (Maybe String)))
(doc lsrc 'description "Quick source location lookup - prints \"file:line\" format")
(define (lsrc sym)
  (let ([loc (format-source-location sym)])
       (if loc
           loc
           (printf "Symbol not found: ~a\n" sym))))

(doc 'section 'quick-reference)

(define (lattice-help)
  (doc 'type (-> Void))
  (doc 'description "Print quick reference for lattice meta-tooling commands")
  (printf "\nLattice Meta-Tooling Quick Reference\n")
  (printf "====\n\n")
  (printf "INITIALIZATION:\n")
  (printf "  (lattice-init!)           - Init with caching (fast if unchanged)\n")
  (printf "  (lattice-init-fresh!)     - Force full rebuild from manifests\n")
  (printf "  (lattice-init-source!)    - Build from source annotations only\n\n")
  (printf "SEARCH (agent-compact by default):\n")
  (printf "  (lf \"query\")              - Full-text search\n")
  (printf "  (lfe 'symbol)             - Exact lookup (falls back to substring)\n")
  (printf "  (lfp 'prefix)             - Prefix search\n")
  (printf "  (lfs 'substr)             - Substring search\n")
  (printf "  (lf-pretty \"query\")       - Verbose search\n")
  (printf "  (lattice-complete \"pre\")  - Autocomplete\n\n")
  (printf "DEPENDENCIES:\n")
  (printf "  (ld 'skill)               - Show what skill depends on\n")
  (printf "  (lu 'skill)               - Show what uses this skill\n")
  (printf "  (lattice-path 'a 'b)      - Find path between skills\n")
  (printf "  (lattice-graph)           - Print full DAG\n\n")
  (printf "CYCLE DETECTION:\n")
  (printf "  (lc 'skill)               - Check skill for cycles\n")
  (printf "  (lattice-would-cycle? 'a 'b) - Would a->b create cycle?\n")
  (printf "  (lattice-check-deps 'skill)  - Validate skill's deps\n")
  (printf "  (lattice-validate-all)    - Validate all skills\n\n")
  (printf "INSPECTION (agent-compact by default):\n")
  (printf "  (li 'name)                - Skill or module info (compact)\n")
  (printf "  (le 'skill)               - Exports (grouped by module)\n")
  (printf "  (lm 'skill)               - Module details\n")
  (printf "  (li-pretty 'skill)        - Skill info (verbose)\n")
  (printf "  (le-pretty 'skill)        - Exports (verbose)\n")
  (printf "  (lef \"file.ss\")           - Exports from any source file\n")
  (printf "  (lsrc 'fn)                - Source location (file:line)\n")
  (printf "  (lattice-info 'skill)     - Structured alist\n")
  (printf "  (lattice-summary)         - One-line summary of all skills\n\n")
  (printf "TESTING:\n")
  (printf "  (lt 'skill)               - List test files\n")
  (printf "  (ltr 'skill)              - Run skill tests\n")
  (printf "  (lattice-tests 'skill)    - Get test file list\n")
  (printf "  (lattice-tests-run 'skill) - Run tests (structured result)\n")
  (printf "  (lattice-tests-summary)   - Test coverage overview\n\n")
  (printf "ANALYTICS:\n")
  (printf "  (ls)                      - Lattice statistics\n")
  (printf "  (lh)                      - Health check\n")
  (printf "  (lattice-coverage-pretty) - Coverage report\n")
  (printf "  (lattice-hubs)            - Most-used skills\n\n")
  (printf "STRUCTURAL:\n")
  (printf "  (lattice-roots)           - Dependency-free skills\n")
  (printf "  (lattice-tier-0)          - Tier 0 foundational skills\n")
  (printf "  (lattice-leaves)          - Skills with no dependents\n")
  (printf "  (lattice-tiers)           - Skills grouped by tier\n\n")
  (printf "CONCEPTS:\n")
  (printf "  (lk 'concept)             - Skills providing concept\n")
  (printf "  (lkk 'skill)              - Concepts of a skill\n")
  (printf "  (lkb 'skill-a 'skill-b)   - Concept bridge (with hierarchy fallback)\n")
  (printf "  (lkr 'skill)              - Skills related by shared concepts\n")
  (printf "  (lkh 'concept)            - Concept hierarchy (ancestors, children, skills)\n\n")
  (printf "BRIDGES:\n")
  (printf "  (lbc)                     - Top concept bridges by surprise\n")
  (printf "  (lbc k)                   - Top k concept bridges\n")
  (printf "  (lbc-all)                 - All bridges (including dep-connected)\n")
  (printf "  (lb)                      - Module-level bridges (structural)\n\n")
  (printf "DISCOVERY:\n")
  (printf "  (lr 'sym)                 - Browse related (pretty-print)\n")
  (printf "  (lrr 'sym)               - Browse related (raw alist)\n"))

(doc 'section 'concept-functions)

(doc lk 'type (-> Symbol Void))
(doc lk 'description "Show skills that provide a concept (normalizes aliases)")
(define (lk concept-name)
  (let* ([canonical (concept-normalize concept-name)]
         [skills (kg-concept-skills canonical)])
    (when (not (eq? canonical concept-name))
      (printf "  (~a -> ~a)\n" concept-name canonical))
    (if (null? skills)
        (printf "No skills found for concept: ~a\n" canonical)
        (begin
          (printf "Concept '~a' is provided by:\n" canonical)
          (for-each (lambda (s) (printf "  ~a\n" s)) skills)))))

(doc lkk 'type (-> Symbol Void))
(doc lkk 'description "Show concepts of a skill")
(define (lkk skill-name)
  (let ([concepts (kg-skill-concepts skill-name)])
    (if (null? concepts)
        (printf "No concepts found for skill: ~a\n" skill-name)
        (begin
          (printf "Skill '~a' provides concepts:\n" skill-name)
          (for-each (lambda (c) (printf "  ~a\n" c)) concepts)))))

(doc lkb 'type (-> Symbol Symbol Void))
(doc lkb 'description "Show concept bridge between two skills, with transitive hierarchy fallback")
(define (lkb skill-a skill-b)
  (let ([bridge (kg-concept-bridge skill-a skill-b)])
    (if (null? bridge)
        ;; Try transitive bridge via hierarchy
        (let ([trans (kg-shared-concepts-transitive skill-a skill-b)])
          (if (null? trans)
              (printf "No concept bridge between ~a and ~a\n" skill-a skill-b)
              (begin
                (printf "~a <-> ~a bridged by concepts (via hierarchy):\n" skill-a skill-b)
                (for-each (lambda (c) (printf "  ~a\n" c)) trans))))
        (begin
          (printf "~a <-> ~a bridged by concepts:\n" skill-a skill-b)
          (for-each (lambda (c) (printf "  ~a\n" c)) bridge)))))

(doc lkr 'type (-> Symbol Void))
(doc lkr 'description "Show skills related by shared concepts")
(define (lkr skill-name)
  (let ([related (kg-related-by-concept skill-name 10)])
    (if (null? related)
        (printf "No concept-related skills for: ~a\n" skill-name)
        (begin
          (printf "Skills related to '~a' by shared concepts:\n" skill-name)
          (for-each
           (lambda (pair)
             (printf "  ~a (~a shared concepts)\n" (car pair) (cdr pair)))
           related)))))

(doc lkh 'type (-> Symbol Void))
(doc lkh 'description "Show concept hierarchy: ancestors, children, and providing skills")
(define (lkh concept-name)
  (let* ([canonical (concept-normalize concept-name)]
         [ancestors (kg-concept-ancestors canonical)]
         [children (concept-children canonical)]
         [skills (kg-concept-skills canonical)]
         [desc (concept-description canonical)]
         [is-xc (concept-cross-cutting? canonical)])
    (when (not (eq? canonical concept-name))
      (printf "  (~a is an alias for ~a)\n" concept-name canonical))
    (printf "Concept: ~a" canonical)
    (when is-xc (printf " [cross-cutting]"))
    (newline)
    (unless (string=? desc "")
      (printf "  ~a\n" desc))
    (unless (null? ancestors)
      (printf "  Ancestors: ~a\n"
              (let build ([ancs ancestors] [parts '()])
                (if (null? ancs)
                    (apply string-append (reverse parts))
                    (build (cdr ancs)
                           (cons (if (null? parts)
                                     (symbol->string (car ancs))
                                     (string-append " > " (symbol->string (car ancs))))
                                 parts))))))
    (unless (null? children)
      (printf "  Children: ~a\n"
              (apply string-append
                     (let fmt ([cs children] [acc '()])
                       (if (null? cs) (reverse acc)
                           (fmt (cdr cs)
                                (cons (if (null? acc)
                                          (symbol->string (car cs))
                                          (string-append ", " (symbol->string (car cs))))
                                      acc)))))))
    (if (null? skills)
        (printf "  No skills directly provide this concept\n")
        (begin
          (printf "  Skills: ~a\n"
                  (apply string-append
                         (let fmt ([ss skills] [acc '()])
                           (if (null? ss) (reverse acc)
                               (fmt (cdr ss)
                                    (cons (if (null? acc)
                                              (symbol->string (car ss))
                                              (string-append ", " (symbol->string (car ss))))
                                          acc))))))))))

(doc 'section 'bridge-detection)

(doc lbc 'type (case-> (-> Void) (-> Int Void)))
(doc lbc 'description "Show top concept bridges ranked by surprise score.
Optional argument k controls how many to show (default 15).
Filters to surprise >= medium (non-dep-connected bridges).")
(define lbc
  (case-lambda
    [() (lbc 15)]
    [(k)
     (let ([bridges (kg-surprising-bridges k)])
       (if (null? bridges)
           (printf "No surprising concept bridges found.\n")
           (begin
             (printf "\nConcept Bridges (~a shown, surprise >= medium)\n" (length bridges))
             (printf "====\n\n")
             (for-each
              (lambda (b)
                (let ([skills (cdr (assq 'skills b))]
                      [score (cdr (assq 'score b))]
                      [surprise (cdr (assq 'surprise b))]
                      [shared (cdr (assq 'shared b))]
                      [cc (cdr (assq 'cross-cutting b))])
                  (printf "  ~a <-> ~a  score=~a [~a]\n"
                          (car skills) (cadr skills)
                          (round-to score 1) surprise)
                  (printf "    shared: ~a\n"
                          (apply string-append
                                 (let fmt ([cs shared] [acc '()])
                                   (if (null? cs) (reverse acc)
                                       (fmt (cdr cs)
                                            (cons (if (null? acc)
                                                      (symbol->string (car cs))
                                                      (string-append ", " (symbol->string (car cs))))
                                                  acc))))))
                  (unless (null? cc)
                    (printf "    cross-cutting: ~a\n"
                            (apply string-append
                                   (let fmt ([cs cc] [acc '()])
                                     (if (null? cs) (reverse acc)
                                         (fmt (cdr cs)
                                              (cons (if (null? acc)
                                                        (symbol->string (car cs))
                                                        (string-append ", " (symbol->string (car cs))))
                                                    acc)))))))))
              bridges)
             (printf "\n"))))]))

(doc lbc-all 'type (-> Void))
(doc lbc-all 'description "Show all concept bridges including low-surprise dep-connected ones.")
(define (lbc-all)
  (let ([bridges (kg-detect-bridges)])
    (if (null? bridges)
        (printf "No concept bridges found.\n")
        (begin
          (printf "\nAll Concept Bridges (~a total)\n" (length bridges))
          (printf "====\n\n")
          (for-each
           (lambda (b)
             (let ([skills (cdr (assq 'skills b))]
                   [score (cdr (assq 'score b))]
                   [surprise (cdr (assq 'surprise b))]
                   [dep (cdr (assq 'dep-connected b))]
                   [shared (cdr (assq 'shared b))])
               (printf "  ~a <-> ~a  score=~a [~a]~a\n"
                       (car skills) (cadr skills)
                       (round-to score 1) surprise
                       (if (eq? dep 'none) ""
                           (format " (dep: ~a)" dep)))
               (printf "    ~a\n"
                       (apply string-append
                              (let fmt ([cs shared] [acc '()])
                                (if (null? cs) (reverse acc)
                                    (fmt (cdr cs)
                                         (cons (if (null? acc)
                                                   (symbol->string (car cs))
                                                   (string-append ", " (symbol->string (car cs))))
                                               acc))))))))
           bridges)
          (printf "\n")))))

(doc 'section 'repl-interface)

(meta-printf "\nmeta.ss loaded — KG-first lattice tooling\n")
(meta-printf "  (lattice-init!)  - Initialize (build KG + indices)\n")
(meta-printf "  (lattice-help)   - Show quick reference\n")
