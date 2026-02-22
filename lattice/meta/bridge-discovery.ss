;;; @module bridge-discovery
;;; @requires prelude

(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
(require 'prelude)

(doc 'module 'bridge-discovery)
(doc 'description "Bridge discovery for the skill lattice. Identifies modules that
  integrate multiple top-level skill domains (e.g., optics+autodiff, linalg+data).
  Provides runtime bridge registry and automatic candidate detection via module
  dependency analysis.")
(doc 'layer 'lattice)
(doc 'purity 'partial)

;;; ====================================================================
;;; Bridge Registry
;;; ====================================================================

(doc 'section 'registry)

(doc *bridge-registry* 'type '(Hashtable Symbol (List Symbol)))
(doc *bridge-registry* 'description "Maps module names to the list of top-level skill
  domains they bridge. Populated by register-bridge! calls below.")
(define *bridge-registry* (make-eq-hashtable))

(doc register-bridge! 'type '(-> Symbol (List Symbol) Void))
(doc register-bridge! 'description "Register a module as bridging the given skill domains.
  Domains should be top-level lattice directory names (e.g., optics, autodiff, linalg).")
(doc register-bridge! 'export #t)
(define (register-bridge! module-name domains)
  (hashtable-set! *bridge-registry* module-name domains))

;;; ====================================================================
;;; Known Bridge Declarations
;;; ====================================================================
;;;
;;; These correspond to (doc 'bridges ...) annotations in the source files.
;;; Maintained here as runtime-queryable data.

(doc 'section 'declarations)

;; autodiff/traced-optics.ss -- bridges optics + autodiff + physics
(register-bridge! 'traced-optics '(optics autodiff physics))

;; linalg/graph-laplacian.ss -- bridges linalg + data
(register-bridge! 'graph-laplacian '(linalg data))

;; data/graph/graph-matrix.ss -- bridges data + linalg
(register-bridge! 'graph-matrix '(data linalg))

;; data/graph/pagerank.ss -- bridges data + linalg
(register-bridge! 'pagerank '(data linalg))

;; data/graph/spectral-community.ss -- bridges data + linalg
(register-bridge! 'spectral-community '(data linalg))

;; data/graph/graph-bridge.ss -- bridges data + linalg
(register-bridge! 'graph-bridge '(data linalg))

;; data/graph/graph-homology.ss -- bridges data + topology
(register-bridge! 'graph-homology '(data topology))

;; data/graph/graph-filtration.ss -- bridges data + topology
(register-bridge! 'graph-filtration '(data topology))

;; info-stat/mi-bridge.ss -- bridges info + statistics
(register-bridge! 'info-stat/mi-bridge '(info statistics))

;; optimization/first-order.ss -- bridges optimization + autodiff + linalg
(register-bridge! 'first-order '(optimization autodiff linalg))

;; optimization/optic-first-order.ss -- bridges optimization + optics + autodiff
(register-bridge! 'optic-first-order '(optimization optics autodiff))

;; linalg/matrix-optics.ss -- bridges linalg + optics
(register-bridge! 'matrix-optics '(linalg optics))

;; geometry/geometry-optics.ss -- bridges geometry + optics
(register-bridge! 'geometry-optics '(geometry optics))

;; physics/lenses/optics-integration.ss -- bridges physics + optics
(register-bridge! 'optics-integration '(physics optics))

;; physics/diff/optic-optimize.ss -- bridges physics + optics + autodiff
(register-bridge! 'optic-optimize '(physics optics autodiff))

;; physics/diff/traced-vec2.ss -- bridges physics + linalg + autodiff
(register-bridge! 'traced-vec2 '(physics linalg autodiff))

;; algebra/tropical-graph.ss -- bridges algebra + data
(register-bridge! 'tropical-graph '(algebra data))

;; algebra/poly-bridge.ss -- bridges algebra + numeric
(register-bridge! 'poly-bridge '(algebra numeric))

;; statistics/regression/traced-regression.ss -- bridges statistics + optics + autodiff
(register-bridge! 'traced-regression '(statistics optics autodiff))

;; statistics/regression/autodiff-fit.ss -- bridges statistics + autodiff + optimization
(register-bridge! 'autodiff-fit '(statistics autodiff optimization))

;; autodiff/differentiable-signal.ss -- bridges autodiff + numeric
(register-bridge! 'differentiable-signal '(autodiff numeric))

;; autodiff/symbolic-diff.ss -- bridges autodiff + fp
(register-bridge! 'symbolic-diff '(autodiff fp))

;; fp/symbolic/egraph-simplify.ss -- bridges fp + egraph
(register-bridge! 'egraph-simplify '(fp egraph))

;; numeric/signal-poly.ss -- bridges numeric + algebra
(register-bridge! 'signal-poly '(numeric algebra))

;;; ====================================================================
;;; Query Functions
;;; ====================================================================

(doc 'section 'queries)

(doc lattice-bridges 'type '(-> (List (Pair Symbol (List Symbol)))))
(doc lattice-bridges 'description "List all modules with bridge annotations.
  Returns an alist of (module-name . bridged-domains).")
(doc lattice-bridges 'export #t)
(define (lattice-bridges)
  (let ([keys (vector->list (hashtable-keys *bridge-registry*))])
    (map (lambda (k) (cons k (hashtable-ref *bridge-registry* k '())))
         (sort (lambda (a b)
                 (string<? (symbol->string a) (symbol->string b)))
               keys))))

(doc lattice-bridges-for 'type '(-> Symbol (List (Pair Symbol (List Symbol)))))
(doc lattice-bridges-for 'description "List modules that bridge a given skill domain to
  other domains. Returns an alist of (module-name . bridged-domains) where
  the given skill appears in the domain list.")
(doc lattice-bridges-for 'export #t)
(define (lattice-bridges-for skill)
  (filter (lambda (entry)
            (memq skill (cdr entry)))
          (lattice-bridges)))

(doc bridge-domains 'type '(-> Symbol (Maybe (List Symbol))))
(doc bridge-domains 'description "Get the bridged domains for a specific module, or #f
  if the module has no bridge annotation.")
(doc bridge-domains 'export #t)
(define (bridge-domains module-name)
  (let ([result (hashtable-ref *bridge-registry* module-name #f)])
    (or result #f)))

(doc bridge? 'type '(-> Symbol Boolean))
(doc bridge? 'description "Check whether a module has a bridge annotation.")
(doc bridge? 'export #t)
(define (bridge? module-name)
  (hashtable-contains? *bridge-registry* module-name))

(doc bridge-count 'type '(-> Nat))
(doc bridge-count 'description "Return the number of registered bridge modules.")
(doc bridge-count 'export #t)
(define (bridge-count)
  (hashtable-size *bridge-registry*))

;;; ====================================================================
;;; Automatic Bridge Candidate Detection
;;; ====================================================================

(doc 'section 'detection)

;;; path->skill-domain : String -> (Maybe Symbol)
;;; Extract the top-level skill domain from a module path.
;;; "lattice/linalg/vec.ss"         -> linalg
;;; "lattice/data/graph/pagerank.ss" -> data
;;; "lattice/fp/sat/solver.ss"      -> fp
;;; "core/autodiff/comp-graph.ss"   -> autodiff
;;; Non-lattice/core paths return #f.
(doc path->skill-domain 'type '(-> String (Maybe Symbol)))
(doc path->skill-domain 'description "Extract top-level skill domain from a module file path.")
(doc path->skill-domain 'export #t)
(define (path->skill-domain path)
  (cond
    [(string-starts-with? path "lattice/")
     (let* ([after (substring path 8 (string-length path))]
            [slash-pos (string-index-of-bd after #\/)])
       (if slash-pos
           (string->symbol (substring after 0 slash-pos))
           #f))]
    [(string-starts-with? path "core/autodiff/")
     'autodiff]
    [else #f]))

;;; string-index-of-bd : String Char -> (Maybe Nat)
;;; Find first occurrence of char in string.  Local helper.
(define (string-index-of-bd str ch)
  (let ([len (string-length str)])
    (let loop ([i 0])
      (cond
        [(>= i len) #f]
        [(char=? (string-ref str i) ch) i]
        [else (loop (+ i 1))]))))

;;; module-skill-domain : Symbol -> (Maybe Symbol)
;;; Look up the skill domain for a module using the module path registry.
(doc module-skill-domain 'type '(-> Symbol (Maybe Symbol)))
(doc module-skill-domain 'description "Get the top-level skill domain for a registered module.")
(doc module-skill-domain 'export #t)
(define (module-skill-domain mod-name)
  (let ([path (hashtable-ref *module-paths* mod-name #f)])
    (if path
        (path->skill-domain path)
        #f)))

;;; module-dep-domains : Symbol -> (List Symbol)
;;; Collect the distinct skill domains of a module's direct dependencies.
(doc module-dep-domains 'type '(-> Symbol (List Symbol)))
(doc module-dep-domains 'description "Get the set of distinct skill domains required by a module's dependencies.")
(doc module-dep-domains 'export #t)
(define (module-dep-domains mod-name)
  (let ([deps (hashtable-ref *module-deps* mod-name '())])
    (unique
      (filter (lambda (x) x)
              (map module-skill-domain deps)))))

;;; ensure-all-deps-registered! : -> Void
;;; Parse @requires headers for all registered lattice modules that lack
;;; dependency data.  This populates *module-deps* without loading modules.
(doc ensure-all-deps-registered! 'type '(-> Void))
(doc ensure-all-deps-registered! 'description "Auto-register dependency metadata for all
  lattice modules by parsing their @requires headers.  Safe to call repeatedly.")
(doc ensure-all-deps-registered! 'export #t)
(define (ensure-all-deps-registered!)
  (let ([all-mods (vector->list (hashtable-keys *module-paths*))])
    (for-each
      (lambda (mod)
        (unless (hashtable-contains? *module-deps* mod)
          (auto-register-module! mod)))
      all-mods)))

;;; find-potential-bridges : -> (List (Pair Symbol (List Symbol)))
;;; Scan the module registry to find modules whose dependencies span 2+
;;; different top-level skill domains but which lack a bridge annotation.
;;; Auto-registers dependency metadata for unloaded modules first.
;;; Returns an alist of (module-name . dep-domains) sorted by module name.
(doc find-potential-bridges 'type '(-> (List (Pair Symbol (List Symbol)))))
(doc find-potential-bridges 'description "Find modules that require from 2+ different skill
  domains but lack bridge annotations. Returns candidate bridges as
  (module-name . domain-list) pairs.")
(doc find-potential-bridges 'export #t)
(define (find-potential-bridges)
  ;; Ensure dependency data is available for all registered modules
  (ensure-all-deps-registered!)
  (let* ([all-mods (vector->list (hashtable-keys *module-paths*))]
         [lattice-mods (filter (lambda (m)
                                 (let ([p (hashtable-ref *module-paths* m "")])
                                   (or (string-starts-with? p "lattice/")
                                       (string-starts-with? p "core/autodiff/"))))
                               all-mods)]
         [candidates
           (filter-map
             (lambda (mod)
               (let* ([own-domain (module-skill-domain mod)]
                      [dep-domains (module-dep-domains mod)]
                      ;; Remove the module's own domain -- bridging requires
                      ;; reaching into OTHER domains
                      [other-domains (if own-domain
                                         (filter (lambda (d) (not (eq? d own-domain)))
                                                 dep-domains)
                                         dep-domains)]
                      ;; Combine own + other to get full domain set
                      [all-domains (if own-domain
                                       (cons own-domain other-domains)
                                       other-domains)])
                 (if (and (>= (length other-domains) 1)
                          (>= (length all-domains) 2)
                          (not (bridge? mod)))
                     (cons mod (sort (lambda (a b)
                                      (string<? (symbol->string a)
                                                (symbol->string b)))
                                    all-domains))
                     #f)))
             lattice-mods)])
    (sort (lambda (a b)
            (string<? (symbol->string (car a))
                      (symbol->string (car b))))
          candidates)))

;;; ====================================================================
;;; Display Functions
;;; ====================================================================

(doc 'section 'display)

(doc lattice-bridges-pretty 'type '(-> Void))
(doc lattice-bridges-pretty 'description "Display all registered bridge modules grouped by domain.")
(doc lattice-bridges-pretty 'export #t)
(define (lattice-bridges-pretty)
  (let ([bridges (lattice-bridges)])
    (printf "\nBridge Modules (~a registered)\n" (length bridges))
    (printf "====\n\n")
    (for-each
      (lambda (entry)
        (printf "  ~a  bridges ~a\n" (car entry) (cdr entry)))
      bridges)
    (printf "\n")))

(doc lattice-bridges-for-pretty 'type '(-> Symbol Void))
(doc lattice-bridges-for-pretty 'description "Display bridge modules for a specific skill domain.")
(doc lattice-bridges-for-pretty 'export #t)
(define (lattice-bridges-for-pretty skill)
  (let ([bridges (lattice-bridges-for skill)])
    (if (null? bridges)
        (printf "No bridge modules found for skill: ~a\n" skill)
        (begin
          (printf "\nBridges involving ~a (~a modules)\n" skill (length bridges))
          (printf "====\n\n")
          (for-each
            (lambda (entry)
              (let ([other-domains (filter (lambda (d) (not (eq? d skill)))
                                          (cdr entry))])
                (printf "  ~a  -> ~a\n" (car entry) other-domains)))
            bridges)
          (printf "\n")))))

(doc find-potential-bridges-pretty 'type '(-> Void))
(doc find-potential-bridges-pretty 'description "Display potential bridge candidates that lack annotations.")
(doc find-potential-bridges-pretty 'export #t)
(define (find-potential-bridges-pretty)
  (let ([candidates (find-potential-bridges)])
    (if (null? candidates)
        (printf "No unannotated bridge candidates found.\n")
        (begin
          (printf "\nPotential Bridge Candidates (~a found)\n" (length candidates))
          (printf "====\n")
          (printf "These modules require from 2+ skill domains but lack bridge annotations.\n\n")
          (for-each
            (lambda (entry)
              (printf "  ~a  spans ~a\n" (car entry) (cdr entry)))
            candidates)
          (printf "\n  Use (register-bridge! 'module '(domain1 domain2)) to annotate.\n\n")))))

;;; ====================================================================
;;; Compact Aliases (for REPL / agent use)
;;; ====================================================================

(doc 'section 'aliases)

(doc lb 'type '(-> (List (Pair Symbol (List Symbol)))))
(doc lb 'description "Alias for lattice-bridges")
(doc lb 'export #t)
(define lb lattice-bridges)

(doc lbf 'type '(-> Symbol (List (Pair Symbol (List Symbol)))))
(doc lbf 'description "Alias for lattice-bridges-for")
(doc lbf 'export #t)
(define lbf lattice-bridges-for)

(doc lbp 'type '(-> (List (Pair Symbol (List Symbol)))))
(doc lbp 'description "Alias for find-potential-bridges")
(doc lbp 'export #t)
(define lbp find-potential-bridges)
