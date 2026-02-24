;;; @module concept-normalize
;;; @requires prelude hamt

(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
(require 'prelude)
(require 'hamt)

(doc 'module 'concept-normalize)
(doc 'description "Pure concept normalization over the skill lattice ontology.
Resolves synonyms to canonical names, answers hierarchy queries (parent,
ancestors, children), and exposes metadata (descriptions, cross-cutting
concept membership).  No I/O — the boundary layer reads the ontology sexp
and calls install-concept-ontology! to populate the module-level maps.")
(doc 'layer 'lattice)
(doc 'purity 'total)

;;; ====================================================================
;;; Internal State
;;; ====================================================================

(doc 'section 'state)

(doc *ontology-installed* 'type 'Boolean)
(doc *ontology-installed* 'description "True after install-concept-ontology! has been called.")
(define *ontology-installed* #f)

(doc *synonym-map* 'type '(HAMT Symbol Symbol))
(doc *synonym-map* 'description "Maps every alias to its canonical concept symbol.")
(define *synonym-map* hamt-empty)

;;; *parent-map* : HAMT Symbol Symbol
;;; Maps each concept to its parent concept.  Root concepts are absent from
;;; this map; hamt-has-key? is used to distinguish "no parent" from a
;;; genuine #f lookup result.
(define *parent-map* hamt-empty)

(doc *children-map* 'type '(HAMT Symbol (List Symbol)))
(doc *children-map* 'description "Maps each concept to its list of direct children.")
(define *children-map* hamt-empty)

(doc *description-map* 'type '(HAMT Symbol String))
(doc *description-map* 'description "Maps each concept to its description string.")
(define *description-map* hamt-empty)

;;; *cross-cutting-set* : HAMT Symbol #t
;;; Cross-cutting concepts are stored with value #t.  hamt-has-key? is the
;;; correct predicate — do not use hamt-lookup for boolean membership tests.
(define *cross-cutting-set* hamt-empty)

(doc *cross-cutting-skills* 'type '(HAMT Symbol (List Symbol)))
(doc *cross-cutting-skills* 'description "Maps each cross-cutting concept to its associated skill list.")
(define *cross-cutting-skills* hamt-empty)

;;; *all-concepts* : (List Symbol)
;;; Flat list of all known canonical concept names, in insertion order.
(define *all-concepts* '())

;;; ====================================================================
;;; Installation
;;; ====================================================================

(doc 'section 'installation)

;;; parse-concept-entry : SExp -> Void
;;; Walk a single (concept name ...) entry and populate the maps.
(define (parse-concept-entry entry)
  (when (and (pair? entry) (eq? (car entry) 'concept) (pair? (cdr entry)))
    (let* ([name (cadr entry)]
           [fields (cddr entry)]
           [desc-entry (assq 'description fields)]
           [parent-entry (assq 'parent fields)]
           [children-entry (assq 'children fields)])
      ;; Description
      (when desc-entry
        (set! *description-map*
              (hamt-assoc name (cadr desc-entry) *description-map*)))
      ;; Parent
      (when parent-entry
        (set! *parent-map*
              (hamt-assoc name (cadr parent-entry) *parent-map*)))
      ;; Children
      (when children-entry
        (let ([kids (cadr children-entry)])
          (set! *children-map*
                (hamt-assoc name (if (list? kids) kids '()) *children-map*))))
      ;; Track canonical name
      (set! *all-concepts* (cons name *all-concepts*)))))

;;; parse-synonym-group : SExp -> Void
;;; Walk a synonym group (canonical alias1 alias2 ...) and register all
;;; aliases, including the canonical form pointing to itself.
(define (parse-synonym-group group)
  (when (and (pair? group) (symbol? (car group)))
    (let ([canonical (car group)]
          [all-names (cdr group)])
      ;; canonical -> canonical
      (set! *synonym-map* (hamt-assoc canonical canonical *synonym-map*))
      ;; each alias -> canonical
      (for-each (lambda (alias)
                  (set! *synonym-map* (hamt-assoc alias canonical *synonym-map*)))
                all-names))))

;;; parse-cross-cutting-entry : SExp -> Void
;;; Walk a (concept name ...) entry from the cross-cutting section.
(define (parse-cross-cutting-entry entry)
  (when (and (pair? entry) (eq? (car entry) 'concept) (pair? (cdr entry)))
    (let* ([name (cadr entry)]
           [fields (cddr entry)]
           [desc-entry (assq 'description fields)]
           [skills-entry (assq 'skills fields)])
      (set! *cross-cutting-set* (hamt-assoc name #t *cross-cutting-set*))
      (when desc-entry
        (set! *description-map*
              (hamt-assoc name (cadr desc-entry) *description-map*)))
      (when skills-entry
        (let ([skills (cadr skills-entry)])
          (set! *cross-cutting-skills*
                (hamt-assoc name (if (list? skills) skills '()) *cross-cutting-skills*))))
      ;; Cross-cutting concepts also go into the all-concepts list
      (unless (memq name *all-concepts*)
        (set! *all-concepts* (cons name *all-concepts*))))))

(doc install-concept-ontology! 'type '(-> SExp Void))
(doc install-concept-ontology! 'description "Parse a concept-ontology sexp and populate
the module-level maps.  Expected shape:
  (concept-ontology
    (version N)
    (concepts     (concept name (description \"...\") (parent p) (children (c1 c2))) ...)
    (synonym-groups (canonical alias1 ...) ...)
    (cross-cutting  (concept name (description \"...\") (skills (s1 s2))) ...))
Idempotent if called multiple times — maps are rebuilt from scratch each call.")
(doc install-concept-ontology! 'export #t)
(define (install-concept-ontology! ontology-sexp)
  ;; Reset state
  (set! *synonym-map* hamt-empty)
  (set! *parent-map* hamt-empty)
  (set! *children-map* hamt-empty)
  (set! *description-map* hamt-empty)
  (set! *cross-cutting-set* hamt-empty)
  (set! *cross-cutting-skills* hamt-empty)
  (set! *all-concepts* '())
  ;; Walk top-level sections
  (when (and (pair? ontology-sexp)
             (eq? (car ontology-sexp) 'concept-ontology))
    (let ([body (cdr ontology-sexp)])
      ;; Concepts
      (let ([concepts-section (assq 'concepts body)])
        (when concepts-section
          (for-each parse-concept-entry (cdr concepts-section))))
      ;; Synonym groups
      (let ([syn-section (assq 'synonym-groups body)])
        (when syn-section
          (for-each parse-synonym-group (cdr syn-section))))
      ;; Cross-cutting
      (let ([cc-section (assq 'cross-cutting body)])
        (when cc-section
          (for-each parse-cross-cutting-entry (cdr cc-section))))))
  (set! *ontology-installed* #t))

;;; ====================================================================
;;; Normalization
;;; ====================================================================

(doc 'section 'normalization)

(doc concept-normalize 'type '(-> Symbol Symbol))
(doc concept-normalize 'description "Resolve sym to its canonical concept name.
Unknown symbols (not in any synonym group) are returned as-is.")
(doc concept-normalize 'export #t)
(define (concept-normalize sym)
  (let ([result (hamt-lookup sym *synonym-map*)])
    (if result result sym)))

(doc concept-canonical? 'type '(-> Symbol Boolean))
(doc concept-canonical? 'description "True if sym is the canonical name for a known concept
(not merely an alias).  False for unknown symbols and for aliases.")
(doc concept-canonical? 'export #t)
(define (concept-canonical? sym)
  (let ([canonical (hamt-lookup sym *synonym-map*)])
    (if canonical
        (eq? sym canonical)
        #f)))

;;; ====================================================================
;;; Hierarchy
;;; ====================================================================

(doc 'section 'hierarchy)

(doc concept-parent 'type '(-> Symbol (Maybe Symbol)))
(doc concept-parent 'description "Return the direct parent of sym in the concept hierarchy,
or #f if sym is a root (has no parent) or is unknown.
Normalizes sym before lookup.")
(doc concept-parent 'export #t)
(define (concept-parent sym)
  (let ([canonical (concept-normalize sym)])
    (hamt-lookup canonical *parent-map*)))

(doc concept-ancestors 'type '(-> Symbol (List Symbol)))
(doc concept-ancestors 'description "Return the chain of ancestors from sym's parent up to the
root, in closest-first order.  Returns '() for root concepts and unknown
symbols.  Does not include sym itself.  Iterative — no stack growth.")
(doc concept-ancestors 'export #t)
(define (concept-ancestors sym)
  (let loop ([current (concept-normalize sym)]
             [acc '()])
    (let ([parent (hamt-lookup current *parent-map*)])
      (if parent
          (loop parent (cons parent acc))
          (reverse acc)))))

(doc concept-children 'type '(-> Symbol (List Symbol)))
(doc concept-children 'description "Return the direct children of sym in the concept hierarchy.
Returns '() for leaves and unknown symbols.  Normalizes sym before lookup.")
(doc concept-children 'export #t)
(define (concept-children sym)
  (let* ([canonical (concept-normalize sym)]
         [result (hamt-lookup canonical *children-map*)])
    (if result result '())))

(doc concept-root? 'type '(-> Symbol Boolean))
(doc concept-root? 'description "True if sym is a known concept with no parent.")
(doc concept-root? 'export #t)
(define (concept-root? sym)
  (let ([canonical (concept-normalize sym)])
    (and (hamt-has-key? canonical *description-map*)
         (not (hamt-has-key? canonical *parent-map*)))))

;;; ====================================================================
;;; Metadata
;;; ====================================================================

(doc 'section 'metadata)

(doc concept-synonyms 'type '(-> Symbol (List Symbol)))
(doc concept-synonyms 'description "Return all names for sym's canonical concept: canonical
first, then aliases, in registration order.  Returns '() if sym is unknown.")
(doc concept-synonyms 'export #t)
(define (concept-synonyms sym)
  (let ([canonical (concept-normalize sym)])
    ;; Walk the entire synonym map collecting keys that map to canonical.
    ;; canonical first, then aliases.
    (if (not (hamt-has-key? canonical *synonym-map*))
        '()
        (let ([all-entries (hamt-entries *synonym-map*)])
          (let loop ([entries all-entries]
                     [aliases '()])
            (if (null? entries)
                (cons canonical aliases)
                (let ([k (caar entries)]
                      [v (cdar entries)])
                  (if (and (eq? v canonical) (not (eq? k canonical)))
                      (loop (cdr entries) (cons k aliases))
                      (loop (cdr entries) aliases)))))))))

(doc concept-description 'type '(-> Symbol String))
(doc concept-description 'description "Return the description string for sym's canonical
concept, or \"\" if the concept is unknown or has no description.")
(doc concept-description 'export #t)
(define (concept-description sym)
  (let* ([canonical (concept-normalize sym)]
         [result (hamt-lookup canonical *description-map*)])
    (if result result "")))

(doc concept-cross-cutting? 'type '(-> Symbol Boolean))
(doc concept-cross-cutting? 'description "True if sym (or its canonical form) is a cross-cutting
concept.  Cross-cutting concepts transcend individual skill domains.")
(doc concept-cross-cutting? 'export #t)
(define (concept-cross-cutting? sym)
  (let ([canonical (concept-normalize sym)])
    (hamt-has-key? canonical *cross-cutting-set*)))

(doc concept-cross-cutting-skills 'type '(-> Symbol (List Symbol)))
(doc concept-cross-cutting-skills 'description "Return the skill list associated with a
cross-cutting concept.  Returns '() if sym is not a cross-cutting concept or
has no associated skills.")
(doc concept-cross-cutting-skills 'export #t)
(define (concept-cross-cutting-skills sym)
  (let* ([canonical (concept-normalize sym)]
         [result (hamt-lookup canonical *cross-cutting-skills*)])
    (if result result '())))

;;; ====================================================================
;;; Introspection
;;; ====================================================================

(doc 'section 'introspection)

(doc concept-ontology-loaded? 'type '(-> Boolean))
(doc concept-ontology-loaded? 'description "True if install-concept-ontology! has been called.")
(doc concept-ontology-loaded? 'export #t)
(define (concept-ontology-loaded?)
  *ontology-installed*)

(doc concept-roots 'type '(-> (List Symbol)))
(doc concept-roots 'description "Return all canonical concept names that have no parent.")
(doc concept-roots 'export #t)
(define (concept-roots)
  (filter concept-root? *all-concepts*))

(doc concept-all 'type '(-> (List Symbol)))
(doc concept-all 'description "Return all known canonical concept names.")
(doc concept-all 'export #t)
(define (concept-all)
  ;; *all-concepts* accumulates in reverse insertion order; return stable order.
  (reverse *all-concepts*))
