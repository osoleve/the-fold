;;; lattice/egraph/match.ss — Pattern Matching on E-Graphs
;;;
;;; Patterns are S-expressions with pattern variables (symbols starting with ?).
;;; Matching a pattern against an e-class yields all substitutions that make
;;; the pattern equivalent to some e-node in that class.
;;;
;;; Key insight: Because e-classes contain multiple equivalent e-nodes,
;;; a single pattern can match in multiple ways. We return ALL valid
;;; substitutions to enable exhaustive rewriting.
;;;
;;; Example:
;;;   Pattern: (+ ?x ?y)
;;;   E-class contains: (+ e0 e1), (+ e2 e0)
;;;   Matches: [{?x → e0, ?y → e1}, {?x → e2, ?y → e0}]
;;;
;;; This is Lattice code: pure operations, no mutation.

(load "core/base/prelude.ss")
(load "lattice/egraph/egraph.ss")

(doc 'module 'egraph/match)
(doc 'description "Pattern matching on e-graphs for equality saturation")
(doc 'layer 'lattice)
(doc 'tier 1)
(doc 'purity 'total)

;;; ============================================================
;;; Pattern Variables
;;; ============================================================

(doc 'section 'variables)

;;; Pattern variables are symbols starting with '?'.
;;; Example: ?x, ?y, ?foo

(define (pattern-var? x)
  (doc 'type (-> Any Boolean))
  (doc 'description "Check if x is a pattern variable (symbol starting with ?).")
  (doc 'export #t)
  (and (symbol? x)
       (let ([s (symbol->string x)])
         (and (> (string-length s) 1)
              (char=? (string-ref s 0) #\?)))))

(define (pattern-var-name var)
  (doc 'type (-> PatternVar Symbol))
  (doc 'description "Get the name of a pattern variable (without the ? prefix).")
  (let ([s (symbol->string var)])
    (string->symbol (substring s 1 (string-length s)))))

;;; ============================================================
;;; Substitutions
;;; ============================================================

(doc 'section 'substitutions)

;;; A substitution is an association list mapping pattern variables to e-class IDs.
;;; Example: ((?x . 0) (?y . 1))

(define (empty-subst)
  (doc 'type (-> Substitution))
  (doc 'description "Create an empty substitution.")
  (doc 'export #t)
  '())

(define (subst-lookup subst var)
  (doc 'type (-> Substitution PatternVar (Or EClassId #f)))
  (doc 'description "Look up a variable in a substitution.")
  (doc 'export #t)
  (let ([entry (assq var subst)])
    (if entry (cdr entry) #f)))

(define (subst-extend subst var class-id)
  (doc 'type (-> Substitution PatternVar EClassId Substitution))
  (doc 'description "Extend substitution with a new binding.")
  (doc 'export #t)
  (cons (cons var class-id) subst))

(define (subst-compatible? subst var class-id)
  (doc 'type (-> Substitution PatternVar EClassId Boolean))
  (doc 'description "Check if binding var to class-id is compatible with existing subst.")
  (doc 'export #t)
  (let ([existing (subst-lookup subst var)])
    (or (not existing)
        (= existing class-id))))

(define (subst-try-extend subst var class-id)
  (doc 'type (-> Substitution PatternVar EClassId (Or Substitution #f)))
  (doc 'description "Try to extend substitution. Returns #f if incompatible.")
  (doc 'export #t)
  (let ([existing (subst-lookup subst var)])
    (cond
      [(not existing) (subst-extend subst var class-id)]
      [(= existing class-id) subst]  ; Already bound to same class
      [else #f])))  ; Conflict

(define (subst-merge subst1 subst2)
  (doc 'type (-> Substitution Substitution (Or Substitution #f)))
  (doc 'description "Merge two substitutions. Returns #f if incompatible.")
  (doc 'export #t)
  (let loop ([s1 subst1] [result subst2])
    (if (null? s1)
        result
        (let* ([binding (car s1)]
               [var (car binding)]
               [class-id (cdr binding)]
               [new-result (subst-try-extend result var class-id)])
          (if new-result
              (loop (cdr s1) new-result)
              #f)))))

;;; ============================================================
;;; Pattern Matching
;;; ============================================================

(doc 'section 'matching)

;;; ematch-pattern : EGraph × Pattern × EClassId × Substitution → (List Substitution)
;;; Match a pattern against an e-class, returning all valid substitutions.
;;;
;;; The pattern can be:
;;;   - A pattern variable (?x) - matches any e-class
;;;   - A symbol/number - matches if the e-class contains that leaf
;;;   - A list (op args...) - matches if e-class contains (op child-classes...)
;;;     where each arg pattern matches the corresponding child class

(define (ematch-pattern eg pattern class-id subst)
  (doc 'type (-> EGraph Pattern EClassId Substitution (List Substitution)))
  (doc 'description "Match pattern against e-class, return all valid substitutions.")
  (doc 'export #t)
  (let ([root (egraph-find eg class-id)])
    (cond
      ;; Pattern variable - bind to this class
      [(pattern-var? pattern)
       (let ([new-subst (subst-try-extend subst pattern root)])
         (if new-subst (list new-subst) '()))]

      ;; Leaf pattern (symbol or number) - check if class contains it
      [(or (symbol? pattern) (number? pattern))
       (let ([nodes (egraph-class-nodes eg root)])
         (if (exists (lambda (node)
                       (and (eqv? (enode-op node) pattern)
                            (zero? (enode-arity node))))
                     nodes)
             (list subst)
             '()))]

      ;; Application pattern (op args...)
      [(pair? pattern)
       (let ([op (car pattern)]
             [arg-patterns (cdr pattern)]
             [nodes (egraph-class-nodes eg root)])
         ;; Find all nodes with matching operator and arity
         (let ([matching-nodes
                (filter (lambda (node)
                          (and (eqv? (enode-op node) op)
                               (= (enode-arity node) (length arg-patterns))))
                        nodes)])
           ;; For each matching node, try to match children
           (apply append
                  (map (lambda (node)
                         (ematch-children eg arg-patterns
                                         (vector->list (enode-children node))
                                         subst))
                       matching-nodes))))]

      ;; Unknown pattern type
      [else '()])))

;;; ematch-children : EGraph × (List Pattern) × (List EClassId) × Subst → (List Subst)
;;; Match a list of patterns against a list of e-class IDs.
(define (ematch-children eg patterns child-ids subst)
  (doc 'type (-> EGraph (List Pattern) (List EClassId) Substitution (List Substitution)))
  (doc 'description "Match patterns against child e-classes.")
  (if (null? patterns)
      (list subst)  ; All patterns matched
      (let* ([pattern (car patterns)]
             [child-id (car child-ids)]
             ;; Get all substitutions from matching first pattern
             [first-substs (ematch-pattern eg pattern child-id subst)])
        ;; For each successful first match, try to match rest
        (apply append
               (map (lambda (s)
                      (ematch-children eg (cdr patterns) (cdr child-ids) s))
                    first-substs)))))

;;; ============================================================
;;; High-Level Matching API
;;; ============================================================

(doc 'section 'api)

;;; ematch : EGraph × Pattern × EClassId → (List Substitution)
;;; Match a pattern against an e-class starting from empty substitution.
(define (ematch eg pattern class-id)
  (doc 'type (-> EGraph Pattern EClassId (List Substitution)))
  (doc 'description "Match pattern against e-class, return all substitutions.")
  (doc 'export #t)
  (ematch-pattern eg pattern class-id (empty-subst)))

;;; ematch-all : EGraph × Pattern → (List (Pair EClassId Substitution))
;;; Match a pattern against ALL e-classes in the e-graph.
;;; Returns list of (class-id . substitution) pairs.
(define (ematch-all eg pattern)
  (doc 'type (-> EGraph Pattern (List (Pair EClassId Substitution))))
  (doc 'description "Match pattern against all e-classes in e-graph.")
  (doc 'export #t)
  (let ([uf (egraph-uf eg)])
    (apply append
           (map (lambda (root)
                  (map (lambda (subst) (cons root subst))
                       (ematch eg pattern root)))
                (uf-roots uf)))))

;;; ============================================================
;;; Pattern Application (for rewriting)
;;; ============================================================

(doc 'section 'application)

;;; E-class reference wrapper (to distinguish from literal numbers)
(define eclass-ref-tag 'eclass-ref)
(define (make-eclass-ref id) (vector eclass-ref-tag id))
(define (eclass-ref? x)
  (and (vector? x)
       (= (vector-length x) 2)
       (eq? (vector-ref x 0) eclass-ref-tag)))
(define (eclass-ref-id x) (vector-ref x 1))

;;; pattern-apply : Substitution × Pattern → Term
;;; Apply a substitution to a pattern, yielding a term.
;;; Pattern variables are replaced with wrapped e-class references.
(define (pattern-apply subst pattern)
  (doc 'type (-> Substitution Pattern Term))
  (doc 'description "Apply substitution to pattern, yielding a term with e-class refs.")
  (doc 'export #t)
  (cond
    [(pattern-var? pattern)
     (let ([binding (subst-lookup subst pattern)])
       (if binding
           (make-eclass-ref binding)  ; Wrap the e-class ID
           (error 'pattern-apply "Unbound pattern variable" pattern)))]
    [(pair? pattern)
     (cons (car pattern)
           (map (lambda (p) (pattern-apply subst p))
                (cdr pattern)))]
    [else pattern]))

;;; ============================================================
;;; Rewrite Rules
;;; ============================================================

(doc 'section 'rules)

;;; A rewrite rule is a pair (lhs . rhs) where both are patterns.
;;; When lhs matches, we instantiate rhs with the substitution and add it.

(define (make-rule lhs rhs)
  (doc 'type (-> Pattern Pattern Rule))
  (doc 'description "Create a rewrite rule: lhs => rhs")
  (doc 'export #t)
  (cons lhs rhs))

(define (rule-lhs rule) (car rule))
(define (rule-rhs rule) (cdr rule))

(define (rule? x)
  (doc 'type (-> Any Boolean))
  (doc 'description "Check if x is a rewrite rule.")
  (doc 'export #t)
  (and (pair? x)
       (not (null? (car x)))
       (not (null? (cdr x)))))

;;; apply-rule : EGraph × Rule × EClassId → Boolean
;;; Apply a rule to an e-class. Returns #t if any matches found.
;;; For each match, instantiates RHS and merges with the matched class.
(define (apply-rule eg rule class-id)
  (doc 'type (-> EGraph Rule EClassId Boolean))
  (doc 'description "Apply rule to e-class. Returns #t if matched.")
  (doc 'export #t)
  (let* ([lhs (rule-lhs rule)]
         [rhs (rule-rhs rule)]
         [matches (ematch eg lhs class-id)])
    (if (null? matches)
        #f
        (begin
          (for-each
           (lambda (subst)
             ;; Instantiate RHS with substitution
             (let ([rhs-term (pattern-apply subst rhs)])
               ;; Add to e-graph and merge with matched class
               (let ([rhs-id (add-instantiated-term eg rhs-term)])
                 (egraph-merge! eg class-id rhs-id))))
           matches)
          #t))))

;;; add-instantiated-term : EGraph × InstantiatedTerm → EClassId
;;; Add a term where leaves may be e-class refs (from substitution).
(define (add-instantiated-term eg term)
  (doc 'type (-> EGraph InstantiatedTerm EClassId))
  (doc 'description "Add term with e-class ref leaves to e-graph.")
  (cond
    ;; E-class reference (from pattern variable)
    [(eclass-ref? term)
     (eclass-ref-id term)]
    ;; Symbol leaf
    [(symbol? term)
     (egraph-add-enode! eg (make-enode term (vector)))]
    ;; Number literal (add as e-node, not e-class ID!)
    [(number? term)
     (egraph-add-enode! eg (make-enode term (vector)))]
    ;; Application
    [(pair? term)
     (let* ([op (car term)]
            [args (cdr term)]
            [child-ids (map (lambda (t) (add-instantiated-term eg t)) args)]
            [enode (make-enode op (list->vector child-ids))])
       (egraph-add-enode! eg enode))]
    [else
     (error 'add-instantiated-term "Unknown term type" term)]))

;;; apply-rules : EGraph × (List Rule) → Nat
;;; Apply all rules to all e-classes once. Returns number of matches.
(define (apply-rules eg rules)
  (doc 'type (-> EGraph (List Rule) Nat))
  (doc 'description "Apply all rules to all e-classes once. Returns match count.")
  (doc 'export #t)
  (let ([uf (egraph-uf eg)]
        [count 0])
    (for-each
     (lambda (root)
       (for-each
        (lambda (rule)
          (when (apply-rule eg rule root)
            (set! count (+ count 1))))
        rules))
     (uf-roots uf))
    count))

;;; ============================================================
;;; Debugging
;;; ============================================================

(doc 'section 'debug)

(define (subst->string subst)
  (doc 'type (-> Substitution String))
  (doc 'description "Convert substitution to string for debugging.")
  (doc 'export #t)
  (if (null? subst)
      "{}"
      (string-append
       "{"
       (apply string-append
              (map (lambda (binding)
                     (format "~a→e~a " (car binding) (cdr binding)))
                   subst))
       "}")))

(define (pattern->string pattern)
  (doc 'type (-> Pattern String))
  (doc 'description "Convert pattern to string for debugging.")
  (doc 'export #t)
  (cond
    [(pattern-var? pattern) (symbol->string pattern)]
    [(symbol? pattern) (symbol->string pattern)]
    [(number? pattern) (number->string pattern)]
    [(pair? pattern)
     (string-append "("
                    (symbol->string (car pattern))
                    " "
                    (apply string-append
                           (map (lambda (p)
                                  (string-append (pattern->string p) " "))
                                (cdr pattern)))
                    ")")]
    [else "?"]))

