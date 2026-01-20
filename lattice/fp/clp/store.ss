(load "core/base/prelude.ss")
(load "lattice/fp/meta/logic.ss")
(load "lattice/fp/clp/domain.ss")

(doc 'module 'store)
(doc 'description "Constraint Store for CLP(FD)")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'note "Extends the miniKanren substitution with finite domains and constraint tracking. May need fuel for propagation.")
(doc 'exports "make-cstore, cstore?, cstore accessors/setters, domain operations, binding operations, constraint management, reification")
(doc 'dependencies "prelude.ss, logic.ss, domain.ss")

(doc 'section 'constraint-store-structure)
(doc 'note "CStore = (cstore subst domains constraints pending)")
(doc 'note "subst: Substitution from logic.ss (variable -> value bindings)")
(doc 'note "domains: Alist of (lvar-id . domain) for FD variables")
(doc 'note "constraints: List of active constraints")
(doc 'note "pending: List of lvar-ids needing propagation")

(define (make-cstore)
  (doc 'export #t)
  (doc 'type '(-> CStore))
  (doc 'description "Create an empty constraint store")
  (list 'cstore empty-subst '() '() '()))

(define (cstore? x)
  (doc 'export #t)
  (doc 'type '(-> α Bool))
  (doc 'description "Check if value is a constraint store")
  (and (pair? x) (eq? (car x) 'cstore) (= (length x) 5)))

(define (cstore-subst cs)
  (doc 'export #t)
  (doc 'type '(-> CStore Substitution))
  (doc 'description "Get substitution from store")
  (list-ref cs 1))

(define (cstore-domains cs)
  (doc 'export #t)
  (doc 'type '(-> CStore (Alist LVarId Domain)))
  (doc 'description "Get domain bindings from store")
  (list-ref cs 2))

(define (cstore-constraints cs)
  (doc 'type '(-> CStore (List Constraint)))
  (doc 'description "Get active constraints from store")
  (list-ref cs 3))

(define (cstore-pending cs)
  (doc 'type '(-> CStore (List LVarId)))
  (doc 'description "Get pending propagation queue from store")
  (list-ref cs 4))

(define (cstore-set-subst cs subst)
  (doc 'type '(-> CStore Substitution CStore))
  (doc 'description "Set substitution in store")
  (list 'cstore subst
        (cstore-domains cs)
        (cstore-constraints cs)
        (cstore-pending cs)))

(define (cstore-set-domains cs domains)
  (doc 'type '(-> CStore (Alist LVarId Domain) CStore))
  (doc 'description "Set domain bindings in store")
  (list 'cstore (cstore-subst cs)
        domains
        (cstore-constraints cs)
        (cstore-pending cs)))

(define (cstore-set-constraints cs constraints)
  (doc 'type '(-> CStore (List Constraint) CStore))
  (doc 'description "Set active constraints in store")
  (list 'cstore (cstore-subst cs)
        (cstore-domains cs)
        constraints
        (cstore-pending cs)))

(define (cstore-set-pending cs pending)
  (doc 'type '(-> CStore (List LVarId) CStore))
  (doc 'description "Set pending propagation queue in store")
  (list 'cstore (cstore-subst cs)
        (cstore-domains cs)
        (cstore-constraints cs)
        pending))

(doc 'section 'domain-operations-on-store)

(define (cstore-get-domain cs var)
  (doc 'export #t)
  (doc 'type '(-> CStore LVar Domain))
  (doc 'description "Get the domain of a variable")
  (doc 'note "Returns #f for unconstrained (meaning no finite domain)")
  (let ([id (lvar-id var)]
        [domains (cstore-domains cs)])
       (let ([binding (assoc id domains)])
            (if binding
                (cdr binding)
                #f))))  ; #f means unconstrained

(define (cstore-set-domain cs var dom)
  (doc 'export #t)
  (doc 'type '(-> CStore LVar Domain (Maybe CStore)))
  (doc 'description "Set the domain of a variable")
  (doc 'returns "#f if domain is empty, otherwise updated store")
  (doc 'note "Only adds variable to pending queue if domain actually changed")
  (if (domain-empty? dom)
      #f  ; Failure - empty domain
      (let* ([id (lvar-id var)]
             [domains (cstore-domains cs)]
             [old-dom (let ([binding (assoc id domains)])
                           (if binding (cdr binding) #f))]
             ;; Only update pending if domain changed
             [changed? (or (not old-dom) (not (domain=? old-dom dom)))]
             [new-domains (if changed?
                              (alist-update id dom domains)
                              domains)]
             [pending (cstore-pending cs)]
             [new-pending (if (and changed? (not (member id pending)))
                              (cons id pending)
                              pending)])
            (list 'cstore
                  (cstore-subst cs)
                  new-domains
                  (cstore-constraints cs)
                  new-pending))))

(define (alist-update key val alist)
  (doc 'type '(-> Key Value Alist Alist))
  (doc 'description "Update or insert key-value pair in alist")
  (cons (cons key val)
        (alist-remove key alist)))

(define (alist-remove key alist)
  (doc 'type '(-> Key Alist Alist))
  (doc 'description "Remove all entries with given key")
  (filter (lambda (pair) (not (equal? (car pair) key))) alist))

(define (cstore-narrow-domain cs var new-dom)
  (doc 'export #t)
  (doc 'type '(-> CStore LVar Domain (Maybe CStore)))
  (doc 'description "Intersect variable's domain with given domain")
  (doc 'returns "#f if intersection is empty")
  (let ([current (cstore-get-domain cs var)])
       (if current
           (cstore-set-domain cs var (domain-intersect current new-dom))
           (cstore-set-domain cs var new-dom))))

(define (cstore-remove-value cs var val)
  (doc 'type '(-> CStore LVar Int (Maybe CStore)))
  (doc 'description "Remove a single value from variable's domain")
  (let ([current (cstore-get-domain cs var)])
       (if current
           (cstore-set-domain cs var (domain-subtract-value current val))
           cs)))  ; No domain = unconstrained, nothing to remove

(doc 'section 'binding-operations)

(define (cstore-bind-var cs var val)
  (doc 'export #t)
  (doc 'type '(-> CStore LVar Int (Maybe CStore)))
  (doc 'description "Bind a variable to a specific integer value")
  (doc 'note "Checks domain constraint and updates substitution")
  (let ([dom (cstore-get-domain cs var)])
       ;; Check domain constraint if one exists
       (if (and dom (not (domain-contains? dom val)))
           #f  ; Value not in domain - failure
           ;; Extend substitution and set domain to singleton
           (let* ([subst (extend-subst var val (cstore-subst cs))]
                  [cs1 (cstore-set-subst cs subst)])
                 (if dom
                     ;; Had a domain - set to singleton and propagate
                     (cstore-set-domain cs1 var (domain-singleton val))
                     ;; No domain - just bind
                     cs1)))))

(define (cstore-unify cs u v)
  (doc 'export #t)
  (doc 'type '(-> CStore α α (Maybe CStore)))
  (doc 'description "Unify two terms, respecting domain constraints")
  (doc 'returns "Updated store or #f on failure")
  (let* ([subst (cstore-subst cs)]
         [u (walk u subst)]
         [v (walk v subst)])
        (cond
         ;; Both are the same variable
         [(and (lvar? u) (lvar? v) (lvar=? u v))
          cs]
         
         ;; u is a variable
         [(lvar? u)
          (cstore-bind-term cs u v)]
         
         ;; v is a variable
         [(lvar? v)
          (cstore-bind-term cs v u)]
         
         ;; Both are pairs - unify recursively
         [(and (pair? u) (pair? v))
          (let ([cs1 (cstore-unify cs (car u) (car v))])
               (and cs1 (cstore-unify cs1 (cdr u) (cdr v))))]
         
         ;; Both are equal atoms
         [(equal? u v) cs]
         
         ;; Cannot unify
         [else #f])))

(define (cstore-bind-term cs var term)
  (doc 'type '(-> CStore LVar Term (Maybe CStore)))
  (doc 'description "Bind a variable to a term (integer, variable, or structure)")
  (cond
   ;; Binding to an integer - check domain
   [(integer? term)
    (cstore-bind-var cs var term)]
   
   ;; Binding to another variable - merge domains
   [(lvar? term)
    (let* ([dom1 (cstore-get-domain cs var)]
           [dom2 (cstore-get-domain cs term)])
          (cond
           ;; Both have domains - intersect and assign to both
           [(and dom1 dom2)
            (let ([merged (domain-intersect dom1 dom2)])
                 (if (domain-empty? merged)
                     #f
                     (let* ([subst (extend-subst var term (cstore-subst cs))]
                            [cs1 (cstore-set-subst cs subst)]
                            [cs2 (cstore-set-domain cs1 var merged)])
                           (and cs2 (cstore-set-domain cs2 term merged)))))]
           ;; Only var has domain - propagate to term
           [dom1
            (let* ([subst (extend-subst var term (cstore-subst cs))]
                   [cs1 (cstore-set-subst cs subst)])
                  (cstore-set-domain cs1 term dom1))]
           ;; Only term has domain - propagate to var
           [dom2
            (let* ([subst (extend-subst var term (cstore-subst cs))]
                   [cs1 (cstore-set-subst cs subst)])
                  (cstore-set-domain cs1 var dom2))]
           ;; Neither has domain - just unify
           [else
            (let ([subst (extend-subst var term (cstore-subst cs))])
                 (cstore-set-subst cs subst))]))]
   
   ;; Binding to a structure - occurs check then bind
   [else
    (if (occurs? var term (cstore-subst cs))
        #f  ; Occurs check failure
        (let ([subst (extend-subst var term (cstore-subst cs))])
             (cstore-set-subst cs subst)))]))

(define (occurs? var term subst)
  (doc 'type '(-> LVar Term Substitution Bool))
  (doc 'description "Check if variable occurs in term (for occurs check)")
  (let ([term (walk term subst)])
       (cond
        [(lvar? term) (lvar=? var term)]
        [(pair? term) (or (occurs? var (car term) subst)
                          (occurs? var (cdr term) subst))]
        [else #f])))

(doc 'section 'constraint-management)
(doc 'note "Constraint = (constraint id type vars propagator)")
(doc 'note "id: Unique identifier")
(doc 'note "type: Symbol naming the constraint type")
(doc 'note "vars: List of LVars involved")
(doc 'note "propagator: (CStore → (Maybe CStore))")

(define *constraint-counter* 0)

(define (make-constraint type vars propagator)
  (doc 'type '(-> Symbol (List LVar) (-> CStore (Maybe CStore)) Constraint))
  (doc 'description "Create a new constraint with unique ID")
  (set! *constraint-counter* (+ *constraint-counter* 1))
  (list 'constraint *constraint-counter* type vars propagator))

(define (constraint? x)
  (doc 'type '(-> α Bool))
  (doc 'description "Check if value is a constraint")
  (and (pair? x) (eq? (car x) 'constraint)))

(define (constraint-id c)
  (doc 'type '(-> Constraint Int))
  (doc 'description "Get constraint ID")
  (list-ref c 1))

(define (constraint-type c)
  (doc 'type '(-> Constraint Symbol))
  (doc 'description "Get constraint type")
  (list-ref c 2))

(define (constraint-vars c)
  (doc 'type '(-> Constraint (List LVar)))
  (doc 'description "Get variables involved in constraint")
  (list-ref c 3))

(define (constraint-propagator c)
  (doc 'type '(-> Constraint (-> CStore (Maybe CStore))))
  (doc 'description "Get constraint propagator function")
  (list-ref c 4))

(define (cstore-add-constraint cs constraint)
  (doc 'export #t)
  (doc 'type '(-> CStore Constraint CStore))
  (doc 'description "Add a constraint to the store")
  (let ([constraints (cstore-constraints cs)])
       (cstore-set-constraints cs (cons constraint constraints))))

(define (cstore-constraints-for-var cs var)
  (doc 'export #t)
  (doc 'type '(-> CStore LVar (List Constraint)))
  (doc 'description "Get all constraints involving a variable")
  (let ([id (lvar-id var)])
       (filter
        (lambda (c)
                (any (lambda (v) (= (lvar-id v) id))
                     (constraint-vars c)))
        (cstore-constraints cs))))

(define (any pred lst)
  (doc 'type '(-> (-> α Bool) (List α) Bool))
  (doc 'description "Check if predicate is true for any element")
  (cond
   [(null? lst) #f]
   [(pred (car lst)) #t]
   [else (any pred (cdr lst))]))

(doc 'section 'propagation-queue)

(define (cstore-add-pending cs var)
  (doc 'type '(-> CStore LVar CStore))
  (doc 'description "Add variable to propagation queue")
  (let* ([id (lvar-id var)]
         [pending (cstore-pending cs)])
        (if (member id pending)
            cs
            (cstore-set-pending cs (cons id pending)))))

(define (cstore-pop-pending cs)
  (doc 'type '(-> CStore (Maybe (Pair LVarId CStore))))
  (doc 'description "Pop a variable from propagation queue")
  (let ([pending (cstore-pending cs)])
       (if (null? pending)
           #f
           (cons (car pending)
                 (cstore-set-pending cs (cdr pending))))))

(define (cstore-clear-pending cs)
  (doc 'type '(-> CStore CStore))
  (doc 'description "Clear the propagation queue")
  (cstore-set-pending cs '()))

(doc 'section 'ground-singleton-detection)

(define (cstore-ground? cs var)
  (doc 'type '(-> CStore LVar Bool))
  (doc 'description "Check if variable is bound to a ground (non-variable) value")
  (let ([val (walk var (cstore-subst cs))])
       (not (lvar? val))))

(define (cstore-singleton? cs var)
  (doc 'type '(-> CStore LVar Bool))
  (doc 'description "Check if variable has a singleton domain")
  (let ([dom (cstore-get-domain cs var)])
       (and dom (domain-singleton? dom))))

(define (cstore-get-value cs var)
  (doc 'export #t)
  (doc 'type '(-> CStore LVar (Maybe Int)))
  (doc 'description "Get the value of a ground variable")
  (let ([val (walk var (cstore-subst cs))])
       (if (integer? val)
           val
           (let ([dom (cstore-get-domain cs var)])
                (and dom (domain-singleton? dom) (domain-min dom))))))

(doc 'section 'reification)

(define (cstore-walk cs term)
  (doc 'export #t)
  (doc 'type '(-> CStore Term Term))
  (doc 'description "Walk a term through the store's substitution")
  (walk term (cstore-subst cs)))

(define (cstore-walk* cs term)
  (doc 'type '(-> CStore Term Term))
  (doc 'description "Deep walk a term through the store's substitution")
  (walk* term (cstore-subst cs)))

(define (cstore-reify cs term)
  (doc 'type '(-> CStore Term Term))
  (doc 'description "Reify a term, replacing unbound variables with readable names")
  (let ([walked (cstore-walk* cs term)])
       (reify walked)))

(doc 'section 'store-display)

(define (cstore->string cs)
  (doc 'type '(-> CStore String))
  (doc 'description "Convert store to readable string for debugging")
  (string-append
   "(cstore\n"
   "  subst: " (format-subst (cstore-subst cs)) "\n"
   "  domains: " (format-domains (cstore-domains cs)) "\n"
   "  constraints: " (number->string (length (cstore-constraints cs))) " active\n"
   "  pending: " (format-pending (cstore-pending cs)) ")"))

;;; format-subst : Substitution → String
(define (format-subst subst)
  (if (null? subst)
      "{}"
      (string-append
       "{"
       (apply string-append
              (list-intersperse
               (map (lambda (binding)
                            (string-append
                             (lvar-name (car binding))
                             "="
                             (format-term (cdr binding))))
                    subst)
               ", "))
       "}")))

;;; format-term : Term → String
(define (format-term term)
  (cond
   [(lvar? term) (string-append "_" (number->string (lvar-id term)))]
   [(pair? term) (string-append "(" (format-term (car term))
                                " . " (format-term (cdr term)) ")")]
   [(number? term) (number->string term)]
   [(symbol? term) (symbol->string term)]
   [else "?"]))

;;; format-domains : (Alist LVarId Domain) → String
(define (format-domains domains)
  (if (null? domains)
      "{}"
      (string-append
       "{"
       (apply string-append
              (list-intersperse
               (map (lambda (binding)
                            (string-append
                             "_" (number->string (car binding))
                             ":" (domain->string (cdr binding))))
                    domains)
               ", "))
       "}")))

;;; format-pending : (List LVarId) → String
(define (format-pending pending)
  (if (null? pending)
      "[]"
      (string-append
       "["
       (apply string-append
              (list-intersperse
               (map (lambda (id) (string-append "_" (number->string id)))
                    pending)
               ", "))
       "]")))
