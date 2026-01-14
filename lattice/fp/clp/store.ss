;;; lattice/fp/clp/store.ss — Constraint Store for CLP(FD)
;;;
;;; Extends the miniKanren substitution with finite domains and
;;; constraint tracking. The constraint store maintains:
;;;   - Substitution (from logic.ss)
;;;   - Domain bindings for logic variables
;;;   - Active constraints
;;;   - Pending propagation queue
;;;
;;; This is Lattice code: pure operations, may need fuel for propagation.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - logic.ss
;;;   - domain.ss

(load "core/base/prelude.ss")
(load "lattice/fp/meta/logic.ss")
(load "lattice/fp/clp/domain.ss")

;;; ====
;;; Constraint Store Structure
;;; ====
;;;
;;; CStore = (cstore subst domains constraints pending)
;;;   subst       : Substitution from logic.ss (variable -> value bindings)
;;;   domains     : Alist of (lvar-id . domain) for FD variables
;;;   constraints : List of active constraints
;;;   pending     : List of lvar-ids needing propagation

;;; make-cstore : → CStore
;;; Create an empty constraint store.
(define (make-cstore)
  (list 'cstore empty-subst '() '() '()))

;;; cstore? : α → Bool
;;; Check if value is a constraint store.
(define (cstore? x)
  (and (pair? x) (eq? (car x) 'cstore) (= (length x) 5)))

;;; cstore-subst : CStore → Substitution
(define (cstore-subst cs)
  (list-ref cs 1))

;;; cstore-domains : CStore → (Alist LVarId Domain)
(define (cstore-domains cs)
  (list-ref cs 2))

;;; cstore-constraints : CStore → (List Constraint)
(define (cstore-constraints cs)
  (list-ref cs 3))

;;; cstore-pending : CStore → (List LVarId)
(define (cstore-pending cs)
  (list-ref cs 4))

;;; cstore-set-subst : CStore × Substitution → CStore
(define (cstore-set-subst cs subst)
  (list 'cstore subst
        (cstore-domains cs)
        (cstore-constraints cs)
        (cstore-pending cs)))

;;; cstore-set-domains : CStore × (Alist LVarId Domain) → CStore
(define (cstore-set-domains cs domains)
  (list 'cstore (cstore-subst cs)
        domains
        (cstore-constraints cs)
        (cstore-pending cs)))

;;; cstore-set-constraints : CStore × (List Constraint) → CStore
(define (cstore-set-constraints cs constraints)
  (list 'cstore (cstore-subst cs)
        (cstore-domains cs)
        constraints
        (cstore-pending cs)))

;;; cstore-set-pending : CStore × (List LVarId) → CStore
(define (cstore-set-pending cs pending)
  (list 'cstore (cstore-subst cs)
        (cstore-domains cs)
        (cstore-constraints cs)
        pending))

;;; ====
;;; Domain Operations on Store
;;; ====

;;; cstore-get-domain : CStore × LVar → Domain
;;; Get the domain of a variable. Returns unconstrained if not set.
;;; Unconstrained is represented as #f (meaning no finite domain).
(define (cstore-get-domain cs var)
  (let ([id (lvar-id var)]
        [domains (cstore-domains cs)])
       (let ([binding (assoc id domains)])
            (if binding
                (cdr binding)
                #f))))  ; #f means unconstrained

;;; cstore-set-domain : CStore × LVar × Domain → (Maybe CStore)
;;; Set the domain of a variable. Returns #f if domain is empty.
;;; Only adds variable to pending queue if domain actually changed.
(define (cstore-set-domain cs var dom)
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

;;; alist-update : Key × Value × Alist → Alist
;;; Update or insert key-value pair in alist.
(define (alist-update key val alist)
  (cons (cons key val)
        (alist-remove key alist)))

;;; alist-remove : Key × Alist → Alist
;;; Remove all entries with given key.
(define (alist-remove key alist)
  (filter (lambda (pair) (not (equal? (car pair) key))) alist))

;;; cstore-narrow-domain : CStore × LVar × Domain → (Maybe CStore)
;;; Intersect variable's domain with given domain.
;;; Returns #f if intersection is empty.
(define (cstore-narrow-domain cs var new-dom)
  (let ([current (cstore-get-domain cs var)])
       (if current
           (cstore-set-domain cs var (domain-intersect current new-dom))
           (cstore-set-domain cs var new-dom))))

;;; cstore-remove-value : CStore × LVar × Int → (Maybe CStore)
;;; Remove a single value from variable's domain.
(define (cstore-remove-value cs var val)
  (let ([current (cstore-get-domain cs var)])
       (if current
           (cstore-set-domain cs var (domain-subtract-value current val))
           cs)))  ; No domain = unconstrained, nothing to remove

;;; ====
;;; Binding Operations
;;; ====

;;; cstore-bind-var : CStore × LVar × Int → (Maybe CStore)
;;; Bind a variable to a specific integer value.
;;; Checks domain constraint and updates substitution.
(define (cstore-bind-var cs var val)
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

;;; cstore-unify : CStore × α × α → (Maybe CStore)
;;; Unify two terms, respecting domain constraints.
;;; Returns updated store or #f on failure.
(define (cstore-unify cs u v)
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

;;; cstore-bind-term : CStore × LVar × Term → (Maybe CStore)
;;; Bind a variable to a term (integer, variable, or structure).
(define (cstore-bind-term cs var term)
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

;;; occurs? : LVar × Term × Substitution → Bool
;;; Check if variable occurs in term (for occurs check).
(define (occurs? var term subst)
  (let ([term (walk term subst)])
       (cond
        [(lvar? term) (lvar=? var term)]
        [(pair? term) (or (occurs? var (car term) subst)
                          (occurs? var (cdr term) subst))]
        [else #f])))

;;; ====
;;; Constraint Management
;;; ====

;;; Constraint = (constraint id type vars propagator)
;;;   id         : Unique identifier
;;;   type       : Symbol naming the constraint type
;;;   vars       : List of LVars involved
;;;   propagator : (CStore → (Maybe CStore))

(define *constraint-counter* 0)

;;; make-constraint : Symbol × (List LVar) × (CStore → (Maybe CStore)) → Constraint
(define (make-constraint type vars propagator)
  (set! *constraint-counter* (+ *constraint-counter* 1))
  (list 'constraint *constraint-counter* type vars propagator))

;;; constraint? : α → Bool
(define (constraint? x)
  (and (pair? x) (eq? (car x) 'constraint)))

;;; constraint-id : Constraint → Int
(define (constraint-id c) (list-ref c 1))

;;; constraint-type : Constraint → Symbol
(define (constraint-type c) (list-ref c 2))

;;; constraint-vars : Constraint → (List LVar)
(define (constraint-vars c) (list-ref c 3))

;;; constraint-propagator : Constraint → (CStore → (Maybe CStore))
(define (constraint-propagator c) (list-ref c 4))

;;; cstore-add-constraint : CStore × Constraint → CStore
;;; Add a constraint to the store.
(define (cstore-add-constraint cs constraint)
  (let ([constraints (cstore-constraints cs)])
       (cstore-set-constraints cs (cons constraint constraints))))

;;; cstore-constraints-for-var : CStore × LVar → (List Constraint)
;;; Get all constraints involving a variable.
(define (cstore-constraints-for-var cs var)
  (let ([id (lvar-id var)])
       (filter
        (lambda (c)
                (any (lambda (v) (= (lvar-id v) id))
                     (constraint-vars c)))
        (cstore-constraints cs))))

;;; any : (α → Bool) × (List α) → Bool
;;; Check if predicate is true for any element.
(define (any pred lst)
  (cond
   [(null? lst) #f]
   [(pred (car lst)) #t]
   [else (any pred (cdr lst))]))

;;; ====
;;; Propagation Queue
;;; ====

;;; cstore-add-pending : CStore × LVar → CStore
;;; Add variable to propagation queue.
(define (cstore-add-pending cs var)
  (let* ([id (lvar-id var)]
         [pending (cstore-pending cs)])
        (if (member id pending)
            cs
            (cstore-set-pending cs (cons id pending)))))

;;; cstore-pop-pending : CStore → (Maybe (LVarId × CStore))
;;; Pop a variable from propagation queue.
(define (cstore-pop-pending cs)
  (let ([pending (cstore-pending cs)])
       (if (null? pending)
           #f
           (cons (car pending)
                 (cstore-set-pending cs (cdr pending))))))

;;; cstore-clear-pending : CStore → CStore
;;; Clear the propagation queue.
(define (cstore-clear-pending cs)
  (cstore-set-pending cs '()))

;;; ====
;;; Ground/Singleton Detection
;;; ====

;;; cstore-ground? : CStore × LVar → Bool
;;; Check if variable is bound to a ground (non-variable) value.
(define (cstore-ground? cs var)
  (let ([val (walk var (cstore-subst cs))])
       (not (lvar? val))))

;;; cstore-singleton? : CStore × LVar → Bool
;;; Check if variable has a singleton domain.
(define (cstore-singleton? cs var)
  (let ([dom (cstore-get-domain cs var)])
       (and dom (domain-singleton? dom))))

;;; cstore-get-value : CStore × LVar → (Maybe Int)
;;; Get the value of a ground variable.
(define (cstore-get-value cs var)
  (let ([val (walk var (cstore-subst cs))])
       (if (integer? val)
           val
           (let ([dom (cstore-get-domain cs var)])
                (and dom (domain-singleton? dom) (domain-min dom))))))

;;; ====
;;; Reification
;;; ====

;;; cstore-walk : CStore × Term → Term
;;; Walk a term through the store's substitution.
(define (cstore-walk cs term)
  (walk term (cstore-subst cs)))

;;; cstore-walk* : CStore × Term → Term
;;; Deep walk a term through the store's substitution.
(define (cstore-walk* cs term)
  (walk* term (cstore-subst cs)))

;;; cstore-reify : CStore × Term → Term
;;; Reify a term, replacing unbound variables with readable names.
(define (cstore-reify cs term)
  (let ([walked (cstore-walk* cs term)])
       (reify walked)))

;;; ====
;;; Store Display
;;; ====

;;; cstore->string : CStore → String
;;; Convert store to readable string for debugging.
(define (cstore->string cs)
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
