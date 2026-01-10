;;; lattice/fp/clp/global-constraints.ss — Global Constraints for CLP(FD)
;;;
;;; Implements global constraints that involve multiple variables:
;;;   - all-different: All variables have distinct values
;;;   - element: Indexed array access
;;;   - sum-fd: Sum of variables equals a result
;;;   - count-fd: Count occurrences of a value
;;;
;;; These constraints have special propagation rules that are more
;;; efficient than decomposing into binary constraints.
;;;
;;; Dependencies:
;;;   - propagate.ss

(load "lattice/fp/clp/propagate.ss")

;;; ============================================================
;;; All-Different Constraint
;;; ============================================================

;;; all-different : (List LVar) → (CStore → (Maybe CStore))
;;; Constrain all variables to have different values.
;;; Uses a combination of:
;;;   1. Remove singleton values from other domains
;;;   2. Check for Hall set violations
(define (all-different vars)
  (lambda (cs)
          (all-different-propagate cs vars)))

;;; all-different-propagate : CStore × (List LVar) → (Maybe CStore)
;;; Propagate all-different constraint.
(define (all-different-propagate cs vars)
  ;; Step 1: Remove singleton values from other domains
  (let ([cs1 (remove-singletons cs vars)])
       (if (not cs1)
           #f
           ;; Step 2: Check for obvious failures (two singletons with same value)
           (if (check-singleton-conflict cs1 vars)
               #f
               cs1))))

;;; remove-singletons : CStore × (List LVar) → (Maybe CStore)
;;; For each singleton variable, remove its value from all other domains.
(define (remove-singletons cs vars)
  (let loop ([vars vars] [cs cs])
       (if (null? vars)
           cs
           (let* ([var (car vars)]
                  [dom (cstore-get-domain cs var)])
                 (if (and dom (domain-singleton? dom))
                     (let ([val (domain-min dom)]
                           [others (filter (lambda (v) (not (eq? v var))) vars)])
                          (let ([cs1 (remove-value-from-all cs val others)])
                               (if cs1
                                   (loop (cdr vars) cs1)
                                   #f)))
                     (loop (cdr vars) cs))))))

;;; remove-value-from-all : CStore × Int × (List LVar) → (Maybe CStore)
;;; Remove a value from all variables in list.
(define (remove-value-from-all cs val vars)
  (let loop ([vars vars] [cs cs])
       (if (null? vars)
           cs
           (let* ([var (car vars)]
                  [dom (cstore-get-domain cs var)])
                 (if dom
                     (let ([new-dom (domain-subtract-value dom val)])
                          (if (domain-empty? new-dom)
                              #f
                              (let ([cs1 (cstore-set-domain cs var new-dom)])
                                   (if cs1
                                       (loop (cdr vars) cs1)
                                       #f))))
                     (loop (cdr vars) cs))))))

;;; check-singleton-conflict : CStore × (List LVar) → Bool
;;; Check if any two singletons have the same value.
(define (check-singleton-conflict cs vars)
  (let* ([singleton-vals
          (filter-map
           (lambda (var)
                   (let ([dom (cstore-get-domain cs var)])
                        (if (and dom (domain-singleton? dom))
                            (domain-min dom)
                            #f)))
           vars)]
         [unique-vals (list-uniq singleton-vals)])
        (not (= (length singleton-vals) (length unique-vals)))))

;;; filter-map : (α → (Maybe β)) × (List α) → (List β)
;;; Map and filter in one pass.
(define (filter-map f lst)
  (let loop ([lst lst] [acc '()])
       (if (null? lst)
           (reverse acc)
           (let ([result (f (car lst))])
                (if result
                    (loop (cdr lst) (cons result acc))
                    (loop (cdr lst) acc))))))

;;; list-uniq : (List α) → (List α)
;;; Remove duplicates.
(define (list-uniq-global lst)
  (let loop ([lst lst] [seen '()] [acc '()])
       (cond
        [(null? lst) (reverse acc)]
        [(member (car lst) seen) (loop (cdr lst) seen acc)]
        [else (loop (cdr lst) (cons (car lst) seen) (cons (car lst) acc))])))

;;; post-all-different : CStore × (List LVar) → (Maybe CStore)
;;; Post all-different constraint and propagate.
(define (post-all-different cs vars)
  (if (< (length vars) 2)
      cs  ; Trivially satisfied
      (post-constraint cs 'all-different vars (all-different vars))))

;;; ============================================================
;;; Element Constraint
;;; ============================================================

;;; element : LVar × (List Int) × LVar → (CStore → (Maybe CStore))
;;; Constrain: array[index] = value
;;; index is 1-based (like SICStus/ECLiPSe convention).
(define (element index array value)
  (lambda (cs)
          (element-propagate cs index array value)))

;;; element-propagate : CStore × LVar × (List Int) × LVar → (Maybe CStore)
(define (element-propagate cs index array value)
  (let* ([n (length array)]
         [idx-dom (cstore-get-domain cs index)]
         [val-dom (cstore-get-domain cs value)])
        ;; Restrict index to valid range [1, n]
        (let ([cs1 (if idx-dom
                       (cstore-narrow-domain cs index (make-domain 1 n))
                       (cstore-set-domain cs index (make-domain 1 n)))])
             (if (not cs1)
                 #f
                 (let ([idx-dom (cstore-get-domain cs1 index)])
                      ;; Collect possible values based on index domain
                      (let* ([possible-vals
                              (filter-map
                               (lambda (i)
                                       (if (domain-contains? idx-dom i)
                                           (list-ref array (- i 1))
                                           #f))
                               (range 1 (+ n 1)))]
                             [val-constraint (domain-from-list possible-vals)])
                            ;; Narrow value domain
                            (let ([cs2 (cstore-narrow-domain cs1 value val-constraint)])
                                 (if (not cs2)
                                     #f
                                     ;; Narrow index based on value domain
                                     (let ([val-dom (cstore-get-domain cs2 value)])
                                          (if val-dom
                                              (let* ([valid-indices
                                                      (filter
                                                       (lambda (i)
                                                               (domain-contains? val-dom (list-ref array (- i 1))))
                                                       (domain->list idx-dom))]
                                                     [idx-constraint (domain-from-list valid-indices)])
                                                    (cstore-narrow-domain cs2 index idx-constraint))
                                              cs2))))))))))

;;; post-element : CStore × LVar × (List Int) × LVar → (Maybe CStore)
(define (post-element cs index array value)
  (post-constraint cs 'element (list index value) (element index array value)))

;;; ============================================================
;;; Sum Constraint
;;; ============================================================

;;; sum-fd : (List LVar) × (LVar | Int) → (CStore → (Maybe CStore))
;;; Constrain: sum(vars) = result
(define (sum-fd vars result)
  (lambda (cs)
          (sum-fd-propagate cs vars result)))

;;; sum-fd-propagate : CStore × (List LVar) × (LVar | Int) → (Maybe CStore)
;;; Bounds propagation for sum constraint.
(define (sum-fd-propagate cs vars result)
  (let* ([bounds (map (lambda (v) (fd-bounds cs v)) vars)]
         [result-bounds (fd-bounds cs result)])
        ;; Check all vars have domains
        (if (any not bounds)
            cs
            (let* ([sum-lo (apply + (map car bounds))]
                   [sum-hi (apply + (map cdr bounds))])
                  ;; If result has no bounds, create them from sum
                  (if (not result-bounds)
                      (if (lvar? result)
                          (cstore-set-domain cs result (make-domain sum-lo sum-hi))
                          (if (and (>= result sum-lo) (<= result sum-hi))
                              cs  ; constant result in range
                              #f)) ; constant result out of range
                      (let* ([res-lo (car result-bounds)]
                             [res-hi (cdr result-bounds)]
                             ;; Narrow result
                             [new-res-lo (max res-lo sum-lo)]
                             [new-res-hi (min res-hi sum-hi)])
                            (if (> new-res-lo new-res-hi)
                                #f
                                (let ([cs1 (fd-narrow-bounds cs result new-res-lo new-res-hi)])
                                     (and cs1
                                          ;; Narrow each variable
                                          (sum-fd-narrow-vars cs1 vars result))))))))))

;;; sum-fd-narrow-vars : CStore × (List LVar) × (LVar | Int) → (Maybe CStore)
;;; Narrow each variable based on sum constraint.
(define (sum-fd-narrow-vars cs vars result)
  (let ([result-bounds (fd-bounds cs result)])
       (if (not result-bounds)
           cs
           (let ([res-lo (car result-bounds)]
                 [res-hi (cdr result-bounds)]
                 [all-vars vars])  ; Capture original list for computing "others"
                (let loop ([remaining vars] [cs cs])
                     (if (null? remaining)
                         cs
                         (let* ([var (car remaining)]
                                ;; Use all-vars, not remaining, to get other summands
                                [others (filter (lambda (v) (not (eq? v var))) all-vars)]
                                [var-bounds (fd-bounds cs var)]
                                [other-bounds (map (lambda (v) (fd-bounds cs v)) others)])
                               (if (or (not var-bounds) (any not other-bounds))
                                   (loop (cdr remaining) cs)
                                   (let* ([others-lo (apply + (map car other-bounds))]
                                          [others-hi (apply + (map cdr other-bounds))]
                                          ;; var = result - sum(others)
                                          ;; var-lo >= res-lo - others-hi
                                          ;; var-hi <= res-hi - others-lo
                                          [new-lo (max (car var-bounds) (- res-lo others-hi))]
                                          [new-hi (min (cdr var-bounds) (- res-hi others-lo))])
                                         (if (> new-lo new-hi)
                                             #f
                                             (let ([cs1 (fd-narrow-bounds cs var new-lo new-hi)])
                                                  (if cs1
                                                      (loop (cdr remaining) cs1)
                                                      #f))))))))))))

;;; post-sum-fd : CStore × (List LVar) × (LVar | Int) → (Maybe CStore)
(define (post-sum-fd cs vars result)
  (let ([all-vars (if (lvar? result) (cons result vars) vars)])
       (post-constraint cs 'sum-fd all-vars (sum-fd vars result))))

;;; ============================================================
;;; Count Constraint
;;; ============================================================

;;; count-fd : Int × (List LVar) × (LVar | Int) → (CStore → (Maybe CStore))
;;; Constrain: count of value in vars = n
(define (count-fd value vars n)
  (lambda (cs)
          (count-fd-propagate cs value vars n)))

;;; count-fd-propagate : CStore × Int × (List LVar) × (LVar | Int) → (Maybe CStore)
(define (count-fd-propagate cs value vars n)
  (let* ([must-count (count-must-equal cs value vars)]
         [can-count (count-can-equal cs value vars)]
         [n-bounds (fd-bounds cs n)])
        (if (not n-bounds)
            cs
            (let ([n-lo (car n-bounds)]
                  [n-hi (cdr n-bounds)])
                 ;; n must be between must-count and can-count
                 (let ([new-lo (max n-lo must-count)]
                       [new-hi (min n-hi can-count)])
                      (if (> new-lo new-hi)
                          #f
                          (let ([cs1 (fd-narrow-bounds cs n new-lo new-hi)])
                               ;; If n = can-count, all "can" vars must have value
                               ;; If n = must-count, all non-"must" vars cannot have value
                               (and cs1
                                    (count-fd-fix cs1 value vars must-count can-count new-lo new-hi)))))))))

;;; count-must-equal : CStore × Int × (List LVar) → Nat
;;; Count variables that must equal value (singleton with that value).
(define (count-must-equal cs value vars)
  (length (filter (lambda (var)
                          (let ([dom (cstore-get-domain cs var)])
                               (and dom
                                    (domain-singleton? dom)
                                    (= (domain-min dom) value))))
                  vars)))

;;; count-can-equal : CStore × Int × (List LVar) → Nat
;;; Count variables that can equal value (value in domain).
(define (count-can-equal cs value vars)
  (length (filter (lambda (var)
                          (let ([dom (cstore-get-domain cs var)])
                               (or (not dom)  ; Unconstrained = can equal anything
                                   (domain-contains? dom value))))
                  vars)))

;;; count-fd-fix : CStore × Int × (List LVar) × Nat × Nat × Nat × Nat → (Maybe CStore)
;;; Apply deductions when count bounds are tight.
(define (count-fd-fix cs value vars must can lo hi)
  (cond
   ;; If we must have exactly can occurrences, fix all "can" vars
   [(= lo can)
    (fix-all-can-vars cs value vars)]
   ;; If we already have must = hi, remove value from all "can but not must" vars
   [(= hi must)
    (remove-value-from-can-vars cs value vars)]
   [else cs]))

;;; fix-all-can-vars : CStore × Int × (List LVar) → (Maybe CStore)
;;; Set all vars that can equal value to that value.
(define (fix-all-can-vars cs value vars)
  (let loop ([vars vars] [cs cs])
       (if (null? vars)
           cs
           (let* ([var (car vars)]
                  [dom (cstore-get-domain cs var)])
                 (if (or (not dom) (domain-contains? dom value))
                     (if (and dom (domain-singleton? dom))
                         (loop (cdr vars) cs)  ; Already fixed
                         (let ([cs1 (cstore-set-domain cs var (domain-singleton value))])
                              (if cs1
                                  (loop (cdr vars) cs1)
                                  #f)))
                     (loop (cdr vars) cs))))))

;;; remove-value-from-can-vars : CStore × Int × (List LVar) → (Maybe CStore)
;;; Remove value from all vars that aren't already singleton with that value.
(define (remove-value-from-can-vars cs value vars)
  (let loop ([vars vars] [cs cs])
       (if (null? vars)
           cs
           (let* ([var (car vars)]
                  [dom (cstore-get-domain cs var)])
                 (if (and dom (domain-singleton? dom) (= (domain-min dom) value))
                     (loop (cdr vars) cs)  ; This is a "must" var
                     (let ([cs1 (if dom
                                    (cstore-set-domain cs var (domain-subtract-value dom value))
                                    cs)])
                          (if cs1
                              (loop (cdr vars) cs1)
                              #f)))))))

;;; post-count-fd : CStore × Int × (List LVar) × (LVar | Int) → (Maybe CStore)
(define (post-count-fd cs value vars n)
  (let ([all-vars (if (lvar? n) (cons n vars) vars)])
       (post-constraint cs 'count-fd all-vars (count-fd value vars n))))
