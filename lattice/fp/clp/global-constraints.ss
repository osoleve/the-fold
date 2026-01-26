(load "lattice/fp/clp/propagate.ss")

(doc 'module 'global-constraints)
(doc 'description "Global Constraints for CLP(FD)")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'note "Implements global constraints that involve multiple variables with special propagation rules more efficient than decomposing into binary constraints")
(doc 'exports "all-different, element, sum-fd, count-fd and their post- variants")
(doc 'dependencies "propagate.ss")

(doc 'section 'all-different-constraint)

(define (all-different vars)
  (doc 'export #t)
  (doc 'type '(-> (List LVar) (-> CStore (Maybe CStore))))
  (doc 'description "Constrain all variables to have different values")
  (doc 'note "Uses combination of: (1) Remove singleton values from other domains (2) Check for Hall set violations")
  (lambda (cs)
          (all-different-propagate cs vars)))

(define (all-different-propagate cs vars)
  (doc 'type '(-> CStore (List LVar) (Maybe CStore)))
  (doc 'description "Propagate all-different constraint")
  ;; Step 1: Remove singleton values from other domains
  (let ([cs1 (remove-singletons cs vars)])
       (if (not cs1)
           #f
           ;; Step 2: Check for obvious failures (two singletons with same value)
           (if (check-singleton-conflict cs1 vars)
               #f
               cs1))))

(define (remove-singletons cs vars)
  (doc 'type '(-> CStore (List LVar) (Maybe CStore)))
  (doc 'description "For each singleton variable, remove its value from all other domains")
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

(define (remove-value-from-all cs val vars)
  (doc 'type '(-> CStore Int (List LVar) (Maybe CStore)))
  (doc 'description "Remove a value from all variables in list")
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

(define (check-singleton-conflict cs vars)
  (doc 'type '(-> CStore (List LVar) Bool))
  (doc 'description "Check if any two singletons have the same value")
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

;;; filter-map provided by prelude

(define (list-uniq-global lst)
  (doc 'type '(-> (List α) (List α)))
  (doc 'description "Remove duplicates from list")
  (let loop ([lst lst] [seen '()] [acc '()])
       (cond
        [(null? lst) (reverse acc)]
        [(member (car lst) seen) (loop (cdr lst) seen acc)]
        [else (loop (cdr lst) (cons (car lst) seen) (cons (car lst) acc))])))

(define (post-all-different cs vars)
  (doc 'export #t)
  (doc 'type '(-> CStore (List LVar) (Maybe CStore)))
  (doc 'description "Post all-different constraint and propagate")
  (if (< (length vars) 2)
      cs  ; Trivially satisfied
      (post-constraint cs 'all-different vars (all-different vars))))

(doc 'section 'element-constraint)

(define (element index array value)
  (doc 'export #t)
  (doc 'type '(-> LVar (List Int) LVar (-> CStore (Maybe CStore))))
  (doc 'description "Constrain: array[index] = value")
  (doc 'note "index is 1-based (like SICStus/ECLiPSe convention)")
  (lambda (cs)
          (element-propagate cs index array value)))

(define (element-propagate cs index array value)
  (doc 'type '(-> CStore LVar (List Int) LVar (Maybe CStore)))
  (doc 'description "Propagate element constraint by narrowing index and value domains")
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

(define (post-element cs index array value)
  (doc 'export #t)
  (doc 'type '(-> CStore LVar (List Int) LVar (Maybe CStore)))
  (doc 'description "Post element constraint and register for re-propagation")
  (post-constraint cs 'element (list index value) (element index array value)))

(doc 'section 'sum-constraint)

(define (sum-fd vars result)
  (doc 'export #t)
  (doc 'type '(-> (List LVar) (Or LVar Int) (-> CStore (Maybe CStore))))
  (doc 'description "Constrain: sum(vars) = result")
  (lambda (cs)
          (sum-fd-propagate cs vars result)))

(define (sum-fd-propagate cs vars result)
  (doc 'type '(-> CStore (List LVar) (Or LVar Int) (Maybe CStore)))
  (doc 'description "Bounds propagation for sum constraint")
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

(define (sum-fd-narrow-vars cs vars result)
  (doc 'type '(-> CStore (List LVar) (Or LVar Int) (Maybe CStore)))
  (doc 'description "Narrow each variable based on sum constraint")
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

(define (post-sum-fd cs vars result)
  (doc 'export #t)
  (doc 'type '(-> CStore (List LVar) (Or LVar Int) (Maybe CStore)))
  (doc 'description "Post sum constraint and register for re-propagation")
  (let ([all-vars (if (lvar? result) (cons result vars) vars)])
       (post-constraint cs 'sum-fd all-vars (sum-fd vars result))))

(doc 'section 'count-constraint)

(define (count-fd value vars n)
  (doc 'export #t)
  (doc 'type '(-> Int (List LVar) (Or LVar Int) (-> CStore (Maybe CStore))))
  (doc 'description "Constrain: count of value in vars = n")
  (lambda (cs)
          (count-fd-propagate cs value vars n)))

(define (count-fd-propagate cs value vars n)
  (doc 'type '(-> CStore Int (List LVar) (Or LVar Int) (Maybe CStore)))
  (doc 'description "Propagate count constraint by narrowing based on must/can counts")
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

(define (count-must-equal cs value vars)
  (doc 'type '(-> CStore Int (List LVar) Nat))
  (doc 'description "Count variables that must equal value (singleton with that value)")
  (length (filter (lambda (var)
                          (let ([dom (cstore-get-domain cs var)])
                               (and dom
                                    (domain-singleton? dom)
                                    (= (domain-min dom) value))))
                  vars)))

(define (count-can-equal cs value vars)
  (doc 'type '(-> CStore Int (List LVar) Nat))
  (doc 'description "Count variables that can equal value (value in domain)")
  (length (filter (lambda (var)
                          (let ([dom (cstore-get-domain cs var)])
                               (or (not dom)  ; Unconstrained = can equal anything
                                   (domain-contains? dom value))))
                  vars)))

(define (count-fd-fix cs value vars must can lo hi)
  (doc 'type '(-> CStore Int (List LVar) Nat Nat Nat Nat (Maybe CStore)))
  (doc 'description "Apply deductions when count bounds are tight")
  (cond
   ;; If we must have exactly can occurrences, fix all "can" vars
   [(= lo can)
    (fix-all-can-vars cs value vars)]
   ;; If we already have must = hi, remove value from all "can but not must" vars
   [(= hi must)
    (remove-value-from-can-vars cs value vars)]
   [else cs]))

(define (fix-all-can-vars cs value vars)
  (doc 'type '(-> CStore Int (List LVar) (Maybe CStore)))
  (doc 'description "Set all vars that can equal value to that value")
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

(define (remove-value-from-can-vars cs value vars)
  (doc 'type '(-> CStore Int (List LVar) (Maybe CStore)))
  (doc 'description "Remove value from all vars that aren't already singleton with that value")
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

(define (post-count-fd cs value vars n)
  (doc 'export #t)
  (doc 'type '(-> CStore Int (List LVar) (Union LVar Int) (Maybe CStore)))
  (doc 'description "Post count constraint and register for re-propagation")
  (let ([all-vars (if (lvar? n) (cons n vars) vars)])
       (post-constraint cs 'count-fd all-vars (count-fd value vars n))))
