(load "lattice/fp/clp/propagate.ss")

(doc 'module 'label)
(doc 'description "Labeling Strategies for CLP(FD)")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'note "Implements variable ordering and value selection strategies for the search phase of constraint solving")
(doc 'exports "input-order, first-fail, most-constrained, max-regret, min-value, max-value, mid-value, label, label-with, label-all, label-first, label-count")
(doc 'dependencies "propagate.ss, stream.ss (for solution enumeration)")

(doc 'section 'variable-ordering-strategies)

(define (input-order cs vars)
  (doc 'type '(-> CStore (List LVar) (Maybe LVar)))
  (doc 'description "Select the first unbound (non-singleton domain) variable")
  (let loop ([vars vars])
       (cond
        [(null? vars) #f]
        [else
         (let ([var (car vars)]
               [dom (cstore-get-domain cs (car vars))])
              (cond
               ;; No domain or non-singleton - needs labeling
               [(or (not dom) (not (domain-singleton? dom)))
                var]
               [else (loop (cdr vars))]))])))

(define (first-fail cs vars)
  (doc 'type '(-> CStore (List LVar) (Maybe LVar)))
  (doc 'description "Select the variable with smallest domain (fail first)")
  (let loop ([vars vars] [best #f] [best-size +inf.0])
       (if (null? vars)
           best
           (let* ([var (car vars)]
                  [dom (cstore-get-domain cs var)])
                 (cond
                  ;; No domain - treat as unconstrained, skip
                  [(not dom)
                   (loop (cdr vars) (or best var) best-size)]
                  ;; Singleton - already bound, skip
                  [(domain-singleton? dom)
                   (loop (cdr vars) best best-size)]
                  ;; Check if smaller than current best
                  [else
                   (let ([size (domain-size dom)])
                        (if (< size best-size)
                            (loop (cdr vars) var size)
                            (loop (cdr vars) best best-size)))])))))

(define (most-constrained cs vars)
  (doc 'type '(-> CStore (List LVar) (Maybe LVar)))
  (doc 'description "Select variable with smallest domain-to-constraint ratio (dom/deg)")
  (doc 'note "Uses number of constraints as tie-breaker for first-fail")
  (let loop ([vars vars] [best #f] [best-score +inf.0])
       (if (null? vars)
           best
           (let* ([var (car vars)]
                  [dom (cstore-get-domain cs var)])
                 (cond
                  [(not dom)
                   (loop (cdr vars) (or best var) best-score)]
                  [(domain-singleton? dom)
                   (loop (cdr vars) best best-score)]
                  [else
                   (let* ([size (domain-size dom)]
                          [degree (length (cstore-constraints-for-var cs var))]
                          ;; Score = size / (degree + 1) to avoid div by zero
                          [score (/ size (+ degree 1))])
                         (if (< score best-score)
                             (loop (cdr vars) var score)
                             (loop (cdr vars) best best-score)))])))))

(define (max-regret cs vars)
  (doc 'type '(-> CStore (List LVar) (Maybe LVar)))
  (doc 'description "Select variable with largest difference between two smallest values")
  (let loop ([vars vars] [best #f] [best-regret -1])
       (if (null? vars)
           best
           (let* ([var (car vars)]
                  [dom (cstore-get-domain cs var)])
                 (cond
                  [(not dom)
                   (loop (cdr vars) (or best var) best-regret)]
                  [(domain-singleton? dom)
                   (loop (cdr vars) best best-regret)]
                  [else
                   (let* ([vals (domain->list dom)]
                          [regret (if (< (length vals) 2)
                                      0
                                      (- (cadr vals) (car vals)))])
                         (if (> regret best-regret)
                             (loop (cdr vars) var regret)
                             (loop (cdr vars) best best-regret)))])))))

(doc 'section 'value-selection-strategies)

(define (min-value dom)
  (doc 'type '(-> Domain Int))
  (doc 'description "Select minimum value from domain")
  (domain-min dom))

(define (max-value dom)
  (doc 'type '(-> Domain Int))
  (doc 'description "Select maximum value from domain")
  (domain-max dom))

(define (mid-value dom)
  (doc 'type '(-> Domain Int))
  (doc 'description "Select middle value from domain")
  (let* ([lo (domain-min dom)]
         [hi (domain-max dom)]
         [mid (quotient (+ lo hi) 2)])
        ;; Find closest value in domain to mid
        (if (domain-contains? dom mid)
            mid
            (let loop ([offset 1])
                 (cond
                  [(domain-contains? dom (+ mid offset)) (+ mid offset)]
                  [(domain-contains? dom (- mid offset)) (- mid offset)]
                  [else (loop (+ offset 1))])))))

(doc 'section 'labeling-functions)

(define (label cs vars)
  (doc 'type '(-> CStore (List LVar) (Stream CStore)))
  (doc 'description "Label variables using default strategy (first-fail, min-value)")
  (label-with first-fail min-value cs vars))

(define (label-with var-order val-select cs vars)
  (doc 'type '(-> VarOrder ValSelect CStore (List LVar) (Stream CStore)))
  (doc 'description "Label variables with custom strategies")
  (doc 'returns "Stream of all solutions")
  (let ([var (var-order cs vars)])
       (if (not var)
           ;; All variables labeled - this is a solution
           (stream-cons cs (lambda () stream-nil))
           ;; Choose a value and continue
           (let ([dom (cstore-get-domain cs var)])
                (if (or (not dom) (domain-empty? dom))
                    stream-nil  ; No valid values - fail
                    (label-values var-order val-select cs vars var dom))))))

(define (label-values var-order val-select cs vars var dom)
  (doc 'type '(-> VarOrder ValSelect CStore (List LVar) LVar Domain (Stream CStore)))
  (doc 'description "Try each value in domain for the selected variable")
  (if (domain-empty? dom)
      stream-nil
      (let* ([val (val-select dom)]
             [cs1 (try-value cs var val)]
             [remaining-dom (domain-subtract-value dom val)])
            ;; Append: try this value, then try remaining values
            (stream-append
             (if cs1
                 (label-with var-order val-select cs1 vars)
                 stream-nil)
             (lambda ()
                     (label-values var-order val-select cs vars var remaining-dom))))))

(define (try-value cs var val)
  (doc 'type '(-> CStore LVar Int (Maybe CStore)))
  (doc 'description "Try assigning a value to a variable and propagate")
  (let ([cs1 (cstore-set-domain cs var (domain-singleton val))])
       (and cs1 (propagate cs1))))

(doc 'section 'convenience-labeling)

(define (label-all cs vars)
  (doc 'type '(-> CStore (List LVar) (List CStore)))
  (doc 'description "Get all solutions as a list (may be expensive for large search spaces)")
  (doc 'note "Default limit of 1000 solutions")
  (label-all-with-limit cs vars 1000))

(define (label-all-with-limit cs vars limit)
  (doc 'type '(-> CStore (List LVar) Nat (List CStore)))
  (doc 'description "Get up to n solutions")
  (stream->list-limit (label cs vars) limit))

(define (stream->list-limit stream limit)
  (doc 'type '(-> Stream Nat List))
  (doc 'description "Convert stream to list with limit")
  (if (or (<= limit 0) (stream-nil? stream))
      '()
      (cons (stream-head stream)
            (stream->list-limit (stream-tail stream) (- limit 1)))))

(define (label-first cs vars)
  (doc 'type '(-> CStore (List LVar) (Maybe CStore)))
  (doc 'description "Get first solution or #f if none")
  (let ([solutions (label cs vars)])
       (if (stream-nil? solutions)
           #f
           (stream-head solutions))))

(define (label-count cs vars)
  (doc 'type '(-> CStore (List LVar) Nat))
  (doc 'description "Count solutions (with limit of 10000 to prevent infinite counting)")
  (label-count-with-limit cs vars 10000))

(define (label-count-with-limit cs vars limit)
  (doc 'type '(-> CStore (List LVar) Nat Nat))
  (doc 'description "Count solutions up to specified limit")
  (let loop ([stream (label cs vars)] [count 0])
       (if (or (>= count limit) (stream-nil? stream))
           count
           (loop (stream-tail stream) (+ count 1)))))

(doc 'section 'strategy-constructors)

(define (make-labeling-strategy var-order val-select)
  (doc 'type '(-> VarOrder ValSelect (-> CStore (List LVar) (Stream CStore))))
  (doc 'description "Create a labeling function from component strategies")
  (lambda (cs vars)
          (label-with var-order val-select cs vars)))

(doc 'note "Common pre-defined labeling strategies")
(define label-ff-min (make-labeling-strategy first-fail min-value))
(define label-ff-max (make-labeling-strategy first-fail max-value))
(define label-mc-min (make-labeling-strategy most-constrained min-value))
(define label-input-min (make-labeling-strategy input-order min-value))
