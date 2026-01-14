;;; lattice/fp/clp/label.ss — Labeling Strategies for CLP(FD)
;;;
;;; Implements variable ordering and value selection strategies
;;; for the search phase of constraint solving.
;;;
;;; Variable Ordering Strategies:
;;;   - input-order: Select first unbound variable
;;;   - first-fail: Select variable with smallest domain
;;;   - most-constrained: Smallest domain / largest degree
;;;
;;; Value Selection Strategies:
;;;   - min-value: Select minimum value in domain
;;;   - max-value: Select maximum value in domain
;;;   - mid-value: Select middle value
;;;
;;; Dependencies:
;;;   - propagate.ss
;;;   - stream.ss (for solution enumeration)

(load "lattice/fp/clp/propagate.ss")

;;; ====
;;; Variable Ordering Strategies
;;; ====

;;; input-order : CStore × (List LVar) → (Maybe LVar)
;;; Select the first unbound (non-singleton domain) variable.
(define (input-order cs vars)
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

;;; first-fail : CStore × (List LVar) → (Maybe LVar)
;;; Select the variable with smallest domain (fail first).
(define (first-fail cs vars)
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

;;; most-constrained : CStore × (List LVar) → (Maybe LVar)
;;; Select variable with smallest domain-to-constraint ratio (dom/deg).
;;; Uses number of constraints as tie-breaker for first-fail.
(define (most-constrained cs vars)
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

;;; max-regret : CStore × (List LVar) → (Maybe LVar)
;;; Select variable with largest difference between two smallest values.
(define (max-regret cs vars)
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

;;; ====
;;; Value Selection Strategies
;;; ====

;;; min-value : Domain → Int
;;; Select minimum value from domain.
(define (min-value dom)
  (domain-min dom))

;;; max-value : Domain → Int
;;; Select maximum value from domain.
(define (max-value dom)
  (domain-max dom))

;;; mid-value : Domain → Int
;;; Select middle value from domain.
(define (mid-value dom)
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

;;; ====
;;; Labeling Functions
;;; ====

;;; label : CStore × (List LVar) → (Stream CStore)
;;; Label variables using default strategy (first-fail, min-value).
(define (label cs vars)
  (label-with first-fail min-value cs vars))

;;; label-with : VarOrder × ValSelect × CStore × (List LVar) → (Stream CStore)
;;; Label variables with custom strategies.
;;; Returns a stream of all solutions.
(define (label-with var-order val-select cs vars)
  (let ([var (var-order cs vars)])
       (if (not var)
           ;; All variables labeled - this is a solution
           (stream-cons cs (lambda () stream-nil))
           ;; Choose a value and continue
           (let ([dom (cstore-get-domain cs var)])
                (if (or (not dom) (domain-empty? dom))
                    stream-nil  ; No valid values - fail
                    (label-values var-order val-select cs vars var dom))))))

;;; label-values : VarOrder × ValSelect × CStore × (List LVar) × LVar × Domain
;;;                → (Stream CStore)
;;; Try each value in domain for the selected variable.
(define (label-values var-order val-select cs vars var dom)
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

;;; try-value : CStore × LVar × Int → (Maybe CStore)
;;; Try assigning a value to a variable and propagate.
(define (try-value cs var val)
  (let ([cs1 (cstore-set-domain cs var (domain-singleton val))])
       (and cs1 (propagate cs1))))

;;; ====
;;; Convenience Labeling
;;; ====

;;; label-all : CStore × (List LVar) → (List CStore)
;;; Get all solutions as a list (may be expensive for large search spaces).
(define (label-all cs vars)
  (label-all-with-limit cs vars 1000))

;;; label-all-with-limit : CStore × (List LVar) × Nat → (List CStore)
;;; Get up to n solutions.
(define (label-all-with-limit cs vars limit)
  (stream->list-limit (label cs vars) limit))

;;; stream->list-limit : Stream × Nat → List
;;; Convert stream to list with limit.
(define (stream->list-limit stream limit)
  (if (or (<= limit 0) (stream-nil? stream))
      '()
      (cons (stream-head stream)
            (stream->list-limit (stream-tail stream) (- limit 1)))))

;;; label-first : CStore × (List LVar) → (Maybe CStore)
;;; Get first solution or #f if none.
(define (label-first cs vars)
  (let ([solutions (label cs vars)])
       (if (stream-nil? solutions)
           #f
           (stream-head solutions))))

;;; label-count : CStore × (List LVar) → Nat
;;; Count solutions (with limit to prevent infinite counting).
(define (label-count cs vars)
  (label-count-with-limit cs vars 10000))

;;; label-count-with-limit : CStore × (List LVar) × Nat → Nat
(define (label-count-with-limit cs vars limit)
  (let loop ([stream (label cs vars)] [count 0])
       (if (or (>= count limit) (stream-nil? stream))
           count
           (loop (stream-tail stream) (+ count 1)))))

;;; ====
;;; Strategy Constructors
;;; ====

;;; make-labeling-strategy : VarOrder × ValSelect → (CStore × (List LVar) → (Stream CStore))
;;; Create a labeling function from component strategies.
(define (make-labeling-strategy var-order val-select)
  (lambda (cs vars)
          (label-with var-order val-select cs vars)))

;;; Common strategies
(define label-ff-min (make-labeling-strategy first-fail min-value))
(define label-ff-max (make-labeling-strategy first-fail max-value))
(define label-mc-min (make-labeling-strategy most-constrained min-value))
(define label-input-min (make-labeling-strategy input-order min-value))
