;;; core/util/cost-model.ss --- Pluggable Cost Model Abstraction
;;;
;;; Provides a framework for defining cost models used by the profiler.
;;; Instead of hard-coded fuel tracking, cost models allow flexible
;;; cost accounting for different analysis purposes.
;;;
;;; A cost model defines:
;;;   - eval-cost:   cost of evaluating an expression
;;;   - prim-cost:   cost of primitive operations (by name)
;;;   - apply-cost:  cost of function application
;;;   - aggregate:   how to combine costs (sum, max, etc.)
;;;
;;; This is Core code: pure, total, assumes perfect input.
;;;
;;; Dependencies:
;;;   - prelude.ss

(load "core/base/prelude.ss")

;;; ============================================================
;;; Cost Model Interface
;;; ============================================================

;;; make-cost-model : Symbol x (Expr -> Nat) x (Symbol -> Nat) x Nat x (List<Nat> -> Nat) -> CostModel
;;; Create a cost model with the given parameters.
;;;
;;; Parameters:
;;;   name:        Identifier for this cost model
;;;   eval-cost:   Function mapping expressions to their evaluation cost
;;;   prim-cost:   Function mapping primitive symbols to their cost
;;;   apply-cost:  Fixed cost for function application
;;;   aggregate:   Function combining a list of costs into a total
(define (make-cost-model name eval-cost prim-cost apply-cost aggregate)
  `(cost-model
    (name . ,name)
    (eval-cost . ,eval-cost)
    (prim-cost . ,prim-cost)
    (apply-cost . ,apply-cost)
    (aggregate . ,aggregate)))

;;; cost-model? : Any -> Bool
;;; Check if value is a cost model.
(define (cost-model? cm)
  (and (pair? cm) (eq? (car cm) 'cost-model)))

;;; ============================================================
;;; Cost Model Accessors
;;; ============================================================

;;; cost-model-get : CostModel x Symbol -> Any
;;; Extract a field from a cost model.
(define (cost-model-get cm key)
  (let ([entry (assq key (cdr cm))])
       (and entry (cdr entry))))

;;; cost-model-name : CostModel -> Symbol
;;; Get the name of a cost model.
(define (cost-model-name cm)
  (cost-model-get cm 'name))

;;; cost-model-eval-cost : CostModel -> (Expr -> Nat)
;;; Get the evaluation cost function.
(define (cost-model-eval-cost cm)
  (cost-model-get cm 'eval-cost))

;;; cost-model-prim-cost : CostModel -> (Symbol -> Nat)
;;; Get the primitive cost function.
(define (cost-model-prim-cost cm)
  (cost-model-get cm 'prim-cost))

;;; cost-model-apply-cost : CostModel -> Nat
;;; Get the application cost.
(define (cost-model-apply-cost cm)
  (cost-model-get cm 'apply-cost))

;;; cost-model-aggregate : CostModel -> (List<Nat> -> Nat)
;;; Get the cost aggregation function.
(define (cost-model-aggregate cm)
  (cost-model-get cm 'aggregate))

;;; ============================================================
;;; Cost Computation Helpers
;;; ============================================================

;;; compute-eval-cost : CostModel x Expr -> Nat
;;; Compute the cost of evaluating an expression.
(define (compute-eval-cost cm expr)
  ((cost-model-eval-cost cm) expr))

;;; compute-prim-cost : CostModel x Symbol -> Nat
;;; Compute the cost of a primitive operation.
(define (compute-prim-cost cm prim-name)
  ((cost-model-prim-cost cm) prim-name))

;;; compute-aggregate : CostModel x List<Nat> -> Nat
;;; Aggregate a list of costs.
(define (compute-aggregate cm costs)
  ((cost-model-aggregate cm) costs))

;;; ============================================================
;;; Built-in Cost Models
;;; ============================================================

;;; fuel-cost-model : CostModel
;;; The default fuel-based cost model.
;;; - Each evaluation costs 1 unit
;;; - Primitives are free
;;; - Application costs 1 unit
;;; - Costs are summed
(define fuel-cost-model
  (make-cost-model
   'fuel
   (lambda (expr) 1)           ; Each eval costs 1
   (lambda (prim) 0)           ; Primitives are free
   1                           ; Apply costs 1
   (lambda (costs)             ; Sum all costs
           (if (null? costs)
               0
               (fold-left + 0 costs)))))

;;; weighted-cost-model : CostModel
;;; A cost model that weighs expressions by complexity.
;;; - Literals/symbols cost 1
;;; - Lambdas cost 2
;;; - Applications cost 3
;;; - Let bindings cost 2
(define weighted-cost-model
  (make-cost-model
   'weighted
   (lambda (expr)
           (cond
            [(or (symbol? expr) (number? expr) (string? expr) (boolean? expr)) 1]
            [(not (pair? expr)) 1]
            [(eq? (car expr) 'fn) 2]
            [(eq? (car expr) 'let) 2]
            [(eq? (car expr) 'if) 2]
            [(eq? (car expr) 'fix) 3]
            [(eq? (car expr) 'call) 3]
            [else 1]))
   (lambda (prim)
           (case prim
                 [(add sub mul) 1]
                 [(div mod) 2]
                 [(cons car cdr) 1]
                 [(list) 2]
                 [else 1]))
   2
   (lambda (costs)
           (if (null? costs)
               0
               (fold-left + 0 costs)))))

;;; memory-cost-model : CostModel
;;; A cost model focused on memory allocation.
;;; - Allocation-heavy operations are expensive
;;; - Pure computation is cheap
(define memory-cost-model
  (make-cost-model
   'memory
   (lambda (expr)
           (cond
            [(pair? expr) 1]    ; Creating pairs costs
            [else 0]))          ; Atoms are free
   (lambda (prim)
           (case prim
                 [(cons) 2]         ; Cons allocates
                 [(list) 3]         ; List allocates more
                 [(append) 5]       ; Append allocates a lot
                 [(map filter) 4]   ; These create new lists
                 [else 0]))         ; Other prims are cheap
   1
   (lambda (costs)
           (if (null? costs)
               0
               (fold-left + 0 costs)))))

;;; max-depth-model : CostModel
;;; A cost model that tracks maximum depth (not sum).
;;; Useful for space complexity analysis.
(define max-depth-model
  (make-cost-model
   'max-depth
   (lambda (expr) 1)
   (lambda (prim) 0)
   1
   (lambda (costs)
           (if (null? costs)
               0
               (fold-left max 0 costs)))))

;;; ============================================================
;;; Cost Tracker
;;; ============================================================

;;; A cost tracker accumulates costs during evaluation,
;;; categorized by operation type.

;;; make-cost-tracker : CostModel -> CostTracker
;;; Create a cost tracker using the given cost model.
(define (make-cost-tracker model)
  `(cost-tracker
    (model . ,model)
    (costs . ())))   ; Alist of (category . accumulated-cost)

;;; cost-tracker? : Any -> Bool
;;; Check if value is a cost tracker.
(define (cost-tracker? ct)
  (and (pair? ct) (eq? (car ct) 'cost-tracker)))

;;; cost-tracker-get : CostTracker x Symbol -> Any
;;; Extract a field from a cost tracker.
(define (cost-tracker-get ct key)
  (let ([entry (assq key (cdr ct))])
       (and entry (cdr entry))))

;;; cost-tracker-model : CostTracker -> CostModel
;;; Get the model used by this tracker.
(define (cost-tracker-model ct)
  (cost-tracker-get ct 'model))

;;; cost-tracker-costs : CostTracker -> Alist
;;; Get the accumulated costs.
(define (cost-tracker-costs ct)
  (cost-tracker-get ct 'costs))

;;; cost-tracker-set : CostTracker x Symbol x Any -> CostTracker
;;; Update a field in a cost tracker (pure).
(define (cost-tracker-set ct key value)
  (cons 'cost-tracker
        (map (lambda (entry)
                     (if (eq? (car entry) key)
                         (cons key value)
                         entry))
             (cdr ct))))

;;; track-cost : CostTracker x Symbol x Nat -> CostTracker
;;; Add a cost to the tracker under the given category.
;;; Returns a new tracker with updated costs (pure).
(define (track-cost ct category cost)
  (let* ([costs (cost-tracker-costs ct)]
         [existing (assq category costs)]
         [new-costs
          (if existing
              (map (lambda (entry)
                           (if (eq? (car entry) category)
                               (cons category (+ cost (cdr entry)))
                               entry))
                   costs)
              (cons (cons category cost) costs))])
        (cost-tracker-set ct 'costs new-costs)))

;;; get-costs : CostTracker -> Alist
;;; Get all accumulated costs as an alist.
(define (get-costs ct)
  (cost-tracker-costs ct))

;;; get-category-cost : CostTracker x Symbol -> Nat
;;; Get the accumulated cost for a specific category.
(define (get-category-cost ct category)
  (let ([entry (assq category (cost-tracker-costs ct))])
       (if entry (cdr entry) 0)))

;;; get-total-cost : CostTracker -> Nat
;;; Get the total of all accumulated costs using the model's aggregate.
(define (get-total-cost ct)
  (let* ([costs (cost-tracker-costs ct)]
         [model (cost-tracker-model ct)]
         [values (map cdr costs)])
        (compute-aggregate model values)))

;;; reset-tracker : CostTracker -> CostTracker
;;; Create a new tracker with same model but zeroed costs.
(define (reset-tracker ct)
  (make-cost-tracker (cost-tracker-model ct)))

;;; ============================================================
;;; Cost Model Composition
;;; ============================================================

;;; combine-cost-models : CostModel x CostModel -> CostModel
;;; Create a new cost model that sums costs from two models.
(define (combine-cost-models cm1 cm2)
  (make-cost-model
   (string->symbol
    (string-append (symbol->string (cost-model-name cm1))
                   "+"
                   (symbol->string (cost-model-name cm2))))
   (lambda (expr)
           (+ (compute-eval-cost cm1 expr)
              (compute-eval-cost cm2 expr)))
   (lambda (prim)
           (+ (compute-prim-cost cm1 prim)
              (compute-prim-cost cm2 prim)))
   (+ (cost-model-apply-cost cm1)
      (cost-model-apply-cost cm2))
   (lambda (costs)
           (if (null? costs)
               0
               (fold-left + 0 costs)))))

;;; scale-cost-model : CostModel x Nat -> CostModel
;;; Scale all costs in a model by a factor.
(define (scale-cost-model cm factor)
  (make-cost-model
   (string->symbol
    (string-append (symbol->string (cost-model-name cm))
                   "*"
                   (number->string factor)))
   (lambda (expr)
           (* factor (compute-eval-cost cm expr)))
   (lambda (prim)
           (* factor (compute-prim-cost cm prim)))
   (* factor (cost-model-apply-cost cm))
   (cost-model-aggregate cm)))
