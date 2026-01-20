(load "core/base/prelude.ss")

(doc 'module 'cost-model)
(doc 'description "Pluggable Cost Model Abstraction. Provides a framework for defining cost models used by the profiler. Instead of hard-coded fuel tracking, cost models allow flexible cost accounting for different analysis purposes. A cost model defines: eval-cost (cost of evaluating an expression), prim-cost (cost of primitive operations by name), apply-cost (cost of function application), aggregate (how to combine costs like sum or max).")
(doc 'layer 'core)

(doc 'section 'cost-model-interface)

(define (make-cost-model name eval-cost prim-cost apply-cost aggregate)
  (doc 'type (-> Symbol (-> Expr Nat) (-> Symbol Nat) Nat (-> (List Nat) Nat) CostModel))
  (doc 'description "Create a cost model with the given cost functions and aggregation strategy.")
  (doc 'export #t)
  `(cost-model
    (name . ,name)
    (eval-cost . ,eval-cost)
    (prim-cost . ,prim-cost)
    (apply-cost . ,apply-cost)
    (aggregate . ,aggregate)))

(define (cost-model? cm)
  (doc 'type (-> Any Bool))
  (doc 'description "Test if value is a cost model.")
  (doc 'export #t)
  (and (pair? cm) (eq? (car cm) 'cost-model)))

(doc 'section 'cost-model-accessors)

(define (cost-model-get cm key)
  (doc 'type (-> CostModel Symbol Any))
  (doc 'description "Get field from cost model.")
  (doc 'export #t)
  (let ([entry (assq key (cdr cm))])
       (and entry (cdr entry))))

;;; cost-model-name : CostModel → Symbol
(define (cost-model-name cm)
  (cost-model-get cm 'name))

;;; cost-model-eval-cost : CostModel → (Expr → Nat)
(define (cost-model-eval-cost cm)
  (cost-model-get cm 'eval-cost))

;;; cost-model-prim-cost : CostModel → (Symbol → Nat)
(define (cost-model-prim-cost cm)
  (cost-model-get cm 'prim-cost))

;;; cost-model-apply-cost : CostModel → Nat
(define (cost-model-apply-cost cm)
  (cost-model-get cm 'apply-cost))

;;; cost-model-aggregate : CostModel → ((List Nat) → Nat)
(define (cost-model-aggregate cm)
  (cost-model-get cm 'aggregate))

(doc 'section 'cost-computation-helpers)

(define (compute-eval-cost cm expr)
  (doc 'type (-> CostModel Expr Nat))
  (doc 'description "Compute the cost of evaluating an expression using the given cost model.")
  (doc 'export #t)
  ((cost-model-eval-cost cm) expr))

;;; compute-prim-cost : CostModel × Symbol → Nat
(define (compute-prim-cost cm prim-name)
  ((cost-model-prim-cost cm) prim-name))

;;; compute-aggregate : CostModel × (List Nat) → Nat
(define (compute-aggregate cm costs)
  ((cost-model-aggregate cm) costs))

(doc 'section 'built-in-cost-models)

(doc fuel-cost-model 'type CostModel)
(doc fuel-cost-model 'description "The default fuel-based cost model. Each evaluation costs 1 unit, primitives are free, application costs 1 unit, costs are summed.")
(doc fuel-cost-model 'export #t)
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

(doc weighted-cost-model 'type CostModel)
(doc weighted-cost-model 'description "A cost model that weighs expressions by complexity. Literals/symbols cost 1, lambdas cost 2, applications cost 3, let bindings cost 2.")
(doc weighted-cost-model 'export #t)
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

(doc memory-cost-model 'type CostModel)
(doc memory-cost-model 'description "A cost model focused on memory allocation. Allocation-heavy operations are expensive, pure computation is cheap.")
(doc memory-cost-model 'export #t)
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

(doc max-depth-model 'type CostModel)
(doc max-depth-model 'description "A cost model that tracks maximum depth (not sum). Useful for space complexity analysis.")
(doc max-depth-model 'export #t)
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

(doc 'section 'cost-tracker)

(define (make-cost-tracker model)
  (doc 'type (-> CostModel CostTracker))
  (doc 'description "Create a cost tracker that accumulates costs during evaluation, categorized by operation type.")
  (doc 'export #t)
  `(cost-tracker
    (model . ,model)
    (costs . ())))   ; Alist of (category . accumulated-cost)

;;; cost-tracker? : α → Boolean
(define (cost-tracker? ct)
  (and (pair? ct) (eq? (car ct) 'cost-tracker)))

;;; cost-tracker-get : CostTracker × Symbol → α
(define (cost-tracker-get ct key)
  (let ([entry (assq key (cdr ct))])
       (and entry (cdr entry))))

;;; cost-tracker-model : CostTracker → CostModel
(define (cost-tracker-model ct)
  (cost-tracker-get ct 'model))

;;; cost-tracker-costs : CostTracker → Alist
(define (cost-tracker-costs ct)
  (cost-tracker-get ct 'costs))

;;; cost-tracker-set : CostTracker × Symbol × α → CostTracker
(define (cost-tracker-set ct key value)
  (cons 'cost-tracker
        (map (lambda (entry)
                     (if (eq? (car entry) key)
                         (cons key value)
                         entry))
             (cdr ct))))

;;; track-cost : CostTracker × Symbol × Nat → CostTracker
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

;;; get-costs : CostTracker → Alist
(define (get-costs ct)
  (cost-tracker-costs ct))

;;; get-category-cost : CostTracker × Symbol → Nat
(define (get-category-cost ct category)
  (let ([entry (assq category (cost-tracker-costs ct))])
       (if entry (cdr entry) 0)))

;;; get-total-cost : CostTracker → Nat
(define (get-total-cost ct)
  (let* ([costs (cost-tracker-costs ct)]
         [model (cost-tracker-model ct)]
         [values (map cdr costs)])
        (compute-aggregate model values)))

;;; reset-tracker : CostTracker → CostTracker
(define (reset-tracker ct)
  (make-cost-tracker (cost-tracker-model ct)))

(doc 'section 'cost-model-composition)

(define (combine-cost-models cm1 cm2)
  (doc 'type (-> CostModel CostModel CostModel))
  (doc 'description "Combine two cost models additively.")
  (doc 'export #t)
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

(define (scale-cost-model cm factor)
  (doc 'type (-> CostModel Nat CostModel))
  (doc 'description "Scale all costs in a cost model by a constant factor.")
  (doc 'export #t)
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
