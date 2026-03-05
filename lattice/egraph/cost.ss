;;; @module egraph/cost
;;; @requires prelude hamt egraph/egraph sort
;;; @description Cost models for CUDA, CPU, and code size optimization
;;; @purity partial
;;; @stability experimental
;;; lattice/egraph/cost.ss — Cost Models for E-Graph Extraction
;;;
;;; Cost models assign numeric costs to e-nodes, enabling extraction
;;; of optimal equivalent forms from an e-graph. Different cost models
;;; optimize for different targets (CUDA, CPU, code size, etc.).
;;;
;;; The cost of an e-class is the minimum cost of any e-node it contains.
;;; The cost of an e-node is its base cost plus the costs of its children.
;;;
;;; This is Lattice code: pure cost computation.

(require 'prelude)
(require 'hamt)
(require 'egraph/egraph)
(require 'sort)

(doc 'module 'egraph/cost)
(doc 'description "Cost models for e-graph extraction")
(doc 'layer 'lattice)
(doc 'tier 1)
(doc 'purity 'partial)

;;; ============================================================
;;; Cost Model Protocol
;;; ============================================================

(doc 'section 'protocol)

;;; A cost model is a function: enode × (class-id → cost) → cost
;;; The second argument provides child costs for recursive computation.

(define cost-model-tag 'cost-model)

(define (make-cost-model name node-cost-fn)
  (doc 'type (-> Symbol (-> ENode (-> ClassId Nat) Nat) CostModel))
  (doc 'description "Create a cost model with name and node cost function.")
  (doc 'export #t)
  (vector cost-model-tag name node-cost-fn))

(define (cost-model? x)
  (doc 'type (-> Any Boolean))
  (doc 'description "Check if x is a cost model.")
  (doc 'export #t)
  (and (vector? x)
       (>= (vector-length x) 3)
       (eq? (vector-ref x 0) cost-model-tag)))

(define (cost-model-name cm)
  (doc 'export #t)
  (vector-ref cm 1))
(define (cost-model-fn cm) (vector-ref cm 2))

;;; ============================================================
;;; Cost Computation
;;; ============================================================

(doc 'section 'computation)

;;; compute-costs : EGraph × CostModel → HAMT ClassId Nat
;;; Compute minimum cost for each e-class using dynamic programming.
;;; Uses iterative refinement until costs stabilize.
(define (compute-costs eg cost-model)
  (doc 'type (-> EGraph CostModel HAMT))
  (doc 'description "Compute minimum cost for each e-class.")
  (doc 'export #t)
  (let ([node-cost-fn (cost-model-fn cost-model)]
        [uf (egraph-uf eg)]
        [store (egraph-classes eg)])
    ;; Initialize all classes with infinite cost
    (let ([init-costs (fold-left (lambda (acc root)
                                   (hamt-assoc root +inf.0 acc))
                                 hamt-empty
                                 (uf-roots uf))])
      ;; Iteratively refine costs until fixpoint
      (let loop ([costs init-costs]
                 [changed #t]
                 [iterations 0])
        (if (or (not changed) (> iterations 1000))
            costs
            (let ([result
                   (fold-left
                    (lambda (state root)
                      (let ([c (car state)]
                            [any (cdr state)])
                        (fold-left
                         (lambda (st node)
                           (let ([c2 (car st)]
                                 [any2 (cdr st)]
                                 [node-cost (compute-node-cost node (car st) node-cost-fn)])
                             (if (< node-cost (hamt-lookup-or root (car st) +inf.0))
                                 (cons (hamt-assoc root node-cost (car st)) #t)
                                 st)))
                         state
                         (eclass-get-nodes store root))))
                    (cons costs #f)
                    (uf-roots uf))])
              (loop (car result) (cdr result) (+ iterations 1))))))))

;;; compute-node-cost : ENode × CostTable × NodeCostFn → Nat
;;; Compute cost of a single e-node given child costs.
(define (compute-node-cost node costs node-cost-fn)
  (doc 'type (-> ENode HAMT NodeCostFn Nat))
  (doc 'description "Compute cost of an e-node using child costs.")
  (let ([child-cost-fn (lambda (class-id)
                         (hamt-lookup-or class-id costs +inf.0))])
    (node-cost-fn node child-cost-fn)))

;;; class-cost : CostTable × ClassId → Nat
;;; Look up the computed cost for an e-class.
(define (class-cost costs class-id)
  (doc 'type (-> HAMT ClassId Nat))
  (doc 'description "Get computed cost for an e-class.")
  (doc 'export #t)
  (hamt-lookup-or class-id costs +inf.0))

;;; ============================================================
;;; Basic Cost Models
;;; ============================================================

(doc 'section 'basic-models)

;;; AST size cost model: cost = 1 + sum of child costs
;;; Minimizes total number of nodes in extracted term.
(define ast-size-cost
  (make-cost-model 'ast-size
    (lambda (node child-cost)
      (let ([children (enode-children node)])
        (+ 1 (fold-left (lambda (acc i)
                          (+ acc (child-cost (vector-ref children i))))
                        0
                        (iota (vector-length children))))))))
(doc 'ast-size-cost 'export #t)

;;; AST depth cost model: cost = 1 + max of child costs
;;; Minimizes maximum depth of extracted term.
(define ast-depth-cost
  (make-cost-model 'ast-depth
    (lambda (node child-cost)
      (let ([children (enode-children node)])
        (if (zero? (vector-length children))
            1
            (+ 1 (fold-left (lambda (acc i)
                              (max acc (child-cost (vector-ref children i))))
                            0
                            (iota (vector-length children)))))))))
(doc 'ast-depth-cost 'export #t)

;;; Leaf-only cost model: cost = number of leaves
;;; Useful for counting variable references.
(define leaf-count-cost
  (make-cost-model 'leaf-count
    (lambda (node child-cost)
      (let ([children (enode-children node)])
        (if (zero? (vector-length children))
            1  ; Leaf
            (fold-left (lambda (acc i)
                         (+ acc (child-cost (vector-ref children i))))
                       0
                       (iota (vector-length children))))))))
(doc 'leaf-count-cost 'export #t)

;;; ============================================================
;;; Weighted Cost Models
;;; ============================================================

(doc 'section 'weighted-models)

;;; make-weighted-cost : HAMT Symbol Nat × Nat → CostModel
;;; Create a cost model with operator-specific weights.
;;; Operators not in the table get the default cost.
(define (make-weighted-cost op-costs default-cost)
  (doc 'type (-> HAMT Nat CostModel))
  (doc 'description "Create weighted cost model with operator-specific costs.")
  (doc 'export #t)
  (make-cost-model 'weighted
    (lambda (node child-cost)
      (let* ([op (enode-op node)]
             [base (if (symbol? op)
                       (hamt-lookup-or op op-costs default-cost)
                       default-cost)]
             [children (enode-children node)]
             [child-sum (fold-left (lambda (acc i)
                                     (+ acc (child-cost (vector-ref children i))))
                                   0
                                   (iota (vector-length children)))])
        (+ base child-sum)))))

;;; ============================================================
;;; CUDA Cost Model
;;; ============================================================

(doc 'section 'cuda)

;;; CUDA cost model optimizes for GPU execution:
;;; - Memory operations are expensive (global memory bandwidth)
;;; - Arithmetic is cheap (high throughput)
;;; - Fused operations reduce memory traffic
;;; - Division and special functions are more expensive

(define cuda-op-costs
  (alist->hamt
   '(;; Memory operations (most expensive)
     (load . 100) (store . 100) (index . 50)
     ;; Basic arithmetic (cheap)
     (+ . 1) (- . 1) (* . 2) (neg . 1)
     ;; Division and modulo (more expensive)
     (/ . 10) (mod . 10) (div . 10)
     ;; Special functions (transcendentals)
     (sqrt . 15) (rsqrt . 8) (exp . 20) (log . 20)
     (sin . 20) (cos . 20) (tan . 25) (pow . 30)
     ;; Fused operations (preferred - reduce memory traffic)
     (fma . 3) (fms . 3) (mad . 3)
     ;; Comparisons and logic (cheap)
     (< . 1) (<= . 1) (> . 1) (>= . 1) (= . 1)
     (and . 1) (or . 1) (not . 1)
     ;; Control flow (prefer predication over branching)
     (if . 5) (select . 2)
     ;; Vector operations (utilize SIMD)
     (vec4-add . 1) (vec4-mul . 2) (dot . 4))))

(define cuda-cost
  (make-weighted-cost cuda-op-costs 5))
(doc 'cuda-cost 'export #t)

;;; ============================================================
;;; CPU Cost Model
;;; ============================================================

(doc 'section 'cpu)

;;; CPU cost model for scalar execution:
;;; - Division is expensive
;;; - Memory is relatively cheap (caching)
;;; - Branch misprediction can be costly

(define cpu-op-costs
  (alist->hamt
   '(;; Memory (cheaper than GPU due to caching)
     (load . 10) (store . 10) (index . 5)
     ;; Basic arithmetic
     (+ . 1) (- . 1) (* . 3) (neg . 1)
     ;; Division (expensive)
     (/ . 20) (mod . 20) (div . 20)
     ;; Special functions
     (sqrt . 15) (exp . 25) (log . 25)
     (sin . 25) (cos . 25) (pow . 40)
     ;; Control flow
     (if . 3))))

(define cpu-cost
  (make-weighted-cost cpu-op-costs 3))
(doc 'cpu-cost 'export #t)

;;; ============================================================
;;; Code Size Cost Model
;;; ============================================================

(doc 'section 'code-size)

;;; Code size model: optimize for minimal generated code
;;; Useful for embedded systems or instruction cache pressure.

(define code-size-op-costs
  (alist->hamt
   '(;; All operations cost their instruction count
     (+ . 1) (- . 1) (* . 1) (/ . 1)
     (if . 3)    ; Branch instructions
     (call . 2))))

(define code-size-cost
  (make-weighted-cost code-size-op-costs 1))
(doc 'code-size-cost 'export #t)

;;; ============================================================
;;; Composite Cost Models
;;; ============================================================

(doc 'section 'composite)

;;; combine-costs : (List (CostModel × Weight)) × EGraph → HAMT
;;; Compute weighted combination of multiple cost models for an e-graph.
;;; Each model is computed independently, then combined at the class level.
(define (combine-costs eg models-and-weights)
  (doc 'type (-> EGraph (List (Pair CostModel Nat)) HAMT))
  (doc 'description "Compute weighted combination of cost models.")
  (doc 'export #t)
  ;; Compute costs for each model independently
  (let ([model-costs (map (lambda (mw)
                            (cons (cdr mw)  ; weight
                                  (compute-costs eg (car mw))))
                          models-and-weights)]
        [uf (egraph-uf eg)])
    ;; Combine costs per class
    (fold-left
     (lambda (combined root)
       (let ([total (fold-left
                     (lambda (acc wc)
                       (+ acc (* (car wc)  ; weight
                                 (hamt-lookup-or root (cdr wc) +inf.0))))
                     0
                     model-costs)])
         (hamt-assoc root total combined)))
     hamt-empty
     (uf-roots uf))))

;;; ============================================================
;;; Cost Analysis
;;; ============================================================

(doc 'section 'analysis)

;;; analyze-costs : EGraph × CostModel → (List (ClassId × Nat × ENode))
;;; Return list of (class-id, cost, best-node) sorted by cost.
(define (analyze-costs eg cost-model)
  (doc 'type (-> EGraph CostModel (List (Triple ClassId Nat ENode))))
  (doc 'description "Analyze costs and find best node per class.")
  (doc 'export #t)
  (let ([costs (compute-costs eg cost-model)]
        [store (egraph-classes eg)]
        [uf (egraph-uf eg)]
        [results '()])
    ;; Find best node for each class
    (for-each
     (lambda (root)
       (let ([best-node #f]
             [best-cost +inf.0]
             [nodes (eclass-get-nodes store root)])
         (for-each
          (lambda (node)
            (let ([c (compute-node-cost node costs (cost-model-fn cost-model))])
              (when (< c best-cost)
                (set! best-cost c)
                (set! best-node node))))
          nodes)
         (when best-node
           (set! results (cons (list root best-cost best-node) results)))))
     (uf-roots uf))
    ;; Sort by cost
    (sort-by (lambda (a b) (< (cadr a) (cadr b))) results)))

;;; ============================================================
;;; Debugging
;;; ============================================================

(doc 'section 'debug)

(define (cost-model->string cm)
  (doc 'type (-> CostModel String))
  (doc 'description "Convert cost model to string.")
  (doc 'export #t)
  (format "CostModel(~a)" (cost-model-name cm)))

(define (costs-debug eg cost-model)
  (doc 'type (-> EGraph CostModel Void))
  (doc 'description "Print cost analysis for e-graph.")
  (doc 'export #t)
  (printf "Cost analysis using ~a:~n" (cost-model-name cost-model))
  (let ([analysis (analyze-costs eg cost-model)])
    (for-each
     (lambda (entry)
       (printf "  Class ~a: cost=~a, node=~a~n"
               (car entry)
               (cadr entry)
               (enode->string (caddr entry))))
     analysis)))

