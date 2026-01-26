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

(load "core/base/prelude.ss")
(load "lattice/egraph/egraph.ss")

(doc 'module 'egraph/cost)
(doc 'description "Cost models for e-graph extraction")
(doc 'layer 'lattice)
(doc 'tier 1)
(doc 'purity 'total)

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

(define (cost-model-name cm) (vector-ref cm 1))
(define (cost-model-fn cm) (vector-ref cm 2))

;;; ============================================================
;;; Cost Computation
;;; ============================================================

(doc 'section 'computation)

;;; compute-costs : EGraph × CostModel → (Hashtable ClassId Nat)
;;; Compute minimum cost for each e-class using dynamic programming.
;;; Uses iterative refinement until costs stabilize.
(define (compute-costs eg cost-model)
  (doc 'type (-> EGraph CostModel (Hashtable ClassId Nat)))
  (doc 'description "Compute minimum cost for each e-class.")
  (doc 'export #t)
  (let ([costs (make-hashtable equal-hash equal?)]
        [node-cost-fn (cost-model-fn cost-model)]
        [uf (egraph-uf eg)]
        [store (egraph-classes eg)])
    ;; Initialize all classes with infinite cost
    (for-each
     (lambda (root)
       (hashtable-set! costs root +inf.0))
     (uf-roots uf))
    ;; Iteratively refine costs until fixpoint
    (let loop ([changed #t]
               [iterations 0])
      (if (or (not changed) (> iterations 1000))
          costs
          (let ([any-changed #f])
            (for-each
             (lambda (root)
               (let ([nodes (eclass-get-nodes store root)])
                 (for-each
                  (lambda (node)
                    (let ([node-cost (compute-node-cost node costs node-cost-fn)])
                      (when (< node-cost (hashtable-ref costs root +inf.0))
                        (hashtable-set! costs root node-cost)
                        (set! any-changed #t))))
                  nodes)))
             (uf-roots uf))
            (loop any-changed (+ iterations 1)))))))

;;; compute-node-cost : ENode × CostTable × NodeCostFn → Nat
;;; Compute cost of a single e-node given child costs.
(define (compute-node-cost node costs node-cost-fn)
  (doc 'type (-> ENode (Hashtable ClassId Nat) NodeCostFn Nat))
  (doc 'description "Compute cost of an e-node using child costs.")
  (let ([child-cost-fn (lambda (class-id)
                         (hashtable-ref costs class-id +inf.0))])
    (node-cost-fn node child-cost-fn)))

;;; class-cost : CostTable × ClassId → Nat
;;; Look up the computed cost for an e-class.
(define (class-cost costs class-id)
  (doc 'type (-> (Hashtable ClassId Nat) ClassId Nat))
  (doc 'description "Get computed cost for an e-class.")
  (doc 'export #t)
  (hashtable-ref costs class-id +inf.0))

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

;;; ============================================================
;;; Weighted Cost Models
;;; ============================================================

(doc 'section 'weighted-models)

;;; make-weighted-cost : (Hashtable Symbol Nat) × Nat → CostModel
;;; Create a cost model with operator-specific weights.
;;; Operators not in the table get the default cost.
(define (make-weighted-cost op-costs default-cost)
  (doc 'type (-> (Hashtable Symbol Nat) Nat CostModel))
  (doc 'description "Create weighted cost model with operator-specific costs.")
  (doc 'export #t)
  (make-cost-model 'weighted
    (lambda (node child-cost)
      (let* ([op (enode-op node)]
             [base (if (symbol? op)
                       (hashtable-ref op-costs op default-cost)
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
  (let ([ht (make-hashtable symbol-hash eq?)])
    ;; Memory operations (most expensive)
    (hashtable-set! ht 'load 100)
    (hashtable-set! ht 'store 100)
    (hashtable-set! ht 'index 50)

    ;; Basic arithmetic (cheap)
    (hashtable-set! ht '+ 1)
    (hashtable-set! ht '- 1)
    (hashtable-set! ht '* 2)
    (hashtable-set! ht 'neg 1)

    ;; Division and modulo (more expensive)
    (hashtable-set! ht '/ 10)
    (hashtable-set! ht 'mod 10)
    (hashtable-set! ht 'div 10)

    ;; Special functions (transcendentals)
    (hashtable-set! ht 'sqrt 15)
    (hashtable-set! ht 'rsqrt 8)   ; Faster than sqrt on GPU
    (hashtable-set! ht 'exp 20)
    (hashtable-set! ht 'log 20)
    (hashtable-set! ht 'sin 20)
    (hashtable-set! ht 'cos 20)
    (hashtable-set! ht 'tan 25)
    (hashtable-set! ht 'pow 30)

    ;; Fused operations (preferred - reduce memory traffic)
    (hashtable-set! ht 'fma 3)     ; Fused multiply-add
    (hashtable-set! ht 'fms 3)     ; Fused multiply-subtract
    (hashtable-set! ht 'mad 3)     ; Multiply-add

    ;; Comparisons and logic (cheap)
    (hashtable-set! ht '< 1)
    (hashtable-set! ht '<= 1)
    (hashtable-set! ht '> 1)
    (hashtable-set! ht '>= 1)
    (hashtable-set! ht '= 1)
    (hashtable-set! ht 'and 1)
    (hashtable-set! ht 'or 1)
    (hashtable-set! ht 'not 1)

    ;; Control flow (prefer predication over branching)
    (hashtable-set! ht 'if 5)
    (hashtable-set! ht 'select 2)  ; Predicated select

    ;; Vector operations (utilize SIMD)
    (hashtable-set! ht 'vec4-add 1)
    (hashtable-set! ht 'vec4-mul 2)
    (hashtable-set! ht 'dot 4)

    ht))

(define cuda-cost
  (make-weighted-cost cuda-op-costs 5))

;;; ============================================================
;;; CPU Cost Model
;;; ============================================================

(doc 'section 'cpu)

;;; CPU cost model for scalar execution:
;;; - Division is expensive
;;; - Memory is relatively cheap (caching)
;;; - Branch misprediction can be costly

(define cpu-op-costs
  (let ([ht (make-hashtable symbol-hash eq?)])
    ;; Memory (cheaper than GPU due to caching)
    (hashtable-set! ht 'load 10)
    (hashtable-set! ht 'store 10)
    (hashtable-set! ht 'index 5)

    ;; Basic arithmetic
    (hashtable-set! ht '+ 1)
    (hashtable-set! ht '- 1)
    (hashtable-set! ht '* 3)
    (hashtable-set! ht 'neg 1)

    ;; Division (expensive)
    (hashtable-set! ht '/ 20)
    (hashtable-set! ht 'mod 20)
    (hashtable-set! ht 'div 20)

    ;; Special functions
    (hashtable-set! ht 'sqrt 15)
    (hashtable-set! ht 'exp 25)
    (hashtable-set! ht 'log 25)
    (hashtable-set! ht 'sin 25)
    (hashtable-set! ht 'cos 25)
    (hashtable-set! ht 'pow 40)

    ;; Control flow
    (hashtable-set! ht 'if 3)

    ht))

(define cpu-cost
  (make-weighted-cost cpu-op-costs 3))

;;; ============================================================
;;; Code Size Cost Model
;;; ============================================================

(doc 'section 'code-size)

;;; Code size model: optimize for minimal generated code
;;; Useful for embedded systems or instruction cache pressure.

(define code-size-op-costs
  (let ([ht (make-hashtable symbol-hash eq?)])
    ;; All operations cost their instruction count
    (hashtable-set! ht '+ 1)
    (hashtable-set! ht '- 1)
    (hashtable-set! ht '* 1)
    (hashtable-set! ht '/ 1)
    (hashtable-set! ht 'if 3)  ; Branch instructions
    (hashtable-set! ht 'call 2)
    ht))

(define code-size-cost
  (make-weighted-cost code-size-op-costs 1))

;;; ============================================================
;;; Composite Cost Models
;;; ============================================================

(doc 'section 'composite)

;;; combine-costs : (List (CostModel × Weight)) × EGraph → Nat
;;; Compute weighted combination of multiple cost models for an e-graph.
;;; Each model is computed independently, then combined at the class level.
(define (combine-costs eg models-and-weights)
  (doc 'type (-> EGraph (List (Pair CostModel Nat)) (Hashtable ClassId Nat)))
  (doc 'description "Compute weighted combination of cost models.")
  (doc 'export #t)
  ;; Compute costs for each model independently
  (let ([model-costs (map (lambda (mw)
                            (cons (cdr mw)  ; weight
                                  (compute-costs eg (car mw))))
                          models-and-weights)]
        [uf (egraph-uf eg)]
        [combined (make-hashtable equal-hash equal?)])
    ;; Combine costs per class
    (for-each
     (lambda (root)
       (let ([total (fold-left
                     (lambda (acc wc)
                       (+ acc (* (car wc)  ; weight
                                 (hashtable-ref (cdr wc) root +inf.0))))
                     0
                     model-costs)])
         (hashtable-set! combined root total)))
     (uf-roots uf))
    combined))

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
    (list-sort (lambda (a b) (< (cadr a) (cadr b))) results)))

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

