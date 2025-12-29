;;; fabric/stitches/comp-graph.ss — Computational Graph for Autodiff
;;;
;;; A pure, functional computational graph for automatic differentiation.
;;; Represents computations as a directed acyclic graph (DAG) where:
;;;   - Nodes represent values (variables, constants, operation results)
;;;   - Edges represent data dependencies
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Design:
;;;   - Nodes are content-addressed by their structure
;;;   - Graph is built incrementally during forward evaluation
;;;   - Supports both forward and reverse mode traversal
;;;   - Efficient topological sorting for gradient computation
;;;
;;; Dependencies:
;;;   - prelude.ss

(load "fabric/stitches/prelude.ss")

;;; ============================================================
;;; Node Types
;;; ============================================================

;;; A computational graph node is one of:
;;;   (var symbol)                   - Input variable
;;;   (const value)                  - Constant value
;;;   (op symbol (node ...))         - Operation with inputs

;;; node-var : Symbol → Node
;;; Create a variable node.
(define (node-var name)
  (list 'var name))

;;; node-const : Number → Node
;;; Create a constant node.
(define (node-const value)
  (list 'const value))

;;; node-op : Symbol × (List Node) → Node
;;; Create an operation node.
(define (node-op op-name inputs)
  (list 'op op-name inputs))

;;; ============================================================
;;; Node Predicates
;;; ============================================================

(define (node-var? n)
  (and (pair? n) (eq? (car n) 'var)))

(define (node-const? n)
  (and (pair? n) (eq? (car n) 'const)))

(define (node-op? n)
  (and (pair? n) (eq? (car n) 'op)))

(define (node? n)
  (or (node-var? n) (node-const? n) (node-op? n)))

;;; ============================================================
;;; Node Accessors
;;; ============================================================

;;; node-var-name : VarNode → Symbol
(define (node-var-name n)
  (cadr n))

;;; node-const-value : ConstNode → Number
(define (node-const-value n)
  (cadr n))

;;; node-op-name : OpNode → Symbol
(define (node-op-name n)
  (cadr n))

;;; node-op-inputs : OpNode → (List Node)
(define (node-op-inputs n)
  (caddr n))

;;; node-inputs : Node → (List Node)
;;; Get all input nodes (empty for var/const).
(define (node-inputs n)
  (cond
   [(node-var? n) '()]
   [(node-const? n) '()]
   [(node-op? n) (node-op-inputs n)]
   [else '()]))

;;; ============================================================
;;; Computational Graph Structure
;;; ============================================================

;;; A computational graph contains:
;;;   - nodes: association list of (id . node)
;;;   - output: the output node id (for single-output graphs)
;;;   - next-id: counter for generating unique ids

;;; make-comp-graph : → CompGraph
;;; Create an empty computational graph.
(define (make-comp-graph)
  (list 'comp-graph '() #f 0))

;;; comp-graph? : Any → Boolean
(define (comp-graph? g)
  (and (pair? g) (eq? (car g) 'comp-graph)))

;;; comp-graph-nodes : CompGraph → AList
(define (comp-graph-nodes g) (cadr g))

;;; comp-graph-output : CompGraph → NodeId | #f
(define (comp-graph-output g) (caddr g))

;;; comp-graph-next-id : CompGraph → Nat
(define (comp-graph-next-id g) (cadddr g))

;;; ============================================================
;;; Graph Construction (Functional Update)
;;; ============================================================

;;; graph-add-node : CompGraph × Node → (Values CompGraph NodeId)
;;; Add a node to the graph, returning updated graph and node id.
(define (graph-add-node g node)
  (let* ([id (comp-graph-next-id g)]
         [nodes (cons (cons id node) (comp-graph-nodes g))]
         [new-g (list 'comp-graph nodes (comp-graph-output g) (+ id 1))])
        (values new-g id)))

;;; graph-set-output : CompGraph × NodeId → CompGraph
;;; Set the output node of the graph.
(define (graph-set-output g output-id)
  (list 'comp-graph
        (comp-graph-nodes g)
        output-id
        (comp-graph-next-id g)))

;;; graph-get-node : CompGraph × NodeId → Node | #f
;;; Look up a node by id.
(define (graph-get-node g id)
  (let ([entry (assv id (comp-graph-nodes g))])
       (if entry (cdr entry) #f)))

;;; graph-node-count : CompGraph → Nat
(define (graph-node-count g)
  (length (comp-graph-nodes g)))

;;; ============================================================
;;; Graph Traversal
;;; ============================================================

;;; graph-topological-order : CompGraph → (List NodeId)
;;; Return node ids in topological order (inputs before outputs).
;;; Since we add nodes in order of construction, reverse gives topo order.
(define (graph-topological-order g)
  (map car (reverse (comp-graph-nodes g))))

;;; graph-reverse-order : CompGraph → (List NodeId)
;;; Return node ids in reverse topological order (outputs before inputs).
(define (graph-reverse-order g)
  (map car (comp-graph-nodes g)))

;;; ============================================================
;;; Graph Operations
;;; ============================================================

;;; graph-map-nodes : (NodeId × Node → a) × CompGraph → (List a)
;;; Apply function to each node.
(define (graph-map-nodes f g)
  (map (lambda (entry) (f (car entry) (cdr entry)))
       (comp-graph-nodes g)))

;;; graph-fold-nodes : (a × NodeId × Node → a) × a × CompGraph → a
;;; Fold over nodes.
(define (graph-fold-nodes f init g)
  (fold-left (lambda (acc entry) (f acc (car entry) (cdr entry)))
             init
             (comp-graph-nodes g)))

;;; graph-filter-nodes : (Node → Boolean) × CompGraph → (List (NodeId . Node))
;;; Filter nodes by predicate.
(define (graph-filter-nodes pred g)
  (filter (lambda (entry) (pred (cdr entry)))
          (comp-graph-nodes g)))

;;; graph-variables : CompGraph → (List Symbol)
;;; Get all variable names in the graph.
(define (graph-variables g)
  (map (lambda (entry) (node-var-name (cdr entry)))
       (graph-filter-nodes node-var? g)))

;;; graph-operations : CompGraph → (List Symbol)
;;; Get all operation types in the graph (with duplicates).
(define (graph-operations g)
  (map (lambda (entry) (node-op-name (cdr entry)))
       (graph-filter-nodes node-op? g)))

;;; ============================================================
;;; Dual Numbers for Forward Mode
;;; ============================================================

;;; A dual number represents a value and its derivative:
;;;   (dual value derivative)

;;; dual : Number × Number → Dual
(define (dual val deriv)
  (list 'dual val deriv))

;;; dual? : Any → Boolean
(define (dual? x)
  (and (pair? x) (eq? (car x) 'dual)))

;;; dual-value : Dual → Number
(define (dual-value d)
  (if (dual? d) (cadr d) d))

;;; dual-deriv : Dual → Number
(define (dual-deriv d)
  (if (dual? d) (caddr d) 0))

;;; lift : Number → Dual
;;; Lift a constant to a dual number (derivative = 0).
(define (dual-lift x)
  (if (dual? x) x (dual x 0)))

;;; dual-variable : Number → Dual
;;; Create a dual for a variable (derivative = 1).
(define (dual-variable x)
  (dual x 1))

;;; ============================================================
;;; Dual Number Arithmetic
;;; ============================================================

;;; dual-add : Dual × Dual → Dual
(define (dual-add a b)
  (let ([a (dual-lift a)]
        [b (dual-lift b)])
       (dual (+ (dual-value a) (dual-value b))
             (+ (dual-deriv a) (dual-deriv b)))))

;;; dual-sub : Dual × Dual → Dual
(define (dual-sub a b)
  (let ([a (dual-lift a)]
        [b (dual-lift b)])
       (dual (- (dual-value a) (dual-value b))
             (- (dual-deriv a) (dual-deriv b)))))

;;; dual-mul : Dual × Dual → Dual
;;; Product rule: d(fg)/dx = f'g + fg'
(define (dual-mul a b)
  (let ([a (dual-lift a)]
        [b (dual-lift b)])
       (dual (* (dual-value a) (dual-value b))
             (+ (* (dual-deriv a) (dual-value b))
                (* (dual-value a) (dual-deriv b))))))

;;; dual-div : Dual × Dual → Dual
;;; Quotient rule: d(f/g)/dx = (f'g - fg')/g²
(define (dual-div a b)
  (let* ([a (dual-lift a)]
         [b (dual-lift b)]
         [fv (dual-value a)]
         [fd (dual-deriv a)]
         [gv (dual-value b)]
         [gd (dual-deriv b)])
        (dual (/ fv gv)
              (/ (- (* fd gv) (* fv gd))
                 (* gv gv)))))

;;; dual-neg : Dual → Dual
(define (dual-neg a)
  (let ([a (dual-lift a)])
       (dual (- (dual-value a))
             (- (dual-deriv a)))))

;;; dual-recip : Dual → Dual
;;; d(1/x)/dx = -1/x²
(define (dual-recip a)
  (let ([a (dual-lift a)])
       (dual (/ 1 (dual-value a))
             (/ (- (dual-deriv a))
                (* (dual-value a) (dual-value a))))))

;;; dual-sq : Dual → Dual
;;; d(x²)/dx = 2x
(define (dual-sq a)
  (let ([a (dual-lift a)])
       (dual (* (dual-value a) (dual-value a))
             (* 2 (dual-value a) (dual-deriv a)))))

;;; dual-sqrt : Dual → Dual
;;; d(√x)/dx = 1/(2√x)
(define (dual-sqrt a)
  (let* ([a (dual-lift a)]
         [v (dual-value a)]
         [s (sqrt v)])
        (dual s (/ (dual-deriv a) (* 2 s)))))

;;; dual-exp : Dual → Dual
;;; d(e^x)/dx = e^x
(define (dual-exp a)
  (let* ([a (dual-lift a)]
         [e (exp (dual-value a))])
        (dual e (* e (dual-deriv a)))))

;;; dual-log : Dual → Dual
;;; d(ln x)/dx = 1/x
(define (dual-log a)
  (let ([a (dual-lift a)])
       (dual (log (dual-value a))
             (/ (dual-deriv a) (dual-value a)))))

;;; dual-sin : Dual → Dual
;;; d(sin x)/dx = cos x
(define (dual-sin a)
  (let ([a (dual-lift a)])
       (dual (sin (dual-value a))
             (* (cos (dual-value a)) (dual-deriv a)))))

;;; dual-cos : Dual → Dual
;;; d(cos x)/dx = -sin x
(define (dual-cos a)
  (let ([a (dual-lift a)])
       (dual (cos (dual-value a))
             (* (- (sin (dual-value a))) (dual-deriv a)))))

;;; dual-tan : Dual → Dual
;;; d(tan x)/dx = sec²x = 1/cos²x
(define (dual-tan a)
  (let* ([a (dual-lift a)]
         [c (cos (dual-value a))])
        (dual (tan (dual-value a))
              (/ (dual-deriv a) (* c c)))))

;;; dual-pow : Dual × Number → Dual
;;; d(x^n)/dx = n*x^(n-1)
(define (dual-pow base exp)
  (let ([base (dual-lift base)]
        [n exp])
       (dual (expt (dual-value base) n)
             (* n
                (expt (dual-value base) (- n 1))
                (dual-deriv base)))))

;;; ============================================================
;;; Forward Mode Differentiation
;;; ============================================================

;;; forward-diff : (Number → Number) × Number → Number
;;; Compute derivative of f at x using forward mode.
(define (forward-diff f x)
  (dual-deriv (f (dual-variable x))))

;;; gradient-forward : (List Number → Number) × (List Number) × Nat → Number
;;; Compute partial derivative with respect to variable i.
(define (gradient-forward f args i)
  (let ([dual-args
         (let loop ([as args] [j 0])
              (if (null? as)
                  '()
                  (cons (if (= j i)
                            (dual-variable (car as))
                            (dual-lift (car as)))
                        (loop (cdr as) (+ j 1)))))])
       (dual-deriv (apply f dual-args))))

;;; gradient-forward-all : (List Number → Number) × (List Number) → (List Number)
;;; Compute full gradient (all partial derivatives).
(define (gradient-forward-all f args)
  (let loop ([i 0])
       (if (>= i (length args))
           '()
           (cons (gradient-forward f args i)
                 (loop (+ i 1))))))

;;; ============================================================
;;; Gradient Tape (for Reverse Mode)
;;; ============================================================

;;; A tape records operations for reverse-mode differentiation.
;;; Each entry: (result-id op input-ids local-gradients)

;;; make-tape : → Tape
(define (make-tape)
  (list 'tape '()))

;;; tape? : Any → Boolean
(define (tape? t)
  (and (pair? t) (eq? (car t) 'tape)))

;;; tape-entries : Tape → (List TapeEntry)
(define (tape-entries t) (cadr t))

;;; tape-record : Tape × Symbol × (List NodeId) × Number × (List Number) → Tape
;;; Record an operation. local-gradients are ∂result/∂input for each input.
(define (tape-record tape result-id op input-ids local-gradients)
  (list 'tape
        (cons (list result-id op input-ids local-gradients)
              (tape-entries tape))))

;;; tape-reverse-pass : Tape × NodeId × Number → AList
;;; Perform reverse pass starting from output-id with grad-seed.
;;; Returns association list of (node-id . gradient).
(define (tape-reverse-pass tape output-id grad-seed)
  (let ([grads (make-hashtable equal-hash equal?)])
       ;; Initialize output gradient
       (hashtable-set! grads output-id grad-seed)
       ;; Process tape in reverse order
       (for-each
        (lambda (entry)
                (let* ([result-id (car entry)]
                       [input-ids (caddr entry)]
                       [local-grads (cadddr entry)]
                       [result-grad (hashtable-ref grads result-id 0)])
                      ;; Accumulate gradients to inputs
                      (for-each
                       (lambda (input-id local-grad)
                               (let ([current (hashtable-ref grads input-id 0)])
                                    (hashtable-set! grads input-id
                                                    (+ current (* result-grad local-grad)))))
                       input-ids local-grads)))
        (tape-entries tape))
       ;; Convert to alist
       (let-values ([(keys vals) (hashtable-entries grads)])
                   (map cons (vector->list keys) (vector->list vals)))))

;;; ============================================================
;;; Graph Printing
;;; ============================================================

;;; graph-print : CompGraph → Void
;;; Print a human-readable representation of the graph.
(define (graph-print g)
  (printf "Computational Graph (~a nodes):~n" (graph-node-count g))
  (for-each
   (lambda (entry)
           (let ([id (car entry)]
                 [node (cdr entry)])
                (printf "  [~a] " id)
                (cond
                 [(node-var? node)
                  (printf "var ~a~n" (node-var-name node))]
                 [(node-const? node)
                  (printf "const ~a~n" (node-const-value node))]
                 [(node-op? node)
                  (printf "~a (...)~n" (node-op-name node))])))
   (reverse (comp-graph-nodes g)))
  (when (comp-graph-output g)
        (printf "  Output: [~a]~n" (comp-graph-output g))))
