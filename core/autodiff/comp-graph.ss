;;; core/autodiff/comp-graph.ss --- Computational Graph for Autodiff
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

;;; @module comp-graph
;;; @requires prelude

(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
(require 'prelude)

;;; ====
;;; Node Types
;;; ====

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

;;; ====
;;; Node Predicates
;;; ====

;;; node-var? : α → Bool
(define (node-var? n)
  (and (pair? n) (eq? (car n) 'var)))

;;; node-const? : α → Bool
(define (node-const? n)
  (and (pair? n) (eq? (car n) 'const)))

;;; node-op? : α → Bool
(define (node-op? n)
  (and (pair? n) (eq? (car n) 'op)))

;;; node? : α → Bool
(define (node? n)
  (or (node-var? n) (node-const? n) (node-op? n)))

;;; ====
;;; Node Accessors
;;; ====

;;; node-var-name : Node → Symbol
(define (node-var-name n)
  (cadr n))

;;; node-const-value : Node → Number
(define (node-const-value n)
  (cadr n))

;;; node-op-name : Node → Symbol
(define (node-op-name n)
  (cadr n))

;;; node-op-inputs : Node → (List Node)
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

;;; ====
;;; Computational Graph Structure
;;; ====

;;; A computational graph contains:
;;;   - nodes: association list of (id . node)
;;;   - output: the output node id (for single-output graphs)
;;;   - next-id: counter for generating unique ids

;;; make-comp-graph : Unit → CompGraph
;;; Create an empty computational graph.
(define (make-comp-graph)
  (list 'comp-graph '() #f 0))

;;; comp-graph? : α → Bool
(define (comp-graph? g)
  (and (pair? g) (eq? (car g) 'comp-graph)))

;;; comp-graph-nodes : CompGraph → (List (Pair Nat Node))
(define (comp-graph-nodes g) (cadr g))

;;; comp-graph-output : CompGraph → Nat
(define (comp-graph-output g) (caddr g))

;;; comp-graph-next-id : CompGraph → Nat
(define (comp-graph-next-id g) (cadddr g))

;;; ====
;;; Graph Construction (Functional Update)
;;; ====

;;; graph-add-node : CompGraph × Node → (Values CompGraph Nat)
;;; Add a node to the graph, returning updated graph and node id.
(define (graph-add-node g node)
  (let* ([id (comp-graph-next-id g)]
         [nodes (cons (cons id node) (comp-graph-nodes g))]
         [new-g (list 'comp-graph nodes (comp-graph-output g) (+ id 1))])
        (values new-g id)))

;;; graph-set-output : CompGraph × Nat → CompGraph
;;; Set the output node of the graph.
(define (graph-set-output g output-id)
  (list 'comp-graph
        (comp-graph-nodes g)
        output-id
        (comp-graph-next-id g)))

;;; graph-get-node : CompGraph × Nat → Node
;;; Look up a node by id.
(define (graph-get-node g id)
  (let ([entry (assv id (comp-graph-nodes g))])
       (if entry (cdr entry) #f)))

;;; graph-node-count : CompGraph → Nat
(define (graph-node-count g)
  (length (comp-graph-nodes g)))

;;; ====
;;; Graph Traversal
;;; ====

;;; graph-topological-order : CompGraph → (List Nat)
;;; Return node ids in topological order (inputs before outputs).
;;; Since we add nodes in order of construction, reverse gives topo order.
(define (graph-topological-order g)
  (map car (reverse (comp-graph-nodes g))))

;;; graph-reverse-order : CompGraph → (List Nat)
;;; Return node ids in reverse topological order (outputs before inputs).
(define (graph-reverse-order g)
  (map car (comp-graph-nodes g)))

;;; ====
;;; Graph Operations
;;; ====

;;; graph-map-nodes : (Nat × Node → α) × CompGraph → (List α)
;;; Apply function to each node.
(define (graph-map-nodes f g)
  (map (lambda (entry) (f (car entry) (cdr entry)))
       (comp-graph-nodes g)))

;;; graph-fold-nodes : (α × Nat × Node → α) × α × CompGraph → α
;;; Fold over nodes.
(define (graph-fold-nodes f init g)
  (fold-left (lambda (acc entry) (f acc (car entry) (cdr entry)))
             init
             (comp-graph-nodes g)))

;;; graph-filter-nodes : (Node → Bool) × CompGraph → (List (Pair Nat Node))
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

;;; ====
;;; Dual Numbers for Forward Mode
;;; ====

;;; A dual number represents a value and its derivative:
;;;   (dual value derivative)

;;; dual : Number × Number → Dual
(define (dual val deriv)
  (list 'dual val deriv))

;;; dual? : α → Bool
(define (dual? x)
  (and (pair? x) (eq? (car x) 'dual)))

;;; dual-value : Dual → Number
(define (dual-value d)
  (if (dual? d) (cadr d) d))

;;; dual-deriv : Dual → Number
(define (dual-deriv d)
  (if (dual? d) (caddr d) 0))

;;; dual-lift : Number → Dual
;;; Lift a constant to a dual number (derivative = 0).
(define (dual-lift x)
  (if (dual? x) x (dual x 0)))

;;; dual-variable : Number → Dual
;;; Create a dual for a variable (derivative = 1).
(define (dual-variable x)
  (dual x 1))

;;; ====
;;; Dual Number Arithmetic
;;; ====

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
;;; Quotient rule: d(f/g)/dx = (f'g - fg')/g^2
(define (dual-div a b)
  (let* ([a (dual-lift a)]
         [b (dual-lift b)]
         [fv (dual-value a)]
         [fd (dual-deriv a)]
         [gv (dual-value b)]
         [gd (dual-deriv b)])
        ;; Guard against division by zero
        (if (= gv 0)
            (dual +nan.0 +nan.0)
            (dual (/ fv gv)
                  (/ (- (* fd gv) (* fv gd))
                     (* gv gv))))))

;;; dual-neg : Dual → Dual
(define (dual-neg a)
  (let ([a (dual-lift a)])
       (dual (- (dual-value a))
             (- (dual-deriv a)))))

;;; dual-recip : Dual → Dual
;;; d(1/x)/dx = -1/x^2
(define (dual-recip a)
  (let ([a (dual-lift a)])
       (dual (/ 1 (dual-value a))
             (/ (- (dual-deriv a))
                (* (dual-value a) (dual-value a))))))

;;; dual-sq : Dual → Dual
;;; d(x^2)/dx = 2x
(define (dual-sq a)
  (let ([a (dual-lift a)])
       (dual (* (dual-value a) (dual-value a))
             (* 2 (dual-value a) (dual-deriv a)))))

;;; dual-sqrt : Dual → Dual
;;; d(sqrt(x))/dx = 1/(2*sqrt(x))
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
  (let* ([a (dual-lift a)]
         [v (dual-value a)])
        ;; Guard against log of non-positive values
        (if (<= v 0)
            (dual +nan.0 +nan.0)
            (dual (log v)
                  (/ (dual-deriv a) v)))))

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
;;; d(tan x)/dx = sec^2(x) = 1/cos^2(x)
(define (dual-tan a)
  (let* ([a (dual-lift a)]
         [c (cos (dual-value a))])
        (dual (tan (dual-value a))
              (/ (dual-deriv a) (* c c)))))

;;; dual-pow : Dual × Number → Dual
;;; d(x^n)/dx = n*x^(n-1)
(define (dual-pow base exp)
  (let* ([base (dual-lift base)]
         [n exp]
         [bv (dual-value base)])
        ;; Guard against zero base with fractional/negative exponents
        (if (and (= bv 0) (< n 1))
            (dual +nan.0 +nan.0)  ; Undefined derivative at 0 for n < 1
            (dual (expt bv n)
                  (* n
                     (expt bv (- n 1))
                     (dual-deriv base))))))

;;; ====
;;; Inverse Trigonometric Functions
;;; ====

;;; dual-asin : Dual → Dual
;;; d(asin x)/dx = 1/sqrt(1-x^2)
(define (dual-asin a)
  (let* ([a (dual-lift a)]
         [v (dual-value a)]
         [one-v2 (- 1 (* v v))]
         ;; Add epsilon for boundary cases
         [denom (sqrt (max one-v2 1e-15))])
        (dual (asin v)
              (/ (dual-deriv a) denom))))

;;; dual-acos : Dual → Dual
;;; d(acos x)/dx = -1/sqrt(1-x^2)
(define (dual-acos a)
  (let* ([a (dual-lift a)]
         [v (dual-value a)]
         [one-v2 (- 1 (* v v))]
         ;; Add epsilon for boundary cases
         [denom (sqrt (max one-v2 1e-15))])
        (dual (acos v)
              (/ (- (dual-deriv a)) denom))))

;;; dual-atan : Dual → Dual
;;; d(atan x)/dx = 1/(1+x^2)
(define (dual-atan a)
  (let* ([a (dual-lift a)]
         [v (dual-value a)])
        (dual (atan v)
              (/ (dual-deriv a) (+ 1 (* v v))))))

;;; ====
;;; Hyperbolic Functions
;;; ====

;;; dual-sinh : Dual → Dual
;;; d(sinh x)/dx = cosh x
(define (dual-sinh a)
  (let* ([a (dual-lift a)]
         [v (dual-value a)]
         ;; sinh = (e^x - e^-x)/2, cosh = (e^x + e^-x)/2
         [ex (exp v)]
         [emx (exp (- v))])
        (dual (/ (- ex emx) 2)
              (* (/ (+ ex emx) 2) (dual-deriv a)))))

;;; dual-cosh : Dual → Dual
;;; d(cosh x)/dx = sinh x
(define (dual-cosh a)
  (let* ([a (dual-lift a)]
         [v (dual-value a)]
         [ex (exp v)]
         [emx (exp (- v))])
        (dual (/ (+ ex emx) 2)
              (* (/ (- ex emx) 2) (dual-deriv a)))))

;;; dual-tanh : Dual → Dual
;;; d(tanh x)/dx = sech^2(x) = 1/cosh^2(x)
(define (dual-tanh a)
  (let* ([a (dual-lift a)]
         [v (dual-value a)]
         [ex (exp v)]
         [emx (exp (- v))]
         [cosh-v (/ (+ ex emx) 2)])
        (dual (/ (- ex emx) (+ ex emx))
              (/ (dual-deriv a) (* cosh-v cosh-v)))))

;;; ====
;;; Forward Mode Differentiation
;;; ====

;;; forward-diff : (Number → Number) × Number → Number
;;; Compute derivative of f at x using forward mode.
(define (forward-diff f x)
  (dual-deriv (f (dual-variable x))))

;;; gradient-forward : (Number ... → Number) × (List Number) × Nat → Number
;;; Compute partial derivative with respect to variable i (f called via apply).
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

;;; gradient-forward-all : (Number ... → Number) × (List Number) → (List Number)
;;; Compute full gradient (all partial derivatives).
;;; f is called via apply — it takes N individual args, not a list.
(define (gradient-forward-all f args)
  (let loop ([i 0])
       (if (>= i (length args))
           '()
           (cons (gradient-forward f args i)
                 (loop (+ i 1))))))

;;; ====
;;; Hyperdual Numbers for Second Derivatives
;;; ====

;;; A hyperdual number represents: a + b*e1 + c*e2 + d*e1e2
;;; where e1^2 = e2^2 = 0 (nilpotent) but e1e2 != 0.
;;;
;;; For f(x,y), evaluating at hyperdual point gives:
;;;   f(a + e1, b + e2) = f(a,b) + (df/dx)e1 + (df/dy)e2 + (d^2f/dxdy)e1e2
;;;
;;; This allows exact computation of mixed second partial derivatives.

;;; hyperdual : Number × Number × Number × Number → Hyperdual
;;; Create hyperdual a + b*e1 + c*e2 + d*e1e2
(define (hyperdual val d1 d2 d12)
  (list 'hyperdual val d1 d2 d12))

;;; hyperdual? : α → Bool
(define (hyperdual? x)
  (and (pair? x) (eq? (car x) 'hyperdual)))

;;; hd-value : Hyperdual → Number
(define (hd-value h)   (if (hyperdual? h) (cadr h) h))

;;; hd-deriv1 : Hyperdual → Number
(define (hd-deriv1 h)  (if (hyperdual? h) (caddr h) 0))

;;; hd-deriv2 : Hyperdual → Number
(define (hd-deriv2 h)  (if (hyperdual? h) (cadddr h) 0))

;;; hd-deriv12 : Hyperdual → Number
(define (hd-deriv12 h) (if (hyperdual? h) (car (cddddr h)) 0))

;;; hd-lift : Number → Hyperdual
;;; Lift a constant (all derivatives = 0).
(define (hd-lift x)
  (if (hyperdual? x) x (hyperdual x 0 0 0)))

;;; hd-var1 : Number → Hyperdual
;;; Variable with e1 = 1 (for differentiating w.r.t. first variable).
(define (hd-var1 x)
  (hyperdual x 1 0 0))

;;; hd-var2 : Number → Hyperdual
;;; Variable with e2 = 1 (for differentiating w.r.t. second variable).
(define (hd-var2 x)
  (hyperdual x 0 1 0))

;;; hd-var12 : Number → Hyperdual
;;; Variable with both e1 = e2 = 1 (for computing diagonal Hessian entry).
(define (hd-var12 x)
  (hyperdual x 1 1 0))

;;; ====
;;; Hyperdual Arithmetic
;;; ====

;;; hd-add : Hyperdual × Hyperdual → Hyperdual
(define (hd-add a b)
  (let ([a (hd-lift a)]
        [b (hd-lift b)])
       (hyperdual (+ (hd-value a) (hd-value b))
                  (+ (hd-deriv1 a) (hd-deriv1 b))
                  (+ (hd-deriv2 a) (hd-deriv2 b))
                  (+ (hd-deriv12 a) (hd-deriv12 b)))))

;;; hd-sub : Hyperdual × Hyperdual → Hyperdual
(define (hd-sub a b)
  (let ([a (hd-lift a)]
        [b (hd-lift b)])
       (hyperdual (- (hd-value a) (hd-value b))
                  (- (hd-deriv1 a) (hd-deriv1 b))
                  (- (hd-deriv2 a) (hd-deriv2 b))
                  (- (hd-deriv12 a) (hd-deriv12 b)))))

;;; hd-mul : Hyperdual × Hyperdual → Hyperdual
;;; (a + b*e1 + c*e2 + d*e1e2) * (a' + b'*e1 + c'*e2 + d'*e1e2)
;;; = aa' + (ab'+ba')e1 + (ac'+ca')e2 + (ad'+da'+bc'+cb')e1e2
(define (hd-mul a b)
  (let* ([a (hd-lift a)]
         [b (hd-lift b)]
         [av (hd-value a)]   [bv (hd-value b)]
         [a1 (hd-deriv1 a)]  [b1 (hd-deriv1 b)]
         [a2 (hd-deriv2 a)]  [b2 (hd-deriv2 b)]
         [a12 (hd-deriv12 a)] [b12 (hd-deriv12 b)])
        (hyperdual (* av bv)
                   (+ (* av b1) (* a1 bv))
                   (+ (* av b2) (* a2 bv))
                   (+ (* av b12) (* a12 bv) (* a1 b2) (* a2 b1)))))

;;; hd-div : Hyperdual × Hyperdual → Hyperdual
;;; Uses quotient rule and chain rule.
(define (hd-div a b)
  (let* ([a (hd-lift a)]
         [b (hd-lift b)]
         [av (hd-value a)]   [bv (hd-value b)]
         [a1 (hd-deriv1 a)]  [b1 (hd-deriv1 b)]
         [a2 (hd-deriv2 a)]  [b2 (hd-deriv2 b)]
         [a12 (hd-deriv12 a)] [b12 (hd-deriv12 b)])
        ;; Guard against division by zero
        (if (= bv 0)
            (hyperdual +nan.0 +nan.0 +nan.0 +nan.0)
            (let* ([bv2 (* bv bv)]
                   [bv3 (* bv2 bv)])
                  (hyperdual (/ av bv)
                             (/ (- (* a1 bv) (* av b1)) bv2)
                             (/ (- (* a2 bv) (* av b2)) bv2)
                             (/ (- (+ (* a12 bv2)
                                      (* 2 av b1 b2))
                                   (+ (* av b12 bv)
                                      (* a1 b2 bv)
                                      (* a2 b1 bv)))
                                bv3))))))

;;; hd-neg : Hyperdual → Hyperdual
(define (hd-neg a)
  (let ([a (hd-lift a)])
       (hyperdual (- (hd-value a))
                  (- (hd-deriv1 a))
                  (- (hd-deriv2 a))
                  (- (hd-deriv12 a)))))

;;; hd-sq : Hyperdual → Hyperdual
(define (hd-sq a)
  (hd-mul a a))

;;; hd-sqrt : Hyperdual → Hyperdual
;;; d(sqrt(x))/dx = 1/(2*sqrt(x)), d^2(sqrt(x))/dx^2 = -1/(4x^(3/2))
(define (hd-sqrt a)
  (let* ([a (hd-lift a)]
         [av (hd-value a)]
         [sv (sqrt av)]
         [a1 (hd-deriv1 a)]
         [a2 (hd-deriv2 a)]
         [a12 (hd-deriv12 a)]
         [d1 (/ a1 (* 2 sv))]
         [d2 (/ a2 (* 2 sv))]
         ;; d^2(sqrt(f))/dxdy = (f12)/(2*sqrt(f)) - (f1*f2)/(4*f^(3/2))
         [d12 (- (/ a12 (* 2 sv))
                 (/ (* a1 a2) (* 4 av sv)))])
        (hyperdual sv d1 d2 d12)))

;;; hd-exp : Hyperdual → Hyperdual
;;; d(e^f)/dx = f'e^f, d^2(e^f)/dxdy = (f12 + f1*f2)e^f
(define (hd-exp a)
  (let* ([a (hd-lift a)]
         [av (hd-value a)]
         [ev (exp av)]
         [a1 (hd-deriv1 a)]
         [a2 (hd-deriv2 a)]
         [a12 (hd-deriv12 a)])
        (hyperdual ev
                   (* a1 ev)
                   (* a2 ev)
                   (* (+ a12 (* a1 a2)) ev))))

;;; hd-log : Hyperdual → Hyperdual
;;; d(ln f)/dx = f'/f, d^2(ln f)/dxdy = (f12*f - f1*f2)/f^2
(define (hd-log a)
  (let* ([a (hd-lift a)]
         [av (hd-value a)]
         [a1 (hd-deriv1 a)]
         [a2 (hd-deriv2 a)]
         [a12 (hd-deriv12 a)])
        ;; Guard against log of non-positive values
        (if (<= av 0)
            (hyperdual +nan.0 +nan.0 +nan.0 +nan.0)
            (hyperdual (log av)
                       (/ a1 av)
                       (/ a2 av)
                       (/ (- (* a12 av) (* a1 a2)) (* av av))))))

;;; hd-sin : Hyperdual → Hyperdual
;;; d(sin f)/dx = f'cos f
;;; d^2(sin f)/dxdy = f12 cos f - f1*f2 sin f
(define (hd-sin a)
  (let* ([a (hd-lift a)]
         [av (hd-value a)]
         [sv (sin av)]
         [cv (cos av)]
         [a1 (hd-deriv1 a)]
         [a2 (hd-deriv2 a)]
         [a12 (hd-deriv12 a)])
        (hyperdual sv
                   (* a1 cv)
                   (* a2 cv)
                   (- (* a12 cv) (* a1 a2 sv)))))

;;; hd-cos : Hyperdual → Hyperdual
;;; d(cos f)/dx = -f'sin f
;;; d^2(cos f)/dxdy = -f12 sin f - f1*f2 cos f
(define (hd-cos a)
  (let* ([a (hd-lift a)]
         [av (hd-value a)]
         [sv (sin av)]
         [cv (cos av)]
         [a1 (hd-deriv1 a)]
         [a2 (hd-deriv2 a)]
         [a12 (hd-deriv12 a)])
        (hyperdual cv
                   (- (* a1 sv))
                   (- (* a2 sv))
                   (- (- (* a12 sv)) (* a1 a2 cv)))))

;;; hd-pow : Hyperdual × Number → Hyperdual
;;; d(f^n)/dx = n*f^(n-1)*f'
;;; d^2(f^n)/dxdy = n*f^(n-2)*(f12*f + (n-1)*f1*f2)
(define (hd-pow base exp)
  (let* ([b (hd-lift base)]
         [n exp]
         [bv (hd-value b)]
         [b1 (hd-deriv1 b)]
         [b2 (hd-deriv2 b)]
         [b12 (hd-deriv12 b)])
        (cond
         ;; Guard against zero base with fractional/negative exponents
         [(and (= bv 0) (< n 1))
          (hyperdual +nan.0 +nan.0 +nan.0 +nan.0)]
         ;; Special case n=1 at zero base: d²(f)/dxdy = f12
         [(and (= bv 0) (= n 1))
          (hyperdual 0 b1 b2 b12)]
         ;; Special case n=2 at zero base: avoid pv2 = 0^0 ambiguity
         [(and (= bv 0) (= n 2))
          (hyperdual 0 0 0 (* 2 b1 b2))]
         ;; General case
         [else
          (let* ([pv (expt bv n)]
                 [pv1 (expt bv (- n 1))]
                 [pv2 (if (>= n 2) (expt bv (- n 2)) 0)])
                (hyperdual pv
                           (* n pv1 b1)
                           (* n pv1 b2)
                           (* n pv2 (+ (* b12 bv) (* (- n 1) b1 b2)))))])))

;;; ====
;;; Hyperdual Inverse Trigonometric Functions
;;; ====

;;; hd-asin : Hyperdual → Hyperdual
;;; d(asin f)/dx = f'/sqrt(1-f^2)
;;; d^2(asin f)/dxdy = (f12*(1-f^2) + f*f1*f2) / (1-f^2)^(3/2)
(define (hd-asin a)
  (let* ([a (hd-lift a)]
         [av (hd-value a)]
         [a1 (hd-deriv1 a)]
         [a2 (hd-deriv2 a)]
         [a12 (hd-deriv12 a)]
         [one-v2 (- 1 (* av av))]
         ;; Add epsilon for boundary cases
         [one-v2-safe (max one-v2 1e-15)]
         [sqrt-one-v2 (sqrt one-v2-safe)])
        (hyperdual (asin av)
                   (/ a1 sqrt-one-v2)
                   (/ a2 sqrt-one-v2)
                   (/ (+ (* a12 one-v2-safe) (* av a1 a2))
                      (* one-v2-safe sqrt-one-v2)))))

;;; hd-acos : Hyperdual → Hyperdual
;;; d(acos f)/dx = -f'/sqrt(1-f^2)
;;; d^2(acos f)/dxdy = -(f12*(1-f^2) + f*f1*f2) / (1-f^2)^(3/2)
(define (hd-acos a)
  (let* ([a (hd-lift a)]
         [av (hd-value a)]
         [a1 (hd-deriv1 a)]
         [a2 (hd-deriv2 a)]
         [a12 (hd-deriv12 a)]
         [one-v2 (- 1 (* av av))]
         ;; Add epsilon for boundary cases
         [one-v2-safe (max one-v2 1e-15)]
         [sqrt-one-v2 (sqrt one-v2-safe)])
        (hyperdual (acos av)
                   (/ (- a1) sqrt-one-v2)
                   (/ (- a2) sqrt-one-v2)
                   (/ (- (+ (* a12 one-v2-safe) (* av a1 a2)))
                      (* one-v2-safe sqrt-one-v2)))))

;;; hd-atan : Hyperdual → Hyperdual
;;; d(atan f)/dx = f'/(1+f^2)
;;; d^2(atan f)/dxdy = (f12*(1+f^2) - 2*f*f1*f2) / (1+f^2)^2
(define (hd-atan a)
  (let* ([a (hd-lift a)]
         [av (hd-value a)]
         [a1 (hd-deriv1 a)]
         [a2 (hd-deriv2 a)]
         [a12 (hd-deriv12 a)]
         [one-v2 (+ 1 (* av av))])
        (hyperdual (atan av)
                   (/ a1 one-v2)
                   (/ a2 one-v2)
                   (/ (- (* a12 one-v2) (* 2 av a1 a2))
                      (* one-v2 one-v2)))))

;;; ====
;;; Hyperdual Hyperbolic Functions
;;; ====

;;; hd-sinh : Hyperdual → Hyperdual
;;; d(sinh f)/dx = f'*cosh(f)
;;; d^2(sinh f)/dxdy = (f12*cosh(f) + f1*f2*sinh(f))
(define (hd-sinh a)
  (let* ([a (hd-lift a)]
         [av (hd-value a)]
         [ex (exp av)]
         [emx (exp (- av))]
         [sv (/ (- ex emx) 2)]
         [cv (/ (+ ex emx) 2)]
         [a1 (hd-deriv1 a)]
         [a2 (hd-deriv2 a)]
         [a12 (hd-deriv12 a)])
        (hyperdual sv
                   (* a1 cv)
                   (* a2 cv)
                   (+ (* a12 cv) (* a1 a2 sv)))))

;;; hd-cosh : Hyperdual → Hyperdual
;;; d(cosh f)/dx = f'*sinh(f)
;;; d^2(cosh f)/dxdy = (f12*sinh(f) + f1*f2*cosh(f))
(define (hd-cosh a)
  (let* ([a (hd-lift a)]
         [av (hd-value a)]
         [ex (exp av)]
         [emx (exp (- av))]
         [sv (/ (- ex emx) 2)]
         [cv (/ (+ ex emx) 2)]
         [a1 (hd-deriv1 a)]
         [a2 (hd-deriv2 a)]
         [a12 (hd-deriv12 a)])
        (hyperdual cv
                   (* a1 sv)
                   (* a2 sv)
                   (+ (* a12 sv) (* a1 a2 cv)))))

;;; hd-tanh : Hyperdual → Hyperdual
;;; d(tanh f)/dx = f'*sech^2(f) = f'/cosh^2(f)
;;; d^2(tanh f)/dxdy = (f12 - 2*tanh(f)*f1*f2) / cosh^2(f)
(define (hd-tanh a)
  (let* ([a (hd-lift a)]
         [av (hd-value a)]
         [ex (exp av)]
         [emx (exp (- av))]
         [tv (/ (- ex emx) (+ ex emx))]
         [cv (/ (+ ex emx) 2)]
         [sech2 (/ 1 (* cv cv))]
         [a1 (hd-deriv1 a)]
         [a2 (hd-deriv2 a)]
         [a12 (hd-deriv12 a)])
        (hyperdual tv
                   (* a1 sech2)
                   (* a2 sech2)
                   (* (- a12 (* 2 tv a1 a2)) sech2))))

;;; ====
;;; Hessian via Hyperdual Numbers
;;; ====

;;; hessian-forward : (Hyperdual ... → Hyperdual) × (List Number) → Matrix
;;; Compute exact Hessian using hyperdual numbers.
;;; H[i,j] = d^2f/dx_i*dx_j is the e1e2 coefficient when
;;; x_i has e1=1 and x_j has e2=1.
(define (hessian-forward f args)
  (let* ([n (length args)]
         [result (make-vector (* n n) 0)])
        (do ([i 0 (+ i 1)])
            ((= i n) (list 'matrix n n result))
            (do ([j i (+ j 1)])  ; Only compute upper triangle (Hessian is symmetric)
                ((= j n))
                (let* ([hd-args (let loop ([xs args] [k 0])
                                     (if (null? xs)
                                         '()
                                         (cons (cond
                                                [(and (= k i) (= k j)) (hd-var12 (car xs))]  ; diagonal
                                                [(= k i) (hd-var1 (car xs))]
                                                [(= k j) (hd-var2 (car xs))]
                                                [else (hd-lift (car xs))])
                                               (loop (cdr xs) (+ k 1)))))]
                       [result-hd (apply f hd-args)]
                       [h-ij (hd-deriv12 result-hd)])
                      ;; Set both H[i,j] and H[j,i] (symmetric)
                      (vector-set! result (+ (* i n) j) h-ij)
                      (when (not (= i j))
                            (vector-set! result (+ (* j n) i) h-ij)))))))

;;; second-derivative-forward : (Hyperdual → Hyperdual) × Number → Number
;;; Compute d^2f/dx^2 at a point using hyperdual numbers.
(define (second-derivative-forward f x)
  (hd-deriv12 (f (hd-var12 x))))

;;; ====
;;; Gradient Tape (for Reverse Mode)
;;; ====

;;; A tape records operations for reverse-mode differentiation.
;;; Each entry: (result-id op input-ids local-gradients)

;;; make-tape : Unit → Tape
(define (make-tape)
  (list 'tape '()))

;;; tape? : α → Bool
(define (tape? t)
  (and (pair? t) (eq? (car t) 'tape)))

;;; tape-entries : Tape → (List α)
(define (tape-entries t) (cadr t))

;;; tape-record : Tape × Nat × Symbol × (List Nat) × (List Number) → Tape
;;; Record an operation. local-gradients are dresult/dinput for each input.
(define (tape-record tape result-id op input-ids local-gradients)
  (list 'tape
        (cons (list result-id op input-ids local-gradients)
              (tape-entries tape))))

;;; tape-reverse-pass : Tape × Nat × Number → (List (Pair Nat Number))
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

;;; ====
;;; Graph Printing
;;; ====

;;; graph-print : CompGraph → Unit
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

;;; ====
;;; Graphviz Export
;;; ====

;;; graph-to-dot : CompGraph → String
;;; Generate DOT format representation for Graphviz visualization.
;;; The output can be rendered with: dot -Tsvg graph.dot -o graph.svg
(define (graph-to-dot g)
  (let ([lines '()])
       ;; Helper to accumulate lines
       (define (emit line)
         (set! lines (cons line lines)))
       
       ;; Header
       (emit "digraph CompGraph {")
       (emit "  rankdir=BT;")  ; Bottom to top (inputs at bottom)
       (emit "  node [fontname=\"monospace\"];")
       (emit "")
       
       ;; Node definitions with styling
       (for-each
        (lambda (entry)
                (let ([id (car entry)]
                      [node (cdr entry)])
                     (cond
                      [(node-var? node)
                       (emit (format "  n~a [label=\"~a\" shape=ellipse style=filled fillcolor=lightblue];"
                                     id (node-var-name node)))]
                      [(node-const? node)
                       (emit (format "  n~a [label=\"~a\" shape=box style=filled fillcolor=lightgray];"
                                     id (node-const-value node)))]
                      [(node-op? node)
                       (emit (format "  n~a [label=\"~a\" shape=diamond style=filled fillcolor=lightyellow];"
                                     id (node-op-name node)))])))
        (comp-graph-nodes g))
       
       (emit "")
       
       ;; Edge definitions (from inputs to operations)
       (for-each
        (lambda (entry)
                (let ([id (car entry)]
                      [node (cdr entry)])
                     (when (node-op? node)
                           (for-each
                            (lambda (input-id)
                                    (emit (format "  n~a -> n~a;" input-id id)))
                            (node-op-inputs node)))))
        (comp-graph-nodes g))
       
       ;; Mark output node
       (when (comp-graph-output g)
             (emit "")
             (emit (format "  n~a [penwidth=3 color=red];" (comp-graph-output g))))
       
       (emit "}")
       
       ;; Join lines in reverse order
       (apply string-append
              (map (lambda (l) (string-append l "\n"))
                   (reverse lines)))))

;;; NOTE: graph-export-dot (file I/O) removed — core must be pure.
;;; Use (graph-to-dot g) to get the DOT string, then write at the boundary.
