(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
(require 'prelude)
(require 'field)
(require 'algebra/polynomial)
(require 'multivariate)
(require 'convergence)

(doc 'module 'poly-optimize)
(doc 'description "Polynomial optimization with exact derivatives and constraints")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'note "Polynomial optimization primitives:")
(doc 'note "- Exact gradient/Hessian computation for polynomial objectives")
(doc 'note "- Polynomial constraint handling (equality/inequality)")
(doc 'note "- Newton's method specialized for polynomial root-finding")
(doc 'note "- Sum-of-squares (SOS) decomposition preparation")
(doc 'note "")
(doc 'note "The key advantage over generic optimization: we have EXACT derivatives")
(doc 'note "rather than numerical approximations or autodiff overhead.")

;;; ====
;;; Section: Univariate Polynomial Optimization
;;; ====

(doc 'section 'univariate-optimization)

;;; poly-gradient : Polynomial → (Number → Number)
;;; Returns a function that evaluates the exact gradient (derivative) at any point.
(define (poly-gradient p)
  (doc 'export #t)
  (doc 'type '(-> Polynomial (-> Number Number)))
  (doc 'description "Get exact gradient function for univariate polynomial")
  (let ([dp (poly-derivative p)])
    (lambda (x) (poly-eval dp x))))

;;; poly-hessian-fn : Polynomial → (Number → Number)
;;; Returns a function that evaluates the exact Hessian (second derivative) at any point.
(define (poly-hessian-fn p)
  (doc 'export #t)
  (doc 'type '(-> Polynomial (-> Number Number)))
  (doc 'description "Get exact Hessian function for univariate polynomial")
  (let ([d2p (poly-derivative (poly-derivative p))])
    (lambda (x) (poly-eval d2p x))))

;;; poly-minimize-univariate : Polynomial × Number × ConvergenceCriteria → OptResult
;;; Minimize a univariate polynomial using Newton's method with exact derivatives.
(define (poly-minimize-univariate p x0 criteria)
  (doc 'export #t)
  (doc 'type '(-> Polynomial Number ConvergenceCriteria OptResult))
  (doc 'description "Minimize univariate polynomial using Newton's method")
  (doc 'note "Uses exact first and second derivatives - no numerical approximation")
  (let* ([F (poly-field p)]
         [grad-fn (poly-gradient p)]
         [hess-fn (poly-hessian-fn p)]
         [init-f (poly-eval p x0)]
         [init-grad (grad-fn x0)]
         [init-state (initial-convergence-state init-f (list init-grad))])
    (let loop ([x x0]
               [state init-state])
      (let ([reason (converged? criteria state)])
        (if reason
            (make-opt-result (list x) (cs-f-val state) (list (grad-fn x))
                             (cs-iter state) reason)
            (let* ([g (grad-fn x)]
                   [h (hess-fn x)]
                   ;; Newton step: x_new = x - g/h (with safeguard)
                   [step (if (< (abs h) 1e-12)
                             (* -0.1 (sign g))  ; Gradient descent fallback
                             (- (/ g h)))]
                   ;; Damped step for global convergence
                   [alpha (poly-line-search p x step)]
                   [x-new (+ x (* alpha step))]
                   [f-new (poly-eval p x-new)]
                   [g-new (grad-fn x-new)]
                   [new-state (update-convergence-state state f-new (list g-new)
                                                        (list x) (list x-new))])
              (loop x-new new-state)))))))

;;; poly-line-search : Polynomial × Number × Number → Number
;;; Backtracking line search for polynomial minimization.
(define (poly-line-search p x direction)
  (let* ([f0 (poly-eval p x)]
         [grad-fn (poly-gradient p)]
         [g0 (grad-fn x)]
         [c 0.0001]  ; Armijo constant
         [rho 0.5])  ; Backtracking factor
    (let loop ([alpha 1.0])
      (if (< alpha 1e-10)
          alpha
          (let ([f-new (poly-eval p (+ x (* alpha direction)))])
            (if (<= f-new (+ f0 (* c alpha g0 direction)))
                alpha
                (loop (* rho alpha))))))))

;;; sign : Number → Number
(define (sign x)
  (cond [(< x 0) -1]
        [(> x 0) 1]
        [else 0]))

;;; ====
;;; Section: Polynomial Root Finding
;;; ====

(doc 'section 'root-finding)

;;; poly-find-root : Polynomial × Number × ConvergenceCriteria → (Union Number #f)
;;; Find a root of polynomial using Newton-Raphson with exact derivative.
(define (poly-find-root p x0 criteria)
  (doc 'export #t)
  (doc 'type '(-> Polynomial Number ConvergenceCriteria (Union Number #f)))
  (doc 'description "Find polynomial root using Newton-Raphson")
  (doc 'note "Returns #f if no convergence within max iterations")
  (let* ([dp (poly-derivative p)]
         [max-iter (cc-max-iter criteria)]
         [tol (cc-f-tol criteria)])
    (let loop ([x x0] [iter 0])
      (if (>= iter max-iter)
          #f  ; No convergence
          (let ([f-val (poly-eval p x)]
                [df-val (poly-eval dp x)])
            (cond
              [(< (abs f-val) tol) x]  ; Found root
              [(< (abs df-val) 1e-15)
               ;; Derivative near zero - perturb and continue
               (loop (+ x 0.1) (+ iter 1))]
              [else
               (let ([x-new (- x (/ f-val df-val))])
                 (loop x-new (+ iter 1)))]))))))

;;; poly-find-all-roots : Polynomial × ConvergenceCriteria → (List Number)
;;; Find all roots by deflation.
;;; Note: Works best for polynomials with distinct real roots.
(define (poly-find-all-roots p criteria)
  (doc 'export #t)
  (doc 'type '(-> Polynomial ConvergenceCriteria (List Number)))
  (doc 'description "Find all roots via successive deflation")
  (doc 'note "May miss roots if polynomial has repeated or complex roots")
  (let ([n (poly-degree p)])
    (if (<= n 0)
        '()
        (let loop ([current-p p]
                   [deg n]
                   [roots '()]
                   [start-points (poly-initial-guesses n)])
          (if (or (<= deg 0) (null? start-points))
              (reverse roots)
              (let ([root (poly-try-find-root current-p start-points criteria)])
                (if root
                    ;; Deflate: divide out (x - root)
                    (let* ([F (poly-field current-p)]
                           [factor (make-polynomial F
                                     (list (field-neg F root) (field-one F)))]
                           [deflated (poly-div current-p factor)])
                      (loop deflated (- deg 1) (cons root roots) start-points))
                    ;; No root found from these starting points
                    (reverse roots))))))))

;;; poly-try-find-root : try multiple starting points
(define (poly-try-find-root p starts criteria)
  (let loop ([xs starts])
    (if (null? xs)
        #f
        (let ([root (poly-find-root p (car xs) criteria)])
          (if root
              root
              (loop (cdr xs)))))))

;;; poly-initial-guesses : Nat → (List Number)
;;; Generate initial guesses spread across real line.
(define (poly-initial-guesses n)
  (let ([count (max 10 (* 2 n))])
    (append
     (list 0 1 -1 0.5 -0.5)
     (map (lambda (k) (* 2.0 (- (/ k count) 0.5) 10))
          (range 0 count)))))

;;; ====
;;; Section: Multivariate Polynomial Gradients
;;; ====

(doc 'section 'multivariate-gradients)

;;; mpoly-partial : MPoly × Symbol → MPoly
;;; Compute partial derivative with respect to variable v.
(define (mpoly-partial p v)
  (doc 'export #t)
  (doc 'type '(-> MPoly Symbol MPoly))
  (doc 'description "Partial derivative ∂p/∂v")
  (let* ([F (mpoly-field p)]
         [vars (mpoly-vars p)]
         [ordering (mpoly-ordering p)]
         [terms (mpoly-terms p)]
         [zero (field-zero F)])
    (make-mpoly F vars ordering
      (filter-map
       (lambda (term)
         (let* ([coeff (car term)]
                [mono (cdr term)]
                [exp (mono-degree-in mono v)])
           (if (= exp 0)
               #f  ; Variable not in this term
               (let* ([new-coeff (poly-scalar-mul-int-field F coeff exp)]
                      [new-mono (mono-decrease-var mono v)])
                 (cons new-coeff new-mono)))))
       terms))))

;;; mono-decrease-var : Monomial × Symbol → Monomial
;;; Decrease exponent of variable by 1.
(define (mono-decrease-var mono v)
  (filter-map
   (lambda (pair)
     (if (eq? (car pair) v)
         (if (= (cdr pair) 1)
             #f  ; Remove variable entirely
             (cons v (- (cdr pair) 1)))
         pair))
   mono))

;;; poly-scalar-mul-int-field : Field × Coeff × Int → Coeff
;;; Multiply coefficient by integer using field operations.
(define (poly-scalar-mul-int-field F c n)
  (let ([add (field-add-op F)]
        [zero (field-zero F)])
    (if (= n 0)
        zero
        (let loop ([i 1] [acc c])
          (if (= i n)
              acc
              (loop (+ i 1) (add acc c)))))))

;;; mpoly-gradient : MPoly → (List MPoly)
;;; Compute gradient vector (list of partial derivatives).
(define (mpoly-gradient p)
  (doc 'export #t)
  (doc 'type '(-> MPoly (List MPoly)))
  (doc 'description "Gradient ∇p = [∂p/∂x₁, ..., ∂p/∂xₙ]")
  (map (lambda (v) (mpoly-partial p v)) (mpoly-vars p)))

;;; mpoly-gradient-at : MPoly × (List (Symbol × Number)) → (List Number)
;;; Evaluate gradient at a point.
(define (mpoly-gradient-at p env)
  (doc 'export #t)
  (doc 'type '(-> MPoly (List (Symbol × Number)) (List Number)))
  (doc 'description "Evaluate gradient at point")
  (map (lambda (dp) (mpoly-eval dp env)) (mpoly-gradient p)))

;;; mpoly-hessian : MPoly → (List (List MPoly))
;;; Compute Hessian matrix (matrix of second partials).
(define (mpoly-hessian p)
  (doc 'export #t)
  (doc 'type '(-> MPoly (List (List MPoly))))
  (doc 'description "Hessian matrix H[i,j] = ∂²p/∂xᵢ∂xⱼ")
  (let ([grad (mpoly-gradient p)])
    (map (lambda (dpi) (mpoly-gradient dpi)) grad)))

;;; ====
;;; Section: Polynomial Constraints
;;; ====

(doc 'section 'constraints)

;;; A polynomial constraint is: poly op 0
;;; where op is one of: = (equality), <= (less-or-equal), >= (greater-or-equal)

;;; make-poly-constraint : (Union Polynomial MPoly) × Symbol → PolyConstraint
(define (make-poly-constraint poly op)
  (doc 'export #t)
  (doc 'type '(-> (Union Polynomial MPoly) Symbol PolyConstraint))
  (doc 'description "Create polynomial constraint: poly op 0")
  (doc 'note "op must be one of: =, <=, >=")
  (unless (memq op '(= <= >=))
    (error 'make-poly-constraint "op must be =, <=, or >=" op))
  (list 'poly-constraint poly op))

;;; poly-constraint? : Any → Boolean
(define (poly-constraint? x)
  (doc 'export #t)
  (and (pair? x) (eq? (car x) 'poly-constraint)))

;;; pc-poly : PolyConstraint → (Union Polynomial MPoly)
(define (pc-poly c) (cadr c))
(doc 'export #t)

;;; pc-op : PolyConstraint → Symbol
(define (pc-op c) (caddr c))
(doc 'export #t)

;;; constraint-satisfied? : PolyConstraint × Number × Number → Boolean
;;; Check if constraint is satisfied at value (with tolerance).
(define (constraint-satisfied? c val tol)
  (doc 'export #t)
  (case (pc-op c)
    [(=) (< (abs val) tol)]
    [(<=) (<= val tol)]
    [(>=) (>= val (- tol))]
    [else #f]))

;;; constraint-violation : PolyConstraint × Number → Number
;;; Compute how much constraint is violated (0 if satisfied).
(define (constraint-violation c val)
  (doc 'export #t)
  (case (pc-op c)
    [(=) (abs val)]
    [(<=) (max 0 val)]
    [(>=) (max 0 (- val))]
    [else 0]))

;;; ====
;;; Section: Constrained Optimization via Penalty Method
;;; ====

(doc 'section 'constrained-optimization)

;;; poly-penalty-fn : Polynomial × (List PolyConstraint) × Number → (Number → Number)
;;; Create penalized objective: f(x) + μ * penalty(constraints).
(define (poly-penalty-fn objective constraints mu)
  (doc 'export #t)
  (doc 'type '(-> Polynomial (List PolyConstraint) Number (-> Number Number)))
  (doc 'description "Penalized objective for univariate constrained optimization")
  (lambda (x)
    (let* ([obj-val (poly-eval objective x)]
           [penalty (fold-left
                     (lambda (acc c)
                       (let* ([p (pc-poly c)]
                              [val (poly-eval p x)]
                              [viol (constraint-violation c val)])
                         (+ acc (* viol viol))))
                     0 constraints)])
      (+ obj-val (* mu penalty)))))

;;; poly-minimize-constrained : Polynomial × (List PolyConstraint) × Number × ConvergenceCriteria → OptResult
;;; Minimize with polynomial constraints using penalty method.
(define (poly-minimize-constrained objective constraints x0 criteria)
  (doc 'export #t)
  (doc 'type '(-> Polynomial (List PolyConstraint) Number ConvergenceCriteria OptResult))
  (doc 'description "Constrained polynomial optimization via penalty method")
  (doc 'note "Uses increasing penalty parameter for feasibility")
  (let loop ([mu 1.0]
             [x x0]
             [iter 0]
             [max-outer 20])
    (if (>= iter max-outer)
        ;; Construct result
        (make-opt-result (list x) (poly-eval objective x)
                         (list ((poly-gradient objective) x))
                         iter 'max-outer-iterations)
        ;; Inner minimization with current penalty
        (let* ([penalized (poly-make-penalized objective constraints mu)]
               [result (poly-minimize-univariate penalized x criteria)]
               [x-new (car (opt-result-x result))]
               ;; Check constraint satisfaction
               [max-viol (poly-max-violation constraints x-new)])
          (if (< max-viol (cc-f-tol criteria))
              ;; Constraints satisfied
              (make-opt-result (list x-new) (poly-eval objective x-new)
                               (list ((poly-gradient objective) x-new))
                               (+ iter (opt-result-iter result)) 'converged)
              ;; Increase penalty and continue
              (loop (* mu 10) x-new (+ iter 1) max-outer))))))

;;; poly-make-penalized : Polynomial × (List PolyConstraint) × Number → Polynomial
;;; Create penalized polynomial (approximation via Taylor).
;;; Note: For exact penalty, we construct polynomial representing f + μ*g²
(define (poly-make-penalized objective constraints mu)
  (let ([F (poly-field objective)])
    (fold-left
     (lambda (acc c)
       (let* ([g (pc-poly c)]
              [g2 (poly-mul g g)]  ; g² for quadratic penalty
              [scaled (poly-scale g2 mu)])
         (poly-add acc scaled)))
     objective
     (filter (lambda (c) (not (eq? (pc-op c) '>=))) constraints))))

;;; poly-max-violation : (List PolyConstraint) × Number → Number
(define (poly-max-violation constraints x)
  (if (null? constraints)
      0
      (apply max
             (map (lambda (c)
                    (constraint-violation c (poly-eval (pc-poly c) x)))
                  constraints))))

;;; ====
;;; Section: Sum-of-Squares (SOS) Decomposition
;;; ====

(doc 'section 'sum-of-squares)

(doc 'note "A polynomial p(x) is SOS if p = Σᵢ qᵢ²")
(doc 'note "SOS polynomials are guaranteed non-negative.")
(doc 'note "Checking SOS is a semidefinite programming (SDP) problem.")
(doc 'note "Here we provide preparation utilities; full SDP requires solver.")

;;; sos-gram-matrix-size : Nat → Nat
;;; Size of Gram matrix for degree-d univariate polynomial.
;;; A polynomial of degree 2d has SOS representation with d+1 monomials.
(define (sos-gram-matrix-size d)
  (doc 'export #t)
  (doc 'type '(-> Nat Nat))
  (doc 'description "Gram matrix size for degree-d polynomial")
  (+ (quotient d 2) 1))

;;; poly-to-gram-constraints : Polynomial → (List (List Number Number Number))
;;; Extract Gram matrix constraints for SOS decomposition.
;;; Returns list of (row, col, coeff) triples for the SDP.
;;; p = v' * Q * v where v = [1, x, x², ...] and Q is PSD.
(define (poly-to-gram-constraints p)
  (doc 'export #t)
  (doc 'type '(-> Polynomial (List (Int Int Number))))
  (doc 'description "Extract Gram matrix constraints for SOS SDP")
  (doc 'note "Coefficient of x^k = sum of Q[i,j] where i+j=k")
  (let* ([coeffs (poly-coeffs p)]
         [n (length coeffs)]
         [m (sos-gram-matrix-size (- n 1))]
         [constraints '()])
    ;; For each coefficient c_k of x^k
    ;; c_k = sum_{i+j=k} Q[i,j]
    (let loop ([k 0] [acc '()])
      (if (>= k n)
          (reverse acc)
          (let ([ck (list-ref coeffs k)]
                ;; Find all (i,j) pairs with i+j=k, i,j < m
                [pairs (filter (lambda (p) (and (< (car p) m) (< (cdr p) m)))
                               (map (lambda (i) (cons i (- k i)))
                                    (range 0 (+ k 1))))])
            (loop (+ k 1)
                  (cons (list k ck pairs) acc)))))))

;;; poly-is-sos-necessary : Polynomial → Boolean
;;; Necessary conditions for polynomial to be SOS.
;;; If false, definitely not SOS. If true, might be SOS.
(define (poly-is-sos-necessary p)
  (doc 'export #t)
  (doc 'type '(-> Polynomial Boolean))
  (doc 'description "Check necessary conditions for SOS")
  (doc 'note "Even degree and non-negative leading coefficient are necessary")
  (let ([d (poly-degree p)]
        [lc (poly-leading-coeff p)])
    (and (even? d)
         (>= lc 0))))

;;; poly-is-sos-univariate-simple : Polynomial → Boolean
;;; Check if univariate polynomial is SOS using simple conditions.
;;; For low-degree cases, we can check directly.
(define (poly-is-sos-univariate-simple p)
  (doc 'export #t)
  (doc 'type '(-> Polynomial Boolean))
  (doc 'description "Simple SOS check for low-degree univariate polynomials")
  (let ([d (poly-degree p)])
    (cond
      [(not (even? d)) #f]  ; Must have even degree
      [(<= d 0) (>= (poly-eval p 0) 0)]  ; Constant: just check sign
      [(= d 2)
       ;; Quadratic: ax² + bx + c is SOS iff a ≥ 0 and b² ≤ 4ac
       (let* ([coeffs (poly-coeffs p)]
              [c (list-ref coeffs 0)]
              [b (if (> (length coeffs) 1) (list-ref coeffs 1) 0)]
              [a (if (> (length coeffs) 2) (list-ref coeffs 2) 0)])
         (and (>= a 0)
              (>= c 0)
              (<= (* b b) (* 4 a c))))]
      [else
       ;; Higher degree: just check necessary conditions
       ;; Full check requires SDP
       (poly-is-sos-necessary p)])))

;;; ====
;;; Section: Utility
;;; ====

;;; filter-map : (α → (Union β #f)) × (List α) → (List β)
(define (filter-map f lst)
  (let loop ([xs lst] [acc '()])
    (if (null? xs)
        (reverse acc)
        (let ([result (f (car xs))])
          (if result
              (loop (cdr xs) (cons result acc))
              (loop (cdr xs) acc))))))

;;; make-opt-result : (List Number) × Number × (List Number) × Nat × Symbol [× Meta] → OptResult
(define (make-opt-result x f-val grad iter reason . rest)
  (doc 'export #t)
  (let ([meta (if (pair? rest) (car rest) '())])
       (list 'opt-result x f-val grad iter reason meta)))

;;; opt-result-x : OptResult → (List Number)
(define (opt-result-x r) (list-ref r 1))
(doc 'export #t)

;;; opt-result-f : OptResult → Number
(define (opt-result-f r) (list-ref r 2))
(doc 'export #t)

;;; opt-result-grad : OptResult → (List Number)
(define (opt-result-grad r) (list-ref r 3))
(doc 'export #t)

;;; opt-result-iter : OptResult → Nat
(define (opt-result-iter r) (list-ref r 4))
(doc 'export #t)

;;; opt-result-reason : OptResult → Symbol
(define (opt-result-reason r) (list-ref r 5))
(doc 'export #t)
