(load "core/base/prelude.ss")
(load "lattice/linalg/vec.ss")
(load "lattice/linalg/matrix.ss")
(load "core/autodiff/comp-graph.ss")
(load "core/autodiff/reverse-diff.ss")

(doc 'module 'higher-order-diff)
(doc 'description "Higher-Order Differentiation - Jacobian, Hessian, and vector-product utilities for efficient higher-order derivative computation")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'note "Key operations: jacobian (full Jacobian matrix), hessian (Hessian matrix using finite differences), hessian-exact (exact Hessian via hyperdual numbers), jvp (Jacobian-vector product, forward mode), vjp (Vector-Jacobian product, reverse mode), second-derivative-exact (exact d^2f/dx^2 via hyperdual numbers)")

(doc 'section 'jacobian-computation)

;;; The Jacobian matrix J of a function f: R^n -> R^m is an m x n matrix
;;; where J[i,j] = df_i/dx_j.
;;;
;;; For efficiency:
;;;   - If m < n (more inputs than outputs): use reverse mode
;;;   - If m > n (more outputs than inputs): use forward mode

;;; jacobian : ((Traced ...) → (List Traced)) × (List Number) → Matrix
;;; Compute the Jacobian matrix of a vector-valued function at a point.
;;; f takes n traced arguments and returns a list of m traced values.
;;; Note: This always uses reverse mode since f uses traced operations.
;;; For forward mode Jacobian, use jacobian-dual with dual operations.
(define (jacobian f args)
  (doc 'export #t)
  (let* ([n (length args)]
         [sample-result (apply f (map (lambda (x) (make-traced-var x (make-reverse-tape)))
                                      args))]
         ;; Check traced? first since traced values are also lists
         [m (if (or (traced? sample-result) (not (list? sample-result)))
                1
                (length sample-result))])
        ;; Always use reverse mode for traced functions
        (jacobian-reverse f args n m)))

;;; jacobian-reverse : ((Traced ...) → (List Traced)) × (List Number) × Nat × Nat → Matrix
;;; Compute Jacobian using reverse mode (efficient when m <= n).
;;; Requires m backward passes.
(define (jacobian-reverse f args n m)
  (doc 'export #t)
  (let ([result (make-vector (* m n) 0)])
       ;; For each output i, compute gradient (row i of Jacobian)
       (do ([i 0 (+ i 1)])
           ((= i m) (list 'matrix m n result))
           (with-fresh-ad-scope
            (lambda ()
              (let* ([tape (make-reverse-tape)]
                     [traced-args (map (lambda (x) (make-traced-var x tape)) args)]
                     [outputs (apply f traced-args)]
                     ;; Check traced? first since traced values are also lists
                     [output-i (if (or (traced? outputs) (not (list? outputs)))
                                   outputs
                                   (list-ref outputs i))]
                     [grads (if (traced? output-i)
                                (backward tape (traced-id output-i) 1)
                                (make-hashtable equal-hash equal?))]
                     [arg-ids (map traced-id traced-args)])
                    (do ([j 0 (+ j 1)])
                        ((= j n))
                        (let* ([arg-id (list-ref arg-ids j)]
                               [grad (hashtable-ref grads arg-id 0)])
                              (vector-set! result (+ (* i n) j) grad)))))))))

;;; jacobian-forward : ((Dual ...) → (List Dual)) × (List Number) × Nat × Nat → Matrix
;;; Compute Jacobian using forward mode (efficient when m > n).
;;; Requires n forward passes.
;;; Note: f must use dual operations (dual-add, dual-mul, etc.)
(define (jacobian-forward f args n m)
  (doc 'export #t)
  (let ([result (make-vector (* m n) 0)])
       ;; For each input j, compute column j of Jacobian
       (do ([j 0 (+ j 1)])
           ((= j n) (list 'matrix m n result))
           (let* ([dual-args (let loop ([xs args] [k 0])
                                  (if (null? xs)
                                      '()
                                      (cons (if (= k j)
                                                (dual-variable (car xs))
                                                (dual-lift (car xs)))
                                            (loop (cdr xs) (+ k 1)))))]
                  [outputs (apply f dual-args)])
                 ;; Check for dual? first since duals are also lists
                 (if (or (dual? outputs) (not (list? outputs)))
                     (vector-set! result j (dual-deriv outputs))
                     (do ([i 0 (+ i 1)]
                          [outs outputs (cdr outs)])
                         ((= i m))
                         (vector-set! result (+ (* i n) j)
                                      (dual-deriv (car outs)))))))))

;;; ====
;;; Hessian Computation
;;; ====

;;; The Hessian matrix H of a scalar function f: R^n -> R is an n x n matrix
;;; where H[i,j] = d^2f/dx_i*dx_j.
;;;
;;; We compute it by taking the gradient of the gradient using forward mode.

;;; hessian : ((Traced ...) → Traced) × (List Number) → Matrix
;;; Compute the Hessian matrix of a scalar function at a point.
(define (hessian f args)
  (doc 'export #t)
  (let ([n (length args)])
       (hessian-reverse-forward f args n)))

;;; hessian-reverse-forward : ((Traced ...) → Traced) × (List Number) × Nat → Matrix
;;; Compute Hessian using reverse-over-forward mode.
;;; First compute gradient, then differentiate each gradient component.
(define (hessian-reverse-forward f args n)
  (let ([result (make-vector (* n n) 0)])
       ;; For each row i (second derivative with respect to x_i)
       (do ([i 0 (+ i 1)])
           ((= i n) (list 'matrix n n result))
           ;; Use forward mode to differentiate the i-th gradient component
           (let ([grad-i-fn (lambda dual-args
                                    ;; Create function that returns gradient at x
                                    ;; but uses dual numbers for the argument we're
                                    ;; differentiating
                                    (gradient-i f dual-args i))])
                (do ([j 0 (+ j 1)])
                    ((= j n))
                    (let* ([dual-args (let loop ([xs args] [k 0])
                                           (if (null? xs)
                                               '()
                                               (cons (if (= k j)
                                                         (dual-variable (car xs))
                                                         (dual-lift (car xs)))
                                                     (loop (cdr xs) (+ k 1)))))]
                           [grad-i (apply grad-i-fn dual-args)])
                          (vector-set! result (+ (* i n) j)
                                       (dual-deriv grad-i))))))))

;;; gradient-i : (f × (List Dual) × Nat) → Dual
;;; Compute the i-th component of gradient using dual numbers.
;;; Each argument is a dual number, result is dual.
(define (gradient-i f dual-args i)
  ;; We need to compute df/dx_i where args are duals
  ;; This is tricky because we need nested differentiation
  ;;
  ;; Approach: use finite differences for the outer derivative
  ;; This is simpler and avoids nested AD complexity
  (let* ([epsilon 1e-6]
         [args-vals (map dual-value dual-args)]
         [args+ (list-update args-vals i (lambda (x) (+ x epsilon)))]
         [args- (list-update args-vals i (lambda (x) (- x epsilon)))]
         [grad+ (gradient f args+)]
         [grad- (gradient f args-)]
         [deriv-vals (map (lambda (g+ g-) (/ (- g+ g-) (* 2 epsilon)))
                          grad+ grad-)])
        ;; The result should propagate dual derivatives
        (let ([val (/ (- (car (gradient f args+)) (car (gradient f args-)))
                      (* 2 epsilon))])
             ;; For the chain rule, multiply by input dual derivative
             (dual val
                   (fold-left + 0
                              (map (lambda (darg dval)
                                           (* (dual-deriv darg) dval))
                                   dual-args deriv-vals))))))

;;; hessian-numerical : ((Number ...) → Number) × (List Number) × Number → Matrix
;;; Compute Hessian numerically using finite differences.
;;; Useful for checking analytical Hessians.
(define (hessian-numerical f args epsilon)
  (let* ([n (length args)]
         [result (make-vector (* n n) 0)])
        (do ([i 0 (+ i 1)])
            ((= i n) (list 'matrix n n result))
            (do ([j 0 (+ j 1)])
                ((= j n))
                (let* ([args-pp (list-update (list-update args i (lambda (x) (+ x epsilon)))
                                             j (lambda (x) (+ x epsilon)))]
                       [args-pm (list-update (list-update args i (lambda (x) (+ x epsilon)))
                                             j (lambda (x) (- x epsilon)))]
                       [args-mp (list-update (list-update args i (lambda (x) (- x epsilon)))
                                             j (lambda (x) (+ x epsilon)))]
                       [args-mm (list-update (list-update args i (lambda (x) (- x epsilon)))
                                             j (lambda (x) (- x epsilon)))]
                       [f-pp (apply f args-pp)]
                       [f-pm (apply f args-pm)]
                       [f-mp (apply f args-mp)]
                       [f-mm (apply f args-mm)]
                       [h-ij (/ (+ f-pp (- f-pm) (- f-mp) f-mm)
                                (* 4 epsilon epsilon))])
                      (vector-set! result (+ (* i n) j) h-ij))))))

;;; ====
;;; Vector-Jacobian Product (VJP) - Reverse Mode
;;; ====

;;; vjp computes v^T J where v is a vector and J is the Jacobian.
;;; This is efficient for computing many VJPs with the same function.

;;; vjp : ((Traced ...) → (List Traced)) × (List Number) × (List Number) → (List Number)
;;; Compute vector-Jacobian product v^T J at point args.
;;; v is a vector of length m (number of outputs).
(define (vjp f args v)
  (doc 'export #t)
  (with-fresh-ad-scope
   (lambda ()
     (let* ([tape (make-reverse-tape)]
         [traced-args (map (lambda (x) (make-traced-var x tape)) args)]
         [outputs (apply f traced-args)]
         [outputs-list (if (list? outputs) outputs (list outputs))]
         [n (length args)]
         [m (length outputs-list)])
        ;; Accumulate gradients weighted by v
        (let ([grads (make-hashtable equal-hash equal?)])
             ;; Initialize output gradients with v
             (do ([i 0 (+ i 1)]
                  [outs outputs-list (cdr outs)]
                  [vs v (cdr vs)])
                 ((= i m))
                 (when (traced? (car outs))
                       (hashtable-set! grads (traced-id (car outs)) (car vs))))
             ;; Backward pass
             (for-each
              (lambda (entry)
                      (let* ([result-id (car entry)]
                             [input-ids (caddr entry)]
                             [local-grads (cadddr entry)]
                             [result-grad (hashtable-ref grads result-id 0)])
                            (for-each
                             (lambda (input-id local-grad)
                                     (when input-id
                                           (let ([current (hashtable-ref grads input-id 0)])
                                                (hashtable-set! grads input-id
                                                                (+ current (* result-grad local-grad))))))
                             input-ids local-grads)))
              (reverse-tape-entries tape))
             ;; Extract gradient for each input
             (map (lambda (targ) (hashtable-ref grads (traced-id targ) 0))
                  traced-args))))))

;;; ====
;;; Jacobian-Vector Product (JVP) - Forward Mode
;;; ====

;;; jvp computes Jv where J is the Jacobian and v is a vector.
;;; Uses forward mode for efficiency.

;;; jvp : ((Dual ...) → (List Dual)) × (List Number) × (List Number) → (List Number)
;;; Compute Jacobian-vector product Jv at point args.
;;; v is a vector of length n (number of inputs).
(define (jvp f args v)
  (doc 'export #t)
  (let* ([dual-args (map (lambda (x dx) (dual x dx)) args v)]
         [outputs (apply f dual-args)])
        ;; Check for dual? first since duals are also lists
        (if (or (dual? outputs) (not (list? outputs)))
            (list (dual-deriv outputs))
            (map dual-deriv outputs))))

;;; ====
;;; Gradient Utilities
;;; ====

;;; grad : ((Traced ...) → Traced) × (List Number) → (List Number)
;;; Compute gradient of scalar function (wrapper for clarity).
(define (grad f args)
  (doc 'export #t)
  (gradient f args))

;;; directional-derivative : ((Traced ...) → Traced) × (List Number) × (List Number) → Number
;;; Compute directional derivative nabla(f) . v at point args in direction v.
(define (directional-derivative f args v)
  (let ([g (gradient f args)])
       (fold-left + 0 (map * g v))))

;;; ====
;;; Convenience: Scalar Second Derivative
;;; ====

;;; second-derivative : (Traced → Traced) × Number → Number
;;; Compute d^2f/dx^2 at x for a single-variable function.
;;; NOTE: Uses finite differences internally. For exact results, use second-derivative-exact.
(define (second-derivative f x)
  ;; f takes a single argument, hessian expects variadic
  (let ([h (hessian (lambda (x) (f x)) (list x))])
       (matrix-ref h 0 0)))

;;; ====
;;; Exact Hessian via Hyperdual Numbers
;;; ====

;;; The functions below use hyperdual numbers (from comp-graph.ss) to compute
;;; exact second derivatives without finite difference approximation.
;;;
;;; Use these when:
;;;   - You need high numerical precision
;;;   - Your function can be expressed using hd-* operations
;;;   - You want reproducible, exact derivatives
;;;
;;; The standard `hessian` function uses finite differences for the outer
;;; derivative, which introduces O(epsilon) error. These exact versions eliminate
;;; that error entirely.

;;; hessian-exact : ((List Hyperdual) → Hyperdual) × (List Number) → Matrix
;;; Compute exact Hessian using hyperdual numbers.
;;; The function f should use hd-add, hd-mul, hd-sin, etc. instead of
;;; traced-add, traced-mul, traced-sin, etc.
;;;
;;; Example:
;;;   (hessian-exact (lambda (x y) (hd-add (hd-sq x) (hd-sq y))) '(1 2))
;;;   -> Matrix with H[0,0]=2, H[1,1]=2, H[0,1]=H[1,0]=0
(define (hessian-exact f args)
  (doc 'export #t)
  (hessian-forward f args))

;;; second-derivative-exact : (Hyperdual → Hyperdual) × Number → Number
;;; Compute exact d^2f/dx^2 at x using hyperdual numbers.
;;;
;;; Example:
;;;   (second-derivative-exact hd-sin 0)  ; -> -sin(0) = 0
;;;   (second-derivative-exact hd-exp 0)  ; -> e^0 = 1
(define (second-derivative-exact f x)
  (doc 'export #t)
  (second-derivative-forward f x))

;;; ====
;;; Helper Functions
;;; ====

;;; list-update : (List α) × Nat × (α → α) → (List α)
;;; Update element at index. (Defined in reverse-diff.ss but duplicated for safety)
(define (hod-list-update lst idx f)
  (if (= idx 0)
      (cons (f (car lst)) (cdr lst))
      (cons (car lst) (hod-list-update (cdr lst) (- idx 1) f))))


;;; ====
;;; JET NUMBERS: Arbitrary-Order Forward Mode Differentiation
;;; ====

;;; A jet number represents a truncated power series:
;;;   f(x + e) = f(x) + f'(x)e + f''(x)e^2/2! + ... + f^(n)(x)e^n/n!
;;;
;;; We store the Taylor coefficients directly (divided by factorial):
;;;   jet = (jet coeffs) where coeffs = [c0, c1, c2, ..., cn]
;;;   represents c0 + c1*e + c2*e^2 + ... + cn*e^n
;;;
;;; To get the k-th derivative at x: k! * (jet-coeff jet k)

;;; jet : (List Number) → Jet
;;; Create a jet number from coefficients [c0, c1, c2, ...].
;;; c_k represents the k-th Taylor coefficient (derivative/k!)
(define (jet coeffs)
  (list 'jet (list->vector coeffs)))

;;; jet? : α → Boolean
(define (jet? x)
  (and (pair? x) (eq? (car x) 'jet)))

;;; jet-coeffs : Jet → (Vector Number)
(define (jet-coeffs j)
  (if (jet? j) (cadr j) (vector j)))

;;; jet-order : Jet → Nat
;;; Maximum derivative order this jet can compute.
(define (jet-order j)
  (if (jet? j)
      (- (vector-length (jet-coeffs j)) 1)
      0))

;;; jet-coeff : Jet × Nat → Number
;;; Get the k-th Taylor coefficient.
(define (jet-coeff j k)
  (if (jet? j)
      (let ([coeffs (jet-coeffs j)])
           (if (< k (vector-length coeffs))
               (vector-ref coeffs k)
               0))
      (if (= k 0) j 0)))

;;; jet-deriv : Jet × Nat → (Either Number Jet)
;;; Get the k-th derivative at the base point.
;;; d^k f/dx^k = k! * c_k
;;; Uses rec-mul to handle nested jets (returns Jet when coefficient is a Jet).
(define (jet-deriv j k)
  (rec-mul (factorial k) (jet-coeff j k)))

;;; factorial : Nat → Nat
(define (factorial n)
  (if (<= n 1) 1 (* n (factorial (- n 1)))))

;;; jet-value : Jet → Number
;;; Get the function value (0-th derivative).
(define (jet-value j)
  (jet-coeff j 0))

;;; jet-lift : Number → Jet
;;; Lift a constant to a jet (all derivatives = 0).
(define (jet-lift x)
  (if (jet? x) x (jet (list x))))

;;; jet-variable : Number × Nat → Jet
;;; Create a jet for a variable with order n.
;;; d/dx x = 1, all higher derivatives = 0.
(define (jet-variable x order)
  (jet (cons x (cons 1 (make-list order 0)))))

;;; ====
;;; Recursive Arithmetic (Generic Dispatch)
;;; ====

;;; These functions provide polymorphic dispatch between plain numbers
;;; and jet numbers. They enable nested jets (jets whose coefficients
;;; are themselves jets), which is needed for computing exact mixed
;;; partial derivatives.
;;;
;;; When both arguments are numbers, use fast primitive operations.
;;; When either is a jet, delegate to jet arithmetic (which handles
;;; lifting constants automatically).

(define (rec-add a b) (if (and (number? a) (number? b)) (+ a b) (jet-add a b)))
(define (rec-sub a b) (if (and (number? a) (number? b)) (- a b) (jet-sub a b)))
(define (rec-mul a b) (if (and (number? a) (number? b)) (* a b) (jet-mul a b)))
(define (rec-div a b) (if (and (number? a) (number? b)) (/ a b) (jet-div a b)))

(define (rec-exp x) (if (number? x) (exp x) (jet-exp x)))
(define (rec-log x) (if (number? x) (log x) (jet-log x)))
(define (rec-sqrt x) (if (number? x) (sqrt x) (jet-sqrt x)))
(define (rec-sin x) (if (number? x) (sin x) (jet-sin x)))
(define (rec-cos x) (if (number? x) (cos x) (jet-cos x)))

;;; ====
;;; Jet Number Arithmetic
;;; ====

;;; Helper: pad coefficient vectors to same length
(define (jet-pad-coeffs v1 v2)
  (let* ([n1 (vector-length v1)]
         [n2 (vector-length v2)]
         [n (max n1 n2)]
         [pad (lambda (v len)
                      (if (= len (vector-length v))
                          v
                          (let ([result (make-vector len 0)])
                               (do ([i 0 (+ i 1)])
                                   ((= i (vector-length v)) result)
                                   (vector-set! result i (vector-ref v i))))))])
        (values (pad v1 n) (pad v2 n) n)))

;;; jet-add : Jet × Jet → Jet
;;; (f + g)^(k) = f^(k) + g^(k)
(define (jet-add a b)
  (let* ([a (jet-lift a)]
         [b (jet-lift b)]
         [ca (jet-coeffs a)]
         [cb (jet-coeffs b)])
        (let-values ([(pa pb n) (jet-pad-coeffs ca cb)])
                    (let ([result (make-vector n 0)])
                         (do ([i 0 (+ i 1)])
                             ((= i n) (jet (vector->list result)))
                             (vector-set! result i (rec-add (vector-ref pa i)
                                                            (vector-ref pb i))))))))

;;; jet-sub : Jet × Jet → Jet
(define (jet-sub a b)
  (let* ([a (jet-lift a)]
         [b (jet-lift b)]
         [ca (jet-coeffs a)]
         [cb (jet-coeffs b)])
        (let-values ([(pa pb n) (jet-pad-coeffs ca cb)])
                    (let ([result (make-vector n 0)])
                         (do ([i 0 (+ i 1)])
                             ((= i n) (jet (vector->list result)))
                             (vector-set! result i (rec-sub (vector-ref pa i)
                                                            (vector-ref pb i))))))))

;;; jet-neg : Jet → Jet
(define (jet-neg a)
  (let* ([a (jet-lift a)]
         [ca (jet-coeffs a)]
         [n (vector-length ca)]
         [result (make-vector n 0)])
        (do ([i 0 (+ i 1)])
            ((= i n) (jet (vector->list result)))
            (vector-set! result i (- (vector-ref ca i))))))

;;; jet-mul : Jet × Jet → Jet
;;; Cauchy product: c_k = sum_{j=0}^{k} a_j * b_{k-j}
(define (jet-mul a b)
  (let* ([a (jet-lift a)]
         [b (jet-lift b)]
         [ca (jet-coeffs a)]
         [cb (jet-coeffs b)])
        (let-values ([(pa pb n) (jet-pad-coeffs ca cb)])
                    (let ([result (make-vector n 0)])
                         (do ([k 0 (+ k 1)])
                             ((= k n) (jet (vector->list result)))
                             (let ([sum 0])
                                  (do ([j 0 (+ j 1)])
                                      ((> j k))
                                      (set! sum (rec-add sum
                                                         (rec-mul (vector-ref pa j)
                                                                  (vector-ref pb (- k j))))))
                                  (vector-set! result k sum)))))))

;;; jet-sq : Jet → Jet
(define (jet-sq a)
  (jet-mul a a))

;;; jet-div : Jet × Jet → Jet
;;; Compute c = a/b where b_0 * c_k = a_k - sum_{j=1}^{k} b_j * c_{k-j}
(define (jet-div a b)
  (let* ([a (jet-lift a)]
         [b (jet-lift b)]
         [ca (jet-coeffs a)]
         [cb (jet-coeffs b)])
        (let-values ([(pa pb n) (jet-pad-coeffs ca cb)])
                    (let ([result (make-vector n 0)]
                          [b0 (vector-ref pb 0)])
                         (do ([k 0 (+ k 1)])
                             ((= k n) (jet (vector->list result)))
                             (let ([sum (vector-ref pa k)])
                                  (do ([j 1 (+ j 1)])
                                      ((> j k))
                                      (set! sum (rec-sub sum
                                                         (rec-mul (vector-ref pb j)
                                                                  (vector-ref result (- k j))))))
                                  (vector-set! result k (rec-div sum b0))))))))

;;; jet-recip : Jet → Jet
;;; 1/a
(define (jet-recip a)
  (jet-div (jet-lift 1) a))

;;; ====
;;; Jet Transcendental Functions
;;; ====

;;; For exp, sin, cos, log, sqrt, we use the recurrence relations
;;; derived from the differential equations they satisfy.

;;; jet-exp : Jet → Jet
;;; d(exp(f))/dx = exp(f) * f'
;;; Recurrence: c_k = (1/k) * sum_{j=1}^{k} j * a_j * c_{k-j}
(define (jet-exp a)
  (let* ([a (jet-lift a)]
         [ca (jet-coeffs a)]
         [n (vector-length ca)]
         [result (make-vector n 0)])
        ;; c_0 = exp(a_0)
        (vector-set! result 0 (rec-exp (vector-ref ca 0)))
        ;; Recurrence for higher terms
        (do ([k 1 (+ k 1)])
            ((= k n) (jet (vector->list result)))
            (let ([sum 0])
                 (do ([j 1 (+ j 1)])
                     ((> j k))
                     (set! sum (rec-add sum (rec-mul j
                                                     (rec-mul (vector-ref ca j)
                                                              (vector-ref result (- k j)))))))
                 (vector-set! result k (rec-div sum k))))))

;;; jet-log : Jet → Jet
;;; d(log(f))/dx = f'/f
;;; Recurrence: c_k = (1/a_0) * (a_k - (1/k) * sum_{j=1}^{k-1} j * c_j * a_{k-j})
(define (jet-log a)
  (let* ([a (jet-lift a)]
         [ca (jet-coeffs a)]
         [n (vector-length ca)]
         [result (make-vector n 0)]
         [a0 (vector-ref ca 0)])
        ;; c_0 = log(a_0)
        (vector-set! result 0 (rec-log a0))
        ;; Recurrence for higher terms
        (do ([k 1 (+ k 1)])
            ((= k n) (jet (vector->list result)))
            (let ([sum 0])
                 (do ([j 1 (+ j 1)])
                     ((>= j k))
                     (set! sum (rec-add sum (rec-mul j
                                                     (rec-mul (vector-ref result j)
                                                              (vector-ref ca (- k j)))))))
                 (vector-set! result k (rec-div (rec-sub (vector-ref ca k) (rec-div sum k)) a0))))))

;;; jet-sqrt : Jet → Jet
;;; d(sqrt(f))/dx = f'/(2*sqrt(f))
;;; Recurrence: c_k = (1/(2*c_0)) * (a_k - sum_{j=1}^{k-1} c_j * c_{k-j})
(define (jet-sqrt a)
  (let* ([a (jet-lift a)]
         [ca (jet-coeffs a)]
         [n (vector-length ca)]
         [result (make-vector n 0)]
         [c0 (rec-sqrt (vector-ref ca 0))])
        ;; c_0 = sqrt(a_0)
        (vector-set! result 0 c0)
        ;; Recurrence for higher terms
        (do ([k 1 (+ k 1)])
            ((= k n) (jet (vector->list result)))
            (let ([sum 0])
                 ;; Only sum up to k-1 to avoid doubling the middle term
                 (do ([j 1 (+ j 1)])
                     ((>= j k))
                     (set! sum (rec-add sum (rec-mul (vector-ref result j)
                                                     (vector-ref result (- k j))))))
                 (vector-set! result k (rec-div (rec-sub (vector-ref ca k) sum) (rec-mul 2 c0)))))))

;;; jet-sin-cos : Jet → (Values Jet Jet)
;;; Compute sin and cos simultaneously using coupled recurrence.
;;; d(sin(f))/dx = cos(f) * f', d(cos(f))/dx = -sin(f) * f'
(define (jet-sin-cos a)
  (let* ([a (jet-lift a)]
         [ca (jet-coeffs a)]
         [n (vector-length ca)]
         [sin-result (make-vector n 0)]
         [cos-result (make-vector n 0)]
         [a0 (vector-ref ca 0)])
        ;; Initial values
        (vector-set! sin-result 0 (rec-sin a0))
        (vector-set! cos-result 0 (rec-cos a0))
        ;; Coupled recurrence
        (do ([k 1 (+ k 1)])
            ((= k n) (values (jet (vector->list sin-result))
                             (jet (vector->list cos-result))))
            (let ([sin-sum 0]
                  [cos-sum 0])
                 (do ([j 1 (+ j 1)])
                     ((> j k))
                     (let ([aj (vector-ref ca j)])
                          (set! sin-sum (rec-add sin-sum (rec-mul j (rec-mul aj (vector-ref cos-result (- k j))))))
                          (set! cos-sum (rec-add cos-sum (rec-mul j (rec-mul aj (vector-ref sin-result (- k j))))))))
                 (vector-set! sin-result k (rec-div sin-sum k))
                 (vector-set! cos-result k (rec-div (rec-sub 0 cos-sum) k))))))

;;; jet-sin : Jet → Jet
(define (jet-sin a)
  (let-values ([(s c) (jet-sin-cos a)]) s))

;;; jet-cos : Jet → Jet
(define (jet-cos a)
  (let-values ([(s c) (jet-sin-cos a)]) c))

;;; jet-tan : Jet → Jet
;;; tan(f) = sin(f)/cos(f)
(define (jet-tan a)
  (let-values ([(s c) (jet-sin-cos a)])
              (jet-div s c)))

;;; jet-pow : Jet × Number → Jet
;;; f^n using exp(n * log(f)) for non-integer, binary exponentiation for integer
(define (jet-pow base exp)
  (let ([base (jet-lift base)])
       (cond
        [(= exp 0) (jet (list 1))]
        [(integer? exp)
         ;; Binary exponentiation: O(log n) instead of O(n)
         (let loop ([n (abs exp)]
                    [base base]
                    [result (jet (list 1))])
              (cond
               [(= n 0) (if (< exp 0) (jet-recip result) result)]
               [(odd? n) (loop (quotient n 2)
                               (jet-mul base base)
                               (jet-mul result base))]
               [else (loop (quotient n 2)
                           (jet-mul base base)
                           result)]))]
        [else
         ;; Non-integer: use exp(n * log(base))
         (jet-exp (jet-mul (jet-lift exp) (jet-log base)))])))

;;; ====
;;; Inverse Trigonometric Functions for Jets
;;; ====

;;; jet-atan : Jet → Jet
;;; d(atan(f))/dx = f'/(1+f^2)
;;; Uses recurrence: a(x) = atan(f(x)), let g = 1 + f^2
;;; c_k = (1/g_0) * (f_k - (1/k) * sum_{j=1}^{k-1} j * c_j * g_{k-j})
(define (jet-atan a)
  (let* ([a (jet-lift a)]
         [ca (jet-coeffs a)]
         [n (vector-length ca)]
         [result (make-vector n 0)]
         [a0 (vector-ref ca 0)]
         ;; Compute g = 1 + f^2
         [a-sq (jet-sq a)]
         [g (jet-add (jet (list 1)) a-sq)]
         [cg (jet-coeffs g)]
         [g0 (vector-ref cg 0)])
        ;; c_0 = atan(a_0)
        (vector-set! result 0 (rec-atan a0))
        ;; Recurrence similar to log
        (do ([k 1 (+ k 1)])
            ((= k n) (jet (vector->list result)))
            (let ([sum 0])
                 (do ([j 1 (+ j 1)])
                     ((>= j k))
                     (set! sum (rec-add sum (rec-mul j
                                                     (rec-mul (vector-ref result j)
                                                              (vector-ref cg (- k j)))))))
                 (vector-set! result k (rec-div (rec-sub (vector-ref ca k) (rec-div sum k)) g0))))))

;;; jet-asin : Jet → Jet
;;; d(asin(f))/dx = f'/sqrt(1-f^2)
;;; Uses recurrence: let g = sqrt(1-f^2)
;;; Similar to atan but with different denominator
(define (jet-asin a)
  (let* ([a (jet-lift a)]
         [ca (jet-coeffs a)]
         [n (vector-length ca)]
         [result (make-vector n 0)]
         [a0 (vector-ref ca 0)]
         ;; Compute g = sqrt(1 - f^2)
         [a-sq (jet-sq a)]
         [one-a-sq (jet-sub (jet (list 1)) a-sq)]
         [g (jet-sqrt one-a-sq)]
         [cg (jet-coeffs g)]
         [g0 (vector-ref cg 0)])
        ;; c_0 = asin(a_0)
        (vector-set! result 0 (rec-asin a0))
        ;; Recurrence: c_k = (a_k - sum_{j=1}^{k-1} c_j * g_{k-j}) / g_0
        (do ([k 1 (+ k 1)])
            ((= k n) (jet (vector->list result)))
            (let ([sum 0])
                 (do ([j 1 (+ j 1)])
                     ((>= j k))
                     (set! sum (rec-add sum (rec-mul (vector-ref result j)
                                                     (vector-ref cg (- k j))))))
                 (vector-set! result k (rec-div (rec-sub (vector-ref ca k) sum) g0))))))

;;; jet-acos : Jet → Jet
;;; d(acos(f))/dx = -f'/sqrt(1-f^2)
;;; Same as asin but negated derivative
(define (jet-acos a)
  (let* ([a (jet-lift a)]
         [ca (jet-coeffs a)]
         [n (vector-length ca)]
         [result (make-vector n 0)]
         [a0 (vector-ref ca 0)]
         ;; Compute g = sqrt(1 - f^2)
         [a-sq (jet-sq a)]
         [one-a-sq (jet-sub (jet (list 1)) a-sq)]
         [g (jet-sqrt one-a-sq)]
         [cg (jet-coeffs g)]
         [g0 (vector-ref cg 0)])
        ;; c_0 = acos(a_0)
        (vector-set! result 0 (rec-acos a0))
        ;; Recurrence: c_k = -(a_k - sum_{j=1}^{k-1} c_j * g_{k-j}) / g_0 (negated)
        (do ([k 1 (+ k 1)])
            ((= k n) (jet (vector->list result)))
            (let ([sum 0])
                 (do ([j 1 (+ j 1)])
                     ((>= j k))
                     (set! sum (rec-add sum (rec-mul (vector-ref result j)
                                                     (vector-ref cg (- k j))))))
                 (vector-set! result k (rec-sub 0 (rec-div (rec-sub (vector-ref ca k) sum) g0)))))))

;;; ====
;;; Hyperbolic Functions for Jets
;;; ====

;;; jet-sinh-cosh : Jet → (Values Jet Jet)
;;; Compute sinh and cosh simultaneously using coupled recurrence.
;;; sinh' = cosh, cosh' = sinh
(define (jet-sinh-cosh a)
  (let* ([a (jet-lift a)]
         [ca (jet-coeffs a)]
         [n (vector-length ca)]
         [sinh-result (make-vector n 0)]
         [cosh-result (make-vector n 0)]
         [a0 (vector-ref ca 0)]
         [ex (rec-exp a0)]
         [emx (rec-exp (rec-sub 0 a0))])
        ;; Initial values: sinh(a0) = (e^a0 - e^-a0)/2, cosh(a0) = (e^a0 + e^-a0)/2
        (vector-set! sinh-result 0 (rec-div (rec-sub ex emx) 2))
        (vector-set! cosh-result 0 (rec-div (rec-add ex emx) 2))
        ;; Coupled recurrence (similar to sin/cos but same signs)
        (do ([k 1 (+ k 1)])
            ((= k n) (values (jet (vector->list sinh-result))
                             (jet (vector->list cosh-result))))
            (let ([sinh-sum 0]
                  [cosh-sum 0])
                 (do ([j 1 (+ j 1)])
                     ((> j k))
                     (let ([aj (vector-ref ca j)])
                          (set! sinh-sum (rec-add sinh-sum (rec-mul j (rec-mul aj (vector-ref cosh-result (- k j))))))
                          (set! cosh-sum (rec-add cosh-sum (rec-mul j (rec-mul aj (vector-ref sinh-result (- k j))))))))
                 (vector-set! sinh-result k (rec-div sinh-sum k))
                 (vector-set! cosh-result k (rec-div cosh-sum k))))))

;;; jet-sinh : Jet → Jet
(define (jet-sinh a)
  (let-values ([(s c) (jet-sinh-cosh a)]) s))

;;; jet-cosh : Jet → Jet
(define (jet-cosh a)
  (let-values ([(s c) (jet-sinh-cosh a)]) c))

;;; jet-tanh : Jet → Jet
;;; tanh(f) = sinh(f)/cosh(f)
(define (jet-tanh a)
  (let-values ([(s c) (jet-sinh-cosh a)])
              (jet-div s c)))

;;; ====
;;; Recursive Helpers for Inverse Trig
;;; ====

;;; rec-atan : (Either Number Jet) → (Either Number Jet)
(define (rec-atan a)
  (cond
   [(jet? a) (jet-atan a)]
   [(number? a) (atan a)]
   [else (error 'rec-atan "unsupported type" a)]))

;;; rec-asin : (Either Number Jet) → (Either Number Jet)
(define (rec-asin a)
  (cond
   [(jet? a) (jet-asin a)]
   [(number? a) (asin a)]
   [else (error 'rec-asin "unsupported type" a)]))

;;; rec-acos : (Either Number Jet) → (Either Number Jet)
(define (rec-acos a)
  (cond
   [(jet? a) (jet-acos a)]
   [(number? a) (acos a)]
   [else (error 'rec-acos "unsupported type" a)]))

;;; ====
;;; Higher-Order Derivative Computation with Jets
;;; ====

;;; nth-derivative : (Jet → Jet) × Number × Nat → Number
;;; Compute the n-th derivative of f at x.
;;; f must use jet-* operations.
(define (nth-derivative f x n)
  (let* ([j (jet-variable x n)]
         [result (f j)])
        (jet-deriv result n)))

;;; all-derivatives : (Jet → Jet) × Number × Nat → (List Number)
;;; Compute derivatives 0 through n at x.
;;; Returns (f(x), f'(x), f''(x), ..., f^(n)(x))
(define (all-derivatives f x n)
  (let* ([j (jet-variable x n)]
         [result (f j)])
        (map (lambda (k) (jet-deriv result k))
             (iota (+ n 1)))))

;;; ====
;;; Taylor Series Expansion
;;; ====

;;; taylor-coefficients : (Jet → Jet) × Number × Nat → (List Number)
;;; Compute Taylor series coefficients at x up to order n.
;;; Returns (c_0, c_1, ..., c_n) where f(x + h) ~ sum c_k * h^k
(define (taylor-coefficients f x n)
  (let* ([j (jet-variable x n)]
         [result (f j)])
        (map (lambda (k) (jet-coeff result k))
             (iota (+ n 1)))))

;;; taylor-series : (Jet → Jet) × Number × Nat → (Number → Number)
;;; Return a function that evaluates the Taylor series approximation.
(define (taylor-series f x n)
  (let ([coeffs (taylor-coefficients f x n)])
       (lambda (h)
               (let loop ([cs coeffs] [power 1] [sum 0])
                    (if (null? cs)
                        sum
                        (loop (cdr cs)
                              (* power h)
                              (+ sum (* (car cs) power))))))))

;;; taylor-remainder-bound : (Jet → Jet) × Number × Number × Nat → Number
;;; Lagrange remainder estimate: |R_n(h)| <= M * |h|^(n+1) / (n+1)!
;;; where M is the max of |f^(n+1)| on [x, x+h].
;;; This computes f^(n+1)(x) as an estimate for M.
(define (taylor-remainder-bound f x h n)
  (let ([deriv-n+1 (abs (nth-derivative f x (+ n 1)))])
       (/ (* deriv-n+1 (expt (abs h) (+ n 1)))
          (factorial (+ n 1)))))

;;; ====
;;; Mixed Partial Derivatives
;;; ====

;;; For multivariate functions, mixed partials are computed by
;;; treating one variable at a time as "active" (with jet structure)
;;; while others are constants.

;;; partial-derivative : ((List Jet) → Jet) × (List Number) × Nat × Nat → Number
;;; Compute d^n f / dx_i^n where i is the specified variable index.
;;; For multivariate functions, we make variable i active and others constant.
(define (partial-derivative f args var-index n)
  (let* ([num-args (length args)]
         ;; Create jets: active variable gets jet-variable, others get jet-lift
         [jets (map (lambda (x i)
                            (if (= i var-index)
                                (jet-variable x n)
                                (jet-lift x)))
                    args (iota num-args))]
         [result (apply f jets)])
        (jet-deriv result n)))

;;; mixed-partial : ((List Jet) → Jet) × (List Number) × (List Nat) → Number
;;; Compute mixed partial derivative d^|k| f / (dx1^k1 * dx2^k2 * ...)
;;; where k = (k1, k2, ...) specifies the order with respect to each variable.
;;;
;;; Example: (mixed-partial f '(1 2) '(2 1)) computes d^3f/dx1^2 dx2
;;; at point (1, 2).
;;;
;;; NOTE: For true mixed partials (multiple variables with non-zero orders),
;;; this uses nested differentiation which is more expensive but exact.
(define (mixed-partial f args orders)
  (let* ([active-indices (filter (lambda (i) (> (list-ref orders i) 0))
                                 (iota (length args)))])
        (if (= (length active-indices) 1)
            ;; Single variable partial - use jet directly
            (partial-derivative f args (car active-indices) (list-ref orders (car active-indices)))
            ;; True mixed partial - use nested differentiation
            ;; d^2f/dx_i dx_j = d/dx_j (df/dx_i)
            (if (null? active-indices)
                (jet-value (apply f (map jet-lift args)))
                (let* ([first-var (car active-indices)]
                       [first-order (list-ref orders first-var)]
                       [remaining-orders (list-update orders first-var (lambda (_) 0))]
                       ;; Create a function that computes df/dx_first
                       [inner-f (lambda jets
                                        (let* ([args-inner (map jet-value jets)]
                                               [first-jet (jet-variable (list-ref args-inner first-var) first-order)]
                                               [jets-inner (map (lambda (x i)
                                                                        (if (= i first-var) first-jet x))
                                                                jets (iota (length jets)))]
                                               [result (apply f jets-inner)])
                                              (jet (list (jet-deriv result first-order)))))])
                      (mixed-partial inner-f args remaining-orders))))))

;;; ====
;;; Gradient, Hessian, and Higher-Order Tensors via Jets
;;; ====

;;; gradient-jet : ((List Jet) → Jet) × (List Number) → (List Number)
;;; Compute gradient using jets (forward mode).
(define (gradient-jet f args)
  (let ([n (length args)])
       (map (lambda (i)
                    (partial-derivative f args i 1))
            (iota n))))

;;; hessian-jet : ((List Jet) → Jet) × (List Number) → Matrix
;;; Compute Hessian using nested differentiation with jets.
;;; Uses exact differentiation for all entries including mixed partials.
;;;
;;; For diagonal entries (i=j): use order-2 jet to get d^2f/dx_i^2 directly.
;;;
;;; For off-diagonal (i!=j): use the identity:
;;;   f(x_i + e, x_j + e) has e^2 coefficient = (H_ii + 2*H_ij + H_jj) / 2
;;;   Therefore: H_ij = (e^2_coeff * 2 - H_ii - H_jj) / 2
;;;
;;; This approach uses exact jet arithmetic throughout with no finite differences.
(define (hessian-jet f args)
  (let* ([n (length args)]
         [result (make-vector (* n n) 0)]
         ;; First pass: compute all diagonal entries
         [diagonals (map (lambda (i)
                                 (let* ([jets (map (lambda (x idx)
                                                           (if (= idx i)
                                                               (jet-variable x 2)
                                                               (jet-lift x)))
                                                   args (iota n))]
                                        [result-jet (apply f jets)])
                                       (jet-deriv result-jet 2)))
                         (iota n))])
        ;; Store diagonals
        (do ([i 0 (+ i 1)])
            ((= i n))
            (vector-set! result (+ (* i n) i) (list-ref diagonals i)))
        ;; Second pass: compute off-diagonal entries using sum formula
        (do ([i 0 (+ i 1)])
            ((= i n) (list 'matrix n n result))
            (do ([j (+ i 1) (+ j 1)])
                ((= j n))
                ;; Compute d^2f/dx_i dx_j using:
                ;; f(x_i + e, x_j + e) has e^2 Taylor coeff = (H_ii + 2*H_ij + H_jj) / 2
                ;; So: H_ij = (2 * e^2_coeff - H_ii - H_jj) / 2
                (let* ([diag-i (list-ref diagonals i)]
                       [diag-j (list-ref diagonals j)]
                       ;; Evaluate f with both x_i and x_j as jet-variables
                       [jets (map (lambda (x idx)
                                          (cond [(= idx i) (jet-variable x 2)]
                                                [(= idx j) (jet-variable x 2)]
                                                [else (jet-lift x)]))
                                  args (iota n))]
                       [result-jet (apply f jets)]
                       ;; e^2 Taylor coefficient (not derivative - derivative would be 2*this)
                       [e2-coeff (jet-coeff result-jet 2)]
                       ;; Sum of second derivatives = 2 * e2-coeff (since d^2/de^2 = 2! * coeff)
                       [sum-term (* 2 e2-coeff)]
                       ;; Mixed partial = (sum - diag_i - diag_j) / 2
                       [h-ij (/ (- sum-term diag-i diag-j) 2)])
                      (vector-set! result (+ (* i n) j) h-ij)
                      (vector-set! result (+ (* j n) i) h-ij))))))

;;; third-derivative-single : (Jet → Jet) × Number → Number
;;; Compute the third derivative tensor for a single-variable function.
;;; For multivariate, this returns d^3f/dx^3.
(define (third-derivative-single f x)
  (nth-derivative f x 3))

;;; ====
;;; Repeated Differentiation Operators
;;; ====

;;; diff-n : Nat → ((α → α) → (α → α))
;;; Apply differentiation n times using dual numbers.
;;; diff-operator is a function that differentiates once.
(define (diff-n n)
  (lambda (f)
          (if (= n 0)
              f
              (let loop ([k n] [g f])
                   (if (= k 0)
                       g
                       (loop (- k 1)
                             (lambda (x)
                                     ;; Differentiate g at x
                                     (dual-deriv (g (dual-variable x))))))))))

;;; diff-operator-naive : (Number → Number) × Number × Nat → Number
;;; Compute n-th derivative of f at x using nested dual numbers.
;;; Note: This has exponential complexity for large n.
;;; For efficiency, prefer jet-based nth-derivative.
(define (diff-operator-naive f x n)
  (if (= n 0)
      (f x)
      (forward-diff (lambda (y) (diff-operator-naive f y (- n 1))) x)))

;;; ====
;;; Convenience: Common Higher-Order Derivatives
;;; ====

;;; second-derivative-jet : (Jet → Jet) × Number → Number
;;; Compute f''(x) using jets.
(define (second-derivative-jet f x)
  (nth-derivative f x 2))

;;; third-derivative-jet : (Jet → Jet) × Number → Number
;;; Compute f'''(x) using jets.
(define (third-derivative-jet f x)
  (nth-derivative f x 3))

;;; fourth-derivative-jet : (Jet → Jet) × Number → Number
;;; Compute f''''(x) using jets.
(define (fourth-derivative-jet f x)
  (nth-derivative f x 4))

;;; laplacian : ((List Jet) → Jet) × (List Number) → Number
;;; Compute the Laplacian (sum of second partial derivatives).
(define (laplacian f args)
  (let ([n (length args)])
       (apply + (map (lambda (i)
                             (partial-derivative f args i 2))
                     (iota n)))))

;;; ====
;;; Derivative-Based Function Properties
;;; ====

;;; is-critical-point? : ((List Jet) → Jet) × (List Number) × Number → Boolean
;;; Check if args is a critical point (all partials are zero).
(define (is-critical-point? f args tolerance)
  (let ([grad (gradient-jet f args)])
       (andmap (lambda (g) (< (abs g) tolerance)) grad)))

;;; classify-critical-point : ((List Jet) → Jet) × (List Number) → Symbol
;;; Classify a critical point using the Hessian.
;;; Returns 'minimum, 'maximum, 'saddle, or 'inconclusive.
(define (classify-critical-point f args)
  (let* ([H (hessian-jet f args)]
         [n (matrix-rows H)])
        (if (= n 1)
            ;; 1D case: check second derivative
            (let ([d2 (matrix-ref H 0 0)])
                 (cond [(> d2 0) 'minimum]
                       [(< d2 0) 'maximum]
                       [else 'inconclusive]))
            ;; Multivariate: check eigenvalues (simplified - check determinant and trace)
            (if (= n 2)
                (let* ([a (matrix-ref H 0 0)]
                       [b (matrix-ref H 0 1)]
                       [c (matrix-ref H 1 0)]
                       [d (matrix-ref H 1 1)]
                       [det (- (* a d) (* b c))]
                       [trace (+ a d)])
                      (cond [(and (> det 0) (> trace 0)) 'minimum]
                            [(and (> det 0) (< trace 0)) 'maximum]
                            [(< det 0) 'saddle]
                            [else 'inconclusive]))
                'inconclusive))))
