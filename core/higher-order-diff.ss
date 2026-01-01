;;; fabric/stitches/higher-order-diff.ss — Higher-Order Differentiation
;;;
;;; Implements Jacobian, Hessian, and vector-product utilities for
;;; efficient higher-order derivative computation.
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Key operations:
;;;   - jacobian: Compute full Jacobian matrix
;;;   - hessian: Compute Hessian matrix (uses finite differences for outer derivative)
;;;   - hessian-exact: Compute exact Hessian using hyperdual numbers
;;;   - jvp: Jacobian-vector product (forward mode)
;;;   - vjp: Vector-Jacobian product (reverse mode)
;;;   - second-derivative-exact: Exact d²f/dx² via hyperdual numbers
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - matrix.ss
;;;   - comp-graph.ss (for forward mode)
;;;   - reverse-diff.ss (for reverse mode)

;;; NOTE: Run from fabric/stitches directory
(load "core/prelude.ss")
(load "core/vec.ss")
(load "core/matrix.ss")
(load "core/comp-graph.ss")
(load "core/reverse-diff.ss")

;;; ============================================================
;;; Jacobian Computation
;;; ============================================================

;;; The Jacobian matrix J of a function f: R^n → R^m is an m×n matrix
;;; where J[i,j] = ∂f_i/∂x_j.
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
  (let ([result (make-vector (* m n) 0)])
       ;; For each output i, compute gradient (row i of Jacobian)
       (do ([i 0 (+ i 1)])
           ((= i m) (list 'matrix m n result))
           (reset-traced-ids!)
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
                           (vector-set! result (+ (* i n) j) grad)))))))

;;; jacobian-forward : ((Dual ...) → (List Dual)) × (List Number) × Nat × Nat → Matrix
;;; Compute Jacobian using forward mode (efficient when m > n).
;;; Requires n forward passes.
;;; Note: f must use dual operations (dual-add, dual-mul, etc.)
(define (jacobian-forward f args n m)
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

;;; ============================================================
;;; Hessian Computation
;;; ============================================================

;;; The Hessian matrix H of a scalar function f: R^n → R is an n×n matrix
;;; where H[i,j] = ∂²f/∂x_i∂x_j.
;;;
;;; We compute it by taking the gradient of the gradient using forward mode.

;;; hessian : ((Traced ...) → Traced) × (List Number) → Matrix
;;; Compute the Hessian matrix of a scalar function at a point.
(define (hessian f args)
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
  ;; We need to compute ∂f/∂x_i where args are duals
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

;;; ============================================================
;;; Vector-Jacobian Product (VJP) - Reverse Mode
;;; ============================================================

;;; vjp computes v^T J where v is a vector and J is the Jacobian.
;;; This is efficient for computing many VJPs with the same function.

;;; vjp : ((Traced ...) → (List Traced)) × (List Number) × (List Number) → (List Number)
;;; Compute vector-Jacobian product v^T J at point args.
;;; v is a vector of length m (number of outputs).
(define (vjp f args v)
  (reset-traced-ids!)
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
                  traced-args))))

;;; ============================================================
;;; Jacobian-Vector Product (JVP) - Forward Mode
;;; ============================================================

;;; jvp computes Jv where J is the Jacobian and v is a vector.
;;; Uses forward mode for efficiency.

;;; jvp : ((Dual ...) → (List Dual)) × (List Number) × (List Number) → (List Number)
;;; Compute Jacobian-vector product Jv at point args.
;;; v is a vector of length n (number of inputs).
(define (jvp f args v)
  (let* ([dual-args (map (lambda (x dx) (dual x dx)) args v)]
         [outputs (apply f dual-args)])
        ;; Check for dual? first since duals are also lists
        (if (or (dual? outputs) (not (list? outputs)))
            (list (dual-deriv outputs))
            (map dual-deriv outputs))))

;;; ============================================================
;;; Gradient Utilities
;;; ============================================================

;;; grad : ((Traced ...) → Traced) × (List Number) → (List Number)
;;; Compute gradient of scalar function (wrapper for clarity).
(define (grad f args)
  (gradient f args))

;;; directional-derivative : ((Traced ...) → Traced) × (List Number) × (List Number) → Number
;;; Compute directional derivative ∇f · v at point args in direction v.
(define (directional-derivative f args v)
  (let ([g (gradient f args)])
       (fold-left + 0 (map * g v))))

;;; ============================================================
;;; Convenience: Scalar Second Derivative
;;; ============================================================

;;; second-derivative : (Traced → Traced) × Number → Number
;;; Compute d²f/dx² at x for a single-variable function.
;;; NOTE: Uses finite differences internally. For exact results, use second-derivative-exact.
(define (second-derivative f x)
  ;; f takes a single argument, hessian expects variadic
  (let ([h (hessian (lambda (x) (f x)) (list x))])
       (matrix-ref h 0 0)))

;;; ============================================================
;;; Exact Hessian via Hyperdual Numbers
;;; ============================================================

;;; The functions below use hyperdual numbers (from comp-graph.ss) to compute
;;; exact second derivatives without finite difference approximation.
;;;
;;; Use these when:
;;;   - You need high numerical precision
;;;   - Your function can be expressed using hd-* operations
;;;   - You want reproducible, exact derivatives
;;;
;;; The standard `hessian` function uses finite differences for the outer
;;; derivative, which introduces O(ε) error. These exact versions eliminate
;;; that error entirely.

;;; hessian-exact : ((List Hyperdual) → Hyperdual) × (List Number) → Matrix
;;; Compute exact Hessian using hyperdual numbers.
;;; The function f should use hd-add, hd-mul, hd-sin, etc. instead of
;;; traced-add, traced-mul, traced-sin, etc.
;;;
;;; Example:
;;;   (hessian-exact (lambda (x y) (hd-add (hd-sq x) (hd-sq y))) '(1 2))
;;;   → Matrix with H[0,0]=2, H[1,1]=2, H[0,1]=H[1,0]=0
(define (hessian-exact f args)
  (hessian-forward f args))

;;; second-derivative-exact : (Hyperdual → Hyperdual) × Number → Number
;;; Compute exact d²f/dx² at x using hyperdual numbers.
;;;
;;; Example:
;;;   (second-derivative-exact hd-sin 0)  ; → -sin(0) = 0
;;;   (second-derivative-exact hd-exp 0)  ; → e^0 = 1
(define (second-derivative-exact f x)
  (second-derivative-forward f x))

;;; ============================================================
;;; Helper Functions
;;; ============================================================

;;; list-update : (List a) × Nat × (a → a) → (List a)
;;; Update element at index. (Defined in reverse-diff.ss but duplicated for safety)
(define (hod-list-update lst idx f)
  (if (= idx 0)
      (cons (f (car lst)) (cdr lst))
      (cons (car lst) (hod-list-update (cdr lst) (- idx 1) f))))
