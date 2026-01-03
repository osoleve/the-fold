;;; core/autodiff/reverse-diff.ss --- Reverse Mode Automatic Differentiation
;;;
;;; Implements reverse-mode AD (backpropagation) for efficient gradient
;;; computation when there are many inputs and few outputs.
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Design:
;;;   - Traced values record their computation history
;;;   - Forward pass builds the tape, backward pass computes gradients
;;;   - Supports gradient accumulation for shared subexpressions
;;;   - Clean interface for single-output gradient computation
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - autodiff/comp-graph.ss (for tape infrastructure)

(load "core/base/prelude.ss")
(load "core/autodiff/comp-graph.ss")

;;; ============================================================
;;; Traced Values
;;; ============================================================

;;; A traced value combines:
;;;   - value: the actual numerical value
;;;   - id: unique identifier for gradient accumulation
;;;   - tape: the computation tape (shared across the computation)

;;; Traced value counter (for unique IDs)
;;; Uses a parameter to support nested AD (each nesting level gets its own counter)
(define *traced-id-counter* (make-parameter 0))

;;; fresh-id : -> Nat
;;; Generate a unique ID within the current AD scope.
(define (fresh-id)
  (let ([id (*traced-id-counter*)])
       (*traced-id-counter* (+ id 1))
       id))

;;; reset-traced-ids! : -> Void
;;; DEPRECATED: Use with-fresh-ad-scope instead.
;;;
;;; WARNING: Calling this function between nested gradient computations
;;; will corrupt the outer computation's tape. Only use for isolated tests.
;;; For production code, always use (with-fresh-ad-scope (lambda () ...)).
(define (reset-traced-ids!)
  (display "WARNING: reset-traced-ids! is deprecated. Use with-fresh-ad-scope.\n")
  (*traced-id-counter* 0))

;;; with-fresh-ad-scope : (-> a) -> a
;;; Execute a computation in a fresh AD scope with its own ID counter and tape.
;;; This enables nested gradient computations without corrupting outer scopes.
(define (with-fresh-ad-scope thunk)
  (parameterize ([*traced-id-counter* 0]
                 [*current-tape* #f])
                (thunk)))

;;; traced : Number x Nat x Tape -> Traced
;;; Create a traced value.
(define (traced val id tape)
  (list 'traced val id tape))

;;; traced? : Any -> Boolean
(define (traced? x)
  (and (pair? x) (eq? (car x) 'traced)))

;;; traced-value : Traced -> Number
(define (traced-value t)
  (if (traced? t) (cadr t) t))

;;; traced-id : Traced -> Nat
(define (traced-id t)
  (if (traced? t) (caddr t) #f))

;;; traced-tape : Traced -> Tape
(define (traced-tape t)
  (if (traced? t) (cadddr t) #f))

;;; ============================================================
;;; Mutable Tape for Recording
;;; ============================================================

;;; Global tape for current computation (parameterized for nested AD)
(define *current-tape* (make-parameter #f))

;;; make-reverse-tape : -> MutableTape
;;; Create a new mutable tape.
(define (make-reverse-tape)
  (list 'reverse-tape (box '())))

;;; reverse-tape? : Any -> Boolean
(define (reverse-tape? t)
  (and (pair? t) (eq? (car t) 'reverse-tape)))

;;; reverse-tape-entries : MutableTape -> (List TapeEntry)
(define (reverse-tape-entries t)
  (unbox (cadr t)))

;;; reverse-tape-push! : MutableTape x TapeEntry -> Void
;;; Push an entry onto the tape.
(define (reverse-tape-push! tape entry)
  (set-box! (cadr tape)
            (cons entry (unbox (cadr tape)))))

;;; ============================================================
;;; Traced Variable Creation
;;; ============================================================

;;; make-traced-var : Number x MutableTape -> Traced
;;; Create a traced variable (input to the computation).
(define (make-traced-var val tape)
  (let ([id (fresh-id)])
       ;; Variables don't add to tape - they're leaves
       (traced val id tape)))

;;; make-traced-const : Number x MutableTape -> Traced
;;; Create a traced constant (gradient = 0).
(define (make-traced-const val tape)
  ;; Constants don't need IDs or tape entries
  val)

;;; ============================================================
;;; Traced Arithmetic Operations
;;; ============================================================

;;; Helper: record operation and return traced result
(define (traced-op op-name result-val inputs local-grads tape)
  (let ([result-id (fresh-id)]
        [input-ids (map (lambda (x) (if (traced? x) (traced-id x) #f))
                        inputs)])
       ;; Record: (result-id op-name input-ids local-grads input-values)
       (reverse-tape-push! tape
                           (list result-id op-name input-ids local-grads))
       (traced result-val result-id tape)))

;;; traced-add : Traced|Number x Traced|Number -> Traced
;;; Addition with gradient tracking. d(a+b)/da = 1, d(a+b)/db = 1
(define (traced-add a b)
  (let* ([av (traced-value a)]
         [bv (traced-value b)]
         [result (+ av bv)]
         [tape (or (and (traced? a) (traced-tape a))
                   (and (traced? b) (traced-tape b)))])
        (if tape
            (traced-op 'add result (list a b) '(1 1) tape)
            result)))

;;; traced-sub : Traced|Number x Traced|Number -> Traced
;;; Subtraction. d(a-b)/da = 1, d(a-b)/db = -1
(define (traced-sub a b)
  (let* ([av (traced-value a)]
         [bv (traced-value b)]
         [result (- av bv)]
         [tape (or (and (traced? a) (traced-tape a))
                   (and (traced? b) (traced-tape b)))])
        (if tape
            (traced-op 'sub result (list a b) '(1 -1) tape)
            result)))

;;; traced-mul : Traced|Number x Traced|Number -> Traced
;;; Multiplication. d(a*b)/da = b, d(a*b)/db = a
(define (traced-mul a b)
  (let* ([av (traced-value a)]
         [bv (traced-value b)]
         [result (* av bv)]
         [tape (or (and (traced? a) (traced-tape a))
                   (and (traced? b) (traced-tape b)))])
        (if tape
            (traced-op 'mul result (list a b) (list bv av) tape)
            result)))

;;; traced-div : Traced|Number x Traced|Number -> Traced
;;; Division. d(a/b)/da = 1/b, d(a/b)/db = -a/b^2
(define (traced-div a b)
  (let* ([av (traced-value a)]
         [bv (traced-value b)]
         [result (/ av bv)]
         [tape (or (and (traced? a) (traced-tape a))
                   (and (traced? b) (traced-tape b)))])
        (if tape
            (traced-op 'div result (list a b)
                       (list (/ 1 bv)
                             (/ (- av) (* bv bv)))
                       tape)
            result)))

;;; traced-neg : Traced|Number -> Traced
;;; Negation. d(-a)/da = -1
(define (traced-neg a)
  (let* ([av (traced-value a)]
         [result (- av)]
         [tape (and (traced? a) (traced-tape a))])
        (if tape
            (traced-op 'neg result (list a) '(-1) tape)
            result)))

;;; traced-sq : Traced|Number -> Traced
;;; Square. d(a^2)/da = 2a
(define (traced-sq a)
  (let* ([av (traced-value a)]
         [result (* av av)]
         [tape (and (traced? a) (traced-tape a))])
        (if tape
            (traced-op 'sq result (list a) (list (* 2 av)) tape)
            result)))

;;; traced-sqrt : Traced|Number -> Traced
;;; Square root. d(sqrt(a))/da = 1/(2*sqrt(a))
(define (traced-sqrt a)
  (let* ([av (traced-value a)]
         [result (sqrt av)]
         [tape (and (traced? a) (traced-tape a))])
        (if tape
            (traced-op 'sqrt result (list a) (list (/ 1 (* 2 result))) tape)
            result)))

;;; traced-exp : Traced|Number -> Traced
;;; Exponential. d(e^a)/da = e^a
(define (traced-exp a)
  (let* ([av (traced-value a)]
         [result (exp av)]
         [tape (and (traced? a) (traced-tape a))])
        (if tape
            (traced-op 'exp result (list a) (list result) tape)
            result)))

;;; traced-log : Traced|Number -> Traced
;;; Natural log. d(ln a)/da = 1/a
(define (traced-log a)
  (let* ([av (traced-value a)]
         [result (log av)]
         [tape (and (traced? a) (traced-tape a))])
        (if tape
            (traced-op 'log result (list a) (list (/ 1 av)) tape)
            result)))

;;; traced-sin : Traced|Number -> Traced
;;; Sine. d(sin a)/da = cos a
(define (traced-sin a)
  (let* ([av (traced-value a)]
         [result (sin av)]
         [tape (and (traced? a) (traced-tape a))])
        (if tape
            (traced-op 'sin result (list a) (list (cos av)) tape)
            result)))

;;; traced-cos : Traced|Number -> Traced
;;; Cosine. d(cos a)/da = -sin a
(define (traced-cos a)
  (let* ([av (traced-value a)]
         [result (cos av)]
         [tape (and (traced? a) (traced-tape a))])
        (if tape
            (traced-op 'cos result (list a) (list (- (sin av))) tape)
            result)))

;;; traced-tan : Traced|Number -> Traced
;;; Tangent. d(tan a)/da = sec^2(a) = 1/cos^2(a)
(define (traced-tan a)
  (let* ([av (traced-value a)]
         [result (tan av)]
         [c (cos av)]
         [tape (and (traced? a) (traced-tape a))])
        (if tape
            (traced-op 'tan result (list a) (list (/ 1 (* c c))) tape)
            result)))

;;; traced-pow : Traced|Number x Number -> Traced
;;; Power. d(a^n)/da = n*a^(n-1)
(define (traced-pow base n)
  (let* ([bv (traced-value base)]
         [result (expt bv n)]
         [tape (and (traced? base) (traced-tape base))])
        (if tape
            (traced-op 'pow result (list base)
                       (list (* n (expt bv (- n 1))))
                       tape)
            result)))

;;; ============================================================
;;; Inverse Trigonometric Functions
;;; ============================================================

;;; traced-asin : Traced|Number -> Traced
;;; Arcsine. d(asin a)/da = 1/sqrt(1-a^2)
(define (traced-asin a)
  (let* ([av (traced-value a)]
         [result (asin av)]
         [tape (and (traced? a) (traced-tape a))])
        (if tape
            (traced-op 'asin result (list a)
                       (list (/ 1 (sqrt (- 1 (* av av)))))
                       tape)
            result)))

;;; traced-acos : Traced|Number -> Traced
;;; Arccosine. d(acos a)/da = -1/sqrt(1-a^2)
(define (traced-acos a)
  (let* ([av (traced-value a)]
         [result (acos av)]
         [tape (and (traced? a) (traced-tape a))])
        (if tape
            (traced-op 'acos result (list a)
                       (list (/ -1 (sqrt (- 1 (* av av)))))
                       tape)
            result)))

;;; traced-atan : Traced|Number -> Traced
;;; Arctangent. d(atan a)/da = 1/(1+a^2)
(define (traced-atan a)
  (let* ([av (traced-value a)]
         [result (atan av)]
         [tape (and (traced? a) (traced-tape a))])
        (if tape
            (traced-op 'atan result (list a)
                       (list (/ 1 (+ 1 (* av av))))
                       tape)
            result)))

;;; ============================================================
;;; Hyperbolic Functions
;;; ============================================================

;;; traced-sinh : Traced|Number -> Traced
;;; Hyperbolic sine. d(sinh a)/da = cosh a
(define (traced-sinh a)
  (let* ([av (traced-value a)]
         [ex (exp av)]
         [emx (exp (- av))]
         [result (/ (- ex emx) 2)]
         [cosh-v (/ (+ ex emx) 2)]
         [tape (and (traced? a) (traced-tape a))])
        (if tape
            (traced-op 'sinh result (list a) (list cosh-v) tape)
            result)))

;;; traced-cosh : Traced|Number -> Traced
;;; Hyperbolic cosine. d(cosh a)/da = sinh a
(define (traced-cosh a)
  (let* ([av (traced-value a)]
         [ex (exp av)]
         [emx (exp (- av))]
         [result (/ (+ ex emx) 2)]
         [sinh-v (/ (- ex emx) 2)]
         [tape (and (traced? a) (traced-tape a))])
        (if tape
            (traced-op 'cosh result (list a) (list sinh-v) tape)
            result)))

;;; traced-tanh : Traced|Number -> Traced
;;; Hyperbolic tangent. d(tanh a)/da = sech^2(a) = 1/cosh^2(a)
(define (traced-tanh a)
  (let* ([av (traced-value a)]
         [ex (exp av)]
         [emx (exp (- av))]
         [result (/ (- ex emx) (+ ex emx))]
         [cosh-v (/ (+ ex emx) 2)]
         [tape (and (traced? a) (traced-tape a))])
        (if tape
            (traced-op 'tanh result (list a)
                       (list (/ 1 (* cosh-v cosh-v)))
                       tape)
            result)))

;;; ============================================================
;;; Backward Pass
;;; ============================================================

;;; backward : MutableTape x Nat x Number -> Hashtable
;;; Compute gradients by walking tape backwards.
;;; Returns hashtable: id -> gradient
(define (backward tape output-id seed)
  (let ([grads (make-hashtable equal-hash equal?)])
       ;; Initialize output gradient with seed (usually 1)
       (hashtable-set! grads output-id seed)
       ;; Process tape entries in reverse order (they're already reversed)
       (for-each
        (lambda (entry)
                (let* ([result-id (car entry)]
                       [op-name (cadr entry)]
                       [input-ids (caddr entry)]
                       [local-grads (cadddr entry)]
                       [result-grad (hashtable-ref grads result-id 0)])
                      ;; Accumulate gradients to each input
                      (for-each
                       (lambda (input-id local-grad)
                               (when input-id  ; Skip non-traced inputs
                                     (let ([current (hashtable-ref grads input-id 0)])
                                          (hashtable-set! grads input-id
                                                          (+ current (* result-grad local-grad))))))
                       input-ids local-grads)))
        (reverse-tape-entries tape))
       grads))

;;; ============================================================
;;; High-Level Interface
;;; ============================================================

;;; gradient : ((Traced ...) -> Traced) x (List Number) -> (List Number)
;;; Compute gradient of f at point args.
;;; f takes individual traced arguments, uses traced operations, returns traced.
;;; Supports nested gradient computations - each call creates its own AD scope.
(define (gradient f args)
  (with-fresh-ad-scope
   (lambda ()
           (let* ([tape (make-reverse-tape)]
                  [traced-args (map (lambda (x) (make-traced-var x tape)) args)]
                  [result (apply f traced-args)])
                 (if (traced? result)
                     (let* ([result-id (traced-id result)]
                            [arg-ids (map traced-id traced-args)]
                            [grads (backward tape result-id 1)])
                           ;; For each input, check if it's the same as output (identity)
                           ;; or get gradient from backward pass
                           (map (lambda (arg-id)
                                        (if (= arg-id result-id)
                                            1  ; Identity function: gradient is 1
                                            (hashtable-ref grads arg-id 0)))
                                arg-ids))
                     ;; Constant function - all gradients are 0
                     (map (lambda (_) 0) args))))))

;;; gradient-at : (Number x ... -> Number) x Number x ... -> (List Number)
;;; Variadic version of gradient.
(define (gradient-at f . args)
  (gradient (lambda (xs) (apply f xs)) args))

;;; value-and-gradient : ((Traced ...) -> Traced) x (List Number) -> (Values Number (List Number))
;;; Compute both value and gradient in one pass.
;;; Supports nested gradient computations.
(define (value-and-gradient f args)
  (with-fresh-ad-scope
   (lambda ()
           (let* ([tape (make-reverse-tape)]
                  [traced-args (map (lambda (x) (make-traced-var x tape)) args)]
                  [result (apply f traced-args)])
                 (if (traced? result)
                     (let* ([val (traced-value result)]
                            [grads (backward tape (traced-id result) 1)]
                            [arg-ids (map traced-id traced-args)])
                           (values val
                                   (map (lambda (id) (hashtable-ref grads id 0))
                                        arg-ids)))
                     ;; Constant function
                     (values result
                             (map (lambda (_) 0) args)))))))

;;; ============================================================
;;; Convenience: Single-Variable Differentiation
;;; ============================================================

;;; reverse-diff : (Traced -> Traced) x Number -> Number
;;; Compute derivative of f at x using reverse mode.
(define (reverse-diff f x)
  (car (gradient f (list x))))

;;; ============================================================
;;; Gradient Checking (for debugging)
;;; ============================================================

;;; numerical-gradient : ((Number ...) -> Number) x (List Number) x Number -> (List Number)
;;; Compute gradient numerically using finite differences.
(define (numerical-gradient f args epsilon)
  (map (lambda (i)
               (let* ([args+ (list-update args i (lambda (x) (+ x epsilon)))]
                      [args- (list-update args i (lambda (x) (- x epsilon)))])
                     (/ (- (apply f args+) (apply f args-))
                        (* 2 epsilon))))
       (iota (length args))))

;;; Helper: update element at index
(define (list-update lst idx f)
  (if (= idx 0)
      (cons (f (car lst)) (cdr lst))
      (cons (car lst) (list-update (cdr lst) (- idx 1) f))))

;;; check-gradient : (List Number -> Number) x (List Number) x Number -> Boolean
;;; Check if analytical gradient matches numerical gradient.
(define (check-gradient f args tolerance)
  (let* ([analytical (gradient f args)]
         [numerical (numerical-gradient f args 1e-5)]
         [diffs (map (lambda (a n) (abs (- a n))) analytical numerical)])
        (andmap (lambda (d) (< d tolerance)) diffs)))

;;; ============================================================
;;; Tape Utilities
;;; ============================================================

;;; print-tape : MutableTape -> Void
;;; Print tape contents for debugging.
(define (print-tape tape)
  (printf "Reverse-Mode Tape (~a entries):~n"
          (length (reverse-tape-entries tape)))
  (for-each
   (lambda (entry)
           (let ([result-id (car entry)]
                 [op-name (cadr entry)]
                 [input-ids (caddr entry)]
                 [local-grads (cadddr entry)])
                (printf "  [~a] = ~a(~a) | grads: ~a~n"
                        result-id op-name input-ids local-grads)))
   (reverse (reverse-tape-entries tape))))
