(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
(load "core/base/prelude.ss")
(load "lattice/numeric/interval.ss")
(require 'hamt)

(doc 'module 'interval-autodiff)
(doc 'description "Interval Automatic Differentiation - extends autodiff to produce interval-valued gradients for verified optimization")
(doc 'layer 'lattice)
(doc 'purity 'partial)
(doc 'tier 1)
(doc 'note "Key insight: The gradient of f at any point x in box B lies within the interval gradient computed here. Enables monotonicity pruning and verified optimization with rigorous gradient bounds.")

(doc 'section 'interval-traced-values)

(define *interval-traced-id* 0)

;;; interval-fresh-id : Unit -> Nat
(define (interval-fresh-id)
  (let ([id *interval-traced-id*])
    (set! *interval-traced-id* (+ id 1))
    id))

;;; reset-interval-traced-ids! : Unit -> Unit
(define (reset-interval-traced-ids!)
  (set! *interval-traced-id* 0))

;;; interval-traced : Interval x Nat x Tape -> IntervalTracedValue
(define (interval-traced iv id tape)
  (doc 'export #t)
  (list 'interval-traced iv id tape))

;;; interval-traced? : Any -> Boolean
(define (interval-traced? x)
  (doc 'export #t)
  (and (pair? x)
       (eq? (car x) 'interval-traced)))

;;; interval-traced-value : IntervalTracedValue -> Interval
(define (interval-traced-value t)
  (if (interval-traced? t)
      (cadr t)
      (if (interval? t) t (interval-singleton t))))

;;; interval-traced-id : IntervalTracedValue -> Nat | #f
(define (interval-traced-id t)
  (if (interval-traced? t) (caddr t) #f))

;;; interval-traced-tape : IntervalTracedValue -> Tape | #f
(define (interval-traced-tape t)
  (if (interval-traced? t) (cadddr t) #f))

;;; ============================================================================
;;; Interval Tape
;;; ============================================================================

;;; make-interval-tape : Unit -> IntervalTape
(define (make-interval-tape)
  (list 'interval-tape (box '())))

;;; interval-tape? : Any -> Boolean
(define (interval-tape? t)
  (and (pair? t)
       (eq? (car t) 'interval-tape)))

;;; interval-tape-entries : IntervalTape -> List
(define (interval-tape-entries t)
  (unbox (cadr t)))

;;; interval-tape-push! : IntervalTape x Entry -> Unit
(define (interval-tape-push! tape entry)
  (set-box! (cadr tape)
            (cons entry (unbox (cadr tape)))))

;;; ============================================================================
;;; Interval-Traced Variable Creation
;;; ============================================================================

;;; make-interval-traced-var : Interval x IntervalTape -> IntervalTracedValue
;;; Create an interval-traced variable (input).
(define (make-interval-traced-var iv tape)
  (let ([id (interval-fresh-id)])
    (interval-traced iv id tape)))

;;; ============================================================================
;;; Interval-Traced Operations
;;; ============================================================================
;;;
;;; Each operation records:
;;;   - The interval result
;;;   - Interval bounds on the local gradients (partial derivatives)
;;;
;;; For f(a, b), we need intervals for df/da and df/db over the input intervals.

;;; Helper: record operation and return interval-traced result
(define (interval-traced-op op-name result-iv inputs local-grad-ivs tape)
  (let ([result-id (interval-fresh-id)]
        [input-ids (map (lambda (x) (if (interval-traced? x) (interval-traced-id x) #f))
                        inputs)])
    ;; Record: (result-id op-name input-ids local-grad-ivs)
    (interval-tape-push! tape
                         (list result-id op-name input-ids local-grad-ivs))
    (interval-traced result-iv result-id tape)))

;;; iv-traced-add : IntervalTracedValue x IntervalTracedValue -> IntervalTracedValue
;;; d(a+b)/da = 1, d(a+b)/db = 1 (constant gradients)
(define (iv-traced-add a b)
  (doc 'export #t)
  (let* ([av (interval-traced-value a)]
         [bv (interval-traced-value b)]
         [result (interval-add av bv)]
         [tape (or (and (interval-traced? a) (interval-traced-tape a))
                   (and (interval-traced? b) (interval-traced-tape b)))])
    (if tape
        (interval-traced-op 'add result (list a b)
                            (list (interval-singleton 1)
                                  (interval-singleton 1))
                            tape)
        result)))

;;; iv-traced-sub : IntervalTracedValue x IntervalTracedValue -> IntervalTracedValue
;;; d(a-b)/da = 1, d(a-b)/db = -1
(define (iv-traced-sub a b)
  (doc 'export #t)
  (let* ([av (interval-traced-value a)]
         [bv (interval-traced-value b)]
         [result (interval-sub av bv)]
         [tape (or (and (interval-traced? a) (interval-traced-tape a))
                   (and (interval-traced? b) (interval-traced-tape b)))])
    (if tape
        (interval-traced-op 'sub result (list a b)
                            (list (interval-singleton 1)
                                  (interval-singleton -1))
                            tape)
        result)))

;;; iv-traced-mul : IntervalTracedValue x IntervalTracedValue -> IntervalTracedValue
;;; d(a*b)/da = b, d(a*b)/db = a (gradients are the other operand)
(define (iv-traced-mul a b)
  (doc 'export #t)
  (let* ([av (interval-traced-value a)]
         [bv (interval-traced-value b)]
         [result (interval-mul av bv)]
         [tape (or (and (interval-traced? a) (interval-traced-tape a))
                   (and (interval-traced? b) (interval-traced-tape b)))])
    (if tape
        (interval-traced-op 'mul result (list a b)
                            (list bv av)  ; Local gradients are interval-valued
                            tape)
        result)))

;;; iv-traced-div : IntervalTracedValue x IntervalTracedValue -> IntervalTracedValue
;;; d(a/b)/da = 1/b, d(a/b)/db = -a/b^2
(define (iv-traced-div a b)
  (doc 'export #t)
  (let* ([av (interval-traced-value a)]
         [bv (interval-traced-value b)]
         [result (interval-div av bv)])
    (if (error? result)
        result
        (let ([tape (or (and (interval-traced? a) (interval-traced-tape a))
                        (and (interval-traced? b) (interval-traced-tape b)))])
          (if tape
              (let* ([one (interval-singleton 1)]
                     [recip-b (interval-recip bv)]
                     [b-sq (interval-sqr bv)]
                     [neg-a-over-b2 (interval-div (interval-neg av) b-sq)])
                (if (or (error? recip-b) (error? neg-a-over-b2))
                    '(error division-by-zero)
                    (interval-traced-op 'div result (list a b)
                                        (list recip-b neg-a-over-b2)
                                        tape)))
              result)))))

;;; iv-traced-neg : IntervalTracedValue -> IntervalTracedValue
;;; d(-a)/da = -1
(define (iv-traced-neg a)
  (let* ([av (interval-traced-value a)]
         [result (interval-neg av)]
         [tape (and (interval-traced? a) (interval-traced-tape a))])
    (if tape
        (interval-traced-op 'neg result (list a)
                            (list (interval-singleton -1))
                            tape)
        result)))

;;; iv-traced-sqr : IntervalTracedValue -> IntervalTracedValue
;;; d(a^2)/da = 2a
(define (iv-traced-sqr a)
  (doc 'export #t)
  (let* ([av (interval-traced-value a)]
         [result (interval-sqr av)]
         [tape (and (interval-traced? a) (interval-traced-tape a))])
    (if tape
        (interval-traced-op 'sqr result (list a)
                            (list (interval-scale av 2))  ; 2*a as interval
                            tape)
        result)))

;;; iv-traced-sqrt : IntervalTracedValue -> IntervalTracedValue
;;; d(sqrt(a))/da = 1/(2*sqrt(a))
(define (iv-traced-sqrt a)
  (doc 'export #t)
  (let* ([av (interval-traced-value a)]
         [result (interval-sqrt av)])
    (if (error? result)
        result
        (let ([tape (and (interval-traced? a) (interval-traced-tape a))])
          (if tape
              (let* ([two (interval-singleton 2)]
                     [two-sqrt-a (interval-mul two result)]
                     [grad (interval-recip two-sqrt-a)])
                (if (error? grad)
                    ;; Handle sqrt at 0: gradient is +inf
                    (interval-traced-op 'sqrt result (list a)
                                        (list (interval 0 1e308))
                                        tape)
                    (interval-traced-op 'sqrt result (list a)
                                        (list grad)
                                        tape)))
              result)))))

;;; iv-traced-exp : IntervalTracedValue -> IntervalTracedValue
;;; d(exp(a))/da = exp(a)
(define (iv-traced-exp a)
  (doc 'export #t)
  (let* ([av (interval-traced-value a)]
         [result (interval-exp av)]
         [tape (and (interval-traced? a) (interval-traced-tape a))])
    (if tape
        (interval-traced-op 'exp result (list a)
                            (list result)  ; Gradient is exp(a) itself
                            tape)
        result)))

;;; iv-traced-log : IntervalTracedValue -> IntervalTracedValue
;;; d(log(a))/da = 1/a
(define (iv-traced-log a)
  (doc 'export #t)
  (let* ([av (interval-traced-value a)]
         [result (interval-log av)])
    (if (error? result)
        result
        (let ([tape (and (interval-traced? a) (interval-traced-tape a))])
          (if tape
              (let ([grad (interval-recip av)])
                (if (error? grad)
                    '(error domain-error)
                    (interval-traced-op 'log result (list a)
                                        (list grad)
                                        tape)))
              result)))))

;;; iv-traced-sin : IntervalTracedValue -> IntervalTracedValue
;;; d(sin(a))/da = cos(a)
(define (iv-traced-sin a)
  (doc 'export #t)
  (let* ([av (interval-traced-value a)]
         [result (interval-sin av)]
         [grad (interval-cos av)]  ; Gradient is cos over the interval
         [tape (and (interval-traced? a) (interval-traced-tape a))])
    (if tape
        (interval-traced-op 'sin result (list a) (list grad) tape)
        result)))

;;; iv-traced-cos : IntervalTracedValue -> IntervalTracedValue
;;; d(cos(a))/da = -sin(a)
(define (iv-traced-cos a)
  (doc 'export #t)
  (let* ([av (interval-traced-value a)]
         [result (interval-cos av)]
         [grad (interval-neg (interval-sin av))]  ; Gradient is -sin
         [tape (and (interval-traced? a) (interval-traced-tape a))])
    (if tape
        (interval-traced-op 'cos result (list a) (list grad) tape)
        result)))

;;; iv-traced-pow : IntervalTracedValue x Number -> IntervalTracedValue
;;; d(a^n)/da = n * a^(n-1)
(define (iv-traced-pow base n)
  (doc 'export #t)
  (let* ([bv (interval-traced-value base)]
         [result (interval-pow bv n)]
         [tape (and (interval-traced? base) (interval-traced-tape base))])
    (if (error? result)
        result
        (if tape
            (let* ([n-iv (interval-singleton n)]
                   [n-1-iv (interval-pow bv (- n 1))]
                   [grad (if (error? n-1-iv)
                             (interval 0 1e308)  ; Unbounded gradient
                             (interval-mul n-iv n-1-iv))])
              (interval-traced-op 'pow result (list base)
                                  (list grad)
                                  tape))
            result))))

;;; ============================================================================
;;; Backward Pass (Interval Gradient Computation)
;;; ============================================================================
;;;
;;; The backward pass computes interval bounds on gradients by propagating
;;; intervals through the chain rule.

;;; interval-backward : IntervalTape x Nat x Interval -> HAMT
;;; Backward pass computing interval gradients for all variables.
;;; Returns HAMT: id -> interval-gradient
(define (interval-backward tape output-id seed-grad)
  (let ([entries (interval-tape-entries tape)])
    ;; Process tape from output toward inputs, threading HAMT
    (fold-left
     (lambda (grads entry)
       (let* ([result-id (car entry)]
              [op-name (cadr entry)]
              [input-ids (caddr entry)]
              [local-grads (cadddr entry)]
              [result-grad (hamt-lookup-or result-id grads (interval-singleton 0))])
         ;; Propagate gradient to inputs via chain rule
         (fold-left
          (lambda (g id-grad)
            (let ([input-id (car id-grad)]
                  [local-grad (cdr id-grad)])
              (if input-id
                  (let* ([contrib (interval-mul result-grad local-grad)]
                         [current (hamt-lookup-or input-id g (interval-singleton 0))]
                         [new-grad (interval-add current contrib)])
                    (hamt-assoc input-id new-grad g))
                  g)))
          grads
          (map cons input-ids local-grads))))
     ;; Initialize with output gradient
     (hamt-assoc output-id seed-grad hamt-empty)
     entries)))

;;; ============================================================================
;;; High-Level Interface
;;; ============================================================================

;;; interval-gradient : ((List IntervalTracedValue) -> IntervalTracedValue) x Box -> (List Interval)
;;; Compute interval bounds on gradient of f over the given box.
;;;
;;; f should be a function that takes a LIST of interval-traced values and returns
;;; an interval-traced result using the iv-traced-* operations.
;;;
;;; Returns a list of intervals, one per input dimension, bounding the
;;; partial derivatives over the box.
(define (interval-gradient f box)
  (doc 'export #t)
  (reset-interval-traced-ids!)
  (let* ([tape (make-interval-tape)]
         [traced-inputs (map (lambda (iv) (make-interval-traced-var iv tape))
                             box)]
         [output (f traced-inputs)]  ; f takes a list, not varargs
         [output-id (interval-traced-id output)]
         [n (length box)]
         [input-ids (map interval-traced-id traced-inputs)])
    (cond
      ;; Output is not traced (constant): all gradients are zero
      [(not output-id)
       (make-list n (interval-singleton 0))]
      ;; Output is one of the inputs (identity): gradient is 1 for that input, 0 for others
      [(memv output-id input-ids)
       (map (lambda (id)
              (if (= id output-id)
                  (interval-singleton 1)
                  (interval-singleton 0)))
            input-ids)]
      ;; Normal case: run backward pass
      [else
       (let ([grads (interval-backward tape output-id (interval-singleton 1))])
         (map (lambda (id)
                (hamt-lookup-or id grads (interval-singleton 0)))
              input-ids))])))

;; make-list is provided by prelude (alias for replicate)

;;; ============================================================================
;;; Monotonicity Analysis for Optimization
;;; ============================================================================

;;; gradient-sign : Interval -> Symbol
;;; Determine the sign of a gradient interval.
;;; Returns: 'positive, 'negative, 'zero, or 'mixed
(define (gradient-sign iv)
  (doc 'export #t)
  (let ([lo (interval-lo iv)]
        [hi (interval-hi iv)])
    (cond
      [(and (= lo 0) (= hi 0)) 'zero]
      [(> lo 0) 'positive]         ; Entire interval is positive
      [(< hi 0) 'negative]         ; Entire interval is negative
      [else 'mixed])))             ; Contains zero

;;; monotonicity-info : (List Interval) -> (List Symbol)
;;; Analyze monotonicity in each dimension.
(define (monotonicity-info grad-intervals)
  (doc 'export #t)
  (map gradient-sign grad-intervals))

;;; all-monotonic? : (List Interval) -> Boolean
;;; True if gradient doesn't contain zero in ANY dimension.
;;; This means the minimum must be on a corner of the box.
(define (all-monotonic? grad-intervals)
  (doc 'export #t)
  (not (any (lambda (iv)
              (interval-contains-zero? iv))
            grad-intervals)))

;;; any : (a -> Boolean) x (List a) -> Boolean
(define (any pred lst)
  (cond
    [(null? lst) #f]
    [(pred (car lst)) #t]
    [else (any pred (cdr lst))]))

;;; monotonic-dimension : (List Interval) -> Nat | #f
;;; Find the first dimension where gradient is monotonic (doesn't contain zero).
;;; Returns the dimension index, or #f if all dimensions contain zero.
(define (monotonic-dimension grad-intervals)
  (let loop ([ivs grad-intervals] [idx 0])
    (cond
      [(null? ivs) #f]
      [(not (interval-contains-zero? (car ivs))) idx]
      [else (loop (cdr ivs) (+ idx 1))])))

;;; reduce-box-by-monotonicity : Box x (List Interval) -> Box | 'minimum-on-boundary
;;; If a gradient component doesn't contain zero, reduce the box to the
;;; appropriate face (where the minimum must occur).
;;;
;;; For minimization:
;;;   - If df/dx_i > 0 over box: minimum is at x_i = lo_i (lower face)
;;;   - If df/dx_i < 0 over box: minimum is at x_i = hi_i (upper face)
;;;
;;; Returns 'minimum-on-boundary if box would reduce to a single point,
;;; indicating the minimum is at a corner.
(define (reduce-box-by-monotonicity box grad-intervals)
  (doc 'export #t)
  (let loop ([box box] [grads grad-intervals] [idx 0] [new-box '()])
    (cond
      [(null? box)
       (reverse new-box)]
      [else
       (let* ([iv (car box)]
              [grad (car grads)]
              [sign (gradient-sign grad)])
         (case sign
           [(positive)
            ;; df/dx > 0: minimum at lower bound
            (loop (cdr box) (cdr grads) (+ idx 1)
                  (cons (interval-singleton (interval-lo iv)) new-box))]
           [(negative)
            ;; df/dx < 0: minimum at upper bound
            (loop (cdr box) (cdr grads) (+ idx 1)
                  (cons (interval-singleton (interval-hi iv)) new-box))]
           [else
            ;; Mixed or zero: keep full interval
            (loop (cdr box) (cdr grads) (+ idx 1)
                  (cons iv new-box))]))])))

;;; ============================================================================
;;; Integration with Interval Global Optimization
;;; ============================================================================

;;; with-monotonicity-pruning : ((Box -> Interval) x (Box -> (List Interval))) -> (Box -> Interval)
;;; Wrap an interval objective function with monotonicity-based box reduction.
;;;
;;; Arguments:
;;;   f-interval: objective function (Box -> Interval)
;;;   grad-fn: interval gradient function (Box -> (List Interval))
;;;
;;; Returns a new objective function that:
;;;   1. Computes gradient intervals
;;;   2. Reduces box if monotonicity detected
;;;   3. Evaluates objective on reduced box
;;;
;;; Note: This is a simple wrapper. For full integration with bb-loop,
;;; use the monotonicity-check in the optimization loop directly.
(define (with-monotonicity-pruning f-interval grad-fn)
  (lambda (box)
    (let* ([grad-ivs (grad-fn box)]
           [reduced-box (reduce-box-by-monotonicity box grad-ivs)])
      (f-interval reduced-box))))

;;; ============================================================================
;;; Convenience: Convert scalar function to interval-traced function
;;; ============================================================================

;;; The user defines their objective using iv-traced-* operations.
;;; Example:
;;;   (define (my-f-traced inputs)
;;;     (let ([x (car inputs)] [y (cadr inputs)])
;;;       (iv-traced-add (iv-traced-sqr x)
;;;                      (iv-traced-sqr y))))
;;;
;;;   (interval-gradient my-f-traced (list (interval 0 1) (interval 0 1)))

;;; ============================================================================
;;; Display
;;; ============================================================================

(display "Use (interval-gradient f box) to compute interval gradient bounds.\n")
(display "Use (monotonicity-info grad-ivs) to analyze monotonicity.\n")
