;;; @module traced-optics
;;; @requires prelude reverse-diff

(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
(require 'prelude)
(require 'reverse-diff)
(load "lattice/optics/optics.ss")
(load "lattice/physics/diff/traced-vec2.ss")

(doc 'module 'traced-optics)
(doc 'bridges '(optics autodiff physics))
(doc 'description "Optics + Autodiff Integration - enables computing gradients through optic-focused paths")
(doc 'layer 'lattice)
(doc 'purity 'partial)
(doc 'note "Core insight: use optics to specify WHICH parameter to differentiate, and autodiff to compute the gradient THROUGH that path. User API: optic-gradient, grad-at, optimize-at")

(doc 'section 'value-tracing)

(define (trace-value v tape)
  (doc 'export #t)
  (doc 'type '(-> alpha Tape alpha-traced))
  (doc 'description "Recursively trace value based on structure - follows 'pair of traced' pattern")
  (cond
    [(traced? v) v]  ; Already traced, return as-is
    [(number? v) (make-traced-var v tape)]
    [(vec2? v) (lift-vec2 v tape)]
    [(and (pair? v) (not (list? v)))
     ;; Pairs: trace both elements
     (cons (trace-value (car v) tape)
           (trace-value (cdr v) tape))]
    [(list? v)
     ;; Lists: trace each element
     (map (lambda (x) (trace-value x tape)) v)]
    [else v]))  ; Unknown structure: treat as constant

;;; untrace-value : alpha-traced -> alpha
;;; Extract numerical values from traced structure.
(define (untrace-value v)
  (doc 'export #t)
  (cond
    [(traced? v) (traced-value v)]
    [(traced-vec2? v) (unpack-traced-vec2 v)]
    [(and (pair? v) (not (list? v)))
     (cons (untrace-value (car v))
           (untrace-value (cdr v)))]
    [(list? v) (map untrace-value v)]
    [else v]))

;;; ============================================================
;;; Part 2: Optic-Directed Tracing
;;; ============================================================
;;;
;;; lift-at-optic: The key innovation. Instead of tracing the entire
;;; structure, we trace only the focus of the optic. The container
;;; stays untraced, enabling efficient gradient computation.

;;; lift-at-optic : Optic x s x Tape -> (s-with-traced-focus, traced-focus)
;;; Lift structure with traced focus based on optic type.
;;; Returns both the modified structure and the traced focus for backprop.
;;;
;;; CRITICAL: The input s must NOT be traced. This function enforces
;;; the "pair of traced" pattern - the container is untraced, only
;;; the leaves (at optic focus) are traced.
(define (lift-at-optic optic s tape)
  (doc 'export #t)
  ;; Fail-fast on traced containers (per Gemini QA feedback)
  (when (traced? s)
    (error 'lift-at-optic
           "Expected structure with traced leaves, got traced container.
            Use pair-of-traced pattern like TracedVec2."))

  (let ([otype (optic-type optic)])
    (cond
      ;; Lens: Single focus
      [(eq? otype 'lens)
       (let* ([focus (view optic s)]
              [traced-focus (trace-value focus tape)]
              [s-traced (set-lens optic traced-focus s)])
         (values s-traced traced-focus))]

      ;; Prism: Maybe focus (0 or 1 targets)
      [(eq? otype 'prism)
       (let ([result (preview optic s)])
         (if (nothing? result)
             ;; No match - return original structure, no traced focus
             (values s nothing)
             (let* ([focus (from-just result)]
                    [traced-focus (trace-value focus tape)]
                    [s-traced (review optic traced-focus)])
               (values s-traced (just traced-focus)))))]

      ;; Affine: Maybe focus (0 or 1 targets)
      [(eq? otype 'affine)
       (let ([result (preview optic s)])
         (if (nothing? result)
             (values s nothing)
             (let* ([focus (from-just result)]
                    [traced-focus (trace-value focus tape)]
                    [s-traced (affine-set optic traced-focus s)])
               (values s-traced (just traced-focus)))))]

      ;; Traversal: List of focuses (0+ targets)
      [(eq? otype 'traversal)
       (let* ([focuses (traversal-to-list optic s)]
              [traced-focuses (map (lambda (f) (trace-value f tape)) focuses)])
         ;; Rebuild structure with traced focuses
         ;; Special case for lists with traversal-each: use direct mapping
         ;; to avoid Chez Scheme's evaluation order issues with traversal-over
         (if (and (list? s) (eq? optic traversal-each))
             (values traced-focuses traced-focuses)
             ;; General case: rebuild using traversal with queue
             (let ([queue traced-focuses])
               (values
                 (traversal-over optic
                                 (lambda (_)
                                   (let ([v (car queue)])
                                     (set! queue (cdr queue))
                                     v))
                                 s)
                 traced-focuses))))]

      ;; Iso: Like lens (single focus, fully reversible)
      [(eq? otype 'iso)
       (let* ([focus (iso-view optic s)]
              [traced-focus (trace-value focus tape)]
              [s-traced (iso-review optic traced-focus)])
         (values s-traced traced-focus))]

      ;; Getter: Read-only, cannot lift for gradient (no set operation)
      [(eq? otype 'getter)
       (error 'lift-at-optic
              "Cannot compute gradient through Getter (read-only optic).
               Use Lens or Traversal for modifiable access.")]

      ;; Fold: Read-only with multiple targets
      [(eq? otype 'fold)
       (error 'lift-at-optic
              "Cannot compute gradient through Fold (read-only optic).
               Use Traversal for modifiable multi-target access.")]

      [else
       (error 'lift-at-optic "Unknown optic type" otype)])))

;;; traversal-over-indexed : Traversal x (Int x a -> b) x s -> t
;;; Helper: Apply indexed function to each traversal target.
(define (traversal-over-indexed trav f s)
  (let ([idx (box 0)])
    (traversal-over trav
                    (lambda (a)
                      (let ([i (unbox idx)])
                        (set-box! idx (+ i 1))
                        (f i a)))
                    s)))

;;; ============================================================
;;; Part 3: Optic-Based Gradient Computation
;;; ============================================================

;;; optic-gradient : ((s-traced -> Traced) x Optic x s) -> gradient
;;; Compute gradient of loss function with respect to optic focus.
;;;
;;; Arguments:
;;;   loss-fn: Function from traced structure to traced scalar loss
;;;   optic: Optic focusing on the parameter(s) to differentiate
;;;   s: The structure containing the parameter
;;;
;;; Returns:
;;;   Gradient of the loss with respect to the focus, same shape as focus.
(define (optic-gradient loss-fn optic s)
  (doc 'export #t)
  (with-fresh-ad-scope
   (lambda ()
     (let ([tape (make-reverse-tape)])
       (let-values ([(s-traced traced-focus) (lift-at-optic optic s tape)])
         ;; Forward pass: compute loss
         (let ([loss (loss-fn s-traced)])
           ;; Backward pass: compute gradients
           (if (traced? loss)
               (let ([grads (backward tape (traced-id loss) 1)])
                 (extract-gradient traced-focus grads))
               ;; Constant loss - gradient is zero
               (zero-like traced-focus))))))))

;;; extract-gradient : TracedFocus x GradTable -> Gradient
;;; Extract gradient values from backward pass results.
(define (extract-gradient traced-focus grads)
  (cond
    [(traced? traced-focus)
     (hashtable-ref grads (traced-id traced-focus) 0)]

    [(traced-vec2? traced-focus)
     (vec2 (hashtable-ref grads (traced-id (traced-vec2-x traced-focus)) 0)
           (hashtable-ref grads (traced-id (traced-vec2-y traced-focus)) 0))]

    [(just? traced-focus)
     (just (extract-gradient (from-just traced-focus) grads))]

    [(nothing? traced-focus)
     nothing]

    [(list? traced-focus)
     (map (lambda (tf) (extract-gradient tf grads)) traced-focus)]

    [(and (pair? traced-focus) (not (list? traced-focus)))
     (cons (extract-gradient (car traced-focus) grads)
           (extract-gradient (cdr traced-focus) grads))]

    [else 0]))  ; Unknown type, assume zero gradient

;;; zero-like : TracedFocus -> ZeroGradient
;;; Create a zero gradient matching the structure of the focus.
(define (zero-like traced-focus)
  (cond
    [(traced? traced-focus) 0]
    [(traced-vec2? traced-focus) (vec2 0 0)]
    [(just? traced-focus) (just (zero-like (from-just traced-focus)))]
    [(nothing? traced-focus) nothing]
    [(list? traced-focus) (map zero-like traced-focus)]
    [(and (pair? traced-focus) (not (list? traced-focus)))
     (cons (zero-like (car traced-focus))
           (zero-like (cdr traced-focus)))]
    [else 0]))

;;; ============================================================
;;; Part 4: Traversal Gradient (Multiple Focuses)
;;; ============================================================

;;; optic-gradient-list : ((s-traced -> Traced) x Traversal x s) -> (List gradient)
;;; Compute gradient for each focus of a traversal.
;;; Returns a list of gradients, one per focus.
(define (optic-gradient-list loss-fn trav s)
  (doc 'export #t)
  (with-fresh-ad-scope
   (lambda ()
     (let ([tape (make-reverse-tape)])
       (let-values ([(s-traced traced-focuses) (lift-at-optic trav s tape)])
         (let ([loss (loss-fn s-traced)])
           (if (traced? loss)
               (let ([grads (backward tape (traced-id loss) 1)])
                 (map (lambda (tf) (extract-gradient tf grads)) traced-focuses))
               ;; Constant loss
               (map zero-like traced-focuses))))))))

;;; ============================================================
;;; Part 5: Partial Optic Gradient (Maybe)
;;; ============================================================

;;; optic-gradient-maybe : ((s-traced -> Traced) x Affine x s) -> Maybe gradient
;;; Compute gradient through a partial optic (prism/affine).
;;; Returns nothing if the optic doesn't match, just(gradient) otherwise.
(define (optic-gradient-maybe loss-fn affine s)
  (doc 'export #t)
  (with-fresh-ad-scope
   (lambda ()
     (let ([tape (make-reverse-tape)])
       (let-values ([(s-traced maybe-traced-focus) (lift-at-optic affine s tape)])
         (if (nothing? maybe-traced-focus)
             nothing  ; Prism/affine didn't match - no gradient flow
             (let ([traced-focus (from-just maybe-traced-focus)]
                   [loss (loss-fn s-traced)])
               (if (traced? loss)
                   (let ([grads (backward tape (traced-id loss) 1)])
                     (just (extract-gradient traced-focus grads)))
                   (just (zero-like traced-focus))))))))))

;;; ============================================================
;;; Part 6: High-Level API
;;; ============================================================

;;; grad-at : Optic x s x (s-traced -> Traced) -> gradient
;;; Shorthand for optic-gradient with curried argument order.
;;; Preferred for pipeline style: (grad-at my-lens my-struct my-loss)
(define (grad-at optic s loss-fn)
  (doc 'export #t)
  (optic-gradient loss-fn optic s))

;;; optimize-at : Optic x s x (s-traced -> Traced) x Number -> s
;;; Single gradient descent step at optic focus.
;;; Updates: focus' = focus - rate * gradient
;;; Note: For traversals, use optimize-traversal instead.
(define (optimize-at optic s loss-fn rate)
  (doc 'export #t)
  (let ([otype (optic-type optic)])
    (case otype
      [(lens iso affine)
       ;; Single-focus optics: straightforward update
       (let ([grad (optic-gradient loss-fn optic s)]
             [focus (view optic s)])
         (set-lens optic (subtract-scaled focus grad rate) s))]
      [(traversal)
       ;; Multi-focus optics: use optimize-traversal
       (optimize-traversal optic s loss-fn rate)]
      [(prism)
       ;; Prism: only update if matched
       (let ([maybe-grad (optic-gradient-maybe loss-fn optic s)])
         (if (nothing? maybe-grad)
             s  ; No match, return unchanged
             (let ([grad (from-just maybe-grad)]
                   [focus (from-just (preview optic s))])
               (review optic (subtract-scaled focus grad rate)))))]
      [else
       (error 'optimize-at "Unsupported optic type for optimization" otype)])))

;;; optimize-traversal : Traversal x s x (s-traced -> Traced) x Number -> s
;;; Gradient descent step for all targets of a traversal.
;;; Each target is updated independently: target_i' = target_i - rate * grad_i
;;;
;;; Note: Due to Chez Scheme's evaluation order, we can't rely on mutable
;;; counters inside traversal-over. For list structures, we compute new values
;;; directly using map. For other structures, we use sequential rebuilding.
(define (optimize-traversal trav s loss-fn rate)
  (let* ([grads (optic-gradient-list loss-fn trav s)]
         [focuses (traversal-to-list trav s)]
         [new-vals (map (lambda (v g) (subtract-scaled v g rate)) focuses grads)])
    (cond
      ;; Special case: list structures - return new-vals directly
      ;; This works because traversal-each on lists just maps over elements
      [(and (list? s) (eq? trav traversal-each))
       new-vals]
      ;; General case: use sequential rebuilding with a queue
      [else
       (let ([queue new-vals])
         (traversal-over trav
                         (lambda (_)
                           (if (null? queue)
                               (error 'optimize-traversal "Queue exhausted")
                               (let ([v (car queue)])
                                 (set! queue (cdr queue))
                                 v)))
                         s))])))

;;; subtract-scaled : alpha x gradient x rate -> alpha
;;; Helper: Compute value - rate * gradient, matching structure.
(define (subtract-scaled value grad rate)
  (cond
    [(number? value)
     (- value (* rate grad))]
    [(vec2? value)
     (vec2 (- (vec2-x value) (* rate (vec2-x grad)))
           (- (vec2-y value) (* rate (vec2-y grad))))]
    [(list? value)
     (map (lambda (v g) (subtract-scaled v g rate)) value grad)]
    [(and (pair? value) (not (list? value)))
     (cons (subtract-scaled (car value) (car grad) rate)
           (subtract-scaled (cdr value) (cdr grad) rate))]
    [else value]))

;;; optimize-steps : Optic x s x (s-traced -> Traced) x Number x Nat -> s
;;; Multiple gradient descent steps.
(define (optimize-steps optic s loss-fn rate n)
  (doc 'export #t)
  (if (<= n 0)
      s
      (optimize-steps optic
                      (optimize-at optic s loss-fn rate)
                      loss-fn rate (- n 1))))

;;; ============================================================
;;; Part 7: Composed Optic Gradient
;;; ============================================================
;;;
;;; Composition `(>>> outer-lens inner-lens)` focuses deeper in the structure.
;;; The gradient computation "flows through" the composition automatically
;;; because lift-at-optic works with the composed optic.

;;; composed-gradient : ((s-traced -> Traced) x Optic x Optic x s) -> gradient
;;; Gradient through composed optics.
;;; Equivalent to: (optic-gradient loss-fn (>>> outer inner) s)
(define (composed-gradient loss-fn outer inner s)
  (doc 'export #t)
  (optic-gradient loss-fn (>>> outer inner) s))

;;; ============================================================
;;; Part 8: Utility Functions
;;; ============================================================

;;; lift-const : alpha -> alpha (untraced)
;;; Wrap a value as a constant (no gradient flow).
;;; Use for values that shouldn't be differentiated.
(define (lift-const v)
  (doc 'export #t)
  v)  ; Simply return as-is (no tracing)

;;; traced-sum : (List Traced) -> Traced
;;; Sum a list of traced values.
(define (traced-sum traced-list)
  (doc 'export #t)
  (if (null? traced-list)
      0  ; Empty sum is 0 (constant)
      (fold-left traced-add (car traced-list) (cdr traced-list))))

;;; traced-mean : (List Traced) -> Traced
;;; Mean of traced values.
(define (traced-mean traced-list)
  (doc 'export #t)
  (if (null? traced-list)
      0
      (traced-div (traced-sum traced-list) (length traced-list))))

;;; traced-l2-norm-sq : (List Traced) -> Traced
;;; Squared L2 norm: sum of squares.
(define (traced-l2-norm-sq traced-list)
  (doc 'export #t)
  (traced-sum (map traced-sq traced-list)))

;;; ============================================================
;;; Part 9: Value-and-Gradient Variant
;;; ============================================================

;;; optic-value-and-gradient : ((s-traced -> Traced) x Optic x s) -> (Values Number gradient)
;;; Compute both loss value and gradient in one pass.
(define (optic-value-and-gradient loss-fn optic s)
  (doc 'export #t)
  (with-fresh-ad-scope
   (lambda ()
     (let ([tape (make-reverse-tape)])
       (let-values ([(s-traced traced-focus) (lift-at-optic optic s tape)])
         (let ([loss (loss-fn s-traced)])
           (if (traced? loss)
               (let ([val (traced-value loss)]
                     [grads (backward tape (traced-id loss) 1)])
                 (values val (extract-gradient traced-focus grads)))
               ;; Constant loss
               (values loss (zero-like traced-focus)))))))))
