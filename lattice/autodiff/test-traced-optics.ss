;;; lattice/autodiff/test-traced-optics.ss --- Tests for Optics + Autodiff Integration
;;;
;;; Tests for gradient computation through optic-focused paths.

(load "core/testing/test-framework.ss")
(load "lattice/autodiff/traced-optics.ss")

;;; ============================================================
;;; Test Data
;;; ============================================================

(define test-pair '(3.0 . 4.0))
(define test-nested '((1.0 . 2.0) . (3.0 . 4.0)))
(define test-list '(1.0 2.0 3.0 4.0 5.0))
(define test-vec2-pair (cons (vec2 1.0 2.0) (vec2 3.0 4.0)))

;;; ============================================================
;;; Basic Value Tracing Tests
;;; ============================================================

(test-group "Value Tracing"
  (define-test "trace-value traces numbers"
    (with-fresh-ad-scope
     (lambda ()
       (let* ([tape (make-reverse-tape)]
              [traced (trace-value 5.0 tape)])
         (assert-true (traced? traced))
         (assert-equal 5.0 (traced-value traced))))))

  (define-test "trace-value traces vec2"
    (with-fresh-ad-scope
     (lambda ()
       (let* ([tape (make-reverse-tape)]
              [v (vec2 3.0 4.0)]
              [traced (trace-value v tape)])
         (assert-true (traced-vec2? traced))
         (assert-equal 3.0 (traced-value (traced-vec2-x traced)))
         (assert-equal 4.0 (traced-value (traced-vec2-y traced)))))))

  (define-test "trace-value traces lists"
    (with-fresh-ad-scope
     (lambda ()
       (let* ([tape (make-reverse-tape)]
              [traced (trace-value '(1.0 2.0 3.0) tape)])
         (assert-equal 3 (length traced))
         (assert-true (traced? (car traced)))
         (assert-equal 1.0 (traced-value (car traced)))))))

  (define-test "trace-value traces pairs"
    (with-fresh-ad-scope
     (lambda ()
       (let* ([tape (make-reverse-tape)]
              [traced (trace-value '(3.0 . 4.0) tape)])
         (assert-true (traced? (car traced)))
         (assert-true (traced? (cdr traced)))))))

  (define-test "untrace-value recovers original values"
    (with-fresh-ad-scope
     (lambda ()
       (let* ([tape (make-reverse-tape)]
              [v (vec2 3.0 4.0)]
              [traced (trace-value v tape)])
         (let ([untraced (untrace-value traced)])
           (assert-true (vec2? untraced))
           (assert-equal 3.0 (vec2-x untraced))
           (assert-equal 4.0 (vec2-y untraced))))))))

;;; ============================================================
;;; Lens Gradient Tests
;;; ============================================================

(test-group "Lens Gradient"
  (define-test "gradient through lens-fst equals manual computation"
    ;; f(pair) = (car pair)^2, gradient w.r.t. car is 2*car
    (let ([grad (optic-gradient
                 (lambda (p) (traced-sq (car p)))
                 lens-fst
                 '(3.0 . 4.0))])
      (assert-equal 6.0 grad)))  ; 2 * 3.0 = 6.0

  (define-test "gradient through lens-snd works correctly"
    ;; f(pair) = (cdr pair)^2, gradient w.r.t. cdr is 2*cdr
    (let ([grad (optic-gradient
                 (lambda (p) (traced-sq (cdr p)))
                 lens-snd
                 '(3.0 . 4.0))])
      (assert-equal 8.0 grad)))  ; 2 * 4.0 = 8.0

  (define-test "gradient with addition loss"
    ;; f(pair) = car + 2*cdr, df/d(car) = 1
    (let ([grad (optic-gradient
                 (lambda (p) (traced-add (car p) (traced-mul (cdr p) 2)))
                 lens-fst
                 '(3.0 . 4.0))])
      (assert-equal 1 grad)))

  (define-test "gradient with constant loss returns zero"
    (let ([grad (optic-gradient
                 (lambda (p) 42)  ; Constant, no traced
                 lens-fst
                 '(3.0 . 4.0))])
      (assert-equal 0 grad))))

;;; ============================================================
;;; Composed Lens Gradient Tests
;;; ============================================================

(test-group "Composed Lens Gradient"
  (define-test "gradient through composed lenses chains correctly"
    ;; Nested: ((1.0 . 2.0) . (3.0 . 4.0))
    ;; Focus on inner-left-fst via: fst . fst
    ;; f(s) = (caar s)^2, gradient = 2 * caar = 2.0
    (let* ([composed (>>> lens-fst lens-fst)]
           [grad (optic-gradient
                  (lambda (s) (traced-sq (view composed s)))
                  composed
                  test-nested)])
      (assert-equal 2.0 grad)))

  (define-test "gradient through deeper composition"
    ;; Focus on inner-right-snd via: snd . snd
    ;; f(s) = (cddr s)^2, gradient = 2 * 4.0 = 8.0
    (let* ([composed (>>> lens-snd lens-snd)]
           [grad (optic-gradient
                  (lambda (s) (traced-sq (view composed s)))
                  composed
                  test-nested)])
      (assert-equal 8.0 grad)))

  (define-test "composed-gradient helper works"
    (let ([grad (composed-gradient
                 (lambda (s) (traced-sq (view (>>> lens-fst lens-snd) s)))
                 lens-fst
                 lens-snd
                 test-nested)])
      (assert-equal 4.0 grad))))  ; 2 * 2.0 = 4.0

;;; ============================================================
;;; Traversal Gradient Tests
;;; ============================================================

(test-group "Traversal Gradient"
  (define-test "gradient through traversal returns list of gradients"
    ;; f(xs) = sum(x^2 for x in xs)
    ;; df/dx_i = 2*x_i
    (let ([grads (optic-gradient-list
                  (lambda (xs)
                    (traced-sum (map traced-sq xs)))
                  traversal-each
                  test-list)])
      (assert-equal 5 (length grads))
      (assert-equal 2.0 (car grads))      ; 2 * 1.0
      (assert-equal 4.0 (cadr grads))     ; 2 * 2.0
      (assert-equal 6.0 (caddr grads))    ; 2 * 3.0
      (assert-equal 8.0 (cadddr grads))   ; 2 * 4.0
      (assert-equal 10.0 (list-ref grads 4))))  ; 2 * 5.0

  (define-test "gradient through traversal with linear loss"
    ;; f(xs) = sum(xs)
    ;; df/dx_i = 1 for all i
    (let ([grads (optic-gradient-list
                  traced-sum
                  traversal-each
                  test-list)])
      (assert-equal 5 (length grads))
      (for-each (lambda (g) (assert-equal 1 g)) grads)))

  (define-test "traversal gradient counts correctly"
    ;; f(xs) = mean(xs) = sum(xs) / n
    ;; df/dx_i = 1/n
    (let ([grads (optic-gradient-list
                  traced-mean
                  traversal-each
                  test-list)])
      (for-each (lambda (g)
                  ;; 1/5 = 0.2
                  (assert-true (< (abs (- g 0.2)) 0.0001)))
                grads))))

;;; ============================================================
;;; Vec2 Gradient Tests
;;; ============================================================

(test-group "Vec2 Gradient"
  (define-test "gradient with vec2 focus returns vec2 gradient"
    ;; f(pair) = |fst(pair)|^2 = x^2 + y^2
    ;; df/dx = 2x, df/dy = 2y
    (let* ([focus-lens (make-lens car (lambda (a s) (cons a (cdr s))))]
           [grad (optic-gradient
                  (lambda (p)
                    (traced-vec2-magnitude-sq (car p)))
                  focus-lens
                  test-vec2-pair)])
      (assert-true (vec2? grad))
      (assert-equal 2.0 (vec2-x grad))   ; 2 * 1.0
      (assert-equal 4.0 (vec2-y grad)))) ; 2 * 2.0

  (define-test "gradient points toward target"
    ;; f(pos) = |pos - target|^2
    ;; At pos = (3, 4), target = (0, 0)
    ;; df/dpos = 2*(pos - target) = 2*pos = (6, 8)
    (let* ([target (vec2 0 0)]
           [structure (vec2 3.0 4.0)]
           [id-lens (make-lens identity (lambda (a s) a))]
           [grad (optic-gradient
                  (lambda (pos)
                    (let* ([diff (traced-vec2-sub pos (lift-vec2-const target))])
                      (traced-vec2-magnitude-sq diff)))
                  id-lens
                  structure)])
      (assert-true (vec2? grad))
      (assert-equal 6.0 (vec2-x grad))
      (assert-equal 8.0 (vec2-y grad)))))

;;; ============================================================
;;; Partial Optic (Prism/Affine) Gradient Tests
;;; ============================================================

(test-group "Partial Optic Gradient"
  (define-test "prism gradient returns nothing when not matching"
    (let ([result (optic-gradient-maybe
                   (lambda (m) (if (nothing? m) 0 (traced-sq (from-just m))))
                   prism-just
                   nothing)])
      (assert-true (nothing? result))))

  (define-test "prism gradient returns just(gradient) when matching"
    (let ([result (optic-gradient-maybe
                   (lambda (m)
                     (if (just? m)
                         (traced-sq (from-just m))
                         0))
                   prism-just
                   (just 5.0))])
      (assert-true (just? result))
      (assert-equal 10.0 (from-just result))))  ; 2 * 5.0

  (define-test "affine gradient returns nothing for out of bounds"
    (let ([result (optic-gradient-maybe
                   (lambda (xs) (traced-sq (car xs)))
                   (affine-nth 10)
                   test-list)])
      (assert-true (nothing? result))))

  (define-test "affine gradient returns gradient when in bounds"
    (let ([result (optic-gradient-maybe
                   (lambda (xs)
                     (let ([focus (preview (affine-nth 2) xs)])
                       (if (just? focus)
                           (traced-sq (from-just focus))
                           0)))
                   (affine-nth 2)
                   test-list)])
      (assert-true (just? result))
      (assert-equal 6.0 (from-just result)))))  ; 2 * 3.0

;;; ============================================================
;;; Optimization Tests
;;; ============================================================

(test-group "Optimization"
  (define-test "optimize-at performs gradient descent step"
    ;; f(x) = x^2, start at x=5
    ;; gradient = 2*5 = 10
    ;; new x = 5 - 0.1 * 10 = 4
    (let* ([structure '(5.0 . ignored)]
           [result (optimize-at lens-fst structure
                                (lambda (p) (traced-sq (car p)))
                                0.1)])
      (assert-equal 4.0 (car result))))

  (define-test "optimize-steps converges toward minimum"
    ;; f(x) = x^2, minimum at x=0
    ;; Start at x=10, rate=0.1, 50 steps
    (let* ([structure '(10.0 . ignored)]
           [result (optimize-steps lens-fst structure
                                   (lambda (p) (traced-sq (car p)))
                                   0.1
                                   50)])
      ;; Should be close to 0
      (assert-true (< (abs (car result)) 0.01))))

  (define-test "optimize-at with vec2"
    ;; f(v) = |v - target|^2
    ;; Gradient descent moves toward target
    (let* ([target (vec2 0 0)]
           [structure (cons (vec2 2.0 2.0) 'rest)]
           [focus-lens (make-lens car (lambda (a s) (cons a (cdr s))))]
           [result (optimize-at focus-lens structure
                                (lambda (p)
                                  (let* ([pos (car p)]
                                         [diff (traced-vec2-sub pos (lift-vec2-const target))])
                                    (traced-vec2-magnitude-sq diff)))
                                0.1)])
      ;; Should move toward (0, 0)
      (let ([new-pos (car result)])
        ;; x = 2 - 0.1 * 4 = 1.6, y = 2 - 0.1 * 4 = 1.6
        (assert-true (< (vec2-x new-pos) 2.0))
        (assert-true (< (vec2-y new-pos) 2.0)))))

  (define-test "optimize-at with traversal updates each target independently"
    ;; f(xs) = sum(x^2 for x in xs)
    ;; Each x_i has gradient 2*x_i, so update: x_i' = x_i - rate * 2 * x_i
    ;; Start: (1.0 2.0 3.0), rate=0.1
    ;; Expected: (1 - 0.2, 2 - 0.4, 3 - 0.6) = (0.8, 1.6, 2.4)
    (let* ([structure '(1.0 2.0 3.0)]
           [result (optimize-at traversal-each structure
                                (lambda (xs)
                                  (traced-sum (map traced-sq xs)))
                                0.1)])
      ;; Each element should be updated based on its own gradient
      (assert-true (< (abs (- (car result) 0.8)) 0.0001))
      (assert-true (< (abs (- (cadr result) 1.6)) 0.0001))
      (assert-true (< (abs (- (caddr result) 2.4)) 0.0001)))))

;;; ============================================================
;;; Value-and-Gradient Tests
;;; ============================================================

(test-group "Value and Gradient"
  (define-test "optic-value-and-gradient returns both"
    (let-values ([(val grad)
                  (optic-value-and-gradient
                   (lambda (p) (traced-sq (car p)))
                   lens-fst
                   '(3.0 . 4.0))])
      (assert-equal 9.0 val)   ; 3^2 = 9
      (assert-equal 6.0 grad))) ; 2*3 = 6

  (define-test "value-and-gradient with complex expression"
    (let-values ([(val grad)
                  (optic-value-and-gradient
                   (lambda (p)
                     (traced-add (traced-sq (car p))
                                 (traced-mul (cdr p) 2)))
                   lens-fst
                   '(3.0 . 4.0))])
      ;; val = 3^2 + 2*4 = 9 + 8 = 17
      ;; grad (w.r.t. fst only) = 2*3 = 6
      (assert-equal 17.0 val)  ; Use float for consistency
      (assert-equal 6.0 grad))))

;;; ============================================================
;;; Error Handling Tests
;;; ============================================================

(test-group "Error Handling"
  (define-test "lift-at-optic errors on traced container"
    (let ([caught #f])
      (with-fresh-ad-scope
       (lambda ()
         (let* ([tape (make-reverse-tape)]
                [traced-struct (traced 5.0 0 tape)])
           (guard (e [else (set! caught #t)])
                  (lift-at-optic lens-fst traced-struct tape)))))
      (assert-true caught)))

  (define-test "getter optic produces error"
    (let ([caught #f]
          [getter (make-getter car)])
      (guard (e [else (set! caught #t)])
             (optic-gradient
              (lambda (x) (traced-sq x))
              getter
              '(3.0 . 4.0)))
      (assert-true caught))))

;;; ============================================================
;;; Helper Function Tests
;;; ============================================================

(test-group "Helper Functions"
  (define-test "traced-sum sums traced values"
    (with-fresh-ad-scope
     (lambda ()
       (let* ([tape (make-reverse-tape)]
              [traced-list (map (lambda (x) (make-traced-var x tape)) '(1 2 3))]
              [result (traced-sum traced-list)])
         (assert-equal 6 (traced-value result))))))

  (define-test "traced-mean computes mean"
    (with-fresh-ad-scope
     (lambda ()
       (let* ([tape (make-reverse-tape)]
              [traced-list (map (lambda (x) (make-traced-var x tape)) '(2 4 6))]
              [result (traced-mean traced-list)])
         (assert-equal 4 (traced-value result))))))

  (define-test "traced-l2-norm-sq computes sum of squares"
    (with-fresh-ad-scope
     (lambda ()
       (let* ([tape (make-reverse-tape)]
              [traced-list (map (lambda (x) (make-traced-var x tape)) '(3 4))]
              [result (traced-l2-norm-sq traced-list)])
         (assert-equal 25 (traced-value result))))))  ; 9 + 16 = 25

  (define-test "subtract-scaled works with numbers"
    (assert-equal 7.0 (subtract-scaled 10.0 3.0 1.0)))

  (define-test "subtract-scaled works with vec2"
    (let ([result (subtract-scaled (vec2 10.0 20.0) (vec2 2.0 4.0) 0.5)])
      (assert-equal 9.0 (vec2-x result))
      (assert-equal 18.0 (vec2-y result)))))

;;; ============================================================
;;; Run Tests
;;; ============================================================

(run-all-tests)
