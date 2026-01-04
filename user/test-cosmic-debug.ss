;;; test-cosmic-debug.ss — Debug cosmic garden issues

(apply-patch 'turtle)

;;; Test 1: Basic turtle operations
(display "Test 1: Basic turtle operations\n")
(define t (make-turtle))
(define t1 (fd t 50))
(define t2 (rt t1 90))
(define t3 (fd t2 50))
(display "✓ Basic operations work\n\n")

;;; Test 2: Color operations
(display "Test 2: Color operations\n")
(define col (color12-from-int #xf00))
(define t4 (setpc t3 col))
(define t5 (setpw t4 5))
(define t6 (fd t5 50))
(display "✓ Color operations work\n\n")

;;; Test 3: State save/restore
(display "Test 3: State save/restore\n")
(define (turtle-state t)
  (list (turtle-x t)
        (turtle-y t)
        (turtle-heading t)
        (turtle-pen-down? t)))

(define (restore-turtle-state t state)
  (let* ([t (setxy t (car state) (cadr state))]
         [t (setheading t (caddr state))])
        (if (cadddr state)
            (pd t)
            (pu t))))

(define st (turtle-state t6))
(define t7 (restore-turtle-state t st))
(display "✓ State save/restore works\n\n")

;;; Test 4: Simple recursive function
(display "Test 4: Simple recursive function\n")
(define (draw-line t depth length)
  (if (<= depth 0)
      t
      (let* ([t (fd t length)]
             [t (draw-line t (- depth 1) (* length 0.8))])
            t)))

(define t8 (draw-line t 5 20))
(display "✓ Simple recursion works\n\n")

;;; Test 5: Branching recursion (simplified fractal)
(display "Test 5: Branching recursion\n")
(define (simple-fractal t depth length)
  (if (<= depth 0)
      t
      (let* ([t (fd t length)]
             [state (turtle-state t)]
             ;; Left branch
             [t (lt t 30)]
             [t (simple-fractal t (- depth 1) (* length 0.7))]
             ;; Restore and right branch
             [t (restore-turtle-state t state)]
             [t (rt t 30)]
             [t (simple-fractal t (- depth 1) (* length 0.7))])
            t)))

(define t9 (simple-fractal t 3 50))
(display "✓ Branching recursion works\n\n")

;;; Test 6: With random colors
(display "Test 6: With random colors\n")
(define palette '(#xf00 #x0f0 #x00f #xff0 #xf0f))

(define (random-color-fractal t depth length palette)
  (if (<= depth 0)
      t
      (let* ([col (color12-from-int (list-ref palette (random (length palette))))]
             [t (setpc t col)]
             [t (setpw t (max 1 (inexact->exact (floor depth))))]
             [t (fd t length)]
             [state (turtle-state t)]
             ;; Left branch
             [t (lt t 30)]
             [t (random-color-fractal t (- depth 1) (* length 0.7) palette)]
             ;; Restore and right branch
             [t (restore-turtle-state t state)]
             [t (rt t 30)]
             [t (random-color-fractal t (- depth 1) (* length 0.7) palette)])
            t)))

(define t10 (random-color-fractal (make-turtle) 4 60 palette))
(display "✓ Random color fractal works\n\n")

(display "All tests passed! Exporting test drawing...\n")
(save-svg (turtle->drawing t10) "test-fractal.svg")
(display "✓ Saved to test-fractal.svg\n")
