;; Test the State monad
(load "core/base/prelude.ss")
(load "core/util/state.ss")

(display "✓ State monad loaded successfully!")
(newline)
(newline)

;; Simple counter that returns old value and increments
(define counter
  (state-bind (state-get)
              (lambda (n)
                      (state-bind (state-put (+ n 1))
                                  (lambda (_)
                                          (state-return n))))))

(define result1 (run-state counter 42))
(display "Counter test:")
(newline)
(display "  Initial state: 42")
(newline)
(display "  Returned value: ")
(display (car result1))
(newline)
(display "  Final state: ")
(display (cdr result1))
(newline)
(newline)

;; Game position example - move diagonally
(define move-diagonal
  (state-bind (state-get)
              (lambda (pos)
                      (let ((x (car pos))
                            (y (cadr pos)))
                           (state-put (list (+ x 1) (+ y 1)))))))

(define result2 (run-state move-diagonal '(10 20)))
(display "Move diagonal test:")
(newline)
(display "  Start position: (10, 20)")
(newline)
(display "  End position: ")
(display (cdr result2))
(newline)
(newline)

;; Chain multiple moves
(define move-right
  (state-modify (lambda (pos)
                        (list (+ (car pos) 1) (cadr pos)))))

(define move-up
  (state-modify (lambda (pos)
                        (list (car pos) (+ (cadr pos) 1)))))

(define move-twice
  (state-bind move-right
              (lambda (_) move-up)))

(define result3 (run-state move-twice '(0 0)))
(display "Chained moves (right then up):")
(newline)
(display "  Start: (0, 0)")
(newline)
(display "  End: ")
(display (cdr result3))
(newline)
(newline)

(display "🎮 State monad is perfect for game development!")
(newline)
