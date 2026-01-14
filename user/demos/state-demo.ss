;;; playpen/demos/state-demo.ss — State Monad Demo
;;;
;;; A simple game simulation using the State monad.
;;; Demonstrates pure state threading for game logic.
;;;
;;; Run from project root:
;;;   scheme --script playpen/demos/state-demo.ss
;;; Or from core/:
;;;   scheme --script ../playpen/demos/state-demo.ss

;; Try to load from project root first, fall back to relative
(guard (exn [else (void)])
       (load "core/base/prelude.ss")
       (load "core/lang/eval.ss")
       (load "core/util/state.ss"))

;; Fall back to relative paths (running from core/)
(guard (exn [else (void)])
       (load "prelude.ss")
       (load "eval.ss")
       (load "state.ss"))

;;; ====
;;; Build Environment
;;; ====

(define (build-demo-env fuel)
  (let ([base-env (build-prelude-env fuel)])
       (let loop ([defs state-prelude-defs] [env base-env] [remaining fuel])
            (if (null? defs)
                env
                (let* ([def (car defs)]
                       [name (car def)]
                       [expr (cdr def)]
                       [result (eval-expr expr env remaining)])
                      (if (eq? (car result) 'ok)
                          (loop (cdr defs)
                                (env-extend env name (cadr result))
                                (caddr result))
                          env))))))

(define demo-env (build-demo-env 2000))

(define (run-demo expr fuel)
  (let ([result (eval-expr expr demo-env fuel)])
       (if (eq? (car result) 'ok)
           (cadr result)
           result)))

;;; ====
;;; Demo 1: Simple Counter
;;; ====

(display "╔══════════════════════════════════════════════════════════════╗\n")
(display "║           STATE MONAD DEMO — Game State Management           ║\n")
(display "╚══════════════════════════════════════════════════════════════╝\n\n")

(display "Demo 1: Counter (tracking game score)\n")
(display "─────────────────────────────────────\n")

;; Increment counter 5 times, collecting each value
(display "Incrementing counter 5 times from 0:\n")
(let ([result (run-demo
               '((run-state ((state-replicate 5) inc-counter)) 0)
               1000)])
     (display "  Values collected: ")
     (write (car result))
     (display "\n  Final state: ")
     (write (cadr result))
     (newline))

;;; ====
;;; Demo 2: Position Movement
;;; ====

(display "\nDemo 2: 2D Position (game entity movement)\n")
(display "───────────────────────────────────────────\n")

;; Move a character: right 3, up 2, right 2, down 1
(display "Moving entity from (0, 0):\n")
(display "  Commands: right 3, up 2, right 2, down 1\n")

(let ([result (run-demo
               '((run-state
                  ((state-bind (move-right 3))
                   (fn (_)
                       ((state-bind (move-up 2))
                        (fn (_)
                            ((state-bind (move-right 2))
                             (fn (_)
                                 ((state-bind (move-up -1))    ; down = negative up
                                  (fn (_)
                                      ((state-bind get-x)
                                       (fn (x)
                                           ((state-bind get-y)
                                            (fn (y)
                                                (state-return (prim 'list x y)))))))))))))))
                 (prim 'list 0 0))
               2000)])
     (display "  Final position: ")
     (write (car result))
     (display "\n  State verification: ")
     (write (cadr result))
     (newline))

;;; ====
;;; Demo 3: Game Tick Simulation
;;; ====

(display "\nDemo 3: Using state-modify for Complex State\n")
(display "─────────────────────────────────────────────\n")

;; Use state-modify to update a counter multiple times
;; This demonstrates composing state modifications
(display "Applying transformations: double, add 10, negate\n")

(let ([result (run-demo
               '((run-state
                  ((state-bind (state-modify (fn (x) (prim 'mul x 2))))  ; double
                   (fn (_)
                       ((state-bind (state-modify (fn (x) (prim 'add x 10))))  ; add 10
                        (fn (_)
                            ((state-bind (state-modify (fn (x) (prim 'neg x))))  ; negate
                             (fn (_)
                                 state-get)))))))  ; return final state
                 5)  ; start with 5
               1000)])
     (display "  Initial: 5\n")
     (display "  After double: 10\n")
     (display "  After add 10: 20\n")
     (display "  After negate: -20\n")
     (display "  Result: ")
     (write (car result))
     (display ", State: ")
     (write (cadr result))
     (newline))

;;; ====
;;; Demo 4: Pure State Laws
;;; ====

(display "\nDemo 4: Monad Laws Verification\n")
(display "────────────────────────────────\n")

(display "Left Identity: (bind (return a) f) ≡ (f a)\n")
(let ([left (run-demo
             '((eval-state
                ((state-bind (state-return 5))
                 (fn (x) (state-return (prim 'mul x 2)))))
               0)
             200)]
      [right (run-demo
              '((eval-state
                 ((fn (x) (state-return (prim 'mul x 2))) 5))
                0)
              100)])
     (display "  Left:  ")
     (write left)
     (display ", Right: ")
     (write right)
     (display " — ")
     (display (if (equal? left right) "✓ Equal" "✗ Not equal"))
     (newline))

(display "Right Identity: (bind m return) ≡ m\n")
(let ([left (run-demo
             '((eval-state
                ((state-bind (state-return 42))
                 state-return))
               0)
             200)]
      [right (run-demo
              '((eval-state (state-return 42)) 0)
              100)])
     (display "  Left:  ")
     (write left)
     (display ", Right: ")
     (write right)
     (display " — ")
     (display (if (equal? left right) "✓ Equal" "✗ Not equal"))
     (newline))

;;; ====
;;; Summary
;;; ====

(display "\n╔══════════════════════════════════════════════════════════════╗\n")
(display "║                        SUMMARY                               ║\n")
(display "╚══════════════════════════════════════════════════════════════╝\n")
(display "The State monad enables:\n")
(display "  • Pure state threading without mutation\n")
(display "  • Composable game logic via bind\n")
(display "  • Deterministic replay (same input → same output)\n")
(display "  • Type-safe state transformations\n\n")
(display "Foundation laid for DUCKIE's game engine!\n")
