;;; lattice/pipeline/mastermind-session.ss — Mutable Session Wrapper
;;;
;;; Wraps the pure game logic with a mutable cell for RLM worker sessions.
;;; Loaded via (eval (load "lattice/pipeline/mastermind-session.ss")) in the worker.

(load "lattice/pipeline/mastermind.ss")

(define *mastermind-game* #f)

(define (mastermind-init! seed)
  (doc 'export #t)
  (set! *mastermind-game* (mastermind-make-episode (default-mastermind-config) seed))
  (mastermind-format-observation *mastermind-game* "Break the code! Guess 4 colors from the available set."))

(define (mastermind-guess! guess)
  (doc 'export #t)
  (let-values ([(game* msg) (mastermind-step-guess *mastermind-game* guess)])
    (set! *mastermind-game* game*)
    msg))

(define (mastermind-result)
  (doc 'export #t)
  (if (mastermind-terminal? *mastermind-game*)
      (list (mastermind-game-status *mastermind-game*)
            (mastermind-reward *mastermind-game*))
      (error 'mastermind-result "Game is still active — keep guessing!")))

(define (mastermind-done?)
  (doc 'export #t)
  (mastermind-terminal? *mastermind-game*))
