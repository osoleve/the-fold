;;; lattice/pipeline/wumpus-session.ss — Mutable Session Wrapper
;;;
;;; Wraps the pure game logic with a mutable cell for RLM worker sessions.
;;; Bang-suffixed functions mutate *wumpus-game* and return observation strings.
;;; Loaded via (eval (load "lattice/pipeline/wumpus-session.ss")) in the worker.

(load "lattice/pipeline/wumpus.ss")

(define *wumpus-game* #f)

(define (wumpus-init! seed)
  (doc 'export #t)
  (set! *wumpus-game* (wumpus-make-episode *lattice-cave-graph*
                                            (default-wumpus-config)
                                            seed))
  (wumpus-format-observation *wumpus-game* "The hunt begins."))

(define (wumpus-move! room)
  (doc 'export #t)
  (let-values ([(game* msg) (wumpus-step-move *wumpus-game* room)])
    (set! *wumpus-game* game*)
    (wumpus-format-observation game* msg)))

(define (wumpus-shoot! path)
  (doc 'export #t)
  (let-values ([(game* msg) (wumpus-step-shoot *wumpus-game* path)])
    (set! *wumpus-game* game*)
    (wumpus-format-observation game* msg)))

(define (wumpus-look!)
  (doc 'export #t)
  (let-values ([(game* msg) (wumpus-step-look *wumpus-game*)])
    (set! *wumpus-game* game*)
    (wumpus-format-observation game* msg)))

(define (wumpus-result)
  (doc 'export #t)
  (if (wumpus-terminal? *wumpus-game*)
      (list (wumpus-game-status *wumpus-game*)
            (wumpus-reward *wumpus-game*))
      (error 'wumpus-result "Game is still active — keep playing!")))

(define (wumpus-done?)
  (doc 'export #t)
  (wumpus-terminal? *wumpus-game*))
