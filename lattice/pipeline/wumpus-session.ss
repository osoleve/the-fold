;;; lattice/pipeline/wumpus-session.ss — Mutable Session Wrapper
;;;
;;; Wraps the pure game logic with a mutable cell for RLM worker sessions.
;;; Bang-suffixed functions mutate *wumpus-game* and return observation strings.
;;; Loaded via (eval (load "lattice/pipeline/wumpus-session.ss")) in the worker.

(load "lattice/pipeline/wumpus.ss")

(define *wumpus-game* #f)

(define (wumpus-init! seed)
  (set! *wumpus-game* (wumpus-make-episode *lattice-cave-graph*
                                            (default-wumpus-config)
                                            seed))
  (wumpus-format-observation *wumpus-game* "The hunt begins."))

(define (wumpus-move! room)
  (let-values ([(game* msg) (wumpus-step-move *wumpus-game* room)])
    (set! *wumpus-game* game*)
    (wumpus-format-observation game* msg)))

(define (wumpus-shoot! path)
  (let-values ([(game* msg) (wumpus-step-shoot *wumpus-game* path)])
    (set! *wumpus-game* game*)
    (wumpus-format-observation game* msg)))

(define (wumpus-look!)
  (let-values ([(game* msg) (wumpus-step-look *wumpus-game*)])
    (set! *wumpus-game* game*)
    (wumpus-format-observation game* msg)))

(define (wumpus-result)
  (list (wumpus-game-status *wumpus-game*)
        (wumpus-reward *wumpus-game*)))

(define (wumpus-done?)
  (wumpus-terminal? *wumpus-game*))
