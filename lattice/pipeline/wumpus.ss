;;; lattice/pipeline/wumpus.ss — Hunt the Wumpus: Pure Game Logic
;;;
;;; The lattice dependency graph IS the cave system. 46 skills are rooms
;;; (template excluded), dependency edges are bidirectional tunnels.
;;; Pure transitions, perception, and reward. No mutation, no I/O.

;; Guard: doc may not be bound in worker environments
(when (not (top-level-bound? 'doc))
  (eval '(define (doc . args) (void))))

(doc 'module 'pipeline/wumpus)
(doc 'description "Hunt the Wumpus over the lattice dependency graph. Pure game logic.")
(doc 'layer 'lattice)
(doc 'purity 'total)

;;; ====
;;; Deterministic PRNG (LCG)
;;; ====
;;; Bare-bones LCG for entity placement and wumpus movement.
;;; State is a single integer. Not the lattice's monadic PCG.

(define *wumpus-lcg-a* 1103515245)
(define *wumpus-lcg-c* 12345)
(define *wumpus-lcg-m* 2147483648) ; 2^31

(define (make-wumpus-rng seed)
  (modulo (abs seed) *wumpus-lcg-m*))

(define (wumpus-rng-next rng)
  (modulo (+ (* rng *wumpus-lcg-a*) *wumpus-lcg-c*) *wumpus-lcg-m*))

;;; Return (values index-in-[0,n) next-rng-state)
(define (wumpus-rng-range rng n)
  (let ([next (wumpus-rng-next rng)])
    (values (modulo (quotient next 65536) n) next)))

;;; Fisher-Yates shuffle, deterministic from RNG state
(define (wumpus-rng-shuffle rng lst)
  (let* ([vec (list->vector lst)]
         [len (vector-length vec)])
    (let loop ([i (- len 1)] [r rng])
      (if (<= i 0)
          (values (vector->list vec) r)
          (let-values ([(j r*) (wumpus-rng-range r (+ i 1))])
            (let ([tmp (vector-ref vec i)])
              (vector-set! vec i (vector-ref vec j))
              (vector-set! vec j tmp)
              (loop (- i 1) r*)))))))

;;; ====
;;; Lattice Dependency Graph (hardcoded from manifests, template excluded)
;;; ====

(define *lattice-deps*
  '((algebra)
    (autodiff linalg numeric symbolic random)
    (automata fp)
    (category fp rewrite)
    (clp fp)
    (control-systems linalg numeric algebra)
    (crypto number-theory)
    (data fp linalg)
    (dataset optics random)
    (diffgeo linalg)
    (dsl fp rewrite)
    (egraph algebra)
    (fp algebra)
    (game-theory linalg optimization)
    (geometry linalg topology optics fp)
    (info fp)
    (info-stat info statistics)
    (interpolate numeric linalg)
    (interval data)
    (linalg)
    (markdown fp)
    (meta data)
    (number-theory)
    (numeric linalg)
    (optics fp)
    (optimization autodiff linalg interval data sat)
    (parallel fp)
    (pde numeric linalg geometry)
    (physics/classical linalg random)
    (physics/classical3d linalg)
    (physics/diff linalg)
    (physics/diff3d linalg)
    (physics/lenses fp optics geometry physics/classical physics/classical3d)
    (physics-problems physics/classical dataset)
    (pipeline data fp)
    (query data optics)
    (random fp)
    (rewrite fp data)
    (sat fp)
    (signal numeric algebra)
    (sim data linalg random numeric fp)
    (statistics linalg fp algebra)
    (symbolic algebra egraph)
    (tiles data topology)
    (topology data linalg)
    (validation)))

;;; ====
;;; Cave Graph Construction
;;; ====

(define (cave-graph-add-neighbor graph room neighbor)
  (map (lambda (entry)
         (if (eq? (car entry) room)
             (if (memq neighbor (cdr entry))
                 entry
                 (cons room (cons neighbor (cdr entry))))
             entry))
       graph))

;;; Build bidirectional adjacency from directed deps.
;;; Each dep edge A->B creates tunnels A<->B.
(define (deps->cave-graph deps)
  (let ([graph (map (lambda (entry) (cons (car entry) '())) deps)])
    (fold-left
      (lambda (g entry)
        (fold-left
          (lambda (g* dep)
            (cave-graph-add-neighbor
              (cave-graph-add-neighbor g* (car entry) dep)
              dep (car entry)))
          g (cdr entry)))
      graph deps)))

(define (cave-graph-neighbors graph room)
  (let ([entry (assq room graph)])
    (if entry (cdr entry) '())))

(define (cave-graph-rooms graph)
  (map car graph))

(define (cave-graph-degree graph room)
  (length (cave-graph-neighbors graph room)))

(define (cave-graph-adjacent? graph a b)
  (and (memq b (cave-graph-neighbors graph a)) #t))

(define (cave-graph-add-edge graph a b)
  (cave-graph-add-neighbor
    (cave-graph-add-neighbor graph a b)
    b a))

;;; ====
;;; Connectivity
;;; ====

(define (cave-graph-bfs graph start)
  (let loop ([queue (list start)] [visited '()])
    (if (null? queue)
        visited
        (let ([room (car queue)] [rest (cdr queue)])
          (if (memq room visited)
              (loop rest visited)
              (loop (append rest
                      (filter (lambda (n) (not (memq n visited)))
                              (cave-graph-neighbors graph room)))
                    (cons room visited)))))))

(define (cave-graph-connected-components graph)
  (let ([rooms (cave-graph-rooms graph)])
    (let loop ([remaining rooms] [components '()])
      (if (null? remaining)
          components
          (let* ([comp (cave-graph-bfs graph (car remaining))]
                 [remaining* (filter (lambda (r) (not (memq r comp)))
                                     remaining)])
            (loop remaining* (cons comp components)))))))

(define (cave-graph-highest-degree graph rooms)
  (let loop ([best (car rooms)]
             [best-deg (cave-graph-degree graph (car rooms))]
             [rest (cdr rooms)])
    (if (null? rest) best
        (let ([deg (cave-graph-degree graph (car rest))])
          (if (> deg best-deg)
              (loop (car rest) deg (cdr rest))
              (loop best best-deg (cdr rest)))))))

;;; Bridge disconnected components to the largest one via hub nodes.
;;; Returns a new graph (pure).
(define (cave-graph-ensure-connected graph)
  (let ([components (cave-graph-connected-components graph)])
    (if (<= (length components) 1)
        graph
        (let* ([main-comp (fold-left
                            (lambda (best c)
                              (if (> (length c) (length best)) c best))
                            (car components) (cdr components))]
               [main-hub (cave-graph-highest-degree graph main-comp)]
               [others (filter (lambda (c) (not (eq? c main-comp)))
                               components)])
          (fold-left
            (lambda (g comp)
              (cave-graph-add-edge g main-hub
                (cave-graph-highest-degree g comp)))
            graph others)))))

;;; The ready-to-use cave graph
(define *lattice-cave-graph*
  (cave-graph-ensure-connected (deps->cave-graph *lattice-deps*)))

;;; ====
;;; Game Configuration
;;; ====

(define (make-wumpus-config arrows pits arrow-range max-moves)
  (list 'wumpus-config arrows pits arrow-range max-moves))

(define (wumpus-config-arrows cfg)      (list-ref cfg 1))
(define (wumpus-config-pits cfg)        (list-ref cfg 2))
(define (wumpus-config-arrow-range cfg) (list-ref cfg 3))
(define (wumpus-config-max-moves cfg)   (list-ref cfg 4))

;; 3 arrows, 3 pits, range 3, 30 moves
(define (default-wumpus-config)
  (make-wumpus-config 3 3 3 30))

(define *wumpus-arrow-range* 3)

;;; ====
;;; Game State
;;; ====

(define (make-wumpus-game cave-graph player-room wumpus-room pit-rooms
                           arrows max-moves moves-used status visited rng)
  (list 'wumpus-game cave-graph player-room wumpus-room pit-rooms
        arrows max-moves moves-used status visited rng))

(define (wumpus-game? x) (and (pair? x) (eq? (car x) 'wumpus-game)))

(define (wumpus-game-cave-graph g)  (list-ref g 1))
(define (wumpus-game-player-room g) (list-ref g 2))
(define (wumpus-game-wumpus-room g) (list-ref g 3))
(define (wumpus-game-pit-rooms g)   (list-ref g 4))
(define (wumpus-game-arrows g)      (list-ref g 5))
(define (wumpus-game-max-moves g)   (list-ref g 6))
(define (wumpus-game-moves-used g)  (list-ref g 7))
(define (wumpus-game-status g)      (list-ref g 8))
(define (wumpus-game-visited g)     (list-ref g 9))
(define (wumpus-game-rng g)         (list-ref g 10))

(define (wumpus-terminal? game)
  (not (eq? (wumpus-game-status game) 'active)))

;;; Functional update: specify only changed fields as key-value pairs.
(define (wumpus-game-update g . kvs)
  (let loop ([player  (wumpus-game-player-room g)]
             [wumpus  (wumpus-game-wumpus-room g)]
             [pits    (wumpus-game-pit-rooms g)]
             [arrows  (wumpus-game-arrows g)]
             [moves   (wumpus-game-moves-used g)]
             [status  (wumpus-game-status g)]
             [visited (wumpus-game-visited g)]
             [rng     (wumpus-game-rng g)]
             [rest    kvs])
    (if (null? rest)
        (make-wumpus-game (wumpus-game-cave-graph g)
          player wumpus pits arrows (wumpus-game-max-moves g)
          moves status visited rng)
        (let ([k (car rest)] [v (cadr rest)])
          (case k
            [(player-room) (loop v wumpus pits arrows moves status visited rng (cddr rest))]
            [(wumpus-room) (loop player v pits arrows moves status visited rng (cddr rest))]
            [(pit-rooms)   (loop player wumpus v arrows moves status visited rng (cddr rest))]
            [(arrows)      (loop player wumpus pits v moves status visited rng (cddr rest))]
            [(moves-used)  (loop player wumpus pits arrows v status visited rng (cddr rest))]
            [(status)      (loop player wumpus pits arrows moves v visited rng (cddr rest))]
            [(visited)     (loop player wumpus pits arrows moves status v rng (cddr rest))]
            [(rng)         (loop player wumpus pits arrows moves status visited v (cddr rest))]
            [else (error 'wumpus-game-update (format "unknown field: ~a" k))])))))

;;; ====
;;; Episode Creation (deterministic from seed)
;;; ====

(define (wumpus-take lst n)
  (if (or (null? lst) (= n 0)) '()
      (cons (car lst) (wumpus-take (cdr lst) (- n 1)))))

(define (wumpus-make-episode graph config seed)
  (let* ([rng (make-wumpus-rng seed)]
         [rooms (cave-graph-rooms graph)]
         [n-pits (wumpus-config-pits config)]
         [arrows (wumpus-config-arrows config)]
         [max-moves (wumpus-config-max-moves config)])
    (let-values ([(shuffled rng*) (wumpus-rng-shuffle rng rooms)])
      (let* ([player (or (find (lambda (r) (>= (cave-graph-degree graph r) 2))
                               shuffled)
                         (car shuffled))]
             [rest (filter (lambda (r) (not (eq? r player))) shuffled)]
             [wumpus (car rest)]
             [rest* (cdr rest)]
             [pits (wumpus-take rest* n-pits)])
        (make-wumpus-game graph player wumpus pits
                           arrows max-moves 0 'active
                           (list player) rng*)))))

;;; ====
;;; Perception
;;; ====

(define (wumpus-perceive game)
  (let* ([graph (wumpus-game-cave-graph game)]
         [room (wumpus-game-player-room game)]
         [neighbors (cave-graph-neighbors graph room)]
         [wumpus (wumpus-game-wumpus-room game)]
         [pits (wumpus-game-pit-rooms game)]
         [senses '()])
    (let* ([senses (if (memq wumpus neighbors)
                       (cons 'stench senses) senses)]
           [senses (if (exists (lambda (p) (memq p neighbors)) pits)
                       (cons 'draft senses) senses)])
      senses)))

;;; ====
;;; Helpers
;;; ====

;;; Check and apply timeout if moves exhausted (only when still active)
(define (wumpus-check-timeout game)
  (if (and (eq? (wumpus-game-status game) 'active)
           (>= (wumpus-game-moves-used game) (wumpus-game-max-moves game)))
      (wumpus-game-update game 'status 'timeout)
      game))

;;; Trace arrow through a path of rooms. Returns #t if wumpus hit.
(define (wumpus-follow-arrow graph from-room wumpus-room path)
  (let loop ([current from-room] [remaining path])
    (if (null? remaining)
        #f
        (let ([next (car remaining)])
          (cond
            [(not (cave-graph-adjacent? graph current next)) #f]
            [(eq? next wumpus-room) #t]
            [else (loop next (cdr remaining))])))))

;;; 75% chance wumpus moves to random adjacent room after missed shot.
;;; If wumpus enters player's room, caller handles eaten status.
(define (wumpus-maybe-move game)
  (let-values ([(roll rng*) (wumpus-rng-range (wumpus-game-rng game) 100)])
    (if (< roll 75)
        (let ([neighbors (cave-graph-neighbors
                           (wumpus-game-cave-graph game)
                           (wumpus-game-wumpus-room game))])
          (if (null? neighbors)
              (wumpus-game-update game 'rng rng*)
              (let-values ([(idx rng**) (wumpus-rng-range rng* (length neighbors))])
                (wumpus-game-update game
                  'wumpus-room (list-ref neighbors idx)
                  'rng rng**))))
        (wumpus-game-update game 'rng rng*))))

;;; ====
;;; Pure Transitions
;;; ====
;;; All actions cost exactly 1 move.
;;; Each returns (values updated-game message-string).

(define (wumpus-step-move game room)
  (cond
    [(wumpus-terminal? game)
     (values game "The game is over.")]

    ;; Invalid: not adjacent
    [(not (cave-graph-adjacent? (wumpus-game-cave-graph game)
                                 (wumpus-game-player-room game) room))
     (let ([g (wumpus-check-timeout
                (wumpus-game-update game
                  'moves-used (+ 1 (wumpus-game-moves-used game))))])
       (values g (format "There is no tunnel to ~a from here." room)))]

    ;; Valid move — resolve hazards
    [else
     (let* ([visited (if (memq room (wumpus-game-visited game))
                         (wumpus-game-visited game)
                         (cons room (wumpus-game-visited game)))]
            [g (wumpus-game-update game
                 'player-room room
                 'moves-used (+ 1 (wumpus-game-moves-used game))
                 'visited visited)])
       (cond
         [(eq? room (wumpus-game-wumpus-room game))
          (values (wumpus-game-update g 'status 'eaten)
                  "A giant wumpus devours you!")]
         [(memq room (wumpus-game-pit-rooms game))
          (values (wumpus-game-update g 'status 'fell)
                  "You fall into a bottomless pit!")]
         [else
          (values (wumpus-check-timeout g)
                  (format "You enter the ~a cavern." room))]))]))

(define (wumpus-step-shoot game path)
  (cond
    [(wumpus-terminal? game)
     (values game "The game is over.")]

    ;; No arrows
    [(= (wumpus-game-arrows game) 0)
     (let ([g (wumpus-check-timeout
                (wumpus-game-update game
                  'moves-used (+ 1 (wumpus-game-moves-used game))))])
       (values g "Your quiver is empty."))]

    ;; Fire arrow
    [else
     (let* ([path* (wumpus-take path *wumpus-arrow-range*)]
            [hit? (wumpus-follow-arrow
                    (wumpus-game-cave-graph game)
                    (wumpus-game-player-room game)
                    (wumpus-game-wumpus-room game)
                    path*)]
            [g (wumpus-game-update game
                 'arrows (- (wumpus-game-arrows game) 1)
                 'moves-used (+ 1 (wumpus-game-moves-used game)))])
       (if hit?
           (values (wumpus-game-update g 'status 'won)
                   "Your arrow strikes true! The wumpus is slain!")
           ;; Miss: wumpus may move, check if it finds the player
           (let* ([g (wumpus-maybe-move g)]
                  [eaten? (eq? (wumpus-game-player-room g)
                               (wumpus-game-wumpus-room g))]
                  [g (if eaten? (wumpus-game-update g 'status 'eaten) g)]
                  [g (wumpus-check-timeout g)])
             (values g (if eaten?
                           "Your arrow misses... and the wumpus finds you!"
                           "Your arrow vanishes into the darkness.")))))]))

(define (wumpus-step-look game)
  (cond
    [(wumpus-terminal? game)
     (values game "The game is over.")]
    [else
     (let ([g (wumpus-check-timeout
                (wumpus-game-update game
                  'moves-used (+ 1 (wumpus-game-moves-used game))))])
       (values g "You survey your surroundings."))]))

;;; ====
;;; Reward
;;; ====

(define (wumpus-reward game)
  (case (wumpus-game-status game)
    [(won)     1.0]
    [(eaten)  -1.0]
    [(fell)   -1.0]
    [(timeout) -0.3]
    [else      0.0]))

;;; ====
;;; Observation Formatting
;;; ====
;;; Returns a string resembling an S-expression for HUD display.

(define (wumpus-format-observation game message)
  (let* ([room (wumpus-game-player-room game)]
         [graph (wumpus-game-cave-graph game)]
         [neighbors (cave-graph-neighbors graph room)]
         [senses (wumpus-perceive game)]
         [arrows (wumpus-game-arrows game)]
         [used (wumpus-game-moves-used game)]
         [maxm (wumpus-game-max-moves game)]
         [n-visited (length (wumpus-game-visited game))]
         [status (wumpus-game-status game)])
    (format "(wumpus\n  (room ~a)\n  (tunnels ~a)\n  (senses ~a)\n  (arrows ~a)\n  (moves ~a/~a)\n  (visited ~a)\n  (status ~a)\n  (message ~s))"
            room neighbors senses arrows used maxm n-visited status message)))
