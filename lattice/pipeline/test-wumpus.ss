;;; lattice/pipeline/test-wumpus.ss — Tests for Hunt the Wumpus
;;;
;;; Run: scheme --script lattice/pipeline/test-wumpus.ss

(when (not (top-level-bound? 'meta-printf))
  (eval '(define meta-printf printf)))
(load "core/testing/test-framework.ss")
(load "lattice/pipeline/wumpus.ss")

;;; ====
;;; Helpers
;;; ====

(define (remove-duplicates lst)
  (let loop ([rest lst] [acc '()])
    (if (null? rest) (reverse acc)
        (if (memq (car rest) acc)
            (loop (cdr rest) acc)
            (loop (cdr rest) (cons (car rest) acc))))))

(define (string-contains-wumpus haystack needle)
  (let ([hlen (string-length haystack)]
        [nlen (string-length needle)])
    (and (<= nlen hlen)
         (let loop ([i 0])
           (cond
             [(> (+ i nlen) hlen) #f]
             [(string=? (substring haystack i (+ i nlen)) needle) #t]
             [else (loop (+ i 1))])))))

;;; ====
;;; Graph Tests
;;; ====

(test-group "cave-graph"

  (define-test "46 rooms (template excluded)"
    (assert-equal 46 (length (cave-graph-rooms *lattice-cave-graph*))))

  (define-test "graph is fully connected (1 component)"
    (assert-equal 1 (length (cave-graph-connected-components *lattice-cave-graph*))))

  (define-test "all rooms reachable from any room"
    (let ([rooms (cave-graph-rooms *lattice-cave-graph*)])
      (assert-equal 46 (length (cave-graph-bfs *lattice-cave-graph* (car rooms))))))

  (define-test "template not in graph"
    (assert-false (assq 'template *lattice-cave-graph*)))

  (define-test "fp is the highest-degree hub"
    (let ([hub (cave-graph-highest-degree *lattice-cave-graph*
                (cave-graph-rooms *lattice-cave-graph*))])
      (assert-equal 'fp hub)))

  (define-test "bidirectional edges: if A->B then B->A"
    (let ([ok? #t])
      (for-each
        (lambda (entry)
          (let ([room (car entry)] [neighbors (cdr entry)])
            (for-each
              (lambda (n)
                (when (not (cave-graph-adjacent? *lattice-cave-graph* n room))
                  (set! ok? #f)))
              neighbors)))
        *lattice-cave-graph*)
      (assert-true ok?)))

  (define-test "known adjacency: fp <-> algebra (dep edge)"
    (assert-true (cave-graph-adjacent? *lattice-cave-graph* 'fp 'algebra))
    (assert-true (cave-graph-adjacent? *lattice-cave-graph* 'algebra 'fp)))

  (define-test "known adjacency: crypto <-> number-theory"
    (assert-true (cave-graph-adjacent? *lattice-cave-graph* 'crypto 'number-theory))
    (assert-true (cave-graph-adjacent? *lattice-cave-graph* 'number-theory 'crypto)))

  (define-test "validation bridged to fp"
    (assert-true (cave-graph-adjacent? *lattice-cave-graph* 'validation 'fp)))

  (define-test "number-theory bridged to fp"
    (assert-true (cave-graph-adjacent? *lattice-cave-graph* 'number-theory 'fp)))
)

;;; ====
;;; Subgraph Extraction Tests
;;; ====

(test-group "subgraph-extraction"

  (define-test "determinism: same seed -> same subgraph"
    (let-values ([(sg1 r1) (extract-playable-subgraph *lattice-cave-graph* (make-wumpus-rng 42))]
                 [(sg2 r2) (extract-playable-subgraph *lattice-cave-graph* (make-wumpus-rng 42))])
      (assert-true (equal? (cave-graph-rooms sg1) (cave-graph-rooms sg2)))))

  (define-test "subgraph size in [15, 25]"
    (let-values ([(sg _) (extract-playable-subgraph *lattice-cave-graph* (make-wumpus-rng 42))])
      (let ([n (length (cave-graph-rooms sg))])
        (assert-true (>= n 15))
        (assert-true (<= n 25)))))

  (define-test "all degrees in [2, 7]"
    (let-values ([(sg _) (extract-playable-subgraph *lattice-cave-graph* (make-wumpus-rng 42))])
      (let ([rooms (cave-graph-rooms sg)])
        (assert-false (exists (lambda (r) (> (cave-graph-degree sg r) 7)) rooms))
        (assert-false (exists (lambda (r) (< (cave-graph-degree sg r) 2)) rooms)))))

  (define-test "subgraph is connected"
    (let-values ([(sg _) (extract-playable-subgraph *lattice-cave-graph* (make-wumpus-rng 42))])
      (assert-equal 1 (length (cave-graph-connected-components sg)))))

  (define-test "all rooms exist in full graph"
    (let-values ([(sg _) (extract-playable-subgraph *lattice-cave-graph* (make-wumpus-rng 42))])
      (let ([full-rooms (cave-graph-rooms *lattice-cave-graph*)])
        (assert-false (exists (lambda (r) (not (memq r full-rooms)))
                              (cave-graph-rooms sg))))))

  (define-test "all edges exist in full graph (real topology)"
    (let-values ([(sg _) (extract-playable-subgraph *lattice-cave-graph* (make-wumpus-rng 42))])
      (let ([ok? #t])
        (for-each
          (lambda (entry)
            (for-each
              (lambda (n)
                (when (not (cave-graph-adjacent? *lattice-cave-graph* (car entry) n))
                  (set! ok? #f)))
              (cdr entry)))
          sg)
        (assert-true ok?))))

  (define-test "different seeds produce different subgraphs"
    (let-values ([(sg1 r1) (extract-playable-subgraph *lattice-cave-graph* (make-wumpus-rng 42))]
                 [(sg2 r2) (extract-playable-subgraph *lattice-cave-graph* (make-wumpus-rng 9999))])
      ;; Rooms should differ (overwhelmingly likely)
      (assert-false (equal? (cave-graph-rooms sg1) (cave-graph-rooms sg2)))))

  (define-test "extraction succeeds for 20 random seeds"
    (let loop ([i 0] [good 0])
      (if (= i 20)
          (assert-equal 20 good)
          (let-values ([(sg _) (extract-playable-subgraph *lattice-cave-graph*
                                 (make-wumpus-rng (* i 7919)))])
            (loop (+ i 1) (if sg (+ good 1) good))))))
)

;;; ====
;;; Episode Creation Tests
;;; ====

(test-group "episode-creation"

  (define-test "determinism: same seed produces identical games"
    (let ([g1 (wumpus-make-episode *lattice-cave-graph* (default-wumpus-config) 42)]
          [g2 (wumpus-make-episode *lattice-cave-graph* (default-wumpus-config) 42)])
      (assert-true (eq? (wumpus-game-player-room g1) (wumpus-game-player-room g2)))
      (assert-true (eq? (wumpus-game-wumpus-room g1) (wumpus-game-wumpus-room g2)))
      (assert-true (equal? (wumpus-game-pit-rooms g1) (wumpus-game-pit-rooms g2)))))

  (define-test "different seeds produce different games"
    (let ([g1 (wumpus-make-episode *lattice-cave-graph* (default-wumpus-config) 42)]
          [g2 (wumpus-make-episode *lattice-cave-graph* (default-wumpus-config) 99)])
      ;; At least one field should differ (overwhelmingly likely)
      (assert-false (and (eq? (wumpus-game-player-room g1) (wumpus-game-player-room g2))
                         (eq? (wumpus-game-wumpus-room g1) (wumpus-game-wumpus-room g2))
                         (equal? (wumpus-game-pit-rooms g1) (wumpus-game-pit-rooms g2))))))

  (define-test "initial status is active"
    (let ([g (wumpus-make-episode *lattice-cave-graph* (default-wumpus-config) 42)])
      (assert-equal 'active (wumpus-game-status g))))

  (define-test "starts with 3 arrows"
    (let ([g (wumpus-make-episode *lattice-cave-graph* (default-wumpus-config) 42)])
      (assert-equal 3 (wumpus-game-arrows g))))

  (define-test "starts with 0 moves used"
    (let ([g (wumpus-make-episode *lattice-cave-graph* (default-wumpus-config) 42)])
      (assert-equal 0 (wumpus-game-moves-used g))))

  (define-test "player starts in visited list"
    (let ([g (wumpus-make-episode *lattice-cave-graph* (default-wumpus-config) 42)])
      (assert-true (pair? (memq (wumpus-game-player-room g) (wumpus-game-visited g))))))

  (define-test "3 pits placed"
    (let ([g (wumpus-make-episode *lattice-cave-graph* (default-wumpus-config) 42)])
      (assert-equal 3 (length (wumpus-game-pit-rooms g)))))

  (define-test "player, wumpus, pits are all distinct rooms"
    (let* ([g (wumpus-make-episode *lattice-cave-graph* (default-wumpus-config) 42)]
           [p (wumpus-game-player-room g)]
           [w (wumpus-game-wumpus-room g)]
           [pits (wumpus-game-pit-rooms g)]
           [all (cons p (cons w pits))])
      ;; All elements should be distinct
      (assert-equal (length all) (length (remove-duplicates all)))))

  (define-test "game uses subgraph, not full graph"
    (let ([g (wumpus-make-episode *lattice-cave-graph* (default-wumpus-config) 42)])
      (let ([game-rooms (cave-graph-rooms (wumpus-game-cave-graph g))])
        (assert-true (< (length game-rooms) 46))
        (assert-true (>= (length game-rooms) 15)))))

  (define-test "player degree >= 2 in game subgraph"
    (let ([g (wumpus-make-episode *lattice-cave-graph* (default-wumpus-config) 42)])
      (assert-true (>= (cave-graph-degree (wumpus-game-cave-graph g)
                          (wumpus-game-player-room g)) 2))))
)

;;; ====
;;; Perception Tests
;;; ====

(test-group "perception"

  (define-test "stench when wumpus is adjacent"
    (let ([game (make-wumpus-game *lattice-cave-graph*
                  'fp 'optics '() 3 30 0 'active '(fp) 0)])
      ;; optics is adjacent to fp
      (assert-true (pair? (memq 'stench (wumpus-perceive game))))))

  (define-test "no stench when wumpus is not adjacent"
    (let ([game (make-wumpus-game *lattice-cave-graph*
                  'fp 'crypto '() 3 30 0 'active '(fp) 0)])
      ;; crypto is NOT adjacent to fp
      (assert-false (memq 'stench (wumpus-perceive game)))))

  (define-test "draft when pit is adjacent"
    (let ([game (make-wumpus-game *lattice-cave-graph*
                  'fp 'meta '(optics) 3 30 0 'active '(fp) 0)])
      ;; optics is adjacent to fp
      (assert-true (pair? (memq 'draft (wumpus-perceive game))))))

  (define-test "no draft when no pit is adjacent"
    (let ([game (make-wumpus-game *lattice-cave-graph*
                  'fp 'meta '(crypto) 3 30 0 'active '(fp) 0)])
      ;; crypto is NOT adjacent to fp
      (assert-false (memq 'draft (wumpus-perceive game)))))

  (define-test "both stench and draft when both adjacent"
    (let ([game (make-wumpus-game *lattice-cave-graph*
                  'fp 'algebra '(optics) 3 30 0 'active '(fp) 0)])
      ;; algebra and optics both adjacent to fp
      (let ([senses (wumpus-perceive game)])
        (assert-true (pair? (memq 'stench senses)))
        (assert-true (pair? (memq 'draft senses))))))

  (define-test "empty senses when nothing adjacent"
    (let ([game (make-wumpus-game *lattice-cave-graph*
                  'fp 'crypto '(crypto) 3 30 0 'active '(fp) 0)])
      ;; crypto not adjacent to fp
      (assert-true (null? (wumpus-perceive game)))))
)

;;; ====
;;; Move Tests
;;; ====

(test-group "move"

  (define-test "move to valid safe room succeeds"
    (let ([game (make-wumpus-game *lattice-cave-graph*
                  'fp 'crypto '() 3 30 0 'active '(fp) 0)])
      (let-values ([(g msg) (wumpus-step-move game 'algebra)])
        (assert-equal 'algebra (wumpus-game-player-room g))
        (assert-equal 'active (wumpus-game-status g))
        (assert-equal 1 (wumpus-game-moves-used g)))))

  (define-test "move to wumpus room -> eaten"
    (let ([game (make-wumpus-game *lattice-cave-graph*
                  'fp 'algebra '() 3 30 0 'active '(fp) 0)])
      (let-values ([(g msg) (wumpus-step-move game 'algebra)])
        (assert-equal 'eaten (wumpus-game-status g))
        (assert-true (wumpus-terminal? g)))))

  (define-test "move to pit room -> fell"
    (let ([game (make-wumpus-game *lattice-cave-graph*
                  'fp 'crypto '(algebra) 3 30 0 'active '(fp) 0)])
      (let-values ([(g msg) (wumpus-step-move game 'algebra)])
        (assert-equal 'fell (wumpus-game-status g))
        (assert-true (wumpus-terminal? g)))))

  (define-test "invalid move: not adjacent, costs 1 move"
    (let ([game (make-wumpus-game *lattice-cave-graph*
                  'fp 'crypto '() 3 30 0 'active '(fp) 0)])
      (let-values ([(g msg) (wumpus-step-move game 'crypto)])
        ;; crypto is not adjacent to fp
        (assert-equal 'fp (wumpus-game-player-room g))
        (assert-equal 'active (wumpus-game-status g))
        (assert-equal 1 (wumpus-game-moves-used g)))))

  (define-test "move adds room to visited"
    (let ([game (make-wumpus-game *lattice-cave-graph*
                  'fp 'crypto '() 3 30 0 'active '(fp) 0)])
      (let-values ([(g msg) (wumpus-step-move game 'algebra)])
        (assert-true (pair? (memq 'algebra (wumpus-game-visited g)))))))

  (define-test "move to already-visited room doesn't duplicate"
    (let ([game (make-wumpus-game *lattice-cave-graph*
                  'fp 'crypto '() 3 30 0 'active '(fp algebra) 0)])
      (let-values ([(g msg) (wumpus-step-move game 'algebra)])
        ;; algebra was already visited, count should stay the same
        (assert-equal 2 (length (wumpus-game-visited g))))))

  (define-test "move on terminal game is no-op"
    (let ([game (make-wumpus-game *lattice-cave-graph*
                  'fp 'crypto '() 3 30 0 'eaten '(fp) 0)])
      (let-values ([(g msg) (wumpus-step-move game 'algebra)])
        (assert-equal 0 (wumpus-game-moves-used g)))))
)

;;; ====
;;; Shoot Tests
;;; ====

(test-group "shoot"

  (define-test "shoot hits wumpus -> won"
    (let ([game (make-wumpus-game *lattice-cave-graph*
                  'fp 'optics '() 3 30 0 'active '(fp) 0)])
      (let-values ([(g msg) (wumpus-step-shoot game '(optics))])
        (assert-equal 'won (wumpus-game-status g))
        (assert-equal 2 (wumpus-game-arrows g))
        (assert-equal 1 (wumpus-game-moves-used g)))))

  (define-test "shoot misses -> arrows decrement"
    (let ([game (make-wumpus-game *lattice-cave-graph*
                  'fp 'crypto '() 3 30 0 'active '(fp) 0)])
      (let-values ([(g msg) (wumpus-step-shoot game '(algebra))])
        (assert-equal 'active (wumpus-game-status g))
        (assert-equal 2 (wumpus-game-arrows g))
        (assert-equal 1 (wumpus-game-moves-used g)))))

  (define-test "shoot with 0 arrows -> no arrow lost"
    (let ([game (make-wumpus-game *lattice-cave-graph*
                  'fp 'crypto '() 0 30 0 'active '(fp) 0)])
      (let-values ([(g msg) (wumpus-step-shoot game '(algebra))])
        (assert-equal 0 (wumpus-game-arrows g))
        (assert-equal 1 (wumpus-game-moves-used g)))))

  (define-test "shoot bare symbol -> error message, not crash"
    (let ([game (make-wumpus-game *lattice-cave-graph*
                  'fp 'crypto '() 3 30 0 'active '(fp) 0)])
      (let-values ([(g msg) (wumpus-step-shoot game 'algebra)])
        (assert-true (string-contains-wumpus msg "expected a list"))
        (assert-equal 3 (wumpus-game-arrows g))
        (assert-equal 1 (wumpus-game-moves-used g)))))

  (define-test "shoot invalid path (non-adjacent) -> miss"
    (let ([game (make-wumpus-game *lattice-cave-graph*
                  'fp 'crypto '() 3 30 0 'active '(fp) 0)])
      ;; crypto is not adjacent to fp, arrow bounces
      (let-values ([(g msg) (wumpus-step-shoot game '(crypto))])
        (assert-equal 2 (wumpus-game-arrows g)))))

  (define-test "shoot multi-room path hits wumpus"
    ;; fp -> algebra -> egraph, wumpus at egraph
    (let ([game (make-wumpus-game *lattice-cave-graph*
                  'fp 'egraph '() 3 30 0 'active '(fp) 0)])
      (let-values ([(g msg) (wumpus-step-shoot game '(algebra egraph))])
        (assert-equal 'won (wumpus-game-status g)))))

  (define-test "shoot on terminal game is no-op"
    (let ([game (make-wumpus-game *lattice-cave-graph*
                  'fp 'crypto '() 3 30 0 'won '(fp) 0)])
      (let-values ([(g msg) (wumpus-step-shoot game '(algebra))])
        (assert-equal 3 (wumpus-game-arrows g))
        (assert-equal 0 (wumpus-game-moves-used g)))))

  (define-test "missed shot: wumpus may move (deterministic from rng)"
    (let ([game (make-wumpus-game *lattice-cave-graph*
                  'fp 'crypto '() 3 30 0 'active '(fp) 100)])
      (let-values ([(g1 msg1) (wumpus-step-shoot game '(algebra))])
        (let-values ([(g2 msg2) (wumpus-step-shoot game '(algebra))])
          ;; Same starting state -> same result (determinism)
          (assert-true (eq? (wumpus-game-wumpus-room g1)
                            (wumpus-game-wumpus-room g2)))))))
)

;;; ====
;;; Look Tests
;;; ====

(test-group "look"

  (define-test "look costs 1 move"
    (let ([game (make-wumpus-game *lattice-cave-graph*
                  'fp 'crypto '() 3 30 0 'active '(fp) 0)])
      (let-values ([(g msg) (wumpus-step-look game)])
        (assert-equal 1 (wumpus-game-moves-used g))
        (assert-equal 'active (wumpus-game-status g)))))

  (define-test "look doesn't change position or arrows"
    (let ([game (make-wumpus-game *lattice-cave-graph*
                  'fp 'crypto '() 3 30 0 'active '(fp) 0)])
      (let-values ([(g msg) (wumpus-step-look game)])
        (assert-equal 'fp (wumpus-game-player-room g))
        (assert-equal 3 (wumpus-game-arrows g)))))

  (define-test "look on terminal game is no-op"
    (let ([game (make-wumpus-game *lattice-cave-graph*
                  'fp 'crypto '() 3 30 0 'eaten '(fp) 0)])
      (let-values ([(g msg) (wumpus-step-look game)])
        (assert-equal 0 (wumpus-game-moves-used g)))))
)

;;; ====
;;; Timeout Tests
;;; ====

(test-group "timeout"

  (define-test "moves-used = max-moves -> timeout"
    (let ([game (make-wumpus-game *lattice-cave-graph*
                  'fp 'crypto '() 3 30 29 'active '(fp) 0)])
      (let-values ([(g msg) (wumpus-step-look game)])
        (assert-equal 'timeout (wumpus-game-status g))
        (assert-equal 30 (wumpus-game-moves-used g)))))

  (define-test "hazard death takes priority over timeout"
    ;; If you walk into the wumpus on your last move, you're eaten not timed out
    (let ([game (make-wumpus-game *lattice-cave-graph*
                  'fp 'algebra '() 3 30 29 'active '(fp) 0)])
      (let-values ([(g msg) (wumpus-step-move game 'algebra)])
        (assert-equal 'eaten (wumpus-game-status g)))))
)

;;; ====
;;; Reward Tests
;;; ====

(test-group "reward"

  (define-test "won -> 1.0"
    (assert-equal 1.0 (wumpus-reward
      (make-wumpus-game '() 'a 'b '() 0 30 0 'won '() 0))))

  (define-test "eaten -> -1.0"
    (assert-equal -1.0 (wumpus-reward
      (make-wumpus-game '() 'a 'b '() 0 30 0 'eaten '() 0))))

  (define-test "fell -> -1.0"
    (assert-equal -1.0 (wumpus-reward
      (make-wumpus-game '() 'a 'b '() 0 30 0 'fell '() 0))))

  (define-test "timeout -> -0.3"
    (assert-equal -0.3 (wumpus-reward
      (make-wumpus-game '() 'a 'b '() 0 30 0 'timeout '() 0))))

  (define-test "active -> 0.0 (should not call on active games)"
    (assert-equal 0.0 (wumpus-reward
      (make-wumpus-game '() 'a 'b '() 0 30 0 'active '() 0))))
)

;;; ====
;;; Observation Format Tests
;;; ====

(test-group "observation"

  (define-test "observation contains room name"
    (let* ([game (make-wumpus-game *lattice-cave-graph*
                   'fp 'crypto '() 3 30 0 'active '(fp) 0)]
           [obs (wumpus-format-observation game "test")])
      (assert-true (string-contains-wumpus obs "room fp"))))

  (define-test "observation contains senses"
    (let* ([game (make-wumpus-game *lattice-cave-graph*
                   'fp 'optics '() 3 30 0 'active '(fp) 0)]
           [obs (wumpus-format-observation game "test")])
      (assert-true (string-contains-wumpus obs "stench"))))

  (define-test "observation contains message"
    (let* ([game (make-wumpus-game *lattice-cave-graph*
                   'fp 'crypto '() 3 30 0 'active '(fp) 0)]
           [obs (wumpus-format-observation game "The hunt begins.")])
      (assert-true (string-contains-wumpus obs "The hunt begins."))))

  (define-test "visited shows room list, not count"
    (let* ([game (make-wumpus-game *lattice-cave-graph*
                   'fp 'crypto '() 3 30 0 'active '(algebra fp) 0)]
           [obs (wumpus-format-observation game "test")])
      ;; Should contain the room names, not just "2"
      (assert-true (string-contains-wumpus obs "algebra"))
      (assert-true (string-contains-wumpus obs "(visited (algebra fp)"))))
)

;;; ====
;;; Session Wrapper Tests
;;; ====

(test-group "session-wrapper"

  (define-test "session wrapper round-trip"
    (load "lattice/pipeline/wumpus-session.ss")
    (let ([obs (wumpus-init! 42)])
      (assert-true (string-contains-wumpus obs "wumpus"))
      (assert-true (string-contains-wumpus obs "The hunt begins."))
      (assert-false (wumpus-done?))))

  (define-test "session move returns observation"
    (load "lattice/pipeline/wumpus-session.ss")
    (wumpus-init! 42)
    ;; seed 42 with subgraph: player=data, neighbors include sim
    (let ([obs (wumpus-move! 'sim)])
      (assert-true (string-contains-wumpus obs "sim"))))

  (define-test "session result returns status+reward"
    (load "lattice/pipeline/wumpus-session.ss")
    (wumpus-init! 42)
    ;; seed 42 with subgraph: linalg is a pit, adjacent to data (player start)
    (wumpus-move! 'linalg)
    (let ([result (wumpus-result)])
      (assert-equal 'fell (car result))
      (assert-equal -1.0 (cadr result))
      (assert-true (wumpus-done?))))
)

;;; ====
;;; Run
;;; ====

(run-all-tests)
