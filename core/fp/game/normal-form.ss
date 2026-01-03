;;; core/fp/game/normal-form.ss — Normal Form (Strategic) Games
;;;
;;; Implements strategic form games with payoff matrices.
;;; Supports Nash equilibrium, dominated strategy elimination,
;;; mixed strategies, and best response dynamics.
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; A normal form game consists of:
;;;   - Players (indexed 0, 1, ... n-1)
;;;   - Strategy sets for each player
;;;   - Payoff function: strategy profile → payoff vector
;;;
;;; For 2-player games, we represent payoffs as a matrix where:
;;;   - Rows are Player 1's strategies
;;;   - Columns are Player 2's strategies
;;;   - Each cell contains (payoff1 . payoff2)
;;;
;;; Dependencies:
;;;   - core/prelude.ss

(load "core/base/prelude.ss")

;;; ============================================================
;;; Game Representation
;;; ============================================================

;;; A 2-player normal form game is represented as:
;;;   (game strategies1 strategies2 payoff-matrix)
;;; where:
;;;   - strategies1 : (Vector Symbol) — Player 1's strategy names
;;;   - strategies2 : (Vector Symbol) — Player 2's strategy names
;;;   - payoff-matrix : (Vector (Vector (Pair Number Number)))
;;;     Matrix[i][j] = (p1-payoff . p2-payoff) when P1 plays i, P2 plays j

(define-record-type game%
  (fields strategies1 strategies2 payoffs))

;;; make-game : (List Symbol) × (List Symbol) × (List (List (Pair Num Num))) → Game
;;; Create a normal form game from lists.
(define (make-game strats1 strats2 payoff-lists)
  (let ([s1 (list->vector strats1)]
        [s2 (list->vector strats2)]
        [matrix (list->vector
                 (map list->vector payoff-lists))])
       (make-game% s1 s2 matrix)))

;;; game-num-strategies : Game × Nat → Nat
;;; Get number of strategies for player (0 or 1).
(define (game-num-strategies game player)
  (if (= player 0)
      (vector-length (game%-strategies1 game))
      (vector-length (game%-strategies2 game))))

;;; game-payoff : Game × Nat × Nat → (Pair Num Num)
;;; Get payoff pair for strategy profile (i, j).
(define (game-payoff game i j)
  (vector-ref (vector-ref (game%-payoffs game) i) j))

;;; game-payoff-p1 : Game × Nat × Nat → Num
;;; Get Player 1's payoff for strategy profile (i, j).
(define (game-payoff-p1 game i j)
  (car (game-payoff game i j)))

;;; game-payoff-p2 : Game × Nat × Nat → Num
;;; Get Player 2's payoff for strategy profile (i, j).
(define (game-payoff-p2 game i j)
  (cdr (game-payoff game i j)))

;;; ============================================================
;;; Classic Games
;;; ============================================================

;;; Prisoner's Dilemma
;;; (C,C)=(3,3), (C,D)=(0,5), (D,C)=(5,0), (D,D)=(1,1)
(define prisoners-dilemma
  (make-game
   '(cooperate defect)
   '(cooperate defect)
   '(((3 . 3) (0 . 5))
     ((5 . 0) (1 . 1)))))

;;; Matching Pennies (zero-sum)
;;; (H,H)=(1,-1), (H,T)=(-1,1), (T,H)=(-1,1), (T,T)=(1,-1)
(define matching-pennies
  (make-game
   '(heads tails)
   '(heads tails)
   '(((1 . -1) (-1 . 1))
     ((-1 . 1) (1 . -1)))))

;;; Battle of the Sexes
;;; (O,O)=(3,2), (O,F)=(0,0), (F,O)=(0,0), (F,F)=(2,3)
(define battle-of-sexes
  (make-game
   '(opera football)
   '(opera football)
   '(((3 . 2) (0 . 0))
     ((0 . 0) (2 . 3)))))

;;; Stag Hunt
;;; (S,S)=(4,4), (S,H)=(0,3), (H,S)=(3,0), (H,H)=(3,3)
(define stag-hunt
  (make-game
   '(stag hare)
   '(stag hare)
   '(((4 . 4) (0 . 3))
     ((3 . 0) (3 . 3)))))

;;; Chicken (Hawk-Dove)
;;; (D,D)=(0,0), (D,S)=(7,2), (S,D)=(2,7), (S,S)=(6,6)
(define chicken
  (make-game
   '(dare swerve)
   '(dare swerve)
   '(((0 . 0) (7 . 2))
     ((2 . 7) (6 . 6)))))

;;; ============================================================
;;; Best Response
;;; ============================================================

;;; best-response-p1 : Game × Nat → (List Nat)
;;; Find Player 1's best responses to Player 2 playing strategy j.
;;; Returns list of strategy indices that maximize P1's payoff.
(define (best-response-p1 game j)
  (let ([n (game-num-strategies game 0)])
       (if (= n 0)
           '()  ; No strategies available
           (let* ([payoffs (map (lambda (i) (game-payoff-p1 game i j))
                                (iota n))]
                  [max-payoff (apply max payoffs)])
                 (filter (lambda (i) (= (list-ref payoffs i) max-payoff))
                         (iota n))))))

;;; best-response-p2 : Game × Nat → (List Nat)
;;; Find Player 2's best responses to Player 1 playing strategy i.
(define (best-response-p2 game i)
  (let ([n (game-num-strategies game 1)])
       (if (= n 0)
           '()  ; No strategies available
           (let* ([payoffs (map (lambda (j) (game-payoff-p2 game i j))
                                (iota n))]
                  [max-payoff (apply max payoffs)])
                 (filter (lambda (j) (= (list-ref payoffs j) max-payoff))
                         (iota n))))))

;;; ============================================================
;;; Dominated Strategies
;;; ============================================================

;;; strictly-dominates-p1? : Game × Nat × Nat → Boolean
;;; Does strategy i strictly dominate strategy i' for Player 1?
;;; (i.e., u1(i,j) > u1(i',j) for all j)
(define (strictly-dominates-p1? game i i-prime)
  (let ([n2 (game-num-strategies game 1)])
       (andmap (lambda (j)
                       (> (game-payoff-p1 game i j)
                          (game-payoff-p1 game i-prime j)))
               (iota n2))))

;;; strictly-dominates-p2? : Game × Nat × Nat → Boolean
;;; Does strategy j strictly dominate strategy j' for Player 2?
(define (strictly-dominates-p2? game j j-prime)
  (let ([n1 (game-num-strategies game 0)])
       (andmap (lambda (i)
                       (> (game-payoff-p2 game i j)
                          (game-payoff-p2 game i j-prime)))
               (iota n1))))

;;; is-strictly-dominated-p1? : Game × Nat → Boolean
;;; Is strategy i strictly dominated by some other strategy for P1?
(define (is-strictly-dominated-p1? game i)
  (let ([n1 (game-num-strategies game 0)])
       (ormap (lambda (other)
                      (and (not (= other i))
                           (strictly-dominates-p1? game other i)))
              (iota n1))))

;;; is-strictly-dominated-p2? : Game × Nat → Boolean
;;; Is strategy j strictly dominated by some other strategy for P2?
(define (is-strictly-dominated-p2? game j)
  (let ([n2 (game-num-strategies game 1)])
       (ormap (lambda (other)
                      (and (not (= other j))
                           (strictly-dominates-p2? game other j)))
              (iota n2))))

;;; find-dominated-strategies : Game → ((List Nat) . (List Nat))
;;; Find all strictly dominated strategies for both players.
;;; Returns (p1-dominated . p2-dominated).
(define (find-dominated-strategies game)
  (let ([n1 (game-num-strategies game 0)]
        [n2 (game-num-strategies game 1)])
       (cons (filter (lambda (i) (is-strictly-dominated-p1? game i)) (iota n1))
             (filter (lambda (j) (is-strictly-dominated-p2? game j)) (iota n2)))))

;;; ============================================================
;;; Iterated Elimination of Dominated Strategies (IESDS)
;;; ============================================================

;;; eliminate-strategy-p1 : Game × Nat → Game
;;; Create new game with P1's strategy i removed.
(define (eliminate-strategy-p1 game i)
  (let* ([s1 (game%-strategies1 game)]
         [s2 (game%-strategies2 game)]
         [payoffs (game%-payoffs game)]
         [n1 (vector-length s1)]
         ;; Remove strategy i from s1
         [new-s1 (list->vector
                  (filter (lambda (x) x)
                          (map (lambda (k)
                                       (if (= k i) #f (vector-ref s1 k)))
                               (iota n1))))]
         ;; Remove row i from payoffs
         [new-payoffs (list->vector
                       (filter (lambda (x) x)
                               (map (lambda (k)
                                            (if (= k i) #f (vector-ref payoffs k)))
                                    (iota n1))))])
        (make-game% new-s1 s2 new-payoffs)))

;;; eliminate-strategy-p2 : Game × Nat → Game
;;; Create new game with P2's strategy j removed.
(define (eliminate-strategy-p2 game j)
  (let* ([s1 (game%-strategies1 game)]
         [s2 (game%-strategies2 game)]
         [payoffs (game%-payoffs game)]
         [n2 (vector-length s2)]
         ;; Remove strategy j from s2
         [new-s2 (list->vector
                  (filter (lambda (x) x)
                          (map (lambda (k)
                                       (if (= k j) #f (vector-ref s2 k)))
                               (iota n2))))]
         ;; Remove column j from each row
         [new-payoffs
          (vector-map
           (lambda (row)
                   (list->vector
                    (filter (lambda (x) x)
                            (map (lambda (k)
                                         (if (= k j) #f (vector-ref row k)))
                                 (iota n2)))))
           payoffs)])
        (make-game% s1 new-s2 new-payoffs)))

;;; iesds : Game × Nat → Game
;;; Iteratively eliminate strictly dominated strategies.
;;; fuel limits iterations to ensure termination.
(define (iesds game fuel)
  (if (<= fuel 0)
      game
      (let ([dominated (find-dominated-strategies game)])
           (cond
            ;; Eliminate P1 dominated strategy
            [(not (null? (car dominated)))
             (iesds (eliminate-strategy-p1 game (car (car dominated)))
                    (- fuel 1))]
            ;; Eliminate P2 dominated strategy
            [(not (null? (cdr dominated)))
             (iesds (eliminate-strategy-p2 game (car (cdr dominated)))
                    (- fuel 1))]
            ;; No dominated strategies remain
            [else game]))))

;;; ============================================================
;;; Pure Strategy Nash Equilibrium
;;; ============================================================

;;; is-pure-nash? : Game × Nat × Nat → Boolean
;;; Is (i, j) a pure strategy Nash equilibrium?
;;; True iff i is a best response to j AND j is a best response to i.
(define (is-pure-nash? game i j)
  (and (member i (best-response-p1 game j))
       (member j (best-response-p2 game i))
       #t))

;;; find-pure-nash : Game → (List (Pair Nat Nat))
;;; Find all pure strategy Nash equilibria.
(define (find-pure-nash game)
  (let ([n1 (game-num-strategies game 0)]
        [n2 (game-num-strategies game 1)])
       (filter (lambda (profile)
                       (is-pure-nash? game (car profile) (cdr profile)))
               (apply append
                      (map (lambda (i)
                                   (map (lambda (j) (cons i j))
                                        (iota n2)))
                           (iota n1))))))

;;; ============================================================
;;; Mixed Strategies
;;; ============================================================

;;; A mixed strategy is a probability distribution over pure strategies.
;;; Represented as a vector of probabilities that sum to 1.

;;; make-mixed-strategy : (List Number) → (Vector Number)
;;; Create a mixed strategy from probability list.
(define (make-mixed-strategy probs)
  (list->vector probs))

;;; pure-strategy : Nat × Nat → (Vector Number)
;;; Create a pure strategy (probability 1 on strategy i).
(define (pure-strategy i n)
  (let ([v (make-vector n 0)])
       (vector-set! v i 1)
       v))

;;; uniform-strategy : Nat → (Vector Number)
;;; Create a uniform mixed strategy.
(define (uniform-strategy n)
  (make-vector n (/ 1 n)))

;;; expected-payoff-p1 : Game × (Vector Num) × (Vector Num) → Num
;;; Compute Player 1's expected payoff under mixed strategies.
(define (expected-payoff-p1 game sigma1 sigma2)
  (let ([n1 (vector-length sigma1)]
        [n2 (vector-length sigma2)])
       (apply + (map (lambda (i)
                             (apply + (map (lambda (j)
                                                   (* (vector-ref sigma1 i)
                                                      (vector-ref sigma2 j)
                                                      (game-payoff-p1 game i j)))
                                           (iota n2))))
                     (iota n1)))))

;;; expected-payoff-p2 : Game × (Vector Num) × (Vector Num) → Num
;;; Compute Player 2's expected payoff under mixed strategies.
(define (expected-payoff-p2 game sigma1 sigma2)
  (let ([n1 (vector-length sigma1)]
        [n2 (vector-length sigma2)])
       (apply + (map (lambda (i)
                             (apply + (map (lambda (j)
                                                   (* (vector-ref sigma1 i)
                                                      (vector-ref sigma2 j)
                                                      (game-payoff-p2 game i j)))
                                           (iota n2))))
                     (iota n1)))))

;;; strategy-payoff-vs-mixed : Game × Nat × Nat × (Vector Num) → Num
;;; Compute payoff for player playing pure strategy against opponent's mixed.
;;; player: 0 for P1, 1 for P2
;;; strategy: the pure strategy index
;;; opp-mixed: opponent's mixed strategy
(define (strategy-payoff-vs-mixed game player strategy opp-mixed)
  (if (= player 0)
      ;; P1 plays pure strategy, P2 plays mixed
      (let ([n2 (vector-length opp-mixed)])
           (apply + (map (lambda (j)
                                 (* (vector-ref opp-mixed j)
                                    (game-payoff-p1 game strategy j)))
                         (iota n2))))
      ;; P2 plays pure strategy, P1 plays mixed
      (let ([n1 (vector-length opp-mixed)])
           (apply + (map (lambda (i)
                                 (* (vector-ref opp-mixed i)
                                    (game-payoff-p2 game i strategy)))
                         (iota n1))))))

;;; ============================================================
;;; 2x2 Mixed Strategy Nash Equilibrium
;;; ============================================================

;;; For 2x2 games, we can compute the mixed strategy Nash equilibrium
;;; analytically using the indifference principle.

;;; solve-2x2-nash : Game → (List ((Vector Num) . (Vector Num)))
;;; Find all Nash equilibria (pure and mixed) for a 2x2 game.
;;; Returns list of (sigma1 . sigma2) pairs.
(define (solve-2x2-nash game)
  (if (or (not (= 2 (game-num-strategies game 0)))
          (not (= 2 (game-num-strategies game 1))))
      '()  ; Not a 2x2 game
      (let* (;; Epsilon for float comparison
             [epsilon 1e-10]
             ;; Get payoffs
             [a (game-payoff-p1 game 0 0)]
             [b (game-payoff-p1 game 0 1)]
             [c (game-payoff-p1 game 1 0)]
             [d (game-payoff-p1 game 1 1)]
             [e (game-payoff-p2 game 0 0)]
             [f (game-payoff-p2 game 0 1)]
             [g (game-payoff-p2 game 1 0)]
             [h (game-payoff-p2 game 1 1)]
             ;; Pure Nash equilibria
             [pure-nash (find-pure-nash game)]
             [pure-results
              (map (lambda (profile)
                           (cons (pure-strategy (car profile) 2)
                                 (pure-strategy (cdr profile) 2)))
                   pure-nash)]
             ;; Mixed Nash: P2's q makes P1 indifferent
             ;; a*q + b*(1-q) = c*q + d*(1-q)
             ;; (a-b-c+d)*q = d-b
             [denom1 (- (+ a d) (+ b c))]
             [q (if (< (abs denom1) epsilon) #f (/ (- d b) denom1))]
             ;; P1's p makes P2 indifferent
             ;; e*p + g*(1-p) = f*p + h*(1-p)
             ;; (e-f-g+h)*p = h-g
             [denom2 (- (+ e h) (+ f g))]
             [p (if (< (abs denom2) epsilon) #f (/ (- h g) denom2))]
             ;; Check if mixed equilibrium is valid
             [mixed-valid? (and q p
                                (>= q 0) (<= q 1)
                                (>= p 0) (<= p 1)
                                ;; Allow pure equilibria (p=0/1 or q=0/1)
                                ;; But don't duplicate pure Nash we already found
                                (not (and (or (< (abs p) epsilon) (< (abs (- p 1)) epsilon))
                                          (or (< (abs q) epsilon) (< (abs (- q 1)) epsilon)))))]
             [mixed-result
              (if mixed-valid?
                  (list (cons (make-mixed-strategy (list p (- 1 p)))
                              (make-mixed-strategy (list q (- 1 q)))))
                  '())])
            (append pure-results mixed-result))))

;;; ============================================================
;;; Game Display
;;; ============================================================

;;; game->string : Game → String
;;; Display a game as a payoff matrix.
(define (game->string game)
  (let* ([s1 (game%-strategies1 game)]
         [s2 (game%-strategies2 game)]
         [n1 (vector-length s1)]
         [n2 (vector-length s2)]
         [header (string-append
                  "        "
                  (apply string-append
                         (map (lambda (j)
                                      (string-append
                                       (symbol->string (vector-ref s2 j))
                                       "    "))
                              (iota n2)))
                  "\n")]
         [rows (map (lambda (i)
                            (string-append
                             (symbol->string (vector-ref s1 i))
                             ": "
                             (apply string-append
                                    (map (lambda (j)
                                                 (let ([p (game-payoff game i j)])
                                                      (string-append
                                                       "("
                                                       (number->string (car p))
                                                       ","
                                                       (number->string (cdr p))
                                                       ") ")))
                                         (iota n2)))
                             "\n"))
                    (iota n1))])
        (apply string-append header rows)))
