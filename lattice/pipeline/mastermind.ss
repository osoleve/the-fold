;;; lattice/pipeline/mastermind.ss — Pure Mastermind Game Logic
;;;
;;; Code-breaking deduction game. A secret code of N colors is generated;
;;; the player guesses and receives (exact, misplaced) feedback per turn.
;;; Designed to test persistent state tracking in RLM agents — without
;;; using (think ...) to accumulate constraints, the model loses early
;;; feedback to the sliding window and degrades to random guessing.
;;;
;;; Standard config: 4 positions, 6 colors, 10 guesses.

;;; ====
;;; RNG (LCG)
;;; ====

(define *mm-lcg-a* 1103515245)
(define *mm-lcg-c* 12345)
(define *mm-lcg-m* (expt 2 31))

(define (make-mm-rng seed)
  (doc 'export #t)
  (list 'mm-rng (modulo (abs seed) *mm-lcg-m*)))

(define (mm-rng-state rng) (cadr rng))

(define (mm-rng-next rng)
  (doc 'export #t)
  (let ([next (modulo (+ (* *mm-lcg-a* (mm-rng-state rng)) *mm-lcg-c*)
                      *mm-lcg-m*)])
    (values next (list 'mm-rng next))))

(define (mm-rng-int rng n)
  (doc 'export #t)
  (let-values ([(val rng*) (mm-rng-next rng)])
    (values (modulo val n) rng*)))

;;; ====
;;; Colors & Config
;;; ====

(define *mastermind-colors* '(red blue green yellow white orange))

(define (make-mastermind-config code-length num-colors max-guesses)
  (doc 'export #t)
  (list code-length num-colors max-guesses))

(define (mastermind-config-code-length cfg) (car cfg))
(define (mastermind-config-num-colors cfg) (cadr cfg))
(define (mastermind-config-max-guesses cfg) (caddr cfg))

;; 4 positions, 6 colors, 10 guesses
(define (default-mastermind-config)
  (doc 'export #t)
  (make-mastermind-config 4 6 10))

;;; ====
;;; Game State
;;; ====

(define (make-mastermind-game secret config guesses status)
  (doc 'export #t)
  (list 'mastermind-game secret config guesses status))

(define (mastermind-game-secret g)  (list-ref g 1))
(define (mastermind-game-config g)  (list-ref g 2))
(define (mastermind-game-guesses g) (list-ref g 3))
(define (mastermind-game-status g)  (list-ref g 4))

(define (mastermind-terminal? game)
  (doc 'export #t)
  (not (eq? 'active (mastermind-game-status game))))

;;; ====
;;; Scoring
;;; ====

;;; Count occurrences of x in lst
(define (mm-count x lst)
  (doc 'export #t)
  (let loop ([l lst] [n 0])
    (cond
      [(null? l) n]
      [(eq? x (car l)) (loop (cdr l) (+ n 1))]
      [else (loop (cdr l) n)])))

;;; Score a guess against the secret.
;;; Returns (exact . misplaced) where:
;;;   exact    = right color, right position
;;;   misplaced = right color, wrong position (not double-counted)
(define (mastermind-score secret guess)
  (doc 'export #t)
  (let* (;; Step 1: count exact matches
         [exact (let loop ([s secret] [g guess] [count 0])
                  (if (null? s) count
                      (loop (cdr s) (cdr g)
                            (if (eq? (car s) (car g)) (+ count 1) count))))]
         ;; Step 2: remove exact-matched positions
         [s-remaining (let loop ([s secret] [g guess] [acc '()])
                        (if (null? s) (reverse acc)
                            (if (eq? (car s) (car g))
                                (loop (cdr s) (cdr g) acc)
                                (loop (cdr s) (cdr g) (cons (car s) acc)))))]
         [g-remaining (let loop ([s secret] [g guess] [acc '()])
                        (if (null? s) (reverse acc)
                            (if (eq? (car s) (car g))
                                (loop (cdr s) (cdr g) acc)
                                (loop (cdr s) (cdr g) (cons (car g) acc)))))]
         ;; Step 3: misplaced = sum_color min(count_in_s, count_in_g)
         ;; Collect unique colors from s-remaining
         [uniq (let loop ([l s-remaining] [acc '()])
                 (if (null? l) acc
                     (if (memq (car l) acc)
                         (loop (cdr l) acc)
                         (loop (cdr l) (cons (car l) acc)))))]
         [misplaced (let loop ([cs uniq] [total 0])
                      (if (null? cs) total
                          (let ([c (car cs)])
                            (loop (cdr cs)
                                  (+ total (min (mm-count c s-remaining)
                                               (mm-count c g-remaining)))))))])
    (cons exact misplaced)))

;;; ====
;;; Episode Creation
;;; ====

(define (mastermind-available-colors config)
  (doc 'export #t)
  (let loop ([cs *mastermind-colors*]
             [n (mastermind-config-num-colors config)]
             [acc '()])
    (if (or (null? cs) (= n 0)) (reverse acc)
        (loop (cdr cs) (- n 1) (cons (car cs) acc)))))

(define (mastermind-make-episode config seed)
  (doc 'export #t)
  (let* ([code-length (mastermind-config-code-length config)]
         [num-colors (mastermind-config-num-colors config)]
         [colors (mastermind-available-colors config)]
         [rng (make-mm-rng seed)]
         [secret (let loop ([i 0] [rng rng] [acc '()])
                   (if (= i code-length) (reverse acc)
                       (let-values ([(idx rng*) (mm-rng-int rng num-colors)])
                         (loop (+ i 1) rng* (cons (list-ref colors idx) acc)))))])
    (make-mastermind-game secret config '() 'active)))

;;; ====
;;; Game Actions
;;; ====

(define (mastermind-step-guess game guess)
  (doc 'export #t)
  (let* ([config (mastermind-game-config game)]
         [code-length (mastermind-config-code-length config)]
         [max-guesses (mastermind-config-max-guesses config)]
         [colors (mastermind-available-colors config)])
    (cond
      ;; Game already over
      [(mastermind-terminal? game)
       (values game "Game is already over.")]
      ;; Not a list
      [(not (list? guess))
       (values game
         (format "Invalid guess: expected a list of ~a colors, e.g. (mastermind-guess! '(red blue green yellow))"
                 code-length))]
      ;; Wrong length
      [(not (= (length guess) code-length))
       (values game
         (format "Invalid guess: expected ~a colors, got ~a." code-length (length guess)))]
      ;; Invalid colors
      [(let loop ([g guess])
         (and (not (null? g))
              (or (not (memq (car g) colors))
                  (loop (cdr g)))))
       (values game
         (format "Invalid color in guess. Valid colors: ~a" colors))]
      [else
       (let* ([secret (mastermind-game-secret game)]
              [score (mastermind-score secret guess)]
              [exact (car score)]
              [misplaced (cdr score)]
              [guesses* (append (mastermind-game-guesses game)
                                (list (list guess exact misplaced)))]
              [won? (= exact code-length)]
              [out-of-guesses? (>= (length guesses*) max-guesses)]
              [status (cond [won? 'won]
                            [out-of-guesses? 'lost]
                            [else 'active])]
              [game* (make-mastermind-game secret config guesses* status)]
              [msg (mastermind-format-feedback guess exact misplaced
                     (- max-guesses (length guesses*)) status)])
         (values game* msg))])))

;;; ====
;;; Observation Formatting
;;; ====

;;; Feedback after a guess — no history, just the result.
;;; The model must use (think ...) to track accumulated constraints.
(define (mastermind-format-feedback guess exact misplaced remaining status)
  (doc 'export #t)
  (string-append
    (format "(mastermind-feedback\n")
    (format "  (your-guess ~a)\n" guess)
    (format "  (exact ~a)  ;; right color, right position\n" exact)
    (format "  (misplaced ~a)  ;; right color, wrong position\n" misplaced)
    (format "  (guesses-remaining ~a)\n" remaining)
    (format "  (status ~a))" status)))

;;; Initial observation — game parameters, no secret.
(define (mastermind-format-observation game message)
  (doc 'export #t)
  (let* ([config (mastermind-game-config game)]
         [code-length (mastermind-config-code-length config)]
         [max-guesses (mastermind-config-max-guesses config)]
         [colors (mastermind-available-colors config)])
    (string-append
      (format "(mastermind\n")
      (format "  (message ~s)\n" message)
      (format "  (colors ~a)\n" colors)
      (format "  (code-length ~a)\n" code-length)
      (format "  (max-guesses ~a)\n" max-guesses)
      (format "  (status ~a))" (mastermind-game-status game)))))

;;; ====
;;; Reward
;;; ====

(define (mastermind-reward game)
  (doc 'export #t)
  (case (mastermind-game-status game)
    [(won)  1.0]
    [(lost) -1.0]
    [else    0.0]))
