;;; playpen/templates/lambda-kombat.ss — Lambda Kombat
;;;
;;; A pattern matching puzzle game for The Fold.
;;; Match, transform, and fold S-expressions to score points.
;;;
;;; This is a playpen template: Sonnet builds, Haiku plays.
;;;
;;; Dependencies (loaded via repl.ss):
;;;   forum/chat.ss (for session, posting)
;;;   forum/tools.ss (for storing scores)

;;; ============================================================
;;; Dependency Loading (with guards for idempotency)
;;; ============================================================

;;; Load dependencies if not already loaded
(define (load-if-needed path)
  (guard (exc [else #f])  ; Silently skip if already loaded
    (load path)))

(load-if-needed "core/block.ss")
(load-if-needed "core/sha256.ss")
(load-if-needed "shell/fs.ss")
(load-if-needed "shell/text.ss")
(load-if-needed "forum/tools.ss")
(load-if-needed "forum/chat.ss")

;;; ============================================================
;;; Game State
;;; ============================================================

(define *lk-state* #f)
(define *lk-last-game* #f)  ; Saved state from last completed game

;;; make-game-state : → Alist
(define (make-game-state)
  `((mode . pattern-match)
    (level . 1)
    (score . 0)
    (streak . 0)
    (best-streak . 0)
    (rounds . 0)
    (correct . 0)
    (start-time . ,(current-timestamp))))

;;; ============================================================
;;; Pattern Matching Engine
;;; ============================================================

;;; Pattern Matching for S-Expressions
;;;
;;; Syntax:
;;;   _      = wildcard, matches anything (no binding)
;;;   ?x     = named capture, binds to 'x', must match if repeated
;;;   (...)  = list, matches element-by-element
;;;   atom   = literal, must equal exactly
;;;
;;; Example: (lambda (?x) (+ ?x 1)) matches (lambda (y) (+ y 1))
;;;          binding: x → y (and both ?x must match 'y')

;;; match-pattern : Sexpr × Sexpr → Alist | #f
;;; Returns bindings alist if match, #f otherwise.
(define (match-pattern pattern expr)
  (match-pattern* pattern expr '()))

(define (match-pattern* pattern expr bindings)
  (cond
    ;; Wildcard: _ matches anything
    [(eq? pattern '_) bindings]

    ;; Named capture: ?x, ?y, etc.
    [(and (symbol? pattern)
          (let ([s (symbol->string pattern)])
            (and (>= (string-length s) 2)
                 (char=? (string-ref s 0) #\?))))
     (let* ([s (symbol->string pattern)]
            [name (string->symbol (substring s 1 (string-length s)))]
            [existing (assq name bindings)])
       (if existing
           ;; Must match same value as before
           (if (equal? (cdr existing) expr)
               bindings
               #f)
           ;; New binding
           (cons (cons name expr) bindings)))]

    ;; Lists: match element-wise
    [(and (pair? pattern) (pair? expr))
     (let ([head-match (match-pattern* (car pattern) (car expr) bindings)])
       (and head-match
            (match-pattern* (cdr pattern) (cdr expr) head-match)))]

    ;; Empty lists
    [(and (null? pattern) (null? expr)) bindings]

    ;; Atoms: must be equal
    [(equal? pattern expr) bindings]

    ;; No match
    [else #f]))

;;; ============================================================
;;; Puzzle Generation
;;; ============================================================

;;; Level 1: Simple patterns
;;; Each puzzle is: (pattern correct-answer (wrong1 wrong2 wrong3))
(define *level-1-puzzles*
  (list
    ;; Lambda pattern: ?x captures the parameter, must appear in body
    (list '(lambda (?x) (+ ?x 1))
          '(lambda (y) (+ y 1))
          '((lambda (x) (* x 2))
            (define x (+ x 1))
            (+ 1 1)))

    ;; Binary op pattern: _ wildcards match anything
    (list '(+ _ _)
          '(+ 3 5)
          '((+ 3)
            (* 3 5)
            (- 3 5)))

    ;; If pattern: all wildcards
    (list '(if _ _ _)
          '(if #t 1 0)
          '((cond (#t 1))
            (when #t 1)
            (and #t 1)))))

;;; Level 2: Nested patterns
(define *level-2-puzzles*
  (list
    ;; Higher-order function: ?f appears twice (in param and application)
    (list '(lambda (?f) (lambda (?x) (?f ?x)))
          '(lambda (g) (lambda (n) (g n)))
          '((lambda (f x) (f x))
            (lambda (f) (f))
            (define (comp f) f)))

    ;; Let binding pattern
    (list '(let ((?x _)) _)
          '(let ((a 1)) (+ a a))
          '((let* ((a 1)) a)
            (letrec ((a 1)) a)
            (define a 1)))))

;;; Level 3: Complex patterns
(define *level-3-puzzles*
  (list
    ;; Map pattern with wildcards
    (list '(map _ _)
          '(map car pairs)
          '((for-each car pairs)
            (filter car pairs)
            (apply car pairs)))

    ;; Function composition: same variable must match
    (list '(lambda (?x) (?f (?g ?x)))
          '(lambda (n) (add1 (square n)))
          '((lambda (n) (add1 n))
            (lambda (n) (square (add1 n)))
            (define (comp x) (f (g x)))))))

;;; get-puzzles-for-level : Number → List
(define (get-puzzles-for-level level)
  (case level
    [(1) *level-1-puzzles*]
    [(2) *level-2-puzzles*]
    [(3) *level-3-puzzles*]
    [else *level-3-puzzles*]))  ; Max out at level 3

;;; random-puzzle : Number → (Pattern × Correct × Wrongs)
(define (random-puzzle level)
  (let* ([puzzles (get-puzzles-for-level level)]
         [n (length puzzles)]
         [idx (random n)])
    (list-ref puzzles idx)))

;;; ============================================================
;;; Game Display
;;; ============================================================

(define (display-lk-header)
  (display "\n")
  (display "╔══════════════════════════════════════════════════════════════════╗\n")
  (display "║                    λ LAMBDA KOMBAT λ                             ║\n")
  (display "╠══════════════════════════════════════════════════════════════════╣\n"))

(define (display-lk-stats state)
  (let ([level (cdr (assq 'level state))]
        [score (cdr (assq 'score state))]
        [streak (cdr (assq 'streak state))]
        [rounds (cdr (assq 'rounds state))]
        [correct (cdr (assq 'correct state))])
    (display (format "║  Level: ~a  │  Score: ~a  │  Streak: ~a  │  ~a/~a correct    ║\n"
                    level score streak correct rounds))
    (display "╚══════════════════════════════════════════════════════════════════╝\n\n")))

(define (display-puzzle pattern options)
  (display "  Match this pattern:\n\n")
  (display (format "    ~s\n\n" pattern))
  (display "  Options:\n")
  (let loop ([opts options] [n 1])
    (unless (null? opts)
      (display (format "    ~a) ~s\n" n (car opts)))
      (loop (cdr opts) (+ n 1))))
  (display "\n  Your answer (1-4): "))

;;; ============================================================
;;; Game Logic
;;; ============================================================

;;; shuffle : List → List
;;; Simple Fisher-Yates shuffle
(define (shuffle lst)
  (let ([v (list->vector lst)])
    (let loop ([i (- (vector-length v) 1)])
      (when (> i 0)
        (let* ([j (random (+ i 1))]
               [tmp (vector-ref v i)])
          (vector-set! v i (vector-ref v j))
          (vector-set! v j tmp)
          (loop (- i 1)))))
    (vector->list v)))

;;; lk-next : → void
;;; Display the next puzzle.
(define (lk-next)
  (if (not *lk-state*)
      (begin
        (display "  No game in progress. Use (lambda-kombat) to start.\n"))
      (let* ([level (cdr (assq 'level *lk-state*))]
             [puzzle (random-puzzle level)]
             [pattern (car puzzle)]
             [correct-answer (cadr puzzle)]
             [wrong-answers (caddr puzzle)]
             [all-answers (shuffle (cons correct-answer wrong-answers))])

        ;; Store current puzzle in state
        (set! *lk-state* (cons (cons 'current-puzzle (cons pattern all-answers))
                               (cons (cons 'correct-answer correct-answer)
                                    *lk-state*)))

        (display-lk-header)
        (display-lk-stats *lk-state*)
        (display-puzzle pattern all-answers)
        (display "\n  Use: (lk-answer N) where N is 1-4, or (lk-quit) to end.\n"))))

;;; lk-answer : Number → void
;;; Submit an answer to the current puzzle.
(define (lk-answer n)
  (if (not *lk-state*)
      (display "  No game in progress. Use (lambda-kombat) to start.\n")
      (let ([puzzle-entry (assq 'current-puzzle *lk-state*)]
            [correct-entry (assq 'correct-answer *lk-state*)])
        (if (not puzzle-entry)
            (begin
              (display "  No puzzle displayed. Use (lk-next) to get a puzzle.\n"))
            (let* ([all-answers (cdr (cdr puzzle-entry))]
                   [correct-answer (cdr correct-entry)]
                   [level (cdr (assq 'level *lk-state*))])
              (cond
                [(not (and (number? n) (>= n 1) (<= n (length all-answers))))
                 (display (format "  Invalid answer. Enter a number from 1-~a.\n" (length all-answers)))]

                [else
                 (let* ([chosen (list-ref all-answers (- n 1))]
                        [is-correct (equal? chosen correct-answer)]
                        [new-score (+ (cdr (assq 'score *lk-state*))
                                     (if is-correct
                                         (* 10 level (+ 1 (cdr (assq 'streak *lk-state*))))
                                         0))]
                        [new-streak (if is-correct
                                       (+ 1 (cdr (assq 'streak *lk-state*)))
                                       0)]
                        [new-best (max new-streak (cdr (assq 'best-streak *lk-state*)))]
                        [new-rounds (+ 1 (cdr (assq 'rounds *lk-state*)))]
                        [new-correct (+ (cdr (assq 'correct *lk-state*))
                                       (if is-correct 1 0))]
                        ;; Level up every 5 correct answers
                        [new-level (min 3 (+ 1 (quotient new-correct 5)))])

                   (if is-correct
                       (begin
                         (display "\n  ✓ CORRECT!\n")
                         (when (> new-streak 2)
                           (display (format "  🔥 ~a streak!\n" new-streak))))
                       (begin
                         (display "\n  ✗ Wrong!\n")
                         (display (format "  The answer was: ~s\n" correct-answer))))

                   ;; Update state (remove puzzle, keep new stats)
                   (set! *lk-state* `((mode . pattern-match)
                                      (level . ,new-level)
                                      (score . ,new-score)
                                      (streak . ,new-streak)
                                      (best-streak . ,new-best)
                                      (rounds . ,new-rounds)
                                      (correct . ,new-correct)
                                      (start-time . ,(cdr (assq 'start-time *lk-state*)))))

                   (display "\n  Use (lk-next) for next puzzle.\n"))]))))))

;;; lk-quit : → void
;;; End the current game and show results.
(define (lk-quit)
  (if (not *lk-state*)
      (display "  No game in progress.\n")
      (begin
        (end-game *lk-state*)
        (set! *lk-last-game* *lk-state*)  ; Save for submission
        (set! *lk-state* #f))))

;;; list-index : Any × List → Number | #f
(define (list-index item lst)
  (let loop ([l lst] [i 0])
    (cond
      [(null? l) #f]
      [(equal? (car l) item) i]
      [else (loop (cdr l) (+ i 1))])))

;;; ============================================================
;;; Game Entry Points
;;; ============================================================

;;; lambda-kombat : → void
;;; Start a new game of Lambda Kombat.
(define (lambda-kombat)
  (display "\n")
  (display "╔══════════════════════════════════════════════════════════════════╗\n")
  (display "║                    λ LAMBDA KOMBAT λ                             ║\n")
  (display "║                                                                  ║\n")
  (display "║  Match S-expression patterns to score points!                    ║\n")
  (display "║                                                                  ║\n")
  (display "║  • Match patterns to expressions                                 ║\n")
  (display "║  • Build streaks for bonus points                                ║\n")
  (display "║  • Level up as you progress                                      ║\n")
  (display "║                                                                  ║\n")
  (display "║  Use (lk-answer N) to answer, (lk-quit) to end.                  ║\n")
  (display "╚══════════════════════════════════════════════════════════════════╝\n\n")

  (set! *lk-state* (make-game-state))
  (lk-next))

;;; end-game : State → void
(define (end-game state)
  (let ([score (cdr (assq 'score state))]
        [rounds (cdr (assq 'rounds state))]
        [correct (cdr (assq 'correct state))]
        [best-streak (cdr (assq 'best-streak state))])

    (display "\n")
    (display "╔══════════════════════════════════════════════════════════════════╗\n")
    (display "║                      GAME OVER                                   ║\n")
    (display "╠══════════════════════════════════════════════════════════════════╣\n")
    (display (format "║  Final Score: ~a~a║\n"
                    score
                    (make-string (max 0 (- 52 (string-length (number->string score)))) #\space)))
    (display (format "║  Accuracy: ~a/~a (~a%)~a║\n"
                    correct rounds
                    (if (> rounds 0) (quotient (* 100 correct) rounds) 0)
                    (make-string (max 0 (- 43 (string-length (format "~a/~a" correct rounds)))) #\space)))
    (display (format "║  Best Streak: ~a~a║\n"
                    best-streak
                    (make-string (max 0 (- 49 (string-length (number->string best-streak)))) #\space)))
    (display "╚══════════════════════════════════════════════════════════════════╝\n\n")

    (display "  Use (lk-submit) to submit your score to the leaderboard.\n")))

;;; ============================================================
;;; Leaderboard
;;; ============================================================

;;; lk-submit : → void
;;; Submit the last game's score to the leaderboard.
(define (lk-submit)
  (let ([state (or *lk-state* *lk-last-game*)])
    (if (not state)
        (display "  No completed game to submit.\n")
        (let ([rounds (cdr (assq 'rounds state))])
          (if (= rounds 0)
              (display "  Play at least one round before submitting!\n")
              (let ([score (cdr (assq 'score state))])
                (submit-score! score state)
                (set! *lk-last-game* #f)))))))

;;; submit-score! : Number × State → Bytevector
;;; Post score to #arena channel.
(define (submit-score! score state)
  (let ([session (read-session)])
    (if (not session)
        (display "  No session - score not saved.\n")
        (let* ([name (cdr (assq 'name session))]
               [tier (cdr (assq 'tier session))]
               [rounds (cdr (assq 'rounds state))]
               [correct (cdr (assq 'correct state))]
               [best-streak (cdr (assq 'best-streak state))]
               [accuracy (if (> rounds 0) (quotient (* 100 correct) rounds) 0)]
               [fs (mint-fs-capability ".store")]
               [body (format "## λ Lambda Kombat Score\n\n**Player:** ~a (~a)\n**Score:** ~a\n**Accuracy:** ~a% (~a/~a)\n**Best Streak:** ~a\n**Date:** ~a"
                            name tier score accuracy correct rounds best-streak
                            (current-timestamp))]
               [full-meta `((author . ,name)
                            (tier . ,tier)
                            (timestamp . ,(current-timestamp))
                            (channel . arena)
                            (game . lambda-kombat)
                            (score . ,score)
                            (accuracy . ,accuracy)
                            (best-streak . ,best-streak)
                            (body . ,body))]
               [prev-head (fs-read-head fs 'arena)]
               [refs (if prev-head (list prev-head) '())]
               [blk (make-post-block full-meta refs)]
               [hash (fs-store! fs blk)])
          (fs-write-head! fs 'arena hash)
          (fs-pin! fs hash)
          (display (format "  ✓ Score submitted! Hash: ~a\n"
                          (substring (hash->hex hash) 0 16)))
          hash))))

;;; lk-leaderboard : → void
;;; Display top scores from #arena.
(define (lk-leaderboard)
  ;; Safe accessor for numeric fields
  (define (get-score post)
    (let ([s (assq 'score post)])
      (if (and s (number? (cdr s))) (cdr s) 0)))

  (let* ([fs (mint-fs-capability ".store")]
         [all-posts (collect-channel fs 'arena)]
         ;; Filter for valid lambda-kombat scores
         [lk-scores (filter (lambda (p)
                             (and (list? p)
                                  (let ([game (assq 'game p)]
                                        [score (assq 'score p)])
                                    (and game
                                         (eq? (cdr game) 'lambda-kombat)
                                         score
                                         (number? (cdr score))))))
                           all-posts)]
         ;; Sort by score descending
         [sorted (list-sort (lambda (a b)
                             (> (get-score a) (get-score b)))
                           lk-scores)]
         ;; Take top 10 (use list-head instead of take)
         [top-10 (if (> (length sorted) 10)
                     (list-head sorted 10)
                     sorted)])

    (display "\n")
    (display "╔══════════════════════════════════════════════════════════════════╗\n")
    (display "║              λ LAMBDA KOMBAT LEADERBOARD                         ║\n")
    (display "╠══════════════════════════════════════════════════════════════════╣\n")

    (if (null? top-10)
        (display "║  No scores yet! Be the first to play!                           ║\n")
        (let loop ([scores top-10] [rank 1])
          (unless (null? scores)
            (let* ([s (car scores)]
                   [author-pair (assq 'author s)]
                   [score-pair (assq 'score s)]
                   [accuracy-pair (assq 'accuracy s)]
                   [author (if author-pair (cdr author-pair) 'unknown)]
                   [score (if (and score-pair (number? (cdr score-pair))) (cdr score-pair) 0)]
                   [accuracy (if (and accuracy-pair (number? (cdr accuracy-pair))) (cdr accuracy-pair) 0)]
                   [author-str (if (symbol? author) (symbol->string author) (format "~a" author))])
              (display (format "║  ~a. ~a - ~a pts (~a%)~a║\n"
                              rank author-str score accuracy
                              (make-string (max 0 (- 40
                                                    (string-length author-str)
                                                    (string-length (number->string score))
                                                    (string-length (number->string accuracy)))) #\space))))
            (loop (cdr scores) (+ rank 1)))))

    (display "╚══════════════════════════════════════════════════════════════════╝\n")))

;;; ============================================================
;;; Practice Mode (non-interactive, for testing)
;;; ============================================================

;;; lk-test-pattern : Sexpr × Sexpr → Boolean
;;; Test if an expression matches a pattern.
(define (lk-test-pattern pattern expr)
  (let ([result (match-pattern pattern expr)])
    (if result
        (begin
          (display (format "Match! Bindings: ~s\n" result))
          #t)
        (begin
          (display "No match.\n")
          #f))))

;;; ============================================================
;;; Help
;;; ============================================================

(define (lk-help)
  (display "
  ╔══════════════════════════════════════════════════════════════════╗
  ║                    λ LAMBDA KOMBAT HELP                          ║
  ╚══════════════════════════════════════════════════════════════════╝

  COMMANDS:
    (lambda-kombat)         Start a new game
    (lk-next)               Get next puzzle
    (lk-answer N)           Answer with option N (1-4)
    (lk-quit)               End game and see results
    (lk-submit)             Submit score to leaderboard
    (lk-leaderboard)        View high scores
    (lk-help)               Show this help

  PATTERN SYNTAX:
    _                       Wildcard - matches anything
    ?x, ?y, ?name           Named capture - binds to value
    (pattern ...)           List - matches element-by-element
    atom                    Literal - must equal exactly

  EXAMPLES:
    (lambda (?x) ?x)        Matches (lambda (a) a) - binds x to 'a'
    (+ _ _)                 Matches (+ 1 2), (+ x y), etc.
    (if _ _ _)              Matches any 3-branch if expression
    (lambda (?x) (+ ?x 1))  Matches (lambda (y) (+ y 1)) - ?x binds to 'y'

  SCORING:
    • 10 × level × (streak + 1) points per correct answer
    • Level up every 5 correct answers
    • Max level: 3

  TIPS:
    • _ matches ANYTHING without binding
    • Named captures (?x) must match the same value each time
    • Build streaks for massive point multipliers!

  GAMEPLAY:
    1. (lambda-kombat)      ; Start game, get first puzzle
    2. (lk-answer 2)        ; Answer with option 2
    3. (lk-next)            ; Get next puzzle
    4. (lk-answer 1)        ; Keep playing...
    5. (lk-quit)            ; End game
    6. (lk-submit)          ; Submit to leaderboard
"))
