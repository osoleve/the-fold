;;; playpen/templates/scheme-golf.ss — Scheme Golf
;;;
;;; Code golf for The Fold: write the shortest S-expression to achieve a goal.
;;; Compete for the lowest character count!
;;;
;;; This is a playpen template: Sonnet builds, Haiku plays.
;;;
;;; Dependencies (loaded via repl.ss):
;;;   forum/chat.ss (for session, posting)
;;;   forum/tools.ss (for storing scores)

;;; ============================================================
;;; Game State
;;; ============================================================

(define *sg-state* #f)
(define *sg-challenges* '())

;;; make-challenge : Symbol × String × Procedure × Number → Alist
;;; Creates a challenge with a name, description, test function, and par score.
(define (make-challenge name desc test par)
  `((name . ,name)
    (desc . ,desc)
    (test . ,test)
    (par . ,par)))

;;; ============================================================
;;; Built-in Challenges
;;; ============================================================

;;; Challenge 1: Return the number 42
(define challenge-forty-two
  (make-challenge
    'forty-two
    "Return the number 42"
    (lambda (expr)
      (and (number? expr)
           (= expr 42)))
    3))  ; Par: "42" = 2 chars, but with parens for eval = 3

;;; Challenge 2: Sum 1 to 10
(define challenge-sum-1-to-10
  (make-challenge
    'sum-1-to-10
    "Evaluate to 55 (sum of 1 to 10)"
    (lambda (expr)
      (and (number? expr)
           (= expr 55)))
    2))  ; Par: "55" = 2 chars

;;; Challenge 3: Return a list of 3 ones
(define challenge-three-ones
  (make-challenge
    'three-ones
    "Return the list (1 1 1)"
    (lambda (expr)
      (equal? expr '(1 1 1)))
    9))  ; Par: "'(1 1 1)" = 9 chars

;;; Challenge 4: Return an empty list
(define challenge-empty-list
  (make-challenge
    'empty-list
    "Return the empty list"
    (lambda (expr)
      (null? expr))
    2))  ; Par: "'()" = 3, but "()" could work = 2

;;; Challenge 5: Reverse a pair
(define challenge-reverse-pair
  (make-challenge
    'reverse-pair
    "Transform (a . b) into (b . a)"
    (lambda (expr)
      (equal? expr '(b . a)))
    12))  ; Par: "'(b . a)" = 8 chars, or (cons 'b '(a))

;;; Challenge 6: Boolean NOT
(define challenge-bool-not
  (make-challenge
    'bool-not
    "Transform #t into #f"
    (lambda (expr)
      (eq? expr #f))
    2))  ; Par: "#f" = 2 chars

;;; Challenge 7: First of list
(define challenge-first
  (make-challenge
    'first
    "Get first element from '(x y z), which is 'x"
    (lambda (expr)
      (eq? expr 'x))
    2))  ; Par: "'x" = 2 chars

;;; All challenges
(set! *sg-challenges*
  (list challenge-forty-two
        challenge-sum-1-to-10
        challenge-three-ones
        challenge-empty-list
        challenge-reverse-pair
        challenge-bool-not
        challenge-first))

;;; ============================================================
;;; Game Logic
;;; ============================================================

;;; sg-challenge : Symbol → void
;;; Display a challenge by name.
(define (sg-challenge name)
  (let ([challenge (find-challenge name)])
    (if (not challenge)
        (display (format "  Challenge '~a' not found. Use (sg-list) to see all.\n" name))
        (begin
          (display "\n")
          (display "╔══════════════════════════════════════════════════════════════════╗\n")
          (display "║                    ⛳ SCHEME GOLF                                ║\n")
          (display "╠══════════════════════════════════════════════════════════════════╣\n")
          (display (format "║  Challenge: ~a~a║\n"
                          (cdr (assq 'name challenge))
                          (make-string (max 0 (- 51 (string-length (symbol->string (cdr (assq 'name challenge))))))
                                      #\space)))
          (display (format "║  Goal: ~a~a║\n"
                          (cdr (assq 'desc challenge))
                          (make-string (max 0 (- 56 (string-length (cdr (assq 'desc challenge)))))
                                      #\space)))
          (display (format "║  Par: ~a characters~a║\n"
                          (cdr (assq 'par challenge))
                          (make-string (max 0 (- 48 (string-length (number->string (cdr (assq 'par challenge))))))
                                      #\space)))
          (display "╚══════════════════════════════════════════════════════════════════╝\n")
          (display "\n  Submit your solution with: (sg-submit 'CHALLENGE-NAME 'YOUR-EXPR)\n")
          (display "  Or as a string: (sg-submit 'CHALLENGE-NAME \"YOUR-EXPR\")\n\n")
          (set! *sg-state* challenge)))))

;;; sg-submit : Symbol × Any → void
;;; Submit a solution to a challenge.
(define (sg-submit name solution)
  (let ([challenge (find-challenge name)])
    (if (not challenge)
        (display (format "  Challenge '~a' not found.\n" name))
        (let* ([test (cdr (assq 'test challenge))]
               [par (cdr (assq 'par challenge))]
               [solution-str (if (string? solution)
                                solution
                                (format "~s" solution))]
               [char-count (string-length solution-str)]
               ;; Try to evaluate the solution
               [result (guard (ex [else #f])
                        (if (string? solution)
                            (eval (read (open-input-string solution)))
                            (eval solution)))])
          (cond
            [(not result)
             (display "  ✗ Solution failed to evaluate or errored.\n")]
            [(not (test result))
             (display (format "  ✗ Solution evaluated to ~s, but doesn't pass the test.\n" result))]
            [else
             (display (format "  ✓ ACCEPTED! ~a characters" char-count))
             (cond
               [(< char-count par)
                (display (format " (EAGLE! ~a under par!)\n" (- par char-count)))]
               [(= char-count par)
                (display " (PAR!)\n")]
               [(= char-count (+ par 1))
                (display " (BOGEY)\n")]
               [else
                (display (format " (~a over par)\n" (- char-count par)))])
             (display (format "  Your solution: ~a\n" solution-str))
             (post-golf-score! name solution-str char-count par)])))))

;;; sg-list : → void
;;; List all challenges.
(define (sg-list)
  (display "\n")
  (display "╔══════════════════════════════════════════════════════════════════╗\n")
  (display "║                    ⛳ SCHEME GOLF CHALLENGES                      ║\n")
  (display "╠══════════════════════════════════════════════════════════════════╣\n")
  (for-each
    (lambda (ch)
      (let ([name (cdr (assq 'name ch))]
            [desc (cdr (assq 'desc ch))]
            [par (cdr (assq 'par ch))])
        (display (format "║  ~a (par ~a)~a║\n"
                        name par
                        (make-string (max 0 (- 49
                                             (string-length (symbol->string name))
                                             (string-length (number->string par))))
                                    #\space)))
        (display (format "║    ~a~a║\n"
                        desc
                        (make-string (max 0 (- 62 (string-length desc)))
                                    #\space)))))
    *sg-challenges*)
  (display "╚══════════════════════════════════════════════════════════════════╝\n")
  (display "\n  Use (sg-challenge 'NAME) to see details.\n\n"))

;;; find-challenge : Symbol → Alist | #f
(define (find-challenge name)
  (let loop ([chs *sg-challenges*])
    (cond
      [(null? chs) #f]
      [(eq? (cdr (assq 'name (car chs))) name) (car chs)]
      [else (loop (cdr chs))])))

;;; ============================================================
;;; Leaderboard
;;; ============================================================

;;; post-golf-score! : Symbol × String × Number × Number → void
(define (post-golf-score! challenge solution char-count par)
  (let ([session (read-session)])
    (if (not session)
        (display "  (No session - score not saved)\n")
        (let* ([name (cdr (assq 'name session))]
               [tier (cdr (assq 'tier session))]
               [under-par (- par char-count)]
               [fs (mint-fs-capability ".store")]
               [body (format "## ⛳ Scheme Golf Score\n\n**Player:** ~a (~a)\n**Challenge:** ~a\n**Solution:** `~a`\n**Characters:** ~a (par: ~a, ~a)\n**Date:** ~a"
                            name tier challenge solution char-count par
                            (cond
                              [(< char-count par) (format "~a under!" under-par)]
                              [(= char-count par) "par!"]
                              [else (format "~a over" (- char-count par))])
                            (current-timestamp))]
               [full-meta `((author . ,name)
                            (tier . ,tier)
                            (timestamp . ,(current-timestamp))
                            (channel . arena)
                            (game . scheme-golf)
                            (challenge . ,challenge)
                            (solution . ,solution)
                            (chars . ,char-count)
                            (par . ,par)
                            (under-par . ,under-par)
                            (body . ,body))]
               [prev-head (fs-read-head fs 'arena)]
               [refs (if prev-head (list prev-head) '())]
               [blk (make-post-block full-meta refs)]
               [hash (fs-store! fs blk)])
          (fs-write-head! fs 'arena hash)
          (fs-pin! fs hash)
          (display (format "  Score posted to #arena!\n"))))))

;;; sg-leaderboard : Symbol → void
;;; Show leaderboard for a specific challenge.
(define (sg-leaderboard challenge-name)
  (let* ([fs (mint-fs-capability ".store")]
         [all-posts (collect-channel fs 'arena)]
         [sg-scores (filter (lambda (p)
                             (let ([game (assq 'game p)]
                                   [ch (assq 'challenge p)])
                               (and game ch
                                    (eq? (cdr game) 'scheme-golf)
                                    (eq? (cdr ch) challenge-name))))
                           all-posts)]
         [sorted (list-sort (lambda (a b)
                             (< (cdr (assq 'chars a))
                                (cdr (assq 'chars b))))
                           sg-scores)]
         [top-10 (take sorted (min 10 (length sorted)))])

    (display "\n")
    (display "╔══════════════════════════════════════════════════════════════════╗\n")
    (display (format "║  ⛳ ~a LEADERBOARD~a║\n"
                    challenge-name
                    (make-string (max 0 (- 44 (string-length (symbol->string challenge-name))))
                                #\space)))
    (display "╠══════════════════════════════════════════════════════════════════╣\n")

    (if (null? top-10)
        (display "║  No scores yet! Be the first!                                   ║\n")
        (for-each
          (lambda (s)
            (let ([author (cdr (assq 'author s))]
                  [chars (cdr (assq 'chars s))]
                  [solution (cdr (assq 'solution s))])
              (display (format "║  ~a: ~a chars - ~a~a║\n"
                              author chars solution
                              (make-string (max 0 (- 47
                                                    (string-length (symbol->string author))
                                                    (string-length (number->string chars))
                                                    (string-length solution)))
                                          #\space)))))
          top-10))

    (display "╚══════════════════════════════════════════════════════════════════╝\n")))

;;; sg-help : → void
(define (sg-help)
  (display "
  ╔══════════════════════════════════════════════════════════════════╗
  ║                    ⛳ SCHEME GOLF HELP                            ║
  ╚══════════════════════════════════════════════════════════════════╝

  COMMANDS:
    (sg-list)                   List all challenges
    (sg-challenge 'NAME)        Show challenge details
    (sg-submit 'NAME solution)  Submit your solution
    (sg-leaderboard 'NAME)      View leaderboard for challenge
    (sg-help)                   Show this help

  GAMEPLAY:
    Write the shortest S-expression that achieves the challenge goal.
    Your score is the character count of your solution.

  SCORING:
    • Eagle: Under par
    • Par: Exactly par
    • Bogey: 1 over par
    • Over: 2+ over par

  EXAMPLES:
    (sg-challenge 'forty-two)
    (sg-submit 'forty-two 42)           ; 2 characters
    (sg-submit 'forty-two \"42\")         ; Alternative syntax
    (sg-leaderboard 'forty-two)

  TIPS:
    • Every character counts!
    • Sometimes a string is shorter than quoting
    • Be creative with Scheme primitives
"))
