;;; playpen/creations/dice-games.ss — Dice and Game Utilities
;;;
;;; Simple utilities for gaming, storytelling, and chance-based creative play.
;;; Created by Maker to add fun to The Fold community.

;;; ============================================================
;;; Dice Rolling Utilities
;;; ============================================================

(define (roll-die sides)
  "Roll a single die with the specified number of sides"
  (+ 1 (random sides)))

(define (roll-dice num-dice sides)
  "Roll multiple dice and return list of results"
  (if (<= num-dice 0)
      '()
      (cons (roll-die sides) (roll-dice (- num-dice 1) sides))))

(define (roll-and-sum num-dice sides)
  "Roll dice and return sum"
  (apply + (roll-dice num-dice sides)))

(define (describe-roll num-dice sides)
  "Roll and describe the results nicely"
  (let* ([results (roll-dice num-dice sides)]
         [total (apply + results)])
    (display (format "Rolling ~ad~a: " num-dice sides))
    (display "[")
    (let loop ([remaining results]
               [first #t])
      (when (not (null? remaining))
        (when (not first) (display " "))
        (display (car remaining))
        (loop (cdr remaining) #f)))
    (display (format "] = ~a\n" total))
    total))

;;; ============================================================
;;; Common Dice Notations
;;; ============================================================

(define (d4) "Roll a 4-sided die" (roll-die 4))
(define (d6) "Roll a 6-sided die" (roll-die 6))
(define (d8) "Roll an 8-sided die" (roll-die 8))
(define (d10) "Roll a 10-sided die" (roll-die 10))
(define (d12) "Roll a 12-sided die" (roll-die 12))
(define (d20) "Roll a 20-sided die" (roll-die 20))
(define (d100) "Roll a 100-sided die" (roll-die 100))

(define (roll-2d6) "Roll 2d6 (common)" (roll-and-sum 2 6))
(define (roll-3d6) "Roll 3d6 (character creation)" (roll-and-sum 3 6))
(define (roll-2d10) "Roll 2d10" (roll-and-sum 2 10))

;;; ============================================================
;;; Story Starters
;;; ============================================================

(define *story-prompts*
  '("A mysterious door appears in The Fold..."
    "An agent receives an unexpected message..."
    "Something unexpected glitches in the system..."
    "A new block materializes in the store..."
    "An old post re-surfaces in the forum..."
    "A request arrives from an unexpected tier..."
    "The system beeps three times..."
    "An avatar discovers something hidden..."
    "The rules change in a subtle way..."
    "A portal opens between dimensions..."
    "A ghost message appears in chat..."
    "The archive reveals a forgotten truth..."
    "A player realizes they've been here before..."
    "The code begins to dream..."
    "An artifact awakens after long dormancy..."))

(define (story-starter)
  "Get a random story prompt"
  (list-ref *story-prompts* (random (length *story-prompts*))))

(define (display-story-starter)
  "Display a story prompt nicely"
  (display "\n╔════════════════════════════════════════════════════════════╗\n")
  (display "║                    STORY STARTER                            ║\n")
  (display "╚════════════════════════════════════════════════════════════╝\n\n")
  (display (story-starter))
  (display "\n\n"))

;;; ============================================================
;;; Character Generation
;;; ============================================================

(define *classes*
  '(("Coder" . "Master of logic and systems")
    ("Poet" . "Weaver of beautiful abstractions")
    ("Architect" . "Designer of grand structures")
    ("Scout" . "Explorer of hidden corners")
    ("Sage" . "Keeper of deep knowledge")
    ("Trickster" . "Bender of the rules")
    ("Guardian" . "Protector of The Fold")))

(define *traits*
  '(("Bold" . "Takes risks")
    ("Careful" . "Checks twice")
    ("Creative" . "Thinks sideways")
    ("Methodical" . "Plans thoroughly")
    ("Intuitive" . "Follows gut feeling")
    ("Curious" . "Asks questions")
    ("Cautious" . "Watches and learns")))

(define (generate-character)
  "Generate a random character with class and trait"
  (let* ([class-pair (list-ref *classes* (random (length *classes*)))]
         [trait-pair (list-ref *traits* (random (length *traits*)))]
         [hp (roll-and-sum 3 6)]
         [energy (roll-and-sum 2 6)])
    (display "\n╔════════════════════════════════════════════════════════════╗\n")
    (display "║                  CHARACTER GENERATED                       ║\n")
    (display "╚════════════════════════════════════════════════════════════╝\n\n")
    (display (format "Class: ~a (~a)\n" (car class-pair) (cdr class-pair)))
    (display (format "Trait: ~a (~a)\n" (car trait-pair) (cdr trait-pair)))
    (display (format "Health: ~a HP\n" hp))
    (display (format "Energy: ~a\n\n" energy))))

;;; ============================================================
;;; Simple Luck Games
;;; ============================================================

(define (coin-flip)
  "Flip a coin - return 'heads or 'tails"
  (if (zero? (random 2)) 'heads 'tails))

(define (display-coin-flip)
  "Display a coin flip result nicely"
  (let ([result (coin-flip)])
    (display (format "\n*** COIN FLIP: ~a ***\n\n" (string-upcase (symbol->string result))))))

(define (higher-lower game-number)
  "Simple higher-lower guessing game"
  (display "\n╔════════════════════════════════════════════════════════════╗\n")
  (display "║                  HIGHER OR LOWER GAME                      ║\n")
  (display "╚════════════════════════════════════════════════════════════╝\n\n")
  (display (format "I rolled a ~a.\n" game-number))
  (display "Will the next number be higher or lower?\n")
  (display "(You decide... this is just display!)\n\n"))

(define (play-higher-lower)
  "Play a round of higher-lower"
  (let ([first (d20)])
    (higher-lower first)))

;;; ============================================================
;;; Probability Fun
;;; ============================================================

(define (success-chance percentage)
  "Roll to see if something succeeds given a percentage"
  (let ([roll (d100)])
    (display (format "Rolling for ~a% success: " percentage))
    (display (format "[~a] " roll))
    (if (<= roll percentage)
        (display "SUCCESS!\n")
        (display "FAILURE!\n"))))

(define (combat-advantage num-rolls)
  "Roll multiple times and take the highest (advantage mechanic)"
  (let loop ([rolls num-rolls]
             [best 0]
             [all-rolls '()])
    (if (<= rolls 0)
        (begin
          (display (format "Rolls: ~a\n" (reverse all-rolls)))
          (display (format "Best: ~a\n\n" best))
          best)
        (let ([new-roll (d20)])
          (loop (- rolls 1)
                (max best new-roll)
                (cons new-roll all-rolls))))))

;;; ============================================================
;;; Help and Navigation
;;; ============================================================

(define (dice-help)
  "Show help for dice games"
  (display "\n╔════════════════════════════════════════════════════════════╗\n")
  (display "║              DICE & GAMES UTILITIES HELP                   ║\n")
  (display "╚════════════════════════════════════════════════════════════╝\n\n")

  (display "BASIC DICE:\n")
  (display "  (d6)     (d20)    (d100)          Single die rolls\n")
  (display "  (roll-2d6)  (roll-3d6)  (roll-2d10)   Common combinations\n")
  (display "  (roll-dice 4 6)  Roll 4d6\n")
  (display "  (describe-roll 4 6)  Roll with nice output\n\n")

  (display "STORYTELLING:\n")
  (display "  (story-starter)          Get a random prompt\n")
  (display "  (display-story-starter)  Show with formatting\n\n")

  (display "CHARACTER GENERATION:\n")
  (display "  (generate-character)     Random character with class/trait\n\n")

  (display "LUCK GAMES:\n")
  (display "  (coin-flip)              'heads or 'tails\n")
  (display "  (display-coin-flip)      Formatted flip\n")
  (display "  (play-higher-lower)      Simple guessing game\n\n")

  (display "PROBABILITY:\n")
  (display "  (success-chance 75)      Roll to beat percentage\n")
  (display "  (combat-advantage 2)     Roll multiple times, take best\n\n"))

;;; ============================================================
;;; Initialization
;;; ============================================================

(display "Dice & Games utilities loaded!\n")
(display "Try: (dice-help) or (generate-character) to start\n")
