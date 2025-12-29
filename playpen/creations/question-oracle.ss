;;; playpen/creations/question-oracle.ss — The Question Oracle Game
;;;
;;; An interactive oracle that answers yes/no questions with mystical flair.
;;; Ask the oracle anything, and it will divine the truth from the computational aether.
;;;
;;; This is a playful exploration of randomness, user interaction, and ASCII art.
;;; Perfect for decision-making moments when you need cosmic wisdom.
;;;
;;; Created by: Haiku (Player tier)
;;; Date: 2025-12-27

;;; ============================================================
;;; The Oracle Database
;;; ============================================================

;;; Positive responses (the oracle is in a good mood)
(define *positive-responses* '(
                               "The patterns align: YES"
                               "The lambda smiles: YES"
                               "Continuations confirm: YES"
                               "The recursion echoes: YES"
                               "The bits are auspicious: YES"
                               "The REPL whispers: YES"
                               "The closure embraces it: YES"
                               "Destiny decrees: YES"
                               "The universe compiles: YES"
                               "Pure function confirms: YES"
                               ))

;;; Negative responses (oracle says no)
(define *negative-responses* '(
                               "The pattern breaks: NO"
                               "The lambda frowns: NO"
                               "Stack overflow detected: NO"
                               "Recursion limit reached: NO"
                               "The bits scatter: NO"
                               "The REPL remains silent: NO"
                               "The closure rejects: NO"
                               "Entropy prevails: NO"
                               "Type error: NO"
                               "Segmentation fault: NO"
                               ))

;;; Uncertain responses (oracle is feeling mysterious)
(define *uncertain-responses* '(
                                "The oracle is confused: MAYBE"
                                "The computation never halts: UNKNOWN"
                                "This question lies outside the universe: UNDEFINED"
                                "The oracle demands clarification: ASK AGAIN"
                                "Too much for one mind to hold: PERHAPS"
                                "The answer transcends logic: BEYOND YES/NO"
                                "Reality is mutable: CANNOT SAY"
                                "The universe awaits more input: MAYBE LATER"
                                "This requires higher fuel: INSUFFICIENT POWER"
                                "The oracle takes a coffee break: TRY AGAIN"
                                ))

;;; ============================================================
;;; ASCII Art Oracle
;;; ============================================================

;;; display-oracle : String → void
;;;
;;; Display the oracle in all its mystical ASCII glory
(define (display-oracle response)
  (display "\n")
  (display "            ∿∿∿∿∿∿∿∿∿∿∿∿∿∿∿∿∿∿∿∿∿∿∿∿∿\n")
  (display "           ∿                         ∿\n")
  (display "          ∿   ◆ THE QUESTION ORACLE ◆   ∿\n")
  (display "           ∿                         ∿\n")
  (display "            ∿∿∿∿∿∿∿∿∿∿∿∿∿∿∿∿∿∿∿∿∿∿∿∿∿\n")
  (display "                    .---.\n")
  (display "                   ( o.o )\n")
  (display "                    > ^ <\n")
  (display "                   /|   |\\\n")
  (display "                  /_|   |_\\\n")
  (display "\n")
  (display "            ✨ The oracle speaks: ✨\n")
  (display "\n")
  (display "              ╔═════════════════╗\n")
  (display "              ║ ")
  (display (pad-center response 15))
  (display " ║\n")
  (display "              ╚═════════════════╝\n")
  (display "\n"))

;;; pad-center : String Nat → String
;;;
;;; Center a string within a given width
(define (pad-center s width)
  (let ([len (string-length s)])
       (if (>= len width)
           (substring s 0 width)
           (let* ([total-padding (- width len)]
                  [left-padding (quotient total-padding 2)]
                  [right-padding (- total-padding left-padding)])
                 (string-append
                  (make-string left-padding #\space)
                  s
                  (make-string right-padding #\space))))))

;;; ============================================================
;;; Oracle Logic
;;; ============================================================

;;; random-element : (List α) → α
;;;
;;; Pick a random element from a non-empty list
(define (random-element lst)
  (list-ref lst (random (length lst))))

;;; predict-answer : → String
;;;
;;; Generate an answer: 70% positive, 20% negative, 10% uncertain
(define (predict-answer)
  (let ([roll (random 100)])
       (cond
        [(< roll 70) (random-element *positive-responses*)]
        [(< roll 90) (random-element *negative-responses*)]
        [else (random-element *uncertain-responses*)])))

;;; ============================================================
;;; Interactive Shell
;;; ============================================================

;;; oracle-shell : → void
;;;
;;; Run the interactive oracle game
(define (oracle-shell)
  (display "\n")
  (display "╔════════════════════════════════════════╗\n")
  (display "║     🔮 WELCOME TO THE ORACLE 🔮      ║\n")
  (display "║                                        ║\n")
  (display "║  Ask any yes/no question, and the     ║\n")
  (display "║  oracle shall divine the answer from  ║\n")
  (display "║  the computational depths below.      ║\n")
  (display "╚════════════════════════════════════════╝\n\n")
  
  (display "Commands:\n")
  (display "  (ask)   - Ask the oracle a question\n")
  (display "  (quit)  - Leave the oracle's presence\n")
  (display "  (help)  - Show oracle wisdom\n\n")
  
  (let main-loop ()
       (display "🔮 What do you wish to know? ")
       (flush-output-port)
       
       (let ([cmd (read)])
            (cond
             [(eq? cmd 'ask)
              (display "Ask your question (and press Enter twice when done):\n> ")
              (flush-output-port)
              (let* ([question (read-line)]
                     [answer (predict-answer)])
                    (display-oracle answer)
                    (main-loop))]
             
             [(eq? cmd 'quit)
              (display "\n🔮 The oracle fades into the computational aether...\n")
              (display "   May your questions find their answers.\n\n")]
             
             [(eq? cmd 'help)
              (display "\n📖 Oracle Wisdom:\n")
              (display "   - Ask yes/no questions\n")
              (display "   - Trust the randomness\n")
              (display "   - The oracle speaks truth (70% of the time)\n")
              (display "   - Some questions transcend yes and no\n")
              (display "   - Type (ask) to consult the oracle\n\n")
              (main-loop)]
             
             [else
              (display "❓ The oracle does not understand.\n")
              (display "   Use (ask), (help), or (quit).\n\n")
              (main-loop)]))))

;;; ============================================================
;;; Quick Access Functions
;;; ============================================================

;;; get-answer : → void
;;;
;;; Get a single answer without the interactive loop
(define (get-answer)
  (display-oracle (predict-answer)))

;;; ============================================================
;;; Test/Demo
;;; ============================================================

;;; When this file is loaded, uncomment one of these to play:
;;;
;;; (oracle-shell)       ; Start the interactive oracle
;;; (get-answer)         ; Get a single answer
;;;
;;; Example session:
;;;   (load "playpen/creations/question-oracle.ss")
;;;   (oracle-shell)
;;;   > (ask)
;;;   > Ask your question: Will Scheme ever defeat Python?
;;;   > [oracle answers]
;;;   > (quit)
;;;
