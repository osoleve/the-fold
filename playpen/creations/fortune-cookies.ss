;;; playpen/creations/fortune-cookies.ss — Scheme Wisdom Fortune Cookies
;;;
;;; A fun interactive utility that cracks open digital fortune cookies
;;; filled with Scheme programming wisdom, Lisp philosophy, and functional
;;; programming enlightenment!
;;;
;;; Each cookie contains a fortune about Scheme, continuations, recursion,
;;; lambda expressions, and the beauty of functional programming.
;;;
;;; Created by: Utility Crafter (Haiku tier)
;;; Date: 2025-12-26

;;; ============================================================
;;; The Fortune Cookie Database
;;; ============================================================

(define *fortunes* '(
  "In Scheme, all is list. All is function. All is possibility."
  "A continuation is a reified return address. Use it wisely."
  "Why mutate when you can transform? Immutability is enlightenment."
  "Let over lambda: write it down, bind it hard."
  "The lambda calculus is complete. Everything else is commentary."
  "Recursion is the spiral of thought made manifest in code."
  "car and cdr: the heartbeat of Lisp programs everywhere."
  "First-class functions: treat them as data, and they will set you free."
  "cons cells are the atoms of the Lisp universe."
  "A macro is code that writes code. Be humble, be wise."
  "The empty list is both nothing and everything. Ponder this."
  "To quote is to defer. To unquote is to unleash power."
  "Higher-order functions: when your functions have functions."
  "Tail recursion: eliminate the stack, embrace the loop."
  "The REPL is your meditation chamber. Type, think, evolve."
  "Functional purity: a promise you make to yourself."
  "Pattern matching is life's greatest joy. Master it."
  "Closure: carrying your environment with you, always."
  "A deep test of your understanding: write a Y combinator."
  "Lexical scope is your shield against chaos."
  "Dynamic scope: with great power comes great responsibility."
  "The call stack is sacred. Respect it, or optimize it away."
  "Continuations make time travel possible. Use them carefully."
  "Error handling: exceptions are not the Scheme way."
  "A pure function is reproducible. A pure function is testable."
  "Refactoring: small changes, big impact. Be patient."
  "Comments are love letters to your future self."
  "Functional composition: elegant, powerful, eternal."
  "A list is just nested pairs. Simple. Beautiful. Infinite."
  "The best code is the code you don't write."
))

;;; ============================================================
;;; ASCII Art Cookie
;;; ============================================================

;;; display-cookie : String → void
;;;
;;; Display a delightful ASCII art fortune cookie with the given fortune.
(define (display-cookie fortune)
  (let ([indent "         "]
        [words (string-split-simple fortune)])

    ;; Top of cookie
    (display indent)
    (display "    ╱─────────────────╲\n")
    (display indent)
    (display "   ╱                   ╲\n")

    ;; Cookie body with fortune - split into 3 lines
    (let* ([line-1 (take-words words 5)]
           [remaining-1 (drop-words words 5)]
           [line-2 (take-words remaining-1 5)]
           [remaining-2 (drop-words remaining-1 5)]
           [line-3 (take-words remaining-2 5)])

      (display indent)
      (display "  │ ")
      (display (pad-string line-1 32))
      (display " │\n")

      (display indent)
      (display "  │ ")
      (display (pad-string line-2 32))
      (display " │\n")

      (display indent)
      (display "  │ ")
      (display (pad-string line-3 32))
      (display " │\n"))

    ;; Bottom of cookie
    (display indent)
    (display "   ╲                   ╱\n")
    (display indent)
    (display "    ╲─────────────────╱\n\n")))

;;; ============================================================
;;; Helper Functions
;;; ============================================================

;;; string-split-simple : String → (List String)
;;;
;;; Split a string by spaces. Very simple version.
(define (string-split-simple s)
  (let ([len (string-length s)])
    (let loop ([i 0] [current ""] [result '()])
      (cond
        [(>= i len)
         (if (string=? current "")
             (reverse result)
             (reverse (cons current result)))]
        [(char=? (string-ref s i) #\space)
         (loop (+ i 1) "" (if (string=? current "")
                             result
                             (cons current result)))]
        [else
         (loop (+ i 1)
               (string-append current (string (string-ref s i)))
               result)]))))

;;; take-words : (List String) × Nat → String
;;;
;;; Take up to n words and join them with spaces.
(define (take-words words n)
  (let loop ([w words] [count 0] [result '()])
    (cond
      [(>= count n) (string-join (reverse result))]
      [(null? w) (string-join (reverse result))]
      [else (loop (cdr w) (+ count 1) (cons (car w) result))])))

;;; drop-words : (List String) × Nat → (List String)
;;;
;;; Drop the first n words from a list.
(define (drop-words words n)
  (if (or (<= n 0) (null? words))
      words
      (drop-words (cdr words) (- n 1))))

;;; string-join : (List String) → String
;;;
;;; Join strings with spaces.
(define (string-join strings)
  (if (null? strings)
      ""
      (let loop ([s (cdr strings)]
                 [result (car strings)])
        (if (null? s)
            result
            (loop (cdr s)
                  (string-append result " " (car s)))))))

;;; string-contains-at : String × Nat × Char → (Option Nat)
;;;
;;; Find the position of a character starting from position i.
(define (string-contains-at s start-pos c)
  (let ([len (string-length s)])
    (let loop ([i start-pos])
      (cond
        [(>= i len) #f]
        [(char=? (string-ref s i) c) i]
        [else (loop (+ i 1))]))))

;;; pad-string : String × Nat → String
;;;
;;; Pad a string to a given length with spaces on the right.
(define (pad-string s len)
  (if (>= (string-length s) len)
      (substring s 0 len)
      (string-append s (make-string (- len (string-length s)) #\space))))

;;; random-fortune : → String
;;;
;;; Pick a random fortune from the database.
(define (random-fortune)
  (let ([index (random (length *fortunes*))])
    (list-ref *fortunes* index)))

;;; ============================================================
;;; Interactive Shell
;;; ============================================================

;;; fortune-shell : → void
;;;
;;; Run the interactive fortune cookie generator.
(define (fortune-shell)
  (display "╔════════════════════════════════════════╗\n")
  (display "║  🥠  SCHEME WISDOM FORTUNE COOKIES  🥠  ║\n")
  (display "║                                        ║\n")
  (display "║  Crack open delightful digital cookies ║\n")
  (display "║  filled with functional programming    ║\n")
  (display "║  wisdom and Scheme enlightenment!      ║\n")
  (display "╚════════════════════════════════════════╝\n\n")

  (let main-loop ()
    (display "Commands:\n")
    (display "  (fortune)  - Get a new fortune cookie\n")
    (display "  (quit)     - Exit the cookie jar\n")
    (display "  (spin)     - Spin the wheel (3 random cookies)\n\n")

    (display "🥠 Your choice: ")
    (flush-output-port)

    (let ([cmd (read)])
      (cond
        [(eq? cmd 'fortune)
         (display "\n✨ Cracking open your fortune cookie...\n\n")
         (display-cookie (random-fortune))
         (main-loop)]

        [(eq? cmd 'spin)
         (display "\n🎡 Spinning the fortune wheel...\n\n")
         (display-cookie (random-fortune))
         (display-cookie (random-fortune))
         (display-cookie (random-fortune))
         (main-loop)]

        [(eq? cmd 'quit)
         (display "\n🙏 May your code be pure and your continuations bright!\n\n")]

        [else
         (display "\n❓ Unknown command. Try (fortune), (spin), or (quit).\n\n")
         (main-loop)]))))

;;; ============================================================
;;; Initialization
;;; ============================================================
;;;
;;; Uncomment the line below to start the interactive shell:
;;; (fortune-shell)
;;;
;;; Or call (display-cookie (random-fortune)) to get a single fortune!
