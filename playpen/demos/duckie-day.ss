;;; playpen/demos/duckie-day.ss — A Day in the Life of DUCKIE
;;;
;;; A simple simulation showing DUCKIE's daily routine.
;;; Demonstrates the persistence layer and state management.
;;;
;;; Run from project root:
;;;   scheme --script playpen/demos/duckie-day.ss

;; Load dependencies
(source-directories (cons "fabric/stitches" (cons "thimble" (cons "playpen" (source-directories)))))
(load "fabric/stitches/prelude.ss")
(load "fabric/stitches/block.ss")
(load "fabric/stitches/sha256.ss")
(load "fabric/stitches/cas.ss")
(load "playpen/duckie.ss")

;;; ============================================================
;;; Display Utilities
;;; ============================================================

(define (clear-line)
  (display "\r                                                            \r"))

(define (show-duckie d)
  (let ([name (duckie-name d)]
        [mood (duckie-mood d)]
        [energy (duckie-energy d)]
        [age (duckie-age d)])
    (display "\n")
    (display "  ╭───────────────────────────────────────╮\n")
    (display (format "  │ ~a~a│\n" name
                     (make-string (max 0 (- 37 (string-length name))) #\space)))
    (display "  ├───────────────────────────────────────┤\n")

    ;; ASCII art based on mood
    (case mood
      [(happy)
       (display "  │           __                         │\n")
       (display "  │       ___( o)>  quack!               │\n")
       (display "  │       \\ <_. )                        │\n")
       (display "  │        `---'                         │\n")]
      [(playful)
       (display "  │           __                         │\n")
       (display "  │       ___( ^)>  wheee!               │\n")
       (display "  │       \\ <_. )~                       │\n")
       (display "  │        `---'                         │\n")]
      [(sleepy)
       (display "  │           __                         │\n")
       (display "  │       ___( -)>  zzz...               │\n")
       (display "  │       \\ <_. )                        │\n")
       (display "  │        `---'                         │\n")]
      [(lonely curious)
       (display "  │           __                         │\n")
       (display "  │       ___( o)>  ?                    │\n")
       (display "  │       \\ <_. )                        │\n")
       (display "  │        `---'                         │\n")]
      [else
       (display "  │           __                         │\n")
       (display "  │       ___( o)>                       │\n")
       (display "  │       \\ <_. )                        │\n")
       (display "  │        `---'                         │\n")])

    (display "  ├───────────────────────────────────────┤\n")
    (display (format "  │ Mood:   ~a~a│\n" mood
                     (make-string (max 0 (- 29 (string-length (symbol->string mood)))) #\space)))

    ;; Energy bar
    (let* ([filled (quotient energy 5)]
           [empty (- 20 filled)]
           [bar (string-append (make-string filled #\█) (make-string empty #\░))])
      (display (format "  │ Energy: ~a~a│\n" bar
                       (make-string (max 0 (- 9 (string-length (number->string energy)))) #\space))))

    (display (format "  │ Age:    ~a ticks~a│\n" age
                     (make-string (max 0 (- 23 (string-length (number->string age)))) #\space)))
    (display "  ╰───────────────────────────────────────╯\n")))

(define (narrate msg)
  (display (format "\n  → ~a\n" msg)))

;;; ============================================================
;;; Simulation
;;; ============================================================

(define (simulate-day name)
  (display "\n")
  (display "╔═══════════════════════════════════════════════════════════════╗\n")
  (display "║           A DAY IN THE LIFE OF DUCKIE                         ║\n")
  (display "╚═══════════════════════════════════════════════════════════════╝\n")

  ;; Birth
  (narrate (format "~a hatches into the world!" name))
  (let* ([d (make-duckie name)]
         [hash (store! (duckie->block d))])
    (pin! hash)
    (show-duckie d)

    ;; Morning - curious exploration
    (narrate "Morning: The sun rises. Time to explore!")
    (let ([d2 (duckie-set-mood (duckie-age-once d) 'curious)])
      (show-duckie d2)

      ;; Play time
      (narrate "Play time! Splashing in the pond.")
      (let ([d3 (duckie-set-mood
                  (duckie-drain-energy (duckie-age-once d2) 20)
                  'playful)])
        (show-duckie d3)

        ;; Lunch - feeding
        (narrate "Lunch: Nom nom nom...")
        (let ([d4 (duckie-set-mood
                    (duckie-restore-energy (duckie-age-once d3) 40)
                    'happy)])
          (show-duckie d4)

          ;; Afternoon nap
          (narrate "Afternoon: Time for a nap.")
          (let ([d5 (duckie-set-mood
                      (duckie-restore-energy (duckie-age-once d4) 20)
                      'sleepy)])
            (show-duckie d5)

            ;; Evening - waiting for friend
            (narrate "Evening: Where did everyone go?")
            (let ([d6 (duckie-set-mood
                        (duckie-drain-energy (duckie-age-once d5) 10)
                        'lonely)])
              (show-duckie d6)

              ;; Friend arrives!
              (narrate "A friend arrives! Petting time!")
              (let ([d7 (duckie-set-mood
                          (duckie-drain-energy (duckie-age-once d6) 5)
                          'happy)])
                (show-duckie d7)

                ;; Save final state
                (let ([final-hash (store! (duckie->block d7))])
                  (pin! final-hash)

                  (display "\n")
                  (display "┌───────────────────────────────────────────────────────────────┐\n")
                  (display "│                         END OF DAY                            │\n")
                  (display "├───────────────────────────────────────────────────────────────┤\n")
                  (display (format "│ Soul saved to CAS: ~a...  │\n"
                                   (substring (hash->hex final-hash) 0 24)))
                  (display (format "│ Total interactions: ~a~a│\n"
                                   (duckie-age d7)
                                   (make-string (max 0 (- 43 (string-length (number->string (duckie-age d7))))) #\space)))
                  (display (format "│ Final energy: ~a/100~a│\n"
                                   (duckie-energy d7)
                                   (make-string (max 0 (- 41 (string-length (number->string (duckie-energy d7))))) #\space)))
                  (display "│                                                               │\n")
                  (display "│ \"Same content = same hash. DUCKIE is immortal.\"               │\n")
                  (display "└───────────────────────────────────────────────────────────────┘\n")

                  d7)))))))))

;;; ============================================================
;;; Main
;;; ============================================================

(simulate-day "Quackers")
