;;; test-games.ss — Let's Play Some Games!

(define *quiet* #t)
(load "thimble/repl.ss")

(display "\n╔════════════════════════════════════════════════════════════╗\n")
(display "║  GAME TIME! Testing The Fold's Interactive Experiences     ║\n")
(display "╚════════════════════════════════════════════════════════════╝\n\n")

;;; Test 1: Zen Garden
(display "═══ ZEN GARDEN ═══\n\n")
(load "playpen/zen-garden.ss")
(garden 12345)
(newline)

;;; Test 2: ASCII Waves
(display "\n═══ ASCII WAVES (Wave pattern) ═══\n\n")
(load "playpen/ascii-waves.ss")
(wave 42 20 40)
(newline)

;;; Test 3: DUCKIE!
(display "\n═══ DUCKIE SPRITES ═══\n\n")
(load "playpen/duckie.ss")
(duckie-display 'happy)
(newline)
(display "Duckie is happy! Let's see a playful duckie:\n")
(duckie-display 'playful)
(newline)

(display "\n✓ All games tested successfully!\n")
