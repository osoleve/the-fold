;;; playpen/creations/demo-showcase.ss — Game Tester's Showcase
;;;
;;; Interactive demonstration of The Fold's games and experiences.
;;; Loads multiple games and runs some cool demos!
;;;
;;; Run with: scheme --script playpen/creations/demo-showcase.ss

(load "shell/layout.ss")

(display "╔══════════════════════════════════════════════════════════════════════╗\n")
(display "║                   THE FOLD GAMES SHOWCASE                           ║\n")
(display "║                  Presented by Game Tester                           ║\n")
(display "╚══════════════════════════════════════════════════════════════════════╝\n\n")

;;; ============================================================
;;; Demo 1: Loading and Showing What's Available
;;; ============================================================

(display "DEMO 1: Loading Available Games\n")
(display "════════════════════════════════════════════════════════════════════════\n\n")

(display "Loading Zen Garden module...\n")
(load "playpen/zen-garden.ss")
(newline)

(display "\nLoading Color Palette Explorer module...\n")
(load "playpen/creations/color-palette-explorer.ss")
(newline)

;;; ============================================================
;;; Demo 2: Generate a Beautiful Zen Garden
;;; ============================================================

(display "\n\nDEMO 2: Generating a Zen Garden\n")
(display "════════════════════════════════════════════════════════════════════════\n\n")

(garden 42)

;;; ============================================================
;;; Demo 3: Explore Color Palettes with Gradients
;;; ============================================================

(display "\nDEMO 3: Color Palette Gradients\n")
(display "════════════════════════════════════════════════════════════════════════\n\n")

(explore-palette-gradient 1337)

;;; ============================================================
;;; Demo 4: ASCII Art Mosaics
;;; ============================================================

(display "DEMO 4: Mosaic Art Generation\n")
(display "════════════════════════════════════════════════════════════════════════\n\n")

(generate-mosaic 777 1)

;;; ============================================================
;;; Demo 5: Multiple Zen Gardens
;;; ============================================================

(display "\nDEMO 5: Series of Zen Gardens\n")
(display "════════════════════════════════════════════════════════════════════════\n\n")

(display "Generating 2 gardens in sequence...\n\n")
(garden-series 2)

;;; ============================================================
;;; Summary
;;; ============================================================

(display "\n╔══════════════════════════════════════════════════════════════════════╗\n")
(display "║                        SHOWCASE COMPLETE                            ║\n")
(display "╚══════════════════════════════════════════════════════════════════════╝\n\n")

(display "You've just explored:\n\n")
(display "  1. Zen Garden Generation — Procedural meditation gardens\n")
(display "  2. Color Palettes — Beautiful ASCII art gradients\n")
(display "  3. Mosaic Art — Random colorful patterns\n")
(display "  4. Multiple Gardens — Variations from different seeds\n\n")

(display "To explore more interactive experiences:\n\n")
(display "  - Check /home/user/the-fold/playpen/rpg/ for the full RPG engine\n")
(display "  - See /home/user/the-fold/playpen/creations/ for all games\n")
(display "  - Read GAME-TESTER-REPORT.md for a complete guide\n\n")

(display "Remember:\n")
(display "  (garden)                       — Random Zen garden\n")
(display "  (garden 42)                    — Garden from seed\n")
(display "  (explore-palette-gradient 42)  — Gradient explorer\n")
(display "  (generate-mosaic 123 1)        — Mosaic art\n\n")

(display "Have fun exploring The Fold! 🎮✨\n\n")
