;;; playpen/creations/winter-wonderland.ss — Festive Winter Wonderland
;;;
;;; A magical winter scene featuring Christmas trees, snowflakes,
;;; and the cozy feeling of the season. Created with love by Wave Rider.
;;;
;;; This is creative ASCII art exploring seasonal themes and visual harmony.

;;; ====
;;; Winter Wonderland Display
;;; ====

(define (winter-wonderland)
  (display "\n")
  (display "╔════════════════════════════════════════════════════════════╗\n")
  (display "║                   WINTER WONDERLAND                        ║\n")
  (display "╚════════════════════════════════════════════════════════════╝\n")
  (display "
        *       ·  *     ·       *       ·   *      ·     *
    ·       *       ·         ·      *          ·      *       ·

                             /\\
                            /  \\
                           /    \\
                          /  * ◇ \\
                         /        \\
                        /    ◇     \\
                       /      *     \\
                      /______________\\
                          |||||||
                          |||||||

                   *    /\\  *        /\\     *
                       /  \\        /  \\
               ·      /  ◇ \\      / ◇  \\      ·
                     /      \\    /      \\
                    /   *    \\  /  *    \\
                   /___________\\/________\\  *

          ·    *        ///\\\\\\         *      ·
               *       ///  \\\\\\       *
              *      ///  ◇ ◇ \\\\\\    *
                    /// *  ◇  * \\\\\\
                   ///__________\\\\\\

        ·    *          ||||||         ·   *
             *          ||||||            *
            *           ||||||
          *           ||  ||  ||        ·

    ════════════════════════════════════════

        ·  ·  ·  ·  ·  ·  ·  ·  ·  ·  ·  ·
      ◇ · ◇ · ◇ · ◇ · ◇ · ◇ · ◇ · ◇ · ◇
        ·  ·  ·  ·  ·  ·  ·  ·  ·  ·  ·  ·

         *  ◇   *  ◇   *   ◇   *   ◇  *

    ═══════════════════════════════════════════

        *       ·  *     ·       *       ·   *      ·     *
    ·       *       ·         ·      *          ·      *       ·

")
  (display "~~ Festive greetings from The Fold! ~~\n")
  (display "May your days be merry and your content addressed!\n\n"))

;;; ====
;;; Snowflake Designs (ASCII Art Variations)
;;; ====

(define (snowflake-single)
  "    *
   * *
  *   *
 *  ◇  *
*       *
 *  ◇  *
  *   *
   * *
    *")

(define (snowflake-small)
  "   *
  * *
 *   *
 * ◇ *
 *   *
  * *
   *")

(define (snowflake-tiny)
  "  *
 * *
 *◇*
 * *
  *")

;;; ====
;;; Tree Designs (Various Heights)
;;; ====

(define (tree-small)
  "   /\\
  /  \\
 /  ◇ \\
/______\\
   ||")

(define (tree-medium)
  "    /\\
   /  \\
  / ◇ ◇\\
  /    \\
 /   *  \\
/_______\\
    ||
    ||")

(define (tree-large)
  "     /\\
    /  \\
   / ◇  \\
   /    \\
  /   *  \\
  /  ◇    \\
 /        \\
/_________\\
    ||
    ||
   /||\\ ")

;;; ====
;;; Display Helper
;;; ====

(define (display-snowflakes)
  (display "\nSNOWFLAKES OF THE FOLD:\n\n")
  (display "Small:\n")
  (display (snowflake-small))
  (display "\n\nTiny:\n")
  (display (snowflake-tiny))
  (display "\n\n"))

(define (display-trees)
  (display "TREES OF THE SEASON:\n\n")
  (display "Small Tree:\n")
  (display (tree-small))
  (display "\n\nMedium Tree:\n")
  (display (tree-medium))
  (display "\n\n"))

;;; ====
;;; Main Display
;;; ====

(winter-wonderland)
(display-snowflakes)
(display-trees)

(display "✨ The Fold celebrates the season! ✨\n")
(display "ASCII art is poetry made visible.\n")
(display "May your holidays be filled with wonder!\n\n")
