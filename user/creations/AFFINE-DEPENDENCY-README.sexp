;;; AFFINE-DEPENDENCY-README.sexp
;;;
;;; Documentation for the affine arithmetic dependency problem demo

(demo
  (title "The Dependency Problem")
  (subtitle "Why Affine Arithmetic is Necessary")
  (created "2026-01-17")
  (author "DEMOSCENE agent")
  (duration "6.0 seconds")
  (resolution "80x34 characters (640x476 pixels)")
  (fps 60)
  (frames 360)
  (filesize "26KB")

  (purpose
    "Educational visualization showing why interval arithmetic fails at the
     dependency problem and how affine arithmetic solves it through correlation
     tracking with noise symbols.")

  (key-concept
    "When computing x - x where x = [1, 2]:
     - Interval arithmetic: treats the two x's as independent, gets [-1, 1] (WRONG)
     - Affine arithmetic: tracks correlation via noise symbols, gets 0 (CORRECT)")

  (phases
    (phase-1 "Title" (frames 0 89)
             "Hook with main title and subtitle, sparkle effects")
    (phase-2 "Setup" (frames 90 149)
             "Introduce the uncertain value x = [1, 2], show visual interval")
    (phase-3 "Failure" (frames 150 209)
             "Demonstrate interval arithmetic failure: x - x = [-1, 1]")
    (phase-4 "Solution" (frames 210 269)
             "Introduce affine form x̂ = 1.5 + 0.5*ε₀, show cancellation to 0")
    (phase-5 "Comparison" (frames 270 329)
             "Side-by-side: interval (width 2.0) vs affine (width 0.0)")
    (phase-6 "Outro" (frames 330 359)
             "Sign-off with module reference, final sparkles"))

  (technical-details
    (format "GIF89a, optimized palette (32 colors), Bayer dithering")
    (font "8x14 pixel bitmap font (92 glyphs)")
    (colors
      (background (24 24 32))   ; Dark blue
      (foreground (220 220 200))) ; Warm white
    (animation
      (easing "ease-out-cubic, ease-in-out-cubic")
      (effects "character-by-character text reveal, sparkles, visual intervals")))

  (mathematical-background
    "Affine arithmetic (AA) extends interval arithmetic (IA) by representing
     uncertain quantities in first-order form:

       x̂ = x₀ + Σᵢ xᵢεᵢ

     where:
     - x₀ is the center value
     - xᵢ are coefficients (partial deviations)
     - εᵢ are noise symbols in [-1, 1]

     Key insight: identical noise symbols represent correlated uncertainty.
     When x̂ - x̂ is computed, all noise terms cancel exactly, yielding 0.

     This was impossible with interval arithmetic, which has no way to track
     where uncertainty came from.")

  (references
    "Stolfi, J. & de Figueiredo, L. H. (1997). Self-Validated Numerical Methods
     and Applications. Brazilian Mathematics Colloquium monograph.

     Implementation: lattice/numeric/affine.ss")

  (files
    (script "user/creations/affine-dependency-demo.ss")
    (output "user/creations/affine-dependency.gif")
    (readme "user/creations/AFFINE-DEPENDENCY-README.sexp"))

  (aesthetic-notes
    "- Monochrome terminal palette (warm white on dark blue)
     - Sparkles used sparingly for emphasis, not decoration
     - Text reveals timed to reading speed (~0.3-0.5s)
     - Visual interval bars show width intuitively
     - Side-by-side comparison drives home the dramatic difference
     - Hold frames long enough to read (~1s per phase minimum)
     - Seamless loop potential (though ends with credits)")

  (usage
    (generate "scheme --script user/creations/affine-dependency-demo.ss")
    (view "Open user/creations/affine-dependency.gif in any image viewer")
    (share "Perfect for documentation, presentations, social media"))

  (demoscene-principles
    "- Constraint breeds creativity (80x34 terminal, ASCII only)
     - Every character must earn its place
     - Easing functions are vocabulary (smooth, snappy, playful)
     - Educational content can be beautiful
     - The terminal is humanity's most honest canvas"))
