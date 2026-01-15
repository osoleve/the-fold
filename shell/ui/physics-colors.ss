;;; shell/ui/physics-colors.ss — Colored Physics ASCII Rendering
;;;
;;; Adds ANSI color to the physics ASCII renderer output.
;;; Works by post-processing rendered frames with color mappings.
;;;
;;; This is Shell code: handles display formatting with ANSI.
;;;
;;; Usage:
;;;   (load "shell/ui/physics-colors.ss")
;;;   (define world (make-world ...))
;;;   (physics-display-colored world)   ; Display with colors
;;;
;;; Dependencies:
;;;   - shell/ui/ansi.ss
;;;   - lattice/physics/classical/ascii-renderer.ss

;; Load physics renderer first (it loads ascii-video.ss which has conflicting names)
(load "lattice/physics/classical/ascii-renderer.ss")
;; Then load our ansi module (overwrites the conflict with correct definitions)
(load "shell/ui/ansi.ss")

;;; Local ANSI reset string (to avoid conflicts with ascii-video.ss)
(define *physics-ansi-reset* "\x1B;[0m")

;;; ====
;;; Color Palette for Physics Elements
;;; ====

;;; Physics element colors
(define physics-static-color color-gray)         ; Static bodies
(define physics-dynamic-color color-cyan)        ; Dynamic bodies
(define physics-velocity-color color-green)      ; Velocity vectors
(define physics-contact-color color-red)         ; Contact points
(define physics-constraint-color color-yellow)   ; Constraints
(define physics-aabb-color color-magenta)        ; Bounding boxes

;;; Character → Color mapping for rendered output
(define (char->physics-color char)
  (case char
    [(#\#) physics-static-color]      ; Static bodies
    [(#\O #\o #\@) physics-dynamic-color]   ; Dynamic bodies
    [(#\> #\< #\^ #\v #\\ #\/ #\.) physics-velocity-color] ; Velocity
    [(#\* #\:) physics-contact-color]   ; Contacts
    [(#\+ #\- #\~ #\=) physics-constraint-color] ; Constraints
    [(#\|) color-dark-gray]             ; AABB borders
    [else #f]))  ; No color (use default)

;;; ====
;;; Colored Frame Rendering
;;; ====

;;; colorize-char : Char → String
;;; Apply ANSI color to a character based on its type.
(define (colorize-char char)
  (let ([color (char->physics-color char)])
    (if color
        (string-append (style->ansi (style-fg color))
                       (string char)
                       *physics-ansi-reset*)
        (string char))))

;;; colorize-row : String → String
;;; Apply colors to each character in a row.
(define (colorize-row row)
  (let ([len (string-length row)])
    (let loop ([i 0] [acc '()])
      (if (>= i len)
          (apply string-append (reverse acc))
          (loop (+ i 1)
                (cons (colorize-char (string-ref row i)) acc))))))

;;; frame-render-colored : Frame → String
;;; Render frame to string with ANSI colors.
(define (frame-render-colored frame)
  (let ([h (frame-height frame)])
    (let loop ([y 0] [acc '()])
      (if (>= y h)
          (apply string-append (reverse acc))
          (loop (+ y 1)
                (cons (string-append (colorize-row (vector-ref frame y)) "\n")
                      acc))))))

;;; ====
;;; High-Level API
;;; ====

;;; physics-render-colored : World × Int × Int → String
;;; Render physics world to colored ASCII string.
(define (physics-render-colored world width height)
  (let* ([config (make-render-config width height 3.0 (/ width 2) (/ height 4))]
         [renderer (make-world-renderer config (full-debug-options) (default-render-style))]
         [frame (make-frame width height #\space)])
    (render-world! frame world renderer #f)
    (frame-render-colored frame)))

;;; physics-display-colored : World → Void
;;; Display physics world with colors (default 80x30).
(define (physics-display-colored world)
  (physics-display-colored-size world 80 30))

;;; physics-display-colored-size : World × Int × Int → Void
;;; Display physics world with colors at specified size.
(define (physics-display-colored-size world width height)
  (display (physics-render-colored world width height)))

;;; physics-display-colored-debug : World × DebugOptions → Void
;;; Display with specific debug options.
(define (physics-display-colored-debug world debug-opts)
  (let* ([width 80]
         [height 30]
         [config (make-render-config width height 3.0 (/ width 2) (/ height 4))]
         [renderer (make-world-renderer config debug-opts (default-render-style))]
         [frame (make-frame width height #\space)])
    (render-world! frame world renderer #f)
    (display (frame-render-colored frame))))

;;; ====
;;; Styled Render Configuration
;;; ====

;;; make-colored-render-style : → RenderStyle
;;; Style optimized for colored display (uses simpler characters).
(define (make-colored-render-style)
  ;; With colors, we can use simpler characters since color conveys type
  (make-render-style #\o #\# #\- #\| #\+ #\> #\*))

;;; ====
;;; Legend Display
;;; ====

;;; display-physics-legend : → Void
;;; Show color legend for physics rendering.
(define (display-physics-legend)
  (display (colored-text (make-style color-cyan #f #t #f #f #f) "Physics Color Legend:"))
  (newline)
  (display "  ")
  (display (colored-text (style-fg physics-static-color) "# "))
  (display "Static bodies")
  (newline)
  (display "  ")
  (display (colored-text (style-fg physics-dynamic-color) "O "))
  (display "Dynamic bodies")
  (newline)
  (display "  ")
  (display (colored-text (style-fg physics-velocity-color) "> "))
  (display "Velocity vectors")
  (newline)
  (display "  ")
  (display (colored-text (style-fg physics-contact-color) "* "))
  (display "Contact points")
  (newline)
  (display "  ")
  (display (colored-text (style-fg physics-constraint-color) "+ "))
  (display "Constraints")
  (newline))

;;; ====
;;; Module Load Message
;;; ====

(display (colored-text (style-fg color-cyan) "physics-colors.ss loaded."))
(newline)
(display "  (physics-display-colored world)   - Display world with colors")
(newline)
(display "  (physics-render-colored world w h) - Render to colored string")
(newline)
(display "  (display-physics-legend)          - Show color key")
(newline)
