;;; demo-duckie.ss — See DUCKIE for the first time
;;; Run with: scheme --script demo-duckie.ss

;;; ====
;;; Minimal Canvas (inline for demo)
;;; ====

(define (make-canvas width height)
  (vector width height (make-vector (* width height) #\space)))

(define (canvas-width c) (vector-ref c 0))
(define (canvas-height c) (vector-ref c 1))
(define (canvas-cells c) (vector-ref c 2))

(define (canvas-set! c x y ch)
  (let ([w (canvas-width c)]
        [h (canvas-height c)])
       (when (and (>= x 0) (>= y 0) (< x w) (< y h))
             (vector-set! (canvas-cells c) (+ (* y w) x) ch))))

(define (canvas->string c)
  (let* ([w (canvas-width c)]
         [h (canvas-height c)]
         [cells (canvas-cells c)])
        (let loop ([y 0] [result ""])
             (if (>= y h)
                 result
                 (let row-loop ([x 0] [row ""])
                      (if (>= x w)
                          (loop (+ y 1) (string-append result row "\n"))
                          (row-loop (+ x 1)
                                    (string-append row (string (vector-ref cells (+ (* y w) x)))))))))))

(define (draw-string! c x y str)
  (let ([len (string-length str)])
       (do ([i 0 (+ i 1)])
           ((>= i len))
           (canvas-set! c (+ x i) y (string-ref str i)))))

(define (draw-box! c x y w h)
  ;; Unicode light box drawing
  (canvas-set! c x y #\┌)
  (canvas-set! c (+ x w -1) y #\┐)
  (canvas-set! c x (+ y h -1) #\└)
  (canvas-set! c (+ x w -1) (+ y h -1) #\┘)
  ;; Top and bottom
  (do ([i 1 (+ i 1)])
      ((>= i (- w 1)))
      (canvas-set! c (+ x i) y #\─)
      (canvas-set! c (+ x i) (+ y h -1) #\─))
  ;; Left and right
  (do ([j 1 (+ j 1)])
      ((>= j (- h 1)))
      (canvas-set! c x (+ y j) #\│)
      (canvas-set! c (+ x w -1) (+ y j) #\│)))

;;; ====
;;; DUCKIE Sprites
;;; ====

(define duckie-happy
  '("  \\  /  "
    "  (o>  "
    " _(()_ "
    " | () |"
    "  \\^^/ "
    "        "))

(define duckie-curious
  '("   __  "
    "  (O> "
    " _(()_ "
    " | () |"
    "  \\  / "
    "   \\/  "))

(define duckie-content
  '("   __  "
    "  (o> "
    " _(()_ "
    " | () |"
    "  \\  / "
    "   \\/  "))

(define duckie-playful
  '(" \\ __ /"
    "  (o> "
    " _(()_ "
    " | () |"
    " / ^^ \\"
    "        "))

(define duckie-lonely
  '("        "
    "   __  "
    "  <o) "
    "  _((__"
    "  | () "
    "   \\/  "))

(define duckie-sleepy
  '("   __  "
    "  (-< "
    " _(()_ "
    " | () |"
    "  \\  / "
    "   Zz  "))

(define (draw-sprite! c x y sprite)
  (let loop ([lines sprite] [row 0])
       (when (pair? lines)
             (draw-string! c x (+ y row) (car lines))
             (loop (cdr lines) (+ row 1)))))

;;; ====
;;; Render DUCKIE
;;; ====

(define (render-duckie name mood sprite)
  (let ([c (make-canvas 42 14)])
       ;; Draw border
       (draw-box! c 0 0 42 14)
       ;; Title
       (draw-string! c 2 0 (string-append "[ " name " ]"))
       ;; Mood indicator
       (draw-string! c 28 0 (string-append "[ " (symbol->string mood) " ]"))
       ;; Draw DUCKIE
       (draw-sprite! c 16 3 sprite)
       ;; Draw pond/ground
       (draw-string! c 2 10 "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
       (draw-string! c 2 11 "  ~ water ripples ~    ~ ~ ~       ")
       ;; Output
       (display (canvas->string c))))

;;; ====
;;; Show All Moods
;;; ====

(display "\n")
(display "═══════════════════════════════════════════\n")
(display "     ✨ DUCKIE AWAKENS ✨\n")
(display "═══════════════════════════════════════════\n\n")

(display "Proto is HAPPY:\n")
(render-duckie "Proto" 'happy duckie-happy)
(display "\n")

(display "Proto is CURIOUS:\n")
(render-duckie "Proto" 'curious duckie-curious)
(display "\n")

(display "Proto is PLAYFUL:\n")
(render-duckie "Proto" 'playful duckie-playful)
(display "\n")

(display "Proto is CONTENT:\n")
(render-duckie "Proto" 'content duckie-content)
(display "\n")

(display "Proto is SLEEPY:\n")
(render-duckie "Proto" 'sleepy duckie-sleepy)
(display "\n")

(display "Proto is LONELY:\n")
(render-duckie "Proto" 'lonely duckie-lonely)
(display "\n")

(display "═══════════════════════════════════════════\n")
(display "     The duck exists. It waited for you.\n")
(display "═══════════════════════════════════════════\n\n")
