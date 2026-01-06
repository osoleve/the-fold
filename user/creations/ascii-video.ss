;;; user/creations/ascii-video.ss --- ASCII Video Buffer and Player
;;;
;;; A frame buffer system for ASCII animations.
;;; Record frames, play them back, save/load sequences.

(load "core/base/prelude.ss")

;;; ============================================================
;;; Frame Buffer
;;; ============================================================

;;; A frame is a vector of strings (one per row)
(define (make-frame width height fill-char)
  (let ([row (make-string width fill-char)])
       (make-vector height row)))

(define (frame-width frame)
  (if (zero? (vector-length frame))
      0
      (string-length (vector-ref frame 0))))

(define (frame-height frame)
  (vector-length frame))

(define (frame-ref frame x y)
  (if (and (>= x 0) (< x (frame-width frame))
           (>= y 0) (< y (frame-height frame)))
      (string-ref (vector-ref frame y) x)
      #\space))

(define (frame-set! frame x y char)
  (when (and (>= x 0) (< x (frame-width frame))
             (>= y 0) (< y (frame-height frame)))
        (let ([row (string-copy (vector-ref frame y))])
             (string-set! row x char)
             (vector-set! frame y row))))

(define (frame-copy frame)
  (let* ([h (frame-height frame)]
         [new-frame (make-vector h)])
        (do ([y 0 (+ y 1)])
            ((>= y h))
            (vector-set! new-frame y (string-copy (vector-ref frame y))))
        new-frame))

;;; ============================================================
;;; Drawing Primitives
;;; ============================================================

(define (frame-clear! frame char)
  (let ([w (frame-width frame)]
        [h (frame-height frame)])
       (do ([y 0 (+ y 1)])
           ((>= y h))
           (vector-set! frame y (make-string w char)))))

(define (frame-put-string! frame x y str)
  (let ([len (string-length str)])
       (do ([i 0 (+ i 1)])
           ((>= i len))
           (frame-set! frame (+ x i) y (string-ref str i)))))

(define (frame-draw-box! frame x y w h)
  (frame-put-string! frame x y (string-append "+" (make-string (- w 2) #\-) "+"))
  (do ([row 1 (+ row 1)])
      ((>= row (- h 1)))
      (frame-set! frame x (+ y row) #\|)
      (frame-set! frame (+ x w -1) (+ y row) #\|))
  (frame-put-string! frame x (+ y h -1) (string-append "+" (make-string (- w 2) #\-) "+")))

(define (frame-fill-rect! frame x y w h char)
  (do ([row 0 (+ row 1)])
      ((>= row h))
      (do ([col 0 (+ col 1)])
          ((>= col w))
          (frame-set! frame (+ x col) (+ y row) char))))

;;; ============================================================
;;; Video (Sequence of Frames)
;;; ============================================================

(define (make-video)
  (list 'video '()))

(define (video? v)
  (and (pair? v) (eq? (car v) 'video)))

(define (video-frames v)
  (cadr v))

(define (video-frame-count v)
  (length (video-frames v)))

(define (video-add-frame! v frame)
  (set-car! (cdr v) (append (video-frames v) (list (frame-copy frame)))))

(define (video-get-frame v n)
  (list-ref (video-frames v) n))

;;; ============================================================
;;; Rendering
;;; ============================================================

(define (frame-render frame)
  (let ([h (frame-height frame)])
       (do ([y 0 (+ y 1)])
           ((>= y h))
           (display (vector-ref frame y))
           (newline))))

(define (frame-render-to-string frame)
  (let ([h (frame-height frame)]
        [lines '()])
       (do ([y (- h 1) (- y 1)])
           ((< y 0))
           (set! lines (cons (vector-ref frame y) lines)))
       (apply string-append
              (map (lambda (s) (string-append s "\n")) lines))))

;;; ============================================================
;;; ANSI Terminal Control
;;; ============================================================

(define *esc* (string (integer->char 27)))

(define (ansi-clear)
  (display *esc*)
  (display "[2J"))

(define (ansi-home)
  (display *esc*)
  (display "[H"))

(define (ansi-goto x y)
  (display *esc*)
  (display "[")
  (display (+ y 1))
  (display ";")
  (display (+ x 1))
  (display "H"))

(define (ansi-hide-cursor)
  (display *esc*)
  (display "[?25l"))

(define (ansi-show-cursor)
  (display *esc*)
  (display "[?25h"))

(define (ansi-color fg)
  (display *esc*)
  (display "[")
  (display (+ 30 fg))
  (display "m"))

(define (ansi-reset)
  (display *esc*)
  (display "[0m"))

;;; ============================================================
;;; Playback (Terminal Only - uses ANSI codes and blocking sleep)
;;; ============================================================

;;; NOTE: video-play and video-play-loop use busy-wait and ANSI codes.
;;; They only work in interactive terminals, NOT in daemon/Discord contexts.
;;; For non-interactive use, see video-flipbook or video-frame-string.

(define (sleep-ms ms)
  ;; Busy-wait approximation (Chez doesn't have usleep in base)
  ;; WARNING: This blocks! Only use in interactive terminal.
  (let ([end (+ (cpu-time) (* ms 1000))])
       (let loop ()
            (when (< (cpu-time) end)
                  (loop)))))

(define (video-play video frame-delay-ms)
  (ansi-hide-cursor)
  (ansi-clear)
  (let ([count (video-frame-count video)])
       (do ([i 0 (+ i 1)])
           ((>= i count))
           (ansi-home)
           (frame-render (video-get-frame video i))
           (display "\nFrame ")
           (display (+ i 1))
           (display "/")
           (display count)
           (flush-output-port)
           (sleep-ms frame-delay-ms)))
  (ansi-show-cursor)
  (newline))

(define (video-play-loop video frame-delay-ms loops)
  (ansi-hide-cursor)
  (ansi-clear)
  (let ([count (video-frame-count video)])
       (do ([loop 0 (+ loop 1)])
           ((>= loop loops))
           (do ([i 0 (+ i 1)])
               ((>= i count))
               (ansi-home)
               (frame-render (video-get-frame video i))
               (display "\nLoop ")
               (display (+ loop 1))
               (display "/")
               (display loops)
               (display " Frame ")
               (display (+ i 1))
               (display "/")
               (display count)
               (flush-output-port)
               (sleep-ms frame-delay-ms))))
  (ansi-show-cursor)
  (newline))

;;; ============================================================
;;; Non-Blocking Frame Access (Safe for Daemon/Discord)
;;; ============================================================

;;; video-frame-string : Video x Nat -> String
;;; Get a single frame as a string (no ANSI, no blocking).
(define (video-frame-string video n)
  (if (and (>= n 0) (< n (video-frame-count video)))
      (frame-render-to-string (video-get-frame video n))
      ""))

;;; video-show-frame : Video x Nat -> Void
;;; Display a single frame (no ANSI, no blocking).
(define (video-show-frame video n)
  (display (video-frame-string video n)))

;;; video-frame-codeblock : Video x Nat -> String
;;; Get a single frame wrapped in markdown code block (preserves whitespace in Discord).
(define (video-frame-codeblock video n)
  (if (and (>= n 0) (< n (video-frame-count video)))
      (string-append "```\n" (frame-render-to-string (video-get-frame video n)) "```")
      ""))

;;; ============================================================
;;; Static Playback (No ANSI, prints all frames)
;;; ============================================================

(define (video-print video)
  (let ([count (video-frame-count video)])
       (do ([i 0 (+ i 1)])
           ((>= i count))
           (display "=== Frame ")
           (display (+ i 1))
           (display "/")
           (display count)
           (display " ===\n")
           (frame-render (video-get-frame video i))
           (newline))))

(define (video-print-range video start end)
  (do ([i start (+ i 1)])
      ((>= i end))
      (display "=== Frame ")
      (display (+ i 1))
      (display " ===\n")
      (frame-render (video-get-frame video i))
      (newline)))

;;; ============================================================
;;; GIF-style Export (Text)
;;; ============================================================

(define (video->string video)
  (let ([count (video-frame-count video)]
        [parts '()])
       (do ([i 0 (+ i 1)])
           ((>= i count))
           (set! parts (cons (string-append "--- FRAME " (number->string i) " ---\n"
                                            (frame-render-to-string (video-get-frame video i)))
                             parts)))
       (apply string-append (reverse parts))))

;;; ============================================================
;;; Flipbook Display (Show key frames)
;;; ============================================================

(define (video-flipbook video step)
  (let ([count (video-frame-count video)])
       (display "Flipbook (every ")
       (display step)
       (display " frames):\n\n")
       (do ([i 0 (+ i step)])
           ((>= i count))
           (display "Frame ")
           (display i)
           (display ":\n")
           (frame-render (video-get-frame video i))
           (newline))))
