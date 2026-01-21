;;; user/demos/lattice-emerge.ss --- Animated lattice emergence
;;;
;;; Watch The Fold's skill lattice emerge from chaos into order.
;;; Nodes start randomly positioned, then force-directed layout
;;; pulls connected skills together while pushing unrelated ones apart.
;;;
;;; This is the demo file for the demoscene agent to convert to GIF.

(load "lattice/data/graph-render.ss")
(load "lattice/meta/meta.ss")

(display "=======================================================\n")
(display "  LATTICE EMERGENCE - The Fold Visualizing Itself\n")
(display "=======================================================\n\n")

;;; Initialize
(display "Initializing lattice...\n")
(kg-ensure!)

;;; ============================================================
;;; Configuration
;;; ============================================================

(define width 100)
(define height 35)
(define n-frames 60)   ; 60 frames for smooth animation
(define frame-delay 0.08)  ; 80ms between frames (12.5 fps)

;;; ============================================================
;;; Build graph from lattice
;;; ============================================================

(define (skill-label skill)
  (let* ([name (symbol->string skill)]
         [len (string-length name)])
    (if (> len 7)
        (substring name 0 7)
        name)))

(define (skill-tier skill)
  (let ([data (kg-skill-data skill)])
    (if data
        (let ([t (assq 'tier data)])
          (if t (cdr t) 0))
        0)))

(define skills (kg-skills))
(define edges (apply append
                     (map (lambda (skill)
                            (map (lambda (dep) (cons skill dep))
                                 (kg-deps skill)))
                          skills)))

(display (format "  ~a skills, ~a dependencies\n\n"
                 (length skills) (length edges)))

;;; ============================================================
;;; Generate animation frames
;;; ============================================================

(display "Generating emergence animation...\n")
(display (format "  ~a frames at ~ax~a\n" n-frames width height))
(flush-output-port (current-output-port))

;; Create initial random layout
(define initial-graph (make-layout-graph skills edges))

;; Generate frames by iterating layout
(define (generate-frames graph n-frames)
  (let loop ([g graph] [i 0] [frames '()])
    (if (>= i n-frames)
        (reverse frames)
        (begin
          (when (= (mod i 10) 0)
            (display ".")
            (flush-output-port (current-output-port)))
          (let* ([normalized (normalize-layout g width height 4)]
                 [frame (render-graph-colored normalized width height
                                              skill-label skill-tier)]
                 ;; Multiple iterations per frame for faster convergence
                 [next-g (run-layout g 3)])
            (loop next-g (+ i 1) (cons frame frames)))))))

(define frames (generate-frames initial-graph n-frames))
(display " done!\n\n")

;;; ============================================================
;;; Add title frame
;;; ============================================================

(define title-frame
  (string-append
   "\x1b;[2J\x1b;[H"
   "\n\n"
   "                        \x1b;[38;5;51m████████╗██╗  ██╗███████╗    ███████╗ ██████╗ ██╗     ██████╗\x1b;[0m\n"
   "                        \x1b;[38;5;51m╚══██╔══╝██║  ██║██╔════╝    ██╔════╝██╔═══██╗██║     ██╔══██╗\x1b;[0m\n"
   "                        \x1b;[38;5;46m   ██║   ███████║█████╗      █████╗  ██║   ██║██║     ██║  ██║\x1b;[0m\n"
   "                        \x1b;[38;5;46m   ██║   ██╔══██║██╔══╝      ██╔══╝  ██║   ██║██║     ██║  ██║\x1b;[0m\n"
   "                        \x1b;[38;5;226m   ██║   ██║  ██║███████╗    ██║     ╚██████╔╝███████╗██████╔╝\x1b;[0m\n"
   "                        \x1b;[38;5;226m   ╚═╝   ╚═╝  ╚═╝╚══════╝    ╚═╝      ╚═════╝ ╚══════╝╚═════╝\x1b;[0m\n"
   "\n"
   "                                    \x1b;[38;5;240mSkill Lattice Emergence\x1b;[0m\n"
   "\n\n"))

;;; Add title at start, hold final frame
(define full-frames
  (append (list title-frame title-frame title-frame)  ; Title for ~0.3s
          frames
          (make-list 20 (car (reverse frames)))))     ; Hold final ~2s

;;; ============================================================
;;; Play animation
;;; ============================================================

(display "Animation ready!\n")
(display (format "  ~a total frames\n\n" (length full-frames)))

(display "Playing animation (Ctrl+C to stop)...\n\n")
(for-each (lambda (frame)
            (display "\x1b;[2J\x1b;[H")
            (display frame)
            (flush-output-port (current-output-port))
            (let ([start (current-time)])
              (let loop ()
                (when (< (- (current-time) start) frame-delay)
                  (loop)))))
          full-frames)

(display "\x1b;[2J\x1b;[H")
(display "\nAnimation complete!\n")
(display "The lattice has emerged.\n")
