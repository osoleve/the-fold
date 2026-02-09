;;; user/demos/lattice-emerge.ss --- Animated lattice visualization
;;;
;;; Watch The Fold's skill lattice - a hierarchical view showing how
;;; skills depend on each other, organized by tier (foundational → advanced).
;;;
;;; This is the demo file for the demoscene agent to convert to GIF.

(load "lattice/data/graph/graph-render.ss")
(load "lattice/meta/meta.ss")

(display "=======================================================\n")
(display "  LATTICE STRUCTURE - The Fold Visualizing Itself\n")
(display "=======================================================\n\n")

;;; Initialize
(display "Initializing lattice...\n")
(kg-ensure!)

;;; ============================================================
;;; Configuration
;;; ============================================================

(define width 140)
(define height 50)
(define frame-delay 0.1)  ; 100ms between frames

;;; ============================================================
;;; Build graph from lattice
;;; ============================================================

(define (skill-label skill)
  (let* ([name (symbol->string skill)]
         [len (string-length name)])
    (if (> len 8)
        (substring name 0 8)
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
;;; Generate hierarchical layout (clean tier-based view)
;;; ============================================================

(display "Generating hierarchical layout...\n")
(flush-output-port (current-output-port))

(define hier-graph (hierarchical-layout skills edges skill-tier))
(define main-frame (render-graph-colored hier-graph width height skill-label skill-tier))

;;; ============================================================
;;; Title frame
;;; ============================================================

(define title-frame
  (string-append
   "\x1b;[2J\x1b;[H"
   "\n\n\n"
   "                        \x1b;[38;5;51m████████╗██╗  ██╗███████╗    ███████╗ ██████╗ ██╗     ██████╗\x1b;[0m\n"
   "                        \x1b;[38;5;51m╚══██╔══╝██║  ██║██╔════╝    ██╔════╝██╔═══██╗██║     ██╔══██╗\x1b;[0m\n"
   "                        \x1b;[38;5;46m   ██║   ███████║█████╗      █████╗  ██║   ██║██║     ██║  ██║\x1b;[0m\n"
   "                        \x1b;[38;5;46m   ██║   ██╔══██║██╔══╝      ██╔══╝  ██║   ██║██║     ██║  ██║\x1b;[0m\n"
   "                        \x1b;[38;5;226m   ██║   ██║  ██║███████╗    ██║     ╚██████╔╝███████╗██████╔╝\x1b;[0m\n"
   "                        \x1b;[38;5;226m   ╚═╝   ╚═╝  ╚═╝╚══════╝    ╚═╝      ╚═════╝ ╚══════╝╚═════╝\x1b;[0m\n"
   "\n"
   "                                    \x1b;[38;5;240mSkill Lattice Structure\x1b;[0m\n"
   "\n"
   "                      \x1b;[38;5;51mTier 0\x1b;[0m ──────────► \x1b;[38;5;46mTier 1\x1b;[0m ──────────► \x1b;[38;5;226mTier 2\x1b;[0m\n"
   "                    \x1b;[38;5;51mFoundational\x1b;[0m         \x1b;[38;5;46mIntermediate\x1b;[0m         \x1b;[38;5;226mAdvanced\x1b;[0m\n"
   "\n\n"))

;;; Build frame sequence
(define full-frames
  (append (make-list 25 title-frame)  ; Title for ~2.5s
          (make-list 60 main-frame))) ; Hold main ~6s

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
(display "\nThe Fold has visualized itself.\n")
(display "\x1b;[38;5;51mTier 0\x1b;[0m: data, linalg, algebra, numeric, number-theory\n")
(display "\x1b;[38;5;46mTier 1\x1b;[0m: fp, autodiff, geometry, statistics, meta, ...\n")
(display "\x1b;[38;5;226mTier 2\x1b;[0m: physics/diff, physics/classical, sim, pipeline\n")
