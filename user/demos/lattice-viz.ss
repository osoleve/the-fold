;;; user/demos/lattice-viz.ss --- Visualize The Fold's skill lattice
;;;
;;; This demo renders the lattice dependency graph using force-directed layout.
;;; Watch the nodes naturally organize themselves - tier 0 foundations spreading out,
;;; higher tiers clustering around their dependencies.

(load "lattice/data/graph/graph-render.ss")
(load "lattice/meta/meta.ss")

(display "=======================================================\n")
(display "  THE FOLD - Skill Lattice Visualization\n")
(display "=======================================================\n\n")

;;; Initialize the knowledge graph
(display "Initializing lattice metadata...\n")
(kg-ensure!)

;;; ============================================================
;;; Build graph from lattice metadata
;;; ============================================================

(define (build-lattice-graph)
  (doc 'description "Extract graph structure from skill lattice")
  (let* ([skills (kg-skills)]
         ;; Build edges from dependencies
         [edges (apply append
                       (map (lambda (skill)
                              (map (lambda (dep) (cons skill dep))
                                   (kg-deps skill)))
                            skills))])
    (make-layout-graph skills edges)))

;;; ============================================================
;;; Label and tier functions for rendering
;;; ============================================================

(define (skill-label skill)
  (let* ([name (symbol->string skill)]
         [len (string-length name)])
    ;; Truncate long names
    (if (> len 8)
        (substring name 0 8)
        name)))

(define (skill-tier skill)
  (let ([data (kg-skill-data skill)])
    (if data
        (let ([t (assq 'tier data)])
          (if t (cdr t) 0))
        0)))

;;; ============================================================
;;; Configuration
;;; ============================================================

(define width 120)
(define height 40)
(define iterations 80)

;;; ============================================================
;;; Generate visualization
;;; ============================================================

(display "Building lattice graph...\n")
(define lattice-graph (build-lattice-graph))
(display (format "  ~a skills, ~a dependencies\n"
                 (length (graph-nodes lattice-graph))
                 (length (graph-edges lattice-graph))))

(display "\nRunning force-directed layout...\n")
(display (format "  ~a iterations\n" iterations))
(flush-output-port (current-output-port))

(define laid-out (run-layout lattice-graph iterations))

(display "\nRendering...\n\n")

;;; Static render
(display (render-graph-colored laid-out width height skill-label skill-tier))

(display "\n")
(display "Legend: ")
(display "\x1b;[38;5;51mcyan=tier0 \x1b;[0m")
(display "\x1b;[38;5;46mgreen=tier1 \x1b;[0m")
(display "\x1b;[38;5;226myellow=tier2 \x1b;[0m")
(display "\x1b;[38;5;208morange=tier3+\x1b;[0m\n")

;;; ============================================================
;;; Animation mode (uncomment to use)
;;; ============================================================

;; (display "\nGenerating animation frames...\n")
;; (define frames (animate-layout-colored
;;                 lattice-graph width height
;;                 skill-label skill-tier 60))
;;
;; (display "\nPlaying animation (Ctrl+C to stop)...\n")
;; (for-each (lambda (frame)
;;             (display "\x1b;[2J\x1b;[H")  ; clear screen
;;             (display frame)
;;             (flush-output-port (current-output-port))
;;             ;; Simple delay
;;             (let ([start (current-time)])
;;               (let loop ()
;;                 (when (< (- (current-time) start) 0.1)
;;                   (loop)))))
;;           frames)
