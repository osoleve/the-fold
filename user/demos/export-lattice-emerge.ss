;;; user/demos/export-lattice-emerge.ss --- Export lattice visualization to GIF
;;;
;;; Generates lattice-emerge.gif showing the hierarchical skill structure

(load "user/creations/ascii-video.ss")
(load "user/creations/ascii-video-export.ss")
(load "lattice/data/graph-render.ss")
(load "lattice/meta/meta.ss")

(display "=======================================================\n")
(display "  EXPORTING LATTICE EMERGE GIF\n")
(display "=======================================================\n\n")

;;; Initialize
(display "Initializing lattice...\n")
(kg-ensure!)

;;; ============================================================
;;; Configuration
;;; ============================================================

(define width 140)
(define height 50)

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
;;; Generate hierarchical layout
;;; ============================================================

(display "Generating hierarchical layout...\n")
(define hier-graph (hierarchical-layout skills edges skill-tier))

;;; ============================================================
;;; Helper: Convert ANSI colored text to frame
;;; ============================================================

(define (strip-ansi str)
  ;; Simple ANSI stripper - removes ESC[...m sequences
  (let loop ([chars (string->list str)] [acc '()])
    (cond
      [(null? chars) (list->string (reverse acc))]
      [(and (char=? (car chars) #\x1b)
            (pair? (cdr chars))
            (char=? (cadr chars) #\[))
       ;; Found ESC[, skip until 'm'
       (let skip ([rest (cddr chars)])
         (if (or (null? rest) (char=? (car rest) #\m))
             (loop (if (null? rest) '() (cdr rest)) acc)
             (skip (cdr rest))))]
      [else (loop (cdr chars) (cons (car chars) acc))])))

(define (text-to-frame text w h)
  (let ([frame (make-frame w h #\space)])
    (let ([lines (string-split text #\newline)])
      (let loop ([lines lines] [y 0])
        (unless (or (null? lines) (>= y h))
          (let* ([line (car lines)]
                 [clean (strip-ansi line)])
            (let ([x-start (quotient (- w (string-length clean)) 2)])
              (when (>= x-start 0)
                (frame-put-string! frame x-start y clean))))
          (loop (cdr lines) (+ y 1)))))
    frame))

;;; ============================================================
;;; Render frames
;;; ============================================================

(display "Rendering frames...\n")

;;; Title frame content
(define title-text
  (string-append
   "\n\n\n"
   "        ████████╗██╗  ██╗███████╗    ███████╗ ██████╗ ██╗     ██████╗\n"
   "        ╚══██╔══╝██║  ██║██╔════╝    ██╔════╝██╔═══██╗██║     ██╔══██╗\n"
   "           ██║   ███████║█████╗      █████╗  ██║   ██║██║     ██║  ██║\n"
   "           ██║   ██╔══██║██╔══╝      ██╔══╝  ██║   ██║██║     ██║  ██║\n"
   "           ██║   ██║  ██║███████╗    ██║     ╚██████╔╝███████╗██████╔╝\n"
   "           ╚═╝   ╚═╝  ╚═╝╚══════╝    ╚═╝      ╚═════╝ ╚══════╝╚═════╝\n"
   "\n"
   "                          Skill Lattice Structure\n"
   "\n"
   "              Tier 0 ──────────► Tier 1 ──────────► Tier 2\n"
   "            Foundational         Intermediate         Advanced\n"))

(define title-frame (text-to-frame title-text width height))

;;; Main graph frame (convert rendered ASCII to frame)
(define main-frame-text (render-graph-colored hier-graph width height skill-label skill-tier))
(define main-frame (text-to-frame main-frame-text width height))

;;; ============================================================
;;; Build video
;;; ============================================================

(display "Building video...\n")
(define video (make-video))

;; Title for 2 seconds (24 frames @ 12fps)
(let loop ([i 0])
  (when (< i 24)
    (video-add-frame! video title-frame)
    (loop (+ i 1))))

;; Main graph for 4 seconds (48 frames @ 12fps)
(let loop ([i 0])
  (when (< i 48)
    (video-add-frame! video main-frame)
    (loop (+ i 1))))

(display (format "  ~a total frames\n" (video-frame-count video)))

;;; ============================================================
;;; Export
;;; ============================================================

(define output-path "user/demos/lattice-emerge.gif")

(display (format "\nExporting to ~a...\n" output-path))
(video->gif video output-path 83)  ; 83ms = ~12fps

(display "\nDone!\n")
(display (format "Lattice visualization saved to: ~a\n" output-path))
(display "\nStructure:\n")
(display "  Tier 0 (cyan): data, linalg, algebra, numeric, number-theory\n")
(display "  Tier 1 (green): fp, autodiff, geometry, statistics, meta, ...\n")
(display "  Tier 2 (yellow): physics/diff, physics/classical, sim, pipeline\n")
