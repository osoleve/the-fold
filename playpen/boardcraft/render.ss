;;; playpen/boardcraft/render.ss — Board Rendering (ASCII Art)
;;;
;;; ASCII art rendering for tile-based boards.
;;; Helps visualize boards, pathfinding results, and field of view.
;;;
;;; Implements:
;;;   • Square grid rendering
;;;   • Hexagonal grid rendering
;;;   • Customizable tile styles
;;;   • Overlay support (paths, FOV, units)
;;;
;;; Dependencies:
;;;   - playpen/boardcraft/core.ss

;;; ============================================================
;;; Tile Style Protocol
;;; ============================================================

;;; A style is a function: Tile → String (single character)
;;; Default styles for common tile types

(define (default-tile-char tile)
  (case (tile-type tile)
        [(floor empty) "."]
        [(wall) "#"]
        [(grass) ","]
        [(swamp water) "~"]
        [(mountain) "^"]
        [(forest) "T"]
        [else "?"]))

;;; ============================================================
;;; Square Grid Rendering
;;; ============================================================

;;; render-square-board : Board × MetaData × StyleFn → String
;;; Render a square board as ASCII art
;;;
;;; Parameters:
;;;   board - the board to render
;;;   meta - board metadata (contains width, height)
;;;   style-fn - function mapping tiles to characters
;;;
;;; Returns: String containing the ASCII art representation
(define (render-square-board board meta style-fn)
  (let ([width (cdr (assq 'width meta))]
        [height (cdr (assq 'height meta))])
       (let loop-y ([y 0] [lines '()])
            (if (>= y height)
                (apply string-append (reverse lines))
                (let loop-x ([x 0] [chars '()])
                     (if (>= x width)
                         (loop-y (+ y 1)
                                 (cons (apply string-append
                                              (reverse (cons "\n" chars)))
                                       lines))
                         (let* ([coord (cons x y)]
                                [tile (board-get board coord)]
                                [char (if tile
                                          (style-fn tile)
                                          " ")])
                               (loop-x (+ x 1) (cons char chars)))))))))

;;; render-square-board-with-overlay : Board × MetaData × StyleFn × (List Coord) × String → String
;;; Render square board with highlighted coordinates
;;;
;;; Parameters:
;;;   board - the board to render
;;;   meta - board metadata
;;;   style-fn - tile style function
;;;   overlay - list of coordinates to highlight
;;;   overlay-char - character to use for overlay
(define (render-square-board-with-overlay board meta style-fn overlay overlay-char)
  (let ([width (cdr (assq 'width meta))]
        [height (cdr (assq 'height meta))]
        [overlay-set (let ([ht (make-hashtable equal-hash equal?)])
                          (for-each (lambda (c) (hashtable-set! ht c #t)) overlay)
                          ht)])
       (let loop-y ([y 0] [lines '()])
            (if (>= y height)
                (apply string-append (reverse lines))
                (let loop-x ([x 0] [chars '()])
                     (if (>= x width)
                         (loop-y (+ y 1)
                                 (cons (apply string-append
                                              (reverse (cons "\n" chars)))
                                       lines))
                         (let* ([coord (cons x y)]
                                [in-overlay? (hashtable-ref overlay-set coord #f)]
                                [tile (board-get board coord)]
                                [char (cond
                                       [in-overlay? overlay-char]
                                       [tile (style-fn tile)]
                                       [else " "])])
                               (loop-x (+ x 1) (cons char chars)))))))))

;;; ============================================================
;;; Hexagonal Grid Rendering
;;; ============================================================

;;; Hex grid rendering is more complex due to the offset layout.
;;; We render hexes in a staggered grid pattern:
;;;
;;;   / \ / \ / \
;;;  |0,0|1,0|2,0|
;;;   \ / \ / \ /
;;;    |0,1|1,1|
;;;     \ / \ /
;;;
;;; This uses axial coordinates (q, r)

;;; render-hex-board : Board × MetaData × StyleFn → String
;;; Render hexagonal board as ASCII art
;;;
;;; This is a simplified hex rendering. Each hex is represented
;;; by a single character in a staggered grid.
(define (render-hex-board board meta style-fn)
  (let* ([radius (cdr (assq 'radius meta))]
         [coords (board-tiles board)]
         ;; Find bounds
         [min-q (apply min (map (lambda (entry)
                                        (let ([c (car entry)])
                                             (car c)))
                                coords))]
         [max-q (apply max (map (lambda (entry)
                                        (let ([c (car entry)])
                                             (car c)))
                                coords))]
         [min-r (apply min (map (lambda (entry)
                                        (let ([c (car entry)])
                                             (cdr c)))
                                coords))]
         [max-r (apply max (map (lambda (entry)
                                        (let ([c (car entry)])
                                             (cdr c)))
                                coords))])
        (let loop-r ([r min-r] [lines '()])
             (if (> r max-r)
                 (apply string-append (reverse lines))
                 (let* ([offset (if (even? r) "" " ")]
                        [row-chars (list offset)])
                       (let loop-q ([q min-q] [chars row-chars])
                            (if (> q max-q)
                                (loop-r (+ r 1)
                                        (cons (apply string-append
                                                     (reverse (cons "\n" chars)))
                                              lines))
                                (let* ([coord (cons q r)]
                                       [tile (board-get board coord)]
                                       [char (if tile
                                                 (string-append (style-fn tile) " ")
                                                 "  ")])
                                      (loop-q (+ q 1) (cons char chars))))))))))

;;; render-hex-board-with-overlay : Board × MetaData × StyleFn × (List Coord) × String → String
;;; Render hex board with highlighted coordinates
(define (render-hex-board-with-overlay board meta style-fn overlay overlay-char)
  (let* ([radius (cdr (assq 'radius meta))]
         [coords (board-tiles board)]
         [overlay-set (let ([ht (make-hashtable equal-hash equal?)])
                           (for-each (lambda (c) (hashtable-set! ht c #t)) overlay)
                           ht)]
         ;; Find bounds
         [min-q (apply min (map (lambda (entry)
                                        (let ([c (car entry)])
                                             (car c)))
                                coords))]
         [max-q (apply max (map (lambda (entry)
                                        (let ([c (car entry)])
                                             (car c)))
                                coords))]
         [min-r (apply min (map (lambda (entry)
                                        (let ([c (car entry)])
                                             (cdr c)))
                                coords))]
         [max-r (apply max (map (lambda (entry)
                                        (let ([c (car entry)])
                                             (cdr c)))
                                coords))])
        (let loop-r ([r min-r] [lines '()])
             (if (> r max-r)
                 (apply string-append (reverse lines))
                 (let* ([offset (if (even? r) "" " ")]
                        [row-chars (list offset)])
                       (let loop-q ([q min-q] [chars row-chars])
                            (if (> q max-q)
                                (loop-r (+ r 1)
                                        (cons (apply string-append
                                                     (reverse (cons "\n" chars)))
                                              lines))
                                (let* ([coord (cons q r)]
                                       [in-overlay? (hashtable-ref overlay-set coord #f)]
                                       [tile (board-get board coord)]
                                       [char (cond
                                              [in-overlay? (string-append overlay-char " ")]
                                              [tile (string-append (style-fn tile) " ")]
                                              [else "  "])])
                                      (loop-q (+ q 1) (cons char chars))))))))))

;;; ============================================================
;;; High-Level Rendering
;;; ============================================================

;;; render-board : Board × StyleFn → String
;;; Render board using default style
(define (render-board board style-fn)
  (let ([shape (board%-shape board)]
        [meta (board%-meta board)])
       (case shape
             [(square) (render-square-board board meta style-fn)]
             [(hex hexagonal) (render-hex-board board meta style-fn)]
             [else (error 'render-board "Unsupported shape" shape)])))

;;; render-board-with-overlay : Board × StyleFn × (List Coord) × String → String
;;; Render board with overlay
(define (render-board-with-overlay board style-fn overlay overlay-char)
  (let ([shape (board%-shape board)]
        [meta (board%-meta board)])
       (case shape
             [(square) (render-square-board-with-overlay board meta style-fn overlay overlay-char)]
             [(hex hexagonal) (render-hex-board-with-overlay board meta style-fn overlay overlay-char)]
             [else (error 'render-board-with-overlay "Unsupported shape" shape)])))

;;; display-board : Board × StyleFn → Void
;;; Display board to stdout
(define (display-board board style-fn)
  (display (render-board board style-fn)))

;;; display-board-with-path : Board × StyleFn × (List Coord) → Void
;;; Display board with path highlighted
(define (display-board-with-path board style-fn path)
  (display (render-board-with-overlay board style-fn path "*")))

;;; display-board-with-fov : Board × StyleFn × (List Coord) → Void
;;; Display board with field of view highlighted
(define (display-board-with-fov board style-fn fov)
  (display (render-board-with-overlay board style-fn fov "+")))

;;; ============================================================
;;; Exports Summary
;;; ============================================================

;;; This module provides:
;;;   Style Protocol:
;;;     • default-tile-char — Default character for tile types
;;;
;;;   Low-Level Rendering:
;;;     • render-square-board — Render square grid
;;;     • render-square-board-with-overlay — Square grid with highlights
;;;     • render-hex-board — Render hex grid
;;;     • render-hex-board-with-overlay — Hex grid with highlights
;;;
;;;   High-Level Rendering:
;;;     • render-board — Render any board type
;;;     • render-board-with-overlay — Render with highlights
;;;     • display-board — Print board to stdout
;;;     • display-board-with-path — Display with path highlighted
;;;     • display-board-with-fov — Display with FOV highlighted
