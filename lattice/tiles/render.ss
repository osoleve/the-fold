(doc 'module 'tiles/render)
(doc 'description "ASCII art rendering for tile-based boards")
(doc 'layer 'lattice)
(doc 'purity 'partial)

(doc 'section 'tile-style)

(define (default-tile-char tile)
  (doc 'description "Default character mapping for common tile types")
  (doc 'type '(-> Tile String))
  (doc 'returns "Single character string representing tile")
  (case (tile-type tile)
        [(floor empty) "."]
        [(wall) "#"]
        [(grass) ","]
        [(swamp water) "~"]
        [(mountain) "^"]
        [(forest) "T"]
        [else "?"]))

(doc 'section 'square-rendering)

(define (render-square-board board meta style-fn)
  (doc 'description "Render a square board as ASCII art")
  (doc 'type '(-> Board MetaData (-> Tile String) String))
  (doc 'param 'board "The board to render")
  (doc 'param 'meta "Board metadata (contains width, height)")
  (doc 'param 'style-fn "Function mapping tiles to characters")
  (doc 'returns "String containing the ASCII art representation")
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

(define (render-square-board-with-overlay board meta style-fn overlay overlay-char)
  (doc 'description "Render square board with highlighted coordinates")
  (doc 'param 'overlay "List of coordinates to highlight")
  (doc 'param 'overlay-char "Character to use for overlay")
  (doc 'returns "String containing rendered board with overlay")
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

(doc 'section 'hex-rendering)
(doc 'note "Hex grid uses staggered layout with axial coordinates (q, r)")

(define (render-hex-board board meta style-fn)
  (doc 'description "Render hexagonal board as ASCII art. Simplified rendering with single character per hex.")
  (doc 'type '(-> Board MetaData (-> Tile String) String))
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

(define (render-hex-board-with-overlay board meta style-fn overlay overlay-char)
  (doc 'description "Render hex board with highlighted coordinates")
  (doc 'type '(-> Board MetaData (-> Tile String) (List Coord) String String))
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

(doc 'section 'high-level)

(define (render-board board style-fn)
  (doc 'export #t)
  (doc 'description "Render board using default style. Dispatches to shape-specific renderer.")
  (doc 'type '(-> Board (-> Tile String) String))
  (let ([shape (board%-shape board)]
        [meta (board%-meta board)])
       (case shape
             [(square) (render-square-board board meta style-fn)]
             [(hex hexagonal) (render-hex-board board meta style-fn)]
             [else (error 'render-board "Unsupported shape" shape)])))

(define (render-board-with-overlay board style-fn overlay overlay-char)
  (doc 'export #t)
  (doc 'description "Render board with overlay. Dispatches to shape-specific renderer.")
  (doc 'type '(-> Board (-> Tile String) (List Coord) String String))
  (let ([shape (board%-shape board)]
        [meta (board%-meta board)])
       (case shape
             [(square) (render-square-board-with-overlay board meta style-fn overlay overlay-char)]
             [(hex hexagonal) (render-hex-board-with-overlay board meta style-fn overlay overlay-char)]
             [else (error 'render-board-with-overlay "Unsupported shape" shape)])))

(define (display-board board style-fn)
  (doc 'export #t)
  (doc 'description "Display board to stdout")
  (doc 'type '(-> Board (-> Tile String) Void))
  (display (render-board board style-fn)))

(define (display-board-with-path board style-fn path)
  (doc 'export #t)
  (doc 'description "Display board with path highlighted using '*' character")
  (doc 'type '(-> Board (-> Tile String) (List Coord) Void))
  (display (render-board-with-overlay board style-fn path "*")))

(define (display-board-with-fov board style-fn fov)
  (doc 'export #t)
  (doc 'description "Display board with field of view highlighted using '+' character")
  (doc 'type '(-> Board (-> Tile String) (List Coord) Void))
  (display (render-board-with-overlay board style-fn fov "+")))

;;; ====
;;; Exports Summary
;;; ====

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
