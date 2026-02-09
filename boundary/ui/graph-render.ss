;;; boundary/ui/graph-render.ss --- ASCII rendering for graph layouts
;;; @module graph-render
;;; @requires prelude graph-layout

(load "core/base/prelude.ss")
(load "lattice/data/graph/graph-layout.ss")

(doc 'module 'graph-render)
(doc 'description "ASCII visualization of graph layouts with nodes, edges, and labels")
(doc 'layer 'boundary)
(doc 'purity 'partial)

;;; ============================================================
;;; Section: Frame Buffer
;;; ============================================================

(doc 'section 'frame-buffer)

(define (make-render-buffer width height)
  (doc 'type '(-> Nat Nat RenderBuffer))
  (doc 'description "Create render buffer filled with spaces")
  (list 'render-buffer width height
        (make-vector (* width height) #\space)))

(define (rb-width rb) (list-ref rb 1))
(define (rb-height rb) (list-ref rb 2))
(define (rb-chars rb) (list-ref rb 3))

(define (rb-index rb x y)
  (+ (inexact->exact (floor x))
     (* (inexact->exact (floor y)) (rb-width rb))))

(define (rb-in-bounds? rb x y)
  (let ([xi (inexact->exact (floor x))]
        [yi (inexact->exact (floor y))])
    (and (>= xi 0) (< xi (rb-width rb))
         (>= yi 0) (< yi (rb-height rb)))))

(define (rb-set! rb x y char)
  (when (rb-in-bounds? rb x y)
    (vector-set! (rb-chars rb) (rb-index rb x y) char)))

(define (rb-get rb x y)
  (if (rb-in-bounds? rb x y)
      (vector-ref (rb-chars rb) (rb-index rb x y))
      #\space))

(define (rb-set-string! rb x y str)
  (doc 'type '(-> RenderBuffer Number Number String Void))
  (doc 'description "Write string starting at (x,y)")
  (let loop ([i 0] [chars (string->list str)])
    (when (pair? chars)
      (rb-set! rb (+ x i) y (car chars))
      (loop (+ i 1) (cdr chars)))))

(define (rb-to-string rb)
  (doc 'type '(-> RenderBuffer String))
  (let ([w (rb-width rb)]
        [h (rb-height rb)]
        [chars (rb-chars rb)]
        [result '()])
    (do ([y 0 (+ y 1)])
        ((>= y h) (apply string-append (reverse result)))
      (do ([x 0 (+ x 1)])
          ((>= x w))
        (set! result (cons (string (vector-ref chars (+ x (* y w)))) result)))
      (set! result (cons "\n" result)))))

;;; ============================================================
;;; Section: Line Drawing
;;; ============================================================

(doc 'section 'line-drawing)

;; Characters for different line directions
(define *edge-chars*
  '((horizontal . #\─)
    (vertical . #\│)
    (diagonal-down . #\\)
    (diagonal-up . #\/)
    (cross . #\+)
    (dot . #\.)))

(define (line-char dx dy)
  (doc 'type '(-> Number Number Char))
  (doc 'description "Choose line character based on direction")
  (let ([ax (abs dx)]
        [ay (abs dy)])
    (cond
      [(< ay (* 0.3 ax)) #\─]   ; mostly horizontal
      [(< ax (* 0.3 ay)) #\│]   ; mostly vertical
      [(and (> dx 0) (> dy 0)) #\\]  ; down-right
      [(and (< dx 0) (< dy 0)) #\\]  ; up-left (same visual)
      [else #\/])))                   ; other diagonals

(define (draw-line! rb x1 y1 x2 y2)
  (doc 'type '(-> RenderBuffer Number Number Number Number Void))
  (doc 'description "Draw line using Bresenham's algorithm")
  (let* ([dx (- x2 x1)]
         [dy (- y2 y1)]
         [steps (max 1 (max (abs dx) (abs dy)))]
         [x-step (/ dx steps)]
         [y-step (/ dy steps)]
         [char (line-char dx dy)])
    (do ([i 0 (+ i 1)])
        ((> i steps))
      (let ([x (+ x1 (* i x-step))]
            [y (+ y1 (* i y-step))])
        (rb-set! rb x y char)))))

(define (draw-arrow! rb x1 y1 x2 y2)
  (doc 'type '(-> RenderBuffer Number Number Number Number Void))
  (doc 'description "Draw line with arrow at end")
  (draw-line! rb x1 y1 x2 y2)
  ;; Add arrowhead
  (let* ([dx (- x2 x1)]
         [dy (- y2 y1)]
         [len (max 0.001 (sqrt (+ (* dx dx) (* dy dy))))]
         [ux (/ dx len)]
         [uy (/ dy len)])
    (cond
      [(> (abs ux) (abs uy))
       (rb-set! rb x2 y2 (if (> ux 0) #\> #\<))]
      [else
       (rb-set! rb x2 y2 (if (> uy 0) #\v #\^))])))

;;; ============================================================
;;; Section: Node Rendering
;;; ============================================================

(doc 'section 'nodes)

(define (draw-node-dot! rb x y)
  (doc 'type '(-> RenderBuffer Number Number Void))
  (doc 'description "Draw node as single dot")
  (rb-set! rb x y #\*))

(define (draw-node-circle! rb x y)
  (doc 'type '(-> RenderBuffer Number Number Void))
  (doc 'description "Draw node as small circle")
  (rb-set! rb (- x 1) y #\()
  (rb-set! rb x y #\*)
  (rb-set! rb (+ x 1) y #\)))

(define (draw-node-box! rb x y label)
  (doc 'type '(-> RenderBuffer Number Number String Void))
  (doc 'description "Draw node as labeled box")
  (let* ([len (string-length label)]
         [half (/ len 2)]
         [left (- x half 1)]
         [right (+ x half 1)])
    ;; Top border
    (rb-set! rb left (- y 1) #\+)
    (do ([i 0 (+ i 1)])
        ((>= i (+ len 2)))
      (rb-set! rb (+ left 1 i) (- y 1) #\-))
    (rb-set! rb (+ right 1) (- y 1) #\+)
    ;; Middle with label
    (rb-set! rb left y #\|)
    (rb-set-string! rb (- x half) y label)
    (rb-set! rb (+ right 1) y #\|)
    ;; Bottom border
    (rb-set! rb left (+ y 1) #\+)
    (do ([i 0 (+ i 1)])
        ((>= i (+ len 2)))
      (rb-set! rb (+ left 1 i) (+ y 1) #\-))
    (rb-set! rb (+ right 1) (+ y 1) #\+)))

(define (draw-node-label! rb x y label)
  (doc 'type '(-> RenderBuffer Number Number String Void))
  (doc 'description "Draw node as dot with label")
  (rb-set! rb x y #\*)
  (let ([half (/ (string-length label) 2)])
    (rb-set-string! rb (- x half) (+ y 1) label)))

;;; ============================================================
;;; Section: Graph Rendering
;;; ============================================================

(doc 'section 'graph-rendering)

(define (render-graph-simple graph width height)
  (doc 'export #t)
  (doc 'type '(-> LayoutGraph Nat Nat String))
  (doc 'description "Render graph with dots for nodes and lines for edges")
  (let* ([normalized (normalize-layout graph width height 2)]
         [nodes (graph-nodes normalized)]
         [edges (graph-edges normalized)]
         [rb (make-render-buffer width height)])
    ;; Draw edges first (so nodes appear on top)
    (for-each
     (lambda (edge)
       (let* ([id1 (car edge)]
              [id2 (cdr edge)]
              [n1 (graph-find-node normalized id1)]
              [n2 (graph-find-node normalized id2)])
         (when (and n1 n2)
           (draw-line! rb (node-x n1) (node-y n1)
                       (node-x n2) (node-y n2)))))
     edges)
    ;; Draw nodes
    (for-each
     (lambda (node)
       (draw-node-dot! rb (node-x node) (node-y node)))
     nodes)
    (rb-to-string rb)))

(define (render-graph-labeled graph width height label-fn)
  (doc 'export #t)
  (doc 'type '(-> LayoutGraph Nat Nat (-> Any String) String))
  (doc 'description "Render graph with labeled nodes")
  (let* ([normalized (normalize-layout graph width height 4)]
         [nodes (graph-nodes normalized)]
         [edges (graph-edges normalized)]
         [rb (make-render-buffer width height)])
    ;; Draw edges first
    (for-each
     (lambda (edge)
       (let* ([id1 (car edge)]
              [id2 (cdr edge)]
              [n1 (graph-find-node normalized id1)]
              [n2 (graph-find-node normalized id2)])
         (when (and n1 n2)
           (draw-arrow! rb (node-x n1) (node-y n1)
                        (node-x n2) (node-y n2)))))
     edges)
    ;; Draw nodes with labels
    (for-each
     (lambda (node)
       (let ([label (label-fn (node-id node))])
         (draw-node-label! rb (node-x node) (node-y node) label)))
     nodes)
    (rb-to-string rb)))

(define (render-graph-boxed graph width height label-fn)
  (doc 'export #t)
  (doc 'type '(-> LayoutGraph Nat Nat (-> Any String) String))
  (doc 'description "Render graph with boxed labeled nodes")
  (let* ([normalized (normalize-layout graph width height 6)]
         [nodes (graph-nodes normalized)]
         [edges (graph-edges normalized)]
         [rb (make-render-buffer width height)])
    ;; Draw edges first
    (for-each
     (lambda (edge)
       (let* ([id1 (car edge)]
              [id2 (cdr edge)]
              [n1 (graph-find-node normalized id1)]
              [n2 (graph-find-node normalized id2)])
         (when (and n1 n2)
           (draw-line! rb (node-x n1) (node-y n1)
                       (node-x n2) (node-y n2)))))
     edges)
    ;; Draw boxed nodes
    (for-each
     (lambda (node)
       (let ([label (label-fn (node-id node))])
         (draw-node-box! rb (node-x node) (node-y node) label)))
     nodes)
    (rb-to-string rb)))

;;; ============================================================
;;; Section: Colored Rendering
;;; ============================================================

(doc 'section 'colored)

(define (ansi-color code)
  (string-append "\x1b;[38;5;" (number->string code) "m"))

(define (ansi-reset)
  "\x1b;[0m")

(define (tier-to-color tier)
  (doc 'type '(-> Nat Number))
  (doc 'description "Map tier to ANSI 256 color (cool to warm)")
  (case tier
    [(0) 51]   ; cyan
    [(1) 46]   ; green
    [(2) 226]  ; yellow
    [(3) 208]  ; orange
    [else 196])) ; red

(define (render-graph-colored graph width height label-fn tier-fn)
  (doc 'export #t)
  (doc 'type '(-> LayoutGraph Nat Nat (-> Any String) (-> Any Nat) String))
  (doc 'description "Render graph with colored nodes based on tier")
  (let* ([normalized (normalize-layout graph width height 4)]
         [nodes (graph-nodes normalized)]
         [edges (graph-edges normalized)]
         [rb (make-render-buffer width height)]
         [colors (make-vector (* width height) 0)])
    ;; Draw edges in gray
    (for-each
     (lambda (edge)
       (let* ([id1 (car edge)]
              [id2 (cdr edge)]
              [n1 (graph-find-node normalized id1)]
              [n2 (graph-find-node normalized id2)])
         (when (and n1 n2)
           (draw-line! rb (node-x n1) (node-y n1)
                       (node-x n2) (node-y n2)))))
     edges)
    ;; Draw nodes with colors
    (for-each
     (lambda (node)
       (let* ([id (node-id node)]
              [label (label-fn id)]
              [tier (tier-fn id)]
              [color (tier-to-color tier)]
              [x (inexact->exact (round (node-x node)))]
              [y (inexact->exact (round (node-y node)))])
         ;; Draw node
         (rb-set! rb x y #\*)
         (when (rb-in-bounds? rb x y)
           (vector-set! colors (rb-index rb x y) color))
         ;; Draw label
         (let* ([half (/ (string-length label) 2)]
                [start-x (- x half)]
                [label-y (+ y 1)])
           (let loop ([i 0] [chars (string->list label)])
             (when (pair? chars)
               (let ([cx (+ start-x i)])
                 (rb-set! rb cx label-y (car chars))
                 (when (rb-in-bounds? rb cx label-y)
                   (vector-set! colors (rb-index rb cx label-y) color)))
               (loop (+ i 1) (cdr chars)))))))
     nodes)
    ;; Build colored string
    (let ([w (rb-width rb)]
          [h (rb-height rb)]
          [chars (rb-chars rb)]
          [result '()]
          [last-color -1])
      (do ([y 0 (+ y 1)])
          ((>= y h))
        (do ([x 0 (+ x 1)])
            ((>= x w))
          (let* ([idx (+ x (* y w))]
                 [ch (vector-ref chars idx)]
                 [col (vector-ref colors idx)])
            (when (and (> col 0) (not (= col last-color)))
              (set! result (cons (ansi-color col) result))
              (set! last-color col))
            (when (and (= col 0) (not (= last-color 0)))
              (set! result (cons (ansi-color 240) result))  ; gray for edges
              (set! last-color 0))
            (set! result (cons (string ch) result))))
        (set! result (cons "\n" result)))
      (string-append (apply string-append (reverse result)) (ansi-reset)))))

;;; ============================================================
;;; Section: Animation
;;; ============================================================

(doc 'section 'animation)

(define (animate-layout graph width height n-frames)
  (doc 'export #t)
  (doc 'type '(-> LayoutGraph Nat Nat Nat (List String)))
  (doc 'description "Generate animation frames as layout converges")
  (let loop ([g graph] [i 0] [frames '()])
    (if (>= i n-frames)
        (reverse frames)
        (let* ([frame (render-graph-simple g width height)]
               [next-g (layout-step g)])
          (loop next-g (+ i 1) (cons frame frames))))))

(define (animate-layout-colored graph width height label-fn tier-fn n-frames)
  (doc 'export #t)
  (doc 'type '(-> LayoutGraph Nat Nat (-> Any String) (-> Any Nat) Nat (List String)))
  (doc 'description "Generate colored animation frames as layout converges")
  (let loop ([g graph] [i 0] [frames '()])
    (if (>= i n-frames)
        (reverse frames)
        (let* ([normalized (normalize-layout g width height 4)]
               [frame (render-graph-colored normalized width height label-fn tier-fn)]
               [next-g (layout-step g)])
          (loop next-g (+ i 1) (cons frame frames))))))

(printf "✓ Graph render loaded\n")
(printf "  (render-graph-simple graph w h)        - Dots and lines\n")
(printf "  (render-graph-labeled graph w h f)     - With labels\n")
(printf "  (render-graph-colored graph w h lf tf) - Tier colors\n")
(printf "  (animate-layout graph w h n)           - Layout animation\n")
