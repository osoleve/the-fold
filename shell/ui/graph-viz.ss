;;; shell/graph-viz.ss — Graph Visualization Utilities
;;;
;;; Complements shell/graph-export.ss with:
;;;   - Layout algorithms (force-directed, tree, radial)
;;;   - Turtle graphics rendering for block graphs
;;;   - Interactive exploration utilities
;;;
;;; This is Shell code: uses filesystem, turtle graphics.
;;;
;;; Dependencies:
;;;   - shell/graph-export.ss (for hash->short, block->label)
;;;   - shell/store-api.ss (for store-get, store-all-hashes)
;;;   - lattice/data/graph-algorithms.ss (for traversal)
;;;   - lattice/linalg/vec2.ss (for layout math)
;;;   - shell/ui/turtle.ss (for rendering)

(load "core/base/prelude.ss")
(load "lattice/linalg/vec2.ss")
(load "shell/store-api.ss")
(load "lattice/data/graph-algorithms.ss")
(load "shell/graph-export.ss")
(load "shell/ui/turtle-color.ss")
(load "shell/ui/turtle-path.ss")
(load "shell/ui/turtle.ss")
(load "shell/ui/turtle-svg.ss")

;;; ====
;;; Layout Data Structures
;;; ====

;;; A layout maps hashes to positions: ((hash . vec2) ...)
;;; Stored as an association list.

;;; layout-get : Layout × Hash → Vec2 | #f
(define (layout-get layout hash)
  (let ([entry (assoc hash layout)])
       (if entry (cdr entry) #f)))

;;; layout-set : Layout × Hash × Vec2 → Layout
(define (layout-set layout hash pos)
  (cons (cons hash pos)
        (filter (lambda (e) (not (equal? (car e) hash))) layout)))

;;; layout-all-positions : Layout → (List Vec2)
(define (layout-all-positions layout)
  (map cdr layout))

;;; ====
;;; Force-Directed Layout
;;; ====

;;; Simple force-directed layout using:
;;;   - Repulsion between all nodes (Coulomb-like)
;;;   - Attraction along edges (spring-like)
;;;   - Gravity toward center

(define *force-layout-repulsion* 5000)    ; Node repulsion constant
(define *force-layout-attraction* 0.01)   ; Edge attraction constant
(define *force-layout-gravity* 0.01)      ; Center gravity
(define *force-layout-damping* 0.85)      ; Velocity damping
(define *force-layout-min-dist* 20)       ; Minimum distance between nodes

;;; force-layout-init : FS × (List Hash) × Number × Number → Layout
;;; Initialize layout with random positions.
(define (force-layout-init fs hashes width height)
  (let ([cx (/ width 2)]
        [cy (/ height 2)]
        [spread (min width height)])
       (let loop ([h hashes] [layout '()] [i 0])
            (if (null? h)
                layout
                (let* ([angle (* i 2.399)]  ; Golden angle
                       [r (* 0.3 spread (/ (+ i 1) (length hashes)))]
                       [x (+ cx (* r (cos angle)))]
                       [y (+ cy (* r (sin angle)))])
                      (loop (cdr h)
                            (cons (cons (car h) (vec2 x y)) layout)
                            (+ i 1)))))))

;;; force-layout-step : FS × Layout × (List Hash) × Number × Number → Layout
;;; Perform one iteration of force-directed layout.
(define (force-layout-step fs layout hashes width height)
  (let ([cx (/ width 2)]
        [cy (/ height 2)])
       ;; Compute forces for each node
       (let loop ([h hashes] [new-layout '()])
            (if (null? h)
                new-layout
                (let* ([hash (car h)]
                       [pos (layout-get layout hash)]
                       [force (vec2 0 0)])
                      
                      ;; Repulsion from all other nodes
                      (for-each
                       (lambda (other-hash)
                               (unless (equal? hash other-hash)
                                       (let* ([other-pos (layout-get layout other-hash)]
                                              [delta (vec2-sub pos other-pos)]
                                              [dist (max *force-layout-min-dist* (vec2-magnitude delta))]
                                              [repel (vec2-scale (vec2-normalize delta)
                                                                 (/ *force-layout-repulsion* (* dist dist)))])
                                             (set! force (vec2-add force repel)))))
                       hashes)
                      
                      ;; Attraction along edges (to referenced blocks)
                      (let ([blk (store-get fs hash)])
                           (when blk
                                 (let ([refs (block-refs blk)])
                                      (let edge-loop ([i 0])
                                           (when (< i (vector-length refs))
                                                 (let* ([ref-hash (vector-ref refs i)]
                                                        [ref-pos (layout-get layout ref-hash)])
                                                       (when ref-pos
                                                             (let* ([delta (vec2-sub ref-pos pos)]
                                                                    [attract (vec2-scale delta *force-layout-attraction*)])
                                                                   (set! force (vec2-add force attract)))))
                                                 (edge-loop (+ i 1)))))))
                      
                      ;; Gravity toward center
                      (let* ([to-center (vec2-sub (vec2 cx cy) pos)]
                             [grav (vec2-scale to-center *force-layout-gravity*)])
                            (set! force (vec2-add force grav)))
                      
                      ;; Apply force with damping
                      (let* ([new-pos (vec2-add pos (vec2-scale force *force-layout-damping*))]
                             ;; Clamp to viewport
                             [clamped (vec2 (max 50 (min (- width 50) (vec2-x new-pos)))
                                            (max 50 (min (- height 50) (vec2-y new-pos))))])
                            (loop (cdr h) (cons (cons hash clamped) new-layout))))))))

;;; force-layout : FS × (List Hash) × Number → Layout
;;; Run force-directed layout for given iterations.
(define (force-layout fs hashes iterations)
  (let* ([width *turtle-viewport-width*]
         [height *turtle-viewport-height*]
         [layout (force-layout-init fs hashes width height)])
        (let loop ([i 0] [l layout])
             (if (>= i iterations)
                 l
                 (loop (+ i 1) (force-layout-step fs l hashes width height))))))

;;; ====
;;; Tree Layout (for DAGs)
;;; ====

;;; tree-layout : FS × Hash × Number × Number → Layout
;;; Layout graph as a tree rooted at given hash.
;;; Uses BFS to assign levels, then spreads nodes horizontally.
(define (tree-layout fs root-hash width height)
  (let ([levels (make-hashtable equal-hash equal?)]  ; hash -> level
        [level-counts (make-hashtable equal-hash equal?)]  ; level -> count
        [visited (make-visited)])
       
       ;; BFS to assign levels
       (let bfs ([queue (list (cons root-hash 0))])
            (unless (null? queue)
                    (let* ([entry (car queue)]
                           [hash (car entry)]
                           [level (cdr entry)]
                           [rest (cdr queue)])
                          (unless (visited-contains? visited hash)
                                  (visited-add visited hash)
                                  (hashtable-set! levels hash level)
                                  (hashtable-set! level-counts level
                                                  (+ 1 (hashtable-ref level-counts level 0)))
                                  (let ([blk (store-get fs hash)])
                                       (when blk
                                             (let ([refs (block-refs blk)])
                                                  (let loop ([i 0] [q rest])
                                                       (if (>= i (vector-length refs))
                                                           (bfs q)
                                                           (loop (+ i 1)
                                                                 (append q (list (cons (vector-ref refs i)
                                                                                       (+ level 1)))))))))))
                          (bfs rest))))
       
       ;; Assign positions based on level
       (let ([max-level 0]
             [level-indices (make-hashtable equal-hash equal?)])  ; level -> current index
            
            ;; Find max level
            (for-each (lambda (h)
                              (let ([l (hashtable-ref levels h 0)])
                                   (when (> l max-level)
                                         (set! max-level l))))
                      (hashtable-keys levels))
            
            ;; Build layout
            (let loop ([hashes (vector->list (hashtable-keys levels))]
                       [layout '()])
                 (if (null? hashes)
                     layout
                     (let* ([hash (car hashes)]
                            [level (hashtable-ref levels hash 0)]
                            [count (hashtable-ref level-counts level 1)]
                            [idx (hashtable-ref level-indices level 0)]
                            [y (+ 50 (* level (/ (- height 100) (max 1 max-level))))]
                            [x (+ 50 (* (+ idx 0.5) (/ (- width 100) count)))])
                           (hashtable-set! level-indices level (+ idx 1))
                           (loop (cdr hashes)
                                 (cons (cons hash (vec2 x y)) layout))))))))

;;; ====
;;; Radial Layout
;;; ====

;;; radial-layout : FS × Hash × Number × Number → Layout
;;; Layout graph radially from a center node.
(define (radial-layout fs center-hash width height)
  (let ([cx (/ width 2)]
        [cy (/ height 2)]
        [visited (make-visited)]
        [layout '()])
       
       ;; Place center
       (set! layout (cons (cons center-hash (vec2 cx cy)) layout))
       (visited-add visited center-hash)
       
       ;; BFS placing nodes in rings
       (let bfs ([queue (list (cons center-hash 0))]
                 [ring-nodes (make-hashtable equal-hash equal?)])  ; level -> (list hash)
            (unless (null? queue)
                    (let* ([entry (car queue)]
                           [hash (car entry)]
                           [level (cdr entry)]
                           [rest (cdr queue)]
                           [blk (store-get fs hash)])
                          (when blk
                                (let ([refs (block-refs blk)])
                                     (let loop ([i 0] [q rest])
                                          (if (>= i (vector-length refs))
                                              (bfs q ring-nodes)
                                              (let ([ref-hash (vector-ref refs i)])
                                                   (unless (visited-contains? visited ref-hash)
                                                           (visited-add visited ref-hash)
                                                           (let ([ring (hashtable-ref ring-nodes (+ level 1) '())])
                                                                (hashtable-set! ring-nodes (+ level 1)
                                                                                (cons ref-hash ring))))
                                                   (loop (+ i 1)
                                                         (append q (list (cons ref-hash (+ level 1))))))))))
                          (when (null? rest)
                                ;; Place nodes in current ring
                                (let place-rings ([ring 1])
                                     (let ([nodes (hashtable-ref ring-nodes ring '())])
                                          (unless (null? nodes)
                                                  (let* ([count (length nodes)]
                                                         [radius (* ring 60)]
                                                         [angle-step (/ (* 2 3.14159) count)])
                                                        (let node-loop ([ns nodes] [i 0])
                                                             (unless (null? ns)
                                                                     (let* ([angle (* i angle-step)]
                                                                            [x (+ cx (* radius (cos angle)))]
                                                                            [y (+ cy (* radius (sin angle)))])
                                                                           (set! layout (cons (cons (car ns) (vec2 x y)) layout))
                                                                           (node-loop (cdr ns) (+ i 1))))))
                                                  (place-rings (+ ring 1)))))))))
       layout))

;;; ====
;;; Turtle Graphics Rendering
;;; ====

;;; Define gray color (not in standard palette)
(define color12-gray (make-color12 8 8 8))

;;; Node colors by tag
(define (tag->color tag)
  (case tag
        [(expr sexpr) color12-blue]
        [(lambda fn) color12-cyan]
        [(define let) color12-green]
        [(list pair) color12-yellow]
        [(number integer real) color12-magenta]
        [(string symbol) color12-red]
        [else color12-gray]))

;;; draw-node : Turtle × Vec2 × String × Color12 → Turtle
;;; Draw a node as a circle with label.
(define (draw-node turtle pos label color)
  (let* ([x (vec2-x pos)]
         [y (vec2-y pos)]
         ;; Move to node position (pen up)
         [t1 (pu turtle)]
         [t2 (goto t1 x y)]
         [t3 (turtle-with-pen-color t2 color)]
         [t4 (pd t3)]
         ;; Draw circle
         [t5 (draw-circle t4 15)])
        t5))

;;; draw-circle : Turtle × Number → Turtle
;;; Draw a circle of given radius at current position.
(define (draw-circle turtle radius)
  (let* ([cx (turtle-x turtle)]
         [cy (turtle-y turtle)]
         [t1 (pu turtle)]
         [t2 (goto t1 (+ cx radius) cy)]
         [t3 (pd t2)])
        (let loop ([t t3] [angle 0])
             (if (>= angle 360)
                 t
                 (let* ([rad (* angle (/ 3.14159 180))]
                        [x (+ cx (* radius (cos rad)))]
                        [y (+ cy (* radius (sin rad)))]
                        [t-next (goto t x y)])
                       (loop t-next (+ angle 15)))))))

;;; draw-edge : Turtle × Vec2 × Vec2 × Color12 → Turtle
;;; Draw an edge between two nodes.
(define (draw-edge turtle from-pos to-pos color)
  (let* ([t1 (pu turtle)]
         [t2 (goto t1 (vec2-x from-pos) (vec2-y from-pos))]
         [t3 (turtle-with-pen-color t2 color)]
         [t4 (pd t3)]
         ;; Draw line with arrow
         [t5 (goto t4 (vec2-x to-pos) (vec2-y to-pos))])
        t5))

;;; goto : Turtle × Number × Number → Turtle
;;; Move turtle to absolute position.
(define (goto turtle x y)
  (let* ([dx (- x (turtle-x turtle))]
         [dy (- y (turtle-y turtle))]
         [dist (sqrt (+ (* dx dx) (* dy dy)))]
         [angle (if (= dist 0) 0 (rad->deg (atan dx (- dy))))])
        (if (= dist 0)
            turtle
            (let* ([t1 (turtle-with-heading turtle angle)]
                   [t2 (fd t1 dist)])
                  t2))))

;;; rad->deg : Number → Number
(define (rad->deg rad)
  (* rad (/ 180 3.14159)))

;;; ====
;;; Graph Rendering Functions
;;; ====

;;; render-graph-turtle : FS × Layout × (List Hash) → Turtle
;;; Render a graph to turtle graphics.
(define (render-graph-turtle fs layout hashes)
  (let ([turtle (make-turtle)])
       ;; Draw edges first (so nodes are on top)
       (let edge-loop ([t turtle] [h hashes])
            (if (null? h)
                ;; Now draw nodes
                (let node-loop ([t2 t] [h2 hashes])
                     (if (null? h2)
                         t2
                         (let* ([hash (car h2)]
                                [pos (layout-get layout hash)]
                                [blk (store-get fs hash)]
                                [tag (if blk (block-tag blk) 'unknown)]
                                [color (tag->color tag)]
                                [t3 (if pos (draw-node t2 pos (hash->short hash) color) t2)])
                               (node-loop t3 (cdr h2)))))
                ;; Draw edge
                (let* ([hash (car h)]
                       [pos (layout-get layout hash)]
                       [blk (store-get fs hash)])
                      (if (and pos blk)
                          (let ([refs (block-refs blk)])
                               (let ref-loop ([t2 t] [i 0])
                                    (if (>= i (vector-length refs))
                                        (edge-loop t2 (cdr h))
                                        (let* ([ref-hash (vector-ref refs i)]
                                               [ref-pos (layout-get layout ref-hash)]
                                               [t3 (if ref-pos
                                                       (draw-edge t2 pos ref-pos color12-gray)
                                                       t2)])
                                              (ref-loop t3 (+ i 1))))))
                          (edge-loop t (cdr h))))))))

;;; graph-to-svg : FS × (List Hash) × Symbol → String
;;; Render graph to SVG string.
;;; layout-type: 'force | 'tree | 'radial
(define (graph-to-svg fs hashes layout-type)
  (let* ([width *turtle-viewport-width*]
         [height *turtle-viewport-height*]
         [layout (case layout-type
                       [(force) (force-layout fs hashes 50)]
                       [(tree) (if (null? hashes)
                                   '()
                                   (tree-layout fs (car hashes) width height))]
                       [(radial) (if (null? hashes)
                                     '()
                                     (radial-layout fs (car hashes) width height))]
                       [else (force-layout fs hashes 50)])]
         [turtle (render-graph-turtle fs layout hashes)]
         [drawing (turtle->drawing turtle)])
        (drawing->svg drawing)))

;;; graph-to-svg-file : FS × (List Hash) × Symbol × String → void
;;; Render graph to SVG file.
(define (graph-to-svg-file fs hashes layout-type output-path)
  (let ([svg (graph-to-svg fs hashes layout-type)])
       (call-with-output-file output-path
                              (lambda (port)
                                      (put-string port svg)))
       (display (format "Saved graph visualization to ~a\n" output-path))))

;;; ====
;;; Interactive Exploration
;;; ====

;;; show-neighborhood : FS × Hash × Number → void
;;; Display ASCII visualization of nodes within N hops.
(define (show-neighborhood fs center-hash radius)
  (let ([visited (make-visited)]
        [nodes-at-level (make-hashtable equal-hash equal?)])
       
       ;; BFS to collect nodes at each level
       (let bfs ([queue (list (cons center-hash 0))])
            (unless (null? queue)
                    (let* ([entry (car queue)]
                           [hash (car entry)]
                           [level (cdr entry)]
                           [rest (cdr queue)])
                          (unless (or (visited-contains? visited hash)
                                      (> level radius))
                                  (visited-add visited hash)
                                  (hashtable-set! nodes-at-level level
                                                  (cons hash (hashtable-ref nodes-at-level level '())))
                                  (let ([blk (store-get fs hash)])
                                       (when blk
                                             (let ([refs (block-refs blk)])
                                                  (let loop ([i 0] [q rest])
                                                       (if (>= i (vector-length refs))
                                                           (bfs q)
                                                           (loop (+ i 1)
                                                                 (append q (list (cons (vector-ref refs i)
                                                                                       (+ level 1)))))))))))
                          (bfs rest))))
       
       ;; Display
       (display "\nNeighborhood View:\n")
       (display "====\n")
       (let level-loop ([l 0])
            (when (<= l radius)
                  (let ([nodes (hashtable-ref nodes-at-level l '())])
                       (display (format "Level ~a: " l))
                       (if (null? nodes)
                           (display "(none)")
                           (for-each (lambda (h)
                                             (let ([blk (store-get fs h)])
                                                  (display (format "[~a:~a] "
                                                                   (if blk (block-tag blk) '?)
                                                                   (hash->short h)))))
                                     (reverse nodes)))
                       (newline))
                  (level-loop (+ l 1))))))

;;; trace-path : FS × Hash × Hash → void
;;; Display step-by-step path between two nodes.
(define (trace-path fs from-hash to-hash)
  (let ([path (shortest-path fs from-hash to-hash)])
       (if (not path)
           (display "No path found.\n")
           (begin
            (display "\nPath Trace:\n")
            (display "====\n")
            (let loop ([p path] [i 0])
                 (unless (null? p)
                         (let* ([hash (car p)]
                                [blk (store-get fs hash)]
                                [tag (if blk (block-tag blk) '?)]
                                [short (hash->short hash)])
                               (display (format "~a. [~a] ~a\n" i tag short))
                               (when blk
                                     (let ([payload (block-payload blk)])
                                          (guard (e [else #f])
                                                 (let ([str (utf8->string payload)])
                                                      (when (< (string-length str) 60)
                                                            (display (format "   ~a\n" str)))))))
                               (loop (cdr p) (+ i 1)))))
            (display (format "\nTotal: ~a nodes\n" (length path)))))))

;;; explore-block : FS × Hash → void
;;; Display detailed information about a block and its connections.
(define (explore-block fs hash)
  (let ([blk (store-get fs hash)])
       (if (not blk)
           (display (format "Block not found: ~a\n" (hash->hex hash)))
           (begin
            (display "\n┌─────────────────────────────────────┐\n")
            (display (format "│ Block: ~a...      │\n" (hash->short hash)))
            (display "├─────────────────────────────────────┤\n")
            (display (format "│ Tag: ~a\n" (block-tag blk)))
            (display "│ Payload:\n")
            (let ([payload (block-payload blk)])
                 (guard (e [else (display "│   [binary data]\n")])
                        (let ([str (utf8->string payload)])
                             (display (format "│   ~a\n"
                                              (if (> (string-length str) 50)
                                                  (string-append (substring str 0 50) "...")
                                                  str))))))
            (display "├─────────────────────────────────────┤\n")
            (let ([refs (block-refs blk)])
                 (display (format "│ References: ~a\n" (vector-length refs)))
                 (let loop ([i 0])
                      (when (< i (vector-length refs))
                            (let* ([ref-hash (vector-ref refs i)]
                                   [ref-blk (store-get fs ref-hash)]
                                   [ref-tag (if ref-blk (block-tag ref-blk) '?)])
                                  (display (format "│   ~a. [~a] ~a...\n"
                                                   i ref-tag (hash->short ref-hash))))
                            (loop (+ i 1)))))
            (display "├─────────────────────────────────────┤\n")
            (let ([incoming (get-incoming-hashes fs hash)])
                 (display (format "│ Referenced by: ~a\n" (length incoming)))
                 (for-each (lambda (in-hash)
                                   (let* ([in-blk (store-get fs in-hash)]
                                          [in-tag (if in-blk (block-tag in-blk) '?)])
                                         (display (format "│   [~a] ~a...\n"
                                                          in-tag (hash->short in-hash)))))
                           (take (min 5 (length incoming)) incoming)))
            (display "└─────────────────────────────────────┘\n")))))

;;; ====
;;; Quick Visualization Commands
;;; ====

;;; viz-recent : FS × Number → void
;;; Visualize the N most recent blocks.
(define (viz-recent fs count)
  (let* ([all-hashes (store-all-hashes fs)]
         [recent (take (min count (length all-hashes)) all-hashes)])
        (graph-to-svg-file fs recent 'force "recent-graph.svg")
        (show-neighborhood fs (if (null? recent) #f (car recent)) 2)))

;;; viz-subgraph : FS × Hash × Number → void
;;; Visualize a subgraph rooted at hash with given depth.
(define (viz-subgraph fs root-hash depth)
  (let ([reachable (reachable-from fs root-hash)])
       ;; Limit to nodes within depth
       (graph-to-svg-file fs (take (min 50 (length reachable)) reachable)
                          'radial
                          "subgraph.svg")))

;;; ====
;;; Module Load Message
;;; ====

(display "Graph Visualization loaded.\n")
(display "  Layouts: (force-layout fs hashes iterations)\n")
(display "           (tree-layout fs root width height)\n")
(display "           (radial-layout fs root width height)\n")
(display "  Render:  (graph-to-svg-file fs hashes layout-type path)\n")
(display "  Explore: (show-neighborhood fs hash radius)\n")
(display "           (trace-path fs from to)\n")
(display "           (explore-block fs hash)\n")
