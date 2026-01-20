
(load "core/base/prelude.ss")
(load "lattice/linalg/vec2.ss")
(load "user/creations/ascii-video.ss")
(load "lattice/physics/classical/world.ss")


(doc 'module 'ascii-renderer)
(doc 'description "ASCII renderer for 2D physics worlds")
(doc 'layer 'lattice)
(doc 'purity 'partial)
(doc 'section 'render)

(doc 'make-render-config 'type 'Int x Int x Number x Int x Int -> RenderConfig)
(doc "Parameters:")
(doc "  width: frame width in characters")
(doc "  height: frame height in characters")
(doc "  scale: physics units to screen pixels ratio")
(doc "  origin-x: screen X offset for physics origin")
(doc "  origin-y: screen Y offset for physics origin")
(define (make-render-config width height scale origin-x origin-y)
  (list 'render-config width height scale origin-x origin-y))

(define (render-config? c)
  (and (pair? c) (eq? (car c) 'render-config)))

(define (config-width c) (list-ref c 1))
(define (config-height c) (list-ref c 2))
(define (config-scale c) (list-ref c 3))
(define (config-origin-x c) (list-ref c 4))
(define (config-origin-y c) (list-ref c 5))

(doc "Default config: 80x40, scale 3, centered")
(define (default-render-config)
  (make-render-config 80 40 3.0 40 10))

(doc 'section 'debug)

(doc 'make-debug-options 'type 'Bool x Bool x Bool x Bool -> DebugOptions)
(doc "Parameters:")
(doc "  show-velocity: draw velocity vectors")
(doc "  show-contacts: draw collision contact points/normals")
(doc "  show-constraints: draw constraint anchors and connections")
(doc "  show-aabb: draw bounding boxes")
(define (make-debug-options show-velocity show-contacts show-constraints show-aabb)
  (list 'debug-options show-velocity show-contacts show-constraints show-aabb))

(define (debug-options? d)
  (and (pair? d) (eq? (car d) 'debug-options)))

(define (debug-show-velocity d) (list-ref d 1))
(define (debug-show-contacts d) (list-ref d 2))
(define (debug-show-constraints d) (list-ref d 3))
(define (debug-show-aabb d) (list-ref d 4))

(doc "Default: all off")
(define (default-debug-options)
  (make-debug-options #f #f #f #f))

(doc "Full debug: all on")
(define (full-debug-options)
  (make-debug-options #t #t #t #t))

(doc 'section 'render)

(doc 'make-render-style 'type 'Char x Char x Char x Char x Char x Char x Char -> RenderStyle)
(doc "Character palette for visual elements")
(define (make-render-style fill-char static-char box-h box-v corner velocity-char contact-char)
  (list 'render-style fill-char static-char box-h box-v corner velocity-char contact-char))

(define (render-style? s)
  (and (pair? s) (eq? (car s) 'render-style)))

(define (style-fill-char s) (list-ref s 1))
(define (style-static-char s) (list-ref s 2))
(define (style-box-h s) (list-ref s 3))
(define (style-box-v s) (list-ref s 4))
(define (style-corner s) (list-ref s 5))
(define (style-velocity-char s) (list-ref s 6))
(define (style-contact-char s) (list-ref s 7))

(doc "Default style")
(define (default-render-style)
  (make-render-style #\O #\# #\- #\| #\+ #\> #\*))

(doc "Minimal style (lighter)")
(define (minimal-render-style)
  (make-render-style #\o #\. #\- #\| #\+ #\> #\x))

(doc "Bold style (heavier)")
(define (bold-render-style)
  (make-render-style #\@ #\# #\= #\| #\# #\> #\*))

(doc 'section 'world)

(define (make-world-renderer config debug-opts style)
  (list 'world-renderer config debug-opts style))

(define (renderer-config r) (list-ref r 1))
(define (renderer-debug r) (list-ref r 2))
(define (renderer-style r) (list-ref r 3))

(doc "Default renderer")
(define (default-world-renderer)
  (make-world-renderer (default-render-config)
                       (default-debug-options)
                       (default-render-style)))

(doc 'section 'coordinate)

(doc "world->screen : Vec2 x RenderConfig -> (Int . Int)")
(doc "Convert physics coordinates to screen coordinates.")
(doc "Physics Y increases upward, screen Y increases downward.")
(define (world->screen pos config)
  (let ([x (vec2-x pos)]
        [y (vec2-y pos)]
        [scale (config-scale config)]
        [ox (config-origin-x config)]
        [oy (config-origin-y config)])
       (cons (inexact->exact (round (+ ox (* x scale))))
             (inexact->exact (round (+ oy (* y scale)))))))

(doc "screen->world : Int x Int x RenderConfig -> Vec2")
(doc "Convert screen coordinates to physics coordinates.")
(define (screen->world sx sy config)
  (let ([scale (config-scale config)]
        [ox (config-origin-x config)]
        [oy (config-origin-y config)])
       (vec2 (/ (- sx ox) scale)
             (/ (- sy oy) scale))))

(doc 'section 'safe)

(doc "safe-frame-set! : Frame x Int x Int x Char x RenderConfig -> Void")
(doc "Set character with bounds checking.")
(define (safe-frame-set! frame x y char config)
  (when (and (>= x 0) (< x (config-width config))
             (>= y 0) (< y (config-height config)))
        (frame-set! frame x y char)))

(doc 'section 'line)

(doc "draw-line! : Frame x Int x Int x Int x Int x Char x RenderConfig -> Void")
(doc "Draw a line using Bresenham's algorithm.")
(define (draw-line! frame x0 y0 x1 y1 char config)
  (let* ([dx (abs (- x1 x0))]
         [dy (abs (- y1 y0))]
         [sx (if (< x0 x1) 1 -1)]
         [sy (if (< y0 y1) 1 -1)]
         [err (- dx dy)])
        (let loop ([x x0] [y y0] [err err])
             (safe-frame-set! frame x y char config)
             (unless (and (= x x1) (= y y1))
                     (let ([e2 (* 2 err)])
                          (let ([new-x (if (> e2 (- dy)) (+ x sx) x)]
                                [new-y (if (< e2 dx) (+ y sy) y)]
                                [new-err (+ err
                                            (if (> e2 (- dy)) (- dy) 0)
                                            (if (< e2 dx) dx 0))])
                               (loop new-x new-y new-err)))))))

(doc 'section 'filled)

(doc "draw-filled-circle! : Frame x Int x Int x Int x Char x RenderConfig -> Void")
(doc "Draw a filled circle using scanline fill.")
(define (draw-filled-circle! frame cx cy radius char config)
  (let ([w (config-width config)]
        [h (config-height config)])
       ;; For each row in the circle's bounding box
       (do ([dy (- radius) (+ dy 1)])
           ((> dy radius))
           (let* ([y (+ cy dy)]
                  [dy-sq (* dy dy)]
                  [r-sq (* radius radius)])
                 (when (and (>= y 0) (< y h) (<= dy-sq r-sq))
                       ;; x extent: sqrt(r^2 - y^2)
                       (let ([dx (inexact->exact (floor (sqrt (- r-sq dy-sq))))])
                            (do ([x (- cx dx) (+ x 1)])
                                ((> x (+ cx dx)))
                                (when (and (>= x 0) (< x w))
                                      (frame-set! frame x y char)))))))))

(doc 'section 'filled)

(doc 'polygon-scanline-intersections 'type '(List (Int . Int)) x Int -> (List Int))
(doc "Find x intersections of horizontal scanline y with polygon edges.")
(define (polygon-scanline-intersections verts y)
  (let ([n (length verts)])
       (let loop ([i 0] [intersections '()])
            (if (>= i n)
                intersections
                (let* ([v1 (list-ref verts i)]
                       [v2 (list-ref verts (modulo (+ i 1) n))]
                       [y1 (cdr v1)]
                       [y2 (cdr v2)])
                      (if (or (and (<= y1 y) (> y2 y))
                              (and (> y1 y) (<= y2 y)))
                          ;; Edge crosses this scanline
                          (let* ([x1 (car v1)]
                                 [x2 (car v2)]
                                 [t (if (= y2 y1) 0.5 (/ (- y y1) (- y2 y1)))]
                                 [x (inexact->exact (round (+ x1 (* t (- x2 x1)))))])
                                (loop (+ i 1) (cons x intersections)))
                          (loop (+ i 1) intersections)))))))

(doc "fill-between-pairs! : Frame x (List Int) x Int x Char x RenderConfig -> Void")
(doc "Fill horizontal spans between sorted x pairs.")
(define (fill-between-pairs! frame xs y char config)
  (let ([w (config-width config)])
       (let loop ([remaining xs])
            (when (and (pair? remaining) (pair? (cdr remaining)))
                  (let ([x1 (car remaining)]
                        [x2 (cadr remaining)])
                       (do ([x (max 0 x1) (+ x 1)])
                           ((> x (min (- w 1) x2)))
                           (frame-set! frame x y char)))
                  (loop (cddr remaining))))))

(doc "draw-filled-polygon! : Frame x (List Vec2) x Char x RenderConfig -> Void")
(doc "Scanline fill for convex polygon.")
(define (draw-filled-polygon! frame vertices char config)
  (when (pair? vertices)
        (let* ([screen-verts (map (lambda (v) (world->screen v config)) vertices)]
               [h (config-height config)]
               [min-y (apply min (map cdr screen-verts))]
               [max-y (apply max (map cdr screen-verts))])
              ;; Scanline for each row
              (do ([y (max 0 min-y) (+ y 1)])
                  ((> y (min (- h 1) max-y)))
                  (let* ([intersections (polygon-scanline-intersections screen-verts y)]
                         [sorted (list-sort < intersections)])
                        (fill-between-pairs! frame sorted y char config))))))

(doc 'section 'aabb)

(doc "draw-filled-aabb! : Frame x AABB x Char x RenderConfig -> Void")
(doc "Fill an axis-aligned bounding box.")
(define (draw-filled-aabb! frame aabb char config)
  (let* ([min-screen (world->screen (aabb-min aabb) config)]
         [max-screen (world->screen (aabb-max aabb) config)]
         [x1 (min (car min-screen) (car max-screen))]
         [y1 (min (cdr min-screen) (cdr max-screen))]
         [x2 (max (car min-screen) (car max-screen))]
         [y2 (max (cdr min-screen) (cdr max-screen))]
         [w (config-width config)]
         [h (config-height config)])
        (do ([y (max 0 y1) (+ y 1)])
            ((> y (min (- h 1) y2)))
            (do ([x (max 0 x1) (+ x 1)])
                ((> x (min (- w 1) x2)))
                (frame-set! frame x y char)))))

(doc "draw-aabb-outline! : Frame x AABB x RenderStyle x RenderConfig -> Void")
(doc "Draw just the outline of an AABB.")
(define (draw-aabb-outline! frame aabb style config)
  (let* ([min-screen (world->screen (aabb-min aabb) config)]
         [max-screen (world->screen (aabb-max aabb) config)]
         [x1 (min (car min-screen) (car max-screen))]
         [y1 (min (cdr min-screen) (cdr max-screen))]
         [x2 (max (car min-screen) (car max-screen))]
         [y2 (max (cdr min-screen) (cdr max-screen))]
         [h-char (style-box-h style)]
         [v-char (style-box-v style)]
         [corner (style-corner style)])
        ;; Corners
        (safe-frame-set! frame x1 y1 corner config)
        (safe-frame-set! frame x2 y1 corner config)
        (safe-frame-set! frame x1 y2 corner config)
        (safe-frame-set! frame x2 y2 corner config)
        ;; Horizontal edges
        (do ([x (+ x1 1) (+ x 1)]) ((>= x x2))
            (safe-frame-set! frame x y1 h-char config)
            (safe-frame-set! frame x y2 h-char config))
        ;; Vertical edges
        (do ([y (+ y1 1) (+ y 1)]) ((>= y y2))
            (safe-frame-set! frame x1 y v-char config)
            (safe-frame-set! frame x2 y v-char config))))

(doc 'section 'debug:)

(doc 'fmod 'type 'Number x Number -> Number)
(doc "Floating-point modulo.")
(define (fmod x y)
  (- x (* (floor (/ x y)) y)))

(doc "angle->arrow-char : Number -> Char")
(doc "Map angle (radians) to arrow character.")
(define (angle->arrow-char angle)
  (let* ([pi 3.141592653589793]
         [two-pi (* 2 pi)]
         [normalized (fmod (+ angle two-pi) two-pi)]
         [octant (inexact->exact (floor (* normalized (/ 8 two-pi))))])
        (case octant
              [(0) #\>]     ; Right
              [(1) #\\]     ; Down-right
              [(2) #\v]     ; Down
              [(3) #\/]     ; Down-left
              [(4) #\<]     ; Left
              [(5) #\\]     ; Up-left
              [(6) #\^]     ; Up
              [(7) #\/]     ; Up-right
              [else #\>])))

(doc "draw-velocity-vector! : Frame x Entity x Number x RenderConfig -> Void")
(doc "Draw velocity arrow from entity center.")
(define (draw-velocity-vector! frame entity vel-scale config)
  (let* ([body (entity-body entity)]
         [pos (if (rigid-body? body) (rigid-body-pos body) (body-pos body))]
         [vel (if (rigid-body? body) (rigid-body-vel body) (body-vel body))]
         [speed (vec2-magnitude vel)])
        (when (> speed 0.5)  ; Only draw if moving significantly
              (let* ([end-pos (vec2-add pos (vec2-scale vel vel-scale))]
                     [start (world->screen pos config)]
                     [end (world->screen end-pos config)]
                     [angle (atan (vec2-y vel) (vec2-x vel))]
                     [arrow-char (angle->arrow-char angle)])
                    ;; Draw line from center toward velocity
                    (draw-line! frame (car start) (cdr start)
                                (car end) (cdr end) #\. config)
                    ;; Draw arrow head at end
                    (safe-frame-set! frame (car end) (cdr end) arrow-char config)))))

(doc 'section 'debug:)

(doc "draw-contact! : Frame x Vec2 x Vec2 x RenderConfig -> Void")
(doc "Draw contact point with normal direction indicator.")
(define (draw-contact! frame point normal config)
  (let* ([screen-pt (world->screen point config)]
         [end-pos (vec2-add point (vec2-scale normal 0.5))]
         [screen-end (world->screen end-pos config)])
        ;; Draw contact point
        (safe-frame-set! frame (car screen-pt) (cdr screen-pt) #\* config)
        ;; Draw short normal line
        (draw-line! frame (car screen-pt) (cdr screen-pt)
                    (car screen-end) (cdr screen-end) #\: config)))

(doc 'section 'debug:)

(doc "draw-constraint! : Frame x World x Constraint x RenderConfig -> Void")
(doc "Draw constraint connection between anchor points.")
(define (draw-constraint! frame world constraint config)
  (let* ([pos-a (constraint-anchor-world-a world constraint)]
         [pos-b (constraint-anchor-world-b world constraint)]
         [screen-a (world->screen pos-a config)]
         [screen-b (world->screen pos-b config)]
         [type (constraint-type constraint)]
         [line-char (case type
                          [(distance) #\-]
                          [(spring) #\~]
                          [(revolute) #\*]
                          [(weld) #\=]
                          [else #\.])])
        ;; Draw anchor points
        (safe-frame-set! frame (car screen-a) (cdr screen-a) #\+ config)
        (safe-frame-set! frame (car screen-b) (cdr screen-b) #\+ config)
        ;; Draw connection line
        (draw-line! frame (car screen-a) (cdr screen-a)
                    (car screen-b) (cdr screen-b) line-char config)))

(doc 'section 'entity)

(doc 'get-entity-shape-world 'type 'Entity -> Shape)
(doc "Get entity's shape in world coordinates.")
(define (get-entity-shape-world entity)
  (let* ([shape (entity-shape entity)]
         [body (entity-body entity)]
         [pos (if (rigid-body? body) (rigid-body-pos body) (body-pos body))])
        (cond
         [(circle? shape)
          ;; Circle center is relative, translate to world pos
          (make-circle pos (circle-radius shape))]
         [(aabb? shape)
          ;; Translate AABB by entity position
          (let ([half-w (/ (- (vec2-x (aabb-max shape)) (vec2-x (aabb-min shape))) 2)]
                [half-h (/ (- (vec2-y (aabb-max shape)) (vec2-y (aabb-min shape))) 2)])
               (make-aabb (vec2 (- (vec2-x pos) half-w) (- (vec2-y pos) half-h))
                          (vec2 (+ (vec2-x pos) half-w) (+ (vec2-y pos) half-h))))]
         [(polygon? shape)
          ;; Translate polygon vertices
          (make-polygon (map (lambda (v) (vec2-add v pos))
                             (polygon-vertices shape)))]
         [else shape])))

(doc "render-entity-shape! : Frame x Entity x Char x RenderConfig -> Void")
(doc "Render an entity's shape with given character.")
(define (render-entity-shape! frame entity char config)
  (let ([shape (get-entity-shape-world entity)])
       (cond
        [(circle? shape)
         (let* ([center (circle-center shape)]
                [radius (circle-radius shape)]
                [screen (world->screen center config)]
                [screen-radius (max 1 (inexact->exact (round (* radius (config-scale config)))))])
               (draw-filled-circle! frame (car screen) (cdr screen)
                                    screen-radius char config))]
        [(aabb? shape)
         (draw-filled-aabb! frame shape char config)]
        [(polygon? shape)
         (draw-filled-polygon! frame (polygon-vertices shape) char config)])))

(doc 'section 'high-level)

(doc "render-world! : Frame x World x WorldRenderer x (Entity -> Char) -> Void")
(doc "Render entire world with optional debug overlays.")
(doc "entity-char-fn: function mapping entity to display character")
(define (render-world! frame world renderer entity-char-fn)
  (let ([config (renderer-config renderer)]
        [debug (renderer-debug renderer)]
        [style (renderer-style renderer)])
       
       ;; 1. Render all entities
       (for-each
        (lambda (entity)
                (let* ([is-static (entity-static? entity)]
                       [char (if entity-char-fn
                                 (entity-char-fn entity)
                                 (if is-static
                                     (style-static-char style)
                                     (style-fill-char style)))])
                      (render-entity-shape! frame entity char config)))
        (world-entity-list world))
       
       ;; 2. Optional: Render constraints
       (when (debug-show-constraints debug)
             (for-each
              (lambda (c)
                      (draw-constraint! frame world c config))
              (world-constraint-list world)))
       
       ;; 3. Optional: Render velocity vectors
       (when (debug-show-velocity debug)
             (for-each
              (lambda (entity)
                      (unless (entity-static? entity)
                              (draw-velocity-vector! frame entity 0.1 config)))
              (world-entity-list world)))
       
       ;; 4. Optional: Render AABBs
       (when (debug-show-aabb debug)
             (for-each
              (lambda (entity)
                      (let ([shape (get-entity-shape-world entity)])
                           (draw-aabb-outline! frame (shape-aabb shape) style config)))
              (world-entity-list world)))))

(doc 'section 'video)

(doc 'record-physics-video 'type 'World x WorldRenderer x Int x Int x Number x (Entity -> Char) -> Video)
(doc "Record physics simulation as video.")
(define (record-physics-video world renderer num-frames steps-per-frame dt entity-char-fn)
  (let* ([config (renderer-config renderer)]
         [video (make-video)]
         [frame (make-frame (config-width config) (config-height config) #\space)])
        (do ([i 0 (+ i 1)])
            ((>= i num-frames) video)
            ;; Step physics
            (do ([s 0 (+ s 1)])
                ((>= s steps-per-frame))
                (world-step! world dt))
            ;; Clear and render frame
            (frame-clear! frame #\space)
            (render-world! frame world renderer entity-char-fn)
            ;; Add to video
            (video-add-frame! video frame))))

(doc 'section 'convenience)

(doc "render-world-simple! : Frame x World x RenderConfig -> Void")
(doc "Simple render without debug overlays.")
(define (render-world-simple! frame world config)
  (let ([renderer (make-world-renderer config (default-debug-options) (default-render-style))])
       (render-world! frame world renderer #f)))

(doc 'quick-render 'type 'World x Int x Int -> String)
(doc "Quick single-frame render to string.")
(define (quick-render world width height)
  (let* ([config (make-render-config width height 3.0 (/ width 2) (/ height 4))]
         [frame (make-frame width height #\space)])
        (render-world-simple! frame world config)
        (frame-render-to-string frame)))
