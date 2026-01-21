;;; user/demos/mesh-terrain-anim.ss --- DEMOSCENE: Delaunay Mesh Animation
;;;
;;; A proper demoscene production showcasing mesh generation:
;;; Random points → Delaunay triangulation → Terrain surface → 3D render
;;;
;;; Output: user/demos/mesh-terrain.gif (800x600, ~6 seconds @ 12fps)

(load "lattice/geometry/mesh-gen.ss")
(load "lattice/geometry/mesh-sdf.ss")
(load "lattice/geometry/geometry.ss")
(load "user/creations/ascii-video.ss")
(load "user/creations/ascii-video-export.ss")

;;; ============================================================
;;; DEMOSCENE CONFIGURATION
;;; ============================================================

;;; Resolution: 800x600 (100 cols × 42 rows, 8x14 font)
(define WIDTH 100)
(define HEIGHT 42)

;;; Timing: 12fps feels right for ASCII (not too smooth, has character)
(define FPS 12)
(define FRAME-DELAY-MS 83)  ; ~1000/12

;;; Phase durations (in frames)
(define INTRO-FRAMES 36)     ; 3s intro
(define BUILD-FRAMES 36)     ; 3s buildup
(define SPIN-FRAMES 36)      ; 3s main spin
(define OUTRO-FRAMES 12)     ; 1s outro
(define TOTAL-FRAMES (+ INTRO-FRAMES BUILD-FRAMES SPIN-FRAMES OUTRO-FRAMES))

;;; Mesh generation parameters
(define NUM-POINTS 30)
(define TERRAIN-SIZE 2.5)
(define CAMERA-DISTANCE 4.5)

;;; Render dimensions
(define RENDER-WIDTH 80)
(define RENDER-HEIGHT 34)

;;; ============================================================
;;; COMPUTE THE MESH (once, before animation)
;;; ============================================================

(display "═══════════════════════════════════════════════════════════\n")
(display "  MESH GENERATION DEMO - Delaunay Terrain\n")
(display "═══════════════════════════════════════════════════════════\n\n")

(display "Generating random points...\n")
(define half-size (/ TERRAIN-SIZE 2))
(define pts (random-points-in-rect (- half-size) half-size
                                    (- half-size) half-size
                                    NUM-POINTS))
(display (format "  ~a points generated\n" (length pts)))

(display "Computing Delaunay triangulation...\n")
(define tris-2d (delaunay-triangulate pts))
(display (format "  ~a triangles\n" (length tris-2d)))

(display "Refining mesh (Ruppert's algorithm)...\n")
(define refined-2d (refine-mesh tris-2d 25 30))
(display (format "  ~a triangles after refinement\n" (length refined-2d)))

;;; Height function: dramatic central peak + rolling hills
(define (terrain-height p)
  (let* ([x (point2-x p)]
         [y (point2-y p)]
         [r (sqrt (+ (* x x) (* y y)))]
         [peak (* 0.8 (exp (- (* r r 0.8))))]
         [hills (* 0.25 (+ (sin (* x 3.0))
                           (sin (* y 3.5))
                           (* 0.5 (sin (* (+ x y) 2.0)))))])
    (+ peak hills)))

(display "Converting to 3D terrain...\n")
(define tris-3d (triangles-to-3d refined-2d terrain-height))

(display "Building mesh with BVH...\n")
(define terrain-mesh (make-mesh tris-3d))
(display "  Mesh ready!\n\n")

;;; ============================================================
;;; RENDERING (no ANSI codes, just ASCII chars)
;;; ============================================================

;;; Simple raymarching render without ANSI codes
(define (render-mesh-simple mesh angle)
  "Render mesh at given angle to ASCII art (no colors)"
  (let* ([bounds (mesh-bounds mesh)]
         [extents (aabb-extents bounds)]
         [max-extent (max (vec3-x extents) (vec3-y extents) (vec3-z extents))]
         [cam-distance (if (> CAMERA-DISTANCE 0) CAMERA-DISTANCE (* max-extent 3))]
         [cam-height (* max-extent 2.5)]
         ;; Camera orbits around origin at fixed distance
         [cam-x (* cam-distance (sin angle))]
         [cam-z (* cam-distance (cos angle))]
         [cam-pos (vec3 cam-x cam-height cam-z)]
         ;; Camera looks at origin
         [look-at (vec3 0 0 0)]
         [up (vec3 0 1 0)]
         ;; Compute camera basis vectors
         [forward (vec3-normalize (vec3-sub look-at cam-pos))]
         [right (vec3-normalize (vec3-cross forward up))]
         [cam-up (vec3-cross right forward)]
         ;; Field of view
         [fov 0.8]
         [aspect (/ RENDER-WIDTH RENDER-HEIGHT 2.0)]
         [half-height (tan (/ fov 2))]
         [half-width (* half-height aspect)]
         [light-dir (vec3-normalize (vec3 0.5 1.0 -0.3))]
         [frame (make-frame RENDER-WIDTH RENDER-HEIGHT #\space)])

    ;; Raycast each pixel
    (do ([y 0 (+ y 1)])
        ((>= y RENDER-HEIGHT))
        (do ([x 0 (+ x 1)])
            ((>= x RENDER-WIDTH))
            ;; Normalized device coordinates [-1, 1]
            (let* ([u (- (* 2.0 (/ x RENDER-WIDTH)) 1.0)]
                   [v (- 1.0 (* 2.0 (/ y RENDER-HEIGHT)))]
                   ;; Ray direction in camera space, transformed to world
                   [dir-x (* u half-width)]
                   [dir-y (* v half-height)]
                   [ray-dir (vec3-normalize
                             (vec3-add forward
                                       (vec3-add (vec3-scale right dir-x)
                                                 (vec3-scale cam-up dir-y))))]
                   [ray (ray3 cam-pos ray-dir)])
              ;; Ray-mesh intersection
              (let ([hit (mesh-intersect-ray mesh ray)])
                (when hit
                  (let* ([triangle (caddr hit)]
                         [v0 (triangle3-p1 triangle)]
                         [v1 (triangle3-p2 triangle)]
                         [v2 (triangle3-p3 triangle)]
                         [edge1 (vec3-sub v1 v0)]
                         [edge2 (vec3-sub v2 v0)]
                         [normal (vec3-normalize (vec3-cross edge1 edge2))]
                         ;; Ensure normal points toward camera
                         [normal (if (< (vec3-dot normal (vec3-sub cam-pos v0)) 0)
                                     (vec3-scale normal -1)
                                     normal)]
                         ;; Lambertian shading
                         [brightness (max 0.1 (vec3-dot normal light-dir))]
                         [char-ramp " .:-=+*#%@"]
                         [idx (min 9 (max 0 (inexact->exact (floor (* brightness 10)))))])
                    (frame-set! frame x y (string-ref char-ramp idx))))))))
    frame))

;;; Helper: string-split
(define (string-split str delim)
  "Split string on delimiter character"
  (let loop ([i 0] [start 0] [result '()])
    (cond
      [(>= i (string-length str))
       (reverse (cons (substring str start i) result))]
      [(char=? (string-ref str i) delim)
       (loop (+ i 1) (+ i 1) (cons (substring str start i) result))]
      [else
       (loop (+ i 1) start result)])))

;;; ============================================================
;;; FRAME GENERATORS
;;; ============================================================

;;; Title frame: Clean intro
(define (make-title-frame)
  (let ([frame (make-frame WIDTH HEIGHT #\space)])
    ;; Title box (centered)
    (frame-put-string! frame 17 10 "╔════════════════════════════════════════════════════════════════╗")
    (frame-put-string! frame 17 11 "║                                                                ║")
    (frame-put-string! frame 17 12 "║               DELAUNAY MESH GENERATION                         ║")
    (frame-put-string! frame 17 13 "║                                                                ║")
    (frame-put-string! frame 17 14 "║          Random Points → Triangulation → 3D Terrain           ║")
    (frame-put-string! frame 17 15 "║                                                                ║")
    (frame-put-string! frame 17 16 "╚════════════════════════════════════════════════════════════════╝")

    ;; Subtitle
    (frame-put-string! frame 22 19 "Bowyer-Watson Triangulation + Ruppert Refinement")

    frame))

;;; 2D mesh view
(define (make-2d-mesh-frame)
  (let* ([frame (make-frame WIDTH HEIGHT #\space)]
         [mesh-str (render-mesh-2d refined-2d 70 30)]
         [lines (string-split mesh-str #\newline)])

    ;; Header
    (frame-put-string! frame 15 2 "2D Triangulation View")
    (frame-put-string! frame 10 3 "────────────────────────────────────────────────────────────────────────")

    ;; Render mesh (centered)
    (let loop ([y 5] [ls lines])
      (when (and (pair? ls) (< y (- HEIGHT 3)))
        (let ([line (car ls)])
          (when (< (string-length line) WIDTH)
            (frame-put-string! frame (quotient (- WIDTH (string-length line)) 2)
                              y line)))
        (loop (+ y 1) (cdr ls))))

    ;; Footer
    (frame-put-string! frame 10 (- HEIGHT 3) "────────────────────────────────────────────────────────────────────────")
    (frame-put-string! frame 30 (- HEIGHT 1)
                      (format "~a points → ~a triangles" NUM-POINTS (length refined-2d)))

    frame))

;;; Generate spinning 3D frames
(define (make-3d-frames count)
  (display "Generating 3D animation frames...\n")
  (let ([frames '()])
    (do ([i 0 (+ i 1)])
        ((>= i count))
        (let* ([angle (* 2.0 3.141592653589793 (/ i count))]
               [render (render-mesh-simple terrain-mesh angle)]
               [final (make-frame WIDTH HEIGHT #\space)])
          ;; Title
          (frame-put-string! final 30 1 "3D Terrain - Raymarched with BVH")
          (frame-put-string! final 5 2 "────────────────────────────────────────────────────────────────────────────────────────")

          ;; Copy rendered mesh to final frame (centered)
          (let ([offset-x (quotient (- WIDTH RENDER-WIDTH) 2)]
                [offset-y 4])
            (do ([y 0 (+ y 1)])
                ((>= y RENDER-HEIGHT))
                (do ([x 0 (+ x 1)])
                    ((>= x RENDER-WIDTH))
                    (let ([ch (frame-ref render x y)])
                      (when (not (char=? ch #\space))
                        (frame-set! final (+ offset-x x) (+ offset-y y) ch))))))

          (set! frames (cons final frames))
          (when (= (modulo i 5) 0)
            (display (format "  Frame ~a/~a\n" (+ i 1) count)))))
    (display (format "  ~a frames rendered\n" count))
    (reverse frames)))

;;; Outro credits
(define (make-outro-frame)
  (let ([frame (make-frame WIDTH HEIGHT #\space)])
    ;; Centered credits
    (frame-put-string! frame 35 17 "── greets to ──")
    (frame-put-string! frame 17 19 "The Fold  ·  Anthropic  ·  Future Crew  ·  All ASCII Artists")

    (frame-put-string! frame 33 23 "═══════════════════════════")
    (frame-put-string! frame 38 24 "code: DEMOSCENE")
    (frame-put-string! frame 35 25 "math: The Lattice")
    (frame-put-string! frame 33 26 "═══════════════════════════")

    frame))

;;; ============================================================
;;; BUILD THE VIDEO
;;; ============================================================

(display "\nBuilding animation sequence...\n")

(define video (make-video))

;; Phase 1: INTRO (title card)
(display "  Phase 1: Intro\n")
(let ([title (make-title-frame)])
  (do ([i 0 (+ i 1)])
      ((>= i INTRO-FRAMES))
      (video-add-frame! video title)))

;; Phase 2: BUILD (show 2D mesh)
(display "  Phase 2: Build\n")
(let ([mesh-2d (make-2d-mesh-frame)])
  (do ([i 0 (+ i 1)])
      ((>= i BUILD-FRAMES))
      (video-add-frame! video mesh-2d)))

;; Phase 3: SPIN (main 3D animation)
(display "  Phase 3: Spin (rendering...)\n")
(let ([spin-frames (make-3d-frames SPIN-FRAMES)])
  (for-each (lambda (f) (video-add-frame! video f)) spin-frames))

;; Phase 4: OUTRO (credits)
(display "  Phase 4: Outro\n")
(let ([credits (make-outro-frame)])
  (do ([i 0 (+ i 1)])
      ((>= i OUTRO-FRAMES))
      (video-add-frame! video credits)))

(display (format "\nTotal frames: ~a (~a seconds @ ~afps)\n"
                 (video-frame-count video)
                 (/ (video-frame-count video) FPS)
                 FPS))

;;; ============================================================
;;; EXPORT TO GIF
;;; ============================================================

(display "\nExporting to GIF...\n")
(video->gif video "user/demos/mesh-terrain.gif" FRAME-DELAY-MS)

(display "\n═══════════════════════════════════════════════════════════\n")
(display "  Animation complete!\n")
(display "  Output: user/demos/mesh-terrain.gif\n")
(display "  Resolution: 800x600 (100×42 chars, 8×14 font)\n")
(display (format "  Duration: ~a seconds @ ~afps\n"
                 (/ TOTAL-FRAMES FPS) FPS))
(display "═══════════════════════════════════════════════════════════\n")
