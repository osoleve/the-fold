;;; Proof-of-Concept: Swappable Rendering Backends for Loop-Scene
;;;
;;; Demonstrates how to decouple scene definition from rendering engine.

;;; ============================================================
;;; Backend Interface
;;; ============================================================

(define-record-type backend
  (fields name           ; Symbol: 'ascii-raymarch, 'direct-raster, etc.
          supports       ; List of symbols: '(gif mp4 webm)
          render-proc))  ; (scene params output-path) -> void

(define *registered-backends* '())

(define (register-backend! backend)
  (set! *registered-backends*
        (cons (cons (backend-name backend) backend)
              *registered-backends*)))

(define (get-backend name)
  (let ([entry (assq name *registered-backends*)])
       (if entry
           (cdr entry)
           (error 'get-backend "Unknown backend" name))))

(define (list-backends)
  (map car *registered-backends*))

;;; ============================================================
;;; Backend Implementations
;;; ============================================================

;;; ASCII Raymarching Backend (wraps existing implementation)
(define ascii-raymarch-backend
  (make-backend
   'ascii-raymarch
   '(gif mp4 webm ascii-txt)
   (lambda (scene params output-path)
           ;; This would call the existing render-loop! function
           (display "Rendering with ASCII raymarcher...\n")
           (display (format "  Output: ~a\n" output-path))
           (display "  [Would call existing render-loop! here]\n"))))

;;; Direct Raster Backend (hypothetical - RGB with lighting)
(define direct-raster-backend
  (make-backend
   'direct-raster
   '(png gif mp4)
   (lambda (scene params output-path)
           (display "Rendering with direct raster (RGB + lighting)...\n")
           (display (format "  Output: ~a\n" output-path))
           (display "  Steps:\n")
           (display "    1. Evaluate SDF on screen grid\n")
           (display "    2. Compute normals via gradient\n")
           (display "    3. Apply Phong shading model\n")
           (display "    4. Anti-alias via supersampling\n")
           (display "    5. Export to RGB bitmap\n")
           (display "    6. Encode with FFmpeg\n"))))

;;; SVG Frames Backend (hypothetical - marching cubes)
(define svg-frames-backend
  (make-backend
   'svg-frames
   '(svg html)
   (lambda (scene params output-path)
           (display "Rendering with marching cubes → SVG...\n")
           (display (format "  Output: ~a\n" output-path))
           (display "  Steps:\n")
           (display "    1. Evaluate SDF on 3D grid\n")
           (display "    2. Extract isosurface (marching cubes)\n")
           (display "    3. Simplify mesh (quadric edge collapse)\n")
           (display "    4. Convert triangles to SVG paths\n")
           (display "    5. Generate frame sequence or morphing\n")
           (display "    6. Wrap in HTML with JS playback\n"))))

;;; WebGL Export Backend (hypothetical - shader compilation)
(define webgl-export-backend
  (make-backend
   'webgl-export
   '(html)
   (lambda (scene params output-path)
           (display "Exporting to WebGL (three.js)...\n")
           (display (format "  Output: ~a\n" output-path))
           (display "  Steps:\n")
           (display "    1. Convert SDF to GLSL shader\n")
           (display "    2. Compile animation curves to uniforms\n")
           (display "    3. Generate three.js scene template\n")
           (display "    4. Add orbit controls + timeline scrubber\n")
           (display "    5. Bundle as standalone HTML\n"))))

;;; Register all backends
(register-backend! ascii-raymarch-backend)
(register-backend! direct-raster-backend)
(register-backend! svg-frames-backend)
(register-backend! webgl-export-backend)

;;; ============================================================
;;; User-Facing API
;;; ============================================================

(define (render-scene scene backend-name output-path . params)
  "Render a scene using the specified backend"
  (let* ([backend (get-backend backend-name)]
         [format (path->format output-path)])
        ;; Validate backend supports output format
        (unless (memq format (backend-supports backend))
                (error 'render-scene
                       (format "Backend ~a does not support format ~a"
                               backend-name format)))
        ;; Dispatch to backend
        ((backend-render-proc backend) scene params output-path)))

(define (path->format path)
  "Extract format from file extension"
  (let ([ext (path-extension path)])
       (cond
        [(string=? ext "gif") 'gif]
        [(string=? ext "mp4") 'mp4]
        [(string=? ext "webm") 'webm]
        [(string=? ext "svg") 'svg]
        [(string=? ext "html") 'html]
        [(string=? ext "png") 'png]
        [else 'unknown])))

(define (path-extension path)
  "Get file extension"
  (let ([parts (string-split path #\.)])
       (if (null? parts)
           ""
           (car (reverse parts)))))

(define (string-split str delim)
  "Split string by delimiter"
  (let loop ([chars (string->list str)]
             [current '()]
             [result '()])
       (cond
        [(null? chars)
         (reverse (cons (list->string (reverse current)) result))]
        [(char=? (car chars) delim)
         (loop (cdr chars) '() (cons (list->string (reverse current)) result))]
        [else
         (loop (cdr chars) (cons (car chars) current) result)])))

;;; ============================================================
;;; Demo
;;; ============================================================

;; Mock scene object
(define mock-scene
  (list (cons 'duration 4.0)
        (cons 'fps 30)
        (cons 'width 100)
        (cons 'height 50)))

(define (demo)
  (display "\n")
  (display "╔══════════════════════════════════════════════════════════════╗\n")
  (display "║  SWAPPABLE RENDERING BACKENDS - PROOF OF CONCEPT            ║\n")
  (display "╚══════════════════════════════════════════════════════════════╝\n")
  (display "\n")
  
  (display "Registered backends:\n")
  (for-each (lambda (name)
                    (let ([backend (get-backend name)])
                         (display (format "  • ~a - supports: ~a\n"
                                          name
                                          (backend-supports backend)))))
            (list-backends))
  (display "\n")
  
  (display "Example: Rendering the same scene with different backends\n\n")
  
  (display "1. ")
  (render-scene mock-scene 'ascii-raymarch "output.gif")
  (display "\n")
  
  (display "2. ")
  (render-scene mock-scene 'direct-raster "output.mp4")
  (display "\n")
  
  (display "3. ")
  (render-scene mock-scene 'svg-frames "output.html")
  (display "\n")
  
  (display "4. ")
  (render-scene mock-scene 'webgl-export "interactive.html")
  (display "\n")
  
  (display "╔══════════════════════════════════════════════════════════════╗\n")
  (display "║  KEY INSIGHT                                                 ║\n")
  (display "╚══════════════════════════════════════════════════════════════╝\n")
  (display "\n")
  (display "The scene definition is UNCHANGED across all backends!\n")
  (display "The integer cycle guarantee works regardless of renderer.\n")
  (display "Users choose output format based on use case:\n")
  (display "  • ASCII GIF     → Quick preview, distinctive aesthetic\n")
  (display "  • Direct MP4    → High fidelity, small file size\n")
  (display "  • SVG/HTML      → Web embeds, infinite scale\n")
  (display "  • WebGL         → Interactive, real-time, portfolio\n")
  (display "\n"))

(demo)
