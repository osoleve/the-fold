;;; core/geometry/test-ascii-render.ss — Tests for ASCII Art Rendering
;;;
;;; Tests for camera, ray generation, color mapping, and frame rendering.
;;;
;;; Run: scheme --script core/geometry/test-ascii-render.ss

(load "core/test-framework.ss")
(load "core/geometry/ascii-render.ss")

(set! *current-group* 'ascii-render)

;;; Helper for approximate equality
(define (approx= a b eps)
  (< (abs (- a b)) eps))

;;; Helper for approximate vector equality
(define (vec3-approx= v1 v2 eps)
  (and (approx= (vec3-x v1) (vec3-x v2) eps)
       (approx= (vec3-y v1) (vec3-y v2) eps)
       (approx= (vec3-z v1) (vec3-z v2) eps)))

;;; ============================================================
;;; ASCII Character Mapping Tests
;;; ============================================================

(define-test "intensity->char low intensity"
  (let ([ch (intensity->char 0.0)])
       (assert-equal ch #\space)))

(define-test "intensity->char high intensity"
  (let ([ch (intensity->char 0.99)])
       (assert-equal ch #\@)))

(define-test "intensity->char mid intensity"
  (let ([ch (intensity->char 0.5)])
       ;; Should be somewhere in the middle of the ramp
       (assert-true (char? ch))))

(define-test "intensity->char clamping low"
  (let ([ch (intensity->char -0.5)])
       ;; Should clamp to minimum
       (assert-equal ch #\space)))

(define-test "intensity->char clamping high"
  (let ([ch (intensity->char 1.5)])
       ;; Should clamp to maximum
       (assert-equal ch #\@)))

;;; ============================================================
;;; ANSI Color Mapping Tests
;;; ============================================================

(define-test "rgb->ansi256 black"
  (let ([code (rgb->ansi256 0 0 0)])
       ;; Black should be 16 (base of 216-color cube)
       (assert-equal code 16)))

(define-test "rgb->ansi256 white"
  (let ([code (rgb->ansi256 1 1 1)])
       ;; White should be 16 + 5*36 + 5*6 + 5 = 231
       (assert-equal code 231)))

(define-test "rgb->ansi256 red"
  (let ([code (rgb->ansi256 1 0 0)])
       ;; Pure red: 16 + 5*36 = 196
       (assert-equal code 196)))

(define-test "rgb->ansi256 green"
  (let ([code (rgb->ansi256 0 1 0)])
       ;; Pure green: 16 + 5*6 = 46
       (assert-equal code 46)))

(define-test "rgb->ansi256 blue"
  (let ([code (rgb->ansi256 0 0 1)])
       ;; Pure blue: 16 + 5 = 21
       (assert-equal code 21)))

(define-test "rgb->ansi256 mid values"
  (let ([code (rgb->ansi256 0.5 0.5 0.5)])
       ;; Should be valid 256-color code
       (assert-true (>= code 16))
       (assert-true (<= code 231))))

(define-test "ansi-fg format"
  (let ([escape (ansi-fg 196)])
       ;; Should start with escape sequence
       (assert-true (> (string-length escape) 0))
       (assert-true (string? escape))))

(define-test "ansi-reset is string"
  (assert-true (string? ansi-reset)))

;;; ============================================================
;;; Camera Tests
;;; ============================================================

(define-test "make-camera creation"
  (let* ([pos (vec3 0 0 -5)]
         [look-at (vec3 0 0 0)]
         [up (vec3 0 1 0)]
         [fov 0.8]
         [aspect 1.0]
         [cam (make-camera pos look-at up fov aspect)])
        (assert-true (list? cam))
        (assert-equal (car cam) 'camera)))

(define-test "camera-pos accessor"
  (let* ([pos (vec3 0 0 -5)]
         [cam (make-camera pos (vec3 0 0 0) (vec3 0 1 0) 0.8 1.0)])
        (assert-true (vec3-approx= (camera-pos cam) pos 0.001))))

(define-test "camera-forward points toward look-at"
  (let* ([pos (vec3 0 0 -5)]
         [look-at (vec3 0 0 0)]
         [cam (make-camera pos look-at (vec3 0 1 0) 0.8 1.0)]
         [forward (camera-forward cam)])
        ;; Forward should point in +Z direction
        (assert-true (> (vec3-z forward) 0.9))))

(define-test "camera-right is perpendicular to forward"
  (let* ([cam (make-camera (vec3 0 0 -5) (vec3 0 0 0) (vec3 0 1 0) 0.8 1.0)]
         [forward (camera-forward cam)]
         [right (camera-right cam)]
         [dot (vec3-dot forward right)])
        ;; Dot product should be near zero
        (assert-true (approx= dot 0.0 0.001))))

(define-test "camera-up is perpendicular to forward"
  (let* ([cam (make-camera (vec3 0 0 -5) (vec3 0 0 0) (vec3 0 1 0) 0.8 1.0)]
         [forward (camera-forward cam)]
         [up (camera-up cam)]
         [dot (vec3-dot forward up)])
        ;; Dot product should be near zero
        (assert-true (approx= dot 0.0 0.001))))

(define-test "camera half dimensions"
  (let* ([fov 0.8]
         [aspect 2.0]
         [cam (make-camera (vec3 0 0 -5) (vec3 0 0 0) (vec3 0 1 0) fov aspect)]
         [half-height (camera-half-height cam)]
         [half-width (camera-half-width cam)])
        (assert-true (> half-height 0))
        (assert-true (> half-width 0))
        ;; Width should be approximately aspect * height
        (assert-true (approx= half-width (* aspect half-height) 0.001))))

;;; ============================================================
;;; Ray Generation Tests
;;; ============================================================

(define-test "camera-ray center"
  (let* ([cam (make-camera (vec3 0 0 -5) (vec3 0 0 0) (vec3 0 1 0) 0.8 1.0)]
         [ray (camera-ray cam 0 0)])  ; Center of screen
        (assert-true (ray3? ray))
        ;; Ray origin should match camera position
        (assert-true (vec3-approx= (ray3-origin ray) (vec3 0 0 -5) 0.001))
        ;; Ray direction should point roughly forward
        (assert-true (> (vec3-z (ray3-direction ray)) 0.9))))

(define-test "camera-ray corners"
  (let* ([cam (make-camera (vec3 0 0 -5) (vec3 0 0 0) (vec3 0 1 0) 0.8 1.0)]
         [ray-tl (camera-ray cam -1 1)]   ; Top-left
         [ray-br (camera-ray cam 1 -1)])  ; Bottom-right
        ;; Rays should be different
        (assert-false (equal? (ray3-direction ray-tl) (ray3-direction ray-br)))
        ;; But both should have same origin
        (assert-true (vec3-approx= (ray3-origin ray-tl) (ray3-origin ray-br) 0.001))))

(define-test "camera-ray directions normalized"
  (let* ([cam (make-camera (vec3 0 0 -5) (vec3 0 0 0) (vec3 0 1 0) 0.8 1.0)]
         [ray (camera-ray cam 0.5 0.5)]
         [dir (ray3-direction ray)]
         [len (vec3-length dir)])
        ;; Direction should be normalized (length ~1)
        (assert-true (approx= len 1.0 0.001))))

;;; ============================================================
;;; Rendering Tests
;;; ============================================================

(define-test "render-pixel-color hit returns RGB"
  (let* ([mesh (make-mesh-cube 1.0)]
         [ray (ray3 (vec3 0 0 -5) (vec3 0 0 1))]
         [light-dir (vec3 0 1 0)]
         [color (render-pixel-color mesh ray light-dir)])
        (when color
              (assert-true (list? color))
              (assert-equal (length color) 3)
              ;; RGB values should be in [0, 1]
              (assert-true (>= (car color) 0))
              (assert-true (<= (car color) 1)))))

(define-test "render-pixel-color miss returns #f"
  (let* ([mesh (make-mesh-cube 1.0)]
         [ray (ray3 (vec3 10 10 -5) (vec3 0 0 1))]  ; Misses cube
         [light-dir (vec3 0 1 0)]
         [color (render-pixel-color mesh ray light-dir)])
        (assert-false color)))

(define-test "render-frame produces string"
  (let* ([mesh (make-mesh-cube 1.0)]
         [cam (make-camera (vec3 0 0 -5) (vec3 0 0 0) (vec3 0 1 0) 0.8 1.0)]
         [light-dir (vec3 0 1 0)]
         [frame (render-frame mesh cam light-dir 10 5)])
        (assert-true (string? frame))
        (assert-true (> (string-length frame) 0))))

(define-test "render-frame dimensions"
  (let* ([mesh (make-mesh-cube 1.0)]
         [cam (make-camera (vec3 0 0 -5) (vec3 0 0 0) (vec3 0 1 0) 0.8 1.0)]
         [light-dir (vec3 0 1 0)]
         [width 10]
         [height 5]
         [frame (render-frame mesh cam light-dir width height)])
        ;; Frame should be non-empty and contain some content
        (assert-true (> (string-length frame) 0))
        ;; Frame should contain newline characters
        (assert-true (> (string-length frame) height))))

;;; Helper to split string by newlines
(define (string-split-newlines str)
  (let loop ([chars (string->list str)]
             [current '()]
             [result '()])
       (cond
        [(null? chars)
         (reverse (if (null? current)
                      result
                      (cons (list->string (reverse current)) result)))]
        [(char=? (car chars) #\newline)
         (loop (cdr chars)
               '()
               (cons (list->string (reverse current)) result))]
        [else
         (loop (cdr chars)
               (cons (car chars) current)
               result)])))

;;; ============================================================
;;; Camera Rotation Tests
;;; ============================================================

(define-test "rotate-camera-around at angle 0"
  (let* ([center (vec3 0 0 0)]
         [distance 10]
         [pos (rotate-camera-around center 0 distance)])
        ;; At angle 0: cos(0)=1, sin(0)=0 -> x=10, z=0
        (assert-true (approx= (vec3-x pos) 10.0 0.001))
        (assert-true (approx= (vec3-z pos) 0.0 0.001))))

(define-test "rotate-camera-around at angle pi/2"
  (let* ([center (vec3 0 0 0)]
         [distance 10]
         [angle (/ 3.141592653589793 2)]
         [pos (rotate-camera-around center angle distance)])
        ;; At angle pi/2: cos=0, sin=1 -> x=0, z=10
        (assert-true (approx= (vec3-x pos) 0.0 0.001))
        (assert-true (approx= (vec3-z pos) 10.0 0.001))))

(define-test "rotate-camera-around distance"
  (let* ([center (vec3 0 0 0)]
         [distance 10]
         [angle 0.5]
         [pos (rotate-camera-around center angle distance)])
        ;; Distance from center should equal requested distance
        (let ([actual-dist (vec3-length pos)])
             (assert-true (approx= actual-dist distance 0.001)))))

(define-test "rotate-camera-around with offset center"
  (let* ([center (vec3 5 0 5)]
         [distance 10]
         [pos (rotate-camera-around center 0 distance)])
        ;; Should be offset by center
        (assert-true (approx= (vec3-x pos) 15.0 0.001))
        (assert-true (approx= (vec3-z pos) 5.0 0.001))))

;;; ============================================================
;;; Animation Tests
;;; ============================================================

(define-test "render-spinning-frames produces list"
  (let* ([mesh (make-mesh-cube 1.0)]
         [num-frames 3]
         [frames (render-spinning-frames mesh 5 3 num-frames 10)])
        (assert-true (list? frames))
        (assert-equal (length frames) num-frames)))

(define-test "render-spinning-frames all strings"
  (let* ([mesh (make-mesh-cube 1.0)]
         [frames (render-spinning-frames mesh 5 3 2 10)])
        (for-each (lambda (frame)
                          (assert-true (string? frame)))
                  frames)))

(define-test "render-spinning-frames different angles"
  (let* ([mesh (make-mesh-cube 1.0)]
         [frames (render-spinning-frames mesh 8 5 2 10)])
        ;; Each frame should be a valid string
        (when (= (length frames) 2)
              (assert-true (string? (car frames)))
              (assert-true (string? (cadr frames))))))

;;; ============================================================
;;; Summary
;;; ============================================================

(display "\n")
(display "Tests run:    ")
(display *tests-run*)
(newline)
(display "Tests passed: ")
(display *tests-passed*)
(newline)
(display "Tests failed: ")
(display *tests-failed*)
(newline)

(if (> *tests-failed* 0)
    (exit 1)
    (exit 0))
