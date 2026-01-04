;;; Generate a spinning torus GIF using the SDF raymarcher
(load "user/creations/ascii-video-export.ss")
(load "user/creations/sdf-raymarcher.ss")

(display "\nGenerating spinning torus animation...\n")

;; Create video
(define torus-video (make-video))

;; Render 30 frames of spinning torus
(do ([i 0 (+ i 1)])
    ((>= i 30))
    (let* ([angle (* i (/ 6.28318 30))]
           [scene (make-torus-scene angle)]
           [frame (render-sdf-frame scene *default-camera* 54 22)])
          (video-add-frame! torus-video frame)
          (display ".")
          (flush-output-port)))

(display "\n")
(display "Animation has ")
(display (video-frame-count torus-video))
(display " frames\n")

;; Export to GIF
(video->gif torus-video "user/creations/spinning-torus.gif" 80)
