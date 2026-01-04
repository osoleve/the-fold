;;; Quick test of video export
(load "user/creations/ascii-video-export.ss")
(load "user/creations/fractal-ascii.ss")

(display "\nGenerating Julia set animation...\n")

(define julia-video
  (animate-julia-sweep 54 22  ; Discord-sized
                       20     ; 20 frames for quick test
                       (complex -0.7 0.27)  ; Center
                       0.12   ; Radius of sweep
                       35))   ; Max iterations

(display "Animation has ")
(display (video-frame-count julia-video))
(display " frames\n\n")

;; Export to GIF
(video->gif julia-video "user/creations/julia-sweep.gif" 100)
