;;; mitosis.ss --- "Mitosis"
;;;
;;; My original scene showcasing the new DSL features:
;;; - dynamic-blob with merging/separating centers (cell division)
;;; - group transforms (membrane rotating as one unit)
;;; - :up parameter for oriented rings (orthogonal membrane structure)
;;;
;;; Visual: Two cells that merge, become one, then divide again -
;;; surrounded by a protective membrane of orthogonal rotating rings.

(load "user/creations/loop-scene.ss")

(display "\n")
(display "============================================================\n")
(display "  MITOSIS\n")
(display "  Cell division with dynamic blobs and oriented rings\n")
(display "============================================================\n\n")

;;; Helper: create the dynamic center function for cell division
;;; At t=0: two cells spread apart
;;; At t=π: cells merged into one
;;; At t=2π: back to two cells (seamless loop)
(define (make-cell-centers)
  (lambda (t)
          ;; spread goes from 1 (apart) to 0 (merged) to 1 (apart)
          (let* ([spread (+ 0.1 (* 0.7 (/ (+ 1 (cos t)) 2)))]
                 ;; Also add slight vertical oscillation
                 [y-offset (* 0.15 (sin (* t 2)))])
                (list (vec3 spread y-offset 0)
                      (vec3 (- spread) (- y-offset) 0)))))

(define mitosis-scene
  (build-loop-scene
   `((duration . 5.0)
     (fps . 30)
     (width . 100)
     (height . 50)
     (camera . ,(look-at '(0 0 -5) '(0 0 0))))
   
   ;; THE CELLS: Two blobs that merge and separate
   (dynamic-blob
    :center-fn (make-cell-centers)
    :r 0.55
    :k-base 0.5
    :k-range 0.2
    :cycles 2)
   
   ;; THE MEMBRANE: Three orthogonal rings forming a cage
   ;; Using :up to orient them before animation
   (group
    :elements (list
               ;; XY plane ring (default, up=Z)
               (ring :R 1.8 :r 0.03 :up '(0 0 1) :axis '(0 0 1) :cycles 3)
               ;; XZ plane ring (up=Y)
               (ring :R 1.8 :r 0.03 :up '(0 1 0) :axis '(0 1 0) :cycles 2)
               ;; YZ plane ring (up=X)
               (ring :R 1.8 :r 0.03 :up '(1 0 0) :axis '(1 0 0) :cycles 5))
    ;; The whole membrane slowly rotates together
    :rotate '(1 1 1)
    :cycles 1)
   
   ;; OUTER SHELL: A larger pulsing ring
   (ring :R 2.3 :r 0.02 :axis '(0.5 1 0.3) :cycles 2)
   ))

(display "Rendering Mitosis...\n\n")

;;; Render to GIF
(render-loop! mitosis-scene "user/creations/mitosis.gif")

(display "\nDone! Checking output...\n")
(system "ls -lh user/creations/mitosis.gif")

;;; Post to Discord
(display "\nPosting to Discord #art...\n")
(load "shell/discord/post-with-files.ss")

(discord-post-files 'art
                    "Mitosis (Original Scene)"
                    "Cell division animation showcasing new DSL features: dynamic-blob with merging/separating centers, group transforms for the membrane cage, :up parameter for orthogonal ring orientations. 100x50 ASCII at 30fps, 5-second loop. The cells merge at the midpoint then divide again."
                    '("user/creations/mitosis.gif"))

(display "\n")
(display "============================================================\n")
(display "  Posted to Discord #art!\n")
(display "============================================================\n")
