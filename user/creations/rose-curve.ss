;;; rose-curve.ss — Mathematical rose curve using polar coordinates
;;;
;;; Creates a colorful rose pattern using the equation r = cos(k*theta)
;;; where k determines the number of petals

;;; Load turtle graphics
(load "shell/ui/turtle-color.ss")
(load "shell/ui/turtle-path.ss")
(load "shell/ui/turtle.ss")
(load "shell/ui/turtle-svg.ss")

;;; Helper: Draw a rose petal curve
(define (rose-petal t n k scale)
  (if (<= n 0)
      t
      (let* ([theta (* n (/ 3.14159 180))]  ; Convert to radians
             [r (* scale (abs (cos (* k theta))))]
             [col-val (mod (* n 13) 4096)]
             [col (color12-from-int col-val)]
             [t (setpc t col)]
             [t (pd t)]
             [t (fd t (/ r 10))]
             [t (rt t 1)])
            (rose-petal t (- n 1) k scale))))

;;; Create multiple rose patterns with different petal counts
(define rose-turtle
  (let* ([t (make-turtle)]
         [t (setxy t 320 240)]  ; Center
         [t (pu t)])
        ;; 3-petal rose
        (let* ([t (setxy t 320 240)]
               [t (rose-petal t 1080 1.5 100)])
              t)))

;;; Export
(save-svg (turtle->drawing rose-turtle) "user/creations/rose-curve.svg")
(display "✓ Created rose-curve.svg with mathematical rose pattern\n")
