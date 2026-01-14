;;; shell/turtle-svg.ss — SVG Generation for Turtle Graphics
;;;
;;; Converts turtle drawings to SVG format for viewing and export.
;;; Generates clean, well-formed SVG 1.1 documents.
;;;
;;; This is Shell code: pure functions for SVG string generation.
;;;
;;; Dependencies:
;;;   - shell/turtle-color.ss (for color12->svg-hex)
;;;   - shell/turtle-path.ss (for path command accessors)
;;;   - shell/ui/turtle.ss (for drawing record)

;;; NOTE: string utilities provided by core/prelude.ss
(load "core/base/prelude.ss")

;;; ====
;;; SVG Document Structure
;;; ====

;;; svg-header : Nat x Nat -> String
;;; Generate SVG document header with XML declaration.
(define (svg-header width height)
  (string-append
   "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
   "<svg xmlns=\"http://www.w3.org/2000/svg\"\n"
   "     width=\"" (number->string width) "\"\n"
   "     height=\"" (number->string height) "\"\n"
   "     viewBox=\"0 0 " (number->string width) " " (number->string height) "\">\n"))

;;; svg-footer : -> String
;;; Generate SVG document footer.
(define (svg-footer)
  "</svg>\n")

;;; svg-background : Nat x Nat x String -> String
;;; Generate background rectangle.
(define (svg-background width height color-hex)
  (string-append
   "  <rect width=\"" (number->string width) "\""
   " height=\"" (number->string height) "\""
   " fill=\"" color-hex "\"/>\n"))

;;; ====
;;; Drawing to SVG Conversion
;;; ====

;;; drawing->svg : Drawing -> String
;;; Convert a complete turtle drawing to an SVG document.
(define (drawing->svg drawing)
  (let* ([w (drawing-width drawing)]
         [h (drawing-height drawing)]
         [bg-hex (color12->svg-hex (drawing-bg-color drawing))]
         [paths (drawing-paths drawing)])
        (string-append
         (svg-header w h)
         (svg-background w h bg-hex)
         (paths->svg paths)
         (svg-footer))))

;;; ====
;;; Path Commands to SVG Elements
;;; ====

;;; paths->svg : (List PathCmd) -> String
;;; Convert a list of path commands to SVG elements.
;;; Groups consecutive line-to commands into path elements.
(define (paths->svg cmds)
  (if (null? cmds)
      ""
      (let loop ([remaining cmds]
                 [current-segment '()]
                 [current-color #f]
                 [current-width #f]
                 [result ""])
           (if (null? remaining)
               ;; Flush any remaining segment
               (string-append result (flush-segment current-segment current-color current-width))
               (let ([cmd (car remaining)]
                     [rest (cdr remaining)])
                    (cond
                     ;; Move-to: flush current segment, start new one
                     [(move-to? cmd)
                      (loop rest
                            (list cmd)
                            current-color
                            current-width
                            (string-append result
                                           (flush-segment current-segment current-color current-width)))]
                     
                     ;; Line-to: check if we can continue current segment
                     [(line-to? cmd)
                      (let ([cmd-color (line-to-color cmd)]
                            [cmd-width (line-to-width cmd)])
                           (if (or (null? current-segment)
                                   (not current-color)  ; First line in segment, adopt its color
                                   (and (equal? cmd-color current-color)
                                        (equal? cmd-width current-width)))
                               ;; Continue current segment
                               (loop rest
                                     (cons cmd current-segment)
                                     cmd-color
                                     cmd-width
                                     result)
                               ;; Color/width changed, flush and start new
                               (loop rest
                                     (list cmd)
                                     cmd-color
                                     cmd-width
                                     (string-append result
                                                    (flush-segment current-segment current-color current-width)))))]
                     
                     ;; Arc: flush segment, render arc separately
                     [(arc? cmd)
                      (loop rest
                            '()
                            #f
                            #f
                            (string-append result
                                           (flush-segment current-segment current-color current-width)
                                           (arc->svg cmd)))]
                     
                     ;; Circle: flush segment, render circle separately
                     [(circle? cmd)
                      (loop rest
                            '()
                            #f
                            #f
                            (string-append result
                                           (flush-segment current-segment current-color current-width)
                                           (circle->svg cmd)))]
                     
                     ;; Polygon: flush segment, render polygon separately
                     [(polygon? cmd)
                      (loop rest
                            '()
                            #f
                            #f
                            (string-append result
                                           (flush-segment current-segment current-color current-width)
                                           (polygon->svg cmd)))]
                     
                     ;; Unknown: skip
                     [else
                      (loop rest current-segment current-color current-width result)]))))))

;;; ====
;;; Path Segment Rendering
;;; ====

;;; flush-segment : (List PathCmd) x Color12 x Nat -> String
;;; Convert accumulated move-to/line-to commands to SVG path element.
(define (flush-segment cmds color width)
  (if (or (null? cmds) (not color))
      ""
      (let ([reversed (reverse cmds)])
           ;; Need at least a move and a line
           (if (and (>= (length reversed) 1)
                    (exists line-to? reversed))
               (string-append
                "  <path d=\""
                (segment->path-d reversed)
                "\" stroke=\"" (color12->svg-hex color) "\""
                " stroke-width=\"" (number->string width) "\""
                " fill=\"none\""
                " stroke-linecap=\"round\""
                " stroke-linejoin=\"round\"/>\n")
               ""))))

;;; segment->path-d : (List PathCmd) -> String
;;; Generate SVG path 'd' attribute from path commands.
(define (segment->path-d cmds)
  (string-join
   (map (lambda (cmd)
                (cond
                 [(move-to? cmd)
                  (format-coord "M" (move-to-x cmd) (move-to-y cmd))]
                 [(line-to? cmd)
                  (format-coord "L" (line-to-x cmd) (line-to-y cmd))]
                 [else ""]))
        cmds)
   " "))

;;; format-coord : String x Real x Real -> String
;;; Format a coordinate command for SVG path.
(define (format-coord prefix x y)
  (string-append prefix " " (format-number x) " " (format-number y)))

;;; format-number : Real -> String
;;; Format a number for SVG, limiting decimal places.
(define (format-number n)
  (let ([rounded (/ (round (* n 100)) 100.0)])
       (if (= rounded (truncate rounded))
           (number->string (inexact->exact (truncate rounded)))
           (let ([s (number->string rounded)])
                ;; Trim trailing zeros after decimal
                (let loop ([i (- (string-length s) 1)])
                     (if (< i 0)
                         s
                         (case (string-ref s i)
                               [(#\0) (loop (- i 1))]
                               [(#\.) (substring s 0 i)]
                               [else (substring s 0 (+ i 1))])))))))

;;; ====
;;; Shape Rendering
;;; ====

;;; circle->svg : CircleCmd -> String
;;; Render a circle command as SVG.
(define (circle->svg cmd)
  (let ([cx (circle-cx cmd)]
        [cy (circle-cy cmd)]
        [r (circle-radius cmd)]
        [color (color12->svg-hex (circle-color cmd))]
        [width (circle-width cmd)]
        [fill? (circle-fill? cmd)])
       (string-append
        "  <circle cx=\"" (format-number cx) "\""
        " cy=\"" (format-number cy) "\""
        " r=\"" (format-number r) "\""
        " stroke=\"" color "\""
        " stroke-width=\"" (number->string width) "\""
        " fill=\"" (if fill? color "none") "\"/>\n")))

;;; polygon->svg : PolygonCmd -> String
;;; Render a polygon command as SVG.
(define (polygon->svg cmd)
  (let ([points (polygon-points cmd)]
        [color (color12->svg-hex (polygon-color cmd))]
        [width (polygon-width cmd)]
        [fill? (polygon-fill? cmd)])
       (string-append
        "  <polygon points=\""
        (points->svg-string points)
        "\" stroke=\"" color "\""
        " stroke-width=\"" (number->string width) "\""
        " fill=\"" (if fill? color "none") "\""
        " stroke-linejoin=\"round\"/>\n")))

;;; points->svg-string : (List (Pair Real Real)) -> String
;;; Convert list of point pairs to SVG points attribute value.
(define (points->svg-string points)
  (string-join
   (map (lambda (p)
                (string-append (format-number (car p)) "," (format-number (cdr p))))
        points)
   " "))

;;; arc->svg : ArcCmd -> String
;;; Render an arc command as SVG path.
;;; Uses SVG arc path command (A).
(define (arc->svg cmd)
  (let* ([cx (arc-cx cmd)]
         [cy (arc-cy cmd)]
         [r (arc-radius cmd)]
         [start-angle (arc-start-angle cmd)]
         [end-angle (arc-end-angle cmd)]
         [color (color12->svg-hex (arc-color cmd))]
         [width (arc-width cmd)]
         ;; Calculate start and end points
         [start-rad (deg->rad start-angle)]
         [end-rad (deg->rad end-angle)]
         [x1 (+ cx (* r (cos start-rad)))]
         [y1 (+ cy (* r (sin start-rad)))]
         [x2 (+ cx (* r (cos end-rad)))]
         [y2 (+ cy (* r (sin end-rad)))]
         ;; SVG arc parameters
         [angle-diff (abs (- end-angle start-angle))]
         [large-arc (if (> angle-diff 180) 1 0)]
         [sweep (if (> end-angle start-angle) 1 0)])
        (string-append
         "  <path d=\""
         "M " (format-number x1) " " (format-number y1) " "
         "A " (format-number r) " " (format-number r) " "
         "0 " (number->string large-arc) " " (number->string sweep) " "
         (format-number x2) " " (format-number y2)
         "\" stroke=\"" color "\""
         " stroke-width=\"" (number->string width) "\""
         " fill=\"none\""
         " stroke-linecap=\"round\"/>\n")))

;;; ====
;;; Utility Functions
;;; ====


;;; exists : (A -> Bool) x (List A) -> Bool
;;; Check if any element satisfies predicate.
(define (exists pred lst)
  (cond
   [(null? lst) #f]
   [(pred (car lst)) #t]
   [else (exists pred (cdr lst))]))

;;; deg->rad / cos / sin should be available from turtle.ss
;;; If not loaded, define them here:
(define pi 3.141592653589793)
(define (deg->rad deg) (* deg (/ pi 180.0)))

;;; ====
;;; File Output Helpers
;;; ====

;;; save-svg : Drawing x String -> Void
;;; Save drawing to an SVG file.
(define (save-svg drawing filename)
  (let ([svg (drawing->svg drawing)])
       (call-with-output-file filename
                              (lambda (port)
                                      (display svg port))
                              '(replace))))

;;; save-svg/turtle : Turtle x String -> Void
;;; Save turtle state to an SVG file.
(define (save-svg/turtle t filename)
  (save-svg (turtle->drawing t) filename))

;;; turtle->svg : Turtle -> String
;;; Convert turtle state directly to SVG string.
;;; Convenience wrapper for (drawing->svg (turtle->drawing t)).
(define (turtle->svg t)
  (drawing->svg (turtle->drawing t)))
