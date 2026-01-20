;;; boundary/turtle-svg.ss — SVG Generation for Turtle Graphics

;;; NOTE: string utilities provided by core/prelude.ss
(load "core/base/prelude.ss")

(doc 'module 'turtle-svg)
(doc 'description "SVG generation for turtle graphics - converts turtle drawings to SVG format")
(doc 'layer 'boundary)
(doc 'purity 'total)

(doc 'note "Converts turtle drawings to SVG format for viewing and export")
(doc 'note "Generates clean, well-formed SVG 1.1 documents")
(doc 'note "This is Shell code: pure functions for SVG string generation")

(doc 'dependencies "boundary/turtle-color.ss (for color12->svg-hex)")
(doc 'dependencies "boundary/turtle-path.ss (for path command accessors)")
(doc 'dependencies "boundary/ui/turtle.ss (for drawing record)")

(doc 'section 'svg-document-structure)

(doc svg-header 'type (-> Nat Nat String))
(doc svg-header 'description "Generate SVG document header with XML declaration")
(define (svg-header width height)
  (string-append
   "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
   "<svg xmlns=\"http://www.w3.org/2000/svg\"\n"
   "     width=\"" (number->string width) "\"\n"
   "     height=\"" (number->string height) "\"\n"
   "     viewBox=\"0 0 " (number->string width) " " (number->string height) "\">\n"))

(doc svg-footer 'type (-> String))
(doc svg-footer 'description "Generate SVG document footer")
(define (svg-footer)
  "</svg>\n")

(doc svg-background 'type (-> Nat Nat String String))
(doc svg-background 'description "Generate background rectangle")
(define (svg-background width height color-hex)
  (string-append
   "  <rect width=\"" (number->string width) "\""
   " height=\"" (number->string height) "\""
   " fill=\"" color-hex "\"/>\n"))

(doc 'section 'drawing-to-svg-conversion)

(doc drawing->svg 'type (-> Drawing String))
(doc drawing->svg 'description "Convert a complete turtle drawing to an SVG document")
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

(doc 'section 'path-commands-to-svg-elements)

(doc paths->svg 'type (-> (List PathCmd) String))
(doc paths->svg 'description "Convert a list of path commands to SVG elements")
(doc paths->svg 'note "Groups consecutive line-to commands into path elements")
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

(doc 'section 'path-segment-rendering)

(doc flush-segment 'type (-> (List PathCmd) Color12 Nat String))
(doc flush-segment 'description "Convert accumulated move-to/line-to commands to SVG path element")
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

(doc segment->path-d 'type (-> (List PathCmd) String))
(doc segment->path-d 'description "Generate SVG path 'd' attribute from path commands")
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

(doc format-coord 'type (-> String Real Real String))
(doc format-coord 'description "Format a coordinate command for SVG path")
(define (format-coord prefix x y)
  (string-append prefix " " (format-number x) " " (format-number y)))

(doc format-number 'type (-> Real String))
(doc format-number 'description "Format a number for SVG, limiting decimal places")
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

(doc 'section 'shape-rendering)

(doc circle->svg 'type (-> CircleCmd String))
(doc circle->svg 'description "Render a circle command as SVG")
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

(doc polygon->svg 'type (-> PolygonCmd String))
(doc polygon->svg 'description "Render a polygon command as SVG")
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

(doc points->svg-string 'type (-> (List (Pair Real Real)) String))
(doc points->svg-string 'description "Convert list of point pairs to SVG points attribute value")
(define (points->svg-string points)
  (string-join
   (map (lambda (p)
                (string-append (format-number (car p)) "," (format-number (cdr p))))
        points)
   " "))

(doc arc->svg 'type (-> ArcCmd String))
(doc arc->svg 'description "Render an arc command as SVG path")
(doc arc->svg 'note "Uses SVG arc path command (A)")
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

(doc 'section 'utility-functions)

(doc exists 'type (-> (-> A Bool) (List A) Bool))
(doc exists 'description "Check if any element satisfies predicate")
(define (exists pred lst)
  (cond
   [(null? lst) #f]
   [(pred (car lst)) #t]
   [else (exists pred (cdr lst))]))

(doc 'note "deg->rad / cos / sin should be available from turtle.ss")
(doc 'note "If not loaded, define them here")
(define pi 3.141592653589793)
(define (deg->rad deg) (* deg (/ pi 180.0)))

(doc 'section 'file-output-helpers)

(doc save-svg 'type (-> Drawing String Void))
(doc save-svg 'description "Save drawing to an SVG file")
(define (save-svg drawing filename)
  (let ([svg (drawing->svg drawing)])
       (call-with-output-file filename
                              (lambda (port)
                                      (display svg port))
                              '(replace))))

(doc save-svg/turtle 'type (-> Turtle String Void))
(doc save-svg/turtle 'description "Save turtle state to an SVG file")
(define (save-svg/turtle t filename)
  (save-svg (turtle->drawing t) filename))

(doc turtle->svg 'type (-> Turtle String))
(doc turtle->svg 'description "Convert turtle state directly to SVG string")
(doc turtle->svg 'note "Convenience wrapper for (drawing->svg (turtle->drawing t))")
(define (turtle->svg t)
  (drawing->svg (turtle->drawing t)))
