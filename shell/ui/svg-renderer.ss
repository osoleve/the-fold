;;; shell/svg-renderer.ss — SVG Renderer for Graphics Primitives
;;;
;;; Renders graphics primitives, canvases, and layout to SVG format.
;;; Integrates with graphics-primitives, transforms, and layout-combinators.
;;;
;;; This is Shell code: pure functions for SVG generation.
;;;
;;; Features:
;;;   - Canvas to SVG conversion (text/character grid)
;;;   - Shape primitives to SVG (circle, rect, polygon, polyline, path)
;;;   - Transform support (translate, rotate, scale, matrix)
;;;   - Styling with fill, stroke, opacity
;;;   - Gradients (linear, radial)
;;;   - Patterns and filters
;;;   - CSS styling and classes
;;;   - Layer composition
;;;
;;; Dependencies:
;;;   - shell/layout.ss — Canvas primitives
;;;   - shell/ui/graphics-primitives.ss — Shape rendering
;;;   - shell/ui/transforms.ss — Transform matrices
;;;
;;; Borrows infrastructure from shell/ui/turtle-svg.ss

(library (shell svg-renderer)
         (export
          ;; Document structure
          svg-document
          svg-header
          svg-footer
          
          ;; Canvas rendering
          canvas->svg
          canvas->svg-text
          
          ;; Shape rendering
          shape->svg
          rect->svg
          circle->svg
          polyline->svg
          polygon->svg
          path->svg
          
          ;; Styling
          make-svg-style
          svg-style-fill
          svg-style-stroke
          svg-style-stroke-width
          svg-style-opacity
          style->svg-attrs
          
          ;; Gradients
          linear-gradient->svg
          radial-gradient->svg
          
          ;; Transforms
          transform-matrix->svg
          
          ;; Groups and layers
          svg-group
          svg-defs
          
          ;; Filters
          svg-filter-blur
          svg-filter-shadow
          
          ;; Utilities
          save-svg)
         
         (import (chezscheme))
         
         ;;; ============================================================
         ;;; SVG Document Structure (from turtle-svg.ss)
         ;;; ============================================================
         
         ;;; svg-header : Nat × Nat × [(List (String . String))] → String
         ;;; Generate SVG document header with optional attributes.
         (define svg-header
           (case-lambda
            [(width height)
             (svg-header width height '())]
            [(width height attrs)
             (string-append
              "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
              "<svg xmlns=\"http://www.w3.org/2000/svg\"\n"
              "     xmlns:xlink=\"http://www.w3.org/1999/xlink\"\n"
              "     width=\"" (number->string width) "\"\n"
              "     height=\"" (number->string height) "\"\n"
              "     viewBox=\"0 0 " (number->string width) " " (number->string height) "\""
              (attrs->string attrs)
              ">\n")]))
         
         ;;; svg-footer : → String
         (define (svg-footer)
           "</svg>\n")
         
         ;;; svg-document : Nat × Nat × String × [(List (String . String))] → String
         ;;; Create complete SVG document with content.
         (define svg-document
           (case-lambda
            [(width height content)
             (svg-document width height content '())]
            [(width height content attrs)
             (string-append
              (svg-header width height attrs)
              content
              (svg-footer))]))
         
         ;;; ============================================================
         ;;; SVG Styling
         ;;; ============================================================
         
         (define-record-type svg-style%
           (fields fill stroke stroke-width opacity css-class))
         
         ;;; make-svg-style : [(String)] × [(String)] × [(Nat)] × [(Real)] × [(String)] → SVGStyle
         (define make-svg-style
           (case-lambda
            [() (make-svg-style% "none" "black" 1 1.0 #f)]
            [(fill) (make-svg-style% fill "black" 1 1.0 #f)]
            [(fill stroke) (make-svg-style% fill stroke 1 1.0 #f)]
            [(fill stroke width) (make-svg-style% fill stroke width 1.0 #f)]
            [(fill stroke width opacity) (make-svg-style% fill stroke width opacity #f)]
            [(fill stroke width opacity css-class) (make-svg-style% fill stroke width opacity css-class)]))
         
         (define svg-style-fill svg-style%-fill)
         (define svg-style-stroke svg-style%-stroke)
         (define svg-style-stroke-width svg-style%-stroke-width)
         (define svg-style-opacity svg-style%-opacity)
         
         ;;; Default styles
         (define default-svg-style (make-svg-style "none" "black" 1 1.0 #f))
         
         ;;; style->svg-attrs : SVGStyle → String
         ;;; Convert style to SVG attribute string.
         (define (style->svg-attrs style)
           (string-append
            " fill=\"" (svg-style-fill style) "\""
            " stroke=\"" (svg-style-stroke style) "\""
            " stroke-width=\"" (number->string (svg-style-stroke-width style)) "\""
            " opacity=\"" (number->string (svg-style-opacity style)) "\""
            (if (svg-style%-css-class style)
                (string-append " class=\"" (svg-style%-css-class style) "\"")
                "")))
         
         ;;; ============================================================
         ;;; Number Formatting (from turtle-svg.ss)
         ;;; ============================================================
         
         (define (format-number n)
           (let ([rounded (/ (round (* n 100)) 100.0)])
                (if (= rounded (truncate rounded))
                    (number->string (inexact->exact (truncate rounded)))
                    (let ([s (number->string rounded)])
                         (let loop ([i (- (string-length s) 1)])
                              (if (< i 0)
                                  s
                                  (case (string-ref s i)
                                        [(#\0) (loop (- i 1))]
                                        [(#\.) (substring s 0 i)]
                                        [else (substring s 0 (+ i 1))])))))))
         
         ;;; ============================================================
         ;;; Attribute Formatting
         ;;; ============================================================
         
         ;;; attrs->string : (List (String . String)) → String
         ;;; Convert attribute alist to SVG attribute string.
         (define (attrs->string attrs)
           (if (null? attrs)
               ""
               (fold-left (lambda (acc pair)
                                  (string-append acc " "
                                                 (car pair) "=\"" (cdr pair) "\""))
                          ""
                          attrs)))
         
         ;;; ============================================================
         ;;; Canvas Rendering
         ;;; ============================================================
         
         ;;; canvas->svg-text : Canvas × [SVGStyle] → String
         ;;; Render canvas as SVG text elements (for ASCII art).
         ;;; Each non-space character becomes a text element.
         (define canvas->svg-text
           (case-lambda
            [(canvas)
             (canvas->svg-text canvas (make-svg-style "black" "none" 1 1.0 #f))]
            [(canvas style)
             (let ([w (canvas-width canvas)]
                   [h (canvas-height canvas)]
                   [char-width 8]   ; Approximate monospace char width
                   [char-height 14]) ; Approximate monospace char height
                  (let loop-y ([y 0] [result ""])
                       (if (>= y h)
                           result
                           (let loop-x ([x 0] [result result])
                                (if (>= x w)
                                    (loop-y (+ y 1) result)
                                    (let ([ch (canvas-ref canvas x y)])
                                         (if (char=? ch #\space)
                                             (loop-x (+ x 1) result)
                                             (loop-x (+ x 1)
                                                     (string-append
                                                      result
                                                      "  <text x=\"" (number->string (* x char-width)) "\""
                                                      " y=\"" (number->string (* (+ y 1) char-height)) "\""
                                                      " font-family=\"monospace\""
                                                      " font-size=\"14\""
                                                      (style->svg-attrs style)
                                                      ">" (char->svg-safe ch) "</text>\n"))))))))]))
           
           ;;; canvas->svg : Canvas × [SVGStyle] → String
           ;;; Alias for canvas->svg-text.
           (define canvas->svg canvas->svg-text)
           
           ;;; char->svg-safe : Char → String
           ;;; Escape special XML characters.
           (define (char->svg-safe ch)
             (case ch
                   [(#\<) "&lt;"]
                   [(#\>) "&gt;"]
                   [(#\&) "&amp;"]
                   [(#\") "&quot;"]
                   [(#\') "&apos;"]
                   [else (string ch)]))
           
           ;;; Canvas accessors (standalone definitions for library use)
           (define (canvas-width canvas) (vector-ref canvas 1))
           (define (canvas-height canvas) (vector-ref canvas 2))
           (define (canvas-ref canvas x y)
             (let ([w (canvas-width canvas)]
                   [h (canvas-height canvas)]
                   [cells (vector-ref canvas 3)])
                  (if (or (< x 0) (>= x w) (< y 0) (>= y h))
                      #\space
                      (vector-ref cells (+ (* y w) x)))))
           
           ;;; ============================================================
           ;;; Shape Rendering
           ;;; ============================================================
           
           ;;; rect->svg : Nat × Nat × Nat × Nat × [SVGStyle] → String
           ;;; Render rectangle.
           (define rect->svg
             (case-lambda
              [(x y width height)
               (rect->svg x y width height default-svg-style)]
              [(x y width height style)
               (string-append
                "  <rect x=\"" (number->string x) "\""
                " y=\"" (number->string y) "\""
                " width=\"" (number->string width) "\""
                " height=\"" (number->string height) "\""
                (style->svg-attrs style)
                "/>\n")]))
           
           ;;; circle->svg : Nat × Nat × Nat × [SVGStyle] → String
           ;;; Render circle.
           (define circle->svg
             (case-lambda
              [(cx cy r)
               (circle->svg cx cy r default-svg-style)]
              [(cx cy r style)
               (string-append
                "  <circle cx=\"" (number->string cx) "\""
                " cy=\"" (number->string cy) "\""
                " r=\"" (number->string r) "\""
                (style->svg-attrs style)
                "/>\n")]))
           
           ;;; polyline->svg : (List Point) × [SVGStyle] → String
           ;;; Render polyline (connected line segments).
           (define polyline->svg
             (case-lambda
              [(points)
               (polyline->svg points default-svg-style)]
              [(points style)
               (if (null? points)
                   ""
                   (string-append
                    "  <polyline points=\""
                    (points->svg-string points)
                    "\""
                    (style->svg-attrs style)
                    "/>\n"))]))
           
           ;;; polygon->svg : (List Point) × [SVGStyle] → String
           ;;; Render polygon (closed shape).
           (define polygon->svg
             (case-lambda
              [(points)
               (polygon->svg points default-svg-style)]
              [(points style)
               (if (null? points)
                   ""
                   (string-append
                    "  <polygon points=\""
                    (points->svg-string points)
                    "\""
                    (style->svg-attrs style)
                    "/>\n"))]))
           
           ;;; path->svg : (List PathCommand) × [SVGStyle] → String
           ;;; Render SVG path from command list.
           (define path->svg
             (case-lambda
              [(commands)
               (path->svg commands default-svg-style)]
              [(commands style)
               (if (null? commands)
                   ""
                   (string-append
                    "  <path d=\""
                    (path-commands->d commands)
                    "\""
                    (style->svg-attrs style)
                    "/>\n"))]))
           
           ;;; path-commands->d : (List PathCommand) → String
           ;;; Convert path commands to SVG path 'd' attribute.
           (define (path-commands->d commands)
             (string-join
              (map (lambda (cmd)
                           (case (car cmd)
                                 [(move-to)
                                  (format-coord "M" (cadr cmd))]
                                 [(line-to)
                                  (format-coord "L" (cadr cmd))]
                                 [(quad-to)
                                  (string-append
                                   (format-coord "Q" (cadr cmd))
                                   " "
                                   (format-coord "" (caddr cmd)))]
                                 [(close)
                                  "Z"]
                                 [else ""]))
                   commands)
              " "))
           
           ;;; points->svg-string : (List Point) → String
           (define (points->svg-string points)
             (string-join
              (map (lambda (p)
                           (string-append
                            (format-number (point-x p))
                            ","
                            (format-number (point-y p))))
                   points)
              " "))
           
           ;;; format-coord : String × Point → String
           (define (format-coord prefix pt)
             (string-append
              prefix
              (if (string=? prefix "") "" " ")
              (format-number (point-x pt))
              " "
              (format-number (point-y pt))))
           
           ;;; Point helpers (fallback)
           (define (point-x pt) (car pt))
           (define (point-y pt) (cdr pt))
           
           ;;; shape->svg : Shape × [SVGStyle] → String
           ;;; Generic shape renderer (dispatches on shape type).
           (define shape->svg
             (case-lambda
              [(shape)
               (shape->svg shape default-svg-style)]
              [(shape style)
               (case (car shape)
                     [(rect)
                      (let ([x (cadr shape)]
                            [y (caddr shape)]
                            [w (cadddr shape)]
                            [h (car (cddddr shape))])
                           (rect->svg x y w h style))]
                     [(circle)
                      (let ([cx (cadr shape)]
                            [cy (caddr shape)]
                            [r (cadddr shape)])
                           (circle->svg cx cy r style))]
                     [(polyline)
                      (polyline->svg (cadr shape) style)]
                     [(polygon)
                      (polygon->svg (cadr shape) style)]
                     [(path)
                      (path->svg (cadr shape) style)]
                     [else ""])]))
           
           ;;; ============================================================
           ;;; Gradients
           ;;; ============================================================
           
           ;;; linear-gradient->svg : String × (List (Real . String)) × Real × Real × Real × Real → String
           ;;; Create linear gradient definition.
           ;;; id: gradient ID, stops: list of (offset . color), coords: x1 y1 x2 y2
           (define (linear-gradient->svg id stops x1 y1 x2 y2)
             (string-append
              "  <linearGradient id=\"" id "\""
              " x1=\"" (format-number x1) "%\""
              " y1=\"" (format-number y1) "%\""
              " x2=\"" (format-number x2) "%\""
              " y2=\"" (format-number y2) "%\">\n"
              (gradient-stops->svg stops)
              "  </linearGradient>\n"))
           
           ;;; radial-gradient->svg : String × (List (Real . String)) × Real × Real × Real → String
           ;;; Create radial gradient definition.
           (define (radial-gradient->svg id stops cx cy r)
             (string-append
              "  <radialGradient id=\"" id "\""
              " cx=\"" (format-number cx) "%\""
              " cy=\"" (format-number cy) "%\""
              " r=\"" (format-number r) "%\">\n"
              (gradient-stops->svg stops)
              "  </radialGradient>\n"))
           
           ;;; gradient-stops->svg : (List (Real . String)) → String
           (define (gradient-stops->svg stops)
             (string-join
              (map (lambda (stop)
                           (string-append
                            "    <stop offset=\"" (format-number (* (car stop) 100)) "%\""
                            " stop-color=\"" (cdr stop) "\"/>\n"))
                   stops)
              ""))
           
           ;;; ============================================================
           ;;; Transforms
           ;;; ============================================================
           
           ;;; transform-matrix->svg : Matrix → String
           ;;; Convert 2x3 affine transform matrix to SVG matrix() transform.
           ;;; Matrix format: (a b c d tx ty)
           (define (transform-matrix->svg matrix)
             (let ([a (list-ref matrix 0)]
                   [b (list-ref matrix 1)]
                   [c (list-ref matrix 2)]
                   [d (list-ref matrix 3)]
                   [tx (list-ref matrix 4)]
                   [ty (list-ref matrix 5)])
                  (string-append
                   "matrix("
                   (format-number a) " "
                   (format-number b) " "
                   (format-number c) " "
                   (format-number d) " "
                   (format-number tx) " "
                   (format-number ty)
                   ")")))
           
           ;;; ============================================================
           ;;; Groups and Layers
           ;;; ============================================================
           
           ;;; svg-group : String × [(List (String . String))] × [String] → String
           ;;; Create SVG group with content.
           (define svg-group
             (case-lambda
              [(content)
               (svg-group content '() #f)]
              [(content attrs)
               (svg-group content attrs #f)]
              [(content attrs transform)
               (string-append
                "  <g"
                (attrs->string attrs)
                (if transform
                    (string-append " transform=\"" transform "\"")
                    "")
                ">\n"
                content
                "  </g>\n")]))
           
           ;;; svg-defs : String → String
           ;;; Create SVG defs section (for gradients, patterns, etc.)
           (define (svg-defs content)
             (string-append
              "  <defs>\n"
              content
              "  </defs>\n"))
           
           ;;; ============================================================
           ;;; Filters
           ;;; ============================================================
           
           ;;; svg-filter-blur : String × Real → String
           ;;; Create Gaussian blur filter definition.
           (define (svg-filter-blur id stddev)
             (string-append
              "  <filter id=\"" id "\">\n"
              "    <feGaussianBlur in=\"SourceGraphic\" stdDeviation=\""
              (format-number stddev) "\"/>\n"
              "  </filter>\n"))
           
           ;;; svg-filter-shadow : String × Real × Real × Real → String
           ;;; Create drop shadow filter.
           (define (svg-filter-shadow id dx dy blur)
             (string-append
              "  <filter id=\"" id "\">\n"
              "    <feGaussianBlur in=\"SourceAlpha\" stdDeviation=\""
              (format-number blur) "\"/>\n"
              "    <feOffset dx=\"" (format-number dx) "\""
              " dy=\"" (format-number dy) "\" result=\"offsetblur\"/>\n"
              "    <feMerge>\n"
              "      <feMergeNode in=\"offsetblur\"/>\n"
              "      <feMergeNode in=\"SourceGraphic\"/>\n"
              "    </feMerge>\n"
              "  </filter>\n"))
           
           ;;; ============================================================
           ;;; Utilities
           ;;; ============================================================
           
           ;;; string-join : (List String) × String → String
           (define (string-join lst sep)
             (if (null? lst)
                 ""
                 (fold-left (lambda (acc s)
                                    (if (string=? acc "")
                                        s
                                        (string-append acc sep s)))
                            ""
                            lst)))
           
           ;;; save-svg : String × String → Void
           ;;; Save SVG content to file.
           (define (save-svg content filename)
             (call-with-output-file filename
                                    (lambda (port)
                                            (display content port))
                                    '(replace)))
           
           ) ; end library
