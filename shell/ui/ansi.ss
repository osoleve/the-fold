;;; shell/ui/ansi.ss — ANSI Terminal Output for Pretty Printing
;;;
;;; Extends the Wadler-Lindig pretty printer with ANSI styling.
;;; Documents can be annotated with styles (color, bold, underline, etc.)
;;; and rendered to ANSI escape sequences for terminal output.
;;;
;;; This is Shell code: handles display formatting (impure side).
;;;
;;; Architecture:
;;;   pretty.ss provides Doc with doc-annotate for generic annotations.
;;;   This module provides:
;;;     - Style type (fg, bg, bold, underline, italic, dim)
;;;     - ANSI escape sequence generation
;;;     - Styled document combinators
;;;     - sdoc->ansi-string renderer
;;;
;;; Usage:
;;;   (load "shell/ui/ansi.ss")
;;;   (define doc (bold (colored color-red (text "Error:"))))
;;;   (ansi-render doc)  ; => "\x1B;[1m\x1B;[38;5;1mError:\x1B;[0m"
;;;
;;; Dependencies:
;;;   - core/util/pretty.ss
;;;   - shell/ui/color.ss

(load "core/util/pretty.ss")
(load "shell/ui/color.ss")

;;; ====
;;; Style Type
;;; ====
;;;
;;; Style : (style fg bg bold? underline? italic? dim?)
;;;
;;; Represents text styling with:
;;;   - fg : Color | #f (foreground, #f = inherit)
;;;   - bg : Color | #f (background, #f = inherit)
;;;   - bold? : Bool (bold text)
;;;   - underline? : Bool (underlined text)
;;;   - italic? : Bool (italic text)
;;;   - dim? : Bool (dimmed text)

(define-record-type style%
  (fields fg bg bold underline italic dim))

;;; make-style : Color × Color × Bool × Bool × Bool × Bool → Style
(define make-style make-style%)

;;; Accessors auto-generated:
;;;   style%-fg, style%-bg, style%-bold, style%-underline, style%-italic, style%-dim

;;; style-default : Style
;;; No styling applied.
(define style-default
  (make-style #f #f #f #f #f #f))

;;; style-fg : Color → Style
;;; Create style with just foreground color.
(define (style-fg color)
  (make-style color #f #f #f #f #f))

;;; style-bg : Color → Style
;;; Create style with just background color.
(define (style-bg color)
  (make-style #f color #f #f #f #f))

;;; style-bold : Style
;;; Bold text style.
(define style-bold
  (make-style #f #f #t #f #f #f))

;;; style-underline : Style
;;; Underlined text style.
(define style-underline
  (make-style #f #f #f #t #f #f))

;;; style-italic : Style
;;; Italic text style.
(define style-italic
  (make-style #f #f #f #f #t #f))

;;; style-dim : Style
;;; Dimmed text style.
(define style-dim
  (make-style #f #f #f #f #f #t))

;;; style? : Any → Bool
(define (style? x)
  (style%? x))

;;; ====
;;; Style Merging
;;; ====
;;;
;;; Nested styles are merged, with inner styles taking precedence.
;;; This allows (bold (colored red x)) to produce bold red text.

;;; style-merge : Style × Style → Style
;;; Merge two styles, with s2 taking precedence over s1.
(define (style-merge s1 s2)
  (make-style
   (or (style%-fg s2) (style%-fg s1))
   (or (style%-bg s2) (style%-bg s1))
   (or (style%-bold s2) (style%-bold s1))
   (or (style%-underline s2) (style%-underline s1))
   (or (style%-italic s2) (style%-italic s1))
   (or (style%-dim s2) (style%-dim s1))))

;;; ====
;;; ANSI Escape Sequence Generation
;;; ====

;;; style->ansi : Style → String
;;; Generate ANSI escape sequence for a style.
(define (style->ansi style)
  (let* ([codes '()]
         [codes (if (style%-bold style) (cons "1" codes) codes)]
         [codes (if (style%-dim style) (cons "2" codes) codes)]
         [codes (if (style%-italic style) (cons "3" codes) codes)]
         [codes (if (style%-underline style) (cons "4" codes) codes)]
         [fg (style%-fg style)]
         [bg (style%-bg style)])
    (let* ([codes (if fg
                      (cond
                       [(color-rgb? fg)
                        (cons (string-append "38;2;"
                                             (number->string (list-ref fg 1)) ";"
                                             (number->string (list-ref fg 2)) ";"
                                             (number->string (list-ref fg 3)))
                              codes)]
                       [(color-palette? fg)
                        (cons (string-append "38;5;" (number->string (list-ref fg 1)))
                              codes)]
                       [else codes])
                      codes)]
           [codes (if bg
                      (cond
                       [(color-rgb? bg)
                        (cons (string-append "48;2;"
                                             (number->string (list-ref bg 1)) ";"
                                             (number->string (list-ref bg 2)) ";"
                                             (number->string (list-ref bg 3)))
                              codes)]
                       [(color-palette? bg)
                        (cons (string-append "48;5;" (number->string (list-ref bg 1)))
                              codes)]
                       [else codes])
                      codes)])
      (if (null? codes)
          ""
          (string-append "\x1B;[" (string-join (reverse codes) ";") "m")))))

;;; string-join : (List String) × String → String
;;; Join strings with separator.
(define (string-join strs sep)
  (if (null? strs)
      ""
      (let loop ([strs (cdr strs)] [acc (car strs)])
        (if (null? strs)
            acc
            (loop (cdr strs) (string-append acc sep (car strs)))))))

;;; ====
;;; Extended Document Type
;;; ====
;;;
;;; We extend the Doc type with doc-annotate for style annotations.
;;; Since Core code shouldn't depend on Shell, we define the extension here.

;;; doc-annotate : Style × Doc → Doc
;;; Annotate a document with a style.
(define (doc-annotate style doc)
  (vector 'doc-annotate style doc))

;;; doc-annotate? : Doc → Bool
(define (doc-annotate? d)
  (and (vector? d)
       (> (vector-length d) 0)
       (eq? (vector-ref d 0) 'doc-annotate)))

;;; doc-annotate-style : Doc → Style
(define (doc-annotate-style d)
  (vector-ref d 1))

;;; doc-annotate-doc : Doc → Doc
(define (doc-annotate-doc d)
  (vector-ref d 2))

;;; ====
;;; Extended Simple Document Type
;;; ====
;;;
;;; For ANSI rendering, we extend SDoc with style push/pop.

;;; sdoc-push-style : Style × SDoc → SDoc
(define (sdoc-push-style style rest)
  (vector 'sdoc-push-style style rest))

;;; sdoc-pop-style : SDoc → SDoc
(define (sdoc-pop-style rest)
  (vector 'sdoc-pop-style rest))

;;; sdoc-push-style? : SDoc → Bool
(define (sdoc-push-style? sd)
  (and (vector? sd) (eq? (vector-ref sd 0) 'sdoc-push-style)))

;;; sdoc-pop-style? : SDoc → Bool
(define (sdoc-pop-style? sd)
  (and (vector? sd) (eq? (vector-ref sd 0) 'sdoc-pop-style)))

;;; sdoc-push-style-style : SDoc → Style
(define (sdoc-push-style-style sd) (vector-ref sd 1))

;;; sdoc-push-style-rest : SDoc → SDoc
(define (sdoc-push-style-rest sd) (vector-ref sd 2))

;;; sdoc-pop-style-rest : SDoc → SDoc
(define (sdoc-pop-style-rest sd) (vector-ref sd 1))

;;; ====
;;; ANSI Layout Algorithm
;;; ====
;;;
;;; Extended layout that handles doc-annotate nodes.

;;; ansi-best : Nat × Nat × Doc → SDoc
;;; Layout algorithm that handles annotations.
(define (ansi-best w k doc)
  (ansi-be w k (list (cons 0 doc))))

;;; ansi-be : Nat × Nat × (List (Nat × Doc)) → SDoc
;;; Core layout with annotation support.
(define (ansi-be w k stack)
  (if (null? stack)
      (sdoc-empty)
      (let ([i (car (car stack))]
            [doc (cdr (car stack))]
            [rest (cdr stack)])
        (cond
         [(doc-empty? doc)
          (ansi-be w k rest)]
         [(doc-text? doc)
          (let ([s (doc-text-str doc)])
            (sdoc-text s (ansi-be w (+ k (string-length s)) rest)))]
         [(doc-line? doc)
          (sdoc-line i (ansi-be w i rest))]
         [(doc-hardline? doc)
          (sdoc-line i (ansi-be w i rest))]
         [(doc-nest? doc)
          (ansi-be w k (cons (cons (+ i (doc-nest-n doc)) (doc-nest-doc doc)) rest))]
         [(doc-concat? doc)
          (ansi-be w k (cons (cons i (doc-concat-left doc))
                             (cons (cons i (doc-concat-right doc)) rest)))]
         [(doc-group? doc)
          (let ([flattened (doc-group-flat doc)])
            (if (ansi-fits? (- w k) (list (cons i flattened)))
                (ansi-be w k (cons (cons i flattened) rest))
                (ansi-be w k (cons (cons i (doc-group-doc doc)) rest))))]
         [(doc-union? doc)
          (if (ansi-fits? (- w k) (list (cons i (doc-union-left doc))))
              (ansi-be w k (cons (cons i (doc-union-left doc)) rest))
              (ansi-be w k (cons (cons i (doc-union-right doc)) rest)))]
         [(doc-annotate? doc)
          ;; Push style, layout inner doc, pop style
          (let ([style (doc-annotate-style doc)]
                [inner (doc-annotate-doc doc)])
            (sdoc-push-style
             style
             (ansi-be-with-pop w k i inner rest)))]
         [else (sdoc-empty)]))))

;;; ansi-be-with-pop : Nat × Nat × Nat × Doc × Stack → SDoc
;;; Layout inner doc and append pop-style when done.
(define (ansi-be-with-pop w k i inner rest)
  (let ([inner-result (ansi-be w k (list (cons i inner)))])
    (sdoc-append-pop inner-result (ansi-be w k rest))))

;;; sdoc-append-pop : SDoc × SDoc → SDoc
;;; Append a pop-style at the end of an SDoc, followed by continuation.
(define (sdoc-append-pop sd continuation)
  (cond
   [(sdoc-empty? sd) (sdoc-pop-style continuation)]
   [(sdoc-text? sd) (sdoc-text (sdoc-text-str sd)
                               (sdoc-append-pop (sdoc-text-rest sd) continuation))]
   [(sdoc-line? sd) (sdoc-line (sdoc-line-n sd)
                               (sdoc-append-pop (sdoc-line-rest sd) continuation))]
   [(sdoc-push-style? sd) (sdoc-push-style (sdoc-push-style-style sd)
                                           (sdoc-append-pop (sdoc-push-style-rest sd) continuation))]
   [(sdoc-pop-style? sd) (sdoc-pop-style (sdoc-append-pop (sdoc-pop-style-rest sd) continuation))]
   [else (sdoc-pop-style continuation)]))

;;; ansi-fits? : Nat × (List (Nat × Doc)) → Bool
;;; Check if doc fits in remaining width (handles annotations).
(define (ansi-fits? w stack)
  (cond
   [(< w 0) #f]
   [(null? stack) #t]
   [else
    (let ([i (car (car stack))]
          [doc (cdr (car stack))]
          [rest (cdr stack)])
      (cond
       [(doc-empty? doc) (ansi-fits? w rest)]
       [(doc-text? doc)
        (ansi-fits? (- w (string-length (doc-text-str doc))) rest)]
       [(doc-line? doc) #t]
       [(doc-hardline? doc) #t]
       [(doc-nest? doc)
        (ansi-fits? w (cons (cons (+ i (doc-nest-n doc)) (doc-nest-doc doc)) rest))]
       [(doc-concat? doc)
        (ansi-fits? w (cons (cons i (doc-concat-left doc))
                            (cons (cons i (doc-concat-right doc)) rest)))]
       [(doc-group? doc)
        (ansi-fits? w (cons (cons i (doc-group-flat doc)) rest))]
       [(doc-union? doc)
        (ansi-fits? w (cons (cons i (doc-union-left doc)) rest))]
       [(doc-annotate? doc)
        ;; Annotations don't affect width
        (ansi-fits? w (cons (cons i (doc-annotate-doc doc)) rest))]
       [else #t]))]))

;;; ====
;;; ANSI Rendering
;;; ====

;;; sdoc->ansi-string : SDoc → String
;;; Render SDoc to string with ANSI escape sequences.
(define (sdoc->ansi-string sd)
  (let loop ([sd sd] [acc '()] [style-stack '()])
    (cond
     [(sdoc-empty? sd)
      ;; Reset if any styles were applied
      (apply string-append (reverse (if (null? style-stack) acc (cons ansi-reset acc))))]
     [(sdoc-text? sd)
      (loop (sdoc-text-rest sd)
            (cons (sdoc-text-str sd) acc)
            style-stack)]
     [(sdoc-line? sd)
      (loop (sdoc-line-rest sd)
            (cons (string-append "\n" (make-string (sdoc-line-n sd) #\space)) acc)
            style-stack)]
     [(sdoc-push-style? sd)
      (let* ([new-style (sdoc-push-style-style sd)]
             [merged (if (null? style-stack)
                         new-style
                         (style-merge (car style-stack) new-style))]
             [ansi-code (style->ansi merged)])
        (loop (sdoc-push-style-rest sd)
              (if (string=? ansi-code "") acc (cons ansi-code acc))
              (cons merged style-stack)))]
     [(sdoc-pop-style? sd)
      (let ([remaining (if (null? style-stack) '() (cdr style-stack))])
        (loop (sdoc-pop-style-rest sd)
              (cons (if (null? remaining)
                        ansi-reset
                        (string-append ansi-reset (style->ansi (car remaining))))
                    acc)
              remaining))]
     [else (apply string-append (reverse acc))])))

;;; ====
;;; Convenience Rendering Functions
;;; ====

;;; ansi-render : Doc → String
;;; Render document with ANSI colors at default width.
(define (ansi-render doc)
  (ansi-render-width *default-width* doc))

;;; ansi-render-width : Nat × Doc → String
;;; Render document with ANSI colors at specified width.
(define (ansi-render-width width doc)
  (sdoc->ansi-string (ansi-best width 0 doc)))

;;; ansi-display : Doc → Void
;;; Display document with ANSI colors.
(define (ansi-display doc)
  (display (ansi-render doc))
  (newline))

;;; ansi-display-width : Nat × Doc → Void
(define (ansi-display-width width doc)
  (display (ansi-render-width width doc))
  (newline))

;;; ====
;;; Direct String Styling
;;; ====
;;;
;;; For cases where you want to style a plain string without
;;; going through the Doc system.

;;; colored-text : Style × String → String
;;; Apply ANSI style to a plain string.
(define (colored-text style str)
  (let ([ansi-code (style->ansi style)])
    (if (string=? ansi-code "")
        str
        (string-append ansi-code str ansi-reset))))

;;; styled-string : Style × String → String
;;; Alias for colored-text.
(define styled-string colored-text)

;;; ====
;;; Styled Document Combinators
;;; ====
;;;
;;; These create annotated documents for common styling needs.

;;; styled : Style × Doc → Doc
;;; Apply a style to a document.
(define (styled style doc)
  (doc-annotate style doc))

;;; bold : Doc → Doc
;;; Make text bold.
(define (bold doc)
  (styled style-bold doc))

;;; underline : Doc → Doc
;;; Make text underlined.
(define (underline doc)
  (styled style-underline doc))

;;; italic : Doc → Doc
;;; Make text italic.
(define (italic doc)
  (styled style-italic doc))

;;; dim : Doc → Doc
;;; Make text dimmed.
(define (dim doc)
  (styled style-dim doc))

;;; colored : Color × Doc → Doc
;;; Apply foreground color to document.
(define (colored color doc)
  (styled (style-fg color) doc))

;;; bg-colored : Color × Doc → Doc
;;; Apply background color to document.
(define (bg-colored color doc)
  (styled (style-bg color) doc))

;;; ====
;;; Semantic Style Helpers
;;; ====
;;;
;;; Common styling patterns for code and messages.

;;; error-style : Style
(define error-style (style-fg color-red))

;;; warning-style : Style
(define warning-style (style-fg color-yellow))

;;; success-style : Style
(define success-style (style-fg color-green))

;;; info-style : Style
(define info-style (style-fg color-cyan))

;;; keyword-style : Style
(define keyword-style (style-fg color-magenta))

;;; string-style : Style
(define string-style (style-fg color-green))

;;; number-style : Style
(define number-style (style-fg color-cyan))

;;; comment-style : Style
(define comment-style (make-style color-gray #f #f #f #t #f))

;;; Semantic document helpers
(define (error-text s) (styled error-style (text s)))
(define (warning-text s) (styled warning-style (text s)))
(define (success-text s) (styled success-style (text s)))
(define (info-text s) (styled info-style (text s)))
(define (keyword-text s) (styled keyword-style (text s)))
(define (string-text s) (styled string-style (text s)))
(define (number-text n) (styled number-style (text (number->string n))))
(define (comment-text s) (styled comment-style (text s)))

;;; ====
;;; Plain Text Rendering (strip ANSI)
;;; ====
;;;
;;; For environments that don't support ANSI.

;;; ansi-render-plain : Doc → String
;;; Render document without ANSI codes.
(define (ansi-render-plain doc)
  (pretty *default-width* (strip-annotations doc)))

;;; strip-annotations : Doc → Doc
;;; Remove all annotations from a document.
(define (strip-annotations doc)
  (cond
   [(doc-empty? doc) doc]
   [(doc-text? doc) doc]
   [(doc-line? doc) doc]
   [(doc-hardline? doc) doc]
   [(doc-nest? doc) (doc-nest (doc-nest-n doc) (strip-annotations (doc-nest-doc doc)))]
   [(doc-concat? doc) (doc-concat (strip-annotations (doc-concat-left doc))
                                   (strip-annotations (doc-concat-right doc)))]
   [(doc-group? doc) (doc-group (strip-annotations (doc-group-doc doc)))]
   [(doc-union? doc) (doc-union (strip-annotations (doc-union-left doc))
                                 (strip-annotations (doc-union-right doc)))]
   [(doc-annotate? doc) (strip-annotations (doc-annotate-doc doc))]
   [else doc]))

;;; ====
;;; Export Summary
;;; ====
;;;
;;; Types:
;;;   Style (style%), style?
;;;
;;; Style Constructors:
;;;   make-style, style-default, style-fg, style-bg
;;;   style-bold, style-underline, style-italic, style-dim
;;;
;;; Style Operations:
;;;   style-merge, style->ansi
;;;
;;; Document Extension:
;;;   doc-annotate, doc-annotate?, doc-annotate-style, doc-annotate-doc
;;;
;;; Layout & Rendering:
;;;   ansi-best, ansi-render, ansi-render-width
;;;   ansi-display, ansi-display-width
;;;   ansi-render-plain, strip-annotations
;;;
;;; Styled Combinators:
;;;   styled, bold, underline, italic, dim
;;;   colored, bg-colored
;;;
;;; Semantic Helpers:
;;;   error-text, warning-text, success-text, info-text
;;;   keyword-text, string-text, number-text, comment-text
;;;
;;; Predefined Styles:
;;;   error-style, warning-style, success-style, info-style
;;;   keyword-style, string-style, number-style, comment-style

(display "ansi.ss loaded.\n")
(display "  Style documents: (bold doc), (underline doc), (colored color doc)\n")
(display "  Render with ANSI: (ansi-render doc), (ansi-display doc)\n")
(display "  Semantic helpers: (error-text s), (keyword-text s), etc.\n")
