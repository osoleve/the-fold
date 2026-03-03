;;; @module markdown-html
;;; @requires prelude
(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
(require 'prelude)

(doc 'module 'markdown-html)
(doc 'description "HTML Renderer for Markdown AST - Converts markdown AST nodes to HTML strings")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'note "Main entry point: (render-html ast) -> String")

(doc 'section 'html-escaping)

;;; html-escape : String -> String
;;; Escape HTML special characters: < > & " '
(define (html-escape str)
  (let ([len (string-length str)])
    (let loop ([i 0] [acc '()])
      (if (>= i len)
          (apply string-append (reverse acc))
          (let ([c (string-ref str i)])
            (loop (+ i 1)
                  (cons (case c
                          [(#\<) "&lt;"]
                          [(#\>) "&gt;"]
                          [(#\&) "&amp;"]
                          [(#\") "&quot;"]
                          [(#\') "&#39;"]
                          [else (string c)])
                        acc)))))))

;;; html-escape-attr : String -> String
;;; Escape for use in HTML attributes (same as html-escape).
(define html-escape-attr html-escape)

(doc 'section 'tag-rendering-helpers)

;;; html-tag : Symbol × String -> String
;;; Wrap content in an HTML tag.
(define (html-tag tag content)
  (string-append "<" (symbol->string tag) ">"
                 content
                 "</" (symbol->string tag) ">"))

;;; html-tag-nl : Symbol × String -> String
;;; Wrap content in an HTML tag with newlines.
(define (html-tag-nl tag content)
  (string-append "<" (symbol->string tag) ">\n"
                 content
                 "</" (symbol->string tag) ">\n"))

;;; html-tag-attrs : Symbol × (Alist Symbol String) × String -> String
;;; Wrap content in an HTML tag with attributes.
(define (html-tag-attrs tag attrs content)
  (let ([attr-str (if (null? attrs)
                      ""
                      (apply string-append
                             (map (lambda (attr)
                                    (string-append " "
                                                   (symbol->string (car attr))
                                                   "=\""
                                                   (html-escape-attr (cdr attr))
                                                   "\""))
                                  attrs)))])
    (string-append "<" (symbol->string tag) attr-str ">"
                   content
                   "</" (symbol->string tag) ">")))

;;; html-void-tag : Symbol -> String
;;; Create a void/self-closing tag.
(define (html-void-tag tag)
  (string-append "<" (symbol->string tag) ">"))

;;; html-void-tag-attrs : Symbol × (Alist Symbol String) -> String
;;; Create a void tag with attributes.
(define (html-void-tag-attrs tag attrs)
  (let ([attr-str (if (null? attrs)
                      ""
                      (apply string-append
                             (map (lambda (attr)
                                    (string-append " "
                                                   (symbol->string (car attr))
                                                   "=\""
                                                   (html-escape-attr (cdr attr))
                                                   "\""))
                                  attrs)))])
    (string-append "<" (symbol->string tag) attr-str ">")))

;;; ============================================================
;;; Main Renderer
;;; ============================================================

;;; render-html : AST -> String
;;; Render a markdown AST node to HTML.
(define (render-html node)
  (cond
    ;; Handle raw strings (shouldn't happen in well-formed AST, but be safe)
    [(string? node) (html-escape node)]

    ;; Handle empty/invalid
    [(not (pair? node)) ""]
    [(null? node) ""]

    ;; Dispatch on tag
    [else
     (let ([tag (car node)]
           [children (cdr node)])
       (case tag
         ;; Document (root)
         [(document)
          (render-children children "\n")]

         ;; Headings
         [(h1 h2 h3 h4 h5 h6)
          (html-tag-nl tag (render-inlines children))]

         ;; Paragraph
         [(p)
          (html-tag-nl 'p (render-inlines children))]

         ;; Code block
         [(code-block)
          (let ([lang (car children)]
                [code (cadr children)])
            (if (string=? lang "")
                (string-append "<pre><code>"
                               (html-escape code)
                               "</code></pre>\n")
                (string-append "<pre><code class=\"language-"
                               (html-escape lang)
                               "\">"
                               (html-escape code)
                               "</code></pre>\n")))]

         ;; Blockquote
         [(blockquote)
          (html-tag-nl 'blockquote (render-children children "\n"))]

         ;; Lists
         [(ul ol)
          (html-tag-nl tag (render-children children ""))]

         ;; List item
         [(li)
          (html-tag 'li (render-inlines children))]

         ;; Table
         [(table)
          (let ([header (car children)]
                [rows (cadr children)])
            (string-append
             "<table>\n<thead>\n<tr>\n"
             (apply string-append
                    (map (lambda (cell)
                           (string-append "<th>" (render-inlines cell) "</th>\n"))
                         header))
             "</tr>\n</thead>\n<tbody>\n"
             (apply string-append
                    (map (lambda (row)
                           (string-append
                            "<tr>\n"
                            (apply string-append
                                   (map (lambda (cell)
                                          (string-append "<td>" (render-inlines cell) "</td>\n"))
                                        row))
                            "</tr>\n"))
                         rows))
             "</tbody>\n</table>\n"))]

         ;; Horizontal rule
         [(hr)
          "<hr>\n"]

         ;; Inline: text
         [(text)
          (html-escape (car children))]

         ;; Inline: strong (bold)
         [(strong)
          (html-tag 'strong (render-inlines children))]

         ;; Inline: emphasis (italic)
         [(em)
          (html-tag 'em (render-inlines children))]

         ;; Inline: code
         [(code)
          (html-tag 'code (html-escape (car children)))]

         ;; Inline: link
         [(link)
          (let ([url (car children)]
                [title (cadr children)]
                [text (caddr children)])
            (if (string=? title "")
                (html-tag-attrs 'a
                                `((href . ,url))
                                (render-inlines text))
                (html-tag-attrs 'a
                                `((href . ,url) (title . ,title))
                                (render-inlines text))))]

         ;; Line break
         [(br)
          "<br>\n"]

         ;; Unknown tag - render children
         [else
          (render-children children "")]))]))

;;; render-children : (List AST) × String -> String
;;; Render a list of child nodes with separator.
(define (render-children nodes sep)
  (if (null? nodes)
      ""
      (let loop ([nodes nodes] [acc '()])
        (if (null? nodes)
            (apply string-append (reverse acc))
            (loop (cdr nodes)
                  (if (null? acc)
                      (list (render-html (car nodes)))
                      (cons (render-html (car nodes))
                            (cons sep acc))))))))

;;; render-inlines : (List AST) -> String
;;; Render inline elements without separator.
(define (render-inlines nodes)
  (apply string-append (map render-html nodes)))

;;; ============================================================
;;; Page Rendering
;;; ============================================================

;;; render-page : String × String × String -> String
;;; Render a complete HTML page with title, CSS, and body content.
(define (render-page title css body)
  (string-append
   "<!DOCTYPE html>\n"
   "<html lang=\"en\">\n"
   "<head>\n"
   "    <meta charset=\"UTF-8\">\n"
   "    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n"
   "    <title>" (html-escape title) "</title>\n"
   "    <style>\n" css "\n    </style>\n"
   "</head>\n"
   "<body>\n"
   body
   "</body>\n"
   "</html>\n"))

;;; ============================================================
;;; Convenience Functions
;;; ============================================================

;;; markdown-to-html : AST -> String
;;; Alias for render-html.
(define markdown-to-html render-html)

;;; render-fragment : AST -> String
;;; Render just the HTML fragment (no page wrapper).
(define render-fragment render-html)
