(load "boundary/tools/string-utils.ss")

(doc 'module 'markdown)
(doc 'description "Utilities for generating Markdown-formatted text programmatically")
(doc 'layer 'boundary)
(doc 'purity 'total)

(doc 'note "Useful for generating reports, prompts, and documentation")
(doc 'note "Usage: (bold \"text\") -> \"**text**\"")

(doc 'section 'inline-formatting)

(define (bold text)
  (doc 'type '(-> String String))
  (string-append "**" text "**"))

(define (italic text)
  (doc 'type '(-> String String))
  (string-append "*" text "*"))

(define (code text)
  (doc 'type '(-> String String))
  (doc 'description "Inline code span")
  (string-append "`" text "`"))

(define (link text url)
  (doc 'type '(-> String String String))
  (doc 'description "[text](url)")
  (string-append "[" text "](" url ")"))

(doc 'section 'block-formatting)

(define (header level text)
  (doc 'type '(-> Integer String String))
  (doc 'description "# Title")
  (string-append
   (make-string level #\#) " " text "\n"))

(define (section title . content)
  (doc 'type '(-> String String ... String))
  (doc 'description "Creates a visual section with a title and content")
  (doc 'note "Uses a separator line for visual distinction in plain text/markdown")
  (string-append
   title "\n"
   (make-string (string-length title) #\-) "\n"
   (if (null? content) "" (apply string-append content))
   "\n\n"))

(define (quote text)
  (doc 'type '(-> String String))
  (doc 'description "> Blockquote")
  (let ([lines (string-split text #\newline)])
       (string-append
        (string-join
         (map (lambda (line) (string-append "> " line)) lines)
         "\n")
        "\n")))

;;; code-block : String [String] → String
;;; ```lang
;;; code
;;; ```
(define code-block
  (case-lambda
   [(text) (code-block text "")]
   [(text lang)
    (string-append "```" lang "\n"
                   (if (string-ends-with? text "\n") text (string-append text "\n"))
                   "```\n")]))

(doc 'section 'lists)

(define bullet-list
  (doc 'type '(-> (List String) Integer String))
  (doc 'description "Create a bulleted list with optional indentation level")
  (case-lambda
   [(items) (bullet-list items 0)]
   [(items indent)
    (if (null? items)
        ""
        (let ([prefix (make-string (* indent 2) #\space)])
             (string-append
              (string-join
               (map (lambda (item) (string-append prefix "• " item)) items)
               "\n")
              "\n")))]))

(define numbered-list
  (doc 'type '(-> (List String) Integer String))
  (doc 'description "Create a numbered list with optional indentation level")
  (case-lambda
   [(items) (numbered-list items 0)]
   [(items indent)
    (if (null? items)
        ""
        (let ([prefix (make-string (* indent 2) #\space)])
             (let loop ([items items] [n 1] [acc ""])
                  (if (null? items)
                      acc
                      (loop (cdr items)
                            (+ n 1)
                            (string-append acc
                                           prefix (number->string n) ". " (car items) "\n"))))))]))

(printf "✓ Markdown utilities loaded\n")
