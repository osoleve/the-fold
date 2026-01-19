;;; boundary/markdown.ss — Markdown Generation Helpers
;;;
;;; Utilities for generating Markdown-formatted text programmatically.
;;; Useful for generating reports, prompts, and documentation.
;;;
;;; TIER ASSIGNMENT:
;;;   Tier 5: Formatting utilities
;;;
;;; Usage:
;;;   (load "boundary/markdown.ss")
;;;   (bold "text") -> "**text**"
;;;   (bullet-list '("a" "b")) -> "• a\\n• b"

(load "boundary/tools/string-utils.ss")

;;; ====
;;; Inline Formatting
;;; ====

;;; bold : String → String
(define (bold text)
  (string-append "**" text "**"))

;;; italic : String → String
(define (italic text)
  (string-append "*" text "*"))

;;; code : String → String
;;; Inline code span
(define (code text)
  (string-append "`" text "`"))

;;; link : String String → String
;;; [text](url)
(define (link text url)
  (string-append "[" text "](" url ")"))

;;; ====
;;; Block Formatting
;;; ====

;;; header : Integer String → String
;;; # Title
(define (header level text)
  (string-append
   (make-string level #\#) " " text "\n"))

;;; section : String String... → String
;;; Creates a visual section with a title and content.
;;; Uses a separator line for visual distinction in plain text/markdown.
;;;
;;; Example:
;;; Title
;;; ─────
;;; Content...
(define (section title . content)
  (string-append
   title "\n"
   (make-string (string-length title) #\─) "\n"
   (if (null? content) "" (apply string-append content))
   "\n\n"))

;;; quote : String → String
;;; > Blockquote
(define (quote text)
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

;;; ====
;;; Lists
;;; ====

;;; bullet-list : List<String> [Integer] → String
;;; Create a bulleted list. Optional indentation level.
;;; bullet-list : List<String> [Integer] → String
;;; Create a bulleted list. Optional indentation level.
(define bullet-list
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

;;; numbered-list : List<String> [Integer] → String
;;; Create a numbered list. Optional indentation level.
(define numbered-list
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
