;;; lattice/dsl/markdown/inline-parser.ss — Markdown Inline Parser
;;;
;;; Parses inline markdown elements: emphasis, code, links, plain text.
;;;
;;; This is Lattice code: pure, total, no IO.
;;;
;;; Inline elements:
;;;   **bold** or __bold__     -> (strong ...)
;;;   *italic* or _italic_     -> (em ...)
;;;   `code`                   -> (code "...")
;;;   [text](url)              -> (link url "" (text ...))
;;;   [text](url "title")      -> (link url "title" (text ...))
;;;   plain text               -> (text "...")
;;;
;;; Dependencies:
;;;   - fp/parsing/parser.ss
;;;   - ast.ss

(load "core/base/prelude.ss")
(load "lattice/fp/parsing/parser.ss")
(load "lattice/dsl/markdown/ast.ss")

;;; ============================================================
;;; Character Classes
;;; ============================================================

;;; Special characters that need escaping or have meaning
(define *md-special-chars* '(#\* #\_ #\` #\[ #\] #\( #\) #\\))

;;; md-special-char? : Char -> Boolean
(define (md-special-char? c)
  (memv c *md-special-chars*))

;;; ============================================================
;;; Inline Parsers
;;; ============================================================

;;; Forward declarations for recursive parsers
(define md-inline #f)
(define md-inline-seq #f)

;;; --- Escaped Character ---

;;; md-escaped : Parser AST
;;; Parse backslash-escaped character: \* \_ \` etc.
(define md-escaped
  (parser-bind (char #\\)
               (lambda (_)
                 (parser-bind any-char
                              (lambda (c)
                                (parser-pure (md-text (string c))))))))

;;; --- Inline Code ---

;;; md-inline-code : Parser AST
;;; Parse inline code: `code` or ``code with `backtick` ``
(define md-inline-code
  (parser-or
   ;; Double backtick
   (parser-bind (string-parser "``")
                (lambda (_)
                  (parser-bind (many-till any-char (string-parser "``"))
                               (lambda (chars)
                                 (parser-pure (md-code (string-trim (list->string chars))))))))
   ;; Single backtick
   (parser-bind (char #\`)
                (lambda (_)
                  (parser-bind (many-till any-char (char #\`))
                               (lambda (chars)
                                 (parser-pure (md-code (list->string chars)))))))))

;;; --- Links ---

;;; md-link-text-char : Parser Char
;;; Characters allowed in link text (not ] or [).
(define md-link-text-char
  (satisfy (lambda (c) (and (not (char=? c #\]))
                            (not (char=? c #\[))))
           "link text character"))

;;; md-link-url-char : Parser Char
;;; Characters allowed in URL (not ) or space).
(define md-link-url-char
  (satisfy (lambda (c) (and (not (char=? c #\)))
                            (not (char=? c #\space))
                            (not (char=? c #\tab))))
           "URL character"))

;;; md-link : Parser AST
;;; Parse a link: [text](url) or [text](url "title")
(define md-link
  (parser-bind (char #\[)
               (lambda (_)
                 (parser-bind (many md-link-text-char)
                              (lambda (text-chars)
                                (parser-bind (char #\])
                                             (lambda (_)
                                               (parser-bind (char #\()
                                                            (lambda (_)
                                                              (parser-bind spaces
                                                                           (lambda (_)
                                                                             (parser-bind (many md-link-url-char)
                                                                                          (lambda (url-chars)
                                                                                            (parser-bind spaces
                                                                                                         (lambda (_)
                                                                                                           (parser-bind (optional (md-link-title-parser) "")
                                                                                                                        (lambda (title)
                                                                                                                          (parser-bind spaces
                                                                                                                                       (lambda (_)
                                                                                                                                         (parser-bind (char #\))
                                                                                                                                                      (lambda (_)
                                                                                                                                                        (let ([url (list->string url-chars)]
                                                                                                                                                              [text (list->string text-chars)])
                                                                                                                                                          (parser-pure
                                                                                                                                                           (md-link url title
                                                                                                                                                                    (list (md-text text))))))))))))))))))))))))))

;;; md-link-title-parser : Parser String
;;; Parse optional link title: "title" or 'title'
(define (md-link-title-parser)
  (parser-or
   ;; Double-quoted title
   (parser-bind (char #\")
                (lambda (_)
                  (parser-bind (many (satisfy (lambda (c) (not (char=? c #\")))
                                              "title character"))
                               (lambda (chars)
                                 (parser-bind (char #\")
                                              (lambda (_)
                                                (parser-pure (list->string chars))))))))
   ;; Single-quoted title
   (parser-bind (char #\')
                (lambda (_)
                  (parser-bind (many (satisfy (lambda (c) (not (char=? c #\')))
                                              "title character"))
                               (lambda (chars)
                                 (parser-bind (char #\')
                                              (lambda (_)
                                                (parser-pure (list->string chars))))))))))

;;; --- Emphasis ---

;;; Note: Emphasis parsing is tricky due to nesting and ambiguity.
;;; We use a simplified approach: **strong**, *em*, no mixing.

;;; md-strong-star : Parser AST
;;; Parse strong emphasis with **: **text**
(define md-strong-star
  (parser-bind (string-parser "**")
               (lambda (_)
                 (parser-bind (some (md-inline-not "**"))
                              (lambda (inlines)
                                (parser-bind (string-parser "**")
                                             (lambda (_)
                                               (parser-pure (md-strong inlines)))))))))

;;; md-strong-under : Parser AST
;;; Parse strong emphasis with __: __text__
(define md-strong-under
  (parser-bind (string-parser "__")
               (lambda (_)
                 (parser-bind (some (md-inline-not "__"))
                              (lambda (inlines)
                                (parser-bind (string-parser "__")
                                             (lambda (_)
                                               (parser-pure (md-strong inlines)))))))))

;;; md-em-star : Parser AST
;;; Parse emphasis with *: *text*
(define md-em-star
  (parser-bind (char #\*)
               (lambda (_)
                 (parser-bind (not-followed-by (char #\*))
                              (lambda (_)
                                (parser-bind (some (md-inline-not-char #\*))
                                             (lambda (inlines)
                                               (parser-bind (char #\*)
                                                            (lambda (_)
                                                              (parser-pure (md-em inlines)))))))))))

;;; md-em-under : Parser AST
;;; Parse emphasis with _: _text_
(define md-em-under
  (parser-bind (char #\_)
               (lambda (_)
                 (parser-bind (not-followed-by (char #\_))
                              (lambda (_)
                                (parser-bind (some (md-inline-not-char #\_))
                                             (lambda (inlines)
                                               (parser-bind (char #\_)
                                                            (lambda (_)
                                                              (parser-pure (md-em inlines)))))))))))

;;; md-strong : Parser AST
;;; Parse strong (bold) text.
(define md-strong-parser
  (parser-or (try md-strong-star)
             (try md-strong-under)))

;;; md-em-parser : Parser AST
;;; Parse emphasized (italic) text.
(define md-em-parser
  (parser-or (try md-em-star)
             (try md-em-under)))

;;; --- Plain Text ---

;;; md-plain-char : Parser Char
;;; A character that's not special (not starting emphasis, code, link, etc.)
(define md-plain-char
  (satisfy (lambda (c)
             (not (memv c '(#\* #\_ #\` #\[ #\\))))
           "plain text character"))

;;; md-plain-text : Parser AST
;;; Parse a run of plain text.
(define md-plain-text
  (parser-bind (some md-plain-char)
               (lambda (chars)
                 (parser-pure (md-text (list->string chars))))))

;;; --- Combined Inline Parser ---

;;; md-inline-element : Parser AST
;;; Parse a single inline element.
(define md-inline-element
  (choice (list (try md-escaped)
                (try md-inline-code)
                (try md-link)
                (try md-strong-parser)
                (try md-em-parser)
                md-plain-text)))

;;; Initialize md-inline
(set! md-inline md-inline-element)

;;; md-inline-not : String -> Parser AST
;;; Parse inline element that doesn't start with the given terminator.
(define (md-inline-not terminator)
  (parser-bind (not-followed-by (string-parser terminator))
               (lambda (_)
                 (parser-or md-inline-element
                            ;; If nothing else matches, take one char as text
                            (parser-bind any-char
                                         (lambda (c)
                                           (parser-pure (md-text (string c)))))))))

;;; md-inline-not-char : Char -> Parser AST
;;; Parse inline element that doesn't start with the given character.
(define (md-inline-not-char c)
  (parser-bind (not-followed-by (char c))
               (lambda (_)
                 (parser-or md-inline-element
                            (parser-bind any-char
                                         (lambda (ch)
                                           (parser-pure (md-text (string ch)))))))))

;;; Initialize md-inline-seq
(set! md-inline-seq (many md-inline-element))

;;; ============================================================
;;; Public API
;;; ============================================================

;;; parse-inline : String -> (Either Error (List AST))
;;; Parse a string of inline markdown into a list of AST nodes.
(define (parse-inline input)
  (parse-all md-inline-seq input))

;;; parse-inline-or-text : String -> (List AST)
;;; Parse inline markdown, falling back to plain text on error.
(define (parse-inline-or-text input)
  (let ([result (parse-inline input)])
    (if (right? result)
        (from-right result)
        (list (md-text input)))))
