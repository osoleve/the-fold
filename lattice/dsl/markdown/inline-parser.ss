;;; @module inline-parser
;;; @requires prelude parser markdown-ast
(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
(require 'prelude)
(require 'parser)
(require 'markdown-ast)

(doc 'module 'markdown-inline-parser)
(doc 'description "Markdown Inline Parser - Parses inline markdown elements: emphasis, code, links, plain text")
(doc 'layer 'lattice)
(doc 'purity 'partial)

(doc 'note "Inline elements:
  **bold** or __bold__     -> (strong ...)
  *italic* or _italic_     -> (em ...)
  `code`                   -> (code \"...\")
  [text](url)              -> (link url \"\" (text ...))
  [text](url \"title\")      -> (link url \"title\" (text ...))
  plain text               -> (text \"...\")")

(doc 'section 'character-classes)

;;; Special characters that need escaping or have meaning
(define *md-special-chars* '(#\* #\_ #\` #\[ #\] #\( #\) #\\))

;;; md-special-char? : Char -> Boolean
(define (md-special-char? c)
  (doc 'export #t)
  (memv c *md-special-chars*))

(doc 'section 'inline-parsers)

;;; Forward declarations for recursive parsers
(define md-inline #f)
(define md-inline-seq #f)

;;; --- Escaped Character ---

;;; md-escaped : Parser AST
;;; Parse backslash-escaped character: \* \_ \` etc.
(define md-escaped
  (parser-bind (parser-char #\\)
               (lambda (_)
                 (parser-bind parser-any-char
                              (lambda (c)
                                (parser-pure (md-text (string c))))))))

;;; --- Inline Code ---

;;; md-inline-code : Parser AST
;;; Parse inline code: `code` or ``code with `backtick` ``
(define md-inline-code
  (parser-or
   ;; Double backtick
   (parser-bind (parser-string "``")
                (lambda (_)
                  (parser-bind (parser-many-till parser-any-char (parser-string "``"))
                               (lambda (chars)
                                 (parser-pure (md-code (string-trim (list->string chars))))))))
   ;; Single backtick
   (parser-bind (parser-char #\`)
                (lambda (_)
                  (parser-bind (parser-many-till parser-any-char (parser-char #\`))
                               (lambda (chars)
                                 (parser-pure (md-code (list->string chars)))))))))

;;; --- Links ---

;;; md-link-text-char : Parser Char
;;; Characters allowed in link text (not ] or [).
(define md-link-text-char
  (parser-satisfy (lambda (c) (and (not (char=? c #\]))
                            (not (char=? c #\[))))
           "link text character"))

;;; md-link-url-char : Parser Char
;;; Characters allowed in URL (not ) or space).
(define md-link-url-char
  (parser-satisfy (lambda (c) (and (not (char=? c #\)))
                            (not (char=? c #\space))
                            (not (char=? c #\tab))))
           "URL character"))

;;; md-link-parser : Parser AST
;;; Parse a link: [text](url) or [text](url "title")
(define md-link-parser
  (parser-bind (parser-char #\[)
               (lambda (_)
                 (parser-bind (parser-many md-link-text-char)
                              (lambda (text-chars)
                                (parser-bind (parser-char #\])
                                             (lambda (_)
                                               (parser-bind (parser-char #\()
                                                            (lambda (_)
                                                              (parser-bind parser-spaces
                                                                           (lambda (_)
                                                                             (parser-bind (parser-many md-link-url-char)
                                                                                          (lambda (url-chars)
                                                                                            (parser-bind parser-spaces
                                                                                                         (lambda (_)
                                                                                                           (parser-bind (parser-optional (md-link-title-parser) "")
                                                                                                                        (lambda (title)
                                                                                                                          (parser-bind parser-spaces
                                                                                                                                       (lambda (_)
                                                                                                                                         (parser-bind (parser-char #\))
                                                                                                                                                      (lambda (_)
                                                                                                                                                        (let ([url (list->string url-chars)]
                                                                                                                                                              [text (list->string text-chars)])
                                                                                                                                                          (parser-pure
                                                                                                                                                           (md-link url title
                                                                                                                                                                    (list (md-text text))))))))))))))))))))))))))

;;; md-link-title-parser : Parser String
;;; Parse optional link title: "title" or 'title'
(define (md-link-title-parser)
  (doc 'export #t)
  (parser-or
   ;; Double-quoted title
   (parser-bind (parser-char #\")
                (lambda (_)
                  (parser-bind (parser-many (parser-satisfy (lambda (c) (not (char=? c #\")))
                                              "title character"))
                               (lambda (chars)
                                 (parser-bind (parser-char #\")
                                              (lambda (_)
                                                (parser-pure (list->string chars))))))))
   ;; Single-quoted title
   (parser-bind (parser-char #\')
                (lambda (_)
                  (parser-bind (parser-many (parser-satisfy (lambda (c) (not (char=? c #\')))
                                              "title character"))
                               (lambda (chars)
                                 (parser-bind (parser-char #\')
                                              (lambda (_)
                                                (parser-pure (list->string chars))))))))))

;;; --- Emphasis ---

;;; Note: Emphasis parsing is tricky due to nesting and ambiguity.
;;; We use a simplified approach: **strong**, *em*, no mixing.

;;; md-strong-star : Parser AST
;;; Parse strong emphasis with **: **text**
(define md-strong-star
  (parser-bind (parser-string "**")
               (lambda (_)
                 (parser-bind (parser-some (md-inline-not "**"))
                              (lambda (inlines)
                                (parser-bind (parser-string "**")
                                             (lambda (_)
                                               (parser-pure (md-strong inlines)))))))))

;;; md-strong-under : Parser AST
;;; Parse strong emphasis with __: __text__
(define md-strong-under
  (parser-bind (parser-string "__")
               (lambda (_)
                 (parser-bind (parser-some (md-inline-not "__"))
                              (lambda (inlines)
                                (parser-bind (parser-string "__")
                                             (lambda (_)
                                               (parser-pure (md-strong inlines)))))))))

;;; md-em-star : Parser AST
;;; Parse emphasis with *: *text*
(define md-em-star
  (parser-bind (parser-char #\*)
               (lambda (_)
                 (parser-bind (parser-not-followed-by (parser-char #\*))
                              (lambda (_)
                                (parser-bind (parser-some (md-inline-not-char #\*))
                                             (lambda (inlines)
                                               (parser-bind (parser-char #\*)
                                                            (lambda (_)
                                                              (parser-pure (md-em inlines)))))))))))

;;; md-em-under : Parser AST
;;; Parse emphasis with _: _text_
(define md-em-under
  (parser-bind (parser-char #\_)
               (lambda (_)
                 (parser-bind (parser-not-followed-by (parser-char #\_))
                              (lambda (_)
                                (parser-bind (parser-some (md-inline-not-char #\_))
                                             (lambda (inlines)
                                               (parser-bind (parser-char #\_)
                                                            (lambda (_)
                                                              (parser-pure (md-em inlines)))))))))))

;;; md-strong : Parser AST
;;; Parse strong (bold) text.
(define md-strong-parser
  (parser-or (parser-try md-strong-star)
             (parser-try md-strong-under)))

;;; md-em-parser : Parser AST
;;; Parse emphasized (italic) text.
(define md-em-parser
  (parser-or (parser-try md-em-star)
             (parser-try md-em-under)))

;;; --- Plain Text ---

;;; md-plain-char : Parser Char
;;; A character that's not special (not starting emphasis, code, link, etc.)
(define md-plain-char
  (parser-satisfy (lambda (c)
             (not (memv c '(#\* #\_ #\` #\[ #\\))))
           "plain text character"))

;;; md-plain-text : Parser AST
;;; Parse a run of plain text.
(define md-plain-text
  (parser-bind (parser-some md-plain-char)
               (lambda (chars)
                 (parser-pure (md-text (list->string chars))))))

;;; --- Combined Inline Parser ---

;;; md-inline-element : Parser AST
;;; Parse a single inline element.
(define md-inline-element
  (parser-choice (list (parser-try md-escaped)
                (parser-try md-inline-code)
                (parser-try md-link-parser)
                (parser-try md-strong-parser)
                (parser-try md-em-parser)
                md-plain-text)))

;;; Initialize md-inline
(set! md-inline md-inline-element)

;;; md-inline-not : String -> Parser AST
;;; Parse inline element that doesn't start with the given terminator.
(define (md-inline-not terminator)
  (doc 'export #t)
  (parser-bind (parser-not-followed-by (parser-string terminator))
               (lambda (_)
                 (parser-or md-inline-element
                            ;; If nothing else matches, take one char as text
                            (parser-bind parser-any-char
                                         (lambda (c)
                                           (parser-pure (md-text (string c)))))))))

;;; md-inline-not-char : Char -> Parser AST
;;; Parse inline element that doesn't start with the given character.
(define (md-inline-not-char c)
  (doc 'export #t)
  (parser-bind (parser-not-followed-by (parser-char c))
               (lambda (_)
                 (parser-or md-inline-element
                            (parser-bind parser-any-char
                                         (lambda (ch)
                                           (parser-pure (md-text (string ch)))))))))

;;; Initialize md-inline-seq
(set! md-inline-seq (parser-many md-inline-element))

;;; ============================================================
;;; Public API
;;; ============================================================

;;; parse-inline : String -> (Either Error (List AST))
;;; Parse a string of inline markdown into a list of AST nodes.
(define (parse-inline input)
  (doc 'export #t)
  (parse-all md-inline-seq input))

;;; parse-inline-or-text : String -> (List AST)
;;; Parse inline markdown, falling back to plain text on error.
(define (parse-inline-or-text input)
  (doc 'export #t)
  (let ([result (parse-inline input)])
    (if (right? result)
        (from-right result)
        (list (md-text input)))))
