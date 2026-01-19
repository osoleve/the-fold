;;; lattice/dsl/markdown/block-parser.ss — Markdown Block Parser
;;;
;;; Parses block-level markdown elements: headings, paragraphs,
;;; code blocks, lists, blockquotes, horizontal rules.
;;;
;;; This is Lattice code: pure, total, no IO.
;;;
;;; Block elements:
;;;   # Heading 1 ... ###### Heading 6
;;;   Paragraph text...
;;;   ```lang
;;;   code
;;;   ```
;;;   > blockquote
;;;   - unordered list item
;;;   1. ordered list item
;;;   --- or *** or ___ (horizontal rule)
;;;
;;; Dependencies:
;;;   - fp/parsing/parser.ss
;;;   - ast.ss
;;;   - inline-parser.ss

(load "core/base/prelude.ss")
(load "lattice/fp/parsing/parser.ss")
(load "lattice/dsl/markdown/ast.ss")
(load "lattice/dsl/markdown/inline-parser.ss")

;;; ============================================================
;;; Character Helpers
;;; ============================================================

(define %newline-char (integer->char 10))

;;; not-newline : Parser Char
(define not-newline
  (satisfy (lambda (c) (not (char=? c %newline-char)))
           "non-newline character"))

;;; rest-of-line : Parser String
;;; Parse the rest of the line (not including newline).
(define rest-of-line
  (parser-map list->string (many not-newline)))

;;; blank-line : Parser Unit
;;; Parse a blank line (whitespace only, then newline or eof).
(define md-blank-line
  (parser-then (many (one-of " \t"))
               (parser-or (char %newline-char)
                          eof)))

;;; skip-blank-lines : Parser Unit
(define md-skip-blank-lines
  (many md-blank-line))

;;; ============================================================
;;; Block Parsers
;;; ============================================================

;;; Forward declarations
(define md-block #f)

;;; --- ATX Headings ---

;;; md-heading : Parser AST
;;; Parse ATX-style heading: # Heading
(define md-atx-heading
  (parser-bind (some (char #\#))
               (lambda (hashes)
                 (let ([level (min 6 (length hashes))])
                   (parser-bind (many (one-of " \t"))
                                (lambda (_)
                                  (parser-bind rest-of-line
                                               (lambda (text)
                                                 (parser-bind (parser-or (char %newline-char) eof)
                                                              (lambda (_)
                                                                ;; Strip trailing hashes and spaces
                                                                (let* ([trimmed (string-trim-right text)]
                                                                       [cleaned (strip-trailing-hashes trimmed)]
                                                                       [inlines (parse-inline-or-text cleaned)])
                                                                  (parser-pure (md-heading level inlines)))))))))))))

;;; strip-trailing-hashes : String -> String
;;; Remove trailing # characters and spaces from heading.
(define (strip-trailing-hashes str)
  (let ([len (string-length str)])
    (if (= len 0)
        str
        (let loop ([i (- len 1)])
          (cond
            [(< i 0) ""]
            [(char=? (string-ref str i) #\#)
             (loop (- i 1))]
            [(char=? (string-ref str i) #\space)
             (loop (- i 1))]
            [else
             (substring str 0 (+ i 1))])))))

;;; --- Fenced Code Blocks ---

;;; md-code-fence : Parser (String × Char)
;;; Parse opening fence: ``` or ~~~, return (lang, fence-char).
(define md-code-fence-open
  (parser-or
   (parser-bind (string-parser "```")
                (lambda (_)
                  (parser-bind rest-of-line
                               (lambda (lang)
                                 (parser-bind (char %newline-char)
                                              (lambda (_)
                                                (parser-pure (cons (string-trim lang) #\`))))))))
   (parser-bind (string-parser "~~~")
                (lambda (_)
                  (parser-bind rest-of-line
                               (lambda (lang)
                                 (parser-bind (char %newline-char)
                                              (lambda (_)
                                                (parser-pure (cons (string-trim lang) #\~))))))))))

;;; md-code-block : Parser AST
;;; Parse fenced code block.
(define md-fenced-code-block
  (parser-bind md-code-fence-open
               (lambda (lang-and-char)
                 (let ([lang (car lang-and-char)]
                       [fence-char (cdr lang-and-char)])
                   (let ([close-fence (if (char=? fence-char #\`)
                                          "```"
                                          "~~~")])
                     (parser-bind (md-code-content-until close-fence)
                                  (lambda (code)
                                    (parser-pure (md-code-block lang code)))))))))

;;; md-code-content-until : String -> Parser String
;;; Parse code content until closing fence.
(define (md-code-content-until fence)
  (make-parser
   (lambda (state)
     (let* ([input (state-input state)]
            [start-idx (state-index state)]
            [input-len (string-length input)]
            [fence-len (string-length fence)])
       ;; Search for fence at start of line
       (let loop ([idx start-idx]
                  [line-start #t])
         (cond
           [(>= idx input-len)
            ;; EOF without closing fence - take all remaining as code
            (let* ([code (substring input start-idx idx)]
                   [new-pos (fold-left advance-pos (state-pos state)
                                       (string->list code))]
                   [new-state (make-state input idx new-pos)])
              (right (cons code new-state)))]

           [(and line-start
                 (<= (+ idx fence-len) input-len)
                 (string=? (substring input idx (+ idx fence-len)) fence))
            ;; Found closing fence at line start
            (let* ([code (substring input start-idx idx)]
                   ;; Skip the fence and possible newline
                   [after-fence (+ idx fence-len)]
                   [final-idx (if (and (< after-fence input-len)
                                       (char=? (string-ref input after-fence) %newline-char))
                                  (+ after-fence 1)
                                  after-fence)]
                   [consumed (substring input start-idx final-idx)]
                   [new-pos (fold-left advance-pos (state-pos state)
                                       (string->list consumed))]
                   [new-state (make-state input final-idx new-pos)])
              (right (cons code new-state)))]

           [(char=? (string-ref input idx) %newline-char)
            (loop (+ idx 1) #t)]

           [else
            (loop (+ idx 1) #f)]))))))

;;; --- Blockquotes ---

;;; md-blockquote-line : Parser String
;;; Parse a single blockquote line: > text
(define md-blockquote-line
  (parser-bind (char #\>)
               (lambda (_)
                 (parser-bind (optional (char #\space) #\space)
                              (lambda (_)
                                (parser-bind rest-of-line
                                             (lambda (text)
                                               (parser-bind (parser-or (char %newline-char) eof)
                                                            (lambda (_)
                                                              (parser-pure text))))))))))

;;; md-blockquote : Parser AST
;;; Parse a blockquote block.
(define md-blockquote-block
  (parser-bind (some md-blockquote-line)
               (lambda (lines)
                 (let* ([content (string-join lines "\n")]
                        [inlines (parse-inline-or-text content)])
                   (parser-pure (md-blockquote (list (md-paragraph inlines))))))))

;;; --- Lists ---

;;; md-ul-marker : Parser Unit
;;; Parse unordered list marker: - or * or +
(define md-ul-marker
  (parser-bind (one-of "-*+")
               (lambda (_)
                 (parser-bind (some (one-of " \t"))
                              (lambda (_)
                                (parser-pure '()))))))

;;; md-ol-marker : Parser Unit
;;; Parse ordered list marker: 1. or 1)
(define md-ol-marker
  (parser-bind (some digit)
               (lambda (_)
                 (parser-bind (one-of ".)")
                              (lambda (_)
                                (parser-bind (some (one-of " \t"))
                                             (lambda (_)
                                               (parser-pure '()))))))))

;;; md-list-item-content : Parser String
;;; Parse content of a list item (rest of line).
(define md-list-item-content
  (parser-bind rest-of-line
               (lambda (text)
                 (parser-bind (parser-or (char %newline-char) eof)
                              (lambda (_)
                                (parser-pure text))))))

;;; md-ul-item : Parser AST
;;; Parse an unordered list item.
(define md-ul-item
  (parser-bind md-ul-marker
               (lambda (_)
                 (parser-bind md-list-item-content
                              (lambda (text)
                                (let ([inlines (parse-inline-or-text (string-trim text))])
                                  (parser-pure (md-list-item inlines))))))))

;;; md-ol-item : Parser AST
;;; Parse an ordered list item.
(define md-ol-item
  (parser-bind md-ol-marker
               (lambda (_)
                 (parser-bind md-list-item-content
                              (lambda (text)
                                (let ([inlines (parse-inline-or-text (string-trim text))])
                                  (parser-pure (md-list-item inlines))))))))

;;; md-unordered-list : Parser AST
(define md-unordered-list
  (parser-bind (some md-ul-item)
               (lambda (items)
                 (parser-pure (md-unordered-list items)))))

;;; md-ordered-list : Parser AST
(define md-ordered-list
  (parser-bind (some md-ol-item)
               (lambda (items)
                 (parser-pure (md-ordered-list items)))))

;;; --- Tables ---

;;; table-line? : String -> Boolean
;;; Check if a line looks like a table row (starts with |).
(define (table-line? str)
  (and (> (string-length str) 0)
       (char=? (string-ref str 0) #\|)))

;;; separator-line? : String -> Boolean
;;; Check if a line is a table separator (|---|---|).
(define (separator-line? str)
  (and (table-line? str)
       (let ([len (string-length str)])
         (let loop ([i 1] [has-dash #f])
           (cond
             [(>= i len) has-dash]
             [(char=? (string-ref str i) #\-) (loop (+ i 1) #t)]
             [(char=? (string-ref str i) #\|) (loop (+ i 1) has-dash)]
             [(char=? (string-ref str i) #\:) (loop (+ i 1) has-dash)]
             [(char=? (string-ref str i) #\space) (loop (+ i 1) has-dash)]
             [else #f])))))

;;; parse-table-row : String -> (List String)
;;; Split a table row into cells.
(define (parse-table-row str)
  (let* ([trimmed (string-trim str)]
         [len (string-length trimmed)])
    ;; Remove leading and trailing |
    (let* ([start (if (and (> len 0) (char=? (string-ref trimmed 0) #\|)) 1 0)]
           [end (if (and (> len 0) (char=? (string-ref trimmed (- len 1)) #\|))
                    (- len 1)
                    len)]
           [inner (if (< start end) (substring trimmed start end) "")])
      ;; Split on |
      (let loop ([i 0] [cell-start 0] [cells '()])
        (cond
          [(>= i (string-length inner))
           (reverse (cons (string-trim (substring inner cell-start i)) cells))]
          [(char=? (string-ref inner i) #\|)
           (loop (+ i 1) (+ i 1)
                 (cons (string-trim (substring inner cell-start i)) cells))]
          [else (loop (+ i 1) cell-start cells)])))))

;;; md-table-row-line : Parser String
;;; Parse a single table row line.
(define md-table-row-line
  (parser-bind (char #\|)
               (lambda (_)
                 (parser-bind rest-of-line
                              (lambda (text)
                                (parser-bind (parser-or (char %newline-char) eof)
                                             (lambda (_)
                                               (parser-pure (string-append "|" text)))))))))

;;; md-table-block : Parser AST
;;; Parse a markdown table.
(define md-table-block
  (parser-bind md-table-row-line
               (lambda (header-line)
                 (parser-bind md-table-row-line
                              (lambda (sep-line)
                                (if (separator-line? sep-line)
                                    (parser-bind (many md-table-row-line)
                                                 (lambda (data-lines)
                                                   (let* ([header-cells (parse-table-row header-line)]
                                                          [header (map (lambda (c)
                                                                         (parse-inline-or-text c))
                                                                       header-cells)]
                                                          [rows (map (lambda (line)
                                                                       (map (lambda (c)
                                                                              (parse-inline-or-text c))
                                                                            (parse-table-row line)))
                                                                     data-lines)])
                                                     (parser-pure (md-table header rows)))))
                                    (parser-fail "Expected table separator line")))))))

;;; --- Horizontal Rule ---

;;; md-hr : Parser AST
;;; Parse horizontal rule: ---, ***, ___
(define md-hr-parser
  (parser-bind (parser-or (try (parser-bind (count 3 (char #\-))
                                             (lambda (_) (many (char #\-)))))
                          (parser-or (try (parser-bind (count 3 (char #\*))
                                                        (lambda (_) (many (char #\*)))))
                                     (parser-bind (count 3 (char #\_))
                                                  (lambda (_) (many (char #\_))))))
               (lambda (_)
                 (parser-bind (many (one-of " \t"))
                              (lambda (_)
                                (parser-bind (parser-or (char %newline-char) eof)
                                             (lambda (_)
                                               (parser-pure (md-hr)))))))))

;;; --- Paragraph ---

;;; md-paragraph-line : Parser String
;;; Parse a line that's part of a paragraph.
(define md-paragraph-line
  (parser-bind (not-followed-by (choice (list (char #\#)
                                               (string-parser "```")
                                               (string-parser "~~~")
                                               (char #\>)
                                               (char #\|)  ; Table rows
                                               md-ul-marker
                                               md-ol-marker
                                               md-hr-parser
                                               eof)))
               (lambda (_)
                 (parser-bind (some not-newline)
                              (lambda (chars)
                                (parser-bind (parser-or (char %newline-char) eof)
                                             (lambda (_)
                                               (parser-pure (list->string chars)))))))))

;;; md-paragraph : Parser AST
;;; Parse a paragraph (one or more non-blank lines).
(define md-paragraph-block
  (parser-bind (some md-paragraph-line)
               (lambda (lines)
                 (let* ([content (string-join lines " ")]
                        [inlines (parse-inline-or-text (string-trim content))])
                   (parser-pure (md-paragraph inlines))))))

;;; --- Combined Block Parser ---

;;; md-block-element : Parser AST
;;; Parse a single block element.
(define md-block-element
  (parser-then
   md-skip-blank-lines
   (choice (list (try md-atx-heading)
                 (try md-fenced-code-block)
                 (try md-blockquote-block)
                 (try md-hr-parser)
                 (try md-table-block)
                 (try md-unordered-list)
                 (try md-ordered-list)
                 md-paragraph-block))))

;;; Initialize md-block
(set! md-block md-block-element)

;;; md-document-parser : Parser AST
;;; Parse a complete markdown document.
(define md-document-parser
  (parser-bind md-skip-blank-lines
               (lambda (_)
                 (parser-bind (many md-block-element)
                              (lambda (blocks)
                                (parser-bind md-skip-blank-lines
                                             (lambda (_)
                                               (parser-bind eof
                                                            (lambda (_)
                                                              (parser-pure (md-document blocks)))))))))))

;;; ============================================================
;;; Public API
;;; ============================================================

;;; parse-markdown : String -> (Either Error AST)
;;; Parse a complete markdown document.
(define (parse-markdown input)
  (parse md-document-parser input))

;;; parse-markdown-or-error : String -> AST
;;; Parse markdown, returning error document on failure.
(define (parse-markdown-or-error input)
  (let ([result (parse-markdown input)])
    (if (right? result)
        (from-right result)
        (md-document
         (list (md-paragraph
                (list (md-text (string-append "Parse error: "
                                              (format-error (from-left result)))))))))))
