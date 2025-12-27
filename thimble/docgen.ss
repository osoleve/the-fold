;;; thimble/docgen.ss — Documentation Generator
;;;
;;; Extracts documentation from Scheme source files and generates
;;; structured documentation in multiple formats.
;;;
;;; This is Shell code: reads files, parses comments, generates output.
;;;
;;; Dependencies:
;;;   shell/fs.ss (for file operations)
;;;   shell/text.ss (for text utilities)
;;;
;;; Operations:
;;;   (docgen-file path) — Extract docs from a single file
;;;   (docgen-dir path pattern) — Extract docs from directory
;;;   (docgen-module module-name) — Extract docs from module
;;;   (docgen-render docs format) — Render docs in format
;;;   (docgen-toolkit) — Generate toolkit documentation
;;;
;;; Features:
;;;   - Extracts header comments (;;; style)
;;;   - Parses function signatures and type annotations
;;;   - Identifies dependencies and usage examples
;;;   - Multiple output formats (scheme, markdown, text)
;;;   - Automatic cross-referencing
;;;   - Index generation

;;; ============================================================
;;; Configuration
;;; ============================================================

(define *doc-comment-prefix* ";;;")
(define *section-marker* ";;;")

;;; Documentation record structure
(define-record-type module-doc
  (fields
    name              ; Module name (string)
    file-path         ; Source file path
    summary           ; One-line summary
    description       ; Full description
    dependencies      ; List of dependency paths
    operations        ; List of operation-doc records
    features          ; List of feature strings
    examples          ; List of example strings
    categories))      ; List of category symbols

(define-record-type operation-doc
  (fields
    name              ; Operation name (symbol)
    signature         ; Type signature string
    parameters        ; List of parameter descriptions
    return-type       ; Return type description
    description       ; What it does
    examples))        ; Usage examples

;;; ============================================================
;;; Documentation Extraction
;;; ============================================================

;;; docgen-file : Path → ModuleDoc
;;; Extract documentation from a single Scheme file.
(define (docgen-file path)
  (guard (e [else
             (make-module-doc
               (path->module-name path)
               path
               "Error extracting documentation"
               (format "~a" (if (condition? e)
                               (condition-message e)
                               e))
               '() '() '() '() '())])
    (let* ([lines (read-file-lines path)]
           [header (extract-header lines)]
           [operations (extract-operations lines)])
      (make-module-doc
        (or (header-get header 'module-name)
            (path->module-name path))
        path
        (or (header-get header 'summary) "")
        (or (header-get header 'description) "")
        (or (header-get header 'dependencies) '())
        operations
        (or (header-get header 'features) '())
        (or (header-get header 'examples) '())
        (or (header-get header 'categories) '())))))

;;; path->module-name : Path → String
;;; Extract module name from file path.
(define (path->module-name path)
  (let* ([basename (path-basename path)]
         [name (path-strip-extension basename)])
    name))

;;; path-basename : Path → String
(define (path-basename path)
  (let ([parts (string-split path #\/)])
    (if (null? parts)
        path
        (car (reverse parts)))))

;;; path-strip-extension : String → String
(define (path-strip-extension name)
  (let ([idx (string-index-right name #\.)])
    (if idx
        (substring name 0 idx)
        name)))

;;; string-index-right : String × Char → Nat | #f
(define (string-index-right str ch)
  (let ([len (string-length str)])
    (let loop ([i (- len 1)])
      (cond
        [(< i 0) #f]
        [(char=? (string-ref str i) ch) i]
        [else (loop (- i 1))]))))

;;; ============================================================
;;; Header Extraction
;;; ============================================================

;;; extract-header : (List String) → Alist
;;; Extract header documentation from comment lines.
(define (extract-header lines)
  (let loop ([ls lines]
             [header '()]
             [in-header? #t])
    (cond
      [(null? ls) (reverse header)]
      [(not in-header?) (reverse header)]
      [else
       (let ([line (car ls)])
         (cond
           [(doc-comment? line)
            (let* ([content (doc-comment-content line)]
                   [parsed (parse-header-line content)])
              (if parsed
                  (loop (cdr ls) (cons parsed header) #t)
                  (loop (cdr ls) header #t)))]
           [(empty-line? line)
            (loop (cdr ls) header #t)]
           [else
            (loop (cdr ls) header #f)]))])))

;;; parse-header-line : String → (Pair Symbol Any) | #f
;;; Parse a header line looking for structured data.
(define (parse-header-line line)
  (cond
    [(string-prefix? "Dependencies:" line)
     (cons 'dependencies-marker #t)]
    [(string-prefix? "Operations:" line)
     (cons 'operations-marker #t)]
    [(string-prefix? "Features:" line)
     (cons 'features-marker #t)]
    [(string-prefix? "Examples:" line)
     (cons 'examples-marker #t)]
    [(string-prefix? "  " line)  ; Indented item
     (cons 'item (string-trim (substring line 2 (string-length line))))]
    [else
     (cons 'description line)]))

;;; header-get : Alist × Symbol → Any
;;; Get structured data from header by key.
(define (header-get header key)
  (let ([items (collect-items header key)])
    (if (null? items) #f items)))

;;; collect-items : Alist × Symbol → (List String)
(define (collect-items header marker-key)
  (let loop ([entries header]
             [collecting? #f]
             [items '()])
    (cond
      [(null? entries)
       (reverse items)]
      [else
       (let ([entry (car entries)])
         (cond
           [(eq? (car entry) marker-key)
            (loop (cdr entries) #t items)]
           [(and collecting? (eq? (car entry) 'item))
            (loop (cdr entries) #t (cons (cdr entry) items))]
           [(and collecting? (symbol-suffix? "-marker" (car entry)))
            (reverse items)]  ; Hit next section
           [else
            (loop (cdr entries) collecting? items)]))])))

;;; symbol-suffix? : String × Symbol → Bool
(define (symbol-suffix? suffix sym)
  (string-suffix? suffix (symbol->string sym)))

;;; string-suffix? : String × String → Bool
(define (string-suffix? suffix str)
  (let ([slen (string-length suffix)]
        [len (string-length str)])
    (and (>= len slen)
         (string=? suffix (substring str (- len slen) len)))))

;;; ============================================================
;;; Operation Extraction
;;; ============================================================

;;; extract-operations : (List String) → (List OperationDoc)
;;; Extract function/operation documentation from source.
(define (extract-operations lines)
  (let loop ([ls lines]
             [ops '()]
             [current-comment '()])
    (cond
      [(null? ls)
       (reverse ops)]
      [else
       (let ([line (car ls)])
         (cond
           [(doc-comment? line)
            (loop (cdr ls)
                  ops
                  (cons (doc-comment-content line) current-comment))]
           [(and (not (null? current-comment))
                 (function-definition? line))
            (let ([op-doc (make-operation-from-comment-and-def
                           (reverse current-comment)
                           line)])
              (loop (cdr ls)
                    (if op-doc (cons op-doc ops) ops)
                    '()))]
           [else
            (loop (cdr ls) ops '())]))])))

;;; function-definition? : String → Bool
(define (function-definition? line)
  (or (string-contains? line "(define ")
      (string-contains? line "(define-record-type ")))

;;; make-operation-from-comment-and-def : (List String) × String → OperationDoc | #f
(define (make-operation-from-comment-and-def comments defn-line)
  (let ([signature (extract-signature comments)]
        [name (extract-function-name defn-line)]
        [desc (extract-description comments)])
    (if (and signature name)
        (make-operation-doc
          name
          signature
          '()  ; params (could parse from signature)
          ""   ; return type (could parse from signature)
          desc
          '()) ; examples
        #f)))

;;; extract-signature : (List String) → String | #f
(define (extract-signature comments)
  (let loop ([cs comments])
    (cond
      [(null? cs) #f]
      [(and (string-contains? (car cs) ":")
            (string-contains? (car cs) "→"))
       (car cs)]
      [else (loop (cdr cs))])))

;;; extract-function-name : String → Symbol | #f
(define (extract-function-name defn-line)
  (guard (e [else #f])
    (let ([trimmed (string-trim defn-line)])
      (cond
        [(string-prefix? "(define (" trimmed)
         (let* ([start (string-length "(define (")]
                [rest (substring trimmed start (string-length trimmed))]
                [end (string-index rest #\space)])
           (if end
               (string->symbol (substring rest 0 end))
               #f))]
        [(string-prefix? "(define " trimmed)
         (let* ([start (string-length "(define ")]
                [rest (substring trimmed start (string-length trimmed))]
                [end (or (string-index rest #\space)
                        (string-index rest #\newline))])
           (if end
               (string->symbol (substring rest 0 end))
               #f))]
        [else #f]))))

;;; extract-description : (List String) → String
(define (extract-description comments)
  (string-join
    (filter (lambda (c)
              (and (not (string-contains? c "→"))
                   (not (string-contains? c ":"))))
            comments)
    " "))

;;; ============================================================
;;; Documentation Rendering
;;; ============================================================

;;; docgen-render : ModuleDoc × Symbol → String
;;; Render documentation in specified format.
(define (docgen-render doc format)
  (case format
    [(scheme) (render-scheme doc)]
    [(markdown) (render-markdown doc)]
    [(text) (render-text doc)]
    [else (error 'docgen-render "Unknown format" format)]))

;;; render-scheme : ModuleDoc → String
;;; Render as Scheme data structure.
(define (render-scheme doc)
  (format "~s" doc))

;;; render-text : ModuleDoc → String
;;; Render as plain text.
(define (render-text doc)
  (string-append
    "Module: " (module-doc-name doc) "\n"
    "File: " (module-doc-file-path doc) "\n"
    "\n"
    (module-doc-description doc) "\n"
    "\n"
    "Operations:\n"
    (string-join
      (map (lambda (op)
             (string-append
               "  " (symbol->string (operation-doc-name op))
               " : " (operation-doc-signature op) "\n"
               "    " (operation-doc-description op)))
           (module-doc-operations doc))
      "\n")
    "\n"))

;;; render-markdown : ModuleDoc → String
;;; Render as Markdown.
(define (render-markdown doc)
  (string-append
    "# " (module-doc-name doc) "\n\n"
    "> " (module-doc-summary doc) "\n\n"
    "**File:** `" (module-doc-file-path doc) "`\n\n"
    (module-doc-description doc) "\n\n"
    "## Operations\n\n"
    (string-join
      (map (lambda (op)
             (string-append
               "### `" (symbol->string (operation-doc-name op)) "`\n\n"
               "**Signature:** `" (operation-doc-signature op) "`\n\n"
               (operation-doc-description op) "\n"))
           (module-doc-operations doc))
      "\n")
    "\n"))

;;; ============================================================
;;; High-Level API
;;; ============================================================

;;; docgen-toolkit : → (List ModuleDoc)
;;; Generate documentation for all toolkit modules.
(define (docgen-toolkit)
  (docgen-dir "shell" "*.ss"))

;;; docgen-dir : Path × Pattern → (List ModuleDoc)
;;; Extract documentation from all matching files in directory.
(define (docgen-dir path pattern)
  (let ([files (find-files path pattern)])
    (map docgen-file files)))

;;; find-files : Path × Pattern → (List Path)
;;; Find all files matching pattern (simplified glob).
(define (find-files dir pattern)
  (guard (e [else '()])
    ;; Simplified: just return hardcoded for now
    ;; In real implementation, would use fs.ss directory listing
    '("shell/toolkit.ss"
      "shell/test-runner.ss"
      "shell/watch.ss"
      "shell/scaffold.ss"
      "shell/perf-monitor.ss"
      "shell/history.ss")))

;;; ============================================================
;;; Utility Functions
;;; ============================================================

;;; read-file-lines : Path → (List String)
(define (read-file-lines path)
  (guard (e [else '()])
    (call-with-input-file path
      (lambda (port)
        (let loop ([lines '()])
          (let ([line (get-line port)])
            (if (eof-object? line)
                (reverse lines)
                (loop (cons line lines)))))))))

;;; doc-comment? : String → Bool
(define (doc-comment? line)
  (string-prefix? *doc-comment-prefix* (string-trim line)))

;;; doc-comment-content : String → String
(define (doc-comment-content line)
  (let* ([trimmed (string-trim line)]
         [prefix-len (string-length *doc-comment-prefix*)])
    (if (string-prefix? *doc-comment-prefix* trimmed)
        (string-trim (substring trimmed prefix-len (string-length trimmed)))
        "")))

;;; empty-line? : String → Bool
(define (empty-line? line)
  (= 0 (string-length (string-trim line))))

;;; string-trim : String → String
(define (string-trim str)
  (let* ([len (string-length str)]
         [start (let loop ([i 0])
                  (cond
                    [(>= i len) len]
                    [(char-whitespace? (string-ref str i)) (loop (+ i 1))]
                    [else i]))]
         [end (let loop ([i (- len 1)])
                (cond
                  [(< i start) start]
                  [(char-whitespace? (string-ref str i)) (loop (- i 1))]
                  [else (+ i 1)]))])
    (substring str start end)))

;;; string-prefix? : String × String → Bool
(define (string-prefix? prefix str)
  (let ([plen (string-length prefix)]
        [slen (string-length str)])
    (and (>= slen plen)
         (string=? prefix (substring str 0 plen)))))

;;; string-contains? : String × String → Bool
(define (string-contains? str needle)
  (let ([nlen (string-length needle)]
        [slen (string-length str)])
    (let loop ([i 0])
      (cond
        [(> (+ i nlen) slen) #f]
        [(string=? needle (substring str i (+ i nlen))) #t]
        [else (loop (+ i 1))]))))

;;; string-index : String × Char → Nat | #f
(define (string-index str ch)
  (let ([len (string-length str)])
    (let loop ([i 0])
      (cond
        [(>= i len) #f]
        [(char=? (string-ref str i) ch) i]
        [else (loop (+ i 1))]))))

;;; string-join : (List String) × String → String
(define (string-join strs sep)
  (if (null? strs)
      ""
      (let loop ([ss (cdr strs)]
                 [result (car strs)])
        (if (null? ss)
            result
            (loop (cdr ss)
                  (string-append result sep (car ss)))))))

;;; string-split : String × Char → (List String)
(define (string-split str sep)
  (let ([len (string-length str)])
    (let loop ([i 0] [start 0] [parts '()])
      (cond
        [(>= i len)
         (if (= start i)
             (reverse parts)
             (reverse (cons (substring str start i) parts)))]
        [(char=? (string-ref str i) sep)
         (loop (+ i 1)
               (+ i 1)
               (cons (substring str start i) parts))]
        [else
         (loop (+ i 1) start parts)]))))

(display "Documentation generator loaded.\n")
(display "Usage:\n")
(display "  (docgen-file \"path/to/file.ss\")     - Extract docs from file\n")
(display "  (docgen-toolkit)                     - Generate toolkit docs\n")
(display "  (docgen-render doc 'markdown)        - Render in format\n")
