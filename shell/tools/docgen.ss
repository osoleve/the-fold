;;; shell/docgen.ss -- API Documentation Generator
;;;
;;; Extracts documentation from Scheme source files and generates
;;; structured documentation as S-expressions.
;;;
;;; This is Shell code: reads files, parses comments, generates output.
;;;
;;; Documentation Patterns Recognized:
;;;   - ;;; Triple-semicolon doc comments
;;;   - ;;; function-name : Type -> Type -> Result
;;;   - ;;; Export Summary sections
;;;   - (define (function-name ...)) definitions
;;;
;;; Output Format:
;;;   ((module . "module-name")
;;;    (exports . ((name . "function-name")
;;;                (signature . "Type -> Type -> Result")
;;;                (doc . "Description")
;;;                (file . "path/to/file.ss")
;;;                (line . 42))))
;;;
;;; Key Functions:
;;;   (docgen-file path)           -- Generate docs for single file
;;;   (docgen-directory path)      -- Generate docs for all .ss files
;;;   (docgen-index docs)          -- Create searchable index by name
;;;   (docgen-summary docs)        -- Create human-readable summary
;;;
;;; Dependencies:
;;;   - prelude.ss (for result types)
;;;
;;; NOTE: Run from project root directory.

;;; Set up source-directories for module loading
(source-directories (cons "shell" (source-directories)))
(load "string-utils.ss")

;;; ============================================================
;;; Configuration
;;; ============================================================

(define *doc-comment-prefix* ";;;")
(define *export-summary-marker* "Export Summary")
(define *section-marker-prefix* ";;; ===")

;;; ============================================================
;;; String Utilities
;;; ============================================================
;;;
;;; NOTE: string-trim, string-split, string-join, string-contains?,
;;; string-starts-with? (alias for string-prefix?) are provided by
;;; core/prelude.ss which is loaded above.

;;; string-prefix? : String x String -> Boolean
;;; Alias for string-starts-with? from prelude.
(define string-prefix? string-starts-with?)

;;; ============================================================
;;; Comment Parsing Utilities
;;; ============================================================


;;; path-basename : Path -> String
(define (path-basename path)
  (let ([parts (string-split path #\/)])
       (if (null? parts)
           path
           (let loop ([ps parts])
                (if (null? (cdr ps))
                    (car ps)
                    (loop (cdr ps)))))))

;;; path-strip-extension : String -> String
(define (path-strip-extension name)
  (let ([idx (string-index-right name #\.)])
       (if idx
           (substring name 0 idx)
           name)))

;;; path->module-name : Path -> String
(define (path->module-name path)
  (path-strip-extension (path-basename path)))

;;; ============================================================
;;; File Reading
;;; ============================================================

;;; read-file-lines : Path -> (List String)
(define (read-file-lines path)
  (guard (e [else '()])
         (call-with-input-file path
                               (lambda (port)
                                       (let loop ([lines '()])
                                            (let ([line (get-line port)])
                                                 (if (eof-object? line)
                                                     (reverse lines)
                                                     (loop (cons line lines)))))))))

;;; ============================================================
;;; Doc Comment Parsing
;;; ============================================================

;;; doc-comment? : String -> Boolean
;;; True if line is a doc comment (starts with ;;;).
(define (doc-comment? line)
  (let ([trimmed (string-trim line)])
       (string-prefix? *doc-comment-prefix* trimmed)))

;;; doc-comment-content : String -> String
;;; Extract content from doc comment (after ;;;).
(define (doc-comment-content line)
  (let* ([trimmed (string-trim line)]
         [prefix-len (string-length *doc-comment-prefix*)])
        (if (and (>= (string-length trimmed) prefix-len)
                 (string-prefix? *doc-comment-prefix* trimmed))
            (string-trim (substring trimmed prefix-len (string-length trimmed)))
            "")))

;;; empty-line? : String -> Boolean
(define (empty-line? line)
  (= 0 (string-length (string-trim line))))

;;; section-header? : String -> Boolean
;;; True if line is a section header (;;; ===...).
(define (section-header? line)
  (let ([trimmed (string-trim line)])
       (string-prefix? *section-marker-prefix* trimmed)))

;;; ============================================================
;;; Signature Parsing
;;; ============================================================

;;; parse-signature : String -> (List Symbol String) | #f
;;; Parse "function-name : Type -> Type" into (name . signature).
;;; Returns (name . signature) or #f if not a signature line.
(define (parse-signature line)
  (let ([content (doc-comment-content line)])
       (if (and (string-contains? content " : ")
                (string-contains? content "->"))  ; Arrow indicator
           (let* ([colon-idx (let loop ([i 0])
                                  (cond
                                   [(>= i (- (string-length content) 2)) #f]
                                   [(and (char=? (string-ref content i) #\:)
                                         (char-whitespace? (string-ref content (+ i 1))))
                                    i]
                                   [else (loop (+ i 1))]))]
                  [name (and colon-idx
                             (string-trim (substring content 0 colon-idx)))]
                  [signature (and colon-idx
                                  (string-trim
                                   (substring content (+ colon-idx 1) (string-length content))))])
                 (if (and name signature (> (string-length name) 0))
                     (cons (string->symbol name) signature)
                     #f))
           #f)))

;;; ============================================================
;;; Function Definition Parsing
;;; ============================================================

;;; function-definition? : String -> Boolean
(define (function-definition? line)
  (let ([trimmed (string-trim line)])
       (or (string-prefix? "(define (" trimmed)
           (string-prefix? "(define-record-type " trimmed))))

;;; extract-function-name : String -> Symbol | #f
;;; Extract function name from definition line.
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
                          ;; Handle single-char functions or closing paren
                          (let ([paren (string-index rest #\))])
                               (if paren
                                   (string->symbol (substring rest 0 paren))
                                   #f))))]
               [(string-prefix? "(define " trimmed)
                (let* ([start (string-length "(define ")]
                       [rest (substring trimmed start (string-length trimmed))]
                       [end (or (string-index rest #\space)
                                (string-index rest #
                                              ewline)
                                (string-index rest #\)))])
                      (if (and end (> end 0))
                          (string->symbol (substring rest 0 end))
                          #f))]
               [else #f]))))

;;; ============================================================
;;; Export Summary Parsing
;;; ============================================================

;;; find-export-summary : (List String) -> (List String)
;;; Find and extract the Export Summary section.
;;; Handles the pattern: ;;; Export Summary / ;;; === / ;;; content...
(define (find-export-summary lines)
  (let loop ([ls lines] [in-summary #f] [skip-next-header #f] [items '()])
       (cond
        [(null? ls) (reverse items)]
        [else
         (let ([line (car ls)])
              (cond
               ;; Start of export summary
               [(and (not in-summary)
                     (doc-comment? line)
                     (string-contains? (doc-comment-content line) *export-summary-marker*))
                ;; Skip the section header that typically follows
                (loop (cdr ls) #t #t items)]
               ;; Skip the first section header after Export Summary marker
               [(and in-summary skip-next-header (section-header? line))
                (loop (cdr ls) #t #f items)]
               ;; Inside export summary - subsequent section header ends it
               [(and in-summary (not skip-next-header) (section-header? line))
                (reverse items)]
               ;; Inside export summary - collect doc comment items
               [(and in-summary (doc-comment? line))
                (let ([content (doc-comment-content line)])
                     (if (> (string-length content) 0)
                         (loop (cdr ls) #t #f (cons content items))
                         (loop (cdr ls) #t #f items)))]
               ;; Inside summary - non-doc-comment/non-empty line ends it
               [(and in-summary (not (empty-line? line)) (not (doc-comment? line)))
                (reverse items)]
               [else
                (loop (cdr ls) in-summary skip-next-header items)]))])))

;;; parse-export-line : String -> (List Symbol) | '()
;;; Parse an export summary line for function names.
(define (parse-export-line line)
  (let* ([clean (string-trim line)]
         ;; Remove category prefixes like "Type:", "Properties:", etc.
         [content (if (string-contains? clean ":")
                      (let ([idx (string-index clean #\:)])
                           (string-trim (substring clean (+ idx 1) (string-length clean))))
                      clean)]
         ;; Split by comma and parse each name
         [parts (string-split content #\,)])
        (filter symbol?
                (map (lambda (p)
                             (let ([trimmed (string-trim p)])
                                  (if (> (string-length trimmed) 0)
                                      (guard (e [else #f])
                                             (string->symbol trimmed))
                                      #f)))
                     parts))))

;;; ============================================================
;;; Documentation Extraction
;;; ============================================================

;;; extract-exports : (List String) -> (List (name signature doc line))
;;; Extract all documented exports from lines.
(define (extract-exports lines)
  (let loop ([ls lines]
             [line-num 1]
             [current-doc '()]
             [current-sig #f]
             [exports '()])
       (cond
        [(null? ls) (reverse exports)]
        [else
         (let ([line (car ls)])
              (cond
               ;; Doc comment - might be signature or description
               [(doc-comment? line)
                (let ([sig (parse-signature line)])
                     (if sig
                         ;; Found a signature
                         (loop (cdr ls) (+ line-num 1)
                               current-doc sig exports)
                         ;; Regular doc comment
                         (loop (cdr ls) (+ line-num 1)
                               (cons (doc-comment-content line) current-doc)
                               current-sig exports)))]
               ;; Function definition following doc comments
               [(and (or current-sig (not (null? current-doc)))
                     (function-definition? line))
                (let* ([def-name (extract-function-name line)]
                       [name (if current-sig (car current-sig) def-name)]
                       [sig (if current-sig (cdr current-sig) "")]
                       [doc (string-join (reverse current-doc) " ")]
                       [export `((name . ,name)
                                 (signature . ,sig)
                                 (doc . ,doc)
                                 (line . ,line-num))])
                      (loop (cdr ls) (+ line-num 1)
                            '() #f (cons export exports)))]
               ;; Empty line - might reset doc accumulator
               [(empty-line? line)
                (loop (cdr ls) (+ line-num 1)
                      current-doc current-sig exports)]
               ;; Other content - reset doc accumulator
               [else
                (loop (cdr ls) (+ line-num 1)
                      '() #f exports)]))])))

;;; ============================================================
;;; Main API
;;; ============================================================

;;; docgen-file : Path -> ModuleDoc
;;; Generate documentation for a single file.
;;; Returns an S-expression with module documentation.
(define (docgen-file path)
  (guard (e [else
             `((module . ,(path->module-name path))
               (file . ,path)
               (error . ,(if (condition? e)
                             (condition-message e)
                             (format "~a" e)))
               (exports . ()))])
         (let* ([lines (read-file-lines path)]
                [exports (extract-exports lines)]
                [export-summary (find-export-summary lines)]
                ;; Add file path to each export
                [exports-with-path
                 (map (lambda (exp)
                              (cons (cons 'file path) exp))
                      exports)])
               `((module . ,(path->module-name path))
                 (file . ,path)
                 (export-summary . ,export-summary)
                 (exports . ,exports-with-path)))))

;;; docgen-directory : Path -> (List ModuleDoc)
;;; Generate documentation for all .ss files in a directory.
(define (docgen-directory path)
  (guard (e [else '()])
         (let* ([entries (directory-list path)]
                [ss-files (filter (lambda (f)
                                          (string-contains? f ".ss"))
                                  entries)]
                [full-paths (map (lambda (f)
                                         (string-append path "/" f))
                                 ss-files)])
               (map docgen-file full-paths))))

;;; docgen-index : (List ModuleDoc) -> Alist
;;; Create a searchable index from documentation.
;;; Returns alist mapping function names to their doc entries.
(define (docgen-index docs)
  (let loop ([ds docs] [index '()])
       (cond
        [(null? ds) index]
        [else
         (let* ([doc (car ds)]
                [exports (cdr (assq 'exports doc))]
                [new-entries
                 (map (lambda (exp)
                              (let ([name (cdr (assq 'name exp))])
                                   (cons name exp)))
                      exports)])
               (loop (cdr ds) (append new-entries index)))])))

;;; docgen-summary : (List ModuleDoc) -> String
;;; Create a human-readable summary of documentation.
(define (docgen-summary docs)
  (string-join
   (map (lambda (doc)
                (let* ([module-name (cdr (assq 'module doc))]
                       [file (cdr (assq 'file doc))]
                       [exports (cdr (assq 'exports doc))]
                       [export-count (length exports)]
                       [header (format "~%=== ~a (~a) ===~%  ~a exports~%"
                                       module-name file export-count)]
                       [export-lines
                        (map (lambda (exp)
                                     (let ([name (cdr (assq 'name exp))]
                                           [sig (cdr (assq 'signature exp))]
                                           [doc-str (cdr (assq 'doc exp))])
                                          (if (> (string-length sig) 0)
                                              (format "    ~a : ~a~%      ~a"
                                                      name sig
                                                      (if (> (string-length doc-str) 60)
                                                          (string-append
                                                           (substring doc-str 0 57) "...")
                                                          doc-str))
                                              (format "    ~a~%      ~a"
                                                      name doc-str))))
                             exports)])
                      (string-append header (string-join export-lines "
"))))
        docs)
   "
"))

;;; ============================================================
;;; Query Functions
;;; ============================================================

;;; docgen-find : (List ModuleDoc) x Symbol -> (List Export) | '()
;;; Find all exports matching a name.
(define (docgen-find docs name)
  (let ([index (docgen-index docs)])
       (filter (lambda (entry)
                       (eq? (car entry) name))
               index)))

;;; docgen-search : (List ModuleDoc) x String -> (List Export)
;;; Search exports by substring in name or doc.
(define (docgen-search docs query)
  (let* ([index (docgen-index docs)]
         [query-lower query])  ; Could lowercase if we had string-downcase
        (filter (lambda (entry)
                        (let ([name-str (symbol->string (car entry))]
                              [doc-str (cdr (assq 'doc (cdr entry)))])
                             (or (string-contains? name-str query)
                                 (string-contains? doc-str query))))
                index)))

;;; ============================================================
;;; Export Summary
;;; ============================================================
;;;
;;; Main API:
;;;   docgen-file, docgen-directory, docgen-index, docgen-summary
;;;
;;; Query Functions:
;;;   docgen-find, docgen-search
;;;
;;; Parsing Utilities:
;;;   parse-signature, extract-exports, find-export-summary
;;;
;;; String Utilities:
;;;   string-trim, string-prefix?, string-contains?, string-split, string-join

(display "Documentation generator loaded.
")
(display "Usage:
")
(display "  (docgen-file \"path/to/file.ss\")         - Generate docs for file
")
(display "  (docgen-directory \"core\")              - Generate docs for directory
")
(display "  (docgen-index docs)                      - Create searchable index
")
(display "  (docgen-summary docs)                    - Create readable summary
")
