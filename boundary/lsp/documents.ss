(load "core/base/prelude.ss")
(load "boundary/lsp/json.ss")
(load "boundary/lsp/protocol.ss")
(load "boundary/lsp/state.ss")

(doc 'module 'lsp/documents)
(doc 'description "Manages open documents and provides position conversion: in-memory document store, line offset computation, and UTF-16 ↔ character position conversion")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'requires '(prelude json protocol))
(doc 'note "LSP uses UTF-16 code units for column positions. This module handles the conversion to/from Scheme's character positions.")

(doc 'note "Debug flag for document operations")
(define *doc-debug* #f)

(doc 'section 'document-structure)

(doc 'note "Document = (document uri version content line-starts) where line-starts is a vector of byte offsets for each line start")

(doc make-document 'type '(-> String Int String Document))
(doc make-document 'description "Create a new document with computed line starts")
(define (make-document uri version content)
  (let ([line-starts (compute-line-starts content)])
       (list 'document uri version content line-starts)))

(define (document? x)
  (and (pair? x) (eq? (car x) 'document)))

(define (document-uri doc)
  (list-ref doc 1))

(define (document-version doc)
  (list-ref doc 2))

(define (document-content doc)
  (list-ref doc 3))

(define (document-line-starts doc)
  (list-ref doc 4))

(doc 'section 'line-start-computation)

(doc compute-line-starts 'type '(-> String (Vector Int)))
(doc compute-line-starts 'description "Compute byte offsets of each line start. Line 0 starts at offset 0. Handles all LSP-spec line endings: \\r\\n (Windows), \\n (Unix), \\r (old Mac)")
(define (compute-line-starts content)
  (let* ([len (string-length content)]
         [starts (list 0)])  ; Line 0 starts at 0
        (let loop ([i 0] [acc starts])
             (if (>= i len)
                 (list->vector (reverse acc))
                 (let ([c (string-ref content i)])
                   (cond
                     ;; \r\n (Windows) - skip both, count as one line break
                     [(and (char=? c #\return)
                           (< (+ i 1) len)
                           (char=? (string-ref content (+ i 1)) #\newline))
                      (loop (+ i 2) (cons (+ i 2) acc))]
                     ;; \n (Unix)
                     [(char=? c #\newline)
                      (loop (+ i 1) (cons (+ i 1) acc))]
                     ;; \r alone (old Mac)
                     [(char=? c #\return)
                      (loop (+ i 1) (cons (+ i 1) acc))]
                     ;; Regular character
                     [else
                      (loop (+ i 1) acc)]))))))

(doc line-count 'type '(-> Document Int))
(define (line-count doc)
  (vector-length (document-line-starts doc)))

(doc get-line-start 'type '(-> Document Int Int))
(doc get-line-start 'description "Get the character offset where a line begins")
(define (get-line-start doc line)
  (let ([starts (document-line-starts doc)])
       (if (< line (vector-length starts))
           (vector-ref starts line)
           (string-length (document-content doc)))))

(doc get-line-end 'type '(-> Document Int Int))
(doc get-line-end 'description "Get the character offset where a line ends (before any line ending chars)")
(doc get-line-end 'note "Handles \\r\\n, \\n, and \\r line endings")
(define (get-line-end doc line)
  (let* ([starts (document-line-starts doc)]
         [content (document-content doc)]
         [len (string-length content)])
        (if (< (+ line 1) (vector-length starts))
            ;; There's a next line - work backward from next line start
            (let* ([next-start (vector-ref starts (+ line 1))]
                   [end (- next-start 1)])  ; Position of final line-ending char
              ;; Check what line ending we have
              (if (and (> end 0)
                       (char=? (string-ref content end) #\newline)
                       (char=? (string-ref content (- end 1)) #\return))
                  ;; \r\n case - skip both
                  (- end 1)
                  ;; \n or \r case - just use end (which is position of the line ending)
                  end))
            ;; Last line - goes to end of content
            len)))

(doc get-line-content 'type '(-> Document Int String))
(doc get-line-content 'description "Get the content of a specific line (without newline)")
(define (get-line-content doc line)
  (let* ([start (get-line-start doc line)]
         [end (get-line-end doc line)]
         [content (document-content doc)])
        (if (<= end start)
            ""
            (substring content start (min end (string-length content))))))

(doc 'section 'utf16-position-conversion)

(doc 'note "In UTF-16: BMP characters (U+0000 to U+FFFF) use 1 code unit; Non-BMP characters (U+10000+) use 2 code units (surrogate pair). Scheme strings use code points, so we need to convert.")

(doc char-utf16-length 'type '(-> Char Int))
(doc char-utf16-length 'description "Return number of UTF-16 code units for a character")
(define (char-utf16-length c)
  (let ([cp (char->integer c)])
       (if (> cp #xFFFF) 2 1)))

(doc utf16-offset->char-offset 'type '(-> String Int Int))
(doc utf16-offset->char-offset 'description "Convert a UTF-16 code unit offset to a character offset")
(define (utf16-offset->char-offset str utf16-offset)
  (let ([len (string-length str)])
       (let loop ([char-idx 0] [utf16-idx 0])
            (cond
             [(>= utf16-idx utf16-offset) char-idx]
             [(>= char-idx len) char-idx]
             [else
              (let ([c (string-ref str char-idx)])
                   (loop (+ char-idx 1)
                         (+ utf16-idx (char-utf16-length c))))]))))

(doc char-offset->utf16-offset 'type '(-> String Int Int))
(doc char-offset->utf16-offset 'description "Convert a character offset to a UTF-16 code unit offset")
(define (char-offset->utf16-offset str char-offset)
  (let ([len (min char-offset (string-length str))])
       (let loop ([i 0] [utf16-count 0])
            (if (>= i len)
                utf16-count
                (loop (+ i 1)
                      (+ utf16-count (char-utf16-length (string-ref str i))))))))

(doc 'section 'lsp-position-conversion)

(doc lsp-position->offset 'type '(-> Document JsonObject Int))
(doc lsp-position->offset 'description "Convert an LSP position {line, character} to a document offset")
(define (lsp-position->offset doc pos)
  (let* ([line (json-get pos "line")]
         [character (json-get pos "character")]
         [line-start (get-line-start doc line)]
         [line-content (get-line-content doc line)]
         [char-col (utf16-offset->char-offset line-content character)])
        (+ line-start char-col)))

(doc offset->lsp-position 'type '(-> Document Int JsonObject))
(doc offset->lsp-position 'description "Convert a document offset to an LSP position")
(define (offset->lsp-position doc offset)
  (let* ([starts (document-line-starts doc)]
         [num-lines (vector-length starts)]
         ;; Binary search for the line
         [line (find-line-for-offset starts offset num-lines)]
         [line-start (vector-ref starts line)]
         [char-col (- offset line-start)]
         [line-content (get-line-content doc line)]
         [utf16-col (char-offset->utf16-offset line-content char-col)])
        (make-position line utf16-col)))

(doc find-line-for-offset 'type '(-> Vector Int Int Int))
(doc find-line-for-offset 'description "Find which line contains the given offset")
(define (find-line-for-offset starts offset num-lines)
  (let loop ([lo 0] [hi (- num-lines 1)])
       (if (>= lo hi)
           lo
           (let ([mid (quotient (+ lo hi 1) 2)])
                (if (> (vector-ref starts mid) offset)
                    (loop lo (- mid 1))
                    (loop mid hi))))))

(doc 'section 'span-conversion)

(doc span->lsp-range 'type '(-> Document Span JsonObject))
(doc span->lsp-range 'description "Convert a source span to an LSP range. Span is 1-indexed; LSP is 0-indexed. Validates boundaries to handle stale spans gracefully.")
(define (span->lsp-range doc span)
  (let* ([num-lines (line-count doc)]
         [max-line (max 0 (- num-lines 1))]
         ;; Clamp lines to valid range
         [start-line (max 0 (min max-line (- (span-line span) 1)))]
         [end-line (max 0 (min max-line (- (span-end-line span) 1)))]
         ;; Get line content (now guaranteed valid)
         [start-content (get-line-content doc start-line)]
         [end-content (get-line-content doc end-line)]
         ;; Clamp columns to line length
         [start-col (max 0 (min (string-length start-content)
                                (- (span-column span) 1)))]
         [end-col (max 0 (min (string-length end-content)
                              (- (span-end-column span) 1)))]
         ;; Convert columns to UTF-16
         [start-utf16 (char-offset->utf16-offset start-content start-col)]
         [end-utf16 (char-offset->utf16-offset end-content end-col)])
        (make-range (make-position start-line start-utf16)
                    (make-position end-line end-utf16))))

(doc lsp-range->span 'type '(-> Document JsonObject String Span))
(doc lsp-range->span 'description "Convert an LSP range to a source span")
(define (lsp-range->span doc range file)
  (let* ([start (json-get range "start")]
         [end (json-get range "end")]
         [start-line (+ 1 (json-get start "line"))]
         [end-line (+ 1 (json-get end "line"))]
         ;; Convert UTF-16 columns to character columns
         [start-content (get-line-content doc (- start-line 1))]
         [end-content (get-line-content doc (- end-line 1))]
         [start-col (+ 1 (utf16-offset->char-offset start-content
                                                    (json-get start "character")))]
         [end-col (+ 1 (utf16-offset->char-offset end-content
                                                  (json-get end "character")))])
        (make-span file start-line start-col end-line end-col)))

(doc 'section 'document-store)

(doc 'note "Global document store: uri → document")
(define *documents* (make-hashtable string-hash string=?))

;;; Register state with the LSP state registry
(lsp-register-state! 'documents
                     '(*documents*)
                     (lambda () (hashtable-clear! *documents*)))

(doc doc-open! 'type '(-> String Int String Document))
(doc doc-open! 'description "Open a document (add to store)")
(define (doc-open! uri version content)
  (let ([doc (make-document uri version content)])
       (hashtable-set! *documents* uri doc)
       doc))

(doc doc-update! 'type '(-> String Int String Document))
(doc doc-update! 'description "Update a document's content (full replacement)")
(define (doc-update! uri version content)
  (doc-open! uri version content))

(doc doc-apply-changes! 'type '(-> String Int (List Change) (U Document #f)))
(doc doc-apply-changes! 'description "Apply incremental changes to a document. Each change has optional range (if missing, it's a full replacement). Validates that version is increasing to prevent sync issues.")
(define (doc-apply-changes! uri version changes)
  (let ([doc (doc-get uri)])
       (if (not doc)
           #f
           (let ([current-version (document-version doc)])
                (cond
                 ;; Version validation: new version should be > current
                 [(and (number? version)
                       (number? current-version)
                       (<= version current-version))
                  ;; Stale update - log warning but don't apply
                  (when *doc-debug*
                        (display (format "Warning: Stale document version for ~a (got ~a, have ~a)\n"
                                        uri version current-version)))
                  doc]  ; Return current doc unchanged
                 [else
                  ;; Valid update - apply changes
                  (let ([new-content (apply-changes (document-content doc) changes)])
                       (doc-open! uri version new-content))])))))

(doc apply-changes 'type '(-> String (List Change) String))
(doc apply-changes 'description "Apply a list of changes to content")
(doc apply-changes 'note "Changes are applied in order from the list")
(define (apply-changes content changes)
  (if (null? changes)
      content
      (apply-changes (apply-single-change content (car changes))
                     (cdr changes))))

(doc apply-single-change 'type '(-> String Change String))
(doc apply-single-change 'description "Apply a single change to content")
(doc apply-single-change 'note "Change is a JSON object with optional 'range' and 'text' fields")
(define (apply-single-change content change)
  (let ([range (json-get change "range")]
        [text (json-get change "text")])
       (if (not range)
           ;; No range = full document replacement
           (or text content)
           ;; Range-based change
           (let* ([start (json-get range "start")]
                  [end (json-get range "end")]
                  [start-offset (lsp-position->offset-in-string content start)]
                  [end-offset (lsp-position->offset-in-string content end)])
                 (if (and start-offset end-offset)
                     (string-append (substring content 0 start-offset)
                                    (or text "")
                                    (substring content end-offset (string-length content)))
                     content)))))

(doc lsp-position->offset-in-string 'type '(-> String Position (U Int #f)))
(doc lsp-position->offset-in-string 'description "Convert an LSP position to an offset in a string")
(doc lsp-position->offset-in-string 'note "Like lsp-position->offset but works on raw content string")
(define (lsp-position->offset-in-string content pos)
  (let* ([line (json-get pos "line")]
         [char (json-get pos "character")]
         [lines (string-split-newlines content)])
        (if (< line (length lines))
            (let* ([line-offset (lines-offset lines line)]
                   [line-content (list-ref lines line)]
                   [char-offset (utf16-offset->char-offset line-content char)])
                  (+ line-offset char-offset))
            (string-length content))))

(doc string-split-newlines 'type '(-> String (List String)))
(doc string-split-newlines 'description "Split a string into lines")
(define (string-split-newlines str)
  (let ([len (string-length str)])
       (let loop ([i 0] [start 0] [acc '()])
            (cond
             [(>= i len)
              (reverse (cons (substring str start len) acc))]
             [(char=? (string-ref str i) #\newline)
              (loop (+ i 1) (+ i 1) (cons (substring str start i) acc))]
             [else
              (loop (+ i 1) start acc)]))))

(doc lines-offset 'type '(-> (List String) Int Int))
(doc lines-offset 'description "Get the offset of the start of line N")
(define (lines-offset lines n)
  (let loop ([ls lines] [i 0] [offset 0])
       (if (or (null? ls) (= i n))
           offset
           (loop (cdr ls) (+ i 1) (+ offset (string-length (car ls)) 1)))))

(doc doc-close! 'type '(-> String Void))
(doc doc-close! 'description "Close a document (remove from store)")
(define (doc-close! uri)
  (hashtable-delete! *documents* uri))

(doc doc-get 'type '(-> String (U Document #f)))
(doc doc-get 'description "Get a document by URI")
(define (doc-get uri)
  (hashtable-ref *documents* uri #f))

(doc doc-list 'type '(-> (List String)))
(doc doc-list 'description "List all open document URIs")
(define (doc-list)
  (vector->list (hashtable-keys *documents*)))

(doc 'section 'symbol-extraction)

(doc symbol-at-offset 'type '(-> Document Int (U String #f)))
(doc symbol-at-offset 'description "Extract the symbol (identifier) at a given offset. If offset is at the end of a symbol (cursor after last char), looks back one position to find the symbol.")
(define (symbol-at-offset doc offset)
  (let* ([content (document-content doc)]
         [len (string-length content)])
        (cond
         ;; Out of bounds
         [(or (< offset 0) (> offset len)) #f]
         ;; At end of document - check previous char
         [(= offset len)
          (if (and (> len 0) (symbol-char? (string-ref content (- len 1))))
              (symbol-at-offset doc (- offset 1))
              #f)]
         ;; Current char is a symbol char - find bounds
         [(symbol-char? (string-ref content offset))
          (let* ([start (find-symbol-start content offset)]
                 [end (find-symbol-end content offset)])
                (if (< start end)
                    (substring content start end)
                    #f))]
         ;; Current char is NOT a symbol char - check previous position
         ;; This handles cursor-at-end-of-symbol (e.g., "define|")
         [(and (> offset 0) (symbol-char? (string-ref content (- offset 1))))
          (symbol-at-offset doc (- offset 1))]
         ;; No symbol at or before this position
         [else #f])))

(doc symbol-at-position 'type '(-> Document JsonObject (U String #f)))
(doc symbol-at-position 'description "Extract the symbol at an LSP position")
(define (symbol-at-position doc pos)
  (symbol-at-offset doc (lsp-position->offset doc pos)))

(doc find-symbol-start 'type '(-> String Int Int))
(doc find-symbol-start 'description "Find the start of the symbol containing offset")
(define (find-symbol-start content offset)
  (let loop ([i offset])
       (if (or (< i 0)
               (not (symbol-char? (string-ref content i))))
           (+ i 1)
           (loop (- i 1)))))

(doc find-symbol-end 'type '(-> String Int Int))
(doc find-symbol-end 'description "Find the end of the symbol containing offset")
(define (find-symbol-end content offset)
  (let ([len (string-length content)])
       (let loop ([i offset])
            (if (or (>= i len)
                    (not (symbol-char? (string-ref content i))))
                i
                (loop (+ i 1))))))

(doc symbol-char? 'type '(-> Char Boolean))
(doc symbol-char? 'description "Check if a character can be part of a Scheme symbol")
(define (symbol-char? c)
  (or (char-alphabetic? c)
      (char-numeric? c)
      (and (memv c '(#\- #\_ #\? #\! #\* #\+ #\/ #\< #\> #\= #\: #\@)) #t)))

(doc 'section 'span-re-export)

(doc 'note "Import span functions from span.ss if not already loaded")
(unless (top-level-bound? 'make-span)
        (load "core/lang/span.ss"))

(doc 'note "Span accessors for convenience")
(define (span-line s) (list-ref s 2))
(define (span-column s) (list-ref s 3))
(define (span-end-line s) (list-ref s 4))
(define (span-end-column s) (list-ref s 5))
