;;; core/lsp/documents.ss — Document Store with UTF-16 Position Conversion
;;; @module documents
;;; @requires prelude json protocol
;;;
;;; Manages open documents and provides position conversion:
;;;   - In-memory document store
;;;   - Line offset computation
;;;   - UTF-16 ↔ character position conversion
;;;
;;; LSP uses UTF-16 code units for column positions. This module
;;; handles the conversion to/from Scheme's character positions.
;;;
;;; This is Core code: pure, except for the global store.

(load "core/base/prelude.ss")
(load "boundary/lsp/json.ss")
(load "boundary/lsp/protocol.ss")

;;; ====
;;; Document Structure
;;; ====

;;; Document = (document uri version content line-starts)
;;; line-starts is a vector of byte offsets for each line start

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

;;; ====
;;; Line Start Computation
;;; ====

;;; compute-line-starts : String → Vector<Int>
;;; Compute byte offsets of each line start.
;;; Line 0 starts at offset 0.
(define (compute-line-starts content)
  (let* ([len (string-length content)]
         [starts (list 0)])  ; Line 0 starts at 0
        (let loop ([i 0] [acc starts])
             (if (>= i len)
                 (list->vector (reverse acc))
                 (if (char=? (string-ref content i) #\newline)
                     (loop (+ i 1) (cons (+ i 1) acc))
                     (loop (+ i 1) acc))))))

;;; line-count : Document → Int
(define (line-count doc)
  (vector-length (document-line-starts doc)))

;;; get-line-start : Document × Int → Int
;;; Get the character offset where a line begins.
(define (get-line-start doc line)
  (let ([starts (document-line-starts doc)])
       (if (< line (vector-length starts))
           (vector-ref starts line)
           (string-length (document-content doc)))))

;;; get-line-end : Document × Int → Int
;;; Get the character offset where a line ends (before newline).
(define (get-line-end doc line)
  (let* ([starts (document-line-starts doc)]
         [content (document-content doc)]
         [len (string-length content)])
        (if (< (+ line 1) (vector-length starts))
            (- (vector-ref starts (+ line 1)) 1)  ; Before the \n
            len)))

;;; get-line-content : Document × Int → String
;;; Get the content of a specific line (without newline).
(define (get-line-content doc line)
  (let* ([start (get-line-start doc line)]
         [end (get-line-end doc line)]
         [content (document-content doc)])
        (if (<= end start)
            ""
            (substring content start (min end (string-length content))))))

;;; ====
;;; UTF-16 Position Conversion
;;; ====

;;; In UTF-16:
;;; - BMP characters (U+0000 to U+FFFF): 1 code unit
;;; - Non-BMP characters (U+10000+): 2 code units (surrogate pair)
;;;
;;; Scheme strings use code points, so we need to convert.

;;; char-utf16-length : Char → Int
;;; Return number of UTF-16 code units for a character.
(define (char-utf16-length c)
  (let ([cp (char->integer c)])
       (if (> cp #xFFFF) 2 1)))

;;; utf16-offset->char-offset : String × Int → Int
;;; Convert a UTF-16 code unit offset to a character offset.
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

;;; char-offset->utf16-offset : String × Int → Int
;;; Convert a character offset to a UTF-16 code unit offset.
(define (char-offset->utf16-offset str char-offset)
  (let ([len (min char-offset (string-length str))])
       (let loop ([i 0] [utf16-count 0])
            (if (>= i len)
                utf16-count
                (loop (+ i 1)
                      (+ utf16-count (char-utf16-length (string-ref str i))))))))

;;; ====
;;; LSP Position ↔ Document Offset
;;; ====

;;; lsp-position->offset : Document × JsonObject → Int
;;; Convert an LSP position {line, character} to a document offset.
(define (lsp-position->offset doc pos)
  (let* ([line (json-get pos "line")]
         [character (json-get pos "character")]
         [line-start (get-line-start doc line)]
         [line-content (get-line-content doc line)]
         [char-col (utf16-offset->char-offset line-content character)])
        (+ line-start char-col)))

;;; offset->lsp-position : Document × Int → JsonObject
;;; Convert a document offset to an LSP position.
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

;;; find-line-for-offset : Vector × Int × Int → Int
;;; Find which line contains the given offset.
(define (find-line-for-offset starts offset num-lines)
  (let loop ([lo 0] [hi (- num-lines 1)])
       (if (>= lo hi)
           lo
           (let ([mid (quotient (+ lo hi 1) 2)])
                (if (> (vector-ref starts mid) offset)
                    (loop lo (- mid 1))
                    (loop mid hi))))))

;;; ====
;;; Span ↔ LSP Range
;;; ====

;;; span->lsp-range : Document × Span → JsonObject
;;; Convert a source span to an LSP range.
;;; Span is 1-indexed; LSP is 0-indexed.
;;; Validates boundaries to handle stale spans gracefully.
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

;;; lsp-range->span : Document × JsonObject → Span
;;; Convert an LSP range to a source span.
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

;;; ====
;;; Document Store
;;; ====

;;; Global document store: uri → document
(define *documents* (make-hashtable string-hash string=?))

;;; doc-open! : String × Int × String → Document
;;; Open a document (add to store).
(define (doc-open! uri version content)
  (let ([doc (make-document uri version content)])
       (hashtable-set! *documents* uri doc)
       doc))

;;; doc-update! : String × Int × String → Document
;;; Update a document's content (full replacement).
(define (doc-update! uri version content)
  (doc-open! uri version content))

;;; doc-apply-changes! : String × Int × (List Change) → Document | #f
;;; Apply incremental changes to a document.
;;; Each change has optional range (if missing, it's a full replacement).
(define (doc-apply-changes! uri version changes)
  (let ([doc (doc-get uri)])
       (if (not doc)
           #f
           (let ([new-content (apply-changes (document-content doc) changes)])
                (doc-open! uri version new-content)))))

;;; apply-changes : String × (List Change) → String
;;; Apply a list of changes to content.
;;; Changes are applied in order from the list.
(define (apply-changes content changes)
  (if (null? changes)
      content
      (apply-changes (apply-single-change content (car changes))
                     (cdr changes))))

;;; apply-single-change : String × Change → String
;;; Apply a single change to content.
;;; Change is a JSON object with optional "range" and "text" fields.
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

;;; lsp-position->offset-in-string : String × Position → Int | #f
;;; Convert an LSP position to an offset in a string.
;;; This is like lsp-position->offset but works on raw content string.
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

;;; string-split-newlines : String → (List String)
;;; Split a string into lines.
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

;;; lines-offset : (List String) × Int → Int
;;; Get the offset of the start of line N.
(define (lines-offset lines n)
  (let loop ([ls lines] [i 0] [offset 0])
       (if (or (null? ls) (= i n))
           offset
           (loop (cdr ls) (+ i 1) (+ offset (string-length (car ls)) 1)))))

;;; doc-close! : String → Void
;;; Close a document (remove from store).
(define (doc-close! uri)
  (hashtable-delete! *documents* uri))

;;; doc-get : String → Document | #f
;;; Get a document by URI.
(define (doc-get uri)
  (hashtable-ref *documents* uri #f))

;;; doc-list : → (List String)
;;; List all open document URIs.
(define (doc-list)
  (vector->list (hashtable-keys *documents*)))

;;; ====
;;; Symbol Extraction
;;; ====

;;; symbol-at-offset : Document × Int → String | #f
;;; Extract the symbol (identifier) at a given offset.
;;; If offset is at the end of a symbol (cursor after last char),
;;; looks back one position to find the symbol.
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

;;; symbol-at-position : Document × JsonObject → String | #f
;;; Extract the symbol at an LSP position.
(define (symbol-at-position doc pos)
  (symbol-at-offset doc (lsp-position->offset doc pos)))

;;; find-symbol-start : String × Int → Int
;;; Find the start of the symbol containing offset.
(define (find-symbol-start content offset)
  (let loop ([i offset])
       (if (or (< i 0)
               (not (symbol-char? (string-ref content i))))
           (+ i 1)
           (loop (- i 1)))))

;;; find-symbol-end : String × Int → Int
;;; Find the end of the symbol containing offset.
(define (find-symbol-end content offset)
  (let ([len (string-length content)])
       (let loop ([i offset])
            (if (or (>= i len)
                    (not (symbol-char? (string-ref content i))))
                i
                (loop (+ i 1))))))

;;; symbol-char? : Char → Boolean
;;; Check if a character can be part of a Scheme symbol.
(define (symbol-char? c)
  (or (char-alphabetic? c)
      (char-numeric? c)
      (memv c '(#\- #\_ #\? #\! #\* #\+ #\/ #\< #\> #\= #\: #\@))))

;;; ====
;;; Span Re-export (for convenience)
;;; ====

;;; Import span functions from span.ss if not already loaded
(unless (top-level-bound? 'make-span)
        (load "core/lang/span.ss"))

;;; Provide span accessors
(define (span-line s) (list-ref s 2))
(define (span-column s) (list-ref s 3))
(define (span-end-line s) (list-ref s 4))
(define (span-end-column s) (list-ref s 5))
