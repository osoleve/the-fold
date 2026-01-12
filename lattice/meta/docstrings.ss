;;; lattice/meta/docstrings.ss — Docstring Extraction
;;;
;;; Extract ;;; docstrings from source files and associate them with
;;; function definitions for improved search indexing.
;;;
;;; Docstring format:
;;;   ;;; function-name : type-signature
;;;   ;;; Description line 1
;;;   ;;; Description line 2
;;;   (define (function-name args...) ...)
;;;
;;; This is Lattice code: impure (reads files), used during indexing.

;;; ============================================================
;;; State
;;; ============================================================

;;; Global docstring cache: symbol -> string
(define *docstrings* (make-hashtable symbol-hash eq?))

;;; ============================================================
;;; File Parsing
;;; ============================================================

;;; read-file-lines : String -> (List String) | #f
;;; Read all lines from a file
(define (read-file-lines path)
  (guard (e [else #f])
         (call-with-input-file path
                               (lambda (port)
                                       (let loop ([lines '()])
                                            (let ([line (get-line port)])
                                                 (if (eof-object? line)
                                                     (reverse lines)
                                                     (loop (cons line lines)))))))))

;;; extract-docstrings-from-file : String -> (List (Symbol . String))
;;; Parse a file and extract docstrings for defined functions
(define (extract-docstrings-from-file path)
  (let ([lines (read-file-lines path)])
       (if (not lines)
           '()
           (parse-docstrings lines))))

;;; parse-docstrings : (List String) -> (List (Symbol . String))
;;; Parse lines looking for docstring + define patterns
(define (parse-docstrings lines)
  (let loop ([lines lines]
             [pending-doc '()]
             [results '()])
       (if (null? lines)
           results
           (let ([line (car lines)])
                (cond
                 ;; Docstring line (;;; ...)
                 [(docstring-line? line)
                  (loop (cdr lines)
                        (cons (extract-docstring-text line) pending-doc)
                        results)]
                 ;; Definition line following docstring
                 [(and (not (null? pending-doc))
                       (define-line? line))
                  (let ([name (extract-define-name line)])
                       (if name
                           (loop (cdr lines)
                                 '()
                                 (cons (cons name (join-docstring (reverse pending-doc)))
                                       results))
                           (loop (cdr lines) '() results)))]
                 ;; Any other line clears pending docstring
                 [else
                  (loop (cdr lines) '() results)])))))

;;; docstring-line? : String -> Boolean
;;; Check if line is a docstring (starts with ;;;)
(define (docstring-line? line)
  (let ([trimmed (string-trim-left line)])
       (and (>= (string-length trimmed) 3)
            (string=? (substring trimmed 0 3) ";;;"))))

;;; extract-docstring-text : String -> String
;;; Extract the text after ;;;
(define (extract-docstring-text line)
  (let* ([trimmed (string-trim-left line)]
         [after-prefix (substring trimmed 3 (string-length trimmed))])
        (string-trim-left after-prefix)))

;;; define-line? : String -> Boolean
;;; Check if line starts a define form
(define (define-line? line)
  (let ([trimmed (string-trim-left line)])
       (and (>= (string-length trimmed) 7)
            (string=? (substring trimmed 0 7) "(define"))))

;;; extract-define-name : String -> Symbol | #f
;;; Extract the name being defined
;;; Handles both (define (name ...) and (define name ...)
(define (extract-define-name line)
  (guard (e [else #f])
         (let* ([trimmed (string-trim-left line)]
                ;; Skip "(define "
                [after-define (substring trimmed 8 (string-length trimmed))]
                [cleaned (string-trim-left after-define)])
               (cond
                ;; (define (name args...) ...)
                [(and (> (string-length cleaned) 0)
                      (char=? (string-ref cleaned 0) #\())
                 (let ([inner (substring cleaned 1 (string-length cleaned))])
                      (extract-first-symbol inner))]
                ;; (define name ...)
                [else
                 (extract-first-symbol cleaned)]))))

;;; extract-first-symbol : String -> Symbol | #f
;;; Extract the first symbol from a string
(define (extract-first-symbol str)
  (let ([len (string-length str)])
       (if (= len 0)
           #f
           (let loop ([i 0])
                (if (>= i len)
                    (string->symbol str)
                    (let ([c (string-ref str i)])
                         (if (or (char-whitespace? c)
                                 (char=? c #\))
                                 (char=? c #\())
                             (if (= i 0)
                                 #f
                                 (string->symbol (substring str 0 i)))
                             (loop (+ i 1)))))))))

;;; join-docstring : (List String) -> String
;;; Join docstring lines into a single string
(define (join-docstring lines)
  (if (null? lines)
      ""
      (let loop ([rest (cdr lines)]
                 [result (car lines)])
           (if (null? rest)
               result
               (loop (cdr rest)
                     (string-append result " " (car rest)))))))

;;; string-trim-left : String -> String
;;; Remove leading whitespace
(define (string-trim-left str)
  (let ([len (string-length str)])
       (let loop ([i 0])
            (if (>= i len)
                ""
                (if (char-whitespace? (string-ref str i))
                    (loop (+ i 1))
                    (substring str i len))))))

;;; ============================================================
;;; Directory Scanning
;;; ============================================================

;;; find-scheme-files : String -> (List String)
;;; Recursively find all .ss files in a directory
(define (find-scheme-files dir)
  (guard (e [else '()])
         (let ([entries (directory-list dir)])
              (apply append
                     (map (lambda (entry)
                                  (let ([path (string-append dir "/" entry)])
                                       (cond
                                        ;; Skip hidden directories
                                        [(and (> (string-length entry) 0)
                                              (char=? (string-ref entry 0) #\.))
                                         '()]
                                        ;; Recurse into subdirectories
                                        [(file-directory? path)
                                         (find-scheme-files path)]
                                        ;; Collect .ss files
                                        [(string-ends-with-ss? entry)
                                         (list path)]
                                        [else '()])))
                          entries)))))

;;; file-directory? : String -> Boolean
(define (file-directory? path)
  (guard (e [else #f])
         (let ([entries (directory-list path)])
              #t)))

;;; string-ends-with-ss? : String -> Boolean
(define (string-ends-with-ss? str)
  (let ([len (string-length str)])
       (and (>= len 3)
            (string=? (substring str (- len 3) len) ".ss"))))

;;; ============================================================
;;; Public API
;;; ============================================================

;;; build-docstring-cache! : -> Void
;;; Build the global docstring cache from all lattice source files
(define (build-docstring-cache!)
  (printf "Building docstring cache...\n")
  (set! *docstrings* (make-hashtable symbol-hash eq?))
  (let* ([files (find-scheme-files "lattice")]
         [count 0])
        (for-each
         (lambda (path)
                 (let ([docstrings (extract-docstrings-from-file path)])
                      (for-each
                       (lambda (entry)
                               (hashtable-set! *docstrings* (car entry) (cdr entry))
                               (set! count (+ count 1)))
                       docstrings)))
         files)
        (printf "  Scanned ~a files, extracted ~a docstrings\n"
                (length files) count)))

;;; get-docstring : Symbol -> String | #f
;;; Get the docstring for a function
(define (get-docstring sym)
  (guard (e [else #f])
         (hashtable-ref *docstrings* sym #f)))

;;; docstring-terms : Symbol -> (List Symbol)
;;; Get search terms from a function's docstring
(define (docstring-terms sym)
  (let ([doc (get-docstring sym)])
       (if (and doc (string? doc) (> (string-length doc) 0))
           (tokenize doc)
           '())))

;;; ============================================================
;;; REPL Interface
;;; ============================================================

(printf "docstrings.ss loaded.\n")
(printf "  (build-docstring-cache!)       - Build cache from sources\n")
(printf "  (get-docstring 'fn)            - Get docstring for function\n")
(printf "  (docstring-terms 'fn)          - Get search terms from docstring\n")
