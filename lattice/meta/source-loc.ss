;;; lattice/meta/source-loc.ss — Source Location Tracking
;;;
;;; Track file paths and line numbers for symbol definitions.
;;; Enables jump-to-definition workflows and better error messages.
;;;
;;; This is Lattice code: impure (reads files), used during indexing.
;;;
;;; Usage:
;;;   (build-source-location-cache!)      ; Build cache from sources
;;;   (get-source-location 'symbol)       ; Get (file . line) or #f
;;;   (format-source-location 'symbol)    ; Get "file:line" string
;;;
;;; Dependencies: None (standalone module)

;;; ============================================================
;;; State
;;; ============================================================

;;; Global source location cache: symbol -> (file . line)
(define *source-locations* (make-hashtable symbol-hash eq?))

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

;;; extract-definitions-from-file : String -> (List (Symbol File Line))
;;; Parse a file and extract all definitions with line numbers
(define (extract-definitions-from-file path)
  (let ([lines (read-file-lines path)])
       (if (not lines)
           '()
           (parse-definitions-with-lines path lines))))

;;; parse-definitions-with-lines : String × (List String) -> (List (Symbol File Line))
;;; Parse lines tracking definitions and their line numbers
(define (parse-definitions-with-lines path lines)
  (let loop ([lines lines]
             [line-num 1]
             [results '()])
       (if (null? lines)
           results
           (let* ([line (car lines)]
                  [name (try-extract-define-name line)])
                 (if name
                     (loop (cdr lines)
                           (+ line-num 1)
                           (cons (list name path line-num) results))
                     (loop (cdr lines)
                           (+ line-num 1)
                           results))))))

;;; try-extract-define-name : String -> Symbol | #f
;;; Try to extract a defined name from a line
;;; Note: Check longer keywords first to avoid prefix matching issues
(define (try-extract-define-name line)
  (let ([trimmed (string-trim-left-loc line)])
       (cond
        ;; (define-test "name" ...) - skip these
        [(and (>= (string-length trimmed) 12)
              (string=? (substring trimmed 0 12) "(define-test"))
         #f]
        ;; (define-record-type name ...)
        [(and (>= (string-length trimmed) 19)
              (string=? (substring trimmed 0 19) "(define-record-type"))
         (extract-name-after-keyword trimmed 19)]
        ;; (define-syntax name ...)
        [(and (>= (string-length trimmed) 14)
              (string=? (substring trimmed 0 14) "(define-syntax"))
         (extract-name-after-keyword trimmed 14)]
        ;; (define (name ...) ...) or (define name ...)
        ;; Check for "(define " or "(define(" to avoid matching define-*
        [(and (>= (string-length trimmed) 8)
              (string=? (substring trimmed 0 7) "(define")
              (let ([next-char (string-ref trimmed 7)])
                   (or (char-whitespace? next-char)
                       (char=? next-char #\())))
         (extract-name-from-define trimmed)]
        [else #f])))

;;; extract-name-from-define : String -> Symbol | #f
;;; Extract name from a define form
(define (extract-name-from-define line)
  (guard (e [else #f])
         (let* ([after-define (substring line 7 (string-length line))]
                [cleaned (string-trim-left-loc after-define)])
               (cond
                ;; (define (name args...) ...)
                [(and (> (string-length cleaned) 0)
                      (char=? (string-ref cleaned 0) #\())
                 (let ([inner (substring cleaned 1 (string-length cleaned))])
                      (extract-first-sym inner))]
                ;; (define name ...)
                [else
                 (extract-first-sym cleaned)]))))

;;; extract-name-after-keyword : String × Int -> Symbol | #f
;;; Extract name after a keyword at given position
(define (extract-name-after-keyword line start-pos)
  (guard (e [else #f])
         (if (>= start-pos (string-length line))
             #f
             (let* ([after-keyword (substring line start-pos (string-length line))]
                    [cleaned (string-trim-left-loc after-keyword)])
                   (extract-first-sym cleaned)))))

;;; extract-first-sym : String -> Symbol | #f
;;; Extract the first symbol from a string
(define (extract-first-sym str)
  (let ([len (string-length str)])
       (if (= len 0)
           #f
           (let loop ([i 0])
                (if (>= i len)
                    (if (> i 0)
                        (string->symbol str)
                        #f)
                    (let ([c (string-ref str i)])
                         (if (or (char-whitespace? c)
                                 (char=? c #\))
                                 (char=? c #\()
                                 (char=? c #\")
                                 (char=? c #\'))
                             (if (= i 0)
                                 #f
                                 (string->symbol (substring str 0 i)))
                             (loop (+ i 1)))))))))

;;; string-trim-left-loc : String -> String
;;; Remove leading whitespace
(define (string-trim-left-loc str)
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

;;; find-scheme-files-loc : String -> (List String)
;;; Recursively find all .ss files in a directory
(define (find-scheme-files-loc dir)
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
                                        [(file-directory-loc? path)
                                         (find-scheme-files-loc path)]
                                        ;; Collect .ss files (skip test files)
                                        [(and (string-ends-with-ss-loc? entry)
                                              (not (string-prefix? "test-" entry)))
                                         (list path)]
                                        [else '()])))
                          entries)))))

;;; file-directory-loc? : String -> Boolean
(define (file-directory-loc? path)
  (guard (e [else #f])
         (let ([entries (directory-list path)])
              #t)))

;;; string-ends-with-ss-loc? : String -> Boolean
(define (string-ends-with-ss-loc? str)
  (let ([len (string-length str)])
       (and (>= len 3)
            (string=? (substring str (- len 3) len) ".ss"))))

;;; string-prefix? : String × String -> Boolean
(define (string-prefix? prefix str)
  (let ([plen (string-length prefix)]
        [slen (string-length str)])
       (and (>= slen plen)
            (string=? (substring str 0 plen) prefix))))

;;; ============================================================
;;; Public API
;;; ============================================================

;;; build-source-location-cache! : -> Void
;;; Build the global source location cache from all lattice and core files
(define (build-source-location-cache!)
  (printf "Building source location cache...\n")
  (set! *source-locations* (make-hashtable symbol-hash eq?))
  (let ([count 0])
       ;; Scan lattice/
       (for-each
        (lambda (path)
                (let ([defs (extract-definitions-from-file path)])
                     (for-each
                      (lambda (def)
                              (let ([name (car def)]
                                    [file (cadr def)]
                                    [line (caddr def)])
                                   ;; Only store first occurrence (don't override)
                                   (unless (hashtable-ref *source-locations* name #f)
                                           (hashtable-set! *source-locations* name (cons file line))
                                           (set! count (+ count 1)))))
                      defs)))
        (find-scheme-files-loc "lattice"))
       ;; Also scan core/
       (for-each
        (lambda (path)
                (let ([defs (extract-definitions-from-file path)])
                     (for-each
                      (lambda (def)
                              (let ([name (car def)]
                                    [file (cadr def)]
                                    [line (caddr def)])
                                   (unless (hashtable-ref *source-locations* name #f)
                                           (hashtable-set! *source-locations* name (cons file line))
                                           (set! count (+ count 1)))))
                      defs)))
        (find-scheme-files-loc "core"))
       (printf "  Indexed ~a definitions\n" count)))

;;; get-source-location : Symbol -> (File . Line) | #f
;;; Get the source location for a symbol
(define (get-source-location sym)
  (guard (e [else #f])
         (hashtable-ref *source-locations* sym #f)))

;;; format-source-location : Symbol -> String | #f
;;; Get formatted "file:line" string for a symbol
(define (format-source-location sym)
  (let ([loc (get-source-location sym)])
       (if loc
           (format "~a:~a" (car loc) (cdr loc))
           #f)))

;;; source-location-count : -> Nat
;;; Get number of indexed definitions
(define (source-location-count)
  (hashtable-size *source-locations*))

;;; ============================================================
;;; REPL Interface
;;; ============================================================

(printf "source-loc.ss loaded.\n")
(printf "  (build-source-location-cache!)  - Build cache from sources\n")
(printf "  (get-source-location 'fn)       - Get (file . line) pair\n")
(printf "  (format-source-location 'fn)    - Get \"file:line\" string\n")
