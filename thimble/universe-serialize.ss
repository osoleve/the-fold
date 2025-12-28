;;; thimble/universe-serialize.ss — Universe Tree Serialization
;;;
;;; Serializes the entire universe-tree (all .sexp files) to a single file.
;;; The .fold project contains .sexp files scattered across forum/, playpen/,
;;; scripture/, and other directories. This tool collects and serializes them.
;;;
;;; This is Shell code: impure, handles I/O and filesystem operations.
;;;
;;; Core operations:
;;;   (scan-sexp-files root-dir)                → (List String)
;;;   (scan-sexp-files-filtered root-dir filter) → (List String)
;;;   (read-sexp-file path)                      → (S-expr | #f)
;;;   (serialize-universe root-dir)              → S-expr
;;;   (serialize-universe-filtered root-dir filter) → S-expr
;;;   (write-universe sexp output-path pretty?)  → void
;;;
;;; Output format:
;;;   ((files
;;;     (("path/to/file.sexp" . <contents>)
;;;      ("another/file.sexp" . <contents>)
;;;      ...)))
;;;
;;; The tool:
;;;   - Preserves relative paths from root directory
;;;   - Handles errors gracefully (skips unreadable files)
;;;   - Supports filtering by directory patterns
;;;   - Supports pretty printing for human readability

(library (shell universe-serialize)
  (export
    ;; File scanning
    scan-sexp-files
    scan-sexp-files-filtered

    ;; File reading
    read-sexp-file
    read-sexp-files

    ;; Serialization
    serialize-universe
    serialize-universe-filtered

    ;; Output
    write-universe
    write-universe-pretty

    ;; Utilities
    make-relative-path
    sexp-file?
    filter-by-directories)

  (import (chezscheme))

;;; ============================================================
;;; Path Utilities
;;; ============================================================

;;; make-relative-path : String × String → String
;;; Convert absolute path to relative path from root.
;;; Example: ("<project-root>", "<project-root>/forum/test.sexp") → "forum/test.sexp"
(define (make-relative-path root-dir absolute-path)
  (let ([root-len (string-length root-dir)])
    (if (and (>= (string-length absolute-path) root-len)
             (string=? root-dir (substring absolute-path 0 root-len)))
        (let ([rel (substring absolute-path root-len (string-length absolute-path))])
          ;; Strip leading slash if present
          (if (and (> (string-length rel) 0)
                   (or (char=? (string-ref rel 0) #\/)
                       (char=? (string-ref rel 0) #\\)))
              (substring rel 1 (string-length rel))
              rel))
        absolute-path)))

;;; normalize-path : String → String
;;; Normalize path separators and remove trailing slashes.
(define (normalize-path path)
  (let* ([normalized (if (string=? path "")
                         "."
                         path)]
         [len (string-length normalized)])
    ;; Remove trailing slash if not root
    (if (and (> len 1)
             (or (char=? (string-ref normalized (- len 1)) #\/)
                 (char=? (string-ref normalized (- len 1)) #\\)))
        (substring normalized 0 (- len 1))
        normalized)))

;;; sexp-file? : String → Boolean
;;; Check if path is a .sexp file.
(define (sexp-file? path)
  (let ([len (string-length path)])
    (and (>= len 5)
         (string=? ".sexp" (substring path (- len 5) len)))))

;;; path-contains-directory? : String × (List String) → Boolean
;;; Check if path contains any of the specified directories.
(define (path-contains-directory? path dirs)
  (if (null? dirs)
      #t  ; No filter means accept all
      (let loop ([remaining dirs])
        (if (null? remaining)
            #f
            (let* ([dir (car remaining)]
                   [dir-with-sep (string-append dir "/")])
              (if (or (string-contains? path dir-with-sep)
                      (string-prefix? dir path))
                  #t
                  (loop (cdr remaining))))))))

;;; string-contains? : String × String → Boolean
(define (string-contains? haystack needle)
  (let ([hlen (string-length haystack)]
        [nlen (string-length needle)])
    (let loop ([i 0])
      (cond
        [(> (+ i nlen) hlen) #f]
        [(string=? needle (substring haystack i (+ i nlen))) #t]
        [else (loop (+ i 1))]))))

;;; string-prefix? : String × String → Boolean
;;; Check if str starts with prefix.
(define (string-prefix? prefix str)
  (let ([plen (string-length prefix)]
        [len (string-length str)])
    (and (>= len plen)
         (string=? prefix (substring str 0 plen)))))

;;; ============================================================
;;; File Scanning
;;; ============================================================

;;; scan-directory-recursive : String → (List String)
;;; Recursively scan directory and return all file paths.
(define (scan-directory-recursive dir)
  (if (file-directory? dir)
      (let ([entries (directory-list dir)])
        (apply append
          (map (lambda (entry)
                 (let ([full-path (string-append dir "/" entry)])
                   (cond
                     [(file-directory? full-path)
                      (scan-directory-recursive full-path)]
                     [(file-regular? full-path)
                      (list full-path)]
                     [else '()])))
               entries)))
      '()))

;;; scan-sexp-files : String → (List String)
;;; Recursively find all .sexp files in directory tree.
(define (scan-sexp-files root-dir)
  (let ([normalized-root (normalize-path root-dir)])
    (filter sexp-file? (scan-directory-recursive normalized-root))))

;;; filter-by-directories : (List String) × (List String) → (List String)
;;; Filter file paths to include only those in specified directories.
(define (filter-by-directories paths dirs)
  (if (null? dirs)
      paths
      (filter (lambda (path) (path-contains-directory? path dirs)) paths)))

;;; scan-sexp-files-filtered : String × (List String) → (List String)
;;; Scan for .sexp files, filtering by directories.
;;; Example: (scan-sexp-files-filtered "/home/fold" '("forum" "scripture"))
(define (scan-sexp-files-filtered root-dir dirs)
  (filter-by-directories (scan-sexp-files root-dir) dirs))

;;; ============================================================
;;; File Reading
;;; ============================================================

;;; read-sexp-file : String → (S-expr | #f)
;;; Read and parse a .sexp file. Returns #f on error.
(define (read-sexp-file path)
  (guard (ex
          [else
           (display "Warning: Failed to read ")
           (display path)
           (display ": ")
           (display (if (condition? ex)
                        (condition-message ex)
                        ex))
           (newline)
           #f])
    (call-with-input-file path
      (lambda (port)
        (read port)))))

;;; read-sexp-files : (List String) × String → (List (cons String S-expr))
;;; Read multiple .sexp files, returning (path . contents) pairs.
;;; Skips files that fail to read (returns #f for content).
(define (read-sexp-files paths root-dir)
  (let loop ([remaining paths]
             [result '()])
    (if (null? remaining)
        (reverse result)
        (let* ([path (car remaining)]
               [rel-path (make-relative-path root-dir path)]
               [contents (read-sexp-file path)])
          (if contents
              (loop (cdr remaining) (cons (cons rel-path contents) result))
              (loop (cdr remaining) result))))))

;;; ============================================================
;;; Serialization
;;; ============================================================

;;; serialize-universe : String → S-expr
;;; Serialize all .sexp files in the universe to a single S-expression.
;;; Returns: ((files (("path" . contents) ...)))
(define (serialize-universe root-dir)
  (let* ([normalized-root (normalize-path root-dir)]
         [sexp-paths (scan-sexp-files normalized-root)]
         [file-data (read-sexp-files sexp-paths normalized-root)])
    `((files ,file-data))))

;;; serialize-universe-filtered : String × (List String) → S-expr
;;; Serialize .sexp files, filtered by directories.
(define (serialize-universe-filtered root-dir dirs)
  (let* ([normalized-root (normalize-path root-dir)]
         [sexp-paths (scan-sexp-files-filtered normalized-root dirs)]
         [file-data (read-sexp-files sexp-paths normalized-root)])
    `((files ,file-data))))

;;; ============================================================
;;; Output Writing
;;; ============================================================

;;; write-universe : S-expr × String × Boolean → void
;;; Write serialized universe to file.
;;; If pretty? is #t, format with indentation for readability.
(define (write-universe sexp output-path pretty?)
  (guard (ex
          [else
           (display "Error writing to ")
           (display output-path)
           (display ": ")
           (display (if (condition? ex)
                        (condition-message ex)
                        ex))
           (newline)
           (raise ex)])
    (call-with-output-file output-path
      (lambda (port)
        (if pretty?
            (pretty-print sexp port)
            (write sexp port))
        (newline port)))))

;;; write-universe-pretty : S-expr × String → void
;;; Convenience wrapper for pretty-printed output.
(define (write-universe-pretty sexp output-path)
  (write-universe sexp output-path #t))

) ;; end library
