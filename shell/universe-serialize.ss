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
          read-sexp-file-strict  ; Ensures entire file is one sexp
          read-sexp-file-safe    ; Returns (ok . value) or (error type . message)
          read-sexp-files
          
          ;; Error handling
          format-sexp-error
          classify-sexp-error
          
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
                                      (char=? (string-ref rel 0) #\)))
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
                              (char=? (string-ref normalized (- len 1)) #\)))
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
         ;;; Example: (scan-sexp-files-filtered "/home/fold" '("forum" "docs/decisions"))
         (define (scan-sexp-files-filtered root-dir dirs)
           (filter-by-directories (scan-sexp-files root-dir) dirs))
         
         ;;; ============================================================
         ;;; Error Formatting
         ;;; ============================================================
         
         ;;; get-condition-irritants : Condition → List
         ;;; Extract irritants from a compound condition.
         (define (get-condition-irritants ex)
           (let loop ([components (simple-conditions ex)])
                (cond
                 [(null? components) '()]
                 [(irritants-condition? (car components))
                  (condition-irritants (car components))]
                 [else (loop (cdr components))])))
         
         ;;; count-format-directives : String → Nat
         ;;; Count the number of format directives (~a, ~s, ~d, etc.) in a string.
         (define (count-format-directives fmt)
           (let loop ([s fmt] [count 0])
                (if (string=? s "")
                    count
                    (let ([len (string-length s)])
                         (if (and (>= len 2)
                                  (char=? (string-ref s 0) #\~))
                             (let ([directive (string-ref s 1)])
                                  (case directive
                                        ;; Format directives that consume an argument
                                        [(# #\s #\d #\x # #\o #