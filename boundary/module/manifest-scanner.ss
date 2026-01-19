;;; boundary/module/manifest-scanner.ss — Manifest Discovery (I/O)
;;;
;;; Impure scanning for manifest.sexp files.
;;; Uses only core primitives (no lattice deps) to avoid bootstrap issues.
;;;
;;; This is Shell code: does I/O, discovers files.
;;;
;;; Usage:
;;;   (find-manifests dir)              ; Recursive directory scan
;;;   (read-manifest-file path)         ; File → S-expression | #f
;;;   (scan-all-manifests)              ; → list of (path . sexp) pairs
;;;   (scan-lattice-manifests)          ; Scan lattice/ directory
;;;
;;; Dependencies:
;;;   core/base/prelude.ss (implicitly loaded)

;;; ====
;;; File Reading
;;; ====

;;; read-manifest-file : String -> SExp | #f
;;; Read and parse a manifest.sexp file.
;;; Returns the first S-expression in the file, or #f on error.
(define (read-manifest-file filepath)
  (guard (e [else #f])
         (call-with-input-file filepath
                               (lambda (port)
                                       ;; Skip comment lines at the start
                                       (let loop ()
                                            (let ([c (peek-char port)])
                                                 (cond
                                                  [(eof-object? c) #f]
                                                  [(char=? c #\;)
                                                   (get-line port)  ; Skip comment line
                                                   (loop)]
                                                  [(char-whitespace? c)
                                                   (get-char port)
                                                   (loop)]
                                                  [else (read port)])))))))

;;; ====
;;; Directory Scanning
;;; ====

;;; string-starts-with? : String String -> Bool
;;; Check if string starts with prefix.
(define (manifest-string-starts-with? str prefix)
  (let ([str-len (string-length str)]
        [pre-len (string-length prefix)])
       (and (>= str-len pre-len)
            (string=? (substring str 0 pre-len) prefix))))

;;; find-manifests : String -> (List String)
;;; Find all manifest.sexp files under a directory (recursive).
;;; Returns a list of file paths in sorted order for deterministic discovery.
(define (find-manifests base-dir)
  ;; Use accumulator-based recursion to avoid O(N²) append
  (define (find-acc dir acc)
    ;; Check for manifest in this directory
    (let* ([manifest-path (string-append dir "/manifest.sexp")]
           [acc (if (file-exists? manifest-path)
                    (cons manifest-path acc)
                    acc)])
          ;; Recursively check subdirectories (sorted for determinism)
          (guard (e [else acc])
                 (let ([entries (sort string<? (directory-list dir))])
                      (fold-left
                       (lambda (acc entry)
                               (let ([path (string-append dir "/" entry)])
                                    (if (and (file-directory? path)
                                             (not (manifest-string-starts-with? entry ".")))
                                        (find-acc path acc)
                                        acc)))
                       acc
                       entries)))))
  (reverse (find-acc base-dir '())))

;;; ====
;;; Main Scanning API
;;; ====

;;; scan-manifests : String -> (List (String . SExp))
;;; Find all manifests under a directory and read their contents.
;;; Returns a list of (path . sexp) pairs where sexp is the parsed manifest.
;;; Skips files that fail to parse.
(define (scan-manifests base-dir)
  (let* ([manifest-paths (find-manifests base-dir)]
         [results '()])
        (for-each
         (lambda (path)
                 (let ([sexp (read-manifest-file path)])
                      (when (and sexp (pair? sexp) (eq? (car sexp) 'skill))
                            (set! results (cons (cons path sexp) results)))))
         manifest-paths)
        results))

;;; scan-lattice-manifests : -> (List (String . SExp))
;;; Scan the lattice/ directory for all manifests.
;;; Convenience function for the common case.
(define (scan-lattice-manifests)
  (scan-manifests "lattice"))

;;; scan-all-manifests : -> (List (String . SExp))
;;; Scan all known directories for manifests.
;;; Currently just scans lattice/ since core modules are registered explicitly.
(define (scan-all-manifests)
  (scan-lattice-manifests))

;;; ====
;;; Debugging
;;; ====

;;; list-manifest-files : String -> (List String)
;;; List all manifest.sexp file paths under a directory (for debugging).
(define (list-manifest-files base-dir)
  (find-manifests base-dir))

;;; count-manifests : String -> Nat
;;; Count manifest files under a directory.
(define (count-manifests base-dir)
  (length (find-manifests base-dir)))
