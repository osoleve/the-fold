(load "core/base/prelude.ss")

(doc 'module 'meta-file-io)
(doc 'description "Shared file I/O primitives for lattice meta-tooling. Consolidates read-file-lines, read-all-sexps, find-scheme-files, and read-manifest which were previously duplicated across 9 files.")
(doc 'layer 'boundary)

;;; ====
;;; File Reading Primitives
;;; ====

;;; read-file-lines : String -> (List String) | '()
;;; Read all lines from a file. Returns empty list if file doesn't exist.
;;; Previously duplicated in: source-loc.ss, docstrings.ss, export-annotator.ss
(define (read-file-lines filepath)
  (if (file-exists? filepath)
      (call-with-input-file filepath
                            (lambda (port)
                                    (let loop ([lines '()])
                                         (let ([line (get-line port)])
                                              (if (eof-object? line)
                                                  (reverse lines)
                                                  (loop (cons line lines)))))))
      '()))

;;; read-file-as-string : String -> String | #f
;;; Read entire file contents as a string. Returns #f if file doesn't exist.
;;; Previously in: audit.ss
(define (read-file-as-string filepath)
  (if (file-exists? filepath)
      (call-with-input-file filepath
                            (lambda (port)
                                    (get-string-all port)))
      #f))

;;; read-all-sexps : String -> (List SExp) | '()
;;; Read all S-expressions from a file. Returns empty list on error.
;;; Previously duplicated in: docs.ss, exports.ss, manifest-sync.ss
(define (read-all-sexps filepath)
  (guard (e [else '()])
         (call-with-input-file filepath
                               (lambda (port)
                                       (let loop ([result '()])
                                            (let ([sexp (read port)])
                                                 (if (eof-object? sexp)
                                                     (reverse result)
                                                     (loop (cons sexp result)))))))))

;;; read-manifest-sexp : String -> SExp | #f
;;; Read and parse a manifest.sexp file, skipping leading comments.
;;; Previously duplicated in: kg.ss, audit.ss, manifest-sync.ss
(define (read-manifest-sexp filepath)
  (guard (e [else #f])
         (call-with-input-file filepath
                               (lambda (port)
                                       (let loop ()
                                            (let ([c (peek-char port)])
                                                 (cond
                                                  [(eof-object? c) #f]
                                                  [(char=? c #\;)
                                                   (get-line port)
                                                   (loop)]
                                                  [(char-whitespace? c)
                                                   (get-char port)
                                                   (loop)]
                                                  [else (read port)])))))))

;;; ====
;;; File Discovery
;;; ====

;;; find-scheme-files : String -> (List String)
;;; Recursively find all .ss files under a directory.
;;; Previously duplicated (with variations) in: docs.ss, docstrings.ss,
;;; source-loc.ss, xref.ss, exports.ss (5 separate implementations!)
(define (find-scheme-files base-dir)
  (guard (e [else '()])
         (let loop ([dirs (list base-dir)] [files '()])
              (if (null? dirs)
                  (reverse files)
                  (let* ([dir (car dirs)]
                         [entries (guard (e [else '()])
                                        (directory-list dir))]
                         [paths (map (lambda (e) (string-append dir "/" e)) entries)]
                         [new-dirs (filter (lambda (p)
                                                   (and (guard (e [else #f])
                                                               (file-directory? p))
                                                        (not (string-starts-with? (car (reverse (string-split-char p #\/))) "."))))
                                          paths)]
                         [new-files (filter (lambda (p) (string-ends-with? p ".ss")) paths)])
                        (loop (append (cdr dirs) new-dirs)
                              (append files new-files)))))))

;;; find-manifests : String -> (List String)
;;; Find all manifest.sexp files under a directory.
;;; Previously in: kg.ss
(define (find-manifests base-dir)
  (let ([result '()])
       (let ([manifest-path (string-append base-dir "/manifest.sexp")])
            (when (file-exists? manifest-path)
                  (set! result (cons manifest-path result))))
       (guard (e [else result])
              (for-each
               (lambda (entry)
                       (let ([path (string-append base-dir "/" entry)])
                            (when (and (file-directory? path)
                                       (not (string-starts-with? entry ".")))
                                  (set! result (append (find-manifests path) result)))))
               (directory-list base-dir)))
       result))

;;; ====
;;; Helpers
;;; ====

;;; string-ends-with? : String String -> Boolean
;;; Previously duplicated with suffix-specific variants across meta files.
(define (string-ends-with? str suffix)
  (let ([slen (string-length str)]
        [plen (string-length suffix)])
       (and (>= slen plen)
            (string=? (substring str (- slen plen) slen) suffix))))

;;; string-split-char : String Char -> (List String)
;;; Split string by character. Helper for path manipulation.
(define (string-split-char str delim)
  (let loop ([chars (string->list str)]
             [current '()]
             [result '()])
       (cond
        [(null? chars)
         (reverse (if (null? current)
                      result
                      (cons (list->string (reverse current)) result)))]
        [(char=? (car chars) delim)
         (loop (cdr chars) '()
               (if (null? current) result
                   (cons (list->string (reverse current)) result)))]
        [else
         (loop (cdr chars) (cons (car chars) current) result)])))
