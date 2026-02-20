;;; lattice/meta/purity-audit.ss — Purity Annotation Audit
;;; @module meta/purity-audit
;;; @requires prelude

(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
(require 'prelude)

(doc 'module 'meta/purity-audit)
(doc 'description "Audit lattice modules for (doc 'purity ...) annotations. Scans registered module files and reports coverage.")
(doc 'layer 'lattice)
(doc 'purity 'partial)  ; File I/O for scanning source files
(doc 'note "Meta-tooling for enforcing purity annotation discipline across the lattice.")

;;; ====
;;; File Content Scanning
;;; ====

;;; read-file-string : String -> (Maybe String)
;;; Read entire file as a string. Returns #f if file doesn't exist.
(define (read-file-string path)
  (guard (e [#t #f])
    (let* ([port (open-input-file path)]
           [contents (get-string-all port)])
      (close-input-port port)
      (if (eof-object? contents) "" contents))))

;;; extract-purity-value : String -> (Maybe Symbol)
;;; Search file contents for (doc 'purity ...) and extract the value.
;;; Returns the purity symbol ('total, 'partial, etc.) or #f if not found.
(define (extract-purity-value contents)
  (let ([needle "(doc 'purity '"]
        [len (string-length contents)])
    (let loop ([i 0])
      (cond
        [(>= i (- len (string-length needle))) #f]
        [(string-prefix-at? contents needle i)
         (let ([start (+ i (string-length needle))])
           (let sym-loop ([j start] [chars '()])
             (cond
               [(>= j len) #f]
               [(char=? (string-ref contents j) #\))
                (string->symbol (list->string (reverse chars)))]
               [(or (char-alphabetic? (string-ref contents j))
                    (char=? (string-ref contents j) #\-))
                (sym-loop (+ j 1) (cons (string-ref contents j) chars))]
               [else #f])))]
        [else (loop (+ i 1))]))))

;;; string-prefix-at? : String String Nat -> Boolean
;;; Check if needle appears at position pos in haystack.
(define (string-prefix-at? haystack needle pos)
  (let ([nlen (string-length needle)]
        [hlen (string-length haystack)])
    (and (<= (+ pos nlen) hlen)
         (let loop ([i 0])
           (or (>= i nlen)
               (and (char=? (string-ref haystack (+ pos i))
                            (string-ref needle i))
                    (loop (+ i 1))))))))

;;; ====
;;; Module Registry Scanning
;;; ====

;;; lattice-module-paths : -> (List (Pair Symbol String))
;;; Extract all registered lattice module paths from the module path registry.
;;; Returns alist of (module-name . file-path), filtered to lattice/ only.
(define (lattice-module-paths)
  (let ([entries '()]
        [names (hashtable-keys *module-paths*)])
    (vector-for-each
      (lambda (name)
        (let ([path (hashtable-ref *module-paths* name #f)])
          (when (and path
                     (>= (string-length path) 8)
                     (string=? (substring path 0 8) "lattice/")
                     ;; Skip test files
                     (not (let ([s (symbol->string name)])
                            (and (>= (string-length s) 5)
                                 (string=? (substring s 0 5) "test-")))))
            (set! entries (cons (cons name path) entries)))))
      names)
    entries))

;;; ====
;;; Audit Functions
;;; ====

;;; purity-audit-file : String -> (Maybe Symbol)
;;; Check a single file for purity annotation.
;;; Returns the purity value if annotated, #f if missing.
(define (purity-audit-file path)
  (let ([contents (read-file-string path)])
    (if contents
        (extract-purity-value contents)
        #f)))

;;; lattice-purity-audit : -> Alist
;;; Scan all registered lattice modules for purity annotations.
;;; Returns summary alist:
;;;   ((total . N) (annotated . M) (missing . K)
;;;    (by-value . ((total . T) (partial . P) ...))
;;;    (files-annotated . ((name path value) ...))
;;;    (files-missing . ((name path) ...)))
(define (lattice-purity-audit)
  (let* ([modules (lattice-module-paths)]
         [results
          (map (lambda (entry)
                 (let ([name (car entry)]
                       [path (cdr entry)])
                   (let ([purity (purity-audit-file path)])
                     (list name path purity))))
               modules)]
         [annotated (filter (lambda (r) (caddr r)) results)]
         [missing (filter (lambda (r) (not (caddr r))) results)]
         ;; Count by purity value
         [by-value
          (let ([counts '()])
            (for-each
              (lambda (r)
                (let* ([val (caddr r)]
                       [existing (assq val counts)])
                  (if existing
                      (set-cdr! existing (+ (cdr existing) 1))
                      (set! counts (cons (cons val 1) counts)))))
              annotated)
            counts)])
    `((total . ,(length modules))
      (annotated . ,(length annotated))
      (missing . ,(length missing))
      (by-value . ,by-value)
      (files-annotated . ,(map (lambda (r) (list (car r) (cadr r) (caddr r))) annotated))
      (files-missing . ,(map (lambda (r) (list (car r) (cadr r))) missing)))))

;;; purity-audit-report : -> Void
;;; Print a human-readable purity audit report.
(define (purity-audit-report)
  (let* ([audit (lattice-purity-audit)]
         [total (cdr (assq 'total audit))]
         [annotated (cdr (assq 'annotated audit))]
         [missing (cdr (assq 'missing audit))]
         [by-value (cdr (assq 'by-value audit))]
         [files-missing (cdr (assq 'files-missing audit))])
    (printf "=== Lattice Purity Audit ===\n\n")
    (printf "Registered lattice modules: ~a\n" total)
    (printf "With purity annotation:     ~a (~a%)\n"
            annotated
            (if (> total 0) (round (* 100.0 (/ annotated total))) 0))
    (printf "Missing annotation:         ~a\n\n" missing)
    (printf "--- By Purity Value ---\n")
    (for-each
      (lambda (pair)
        (printf "  ~a: ~a\n" (car pair) (cdr pair)))
      by-value)
    (when (> missing 0)
      (printf "\n--- Missing Annotations (~a files) ---\n" missing)
      (for-each
        (lambda (entry)
          (printf "  ~a  (~a)\n" (cadr entry) (car entry)))
        (list-sort (lambda (a b) (string<? (symbol->string (car a))
                                          (symbol->string (car b))))
                   files-missing)))
    (printf "\n")))
