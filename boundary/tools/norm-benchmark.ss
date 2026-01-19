;;; boundary/tools/norm-benchmark.ss — Normalization Equivalence Benchmarks
;;; @module norm-benchmark
;;;
;;; Measures how often different normalization levels detect equivalences.
;;; Compares v0x00 (α-only), v0x01 (algebraic), and v0x02 (full) hashing.
;;;
;;; Usage:
;;;   (load "boundary/tools/norm-benchmark.ss")
;;;   (norm-benchmark-file "lattice/linalg/vec.ss")
;;;   (norm-benchmark-directory "lattice/linalg")
;;;   (norm-benchmark-lattice)  ; Full lattice scan

(load "core/blocks/cas.ss")

;;; ============================================================
;;; Expression Extraction
;;; ============================================================

;;; extract-exprs-from-file : String → (List S-expr)
;;; Read all top-level S-expressions from a file.
(define (extract-exprs-from-file path)
  (guard (ex [else '()])
    (call-with-input-file path
      (lambda (port)
        (let loop ([exprs '()])
          (let ([expr (read port)])
            (if (eof-object? expr)
                (reverse exprs)
                (loop (cons expr exprs)))))))))

;;; list? : any → Bool
;;; Check if something is a proper list.
(define (proper-list? x)
  (or (null? x)
      (and (pair? x) (proper-list? (cdr x)))))

;;; extract-subexprs : S-expr → (List S-expr)
;;; Extract all subexpressions from an expression (for deeper analysis).
;;; Handles dotted pairs and improper lists safely.
(define (extract-subexprs expr)
  (cond
    [(not (pair? expr)) (list expr)]
    [(eq? (car expr) 'quote) (list expr)]
    [(not (proper-list? expr)) (list expr)]  ; Don't recurse into improper lists
    [else
     (cons expr
           (apply append (map extract-subexprs expr)))]))

;;; ============================================================
;;; Hashing at Multiple Levels
;;; ============================================================

;;; hash-expr-all-levels : S-expr → (List (Version . Hash))
;;; Hash an expression at all three normalization levels.
;;; Returns alist: ((v0 . hash0) (v1 . hash1) (v2 . hash2))
(define (hash-expr-all-levels expr)
  (guard (ex [else #f])  ; Skip expressions that fail to normalize
    (list
      (cons 'v0 (hash-sexpr 'benchmark expr))
      (cons 'v1 (hash-sexpr-algebraic 'benchmark expr))
      (cons 'v2 (hash-sexpr-v2 'benchmark expr)))))

;;; ============================================================
;;; Unique Hash Counting
;;; ============================================================

;;; count-unique-hashes : (List S-expr) → Alist
;;; Count unique hashes at each normalization level.
;;; Returns ((v0-unique . N) (v1-unique . N) (v2-unique . N)
;;;          (v0-v1-reduction . N) (v1-v2-reduction . N) (total . N))
(define (count-unique-hashes exprs)
  (let ([v0-set (make-hashtable equal-hash equal?)]
        [v1-set (make-hashtable equal-hash equal?)]
        [v2-set (make-hashtable equal-hash equal?)]
        [total 0]
        [failed 0])

    ;; Hash each expression
    (for-each
      (lambda (expr)
        (let ([hashes (hash-expr-all-levels expr)])
          (if hashes
              (begin
                (set! total (+ total 1))
                (hashtable-set! v0-set (cdr (assq 'v0 hashes)) #t)
                (hashtable-set! v1-set (cdr (assq 'v1 hashes)) #t)
                (hashtable-set! v2-set (cdr (assq 'v2 hashes)) #t))
              (set! failed (+ failed 1)))))
      exprs)

    (let ([v0-count (hashtable-size v0-set)]
          [v1-count (hashtable-size v1-set)]
          [v2-count (hashtable-size v2-set)])
      `((total-expressions . ,total)
        (failed-expressions . ,failed)
        (v0-unique-hashes . ,v0-count)
        (v1-unique-hashes . ,v1-count)
        (v2-unique-hashes . ,v2-count)
        (v0-v1-reduction . ,(- v0-count v1-count))
        (v1-v2-reduction . ,(- v1-count v2-count))
        (total-reduction . ,(- v0-count v2-count))
        (v0-v1-reduction-pct . ,(if (> v0-count 0)
                                    (exact->inexact (* 100 (/ (- v0-count v1-count) v0-count)))
                                    0.0))
        (v1-v2-reduction-pct . ,(if (> v1-count 0)
                                    (exact->inexact (* 100 (/ (- v1-count v2-count) v1-count)))
                                    0.0))
        (total-reduction-pct . ,(if (> v0-count 0)
                                    (exact->inexact (* 100 (/ (- v0-count v2-count) v0-count)))
                                    0.0))))))

;;; ============================================================
;;; File and Directory Benchmarks
;;; ============================================================

;;; norm-benchmark-file : String → Alist
;;; Benchmark a single file.
(define (norm-benchmark-file path)
  (let* ([exprs (extract-exprs-from-file path)]
         [all-subexprs (apply append (map extract-subexprs exprs))]
         [results (count-unique-hashes all-subexprs)])
    (cons `(file . ,path) results)))

;;; norm-benchmark-files : (List String) → Alist
;;; Benchmark multiple files, aggregating results.
(define (norm-benchmark-files paths)
  (let ([all-exprs '()])
    (for-each
      (lambda (path)
        (let* ([exprs (extract-exprs-from-file path)]
               [subexprs (apply append (map extract-subexprs exprs))])
          (set! all-exprs (append subexprs all-exprs))))
      paths)
    (let ([results (count-unique-hashes all-exprs)])
      (cons `(files . ,(length paths)) results))))

;;; ============================================================
;;; Directory Scanning
;;; ============================================================

;;; find-scheme-files : String → (List String)
;;; Find all .ss files in a directory (non-recursive).
(define (find-scheme-files dir)
  (guard (ex [else '()])
    (let ([entries (directory-list dir)])
      (filter
        (lambda (f) (string-suffix? ".ss" f))
        (map (lambda (f) (string-append dir "/" f)) entries)))))

;;; find-scheme-files-recursive : String → (List String)
;;; Find all .ss files recursively.
(define (find-scheme-files-recursive dir)
  (guard (ex [else '()])
    (let ([entries (directory-list dir)])
      (apply append
        (map (lambda (entry)
               (let ([path (string-append dir "/" entry)])
                 (cond
                   [(string-suffix? ".ss" entry) (list path)]
                   [(and (not (string-prefix? "." entry))
                         (file-directory? path))
                    (find-scheme-files-recursive path)]
                   [else '()])))
             entries)))))

;;; string-suffix? : String × String → Bool
(define (string-suffix? suffix str)
  (let ([slen (string-length suffix)]
        [len (string-length str)])
    (and (>= len slen)
         (string=? suffix (substring str (- len slen) len)))))

;;; string-prefix? : String × String → Bool
(define (string-prefix? prefix str)
  (let ([plen (string-length prefix)]
        [len (string-length str)])
    (and (>= len plen)
         (string=? prefix (substring str 0 plen)))))

;;; norm-benchmark-directory : String → Alist
;;; Benchmark all .ss files in a directory.
(define (norm-benchmark-directory dir)
  (let* ([files (find-scheme-files-recursive dir)]
         [results (norm-benchmark-files files)])
    (cons `(directory . ,dir) results)))

;;; ============================================================
;;; Lattice-Wide Benchmark
;;; ============================================================

;;; norm-benchmark-lattice : → Alist
;;; Benchmark the entire lattice directory.
(define (norm-benchmark-lattice)
  (norm-benchmark-directory "lattice"))

;;; norm-benchmark-core : → Alist
;;; Benchmark the core directory.
(define (norm-benchmark-core)
  (norm-benchmark-directory "core"))

;;; norm-benchmark-all : → Alist
;;; Benchmark both core and lattice.
(define (norm-benchmark-all)
  (let* ([core-files (find-scheme-files-recursive "core")]
         [lattice-files (find-scheme-files-recursive "lattice")]
         [all-files (append core-files lattice-files)]
         [results (norm-benchmark-files all-files)])
    (cons `(scope . "core+lattice") results)))

;;; ============================================================
;;; Pretty Printing
;;; ============================================================

;;; print-benchmark-results : Alist → void
;;; Pretty-print benchmark results.
(define (print-benchmark-results results)
  (newline)
  (display "═══════════════════════════════════════════════════════════\n")
  (display "         NORMALIZATION EQUIVALENCE BENCHMARK\n")
  (display "═══════════════════════════════════════════════════════════\n")
  (newline)

  ;; Scope
  (cond
    [(assq 'file results)
     (display (format "File: ~a\n" (cdr (assq 'file results))))]
    [(assq 'directory results)
     (display (format "Directory: ~a\n" (cdr (assq 'directory results))))]
    [(assq 'scope results)
     (display (format "Scope: ~a\n" (cdr (assq 'scope results))))]
    [(assq 'files results)
     (display (format "Files scanned: ~a\n" (cdr (assq 'files results))))])

  (newline)
  (display "───────────────────────────────────────────────────────────\n")
  (display "                    EXPRESSION COUNTS\n")
  (display "───────────────────────────────────────────────────────────\n")
  (display (format "  Total expressions:     ~a\n" (cdr (assq 'total-expressions results))))
  (display (format "  Failed to normalize:   ~a\n" (cdr (assq 'failed-expressions results))))

  (newline)
  (display "───────────────────────────────────────────────────────────\n")
  (display "                    UNIQUE HASHES\n")
  (display "───────────────────────────────────────────────────────────\n")
  (display (format "  v0x00 (α-only):        ~a\n" (cdr (assq 'v0-unique-hashes results))))
  (display (format "  v0x01 (algebraic):     ~a\n" (cdr (assq 'v1-unique-hashes results))))
  (display (format "  v0x02 (full v2):       ~a\n" (cdr (assq 'v2-unique-hashes results))))

  (newline)
  (display "───────────────────────────────────────────────────────────\n")
  (display "                  EQUIVALENCE DETECTION\n")
  (display "───────────────────────────────────────────────────────────\n")
  (display (format "  v0→v1 reduction:       ~a (~,1f%)\n"
                   (cdr (assq 'v0-v1-reduction results))
                   (cdr (assq 'v0-v1-reduction-pct results))))
  (display (format "  v1→v2 reduction:       ~a (~,1f%)\n"
                   (cdr (assq 'v1-v2-reduction results))
                   (cdr (assq 'v1-v2-reduction-pct results))))
  (display (format "  Total reduction:       ~a (~,1f%)\n"
                   (cdr (assq 'total-reduction results))
                   (cdr (assq 'total-reduction-pct results))))

  (newline)
  (display "═══════════════════════════════════════════════════════════\n")
  (newline))

;;; run-benchmark : [String] → void
;;; Run benchmark and print results. Optional path argument.
(define run-benchmark
  (case-lambda
    [() (print-benchmark-results (norm-benchmark-all))]
    [(path)
     (if (file-directory? path)
         (print-benchmark-results (norm-benchmark-directory path))
         (print-benchmark-results (norm-benchmark-file path)))]))
