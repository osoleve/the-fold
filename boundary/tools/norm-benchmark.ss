(load "core/blocks/cas.ss")

(doc 'module 'norm-benchmark)
(doc 'description "Normalization equivalence benchmarks. Measures how often different normalization levels detect equivalences across v0x00 (α-only), v0x01 (algebraic), and v0x02 (full) hashing.")
(doc 'layer 'boundary)
(doc 'purity 'partial)

(doc 'section 'expression-extraction)

(doc extract-exprs-from-file 'type (-> String (List Sexpr)))
(doc extract-exprs-from-file 'description "Read all top-level S-expressions from a file")
(define (extract-exprs-from-file path)
  (guard (ex [else '()])
    (call-with-input-file path
      (lambda (port)
        (let loop ([exprs '()])
          (let ([expr (read port)])
            (if (eof-object? expr)
                (reverse exprs)
                (loop (cons expr exprs)))))))))

(doc proper-list? 'type (-> Any Bool))
(doc proper-list? 'description "Check if something is a proper list")
(define (proper-list? x)
  (or (null? x)
      (and (pair? x) (proper-list? (cdr x)))))

(doc extract-subexprs 'type (-> Sexpr (List Sexpr)))
(doc extract-subexprs 'description "Extract all subexpressions from an expression for deeper analysis. Handles dotted pairs and improper lists safely.")
(define (extract-subexprs expr)
  (cond
    [(not (pair? expr)) (list expr)]
    [(eq? (car expr) 'quote) (list expr)]
    [(not (proper-list? expr)) (list expr)]  ; Don't recurse into improper lists
    [else
     (cons expr
           (apply append (map extract-subexprs expr)))]))

(doc 'section 'hashing)

(doc hash-expr-all-levels 'type (-> Sexpr (Maybe Alist)))
(doc hash-expr-all-levels 'description "Hash an expression at all three normalization levels. Returns alist: ((v0 . hash0) (v1 . hash1) (v2 . hash2)) or #f if normalization fails.")
(define (hash-expr-all-levels expr)
  (guard (ex [else #f])  ; Skip expressions that fail to normalize
    (list
      (cons 'v0 (hash-sexpr 'benchmark expr))
      (cons 'v1 (hash-sexpr-algebraic 'benchmark expr))
      (cons 'v2 (hash-sexpr-v2 'benchmark expr)))))

(doc 'section 'counting)

(doc count-unique-hashes 'type (-> (List Sexpr) Alist))
(doc count-unique-hashes 'description "Count unique hashes at each normalization level. Returns statistics including reduction percentages between levels.")
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

(doc 'section 'file-benchmarks)

(doc norm-benchmark-file 'type (-> String Alist))
(doc norm-benchmark-file 'description "Benchmark normalization equivalences in a single file")
(define (norm-benchmark-file path)
  (let* ([exprs (extract-exprs-from-file path)]
         [all-subexprs (apply append (map extract-subexprs exprs))]
         [results (count-unique-hashes all-subexprs)])
    (cons `(file . ,path) results)))

(doc norm-benchmark-files 'type (-> (List String) Alist))
(doc norm-benchmark-files 'description "Benchmark multiple files, aggregating results")
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

(doc 'section 'directory-scanning)

(doc find-scheme-files 'type (-> String (List String)))
(doc find-scheme-files 'description "Find all .ss files in a directory (non-recursive)")
(define (find-scheme-files dir)
  (guard (ex [else '()])
    (let ([entries (directory-list dir)])
      (filter
        (lambda (f) (string-suffix? ".ss" f))
        (map (lambda (f) (string-append dir "/" f)) entries)))))

(doc find-scheme-files-recursive 'type (-> String (List String)))
(doc find-scheme-files-recursive 'description "Find all .ss files recursively in a directory tree")
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

(doc string-suffix? 'type (-> String String Bool))
(doc string-suffix? 'description "Check if a string ends with the given suffix")
(define (string-suffix? suffix str)
  (let ([slen (string-length suffix)]
        [len (string-length str)])
    (and (>= len slen)
         (string=? suffix (substring str (- len slen) len)))))

(doc string-prefix? 'type (-> String String Bool))
(doc string-prefix? 'description "Check if a string starts with the given prefix")
(define (string-prefix? prefix str)
  (let ([plen (string-length prefix)]
        [len (string-length str)])
    (and (>= len plen)
         (string=? prefix (substring str 0 plen)))))

(doc norm-benchmark-directory 'type (-> String Alist))
(doc norm-benchmark-directory 'description "Benchmark all .ss files in a directory recursively")
(define (norm-benchmark-directory dir)
  (let* ([files (find-scheme-files-recursive dir)]
         [results (norm-benchmark-files files)])
    (cons `(directory . ,dir) results)))

(doc 'section 'lattice-benchmarks)

(doc norm-benchmark-lattice 'type (-> Alist))
(doc norm-benchmark-lattice 'description "Benchmark the entire lattice directory")
(define (norm-benchmark-lattice)
  (norm-benchmark-directory "lattice"))

(doc norm-benchmark-core 'type (-> Alist))
(doc norm-benchmark-core 'description "Benchmark the core directory")
(define (norm-benchmark-core)
  (norm-benchmark-directory "core"))

(doc norm-benchmark-all 'type (-> Alist))
(doc norm-benchmark-all 'description "Benchmark both core and lattice directories")
(define (norm-benchmark-all)
  (let* ([core-files (find-scheme-files-recursive "core")]
         [lattice-files (find-scheme-files-recursive "lattice")]
         [all-files (append core-files lattice-files)]
         [results (norm-benchmark-files all-files)])
    (cons `(scope . "core+lattice") results)))

(doc 'section 'output)

(doc print-benchmark-results 'type (-> Alist Void))
(doc print-benchmark-results 'description "Pretty-print benchmark results with formatted tables")
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

(doc run-benchmark 'type (-> (Optional String) Void))
(doc run-benchmark 'description "Run benchmark and print results. Optional path argument specifies file or directory to benchmark.")
(define run-benchmark
  (case-lambda
    [() (print-benchmark-results (norm-benchmark-all))]
    [(path)
     (if (file-directory? path)
         (print-benchmark-results (norm-benchmark-directory path))
         (print-benchmark-results (norm-benchmark-file path)))]))
