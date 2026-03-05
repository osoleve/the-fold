;;; boundary/meta/lattice-walk-io.ss — I/O driver for lattice analysis walker
;;; @module lattice-walk-io
;;; @requires file-io meta/lattice-walk meta/analyzer-exports meta/analyzer-effects meta/analyzer-unused meta/analyzer-patterns meta/analyzer-coverage meta/analyzer-fuel

(unless (top-level-bound? 'read-all-sexps)
  (load "boundary/meta/file-io.ss"))
(unless (top-level-bound? 'make-module-record)
  (load "lattice/meta/lattice-walk.ss"))
(unless (top-level-bound? 'make-export-analyzer)
  (load "lattice/meta/analyzer-exports.ss"))
(unless (top-level-bound? 'make-effect-analyzer)
  (load "lattice/meta/analyzer-effects.ss"))
(unless (top-level-bound? 'make-unused-requires-analyzer)
  (load "lattice/meta/analyzer-unused.ss"))
(unless (top-level-bound? 'make-pattern-analyzer)
  (load "lattice/meta/analyzer-patterns.ss"))
(unless (top-level-bound? 'make-coverage-analyzer)
  (load "lattice/meta/analyzer-coverage.ss"))
(unless (top-level-bound? 'make-fuel-analyzer)
  (load "lattice/meta/analyzer-fuel.ss"))

(doc 'module 'lattice-walk-io)
(doc 'description "I/O driver for the lattice analysis walker. Reads source files,
builds ModuleRecords, and runs the pure topsort walker with all analyzers.")
(doc 'layer 'boundary)

;;; ============================================================================
;;; Module Discovery
;;; ============================================================================

;;; build-module-records : -> (List ModuleRecord)
;;; Read all registered lattice modules, parse their headers, and create records.
;;; Skips test files and non-lattice modules.
(define (build-module-records)
  (let* ([names (vector->list (hashtable-keys *module-paths*))]
         [records '()])
    (for-each
     (lambda (name)
       (let ([path (hashtable-ref *module-paths* name #f)])
         (when (and path
                    (>= (string-length path) 8)
                    (string=? "lattice/" (substring path 0 8))
                    (not (test-file? path))
                    (file-exists? path))
           (guard (e [else #f])
             (let* ([sexps (read-all-sexps path)]
                    [header (parse-module-header path)]
                    [deps (if header (cdr header) '())]
                    [fp (file-fingerprint path)])
               (when (pair? sexps)  ; skip empty files
                 (set! records
                       (cons (make-module-record name path deps sexps fp)
                             records))))))))
     names)
    records))

;;; test-file? : String -> Boolean
(define (test-file? path)
  (let ([base (path-basename path)])
    (and (>= (string-length base) 5)
         (string=? "test-" (substring base 0 5)))))

;;; path-basename : String -> String
(define (path-basename path)
  (let loop ([i (- (string-length path) 1)])
    (cond
      [(< i 0) path]
      [(char=? (string-ref path i) #\/)
       (substring path (+ i 1) (string-length path))]
      [else (loop (- i 1))])))

;;; file-fingerprint : String -> String
;;; Simple content hash for cache invalidation.
(define (file-fingerprint path)
  (guard (e [else ""])
    (let ([content (call-with-input-file path get-string-all)])
      (number->string (string-hash content)))))

;;; string-hash : String -> Integer
;;; FNV-1a-inspired hash for strings.
(define (string-hash str)
  (let ([len (string-length str)])
    (let loop ([i 0] [h 2166136261])
      (if (>= i len)
          (bitwise-and h #xFFFFFFFF)
          (loop (+ i 1)
                (bitwise-and
                 (* (bitwise-xor h (char->integer (string-ref str i)))
                    16777619)
                 #xFFFFFFFF))))))

;;; ============================================================================
;;; Analysis Runner
;;; ============================================================================

;;; run-lattice-analysis : -> WalkResult
;;; Main entry point: build records, configure all analyzers, run the walk.
(define (run-lattice-analysis)
  (let* ([records (build-module-records)]
         [analyzers (list (make-export-analyzer)
                          (make-effect-analyzer)
                          (make-unused-requires-analyzer)
                          (make-pattern-analyzer)
                          (make-coverage-analyzer)
                          (make-fuel-analyzer))]
         [result (lattice-walk records analyzers)])
    result))

;;; ============================================================================
;;; Report Formatting
;;; ============================================================================

;;; print-analysis-report : WalkResult -> Void
(define (print-analysis-report result)
  (display "\n")
  (display "  Lattice Analysis Report\n")
  (display "  ============================================\n\n")
  (cond
    [(not (walk-ok? result))
     (printf "  ERROR: ~a\n" (cdr (assq 'error result)))
     (printf "  Cycle: ~a\n" (cdr (assq 'cycle result)))]
    [else
     (let* ([state (walk-state result)]
            [n-modules (cdr (assq 'modules-processed result))])
       (printf "  Modules processed: ~a\n\n" n-modules)

       ;; Effect summary
       (print-effect-summary (acc-ref state 'effects))

       ;; Unused-requires summary
       (print-unused-summary (acc-ref state 'unused))

       ;; Export summary
       (print-export-summary (acc-ref state 'exports))

       ;; Pattern lint summary
       (print-pattern-summary (acc-ref state 'patterns))

       ;; Export utilization summary
       (print-coverage-summary (acc-ref state 'coverage)
                               (acc-ref state 'exports))

       ;; Fuel cost summary
       (print-fuel-summary (acc-ref state 'fuel)))])
  (display "\n"))

;;; print-effect-summary : Hashtable -> Void
(define (print-effect-summary db)
  (display "  --- Effect Analysis ---\n")
  (let* ([keys (vector->list (hashtable-keys db))]
         ;; Filter to only module-defined functions (skip primitives)
         ;; by counting entries by effect
         [effects (map (lambda (k) (hashtable-ref db k 'unknown)) keys)]
         [pure-count (length (filter (lambda (e) (eq? e 'pure)) effects))]
         [io-count (length (filter (lambda (e) (eq? e 'io)) effects))]
         [mutation-count (length (filter (lambda (e) (eq? e 'mutation)) effects))]
         [exception-count (length (filter (lambda (e) (eq? e 'exception)) effects))]
         [unknown-count (length (filter (lambda (e) (eq? e 'unknown)) effects))])
    (printf "  Total symbols: ~a\n" (length keys))
    (printf "  Pure: ~a  IO: ~a  Mutation: ~a  Exception: ~a  Unknown: ~a\n\n"
            pure-count io-count mutation-count exception-count unknown-count)))

;;; print-unused-summary : Alist -> Void
(define (print-unused-summary unused-db)
  (display "  --- Unused Requires ---\n")
  (let* ([all-unused '()]
         [all-uncertain '()]
         [total-modules 0])
    (for-each
     (lambda (entry)
       (let* ([name (car entry)]
              [result (cdr entry)]
              [unused (analysis-unused result)]
              [uncertain (analysis-uncertain result)])
         (set! total-modules (+ total-modules 1))
         (when (pair? unused)
           (set! all-unused (cons (cons name unused) all-unused)))
         (when (pair? uncertain)
           (set! all-uncertain (cons (cons name uncertain) all-uncertain)))))
     unused-db)
    (printf "  Modules analyzed: ~a\n" total-modules)
    (printf "  Modules with unused requires: ~a\n" (length all-unused))
    (printf "  Modules with uncertain requires: ~a\n" (length all-uncertain))
    (when (pair? all-unused)
      (display "\n  Unused:\n")
      (for-each
       (lambda (entry)
         (printf "    ~a: ~a\n" (car entry) (cdr entry)))
       (list-sort (lambda (a b) (string<? (symbol->string (car a))
                                           (symbol->string (car b))))
                  all-unused)))
    (display "\n")))

;;; print-export-summary : Alist -> Void
(define (print-export-summary export-db)
  (display "  --- Export Collection ---\n")
  (let* ([total (length export-db)]
         [with-exports (length (filter (lambda (e)
                                         (and (pair? (cdr e))
                                              (eq? (cadr e) 'known)
                                              (pair? (caddr e))))
                                       export-db))]
         [empty (- total with-exports)])
    (printf "  Modules indexed: ~a (with exports: ~a, empty: ~a)\n\n"
            total with-exports empty)))

;;; print-pattern-summary : Alist -> Void
(define (print-pattern-summary pattern-db)
  (display "  --- Pattern Lint ---\n")
  (if (null? pattern-db)
      (display "  No issues found.\n\n")
      (let* ([total-incomplete 0]
             [total-dups 0])
        (for-each
         (lambda (entry)
           (set! total-incomplete
                 (+ total-incomplete
                    (cdr (assq 'cases-without-else (cdr entry)))))
           (set! total-dups
                 (+ total-dups
                    (cdr (assq 'duplicate-datums (cdr entry))))))
         pattern-db)
        (printf "  Modules with findings: ~a\n" (length pattern-db))
        (printf "  Case expressions without else: ~a\n" total-incomplete)
        (printf "  Case expressions with duplicate datums: ~a\n" total-dups)
        (when (pair? pattern-db)
          (display "\n  Details:\n")
          (for-each
           (lambda (entry)
             (let* ([name (car entry)]
                    [data (cdr entry)]
                    [n-cases (cdr (assq 'cases data))]
                    [n-incomplete (cdr (assq 'cases-without-else data))]
                    [n-dups (cdr (assq 'duplicate-datums data))])
               (printf "    ~a: ~a case(s), ~a without else, ~a with dups\n"
                       name n-cases n-incomplete n-dups)))
           (list-sort (lambda (a b) (string<? (symbol->string (car a))
                                               (symbol->string (car b))))
                      pattern-db)))
        (display "\n"))))

;;; has-export-annotations? : ExportDB Symbol -> Boolean
;;; Check if a module has explicit doc 'export annotations (vs fallback to all defines).
;;; Heuristic: modules with >30 "exports" likely used fallback.
;;; More precise: check if the exports are a subset of annotated symbols.
;;; For the report, we use a simpler proxy: modules whose exports exactly equal
;;; their top-level defines are fallback; those with fewer are annotated.
(define (leaf-module? mod-name coverage-db)
  (not (hashtable-ref coverage-db mod-name #f)))

;;; print-coverage-summary : Hashtable Alist -> Void
(define (print-coverage-summary coverage-db export-db)
  (display "  --- Export Utilization ---\n")
  (if (or (not coverage-db) (not export-db))
      (display "  No data available.\n\n")
      (let* ([all-modules (map car export-db)]
             ;; Classify modules
             [n-leaf 0] [n-nonleaf 0]
             [leaf-exports 0] [nonleaf-exports 0]
             [nonleaf-used 0]
             [dead-nonleaf '()])
        (for-each
         (lambda (mod-name)
           (let* ([exports (export-db-symbols export-db mod-name)]
                  [n-exports (length exports)]
                  [stats (coverage-module-stats coverage-db export-db mod-name)]
                  [n-used (car stats)]
                  [is-leaf (leaf-module? mod-name coverage-db)])
             (if is-leaf
                 (begin
                   (set! n-leaf (+ n-leaf 1))
                   (set! leaf-exports (+ leaf-exports n-exports)))
                 (begin
                   (set! n-nonleaf (+ n-nonleaf 1))
                   (set! nonleaf-exports (+ nonleaf-exports n-exports))
                   (set! nonleaf-used (+ nonleaf-used n-used))
                   (when (and (> n-exports 0) (zero? n-used))
                     (set! dead-nonleaf (cons mod-name dead-nonleaf)))))))
         all-modules)
        (let ([total-exports (+ leaf-exports nonleaf-exports)])
          (printf "  Total exports: ~a (~a from ~a non-leaf, ~a from ~a leaf)\n"
                  total-exports nonleaf-exports n-nonleaf leaf-exports n-leaf)
          (printf "  Non-leaf exports referenced by downstream: ~a\n" nonleaf-used)
          (when (> nonleaf-exports 0)
            (printf "  Non-leaf utilization: ~a%\n"
                    (quotient (* nonleaf-used 100) nonleaf-exports)))
          (printf "  Non-leaf modules with zero usage: ~a\n"
                  (length dead-nonleaf))
          (display "  (Leaf modules excluded — consumed by boundary/tests/REPL)\n")
          (display "\n")))))

;;; print-fuel-summary : Hashtable -> Void
(define (print-fuel-summary fuel-db)
  (display "  --- Fuel Cost Analysis ---\n")
  (if (not fuel-db)
      (display "  No data available.\n\n")
      (let* ([keys (vector->list (hashtable-keys fuel-db))]
             [costs (map (lambda (k) (hashtable-ref fuel-db k 'unknown)) keys)]
             [numeric (filter integer? costs)]
             [n-recursive (length (filter (lambda (c) (eq? c 'recursive)) costs))]
             [n-unknown (length (filter (lambda (c) (eq? c 'unknown)) costs))]
             [n-numeric (length numeric)])
        (printf "  Total symbols: ~a\n" (length keys))
        (printf "  Deterministic: ~a  Recursive: ~a  Unknown: ~a\n"
                n-numeric n-recursive n-unknown)
        (when (pair? numeric)
          (let* ([sorted (list-sort > numeric)]
                 [median-idx (quotient (length sorted) 2)])
            (printf "  Cost range: ~a — ~a (median: ~a)\n"
                    (car (reverse sorted))
                    (car sorted)
                    (list-ref sorted median-idx))))
        ;; Show top-10 most expensive deterministic functions
        (let ([hot (fuel-db-hot-functions fuel-db 0)])
          (when (pair? hot)
            (let ([top (if (> (length hot) 10) (list-head hot 10) hot)])
              (display "\n  Top costly functions:\n")
              (for-each
               (lambda (entry)
                 (printf "    ~a: ~a fuel\n" (car entry) (cdr entry)))
               top))))
        (display "\n"))))

;;; ============================================================================
;;; REPL Convenience
;;; ============================================================================

;;; la! : -> Void
;;; Run full lattice analysis and print report.
(define (la!)
  (let ([start (current-time)])
    (let ([result (run-lattice-analysis)])
      (let* ([end (current-time)]
             [ms (- (* (time-second end) 1000)
                    (* (time-second start) 1000))])
        (print-analysis-report result)
        (printf "  Completed in ~ams.\n\n" ms)
        result))))
