(load "core/base/prelude.ss")

(doc 'module 'test-count)
(doc 'description "Test counting utility for lattice skills")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'note "Scans test files for (define-test ...) to count tests per skill")

(doc 'section 'file-scanning)

(define (count-tests-in-file filepath)
  (doc 'description "Count (define-test ...) occurrences in a Scheme file")
  (doc 'param 'filepath "Path to .ss file to scan")
  (doc 'returns "Integer count of tests")
  (doc 'export #t)
  (guard (e [else 0])
    (if (not (file-exists? filepath))
        0
        (call-with-input-file filepath
          (lambda (port)
            (let loop ([count 0])
              (let ([expr (guard (e [else #f]) (read port))])
                (cond
                  [(eof-object? expr) count]
                  [(not expr) count]  ; Read error, return what we have
                  [(and (pair? expr) (eq? (car expr) 'define-test))
                   (loop (+ count 1))]
                  [(and (pair? expr) (eq? (car expr) 'test-group))
                   ;; test-group contains multiple define-test forms
                   (loop (+ count (count-tests-in-expr expr)))]
                  [else (loop count)]))))))))

(define (count-tests-in-expr expr)
  (doc 'description "Count define-test forms within an expression (e.g., test-group)")
  (cond
    [(not (pair? expr)) 0]
    [(eq? (car expr) 'define-test) 1]
    [else
     (fold-left + 0 (map count-tests-in-expr (cdr expr)))]))

(doc 'section 'directory-scanning)

(define (list-test-files dir)
  (doc 'description "List all test-*.ss files in a directory")
  (doc 'export #t)
  (guard (e [else '()])
    (filter (lambda (f)
              (and (string? f)
                   (> (string-length f) 8)
                   (string=? (substring f 0 5) "test-")
                   (string=? (substring f (- (string-length f) 3) (string-length f)) ".ss")))
            (directory-list dir))))

(define (count-tests-in-dir dir)
  (doc 'description "Count all tests in a directory's test-*.ss files")
  (doc 'param 'dir "Path to directory")
  (doc 'returns "(total . ((file . count) ...))")
  (doc 'export #t)
  (let* ([test-files (list-test-files dir)]
         [counts (map (lambda (f)
                        (let ([path (string-append dir "/" f)])
                          (cons f (count-tests-in-file path))))
                      test-files)]
         [total (fold-left + 0 (map cdr counts))])
    (cons total counts)))

(doc 'section 'lattice-summary)

(define (lattice-test-summary)
  (doc 'description "Generate test count summary for all lattice skills")
  (doc 'returns "List of (skill-name total-tests ((file . count) ...))")
  (doc 'export #t)
  (let* ([base "lattice/"]
         [skills (guard (e [else '()])
                   (filter (lambda (d)
                             (and (not (string=? d "."))
                                  (not (string=? d ".."))
                                  (file-directory? (string-append base d))))
                           (directory-list base)))])
    (map (lambda (skill)
           (let* ([dir (string-append base skill)]
                  [result (count-tests-in-dir dir)])
             (list skill (car result) (cdr result))))
         (list-sort string<? skills))))

(define (print-test-summary)
  (doc 'description "Print formatted test summary for all lattice skills")
  (doc 'export #t)
  (let ([summary (lattice-test-summary)]
        [grand-total 0])
    (printf "\n╔══════════════════════════════════════════════════════════╗\n")
    (printf "║           LATTICE TEST COUNT SUMMARY                     ║\n")
    (printf "╚══════════════════════════════════════════════════════════╝\n\n")
    (for-each
      (lambda (entry)
        (let ([skill (car entry)]
              [total (cadr entry)]
              [files (caddr entry)])
          (when (> total 0)
            (printf "~a: ~a tests\n" skill total)
            (set! grand-total (+ grand-total total))
            (for-each
              (lambda (fc)
                (printf "  ~a: ~a\n" (car fc) (cdr fc)))
              (list-sort (lambda (a b) (> (cdr a) (cdr b))) files))
            (printf "\n"))))
      summary)
    (printf "────────────────────────────────────────────────────────────\n")
    (printf "GRAND TOTAL: ~a tests\n" grand-total)
    grand-total))

(define (test-count skill-name)
  (doc 'description "Get test count for a specific skill")
  (doc 'param 'skill-name "Skill name as string or symbol")
  (doc 'returns "(total . ((file . count) ...))")
  (doc 'export #t)
  (let ([name (if (symbol? skill-name)
                  (symbol->string skill-name)
                  skill-name)])
    (count-tests-in-dir (string-append "lattice/" name))))

(doc 'section 'repl-interface)

(printf "test-count.ss loaded — Test Counting Utility\n")
(printf "  (print-test-summary)    - Print full summary for all lattice skills\n")
(printf "  (test-count 'skill)     - Get counts for specific skill\n")
(printf "  (lattice-test-summary)  - Return summary as list\n")
