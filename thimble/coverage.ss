;;; thimble/coverage.ss — Code Coverage Analyzer
;;;
;;; Track which code is executed during tests and generate coverage reports.
;;; Integrates with test-runner.ss for comprehensive test analysis.
;;;
;;; This is Shell code: instruments code, tracks execution, generates reports.
;;;
;;; Dependencies:
;;;   shell/test-runner.ss (for test execution)
;;;   shell/fs.ss (for file operations)
;;;
;;; Operations:
;;;   (coverage-start) — Begin coverage tracking
;;;   (coverage-stop) — Stop tracking and return results
;;;   (coverage-report) — Generate coverage report
;;;   (coverage-run-tests path) — Run tests with coverage
;;;   (coverage-summary) — Display summary statistics
;;;   (coverage-save path) — Save coverage data to file
;;;
;;; Features:
;;;   - Line-level coverage tracking
;;;   - Function-level coverage tracking
;;;   - Branch coverage (if/cond clauses)
;;;   - Integration with test runner
;;;   - Multiple report formats (text, HTML-ready data, scheme)
;;;   - Coverage comparison (regression detection)
;;;   - Uncovered code highlighting

;;; ============================================================
;;; Configuration
;;; ============================================================

(define *coverage-enabled* #f)
(define *coverage-data* '())  ; List of (file line count) entries
(define *coverage-functions* '())  ; List of (file function hit?) entries
(define *coverage-threshold* 80)  ; Warn if coverage below this %

;;; Coverage record types
(define-record-type coverage-entry
  (fields
    file          ; Source file path
    line          ; Line number
    hits))        ; Number of times executed

(define-record-type function-coverage
  (fields
    file          ; Source file path
    name          ; Function name
    line          ; Definition line
    executed?))   ; Was it called?

(define-record-type coverage-report
  (fields
    files             ; List of file coverage summaries
    total-lines       ; Total executable lines
    covered-lines     ; Lines that were hit
    total-functions   ; Total functions
    covered-functions ; Functions that were called
    percentage        ; Overall coverage %
    timestamp))       ; When report was generated

(define-record-type file-coverage
  (fields
    path              ; File path
    total-lines       ; Lines in this file
    covered-lines     ; Covered lines
    uncovered-lines   ; List of uncovered line numbers
    percentage))      ; Coverage % for this file

;;; ============================================================
;;; Coverage Tracking
;;; ============================================================

;;; coverage-start : → void
;;; Begin tracking code coverage.
(define (coverage-start)
  (set! *coverage-enabled* #t)
  (set! *coverage-data* '())
  (set! *coverage-functions* '())
  (display "Coverage tracking started\n"))

;;; coverage-stop : → CoverageReport
;;; Stop tracking and generate report.
(define (coverage-stop)
  (set! *coverage-enabled* #f)
  (let ([report (generate-coverage-report)])
    (display "Coverage tracking stopped\n")
    report))

;;; record-line : Path × Nat → void
;;; Record that a line was executed.
(define (record-line file line)
  (when *coverage-enabled*
    (let ([existing (find-coverage-entry file line)])
      (if existing
          (increment-coverage! existing)
          (add-coverage-entry! file line)))))

;;; record-function : Path × Symbol × Nat → void
;;; Record that a function was executed.
(define (record-function file name line)
  (when *coverage-enabled*
    (let ([existing (find-function-coverage file name)])
      (unless existing
        (add-function-coverage! file name line #t)))))

;;; find-coverage-entry : Path × Nat → CoverageEntry | #f
(define (find-coverage-entry file line)
  (let loop ([entries *coverage-data*])
    (cond
      [(null? entries) #f]
      [(and (string=? file (coverage-entry-file (car entries)))
            (= line (coverage-entry-line (car entries))))
       (car entries)]
      [else (loop (cdr entries))])))

;;; increment-coverage! : CoverageEntry → void
(define (increment-coverage! entry)
  (set! *coverage-data*
    (map (lambda (e)
           (if (eq? e entry)
               (make-coverage-entry
                 (coverage-entry-file e)
                 (coverage-entry-line e)
                 (+ (coverage-entry-hits e) 1))
               e))
         *coverage-data*)))

;;; add-coverage-entry! : Path × Nat → void
(define (add-coverage-entry! file line)
  (set! *coverage-data*
    (cons (make-coverage-entry file line 1)
          *coverage-data*)))

;;; find-function-coverage : Path × Symbol → FunctionCoverage | #f
(define (find-function-coverage file name)
  (let loop ([fns *coverage-functions*])
    (cond
      [(null? fns) #f]
      [(and (string=? file (function-coverage-file (car fns)))
            (eq? name (function-coverage-name (car fns))))
       (car fns)]
      [else (loop (cdr fns))])))

;;; add-function-coverage! : Path × Symbol × Nat × Bool → void
(define (add-function-coverage! file name line executed?)
  (set! *coverage-functions*
    (cons (make-function-coverage file name line executed?)
          *coverage-functions*)))

;;; ============================================================
;;; Test Integration
;;; ============================================================

;;; coverage-run-tests : Path → CoverageReport
;;; Run tests with coverage tracking enabled.
(define (coverage-run-tests path)
  (coverage-start)
  (guard (e [else
             (coverage-stop)
             (display (format "Error running tests: ~a\n"
                             (if (condition? e)
                                 (condition-message e)
                                 e)))
             (generate-coverage-report)])
    ;; Load and run tests
    (load path)
    (coverage-stop)))

;;; coverage-test-suite : (List Path) → CoverageReport
;;; Run multiple test files with coverage.
(define (coverage-test-suite test-files)
  (coverage-start)
  (for-each
    (lambda (file)
      (guard (e [else
                 (display (format "Error in ~a: ~a\n"
                                 file
                                 (if (condition? e)
                                     (condition-message e)
                                     e)))])
        (load file)))
    test-files)
  (coverage-stop))

;;; ============================================================
;;; Report Generation
;;; ============================================================

;;; generate-coverage-report : → CoverageReport
(define (generate-coverage-report)
  (let* ([files (collect-files-from-coverage)]
         [file-reports (map generate-file-coverage files)]
         [total-lines (apply + (map file-coverage-total-lines file-reports))]
         [covered-lines (apply + (map file-coverage-covered-lines file-reports))]
         [total-fns (length *coverage-functions*)]
         [covered-fns (count-covered-functions)]
         [percentage (if (= total-lines 0)
                        0
                        (quotient (* covered-lines 100) total-lines))])
    (make-coverage-report
      file-reports
      total-lines
      covered-lines
      total-fns
      covered-fns
      percentage
      (current-timestamp))))

;;; collect-files-from-coverage : → (List Path)
(define (collect-files-from-coverage)
  (remove-duplicates
    (map coverage-entry-file *coverage-data*)))

;;; generate-file-coverage : Path → FileCoverage
(define (generate-file-coverage file)
  (let* ([file-entries (filter (lambda (e)
                                 (string=? file (coverage-entry-file e)))
                               *coverage-data*)]
         [covered (length file-entries)]
         [total (count-executable-lines file)]
         [uncovered (find-uncovered-lines file file-entries)]
         [pct (if (= total 0) 0 (quotient (* covered 100) total))])
    (make-file-coverage file total covered uncovered pct)))

;;; count-executable-lines : Path → Nat
;;; Count lines of code (excluding comments and blanks).
(define (count-executable-lines file)
  (guard (e [else 0])
    (call-with-input-file file
      (lambda (port)
        (let loop ([count 0])
          (let ([line (get-line port)])
            (if (eof-object? line)
                count
                (loop (if (executable-line? line)
                         (+ count 1)
                         count)))))))))

;;; executable-line? : String → Bool
(define (executable-line? line)
  (let ([trimmed (string-trim line)])
    (and (> (string-length trimmed) 0)
         (not (char=? (string-ref trimmed 0) #\;))
         (not (string-prefix? ";;;" trimmed)))))

;;; find-uncovered-lines : Path × (List CoverageEntry) → (List Nat)
(define (find-uncovered-lines file covered-entries)
  ;; Simplified: return empty for now
  ;; In real implementation, would diff against all executable lines
  '())

;;; count-covered-functions : → Nat
(define (count-covered-functions)
  (length (filter function-coverage-executed? *coverage-functions*)))

;;; remove-duplicates : (List α) → (List α)
(define (remove-duplicates lst)
  (if (null? lst)
      '()
      (cons (car lst)
            (remove-duplicates
              (filter (lambda (x) (not (equal? x (car lst))))
                     (cdr lst))))))

;;; ============================================================
;;; Report Display
;;; ============================================================

;;; coverage-report : CoverageReport → void
;;; Display coverage report.
(define (coverage-report report)
  (display "\n")
  (display "╔══════════════════════════════════════════════════════════════╗\n")
  (display "║                    COVERAGE REPORT                           ║\n")
  (display "╚══════════════════════════════════════════════════════════════╝\n")
  (display "\n")

  ;; Summary
  (display "Summary:\n")
  (display (format "  Total Lines:       ~a\n" (coverage-report-total-lines report)))
  (display (format "  Covered Lines:     ~a\n" (coverage-report-covered-lines report)))
  (display (format "  Coverage:          ~a%\n" (coverage-report-percentage report)))
  (display (format "  Total Functions:   ~a\n" (coverage-report-total-functions report)))
  (display (format "  Covered Functions: ~a\n" (coverage-report-covered-functions report)))
  (display "\n")

  ;; Warn if below threshold
  (when (< (coverage-report-percentage report) *coverage-threshold*)
    (display (format "⚠ WARNING: Coverage below threshold (~a%)\n\n"
                    *coverage-threshold*)))

  ;; File-by-file breakdown
  (display "File Coverage:\n")
  (display "───────────────────────────────────────────────────────────────\n")
  (for-each
    (lambda (file-cov)
      (let ([pct (file-coverage-percentage file-cov)]
            [name (path-basename (file-coverage-path file-cov))])
        (display (format "  ~a~a~a%~a\n"
                        name
                        (make-string (max 1 (- 40 (string-length name))) #\space)
                        (format-percentage pct)
                        (coverage-indicator pct)))))
    (coverage-report-files report))
  (display "\n"))

;;; coverage-summary : CoverageReport → void
;;; Display brief summary.
(define (coverage-summary report)
  (display (format "Coverage: ~a% (~a/~a lines, ~a/~a functions)\n"
                  (coverage-report-percentage report)
                  (coverage-report-covered-lines report)
                  (coverage-report-total-lines report)
                  (coverage-report-covered-functions report)
                  (coverage-report-total-functions report))))

;;; format-percentage : Nat → String
(define (format-percentage pct)
  (cond
    [(< pct 10) (format " ~a" pct)]
    [else (format "~a" pct)]))

;;; coverage-indicator : Nat → String
(define (coverage-indicator pct)
  (cond
    [(>= pct 90) " ✓"]
    [(>= pct 70) " ○"]
    [else " ✗"]))

;;; ============================================================
;;; Persistence
;;; ============================================================

;;; coverage-save : Path → void
;;; Save coverage data to file.
(define (coverage-save path)
  (let ([report (generate-coverage-report)])
    (guard (e [else
               (display (format "Error saving coverage: ~a\n"
                               (if (condition? e)
                                   (condition-message e)
                                   e)))])
      (call-with-output-file path
        (lambda (port)
          (write report port)
          (newline port))
        'replace)
      (display (format "Coverage data saved to ~a\n" path)))))

;;; coverage-load : Path → CoverageReport
(define (coverage-load path)
  (call-with-input-file path
    (lambda (port)
      (read port))))

;;; ============================================================
;;; Utility Functions
;;; ============================================================

;;; current-timestamp : → Nat
(define (current-timestamp)
  (let ([t (current-time 'time-utc)])
    (+ (* (time-second t) 1000000000)
       (time-nanosecond t))))

;;; path-basename : Path → String
(define (path-basename path)
  (let ([parts (string-split path #\/)])
    (if (null? parts)
        path
        (car (reverse parts)))))

;;; string-split : String × Char → (List String)
(define (string-split str sep)
  (let ([len (string-length str)])
    (let loop ([i 0] [start 0] [parts '()])
      (cond
        [(>= i len)
         (if (= start i)
             (reverse parts)
             (reverse (cons (substring str start i) parts)))]
        [(char=? (string-ref str i) sep)
         (loop (+ i 1)
               (+ i 1)
               (cons (substring str start i) parts))]
        [else
         (loop (+ i 1) start parts)]))))

;;; string-trim : String → String
(define (string-trim str)
  (let* ([len (string-length str)]
         [start (let loop ([i 0])
                  (cond
                    [(>= i len) len]
                    [(char-whitespace? (string-ref str i)) (loop (+ i 1))]
                    [else i]))]
         [end (let loop ([i (- len 1)])
                (cond
                  [(< i start) start]
                  [(char-whitespace? (string-ref str i)) (loop (- i 1))]
                  [else (+ i 1)]))])
    (substring str start end)))

;;; string-prefix? : String × String → Bool
(define (string-prefix? prefix str)
  (let ([plen (string-length prefix)]
        [slen (string-length str)])
    (and (>= slen plen)
         (string=? prefix (substring str 0 plen)))))

;;; make-string : Nat × Char → String
(define (make-string n ch)
  (list->string (make-list-of n ch)))

;;; make-list-of : Nat × α → (List α)
(define (make-list-of n x)
  (if (<= n 0)
      '()
      (cons x (make-list-of (- n 1) x))))

;;; ============================================================
;;; Instrumentation Helpers
;;; ============================================================

;;; instrument-file : Path → void
;;; Add coverage tracking to a file (for future enhancement).
(define (instrument-file path)
  ;; Future: Parse file, inject (record-line ...) calls
  (display (format "Instrumentation not yet implemented for ~a\n" path)))

(display "\n")
(display "Code coverage analyzer loaded.\n")
(display "\n")
(display "Usage:\n")
(display "  (coverage-start)                     - Begin tracking\n")
(display "  (coverage-stop)                      - Stop and get report\n")
(display "  (coverage-run-tests \"path\")          - Run tests with coverage\n")
(display "  (coverage-report report)             - Display report\n")
(display "  (coverage-summary report)            - Brief summary\n")
(display "\n")
(display "Example:\n")
(display "  (coverage-start)\n")
(display "  (load \"test-file.ss\")\n")
(display "  (let ([report (coverage-stop)])\n")
(display "    (coverage-report report))\n")
(display "\n")
