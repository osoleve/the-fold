;;; thimble/introspect/complexity.ss — Codebase Complexity and Coverage Analysis
;;;
;;; Analyzes Scheme source files for complexity metrics and test coverage.
;;; This is Shell code: reads filesystem, performs static analysis.
;;;
;;; Capabilities:
;;;   Requires FS capability to read source files.
;;;
;;; Operations:
;;;   (analyze-file fs path) → FileAnalysis
;;;   (analyze-directory fs path) → DirectoryAnalysis
;;;   (compute-coverage test-files source-files) → CoverageReport
;;;   (complexity-report fs paths) → ComplexityReport
;;;
;;; Metrics computed:
;;;   - Lines of code (LOC)
;;;   - Lines of comments
;;;   - Number of definitions (define, define-record-type, etc.)
;;;   - Nesting depth
;;;   - Definition complexity (based on form size)
;;;   - Test coverage (definitions exercised by tests)

;;; ============================================================
;;; File Metrics
;;; ============================================================

(define-record-type file-metrics
  (fields
   path              ; File path
   total-lines       ; Total line count
   code-lines        ; Non-blank, non-comment lines
   comment-lines     ; Lines that are comments
   blank-lines       ; Empty lines
   definitions       ; List of definition-info records
   max-nesting       ; Maximum parenthesis nesting depth
   load-deps))       ; List of loaded dependencies

(define-record-type definition-info
  (fields
   name              ; Symbol name of the definition
   type              ; 'define, 'define-record-type, 'define-syntax, etc.
   line-number       ; Starting line
   form-size         ; Number of S-expression nodes
   nesting-depth     ; Maximum nesting within definition
   references))      ; Other definitions this references (list of symbols)

;;; ============================================================
;;; Parsing Utilities
;;; ============================================================

;;; read-file-lines : String → (List String)
;;; Read all lines from a file.
(define (read-file-lines path)
  (call-with-input-file path
                        (lambda (port)
                                (let loop ([lines '()])
                                     (let ([line (get-line port)])
                                          (if (eof-object? line)
                                              (reverse lines)
                                              (loop (cons line lines))))))))

;;; line-type : String → Symbol
;;; Classify a line as 'blank, 'comment, or 'code.
(define (line-type line)
  (let ([trimmed (string-trim line)])
       (cond
        [(= (string-length trimmed) 0) 'blank]
        [(char=? (string-ref trimmed 0) #\;) 'comment]
        [else 'code])))

;;; string-trim : String → String
;;; Remove leading and trailing whitespace.
(define (string-trim str)
  (let* ([len (string-length str)]
         [start (let loop ([i 0])
                     (if (or (>= i len)
                             (not (char-whitespace? (string-ref str i))))
                         i
                         (loop (+ i 1))))]
         [end (let loop ([i len])
                   (if (or (<= i start)
                           (not (char-whitespace? (string-ref str (- i 1)))))
                       i
                       (loop (- i 1))))])
        (if (>= start end)
            ""
            (substring str start end))))

;;; ============================================================
;;; S-Expression Analysis
;;; ============================================================

;;; read-all-sexps : String → (List Sexp)
;;; Read all S-expressions from a file.
(define (read-all-sexps path)
  (call-with-input-file path
                        (lambda (port)
                                (let loop ([sexps '()])
                                     (let ([sexp (read port)])
                                          (if (eof-object? sexp)
                                              (reverse sexps)
                                              (loop (cons sexp sexps))))))))

;;; sexp-size : Sexp → Nat
;;; Count the number of nodes in an S-expression tree.
(define (sexp-size sexp)
  (cond
   [(pair? sexp)
    (+ 1 (sexp-size (car sexp)) (sexp-size (cdr sexp)))]
   [(null? sexp) 0]
   [else 1]))

;;; sexp-depth : Sexp → Nat
;;; Compute maximum nesting depth.
(define (sexp-depth sexp)
  (cond
   [(pair? sexp)
    (+ 1 (max (sexp-depth (car sexp)) (sexp-depth (cdr sexp))))]
   [else 0]))

;;; extract-references : Sexp → (List Symbol)
;;; Extract all symbols referenced in an S-expression.
(define (extract-references sexp)
  (cond
   [(symbol? sexp) (list sexp)]
   [(pair? sexp)
    (append (extract-references (car sexp))
            (extract-references (cdr sexp)))]
   [else '()]))

;;; unique : (List A) → (List A)
;;; Remove duplicates from a list.
(define (unique lst)
  (let loop ([lst lst] [seen '()] [acc '()])
       (cond
        [(null? lst) (reverse acc)]
        [(member (car lst) seen) (loop (cdr lst) seen acc)]
        [else (loop (cdr lst) (cons (car lst) seen) (cons (car lst) acc))])))

;;; ============================================================
;;; Definition Extraction
;;; ============================================================

;;; definition? : Sexp → Boolean
;;; Check if S-expression is a definition form.
(define (definition? sexp)
  (and (pair? sexp)
       (symbol? (car sexp))
       (memq (car sexp) '(define define-record-type define-syntax
                          define-condition-type))))

;;; extract-definition-name : Sexp → Symbol | #f
;;; Extract the name being defined.
(define (extract-definition-name sexp)
  (cond
   [(not (pair? sexp)) #f]
   [(not (pair? (cdr sexp))) #f]
   [else
    (let ([form (car sexp)]
          [name-part (cadr sexp)])
         (cond
          ;; (define name value) or (define (name args...) body)
          [(eq? form 'define)
           (if (pair? name-part)
               (car name-part)  ; (define (name ...) ...)
               name-part)]      ; (define name ...)
          ;; (define-record-type name ...)
          [(eq? form 'define-record-type)
           name-part]
          ;; (define-syntax name ...)
          [(eq? form 'define-syntax)
           name-part]
          [else #f]))]))

;;; extract-definition-type : Sexp → Symbol
(define (extract-definition-type sexp)
  (if (pair? sexp) (car sexp) 'unknown))

;;; analyze-definition : Sexp × Nat → DefinitionInfo
(define (analyze-definition sexp line-number)
  (make-definition-info
   (extract-definition-name sexp)
   (extract-definition-type sexp)
   line-number
   (sexp-size sexp)
   (sexp-depth sexp)
   (unique (extract-references sexp))))

;;; ============================================================
;;; Load Dependency Extraction
;;; ============================================================

;;; load-form? : Sexp → Boolean
(define (load-form? sexp)
  (and (pair? sexp)
       (eq? (car sexp) 'load)
       (pair? (cdr sexp))
       (string? (cadr sexp))))

;;; extract-load-path : Sexp → String | #f
(define (extract-load-path sexp)
  (if (load-form? sexp)
      (cadr sexp)
      #f))

;;; ============================================================
;;; File Analysis
;;; ============================================================

;;; analyze-file : String → FileMetrics
;;; Analyze a single Scheme source file.
(define (analyze-file path)
  (let* ([lines (read-file-lines path)]
         [line-types (map line-type lines)]
         [total (length lines)]
         [code-count (length (filter (lambda (t) (eq? t 'code)) line-types))]
         [comment-count (length (filter (lambda (t) (eq? t 'comment)) line-types))]
         [blank-count (length (filter (lambda (t) (eq? t 'blank)) line-types))]
         [sexps (guard (ex [else '()])  ; Handle parse errors gracefully
                       (read-all-sexps path))]
         [definitions (filter-map
                       (lambda (sexp)
                               (and (definition? sexp)
                                    (analyze-definition sexp 0)))  ; TODO: track line numbers
                       sexps)]
         [loads (filter-map extract-load-path sexps)]
         [max-nest (if (null? sexps)
                       0
                       (apply max (map sexp-depth sexps)))])
        (make-file-metrics
         path
         total
         code-count
         comment-count
         blank-count
         definitions
         max-nest
         loads)))

;;; filter-map : (A → B | #f) × (List A) → (List B)
(define (filter-map f lst)
  (let loop ([lst lst] [acc '()])
       (if (null? lst)
           (reverse acc)
           (let ([result (f (car lst))])
                (if result
                    (loop (cdr lst) (cons result acc))
                    (loop (cdr lst) acc))))))

;;; ============================================================
;;; Directory Analysis
;;; ============================================================

(define-record-type directory-metrics
  (fields
   path              ; Directory path
   file-count        ; Number of .ss files
   total-lines       ; Total lines across all files
   total-code        ; Total code lines
   total-comments    ; Total comment lines
   total-definitions ; Total definition count
   files))           ; List of FileMetrics

;;; scheme-file? : String → Boolean
(define (scheme-file? path)
  (let ([len (string-length path)])
       (and (> len 3)
            (string=? (substring path (- len 3) len) ".ss"))))

;;; analyze-directory : String → DirectoryMetrics
;;; Analyze all Scheme files in a directory (non-recursive).
(define (analyze-directory path)
  (let* ([entries (directory-list path)]
         [ss-files (filter scheme-file? entries)]
         [full-paths (map (lambda (f) (string-append path "/" f)) ss-files)]
         [file-analyses (map analyze-file full-paths)]
         [total-lines (fold-left + 0 (map file-metrics-total-lines file-analyses))]
         [total-code (fold-left + 0 (map file-metrics-code-lines file-analyses))]
         [total-comments (fold-left + 0 (map file-metrics-comment-lines file-analyses))]
         [total-defs (fold-left + 0
                                (map (lambda (fm) (length (file-metrics-definitions fm)))
                                     file-analyses))])
        (make-directory-metrics
         path
         (length ss-files)
         total-lines
         total-code
         total-comments
         total-defs
         file-analyses)))

;;; ============================================================
;;; Coverage Analysis
;;; ============================================================

(define-record-type coverage-report
  (fields
   source-definitions   ; List of symbols defined in sources
   tested-definitions   ; List of symbols referenced in tests
   covered              ; Intersection (actually tested)
   uncovered            ; Defined but not tested
   coverage-ratio))     ; 0.0 to 1.0

;;; compute-coverage : (List FileMetrics) × (List FileMetrics) → CoverageReport
;;; Compare test files against source files to determine coverage.
(define (compute-coverage test-analyses source-analyses)
  (let* ([source-defs (unique
                       (apply append
                              (map (lambda (fm)
                                           (map definition-info-name
                                                (file-metrics-definitions fm)))
                                   source-analyses)))]
         [test-refs (unique
                     (apply append
                            (map (lambda (fm)
                                         (apply append
                                                (map definition-info-references
                                                     (file-metrics-definitions fm))))
                                 test-analyses)))]
         [covered (filter (lambda (d) (member d test-refs)) source-defs)]
         [uncovered (filter (lambda (d) (not (member d test-refs))) source-defs)]
         [ratio (if (null? source-defs)
                    1.0
                    (/ (length covered) (length source-defs)))])
        (make-coverage-report
         source-defs
         test-refs
         covered
         uncovered
         (inexact ratio))))

;;; ============================================================
;;; Complexity Scoring
;;; ============================================================

;;; complexity-score : DefinitionInfo → Nat
;;; Compute a complexity score for a definition.
;;; Higher = more complex.
(define (complexity-score def)
  (let ([size (definition-info-form-size def)]
        [depth (definition-info-nesting-depth def)]
        [refs (length (definition-info-references def))])
       (+ (* size 1)           ; Each node contributes 1
          (* depth 5)          ; Deep nesting is concerning
          (* refs 2))))        ; Many references add complexity

;;; high-complexity? : DefinitionInfo × Nat → Boolean
;;; Check if definition exceeds complexity threshold.
(define (high-complexity? def threshold)
  (> (complexity-score def) threshold))

;;; ============================================================
;;; Reporting
;;; ============================================================

;;; format-file-metrics : FileMetrics → String
(define (format-file-metrics fm)
  (format "~a:\n  Lines: ~a (code: ~a, comments: ~a, blank: ~a)\n  Definitions: ~a\n  Max nesting: ~a\n  Dependencies: ~a"
          (file-metrics-path fm)
          (file-metrics-total-lines fm)
          (file-metrics-code-lines fm)
          (file-metrics-comment-lines fm)
          (file-metrics-blank-lines fm)
          (length (file-metrics-definitions fm))
          (file-metrics-max-nesting fm)
          (length (file-metrics-load-deps fm))))

;;; format-directory-metrics : DirectoryMetrics → String
(define (format-directory-metrics dm)
  (format "~a/:\n  Files: ~a\n  Total lines: ~a (code: ~a, comments: ~a)\n  Definitions: ~a"
          (directory-metrics-path dm)
          (directory-metrics-file-count dm)
          (directory-metrics-total-lines dm)
          (directory-metrics-total-code dm)
          (directory-metrics-total-comments dm)
          (directory-metrics-total-definitions dm)))

;;; format-coverage : CoverageReport → String
(define (format-coverage cr)
  (format "Coverage: ~,1f% (~a/~a definitions)\n  Uncovered: ~a"
          (* (coverage-report-coverage-ratio cr) 100)
          (length (coverage-report-covered cr))
          (length (coverage-report-source-definitions cr))
          (if (null? (coverage-report-uncovered cr))
              "(none)"
              (apply string-append
                     (map (lambda (s) (format " ~a" s))
                          (coverage-report-uncovered cr))))))

;;; ============================================================
;;; Display Utilities
;;; ============================================================

;;; print-file-metrics : FileMetrics → void
(define (print-file-metrics fm)
  (display (format-file-metrics fm))
  (newline))

;;; print-directory-metrics : DirectoryMetrics → void
(define (print-directory-metrics dm)
  (display (format-directory-metrics dm))
  (newline))

;;; print-coverage : CoverageReport → void
(define (print-coverage cr)
  (display (format-coverage cr))
  (newline))

;;; print-complexity-warnings : (List FileMetrics) × Nat → void
;;; Print definitions that exceed the complexity threshold.
(define (print-complexity-warnings file-analyses threshold)
  (display (format "Definitions exceeding complexity threshold (~a):\n" threshold))
  (for-each
   (lambda (fm)
           (for-each
            (lambda (def)
                    (when (high-complexity? def threshold)
                          (display (format "  ~a:~a - ~a (score: ~a)\n"
                                           (file-metrics-path fm)
                                           (definition-info-line-number def)
                                           (definition-info-name def)
                                           (complexity-score def)))))
            (file-metrics-definitions fm)))
   file-analyses))

;;; ============================================================
;;; Full Codebase Report
;;; ============================================================

(define-record-type codebase-report
  (fields
   directories       ; List of DirectoryMetrics
   total-files       ; Total file count
   total-lines       ; Total line count
   total-definitions ; Total definitions
   coverage          ; CoverageReport (if tests analyzed)
   high-complexity)) ; List of high-complexity definitions

;;; analyze-codebase : (List String) × (List String) × Nat → CodebaseReport
;;; Full analysis of source and test directories.
(define (analyze-codebase source-dirs test-dirs complexity-threshold)
  (let* ([source-analyses (map analyze-directory source-dirs)]
         [test-analyses (map analyze-directory test-dirs)]
         [all-source-files (apply append (map directory-metrics-files source-analyses))]
         [all-test-files (apply append (map directory-metrics-files test-analyses))]
         [coverage (compute-coverage all-test-files all-source-files)]
         [all-defs (apply append
                          (map (lambda (fm) (file-metrics-definitions fm))
                               all-source-files))]
         [complex-defs (filter (lambda (d) (high-complexity? d complexity-threshold))
                               all-defs)])
        (make-codebase-report
         source-analyses
         (fold-left + 0 (map directory-metrics-file-count source-analyses))
         (fold-left + 0 (map directory-metrics-total-lines source-analyses))
         (fold-left + 0 (map directory-metrics-total-definitions source-analyses))
         coverage
         complex-defs)))

;;; print-codebase-report : CodebaseReport → void
(define (print-codebase-report report)
  (display "=== Codebase Analysis Report ===\n\n")
  (display (format "Total files: ~a\n" (codebase-report-total-files report)))
  (display (format "Total lines: ~a\n" (codebase-report-total-lines report)))
  (display (format "Total definitions: ~a\n\n" (codebase-report-total-definitions report)))
  
  (display "--- By Directory ---\n")
  (for-each
   (lambda (dm)
           (print-directory-metrics dm)
           (newline))
   (codebase-report-directories report))
  
  (display "--- Coverage ---\n")
  (print-coverage (codebase-report-coverage report))
  (newline)
  
  (let ([complex (codebase-report-high-complexity report)])
       (when (not (null? complex))
             (display (format "--- High Complexity (~a definitions) ---\n" (length complex)))
             (for-each
              (lambda (def)
                      (display (format "  ~a: ~a (score: ~a)\n"
                                       (definition-info-name def)
                                       (definition-info-type def)
                                       (complexity-score def))))
              complex))))
