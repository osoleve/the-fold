(doc 'module 'boundary/introspect/complexity)
(doc 'description "Codebase complexity and coverage analysis for Scheme source files")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'dependencies '(core/base/prelude))

(doc 'note "String utilities provided by core/prelude.ss")
(load "core/base/prelude.ss")

(doc 'section 'file-metrics)

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

(doc 'section 'parsing-utilities)

(define (read-file-lines path)
  (doc 'type '(-> String (List String)))
  (doc 'description "Read all lines from a file")
  (doc 'export #t)
  (call-with-input-file path
                        (lambda (port)
                                (let loop ([lines '()])
                                     (let ([line (get-line port)])
                                          (if (eof-object? line)
                                              (reverse lines)
                                              (loop (cons line lines))))))))

(define (line-type line)
  (doc 'type '(-> String Symbol))
  (doc 'description "Classify a line as 'blank, 'comment, or 'code")
  (doc 'export #t)
  (let ([trimmed (string-trim line)])
       (cond
        [(= (string-length trimmed) 0) 'blank]
        [(char=? (string-ref trimmed 0) #\;) 'comment]
        [else 'code])))

(define (string-count-char str ch)
  (doc 'type '(-> String Char Nat))
  (doc 'description "Count occurrences of a character in a string")
  (doc 'export #t)
  (let ([len (string-length str)])
       (let loop ([i 0] [count 0])
            (if (>= i len)
                count
                (loop (+ i 1)
                      (if (char=? (string-ref str i) ch)
                          (+ count 1)
                          count))))))


(doc 'section 's-expression-analysis)

(define (read-all-sexps path)
  (doc 'type '(-> String (List Sexp)))
  (doc 'description "Read all S-expressions from a file")
  (doc 'export #t)
  (call-with-input-file path
                        (lambda (port)
                                (let loop ([sexps '()])
                                     (let ([sexp (read port)])
                                          (if (eof-object? sexp)
                                              (reverse sexps)
                                              (loop (cons sexp sexps))))))))

(define (read-all-sexps-with-lines path)
  (doc 'type '(-> String (List (Cons Nat Sexp))))
  (doc 'description "Read all S-expressions from a file with their starting line numbers")
  (doc 'note "Returns list of (line-number . sexp) pairs")
  (doc 'export #t)
  (let* ([lines (read-file-lines path)]
         [form-start-lines (find-form-start-lines lines)])
        (call-with-input-file path
                              (lambda (port)
                                      (let loop ([results '()] [line-nums form-start-lines])
                                           (let ([sexp (read port)])
                                                (if (eof-object? sexp)
                                                    (reverse results)
                                                    (loop (cons (cons (if (null? line-nums) 0 (car line-nums)) sexp)
                                                                results)
                                                          (if (null? line-nums) '() (cdr line-nums))))))))))

(define (find-form-start-lines lines)
  (doc 'type '(-> (List String) (List Nat)))
  (doc 'description "Find line numbers where top-level forms start")
  (doc 'note "A top-level form starts when we're at depth 0 and see '('")
  (doc 'export #t)
  (let loop ([lines lines] [line-num 1] [depth 0] [results '()])
       (if (null? lines)
           (reverse results)
           (let* ([line (car lines)]
                  [trimmed (string-trim line)]
                  [starts-form? (and (= depth 0)
                                     (> (string-length trimmed) 0)
                                     (char=? (string-ref trimmed 0) #\())]
                  [new-depth (+ depth (count-paren-delta line))])
                 (loop (cdr lines)
                       (+ line-num 1)
                       new-depth
                       (if starts-form?
                           (cons line-num results)
                           results))))))

(define (count-paren-delta line)
  (doc 'type '(-> String Int))
  (doc 'description "Count net change in paren depth for a line")
  (doc 'note "Positive = more opens than closes, negative = more closes. Skips content after semicolon (comments) and inside strings")
  (doc 'export #t)
  (let ([len (string-length line)])
       (let loop ([i 0] [delta 0] [in-string? #f] [escaped? #f])
            (if (>= i len)
                delta
                (let ([ch (string-ref line i)])
                     (cond
                      [escaped?
                       (loop (+ i 1) delta in-string? #f)]
                      [(char=? ch #\\)
                       (loop (+ i 1) delta in-string? #t)]
                      [(char=? ch #\")
                       (loop (+ i 1) delta (not in-string?) #f)]
                      [(and (not in-string?) (char=? ch #\;))
                       delta]
                      [(and (not in-string?) (char=? ch #\())
                       (loop (+ i 1) (+ delta 1) in-string? #f)]
                      [(and (not in-string?) (char=? ch #\)))
                       (loop (+ i 1) (- delta 1) in-string? #f)]
                      [else
                       (loop (+ i 1) delta in-string? #f)]))))))

(define (sexp-size sexp)
  (doc 'type '(-> Sexp Nat))
  (doc 'description "Count the number of nodes in an S-expression tree")
  (doc 'export #t)
  (cond
   [(pair? sexp)
    (+ 1 (sexp-size (car sexp)) (sexp-size (cdr sexp)))]
   [(null? sexp) 0]
   [else 1]))

(define (sexp-depth sexp)
  (doc 'type '(-> Sexp Nat))
  (doc 'description "Compute maximum nesting depth")
  (doc 'export #t)
  (cond
   [(pair? sexp)
    (+ 1 (max (sexp-depth (car sexp)) (sexp-depth (cdr sexp))))]
   [else 0]))

(define (extract-references sexp)
  (doc 'type '(-> Sexp (List Symbol)))
  (doc 'description "Extract all symbols referenced in an S-expression")
  (doc 'export #t)
  (cond
   [(symbol? sexp) (list sexp)]
   [(pair? sexp)
    (append (extract-references (car sexp))
            (extract-references (cdr sexp)))]
   [else '()]))

(doc 'note "unique is provided by core/base/prelude.ss")

(doc 'section 'definition-extraction)

(define (definition? sexp)
  (doc 'type '(-> Sexp Boolean))
  (doc 'description "Check if S-expression is a definition form")
  (doc 'export #t)
  (and (pair? sexp)
       (symbol? (car sexp))
       (memq (car sexp) '(define define-record-type define-syntax
                          define-condition-type))))

(define (extract-definition-name sexp)
  (doc 'type '(-> Sexp (or Symbol #f)))
  (doc 'description "Extract the name being defined")
  (doc 'export #t)
  (cond
   [(not (pair? sexp)) #f]
   [(not (pair? (cdr sexp))) #f]
   [else
    (let ([form (car sexp)]
          [name-part (cadr sexp)])
         (cond
          [(eq? form 'define)
           (if (pair? name-part)
               (car name-part)
               name-part)]
          [(eq? form 'define-record-type)
           name-part]
          [(eq? form 'define-syntax)
           name-part]
          [else #f]))]))

(define (extract-definition-type sexp)
  (doc 'type '(-> Sexp Symbol))
  (doc 'export #t)
  (if (pair? sexp) (car sexp) 'unknown))

(define (analyze-definition sexp line-number)
  (doc 'type '(-> Sexp Nat DefinitionInfo))
  (doc 'export #t)
  (make-definition-info
   (extract-definition-name sexp)
   (extract-definition-type sexp)
   line-number
   (sexp-size sexp)
   (sexp-depth sexp)
   (unique (extract-references sexp))))

(doc 'section 'load-dependency-extraction)

(define (load-form? sexp)
  (doc 'type '(-> Sexp Boolean))
  (doc 'export #t)
  (and (pair? sexp)
       (eq? (car sexp) 'load)
       (pair? (cdr sexp))
       (string? (cadr sexp))))

(define (extract-load-path sexp)
  (doc 'type '(-> Sexp (or String #f)))
  (doc 'export #t)
  (if (load-form? sexp)
      (cadr sexp)
      #f))

(doc 'section 'file-analysis)

(define (analyze-file path)
  (doc 'type '(-> String FileMetrics))
  (doc 'description "Analyze a single Scheme source file")
  (doc 'export #t)
  (let* ([lines (read-file-lines path)]
         [line-types (map line-type lines)]
         [total (length lines)]
         [code-count (length (filter (lambda (t) (eq? t 'code)) line-types))]
         [comment-count (length (filter (lambda (t) (eq? t 'comment)) line-types))]
         [blank-count (length (filter (lambda (t) (eq? t 'blank)) line-types))]
         [sexps-with-lines (guard (ex [else '()])
                                  (read-all-sexps-with-lines path))]
         [sexps (map cdr sexps-with-lines)]
         [definitions (filter-map
                       (lambda (line-sexp)
                               (let ([line (car line-sexp)]
                                     [sexp (cdr line-sexp)])
                                    (and (definition? sexp)
                                         (analyze-definition sexp line))))
                       sexps-with-lines)]
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

(define (filter-map f lst)
  (doc 'type '(-> (-> A (or B #f)) (List A) (List B)))
  (doc 'export #t)
  (let loop ([lst lst] [acc '()])
       (if (null? lst)
           (reverse acc)
           (let ([result (f (car lst))])
                (if result
                    (loop (cdr lst) (cons result acc))
                    (loop (cdr lst) acc))))))

(doc 'section 'directory-analysis)

(define-record-type directory-metrics
  (fields
   path              ; Directory path
   file-count        ; Number of .ss files
   total-lines       ; Total lines across all files
   total-code        ; Total code lines
   total-comments    ; Total comment lines
   total-definitions ; Total definition count
   files))           ; List of FileMetrics

(define (scheme-file? path)
  (doc 'type '(-> String Boolean))
  (doc 'export #t)
  (let ([len (string-length path)])
       (and (> len 3)
            (string=? (substring path (- len 3) len) ".ss"))))

(define (analyze-directory path)
  (doc 'type '(-> String DirectoryMetrics))
  (doc 'description "Analyze all Scheme files in a directory (non-recursive)")
  (doc 'export #t)
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

(doc 'section 'coverage-analysis)

(define-record-type coverage-report
  (fields
   source-definitions   ; List of symbols defined in sources
   tested-definitions   ; List of symbols referenced in tests
   covered              ; Intersection (actually tested)
   uncovered            ; Defined but not tested
   coverage-ratio))     ; 0.0 to 1.0

(define (compute-coverage test-analyses source-analyses)
  (doc 'type '(-> (List FileMetrics) (List FileMetrics) CoverageReport))
  (doc 'description "Compare test files against source files to determine coverage")
  (doc 'export #t)
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

(doc 'section 'complexity-scoring)

(define (complexity-score def)
  (doc 'type '(-> DefinitionInfo Nat))
  (doc 'description "Compute a complexity score for a definition. Higher = more complex")
  (doc 'export #t)
  (let ([size (definition-info-form-size def)]
        [depth (definition-info-nesting-depth def)]
        [refs (length (definition-info-references def))])
       (+ (* size 1)
          (* depth 5)
          (* refs 2))))

(define (high-complexity? def threshold)
  (doc 'type '(-> DefinitionInfo Nat Boolean))
  (doc 'description "Check if definition exceeds complexity threshold")
  (doc 'export #t)
  (> (complexity-score def) threshold))

(doc 'section 'reporting)

(define (format-file-metrics fm)
  (doc 'type '(-> FileMetrics String))
  (doc 'export #t)
  (format "~a:\n  Lines: ~a (code: ~a, comments: ~a, blank: ~a)\n  Definitions: ~a\n  Max nesting: ~a\n  Dependencies: ~a"
          (file-metrics-path fm)
          (file-metrics-total-lines fm)
          (file-metrics-code-lines fm)
          (file-metrics-comment-lines fm)
          (file-metrics-blank-lines fm)
          (length (file-metrics-definitions fm))
          (file-metrics-max-nesting fm)
          (length (file-metrics-load-deps fm))))

(define (format-directory-metrics dm)
  (doc 'type '(-> DirectoryMetrics String))
  (doc 'export #t)
  (format "~a/:\n  Files: ~a\n  Total lines: ~a (code: ~a, comments: ~a)\n  Definitions: ~a"
          (directory-metrics-path dm)
          (directory-metrics-file-count dm)
          (directory-metrics-total-lines dm)
          (directory-metrics-total-code dm)
          (directory-metrics-total-comments dm)
          (directory-metrics-total-definitions dm)))

(define (format-coverage cr)
  (doc 'type '(-> CoverageReport String))
  (doc 'export #t)
  (format "Coverage: ~,1f% (~a/~a definitions)\n  Uncovered: ~a"
          (* (coverage-report-coverage-ratio cr) 100)
          (length (coverage-report-covered cr))
          (length (coverage-report-source-definitions cr))
          (if (null? (coverage-report-uncovered cr))
              "(none)"
              (apply string-append
                     (map (lambda (s) (format " ~a" s))
                          (coverage-report-uncovered cr))))))

(doc 'section 'display-utilities)

(define (print-file-metrics fm)
  (doc 'type '(-> FileMetrics void))
  (doc 'export #t)
  (display (format-file-metrics fm))
  (newline))

(define (print-directory-metrics dm)
  (doc 'type '(-> DirectoryMetrics void))
  (doc 'export #t)
  (display (format-directory-metrics dm))
  (newline))

(define (print-coverage cr)
  (doc 'type '(-> CoverageReport void))
  (doc 'export #t)
  (display (format-coverage cr))
  (newline))

(define (print-complexity-warnings file-analyses threshold)
  (doc 'type '(-> (List FileMetrics) Nat void))
  (doc 'description "Print definitions that exceed the complexity threshold")
  (doc 'export #t)
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

(doc 'section 'dead-code-detection)

(define-record-type dead-code-report
  (fields
   all-definitions    ; List of (file . definition-info) pairs
   all-references     ; Set of all referenced symbols
   dead-definitions   ; List of (file . definition-info) never referenced
   dead-count         ; Number of dead definitions
   live-count))       ; Number of live definitions

(define (find-dead-code file-analyses)
  (doc 'type '(-> (List FileMetrics) DeadCodeReport))
  (doc 'description "Find definitions that are never referenced by any other definition")
  (doc 'note "This may report false positives for entry points, exported API functions, and record type accessors")
  (doc 'export #t)
  (let* ([all-defs (apply append
                          (map (lambda (fm)
                                       (map (lambda (d) (cons (file-metrics-path fm) d))
                                            (file-metrics-definitions fm)))
                               file-analyses))]
         [all-refs (unique
                    (apply append
                           (map (lambda (fd)
                                        (definition-info-references (cdr fd)))
                                all-defs)))]
         [ref-table (let ([ht (make-eq-hashtable)])
                         (for-each (lambda (sym) (hashtable-set! ht sym #t)) all-refs)
                         ht)]
         [dead (filter
                (lambda (fd)
                        (let ([name (definition-info-name (cdr fd))])
                             (and name
                                  (not (hashtable-ref ref-table name #f)))))
                all-defs)]
         [live-count (- (length all-defs) (length dead))])
        (make-dead-code-report
         all-defs
         all-refs
         dead
         (length dead)
         live-count)))

(define (print-dead-code-report report)
  (doc 'type '(-> DeadCodeReport void))
  (doc 'export #t)
  (display "=== Dead Code Analysis ===\n\n")
  (display (format "Total definitions: ~a\n" (+ (dead-code-report-dead-count report)
                                                (dead-code-report-live-count report))))
  (display (format "Live (referenced): ~a\n" (dead-code-report-live-count report)))
  (display (format "Dead (unreferenced): ~a\n\n" (dead-code-report-dead-count report)))

  (let ([dead (dead-code-report-dead-definitions report)])
       (if (null? dead)
           (display "No dead code found.\n")
           (begin
            (display "Potentially dead definitions:\n")
            (for-each
             (lambda (fd)
                     (let ([file (car fd)]
                           [def (cdr fd)])
                          (display (format "  ~a:~a - ~a (~a)\n"
                                           file
                                           (definition-info-line-number def)
                                           (definition-info-name def)
                                           (definition-info-type def)))))
             dead)))))

(define (find-dead-code-in-directory path)
  (doc 'type '(-> String DeadCodeReport))
  (doc 'description "Convenience function to analyze a single directory")
  (doc 'export #t)
  (let ([dm (analyze-directory path)])
       (find-dead-code (directory-metrics-files dm))))

(doc 'section 'dependency-analysis)

(define-record-type dependency-graph
  (fields
   nodes             ; List of definition names (symbols)
   edges             ; List of (from . to) pairs
   in-degree         ; Alist of (symbol . count) for incoming edges
   out-degree))      ; Alist of (symbol . count) for outgoing edges

(define (build-dependency-graph file-analyses)
  (doc 'type '(-> (List FileMetrics) DependencyGraph))
  (doc 'description "Build a graph of definition dependencies")
  (doc 'export #t)
  (let* ([all-defs (apply append
                          (map file-metrics-definitions file-analyses))]
         [def-names (filter symbol?
                            (map definition-info-name all-defs))]
         [edges (apply append
                       (map (lambda (def)
                                    (let ([name (definition-info-name def)]
                                          [refs (definition-info-references def)])
                                         (if name
                                             (filter-map
                                              (lambda (ref)
                                                      (and (member ref def-names)
                                                           (not (eq? ref name))
                                                           (cons name ref)))
                                              refs)
                                             '())))
                            all-defs))]
         [in-deg (map (lambda (name)
                              (cons name
                                    (length (filter (lambda (e) (eq? (cdr e) name)) edges))))
                      def-names)]
         [out-deg (map (lambda (name)
                               (cons name
                                     (length (filter (lambda (e) (eq? (car e) name)) edges))))
                       def-names)])
        (make-dependency-graph def-names edges in-deg out-deg)))

(define (find-dependency-roots graph)
  (doc 'type '(-> DependencyGraph (List Symbol)))
  (doc 'description "Find definitions that nothing depends on (potential entry points)")
  (doc 'export #t)
  (filter (lambda (name)
                  (let ([entry (assq name (dependency-graph-in-degree graph))])
                       (and entry (= (cdr entry) 0))))
          (dependency-graph-nodes graph)))

(define (find-dependency-leaves graph)
  (doc 'type '(-> DependencyGraph (List Symbol)))
  (doc 'description "Find definitions that don't depend on anything (primitives/base)")
  (doc 'export #t)
  (filter (lambda (name)
                  (let ([entry (assq name (dependency-graph-out-degree graph))])
                       (and entry (= (cdr entry) 0))))
          (dependency-graph-nodes graph)))

(define (find-most-depended-on graph n)
  (doc 'type '(-> DependencyGraph Nat (List (Cons Symbol Nat))))
  (doc 'description "Find definitions with the highest in-degree (most depended upon)")
  (doc 'export #t)
  (let* ([sorted (list-sort
                  (lambda (a b) (> (cdr a) (cdr b)))
                  (dependency-graph-in-degree graph))])
        (take (min n (length sorted)) sorted)))

(define (find-most-dependencies graph n)
  (doc 'type '(-> DependencyGraph Nat (List (Cons Symbol Nat))))
  (doc 'description "Find definitions with the highest out-degree (most dependencies)")
  (doc 'export #t)
  (let* ([sorted (list-sort
                  (lambda (a b) (> (cdr a) (cdr b)))
                  (dependency-graph-out-degree graph))])
        (take (min n (length sorted)) sorted)))

(define (print-dependency-report graph)
  (doc 'type '(-> DependencyGraph void))
  (doc 'export #t)
  (display "=== Dependency Analysis ===\n\n")
  (display (format "Total definitions: ~a\n" (length (dependency-graph-nodes graph))))
  (display (format "Total dependencies: ~a\n\n" (length (dependency-graph-edges graph))))

  (display "--- Most Depended On (top 10) ---\n")
  (for-each
   (lambda (entry)
           (display (format "  ~a: ~a dependents\n" (car entry) (cdr entry))))
   (find-most-depended-on graph 10))
  (newline)

  (display "--- Most Dependencies (top 10) ---\n")
  (for-each
   (lambda (entry)
           (display (format "  ~a: ~a dependencies\n" (car entry) (cdr entry))))
   (find-most-dependencies graph 10))
  (newline)

  (let ([roots (find-dependency-roots graph)])
       (display (format "--- Entry Points (~a definitions with no dependents) ---\n"
                        (length roots)))
       (for-each
        (lambda (name)
                (display (format "  ~a\n" name)))
        (take (min 20 (length roots)) roots))
       (when (> (length roots) 20)
             (display (format "  ... and ~a more\n" (- (length roots) 20)))))
  (newline))

(define (export-dependency-dot graph output-path)
  (doc 'type '(-> DependencyGraph String void))
  (doc 'description "Export dependency graph in DOT format for visualization")
  (doc 'export #t)
  (call-with-output-file output-path
                         (lambda (port)
                                 (display "digraph dependencies {\n" port)
                                 (display "  rankdir=LR;\n" port)
                                 (display "  node [shape=box];\n" port)
                                 (for-each
                                  (lambda (edge)
                                          (display (format "  \"~a\" -> \"~a\";\n" (car edge) (cdr edge)) port))
                                  (dependency-graph-edges graph))
                                 (display "}\n" port)))
  (display (format "Exported to ~a\n" output-path)))

(doc 'section 'full-codebase-report)

(define-record-type codebase-report
  (fields
   directories       ; List of DirectoryMetrics
   total-files       ; Total file count
   total-lines       ; Total line count
   total-definitions ; Total definitions
   coverage          ; CoverageReport (if tests analyzed)
   high-complexity)) ; List of high-complexity definitions

(define (analyze-codebase source-dirs test-dirs complexity-threshold)
  (doc 'type '(-> (List String) (List String) Nat CodebaseReport))
  (doc 'description "Full analysis of source and test directories")
  (doc 'export #t)
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

(define (print-codebase-report report)
  (doc 'type '(-> CodebaseReport void))
  (doc 'export #t)
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
