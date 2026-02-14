;;; boundary/tools/test-counter.ss — Automated test counting and README.sexp updates
;;;
;;; Eliminates hardcoded test counts by scanning test files and generating
;;; accurate counts. Supports both ad-hoc tests and test-framework patterns.
;;;
;;; Usage:
;;;   (load "boundary/tools/test-counter.ss")
;;;   (tc-report)                     ; Show all test counts
;;;   (tc-report "lattice/linalg")    ; Show counts for specific directory
;;;   (tc-drift)                      ; Show files where README count != actual
;;;   (tc-update-readme! "path")      ; Update a README.sexp with actual counts

(load "core/base/prelude.ss")

(doc 'module 'test-counter)
(doc 'description "Automated test counting to eliminate hardcoded test counts in README.sexp files")
(doc 'layer 'boundary)

;;; ============================================================================
;;; Test Pattern Detection and Counting
;;; ============================================================================

(define (count-tests-in-file path)
  (doc 'export #t)
  (doc 'description "Count tests in a test file, detecting pattern automatically")
  (doc 'returns "(test-count . pattern) where pattern is 'framework or 'adhoc")
  (guard (ex [else (cons 0 'error)])
    (let* ([content (call-with-input-file path
                      (lambda (p)
                        (let loop ([lines '()])
                          (let ([line (get-line p)])
                            (if (eof-object? line)
                                (apply string-append (reverse lines))
                                (loop (cons (string-append line "\n") lines)))))))]
           [uses-framework? (string-contains content "define-test")]
           [pattern (if uses-framework? 'framework 'adhoc)])
      (cons (if uses-framework?
                (count-framework-tests content)
                (count-adhoc-tests content))
            pattern))))

(define (string-contains haystack needle)
  (doc 'description "Check if haystack contains needle substring")
  (let ([hlen (string-length haystack)]
        [nlen (string-length needle)])
    (let loop ([i 0])
      (cond
        [(> (+ i nlen) hlen) #f]
        [(string=? (substring haystack i (+ i nlen)) needle) #t]
        [else (loop (+ i 1))]))))

(define (count-framework-tests content)
  (doc 'description "Count define-test occurrences for test-framework style")
  (count-pattern-occurrences content "define-test"))

(define (count-adhoc-tests content)
  (doc 'description "Count (test \"...\") and variants for ad-hoc style")
  ;; Count: (test ", (test-true ", (test-false ", (test-error ", (test-near ", (test-approx "
  (+ (count-pattern-occurrences content "(test \"")
     (count-pattern-occurrences content "(test-true \"")
     (count-pattern-occurrences content "(test-false \"")
     (count-pattern-occurrences content "(test-error \"")
     (count-pattern-occurrences content "(test-near \"")
     (count-pattern-occurrences content "(test-approx \"")))

(define (count-pattern-occurrences content pattern)
  (doc 'description "Count non-overlapping occurrences of pattern in content")
  (let ([clen (string-length content)]
        [plen (string-length pattern)])
    (let loop ([i 0] [count 0])
      (cond
        [(> (+ i plen) clen) count]
        [(string=? (substring content i (+ i plen)) pattern)
         (loop (+ i plen) (+ count 1))]
        [else (loop (+ i 1) count)]))))

;;; ============================================================================
;;; File Discovery
;;; ============================================================================

(define (find-test-files dir)
  (doc 'export #t)
  (doc 'description "Find all test-*.ss files under directory")
  (let ([result '()])
    (define (scan path)
      (guard (ex [else #f])
        (let ([entries (directory-list path)])
          (for-each
            (lambda (entry)
              (let ([full (string-append path "/" entry)])
                (cond
                  [(and (> (string-length entry) 0)
                        (char=? (string-ref entry 0) #\.))
                   ;; Skip hidden files/dirs
                   #f]
                  [(file-directory? full)
                   (scan full)]
                  [(and (string-suffix? ".ss" entry)
                        (string-prefix? "test-" entry))
                   (set! result (cons full result))])))
            entries))))
    (scan dir)
    (list-sort string<? result)))

(define (string-prefix? prefix str)
  (and (>= (string-length str) (string-length prefix))
       (string=? (substring str 0 (string-length prefix)) prefix)))

(define (string-suffix? suffix str)
  (let ([slen (string-length str)]
        [xlen (string-length suffix)])
    (and (>= slen xlen)
         (string=? (substring str (- slen xlen) slen) suffix))))

;;; ============================================================================
;;; README.sexp Parsing
;;; ============================================================================

(define (parse-readme-test-counts readme-path)
  (doc 'export #t)
  (doc 'description "Extract test counts from README.sexp")
  (doc 'returns "List of (module-name . count) pairs")
  (guard (ex [else '()])
    (let ([sexp (call-with-input-file readme-path read)])
      (extract-test-counts-from-sexp sexp))))

(define (extract-test-counts-from-sexp sexp)
  (doc 'description "Walk sexp looking for '- N tests' patterns")
  (let ([result '()])
    (define (walk x)
      (cond
        [(string? x)
         ;; Look for "module.ss ... - N tests" pattern
         (let ([match (extract-test-count-from-string x)])
           (when match
             (set! result (cons match result))))]
        [(pair? x)
         (walk (car x))
         (walk (cdr x))]))
    (walk sexp)
    (reverse result)))

(define (extract-test-count-from-string str)
  (doc 'description "Extract (module . count) from 'module.ss ... - N tests' string")
  ;; Match pattern: "something.ss ... - N tests"
  (let* ([parts (string-split str #\-)]
         [last-part (and (pair? parts) (list-ref parts (- (length parts) 1)))])
    (and last-part
         (let ([trimmed (string-trim last-part)])
           (and (string-suffix? "tests" trimmed)
                (let* ([words (string-split trimmed #\space)]
                       [count-str (and (>= (length words) 2) (car words))]
                       [count (and count-str (string->number count-str))]
                       [module-name (extract-module-name str)])
                  (and count module-name (cons module-name count))))))))

(define (extract-module-name str)
  (doc 'description "Extract module.ss name from string")
  (let ([parts (string-split str #\space)])
    (let loop ([ps parts])
      (cond
        [(null? ps) #f]
        [(string-suffix? ".ss" (car ps)) (car ps)]
        [else (loop (cdr ps))]))))

(define (string-split str char)
  (doc 'description "Split string by delimiter character")
  (let ([len (string-length str)])
    (let loop ([i 0] [start 0] [result '()])
      (cond
        [(= i len)
         (reverse (cons (substring str start len) result))]
        [(char=? (string-ref str i) char)
         (loop (+ i 1) (+ i 1) (cons (substring str start i) result))]
        [else
         (loop (+ i 1) start result)]))))

(define (string-trim str)
  (doc 'description "Trim leading and trailing whitespace")
  (let* ([len (string-length str)]
         [start (let loop ([i 0])
                  (if (and (< i len) (char-whitespace? (string-ref str i)))
                      (loop (+ i 1))
                      i))]
         [end (let loop ([i len])
                (if (and (> i start) (char-whitespace? (string-ref str (- i 1))))
                    (loop (- i 1))
                    i))])
    (substring str start end)))

;;; ============================================================================
;;; Reporting
;;; ============================================================================

(define (tc-report . args)
  (doc 'export #t)
  (doc 'description "Generate test count report for directory")
  (doc 'param 'args "Optional: directory path (default: entire project)")
  (let* ([dir (if (null? args) "." (car args))]
         [files (find-test-files dir)]
         [total 0])
    (display "Test Count Report\n")
    (display "=================\n\n")
    (for-each
      (lambda (file)
        (let* ([result (count-tests-in-file file)]
               [count (car result)]
               [pattern (cdr result)]
               [short-path (if (string-prefix? "./" file)
                              (substring file 15 (string-length file))
                              file)])
          (set! total (+ total count))
          (display (format "~a: ~a (~a)\n" short-path count pattern))))
      files)
    (display (format "\nTotal: ~a tests in ~a files\n" total (length files)))
    total))

(define (tc-summary . args)
  (doc 'export #t)
  (doc 'description "Summarized test counts by directory")
  (let* ([dir (if (null? args) "." (car args))]
         [files (find-test-files dir)]
         [by-dir (make-hashtable string-hash string=?)])
    ;; Group by parent directory
    (for-each
      (lambda (file)
        (let* ([result (count-tests-in-file file)]
               [count (car result)]
               [parent (dirname file)]
               [short-parent (if (string-prefix? "./" parent)
                                (substring parent 15 (string-length parent))
                                parent)]
               [existing (hashtable-ref by-dir short-parent '(0 . 0))])
          (hashtable-set! by-dir short-parent
            (cons (+ (car existing) count)
                  (+ (cdr existing) 1)))))
      files)
    ;; Display
    (display "Test Summary by Directory\n")
    (display "=========================\n\n")
    (let ([dirs (list-sort string<? (vector->list (hashtable-keys by-dir)))]
          [grand-total 0])
      (for-each
        (lambda (d)
          (let ([stats (hashtable-ref by-dir d '(0 . 0))])
            (set! grand-total (+ grand-total (car stats)))
            (display (format "~a: ~a tests (~a files)\n" d (car stats) (cdr stats)))))
        dirs)
      (display (format "\nGrand Total: ~a tests\n" grand-total))
      grand-total)))

(define (dirname path)
  (doc 'description "Get directory portion of path")
  (let ([len (string-length path)])
    (let loop ([i (- len 1)])
      (cond
        [(< i 0) "."]
        [(char=? (string-ref path i) #\/)
         (substring path 0 i)]
        [else (loop (- i 1))]))))

(define (basename path)
  (doc 'description "Get filename portion of path")
  (let ([len (string-length path)])
    (let loop ([i (- len 1)])
      (cond
        [(< i 0) path]
        [(char=? (string-ref path i) #\/)
         (substring path (+ i 1) len)]
        [else (loop (- i 1))]))))

;;; ============================================================================
;;; Drift Detection
;;; ============================================================================

(define (tc-drift . args)
  (doc 'export #t)
  (doc 'description "Show files where README.sexp count differs from actual")
  (let* ([dir (if (null? args) "." (car args))]
         [test-files (find-test-files dir)]
         [drifts '()])
    ;; Build actual counts by module name
    (let ([actual-counts (make-hashtable string-hash string=?)])
      (for-each
        (lambda (file)
          (let* ([result (count-tests-in-file file)]
                 [count (car result)]
                 [module-base (basename file)]
                 ;; Convert test-foo.ss to foo.ss
                 [source-module (if (string-prefix? "test-" module-base)
                                   (substring module-base 5 (string-length module-base))
                                   module-base)])
            (hashtable-set! actual-counts source-module count)
            ;; Also store under test file name
            (hashtable-set! actual-counts module-base count)))
        test-files)
      ;; Find README.sexp files and compare
      (define (scan-for-readmes path)
        (guard (ex [else #f])
          (let ([entries (directory-list path)])
            (for-each
              (lambda (entry)
                (let ([full (string-append path "/" entry)])
                  (cond
                    [(and (> (string-length entry) 0)
                          (char=? (string-ref entry 0) #\.))
                     #f]
                    [(file-directory? full)
                     (scan-for-readmes full)]
                    [(string=? entry "README.sexp")
                     (check-readme-drift full actual-counts)])))
              entries))))

      (define (check-readme-drift readme-path actual-counts)
        (let ([documented (parse-readme-test-counts readme-path)])
          (for-each
            (lambda (doc-pair)
              (let* ([module (car doc-pair)]
                     [doc-count (cdr doc-pair)]
                     [actual (hashtable-ref actual-counts module #f)])
                (when (and actual (not (= actual doc-count)))
                  (set! drifts (cons (list readme-path module doc-count actual) drifts)))))
            documented)))

      (scan-for-readmes dir))

    ;; Report
    (if (null? drifts)
        (display "No drift detected - all README.sexp counts match actual.\n")
        (begin
          (display "Test Count Drift Detected\n")
          (display "=========================\n\n")
          (for-each
            (lambda (drift)
              (let ([readme (list-ref drift 0)]
                    [module (list-ref drift 1)]
                    [documented (list-ref drift 2)]
                    [actual (list-ref drift 3)])
                (let ([short-readme (if (string-prefix? "./" readme)
                                       (substring readme 15 (string-length readme))
                                       readme)])
                  (display (format "~a: ~a documented=~a actual=~a (~a~a)\n"
                            short-readme module documented actual
                            (if (> actual documented) "+" "")
                            (- actual documented))))))
            (reverse drifts))))
    drifts))

;;; ============================================================================
;;; Quick Stats
;;; ============================================================================

(define (tc-quick path)
  (doc 'export #t)
  (doc 'description "Quick count for a single test file")
  (let ([result (count-tests-in-file path)])
    (display (format "~a: ~a tests (~a pattern)\n"
              path (car result) (cdr result)))
    (car result)))

;;; ============================================================================
;;; Exports
;;; ============================================================================

(display "test-counter.ss loaded.\n")
(display "  (tc-report)           - Full report of all test files\n")
(display "  (tc-report \"path\")    - Report for specific directory\n")
(display "  (tc-summary)          - Summary by directory\n")
(display "  (tc-drift)            - Find README.sexp count mismatches\n")
(display "  (tc-quick \"file.ss\")  - Quick count for single file\n")
