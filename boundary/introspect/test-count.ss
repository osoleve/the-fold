(load "core/base/prelude.ss")

(doc 'module 'test-count)
(doc 'description "Test counting utility for lattice skills")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'note "Scans test files for (define-test ...) and legacy (test ...) forms")

(doc 'section 'file-scanning)

(define (count-tests-in-file filepath)
  (doc 'description "Count test occurrences in a Scheme file (new and legacy styles)")
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
                  ;; Recursively count all test forms in each top-level expression
                  [else (loop (+ count (count-tests-in-expr expr)))]))))))))

;; Legacy test form names (each call = 1 test)
(define *legacy-test-forms*
  '(test test-approx test-vec test-mat
    test-true test-false test-error test-near test-assert
    test-eq test-equal test-eqv))

(define (count-tests-in-expr expr)
  (doc 'description "Recursively count test forms within an expression")
  (cond
    [(not (pair? expr)) 0]
    ;; New-style: (define-test "name" body...) - count as 1, don't recurse
    [(eq? (car expr) 'define-test) 1]
    ;; test-group contains define-test forms - recurse but don't count the group itself
    [(eq? (car expr) 'test-group)
     (count-in-list (cddr expr))]  ; skip name
    ;; Legacy test forms - count as 1 each
    [(memq (car expr) *legacy-test-forms*) 1]
    ;; Recurse into other forms (let, let*, begin, when, etc.)
    [else (count-in-list (cdr expr))]))

(define (count-in-list lst)
  (doc 'description "Count tests in a list, handling improper lists safely")
  (let loop ([ls lst] [sum 0])
    (cond
      [(null? ls) sum]
      [(pair? ls) (loop (cdr ls) (+ sum (count-tests-in-expr (car ls))))]
      [else (+ sum (count-tests-in-expr ls))])))

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
                             (and (> (string-length d) 0)
                                  (not (char=? (string-ref d 0) #\.))  ; Skip hidden dirs
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

(doc 'section 'readme-updates)

(define (extract-module-name str)
  (doc 'description "Extract module.ss name from a description string")
  (and (string? str)
       (let ([len (string-length str)])
         (and (> len 3)
              (let loop ([i 0])
                (cond
                  [(>= i (- len 3)) #f]
                  [(and (char=? (string-ref str i) #\.)
                        (char=? (string-ref str (+ i 1)) #\s)
                        (char=? (string-ref str (+ i 2)) #\s))
                   ;; Found .ss, now find the start of the word
                   (let find-start ([j (- i 1)])
                     (cond
                       [(< j 0) (substring str 0 (+ i 3))]
                       [(char-whitespace? (string-ref str j))
                        (substring str (+ j 1) (+ i 3))]
                       [(= j 0) (substring str 0 (+ i 3))]
                       [else (find-start (- j 1))]))]
                  [else (loop (+ i 1))]))))))

(define (update-test-count-in-string str actual-count)
  (doc 'description "Update test count in a module description string")
  (doc 'example "(update-test-count-in-string \"vec.ss - 50 tests\" 56) => \"vec.ss - 56 tests\"")
  (if (not (string? str))
      str
      (let* ([len (string-length str)]
             ;; Find " - N test" or "- N test" pattern
             [dash-pos (let loop ([i 0])
                         (cond
                           [(>= i (- len 4)) #f]
                           [(and (char=? (string-ref str i) #\-)
                                 (< (+ i 2) len)
                                 (char-whitespace? (string-ref str (+ i 1))))
                            i]
                           [else (loop (+ i 1))]))])
        (if (not dash-pos)
            str
            ;; Find the number after the dash
            (let* ([after-dash (+ dash-pos 2)]
                   [num-start (let loop ([i after-dash])
                                (cond
                                  [(>= i len) #f]
                                  [(char-numeric? (string-ref str i)) i]
                                  [(char-whitespace? (string-ref str i)) (loop (+ i 1))]
                                  [else #f]))])
              (if (not num-start)
                  str
                  ;; Find end of number
                  (let* ([num-end (let loop ([i num-start])
                                    (cond
                                      [(>= i len) i]
                                      [(char-numeric? (string-ref str i)) (loop (+ i 1))]
                                      [else i]))]
                         ;; Check if followed by " test"
                         [after-num (if (< num-end len) num-end len)]
                         [rest (if (< after-num len) (substring str after-num len) "")]
                         [is-test-pattern (or (string-prefix? " test" rest)
                                              (string-prefix? " tests" rest))])
                    (if is-test-pattern
                        (string-append
                         (substring str 0 num-start)
                         (number->string actual-count)
                         rest)
                        str))))))))

(define (string-prefix? prefix str)
  (doc 'description "Check if str starts with prefix")
  (and (>= (string-length str) (string-length prefix))
       (string=? prefix (substring str 0 (string-length prefix)))))

(define (build-test-count-map dir)
  (doc 'description "Build a map of module.ss -> test count for a directory")
  (let* ([test-files (list-test-files dir)]
         [pairs (map (lambda (f)
                       (let* ([path (string-append dir "/" f)]
                              [count (count-tests-in-file path)]
                              ;; Convert test-foo.ss -> foo.ss
                              [module-name (if (string-prefix? "test-" f)
                                               (substring f 5 (string-length f))
                                               f)])
                         (cons module-name count)))
                     test-files)])
    pairs))

(define (update-modules-sexp modules-list count-map)
  (doc 'description "Update test counts in a modules S-expression list")
  (map (lambda (entry)
         (cond
           ;; Entry is (module.ss "description - N tests")
           [(and (pair? entry)
                 (symbol? (car entry))
                 (pair? (cdr entry))
                 (string? (cadr entry)))
            (let* ([module-name (symbol->string (car entry))]
                   [desc (cadr entry)]
                   [count-entry (assoc module-name count-map)]
                   [new-desc (if count-entry
                                 (update-test-count-in-string desc (cdr count-entry))
                                 desc)])
              (list (car entry) new-desc))]
           ;; Entry is "module.ss description - N tests" (string only)
           [(string? entry)
            (let* ([module-name (extract-module-name entry)]
                   [count-entry (and module-name (assoc module-name count-map))])
              (if count-entry
                  (update-test-count-in-string entry (cdr count-entry))
                  entry))]
           [else entry]))
       modules-list))

(define (update-readme-sexp sexp count-map total-count)
  (doc 'description "Recursively update test counts in a README S-expression")
  (cond
    [(not (pair? sexp)) sexp]
    ;; (modules (...)) form
    [(and (pair? sexp)
          (eq? (car sexp) 'modules)
          (pair? (cdr sexp))
          (pair? (cadr sexp)))
     (list 'modules (update-modules-sexp (cadr sexp) count-map))]
    ;; (total-tests N) form
    [(and (pair? sexp)
          (eq? (car sexp) 'total-tests)
          (pair? (cdr sexp))
          (number? (cadr sexp)))
     (list 'total-tests total-count)]
    ;; Recurse into nested lists
    [(pair? sexp)
     (cons (update-readme-sexp (car sexp) count-map total-count)
           (update-readme-sexp (cdr sexp) count-map total-count))]
    [else sexp]))

(define (pretty-write-sexp sexp port indent)
  (doc 'description "Write S-expression with reasonable formatting")
  (letrec ([write-indent (lambda (n)
                           (do ([i 0 (+ i 1)]) [(>= i n)] (display " " port)))])
    (cond
    [(null? sexp) (display "()" port)]
    [(not (pair? sexp)) (write sexp port)]
    ;; Top-level list of forms
    [(and (= indent 0) (pair? (car sexp)))
     (display "(" port)
     (let loop ([items sexp] [first? #t])
       (cond
         [(null? items) (display ")" port)]
         [else
          (unless first? (newline port))
          (pretty-write-sexp (car items) port 1)
          (loop (cdr items) #f)]))]
    ;; (key value) pair
    [(and (symbol? (car sexp))
          (pair? (cdr sexp))
          (null? (cddr sexp)))
     (display "(" port)
     (write (car sexp) port)
     (display " " port)
     (let ([val (cadr sexp)])
       (if (and (pair? val) (pair? (car val)))
           (begin
             (newline port)
             (write-indent (+ indent 1))
             (pretty-write-sexp val port (+ indent 1)))
           (pretty-write-sexp val port (+ indent 1))))
     (display ")" port)]
    ;; List of items (handle improper lists / dotted pairs)
    [else
     (display "(" port)
     (let loop ([items sexp] [first? #t])
       (cond
         [(null? items) (display ")" port)]
         [(not (pair? items))
          ;; Improper list tail (dotted pair)
          (display " . " port)
          (write items port)
          (display ")" port)]
         [else
          (unless first?
            (if (and (pair? (car items)) (symbol? (caar items)))
                (begin (newline port) (write-indent indent))
                (display " " port)))
          (pretty-write-sexp (car items) port (+ indent 1))
          (loop (cdr items) #f)]))])))

(define (update-skill-readme! skill-name)
  (doc 'description "Update test counts in a skill's README.sexp")
  (doc 'param 'skill-name "Skill name as string or symbol")
  (doc 'returns "#t on success, #f on failure")
  (doc 'export #t)
  (let* ([name (if (symbol? skill-name)
                   (symbol->string skill-name)
                   skill-name)]
         [dir (string-append "lattice/" name)]
         [readme-path (string-append dir "/README.sexp")])
    (guard (e [else
               (printf "Error updating ~a: ~a\n" readme-path
                       (if (condition? e) (condition-message e) e))
               #f])
      (if (not (file-exists? readme-path))
          (begin
            (printf "No README.sexp found at ~a\n" readme-path)
            #f)
          (let* ([count-map (build-test-count-map dir)]
                 [total-count (fold-left + 0 (map cdr count-map))]
                 [sexp (call-with-input-file readme-path read)]
                 [updated (update-readme-sexp sexp count-map total-count)]
                 ;; Format to string first to avoid corrupting file on error
                 ;; Try custom formatter, fall back to pretty-print
                 [output-str (guard (fmt-err [else
                               ;; Fallback to Chez's pretty-print
                               (call-with-string-output-port
                                 (lambda (port)
                                   (pretty-print updated port)
                                   (newline port)))])
                               (call-with-string-output-port
                                 (lambda (port)
                                   (pretty-write-sexp updated port 0)
                                   (newline port))))])
            ;; Only write if formatting succeeded
            (call-with-output-file readme-path
              (lambda (port)
                (display output-str port))
              'replace)
            (printf "Updated ~a (total: ~a tests)\n" readme-path total-count)
            #t)))))

(define (update-all-readme!)
  (doc 'description "Update test counts in all lattice skill README.sexp files")
  (doc 'export #t)
  (let* ([base "lattice/"]
         [skills (guard (e [else '()])
                   (filter (lambda (d)
                             (and (> (string-length d) 0)
                                  (not (char=? (string-ref d 0) #\.))
                                  (file-directory? (string-append base d))
                                  (file-exists? (string-append base d "/README.sexp"))))
                           (directory-list base)))]
         [results (map (lambda (skill)
                         (cons skill (update-skill-readme! skill)))
                       (list-sort string<? skills))]
         [updated (filter cdr results)]
         [failed (filter (lambda (r) (not (cdr r))) results)])
    (printf "\n────────────────────────────────────────────────────────────\n")
    (printf "Updated ~a README files\n" (length updated))
    (when (> (length failed) 0)
      (printf "Failed: ~a\n" (map car failed)))
    (length updated)))

(define (check-readme-drift skill-name)
  (doc 'description "Check if README.sexp test counts are out of date")
  (doc 'param 'skill-name "Skill name as string or symbol")
  (doc 'returns "List of (module documented actual) for mismatches")
  (doc 'export #t)
  (let* ([name (if (symbol? skill-name)
                   (symbol->string skill-name)
                   skill-name)]
         [dir (string-append "lattice/" name)]
         [readme-path (string-append dir "/README.sexp")]
         [count-map (build-test-count-map dir)])
    (guard (e [else '()])
      (if (not (file-exists? readme-path))
          '()
          (let* ([sexp (call-with-input-file readme-path read)]
                 [modules-entry (assq 'modules sexp)]
                 [modules (if (and modules-entry (pair? (cdr modules-entry)))
                              (cadr modules-entry)
                              '())])
            ;; Extract documented counts and compare with actual
            (filter
             values
             (map (lambda (entry)
                    ;; Entry format: (module.ss "description - N tests")
                    (and (pair? entry)
                         (symbol? (car entry))
                         (pair? (cdr entry))
                         (string? (cadr entry))
                         (let* ([module-name (symbol->string (car entry))]
                                [desc (cadr entry)]
                                [actual-entry (assoc module-name count-map)]
                                [actual (if actual-entry (cdr actual-entry) #f)]
                                [doc-count (extract-count-from-string desc)])
                           (and actual doc-count
                                (not (= doc-count actual))
                                (list module-name doc-count actual)))))
                  modules)))))))

(define (extract-count-from-string str)
  (doc 'description "Extract the test count number from a description string")
  (let* ([len (string-length str)])
    (let loop ([i 0])
      (cond
        [(>= i (- len 5)) #f]  ; Need room for "N test"
        [(and (char=? (string-ref str i) #\-)
              (< (+ i 2) len))
         ;; Found dash, look for number - but continue searching if pattern fails
         (let ([result
                (let num-loop ([j (+ i 1)])
                  (cond
                    [(>= j len) #f]
                    [(char-numeric? (string-ref str j))
                     ;; Found start of number
                     (let end-loop ([k j])
                       (cond
                         [(>= k len) (string->number (substring str j k))]
                         [(char-numeric? (string-ref str k)) (end-loop (+ k 1))]
                         [(and (< (+ k 5) len)
                               (or (string-prefix? " test" (substring str k len))
                                   (string-prefix? " tests" (substring str k len))))
                          (string->number (substring str j k))]
                         [else #f]))]
                    [(char-whitespace? (string-ref str j)) (num-loop (+ j 1))]
                    [else #f]))])
           ;; If this dash didn't match, continue searching for next dash
           (or result (loop (+ i 1))))]
        [else (loop (+ i 1))]))))

(doc 'section 'repl-interface)

(printf "test-count.ss loaded — Test Counting Utility\n")
(printf "  (print-test-summary)      - Print full summary for all lattice skills\n")
(printf "  (test-count 'skill)       - Get counts for specific skill\n")
(printf "  (lattice-test-summary)    - Return summary as list\n")
(printf "  (check-readme-drift 'sk)  - Check if README counts are stale\n")
(printf "  (update-skill-readme! 's) - Update a skill's README.sexp\n")
(printf "  (update-all-readme!)      - Update all README.sexp files\n")
