(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
;;; @module meta/promotion
;;; @requires prelude meta/readme-audit
(require 'prelude)
(require 'meta/readme-audit)

(doc 'module 'meta/promotion)
(doc 'description "Lattice promotion protocol. Automated quality gate that checks whether
code in user/ meets the standards required for promotion to lattice/.
Runs structural, purity, testing, documentation, and boundary checks.")
(doc 'layer 'lattice)
(doc 'purity 'partial)  ; File I/O for scanning source directories

;;; ============================================================
;;; File Utilities
;;; ============================================================

(doc 'section 'file-utils)

;;; Read entire file as string. Returns #f if unreadable.
(define (read-file-string path)
  (guard (e [#t #f])
    (let* ([port (open-input-file path)]
           [contents (get-string-all port)])
      (close-input-port port)
      (if (eof-object? contents) "" contents))))

;;; Check if a file exists by attempting to open it.
(define (file-exists? path)
  (guard (e [#t #f])
    (let ([port (open-input-file path)])
      (close-input-port port)
      #t)))

;;; List .ss files in a directory (non-recursive).
(define (list-scheme-files dir)
  (guard (e [#t '()])
    (let ([entries (directory-list dir)])
      (filter (lambda (f) (string-suffix? f ".ss")) entries))))

;;; List all files matching a predicate in a directory.
(define (list-files-matching dir pred)
  (guard (e [#t '()])
    (filter pred (directory-list dir))))

(define (string-suffix? str suffix)
  (let ([slen (string-length str)]
        [plen (string-length suffix)])
    (and (>= slen plen)
         (string=? (substring str (- slen plen) slen) suffix))))

;;; string-contains? for simple substring search.
(define (str-contains? haystack needle)
  (let ([hlen (string-length haystack)]
        [nlen (string-length needle)])
    (let loop ([i 0])
      (cond
        [(> (+ i nlen) hlen) #f]
        [(string=? (substring haystack i (+ i nlen)) needle) #t]
        [else (loop (+ i 1))]))))

;;; ============================================================
;;; Individual Checks
;;; ============================================================

(doc 'section 'checks)

;;; Check: manifest.sexp exists and parses.
(doc 'type '(-> String (List CheckResult)))
(doc 'description "Verify manifest.sexp exists and has required fields.")
(define (check-manifest dir)
  (let ([path (string-append dir "/manifest.sexp")])
    (if (not (file-exists? path))
        (list (make-issue 'error 'manifest "manifest.sexp not found"))
        (let ([contents (read-file-string path)])
          (if (not contents)
              (list (make-issue 'error 'manifest "manifest.sexp unreadable"))
              (guard (e [#t (list (make-issue 'error 'manifest
                                   "manifest.sexp failed to parse"))])
                (let ([sexp (read (open-input-string contents))])
                  (check-manifest-fields sexp))))))))

(define (check-manifest-fields sexp)
  (if (not (pair? sexp))
      (list (make-issue 'error 'manifest "manifest.sexp is not a valid S-expression"))
      (let* ([skill-issue
              (if (eq? 'skill (car sexp)) '()
                  (list (make-issue 'error 'manifest
                           "manifest must start with (skill name ...)")))]
             [body (if (and (pair? sexp) (pair? (cddr sexp))) (cddr sexp) '())]
             [field-issues
              (fold-left
               (lambda (acc required)
                 (if (assq required body) acc
                     (cons (make-issue 'error 'manifest
                              (string-append "missing required field: "
                                             (symbol->string required)))
                           acc)))
               '()
               '(version tier path purity deps description exports modules))]
             [issues (append skill-issue field-issues)])
        (if (null? issues)
            (list (make-issue 'pass 'manifest "manifest.sexp is valid"))
            (reverse issues)))))

;;; Check: README.sexp exists and validates.
(doc 'type '(-> String (List CheckResult)))
(define (check-readme dir)
  (let ([path (string-append dir "/README.sexp")])
    (if (not (file-exists? path))
        (list (make-issue 'warning 'readme "README.sexp not found (recommended)"))
        (guard (e [#t (list (make-issue 'error 'readme "README.sexp failed to parse"))])
          (let* ([contents (read-file-string path)]
                 [sexp (read (open-input-string contents))]
                 [issues (readme-validate sexp)])
            (if (readme-valid? issues)
                (list (make-issue 'pass 'readme "README.sexp validates"))
                (map (lambda (issue)
                       (make-issue
                         (case (car issue)
                           [(error) 'error]
                           [(warning) 'warning]
                           [else 'info])
                         'readme
                         (cadr issue)))
                     issues)))))))

;;; Check: purity annotations present on all source files.
(doc 'type '(-> String (List CheckResult)))
(define (check-purity dir)
  (let* ([files (list-scheme-files dir)]
         [source-files (filter (lambda (f)
                                 (and (not (string-prefix? f "test-"))
                                      (not (string-prefix? f "bench-"))))
                               files)])
    (if (null? source-files)
        (list (make-issue 'warning 'purity "no source files found"))
        (let ([issues
               (fold-left
                (lambda (acc file)
                  (let ([contents (read-file-string (string-append dir "/" file))])
                    (if (and contents (not (str-contains? contents "(doc 'purity '")))
                        (cons (make-issue 'error 'purity
                                 (string-append file ": missing purity annotation"))
                              acc)
                        acc)))
                '()
                source-files)])
          (if (null? issues)
              (list (make-issue 'pass 'purity
                       (string-append "all " (number->string (length source-files))
                                      " source files have purity annotations")))
              (reverse issues))))))

;;; Check: test files exist for source modules.
(doc 'type '(-> String (List CheckResult)))
(define (check-tests dir)
  (let* ([files (list-scheme-files dir)]
         [source-files (filter (lambda (f)
                                 (and (not (string-prefix? f "test-"))
                                      (not (string-prefix? f "bench-"))
                                      (not (string=? f "manifest.sexp"))))
                               files)]
         [test-files (filter (lambda (f) (string-prefix? f "test-")) files)])
    (if (null? source-files)
        (list (make-issue 'warning 'tests "no source files found"))
        (let* ([result
                (fold-left
                 (lambda (acc src)
                   (let ([tested (car acc)]
                         [issues (cdr acc)]
                         [expected-test (string-append "test-" src)])
                     (if (member expected-test test-files)
                         (cons (+ tested 1) issues)
                         (cons tested
                               (cons (make-issue 'warning 'tests
                                        (string-append src ": no matching test-" src))
                                     issues)))))
                 (cons 0 '())
                 source-files)]
               [tested (car result)]
               [issues (cdr result)])
          (cons (make-issue (if (> tested 0) 'pass 'warning) 'tests
                   (string-append (number->string tested) "/"
                                  (number->string (length source-files))
                                  " source files have tests"))
                (reverse issues))))))

;;; Check: module headers (@module, @requires, require).
(doc 'type '(-> String (List CheckResult)))
(define (check-headers dir)
  (let* ([files (list-scheme-files dir)]
         [source-files (filter (lambda (f)
                                 (and (not (string-prefix? f "test-"))
                                      (not (string-prefix? f "bench-"))))
                               files)])
    (if (null? source-files)
        (list (make-issue 'warning 'headers "no source files found"))
        (let ([issues
               (fold-left
                (lambda (acc file)
                  (let ([contents (read-file-string (string-append dir "/" file))])
                    (if (not contents) acc
                        (let* ([acc (if (str-contains? contents ";;; @module") acc
                                        (cons (make-issue 'warning 'headers
                                                 (string-append file ": missing @module annotation"))
                                              acc))]
                               [acc (if (or (str-contains? contents "(require '")
                                            (str-contains? contents "(doc 'module '"))
                                        acc
                                        (cons (make-issue 'warning 'headers
                                                 (string-append file ": no require or module doc"))
                                              acc))])
                          acc))))
                '()
                source-files)])
          (if (null? issues)
              (list (make-issue 'pass 'headers "all source files have proper headers"))
              (reverse issues))))))

;;; Check: no boundary layer violations (no loading from boundary/).
(doc 'type '(-> String (List CheckResult)))
(define (check-boundaries dir)
  (let* ([files (list-scheme-files dir)]
         [source-files (filter (lambda (f) (not (string-prefix? f "test-"))) files)]
         [violations
          (fold-left
           (lambda (acc file)
             (let ([contents (read-file-string (string-append dir "/" file))])
               (if (not contents) acc
                   (let* ([acc (if (str-contains? contents "(load \"boundary/")
                                   (cons (make-issue 'error 'boundaries
                                            (string-append file ": loads from boundary/"))
                                         acc)
                                   acc)]
                          [acc (if (str-contains? contents "(require 'boundary/")
                                   (cons (make-issue 'error 'boundaries
                                            (string-append file ": requires from boundary/"))
                                         acc)
                                   acc)])
                     acc))))
           '()
           source-files)])
    (if (null? violations)
        (list (make-issue 'pass 'boundaries "no boundary layer violations"))
        (reverse violations))))

;;; Check: no impurity markers in source files.
;;; Looks for common I/O patterns that shouldn't be in pure lattice code.
(doc 'type '(-> String (List CheckResult)))
(define (check-impurity-markers dir)
  (let* ([files (list-scheme-files dir)]
         [source-files (filter (lambda (f)
                                 (and (not (string-prefix? f "test-"))
                                      (not (string-prefix? f "bench-"))))
                               files)]
         ;; Only check for actual calls: "(marker" pattern, not substring matches
         ;; in doc strings or comments.
         [markers '("(open-input-file" "(open-output-file" "(with-output-to-file"
                     "(delete-file" "(system " "(call-with-port"
                     "(display " "(display)" "(printf " "(printf\n"
                     "(current-input-port)" "(current-output-port)")]
         [issues
          (fold-left
           (lambda (acc file)
             (let ([contents (read-file-string (string-append dir "/" file))])
               (if (not contents) acc
                   (fold-left
                    (lambda (inner-acc marker)
                      (if (str-contains? contents marker)
                          (cons (make-issue 'warning 'impurity
                                   (string-append file ": contains '" marker "'"))
                                inner-acc)
                          inner-acc))
                    acc
                    markers))))
           '()
           source-files)])
    (if (null? issues)
        (list (make-issue 'pass 'impurity "no impurity markers in source files"))
        (reverse issues))))

;;; ============================================================
;;; Issue Record
;;; ============================================================

(doc 'section 'issues)

;;; Issue: (severity category message)
;;; severity: 'error | 'warning | 'info | 'pass
(define (make-issue severity category message)
  (list severity category message))

(define (issue-severity i) (car i))
(define (issue-category i) (cadr i))
(define (issue-message i) (caddr i))

(define (string-prefix? str prefix)
  (let ([slen (string-length str)]
        [plen (string-length prefix)])
    (and (>= slen plen)
         (string=? (substring str 0 plen) prefix))))

;;; ============================================================
;;; Promotion Audit (Orchestrator)
;;; ============================================================

(doc 'section 'audit)

(doc 'type '(-> String Alist))
(doc 'description "Run all promotion checks on a directory. Returns a structured report
with per-category results and an overall readiness verdict.")
(define (promotion-audit dir)
  (let* ([manifest-results  (check-manifest dir)]
         [readme-results    (check-readme dir)]
         [purity-results    (check-purity dir)]
         [test-results      (check-tests dir)]
         [header-results    (check-headers dir)]
         [boundary-results  (check-boundaries dir)]
         [impurity-results  (check-impurity-markers dir)]
         [all-results (append manifest-results readme-results purity-results
                              test-results header-results boundary-results
                              impurity-results)]
         [errors   (filter (lambda (i) (eq? 'error (issue-severity i))) all-results)]
         [warnings (filter (lambda (i) (eq? 'warning (issue-severity i))) all-results)]
         [passes   (filter (lambda (i) (eq? 'pass (issue-severity i))) all-results)])
    (list 'promotion-audit
          (list 'directory dir)
          (list 'ready? (null? errors))
          (list 'errors (length errors))
          (list 'warnings (length warnings))
          (list 'passes (length passes))
          (list 'checks
                (list 'manifest manifest-results)
                (list 'readme readme-results)
                (list 'purity purity-results)
                (list 'tests test-results)
                (list 'headers header-results)
                (list 'boundaries boundary-results)
                (list 'impurity impurity-results)))))

(doc 'type '(-> Alist Boolean))
(doc 'description "Is this code ready for promotion? True if zero errors.")
(define (promotion-ready? audit)
  (cadr (assq 'ready? (cdr audit))))

;;; ============================================================
;;; Pretty Printing
;;; ============================================================

(doc 'section 'display)

(doc 'type '(-> Alist Void))
(doc 'description "Pretty-print a promotion audit report.")
(define (promotion-report audit)
  (let ([dir (cadr (assq 'directory (cdr audit)))]
        [ready? (cadr (assq 'ready? (cdr audit)))]
        [errors (cadr (assq 'errors (cdr audit)))]
        [warnings (cadr (assq 'warnings (cdr audit)))]
        [passes (cadr (assq 'passes (cdr audit)))]
        [checks (cdr (assq 'checks (cdr audit)))])
    (printf "\n  Promotion Audit: ~a\n" dir)
    (printf "  ~a\n\n"
      (if ready? "READY for lattice promotion" "NOT READY — errors must be fixed"))
    ;; Print each category
    (for-each
      (lambda (category)
        (let ([name (car category)]
              [results (cadr category)])
          (for-each
            (lambda (result)
              (let ([sev (issue-severity result)]
                    [msg (issue-message result)])
                (printf "  ~a ~a: ~a\n"
                  (case sev
                    [(error)   "[FAIL]"]
                    [(warning) "[WARN]"]
                    [(pass)    "[ OK ]"]
                    [(info)    "[INFO]"]
                    [else      "[    ]"])
                  name msg)))
            results)))
      checks)
    (printf "\n  Summary: ~a passed, ~a errors, ~a warnings\n\n"
      passes errors warnings)))

;;; ============================================================
;;; Convenience
;;; ============================================================

(doc 'section 'convenience)

(doc 'type '(-> String Void))
(doc 'description "Quick promotion check — run audit and print report.")
(define (promote? dir)
  (promotion-report (promotion-audit dir)))
