;;; shell/lens/test-finder.ss — Test Discovery Module
;;;
;;; Finds tests related to functions using naming conventions.
;;; Convention: test-<module>.ss files test symbols from <module>.ss
;;;
;;; This is Shell code: reads filesystem, scans files.
;;;
;;; Usage:
;;;   (find-tests-for 'symbol-name)     - Find tests for a symbol
;;;   (find-test-file "module.ss")      - Find test file for module
;;;   (list-all-tests)                  - List all test files
;;;
;;; Dependencies:
;;;   shell/tools/index.ss (symbol index)

;;; ============================================================
;;; Test Discovery
;;; ============================================================

;;; find-test-file : String -> String | #f
;;; Given a module path, find its corresponding test file.
;;; Convention: test file is named test-<basename>.ss in same or tests/ dir.
(define (find-test-file module-path)
  (let* ([basename (path-basename module-path)]
         [dirname (path-dirname module-path)]
         [test-name (string-append "test-" basename)]
         [candidates
          (list
           ;; Same directory
           (string-append dirname "/" test-name)
           ;; tests/ subdirectory
           (string-append dirname "/tests/" test-name)
           ;; Parent tests/ directory
           (let ([parent (path-dirname dirname)])
                (string-append parent "/tests/" test-name)))])
        (find (lambda (p) (file-exists? p)) candidates)))

;;; path-basename : String -> String
;;; Extract filename from path.
(define (path-basename path)
  (let ([idx (string-rindex path #\/)])
       (if idx
           (substring path (+ idx 1) (string-length path))
           path)))

;;; path-dirname : String -> String
;;; Extract directory from path.
(define (path-dirname path)
  (let ([idx (string-rindex path #\/)])
       (if idx
           (substring path 0 idx)
           ".")))

;;; string-rindex : String × Char -> Nat | #f
;;; Find rightmost occurrence of char in string.
(define (string-rindex str ch)
  (let loop ([i (- (string-length str) 1)])
       (cond
        [(< i 0) #f]
        [(char=? (string-ref str i) ch) i]
        [else (loop (- i 1))])))

;;; ============================================================
;;; Test Content Analysis
;;; ============================================================

;;; test-file-references-symbol? : String × Symbol -> Boolean
;;; Check if a test file references the given symbol.
(define (test-file-references-symbol? test-path sym)
  (guard (e [else #f])
         (call-with-input-file test-path
                               (lambda (port)
                                       (let loop ()
                                            (let ([expr (guard (e [else #f]) (read port))])
                                                 (cond
                                                  [(eof-object? expr) #f]
                                                  [(not expr) #f]
                                                  [(contains-symbol? expr sym) #t]
                                                  [else (loop)])))))))

;;; contains-symbol? : S-expr × Symbol -> Boolean
;;; Check if symbol appears anywhere in expression.
(define (contains-symbol? expr sym)
  (cond
   [(eq? expr sym) #t]
   [(pair? expr)
    (or (contains-symbol? (car expr) sym)
        (contains-symbol? (cdr expr) sym))]
   [else #f]))

;;; extract-test-names : String -> (List Symbol)
;;; Extract names of test procedures from a test file.
;;; Looks for (define (test-...) ...) and (test "..." ...) patterns.
(define (extract-test-names test-path)
  (guard (e [else '()])
         (call-with-input-file test-path
                               (lambda (port)
                                       (let loop ([tests '()])
                                            (let ([expr (guard (e [else #f]) (read port))])
                                                 (cond
                                                  [(eof-object? expr) (reverse tests)]
                                                  [(not expr) (reverse tests)]
                                                  [else
                                                   (let ([test-name (extract-test-name-from-expr expr)])
                                                        (if test-name
                                                            (loop (cons test-name tests))
                                                            (loop tests)))])))))))

;;; extract-test-name-from-expr : S-expr -> Symbol | #f
;;; Extract test name from a test definition or test call.
(define (extract-test-name-from-expr expr)
  (cond
   [(not (pair? expr)) #f]
   
   ;; (define (test-xyz ...) ...)
   [(and (eq? (car expr) 'define)
         (pair? (cdr expr))
         (pair? (cadr expr))
         (symbol? (caadr expr))
         (string-prefix? (symbol->string (caadr expr)) "test-"))
    (caadr expr)]
   
   ;; (test "description" ...)
   [(and (eq? (car expr) 'test)
         (pair? (cdr expr))
         (string? (cadr expr)))
    (string->symbol (string-append "test:" (cadr expr)))]
   
   ;; (test-case "description" ...)
   [(and (eq? (car expr) 'test-case)
         (pair? (cdr expr))
         (string? (cadr expr)))
    (string->symbol (string-append "test-case:" (cadr expr)))]
   
   [else #f]))

;;; string-prefix? : String × String -> Boolean
(define (string-prefix? str prefix)
  (let ([str-len (string-length str)]
        [pre-len (string-length prefix)])
       (and (>= str-len pre-len)
            (string=? (substring str 0 pre-len) prefix))))

;;; ============================================================
;;; Symbol to Test Mapping
;;; ============================================================

;;; find-tests-for : Symbol -> (List (file line test-names))
;;; Find all tests that reference the given symbol.
(define (find-tests-for sym)
  ;; First, try to find the symbol's definition location
  (let ([entry (guard (e [else #f])
                      (and (top-level-bound? 'index-lookup)
                           (procedure? index-lookup)
                           (index-lookup sym)))])
       (if entry
           ;; Symbol found - look for tests in its module's test file
           (let* ([file (cdr (assq 'file entry))]
                  [test-file (find-test-file file)])
                 (if test-file
                     (if (test-file-references-symbol? test-file sym)
                         (let ([tests (extract-test-names test-file)])
                              (list (list test-file 1 tests)))
                         '())
                     ;; No specific test file - search all test files
                     (search-all-tests-for sym)))
           ;; Symbol not in index - search all test files
           (search-all-tests-for sym))))

;;; search-all-tests-for : Symbol -> (List (file line test-names))
;;; Search all test files for references to symbol.
(define (search-all-tests-for sym)
  (let ([test-files (find-all-test-files)])
       (filter-map
        (lambda (test-path)
                (and (test-file-references-symbol? test-path sym)
                     (list test-path 1 (extract-test-names test-path))))
        test-files)))

;;; find-all-test-files : -> (List String)
;;; Find all test-*.ss files in the codebase.
(define (find-all-test-files)
  (append
   (find-test-files-in "core")
   (find-test-files-in "shell")
   (find-test-files-in "user")
   (find-test-files-in "tests")))

;;; find-test-files-in : String -> (List String)
;;; Recursively find test files in a directory.
(define (find-test-files-in dir)
  (define (skip-dir? name)
    (or (string=? name ".")
        (string=? name "..")
        (string=? name ".git")
        (string=? name "node_modules")))
  
  (guard (e [else '()])
         (let loop ([pending (list dir)] [results '()])
              (if (null? pending)
                  results
                  (let ([current (car pending)]
                        [rest (cdr pending)])
                       (cond
                        [(not (file-exists? current))
                         (loop rest results)]
                        [(file-directory? current)
                         (let ([entries (guard (e [else '()])
                                               (directory-list current))])
                              (let ([full-paths
                                     (filter-map-test
                                      (lambda (name)
                                              (and (not (skip-dir? name))
                                                   (string-append current "/" name)))
                                      entries)])
                                   (loop (append full-paths rest) results)))]
                        [(and (string-suffix? ".ss" current)
                              (is-test-file? current))
                         (loop rest (cons current results))]
                        [else
                         (loop rest results)]))))))

;;; filter-map-test : (α -> β | #f) × (List α) -> (List β)
(define (filter-map-test f lst)
  (let loop ([lst lst] [acc '()])
       (if (null? lst)
           (reverse acc)
           (let ([result (f (car lst))])
                (if result
                    (loop (cdr lst) (cons result acc))
                    (loop (cdr lst) acc))))))

;;; is-test-file? : String -> Boolean
;;; Check if path is a test file based on naming convention.
(define (is-test-file? path)
  (let ([basename (path-basename path)])
       (string-prefix? basename "test-")))

;;; string-suffix? is provided by shell/string-utils.ss with signature (suffix str)

;;; list-all-tests : -> void
;;; Display all test files in the codebase.
(define (list-all-tests)
  (let ([tests (find-all-test-files)])
       (display "\n")
       (display "  Test Files in Codebase\n")
       (display "  ────────────────────────────────\n")
       (printf "  Total: ~a test files\n\n" (length tests))
       (for-each
        (lambda (path)
                (printf "    ~a\n" path))
        (list-sort string<? tests))
       (display "\n")))

(display "Test finder module loaded.\n")
