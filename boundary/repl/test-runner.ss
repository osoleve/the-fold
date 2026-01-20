(doc 'module 'test-runner)
(doc 'description "Test discovery and runner for quick testing during development")
(doc 'layer 'boundary)
(doc 'purity 'partial)

(doc 'section 'path-utilities)

(doc path-directory 'type "String -> String")
(doc path-directory 'description "Extract directory from a path (everything before last /)")
(define (path-directory path)
  (let ([idx (string-rindex path #\/)])
    (if idx
        (substring path 0 idx)
        ".")))

(doc path-basename 'type "String -> String")
(doc path-basename 'description "Extract filename from a path (everything after last /)")
(define (path-basename path)
  (let ([idx (string-rindex path #\/)])
    (if idx
        (substring path (+ idx 1) (string-length path))
        path)))

(doc path-stem 'type "String -> String")
(doc path-stem 'description "Extract filename without extension")
(define (path-stem path)
  (let* ([base (path-basename path)]
         [idx (string-rindex base #\.)])
    (if idx
        (substring base 0 idx)
        base)))

(doc string-rindex 'type "String Char -> Int | #f")
(doc string-rindex 'description "Find last index of char in string")
(define (string-rindex str char)
  (let loop ([i (- (string-length str) 1)])
    (cond
     [(< i 0) #f]
     [(char=? (string-ref str i) char) i]
     [else (loop (- i 1))])))

(doc path-join 'type "String String -> String")
(doc path-join 'description "Join two path components")
(define (path-join dir file)
  (if (string=? dir ".")
      file
      (string-append dir "/" file)))

(doc 'section 'test-discovery)

(doc find-test-files 'type "String -> (List String)")
(doc find-test-files 'description "Find test files associated with a module. Returns list of existing test file paths.")
(define (find-test-files module-path)
  (doc 'description "Discovers tests using these patterns (in order):
  1. test-<name>.ss in same directory
  2. <name>-test.ss in same directory
  3. tests/test-<name>.ss subdirectory")
  (let* ([dir (path-directory module-path)]
         [stem (path-stem module-path)]
         [candidates (list
                      (path-join dir (string-append "test-" stem ".ss"))
                      (path-join dir (string-append stem "-test.ss"))
                      (path-join (path-join dir "tests")
                                 (string-append "test-" stem ".ss")))])
    (filter file-exists? candidates)))

(doc 'section 'test-running)

(doc run-test-file 'type "String -> Boolean")
(doc run-test-file 'description "Run a test file. Returns #t if successful, #f on error")
(define (run-test-file path)
  (display (format "Running: ~a\n" path))
  (guard (e [else
             (display (format "  ERROR: ~a\n"
                              (if (message-condition? e)
                                  (condition-message e)
                                  e)))
             #f])
    (load path)
    (display "  OK\n")
    #t))

(doc 'section 'public-api)

(doc test-module 'type "String -> Void")
(doc test-module 'description "Find and run tests associated with a module")
(doc test-module 'example "(test-module \"boundary/bbs/ops.ss\")")
(define (test-module module-path)
  (let ([test-files (find-test-files module-path)])
    (cond
     [(null? test-files)
      (display (format "No tests found for ~a\n" module-path))
      (display "Looked for:\n")
      (let* ([dir (path-directory module-path)]
             [stem (path-stem module-path)])
        (display (format "  ~a/test-~a.ss\n" dir stem))
        (display (format "  ~a/~a-test.ss\n" dir stem))
        (display (format "  ~a/tests/test-~a.ss\n" dir stem)))]
     [else
      (display (format "Found ~a test file(s) for ~a:\n"
                       (length test-files) module-path))
      (let ([results (map run-test-file test-files)])
        (newline)
        (let ([passed (length (filter (lambda (x) x) results))]
              [total (length results)])
          (display (format "Results: ~a/~a passed\n" passed total))))])))

(doc test-dir 'type "String -> Void")
(doc test-dir 'description "Find and run all test files in a directory")
(define (test-dir dir-path)
  (let ([files (directory-list dir-path)])
    (let ([test-files (filter
                       (lambda (f)
                         (and (> (string-length f) 5)
                              (string=? (substring f 0 5) "test-")
                              (let ([len (string-length f)])
                                (string=? (substring f (- len 3) len) ".ss"))))
                       files)])
      (if (null? test-files)
          (display (format "No test files found in ~a\n" dir-path))
          (begin
            (display (format "Found ~a test file(s) in ~a:\n"
                             (length test-files) dir-path))
            (for-each
             (lambda (f)
               (run-test-file (path-join dir-path f)))
             test-files))))))

(doc 'section 'startup-banner)

(unless (and (top-level-bound? '*quiet*) *quiet*)
  (display "Test runner ready.\n")
  (display "  (test-module \"path\")  - Run tests for module\n")
  (display "  (test-dir \"path\")     - Run all tests in directory\n"))
