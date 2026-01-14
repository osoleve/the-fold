;;; shell/repl/test-runner.ss — Test discovery and runner
;;;
;;; Provides test-module for quick testing during development:
;;;   (test-module "path/to/module.ss")  - Find and run associated tests
;;;
;;; Discovers tests using these patterns (in order):
;;;   1. test-<name>.ss in same directory
;;;   2. <name>-test.ss in same directory
;;;   3. tests/test-<name>.ss in same directory
;;;
;;; This is Shell code: uses IO.

;;; ====
;;; Path Utilities
;;; ====

;;; path-directory : String -> String
;;; Extract directory from a path (everything before last /).
(define (path-directory path)
  (let ([idx (string-rindex path #\/)])
    (if idx
        (substring path 0 idx)
        ".")))

;;; path-basename : String -> String
;;; Extract filename from a path (everything after last /).
(define (path-basename path)
  (let ([idx (string-rindex path #\/)])
    (if idx
        (substring path (+ idx 1) (string-length path))
        path)))

;;; path-stem : String -> String
;;; Extract filename without extension.
(define (path-stem path)
  (let* ([base (path-basename path)]
         [idx (string-rindex base #\.)])
    (if idx
        (substring base 0 idx)
        base)))

;;; string-rindex : String Char -> Int | #f
;;; Find last index of char in string.
(define (string-rindex str char)
  (let loop ([i (- (string-length str) 1)])
    (cond
     [(< i 0) #f]
     [(char=? (string-ref str i) char) i]
     [else (loop (- i 1))])))

;;; path-join : String String -> String
;;; Join two path components.
(define (path-join dir file)
  (if (string=? dir ".")
      file
      (string-append dir "/" file)))

;;; ====
;;; Test Discovery
;;; ====

;;; find-test-files : String -> (List String)
;;; Find test files associated with a module.
;;; Returns list of existing test file paths.
(define (find-test-files module-path)
  (let* ([dir (path-directory module-path)]
         [stem (path-stem module-path)]
         [candidates (list
                      ;; test-<name>.ss in same directory
                      (path-join dir (string-append "test-" stem ".ss"))
                      ;; <name>-test.ss in same directory
                      (path-join dir (string-append stem "-test.ss"))
                      ;; tests/test-<name>.ss subdirectory
                      (path-join (path-join dir "tests")
                                 (string-append "test-" stem ".ss")))])
    (filter file-exists? candidates)))

;;; ====
;;; Test Running
;;; ====

;;; run-test-file : String -> Boolean
;;; Run a test file. Returns #t if successful, #f on error.
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

;;; ====
;;; Public API
;;; ====

;;; test-module : String -> Void
;;; Find and run tests associated with a module.
;;;
;;; Example:
;;;   (test-module "shell/bbs/ops.ss")
;;;   ; Looks for: shell/bbs/test-ops.ss
;;;   ;            shell/bbs/ops-test.ss
;;;   ;            shell/bbs/tests/test-ops.ss
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

;;; test-dir : String -> Void
;;; Find and run all test files in a directory.
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

;;; ====
;;; Startup Banner (respects *quiet* mode)
;;; ====

(unless (and (top-level-bound? '*quiet*) *quiet*)
  (display "Test runner ready.\n")
  (display "  (test-module \"path\")  - Run tests for module\n")
  (display "  (test-dir \"path\")     - Run all tests in directory\n"))
