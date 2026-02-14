(unless (top-level-bound? 'lattice-describe) (load "lattice/meta/inspect.ss"))
(unless (top-level-bound? 'scan-skill-exports) (load "boundary/meta/exports-io.ss"))
(unless (top-level-bound? 'shell-capture) (load "boundary/io/process.ss"))

(doc 'module 'inspect-io)
(doc 'description "I/O layer for skill inspection — test discovery, execution, and export verification")
(doc 'layer 'boundary)

;;; ====
;;; Test Discovery (I/O — reads directories)
;;; ====

;;; find-test-files : String -> (List String)
;;; Find all test-*.ss files in a directory (non-recursive)
;;; Filters out directories that might match the pattern
(define (find-test-files dir)
  (guard (e [else '()])
         (let ([entries (directory-list dir)])
              (filter (lambda (f)
                              (and (string-starts-with? f "test-")
                                   (string-ends-with? f ".ss")
                                   ;; Verify it's a file, not a directory
                                   (file-regular? (string-append dir "/" f))))
                      entries))))

;;; lattice-tests : Symbol -> (List String)
;;; Get list of test files for a skill
;;; Returns full paths to test-*.ss files in the skill's directory
(define (lattice-tests skill-name)
  (let ([data (kg-skill-data skill-name)])
       (if (not data)
           (begin
             (printf "Skill not found: ~a\n" skill-name)
             '())
           (let* ([path (cdr (or (assq 'path data) '(path . "")))]
                  [test-files (find-test-files path)])
                 (map (lambda (f) (string-append path "/" f))
                      test-files)))))

;;; lattice-tests-pretty : Symbol -> void
;;; Pretty-print test files for a skill
(define (lattice-tests-pretty skill-name)
  (let ([tests (lattice-tests skill-name)])
       (if (null? tests)
           (printf "No tests found for ~a\n" skill-name)
           (begin
             (printf "Tests for ~a (~a files)\n" skill-name (length tests))
             (printf "~a\n\n" (make-string 40 #\-))
             (for-each (lambda (t) (printf "  ~a\n" t)) tests)))))

;;; lt : Symbol -> void
;;; Quick test file listing (follows li, le, lm pattern)
(define (lt skill-name)
  (lattice-tests-pretty skill-name))

;;; lattice-all-tests : -> (List (Pair Symbol (List String)))
;;; Get test files for all skills
(define (lattice-all-tests)
  (map (lambda (skill-name)
               (cons skill-name (lattice-tests skill-name)))
       (kg-skills)))

;;; lattice-tests-summary : -> void
;;; Print summary of test coverage across all skills
(define (lattice-tests-summary)
  (if (not (kg-initialized?))
      (begin
        (printf "Knowledge graph not initialized.\n")
        (printf "Run (lattice-init!) first, or (kg-build!) for just the graph.\n"))
      (begin
        (printf "Test Coverage Summary\n")
        (printf "~a\n\n" (make-string 50 #\=))
        (let* ([all-tests (lattice-all-tests)]
               [with-tests (filter (lambda (e) (not (null? (cdr e)))) all-tests)]
               [without-tests (filter (lambda (e) (null? (cdr e))) all-tests)])
              (printf "Skills with tests: ~a/~a\n\n" (length with-tests) (length all-tests))
              (for-each
               (lambda (entry)
                       (printf "  ~20a ~a test file~a\n"
                               (car entry)
                               (length (cdr entry))
                               (if (= 1 (length (cdr entry))) "" "s")))
               (sort (lambda (a b) (> (length (cdr a)) (length (cdr b)))) with-tests))
              (when (not (null? without-tests))
                    (printf "\nSkills without tests:\n")
                    (for-each
                     (lambda (entry) (printf "  ~a\n" (car entry)))
                     without-tests))))))

;;; ====
;;; Export Verification (I/O — calls scan-skill-exports)
;;; ====

;;; list->set-ex : (List a) -> (List a)
;;; Remove duplicates (for export comparison)
(define (list->set-ex lst)
  (let loop ([lst lst] [seen '()] [acc '()])
    (if (null? lst)
        (reverse acc)
        (if (memq (car lst) seen)
            (loop (cdr lst) seen acc)
            (loop (cdr lst) (cons (car lst) seen) (cons (car lst) acc))))))

;;; lattice-verify-exports : Symbol -> Alist
;;; Compare manifest exports with actual code exports.
;;; Returns: ((manifest-only . (sym ...)) (code-only . (sym ...)) (match . Bool))
(define (lattice-verify-exports skill-name)
  (let ([data (kg-skill-data skill-name)])
       (if (not data)
           (begin
             (printf "Skill not found: ~a\n" skill-name)
             '())
           (let* ([path (cdr (or (assq 'path data) '(path . "")))]
                  [manifest-exports (lattice-skill-exports skill-name)]
                  [scanned (scan-skill-exports path)]
                  [code-exports (append-map cadr scanned)]
                  [manifest-set (list->set-ex manifest-exports)]
                  [code-set (list->set-ex code-exports)]
                  [manifest-only (filter (lambda (x) (not (memq x code-set))) manifest-set)]
                  [code-only (filter (lambda (x) (not (memq x manifest-set))) code-set)])
                 `((manifest-only . ,manifest-only)
                   (code-only . ,code-only)
                   (match . ,(and (null? manifest-only) (null? code-only))))))))

;;; lattice-verify-exports-pretty : Symbol -> void
;;; Pretty-print export verification for a skill
(define (lattice-verify-exports-pretty skill-name)
  (let ([result (lattice-verify-exports skill-name)])
       (when (pair? result)
             (let ([manifest-only (cdr (assq 'manifest-only result))]
                   [code-only (cdr (assq 'code-only result))]
                   [match? (cdr (assq 'match result))])
                  (printf "Export verification for ~a\n" skill-name)
                  (printf "~a\n\n" (make-string 40 #\-))
                  (if match?
                      (printf "Manifest and code exports match\n")
                      (begin
                        (unless (null? manifest-only)
                                (printf "In manifest but not in code:\n")
                                (for-each (lambda (s) (printf "  - ~a\n" s)) manifest-only))
                        (unless (null? code-only)
                                (printf "In code but not in manifest:\n")
                                (for-each (lambda (s) (printf "  - ~a\n" s)) code-only))))))))

;;; lv : Symbol -> void
;;; Quick export verification (follows li, le, lm pattern)
(define (lv skill-name)
  (lattice-verify-exports-pretty skill-name))

;;; ====
;;; Test Execution (I/O — runs shell commands)
;;; ====

;;; run-test-file : String -> 'ok | (error . String)
;;; Run a single test file and return result.
;;; Primary: parse structured [TEST-RESULT ...] line
;;; Fallback: check exit code
(define (run-test-file path)
  (guard (e [else `(error . ,(format "~a" e))])
         (let* ([cmd (format "scheme --script '~a' 2>&1" (shell-escape path))]
                [result (shell-capture-result cmd)]
                [ok? (process-ok? result)]
                [output (process-stdout result)]
                [parsed (parse-test-result-line output)])
               (cond
                [parsed
                 (if (= 0 (cdr (assq 'failed parsed)))
                     'ok
                     `(error . ,(format "~a/~a tests failed"
                                        (cdr (assq 'failed parsed))
                                        (cdr (assq 'total parsed)))))]
                [ok? 'ok]
                [else `(error . ,(truncate-error-output output))]))))

;;; lattice-tests-run : Symbol -> Alist
;;; Run all tests for a skill, returning structured results.
(define (lattice-tests-run skill-name)
  (let ([tests (lattice-tests skill-name)])
       (if (null? tests)
           `((total . 0) (passed . 0) (failed . 0) (files . ()))
           (let loop ([remaining tests]
                      [passed 0]
                      [failed 0]
                      [results '()])
                (if (null? remaining)
                    `((total . ,(length tests))
                      (passed . ,passed)
                      (failed . ,failed)
                      (files . ,(reverse results)))
                    (let* ([test-file (car remaining)]
                           [result (run-test-file test-file)]
                           [success? (eq? result 'ok)])
                          (loop (cdr remaining)
                                (if success? (+ passed 1) passed)
                                (if success? failed (+ failed 1))
                                (cons (cons test-file result) results))))))))

;;; lattice-tests-run-pretty : Symbol -> void
;;; Run tests and display results.
(define (lattice-tests-run-pretty skill-name)
  (printf "Running tests for ~a...\n\n" skill-name)
  (let* ([results (lattice-tests-run skill-name)]
         [total (cdr (assq 'total results))]
         [passed (cdr (assq 'passed results))]
         [failed (cdr (assq 'failed results))]
         [files (cdr (assq 'files results))])
        (if (= total 0)
            (printf "No tests found.\n")
            (begin
              (for-each
               (lambda (entry)
                       (let ([path (car entry)]
                             [status (cdr entry)])
                            (if (eq? status 'ok)
                                (printf "  ~a\n" path)
                                (printf "  ~a\n    ~a\n" path (cdr status)))))
               files)
              (printf "\n~a\n" (make-string 40 #\-))
              (printf "Total: ~a  Passed: ~a  Failed: ~a\n" total passed failed)
              (if (= failed 0)
                  (printf "All tests passed!\n")
                  (printf "~a test~a failed\n" failed (if (= failed 1) "" "s")))))))

;;; ltr : Symbol -> void
;;; Quick test runner (lt + run)
(define (ltr skill-name)
  (lattice-tests-run-pretty skill-name))
