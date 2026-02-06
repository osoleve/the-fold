(load "lattice/meta/inspect.ss")
(load "boundary/io/process.ss")

(doc 'module 'inspect-io)
(doc 'description "I/O layer for skill inspection — test execution via shell")
(doc 'layer 'boundary)

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
                                (printf "  ✓ ~a\n" path)
                                (printf "  ✗ ~a\n    ~a\n" path (cdr status)))))
               files)
              (printf "\n~a\n" (make-string 40 #\-))
              (printf "Total: ~a  Passed: ~a  Failed: ~a\n" total passed failed)
              (if (= failed 0)
                  (printf "✓ All tests passed!\n")
                  (printf "✗ ~a test~a failed\n" failed (if (= failed 1) "" "s")))))))

;;; ltr : Symbol -> void
;;; Quick test runner (lt + run)
(define (ltr skill-name)
  (lattice-tests-run-pretty skill-name))
