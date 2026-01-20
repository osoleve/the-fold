(load "core/base/prelude.ss")
(load "boundary/tools/project-status.ss")

(doc 'module 'test-project-status)
(doc 'description "Tests for Project Status Module")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'note "Run from ccverse root: scheme --script boundary/test-project-status.ss")

(doc 'section 'test-framework)

(doc 'description "Test counter")
(define *tests-run* 0)
(define *tests-passed* 0)

(define (test name passed?)
  (doc 'type "String x Boolean -> void")
  (doc 'description "Run a single test")
  (set! *tests-run* (+ *tests-run* 1))
  (display "  ")
  (display name)
  (display ": ")
  (if passed?
      (begin
       (set! *tests-passed* (+ *tests-passed* 1))
       (display "PASS\n"))
      (display "FAIL\n")))

;;; test-equal : String x Any x Any -> void
;;; Test that two values are equal.
(define (test-equal name expected actual)
  (test name (equal? expected actual)))

;;; test-true : String x Any -> void
;;; Test that value is truthy.
(define (test-true name value)
  (test name value))

;;; test-alist-has-key : String x Symbol x Alist -> void
;;; Test that an alist has a specific key.
(define (test-alist-has-key name key alist)
  (test name (assq key alist)))

(doc 'section 'tests)

(display "\n")
(display "+====+\n")
(display "|          boundary/tools/project-status.ss Test Suite                  |\n")
(display "+====+\n")
(display "\n")

(doc 'section 'path-utilities-tests)

(display "Test 1: Path utilities\n")

(test-equal "normalize-path converts backslashes"
            "foo/bar/baz"
            (normalize-path "foo\\bar\\baz"))

(test-equal "normalize-path preserves forward slashes"
            "foo/bar/baz"
            (normalize-path "foo/bar/baz"))

(test-equal "path-join combines paths"
            "foo/bar"
            (path-join "foo" "bar"))

(test-true "ends-with? detects suffix"
           (ends-with? "test-foo.ss" ".ss"))

(test-true "ends-with? rejects non-suffix"
           (not (ends-with? "test-foo.ss" ".txt")))

(test-true "starts-with? detects prefix"
           (starts-with? "test-foo.ss" "test-"))

(test-true "starts-with? rejects non-prefix"
           (not (starts-with? "foo-test.ss" "test-")))

(doc 'section 'string-utilities-tests)

(display "\nTest 2: String utilities\n")

(test-equal "string-trim removes leading spaces"
            "hello"
            (string-trim "   hello"))

(test-equal "string-trim handles no spaces"
            "hello"
            (string-trim "hello"))

(test-equal "string-trim handles empty"
            ""
            (string-trim "   "))

(test-equal "string-split on delimiter"
            '("a" "b" "c")
            (string-split "a|b|c" #\|))

(test-equal "string-split with empty parts"
            '("a" "b")
            (string-split "a||b" #\|))

(doc 'section 'test-file-detection-tests)

(display "\nTest 3: Test file detection\n")

(test-true "is-test-file? detects test- prefix"
           (is-test-file? "test-foo.ss"))

(test-true "is-test-file? detects -test suffix"
           (is-test-file? "foo-test.ss"))

(test-true "is-test-file? rejects non-test"
           (not (is-test-file? "foo.ss")))

(test-true "is-test-file? rejects non-scheme"
           (not (is-test-file? "test-foo.txt")))

(doc 'section 'scheme-file-detection-tests)

(display "\nTest 4: Scheme file detection\n")

(test-true "is-scheme-file? detects .ss"
           (is-scheme-file? "foo.ss"))

(test-true "is-scheme-file? rejects .txt"
           (not (is-scheme-file? "foo.txt")))

(doc 'section 'blank-comment-detection-tests)

(display "\nTest 5: Blank and comment detection\n")

(test-true "is-blank-or-comment? detects empty"
           (is-blank-or-comment? ""))

(test-true "is-blank-or-comment? detects whitespace"
           (is-blank-or-comment? "   "))

(test-true "is-blank-or-comment? detects comment"
           (is-blank-or-comment? ";;; this is a comment"))

(test-true "is-blank-or-comment? detects indented comment"
           (is-blank-or-comment? "   ; comment"))

(test-true "is-blank-or-comment? rejects code"
           (not (is-blank-or-comment? "(define x 5)")))

(doc 'section 'git-status-parsing-tests)

(display "\nTest 6: Git status parsing\n")

(let ([clean-result (parse-git-status "")])
     (test-equal "clean status detected"
                 "clean"
                 (cdr (assq 'status clean-result))))

(let ([dirty-result (parse-git-status " M modified.ss\n")])
     (test-equal "dirty status detected"
                 "dirty"
                 (cdr (assq 'status dirty-result))))

(let ([untracked-result (parse-git-status "?? new-file.ss\n")])
     (test-true "untracked file captured"
                (member "new-file.ss" (cdr (assq 'untracked untracked-result)))))

(doc 'section 'commit-parsing-tests)

(display "\nTest 7: Commit parsing\n")

(let ([commit (parse-commit-line "abc123|Fix bug|2 hours ago")])
     (test-equal "commit hash parsed"
                 "abc123"
                 (cdr (assq 'hash commit)))
     (test-equal "commit message parsed"
                 "Fix bug"
                 (cdr (assq 'message commit)))
     (test-equal "commit time parsed"
                 "2 hours ago"
                 (cdr (assq 'time commit))))

(doc 'section 'integration-tests)

(display "\nTest 8: Integration tests (counting in current directory)\n")

(let ([test-info (count-tests ".")])
     (test-alist-has-key "count-tests returns files" 'files test-info)
     (test-alist-has-key "count-tests returns cases" 'cases test-info)
     (test-true "found at least 1 test file"
                (> (cdr (assq 'files test-info)) 0)))

(let ([line-info (count-lines ".")])
     (test-alist-has-key "count-lines returns files" 'files line-info)
     (test-alist-has-key "count-lines returns code-lines" 'code-lines line-info)
     (test-true "found at least 1 file"
                (> (cdr (assq 'files line-info)) 0))
     (test-true "found at least 100 lines"
                (> (cdr (assq 'code-lines line-info)) 100)))

(let ([git-info (git-status-info)])
     (test-alist-has-key "git-status-info returns status" 'status git-info)
     (test-alist-has-key "git-status-info returns modified" 'modified git-info))

(let ([commits (recent-commits 3)])
     (test-true "recent-commits returns list"
                (list? commits))
     (test-true "recent-commits has at least 1 commit"
                (> (length commits) 0)))

(doc 'section 'full-status-test)

(display "\nTest 9: Full project status\n")

(let ([status (project-status)])
     (test-alist-has-key "status has files" 'files status)
     (test-alist-has-key "status has code-lines" 'code-lines status)
     (test-alist-has-key "status has test-files" 'test-files status)
     (test-alist-has-key "status has test-cases" 'test-cases status)
     (test-alist-has-key "status has git-status" 'git-status status)
     (test-alist-has-key "status has last-commit" 'last-commit status))

(doc 'section 'health-check-test)

(display "\nTest 10: Health check\n")

(let ([h (health)])
     (test-true "health returns symbol"
                (symbol? h))
     (test-true "health is valid value"
                (memq h '(healthy dirty unknown))))

(doc 'section 'summary)

(display "\n")
(display "+====+\n")
(display "|                       TEST SUMMARY                           |\n")
(display "+====+\n")
(display (format "  Total:  ~a tests\n" *tests-run*))
(display (format "  Passed: ~a\n" *tests-passed*))
(display (format "  Failed: ~a\n" (- *tests-run* *tests-passed*)))

(if (= *tests-run* *tests-passed*)
    (display "\n  All tests passed!\n\n")
    (begin
     (display "\n  Some tests failed!\n\n")
     (exit 1)))

(doc 'section 'demo)

(display "Demo: Running project-status-report on ccverse...\n")
(project-status-report)
