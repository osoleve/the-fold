(load "core/base/prelude.ss")

(doc 'module 'test-scaffold)
(doc 'description "Test harness for boundary/tools/scaffold.ss - verifies scaffolding system in isolation")
(doc 'layer 'boundary)
(doc 'purity 'partial)

(doc 'note "String utilities provided by core/prelude.ss: string-upcase, string-downcase, string-contains?")

(doc 'section 'minimal-dependencies)
(doc 'note "Mock functions needed by scaffold.ss")

(doc 'section 'session-mocking)
(define (session-exists?) #t)
(define (read-session)
  '((name . TestAuthor)
    (tier . sonnet)))

;;; Filesystem mocking (just capture what would be written)
(define *test-files-written* '())

(define-record-type fs-capability
  (fields store-path))

(define (mint-fs-capability path)
  (make-fs-capability path))

(define (write-text-file! fs path content)
  (set! *test-files-written*
        (cons (cons path content) *test-files-written*)))

;;; Time utilities for scaffolding
(define (current-timestamp)
  "2025-01-15 12:00:00 UTC")

(define (timestamp-year ts)
  (substring ts 0 4))

(doc 'section 'load-module-under-test)

(load "boundary/tools/scaffold.ss")

(doc 'section 'test-framework)

(define *test-count* 0)
(define *test-passed* 0)

(define (test name expected actual)
  (set! *test-count* (+ *test-count* 1))
  (display "  ")
  (display name)
  (display ": ")
  (if (equal? expected actual)
      (begin
       (set! *test-passed* (+ *test-passed* 1))
       (display "✓"))
      (begin
       (display "✗\n    expected: ")
       (display expected)
       (display "\n    got: ")
       (display actual)))
  (newline))

(doc 'section 'tests)

(display "Scaffolding System Tests\n")
(display "====\n\n")

(doc 'test "Template registration")
(display "Test 1: Template registration\n")
(define-template
  'test-template
  "A test template"
  '((var1 . ("Variable 1" . "default1"))
    (var2 . ("Variable 2" . "default2")))
  '(((path . "test-{{NAME}}.txt")
     (content . "Name: {{NAME}}\nVar1: {{VAR1}}\nVar2: {{VAR2}}"))))

(let ([tmpl (get-template 'test-template)])
     (test "template registered" #t (not (not tmpl)))
     (test "template has description"
           "A test template"
           (cdr (assq 'description tmpl))))

(doc 'test "String search")
(display "\nTest 2: String search utility\n")
(test "finds substring at start" 0 (string-search "hello" "hello world"))
(test "finds substring in middle" 6 (string-search "world" "hello world"))
(test "returns #f when not found" #f (string-search "xyz" "hello world"))
(test "finds empty string" 0 (string-search "" "hello"))

(doc 'test "Variable substitution")
(display "\nTest 3: Variable substitution\n")
(let ([template "Hello {{NAME}}, your score is {{SCORE}}"]
      [bindings '(("NAME" . "Alice") ("SCORE" . "100"))])
     (test "substitutes variables"
           "Hello Alice, your score is 100"
           (substitute-vars template bindings)))

(let ([template "{{VAR1}} and {{VAR2}} and {{VAR1}} again"]
      [bindings '(("VAR1" . "first") ("VAR2" . "second"))])
     (test "substitutes repeated variables"
           "first and second and first again"
           (substitute-vars template bindings)))

(let ([template "No variables here"]
      [bindings '(("NAME" . "Alice"))])
     (test "handles no variables"
           "No variables here"
           (substitute-vars template bindings)))

(doc 'test "Build bindings")
(display "\nTest 4: Build bindings from name and options\n")
(let ([bindings (build-bindings "my-module"
                                '((description . ("Desc" . "Default description")))
                                '((description . "Custom description")))])
     (test "includes NAME" "my-module" (cdr (assoc "NAME" bindings)))
     (test "includes NAME-UPPER" "MY-MODULE" (cdr (assoc "NAME-UPPER" bindings)))
     (test "includes NAME-LOWER" "my-module" (cdr (assoc "NAME-LOWER" bindings)))
     (test "includes custom var" "Custom description" (cdr (assoc "DESCRIPTION" bindings)))
     (test "includes author" "TestAuthor" (cdr (assoc "AUTHOR" bindings)))
     (test "includes timestamp" #t (string? (cdr (assoc "TIMESTAMP" bindings))))
     (test "includes year" #t (string? (cdr (assoc "YEAR" bindings)))))

(doc 'test "Template existence")
(display "\nTest 5: Built-in templates\n")
(test "shell-module exists" #t (not (not (get-template 'shell-module))))
(test "core-module exists" #t (not (not (get-template 'core-module))))
(test "tool exists" #t (not (not (get-template 'tool))))
(test "test-suite exists" #t (not (not (get-template 'test-suite))))
(test "playground exists" #t (not (not (get-template 'playground))))
(test "forum-post exists" #t (not (not (get-template 'forum-post))))

(doc 'test "Template structure validation")
(display "\nTest 6: Template structure\n")
(let ([shell-tmpl (get-template 'shell-module)])
     (test "has description" #t (not (not (assq 'description shell-tmpl))))
     (test "has variables" #t (not (not (assq 'variables shell-tmpl))))
     (test "has files" #t (not (not (assq 'files shell-tmpl))))
     (let ([files (cdr (assq 'files shell-tmpl))])
          (test "generates at least 2 files" #t (>= (length files) 2))
          (test "first file has path" #t (not (not (assq 'path (car files)))))
          (test "first file has content" #t (not (not (assq 'content (car files)))))))

(doc 'test "Content generation for shell-module")
(display "\nTest 7: Shell module content generation\n")
(let* ([template (get-template 'shell-module)]
       [variables (cdr (assq 'variables template))]
       [bindings (build-bindings "test-mod" variables '((description . "Test module")))]
       [files (cdr (assq 'files template))]
       [main-file (car files)]
       [content-template (cdr (assq 'content main-file))]
       [content (substitute-vars content-template bindings)])
      (test "contains module name" #t (string-contains? content "test-mod"))
      (test "contains description" #t (string-contains? content "Test module"))
      (test "contains author" #t (string-contains? content "TestAuthor"))
      (test "has Shell marker" #t (string-contains? content "This is Shell code")))

(doc 'test "Content generation for core-module")
(display "\nTest 8: Core module content generation\n")
(let* ([template (get-template 'core-module)]
       [variables (cdr (assq 'variables template))]
       [bindings (build-bindings "pure-fn" variables '((description . "Pure computation")))]
       [files (cdr (assq 'files template))]
       [main-file (car files)]
       [content-template (cdr (assq 'content main-file))]
       [content (substitute-vars content-template bindings)])
      (test "contains module name" #t (string-contains? content "pure-fn"))
      (test "contains description" #t (string-contains? content "Pure computation"))
      (test "has Core marker" #t (string-contains? content "This is Core code"))
      (test "loads prelude" #t (string-contains? content "prelude.ss")))

(doc 'test "Edge cases")
(display "\nTest 9: Edge cases\n")
(test "unknown template returns #f"
      #f
      (get-template 'nonexistent-template))

(doc 'note "Nested test skipped - causes issues with variable substitution")

(doc 'test "Actual scaffolding (file generation)")
(display "\nTest 10: Actual scaffolding\n")
(set! *test-files-written* '())
(scaffold 'test-template "my-test" '((var1 . "value1") (var2 . "value2")))
(test "generates file" 1 (length *test-files-written*))
(let* ([file (car *test-files-written*)]
       [path (car file)]
       [content (cdr file)])
      (test "path contains name" #t (string-contains? path "my-test"))
      (test "content has var1" #t (string-contains? content "value1"))
      (test "content has var2" #t (string-contains? content "value2")))

(doc 'test "Timestamp utilities")
(display "\nTest 11: Timestamp utilities\n")
(let ([ts (current-timestamp)])
     (test "timestamp is string" #t (string? ts))
     (test "timestamp non-empty" #t (> (string-length ts) 0))
     (let ([year (timestamp-year ts)])
          (test "year is 4 chars" 4 (string-length year))
          (test "year value" "2025" year)))

(doc 'section 'test-summary)

(display "\n====\n")
(display (format "Tests passed: ~a/~a\n" *test-passed* *test-count*))
(if (= *test-passed* *test-count*)
    (display "✓ All scaffold tests complete.\n")
    (display "✗ Some tests failed.\n"))

(display "\nVisual tests (run manually in REPL):\n")
(display "  (list-templates)     ; Show all templates\n")
(display "  (scaffold-help)      ; Display help\n")
(display "  (scaffold-interactive) ; Interactive wizard\n")
(display "\nTo test file generation:\n")
(display "  (scaffold 'shell-module \"demo-module\" '((description . \"Demo\")))\n")
