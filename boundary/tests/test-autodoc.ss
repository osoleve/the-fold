(load "boundary/tools/autodoc.ss")

(doc 'module 'test-autodoc)
(doc 'description "Tests for autodoc.ss")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'dependencies '(autodoc.ss))

(define tests-passed 0)
(define tests-failed 0)

(define (test name expected actual)
  (if (equal? expected actual)
      (begin
       (set! tests-passed (+ tests-passed 1))
       (display "  ✓ ") (display name) (newline))
      (begin
       (set! tests-failed (+ tests-failed 1))
       (display "  ✗ ") (display name)
       (display " — expected ") (write expected)
       (display ", got ") (write actual)
       (newline))))

(display "\nTesting autodoc.ss\n")
(display "====\n\n")

;;; Test path utilities
(display "derive-category:\n")
(test "linalg path" 'linalg (derive-category "lattice/linalg/vec.ss"))
(test "types path" 'types (derive-category "core/types/infer.ss"))
(test "boundary path" 'boundary (derive-category "boundary/commands.ss"))
(test "base path" 'base (derive-category "core/base/prelude.ss"))
(test "fp-control path" 'fp-control (derive-category "lattice/fp/control/monad.ss"))
(test "other path" 'other (derive-category "unknown/path.ss"))

(display "\nderive-visibility:\n")
(test "public function" 'public (derive-visibility 'fold-left))
(test "underscore private" 'private (derive-visibility '_helper))
(test "percent private" 'private (derive-visibility '%internal))

;;; Test string utilities
(display "\nstring-split-paragraphs:\n")
(test "single paragraph" '("hello world")
      (string-split-paragraphs "hello world"))
(test "two paragraphs" '("first" "second")
      (string-split-paragraphs "first\n\nsecond"))
(test "blank lines" '("a" "b")
      (string-split-paragraphs "a\n\n\n\nb"))

(display "\nstrip-comment-prefix:\n")
(test "with space" "hello" (strip-comment-prefix ";;; hello"))
(test "no space" "hello" (strip-comment-prefix ";;;hello"))
(test "no prefix" "hello" (strip-comment-prefix "hello"))

;;; Test example extraction (mock)
(display "\nextract-target-function:\n")
(test "simple call" 'foo (extract-target-function '(foo 1 2 3)))
(test "nested call" 'bar (extract-target-function '((bar x) y)))
(test "literal" #f (extract-target-function 42))

;;; Test the index was built
(display "\nindex building:\n")
(doc-refresh!)
(test "registry not empty" #t (> (hashtable-size *autodoc-registry*) 0))
(test "categories not empty" #t (> (hashtable-size *autodoc-categories*) 0))

;;; Test lookups
(display "\ndoc lookups:\n")
(let ([entry (hashtable-ref *autodoc-registry* 'fold-left #f)])
     (test "fold-left exists" #t (not (not entry)))
     (when entry
           (test "has category" #t (symbol? (cdr (assq 'category entry))))
           (test "has file" #t (string? (cdr (assq 'file entry))))))

;;; Test search
(display "\ndoc-search:\n")
(let ([results (doc-search "matrix")])
     (test "matrix results not empty" #t (> (length results) 0))
     (test "matrix-add in results" #t (not (not (memq 'matrix-add results)))))

;;; Summary
(newline)
(display "====\n")
(display "Tests passed: ") (display tests-passed) (newline)
(display "Tests failed: ") (display tests-failed) (newline)
(newline)

(when (> tests-failed 0)
      (error 'test-autodoc "Some tests failed"))
