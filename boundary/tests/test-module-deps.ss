(load "core/base/prelude.ss")
(load "core/test-framework.ss")
(load "boundary/tools/module-deps.ss")

(doc 'module 'test-module-deps)
(doc 'description "Tests for Module Dependency Analyzer")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'note "Run from project root: scheme --script boundary/tests/test-module-deps.ss")

(display "
")
(display "====
")
(display "          MODULE DEPENDENCY ANALYZER TESTS
")
(display "====
")

(doc 'section 'load-expression-extraction-tests)

(test-group extract-loads-tests
            (define-test extract-loads-simple-test
              (let ([loads (extract-loads "(load \"foo.ss\")")])
                   (assert-equal 1 (length loads))
                   (assert-equal "foo.ss" (car loads))))

            (define-test extract-loads-multiple-test
              (let ([loads (extract-loads "(load \"a.ss\") (load \"b.ss\") (load \"c.ss\")")])
                   (assert-equal 3 (length loads))
                   (assert-true (if (member "a.ss" loads) #t #f))
                   (assert-true (if (member "b.ss" loads) #t #f))
                   (assert-true (if (member "c.ss" loads) #t #f))))

            (define-test extract-loads-with-other-code-test
              (let ([loads (extract-loads "(define x 10) (load \"dep.ss\") (+ x 1)")])
                   (assert-equal 1 (length loads))
                   (assert-equal "dep.ss" (car loads))))

            (define-test extract-loads-nested-path-test
              (let ([loads (extract-loads "(load \"core/base/prelude.ss\")")])
                   (assert-equal 1 (length loads))
                   (assert-equal "core/base/prelude.ss" (car loads))))

            (define-test extract-loads-empty-test
              (let ([loads (extract-loads "(define x 10)")])
                   (assert-equal 0 (length loads))))

            (define-test extract-loads-non-string-test
              (doc 'note "load with non-string argument should be ignored")
              (let ([loads (extract-loads "(load x)")])
                   (assert-equal 0 (length loads)))))

(doc 'section 'path-normalization-tests)

(test-group path-normalization-tests
            (define-test path-directory-simple-test
              (assert-equal "shell" (path-directory "boundary/repl/repl.ss")))

            (define-test path-directory-nested-test
              (assert-equal "core/base" (path-directory "core/base/prelude.ss")))

            (define-test path-directory-no-dir-test
              (assert-equal "" (path-directory "foo.ss")))

            (define-test normalize-path-relative-test
              (let ([result (normalize-path "boundary/repl/repl.ss" "fs.ss")])
                   (assert-equal "boundary/io/fs.ss" result)))

            (define-test normalize-path-absolute-test
              (let ([result (normalize-path "boundary/repl/repl.ss" "/abs/path.ss")])
                   (assert-equal "/abs/path.ss" result)))

            (define-test normalize-path-dotdot-test
              (let ([result (normalize-path "boundary/sub/file.ss" "../other.ss")])
                   (assert-true (string? result)))))

(doc 'section 'string-split-path-tests)

(test-group string-split-path-tests
            (define-test string-split-path-simple-test
              (let ([parts (string-split-path "a/b/c")])
                   (assert-equal 3 (length parts))
                   (assert-equal "a" (car parts))
                   (assert-equal "b" (cadr parts))
                   (assert-equal "c" (caddr parts))))

            (define-test string-split-path-single-test
              (let ([parts (string-split-path "foo")])
                   (assert-equal 1 (length parts))
                   (assert-equal "foo" (car parts))))

            (define-test string-split-path-trailing-slash-test
              (let ([parts (string-split-path "a/b/")])
                   (doc 'note "Trailing slash means empty component, should be skipped")
                   (assert-equal 2 (length parts))))

            (define-test string-split-path-leading-slash-test
              (let ([parts (string-split-path "/a/b")])
                   (doc 'note "Leading slash produces empty component, should be skipped")
                   (assert-equal 2 (length parts)))))

(doc 'section 'normalize-parts-tests)

(test-group normalize-parts-tests
            (define-test normalize-parts-simple-test
              (let ([result (normalize-parts '("a" "b" "c"))])
                   (assert-equal '("a" "b" "c") result)))

            (define-test normalize-parts-dotdot-test
              (let ([result (normalize-parts '("a" "b" ".." "c"))])
                   (assert-equal '("a" "c") result)))

            (define-test normalize-parts-dot-test
              (let ([result (normalize-parts '("a" "." "b"))])
                   (assert-equal '("a" "b") result)))

            (define-test normalize-parts-multiple-dotdot-test
              (let ([result (normalize-parts '("a" "b" "c" ".." ".."))])
                   (assert-equal '("a") result)))

            (define-test normalize-parts-dotdot-at-start-test
              (doc 'note "Can't go up from root")
              (let ([result (normalize-parts '(".." "a" "b"))])
                   (assert-equal '("a" "b") result))))

(doc 'section 'string-join-path-tests)

(test-group string-join-path-tests
            (define-test string-join-path-simple-test
              (assert-equal "a/b/c" (string-join-path '("a" "b" "c"))))

            (define-test string-join-path-single-test
              (assert-equal "foo" (string-join-path '("foo"))))

            (define-test string-join-path-empty-test
              (assert-equal "" (string-join-path '()))))

(doc 'section 'dependency-graph-construction-tests)

(test-group dep-graph-tests
            (define-test build-dep-graph-empty-test
              (let ([graph (build-dep-graph #f '())])
                   (assert-equal 0 (length graph))))

            (doc 'note "build-dep-graph requires actual filesystem access - Integration tests would need real files or mock fs")

            (doc 'section 'circular-dependency-detection-tests)

            (test-group circular-dep-tests
                        (define-test find-circular-deps-no-cycle-test
                          (let* ([graph '(("a.ss" . ("b.ss"))
                                          ("b.ss" . ("c.ss"))
                                          ("c.ss" . ()))]
                                 [cycles (find-circular-deps graph)])
                                (assert-equal 0 (length cycles))))

                        (define-test find-circular-deps-simple-cycle-test
                          (let* ([graph '(("a.ss" . ("b.ss"))
                                          ("b.ss" . ("a.ss")))]
                                 [cycles (find-circular-deps graph)])
                                (assert-true (> (length cycles) 0))))

                        (define-test find-circular-deps-self-cycle-test
                          (let* ([graph '(("a.ss" . ("a.ss")))]
                                 [cycles (find-circular-deps graph)])
                                (assert-true (> (length cycles) 0))))

                        (define-test detect-cycle-no-cycle-test
                          (let* ([graph '(("a.ss" . ("b.ss"))
                                          ("b.ss" . ()))]
                                 [result (detect-cycle "a.ss" graph '())])
                                (assert-false result)))

                        (define-test detect-cycle-found-test
                          (let* ([graph '(("a.ss" . ("b.ss"))
                                          ("b.ss" . ("a.ss")))]
                                 [result (detect-cycle "a.ss" graph '())])
                                (assert-true (pair? result)))))

            (doc 'section 'cycle-comparison-tests)

            (test-group cycle-comparison-tests
                        (define-test cycle-equal-same-test
                          (assert-true (cycle-equal? '("a" "b" "c") '("a" "b" "c"))))

                        (define-test cycle-equal-rotated-test
                          (assert-true (cycle-equal? '("a" "b" "c") '("b" "c" "a"))))

                        (define-test cycle-equal-different-test
                          (assert-false (cycle-equal? '("a" "b" "c") '("a" "b" "d"))))

                        (define-test cycle-equal-different-length-test
                          (assert-false (cycle-equal? '("a" "b") '("a" "b" "c")))))

            (doc 'section 'rotation-tests)

            (test-group rotation-tests
                        (define-test rotations-empty-test
                          (let ([result (rotations '())])
                               (assert-equal 1 (length result))
                               (assert-equal '() (car result))))

                        (define-test rotations-single-test
                          (let ([result (rotations '(a))])
                               (assert-true (> (length result) 0))))

                        (define-test rotations-multiple-test
                          (let ([result (rotations '(a b c))])
                               (assert-equal 3 (length result))
                               (doc 'note "All rotations should be present")
                               (assert-true (if (member '(a b c) result) #t #f))
                               (assert-true (if (member '(b c a) result) #t #f))
                               (assert-true (if (member '(c a b) result) #t #f)))))

            (doc 'section 'reverse-dependency-tests)

            (test-group reverse-dep-tests
                        (define-test find-reverse-deps-simple-test
                          (let* ([graph '(("a.ss" . ("c.ss"))
                                          ("b.ss" . ("c.ss"))
                                          ("c.ss" . ()))]
                                 [rdeps (find-reverse-deps-from-graph "c.ss" graph)])
                                (assert-equal 2 (length rdeps))))

                        (define-test find-reverse-deps-none-test
                          (let* ([graph '(("a.ss" . ("b.ss"))
                                          ("b.ss" . ()))]
                                 [rdeps (find-reverse-deps-from-graph "a.ss" graph)])
                                (assert-equal 0 (length rdeps)))))

            (doc 'section 'remove-duplicate-cycles-tests)

            (test-group remove-duplicates-tests
                        (define-test remove-duplicate-cycles-no-dups-test
                          (let* ([cycles '(("a" "b") ("c" "d"))]
                                 [result (remove-duplicate-cycles cycles)])
                                (assert-equal 2 (length result))))

                        (define-test remove-duplicate-cycles-with-dups-test
                          (let* ([cycles '(("a" "b" "c") ("b" "c" "a") ("d" "e"))]
                                 [result (remove-duplicate-cycles cycles)])
                                (doc 'note "First two are rotations of same cycle")
                                (assert-equal 2 (length result)))))

            (doc 'section 'any-predicate-tests)

            (test-group any-tests
                        (define-test any-true-test
                          (assert-true (any number? '(a b 3 c))))

                        (define-test any-false-test
                          (assert-false (any number? '(a b c d))))

                        (define-test any-empty-test
                          (assert-false (any number? '()))))

            (doc 'section 'edge-case-tests)

            (test-group edge-case-tests
                        (define-test extract-loads-malformed-test
                          (doc 'note "Malformed code should return empty list gracefully")
                          (let ([loads (extract-loads "(load \"foo.ss\"")])
                               (doc 'note "May return partial results or empty")
                               (assert-true (list? loads))))

                        (define-test path-directory-empty-test
                          (assert-equal "" (path-directory "")))

                        (define-test normalize-path-same-dir-test
                          (let ([result (normalize-path "file.ss" "other.ss")])
                               (assert-equal "other.ss" result)))

                        (define-test detect-cycle-not-in-graph-test
                          (doc 'note "File not in graph should return #f")
                          (let* ([graph '(("a.ss" . ("b.ss")))]
                                 [result (detect-cycle "x.ss" graph '())])
                                (assert-false result))))

            (doc 'section 'normalize-path-components-tests)

            (test-group normalize-path-components-tests
                        (define-test normalize-path-components-simple-test
                          (assert-equal "a/b/c" (normalize-path-components "a/b/c")))

                        (define-test normalize-path-components-dotdot-test
                          (assert-equal "a/c" (normalize-path-components "a/b/../c")))

                        (define-test normalize-path-components-dot-test
                          (assert-equal "a/b" (normalize-path-components "a/./b")))

                        (define-test normalize-path-components-complex-test
                          (assert-equal "core/blocks/block.ss"
                                        (normalize-path-components "boundary/../core/blocks/block.ss"))))

            (doc 'note "Helper Functions Used in Tests - 'any' is already defined in the module")

            (doc 'section 'run-tests)

            (display "
")
            (display "====
")
            (display (format "Tests passed: ~a
" *tests-passed*))
            (display (format "Tests failed: ~a
" *tests-failed*))
            (display (format "Total tests:  ~a
" *tests-run*))

            (if (= *tests-failed* 0)
                (display "
[SUCCESS] All module dependency analyzer tests passed.
")
                (display "
[FAILURE] Some tests failed.
"))
