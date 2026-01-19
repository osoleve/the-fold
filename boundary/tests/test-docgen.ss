;;; boundary/test-docgen.ss -- Tests for API Documentation Generator
;;;
;;; Tests the docgen module's parsing and documentation generation.
;;;
;;; Run with: scheme --script boundary/test-docgen.ss

;;; Set up paths
(source-directories (cons "core" (source-directories)))

;;; Load test framework
(load "core/test-framework.ss")

;;; Load the module under test
(load "boundary/tools/docgen.ss")

(display "
Docgen Tests
")
(display "====

")

;;; ====
;;; String Utility Tests
;;; ====

(test-group string-utilities
            
            (define-test string-trim-basic
              (assert-equal "hello" (string-trim "  hello  "))
              (assert-equal "hello" (string-trim "hello"))
              (assert-equal "" (string-trim "   ")))
            
            (define-test string-prefix-basic
              (assert-true (string-prefix? ";;;" ";;; comment"))
              (assert-true (string-prefix? "" "anything"))
              (assert-false (string-prefix? "abc" "xyz")))
            
            (define-test string-contains-basic
              (assert-true (string-contains? "hello world" "world"))
              (assert-true (string-contains? "hello world" "hello"))
              (assert-false (string-contains? "hello world" "xyz")))
            
            (define-test string-index-basic
              (assert-equal 5 (string-index "hello world" #\space))
              (assert-equal 0 (string-index "hello" #\h))
              (assert-false (string-index "hello" #\z)))
            
            (define-test string-split-basic
              (assert-equal '("a" "b" "c") (string-split "a/b/c" #\/))
              (assert-equal '("hello") (string-split "hello" #\/))
              (assert-equal '("" "a" "b") (string-split "/a/b" #\/)))
            
            (define-test string-join-basic
              (assert-equal "a, b, c" (string-join '("a" "b" "c") ", "))
              (assert-equal "hello" (string-join '("hello") ", "))
              (assert-equal "" (string-join '() ", "))))

;;; ====
;;; Path Utility Tests
;;; ====

(test-group path-utilities
            
            (define-test path-basename-basic
              (assert-equal "file.ss" (path-basename "path/to/file.ss"))
              (assert-equal "file.ss" (path-basename "file.ss"))
              (assert-equal "module.ss" (path-basename "/absolute/path/module.ss")))
            
            (define-test path-strip-extension-basic
              (assert-equal "file" (path-strip-extension "file.ss"))
              ;; Note: strips only the last extension
              (assert-equal "file.test" (path-strip-extension "file.test.ss"))
              (assert-equal "noext" (path-strip-extension "noext")))
            
            (define-test path-to-module-name
              (assert-equal "module" (path->module-name "path/to/module.ss"))
              (assert-equal "test" (path->module-name "test.ss"))))

;;; ====
;;; Doc Comment Detection Tests
;;; ====

(test-group doc-comment-detection
            
            (define-test doc-comment-detection-basic
              (assert-true (doc-comment? ";;; This is a doc comment"))
              (assert-true (doc-comment? "  ;;; Indented doc comment"))
              (assert-false (doc-comment? ";; Regular comment"))
              (assert-false (doc-comment? "; Single semicolon"))
              (assert-false (doc-comment? "(define x 1)")))
            
            (define-test doc-comment-content-extraction
              (assert-equal "This is content" (doc-comment-content ";;; This is content"))
              (assert-equal "Trimmed" (doc-comment-content ";;;   Trimmed  "))
              (assert-equal "" (doc-comment-content ";;;")))
            
            (define-test empty-line-detection
              (assert-true (empty-line? ""))
              (assert-true (empty-line? "   "))
              (assert-true (empty-line? "		"))
              (assert-false (empty-line? "content")))
            
            (define-test section-header-detection
              (assert-true (section-header? ";;; ===="))
              (assert-true (section-header? ";;; === Section"))
              (assert-false (section-header? ";;; Regular comment"))
              (assert-false (section-header? ";; Not a doc comment"))))

;;; ====
;;; Signature Parsing Tests
;;; ====

(test-group signature-parsing
            
            (define-test parse-simple-signature
              (let ([result (parse-signature ";;; add : Int -> Int -> Int")])
                   (assert-true (pair? result))
                   (assert-equal 'add (car result))
                   (assert-equal "Int -> Int -> Int" (cdr result))))
            
            (define-test parse-complex-signature
              (let ([result (parse-signature ";;; map-maybe : (a -> b) -> Maybe a -> Maybe b")])
                   (assert-true (pair? result))
                   (assert-equal 'map-maybe (car result))))
            
            (define-test parse-non-signature
              (assert-false (parse-signature ";;; This is just a comment"))
              (assert-false (parse-signature ";; Not a doc comment"))
              (assert-false (parse-signature "(define x 1)")))
            
            (define-test parse-unicode-arrow-signature
              ;; Test with Unicode arrow character if present
              (let ([result (parse-signature ";;; test : a -> b")])
                   (assert-true (pair? result)))))

;;; ====
;;; Function Definition Parsing Tests
;;; ====

(test-group function-definition-parsing
            
            (define-test function-definition-detection
              (assert-true (function-definition? "(define (foo x) x)"))
              ;; Value definitions with lambda are not detected as function defs
              ;; because function-definition? looks for "(define (" pattern
              (assert-false (function-definition? "(define foo (lambda (x) x))"))
              (assert-true (function-definition? "(define-record-type point (fields x y))"))
              (assert-false (function-definition? ";;; comment"))
              (assert-false (function-definition? "(+ 1 2)")))
            
            (define-test extract-function-name-basic
              (assert-equal 'foo (extract-function-name "(define (foo x) x)"))
              (assert-equal 'bar (extract-function-name "(define (bar a b c) body)"))
              (assert-equal 'x (extract-function-name "(define x 42)"))))

;;; ====
;;; Export Summary Tests
;;; ====

(test-group export-summary
            
            (define-test find-export-summary-basic
              (let* ([lines '(";;; Some header"
                              ";;; Export Summary"
                              ";;; foo, bar, baz"
                              ";;; qux"
                              ";;; ====")]
                     [summary (find-export-summary lines)])
                    (assert-equal 2 (length summary))
                    (assert-true (string-contains? (car summary) "foo"))))
            
            (define-test find-export-summary-empty
              (let* ([lines '(";;; No export summary here"
                              "(define x 1)")]
                     [summary (find-export-summary lines)])
                    (assert-equal '() summary)))
            
            (define-test parse-export-line-basic
              (let ([names (parse-export-line "foo, bar, baz")])
                   (assert-equal 3 (length names))
                   ;; memq returns the sublist starting at the found element, not #t
                   (assert-true (and (memq 'foo names) #t))
                   (assert-true (and (memq 'bar names) #t))))
            
            (define-test parse-export-line-with-category
              (let ([names (parse-export-line "Type: make-foo, foo?")])
                   (assert-equal 2 (length names)))))

;;; ====
;;; Full Documentation Extraction Tests
;;; ====

(test-group full-extraction
            
            (define-test extract-exports-basic
              (let* ([lines '(";;; add : Int -> Int -> Int"
                              ";;; Add two integers."
                              "(define (add x y) (+ x y))"
                              ""
                              ";;; sub : Int -> Int -> Int"
                              "(define (sub x y) (- x y))")]
                     [exports (extract-exports lines)])
                    (assert-equal 2 (length exports))
                    ;; Check first export
                    (let ([first-exp (car exports)])
                         (assert-equal 'add (cdr (assq 'name first-exp)))
                         (assert-true (string-contains? (cdr (assq 'signature first-exp)) "Int")))))
            
            (define-test extract-exports-with-description
              (let* ([lines '(";;; helper : a -> a"
                              ";;; A helper function."
                              ";;; It does helpful things."
                              "(define (helper x) x)")]
                     [exports (extract-exports lines)]
                     [first-exp (car exports)])
                    (assert-true (string-contains? (cdr (assq 'doc first-exp)) "helper")))))

;;; ====
;;; Main API Tests
;;; ====

(test-group main-api
            
            (define-test docgen-file-on-real-file
              ;; Test on actual prelude.ss file
              (let ([doc (docgen-file "core/base/prelude.ss")])
                   (assert-true (pair? doc))
                   (assert-equal "prelude" (cdr (assq 'module doc)))
                   (assert-true (list? (cdr (assq 'exports doc))))))
            
            (define-test docgen-file-nonexistent
              ;; Should handle missing files gracefully
              (let ([doc (docgen-file "nonexistent-file.ss")])
                   (assert-true (pair? doc))
                   ;; Should have exports as empty list
                   (assert-equal '() (cdr (assq 'exports doc)))))
            
            (define-test docgen-index-basic
              (let* ([doc '(((module . "test")
                             (exports . (((name . foo) (signature . "a -> b") (doc . ""))
                                         ((name . bar) (signature . "") (doc . "desc"))))))]
                     [index (docgen-index doc)])
                    (assert-equal 2 (length index))
                    ;; assq returns the pair, not #t
                    (assert-true (and (assq 'foo index) #t))
                    (assert-true (and (assq 'bar index) #t))))
            
            (define-test docgen-summary-format
              (let* ([doc '(((module . "test")
                             (file . "test.ss")
                             (exports . (((name . foo)
                                          (signature . "a -> b")
                                          (doc . "Test function"))))))]
                     [summary (docgen-summary doc)])
                    (assert-true (string? summary))
                    (assert-true (string-contains? summary "test"))
                    (assert-true (string-contains? summary "foo")))))

;;; ====
;;; Query Function Tests
;;; ====

(test-group query-functions
            
            (define-test docgen-find-basic
              (let* ([doc '(((module . "m1")
                             (exports . (((name . target) (signature . "") (doc . "found")))))
                            ((module . "m2")
                             (exports . (((name . other) (signature . "") (doc . ""))
                                         ((name . target) (signature . "x") (doc . "also"))))))]
                     [results (docgen-find doc 'target)])
                    (assert-equal 2 (length results))))
            
            (define-test docgen-search-basic
              (let* ([doc '(((module . "test")
                             (exports . (((name . foobar) (signature . "") (doc . "baz"))
                                         ((name . other) (signature . "") (doc . "test"))))))]
                     [results (docgen-search doc "foo")])
                    (assert-equal 1 (length results))
                    (assert-equal 'foobar (car (car results))))))

;;; ====
;;; Integration Tests
;;; ====

(test-group integration
            
            (define-test full-pipeline-on-interval
              ;; Run full docgen on interval.ss which has known documentation
              (let* ([doc (docgen-file "lattice/fp/numeric/interval.ss")]
                     [exports (cdr (assq 'exports doc))]
                     [export-summary-from-doc (cdr (assq 'export-summary doc))])
                    ;; Should have exports
                    (assert-true (> (length exports) 0))
                    ;; Should have export summary in doc
                    (assert-true (> (length export-summary-from-doc) 0))))
            
            (define-test full-pipeline-on-numeric
              ;; Test on numeric.ss with full type class hierarchy
              (let* ([doc (docgen-file "lattice/fp/numeric/numeric.ss")]
                     [exports (cdr (assq 'exports doc))])
                    ;; numeric.ss has many exports
                    (assert-true (> (length exports) 5)))))

;;; ====
;;; Run Summary
;;; ====

(newline)
(print-summary)
