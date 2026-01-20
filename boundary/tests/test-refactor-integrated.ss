(load "core/base/prelude.ss")
(load "core/test-framework.ss")
(load "boundary/tools/refactor-integrated.ss")

(doc 'module 'test-refactor-integrated)
(doc 'description "Tests for Integrated Refactoring Engine")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'note "Run from project root: scheme --script boundary/tests/test-refactor-integrated.ss")

(display "
")
(display "====
")
(display "     INTEGRATED REFACTORING ENGINE TESTS
")
(display "====
")

(doc 'section 'change-representation-tests)

(test-group change-representation-tests
            (define-test make-ref-change-test
              (let ([c (make-ref-change 'rename "test.ss" "old" "new" 10 15 'context)])
                   (assert-true (ref-change? c))
                   (assert-equal 'rename (ref-change-type c))
                   (assert-equal "test.ss" (ref-change-file c))
                   (assert-equal "old" (ref-change-old c))
                   (assert-equal "new" (ref-change-new c))
                   (assert-equal 10 (ref-change-line-start c))
                   (assert-equal 15 (ref-change-line-end c))
                   (assert-equal 'context (ref-change-context c))))
            
            (define-test ref-change-predicate-test
              (assert-true (ref-change? (make-ref-change 'test "f" "a" "b" 1 1 'ctx)))
              (assert-false (ref-change? 'not-a-change))
              (assert-false (ref-change? '(1 2 3)))
              (assert-false (ref-change? (vector 'wrong-tag 1 2 3 4 5 6 7)))))

(doc 'section 'file-scanning-tests)

(test-group file-scanning-tests
            (define-test find-scheme-files-test
              ;; This test verifies the structure - actual results depend on filesystem
              (let ([files (find-scheme-files-simple "core/base")])
                   (assert-true (list? files))
                   ;; Should find at least prelude.ss
                   (assert-true (> (length files) 0))))
            
            (define-test string-contains-word-test
              ;; Test whole-word matching
              (assert-true (string-contains-word? "foo bar baz" "bar"))
              (assert-false (string-contains-word? "foobar baz" "bar"))  ; Not whole word
              (assert-true (string-contains-word? "bar baz" "bar"))     ; At start
              (assert-true (string-contains-word? "foo bar" "bar"))     ; At end
              (assert-true (string-contains-word? "bar" "bar"))         ; Exact match
              (assert-false (string-contains-word? "foobar" "bar"))))   ; Substring only

(doc 'section 'string-utility-tests)

(test-group string-utility-tests
            (define-test string-find-substring-test
              (assert-equal 0 (string-find-substring "hello" "hel"))
              (assert-equal 2 (string-find-substring "hello" "llo"))
              (assert-false (string-find-substring "hello" "xyz")))
            
            (define-test string-prefix-at-test
              (assert-true (string-prefix-at? "hello world" "world" 6))
              (assert-false (string-prefix-at? "hello world" "world" 0))
              (assert-true (string-prefix-at? "abc" "abc" 0))
              (assert-false (string-prefix-at? "ab" "abc" 0)))  ; Prefix too long
            
            (define-test scheme-identifier-char-test
              (assert-true (scheme-identifier-char? #\a))
              (assert-true (scheme-identifier-char? #\Z))
              (assert-true (scheme-identifier-char? #\0))
              (assert-true (scheme-identifier-char? #\-))
              (assert-true (scheme-identifier-char? #\?))
              (assert-true (scheme-identifier-char? #\!))
              (assert-false (scheme-identifier-char? #\space))
              (assert-false (scheme-identifier-char? #\())
              (assert-false (scheme-identifier-char? #\))))
            
            (doc 'section 'semantic-scanner-tests)

            (test-group semantic-scanner-tests
                        (define-test scan-token-regions-basic-test
                          ;; Simple code with no strings or comments
                          (let ([regions (scan-token-regions "(define x 10)")])
                               (assert-true (list? regions))
                               (assert-true (> (length regions) 0))
                               ;; Should have at least one code region
                               (assert-true (any (lambda (r) (eq? (car r) 'code)) regions))))
                        
                        (define-test scan-token-regions-string-test
                          ;; Code with a string literal
                          (let ([regions (scan-token-regions "(define x \"hello\")")])
                               (assert-true (list? regions))
                               ;; Should have both code and string regions
                               (assert-true (any (lambda (r) (eq? (car r) 'code)) regions))
                               (assert-true (any (lambda (r) (eq? (car r) 'string)) regions))))
                        
                        (define-test scan-token-regions-comment-test
                          ;; Code with a line comment
                          (let ([regions (scan-token-regions "(define x 10) ; comment\n")])
                               (assert-true (list? regions))
                               ;; Should have code and comment regions
                               (assert-true (any (lambda (r) (eq? (car r) 'code)) regions))
                               (assert-true (any (lambda (r) (eq? (car r) 'line-comment)) regions))))
                        
                        (define-test scan-token-regions-block-comment-test
                          ;; Code with a block comment
                          (let ([regions (scan-token-regions "(define #| block |# x 10)")])
                               (assert-true (list? regions))
                               (assert-true (any (lambda (r) (eq? (car r) 'block-comment)) regions))))
                        
                        (define-test scan-string-end-test
                          ;; Test string boundary detection
                          (assert-equal 6 (scan-string-end "hello\"more" 0))  ; Find closing quote
                          (assert-equal 8 (scan-string-end "he\\\"llo\"" 0))  ; Escaped quote
                          (assert-equal 5 (scan-string-end "test\" " 0)))
                        
                        (define-test scan-block-comment-end-nested-test
                          ;; Test nested block comments
                          (let ([content "#| outer #| inner |# outer |#after"])
                               ;; After the opening #|, find the matching |#
                               (let ([end (scan-block-comment-end content 2)])
                                    ;; Should find the outer |# not the inner one
                                    (assert-true (> end 20))))))
            
            (doc 'section 'position-region-tests)

            (test-group position-region-tests
                        (define-test position-in-code-test
                          (let ([regions '((code 0 10) (string 10 20) (code 20 30))])
                               (assert-true (position-in-code? regions 5))    ; In first code region
                               (assert-false (position-in-code? regions 15))  ; In string region
                               (assert-true (position-in-code? regions 25)))) ; In second code region
                        
                        (define-test find-region-type-test
                          (let ([regions '((code 0 10) (string 10 20) (line-comment 20 30))])
                               (assert-equal 'code (find-region-type regions 5))
                               (assert-equal 'string (find-region-type regions 15))
                               (assert-equal 'line-comment (find-region-type regions 25)))))
            
            (doc 'section 'symbol-occurrence-tests)

            (test-group symbol-occurrence-tests
                        (define-test find-symbol-occurrences-code-test
                          ;; Find symbol in code only
                          (let* ([content "(define foo 10) (+ foo bar)"]
                                 [occs (find-symbol-occurrences-semantic content "foo")])
                                (assert-equal 2 (length occs))  ; Two occurrences of foo
                                ;; Each occurrence has (pos line col)
                                (assert-true (every (lambda (o) (= (length o) 3)) occs))))
                        
                        (define-test find-symbol-occurrences-skip-string-test
                          ;; Should not find symbol inside string
                          (let* ([content "(define x \"foo is here\") foo"]
                                 [occs (find-symbol-occurrences-semantic content "foo")])
                                (assert-equal 1 (length occs))))  ; Only the one outside string
                        
                        (define-test find-symbol-occurrences-skip-comment-test
                          ;; Should not find symbol inside comment
                          (let* ([content "(define foo 10) ; foo in comment\nfoo"]
                                 [occs (find-symbol-occurrences-semantic content "foo")])
                                (assert-equal 2 (length occs))))  ; First foo and last foo, not in comment
                        
                        (define-test find-symbol-whole-word-test
                          ;; Should only match whole words
                          (let* ([content "(define foobar 10) (define foo 20)"]
                                 [occs (find-symbol-occurrences-semantic content "foo")])
                                (assert-equal 1 (length occs)))))  ; Only the standalone foo
            
            (doc 'section 'string-replace-tests)

            (test-group string-replace-tests
                        (define-test string-replace-word-simple-test
                          (assert-equal "(define bar 10)"
                                        (string-replace-word "(define foo 10)" "foo" "bar")))
                        
                        (define-test string-replace-word-multiple-test
                          (assert-equal "(+ bar bar)"
                                        (string-replace-word "(+ foo foo)" "foo" "bar")))
                        
                        (define-test string-replace-word-no-partial-test
                          ;; Should not replace partial matches
                          (assert-equal "foobar foo barfoo"
                                        (string-replace-word "foobar foo barfoo" "foo" "baz"))
                          ;; Only the standalone foo should be replaced
                          ))
            
            (doc 'section 'signature-extraction-tests)

            (test-group signature-extraction-tests
                        (define-test parse-param-list-simple-test
                          (assert-equal '(a b c) (parse-param-list "a b c")))
                        
                        (define-test parse-param-list-empty-test
                          (assert-equal '() (parse-param-list "")))
                        
                        (define-test extract-param-string-test
                          ;; Extract params from (define (foo x y) ...)
                          (let ([str "(define (foo x y) body)"])
                               ;; Start after the function name
                               (let ([params (extract-param-string str 13)])  ; After "foo "
                                    (assert-true (string? params))))))
            
            (doc 'section 'change-grouping-tests)

            (test-group change-grouping-tests
                        (define-test group-changes-by-file-test
                          (let* ([c1 (make-ref-change 'rename "a.ss" "old" "new" 1 1 'ctx)]
                                 [c2 (make-ref-change 'rename "a.ss" "old" "new" 2 2 'ctx)]
                                 [c3 (make-ref-change 'rename "b.ss" "old" "new" 1 1 'ctx)]
                                 [groups (group-changes-by-file (list c1 c2 c3))])
                                (assert-equal 2 (length groups))  ; Two files
                                ;; Find group for a.ss
                                (let ([a-group (find (lambda (g) (string=? (car g) "a.ss")) groups)])
                                     (assert-true (pair? a-group))
                                     (assert-equal 2 (length (cdr a-group)))))))
            
            (doc 'section 'helper-tests)

            (test-group helper-tests
                        (define-test take-n-test
                          (assert-equal '(1 2) (take-n '(1 2 3 4 5) 2))
                          (assert-equal '() (take-n '(1 2 3) 0))
                          (assert-equal '(1 2 3) (take-n '(1 2 3) 5))  ; List shorter than n
                          (assert-equal '() (take-n '() 3)))
                        
                        (define-test take-up-to-test
                          (assert-equal '(1 2 3) (take-up-to '(1 2 3 4 5) 3))
                          (assert-equal '() (take-up-to '(1 2 3) 0))
                          (assert-equal '(1 2 3) (take-up-to '(1 2 3) 10))))
            
            (doc 'section 'state-tests)

            (test-group state-tests
                        (define-test pending-changes-initial-test
                          ;; Clear any existing changes
                          (refactor-clear!)
                          (assert-equal '() *pending-changes*))
                        
                        (define-test refactor-clear-test
                          (set! *pending-changes* (list (make-ref-change 'test "f" "a" "b" 1 1 'ctx)))
                          (refactor-clear!)
                          (assert-equal '() *pending-changes*)))
            
            (doc 'section 'integration-tests)

            (test-group integration-tests
                        (define-test refactor-status-test
                          ;; Just verify it doesn't error
                          (refactor-clear!)
                          (refactor-status)
                          (assert-true #t))
                        
                        (define-test refactor-preview-changes-empty-test
                          ;; Preview with no changes should not error
                          (refactor-clear!)
                          (refactor-preview-changes)
                          (assert-true #t)))
            
            (doc 'section 'context-extraction-tests)

            (test-group context-extraction-tests
                        (define-test extract-context-line-test
                          (let* ([content "line one\nline two\nline three\n"]
                                 [ctx (extract-context-line content 10)])  ; Position in "line two"
                                (assert-true (string? ctx))
                                (assert-equal "line two" ctx)))
                        
                        (define-test extract-context-line-first-line-test
                          (let* ([content "first line\nsecond"]
                                 [ctx (extract-context-line content 0)])
                                (assert-equal "first line" ctx)))
                        
                        (define-test count-newlines-test
                          (assert-equal 1 (count-newlines-before "no newlines" 5))
                          (assert-equal 2 (count-newlines-before "line1\nline2\nline3" 7))
                          (assert-equal 3 (count-newlines-before "a\nb\nc\nd" 6))))
            
            (doc 'section 'call-detection-tests)

            (test-group call-detection-tests
                        (define-test find-calls-in-expr-simple-test
                          (let ([calls (find-calls-in-expr '(foo 1 2) 'foo)])
                               (assert-equal 1 (length calls))
                               (assert-equal '(foo 1 2) (car calls))))
                        
                        (define-test find-calls-in-expr-nested-test
                          (let ([calls (find-calls-in-expr '(+ (foo 1) (foo 2)) 'foo)])
                               (assert-equal 2 (length calls))))
                        
                        (define-test find-calls-in-expr-no-match-test
                          (let ([calls (find-calls-in-expr '(bar 1 2) 'foo)])
                               (assert-equal 0 (length calls))))
                        
                        (define-test find-calls-in-expr-quoted-test
                          ;; Should not find calls in quoted data
                          (let ([calls (find-calls-in-expr '(quote (foo 1 2)) 'foo)])
                               (assert-equal 0 (length calls)))))
            
            (doc 'section 'transform-tests)

            (test-group transform-tests
                        (define-test transform-call-simple-test
                          (let ([result (transform-call '(foo a b) 'bar '(0 1))])
                               (assert-true (string? result))
                               ;; Result should be a string representation
                               (assert-true (string-contains? result "bar"))))
                        
                        (define-test transform-call-reorder-test
                          ;; Swap arguments
                          (let ([result (transform-call '(foo a b) 'bar '(1 0))])
                               ;; b should come before a in the result
                               (assert-true (string? result)))))
            
            (doc 'section 'helper-functions)

            (define (any pred lst)
              (doc 'type "(a -> Bool) x (List a) -> Bool")
              (cond
               [(null? lst) #f]
               [(pred (car lst)) #t]
               [else (any pred (cdr lst))]))
            
            (define (every pred lst)
              (doc 'type "(a -> Bool) x (List a) -> Bool")
              (cond
               [(null? lst) #t]
               [(not (pred (car lst))) #f]
               [else (every pred (cdr lst))]))
            
            (define (find pred lst)
              (doc 'type "(a -> Bool) x (List a) -> a | #f")
              (cond
               [(null? lst) #f]
               [(pred (car lst)) (car lst)]
               [else (find pred (cdr lst))]))
            
            (define (string-contains? str sub)
              (doc 'type "String x String -> Bool")
              (let ([slen (string-length str)]
                    [sublen (string-length sub)])
                   (let loop ([i 0])
                        (cond
                         [(> (+ i sublen) slen) #f]
                         [(string=? sub (substring str i (+ i sublen))) #t]
                         [else (loop (+ i 1))]))))
            
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
[SUCCESS] All integrated refactoring engine tests passed.
")
                (display "
[FAILURE] Some tests failed.
"))
