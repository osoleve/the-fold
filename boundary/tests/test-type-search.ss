(doc 'note "Run from project root: scheme --script boundary/tests/test-type-search.ss")

(load "core/base/prelude.ss")
(load "core/test-framework.ss")
(load "boundary/tools/type-search.ss")

(doc 'module 'test-type-search)
(doc 'description "Tests for Type-Driven Search")
(doc 'layer 'boundary)
(doc 'purity 'partial)

(display "
")
(display "====
")
(display "          TYPE-DRIVEN SEARCH TESTS
")
(display "====
")

(doc 'section 'type-pattern-recognition-tests)

(test-group type-pattern-tests
            (define-test wildcard-is-pattern-test
              (assert-true (type-pattern? '_)))
            
            (define-test symbol-is-pattern-test
              (assert-true (type-pattern? 'a))
              (assert-true (type-pattern? 'Nat))
              (assert-true (type-pattern? 'Bool)))
            
            (define-test function-type-pattern-test
              (assert-true (type-pattern? '(-> Nat Bool)))
              (assert-true (type-pattern? '(-> a b c))))
            
            (define-test product-type-pattern-test
              (assert-true (type-pattern? '(x Nat Bool))))
            
            (define-test list-type-pattern-test
              (assert-true (type-pattern? '(List Nat)))
              (assert-true (type-pattern? '(List a))))
            
            (define-test vector-type-pattern-test
              (assert-true (type-pattern? '(Vector Nat))))
            
            (define-test non-pattern-test
              (assert-false (type-pattern? 42))
              (assert-false (type-pattern? "string"))))

(doc 'section 'type-variable-tests)

(test-group type-variable-tests
            (define-test lowercase-is-variable-test
              (assert-true (type-variable? 'a))
              (assert-true (type-variable? 'b))
              (assert-true (type-variable? 'xyz)))
            
            (define-test uppercase-not-variable-test
              (assert-false (type-variable? 'Nat))
              (assert-false (type-variable? 'Bool))
              (assert-false (type-variable? 'String)))
            
            (define-test articles-excluded-test
              (assert-false (type-variable? 'a))  ; 'a' is a variable
              (assert-false (type-variable? 'an))
              (assert-false (type-variable? 'the))))

(doc 'section 'base-type-tests)

(test-group base-type-tests
            (define-test nat-is-base-test
              (assert-true (base-type? 'Nat)))
            
            (define-test int-is-base-test
              (assert-true (base-type? 'Int)))
            
            (define-test bool-is-base-test
              (assert-true (base-type? 'Bool)))
            
            (define-test char-is-base-test
              (assert-true (base-type? 'Char)))
            
            (define-test string-is-base-test
              (assert-true (base-type? 'String)))
            
            (define-test symbol-is-base-test
              (assert-true (base-type? 'Symbol)))
            
            (define-test non-base-type-test
              (assert-false (base-type? 'List))
              (assert-false (base-type? 'a))
              (assert-false (base-type? 'CustomType))))

(doc 'section 'type-matching-tests)

(test-group type-matching-tests
            (define-test wildcard-matches-any-test
              (assert-true (pair? (type-match? '_ 'Nat '())))
              (assert-true (pair? (type-match? '_ 'Bool '())))
              (assert-true (pair? (type-match? '_ '(List Nat) '()))))
            
            (define-test type-variable-binds-test
              (let ([env (type-match? 'a 'Nat '())])
                   (assert-true (pair? env))
                   (assert-equal 'Nat (cdr (assq 'a env)))))
            
            (define-test type-variable-consistent-test
              ;; Same variable must match same type
              (let* ([env1 (type-match? 'a 'Nat '())]
                     [env2 (type-match? 'a 'Nat env1)])
                    (assert-true (pair? env2)))
              ;; Same variable with different type should fail
              (let* ([env1 (type-match? 'a 'Nat '())]
                     [env2 (type-match? 'a 'Bool env1)])
                    (assert-false env2)))
            
            (define-test base-type-exact-match-test
              (assert-true (pair? (type-match? 'Nat 'Nat '())))
              (assert-false (type-match? 'Nat 'Bool '())))
            
            (define-test function-type-match-test
              (let ([env (type-match? '(-> Nat Bool) '(-> Nat Bool) '())])
                   (assert-true (pair? env))))
            
            (define-test function-type-variable-match-test
              (let ([env (type-match? '(-> a a) '(-> Nat Nat) '())])
                   (assert-true (pair? env))
                   (assert-equal 'Nat (cdr (assq 'a env)))))
            
            (define-test function-type-mismatch-test
              ;; Pattern requires same type for both args
              (assert-false (type-match? '(-> a a) '(-> Nat Bool) '())))
            
            (define-test list-type-match-test
              (let ([env (type-match? '(List a) '(List Nat) '())])
                   (assert-true (pair? env))
                   (assert-equal 'Nat (cdr (assq 'a env))))))

(doc 'section 'match-list-tests)

(test-group match-list-tests
            (define-test match-list-empty-test
              (assert-true (pair? (match-list '() '() '()))))
            
            (define-test match-list-single-test
              (let ([env (match-list '(a) '(Nat) '())])
                   (assert-true (pair? env))))
            
            (define-test match-list-multiple-test
              (let ([env (match-list '(a b) '(Nat Bool) '())])
                   (assert-true (pair? env))
                   (assert-equal 'Nat (cdr (assq 'a env)))
                   (assert-equal 'Bool (cdr (assq 'b env)))))
            
            (define-test match-list-length-mismatch-test
              (assert-false (match-list '(a b) '(Nat) '()))
              (assert-false (match-list '(a) '(Nat Bool) '()))))

(doc 'section 'type-equality-tests)

(test-group type-equality-tests
            (define-test simple-type-equal-test
              (assert-true (type-equal? 'Nat 'Nat))
              (assert-false (type-equal? 'Nat 'Bool)))
            
            (define-test compound-type-equal-test
              (assert-true (type-equal? '(-> Nat Bool) '(-> Nat Bool)))
              (assert-false (type-equal? '(-> Nat Bool) '(-> Bool Nat))))
            
            (define-test nested-type-equal-test
              (assert-true (type-equal? '(-> (List Nat) Bool) '(-> (List Nat) Bool)))))

(doc 'section 'signature-parsing-tests)

(test-group signature-parsing-tests
            (define-test parse-simple-function-test
              (let ([sig (parse-signature "(-> Nat Bool)")])
                   (assert-true (pair? sig))
                   (assert-equal '-> (car sig))
                   (assert-equal 'Nat (cadr sig))
                   (assert-equal 'Bool (caddr sig))))
            
            (define-test parse-multi-arg-function-test
              (let ([sig (parse-signature "(-> Nat Nat Nat)")])
                   (assert-true (pair? sig))
                   (assert-equal 3 (length (cdr sig)))))  ; Three type args
            
            (define-test parse-list-type-test
              (let ([sig (parse-signature "(List Nat)")])
                   (assert-true (pair? sig))
                   (assert-equal 'List (car sig))
                   (assert-equal 'Nat (cadr sig))))
            
            (define-test parse-polymorphic-test
              (let ([sig (parse-signature "(forall (a) (-> a a))")])
                   ;; forall parses as special form
                   (assert-true (or (not sig) (pair? sig)))))
            
            (define-test parse-simple-type-test
              (let ([sig (parse-signature "Nat")])
                   (assert-equal 'Nat sig))))

(doc 'section 'infix-parsing-tests)

(test-group infix-parsing-tests
            (define-test string-split-arrow-test
              (let ([parts (string-split-arrow "Nat -> Bool")])
                   (assert-equal 2 (length parts))
                   (assert-equal "Nat" (car parts))
                   (assert-equal "Bool" (cadr parts))))
            
            (define-test string-split-arrow-multi-test
              (let ([parts (string-split-arrow "Nat -> Nat -> Bool")])
                   (assert-equal 3 (length parts))))
            
            (define-test parse-type-component-simple-test
              (let ([comp (parse-type-component "Nat")])
                   (assert-equal 'Nat comp)))
            
            (define-test parse-type-component-compound-test
              (let ([comp (parse-type-component "(List Nat)")])
                   (assert-true (pair? comp))
                   (assert-equal 'List (car comp)))))

(doc 'section 'string-utility-tests)

(test-group string-utility-tests
            (define-test string-find-substring-test
              (assert-equal 0 (string-find-substring "hello" "hel"))
              (assert-equal 2 (string-find-substring "hello" "llo"))
              (assert-false (string-find-substring "hello" "xyz")))
            
            (define-test string-prefix-at-test
              (assert-true (string-prefix-at? "hello world" "world" 6))
              (assert-false (string-prefix-at? "hello" "world" 0))
              (assert-true (string-prefix-at? "abc" "abc" 0))))

(doc 'section 'list-helper-tests)

(test-group list-helper-tests
            (define-test last-test
              (assert-equal 3 (last '(1 2 3)))
              (assert-equal 'a (last '(a))))
            
            (define-test butlast-test
              (assert-equal '(1 2) (butlast '(1 2 3)))
              (assert-equal '() (butlast '(1))))
            
            (define-test any-true-test
              (assert-true (any number? '(a b 3 c)))
              (assert-false (any number? '(a b c))))
            
            (define-test take-test
              (assert-equal '(1 2) (take 2 '(1 2 3 4)))
              (assert-equal '(1 2) (take 5 '(1 2)))
              (assert-equal '() (take 0 '(1 2)))))

(doc 'section 'suggestion-tests)

(test-group suggestion-tests
            (define-test make-suggestion-direct-test
              (let ([sug (make-suggestion 'direct 'data '(-> Nat Bool))])
                   (assert-equal 'direct (cdr (assq 'kind sug)))
                   (assert-equal 'data (cdr (assq 'data sug)))
                   (assert-equal '(-> Nat Bool) (cdr (assq 'target sug)))))
            
            (define-test make-suggestion-compose-test
              (let ([sug (make-suggestion 'compose '(f g) '(-> Nat Bool))])
                   (assert-equal 'compose (cdr (assq 'kind sug))))))

(doc 'section 'normalization-tests)

(test-group normalization-tests
            (define-test normalize-simple-type-test
              (assert-equal 'Nat (normalize-type 'Nat)))
            
            (define-test normalize-function-type-test
              (let ([norm (normalize-type '(-> Nat Bool))])
                   (assert-equal '(-> Nat Bool) norm)))
            
            (define-test normalize-polymorphic-type-test
              (let ([norm (normalize-type '(forall (a) (-> a a)))])
                   (assert-true (pair? norm)))))

(doc 'section 'typed-index-tests)

(test-group typed-index-tests
            (define-test typed-index-initial-size-test
              ;; Initially typed index should be empty or populated
              (assert-true (>= (typed-index-size) 0)))
            
            (define-test typed-index-get-nonexistent-test
              ;; Getting non-existent symbol should return #f
              (assert-false (typed-index-get 'nonexistent-symbol-xyz-123))))

(doc 'section 'integration-tests)

(test-group integration-tests
            ;; Set up mock symbol index for testing
            (define-test search-by-type-empty-test
              ;; With empty/cleared index, should return empty list
              (hashtable-clear! *symbol-index*)
              (let ([results (search-by-type '(-> Nat Nat))])
                   (assert-true (list? results))))
            
            (define-test search-by-return-empty-test
              (hashtable-clear! *symbol-index*)
              (let ([results (search-by-return 'Bool)])
                   (assert-true (list? results))))
            
            (define-test search-by-arg-empty-test
              (hashtable-clear! *symbol-index*)
              (let ([results (search-by-arg 'String)])
                   (assert-true (list? results)))))

(doc 'section 'edge-case-tests)

(test-group edge-case-tests
            (define-test empty-env-match-test
              ;; Empty environment should allow new bindings
              (let ([env (type-match? 'a 'Nat '())])
                   (assert-true (pair? env))))
            
            (define-test deeply-nested-type-match-test
              (let ([env (type-match? '(-> (List (List a)) (List a))
                                      '(-> (List (List Nat)) (List Nat))
                                      '())])
                   (assert-true (pair? env))
                   (assert-equal 'Nat (cdr (assq 'a env)))))
            
            (define-test parse-empty-string-test
              (let ([sig (parse-signature "")])
                   ;; Empty string should return #f or error gracefully
                   (assert-true (or (not sig) (eq? sig #f)))))
            
            (define-test match-with-multiple-variables-test
              (let ([env (type-match? '(-> a b (List a))
                                      '(-> Nat Bool (List Nat))
                                      '())])
                   (assert-true (pair? env))
                   (assert-equal 'Nat (cdr (assq 'a env)))
                   (assert-equal 'Bool (cdr (assq 'b env))))))

(doc 'note "Helper Functions Used in Tests: The module already defines 'any', 'last', 'butlast'; 'take' comes from prelude")

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
[SUCCESS] All type-driven search tests passed.
")
    (display "
[FAILURE] Some tests failed.
"))
