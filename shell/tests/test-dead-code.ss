;;; shell/tests/test-dead-code.ss -- Tests for Dead Code Detection

;;; NOTE: Run from project root:
;;;   scheme --script shell/tests/test-dead-code.ss

(load "core/base/prelude.ss")
(load "core/test-framework.ss")
(load "shell/tools/dead-code.ss")

(display "
")
(display "====
")
(display "          DEAD CODE DETECTION TESTS
")
(display "====
")

;;; ====
;;; Definition Extraction Tests
;;; ====

(test-group definition-extraction-tests
            (define-test extract-defs-simple-function-test
              (let ([defs (extract-defs '(define (foo x) (+ x 1)) "test.ss" 10)])
                   (assert-equal 1 (length defs))
                   (assert-equal 'foo (car (car defs)))
                   (assert-equal "test.ss" (cadr (car defs)))
                   (assert-equal 10 (caddr (car defs)))))
            
            (define-test extract-defs-simple-value-test
              (let ([defs (extract-defs '(define bar 42) "test.ss" 5)])
                   (assert-equal 1 (length defs))
                   (assert-equal 'bar (car (car defs)))))
            
            (define-test extract-defs-define-syntax-test
              (let ([defs (extract-defs '(define-syntax my-macro ...) "test.ss" 1)])
                   (assert-equal 1 (length defs))
                   (assert-equal 'my-macro (car (car defs)))))
            
            (define-test extract-defs-non-define-test
              (let ([defs (extract-defs '(+ 1 2) "test.ss" 1)])
                   (assert-equal 0 (length defs))))
            
            (define-test extract-defs-lambda-value-test
              (let ([defs (extract-defs '(define foo (lambda (x) x)) "test.ss" 1)])
                   (assert-equal 1 (length defs))
                   (assert-equal 'foo (car (car defs))))))

;;; ====
;;; Reference Collection Tests
;;; ====

(test-group reference-collection-tests
            (define-test walk-for-refs-symbol-test
              ;; Reset reference table
              (hashtable-clear! *all-references*)
              (walk-for-refs 'test-symbol "test.ss")
              (assert-equal 1 (get-ref-count 'test-symbol)))
            
            (define-test walk-for-refs-multiple-test
              (hashtable-clear! *all-references*)
              (walk-for-refs '(foo bar foo baz) "test.ss")
              (assert-equal 2 (get-ref-count 'foo))
              (assert-equal 1 (get-ref-count 'bar))
              (assert-equal 1 (get-ref-count 'baz)))
            
            (define-test walk-for-refs-nested-test
              (hashtable-clear! *all-references*)
              (walk-for-refs '((nested (deeply (foo)))) "test.ss")
              (assert-equal 1 (get-ref-count 'foo))
              (assert-equal 1 (get-ref-count 'deeply))
              (assert-equal 1 (get-ref-count 'nested)))
            
            (define-test get-ref-files-test
              (hashtable-clear! *all-references*)
              (walk-for-refs 'shared-sym "file1.ss")
              (walk-for-refs 'shared-sym "file2.ss")
              (let ([files (get-ref-files 'shared-sym)])
                   (assert-equal 2 (length files))
                   (assert-true (if (member "file1.ss" files) #t #f))
                   (assert-true (if (member "file2.ss" files) #t #f)))))

;;; ====
;;; String Utility Tests
;;; ====

(test-group string-utility-tests
            (define-test string-contains-test
              (assert-true (string-contains? "hello world" "world"))
              (assert-true (string-contains? "hello world" "hello"))
              (assert-false (string-contains? "hello world" "xyz"))
              (assert-true (string-contains? "test" "test")))
            
            (define-test string-prefix-at-test
              (assert-true (string-prefix-at? "hello" "hel" 0))
              (assert-true (string-prefix-at? "hello" "llo" 2))
              (assert-false (string-prefix-at? "hello" "xyz" 0))
              (assert-false (string-prefix-at? "hi" "hello" 0))))

;;; ====
;;; Test File Detection Tests
;;; ====

(test-group test-detection-tests
            (define-test test-file-prefix-test
              (assert-true (test-file? "test-foo.ss"))
              (assert-true (test-file? "core/test-block.ss")))
            
            (define-test test-file-suffix-test
              (assert-true (test-file? "foo-test.ss")))
            
            (define-test test-file-directory-test
              (assert-true (test-file? "shell/tests/foo.ss"))
              (assert-true (test-file? "core/test/bar.ss")))
            
            (define-test test-file-non-test-test
              (assert-false (test-file? "core/blocks/block.ss"))
              (assert-false (test-file? "shell/repl.ss")))
            
            (define-test all-test-files-test
              (assert-true (all-test-files? '("test-a.ss" "test-b.ss")))
              (assert-false (all-test-files? '("test-a.ss" "normal.ss")))
              (assert-false (all-test-files? '()))))

;;; ====
;;; Test Related Symbol Tests
;;; ====

(test-group test-related-tests
            (define-test test-related-prefix-test
              (assert-true (test-related? 'test-foo))
              (assert-true (test-related? 'check-something))
              (assert-true (test-related? 'assert-equal)))
            
            (define-test test-related-suffix-test
              (assert-true (test-related? 'foo-test)))
            
            (define-test test-related-negative-test
              (assert-false (test-related? 'regular-function))
              (assert-false (test-related? 'testy))  ; Not a prefix
              (assert-false (test-related? 'protest))))  ; Contains test but not test-related

;;; ====
;;; Exported Symbol Tests
;;; ====

(test-group exported-tests
            (define-test exported-api-pattern-test
              (assert-true (exported? 'dead-code-scan "dead-code.ss"))
              (assert-true (exported? 'refactor-rename "refactor.ss"))
              (assert-true (exported? 'type-search-help "type-search.ss")))
            
            (define-test exported-private-test
              ;; Underscore prefix is never exported
              (assert-false (exported? '_internal-helper "any.ss"))))

;;; ====
;;; Unused Local Binding Tests
;;; ====

(test-group unused-locals-tests
            (define-test collect-body-refs-test
              (let ([refs (collect-body-refs '(+ x y z))])
                   (assert-true (if (member 'x refs) #t #f))
                   (assert-true (if (member 'y refs) #t #f))
                   (assert-true (if (member 'z refs) #t #f))
                   (assert-true (if (member '+ refs) #t #f))))
            
            (define-test collect-body-refs-nested-test
              (let ([refs (collect-body-refs '(let ([a 1]) (if a (foo b) (bar c))))])
                   (assert-true (if (member 'a refs) #t #f))
                   (assert-true (if (member 'foo refs) #t #f))
                   (assert-true (if (member 'b refs) #t #f))))
            
            (define-test find-unused-in-expr-let-test
              ;; Test finding unused binding in let
              (let ([unused (find-unused-in-expr
                             '(let ([unused-var 1] [used-var 2])
                               (+ used-var 3))
                             "test.ss" 1)])
                   ;; Should find unused-var as unused
                   (assert-true (any (lambda (u) (eq? (car u) 'unused-var)) unused))
                   ;; Should NOT find used-var
                   (assert-false (any (lambda (u) (eq? (car u) 'used-var)) unused))))
            
            (define-test find-unused-in-expr-lambda-test
              ;; Test finding unused lambda parameter
              (let ([unused (find-unused-in-expr
                             '(lambda (used unused) used)
                             "test.ss" 1)])
                   (assert-true (any (lambda (u) (eq? (car u) 'unused)) unused))
                   (assert-false (any (lambda (u) (eq? (car u) 'used)) unused)))))

;;; ====
;;; Classification Tests
;;; ====

(test-group classification-tests
            (define-test classify-definitely-dead-test
              ;; Reset state
              (hashtable-clear! *all-references*)
              ;; A symbol with no references should be definitely dead
              (let ([result (classify-definition '(my-unused-func "test.ss" 1))])
                   (when result
                         (assert-equal 'definitely-dead (list-ref result 3)))))
            
            (define-test classify-ignore-test-related-test
              ;; Test-related symbols should be ignored (return #f)
              (let ([result (classify-definition '(test-something "test.ss" 1))])
                   (assert-false result)))
            
            (define-test classify-ignore-builtin-test
              ;; Built-in symbols should be ignored
              (let ([result (classify-definition '(define "test.ss" 1))])
                   (assert-false result))))

;;; ====
;;; File Finding Tests
;;; ====

(test-group file-finding-tests
            (define-test find-scheme-files-test
              ;; Should find .ss files
              (let ([files (find-scheme-files "core/base")])
                   (assert-true (list? files))
                   (assert-true (> (length files) 0))
                   ;; All files should end in .ss
                   (assert-true (every (lambda (f) (string-ends-with? f ".ss")) files)))))

;;; ====
;;; Group By File Tests
;;; ====

(test-group grouping-tests
            (define-test group-by-file-test
              (let* ([defs '((sym1 "a.ss" 1) (sym2 "a.ss" 5) (sym3 "b.ss" 1))]
                     [groups (group-by-file defs)])
                    (assert-equal 2 (length groups))
                    ;; Find a.ss group
                    (let ([a-group (assoc "a.ss" groups)])
                         (assert-true (pair? a-group))
                         (assert-equal 2 (length (cdr a-group))))))
            
            (define-test sort-groups-test
              (let* ([unsorted '(("z.ss" . ()) ("a.ss" . ()) ("m.ss" . ()))]
                     [sorted (sort-groups unsorted)])
                    (assert-equal "a.ss" (caar sorted))
                    (assert-equal "z.ss" (car (caddr sorted))))))

;;; ====
;;; Take Up To Helper Tests
;;; ====

(test-group helper-tests
            (define-test take-up-to-normal-test
              (assert-equal '(1 2 3) (take-up-to '(1 2 3 4 5) 3)))
            
            (define-test take-up-to-short-list-test
              (assert-equal '(1 2) (take-up-to '(1 2) 5)))
            
            (define-test take-up-to-zero-test
              (assert-equal '() (take-up-to '(1 2 3) 0)))
            
            (define-test take-up-to-empty-test
              (assert-equal '() (take-up-to '() 5))))

;;; ====
;;; Find Definition Helper Tests
;;; ====

(test-group find-def-tests
            (define-test find-def-in-list-found-test
              (let* ([defs '((foo "a.ss" 1) (bar "b.ss" 2) (baz "c.ss" 3))]
                     [result (find-def-in-list 'bar defs)])
                    (assert-true (pair? result))
                    (assert-equal 'bar (car result))
                    (assert-equal "b.ss" (cadr result))))
            
            (define-test find-def-in-list-not-found-test
              (let* ([defs '((foo "a.ss" 1) (bar "b.ss" 2))]
                     [result (find-def-in-list 'qux defs)])
                    (assert-false result))))

;;; ====
;;; Andmap Helper Tests
;;; ====

(test-group andmap-tests
            (define-test andmap-all-true-test
              (assert-true (andmap number? '(1 2 3 4))))
            
            (define-test andmap-some-false-test
              (assert-false (andmap number? '(1 2 "three" 4))))
            
            (define-test andmap-empty-test
              (assert-true (andmap number? '()))))

;;; ====
;;; State Management Tests
;;; ====

(test-group state-tests
            (define-test state-initialization-test
              ;; Verify state variables exist
              (assert-true (list? *all-definitions*))
              (assert-true (hashtable? *all-references*))
              (assert-true (list? *unused-symbols*))
              (assert-true (list? *dead-code-by-confidence*))))

;;; ====
;;; Helper Functions Used in Tests
;;; ====

;;; any : (a -> Bool) x (List a) -> Bool
(define (any pred lst)
  (cond
   [(null? lst) #f]
   [(pred (car lst)) #t]
   [else (any pred (cdr lst))]))

;;; every : (a -> Bool) x (List a) -> Bool
(define (every pred lst)
  (cond
   [(null? lst) #t]
   [(not (pred (car lst))) #f]
   [else (every pred (cdr lst))]))

;;; ====
;;; Run Tests
;;; ====

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
[SUCCESS] All dead code detection tests passed.
")
    (display "
[FAILURE] Some tests failed.
"))
