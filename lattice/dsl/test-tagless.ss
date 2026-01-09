;;; fabric/stitches/dsl/test-tagless.ss — Tests for Tagless Final
;;;
;;; Comprehensive tests for the tagless final DSL pattern.

(load "core/test-framework.ss")
(load "lattice/dsl/tagless.ss")

;;; ============================================================
;;; Dictionary Tests
;;; ============================================================

(test-group tagless-dict-basics
            (define-test make-dict-creates-dictionary
              (let ([d (make-dict 'test `((foo . ,(lambda (x) x))))])
                   (assert-equal 'test (dict-tag d))))
            
            (define-test dict-ref-finds-operation
              (let ([d (make-dict 'test `((add . ,(lambda (x y) (+ x y)))))])
                   (assert-equal 5 ((dict-ref d 'add) 2 3))))
            
            (define-test dict-has-checks-presence
              (let ([d (make-dict 'test `((foo . ,(lambda () 1))))])
                   (assert-true (dict-has? d 'foo))
                   (assert-false (dict-has? d 'bar))))
            
            (define-test dict-extend-adds-operations
              (let* ([d1 (make-dict 'test `((foo . ,(lambda () 1))))]
                     [d2 (dict-extend d1 `((bar . ,(lambda () 2))))])
                    (assert-true (dict-has? d2 'foo))
                    (assert-true (dict-has? d2 'bar))))
            
            (define-test dict-merge-combines-dicts
              (let* ([d1 (make-dict 'a `((x . ,(lambda () 1))))]
                     [d2 (make-dict 'b `((y . ,(lambda () 2))))]
                     [d3 (dict-merge d1 d2)])
                    (assert-true (dict-has? d3 'x))
                    (assert-true (dict-has? d3 'y)))))

;;; ============================================================
;;; Expression Algebra Tests
;;; ============================================================

(test-group tagless-expr-eval
            (define-test eval-lit
              (assert-equal 42 (expr-lit eval-expr-dict 42)))
            
            (define-test eval-add
              (assert-equal 7 (expr-add eval-expr-dict 3 4)))
            
            (define-test eval-neg
              (assert-equal -5 (expr-neg eval-expr-dict 5)))
            
            (define-test eval-complex-expr
              ;; 8 + (-(1 + 2)) = 8 + (-3) = 5
              (assert-equal 5 (example-expr-1 eval-expr-dict)))
            
            (define-test eval-double-neg
              ;; -(-(5)) = 5
              (assert-equal 5 (example-expr-2 eval-expr-dict))))

(test-group tagless-expr-pretty
            (define-test pretty-lit
              (assert-equal "42" (expr-lit pretty-expr-dict 42)))
            
            (define-test pretty-add
              (assert-equal "(3 + 4)" (expr-add pretty-expr-dict "3" "4")))
            
            (define-test pretty-neg
              (assert-equal "(-5)" (expr-neg pretty-expr-dict "5")))
            
            (define-test pretty-complex-expr
              (assert-equal "(8 + (-(1 + 2)))" (example-expr-1 pretty-expr-dict))))

(test-group tagless-expr-count
            (define-test count-lit
              (assert-equal 0 (expr-lit count-expr-dict 42)))
            
            (define-test count-add
              ;; 1 for add + 0 for each lit child
              (assert-equal 1 (expr-add count-expr-dict 0 0)))
            
            (define-test count-complex-expr
              ;; add + neg + add = 3 operations, plus recursive counts
              ;; outer add: 1 + 0 (lit) + inner
              ;; inner neg: 1 + inner add
              ;; inner add: 1 + 0 + 0
              ;; Total: 1 + 0 + 1 + 1 + 0 + 0 = 3... let's trace
              ;; Actually: add(lit(8), neg(add(lit(1), lit(2))))
              ;; lit(8) = 0
              ;; lit(1) = 0, lit(2) = 0
              ;; add(0, 0) = 1
              ;; neg(1) = 2
              ;; add(0, 2) = 3
              ;; Wait that's wrong too. Let me trace properly:
              ;; add(x, y) = 1 + x + y
              ;; neg(x) = 1 + x
              ;; lit(n) = 0
              ;; So: add(lit(8), neg(add(lit(1), lit(2))))
              ;;   = 1 + lit(8) + neg(add(lit(1), lit(2)))
              ;;   = 1 + 0 + (1 + add(lit(1), lit(2)))
              ;;   = 1 + 0 + 1 + (1 + lit(1) + lit(2))
              ;;   = 1 + 0 + 1 + 1 + 0 + 0 = 3
              ;; Hmm, but depth would be 4. Let me check the actual implementation.
              ;; Oh wait, count is counting operations not nodes.
              ;; Actually: 1 (outer add) + 0 (lit 8) + (1 (neg) + (1 (inner add) + 0 + 0))
              ;; = 1 + 0 + 1 + 1 + 0 + 0 = 3
              ;; But the original said 4... let me check example-expr-1
              ;; add(lit(8), neg(add(lit(1), lit(2)))) should have 3 ops: add, neg, add
              (assert-equal 3 (example-expr-1 count-expr-dict))))

(test-group tagless-expr-depth
            (define-test depth-lit
              (assert-equal 1 (expr-lit depth-expr-dict 42)))
            
            (define-test depth-complex-expr
              ;; add(lit(8), neg(add(lit(1), lit(2))))
              ;; Depth: add -> max(lit(8)=1, neg->add->max(lit,lit)=1 => 2 => 3) = 3 + 1 = 4
              (assert-equal 4 (example-expr-1 depth-expr-dict))))

;;; ============================================================
;;; Extensibility Tests
;;; ============================================================

(test-group tagless-extensibility
            (define-test mul-extends-expr
              ;; (2 * 3) + 4 = 10
              (assert-equal 10 (example-mul eval-mul-dict)))
            
            (define-test mul-extends-pretty
              (assert-equal "((2 * 3) + 4)" (example-mul pretty-mul-dict))))

;;; ============================================================
;;; Boolean Algebra Tests
;;; ============================================================

(test-group tagless-bool
            (define-test bool-and-true
              (assert-true (bool-and eval-bool-dict #t #t)))
            
            (define-test bool-and-false
              (assert-false (bool-and eval-bool-dict #t #f)))
            
            (define-test bool-or-true
              (assert-true (bool-or eval-bool-dict #f #t)))
            
            (define-test bool-not
              (assert-false (bool-not eval-bool-dict #t)))
            
            (define-test bool-complex
              ;; (true && false) || (!false) = false || true = true
              (let ([d eval-bool-dict])
                   (assert-true
                    (bool-or d
                             (bool-and d #t #f)
                             (bool-not d #f)))))
            
            (define-test bool-pretty
              (assert-equal "(true && false)"
                            (bool-and pretty-bool-dict "true" "false"))))

;;; ============================================================
;;; Environment-Based Expression Tests
;;; ============================================================

(test-group tagless-env-expr
            (define-test env-lit-ignores-env
              (let ([d (make-env-expr-dict)])
                   (assert-equal 42 (run-env-expr d (env-lit d 42)))))
            
            (define-test env-add-combines
              (let ([d (make-env-expr-dict)])
                   (assert-equal 7 (run-env-expr d (env-add d (env-lit d 3) (env-lit d 4))))))
            
            (define-test env-let-binds
              (let ([d (make-env-expr-dict)])
                   ;; let x = 5 in x
                   (assert-equal 5
                                 (run-env-expr d
                                               (env-let d 'x (env-lit d 5)
                                                        (env-var d 'x))))))
            
            (define-test env-let-nested
              (let ([d (make-env-expr-dict)])
                   ;; let x = 5 in let y = 3 in x + y
                   (assert-equal 8 (run-env-expr d (example-let d))))))

;;; ============================================================
;;; State Monad Tests
;;; ============================================================

(test-group tagless-state
            (define-test state-pure
              (let ([comp (state-pure state-monad-dict 42)])
                   (assert-equal '(42 . initial)
                                 (run-state comp 'initial))))
            
            (define-test state-get
              (let ([comp (state-get state-monad-dict)])
                   (assert-equal '(foo . foo)
                                 (run-state comp 'foo))))
            
            (define-test state-put
              (let ([comp (state-put state-monad-dict 'new-state)])
                   (assert-equal '(() . new-state)
                                 (run-state comp 'old-state))))
            
            (define-test state-bind
              (let* ([d state-monad-dict]
                     [comp (state-bind d
                                       (state-get d)
                                       (lambda (s)
                                               (state-put d (cons 'modified s))))])
                    (assert-equal '(() . (modified . original))
                                  (run-state comp 'original)))))

;;; ============================================================
;;; Multiple Interpreter Tests
;;; ============================================================

(test-group tagless-run-both
            (define-test run-both-eval-and-count
              (let ([result (run-both example-expr-1 eval-expr-dict count-expr-dict)])
                   (assert-equal 5 (car result))   ; eval result
                   (assert-equal 3 (cdr result)))) ; count result
            
            (define-test run-both-eval-and-pretty
              (let ([result (run-both example-expr-1 eval-expr-dict pretty-expr-dict)])
                   (assert-equal 5 (car result))
                   (assert-equal "(8 + (-(1 + 2)))" (cdr result)))))

;;; ============================================================
;;; Free Monad Bridge Tests
;;; ============================================================

(test-group tagless-free-bridge
            (define-test tagless-to-free-produces-ast
              (let ([ast (tagless-to-free example-expr-1)])
                   ;; Should produce: (add (lit 8) (neg (add (lit 1) (lit 2))))
                   (assert-equal 'add (car ast))
                   (assert-equal '(lit 8) (cadr ast))))
            
            (define-test free-to-tagless-roundtrip
              (let* ([ast (tagless-to-free example-expr-1)]
                     [program (free-to-tagless ast)])
                    (assert-equal 5 (program eval-expr-dict))))
            
            (define-test optimize-eliminates-double-neg
              (let ([ast '(neg (neg (lit 5)))])
                   (assert-equal '(lit 5) (optimize-expr ast))))
            
            (define-test optimize-eliminates-add-zero
              (let ([ast '(add (lit 0) (lit 5))])
                   (assert-equal '(lit 5) (optimize-expr ast))))
            
            (define-test run-optimized-works
              ;; -(-(5)) should optimize to 5
              (assert-equal 5 (run-optimized example-expr-2 eval-expr-dict))))

;;; ============================================================
;;; Dictionary Transformer Tests
;;; ============================================================

(test-group tagless-transformers
            (define-test with-tracing-still-works
              ;; Just verify it doesn't break functionality
              (let ([d (with-tracing eval-expr-dict)])
                   (assert-equal 5 (example-expr-1 d)))))

;;; ============================================================
;;; Typed Expression Tests
;;; ============================================================

(test-group tagless-typed
            (define-test typed-lit-creates-int
              (let ([v ((dict-ref eval-typed-dict 'lit) 42)])
                   (assert-equal 'int (typed-type v))
                   (assert-equal 42 (typed-value v))))
            
            (define-test typed-add-preserves-type
              (let* ([x (typed-int 3)]
                     [y (typed-int 4)]
                     [result ((dict-ref eval-typed-dict 'add) x y)])
                    (assert-equal 'int (typed-type result))
                    (assert-equal 7 (typed-value result))))
            
            (define-test typed-eq-returns-bool
              (let* ([x (typed-int 5)]
                     [y (typed-int 5)]
                     [result ((dict-ref eval-typed-dict 'eq) x y)])
                    (assert-equal 'bool (typed-type result))
                    (assert-true (typed-value result))))
            
            (define-test typed-if-branches
              (let* ([c (typed-bool #t)]
                     [t (typed-int 1)]
                     [e (typed-int 2)]
                     [result ((dict-ref eval-typed-dict 'if) c t e)])
                    (assert-equal 1 (typed-value result)))))

;;; ============================================================
;;; Partial Evaluation Tests
;;; ============================================================

(test-group tagless-partial-eval
            (define-test pe-lit-is-static
              (let* ([d (make-pe-dict)]
                     [result ((dict-ref d 'lit) 42)])
                    (assert-equal 'static (car result))
                    (assert-equal 42 (cadr result))))
            
            (define-test pe-add-static-folds
              (let* ([d (make-pe-dict)]
                     [x ((dict-ref d 'lit) 3)]
                     [y ((dict-ref d 'lit) 4)]
                     [result ((dict-ref d 'add) x y)])
                    (assert-equal 'static (car result))
                    (assert-equal 7 (cadr result))))
            
            (define-test pe-neg-static-folds
              (let* ([d (make-pe-dict)]
                     [x ((dict-ref d 'lit) 5)]
                     [result ((dict-ref d 'neg) x)])
                    (assert-equal 'static (car result))
                    (assert-equal -5 (cadr result)))))

;;; ============================================================
;;; Run All Tests
;;; ============================================================

(display "\n========================================\n")
(display "Running Tagless Final Tests\n")
(display "========================================\n\n")

(run-all-tests)
