;;; core/dsl/test-compose.ss --- Tests for Modular DSL Composition
;;;
;;; Comprehensive tests for the three DSL composition strategies:
;;; 1. Data Types à la Carte
;;; 2. Effect Composition
;;; 3. Tagless Composition

(load "core/lang/module.ss")
(load "core/test-framework.ss")
(load "lattice/dsl/compose.ss")

;;; ====
;;; Part 1: Data Types à la Carte Tests
;;; ====

(test-group coproduct-basics
            (define-test inl-creates-left-injection
              (let ([x (inl 'foo)])
                   (assert-true (inl? x))
                   (assert-false (inr? x))))
            
            (define-test inr-creates-right-injection
              (let ([x (inr 'bar)])
                   (assert-true (inr? x))
                   (assert-false (inl? x))))
            
            (define-test from-inl-extracts-value
              (let ([x (inl 42)])
                   (assert-equal 42 (from-inl x))))
            
            (define-test from-inr-extracts-value
              (let ([x (inr "hello")])
                   (assert-equal "hello" (from-inr x))))
            
            (define-test coproduct-fmap-maps-left
              (let* ([x (inl 5)]
                     [fmap (make-coproduct-fmap
                             (lambda (f v) (f v))    ;; fmap for left (identity functor)
                             (lambda (f v) (f v)))]  ;; fmap for right
                     [result (fmap (lambda (n) (* n 2)) x)])
                    (assert-true (inl? result))
                    (assert-equal 10 (from-inl result))))

            (define-test coproduct-fmap-maps-right
              (let* ([x (inr "hi")]
                     [fmap (make-coproduct-fmap
                             (lambda (f v) (f v))
                             (lambda (f v) (f v)))]
                     [result (fmap (lambda (s) (string-append s "!")) x)])
                    (assert-true (inr? result))
                    (assert-equal "hi!" (from-inr result)))))

(test-group functor-row-basics
            (define-test make-functor-row-creates-row
              (let ([row (make-functor-row '(A B C))])
                   (assert-true (functor-row? row))
                   (assert-equal '(A B C) (functor-row-functors row))))
            
            (define-test functor-row-contains-finds-functor
              (let ([row (make-functor-row '(A B C))])
                   (assert-true (functor-row-contains? row 'A))
                   (assert-true (functor-row-contains? row 'B))
                   (assert-false (functor-row-contains? row 'X))))
            
            (define-test functor-row-index-finds-position
              (let ([row (make-functor-row '(A B C))])
                   (assert-equal 0 (functor-row-index row 'A))
                   (assert-equal 1 (functor-row-index row 'B))
                   (assert-equal 2 (functor-row-index row 'C))
                   (assert-false (functor-row-index row 'X))))
            
            (define-test functor-row-extend-adds-functor
              (let* ([row1 (make-functor-row '(A B))]
                     [row2 (functor-row-extend row1 'C)])
                    (assert-true (functor-row-contains? row2 'C))
                    (assert-equal 3 (length (functor-row-functors row2)))))
            
            (define-test functor-row-extend-no-duplicates
              (let* ([row1 (make-functor-row '(A B))]
                     [row2 (functor-row-extend row1 'A)])
                    (assert-equal 2 (length (functor-row-functors row2)))))
            
            (define-test functor-row-union-combines-rows
              (let* ([row1 (make-functor-row '(A B))]
                     [row2 (make-functor-row '(B C))]
                     [row3 (functor-row-union row1 row2)])
                    (assert-true (functor-row-contains? row3 'A))
                    (assert-true (functor-row-contains? row3 'B))
                    (assert-true (functor-row-contains? row3 'C)))))

(test-group injection-path
            (define-test build-injection-path-index-0
              (let ([path (build-injection-path 0 'value)])
                   (assert-true (inl? path))
                   (assert-equal 'value (from-inl path))))
            
            (define-test build-injection-path-index-1
              (let ([path (build-injection-path 1 'value)])
                   (assert-true (inr? path))
                   (assert-true (inl? (from-inr path)))
                   (assert-equal 'value (from-inl (from-inr path)))))
            
            (define-test build-injection-path-index-2
              (let ([path (build-injection-path 2 'value)])
                   (assert-true (inr? path))
                   (assert-true (inr? (from-inr path)))
                   (assert-true (inl? (from-inr (from-inr path))))))
            
            (define-test tagged-functor-creation
              (let ([tf (make-tagged-functor 'Arith '(lit 5))])
                   (assert-true (tagged-functor? tf))
                   (assert-equal 'Arith (tagged-functor-tag tf))
                   (assert-equal '(lit 5) (tagged-functor-value tf))))
            
            (define-test inject-uses-row-index
              (let* ([row (make-functor-row '(Arith State))]
                     [tf (make-tagged-functor 'Arith '(lit 5))]
                     [path (inject row tf)])
                    ;; Arith is at index 0, so should be inl
                    (assert-true (inl? path)))))

;;; ====
;;; Part 2: Effect Composition Tests
;;; ====

(test-group handler-stack
            (define-test make-handler-stack-creates-stack
              (let ([stack (make-handler-stack '())])
                   (assert-true (handler-stack? stack))))
            
            (define-test push-handler-adds-to-stack
              (let* ([h (deep-handler identity '())]
                     [stack1 (make-handler-stack '())]
                     [stack2 (push-handler h stack1)])
                    (assert-equal 1 (length (handler-stack-handlers stack2)))))
            
            (define-test pop-handler-removes-from-stack
              (let* ([h (deep-handler identity '())]
                     [stack1 (make-handler-stack (list h))]
                     [stack2 (pop-handler stack1)])
                    (assert-equal 0 (length (handler-stack-handlers stack2))))))

(test-group effect-row-operations
            (define-test effect-row-handled-removes-effect
              (let* ([row (make-effect-row '(State Writer))]
                     [row2 (effect-row-handled row 'State)])
                    (assert-false (row-contains? row2 'State))
                    (assert-true (row-contains? row2 'Writer))))
            
            (define-test effect-row-satisfies-checks-subset
              (let ([provided (make-effect-row '(State Writer Reader))]
                    [required (make-effect-row '(State Writer))])
                   (assert-true (effect-row-satisfies provided required))))
            
            (define-test effect-row-satisfies-fails-on-missing
              (let ([provided (make-effect-row '(State))]
                    [required (make-effect-row '(State Writer))])
                   (assert-false (effect-row-satisfies provided required)))))

(test-group combined-effect-handlers
            (define-test state-effect-basic
              (let ([result (run-state 0
                                       (eff-bind state-get
                                                 (lambda (n)
                                                         (eff-bind (state-put (+ n 10))
                                                                   (lambda (_)
                                                                           state-get)))))])
                   (assert-equal 10 (car result))   ; value
                   (assert-equal 10 (cdr result)))) ; final state
            
            (define-test writer-effect-basic
              (let ([result (run-writer
                             (eff-bind (writer-tell "hello")
                                       (lambda (_)
                                               (eff-bind (writer-tell "world")
                                                         (lambda (_)
                                                                 (eff-return 42))))))])
                   (assert-equal 42 (car result))
                   (assert-equal '("hello" "world") (cdr result))))
            
            (define-test reader-effect-basic
              (let ([result (run-reader 100
                                        (eff-bind reader-ask
                                                  (lambda (env)
                                                          (eff-return (* env 2)))))])
                   (assert-equal 200 result))))

;;; ====
;;; Part 3: Tagless Composition Tests
;;; ====

(test-group dict-algebra
            (define-test empty-dict-is-empty
              (assert-equal '() (dict-ops empty-dict)))
            
            (define-test dict-singleton-has-one-op
              (let ([d (dict-singleton 'test '(foo . ,(lambda () 1)))])
                   (assert-equal 1 (length (dict-ops d)))))
            
            (define-test dict-concat-merges-all
              (let* ([d1 (make-dict 'a '((x . 1)))]
                     [d2 (make-dict 'b '((y . 2)))]
                     [d3 (make-dict 'c '((z . 3)))]
                     [merged (dict-concat (list d1 d2 d3))])
                    (assert-equal 3 (length (dict-ops merged))))))

(test-group interface-constraints
            (define-test make-interface-creates-interface
              (let ([iface (make-interface 'MyInterface '(op1 op2))])
                   (assert-true (interface? iface))
                   (assert-equal 'MyInterface (interface-name iface))
                   (assert-equal '(op1 op2) (interface-operations iface))))
            
            (define-test dict-satisfies-with-all-ops
              (let ([d (make-dict 'test '((lit . 1) (add . 2) (neg . 3)))]
                    [iface iface-expr])
                   (assert-true (dict-satisfies? d iface))))
            
            (define-test dict-satisfies-fails-on-missing
              (let ([d (make-dict 'test '((lit . 1) (add . 2)))]  ; missing neg
                    [iface iface-expr])
                   (assert-false (dict-satisfies? d iface))))
            
            (define-test interface-union-combines-ops
              (let* ([i1 (make-interface 'I1 '(a b))]
                     [i2 (make-interface 'I2 '(c d))]
                     [i3 (interface-union i1 i2)])
                    (assert-equal 4 (length (interface-operations i3))))))

(test-group interpreter-combinators
            (define-test make-interpreter-creates-interpreter
              (let ([interp (make-interpreter 'test (lambda () empty-dict))])
                   (assert-true (interpreter? interp))
                   (assert-equal 'test (interpreter-name interp))))
            
            (define-test interpreter-make-returns-dict
              (let ([interp (make-interpreter 'test
                                              (lambda () (make-dict 'foo '((x . 1)))))])
                   (let ([d (interpreter-make interp)])
                        (assert-equal 'foo (dict-tag d)))))
            
            (define-test combine-interpreters-merges-all
              (let* ([i1 (make-interpreter 'i1
                                           (lambda () (make-dict 'a '((x . 1)))))]
                     [i2 (make-interpreter 'i2
                                           (lambda () (make-dict 'b '((y . 2)))))]
                     [combined (combine-interpreters (list i1 i2))])
                    (assert-true (dict-has? combined 'x))
                    (assert-true (dict-has? combined 'y)))))

(test-group dsl-patterns
            (define-test extend-dsl-adds-ops
              (let* ([base (make-dict 'base '((a . 1)))]
                     [extended (extend-dsl base '((b . 2)))])
                    (assert-true (dict-has? extended 'a))
                    (assert-true (dict-has? extended 'b))))
            
            (define-test restrict-dsl-limits-ops
              (let* ([full (make-dict 'full '((a . 1) (b . 2) (c . 3)))]
                     [limited (restrict-dsl full '(a c))])
                    (assert-true (dict-has? limited 'a))
                    (assert-false (dict-has? limited 'b))
                    (assert-true (dict-has? limited 'c))))
            
            (define-test rename-dsl-changes-names
              (let* ([original (make-dict 'orig '((old-name . 42)))]
                     [renamed (rename-dsl original '((old-name . new-name)))])
                    (assert-false (dict-has? renamed 'old-name))
                    (assert-true (dict-has? renamed 'new-name))))
            
            (define-test override-dsl-replaces-ops
              (let* ([base (make-dict 'base `((add . ,(lambda (x y) (+ x y)))))]
                     [overridden (override-dsl base
                                               `((add . ,(lambda (x y) (* x y)))))])
                    (assert-equal 6 ((dict-ref overridden 'add) 2 3)))))  ; 2*3 not 2+3

(test-group transformer-composition
            (define-test make-transformer-creates-transformer
              (let ([t (make-transformer 'test identity)])
                   (assert-true (transformer? t))
                   (assert-equal 'test (transformer-name t))))
            
            (define-test transformer-apply-transforms-dict
              (let* ([base (make-dict 'base '((x . 1)))]
                     [t (make-transformer 'double
                                          (lambda (d)
                                                  (make-dict 'doubled
                                                             (map (lambda (p) (cons (car p) (* 2 (cdr p))))
                                                                  (dict-ops d)))))]
                     [result (transformer-apply t base)])
                    (assert-equal 2 (dict-ref result 'x))))
            
            (define-test chain-transformers-applies-all
              (let* ([base (make-dict 'base '((x . 1)))]
                     [t1 (make-transformer 't1
                                           (lambda (d)
                                                   (dict-extend d '((from-t1 . yes)))))]
                     [t2 (make-transformer 't2
                                           (lambda (d)
                                                   (dict-extend d '((from-t2 . yes)))))]
                     [result (chain-transformers (list t1 t2) base)])
                    (assert-true (dict-has? result 'x))
                    (assert-true (dict-has? result 'from-t1))
                    (assert-true (dict-has? result 'from-t2)))))

;;; ====
;;; N-ary Row Fmap Tests
;;; ====

(test-group row-fmap
            (define-test make-row-fmap-creates-function
              (let ([row (make-functor-row '(Arith State))]
                    [fmap-fn (make-row-fmap (make-functor-row '(Arith State))
                                            standard-fmap-registry)])
                   (assert-true (procedure? fmap-fn))))
            
            (define-test row-fmap-handles-first-functor
              (let* ([row (make-functor-row '(Arith State))]
                     [fmap-fn (make-row-fmap row standard-fmap-registry)]
                     ;; Create an Arith value at index 0 (inl)
                     [value (inl (list 'neg 5))]
                     [result (fmap-fn add1 value)])
                    ;; Should still be inl after mapping
                    (assert-true (inl? result))))
            
            (define-test row-fmap-handles-second-functor
              (let* ([row (make-functor-row '(Arith State))]
                     [fmap-fn (make-row-fmap row standard-fmap-registry)]
                     ;; Create a State value at index 1 (inr (inl ...))
                     ;; Use identity since state-f-fmap applies f to continuation
                     [value (inr (inl (list 'put 42 'cont)))]
                     [result (fmap-fn identity value)])
                    ;; Should still be inr after mapping
                    (assert-true (inr? result)))))

;;; ====
;;; Tag-based DSL Operations Tests
;;; ====

(test-group tag-based-dsl
            (define-test dsl-lit-uses-tag-injection
              (let* ([row (make-functor-row '(Arith State))]
                     [term (dsl-lit row 42)])
                    ;; Should be a free term
                    (assert-true (free-suspended? term))
                    ;; The injected value should be at index 0 (Arith)
                    (assert-true (inl? (from-free term)))))
            
            (define-test dsl-get-uses-tag-injection
              (let* ([row (make-functor-row '(Arith State))]
                     [term (dsl-get row)])
                    ;; Should be a free term
                    (assert-true (free-suspended? term))
                    ;; The injected value should be at index 1 (State)
                    (assert-true (inr? (from-free term)))))
            
            (define-test dsl-operations-work-with-different-row-order
              ;; Key test: changing row order should still work
              (let* ([row (make-functor-row '(State Arith))]  ; Reversed order!
                     [lit-term (dsl-lit row 10)]
                     [get-term (dsl-get row)])
                    ;; Now Arith is at index 1, State at index 0
                    (assert-true (inr? (from-free lit-term)))   ; Arith at index 1 = inr
                    (assert-true (inl? (from-free get-term))))) ; State at index 0 = inl
            
            (define-test dsl-add-constructs-ast-node
              ;; dsl-add should build an AST node, not evaluate
              (let* ([row (make-functor-row '(Arith State))]
                     [x (dsl-lit row 3)]
                     [y (dsl-lit row 4)]
                     [add-term (dsl-add row x y)])
                    ;; Should be a free term
                    (assert-true (free-suspended? add-term))
                    ;; The inner value should be an 'add node with children
                    (let* ([injected (from-free add-term)]
                           [inner (from-inl injected)])  ; Arith at index 0
                          (assert-equal 'add (car inner))
                          ;; Children should be the original terms
                          (assert-true (free-suspended? (cadr inner)))
                          (assert-true (free-suspended? (caddr inner))))))
            )

;;; ====
;;; Optimized Handler Stack Tests
;;; ====

(test-group optimized-handler-stack
            (define-test run-with-composed-stack-works
              ;; Should produce same result as run-with-stack
              (let* ([h (deep-handler identity '())]
                     [stack (make-handler-stack (list h))]
                     [result (run-with-composed-stack stack (eff-return 42))])
                    (assert-equal 42 result))))

;;; ====
;;; Generic Tagless Bridge Tests
;;; ====

(test-group generic-tagless-bridge
            (define-test tagless->free-with-custom-dict
              (let* ([custom-ast-dict (make-ast-dict 'custom
                                                     `((lit . ,(lambda (n) (list 'NUM n)))
                                                       (add . ,(lambda (x y) (list 'PLUS x y)))
                                                       (neg . ,(lambda (x) (list 'MINUS x)))))]
                     [program (lambda (d)
                                      (expr-add d (expr-lit d 1) (expr-lit d 2)))]
                     [ast (tagless->free program custom-ast-dict)])
                    ;; Should use custom constructors
                    (assert-equal 'PLUS (car ast))))
            
            (define-test make-term-interpreter-works
              (let* ([handlers `((lit . ,(lambda (d term recurse) (cadr term)))
                                 (add . ,(lambda (d term recurse)
                                                 (+ (recurse (cadr term))
                                                    (recurse (caddr term))))))]
                     [interp (make-term-interpreter handlers)]
                     [term '(add (lit 3) (lit 4))])
                    (assert-equal 7 (interp term '()))))
            
            (define-test free->tagless-with-custom-interpreter
              (let* ([double-handlers
                      `((lit . ,(lambda (d term recurse)
                                        (* 2 (expr-lit d (cadr term)))))
                        (add . ,(lambda (d term recurse)
                                        (expr-add d (recurse (cadr term)) (recurse (caddr term)))))
                        (neg . ,(lambda (d term recurse)
                                        (expr-neg d (recurse (cadr term))))))]
                     [term '(lit 5)]
                     [program (free->tagless (make-term-interpreter double-handlers) term)]
                     [result (program eval-expr-dict)])
                    ;; Should double the literal
                    (assert-equal 10 result))))

;;; ====
;;; Integration Tests
;;; ====

(test-group composition-verification
            (define-test verify-composition-all-satisfied
              (let ([d (make-dict 'full
                                  '((lit . 1) (add . 2) (neg . 3)
                                    (bool-lit . 4) (and . 5) (or . 6) (not . 7)))])
                   (assert-true (verify-composition d (list iface-expr iface-bool)))))
            
            (define-test verify-composition-some-missing
              (let ([d (make-dict 'partial '((lit . 1) (add . 2) (neg . 3)))])
                   (assert-false (verify-composition d (list iface-expr iface-bool))))))

(test-group full-language-example
            (define-test arith-eval-interpreter-works
              (let* ([d (make-full-language-dict 'eval)]
                     [result (expr-add d (expr-lit d 3) (expr-lit d 4))])
                    (assert-equal 7 result)))
            
            (define-test arith-pretty-interpreter-works
              (let* ([d (make-full-language-dict 'pretty)]
                     [result (expr-add d (expr-lit d 3) (expr-lit d 4))])
                    (assert-equal "(3 + 4)" result))))

(test-group tagless-free-bridge
            (define-test tagless-to-free-creates-ast
              (let* ([program (lambda (d)
                                      (expr-add d (expr-lit d 2) (expr-lit d 3)))]
                     [ast (tagless-program->free program)])
                    (assert-true (pair? ast))
                    (assert-equal 'add (car ast))))
            
            (define-test free-to-tagless-evaluates
              (let* ([ast '(add (lit 10) (lit 5))]
                     [program (free-term->tagless ast)]
                     [result (program eval-expr-dict)])
                    (assert-equal 15 result))))

;;; ====
;;; Run All Tests
;;; ====

(display "\n====\n")
(display "Running DSL Composition Tests\n")
(display "====\n\n")

(run-all-tests)
