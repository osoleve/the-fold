;;; Test harness for type signature parser

(load "core/test-framework.ss")
(load "core/types/sig-parser.ss")

(display "
================================================================================
                    TYPE SIGNATURE PARSER TESTS
================================================================================
")

;;; ============================================================
;;; Tokenizer Tests
;;; ============================================================

(test-group tokenizer
            
            (define-test tokenize-simple-type
              (let ([result (tokenize-sig "Int")])
                   (assert-true (eq? (car result) 'ok))
                   (assert-equal 1 (length (cadr result)))))
            
            (define-test tokenize-arrow
              (let ([result (tokenize-sig "Int → Bool")])
                   (assert-true (eq? (car result) 'ok))
                   (assert-equal 3 (length (cadr result)))))
            
            (define-test tokenize-product
              (let ([result (tokenize-sig "Int × String")])
                   (assert-true (eq? (car result) 'ok))
                   (assert-equal 3 (length (cadr result)))))
            
            (define-test tokenize-paren
              (let ([result (tokenize-sig "(List Int)")])
                   (assert-true (eq? (car result) 'ok))
                   (assert-equal 4 (length (cadr result)))))
            
            (define-test tokenize-greek
              (let ([result (tokenize-sig "α → β")])
                   (assert-true (eq? (car result) 'ok))
                   (let ([tokens (cadr result)])
                        (assert-equal 'α (token-value (car tokens))))))
            
            (define-test tokenize-complex
              (let ([result (tokenize-sig "(List α) × α → (List α)")])
                   (assert-true (eq? (car result) 'ok))))
            )

;;; ============================================================
;;; Parser Tests - Simple Types
;;; ============================================================

(test-group parse-simple
            
            (define-test parse-base-type
              (let* ([tokens (cadr (tokenize-sig "Int"))]
                     [result (parse-type tokens)])
                    (assert-true (eq? (car result) 'ok))
                    (assert-equal 'Int (cadr result))))
            
            (define-test parse-type-var
              (let* ([tokens (cadr (tokenize-sig "α"))]
                     [result (parse-type tokens)])
                    (assert-true (eq? (car result) 'ok))
                    (assert-equal 'α (cadr result))))
            )

;;; ============================================================
;;; Parser Tests - Function Types
;;; ============================================================

(test-group parse-functions
            
            (define-test parse-simple-arrow
              (let* ([tokens (cadr (tokenize-sig "Int → Bool"))]
                     [result (parse-type tokens)])
                    (assert-true (eq? (car result) 'ok))
                    (assert-equal '(-> Int Bool) (cadr result))))
            
            (define-test parse-multi-arrow
              (let* ([tokens (cadr (tokenize-sig "Int → Bool → String"))]
                     [result (parse-type tokens)])
                    (assert-true (eq? (car result) 'ok))
                    ;; Right-associative: Int → (Bool → String)
                    (assert-equal '(-> Int (-> Bool String)) (cadr result))))
            )

;;; ============================================================
;;; Parser Tests - Product Types
;;; ============================================================

(test-group parse-products
            
            (define-test parse-simple-product
              (let* ([tokens (cadr (tokenize-sig "Int × Bool"))]
                     [result (parse-type tokens)])
                    (assert-true (eq? (car result) 'ok))
                    (assert-equal '(× Int Bool) (cadr result))))
            
            (define-test parse-multi-product
              (let* ([tokens (cadr (tokenize-sig "Int × Bool × String"))]
                     [result (parse-type tokens)])
                    (assert-true (eq? (car result) 'ok))
                    (assert-equal '(× Int Bool String) (cadr result))))
            )

;;; ============================================================
;;; Parser Tests - Type Applications
;;; ============================================================

(test-group parse-applications
            
            (define-test parse-list-type
              (let* ([tokens (cadr (tokenize-sig "(List Int)"))]
                     [result (parse-type tokens)])
                    (assert-true (eq? (car result) 'ok))
                    (assert-equal '(List Int) (cadr result))))
            
            (define-test parse-pair-type
              (let* ([tokens (cadr (tokenize-sig "(Pair Int Bool)"))]
                     [result (parse-type tokens)])
                    (assert-true (eq? (car result) 'ok))
                    (assert-equal '(Pair Int Bool) (cadr result))))
            
            (define-test parse-maybe-type
              (let* ([tokens (cadr (tokenize-sig "(Maybe Int)"))]
                     [result (parse-type tokens)])
                    (assert-true (eq? (car result) 'ok))
                    (assert-equal '(Maybe Int) (cadr result))))
            )

;;; ============================================================
;;; Parser Tests - Union Types
;;; ============================================================

(test-group parse-unions
            
            (define-test parse-simple-union
              (let* ([tokens (cadr (tokenize-sig "Int | #f"))]
                     [result (parse-type tokens)])
                    (assert-true (eq? (car result) 'ok))
                    (assert-equal '(+ Int #f) (cadr result))))
            )

;;; ============================================================
;;; Parser Tests - Complex Types
;;; ============================================================

(test-group parse-complex
            
            (define-test parse-function-with-list
              (let* ([tokens (cadr (tokenize-sig "(List α) → Int"))]
                     [result (parse-type tokens)])
                    (assert-true (eq? (car result) 'ok))
                    (assert-equal '(-> (List α) Int) (cadr result))))
            
            (define-test parse-product-to-arrow
              (let* ([tokens (cadr (tokenize-sig "Int × Bool → String"))]
                     [result (parse-type tokens)])
                    (assert-true (eq? (car result) 'ok))
                    ;; Product binds tighter than arrow
                    (assert-equal '(-> (× Int Bool) String) (cadr result))))
            
            (define-test parse-grouped-arrow
              (let* ([tokens (cadr (tokenize-sig "(Int → Bool) → String"))]
                     [result (parse-type tokens)])
                    (assert-true (eq? (car result) 'ok))
                    (assert-equal '(-> (-> Int Bool) String) (cadr result))))
            )

;;; ============================================================
;;; Signature Line Tests
;;; ============================================================

(test-group parse-sig-lines
            
            (define-test parse-simple-sig-line
              (let ([result (parse-sig-line ";;; foo : Int → Bool")])
                   (assert-true (eq? (car result) 'ok))
                   (assert-equal 'foo (caadr result))
                   (assert-equal '(-> Int Bool) (cdadr result))))
            
            (define-test skip-dependencies
              (let ([result (parse-sig-line ";;; Dependencies: prelude.ss")])
                   (assert-equal 'skip (car result))))
            
            (define-test skip-non-signature
              (let ([result (parse-sig-line ";;; This is a comment")])
                   (assert-equal 'skip (car result))))
            
            (define-test parse-with-greek
              (let ([result (parse-sig-line ";;; map : (α → β) × (List α) → (List β)")])
                   (assert-true (eq? (car result) 'ok))
                   (assert-equal 'map (caadr result))))
            
            (define-test parse-with-product
              (let ([result (parse-sig-line ";;; foldl : (β × α → β) × β × (List α) → β")])
                   (assert-true (eq? (car result) 'ok))
                   (assert-equal 'foldl (caadr result))))
            )

;;; ============================================================
;;; Summary
;;; ============================================================

(newline)
(display "================================================================================")
(newline)
(display "                    SIG PARSER TESTS COMPLETE")
(newline)
(display "================================================================================")
(newline)
(exit-with-summary)
