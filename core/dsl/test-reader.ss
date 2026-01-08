;;; core/dsl/test-reader.ss — Tests for Reader Extensions
;;;
;;; Tests for readtables, reader macros, and custom notation.

(load "core/test-framework.ss")
(load "core/dsl/reader.ss")

;;; ============================================================
;;; Readtable Tests
;;; ============================================================

(test-group readtable-basics
            
            (define-test make-readtable-creates-empty
              (let ([rt (make-readtable)])
                   (assert-true (readtable? rt))
                   (assert-equal (readtable-entries rt) '())))
            
            (define-test readtable-extend-adds-entry
              (let* ([rt (make-readtable)]
                     [rt2 (readtable-extend rt #\v vector-reader-macro)])
                    (assert-true (just? (readtable-lookup rt2 #\v)))
                    (assert-equal (reader-macro-name (from-just (readtable-lookup rt2 #\v))) 'vector)))
            
            (define-test readtable-extend-replaces-existing
              (let* ([rt (make-readtable)]
                     [rt2 (readtable-extend rt #\v vector-reader-macro)]
                     [rt3 (readtable-extend rt2 #\v matrix-reader-macro)])
                    (assert-equal (reader-macro-name (from-just (readtable-lookup rt3 #\v))) 'matrix)))
            
            (define-test readtable-lookup-returns-nothing-for-missing
              (let ([rt (make-readtable)])
                   (assert-true (nothing? (readtable-lookup rt #\v)))))
            
            (define-test readtable-remove-removes-entry
              (let* ([rt (make-readtable)]
                     [rt2 (readtable-extend rt #\v vector-reader-macro)]
                     [rt3 (readtable-remove rt2 #\v)])
                    (assert-true (nothing? (readtable-lookup rt3 #\v))))))

;;; ============================================================
;;; Readtable Stack Tests
;;; ============================================================

(test-group readtable-stack
            
            (define-test make-stack-creates-singleton
              (let ([stack (make-readtable-stack)])
                   (assert-true (readtable-stack? stack))
                   (assert-true (readtable? (readtable-stack-current stack)))))
            
            (define-test stack-push-adds-readtable
              (let* ([stack (make-readtable-stack)]
                     [rt (readtable-extend (make-readtable) #\v vector-reader-macro)]
                     [stack2 (readtable-stack-push stack rt)])
                    (assert-true (just? (readtable-stack-lookup stack2 #\v)))))
            
            (define-test stack-pop-removes-top
              (let* ([stack (make-readtable-stack)]
                     [rt (readtable-extend (make-readtable) #\v vector-reader-macro)]
                     [stack2 (readtable-stack-push stack rt)]
                     [result (readtable-stack-pop stack2)])
                    (assert-true (just? result))
                    (let ([stack3 (from-just result)])
                         (assert-true (nothing? (readtable-stack-lookup stack3 #\v))))))
            
            (define-test cannot-pop-base-readtable
              (let* ([stack (make-readtable-stack)]
                     [result (readtable-stack-pop stack)])
                    (assert-true (nothing? result))))
            
            (define-test stack-lookup-searches-top-to-bottom
              (let* ([rt1 (readtable-extend (make-readtable) #\a vector-reader-macro)]
                     [rt2 (readtable-extend (make-readtable) #\a matrix-reader-macro)]
                     [stack (readtable-stack-push
                             (readtable-stack-push (make-readtable-stack) rt1)
                             rt2)])
                    ;; rt2 is on top, so we should get matrix
                    (assert-equal (reader-macro-name (from-just (readtable-stack-lookup stack #\a)))
                                  'matrix))))

;;; ============================================================
;;; Reader Macro Tests
;;; ============================================================

(test-group reader-macro-structure
            
            (define-test make-reader-macro-creates-valid-structure
              (let ([rm (make-reader-macro 'test (lambda (c l) c) #\[)])
                   (assert-true (reader-macro? rm))
                   (assert-equal (reader-macro-name rm) 'test)
                   (assert-equal (reader-macro-delimiter rm) #\[)))
            
            (define-test reader-macro-transform-is-callable
              (let* ([rm (make-reader-macro 'double
                                            (lambda (contents loc)
                                                    `(list ,@contents ,@contents))
                                            nothing)]
                     [transform (reader-macro-transform rm)])
                    (assert-equal (transform '(1 2) no-reader-source) '(list 1 2 1 2)))))

;;; ============================================================
;;; Standard Reader Macro Tests
;;; ============================================================

(test-group vector-reader-macro-tests
            
            (define-test vector-transforms-list-to-vector-call
              (let* ([transform (reader-macro-transform vector-reader-macro)]
                     [result (transform '(1 2 3) no-reader-source)])
                    (assert-equal result '(vector 1 2 3))))
            
            (define-test vector-handles-empty
              (let* ([transform (reader-macro-transform vector-reader-macro)]
                     [result (transform '() no-reader-source)])
                    (assert-equal result '(vector))))
            
            (define-test vector-handles-nested-expressions
              (let* ([transform (reader-macro-transform vector-reader-macro)]
                     [result (transform '((+ 1 2) x) no-reader-source)])
                    (assert-equal result '(vector (+ 1 2) x)))))

(test-group matrix-reader-macro-tests
            
            (define-test matrix-transforms-nested-lists
              (let* ([transform (reader-macro-transform matrix-reader-macro)]
                     [result (transform '((1 2) (3 4)) no-reader-source)])
                    (assert-equal result '(matrix '((1 2) (3 4))))))
            
            (define-test matrix-handles-1x1
              (let* ([transform (reader-macro-transform matrix-reader-macro)]
                     [result (transform '((42)) no-reader-source)])
                    (assert-equal result '(matrix '((42)))))))

(test-group regex-reader-macro-tests
            
            (define-test regex-transforms-pattern-with-flags
              (let* ([transform (reader-macro-transform regex-reader-macro)]
                     [result (transform '("abc" (i g)) no-reader-source)])
                    (assert-equal result '(regex "abc" '(i g)))))
            
            (define-test regex-handles-pattern-without-flags
              (let* ([transform (reader-macro-transform regex-reader-macro)]
                     [result (transform '("abc") no-reader-source)])
                    (assert-equal result '(regex "abc" '()))))
            
            (define-test regex-handles-empty-contents
              (let* ([transform (reader-macro-transform regex-reader-macro)]
                     [result (transform '() no-reader-source)])
                    (assert-equal result '(regex "" '())))))

(test-group set-reader-macro-tests
            
            (define-test set-transforms-to-set-call
              (let* ([transform (reader-macro-transform set-reader-macro)]
                     [result (transform '(1 2 3) no-reader-source)])
                    (assert-equal result '(set 1 2 3)))))

(test-group hash-reader-macro-tests
            
            (define-test hash-transforms-pairs
              (let* ([transform (reader-macro-transform hash-reader-macro)]
                     [result (transform '(a 1 b 2) no-reader-source)])
                    (assert-equal result '(hash 'a 1 'b 2))))
            
            (define-test hash-handles-empty
              (let* ([transform (reader-macro-transform hash-reader-macro)]
                     [result (transform '() no-reader-source)])
                    (assert-equal result '(hash))))
            
            (define-test hash-rejects-odd-length
              ;; Odd-length input (missing value for last key) should error
              (let ([transform (reader-macro-transform hash-reader-macro)])
                   (assert-error (lambda ()
                                         (transform '(a 1 b) no-reader-source))))))

;;; ============================================================
;;; Default Readtable Tests
;;; ============================================================

(test-group default-readtable-tests
            
            (define-test has-vector-macro
              (assert-true (just? (readtable-lookup default-readtable #\v))))
            
            (define-test has-matrix-macro
              (assert-true (just? (readtable-lookup default-readtable #\m))))
            
            (define-test has-regex-macro
              (assert-true (just? (readtable-lookup default-readtable #\r))))
            
            (define-test has-set-macro
              (assert-true (just? (readtable-lookup default-readtable #\s))))
            
            (define-test has-hash-macro
              (assert-true (just? (readtable-lookup default-readtable #\h)))))

;;; ============================================================
;;; Reader Macro Expansion Tests
;;; ============================================================

(test-group reader-macro-expansion
            
            (define-test expand-reader-macro-applies-transform
              (let ([result (expand-reader-macro #\v '(1 2 3) no-reader-source default-readtable)])
                   (assert-true (just? result))
                   (assert-equal (from-just result) '(vector 1 2 3))))
            
            (define-test expand-reader-macro-returns-nothing-for-unknown
              (let ([result (expand-reader-macro #\z '(1 2 3) no-reader-source default-readtable)])
                   (assert-true (nothing? result))))
            
            (define-test expand-reader-macro-stack-uses-stack
              (let* ([stack (readtable-stack-push (make-readtable-stack) default-readtable)]
                     [result (expand-reader-macro-stack #\v '(a b) no-reader-source stack)])
                    (assert-true (just? result))
                    (assert-equal (from-just result) '(vector a b)))))

;;; ============================================================
;;; Registration Tests
;;; ============================================================

(test-group reader-macro-registration
            
            (define-test register-reader-macro-adds-new
              (let* ([rt (register-reader-macro (make-readtable) #\q 'quote-all
                                                (lambda (contents loc) `(quote ,contents)))]
                     [result (readtable-lookup rt #\q)])
                    (assert-true (just? result))
                    (assert-equal (reader-macro-name (from-just result)) 'quote-all)))
            
            (define-test registered-macro-is-callable
              (let* ([rt (register-reader-macro (make-readtable) #\d 'double
                                                (lambda (contents loc) `(* 2 ,@contents)))]
                     [macro (from-just (readtable-lookup rt #\d))]
                     [transform (reader-macro-transform macro)])
                    (assert-equal (transform '(x) no-reader-source) '(* 2 x)))))

;;; ============================================================
;;; Delimiter Tests
;;; ============================================================

(test-group delimiter-pairs-tests
            
            (define-test delimiter-close-finds-matching
              (assert-equal (from-just (delimiter-close #\[)) #\])
              (assert-equal (from-just (delimiter-close #\{)) #\})
              (assert-equal (from-just (delimiter-close #\<)) #\>)
              (assert-equal (from-just (delimiter-close #\()) #\)))
            
            (define-test delimiter-close-returns-nothing-for-unknown
              (assert-true (nothing? (delimiter-close #\a))))
            
            (define-test delimiter-open-finds-matching
              (assert-equal (from-just (delimiter-open #\])) #\[)
              (assert-equal (from-just (delimiter-open #\})) #\{)))

;;; ============================================================
;;; Dispatch Character Tests
;;; ============================================================

(test-group dispatch-characters
            
            (define-test alphanumeric-is-valid-dispatch
              (assert-true (dispatch-char? #\a))
              (assert-true (dispatch-char? #\V))
              (assert-true (dispatch-char? #\0)))
            
            (define-test whitespace-is-not-valid-dispatch
              (assert-false (dispatch-char? #\space))
              (assert-false (dispatch-char? #\newline)))
            
            (define-test parens-are-not-valid-dispatch
              (assert-false (dispatch-char? #\())
              (assert-false (dispatch-char? #\)))))

;;; ============================================================
;;; Reader State Tests
;;; ============================================================

(test-group reader-state-tests
            
            (define-test make-reader-state-creates-valid
              (let ([rs (make-reader-state "test.ss" 1 0 (make-readtable-stack))])
                   (assert-true (reader-state? rs))
                   (assert-equal (reader-state-filename rs) "test.ss")
                   (assert-equal (reader-state-line rs) 1)
                   (assert-equal (reader-state-column rs) 0)))
            
            (define-test reader-state-update-position-updates-correctly
              (let* ([rs (make-reader-state "test.ss" 1 0 (make-readtable-stack))]
                     [rs2 (reader-state-update-position rs 5 10)])
                    (assert-equal (reader-state-line rs2) 5)
                    (assert-equal (reader-state-column rs2) 10)))
            
            (define-test reader-state-source-loc-creates-location
              (let* ([rs (make-reader-state "test.ss" 42 7 (make-readtable-stack))]
                     [loc (reader-state-source-loc rs)])
                    (assert-equal (reader-source-loc-file loc) "test.ss")
                    (assert-equal (reader-source-loc-line loc) 42)
                    (assert-equal (reader-source-loc-column loc) 7))))

;;; ============================================================
;;; Escape Sequence Tests
;;; ============================================================

(test-group escape-sequences
            
            (define-test unescape-handles-newline
              (assert-equal (unescape-string "a\\nb") "a\nb"))
            
            (define-test unescape-handles-tab
              (assert-equal (unescape-string "a\\tb") "a\tb"))
            
            (define-test unescape-handles-escaped-backslash
              (assert-equal (unescape-string "a\\\\b") "a\\b"))
            
            (define-test unescape-handles-no-escapes
              (assert-equal (unescape-string "abc") "abc"))
            
            (define-test unescape-handles-escaped-slash
              (assert-equal (unescape-string "a\\/b") "a/b"))
            
            (define-test unescape-handles-escaped-double-quote
              (assert-equal (unescape-string "a\\\"b") "a\"b"))
            
            (define-test unescape-handles-escaped-single-quote
              (assert-equal (unescape-string "a\\'b") "a'b")))

;;; ============================================================
;;; Composition Tests
;;; ============================================================

(test-group readtable-composition
            
            (define-test compose-readtables-merges-entries
              (let* ([rt1 (readtable-extend (make-readtable) #\a vector-reader-macro)]
                     [rt2 (readtable-extend (make-readtable) #\b matrix-reader-macro)]
                     [composed (compose-readtables (list rt1 rt2))])
                    (assert-true (just? (readtable-lookup composed #\a)))
                    (assert-true (just? (readtable-lookup composed #\b)))))
            
            (define-test later-entries-override-earlier
              (let* ([rt1 (readtable-extend (make-readtable) #\a vector-reader-macro)]
                     [rt2 (readtable-extend (make-readtable) #\a matrix-reader-macro)]
                     [composed (compose-readtables (list rt1 rt2))])
                    (assert-equal (reader-macro-name (from-just (readtable-lookup composed #\a)))
                                  'matrix))))

;;; ============================================================
;;; Reader Token Tests
;;; ============================================================

(test-group reader-tokens
            
            (define-test make-reader-token-creates-valid
              (let ([tok (make-reader-token 'number 42 no-reader-source)])
                   (assert-true (reader-token? tok))
                   (assert-equal (reader-token-type tok) 'number)
                   (assert-equal (reader-token-value tok) 42)))
            
            (define-test reader-token-source-retrieves-location
              (let* ([loc (make-reader-source-loc "test.ss" 10 5)]
                     [tok (make-reader-token 'symbol 'foo loc)])
                    (assert-equal (reader-source-loc-line (reader-token-source tok)) 10))))

;;; ============================================================
;;; Reader Error Tests
;;; ============================================================

(test-group reader-errors
            
            (define-test make-reader-error-creates-valid
              (let ([err (make-reader-error "unexpected EOF" no-reader-source)])
                   (assert-true (reader-error? err))
                   (assert-equal (reader-error-message err) "unexpected EOF")))
            
            (define-test reader-error-source-retrieves-location
              (let* ([loc (make-reader-source-loc "bad.ss" 99 0)]
                     [err (make-reader-error "syntax error" loc)])
                    (assert-equal (reader-source-loc-line (reader-error-source err)) 99))))

;;; ============================================================
;;; Unit Annotation Tests
;;; ============================================================

(test-group unit-annotations
            
            (define-test unit-reader-macro-transforms-correctly
              (let* ([transform (reader-macro-transform unit-reader-macro)]
                     [result (transform '(5 "m/s") no-reader-source)])
                    (assert-equal result '(with-unit 5 '|m/s|))))
            
            (define-test unit-reader-macro-handles-invalid-input
              (let* ([transform (reader-macro-transform unit-reader-macro)]
                     [result (transform '(not-valid) no-reader-source)])
                    (assert-equal result '(not-valid)))))

;;; ============================================================
;;; Run All Tests
;;; ============================================================

(display "\n========================================\n")
(display "Running Reader Extension Tests\n")
(display "========================================\n\n")

(run-all-tests)
