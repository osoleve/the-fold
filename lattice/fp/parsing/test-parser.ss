;;; fabric/stitches/fp/test-parser.ss — Tests for Parser Combinators

;;; NOTE: Run from fabric/stitches directory

(load "core/test-framework.ss")
(load "lattice/fp/parsing/parser.ss")

(display "
")
(display "==============================================================
")
(display "         PARSER COMBINATORS TESTS
")
(display "==============================================================
")

;;; Define character constants to avoid formatter issues
(define %char-a (integer->char 97))   ; a
(define %char-b (integer->char 98))   ; b
(define %char-c (integer->char 99))   ; c
(define %char-h (integer->char 104))  ; h
(define %char-x (integer->char 120))  ; x
(define %char-0 (integer->char 48))   ; 0
(define %char-1 (integer->char 49))   ; 1
(define %char-5 (integer->char 53))   ; 5
(define %char-A (integer->char 65))   ; A
(define %newline (integer->char 10))
(define %space (integer->char 32))

;;; ============================================================
;;; Test Helpers
;;; ============================================================

(define (assert-parses parser input expected)
  (let ([result (parse parser input)])
       (cond
        [(left? result)
         (error 'assert-parses
                (string-append "Expected success but got error"))]
        [(not (equal? expected (from-right result)))
         (error 'assert-parses
                (format "Expected ~s but got ~s" expected (from-right result)))]
        [else #t])))

(define (assert-fails parser input)
  (let ([result (parse parser input)])
       (if (right? result)
           (error 'assert-fails
                  (format "Expected failure but got success: ~s" (from-right result)))
           #t)))

;;; ============================================================
;;; Position Tests
;;; ============================================================

(test-group position-tests
            (define-test make-pos-test
              (let ([p (make-pos 1 5 10)])
                   (assert-equal 1 (pos-line p))
                   (assert-equal 5 (pos-col p))
                   (assert-equal 10 (pos-offset p))))
            
            (define-test advance-pos-char-test
              (let* ([p1 (make-pos 1 1 0)]
                     [p2 (advance-pos p1 %char-a)])
                    (assert-equal 1 (pos-line p2))
                    (assert-equal 2 (pos-col p2))
                    (assert-equal 1 (pos-offset p2))))
            
            (define-test advance-pos-newline-test
              (let* ([p1 (make-pos 1 5 10)]
                     [p2 (advance-pos p1 %newline)])
                    (assert-equal 2 (pos-line p2))
                    (assert-equal 1 (pos-col p2))
                    (assert-equal 11 (pos-offset p2)))))

;;; ============================================================
;;; State Tests
;;; ============================================================

(test-group state-tests
            (define-test make-state-test
              ;; make-state now takes (input, index, pos)
              (let ([s (make-state "abc" 0 (make-pos 1 1 0))])
                   (assert-equal "abc" (state-input s))
                   (assert-equal 0 (state-index s))))
            
            (define-test initial-state-test
              (let ([s (initial-state "hello")])
                   (assert-equal "hello" (state-input s))
                   (assert-equal 0 (state-index s))
                   (assert-equal 1 (pos-line (state-pos s)))
                   (assert-equal 1 (pos-col (state-pos s))))))

;;; ============================================================
;;; Error Tests
;;; ============================================================

(test-group error-tests
            (define-test make-error-test
              (let ([e (make-parse-error (make-pos 1 1 0) "msg" '("expected x"))])
                   (assert-true (parse-error? e))
                   (assert-equal 1 (length (error-expected e)))))
            
            (define-test merge-errors-test
              (let* ([e1 (make-parse-error (make-pos 1 1 0) "msg1" '("expected a"))]
                     [e2 (make-parse-error (make-pos 1 2 1) "msg2" '("expected b"))]
                     [merged (merge-errors e1 e2)])
                    ;; Later position should win
                    (assert-equal 2 (pos-col (error-pos merged)))))
            
            (define-test merge-errors-same-pos-test
              (let* ([e1 (make-parse-error (make-pos 1 1 0) "msg1" '("expected a"))]
                     [e2 (make-parse-error (make-pos 1 1 0) "msg2" '("expected b"))]
                     [merged (merge-errors e1 e2)])
                    ;; Same position, expectations should be combined
                    (assert-equal 2 (length (error-expected merged))))))

;;; ============================================================
;;; Basic Parsers Tests
;;; ============================================================

(test-group basic-parsers
            (define-test pure-parser-test
              (assert-parses (parser-pure 42) "anything" 42)
              (assert-parses (parser-pure "hello") "" "hello"))
            
            (define-test fail-parser-test
              (assert-fails (parser-fail "expected nothing") "input"))
            
            (define-test any-char-test
              (assert-parses any-char "hello" %char-h)
              (assert-parses any-char "x" %char-x))
            
            (define-test any-char-empty-test
              (assert-fails any-char ""))
            
            (define-test satisfy-test
              (assert-parses (satisfy char-numeric? "digit") "5" %char-5)
              (assert-fails (satisfy char-numeric? "digit") "a"))
            
            (define-test char-test
              (assert-parses (char %char-x) "xyz" %char-x)
              (assert-fails (char %char-x) "abc"))
            
            (define-test string-parser-test
              (assert-parses (string-parser "hello") "hello world" "hello")
              (assert-fails (string-parser "hello") "hel")))

;;; ============================================================
;;; Character Class Tests
;;; ============================================================

(test-group character-classes
            (define-test digit-test
              (assert-parses digit "5abc" %char-5)
              (assert-parses digit "0" %char-0)
              (assert-fails digit "abc"))
            
            (define-test letter-test
              (assert-parses letter "abc" %char-a)
              (assert-fails letter "123"))
            
            (define-test alpha-num-test
              (assert-parses alpha-num "a1" %char-a)
              (assert-parses alpha-num "1a" %char-1)
              (assert-fails alpha-num " x")))

;;; ============================================================
;;; Combinator Tests
;;; ============================================================

(test-group combinators
            (define-test parser-map-test
              (let ([p (parser-map char-upcase letter)])
                   (assert-parses p "abc" %char-A)))
            
            (define-test parser-bind-test
              (let ([p (parser-bind letter
                                    (lambda (c)
                                            (parser-pure (char-upcase c))))])
                   (assert-parses p "abc" %char-A)))
            
            (define-test parser-then-test
              (let ([p (parser-then (char %char-a) (char %char-b))])
                   (assert-parses p "abc" %char-b)))
            
            (define-test parser-left-test
              (let ([p (parser-left (char %char-a) (char %char-b))])
                   (assert-parses p "abc" %char-a)))
            
            (define-test parser-or-a-test
              (let ([p (parser-or (char %char-a) (char %char-b))])
                   (assert-parses p "abc" %char-a)))
            
            (define-test parser-or-b-test
              (let ([p (parser-or (char %char-a) (char %char-b))])
                   (assert-parses p "bcd" %char-b)))
            
            (define-test parser-or-fail-test
              (let ([p (parser-or (char %char-a) (char %char-b))])
                   (assert-fails p "cde")))
            
            (define-test choice-test
              (let ([p (choice (list (char %char-a) (char %char-b) (char %char-c)))])
                   (assert-parses p "abc" %char-a)
                   (assert-parses p "bcd" %char-b)
                   (assert-parses p "cde" %char-c))))

;;; ============================================================
;;; Repetition Tests
;;; ============================================================

(test-group repetition
            (define-test many-test
              (assert-parses (many digit) "123abc" (list %char-1 (integer->char 50) (integer->char 51)))
              (assert-parses (many digit) "abc" '()))
            
            (define-test some-test
              (assert-parses (some digit) "123" (list %char-1 (integer->char 50) (integer->char 51)))
              (assert-fails (some digit) "abc"))
            
            (define-test count-test
              (assert-parses (count 3 letter) "abcdef"
                             (list %char-a %char-b %char-c))
              (assert-fails (count 3 letter) "ab"))
            
            (define-test optional-test
              (assert-parses (optional digit %char-0) "5" %char-5)
              (assert-parses (optional digit %char-0) "a" %char-0)))

;;; ============================================================
;;; Sequence Tests
;;; ============================================================

(test-group sequences
            (define-test between-test
              (let ([p (between (char (integer->char 40))  ; '('
                                (char (integer->char 41))  ; ')'
                                letter)])
                   (assert-parses p "(a)" %char-a)))
            
            (define-test sep-by-test
              (let ([p (sep-by digit (char (integer->char 44)))])  ; ','
                   (assert-parses p "1,2,3" (list %char-1 (integer->char 50) (integer->char 51)))
                   (assert-parses p "" '()))))

;;; ============================================================
;;; Token Tests
;;; ============================================================

(test-group tokens
            (define-test lexeme-test
              (let ([p (lexeme (string-parser "hello"))])
                   (assert-parses p "hello   world" "hello")))
            
            (define-test integer-test
              (assert-parses integer "123abc" 123)
              (assert-parses integer "-42xyz" -42)))

;;; ============================================================
;;; Packrat/Memoization Tests
;;; ============================================================

(test-group packrat
            ;; Test basic memoization
            (define-test memo-basic-test
              (let* ([p (char %char-a)]
                     [mp (memo 'test-a p)]
                     [table (make-memo-table)]
                     [result (parse-with-memo mp "abc" table)])
                    (assert-true (right? result))
                    (assert-equal %char-a (from-right result))))
            
            ;; Test cache hit (same position returns same result)
            (define-test memo-cache-hit-test
              (let* ([call-count 0]
                     ;; Parser that counts how many times it's called
                     [p (make-parser
                         (lambda (state)
                                 (set! call-count (+ call-count 1))
                                 (run-parser (char %char-a) state)))]
                     [mp (memo 'counting p)]
                     [table (make-memo-table)])
                    ;; Parse once
                    (parse-with-memo mp "abc" table)
                    (assert-equal 1 call-count)
                    ;; Parse again with same table at same position
                    (parse-with-memo mp "abc" table)
                    ;; Should still be 1 due to memoization
                    (assert-equal 1 call-count)))
            
            ;; Test different positions get different entries
            (define-test memo-different-positions-test
              (let* ([table (make-memo-table)]
                     ;; Create a sequence that memoizes at different positions
                     [mp-a (memo 'get-a (char %char-a))]
                     [mp-b (memo 'get-b (char %char-b))]
                     [mp-seq (memo-then mp-a mp-b)])
                    (let ([result (parse-with-memo mp-seq "ab" table)])
                         (assert-true (right? result))
                         (assert-equal %char-b (from-right result)))
                    ;; Should have at least 2 entries
                    (assert-true (>= (cdr (memo-stats table)) 2))))
            
            ;; Test memo-bind
            (define-test memo-bind-test
              (let* ([mp-digit (memo 'digit digit)]
                     ;; digit returns a char, convert to number then double
                     [mp-doubled (memo-bind mp-digit
                                            (lambda (d)
                                                    (memo-pure (* (- (char->integer d) 48) 2))))]
                     [result (parse-packrat mp-doubled "5abc")])
                    (assert-true (right? result))
                    (assert-equal 10 (from-right result))))
            
            ;; Test memo-or
            (define-test memo-or-test
              (let* ([mp-a (memo 'alt-a (char %char-a))]
                     [mp-b (memo 'alt-b (char %char-b))]
                     [mp-either (memo-or mp-a mp-b)])
                    (assert-true (right? (parse-packrat mp-either "abc")))
                    (assert-true (right? (parse-packrat mp-either "bcd")))))
            
            ;; Test memo-many
            (define-test memo-many-test
              (let* ([mp-a (memo 'many-a (char %char-a))]
                     [mp-as (memo-many mp-a)]
                     [result (parse-packrat mp-as "aaabbb")])
                    (assert-true (right? result))
                    (assert-equal (list %char-a %char-a %char-a) (from-right result))))
            
            ;; Test memo-some
            (define-test memo-some-test
              (let* ([mp-b (memo 'some-b (char %char-b))]
                     [mp-bs (memo-some mp-b)]
                     [result (parse-packrat mp-bs "bbbccc")])
                    (assert-true (right? result))
                    (assert-equal (list %char-b %char-b %char-b) (from-right result))))
            
            ;; Test lift-parser
            (define-test lift-parser-test
              (let* ([p (string-parser "hello")]
                     [mp (lift-parser p)]
                     [result (parse-packrat mp "hello world")])
                    (assert-true (right? result))
                    (assert-equal "hello" (from-right result))))
            
            ;; Test memo-map
            (define-test memo-map-test
              (let* ([mp-digit (memo 'map-digit digit)]
                     ;; digit returns a char, use string to convert to string
                     [mp-str (memo-map string mp-digit)]
                     [result (parse-packrat mp-str "7xyz")])
                    (assert-true (right? result))
                    (assert-equal "7" (from-right result))))
            
            ;; Test parse-packrat convenience function
            (define-test parse-packrat-test
              (let* ([mp (memo 'simple (string-parser "test"))]
                     [result (parse-packrat mp "testing")])
                    (assert-true (right? result))
                    (assert-equal "test" (from-right result)))))

;;; ============================================================
;;; Performance Tests
;;; ============================================================
;;;
;;; These tests verify that the O(N) optimization works correctly.
;;; Before the fix, parsing used substring copying which was O(N) per
;;; character consumed, leading to O(N^2) total time.
;;; After the fix, we use index-based access which is O(1) per character.

(test-group performance-tests
            ;; Test parsing a large input with many
            ;; With old O(N^2) approach, this would be very slow for 100k chars
            ;; With new O(N) approach, this should complete quickly
            (define-test large-input-many-test
              (let* ([n 100000]
                     [large-input (make-string n (integer->char 97))]  ; "aaa...a" (100k 'a's)
                     [p (many (char (integer->char 97)))]  ; parser for many 'a's
                     [result (parse p large-input)])
                    (assert-true (right? result))
                    (assert-equal n (length (from-right result)))))
            
            ;; Test parsing with repeated string matching
            (define-test large-input-string-parser-test
              (let* ([n 10000]
                     ;; Create "abcabc...abc" (10k repetitions)
                     [large-input (apply string-append
                                         (map (lambda (_) "abc")
                                              (iota n)))]
                     [p (many (string-parser "abc"))]
                     [result (parse p large-input)])
                    (assert-true (right? result))
                    (assert-equal n (length (from-right result)))))
            
            ;; Test that index-based access works correctly at various positions
            (define-test index-consistency-test
              (let* ([input "Hello, World! This is a test."]
                     [p (many any-char)]
                     [result (parse p input)])
                    (assert-true (right? result))
                    (assert-equal (string-length input)
                                  (length (from-right result)))
                    ;; Verify that all characters were correctly extracted
                    (assert-equal input
                                  (list->string (from-right result))))))

;;; ============================================================
;;; Bounded Memoization Tests (DoS Prevention)
;;; ============================================================

(test-group "bounded-memo-tests"
            ;; Test that bounded memo table enforces its limit
            (define-test bounded-memo-limit-test
              (let* ([limit 100]
                     [table (make-bounded-memo-table limit)])
                    ;; Fill the table beyond its limit
                    (do ([i 0 (+ i 1)])
                        ((= i (* limit 2)))  ; Add 2x limit entries
                        (memo-store! table 'test-rule i (right (cons i '()))))
                    ;; Verify table size is bounded (should be at most limit)
                    (let ([size (memo-table-size table)])
                         (assert-true (<= size limit)))))
            
            ;; Test that bounded table still allows cache hits
            (define-test bounded-memo-hits-test
              (let* ([table (make-bounded-memo-table 1000)]
                     [mp (memo 'test (char (integer->char 97)))])
                    ;; First parse
                    (let ([r1 (parse-with-memo mp "abc" table)])
                         (assert-true (right? r1))
                         ;; Second parse should use cached result
                         (let ([r2 (parse-with-memo mp "abc" table)])
                              (assert-true (right? r2))
                              (assert-equal (from-right r1) (from-right r2))))))
            
            ;; Test that default make-memo-table is bounded
            (define-test default-memo-bounded-test
              (let ([table (make-memo-table)])
                   ;; Default table should be bounded
                   (assert-true (bounded-memo-table? table))
                   ;; Should have a reasonable limit
                   (let ([stats (memo-stats table)])
                        (assert-true (number? (cdr stats)))
                        (assert-true (> (cdr stats) 0)))))
            
            ;; Test unbounded table for comparison
            (define-test unbounded-memo-test
              (let ([table (make-unbounded-memo-table)])
                   (assert-true (unbounded-memo-table? table))
                   (let ([stats (memo-stats table)])
                        (assert-equal #f (cdr stats)))))
            
            ;; Test memo-table-size function
            (define-test memo-table-size-test
              (let ([table (make-bounded-memo-table 1000)])
                   (assert-equal 0 (memo-table-size table))
                   (memo-store! table 'rule1 0 (right (cons 'a '())))
                   (assert-equal 1 (memo-table-size table))
                   (memo-store! table 'rule1 1 (right (cons 'b '())))
                   (assert-equal 2 (memo-table-size table))))
            
            ;; Test that eviction prefers older entries (LRU behavior)
            (define-test bounded-memo-lru-test
              (let* ([limit 10]
                     [table (make-bounded-memo-table limit)])
                    ;; Add entries 0-9
                    (do ([i 0 (+ i 1)])
                        ((= i limit))
                        (memo-store! table 'test i (right (cons i '()))))
                    ;; Access entry 0 to refresh its timestamp
                    (memo-lookup table 'test 0)
                    ;; Add more entries to trigger eviction
                    (do ([i limit (+ i 1)])
                        ((= i (+ limit 5)))
                        (memo-store! table 'test i (right (cons i '()))))
                    ;; Entry 0 should still be present (was refreshed)
                    (let ([lookup-0 (memo-lookup table 'test 0)])
                         (assert-true (just? lookup-0))))))

;;; ============================================================
;;; Summary
;;; ============================================================

(display "
")
(display "==============================================================
")
(printf "Tests passed: ~a
" *tests-passed*)
(printf "Tests failed: ~a
" *tests-failed*)
(printf "Total tests:  ~a
" *tests-run*)

(if (= *tests-failed* 0)
    (display "
[SUCCESS] All parser combinator tests passed.
")
    (display "
[FAILURE] Some parser combinator tests failed.
"))
