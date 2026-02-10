;;; fabric/stitches/fp/test-parser.ss — Tests for Parser Combinators

;;; NOTE: Run from fabric/stitches directory

(load "core/lang/module.ss")
(load "core/test-framework.ss")
(load "lattice/fp/parsing/parser.ss")

(display "
")
(display "====
")
(display "         PARSER COMBINATORS TESTS
")
(display "====
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

;;; ====
;;; Test Helpers
;;; ====

(define (assert-parses parser input expected)
  (let ([result (parser-parse parser input)])
       (cond
        [(left? result)
         (error 'assert-parses
                (string-append "Expected success but got error"))]
        [(not (equal? expected (from-right result)))
         (error 'assert-parses
                (format "Expected ~s but got ~s" expected (from-right result)))]
        [else #t])))

(define (assert-fails parser input)
  (let ([result (parser-parse parser input)])
       (if (right? result)
           (error 'assert-fails
                  (format "Expected failure but got success: ~s" (from-right result)))
           #t)))

;;; ====
;;; Position Tests
;;; ====

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

;;; ====
;;; State Tests
;;; ====

(test-group state-tests
            (define-test make-state-test
              ;; make-state now takes (input, index, pos)
              (let ([s (parser-make-state "abc" 0 (make-pos 1 1 0))])
                   (assert-equal "abc" (parser-state-input s))
                   (assert-equal 0 (parser-state-index s))))
            
            (define-test initial-state-test
              (let ([s (parser-initial-state "hello")])
                   (assert-equal "hello" (parser-state-input s))
                   (assert-equal 0 (parser-state-index s))
                   (assert-equal 1 (pos-line (parser-state-pos s)))
                   (assert-equal 1 (pos-col (parser-state-pos s))))))

;;; ====
;;; Error Tests
;;; ====

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

;;; ====
;;; Basic Parsers Tests
;;; ====

(test-group basic-parsers
            (define-test pure-parser-test
              (assert-parses (parser-pure 42) "anything" 42)
              (assert-parses (parser-pure "hello") "" "hello"))
            
            (define-test fail-parser-test
              (assert-fails (parser-fail "expected nothing") "input"))
            
            (define-test any-char-test
              (assert-parses parser-any-char "hello" %char-h)
              (assert-parses parser-any-char "x" %char-x))
            
            (define-test any-char-empty-test
              (assert-fails parser-any-char ""))
            
            (define-test satisfy-test
              (assert-parses (parser-satisfy char-numeric? "digit") "5" %char-5)
              (assert-fails (parser-satisfy char-numeric? "digit") "a"))
            
            (define-test char-test
              (assert-parses (parser-char %char-x) "xyz" %char-x)
              (assert-fails (parser-char %char-x) "abc"))
            
            (define-test string-parser-test
              (assert-parses (parser-string "hello") "hello world" "hello")
              (assert-fails (parser-string "hello") "hel")))

;;; ====
;;; Character Class Tests
;;; ====

(test-group character-classes
            (define-test digit-test
              (assert-parses parser-digit "5abc" %char-5)
              (assert-parses parser-digit "0" %char-0)
              (assert-fails parser-digit "abc"))
            
            (define-test letter-test
              (assert-parses parser-letter "abc" %char-a)
              (assert-fails parser-letter "123"))
            
            (define-test alpha-num-test
              (assert-parses parser-alpha-num "a1" %char-a)
              (assert-parses parser-alpha-num "1a" %char-1)
              (assert-fails parser-alpha-num " x")))

;;; ====
;;; Combinator Tests
;;; ====

(test-group combinators
            (define-test parser-map-test
              (let ([p (parser-map char-upcase parser-letter)])
                   (assert-parses p "abc" %char-A)))
            
            (define-test parser-bind-test
              (let ([p (parser-bind parser-letter
                                    (lambda (c)
                                            (parser-pure (char-upcase c))))])
                   (assert-parses p "abc" %char-A)))
            
            (define-test parser-then-test
              (let ([p (parser-then (parser-char %char-a) (parser-char %char-b))])
                   (assert-parses p "abc" %char-b)))
            
            (define-test parser-left-test
              (let ([p (parser-left (parser-char %char-a) (parser-char %char-b))])
                   (assert-parses p "abc" %char-a)))
            
            (define-test parser-or-a-test
              (let ([p (parser-or (parser-char %char-a) (parser-char %char-b))])
                   (assert-parses p "abc" %char-a)))
            
            (define-test parser-or-b-test
              (let ([p (parser-or (parser-char %char-a) (parser-char %char-b))])
                   (assert-parses p "bcd" %char-b)))
            
            (define-test parser-or-fail-test
              (let ([p (parser-or (parser-char %char-a) (parser-char %char-b))])
                   (assert-fails p "cde")))
            
            (define-test choice-test
              (let ([p (parser-choice (list (parser-char %char-a) (parser-char %char-b) (parser-char %char-c)))])
                   (assert-parses p "abc" %char-a)
                   (assert-parses p "bcd" %char-b)
                   (assert-parses p "cde" %char-c))))

;;; ====
;;; Repetition Tests
;;; ====

(test-group repetition
            (define-test many-test
              (assert-parses (parser-many parser-digit) "123abc" (list %char-1 (integer->char 50) (integer->char 51)))
              (assert-parses (parser-many parser-digit) "abc" '()))
            
            (define-test some-test
              (assert-parses (parser-some parser-digit) "123" (list %char-1 (integer->char 50) (integer->char 51)))
              (assert-fails (parser-some parser-digit) "abc"))
            
            (define-test count-test
              (assert-parses (parser-count 3 parser-letter) "abcdef"
                             (list %char-a %char-b %char-c))
              (assert-fails (parser-count 3 parser-letter) "ab"))
            
            (define-test optional-test
              (assert-parses (parser-optional parser-digit %char-0) "5" %char-5)
              (assert-parses (parser-optional parser-digit %char-0) "a" %char-0)))

;;; ====
;;; Sequence Tests
;;; ====

(test-group sequences
            (define-test between-test
              (let ([p (parser-between (parser-char (integer->char 40))  ; '('
                                (parser-char (integer->char 41))  ; ')'
                                parser-letter)])
                   (assert-parses p "(a)" %char-a)))
            
            (define-test sep-by-test
              (let ([p (parser-sep-by parser-digit (parser-char (integer->char 44)))])  ; ','
                   (assert-parses p "1,2,3" (list %char-1 (integer->char 50) (integer->char 51)))
                   (assert-parses p "" '()))))

;;; ====
;;; Token Tests
;;; ====

(test-group tokens
            (define-test lexeme-test
              (let ([p (parser-lexeme (parser-string "hello"))])
                   (assert-parses p "hello   world" "hello")))
            
            (define-test integer-test
              (assert-parses parser-integer "123abc" 123)
              (assert-parses parser-integer "-42xyz" -42)))

;;; ====
;;; Packrat/Memoization Tests
;;; ====

(test-group packrat
            ;; Test basic memoization
            (define-test memo-basic-test
              (let* ([p (parser-char %char-a)]
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
                                 (run-parser (parser-char %char-a) state)))]
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
                     [mp-a (memo 'get-a (parser-char %char-a))]
                     [mp-b (memo 'get-b (parser-char %char-b))]
                     [mp-seq (memo-then mp-a mp-b)])
                    (let ([result (parse-with-memo mp-seq "ab" table)])
                         (assert-true (right? result))
                         (assert-equal %char-b (from-right result)))
                    ;; Should have at least 2 entries
                    (assert-true (>= (cdr (memo-stats table)) 2))))
            
            ;; Test memo-bind
            (define-test memo-bind-test
              (let* ([mp-digit (memo 'digit parser-digit)]
                     ;; parser-digit returns a char, convert to number then double
                     [mp-doubled (memo-bind mp-digit
                                            (lambda (d)
                                                    (memo-pure (* (- (char->integer d) 48) 2))))]
                     [result (parse-packrat mp-doubled "5abc")])
                    (assert-true (right? result))
                    (assert-equal 10 (from-right result))))
            
            ;; Test memo-or
            (define-test memo-or-test
              (let* ([mp-a (memo 'alt-a (parser-char %char-a))]
                     [mp-b (memo 'alt-b (parser-char %char-b))]
                     [mp-either (memo-or mp-a mp-b)])
                    (assert-true (right? (parse-packrat mp-either "abc")))
                    (assert-true (right? (parse-packrat mp-either "bcd")))))
            
            ;; Test memo-many
            (define-test memo-many-test
              (let* ([mp-a (memo 'many-a (parser-char %char-a))]
                     [mp-as (memo-many mp-a)]
                     [result (parse-packrat mp-as "aaabbb")])
                    (assert-true (right? result))
                    (assert-equal (list %char-a %char-a %char-a) (from-right result))))
            
            ;; Test memo-some
            (define-test memo-some-test
              (let* ([mp-b (memo 'some-b (parser-char %char-b))]
                     [mp-bs (memo-some mp-b)]
                     [result (parse-packrat mp-bs "bbbccc")])
                    (assert-true (right? result))
                    (assert-equal (list %char-b %char-b %char-b) (from-right result))))
            
            ;; Test lift-parser
            (define-test lift-parser-test
              (let* ([p (parser-string "hello")]
                     [mp (lift-parser p)]
                     [result (parse-packrat mp "hello world")])
                    (assert-true (right? result))
                    (assert-equal "hello" (from-right result))))
            
            ;; Test memo-map
            (define-test memo-map-test
              (let* ([mp-digit (memo 'map-digit parser-digit)]
                     ;; parser-digit returns a char, use string to convert to string
                     [mp-str (memo-map string mp-digit)]
                     [result (parse-packrat mp-str "7xyz")])
                    (assert-true (right? result))
                    (assert-equal "7" (from-right result))))
            
            ;; Test parse-packrat convenience function
            (define-test parse-packrat-test
              (let* ([mp (memo 'simple (parser-string "test"))]
                     [result (parse-packrat mp "testing")])
                    (assert-true (right? result))
                    (assert-equal "test" (from-right result)))))

;;; ====
;;; Performance Tests
;;; ====
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
                     [p (parser-many (parser-char (integer->char 97)))]  ; parser for many 'a's
                     [result (parser-parse p large-input)])
                    (assert-true (right? result))
                    (assert-equal n (length (from-right result)))))
            
            ;; Test parsing with repeated string matching
            (define-test large-input-string-parser-test
              (let* ([n 10000]
                     ;; Create "abcabc...abc" (10k repetitions)
                     [large-input (apply string-append
                                         (map (lambda (_) "abc")
                                              (iota n)))]
                     [p (parser-many (parser-string "abc"))]
                     [result (parser-parse p large-input)])
                    (assert-true (right? result))
                    (assert-equal n (length (from-right result)))))
            
            ;; Test that index-based access works correctly at various positions
            (define-test index-consistency-test
              (let* ([input "Hello, World! This is a test."]
                     [p (parser-many parser-any-char)]
                     [result (parser-parse p input)])
                    (assert-true (right? result))
                    (assert-equal (string-length input)
                                  (length (from-right result)))
                    ;; Verify that all characters were correctly extracted
                    (assert-equal input
                                  (list->string (from-right result))))))

;;; ====
;;; Bounded Memoization Tests (DoS Prevention)
;;; ====

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
                     [mp (memo 'test (parser-char (integer->char 97)))])
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

;;; ====
;;; Indentation-Sensitive Parsing Tests
;;; ====

(test-group indentation-tests
            ;; Test get-column
            (define-test get-column-at-start-test
              (let ([result (parser-parse get-column "hello")])
                   (assert-true (right? result))
                   (assert-equal 1 (from-right result))))
            
            ;; Test get-line
            (define-test get-line-at-start-test
              (let ([result (parser-parse get-line "hello")])
                   (assert-true (right? result))
                   (assert-equal 1 (from-right result))))
            
            ;; Test at-column - success
            (define-test at-column-success-test
              (let ([p (at-column 1 (parser-char (integer->char 97)))])
                   (assert-parses p "a" (integer->char 97))))
            
            ;; Test at-column - failure (wrong column)
            (define-test at-column-failure-test
              (let* ([p1 (parser-then (parser-many (parser-char (integer->char 32)))
                                      (at-column 1 (parser-char (integer->char 97))))]
                     [result (parser-parse p1 "  a")])  ; 'a' at column 3, not 1
                    (assert-true (left? result))))
            
            ;; Test column-gt - success
            (define-test column-gt-success-test
              (let* ([p (parser-then (parser-many (parser-char (integer->char 32)))
                                     (column-gt 2 (parser-char (integer->char 97))))]
                     [result (parser-parse p "   a")])  ; 'a' at column 4 > 2
                    (assert-true (right? result))))
            
            ;; Test column-gt - failure
            (define-test column-gt-failure-test
              (let* ([p (column-gt 2 (parser-char (integer->char 97)))]
                     [result (parser-parse p "a")])  ; 'a' at column 1, not > 2
                    (assert-true (left? result))))
            
            ;; Test column-ge - success at exact column
            (define-test column-ge-exact-test
              (let* ([p (parser-then (parser-char (integer->char 32))
                                     (column-ge 2 (parser-char (integer->char 97))))]
                     [result (parser-parse p " a")])  ; 'a' at column 2 = 2
                    (assert-true (right? result))))
            
            ;; Test same-line - success
            (define-test same-line-success-test
              (let ([p (same-line (parser-string "hello"))])
                   (assert-parses p "hello" "hello")))
            
            ;; Test same-line - failure when crossing newline
            (define-test same-line-failure-test
              (let* ([p (same-line (parser-then (parser-string "hel")
                                                (parser-then (parser-char (integer->char 10))
                                                             (parser-string "lo"))))]
                     [result (parser-parse p "hel\nlo")])
                    (assert-true (left? result))))
            
            ;; Test newline-parser
            (define-test newline-parser-test
              (let ([result (parser-parse newline-parser "\n")])
                   (assert-true (right? result))))
            
            ;; Test indent-spaces returns correct column
            (define-test indent-spaces-test
              (let ([result (parser-parse indent-spaces "  hello")])
                   (assert-true (right? result))
                   (assert-equal 3 (from-right result))))  ; After 2 spaces, at col 3
            
            ;; Test newline-indent
            (define-test newline-indent-test
              (let* ([p (parser-then (parser-string "x") newline-indent)]
                     [result (parser-parse p "x\n  y")])  ; After newline and 2 spaces
                    (assert-true (right? result))
                    (assert-equal 3 (from-right result))))
            
            ;; Test blank-line
            (define-test blank-line-test
              (let ([result (parser-parse blank-line "   \n")])
                   (assert-true (right? result))))
            
            ;; Test blank-line fails on non-blank
            (define-test blank-line-non-blank-test
              (let ([result (parser-parse blank-line "   x\n")])
                   (assert-true (left? result))))
            
            ;; Test indented (column > 1)
            (define-test indented-test
              (let* ([p (parser-then (parser-char (integer->char 32))
                                     (parser-indented (parser-char (integer->char 97))))]
                     [result (parser-parse p " a")])  ; 'a' at column 2 > 1
                    (assert-true (right? result))))
            
            ;; Test indented fails at column 1
            (define-test indented-fail-test
              (let ([result (parser-parse (parser-indented (parser-char (integer->char 97))) "a")])
                   (assert-true (left? result))))
            
            ;; Test aligned (exact column match)
            (define-test aligned-test
              (let* ([p (parser-then (parser-many (parser-char (integer->char 32)))
                                     (aligned 3 (parser-char (integer->char 97))))]
                     [result (parser-parse p "  a")])  ; 'a' at column 3
                    (assert-true (right? result))))
            
            ;; Test indented-block parses multiple items
            (define-test indented-block-basic-test
              (let* ([line-p (parser-some parser-letter)]  ; One or more letters
                     [block-p (indented-block line-p)]
                     ;; Parse starting at column 3, with items at column 3
                     [wrapped (parser-then (parser-string "  ")
                                           block-p)]
                     [result (parser-parse wrapped "  foo\n  bar\n  baz")])
                    (assert-true (right? result))
                    (let ([items (from-right result)])
                         (assert-equal 3 (length items)))))
            
            ;; Test indented-block stops on dedent
            (define-test indented-block-dedent-test
              (let* ([line-p (parser-some parser-letter)]
                     [block-p (indented-block line-p)]
                     [wrapped (parser-then (parser-string "  ")
                                           block-p)]
                     [result (parser-parse wrapped "  foo\n  bar\nx")])  ; 'x' dedented
                    (assert-true (right? result))
                    (let ([items (from-right result)])
                         (assert-equal 2 (length items)))))  ; Only foo, bar
            
            ;; Test indented-block-strict fails on different indentation
            (define-test indented-block-strict-test
              (let* ([line-p (parser-some parser-letter)]
                     [block-p (indented-block-strict line-p)]
                     [wrapped (parser-then (parser-string "  ")
                                           block-p)]
                     ;; foo at col 3, bar at col 4 (different)
                     [result (parser-parse wrapped "  foo\n   bar")])
                    ;; Should only parse foo since bar is at different column
                    (assert-true (right? result))
                    (let ([items (from-right result)])
                         (assert-equal 1 (length items)))))
            
            ;; Test next-line
            (define-test next-line-test
              (let* ([p (parser-then (parser-string "first")
                                     (next-line (parser-string "second")))]
                     [result (parser-parse p "first\n  second")])
                    (assert-true (right? result))
                    (assert-equal "second" (from-right result))))
            
            ;; Test offside (alias for column-gt)
            (define-test offside-test
              (let* ([p (parser-then (parser-many (parser-char (integer->char 32)))
                                     (offside 1 (parser-char (integer->char 97))))]
                     [result (parser-parse p "  a")])  ; col 3 > 1
                    (assert-true (right? result)))))

;;; ====
;;; Summary
;;; ====

(display "
")
(display "====
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
