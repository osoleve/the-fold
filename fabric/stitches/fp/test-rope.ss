;;; fabric/stitches/fp/test-rope.ss — Tests for Rope Library
;;;
;;; NOTE: Run from fabric/stitches directory

(load "test-framework.ss")
(load "fp/rope.ss")

(display "\n")
(display "==============================================================\n")
(display "         ROPE TESTS\n")
(display "==============================================================\n")

;;; Helper: create test string of length n
(define (make-test-string n)
  (list->string (map (lambda (i) (integer->char (+ 97 (modulo i 26))))
                     (iota n))))

(define (iota n)
  (let loop ([i 0] [acc '()])
    (if (>= i n)
        (reverse acc)
        (loop (+ i 1) (cons i acc)))))

;;; ============================================================
;;; Basic Rope Tests
;;; ============================================================

(test-group rope-basics
  (define-test empty-rope-test
    (assert-true (rope-empty? rope-empty))
    (assert-equal 0 (rope-length rope-empty)))

  (define-test leaf-rope-test
    (let ([r (string->rope "hello")])
      (assert-true (rope? r))
      (assert-false (rope-empty? r))
      (assert-equal 5 (rope-length r))))

  (define-test string-conversion-test
    (let* ([original "Hello, World!"]
           [r (string->rope original)]
           [result (rope->string r)])
      (assert-equal original result)))

  (define-test empty-string-conversion-test
    (let* ([r (string->rope "")]
           [result (rope->string r)])
      (assert-equal "" result)
      (assert-true (rope-empty? r)))))

;;; ============================================================
;;; Character Access Tests
;;; ============================================================

(test-group char-access
  (define-test rope-ref-test
    (let ([r (string->rope "abcde")])
      (assert-equal (string-ref "a" 0) (from-just (rope-ref r 0)))
      (assert-equal (string-ref "c" 0) (from-just (rope-ref r 2)))
      (assert-equal (string-ref "e" 0) (from-just (rope-ref r 4)))))

  (define-test rope-ref-bounds-test
    (let ([r (string->rope "abc")])
      (assert-true (nothing? (rope-ref r -1)))
      (assert-true (nothing? (rope-ref r 3)))
      (assert-true (nothing? (rope-ref r 100)))))

  (define-test rope-ref-large-test
    (let* ([str (make-test-string 200)]
           [r (string->rope str)])
      (assert-equal (string-ref str 0) (from-just (rope-ref r 0)))
      (assert-equal (string-ref str 99) (from-just (rope-ref r 99)))
      (assert-equal (string-ref str 199) (from-just (rope-ref r 199))))))

;;; ============================================================
;;; Concatenation Tests
;;; ============================================================

(test-group concatenation
  (define-test append-simple-test
    (let* ([r1 (string->rope "Hello, ")]
           [r2 (string->rope "World!")]
           [result (rope-append r1 r2)])
      (assert-equal "Hello, World!" (rope->string result))))

  (define-test append-empty-left-test
    (let* ([r (string->rope "test")]
           [result (rope-append rope-empty r)])
      (assert-equal "test" (rope->string result))))

  (define-test append-empty-right-test
    (let* ([r (string->rope "test")]
           [result (rope-append r rope-empty)])
      (assert-equal "test" (rope->string result))))

  (define-test append-many-test
    (let* ([r1 (string->rope "a")]
           [r2 (string->rope "b")]
           [r3 (string->rope "c")]
           [r4 (string->rope "d")]
           [result (rope-append (rope-append r1 r2) (rope-append r3 r4))])
      (assert-equal "abcd" (rope->string result))))

  (define-test mconcat-test
    (let* ([ropes (list (string->rope "a")
                        (string->rope "b")
                        (string->rope "c"))]
           [result (rope-mconcat ropes)])
      (assert-equal "abc" (rope->string result)))))

;;; ============================================================
;;; Split Tests
;;; ============================================================

(test-group split-operations
  (define-test split-middle-test
    (let* ([r (string->rope "abcdef")]
           [result (rope-split r 3)])
      (assert-equal "abc" (rope->string (car result)))
      (assert-equal "def" (rope->string (cdr result)))))

  (define-test split-start-test
    (let* ([r (string->rope "abcdef")]
           [result (rope-split r 0)])
      (assert-equal "" (rope->string (car result)))
      (assert-equal "abcdef" (rope->string (cdr result)))))

  (define-test split-end-test
    (let* ([r (string->rope "abcdef")]
           [result (rope-split r 6)])
      (assert-equal "abcdef" (rope->string (car result)))
      (assert-equal "" (rope->string (cdr result)))))

  (define-test take-test
    (let ([r (string->rope "Hello, World!")])
      (assert-equal "Hello" (rope->string (rope-take r 5)))))

  (define-test drop-test
    (let ([r (string->rope "Hello, World!")])
      (assert-equal "World!" (rope->string (rope-drop r 7))))))

;;; ============================================================
;;; Insert/Delete Tests
;;; ============================================================

(test-group insert-delete
  (define-test insert-middle-test
    (let* ([r (string->rope "ac")]
           [result (rope-insert r 1 "b")])
      (assert-equal "abc" (rope->string result))))

  (define-test insert-start-test
    (let* ([r (string->rope "bc")]
           [result (rope-insert r 0 "a")])
      (assert-equal "abc" (rope->string result))))

  (define-test insert-end-test
    (let* ([r (string->rope "ab")]
           [result (rope-insert r 2 "c")])
      (assert-equal "abc" (rope->string result))))

  (define-test insert-empty-test
    (let* ([r (string->rope "abc")]
           [result (rope-insert r 1 "")])
      (assert-equal "abc" (rope->string result))))

  (define-test delete-middle-test
    (let* ([r (string->rope "abXYZcd")]
           [result (rope-delete r 2 5)])
      (assert-equal "abcd" (rope->string result))))

  (define-test delete-start-test
    (let* ([r (string->rope "XXXabc")]
           [result (rope-delete r 0 3)])
      (assert-equal "abc" (rope->string result))))

  (define-test delete-end-test
    (let* ([r (string->rope "abcXXX")]
           [result (rope-delete r 3 6)])
      (assert-equal "abc" (rope->string result))))

  (define-test replace-test
    (let* ([r (string->rope "Hello, World!")]
           [result (rope-replace r 7 12 "Scheme")])
      (assert-equal "Hello, Scheme!" (rope->string result)))))

;;; ============================================================
;;; Substring Tests
;;; ============================================================

(test-group substring-operations
  (define-test substring-test
    (let ([r (string->rope "Hello, World!")])
      (assert-equal "World" (rope->string (rope-substring r 7 12)))))

  (define-test substring-full-test
    (let ([r (string->rope "abc")])
      (assert-equal "abc" (rope->string (rope-substring r 0 3)))))

  (define-test substring-empty-test
    (let ([r (string->rope "abc")])
      (assert-equal "" (rope->string (rope-substring r 1 1))))))

;;; ============================================================
;;; Search Tests
;;; ============================================================

(test-group search-operations
  (define-test index-of-test
    (let ([r (string->rope "Hello, World!")])
      (assert-equal 7 (from-just (rope-index-of r (string-ref "W" 0))))))

  (define-test index-of-not-found-test
    (let ([r (string->rope "Hello, World!")])
      (assert-true (nothing? (rope-index-of r (string-ref "z" 0))))))

  (define-test count-char-test
    (let ([r (string->rope "banana")])
      (assert-equal 3 (rope-count-char r (string-ref "a" 0)))
      (assert-equal 2 (rope-count-char r (string-ref "n" 0)))
      (assert-equal 0 (rope-count-char r (string-ref "z" 0)))))

  (define-test find-test
    (let ([r (string->rope "Hello, World!")])
      (assert-equal 7 (from-just (rope-find r "World")))))

  (define-test find-not-found-test
    (let ([r (string->rope "Hello, World!")])
      (assert-true (nothing? (rope-find r "Scheme"))))))

;;; ============================================================
;;; Line Operations Tests
;;; ============================================================

(test-group line-operations
  (define-test line-count-test
    (let ([r (string->rope "line1\nline2\nline3")])
      (assert-equal 3 (rope-line-count r))))

  (define-test line-count-no-newline-test
    (let ([r (string->rope "single line")])
      (assert-equal 1 (rope-line-count r))))

  (define-test get-line-test
    (let ([r (string->rope "line1\nline2\nline3")])
      (assert-equal "line1" (from-just (rope-get-line r 0)))
      (assert-equal "line2" (from-just (rope-get-line r 1)))
      (assert-equal "line3" (from-just (rope-get-line r 2)))))

  (define-test get-line-bounds-test
    (let ([r (string->rope "line1\nline2")])
      (assert-true (nothing? (rope-get-line r 5)))))

  (define-test line-start-test
    (let ([r (string->rope "line1\nline2\nline3")])
      (assert-equal 0 (from-just (rope-line-start r 0)))
      (assert-equal 6 (from-just (rope-line-start r 1)))
      (assert-equal 12 (from-just (rope-line-start r 2))))))

;;; ============================================================
;;; Iteration Tests
;;; ============================================================

(test-group iteration
  (define-test fold-left-test
    (let* ([r (string->rope "abc")]
           [chars (rope-fold-left (lambda (acc c) (cons c acc)) '() r)])
      (assert-equal 3 (length chars))))

  (define-test map-test
    (let* ([r (string->rope "abc")]
           [result (rope-map char-upcase r)])
      (assert-equal "ABC" (rope->string result))))

  (define-test filter-test
    (let* ([r (string->rope "a1b2c3")]
           [result (rope-filter char-alphabetic? r)])
      (assert-equal "abc" (rope->string result)))))

;;; ============================================================
;;; Comparison Tests
;;; ============================================================

(test-group comparison
  (define-test equal-test
    (let ([r1 (string->rope "hello")]
          [r2 (string->rope "hello")]
          [r3 (string->rope "world")])
      (assert-true (rope-equal? r1 r2))
      (assert-false (rope-equal? r1 r3))))

  (define-test compare-test
    (let ([r1 (string->rope "abc")]
          [r2 (string->rope "abd")]
          [r3 (string->rope "abc")])
      (assert-equal -1 (rope-compare r1 r2))
      (assert-equal 1 (rope-compare r2 r1))
      (assert-equal 0 (rope-compare r1 r3)))))

;;; ============================================================
;;; Balancing Tests
;;; ============================================================

(test-group balancing
  (define-test rebalance-test
    (let* ([r (fold-left (lambda (acc s) (rope-append acc (string->rope s)))
                         rope-empty
                         (list "a" "b" "c" "d" "e" "f" "g" "h"))]
           [rebalanced (rope-rebalance r)])
      (assert-equal (rope->string r) (rope->string rebalanced))))

  (define-test depth-leaf-test
    (let ([r (string->rope "short")])
      (assert-equal 0 (rope-depth r))))

  (define-test depth-node-test
    (let* ([r1 (string->rope "left")]
           [r2 (string->rope "right")]
           [combined (rope-append r1 r2)])
      (assert-true (>= (rope-depth combined) 0)))))

;;; ============================================================
;;; Advanced Operations Tests
;;; ============================================================

(test-group advanced-operations
  (define-test reverse-test
    (let ([r (string->rope "hello")])
      (assert-equal "olleh" (rope->string (rope-reverse r)))))

  (define-test upcase-test
    (let ([r (string->rope "Hello, World!")])
      (assert-equal "HELLO, WORLD!" (rope->string (rope-upcase r)))))

  (define-test downcase-test
    (let ([r (string->rope "Hello, World!")])
      (assert-equal "hello, world!" (rope->string (rope-downcase r)))))

  (define-test trim-test
    (let ([r (string->rope "  hello  ")])
      (assert-equal "hello" (rope->string (rope-trim r))))))

;;; ============================================================
;;; Builder Tests
;;; ============================================================

(test-group builder
  (define-test builder-basic-test
    (let* ([b (rope-builder)]
           [b (builder-append-string! b "Hello")]
           [b (builder-append-string! b ", ")]
           [b (builder-append-string! b "World!")]
           [result (builder-finish b)])
      (assert-equal "Hello, World!" (rope->string result))))

  (define-test builder-line-test
    (let* ([b (rope-builder)]
           [b (builder-append-line! b "line1")]
           [b (builder-append-line! b "line2")]
           [result (builder-finish b)])
      (assert-equal "line1\nline2\n" (rope->string result)))))

;;; ============================================================
;;; Large String Tests
;;; ============================================================

(test-group large-strings
  (define-test large-string-test
    (let* ([str (make-test-string 1000)]
           [r (string->rope str)]
           [result (rope->string r)])
      (assert-equal 1000 (rope-length r))
      (assert-equal str result)))

  (define-test many-appends-test
    (let loop ([i 0] [r rope-empty])
      (if (>= i 100)
          (assert-equal 100 (rope-length r))
          (loop (+ i 1) (rope-append r (string->rope "x"))))))

  (define-test split-large-test
    (let* ([str (make-test-string 500)]
           [r (string->rope str)]
           [split (rope-split r 250)])
      (assert-equal 250 (rope-length (car split)))
      (assert-equal 250 (rope-length (cdr split))))))

;;; ============================================================
;;; Edge Cases
;;; ============================================================

(test-group edge-cases
  (define-test single-char-rope-test
    (let ([r (string->rope "x")])
      (assert-equal 1 (rope-length r))
      (assert-equal (string-ref "x" 0) (from-just (rope-ref r 0)))))

  (define-test split-single-test
    (let* ([r (string->rope "x")]
           [split (rope-split r 1)])
      (assert-equal "x" (rope->string (car split)))
      (assert-equal "" (rope->string (cdr split)))))

  (define-test chars-conversion-test
    (let* ([chars (list (string-ref "a" 0) (string-ref "b" 0) (string-ref "c" 0))]
           [r (chars->rope chars)]
           [result (rope->chars r)])
      (assert-equal chars result))))

;;; ============================================================
;;; Summary
;;; ============================================================

(display "\n")
(display "==============================================================\n")
(printf "Tests passed: ~a\n" *tests-passed*)
(printf "Tests failed: ~a\n" *tests-failed*)
(printf "Total tests:  ~a\n" *tests-run*)

(if (= *tests-failed* 0)
    (display "\n[SUCCESS] All rope tests passed.\n")
    (display "\n[FAILURE] Some rope tests failed.\n"))
