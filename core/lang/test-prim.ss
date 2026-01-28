;;; Test harness for core/lang/prim.ss
;;; Standardized to use test-framework.ss

(load "core/testing/test-framework.ss")
(load "core/blocks/block.ss")
(load "core/base/sha256.ss")
(load "core/blocks/cas.ss")
(load "core/lang/prim.ss")

;;; ============================================================================
;;; Tests
;;; ============================================================================

(test-group arithmetic
  (define-test "add"
    (assert-equal 5 (prim 'add 2 3)))

  (define-test "add many"
    (assert-equal 15 (prim 'add 1 2 3 4 5)))

  (define-test "sub"
    (assert-equal 7 (prim 'sub 10 3)))

  (define-test "mul"
    (assert-equal 24 (prim 'mul 4 6)))

  (define-test "div"
    (assert-equal 4 (prim 'div 17 4)))

  (define-test "div-by-zero"
    (assert-equal '(error div-by-zero) (prim 'div 5 0)))

  (define-test "mod"
    (assert-equal 2 (prim 'mod 17 5)))

  (define-test "mod-by-zero"
    (assert-equal '(error mod-by-zero) (prim 'mod 5 0)))

  (define-test "neg"
    (assert-equal -7 (prim 'neg 7)))

  (define-test "abs positive"
    (assert-equal 5 (prim 'abs 5)))

  (define-test "abs negative"
    (assert-equal 5 (prim 'abs -5)))

  (define-test "sqrt"
    (assert-equal 3 (prim 'sqrt 9)))

  (define-test "expt"
    (assert-equal 32 (prim 'expt 2 5)))

  (define-test "log 1"
    (assert-equal 0 (prim 'log 1)))

  (define-test "sin 0"
    (assert-equal 0 (prim 'sin 0)))

  (define-test "cos 0"
    (assert-equal 1 (prim 'cos 0)))

  (define-test "tan 0"
    (assert-equal 0 (prim 'tan 0)))

  (define-test "floor"
    (assert-equal 3.0 (prim 'floor 3.7)))

  (define-test "ceiling"
    (assert-equal 4.0 (prim 'ceiling 3.2)))

  (define-test "round"
    (assert-equal 4.0 (prim 'round 3.6))))

(test-group comparison
  (define-test "eq? true"
    (assert-true (prim 'eq? 5 5)))

  (define-test "eq? false"
    (assert-false (prim 'eq? 5 6)))

  (define-test "eq? symbols"
    (assert-true (prim 'eq? 'foo 'foo)))

  (define-test "lt?"
    (assert-true (prim 'lt? 3 5)))

  (define-test "le? equal"
    (assert-true (prim 'le? 5 5)))

  (define-test "gt?"
    (assert-true (prim 'gt? 7 3)))

  (define-test "ge?"
    (assert-true (prim 'ge? 5 5)))

  (define-test "zero?"
    (assert-true (prim 'zero? 0)))

  (define-test "positive?"
    (assert-true (prim 'positive? 5)))

  (define-test "negative?"
    (assert-true (prim 'negative? -3))))

(test-group bitwise
  (define-test "bitand"
    (assert-equal #b1010 (prim 'bitand #b1111 #b1010)))

  (define-test "bitor"
    (assert-equal #b1111 (prim 'bitor #b1100 #b0011)))

  (define-test "bitxor"
    (assert-equal #b0110 (prim 'bitxor #b1100 #b1010)))

  (define-test "bitnot"
    (assert-equal -1 (prim 'bitnot 0)))

  (define-test "shl"
    (assert-equal 8 (prim 'shl 1 3)))

  (define-test "shr"
    (assert-equal 4 (prim 'shr 32 3))))

(test-group boolean
  (define-test "not true"
    (assert-false (prim 'not #t)))

  (define-test "not false"
    (assert-true (prim 'not #f)))

  (define-test "and"
    (assert-false (prim 'and #t #f)))

  (define-test "or"
    (assert-true (prim 'or #f #t))))

(test-group block-operations
  (define-test "block-tag"
    (let* ([payload (string->utf8 "hello")]
           [blk (prim 'make-block 'greeting payload (vector))])
      (assert-equal 'greeting (prim 'block-tag blk))))

  (define-test "block-payload"
    (let* ([payload (string->utf8 "hello")]
           [blk (prim 'make-block 'greeting payload (vector))])
      (assert-equal payload (prim 'block-payload blk))))

  (define-test "block-refs empty"
    (let* ([payload (string->utf8 "hello")]
           [blk (prim 'make-block 'greeting payload (vector))])
      (assert-equal (vector) (prim 'block-refs blk))))

  (define-test "block-ref 0"
    (let* ([ref1 (make-bytevector address-size #xaa)]
           [ref2 (make-bytevector address-size #xbb)]
           [parent-block (prim 'make-block 'parent (string->utf8 "data") (vector ref1 ref2))])
      (assert-equal ref1 (prim 'block-ref parent-block 0))))

  (define-test "block-ref 1"
    (let* ([ref1 (make-bytevector address-size #xaa)]
           [ref2 (make-bytevector address-size #xbb)]
           [parent-block (prim 'make-block 'parent (string->utf8 "data") (vector ref1 ref2))])
      (assert-equal ref2 (prim 'block-ref parent-block 1))))

  (define-test "block-ref out of bounds"
    (let* ([ref1 (make-bytevector address-size #xaa)]
           [ref2 (make-bytevector address-size #xbb)]
           [parent-block (prim 'make-block 'parent (string->utf8 "data") (vector ref1 ref2))])
      (assert-equal '(error index-out-of-bounds) (prim 'block-ref parent-block 5))))

  (define-test "block round-trip tag"
    (let* ([payload (string->utf8 "hello")]
           [blk (prim 'make-block 'greeting payload (vector))]
           [serialized (prim 'block->bytes blk)]
           [restored (prim 'bytes->block serialized)])
      (assert-equal 'greeting (prim 'block-tag restored)))))

(test-group bytevector-operations
  (define-test "bv-length"
    (let ([bv (make-bytevector 5 #x42)])
      (assert-equal 5 (prim 'bv-length bv))))

  (define-test "bv-ref"
    (let ([bv (make-bytevector 5 #x42)])
      (assert-equal #x42 (prim 'bv-ref bv 0))))

  (define-test "bv-make"
    (assert-equal 10 (prim 'bv-length (prim 'bv-make 10))))

  (define-test "bv-concat length"
    (let* ([bv1 (string->utf8 "hello")]
           [bv2 (string->utf8 "world")]
           [combined (prim 'bv-concat bv1 bv2)])
      (assert-equal 10 (prim 'bv-length combined))))

  (define-test "bv-slice"
    (let* ([bv1 (string->utf8 "hello")]
           [bv2 (string->utf8 "world")]
           [combined (prim 'bv-concat bv1 bv2)]
           [sliced (prim 'bv-slice combined 0 5)])
      (assert-equal "hello" (utf8->string sliced))))

  (define-test "bv-copy returns dst"
    (let* ([copy-src (string->utf8 "abcde")]
           [copy-dst (make-bytevector 5 0)]
           [copy-result (prim 'bv-copy copy-src 1 copy-dst 0 3)])
      (assert-equal copy-dst copy-result)))

  (define-test "bv-copy first"
    (let* ([copy-src (string->utf8 "abcde")]
           [copy-dst (make-bytevector 5 0)])
      (prim 'bv-copy copy-src 1 copy-dst 0 3)
      (assert-equal (char->integer #\b) (prim 'bv-ref copy-dst 0))))

  (define-test "bv-copy third"
    (let* ([copy-src (string->utf8 "abcde")]
           [copy-dst (make-bytevector 5 0)])
      (prim 'bv-copy copy-src 1 copy-dst 0 3)
      (assert-equal (char->integer #\d) (prim 'bv-ref copy-dst 2))))

  (define-test "bv-copy untouched"
    (let* ([copy-src (string->utf8 "abcde")]
           [copy-dst (make-bytevector 5 0)])
      (prim 'bv-copy copy-src 1 copy-dst 0 3)
      (assert-equal 0 (prim 'bv-ref copy-dst 4)))))

(test-group string-conversion
  (define-test "string->utf8->string"
    (assert-equal "test" (prim 'utf8->string (prim 'string->utf8 "test"))))

  (define-test "symbol->string"
    (assert-equal "foo" (prim 'symbol->string 'foo)))

  (define-test "string->symbol"
    (assert-equal 'bar (prim 'string->symbol "bar")))

  (define-test "number->string"
    (assert-equal "42" (prim 'number->string 42)))

  (define-test "string->number"
    (assert-equal 123 (prim 'string->number "123"))))

(test-group list-operations
  (define-test "cons"
    (assert-equal '(1 . 2) (prim 'cons 1 2)))

  (define-test "car"
    (assert-equal 1 (prim 'car '(1 2 3))))

  (define-test "cdr"
    (assert-equal '(2 3) (prim 'cdr '(1 2 3))))

  (define-test "null? true"
    (assert-true (prim 'null? '())))

  (define-test "null? false"
    (assert-false (prim 'null? '(1))))

  (define-test "pair?"
    (assert-true (prim 'pair? '(1 . 2))))

  (define-test "list"
    (assert-equal '(1 2 3) (prim 'list 1 2 3)))

  (define-test "length"
    (assert-equal 3 (prim 'length '(a b c))))

  (define-test "append"
    (assert-equal '(1 2 3 4) (prim 'append '(1 2) '(3 4))))

  (define-test "reverse"
    (assert-equal '(3 2 1) (prim 'reverse '(1 2 3))))

  (define-test "list-ref"
    (assert-equal 'b (prim 'list-ref '(a b c) 1)))

  (define-test "memq found"
    (assert-equal '(b c) (prim 'memq 'b '(a b c))))

  (define-test "assq"
    (assert-equal '(b . 2) (prim 'assq 'b '((a . 1) (b . 2))))))

(test-group vector-operations
  (define-test "vec-make"
    (let ([v (prim 'vec-make 1 2 3)])
      (assert-equal (vector 1 2 3) v)))

  (define-test "vec-empty"
    (assert-equal (vector) (prim 'vec-empty)))

  (define-test "vec-ref"
    (let ([v (prim 'vec-make 1 2 3)])
      (assert-equal 2 (prim 'vec-ref v 1))))

  (define-test "vec-length"
    (let ([v (prim 'vec-make 1 2 3)])
      (assert-equal 3 (prim 'vec-length v))))

  (define-test "vec->list"
    (let ([v (prim 'vec-make 1 2 3)])
      (assert-equal '(1 2 3) (prim 'vec->list v))))

  (define-test "list->vec"
    (assert-equal (vector 'a 'b) (prim 'list->vec '(a b)))))

(test-group string-operations
  (define-test "string-length"
    (assert-equal 5 (prim 'string-length "hello")))

  (define-test "string-length empty"
    (assert-equal 0 (prim 'string-length "")))

  (define-test "string-ref"
    (assert-equal #\e (prim 'string-ref "hello" 1)))

  (define-test "string-append"
    (assert-equal "helloworld" (prim 'string-append "hello" "world")))

  (define-test "string-append many"
    (assert-equal "abcdef" (prim 'string-append "ab" "cd" "ef")))

  (define-test "substring"
    (assert-equal "ell" (prim 'substring "hello" 1 4)))

  (define-test "string=?"
    (assert-true (prim 'string=? "hello" "hello")))

  (define-test "string=? false"
    (assert-false (prim 'string=? "hello" "world")))

  (define-test "string<?"
    (assert-true (prim 'string<? "abc" "abd")))

  (define-test "string>?"
    (assert-true (prim 'string>? "xyz" "abc")))

  (define-test "make-string"
    (assert-equal "   " (prim 'make-string 3 #\space)))

  (define-test "string->list"
    (assert-equal '(#\h #\e #\l #\l #\o) (prim 'string->list "hello")))

  (define-test "list->string"
    (assert-equal "hi" (prim 'list->string '(#\h #\i)))))

(test-group character-operations
  (define-test "char->integer"
    (assert-equal 65 (prim 'char->integer #\A)))

  (define-test "integer->char"
    (assert-equal #\A (prim 'integer->char 65)))

  (define-test "char=?"
    (assert-true (prim 'char=? #\a #\a)))

  (define-test "char<?"
    (assert-true (prim 'char<? #\a #\b)))

  (define-test "char-alphabetic?"
    (assert-true (prim 'char-alphabetic? #\q)))

  (define-test "char-numeric?"
    (assert-true (prim 'char-numeric? #\5)))

  (define-test "char-whitespace?"
    (assert-true (prim 'char-whitespace? #\space)))

  (define-test "char-upper-case?"
    (assert-true (prim 'char-upper-case? #\Z)))

  (define-test "char-lower-case?"
    (assert-true (prim 'char-lower-case? #\z)))

  (define-test "char-upcase"
    (assert-equal #\A (prim 'char-upcase #\a)))

  (define-test "char-downcase"
    (assert-equal #\a (prim 'char-downcase #\A))))

(test-group primitive-errors
  (define-test "unknown primitive"
    (assert-equal '(error unknown-primitive nope) (prim 'nope 1))))

;;; ============================================================================
;;; Run all tests
;;; ============================================================================

(run-all-tests-and-exit)
