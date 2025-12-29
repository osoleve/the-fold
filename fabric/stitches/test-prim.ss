;;; Test harness for core/prim.ss

(load "fabric/stitches/block.ss")
(load "fabric/stitches/sha256.ss")
(load "fabric/stitches/cas.ss")
(load "fabric/stitches/prim.ss")

(define (test name expected actual)
  (display "  ")
  (display name)
  (display ": ")
  (if (equal? expected actual)
      (display "✓")
      (begin
       (display "✗
    expected: ")
       (display expected)
       (display "
    got: ")
       (display actual)))
  (newline))

;; Note: Removed pass-count/fail-count definitions to avoid
;; shadowing run-tests.ss variables

(define (test-section name)
  (newline)
  (display name)
  (newline))

;;; ============================================================
;;; Arithmetic
;;; ============================================================
(test-section "Arithmetic")
(test "add" 5 (prim 'add 2 3))
(test "add many" 15 (prim 'add 1 2 3 4 5))
(test "sub" 7 (prim 'sub 10 3))
(test "mul" 24 (prim 'mul 4 6))
(test "div" 4 (prim 'div 17 4))
(test "div-by-zero" '(error div-by-zero) (prim 'div 5 0))
(test "mod" 2 (prim 'mod 17 5))
(test "mod-by-zero" '(error mod-by-zero) (prim 'mod 5 0))
(test "neg" -7 (prim 'neg 7))
(test "abs positive" 5 (prim 'abs 5))
(test "abs negative" 5 (prim 'abs -5))

;;; ============================================================
;;; Comparison
;;; ============================================================
(test-section "Comparison")
(test "eq? true" #t (prim 'eq? 5 5))
(test "eq? false" #f (prim 'eq? 5 6))
(test "eq? symbols" #t (prim 'eq? 'foo 'foo))
(test "lt?" #t (prim 'lt? 3 5))
(test "le? equal" #t (prim 'le? 5 5))
(test "gt?" #t (prim 'gt? 7 3))
(test "ge?" #t (prim 'ge? 5 5))
(test "zero?" #t (prim 'zero? 0))
(test "positive?" #t (prim 'positive? 5))
(test "negative?" #t (prim 'negative? -3))

;;; ============================================================
;;; Bitwise
;;; ============================================================
(test-section "Bitwise")
(test "bitand" #b1010 (prim 'bitand #b1111 #b1010))
(test "bitor" #b1111 (prim 'bitor #b1100 #b0011))
(test "bitxor" #b0110 (prim 'bitxor #b1100 #b1010))
(test "shl" 8 (prim 'shl 1 3))
(test "shr" 4 (prim 'shr 32 3))

;;; ============================================================
;;; Boolean
;;; ============================================================
(test-section "Boolean")
(test "not true" #f (prim 'not #t))
(test "not false" #t (prim 'not #f))

;;; ============================================================
;;; Block operations
;;; ============================================================
(test-section "Block operations")
(define test-payload (string->utf8 "hello"))
(define test-block (prim 'make-block 'greeting test-payload (vector)))
(test "block-tag" 'greeting (prim 'block-tag test-block))
(test "block-payload" test-payload (prim 'block-payload test-block))
(test "block-refs empty" (vector) (prim 'block-refs test-block))

;;; Block with refs
(define ref1 (make-bytevector address-size #xaa))
(define ref2 (make-bytevector address-size #xbb))
(define parent-block (prim 'make-block 'parent (string->utf8 "data") (vector ref1 ref2)))
(test "block-ref 0" ref1 (prim 'block-ref parent-block 0))
(test "block-ref 1" ref2 (prim 'block-ref parent-block 1))
(test "block-ref out of bounds" '(error index-out-of-bounds) (prim 'block-ref parent-block 5))

;;; Round-trip
(define serialized (prim 'block->bytes test-block))
(define restored (prim 'bytes->block serialized))
(test "block round-trip tag" 'greeting (prim 'block-tag restored))

;;; ============================================================
;;; Bytevector operations
;;; ============================================================
(test-section "Bytevector operations")
(define bv (make-bytevector 5 #x42))
(test "bv-length" 5 (prim 'bv-length bv))
(test "bv-ref" #x42 (prim 'bv-ref bv 0))
(test "bv-make" 10 (prim 'bv-length (prim 'bv-make 10)))

(define bv1 (string->utf8 "hello"))
(define bv2 (string->utf8 "world"))
(define combined (prim 'bv-concat bv1 bv2))
(test "bv-concat length" 10 (prim 'bv-length combined))

(define sliced (prim 'bv-slice combined 0 5))
(test "bv-slice" "hello" (utf8->string sliced))

;;; ============================================================
;;; String/Bytevector conversion
;;; ============================================================
(test-section "String conversion")
(test "string->utf8->string" "test" (prim 'utf8->string (prim 'string->utf8 "test")))
(test "symbol->string" "foo" (prim 'symbol->string 'foo))
(test "string->symbol" 'bar (prim 'string->symbol "bar"))

;;; ============================================================
;;; List operations
;;; ============================================================
(test-section "List operations")
(test "cons" '(1 . 2) (prim 'cons 1 2))
(test "car" 1 (prim 'car '(1 2 3)))
(test "cdr" '(2 3) (prim 'cdr '(1 2 3)))
(test "null? true" #t (prim 'null? '()))
(test "null? false" #f (prim 'null? '(1)))
(test "pair?" #t (prim 'pair? '(1 . 2)))
(test "list" '(1 2 3) (prim 'list 1 2 3))
(test "length" 3 (prim 'length '(a b c)))
(test "append" '(1 2 3 4) (prim 'append '(1 2) '(3 4)))
(test "reverse" '(3 2 1) (prim 'reverse '(1 2 3)))
(test "list-ref" 'b (prim 'list-ref '(a b c) 1))
(test "memq found" '(b c) (prim 'memq 'b '(a b c)))
(test "assq" '(b . 2) (prim 'assq 'b '((a . 1) (b . 2))))

;;; ============================================================
;;; Vector operations
;;; ============================================================
(test-section "Vector operations")
(define v (prim 'vec-make 1 2 3))
(test "vec-make" (vector 1 2 3) v)
(test "vec-ref" 2 (prim 'vec-ref v 1))
(test "vec-length" 3 (prim 'vec-length v))
(test "vec->list" '(1 2 3) (prim 'vec->list v))
(test "list->vec" (vector 'a 'b) (prim 'list->vec '(a b)))

;;; ============================================================
;;; String operations
;;; ============================================================
(test-section "String operations")
(test "string-length" 5 (prim 'string-length "hello"))
(test "string-length empty" 0 (prim 'string-length ""))
(test "string-ref" # (prim 'string-ref "hello" 1))
(test "string-append" "helloworld" (prim 'string-append "hello" "world"))
(test "string-append many" "abcdef" (prim 'string-append "ab" "cd" "ef"))
(test "substring" "ell" (prim 'substring "hello" 1 4))
(test "string=?" #t (prim 'string=? "hello" "hello"))
(test "string=? false" #f (prim 'string=? "hello" "world"))
(test "string<?" #t (prim 'string<? "abc" "abd"))
(test "string>?" #t (prim 'string>? "xyz" "abc"))
(test "make-string" "   " (prim 'make-string 3 #\space))
(test "string->list" '(# # #