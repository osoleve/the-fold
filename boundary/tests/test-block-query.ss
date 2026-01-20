(load "core/blocks/block.ss")
(load "core/base/sha256.ss")
(load "core/blocks/cas.ss")
(load "boundary/blocks/block-query.ss")

(import (shell block-query))

(doc 'module 'test-block-query)
(doc 'description "Tests for block query language - basic query primitives and combinators")
(doc 'layer 'boundary)
(doc 'purity 'partial)

(doc 'section 'test-setup)
(define test-block-1
  (make-block 'expr
              (string->utf8 "(lambda (x) (+ x 1))")
              empty-refs))

(define test-block-2
  (make-block 'test-expr
              (string->utf8 "(define (foo) 42)")
              empty-refs))

(define test-block-3
  (make-block 'data
              (string->utf8 "some data here")
              (vector (make-bytevector address-size))))

;; Store them
(define hash-1 (store! test-block-1))
(define hash-2 (store! test-block-2))
(define hash-3 (store! test-block-3))

(doc 'section 'tag-queries)
(display "Testing tag-is...\n")
(assert ((tag-is 'expr) test-block-1))
(assert (not ((tag-is 'expr) test-block-2)))

;; Test tag-matches
(display "Testing tag-matches...\n")
(assert ((tag-matches "^test-") test-block-2))
(assert (not ((tag-matches "^test-") test-block-1)))

(doc 'section 'payload-queries)
(display "Testing payload-contains...\n")
(assert ((payload-contains "lambda") test-block-1))
(assert (not ((payload-contains "lambda") test-block-2)))

(doc 'section 'refs-queries)
(display "Testing refs-count...\n")
(assert ((refs-count zero?) test-block-1))
(assert ((refs-count (lambda (n) (> n 0))) test-block-3))

(doc 'section 'query-combinators)
(display "Testing query-and...\n")
(assert ((query-and (tag-is 'expr) (payload-contains "lambda")) test-block-1))
(assert (not ((query-and (tag-is 'expr) (payload-contains "define")) test-block-1)))

(display "Testing query-or...\n")
(assert ((query-or (tag-is 'expr) (tag-is 'data)) test-block-1))
(assert ((query-or (tag-is 'expr) (tag-is 'data)) test-block-3))

(display "Testing query-not...\n")
(assert ((query-not (tag-is 'expr)) test-block-2))
(assert (not ((query-not (tag-is 'expr)) test-block-1)))

(doc 'section 'query-execution)
(display "Testing query-blocks...\n")
(let ([results (query-blocks (tag-matches "^(expr|test-)"))])
     (assert (= (length results) 2)))

(display "All block query tests passed!\n")
