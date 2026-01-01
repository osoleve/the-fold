;;; thimble/block-query.ss — Query language for content-addressed blocks
;;; Created by sonnet-secretive
;;;
;;; A simple DSL for searching blocks by patterns:
;;;   (query (tag 'expr) (payload-contains "lambda"))
;;;   (query (refs-count > 2))
;;;   (query (tag-matches "^test-"))

;;; NOTE: string-contains? is provided by core/prelude.ss

(library (shell block-query)
         (export
          ;; Query execution
          query-blocks
          
          ;; Pattern constructors
          tag-is
          tag-matches
          payload-contains
          payload-matches
          refs-count
          has-ref
          
          ;; Combinators
          query-and
          query-or
          query-not)
         
         (import (chezscheme))
         
         ;; Load core dependencies (assume they're available)
         ;; In actual use, this would import from (core block) and (core cas)
         
         ;;; ============================================================
         ;;; Query Pattern Types
         ;;; ============================================================
         
         ;; A query pattern is a predicate: Block → Boolean
         ;; We represent queries as procedures for maximum flexibility
         
         ;;; tag-is : Symbol → QueryPattern
         ;;; Match blocks with exact tag
         (define (tag-is expected-tag)
           (lambda (blk)
                   (eq? (block-tag blk) expected-tag)))
         
         ;;; tag-matches : String → QueryPattern
         ;;; Match blocks whose tag matches regex pattern
         (define (tag-matches pattern)
           (let ([rx (pregexp pattern)])
                (lambda (blk)
                        (pregexp-match rx (symbol->string (block-tag blk))))))
         
         ;;; payload-contains : String → QueryPattern
         ;;; Match blocks whose payload (as UTF-8) contains substring
         (define (payload-contains substring)
           (lambda (blk)
                   (let ([payload-str (utf8->string (block-payload blk))])
                        (string-contains? payload-str substring))))
         
         ;;; payload-matches : String → QueryPattern
         ;;; Match blocks whose payload matches regex
         (define (payload-matches pattern)
           (let ([rx (pregexp pattern)])
                (lambda (blk)
                        (let ([payload-str (utf8->string (block-payload blk))])
                             (pregexp-match rx payload-str)))))
         
         ;;; refs-count : (Nat → Boolean) → QueryPattern
         ;;; Match blocks based on reference count
         ;;; Examples: (refs-count (lambda (n) (> n 2)))
         ;;;           (refs-count zero?)
         (define (refs-count predicate)
           (lambda (blk)
                   (predicate (vector-length (block-refs blk)))))
         
         ;;; has-ref : Bytevector → QueryPattern
         ;;; Match blocks that reference the given hash
         (define (has-ref target-hash)
           (lambda (blk)
                   (let ([refs (block-refs blk)])
                        (let loop ([i 0])
                             (cond
                              [(= i (vector-length refs)) #f]
                              [(bytevector=? (vector-ref refs i) target-hash) #t]
                              [else (loop (+ i 1))])))))
         
         ;;; ============================================================
         ;;; Query Combinators
         ;;; ============================================================
         
         ;;; query-and : QueryPattern ... → QueryPattern
         ;;; Match blocks that satisfy ALL patterns
         (define (query-and . patterns)
           (lambda (blk)
                   (let loop ([ps patterns])
                        (cond
                         [(null? ps) #t]
                         [((car ps) blk) (loop (cdr ps))]
                         [else #f]))))
         
         ;;; query-or : QueryPattern ... → QueryPattern
         ;;; Match blocks that satisfy ANY pattern
         (define (query-or . patterns)
           (lambda (blk)
                   (let loop ([ps patterns])
                        (cond
                         [(null? ps) #f]
                         [((car ps) blk) #t]
                         [else (loop (cdr ps))]))))
         
         ;;; query-not : QueryPattern → QueryPattern
         ;;; Match blocks that do NOT satisfy pattern
         (define (query-not pattern)
           (lambda (blk)
                   (not (pattern blk))))
         
         ;;; ============================================================
         ;;; Query Execution
         ;;; ============================================================
         
         ;;; query-blocks : QueryPattern → (List (Pair Bytevector Block))
         ;;; Execute query against all blocks in the store
         ;;; Returns list of (hash . block) pairs
         ;;;
         ;;; Note: This requires access to the store, which is in core/cas.ss
         ;;; In practice, this would need to be called with store access
         (define (query-blocks pattern)
           ;; This is a placeholder - actual implementation would need
           ;; to iterate over the store
           ;; Assuming we have access to (store-hashes) and (fetch)
           (let ([all-hashes (store-hashes)])
                (filter
                 (lambda (hash-block-pair)
                         (pattern (cdr hash-block-pair)))
                 (map (lambda (hash)
                              (cons hash (fetch hash)))
                      all-hashes))))
         
         ;;; NOTE: string-contains? provided by core/prelude.ss
         ;;; If this module is used standalone, ensure core/prelude.ss is loaded.
         )
