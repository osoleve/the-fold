;;; test-graph-algorithms.ss — Tests for traversal primitives

(source-directories (cons "fabric/stitches" (source-directories)))
(load "fabric/stitches/block.ss")
(load "fabric/stitches/sha256.ss")
(load "thimble/fs.ss")
(load "thimble/store-api.ss")
(load "fabric/patterns/graph-algorithms.ss")

(define tests-passed 0)
(define tests-failed 0)

(define (test name expected actual)
  (if (equal? expected actual)
      (begin
       (set! tests-passed (+ tests-passed 1))
       (printf "  [PASS] ~a
" name))
      (begin
       (set! tests-failed (+ tests-failed 1))
       (printf "  [FAIL] ~a
" name)
       (printf "      Expected: ~s
" expected)
       (printf "      Got:      ~s
" actual))))

(define (test-true name actual)
  (test name #t actual))

(define (test-false name actual)
  (test name #f actual))

(define (list-unique xs)
  (let loop ([rest xs]
             [seen '()])
       (cond
        [(null? rest) (reverse seen)]
        [(memq (car rest) seen) (loop (cdr rest) seen)]
        [else (loop (cdr rest) (cons (car rest) seen))])))

(define (same-set? a b)
  (let ([ua (list-unique a)]
        [ub (list-unique b)])
       (and (= (length ua) (length ub))
            (let loop ([xs ua])
                 (cond
                  [(null? xs) #t]
                  [(memq (car xs) ub) (loop (cdr xs))]
                  [else #f])))))

(define (collect-visit-tags traverse fs start-hash)
  (let ([tags '()])
       (traverse fs start-hash
                 (lambda (_h b)
                         (set! tags (append tags (list (block-tag b))))))
       tags))

(printf "
")
(printf "================================================================
")
(printf "              GRAPH TRAVERSAL TESTS                            
")
(printf "================================================================

")

;;; ============================================================
;;; Test Setup
;;; ============================================================

(define test-store-path ".test-graph-algorithms")

(when (file-exists? test-store-path)
      (system (format "rm -rf ~a" test-store-path)))

(define fs (mint-fs-capability test-store-path))

;; Build a DAG with shared children to test duplicate visits.
;; A -> B, C
;; B -> D
;; C -> D, E
;; D -> E
;; E -> (none)
(define block-e (make-block 'node-e (string->utf8 "E") (vector)))
(define hash-e (hash-block block-e))

(define block-d (make-block 'node-d (string->utf8 "D") (vector hash-e)))
(define hash-d (hash-block block-d))

(define block-b (make-block 'node-b (string->utf8 "B") (vector hash-d)))
(define hash-b (hash-block block-b))

(define block-c (make-block 'node-c (string->utf8 "C") (vector hash-d hash-e)))
(define hash-c (hash-block block-c))

(define block-a (make-block 'node-a (string->utf8 "A") (vector hash-b hash-c)))
(define hash-a (hash-block block-a))

(for-each (lambda (b) (store-put! fs b))
          (list block-a block-b block-c block-d block-e))

(printf "Test blocks created and stored: ~a blocks

" (store-count fs))

;;; ============================================================
;;; BFS Traversal
;;; ============================================================

(printf "BFS traversal:
")
(printf "----------------------------------------------------------------
")

(define bfs-tags (collect-visit-tags bfs-traverse fs hash-a))
(test "bfs-traverse order"
      '(node-a node-b node-c node-d node-e)
      bfs-tags)
(test "bfs-traverse visits each node once"
      (length (list-unique bfs-tags))
      (length bfs-tags))

(define missing-block (make-block 'missing (string->utf8 "missing") (vector)))
(define missing-hash (hash-block missing-block))
(define bfs-missing (collect-visit-tags bfs-traverse fs missing-hash))
(test "bfs-traverse skips missing start hash"
      '()
      bfs-missing)

(printf "
")

;;; ============================================================
;;; DFS Traversal
;;; ============================================================

(printf "DFS traversal:
")
(printf "----------------------------------------------------------------
")

(define dfs-tags (collect-visit-tags dfs-traverse fs hash-a))
(test "dfs-traverse order"
      '(node-a node-b node-d node-e node-c)
      dfs-tags)
(test "dfs-traverse visits each node once"
      (length (list-unique dfs-tags))
      (length dfs-tags))

(printf "
")

;;; ============================================================
;;; Reverse BFS Traversal
;;; ============================================================

(printf "Reverse BFS traversal:
")
(printf "----------------------------------------------------------------
")

(define reverse-tags (collect-visit-tags bfs-traverse-reverse fs hash-d))
(test-true "bfs-traverse-reverse reaches upstream nodes"
           (same-set? reverse-tags '(node-a node-b node-c node-d)))
(test-false "bfs-traverse-reverse does not include unrelated nodes"
            (memq 'node-e reverse-tags))

(printf "
")

;;; ============================================================
;;; Cleanup
;;; ============================================================

(printf "Cleaning up test store...
")
(system (format "rm -rf ~a" test-store-path))
(printf "Cleanup complete.

")

;;; ============================================================
;;; Summary
;;; ============================================================

(printf "================================================================
")
(printf "                    TEST RESULTS                               
")
(printf "================================================================

")

(printf "Tests passed: ~a
" tests-passed)
(printf "Tests failed: ~a
" tests-failed)
(printf "Total tests:  ~a
" (+ tests-passed tests-failed))

(if (= tests-failed 0)
    (printf "
[SUCCESS] All traversal tests passed.

")
    (printf "
[FAILURE] Some traversal tests failed.

"))
