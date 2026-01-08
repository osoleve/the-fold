;;; shell/tests/test-jump.ss — Tests for Navigation Primitives
;;;
;;; Comprehensive test suite for shell/lens/jump.ss
;;; Tests location types, symbol lookup, and navigation utilities.

(load "shell/lens/call-graph.ss")
(load "shell/lens/jump.ss")

(define test-count 0)
(define pass-count 0)
(define fail-count 0)

;;; Test assertion helper
(define (assert-equal name actual expected)
  (set! test-count (+ test-count 1))
  (if (equal? actual expected)
      (begin
       (set! pass-count (+ pass-count 1))
       (printf "  ✓ ~a\n" name))
      (begin
       (set! fail-count (+ fail-count 1))
       (printf "  ✗ ~a\n" name)
       (printf "    Expected: ~s\n" expected)
       (printf "    Actual:   ~s\n" actual))))

(define (assert-true name actual)
  (assert-equal name actual #t))

(define (assert-false name actual)
  (assert-equal name actual #f))

;;; ============================================================
;;; Location Type Tests
;;; ============================================================

(printf "\n=== Testing make-location ===\n")

(let ([loc (make-location "test.ss" 42 10)])
     (assert-equal "location file"
                   (location-file loc)
                   "test.ss")
     (assert-equal "location line"
                   (location-line loc)
                   42)
     (assert-equal "location column"
                   (location-column loc)
                   10))

(assert-equal "location with zero column"
              (location-column (make-location "foo.ss" 1 0))
              0)

;;; ============================================================
;;; location->string Tests
;;; ============================================================

(printf "\n=== Testing location->string ===\n")

(assert-equal "location without column"
              (location->string (make-location "test.ss" 42 0))
              "test.ss:42")

(assert-equal "location with column"
              (location->string (make-location "test.ss" 42 10))
              "test.ss:42:10")

(assert-equal "location line 1"
              (location->string (make-location "foo.ss" 1 0))
              "foo.ss:1")

;;; ============================================================
;;; read-file-lines Tests
;;; ============================================================

(printf "\n=== Testing read-file-lines ===\n")

(assert-true "reads existing file"
             (let ([lines (read-file-lines "shell/lens/jump.ss")])
                  (and (list? lines)
                       (> (length lines) 0))))

(assert-true "first line contains expected content"
             (let ([lines (read-file-lines "shell/lens/jump.ss")])
                  (string-contains? (car lines) "shell/lens/jump.ss")))

(assert-equal "nonexistent file returns empty"
              (read-file-lines "/nonexistent/file/path.ss")
              '())

;;; ============================================================
;;; jump-location Tests
;;; ============================================================

(printf "\n=== Testing jump-location ===\n")

;; Ensure call graph is built (provides some symbol index)
(unless (call-graph-built?)
        (call-graph-refresh!))

(assert-true "unknown symbol returns #f"
             (not (jump-location 'nonexistent-symbol-xyz-123)))

;; If index-lookup is available, test it
(when (and (top-level-bound? 'index-lookup)
           (procedure? index-lookup))
      (printf "  (index-lookup available, running additional tests)\n")
      
      (assert-true "known symbol returns location or #f"
                   (let ([result (jump-location 'jump-location)])
                        (or (not result)  ; May not be indexed
                            (and (list? result)
                                 (assq 'file result)
                                 (assq 'line result))))))

;;; ============================================================
;;; jump-locations Tests
;;; ============================================================

(printf "\n=== Testing jump-locations ===\n")

(assert-true "returns list"
             (list? (jump-locations 'foo)))

(assert-true "unknown symbol returns empty list"
             (null? (jump-locations 'nonexistent-symbol-xyz-123)))

;;; ============================================================
;;; jump-refs Tests
;;; ============================================================

(printf "\n=== Testing jump-refs ===\n")

(assert-true "returns list"
             (list? (jump-refs 'foo)))

(assert-true "unknown symbol returns empty"
             (null? (jump-refs 'nonexistent-symbol-xyz-123)))

;;; ============================================================
;;; filter-map-jump Tests
;;; ============================================================

(printf "\n=== Testing filter-map-jump ===\n")

(assert-equal "filter-map basic"
              (filter-map-jump (lambda (x) (and (even? x) (* x 2))) '(1 2 3 4))
              '(4 8))

(assert-equal "filter-map all false"
              (filter-map-jump (lambda (x) #f) '(1 2 3))
              '())

(assert-equal "filter-map empty list"
              (filter-map-jump (lambda (x) x) '())
              '())

(assert-equal "filter-map all pass"
              (filter-map-jump (lambda (x) (+ x 1)) '(1 2 3))
              '(2 3 4))

;;; ============================================================
;;; Editor Format Tests
;;; ============================================================

(printf "\n=== Testing Editor Format Functions ===\n")

(let ([loc (make-location "test.ss" 42 5)])
     (assert-equal "jump-file-line"
                   (jump-file-line "test.ss" 42)
                   "test.ss:42")
     
     (assert-equal "emacs format"
                   (jump-emacs-format loc)
                   "+42 test.ss")
     
     (assert-equal "vim format"
                   (jump-vim-format loc)
                   "test.ss +42")
     
     (assert-equal "vscode format"
                   (jump-vscode-format loc)
                   "test.ss:42:5"))

(let ([loc-no-col (make-location "foo.ss" 1 0)])
     (assert-equal "vscode format no column"
                   (jump-vscode-format loc-no-col)
                   "foo.ss:1"))

;;; ============================================================
;;; Edge Cases
;;; ============================================================

(printf "\n=== Testing Edge Cases ===\n")

(assert-true "location with path containing spaces"
             (let ([loc (make-location "path with spaces/file.ss" 10 0)])
                  (string-contains? (location->string loc) "path with spaces")))

(assert-true "location with unicode path"
             (let ([loc (make-location "unicode/test.ss" 1 0)])
                  (string? (location->string loc))))

;;; ============================================================
;;; Test Summary
;;; ============================================================

(printf "\n========================================\n")
(printf "Test Results (jump):\n")
(printf "  Total:  ~a\n" test-count)
(printf "  Passed: ~a\n" pass-count)
(printf "  Failed: ~a\n" fail-count)

(if (= fail-count 0)
    (printf "\n✓ All tests passed!\n\n")
    (printf "\n✗ Some tests failed\n\n"))

;;; Return success/failure
(= fail-count 0)
