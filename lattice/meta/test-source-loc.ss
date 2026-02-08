;;; lattice/meta/test-source-loc.ss — Tests for Source Location Tracking
;;;
;;; Tests the source location extraction and formatting.
;;;
;;; Dependencies:
;;;   - core/testing/test-framework.ss
;;;   - lattice/meta/source-loc.ss

(load "core/testing/test-framework.ss")
(load "boundary/meta/source-loc-io.ss")

;;; ====
;;; Helper Tests
;;; ====

(test-group "string-helpers"

  (define-test "string-trim-left-loc removes leading whitespace"
    (assert-equal "hello" (string-trim-left-loc "  hello"))
    (assert-equal "world" (string-trim-left-loc "\t\n world")))

  (define-test "string-trim-left-loc handles empty string"
    (assert-equal "" (string-trim-left-loc ""))
    (assert-equal "" (string-trim-left-loc "   ")))

  (define-test "string-starts-with? works correctly"
    (assert-true (string-starts-with? "test-foo.ss" "test-"))
    (assert-false (string-starts-with? "foo.ss" "test-"))
    (assert-true (string-starts-with? "anything" ""))))

;;; ====
;;; Name Extraction Tests
;;; ====

(test-group "name-extraction"

  (define-test "extract simple define"
    (assert-equal 'foo (try-extract-define-name "(define foo 42)")))

  (define-test "extract function define"
    (assert-equal 'bar (try-extract-define-name "(define (bar x y) (+ x y))")))

  (define-test "extract with leading whitespace"
    (assert-equal 'baz (try-extract-define-name "  (define baz 10)")))

  (define-test "extract define-syntax"
    (assert-equal 'my-macro (try-extract-define-name "(define-syntax my-macro")))

  (define-test "extract define-record-type"
    (assert-equal 'point (try-extract-define-name "(define-record-type point")))

  (define-test "skip define-test"
    (assert-false (try-extract-define-name "(define-test \"my test\"")))

  (define-test "return #f for non-define"
    (assert-false (try-extract-define-name "(let ([x 1]) x)"))
    (assert-false (try-extract-define-name "; just a comment"))))

;;; ====
;;; File Parsing Tests
;;; ====

(test-group "file-parsing"

  (define-test "extract-first-sym handles simple symbols"
    (assert-equal 'hello (extract-first-sym "hello world"))
    (assert-equal 'foo (extract-first-sym "foo)")))

  (define-test "extract-first-sym handles empty/invalid"
    (assert-false (extract-first-sym ""))
    (assert-false (extract-first-sym ")invalid"))))

;;; ====
;;; Integration Tests (require cache to be built)
;;; ====

(test-group "source-location-cache"

  ;; Build the cache first
  (build-source-location-cache!)

  (define-test "cache has entries"
    (assert-true (> (source-location-count) 0)))

  (define-test "can look up known function"
    (let ([loc (get-source-location 'get-source-location)])
         (assert-true (pair? loc))
         (assert-true (string? (car loc)))
         (assert-true (integer? (cdr loc)))))

  (define-test "format-source-location returns string"
    (let ([formatted (format-source-location 'get-source-location)])
         (assert-true (or (not formatted) (string? formatted)))
         (when formatted
               ;; Should contain colon separator
               (assert-true (string-contains formatted ":")))))

  (define-test "unknown symbol returns #f"
    (assert-false (get-source-location 'this-symbol-does-not-exist-xyz123)))

  (define-test "format unknown symbol returns #f"
    (assert-false (format-source-location 'another-nonexistent-symbol-abc))))

;;; Helper: check if string contains substring
(define (string-contains str substr)
  (let ([slen (string-length str)]
        [plen (string-length substr)])
       (if (> plen slen)
           #f
           (let loop ([i 0])
                (if (> (+ i plen) slen)
                    #f
                    (if (string=? (substring str i (+ i plen)) substr)
                        #t
                        (loop (+ i 1))))))))

;;; ====
;;; Run All Tests
;;; ====

(run-all-tests)
