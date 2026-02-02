;;; Test suite for flashmob/llm-review.ss
;;; Run with: scheme --script boundary/flashmob/test-llm-review.ss

(load "core/testing/test-framework.ss")
(load "boundary/flashmob/llm-review.ss")

(test-group "llm-review parsing"

  (define-test "llm-review-extract-nemotron-response extracts content from valid response"
    (let ([response "{\"choices\":[{\"message\":{\"content\":\"42|high|correctness|Test|Desc\"}}]}"])
      (assert-equal "42|high|correctness|Test|Desc"
                    (llm-review-extract-nemotron-response response))))

  (define-test "llm-review-extract-nemotron-response handles empty content with reasoning fallback"
    (let ([response "{\"choices\":[{\"message\":{\"content\":\"\",\"reasoning_content\":\"10|low|style|Note|Info\"}}]}"])
      (assert-equal "10|low|style|Note|Info"
                    (llm-review-extract-nemotron-response response))))

  (define-test "llm-review-extract-nemotron-response returns raw json on parse failure"
    (let ([bad-json "not valid json"])
      (assert-equal "not valid json"
                    (llm-review-extract-nemotron-response bad-json))))

  (define-test "llm-review-parse-findings parses valid finding lines"
    (let* ([response "42|high|correctness|Test finding|Some description\n100|low|style|Style issue|Minor"]
           [findings (llm-review-parse-findings response "/test/file.ss")])
      (assert-equal 2 (length findings))
      (assert-equal 42 (cdr (assq 'line (car findings))))
      (assert-equal 'high (cdr (assq 'severity (car findings))))
      (assert-equal 'correctness (cdr (assq 'category (car findings))))))

  (define-test "llm-review-parse-findings ignores NO_ISSUES_FOUND"
    (let ([findings (llm-review-parse-findings "NO_ISSUES_FOUND" "/test/file.ss")])
      (assert-equal 0 (length findings))))

  (define-test "llm-review-parse-findings ignores comment lines"
    (let* ([response "# This is a comment\n42|high|correctness|Real finding|Desc"]
           [findings (llm-review-parse-findings response "/test/file.ss")])
      (assert-equal 1 (length findings))))

  (define-test "llm-review-parse-findings handles malformed lines gracefully"
    (let* ([response "not a valid line\n42|high|correctness|Real|Desc\nalso bad"]
           [findings (llm-review-parse-findings response "/test/file.ss")])
      (assert-equal 1 (length findings)))))

(test-group "llm-review utilities"

  (define-test "llm-json-escape handles special characters"
    (let ([escaped (llm-json-escape "hello\nworld\"test")])
      (assert-true (string? escaped))
      ;; Should contain escaped newline and quote
      (assert-true (> (string-length escaped) 16))))

  (define-test "llm-json-unescape reverses escaping"
    (let ([original "hello\\nworld\\\"test"]
          [unescaped (llm-json-unescape "hello\\nworld\\\"test")])
      (assert-equal #\newline (string-ref unescaped 5))
      (assert-equal #\" (string-ref unescaped 11))))

  (define-test "llm-string-split splits on delimiter"
    (let ([parts (llm-string-split "a|b|c" #\|)])
      (assert-equal 3 (length parts))
      (assert-equal "a" (car parts))
      (assert-equal "b" (cadr parts))
      (assert-equal "c" (caddr parts))))

  (define-test "llm-string-join joins with delimiter"
    (let ([joined (llm-string-join '("a" "b" "c") "|")])
      (assert-equal "a|b|c" joined)))

  (define-test "llm-string-prefix? detects prefixes"
    (assert-true (llm-string-prefix? "#comment" "#"))
    (assert-false (llm-string-prefix? "not a comment" "#")))

  (define-test "llm-filter-map filters and maps"
    (let ([result (llm-filter-map
                   (lambda (x) (and (> x 5) (* x 2)))
                   '(1 3 6 8 2 10))])
      (assert-equal '(12 16 20) result))))

(test-group "llm-review configuration"

  (define-test "*llm-server-port* is configurable"
    (assert-equal 30000 *llm-server-port*)
    ;; Can be changed
    (set! *llm-server-port* 8080)
    (assert-equal 8080 *llm-server-port*)
    ;; Restore
    (set! *llm-server-port* 30000))

  (define-test "*llm-review-temperature* has sensible default"
    (assert-true (and (> *llm-review-temperature* 0)
                      (< *llm-review-temperature* 1)))))

(run-all-tests)
