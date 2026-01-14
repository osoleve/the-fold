;;; core/query/test-query.ss --- Tests for query.ss
;;;
;;; Tests the tag query functions with focus on handling missing 'body' fields.
;;;
;;; Run with: scheme --script core/query/test-query.ss

(load "lattice/query/patterns-parse.ss")
(load "lattice/query/query.ss")

;;; ====
;;; Test Framework
;;; ====

(define *tests-run* 0)
(define *tests-passed* 0)
(define *tests-failed* 0)

(define (test name expected actual)
  (set! *tests-run* (+ *tests-run* 1))
  (if (equal? expected actual)
      (begin
       (set! *tests-passed* (+ *tests-passed* 1))
       (display (format "  PASS: ~a\n" name)))
      (begin
       (set! *tests-failed* (+ *tests-failed* 1))
       (display (format "  FAIL: ~a\n" name))
       (display (format "    Expected: ~s\n" expected))
       (display (format "    Actual:   ~s\n" actual)))))

(define (test-summary)
  (display (format "\n~a tests: ~a passed, ~a failed\n"
                   *tests-run* *tests-passed* *tests-failed*))
  (if (= *tests-failed* 0)
      (display "All tests passed!\n")
      (display "SOME TESTS FAILED\n")))

;;; ====
;;; Test Helpers
;;; ====

;;; Mock list-channels and collect-channel for testing
(define test-channels '(test-channel))

(define (list-channels fs)
  test-channels)

(define test-posts '())

(define (collect-channel fs channel)
  test-posts)

;;; Dummy fs (not used directly in our tests)
(define (make-test-fs)
  'dummy-fs)

;;; ====
;;; Test: safe-get-body
;;; ====

(display "\n=== safe-get-body Tests ===\n\n")

(test "returns body when present"
      "Hello world"
      (safe-get-body '((body . "Hello world"))))

(test "returns empty string when body is missing"
      ""
      (safe-get-body '((title . "No body"))))

(test "returns empty string for empty post"
      ""
      (safe-get-body '()))

;;; ====
;;; Test: find-tagged with missing body
;;; ====

(display "\n=== find-tagged Tests ===\n\n")

;; Setup test posts with various cases
(set! test-posts
      (list
       '((title . "Normal post")
         (body . "This has @status:complete tag"))
       '((title . "Missing body post"))
       '((title . "Another normal")
         (body . "Has @status:pending"))
       '((title . "Empty body")
         (body . ""))
       '((title . "Also missing body"))))

(test "finds posts with tag, skips posts without body"
      1
      (length (find-tagged (make-test-fs) 'status "complete")))

(test "correct post found"
      "Normal post"
      (cdr (assq 'title (car (find-tagged (make-test-fs) 'status "complete")))))

;; Test with all posts missing body
(set! test-posts (list '((title . "No body 1"))
                       '((title . "No body 2"))))

(test "handles all posts missing body gracefully"
      0
      (length (find-tagged (make-test-fs) 'status "complete")))

;; Test finding with any value
(set! test-posts
      (list
       '((body . "@status:complete"))
       '((body . "@status:pending"))
       '((title . "No body"))))

(test "finds posts with any value for tag"
      2
      (length (find-tagged (make-test-fs) 'status #t)))

;;; ====
;;; Test: list-all-tags with missing body
;;; ====

(display "\n=== list-all-tags Tests ===\n\n")

(set! test-posts
      (list
       '((body . "@status:complete @priority:high"))
       '((title . "Missing body"))
       '((body . "@category:bug"))
       '((title . "Also missing"))))

(test "lists all tags, skips posts without body"
      3
      (length (list-all-tags (make-test-fs))))

(test "contains status tag"
      #t
      (if (member 'status (list-all-tags (make-test-fs))) #t #f))

(test "contains priority tag"
      #t
      (if (member 'priority (list-all-tags (make-test-fs))) #t #f))

(test "contains category tag"
      #t
      (if (member 'category (list-all-tags (make-test-fs))) #t #f))

;; Test all posts without body
(set! test-posts (list '((title . "No body"))))

(test "handles all posts without body"
      0
      (length (list-all-tags (make-test-fs))))

;;; ====
;;; Test: tag-histogram with missing body
;;; ====

(display "\n=== tag-histogram Tests ===\n\n")

(set! test-posts
      (list
       '((body . "@status:complete"))
       '((title . "Missing body"))
       '((body . "@status:pending"))
       '((body . "@priority:high"))
       '((title . "Also missing"))))

(let ([hist (tag-histogram (make-test-fs))])
     (test "histogram has 2 entries"
           2
           (length hist))
     (test "status count is 2"
           2
           (cdr (assq 'status hist)))
     (test "priority count is 1"
           1
           (cdr (assq 'priority hist))))

;;; ====
;;; Test: tag-values with missing body
;;; ====

(display "\n=== tag-values Tests ===\n\n")

(set! test-posts
      (list
       '((body . "@status:complete"))
       '((title . "Missing body"))
       '((body . "@status:pending"))
       '((body . "@status:complete"))
       '((title . "Also missing"))))

(let ([values (tag-values (make-test-fs) 'status)])
     (test "gets 2 unique values"
           2
           (length values))
     (test "contains complete"
           #t
           (if (member "complete" values) #t #f))
     (test "contains pending"
           #t
           (if (member "pending" values) #t #f)))

;;; ====
;;; Test: tag-value-histogram with missing body
;;; ====

(display "\n=== tag-value-histogram Tests ===\n\n")

(set! test-posts
      (list
       '((body . "@status:complete"))
       '((title . "Missing body"))
       '((body . "@status:pending"))
       '((body . "@status:complete"))
       '((title . "Also missing"))))

(let ([hist (tag-value-histogram (make-test-fs) 'status)])
     (test "histogram has 2 entries"
           2
           (length hist))
     (test "complete count is 2"
           2
           (cdr (assoc "complete" hist)))
     (test "pending count is 1"
           1
           (cdr (assoc "pending" hist))))

;;; ====
;;; Test Summary
;;; ====

(test-summary)
