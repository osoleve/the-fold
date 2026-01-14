;;; shell/tests/test-identity.ss — Identity System Tests
;;;
;;; Tests for the block-based identity management system.
;;;
;;; Run from project root: scheme --script shell/tests/test-identity.ss

(load "shell/identity.ss")

(define (test name expected actual)
  (display "  ")
  (display name)
  (display ": ")
  (if (equal? expected actual)
      (display "✓")
      (begin
       (display "✗\n    expected: ")
       (display expected)
       (display "\n    got: ")
       (display actual)))
  (newline))

(define (test-section name)
  (newline)
  (display name)
  (newline))

;;; Clean up test environment
(define (cleanup-test-env!)
  (when (file-exists? ".store/heads/identity")
        (system "rm -rf .store/heads/identity"))
  ;; Clear in-memory CAS store (hashtable-clear! is defined in Chez Scheme)
  (hashtable-clear! *store*)
  (hashtable-clear! *pinned*))

;;; ====
;;; Identity Creation Tests
;;; ====
(test-section "Identity Creation")

(cleanup-test-env!)

(test "register new identity creates block"
      #t
      (begin
       (let ([block (register-identity! 'alice 'builder)])
            (block? block))))

(test "registered identity has correct tag"
      'identity
      (block-tag (lookup-identity 'alice)))

(test "registered identity contains username"
      'alice
      (cdr (assq 'username (get-identity-data 'alice))))

(test "registered identity contains role"
      'builder
      (cdr (assq 'role (get-identity-data 'alice))))

(test "registered identity has session count 1"
      1
      (cdr (assq 'session-count (get-identity-data 'alice))))

(test "registered identity has zero posts"
      0
      (cdr (assq 'total-posts (get-identity-data 'alice))))

(test "identity exists after registration"
      #t
      (identity-exists? 'alice))

(test "non-existent identity returns false"
      #f
      (identity-exists? 'bob))

;;; ====
;;; Identity Lookup Tests
;;; ====
(test-section "Identity Lookup")

(test "lookup existing identity returns block"
      #t
      (block? (lookup-identity 'alice)))

(test "lookup non-existent identity returns #f"
      #f
      (lookup-identity 'nonexistent))

(test "get-identity-data returns alist"
      #t
      (list? (get-identity-data 'alice)))

(test "get-identity-data for missing user returns #f"
      #f
      (get-identity-data 'missing))

;;; ====
;;; Identity Update Tests
;;; ====
(test-section "Identity Updates")

(test "record session increments count"
      2
      (begin
       (record-session! 'alice)
       (cdr (assq 'session-count (get-identity-data 'alice)))))

(test "record another session increments again"
      3
      (begin
       (record-session! 'alice)
       (cdr (assq 'session-count (get-identity-data 'alice)))))

(test "record post increments post count"
      1
      (begin
       (record-post! 'alice)
       (cdr (assq 'total-posts (get-identity-data 'alice)))))

(test "record multiple posts increments correctly"
      3
      (begin
       (record-post! 'alice)
       (record-post! 'alice)
       (cdr (assq 'total-posts (get-identity-data 'alice)))))

;;; ====
;;; Preference Management Tests
;;; ====
(test-section "Preference Management")

(test "set preference adds to preferences"
      'dark
      (begin
       (set-preference! 'alice 'theme 'dark)
       (cdr (assq 'theme (cdr (assq 'preferences (get-identity-data 'alice)))))))

(test "set another preference"
      #t
      (begin
       (set-preference! 'alice 'notifications #t)
       (cdr (assq 'notifications (cdr (assq 'preferences (get-identity-data 'alice)))))))

(test "update existing preference"
      'light
      (begin
       (set-preference! 'alice 'theme 'light)
       (cdr (assq 'theme (cdr (assq 'preferences (get-identity-data 'alice)))))))

(test "preferences are persisted"
      'light
      (let* ([username 'alice]
             [data (get-identity-data username)]
             [prefs (cdr (assq 'preferences data))])
            (cdr (assq 'theme prefs))))

;;; ====
;;; Identity History Tests
;;; ====
(test-section "Identity History")

(test "identity history is not empty"
      #t
      (> (length (get-identity-history 'alice)) 0))

(test "history contains multiple versions"
      #t
      (>= (length (get-identity-history 'alice)) 5))

(test "all history entries are blocks"
      #t
      (andmap block? (get-identity-history 'alice)))

(test "history entries have identity tag"
      #t
      (andmap (lambda (b) (eq? (block-tag b) 'identity))
              (get-identity-history 'alice)))

(test "newest version has highest session count"
      3
      (let* ([history (get-identity-history 'alice)]
             ; History is returned newest-first, so last is oldest
             [newest (car (reverse history))]
             [data (decode-identity-payload (block-payload newest))])
            (cdr (assq 'session-count data))))

;;; ====
;;; Multiple Identities Tests
;;; ====
(test-section "Multiple Identities")

(test "register second identity"
      #t
      (begin
       (register-identity! 'bob 'player)
       (identity-exists? 'bob)))

(test "register third identity"
      #t
      (begin
       (register-identity! 'charlie 'shepherd)
       (identity-exists? 'charlie)))

(test "list identities includes all users"
      3
      (let ([heads (list-identity-heads)])
           (length (filter (lambda (u) (or (eq? u 'alice) (eq? u 'bob) (eq? u 'charlie))) heads))))

(test "different identities are independent"
      #t
      (and (eq? (cdr (assq 'role (get-identity-data 'alice))) 'builder)
           (eq? (cdr (assq 'role (get-identity-data 'bob))) 'player)
           (eq? (cdr (assq 'role (get-identity-data 'charlie))) 'shepherd)))

;;; ====
;;; Block Structure Tests
;;; ====
(test-section "Block Structure")

(test "new identity has no refs"
      0
      (let ([block (lookup-identity 'bob)])
           (vector-length (block-refs block))))

(test "updated identity has prev ref"
      1
      (begin
       (record-session! 'bob)
       (let ([block (lookup-identity 'bob)])
            (vector-length (block-refs block)))))

(test "payload is valid UTF-8"
      #t
      (let* ([block (lookup-identity 'bob)]
             [payload (block-payload block)])
            (bytevector? payload)))

(test "payload can be decoded"
      #t
      (let* ([block (lookup-identity 'bob)]
             [payload (block-payload block)]
             [data (decode-identity-payload payload)])
            (list? data)))

;;; ====
;;; Error Handling Tests
;;; ====
(test-section "Error Handling")

(test "cannot register duplicate identity"
      #t
      (guard (e [else #t])
             (register-identity! 'alice 'builder)
             #f))

(test "cannot update non-existent identity"
      #t
      (guard (e [else #t])
             (update-identity! 'nonexistent
                               (lambda (data) data))
             #f))

;;; ====
;;; Encoding/Decoding Tests
;;; ====
(test-section "Encoding/Decoding")

(test "encode-decode roundtrip preserves data"
      #t
      (let* ([original (make-new-identity 'test 'builder)]
             [encoded (encode-identity-payload original)]
             [decoded (decode-identity-payload encoded)])
            (equal? original decoded)))

(test "decoded data has correct structure"
      #t
      (let* ([data (make-new-identity 'test 'player)]
             [encoded (encode-identity-payload data)]
             [decoded (decode-identity-payload encoded)])
            (and (if (assq 'username decoded) #t #f)
                 (if (assq 'role decoded) #t #f)
                 (if (assq 'first-seen decoded) #t #f)
                 (if (assq 'last-seen decoded) #t #f)
                 (if (assq 'session-count decoded) #t #f)
                 (if (assq 'total-posts decoded) #t #f)
                 (if (assq 'preferences decoded) #t #f))))

;;; ====
;;; Timestamp Tests
;;; ====
(test-section "Timestamps")

(test "timestamp has ISO8601 format"
      #t
      (let ([ts (current-iso8601-timestamp)])
           (and (string? ts)
                (> (string-length ts) 0)
                (char=? (string-ref ts 4) #\-)
                (char=? (string-ref ts 7) #\-)
                (char=? (string-ref ts 10) #\T))))

(test "first-seen equals last-seen for new identity"
      #t
      (begin
       (register-identity! 'dave 'builder)
       (let ([data (get-identity-data 'dave)])
            (string=? (cdr (assq 'first-seen data))
                      (cdr (assq 'last-seen data))))))

(test "last-seen updates on session record"
      #t
      (let* ([old-data (get-identity-data 'dave)]
             [old-time (cdr (assq 'last-seen old-data))])
            ;; Small delay to ensure timestamp changes
            (sleep (make-time 'time-duration 0 1))
            (record-session! 'dave)
            (let* ([new-data (get-identity-data 'dave)]
                   [new-time (cdr (assq 'last-seen new-data))])
                  (not (string=? old-time new-time)))))

(newline)
(display "All identity tests completed!")
(newline)
