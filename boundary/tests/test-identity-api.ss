;;; boundary/tests/test-identity-api.ss — Identity API Tests (fold-n5z)
;;;
;;; Tests for the public identity API:
;;;   - identity-exists?
;;;   - identity-get
;;;   - identity-create!
;;;   - identity-update!
;;;
;;; Run from project root: scheme --script boundary/tests/test-identity-api.ss

(load "boundary/identity.ss")

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
  (hashtable-clear! *store*)
  (hashtable-clear! *pinned*))

;;; ====
;;; Validation Tests
;;; ====
(test-section "Validation")

(test "valid-role? accepts shepherd"
      #t
      (valid-role? 'shepherd))

(test "valid-role? accepts builder"
      #t
      (valid-role? 'builder))

(test "valid-role? accepts player"
      #t
      (valid-role? 'player))

(test "valid-role? rejects invalid role"
      #f
      (valid-role? 'admin))

(test "validate-username! accepts symbol"
      #t
      (guard (e [else #f])
             (validate-username! 'alice)
             #t))

(test "validate-username! rejects string"
      #t
      (guard (e [else #t])
             (validate-username! "alice")
             #f))

(test "validate-role! accepts valid role"
      #t
      (guard (e [else #f])
             (validate-role! 'builder)
             #t))

(test "validate-role! rejects invalid role"
      #t
      (guard (e [else #t])
             (validate-role! 'invalid)
             #f))

;;; ====
;;; identity-exists? Tests
;;; ====
(test-section "identity-exists?")

(cleanup-test-env!)

(test "non-existent user returns false"
      #f
      (identity-exists? 'alice))

(test "create user and check exists"
      #t
      (begin
       (identity-create! 'alice 'builder)
       (identity-exists? 'alice)))

(test "different user still doesn't exist"
      #f
      (identity-exists? 'bob))

;;; ====
;;; identity-create! Tests
;;; ====
(test-section "identity-create!")

(test "create new identity returns alist"
      #t
      (begin
       (let ([result (identity-create! 'bob 'player)])
            (list? result))))

(test "created identity has username"
      'bob
      (cdr (assq 'username (identity-get 'bob))))

(test "created identity has role"
      'player
      (cdr (assq 'role (identity-get 'bob))))

(test "created identity has session-count 1"
      1
      (cdr (assq 'session-count (identity-get 'bob))))

(test "cannot create duplicate username"
      #t
      (guard (e [else #t])
             (identity-create! 'alice 'shepherd)
             #f))

(test "validate username must be symbol"
      #t
      (guard (e [else #t])
             (identity-create! "charlie" 'builder)
             #f))

(test "validate role must be valid"
      #t
      (guard (e [else #t])
             (identity-create! 'charlie 'admin)
             #f))

;;; ====
;;; identity-get Tests
;;; ====
(test-section "identity-get")

(test "get existing identity returns alist"
      #t
      (list? (identity-get 'alice)))

(test "get non-existent identity returns #f"
      #f
      (identity-get 'nonexistent))

(test "get returns correct username"
      'alice
      (cdr (assq 'username (identity-get 'alice))))

(test "get returns correct role"
      'builder
      (cdr (assq 'role (identity-get 'alice))))

(test "get includes all fields"
      #t
      (let ([data (identity-get 'alice)])
           (and (if (assq 'username data) #t #f)
                (if (assq 'role data) #t #f)
                (if (assq 'first-seen data) #t #f)
                (if (assq 'last-seen data) #t #f)
                (if (assq 'session-count data) #t #f)
                (if (assq 'total-posts data) #t #f)
                (if (assq 'preferences data) #t #f))))

;;; ====
;;; identity-update! Tests
;;; ====
(test-section "identity-update!")

(test "update single field"
      42
      (begin
       (identity-update! 'alice (list (cons 'total-posts 42)))
       (cdr (assq 'total-posts (identity-get 'alice)))))

(test "update multiple fields"
      #t
      (begin
       (identity-update! 'bob (list (cons 'total-posts 10)
                                    (cons 'session-count 5)))
       (and (= (cdr (assq 'total-posts (identity-get 'bob))) 10)
            (= (cdr (assq 'session-count (identity-get 'bob))) 5))))

(test "update preserves other fields"
      'bob
      (begin
       (identity-update! 'bob (list (cons 'total-posts 20)))
       (cdr (assq 'username (identity-get 'bob)))))

(test "update non-existent user fails"
      #t
      (guard (e [else #t])
             (identity-update! 'nonexistent (list (cons 'total-posts 1)))
             #f))

(test "update with invalid username fails"
      #t
      (guard (e [else #t])
             (identity-update! "alice" (list (cons 'total-posts 1)))
             #f))

(test "update with non-list fails"
      #t
      (guard (e [else #t])
             (identity-update! 'alice 'not-a-list)
             #f))

;;; ====
;;; Registration Flow Tests
;;; ====
(test-section "Registration Flow")

(test "check username not taken"
      #f
      (identity-exists? 'charlie))

(test "create new user"
      #t
      (begin
       (let ([result (identity-create! 'charlie 'shepherd)])
            (and (list? result)
                 (eq? (cdr (assq 'username result)) 'charlie)))))

(test "username now taken"
      #t
      (identity-exists? 'charlie))

(test "get returns created user"
      'charlie
      (cdr (assq 'username (identity-get 'charlie))))

(test "attempting duplicate fails gracefully"
      #t
      (guard (e [else #t])
             (identity-create! 'charlie 'builder)
             #f))

;;; ====
;;; API Contract Tests
;;; ====
(test-section "API Contracts")

(test "identity-get returns alist or #f"
      #t
      (let ([result (identity-get 'alice)])
           (or (not result) (list? result))))

(test "identity-create! returns alist"
      #t
      (begin
       (identity-create! 'dave 'player)
       (list? (identity-get 'dave))))

(test "identity-update! returns alist"
      #t
      (list? (identity-update! 'dave (list (cons 'total-posts 5)))))

(test "all operations preserve identity structure"
      #t
      (let ([data (identity-get 'dave)])
           (and (assq 'username data)
                (assq 'role data)
                (assq 'session-count data)
                (assq 'total-posts data)
                (eq? (cdr (assq 'total-posts data)) 5))))

;;; ====
;;; Edge Cases
;;; ====
(test-section "Edge Cases")

(test "empty updates list does nothing"
      #t
      (begin
       (let ([before (identity-get 'alice)]
             [after (identity-update! 'alice '())])
            ;; Username should be preserved
            (eq? (cdr (assq 'username before))
                 (cdr (assq 'username after))))))

(test "update with same value succeeds"
      'builder
      (begin
       (identity-update! 'alice (list (cons 'role 'builder)))
       (cdr (assq 'role (identity-get 'alice)))))

(test "update creates new version in history"
      #t
      (let* ([before-count (length (get-identity-history 'alice))]
             [_ (identity-update! 'alice (list (cons 'total-posts 99)))]
             [after-count (length (get-identity-history 'alice))])
            (> after-count before-count)))

(newline)
(display "All identity API tests completed!")
(newline)
