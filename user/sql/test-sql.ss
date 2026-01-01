;;; playpen/sql/test-sql.ss — SQL DSL Tests
;;;
;;; Tests for the SQL parser, validator, and formatter.
;;;
;;; Usage:
;;;   scheme --script playpen/sql/test-sql.ss

;;; Set up source directories for loading
(let ([stitches-path (string-append (current-directory) "/fabric/stitches")])
     (unless (member stitches-path (source-directories))
             (source-directories (cons stitches-path (source-directories)))))

(load "user/sql/sql.ss")

;;; ============================================================
;;; Test Framework
;;; ============================================================

(define test-count 0)
(define pass-count 0)
(define fail-count 0)

(define (test name thunk)
  (set! test-count (+ test-count 1))
  (display "  ")
  (display name)
  (display " ... ")
  (guard (exn [else
               (set! fail-count (+ fail-count 1))
               (display "FAIL (exception: ")
               (display (if (condition? exn)
                            (condition-message exn)
                            exn))
               (display ")")
               (newline)])
         (let ([result (thunk)])
              (if result
                  (begin
                   (set! pass-count (+ pass-count 1))
                   (display "ok")
                   (newline))
                  (begin
                   (set! fail-count (+ fail-count 1))
                   (display "FAIL")
                   (newline))))))

(define (assert-parse sql)
  (let ([result (parse-sql sql)])
       (right? result)))

(define (assert-parse-fails sql)
  (let ([result (parse-sql sql)])
       (left? result)))

(define (assert-valid sql)
  (let ([result (check-sql sql)])
       (right? result)))

(define (assert-roundtrip sql)
  (let ([result (reformat-sql sql '((compact . #t)))])
       (and (right? result)
            (let ([reformatted (from-right result)])
                 ;; Just check we can reparse the output
                 (right? (parse-sql reformatted))))))

;;; ============================================================
;;; Parser Tests
;;; ============================================================

(display "=== SQL Parser Tests ===
")

(test "SELECT simple"
      (lambda () (assert-parse "SELECT * FROM users")))

(test "SELECT with columns"
      (lambda () (assert-parse "SELECT id, name, email FROM users")))

(test "SELECT with alias"
      (lambda () (assert-parse "SELECT name AS user_name FROM users")))

(test "SELECT with table alias"
      (lambda () (assert-parse "SELECT u.name FROM users AS u")))

(test "SELECT DISTINCT"
      (lambda () (assert-parse "SELECT DISTINCT status FROM orders")))

(test "SELECT with WHERE"
      (lambda () (assert-parse "SELECT * FROM users WHERE id = 1")))

(test "SELECT with AND/OR"
      (lambda () (assert-parse "SELECT * FROM users WHERE active = TRUE AND age > 18 OR admin = TRUE")))

(test "SELECT with LIKE"
      (lambda () (assert-parse "SELECT * FROM users WHERE name LIKE '%john%'")))

(test "SELECT with IN"
      (lambda () (assert-parse "SELECT * FROM users WHERE status IN ('active', 'pending')")))

(test "SELECT with BETWEEN"
      (lambda () (assert-parse "SELECT * FROM orders WHERE amount BETWEEN 100 AND 500")))

(test "SELECT with IS NULL"
      (lambda () (assert-parse "SELECT * FROM users WHERE deleted_at IS NULL")))

(test "SELECT with IS NOT NULL"
      (lambda () (assert-parse "SELECT * FROM users WHERE email IS NOT NULL")))

(test "SELECT with JOIN"
      (lambda () (assert-parse "SELECT * FROM users INNER JOIN orders ON users.id = orders.user_id")))

(test "SELECT with LEFT JOIN"
      (lambda () (assert-parse "SELECT * FROM users LEFT JOIN orders ON users.id = orders.user_id")))

(test "SELECT with GROUP BY"
      (lambda () (assert-parse "SELECT status, COUNT(*) FROM orders GROUP BY status")))

(test "SELECT with HAVING"
      (lambda () (assert-parse "SELECT status, COUNT(*) FROM orders GROUP BY status HAVING COUNT(*) > 5")))

(test "SELECT with ORDER BY"
      (lambda () (assert-parse "SELECT * FROM users ORDER BY created_at DESC")))

(test "SELECT with ORDER BY NULLS"
      (lambda () (assert-parse "SELECT * FROM users ORDER BY name ASC NULLS LAST")))

(test "SELECT with LIMIT"
      (lambda () (assert-parse "SELECT * FROM users LIMIT 10")))

(test "SELECT with LIMIT OFFSET"
      (lambda () (assert-parse "SELECT * FROM users LIMIT 10 OFFSET 20")))

(test "SELECT with CASE"
      (lambda () (assert-parse "SELECT CASE WHEN status = 1 THEN 'active' ELSE 'inactive' END FROM users")))

(test "SELECT with function"
      (lambda () (assert-parse "SELECT COUNT(id), SUM(amount) FROM orders")))

(test "SELECT with arithmetic"
      (lambda () (assert-parse "SELECT price * quantity AS total FROM items")))

(test "SELECT with subquery"
      (lambda () (assert-parse "SELECT * FROM users WHERE id IN (SELECT user_id FROM orders)")))

(test "SELECT with EXISTS"
      (lambda () (assert-parse "SELECT * FROM users WHERE EXISTS (SELECT 1 FROM orders WHERE orders.user_id = users.id)")))

(test "INSERT simple"
      (lambda () (assert-parse "INSERT INTO users VALUES (1, 'john', 'john@example.com')")))

(test "INSERT with columns"
      (lambda () (assert-parse "INSERT INTO users (name, email) VALUES ('john', 'john@example.com')")))

(test "INSERT multiple rows"
      (lambda () (assert-parse "INSERT INTO users (name) VALUES ('john'), ('jane'), ('bob')")))

(test "INSERT with SELECT"
      (lambda () (assert-parse "INSERT INTO archive SELECT * FROM users WHERE deleted = TRUE")))

(test "INSERT with DEFAULT"
      (lambda () (assert-parse "INSERT INTO users (name, created_at) VALUES ('john', DEFAULT)")))

(test "UPDATE simple"
      (lambda () (assert-parse "UPDATE users SET name = 'john'")))

(test "UPDATE with WHERE"
      (lambda () (assert-parse "UPDATE users SET name = 'john' WHERE id = 1")))

(test "UPDATE multiple columns"
      (lambda () (assert-parse "UPDATE users SET name = 'john', email = 'john@test.com' WHERE id = 1")))

(test "DELETE simple"
      (lambda () (assert-parse "DELETE FROM users")))

(test "DELETE with WHERE"
      (lambda () (assert-parse "DELETE FROM users WHERE id = 1")))

(test "DELETE with complex WHERE"
      (lambda () (assert-parse "DELETE FROM orders WHERE status = 'cancelled' AND created_at < '2024-01-01'")))

;;; ============================================================
;;; Case Insensitivity Tests
;;; ============================================================

(display "
=== Case Insensitivity Tests ===
")

(test "lowercase keywords"
      (lambda () (assert-parse "select * from users where id = 1")))

(test "mixed case keywords"
      (lambda () (assert-parse "Select * From Users Where Id = 1")))

(test "UPPERCASE keywords"
      (lambda () (assert-parse "SELECT * FROM USERS WHERE ID = 1")))

;;; ============================================================
;;; Expression Tests
;;; ============================================================

(display "
=== Expression Tests ===
")

(test "Arithmetic precedence"
      (lambda () (assert-parse "SELECT 1 + 2 * 3 FROM dual")))

(test "Parenthesized expr"
      (lambda () (assert-parse "SELECT (1 + 2) * 3 FROM dual")))

(test "String concatenation"
      (lambda () (assert-parse "SELECT first_name || ' ' || last_name FROM users")))

(test "Comparison operators"
      (lambda () (assert-parse "SELECT * FROM t WHERE a = 1 AND b <> 2 AND c < 3 AND d > 4 AND e <= 5 AND f >= 6")))

(test "NOT IN"
      (lambda () (assert-parse "SELECT * FROM users WHERE status NOT IN ('deleted', 'banned')")))

(test "NOT LIKE"
      (lambda () (assert-parse "SELECT * FROM users WHERE name NOT LIKE 'test%'")))

(test "NOT BETWEEN"
      (lambda () (assert-parse "SELECT * FROM orders WHERE amount NOT BETWEEN 0 AND 100")))

(test "Complex CASE"
      (lambda () (assert-parse "SELECT CASE status WHEN 1 THEN 'a' WHEN 2 THEN 'b' ELSE 'c' END FROM t")))

;;; ============================================================
;;; Validation Tests
;;; ============================================================

(display "
=== Validation Tests ===
")

(test "Valid SELECT validates"
      (lambda () (assert-valid "SELECT * FROM users")))

(test "Valid INSERT validates"
      (lambda () (assert-valid "INSERT INTO users (name) VALUES ('test')")))

(test "Valid UPDATE validates"
      (lambda () (assert-valid "UPDATE users SET name = 'test' WHERE id = 1")))

(test "Valid DELETE validates"
      (lambda () (assert-valid "DELETE FROM users WHERE id = 1")))

;;; ============================================================
;;; Formatter Tests
;;; ============================================================

(display "
=== Formatter Tests ===
")

(test "Format SELECT roundtrip"
      (lambda () (assert-roundtrip "SELECT * FROM users")))

(test "Format SELECT with WHERE roundtrip"
      (lambda () (assert-roundtrip "SELECT * FROM users WHERE id = 1")))

(test "Format INSERT roundtrip"
      (lambda () (assert-roundtrip "INSERT INTO users (name) VALUES ('test')")))

(test "Format UPDATE roundtrip"
      (lambda () (assert-roundtrip "UPDATE users SET name = 'test'")))

(test "Format DELETE roundtrip"
      (lambda () (assert-roundtrip "DELETE FROM users WHERE id = 1")))

(test "Format SELECT JOIN roundtrip"
      (lambda () (assert-roundtrip "SELECT * FROM a INNER JOIN b ON a.id = b.a_id")))

(test "Format CASE roundtrip"
      (lambda () (assert-roundtrip "SELECT CASE WHEN x = 1 THEN 'a' ELSE 'b' END FROM t")))

;;; ============================================================
;;; Error Cases
;;; ============================================================

(display "
=== Error Cases ===
")

(test "Missing FROM fails"
      (lambda () (assert-parse-fails "SELECT *")))

(test "Invalid keyword fails"
      (lambda () (assert-parse-fails "SELEC * FROM users")))

(test "Unclosed string fails"
      (lambda () (assert-parse-fails "SELECT * FROM users WHERE name = 'test")))

(test "Missing table name fails"
      (lambda () (assert-parse-fails "SELECT * FROM")))

(test "Empty IN list fails"
      (lambda () (assert-parse-fails "SELECT * FROM t WHERE x IN ()")))

;;; ============================================================
;;; Integration Tests
;;; ============================================================

(display "
=== Integration Tests ===
")

(test "Full workflow: parse-validate-format"
      (lambda ()
              (let* ([input "select  ID, NAME  from  USERS  where  active = true"]
                     [result (reformat-sql input '((compact . #t) (uppercase-keywords . #t)))])
                    (right? result))))

(test "Complex query workflow"
      (lambda ()
              (let* ([input "SELECT u.name, COUNT(o.id) as order_count
                             FROM users u
                             LEFT JOIN orders o ON u.id = o.user_id
                             WHERE u.status = 'active'
                             GROUP BY u.name
                             HAVING COUNT(o.id) > 5
                             ORDER BY order_count DESC
                             LIMIT 10"]
                     [result (check-sql input)])
                    (right? result))))

;;; ============================================================
;;; Summary
;;; ============================================================

(newline)
(display "╔══════════════════════════════════════╗
")
(display "║        SQL DSL Test Summary          ║
")
(display "╚══════════════════════════════════════╝
")
(display (string-append "  Total:  " (number->string test-count) " tests
"))
(display (string-append "  Passed: " (number->string pass-count) "
"))
(display (string-append "  Failed: " (number->string fail-count) "
"))

(if (= fail-count 0)
    (display "
✓ All SQL tests passed!
")
    (begin
     (display "
✗ Some tests failed!
")
     (exit 1)))
