;;; playpen/sql/test-dialect.ss — SQL Dialect Translation Tests
;;;
;;; Tests for dialect-specific SQL translation.
;;;
;;; Usage:
;;;   scheme --script playpen/sql/test-dialect.ss

;;; Set up source directories for loading
(let ([stitches-path (string-append (current-directory) "/fabric/stitches")])
     (unless (member stitches-path (source-directories))
             (source-directories (cons stitches-path (source-directories)))))

(load "playpen/sql/sql.ss")
(load "playpen/sql/dialect.ss")

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

;;; ============================================================
;;; Translation Tests
;;; ============================================================

(display "=== Dialect Translation Tests ===
")

;;; Basic translation (no changes needed)
(test "ANSI to ANSI (identity)"
      (lambda ()
              (let ([result (translate-sql "SELECT * FROM users" 'ansi 'ansi)])
                   (right? result))))

(test "ANSI to MySQL"
      (lambda ()
              (let ([result (translate-sql "SELECT * FROM users" 'ansi 'mysql)])
                   (right? result))))

(test "ANSI to PostgreSQL"
      (lambda ()
              (let ([result (translate-sql "SELECT * FROM users" 'ansi 'pgsql)])
                   (right? result))))

(test "ANSI to T-SQL"
      (lambda ()
              (let ([result (translate-sql "SELECT * FROM users" 'ansi 'tsql)])
                   (right? result))))

(test "ANSI to Oracle"
      (lambda ()
              (let ([result (translate-sql "SELECT * FROM users" 'ansi 'oracle)])
                   (right? result))))

;;; ============================================================
;;; LIMIT/OFFSET Translation
;;; ============================================================

(display "
=== LIMIT/OFFSET Translation ===
")

(test "LIMIT to MySQL"
      (lambda ()
              (let ([result (translate-sql "SELECT * FROM users LIMIT 10" 'ansi 'mysql)])
                   (and (right? result)
                        (string-contains? (from-right result) "LIMIT")))))

(test "LIMIT to PostgreSQL"
      (lambda ()
              (let ([result (translate-sql "SELECT * FROM users LIMIT 10" 'ansi 'pgsql)])
                   (and (right? result)
                        (string-contains? (from-right result) "LIMIT")))))

(test "LIMIT to T-SQL (should use TOP)"
      (lambda ()
              (let ([result (translate-sql "SELECT * FROM users LIMIT 10" 'ansi 'tsql)])
                   (and (right? result)
                        (string-contains? (from-right result) "TOP")))))

(test "LIMIT to Oracle (should use FETCH FIRST)"
      (lambda ()
              (let ([result (translate-sql "SELECT * FROM users LIMIT 10" 'ansi 'oracle)])
                   (and (right? result)
                        (string-contains? (from-right result) "FETCH FIRST")))))

(test "LIMIT OFFSET to T-SQL"
      (lambda ()
              (let ([result (translate-sql "SELECT * FROM users LIMIT 10 OFFSET 5" 'ansi 'tsql)])
                   (and (right? result)
                        (string-contains? (from-right result) "OFFSET")
                        (string-contains? (from-right result) "FETCH NEXT")))))

;;; ============================================================
;;; String Concatenation Translation
;;; ============================================================

(display "
=== String Concatenation Translation ===
")

(test "Concat || to MySQL CONCAT()"
      (lambda ()
              (let ([result (translate-sql "SELECT fname || ' ' || lname FROM users" 'ansi 'mysql)])
                   (and (right? result)
                        (string-contains? (from-right result) "CONCAT")))))

(test "Concat || to T-SQL +"
      (lambda ()
              (let ([result (translate-sql "SELECT fname || ' ' || lname FROM users" 'ansi 'tsql)])
                   (and (right? result)
                        ;; Should have + instead of ||
                        (not (string-contains? (from-right result) "||"))))))

(test "Concat || stays || in PostgreSQL"
      (lambda ()
              (let ([result (translate-sql "SELECT fname || lname FROM users" 'ansi 'pgsql)])
                   (and (right? result)
                        (string-contains? (from-right result) "||")))))

;;; ============================================================
;;; Boolean Literal Translation
;;; ============================================================

(display "
=== Boolean Literal Translation ===
")

(test "Boolean TRUE to T-SQL 1"
      (lambda ()
              (let ([result (translate-sql "SELECT * FROM users WHERE active = TRUE" 'ansi 'tsql)])
                   (and (right? result)
                        ;; T-SQL should use 1 instead of TRUE
                        (not (string-contains? (from-right result) "TRUE"))))))

(test "Boolean FALSE to T-SQL 0"
      (lambda ()
              (let ([result (translate-sql "SELECT * FROM users WHERE active = FALSE" 'ansi 'tsql)])
                   (and (right? result)
                        (not (string-contains? (from-right result) "FALSE"))))))

(test "Boolean TRUE stays TRUE in PostgreSQL"
      (lambda ()
              (let ([result (translate-sql "SELECT * FROM users WHERE active = TRUE" 'ansi 'pgsql)])
                   (and (right? result)
                        (string-contains? (from-right result) "TRUE")))))

;;; ============================================================
;;; Function Name Translation
;;; ============================================================

(display "
=== Function Name Translation ===
")

(test "LENGTH to LEN in T-SQL"
      (lambda ()
              (let ([result (translate-sql "SELECT LENGTH(name) FROM users" 'ansi 'tsql)])
                   (and (right? result)
                        (string-contains? (from-right result) "LEN(")))))

(test "LENGTH stays LENGTH in MySQL"
      (lambda ()
              (let ([result (translate-sql "SELECT LENGTH(name) FROM users" 'ansi 'mysql)])
                   (and (right? result)
                        (string-contains? (from-right result) "LENGTH(")))))

(test "SUBSTRING to SUBSTR in Oracle"
      (lambda ()
              (let ([result (translate-sql "SELECT SUBSTRING(name, 1, 3) FROM users" 'ansi 'oracle)])
                   (and (right? result)
                        (string-contains? (from-right result) "SUBSTR(")))))

(test "COALESCE to ISNULL in T-SQL"
      (lambda ()
              (let ([result (translate-sql "SELECT COALESCE(name, 'unknown') FROM users" 'ansi 'tsql)])
                   (and (right? result)
                        (string-contains? (from-right result) "ISNULL(")))))

(test "COALESCE to NVL in Oracle"
      (lambda ()
              (let ([result (translate-sql "SELECT COALESCE(name, 'unknown') FROM users" 'ansi 'oracle)])
                   (and (right? result)
                        (string-contains? (from-right result) "NVL(")))))

(test "COALESCE to IFNULL in MySQL"
      (lambda ()
              (let ([result (translate-sql "SELECT COALESCE(name, 'unknown') FROM users" 'ansi 'mysql)])
                   (and (right? result)
                        (string-contains? (from-right result) "IFNULL(")))))

;;; ============================================================
;;; Translate All Dialects Test
;;; ============================================================

(display "
=== Translate to All Dialects ===
")

(test "translate-sql-all works"
      (lambda ()
              (let ([results (translate-sql-all "SELECT * FROM users LIMIT 10" 'ansi)])
                   (= (length results) 5))))

;;; ============================================================
;;; Helper Functions
;;; ============================================================

;;; string-contains? : String × String → Boolean
(define (string-contains? str substr)
  (let ([str-len (string-length str)]
        [sub-len (string-length substr)])
       (let loop ([i 0])
            (cond
             [(> (+ i sub-len) str-len) #f]
             [(string-prefix? (substring str i str-len) substr) #t]
             [else (loop (+ i 1))]))))

;;; string-prefix? : String × String → Boolean
(define (string-prefix? str prefix)
  (let ([str-len (string-length str)]
        [pre-len (string-length prefix)])
       (and (>= str-len pre-len)
            (string=? (substring str 0 pre-len) prefix))))

;;; ============================================================
;;; Demo: Show all translations
;;; ============================================================

(display "
=== Demo: Complex Query in All Dialects ===
")

(let* ([sql "SELECT u.name, COUNT(o.id) FROM users AS u LEFT JOIN orders AS o ON u.id = o.user_id WHERE u.active = TRUE GROUP BY u.name HAVING COUNT(o.id) > 5 ORDER BY u.name LIMIT 10"]
       [results (translate-sql-all sql 'ansi)])
      (for-each
       (lambda (pair)
               (display (string-append "
--- " (symbol->string (car pair)) " ---
"))
               (display (cdr pair))
               (newline))
       results))

;;; ============================================================
;;; Summary
;;; ============================================================

(newline)
(display "╔══════════════════════════════════════╗
")
(display "║    Dialect Translation Summary       ║
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
✓ All dialect tests passed!
")
    (begin
     (display "
✗ Some tests failed!
")
     (exit 1)))
