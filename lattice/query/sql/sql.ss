;;; playpen/sql/sql.ss — SQL DSL Public API
;;;
;;; High-level API for parsing, validating, and formatting SQL.
;;; This module re-exports the essential functions from sub-modules.
;;;
;;; Usage:
;;;   (load "lattice/query/sql/sql.ss")
;;;   (parse-sql "SELECT * FROM users")
;;;   (check-sql "SELECT name FROM users WHERE id = 1")
;;;   (reformat-sql "select NAME from USERS" '((uppercase-keywords . #t)))
;;;
;;; Dependencies:
;;;   - playpen/sql/types.ss
;;;   - playpen/sql/lexer.ss
;;;   - playpen/sql/parser.ss
;;;   - playpen/sql/validate.ss
;;;   - playpen/sql/format.ss

;;; Load dependencies - assumes running from project root
(define *sql-loaded* #t)

(unless (top-level-bound? '*sql-parser-loaded*)
        (load "lattice/query/sql/parser.ss"))
(unless (top-level-bound? '*sql-validate-loaded*)
        (load "lattice/query/sql/validate.ss"))
(unless (top-level-bound? '*sql-format-loaded*)
        (load "lattice/query/sql/format.ss"))

;;; ============================================================
;;; Core API
;;; ============================================================

;;; parse-sql : String → Either Error AST
;;; Parse SQL string into AST.
;;;
;;; Example:
;;;   (parse-sql "SELECT * FROM users")
;;;   => (right (sql-ast 'select ...))
;;;
;;;   (parse-sql "SELEC * FROM users")
;;;   => (left (parse-error ...))
(define (parse-sql input)
  (parse-sql-stmt input))

;;; validate-sql : AST → Validation (List Error) AST
;;; Validate SQL AST for semantic correctness.
;;; Returns validation result that may contain multiple errors.
;;;
;;; Example:
;;;   (validate-sql ast)
;;;   => (success ast)
;;;
;;;   (validate-sql bad-ast)
;;;   => (failure ((sql-error ...) (sql-error ...)))
(define validate-sql validate-statement)

;;; format-sql : AST × Alist → String
;;; Format SQL AST back to string.
;;;
;;; Options:
;;;   - indent: number of spaces for indentation (default: 2)
;;;   - uppercase-keywords: #t for uppercase SQL keywords (default: #t)
;;;   - compact: #t for single-line output (default: #f)
;;;   - max-line-width: target line width (default: 80)
;;;
;;; Example:
;;;   (format-sql ast)
;;;   => "SELECT
	*
FROM
	users"
;;;
;;;   (format-sql ast '((compact . #t)))
;;;   => "SELECT * FROM users"
;;; format-sql is exported from format.ss

;;; ============================================================
;;; Convenience API
;;; ============================================================

;;; check-sql : String → Either (List Error) AST
;;; Parse and validate SQL in one step.
;;;
;;; Example:
;;;   (check-sql "SELECT * FROM users")
;;;   => (right (sql-ast 'select ...))
(define (check-sql input)
  (let ([parse-result (parse-sql input)])
       (if (left? parse-result)
           parse-result
           (let ([ast (from-right parse-result)])
                (let ([valid-result (validate-sql ast)])
                     (if (validation-success? valid-result)
                         (right (from-success valid-result))
                         (left (from-failure valid-result))))))))

;;; reformat-sql : String × Alist → Either Error String
;;; Parse, validate, and reformat SQL string.
;;;
;;; Example:
;;;   (reformat-sql "select * from users")
;;;   => (right "SELECT
	*
FROM
	users")
;;;
;;;   (reformat-sql "select * from users" '((compact . #t)))
;;;   => (right "SELECT * FROM users")
(define (reformat-sql input . opts-arg)
  (let* ([opts (if (null? opts-arg) default-format-options (car opts-arg))]
         [check-result (check-sql input)])
        (if (left? check-result)
            check-result
            (right (format-sql (from-right check-result) opts)))))

;;; format-sql-compact : AST → String
;;; Format SQL as a single line.
(define (format-sql-compact ast)
  (format-sql ast '((compact . #t))))

;;; format-sql-pretty : AST → String
;;; Format SQL with nice indentation.
(define (format-sql-pretty ast)
  (format-sql ast '((indent . 2) (uppercase-keywords . #t) (compact . #f))))

;;; ============================================================
;;; Error Formatting
;;; ============================================================

;;; format-parse-errors : Either Error a → String
;;; Format parse or validation errors for display.
(define (format-sql-errors result)
  (if (right? result)
      ""
      (let ([errors (from-left result)])
           (if (list? errors)
               ;; Multiple validation errors
               (string-join (map format-sql-error errors) "
")
               ;; Single parse error
               (format-error errors)))))

;;; ============================================================
;;; Schema-Aware API
;;; ============================================================

;;; check-sql-with-schema : String × Schema → Either (List Error) AST
;;; Parse and validate SQL against a schema.
;;; Schema is an alist: ((table-name . (col1 col2 ...)) ...)
;;;
;;; Example:
;;;   (check-sql-with-schema
;;;     "SELECT name FROM users"
;;;     '(("users" . ("id" "name" "email"))))
(define (check-sql-with-schema input schema)
  (let ([parse-result (parse-sql input)])
       (if (left? parse-result)
           parse-result
           (let* ([ast (from-right parse-result)]
                  [valid-result (validate-with-schema ast schema)])
                 (if (validation-success? valid-result)
                     (right (from-success valid-result))
                     (left (from-failure valid-result)))))))

;;; ============================================================
;;; AST Inspection
;;; ============================================================

;;; sql-type : AST → Symbol
;;; Get the type of an SQL AST node.
(define (sql-type ast)
  (if (sql-ast? ast)
      (sql-tag ast)
      'unknown))

;;; sql-select? : Any → Boolean
(define sql-select? select?)

;;; sql-insert? : Any → Boolean
(define sql-insert? insert?)

;;; sql-update? : Any → Boolean
(define sql-update? update?)

;;; sql-delete? : Any → Boolean
(define sql-delete? delete?)

;;; ============================================================
;;; AST Construction (for programmatic SQL building)
;;; ============================================================

;;; Re-export constructors from types.ss
;;; These allow building SQL AST programmatically

;;; Example:
;;;   (format-sql
;;;     (make-select no-span
;;;       #f
;;;       (list (make-star no-span #f))
;;;       (list (make-table-ref no-span "users" #f))
;;;       #f '() #f '() #f))
;;;   => "SELECT * FROM users"

;;; Re-exports from types.ss:
;;; make-select, make-insert, make-update, make-delete
;;; make-column-ref, make-star, make-alias, make-table-ref
;;; make-literal, make-binary-op, make-unary-op, make-function-call
;;; etc.

;;; ============================================================
;;; Quick Reference
;;; ============================================================
;;;
;;; Parse:
;;;   (parse-sql "SELECT * FROM t")       → Either Error AST
;;;
;;; Validate:
;;;   (validate-sql ast)                  → Validation (List Error) AST
;;;
;;; Format:
;;;   (format-sql ast)                    → String
;;;   (format-sql-compact ast)            → String (one line)
;;;   (format-sql-pretty ast)             → String (indented)
;;;
;;; All-in-one:
;;;   (check-sql "...")                   → Either (List Error) AST
;;;   (reformat-sql "...")                → Either Error String
;;;
;;; With schema:
;;;   (check-sql-with-schema "..." schema) → Either (List Error) AST
