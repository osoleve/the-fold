;;; playpen/sql/format.ss — SQL Pretty Printer
;;;
;;; Formats SQL AST back to readable SQL strings.
;;; Supports configurable formatting options.
;;;
;;; Dependencies:
;;;   - playpen/sql/types.ss

;;; Load dependencies - assumes running from project root
(define *sql-format-loaded* #t)

(unless (top-level-bound? '*sql-types-loaded*)
        (load "user/sql/types.ss"))

;;; ============================================================
;;; Formatting Options
;;; ============================================================

;;; default-format-options : Alist
;;; Options:
;;;   indent            : number of spaces, or 'tab for tab characters
;;;   uppercase-keywords: #t for SELECT, #f for select
;;;   compact           : #t for single line, #f for multi-line
;;;   max-line-width    : max characters per line (0 for no limit)
;;;   leading-commas    : #t for ", col" style, #f for "col," style
;;;   align-keywords    : #t to right-align SELECT/FROM/WHERE
;;;   and-or-newline    : 'start for "AND x", 'end for "x AND"
;;;   explicit-as       : #t to always include AS for aliases
;;;   join-newline      : #t for JOINs on new lines
;;;   trailing-semicolon: #t to append semicolon
;;;   paren-spacing     : #t for "( x )", #f for "(x)"
;;;   column-per-line   : #t for each column on own line
(define default-format-options
  '((indent . tab)
    (uppercase-keywords . #t)
    (compact . #f)
    (max-line-width . 120)
    (leading-commas . #t)
    (align-keywords . #f)
    (and-or-newline . start)
    (explicit-as . #t)
    (join-newline . #t)
    (trailing-semicolon . #f)
    (paren-spacing . #f)
    (column-per-line . #t)))

;;; get-option : Alist × Symbol × Any → Any
(define (get-option opts key default)
  (let ([pair (assq key opts)])
       (if pair (cdr pair) default)))

;;; ============================================================
;;; Keyword Formatting
;;; ============================================================

;;; format-keyword : String × Alist → String
(define (format-keyword kw opts)
  (if (get-option opts 'uppercase-keywords #t)
      (string-upcase kw)
      (string-downcase kw)))

;;; string-upcase : String → String
(define (string-upcase s)
  (list->string (map char-upcase (string->list s))))

;;; string-downcase : String → String
(define (string-downcase s)
  (list->string (map char-downcase (string->list s))))

;;; ============================================================
;;; Indentation
;;; ============================================================

;;; make-indent : Nat × Alist → String
;;; Supports numeric indent (spaces) or 'tab for tab characters
(define (make-indent level opts)
  (let ([indent-val (get-option opts 'indent 'tab)])
       (if (eq? indent-val 'tab)
           (make-string level #	ab)
           (make-string (* level indent-val) #\space))))

;;; make-string : Nat × Char → String
(define (make-string n ch)
  (list->string (make-list n ch)))

;;; make-list : Nat × a → (List a)
(define (make-list n val)
  (if (<= n 0) '() (cons val (make-list (- n 1) val))))

;;; ============================================================
;;; Statement Formatting
;;; ============================================================

;;; format-sql : AST × Alist → String
;;; Main entry point for formatting
(define (format-sql ast . opts-arg)
  (let* ([opts (if (null? opts-arg) default-format-options (car opts-arg))]
         [trailing-semicolon? (get-option opts 'trailing-semicolon #f)]
         [result (cond
                  [(select? ast) (format-select ast opts)]
                  [(insert? ast) (format-insert ast opts)]
                  [(update? ast) (format-update ast opts)]
                  [(delete? ast) (format-delete ast opts)]
                  [else (format-expression ast opts)])])
        (if trailing-semicolon?
            (string-append result ";")
            result)))

;;; ============================================================
;;; SELECT Formatting
;;; ============================================================

;;; format-select : AST × Alist → String
(define (format-select ast opts)
  (let* ([compact? (get-option opts 'compact #f)]
         [nl (if compact? " " "
")]
         [indent (if compact? "" (make-indent 1 opts))]
         [distinct? (select-distinct? ast)]
         [columns (select-columns ast)]
         [from (select-from ast)]
         [where (select-where ast)]
         [group-by (select-group-by ast)]
         [having (select-having ast)]
         [order-by (select-order-by ast)]
         [limit (select-limit ast)])
        (string-append
         (format-keyword "SELECT" opts)
         (if distinct? (string-append " " (format-keyword "DISTINCT" opts)) "")
         nl indent
         (format-select-list columns opts)
         (if (null? from)
             ""
             (string-append nl (format-keyword "FROM" opts) nl indent
                            (format-from-list from opts)))
         (if where
             (string-append nl (format-keyword "WHERE" opts) nl indent
                            (format-expression where opts))
             "")
         (if (null? group-by)
             ""
             (string-append nl (format-keyword "GROUP BY" opts) nl indent
                            (string-join (map (lambda (e) (format-expression e opts)) group-by) ", ")))
         (if having
             (string-append nl (format-keyword "HAVING" opts) nl indent
                            (format-expression having opts))
             "")
         (if (null? order-by)
             ""
             (string-append nl (format-keyword "ORDER BY" opts) nl indent
                            (string-join (map (lambda (o) (format-order-item o opts)) order-by) ", ")))
         (if limit
             (string-append nl (format-limit-clause limit opts))
             ""))))

;;; format-select-list : (List AST) × Alist → String
(define (format-select-list items opts)
  (let* ([compact? (get-option opts 'compact #f)]
         [leading-commas? (get-option opts 'leading-commas #t)]
         [column-per-line? (get-option opts 'column-per-line #t)]
         [indent (make-indent 1 opts)]
         [formatted-items (map (lambda (item) (format-select-item item opts)) items)])
        (if (or compact? (not column-per-line?))
            ;; Compact: join with ", "
            (string-join formatted-items ", ")
            ;; Multi-line with column-per-line
            (if leading-commas?
                ;; Leading commas: first item, then ", item" on new lines
                (if (null? formatted-items)
                    ""
                    (string-append
                     (car formatted-items)
                     (apply string-append
                            (map (lambda (item)
                                         (string-append "
" indent ", " item))
                                 (cdr formatted-items)))))
                ;; Trailing commas: "item," on each line except last
                (string-join formatted-items (string-append ",
" indent))))))

;;; format-select-item : AST × Alist → String
(define (format-select-item item opts)
  (cond
   [(star? item)
    (let ([table (star-table item)])
         (if table (string-append table ".*") "*"))]
   [(alias? item)
    (let ([explicit-as? (get-option opts 'explicit-as #t)])
         (string-append (format-expression (alias-expr item) opts)
                        (if explicit-as?
                            (string-append " " (format-keyword "AS" opts) " ")
                            " ")
                        (alias-name item)))]
   [else (format-expression item opts)]))

;;; format-from-list : (List AST) × Alist → String
(define (format-from-list items opts)
  (string-join (map (lambda (item) (format-from-item item opts)) items) ", "))

;;; format-from-item : AST × Alist → String
(define (format-from-item item opts)
  (cond
   [(table-ref? item)
    (let ([name (table-ref-name item)]
          [alias (table-ref-alias item)]
          [explicit-as? (get-option opts 'explicit-as #t)])
         (if alias
             (string-append name
                            (if explicit-as?
                                (string-append " " (format-keyword "AS" opts) " ")
                                " ")
                            alias)
             name))]
   [(join? item)
    (format-join item opts)]
   [else "?"]))

;;; format-join : AST × Alist → String
(define (format-join j opts)
  (let* ([type (join-type j)]
         [left (join-left j)]
         [right (join-right j)]
         [condition (join-condition j)]
         [compact? (get-option opts 'compact #f)]
         [join-newline? (get-option opts 'join-newline #t)]
         [nl (if (and join-newline? (not compact?))
                 (string-append "
" (make-indent 1 opts))
                 " ")])
        (string-append
         (format-from-item left opts)
         nl
         (format-join-type type opts)
         " "
         (format-keyword "JOIN" opts)
         " "
         (format-from-item right opts)
         (if condition
             (string-append " " (format-keyword "ON" opts) " " (format-expression condition opts))
             ""))))

;;; format-join-type : Symbol × Alist → String
(define (format-join-type type opts)
  (case type
        [(inner) (format-keyword "INNER" opts)]
        [(left) (format-keyword "LEFT" opts)]
        [(right) (format-keyword "RIGHT" opts)]
        [(full) (format-keyword "FULL" opts)]
        [(cross) (format-keyword "CROSS" opts)]
        [(natural) (format-keyword "NATURAL" opts)]
        [else ""]))

;;; format-order-item : AST × Alist → String
(define (format-order-item item opts)
  (let ([expr (order-item-expr item)]
        [direction (order-item-direction item)]
        [nulls (order-item-nulls item)])
       (string-append
        (format-expression expr opts)
        " "
        (format-keyword (if (eq? direction 'desc) "DESC" "ASC") opts)
        (if nulls
            (string-append " " (format-keyword "NULLS" opts) " "
                           (format-keyword (if (eq? nulls 'first) "FIRST" "LAST") opts))
            ""))))

;;; format-limit-clause : AST × Alist → String
(define (format-limit-clause limit opts)
  (let ([val (limit-value limit)]
        [offset (limit-offset limit)])
       (string-append
        (format-keyword "LIMIT" opts) " " (number->string val)
        (if offset
            (string-append " " (format-keyword "OFFSET" opts) " " (number->string offset))
            ""))))

;;; ============================================================
;;; INSERT Formatting
;;; ============================================================

;;; format-insert : AST × Alist → String
(define (format-insert ast opts)
  (let* ([compact? (get-option opts 'compact #f)]
         [nl (if compact? " " "
")]
         [indent (if compact? "" (make-indent 1 opts))]
         [table (insert-table ast)]
         [columns (insert-columns ast)]
         [values (insert-values ast)])
        (string-append
         (format-keyword "INSERT INTO" opts) " " table
         (if (null? columns)
             ""
             (string-append " (" (string-join columns ", ") ")"))
         nl
         (format-insert-values values opts))))

;;; format-insert-values : AST × Alist → String
(define (format-insert-values values opts)
  (cond
   [(values-clause? values)
    (string-append
     (format-keyword "VALUES" opts) " "
     (string-join (map (lambda (row)
                               (string-append "(" (format-values-row row opts) ")"))
                       (values-clause-rows values))
                  ", "))]
   [(select? values)
    (format-select values opts)]
   [else "?"]))

;;; format-values-row : (List AST) × Alist → String
(define (format-values-row row opts)
  (string-join (map (lambda (val)
                            (if (default-value? val)
                                (format-keyword "DEFAULT" opts)
                                (format-expression val opts)))
                    row)
               ", "))

;;; ============================================================
;;; UPDATE Formatting
;;; ============================================================

;;; format-update : AST × Alist → String
(define (format-update ast opts)
  (let* ([compact? (get-option opts 'compact #f)]
         [nl (if compact? " " "
")]
         [indent (if compact? "" (make-indent 1 opts))]
         [table (update-table ast)]
         [set-clauses (update-set-clauses ast)]
         [where (update-where ast)])
        (string-append
         (format-keyword "UPDATE" opts) " " table nl
         (format-keyword "SET" opts) nl indent
         (string-join (map (lambda (s) (format-set-clause s opts)) set-clauses)
                      (string-append "," nl indent))
         (if where
             (string-append nl (format-keyword "WHERE" opts) nl indent
                            (format-expression where opts))
             ""))))

;;; format-set-clause : AST × Alist → String
(define (format-set-clause clause opts)
  (string-append
   (set-clause-column clause)
   " = "
   (format-expression (set-clause-value clause) opts)))

;;; ============================================================
;;; DELETE Formatting
;;; ============================================================

;;; format-delete : AST × Alist → String
(define (format-delete ast opts)
  (let* ([compact? (get-option opts 'compact #f)]
         [nl (if compact? " " "
")]
         [indent (if compact? "" (make-indent 1 opts))]
         [table (delete-table ast)]
         [where (delete-where ast)])
        (string-append
         (format-keyword "DELETE FROM" opts) " " table
         (if where
             (string-append nl (format-keyword "WHERE" opts) nl indent
                            (format-expression where opts))
             ""))))

;;; ============================================================
;;; Expression Formatting
;;; ============================================================

;;; format-expression : AST × Alist → String
(define (format-expression expr opts)
  (cond
   [(literal? expr) (format-literal expr opts)]
   [(column-ref? expr) (format-column-ref expr opts)]
   [(binary-op? expr) (format-binary-op expr opts)]
   [(unary-op? expr) (format-unary-op expr opts)]
   [(function-call? expr) (format-function-call expr opts)]
   [(case-expr? expr) (format-case-expr expr opts)]
   [(in-expr? expr) (format-in-expr expr opts)]
   [(between-expr? expr) (format-between-expr expr opts)]
   [(like-expr? expr) (format-like-expr expr opts)]
   [(is-null-expr? expr) (format-is-null-expr expr opts)]
   [(subquery? expr) (format-subquery expr opts)]
   [(exists-expr? expr) (format-exists-expr expr opts)]
   [else "?"]))

;;; format-literal : AST × Alist → String
(define (format-literal lit opts)
  (let ([value (literal-value lit)]
        [type (literal-type lit)])
       (case type
             [(number) (number->string value)]
             [(string) (string-append "'" (escape-sql-string value) "'")]
             [(boolean) (format-keyword (if value "TRUE" "FALSE") opts)]
             [(null) (format-keyword "NULL" opts)]
             [else (format "~a" value)])))

;;; escape-sql-string : String → String
;;; Escape single quotes by doubling them
(define (escape-sql-string s)
  (let loop ([chars (string->list s)]
             [result '()])
       (if (null? chars)
           (list->string (reverse result))
           (let ([c (car chars)])
                (if (char=? c #\')
                    (loop (cdr chars) (cons #\' (cons #\' result)))
                    (loop (cdr chars) (cons c result)))))))

;;; format-column-ref : AST × Alist → String
(define (format-column-ref ref opts)
  (let ([table (column-ref-table ref)]
        [column (column-ref-name ref)])
       (if table
           (string-append table "." column)
           column)))

;;; format-binary-op : AST × Alist → String
(define (format-binary-op expr opts)
  (let* ([op (binary-op-op expr)]
         [left (binary-op-left expr)]
         [right (binary-op-right expr)]
         [compact? (get-option opts 'compact #f)]
         [paren-spacing? (get-option opts 'paren-spacing #f)]
         [and-or-newline (get-option opts 'and-or-newline 'start)]
         [is-logical? (memq op '(and or))]
         [lp (if paren-spacing? "( " "(")]
         [rp (if paren-spacing? " )" ")")]
         [indent (make-indent 1 opts)])
        (if (and is-logical? (not compact?))
            ;; Special formatting for AND/OR
            (if (eq? and-or-newline 'start)
                ;; AND/OR at start of line
                (string-append
                 (format-expression left opts)
                 "
" indent (format-operator op opts) " "
                 (format-expression right opts))
                ;; AND/OR at end of line
                (string-append
                 (format-expression left opts) " "
                 (format-operator op opts)
                 "
" indent (format-expression right opts)))
            ;; Regular inline formatting
            (string-append
             lp (format-expression left opts) " "
             (format-operator op opts) " "
             (format-expression right opts) rp))))

;;; format-unary-op : AST × Alist → String
(define (format-unary-op expr opts)
  (let ([op (unary-op-op expr)]
        [operand (unary-op-operand expr)])
       (if (eq? op 'not)
           (string-append (format-keyword "NOT" opts) " (" (format-expression operand opts) ")")
           (string-append (symbol->string op) "(" (format-expression operand opts) ")"))))

;;; format-operator : Symbol × Alist → String
(define (format-operator op opts)
  (case op
        [(and) (format-keyword "AND" opts)]
        [(or) (format-keyword "OR" opts)]
        [(not) (format-keyword "NOT" opts)]
        [(concat) "||"]
        [else (symbol->string op)]))

;;; format-function-call : AST × Alist → String
(define (format-function-call expr opts)
  (let* ([name (function-call-name expr)]
         [args (function-call-args expr)]
         [distinct? (function-call-distinct? expr)]
         [paren-spacing? (get-option opts 'paren-spacing #f)]
         [lp (if paren-spacing? "( " "(")]
         [rp (if paren-spacing? " )" ")")])
        (string-append
         (string-upcase name) lp
         (if distinct? (string-append (format-keyword "DISTINCT" opts) " ") "")
         (if (and (pair? args) (eq? (car args) '*))
             "*"
             (string-join (map (lambda (a) (format-expression a opts)) args) ", "))
         rp)))

;;; format-case-expr : AST × Alist → String
(define (format-case-expr expr opts)
  (let ([operand (case-expr-operand expr)]
        [when-clauses (case-expr-when-clauses expr)]
        [else-clause (case-expr-else expr)])
       (string-append
        (format-keyword "CASE" opts)
        (if operand (string-append " " (format-expression operand opts)) "")
        " "
        (string-join (map (lambda (w) (format-when-clause w opts)) when-clauses) " ")
        (if else-clause
            (string-append " " (format-keyword "ELSE" opts) " " (format-expression else-clause opts))
            "")
        " " (format-keyword "END" opts))))

;;; format-when-clause : AST × Alist → String
(define (format-when-clause clause opts)
  (string-append
   (format-keyword "WHEN" opts) " " (format-expression (when-condition clause) opts)
   " " (format-keyword "THEN" opts) " " (format-expression (when-result clause) opts)))

;;; format-in-expr : AST × Alist → String
(define (format-in-expr expr opts)
  (let ([left (in-expr-expr expr)]
        [values (in-expr-values expr)]
        [not? (in-expr-not? expr)])
       (string-append
        (format-expression left opts)
        (if not? (string-append " " (format-keyword "NOT" opts)) "")
        " " (format-keyword "IN" opts) " ("
        (string-join (map (lambda (v) (format-expression v opts)) values) ", ")
        ")")))

;;; format-between-expr : AST × Alist → String
(define (format-between-expr expr opts)
  (let ([left (between-expr-expr expr)]
        [low (between-expr-low expr)]
        [high (between-expr-high expr)]
        [not? (between-expr-not? expr)])
       (string-append
        (format-expression left opts)
        (if not? (string-append " " (format-keyword "NOT" opts)) "")
        " " (format-keyword "BETWEEN" opts) " "
        (format-expression low opts)
        " " (format-keyword "AND" opts) " "
        (format-expression high opts))))

;;; format-like-expr : AST × Alist → String
(define (format-like-expr expr opts)
  (let ([left (like-expr-expr expr)]
        [pattern (like-expr-pattern expr)]
        [escape (like-expr-escape expr)]
        [not? (like-expr-not? expr)])
       (string-append
        (format-expression left opts)
        (if not? (string-append " " (format-keyword "NOT" opts)) "")
        " " (format-keyword "LIKE" opts) " "
        (format-expression pattern opts)
        (if escape
            (string-append " " (format-keyword "ESCAPE" opts) " '" escape "'")
            ""))))

;;; format-is-null-expr : AST × Alist → String
(define (format-is-null-expr expr opts)
  (let ([left (is-null-expr-expr expr)]
        [not? (is-null-expr-not? expr)])
       (string-append
        (format-expression left opts)
        " " (format-keyword "IS" opts)
        (if not? (string-append " " (format-keyword "NOT" opts)) "")
        " " (format-keyword "NULL" opts))))

;;; format-subquery : AST × Alist → String
(define (format-subquery expr opts)
  (string-append "(" (format-select (subquery-select expr) opts) ")"))

;;; format-exists-expr : AST × Alist → String
(define (format-exists-expr expr opts)
  (let ([subq (exists-expr-subquery expr)]
        [not? (exists-expr-not? expr)])
       (string-append
        (if not? (string-append (format-keyword "NOT" opts) " ") "")
        (format-keyword "EXISTS" opts) " "
        (format-subquery subq opts))))

;;; ============================================================
;;; Utility Functions
;;; ============================================================

;;; string-join : (List String) × String → String
(define (string-join strings delimiter)
  (if (null? strings)
      ""
      (fold-left (lambda (acc s)
                         (string-append acc delimiter s))
                 (car strings)
                 (cdr strings))))
