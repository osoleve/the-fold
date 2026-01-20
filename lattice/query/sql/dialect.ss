(define *sql-dialect-loaded* #t)

(doc 'module 'sql-dialect)
(doc 'description "SQL Dialect Translation")
(doc 'note "Translates SQL AST between dialects: ansi (ANSI SQL baseline), mysql (MySQL 8.x), pgsql (PostgreSQL 15+), tsql (T-SQL SQL Server), oracle (Oracle 19c+)")
(doc 'layer 'lattice)
(doc 'purity 'total)

(unless (top-level-bound? '*sql-types-loaded*)
        (load "lattice/query/sql/types.ss"))
(unless (top-level-bound? '*sql-format-loaded*)
        (load "lattice/query/sql/format.ss"))

;;; ====
;;; Dialect Definitions
;;; ====

;;; Supported dialects
(define sql-dialects '(ansi mysql pgsql tsql oracle))

;;; dialect-valid? : Symbol → Boolean
(define (dialect-valid? dialect)
  (memq dialect sql-dialects))

;;; ====
;;; Dialect Feature Differences
;;; ====
;;;
;;; Key differences between dialects:
;;;
;;; 1. LIMIT/OFFSET
;;;    - ansi/mysql/pgsql: LIMIT n OFFSET m
;;;    - tsql: TOP n / OFFSET m ROWS FETCH NEXT n ROWS ONLY
;;;    - oracle: FETCH FIRST n ROWS ONLY / OFFSET m ROWS
;;;
;;; 2. String Concatenation
;;;    - ansi/pgsql/oracle: ||
;;;    - mysql: CONCAT() function
;;;    - tsql: +
;;;
;;; 3. Identifier Quoting
;;;    - ansi/pgsql/oracle: "identifier"
;;;    - mysql: `identifier`
;;;    - tsql: [identifier]
;;;
;;; 4. Boolean Literals
;;;    - ansi/pgsql/mysql: TRUE, FALSE
;;;    - tsql: 1, 0 (no boolean keywords)
;;;    - oracle: 1, 0 (or 'Y', 'N')
;;;
;;; 5. String Functions
;;;    - LENGTH: ansi/pgsql/mysql
;;;    - LEN: tsql
;;;    - LENGTH/LENGTHB: oracle
;;;
;;; 6. NULL Handling
;;;    - COALESCE: all (standard)
;;;    - IFNULL: mysql
;;;    - ISNULL: tsql
;;;    - NVL: oracle
;;;
;;; 7. LIKE Escape
;;;    - ansi/pgsql: ESCAPE 'char'
;;;    - mysql: ESCAPE 'char' or backslash default
;;;    - tsql: ESCAPE 'char'
;;;    - oracle: ESCAPE 'char'

;;; ====
;;; Dialect Configuration
;;; ====

;;; dialect-config : Symbol → Alist
;;; Returns configuration for a dialect
(define (dialect-config dialect)
  (case dialect
        [(ansi)
         '((string-concat . ||)
           (identifier-quote . #\")
           (limit-style . limit-offset)
           (boolean-style . keywords)
           (null-coalesce . coalesce)
           (length-function . "LENGTH")
           (substring-function . "SUBSTRING"))]
        [(mysql)
         '((string-concat . concat-function)
           (identifier-quote . #\`)
           (limit-style . limit-offset)
           (boolean-style . keywords)
           (null-coalesce . ifnull)
           (length-function . "LENGTH")
           (substring-function . "SUBSTRING"))]
        [(pgsql)
         '((string-concat . ||)
           (identifier-quote . #\")
           (limit-style . limit-offset)
           (boolean-style . keywords)
           (null-coalesce . coalesce)
           (length-function . "LENGTH")
           (substring-function . "SUBSTRING"))]
        [(tsql)
         '((string-concat . +)
           (identifier-quote . brackets)
           (limit-style . top-fetch)
           (boolean-style . numbers)
           (null-coalesce . isnull)
           (length-function . "LEN")
           (substring-function . "SUBSTRING"))]
        [(oracle)
         '((string-concat . ||)
           (identifier-quote . #\")
           (limit-style . fetch-first)
           (boolean-style . numbers)
           (null-coalesce . nvl)
           (length-function . "LENGTH")
           (substring-function . "SUBSTR"))]
        [else (error 'dialect-config "Unknown dialect" dialect)]))

;;; dialect-get : Symbol × Symbol → Any
(define (dialect-get dialect key)
  (let ([config (dialect-config dialect)])
       (let ([pair (assq key config)])
            (if pair (cdr pair)
                (error 'dialect-get "Unknown config key" key)))))

;;; ====
;;; AST Transformation
;;; ====

;;; translate-ast : AST × Symbol × Symbol → AST
;;; Transform AST from source dialect to target dialect
(define (translate-ast ast from-dialect to-dialect)
  (if (eq? from-dialect to-dialect)
      ast
      (transform-node ast from-dialect to-dialect)))

;;; transform-node : AST × Symbol × Symbol → AST
;;; Recursively transform AST node
(define (transform-node node from to)
  (cond
   [(not (sql-ast? node)) node]
   [(select? node) (transform-select node from to)]
   [(insert? node) (transform-insert node from to)]
   [(update? node) (transform-update node from to)]
   [(delete? node) (transform-delete node from to)]
   [(binary-op? node) (transform-binary-op node from to)]
   [(function-call? node) (transform-function-call node from to)]
   [(literal? node) (transform-literal node from to)]
   [else (transform-children node from to)]))

;;; transform-children : AST × Symbol × Symbol → AST
;;; Transform all child nodes
(define (transform-children node from to)
  (let ([tag (sql-tag node)]
        [span (sql-span node)]
        [data (sql-data node)])
       (apply sql-ast tag span
              (map (lambda (child)
                           (cond
                            [(sql-ast? child) (transform-node child from to)]
                            [(list? child) (map (lambda (c)
                                                        (if (sql-ast? c)
                                                            (transform-node c from to)
                                                            c))
                                                child)]
                            [else child]))
                   data))))

;;; ====
;;; SELECT Transformation
;;; ====

;;; transform-select : AST × Symbol × Symbol → AST
(define (transform-select node from to)
  (let* ([span (sql-span node)]
         [distinct? (select-distinct? node)]
         [columns (map (lambda (c) (transform-node c from to)) (select-columns node))]
         [from-clause (select-from node)]
         [where (select-where node)]
         [group-by (select-group-by node)]
         [having (select-having node)]
         [order-by (select-order-by node)]
         [limit (select-limit node)]
         ;; Transform clauses
         [from-transformed (if (null? from-clause) '()
                               (map (lambda (f) (transform-node f from to)) from-clause))]
         [where-transformed (if where (transform-node where from to) #f)]
         [group-transformed (map (lambda (g) (transform-node g from to)) group-by)]
         [having-transformed (if having (transform-node having from to) #f)]
         [order-transformed (map (lambda (o) (transform-node o from to)) order-by)]
         ;; Handle LIMIT dialect differences
         [limit-transformed (transform-limit limit from to)])
        (make-select span distinct? columns from-transformed
                     where-transformed group-transformed having-transformed
                     order-transformed limit-transformed)))

;;; transform-limit : AST × Symbol × Symbol → AST
;;; LIMIT/OFFSET varies significantly between dialects
(define (transform-limit limit from to)
  (if (not limit)
      #f
      ;; For now, keep the same structure - formatting handles differences
      limit))

;;; ====
;;; Expression Transformation
;;; ====

;;; transform-binary-op : AST × Symbol × Symbol → AST
(define (transform-binary-op node from to)
  (let* ([span (sql-span node)]
         [op (binary-op-op node)]
         [left (transform-node (binary-op-left node) from to)]
         [right (transform-node (binary-op-right node) from to)])
        ;; Transform string concatenation (stored as 'concat, not '|| which is empty symbol)
        (if (eq? op 'concat)
            (transform-concat left right span from to)
            (make-binary-op span op left right))))

;;; transform-concat : AST × AST × Span × Symbol × Symbol → AST
;;; Transform || concatenation to target dialect
;;; Note: We use 'concat as the internal symbol because '|| is the empty symbol in Chez Scheme
;;; (the |...| syntax escapes symbol names, so || = empty symbol with length 0)
(define (transform-concat left right span from to)
  (let ([concat-style (dialect-get to 'string-concat)])
       (case concat-style
             [(||) (make-binary-op span 'concat left right)]  ; Keep as 'concat, formatter outputs "||"
             [(+) (make-binary-op span '+ left right)]
             [(concat-function)
              ;; Convert to CONCAT(left, right)
              (make-function-call span "CONCAT" (list left right) #f)]
             [else (make-binary-op span 'concat left right)])))

;;; transform-function-call : AST × Symbol × Symbol → AST
(define (transform-function-call node from to)
  (let* ([span (sql-span node)]
         [name (function-call-name node)]
         [args (map (lambda (a) (if (sql-ast? a) (transform-node a from to) a))
                    (function-call-args node))]
         [distinct? (function-call-distinct? node)]
         [uname (string-upcase name)])
        ;; Transform function names between dialects
        (let ([new-name (transform-function-name uname from to)])
             (make-function-call span new-name args distinct?))))

;;; transform-function-name : String × Symbol × Symbol → String
;;; Map function names between dialects
(define (transform-function-name name from to)
  (cond
   ;; LENGTH variations
   [(string-ci-equal? name "LENGTH")
    (dialect-get to 'length-function)]
   [(string-ci-equal? name "LEN")
    (dialect-get to 'length-function)]
   ;; SUBSTRING variations
   [(string-ci-equal? name "SUBSTRING")
    (dialect-get to 'substring-function)]
   [(string-ci-equal? name "SUBSTR")
    (dialect-get to 'substring-function)]
   ;; NULL coalescing
   [(string-ci-equal? name "COALESCE")
    (transform-null-function to)]
   [(string-ci-equal? name "IFNULL")
    (transform-null-function to)]
   [(string-ci-equal? name "ISNULL")
    (transform-null-function to)]
   [(string-ci-equal? name "NVL")
    (transform-null-function to)]
   ;; Default: keep as-is
   [else name]))

;;; transform-null-function : Symbol → String
(define (transform-null-function dialect)
  (case (dialect-get dialect 'null-coalesce)
        [(coalesce) "COALESCE"]
        [(ifnull) "IFNULL"]
        [(isnull) "ISNULL"]
        [(nvl) "NVL"]
        [else "COALESCE"]))

;;; transform-literal : AST × Symbol × Symbol → AST
(define (transform-literal node from to)
  (let* ([span (sql-span node)]
         [value (literal-value node)]
         [type (literal-type node)])
        ;; Transform boolean literals for dialects without TRUE/FALSE
        (if (eq? type 'boolean)
            (let ([bool-style (dialect-get to 'boolean-style)])
                 (case bool-style
                       [(keywords) node]
                       [(numbers)
                        (make-literal span (if value 1 0) 'number)]
                       [else node]))
            node)))

;;; ====
;;; INSERT/UPDATE/DELETE Transformation
;;; ====

(define (transform-insert node from to)
  (transform-children node from to))

(define (transform-update node from to)
  (transform-children node from to))

(define (transform-delete node from to)
  (transform-children node from to))

;;; ====
;;; Dialect-Aware Formatting
;;; ====

;;; format-sql-dialect : AST × Symbol × Alist → String
;;; Format AST for specific dialect
(define (format-sql-dialect ast dialect . opts-arg)
  (let* ([opts (if (null? opts-arg) default-format-options (car opts-arg))]
         [dialect-opts (cons (cons 'dialect dialect) opts)])
        (format-sql-with-dialect ast dialect-opts)))

;;; format-sql-with-dialect : AST × Alist → String
;;; Format with dialect-specific rules
(define (format-sql-with-dialect ast opts)
  (let ([dialect (get-option opts 'dialect 'ansi)])
       (cond
        [(select? ast) (format-select-dialect ast dialect opts)]
        [(insert? ast) (format-insert-dialect ast dialect opts)]
        [(update? ast) (format-update-dialect ast dialect opts)]
        [(delete? ast) (format-delete-dialect ast dialect opts)]
        [else (format-sql ast opts)])))

;;; format-select-dialect : AST × Symbol × Alist → String
(define (format-select-dialect ast dialect opts)
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
         [limit (select-limit ast)]
         [limit-style (dialect-get dialect 'limit-style)])
        (string-append
         ;; T-SQL TOP clause
         (if (and (eq? limit-style 'top-fetch) limit (not (limit-offset limit)))
             (string-append (format-keyword "SELECT" opts) " "
                            (format-keyword "TOP" opts) " "
                            (number->string (limit-value limit)))
             (format-keyword "SELECT" opts))
         (if distinct? (string-append " " (format-keyword "DISTINCT" opts)) "")
         nl indent
         (format-select-list columns opts)
         (if (null? from)
             ""
             (string-append nl (format-keyword "FROM" opts) nl indent
                            (format-from-list from opts)))
         (if where
             (string-append nl (format-keyword "WHERE" opts) nl indent
                            (format-expression-dialect where dialect opts))
             "")
         (if (null? group-by)
             ""
             (string-append nl (format-keyword "GROUP BY" opts) nl indent
                            (string-join (map (lambda (e) (format-expression-dialect e dialect opts)) group-by) ", ")))
         (if having
             (string-append nl (format-keyword "HAVING" opts) nl indent
                            (format-expression-dialect having dialect opts))
             "")
         (if (null? order-by)
             ""
             (string-append nl (format-keyword "ORDER BY" opts) nl indent
                            (string-join (map (lambda (o) (format-order-item o opts)) order-by) ", ")))
         ;; LIMIT formatting varies by dialect
         (format-limit-dialect limit dialect opts))))

;;; format-limit-dialect : AST × Symbol × Alist → String
(define (format-limit-dialect limit dialect opts)
  (if (not limit)
      ""
      (let ([val (limit-value limit)]
            [offset (limit-offset limit)]
            [style (dialect-get dialect 'limit-style)]
            [nl (if (get-option opts 'compact #f) " " "
")])
           (case style
                 [(limit-offset)
                  ;; MySQL, PostgreSQL style
                  (string-append
                   nl (format-keyword "LIMIT" opts) " " (number->string val)
                   (if offset
                       (string-append " " (format-keyword "OFFSET" opts) " " (number->string offset))
                       ""))]
                 [(top-fetch)
                  ;; T-SQL style - TOP already handled, now OFFSET...FETCH
                  (if offset
                      (string-append
                       nl (format-keyword "OFFSET" opts) " " (number->string offset) " "
                       (format-keyword "ROWS" opts)
                       nl (format-keyword "FETCH NEXT" opts) " " (number->string val) " "
                       (format-keyword "ROWS ONLY" opts))
                      "")]
                 [(fetch-first)
                  ;; Oracle style
                  (string-append
                   (if offset
                       (string-append nl (format-keyword "OFFSET" opts) " " (number->string offset) " "
                                      (format-keyword "ROWS" opts))
                       "")
                   nl (format-keyword "FETCH FIRST" opts) " " (number->string val) " "
                   (format-keyword "ROWS ONLY" opts))]
                 [else ""]))))

;;; format-expression-dialect : AST × Symbol × Alist → String
(define (format-expression-dialect expr dialect opts)
  ;; For now, use standard formatting
  ;; Could extend to handle dialect-specific expression formatting
  (format-expression expr opts))

;;; Placeholders for other statement types
(define (format-insert-dialect ast dialect opts)
  (format-insert ast opts))

(define (format-update-dialect ast dialect opts)
  (format-update ast opts))

(define (format-delete-dialect ast dialect opts)
  (format-delete ast opts))

;;; ====
;;; Identifier Quoting
;;; ====

;;; quote-identifier : String × Symbol → String
(define (quote-identifier name dialect)
  (let ([quote-style (dialect-get dialect 'identifier-quote)])
       (cond
        [(char? quote-style)
         (string-append (string quote-style) name (string quote-style))]
        [(eq? quote-style 'brackets)
         (string-append "[" name "]")]
        [else name])))

;;; ====
;;; High-Level Translation API
;;; ====

;;; translate-sql : String × Symbol × Symbol → Either Error String
;;; Parse SQL, translate AST, format for target dialect
(define (translate-sql input from-dialect to-dialect . opts-arg)
  (let ([opts (if (null? opts-arg) default-format-options (car opts-arg))])
       (unless (dialect-valid? from-dialect)
               (error 'translate-sql "Invalid source dialect" from-dialect))
       (unless (dialect-valid? to-dialect)
               (error 'translate-sql "Invalid target dialect" to-dialect))
       (let ([parse-result (parse-sql-stmt input)])
            (if (left? parse-result)
                parse-result
                (let* ([ast (from-right parse-result)]
                       [translated (translate-ast ast from-dialect to-dialect)]
                       [formatted (format-sql-dialect translated to-dialect opts)])
                      (right formatted))))))

;;; translate-sql-all : String × Symbol → Alist
;;; Translate SQL to all supported dialects
(define (translate-sql-all input from-dialect)
  (map (lambda (dialect)
               (cons dialect
                     (let ([result (translate-sql input from-dialect dialect)])
                          (if (right? result)
                              (from-right result)
                              (format "Error: ~a" (from-left result))))))
       sql-dialects))
