;;; lattice/query/sql/test-ast-zipper.ss — Tests for SQL AST Zipper
;;;
;;; Tests zipper-based navigation for SQL AST trees.

(load "lattice/query/sql/ast-zipper.ss")
(load "core/testing/test-framework.ss")

;;; ============================================================
;;; Test AST Constructors (simplified versions for testing)
;;; ============================================================

(define (make-test-ast tag . data)
  (list 'sql-ast tag '(0 0) data))

;;; ============================================================
;;; Test Fixtures
;;; ============================================================

;;; Simple SELECT: SELECT id, name FROM users
(define simple-select
  (make-test-ast 'select
                 #f  ; not distinct
                 (list (make-test-ast 'column-ref #f 'id)
                       (make-test-ast 'column-ref #f 'name))
                 (make-test-ast 'table-ref 'users #f)
                 #f #f #f #f #f))

;;; SELECT with WHERE: SELECT * FROM users WHERE age > 18
(define select-with-where
  (make-test-ast 'select
                 #f
                 (list (make-test-ast 'star #f))
                 (make-test-ast 'table-ref 'users #f)
                 (make-test-ast 'binary-op
                                '>
                                (make-test-ast 'column-ref #f 'age)
                                (make-test-ast 'literal 18 'number))
                 #f #f #f #f))

;;; Nested SELECT with subquery
(define nested-select
  (make-test-ast 'select
                 #f
                 (list (make-test-ast 'column-ref #f 'name))
                 (make-test-ast 'table-ref 'users #f)
                 (make-test-ast 'in-expr
                                (make-test-ast 'column-ref #f 'id)
                                (make-test-ast 'subquery
                                               (make-test-ast 'select
                                                              #f
                                                              (list (make-test-ast 'column-ref #f 'user_id))
                                                              (make-test-ast 'table-ref 'orders #f)
                                                              #f #f #f #f #f))
                                #f)
                 #f #f #f #f))

;;; Simple literal
(define simple-literal
  (make-test-ast 'literal 42 'number))

;;; ============================================================
;;; Constructor Tests
;;; ============================================================

(test-group "ast-zipper-constructors"

  (define-test "ast->zipper creates zipper"
    (let ([az (ast->zipper simple-select)])
      (assert-true (ast-zipper? az))
      (assert-equal 'select (ast-zipper-tag-of az))))

  (define-test "zipper->ast round-trips"
    (let* ([az (ast->zipper simple-select)]
           [result (zipper->ast az)])
      (assert-equal 'select (sql-tag result))))

  (define-test "literal creates leaf zipper"
    (let ([az (ast->zipper simple-literal)])
      (assert-true (ast-zipper-at-leaf? az)))))

;;; ============================================================
;;; Navigation Tests
;;; ============================================================

(test-group "ast-zipper-navigation"

  (define-test "ast-zipper-down moves to first child"
    (let* ([az (ast->zipper simple-select)]
           [down (ast-zipper-down az)])
      (assert-true (just? down))
      ;; First child should be a column-ref
      (assert-equal 'column-ref (ast-zipper-tag-of (from-just down)))))

  (define-test "ast-zipper-right moves to sibling"
    (let* ([az (ast->zipper simple-select)]
           [first (from-just (ast-zipper-down az))]
           [second (ast-zipper-right first)])
      (assert-true (just? second))
      (assert-equal 'column-ref (ast-zipper-tag-of (from-just second)))))

  (define-test "ast-zipper-up returns to parent"
    (let* ([az (ast->zipper simple-select)]
           [down (from-just (ast-zipper-down az))]
           [up (ast-zipper-up down)])
      (assert-true (just? up))
      (assert-equal 'select (ast-zipper-tag-of (from-just up)))))

  (define-test "ast-zipper-root returns to root"
    (let* ([az (ast->zipper simple-select)]
           ;; Navigate down to a column-ref
           [child (from-just (ast-zipper-down az))]
           [root (ast-zipper-root child)])
      (assert-equal 'select (ast-zipper-tag-of root))))

  (define-test "ast-zipper-at-root? is true at root"
    (let ([az (ast->zipper simple-select)])
      (assert-true (ast-zipper-at-root? az))))

  (define-test "ast-zipper-at-leaf? is true for literals"
    (let ([az (ast->zipper simple-literal)])
      (assert-true (ast-zipper-at-leaf? az)))))

;;; ============================================================
;;; Traversal Tests
;;; ============================================================

(test-group "ast-zipper-traversal"

  (define-test "ast-zipper-preorder visits all nodes"
    (let* ([az (ast->zipper simple-select)]
           [all (ast-zipper-preorder az)])
      ;; simple-select has: select, 2 column-refs, 1 table-ref = 4 nodes
      (assert-equal 4 (length all))))

  (define-test "ast-zipper-next iterates"
    (let* ([az (ast->zipper simple-select)]
           [count (let loop ([current (just az)] [n 0])
                    (if (nothing? current)
                        n
                        (loop (ast-zipper-next (from-just current)) (+ n 1))))])
      (assert-equal 4 count)))

  (define-test "nested select has more nodes"
    (let* ([az (ast->zipper nested-select)]
           [all (ast-zipper-preorder az)])
      ;; Should have multiple selects, column-refs, table-refs, etc.
      (assert-true (> (length all) 5)))))

;;; ============================================================
;;; Collection Tests
;;; ============================================================

(test-group "ast-zipper-collection"

  (define-test "ast-collect finds all matching"
    (let ([cols (ast-collect (lambda (n) (eq? (sql-tag n) 'column-ref)) simple-select)])
      (assert-equal 2 (length cols))))

  (define-test "ast-collect-by-tag works"
    (let ([cols (ast-collect-by-tag 'column-ref simple-select)])
      (assert-equal 2 (length cols))))

  (define-test "ast-column-refs shorthand"
    (let ([cols (ast-column-refs simple-select)])
      (assert-equal 2 (length cols))))

  (define-test "ast-table-refs finds tables"
    (let ([tables (ast-table-refs simple-select)])
      (assert-equal 1 (length tables))))

  (define-test "ast-literals finds literals"
    (let ([lits (ast-literals select-with-where)])
      (assert-equal 1 (length lits))))

  (define-test "ast-subqueries finds subqueries"
    (let ([subs (ast-subqueries nested-select)])
      (assert-equal 1 (length subs))))

  (define-test "collect in nested finds all"
    (let ([cols (ast-column-refs nested-select)])
      ;; outer: name, id; inner: user_id = 3 total
      (assert-equal 3 (length cols)))))

;;; ============================================================
;;; Find Tests
;;; ============================================================

(test-group "ast-zipper-find"

  (define-test "ast-find locates node"
    (let ([found (ast-find (lambda (n) (eq? (sql-tag n) 'table-ref)) simple-select)])
      (assert-true (just? found))
      (assert-equal 'table-ref (ast-zipper-tag-of (from-just found)))))

  (define-test "ast-find-by-tag works"
    (let ([found (ast-find-by-tag 'literal select-with-where)])
      (assert-true (just? found))
      (assert-equal 'literal (ast-zipper-tag-of (from-just found)))))

  (define-test "ast-find returns nothing if not found"
    (let ([found (ast-find-by-tag 'nonexistent simple-select)])
      (assert-true (nothing? found)))))

;;; ============================================================
;;; Walk Tests
;;; ============================================================

(test-group "ast-zipper-walk"

  (define-test "ast-walk visits all nodes"
    (let ([count 0])
      (ast-walk (lambda (n) (set! count (+ count 1))) simple-select)
      (assert-equal 4 count)))

  (define-test "ast-walk order is preorder"
    (let ([tags '()])
      (ast-walk (lambda (n) (set! tags (cons (sql-tag n) tags))) simple-select)
      ;; First visited should be last in list (reversed by cons)
      (assert-equal 'select (car (reverse tags))))))

;;; ============================================================
;;; Context Tests
;;; ============================================================

(test-group "ast-zipper-context"

  (define-test "ast-zipper-depth at root is 0"
    (let ([az (ast->zipper simple-select)])
      (assert-equal 0 (ast-zipper-depth az))))

  (define-test "ast-zipper-depth increases with navigation"
    (let* ([az (ast->zipper simple-select)]
           [child (from-just (ast-zipper-down az))])
      (assert-equal 1 (ast-zipper-depth child))))

  (define-test "ast-zipper-ancestors empty at root"
    (let ([az (ast->zipper simple-select)])
      (assert-equal '() (ast-zipper-ancestors az))))

  (define-test "ast-zipper-ancestors has parent"
    (let* ([az (ast->zipper simple-select)]
           [child (from-just (ast-zipper-down az))]
           [ancestors (ast-zipper-ancestors child)])
      (assert-equal 1 (length ancestors)))))

;;; ============================================================
;;; Modification Tests
;;; ============================================================

(test-group "ast-zipper-modification"

  (define-test "ast-zipper-set replaces node"
    (let* ([az (ast->zipper simple-literal)]
           [new-lit (make-test-ast 'literal 100 'number)]
           [modified (ast-zipper-set az new-lit)]
           [result (zipper->ast modified)])
      (assert-equal 100 (list-ref (sql-data result) 0))))

  (define-test "ast-zipper-modify transforms node"
    (let* ([az (ast->zipper simple-literal)]
           [modified (ast-zipper-modify az
                       (lambda (n)
                         (make-test-ast 'literal
                                        (* 2 (list-ref (sql-data n) 0))
                                        'number)))]
           [result (zipper->ast modified)])
      (assert-equal 84 (list-ref (sql-data result) 0)))))

;;; ============================================================
;;; Transformation Tests
;;; ============================================================

(test-group "ast-zipper-transform"

  (define-test "ast-transform-if transforms matching nodes"
    ;; Simple test: transform a literal
    (let* ([lit (make-test-ast 'literal 10 'number)]
           [result (ast-transform-if
                    (lambda (n) (eq? (sql-tag n) 'literal))
                    (lambda (n)
                      (make-test-ast 'literal
                                     (* 2 (list-ref (sql-data n) 0))
                                     'number))
                    lit)])
      (assert-equal 20 (list-ref (sql-data result) 0)))))

;;; ============================================================
;;; Run Tests
;;; ============================================================

(run-all-tests)
