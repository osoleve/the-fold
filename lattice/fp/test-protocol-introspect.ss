(load "core/testing/test-framework.ss")
(load "lattice/fp/protocol-introspect.ss")

(doc 'module 'test-protocol-introspect)
(doc 'description "Tests for protocol introspection and visualization")

;;; ============================================================
;;; Setup: Define some test protocols and implementations
;;; ============================================================

(define-protocol+ (test-get-name obj) "Get entity name" (-> Entity String) test-module)
(define-protocol+ (test-get-pos obj) "Get entity position")

;;; Test entity constructors
(define (make-test-player name x y)
  (list 'test-player name x y))
(define (make-test-enemy type x y)
  (list 'test-enemy type x y))

;;; Implementations for test-player
(implement-protocol! 'test-get-name 'test-player
  (lambda (p) (cadr p)))
(implement-protocol! 'test-get-pos 'test-player
  (lambda (p) (list (caddr p) (cadddr p))))

;;; Implementations for test-enemy
(implement-protocol! 'test-get-name 'test-enemy
  (lambda (e) (symbol->string (cadr e))))
(implement-protocol! 'test-get-pos 'test-enemy
  (lambda (e) (list (caddr e) (cadddr e))))

;;; ============================================================
;;; Tests
;;; ============================================================

(test-group "Protocol Documentation"
  (define-test "register-protocol-doc! stores documentation"
    (let ([doc (get-protocol-doc 'test-get-name)])
      (assert-true (protocol-doc? doc))
      (assert-equal "Get entity name" (protocol-doc-docstring doc))
      (assert-equal '(-> Entity String) (protocol-doc-signature doc))
      (assert-equal 'test-module (protocol-doc-module doc))))

  (define-test "protocol-docstring returns just the docstring"
    (assert-equal "Get entity name" (protocol-docstring 'test-get-name)))

  (define-test "protocol-docstring returns #f for undocumented protocols"
    (assert-false (protocol-docstring 'nonexistent-protocol))))

(test-group "Reverse Lookup"
  (define-test "protocols-for-type finds all protocols"
    (let ([protos (protocols-for-type 'test-player)])
      (assert-true (pair? (member 'test-get-name protos)))
      (assert-true (pair? (member 'test-get-pos protos)))))

  (define-test "all-type-tags collects all types"
    (let ([types (all-type-tags)])
      (assert-true (pair? (member 'test-player types)))
      (assert-true (pair? (member 'test-enemy types))))))

(test-group "Protocol Description"
  (define-test "protocol-describe generates non-empty output"
    (let ([desc (protocol-describe 'test-get-name)])
      (assert-true (> (string-length desc) 0))
      (assert-true (string-contains desc "test-get-name"))
      (assert-true (string-contains desc "test-player"))
      (assert-true (string-contains desc "test-enemy"))))

  (define-test "type-describe generates non-empty output"
    (let ([desc (type-describe 'test-player)])
      (assert-true (> (string-length desc) 0))
      (assert-true (string-contains desc "test-player"))
      (assert-true (string-contains desc "test-get-name")))))

(test-group "Visualization"
  (define-test "protocol-matrix generates table"
    (let ([matrix (protocol-matrix '(test-player test-enemy) '(test-get-name test-get-pos))])
      (assert-true (> (string-length matrix) 0))
      (assert-true (string-contains matrix "test-player"))
      (assert-true (string-contains matrix "✓"))))

  (define-test "protocol-graph generates tree"
    (let ([graph (protocol-graph)])
      (assert-true (> (string-length graph) 0))
      (assert-true (string-contains graph "test-get-name"))
      (assert-true (string-contains graph "├──")))))

(test-group "Statistics"
  (define-test "protocol-stats returns valid data"
    (let ([stats (protocol-stats)])
      (assert-true (> (cdr (assq 'protocols stats)) 0))
      (assert-true (> (cdr (assq 'types stats)) 0))
      (assert-true (> (cdr (assq 'total-implementations stats)) 0)))))

(test-group "REPL Helpers"
  (define-test "pi outputs without error"
    (let ([output (with-output-to-string (lambda () (pi 'test-get-name)))])
      (assert-true (> (string-length output) 0))))

  (define-test "ti outputs without error"
    (let ([output (with-output-to-string (lambda () (ti 'test-player)))])
      (assert-true (> (string-length output) 0)))))

;;; Helper
(define (string-contains haystack needle)
  (let ([hay-len (string-length haystack)]
        [need-len (string-length needle)])
    (let loop ([i 0])
      (cond
        [(> (+ i need-len) hay-len) #f]
        [(string=? (substring haystack i (+ i need-len)) needle) #t]
        [else (loop (+ i 1))]))))

(run-all-tests)
