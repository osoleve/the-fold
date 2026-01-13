;;; lattice/dsl/template/test-template.ss — Tests for Template DSL
;;;
;;; Tests for the grammar-driven code construction system.

(load "core/testing/test-framework.ss")
(load "lattice/dsl/template/template.ss")

;;; ============================================================
;;; Hole Detection Tests
;;; ============================================================

(test-group template-holes

  (define-test "hole? recognizes $-prefixed symbols"
    (assert-true (hole? '$foo))
    (assert-true (hole? '$body))
    (assert-true (hole? '$x)))

  (define-test "hole? rejects non-holes"
    (assert-false (hole? 'foo))
    (assert-false (hole? 'body))
    (assert-false (hole? '$))  ; Just $ is not a valid hole (needs name)
    (assert-false (hole? 123))
    (assert-false (hole? "string"))
    (assert-false (hole? '(list))))

  (define-test "hole-name extracts name from hole"
    (assert-equal 'foo (hole-name '$foo))
    (assert-equal 'body (hole-name '$body))
    (assert-equal 'x (hole-name '$x)))

  (define-test "find-holes finds holes in expression"
    (assert-equal '($a $b) (find-holes '($a $b)))
    (assert-equal '($x) (find-holes '(foo $x bar)))
    (assert-equal '() (find-holes '(foo bar baz))))

  (define-test "find-holes preserves left-to-right order"
    (assert-equal '($a $b $c) (find-holes '($a ($b) $c)))
    (assert-equal '($first $second $third)
                  (find-holes '(if $first $second $third))))

  (define-test "find-holes removes duplicates preserving first occurrence"
    (assert-equal '($x $y) (find-holes '($x $y $x)))
    (assert-equal '($a) (find-holes '($a $a $a))))

  (define-test "find-holes handles nested structures"
    (assert-equal '($sig $body)
                  (find-holes '(define $sig $body)))
    (assert-equal '($name $args $body)
                  (find-holes '(define ($name $args) $body)))))

;;; ============================================================
;;; Template Construction Tests
;;; ============================================================

(test-group template-construction

  (define-test "new-template creates template with holes"
    (let ([t (new-template '(define $sig $body))])
      (assert-true (template? t))
      (assert-equal '($sig $body) (template-holes t))))

  (define-test "new-template with no holes"
    (let ([t (new-template '(+ 1 2))])
      (assert-true (template? t))
      (assert-equal '() (template-holes t))
      (assert-true (template-complete? t))))

  (define-test "template-hole-count returns correct count"
    (assert-equal 2 (template-hole-count (new-template '($a $b))))
    (assert-equal 0 (template-hole-count (new-template '(foo bar))))
    (assert-equal 3 (template-hole-count (new-template '(if $a $b $c))))))

;;; ============================================================
;;; Hole Filling Tests
;;; ============================================================

(test-group template-filling

  (define-test "fill-hole replaces single hole"
    (let* ([t1 (new-template '(define $name 42))]
           [t2 (fill-hole t1 '$name 'x)])
      (assert-equal '(define x 42) (template-expr t2))
      (assert-equal '() (template-holes t2))))

  (define-test "fill-hole propagates new holes from value"
    (let* ([t1 (new-template '(define $sig $body))]
           [t2 (fill-hole t1 '$sig '($name $args))])
      (assert-equal '(define ($name $args) $body) (template-expr t2))
      (assert-equal '($name $args $body) (template-holes t2))))

  (define-test "fill-holes fills multiple holes"
    (let* ([t1 (new-template '(+ $a $b))]
           [t2 (fill-holes t1 '(($a . 1) ($b . 2)))])
      (assert-equal '(+ 1 2) (template-expr t2))
      (assert-true (template-complete? t2))))

  (define-test "fill-hole replaces all occurrences of hole"
    (let* ([t1 (new-template '(+ $x $x))]
           [t2 (fill-hole t1 '$x 5)])
      (assert-equal '(+ 5 5) (template-expr t2)))))

;;; ============================================================
;;; Compilation Tests
;;; ============================================================

(test-group template-compilation

  (define-test "compile-template returns expr when complete"
    (let ([t (new-template '(+ 1 2))])
      (assert-equal '(+ 1 2) (compile-template t))))

  (define-test "compile-template errors when incomplete"
    (let ([t (new-template '(+ $a $b))])
      (assert-error (lambda () (compile-template t)))))

  (define-test "try-compile-template returns ok when complete"
    (let ([result (try-compile-template (new-template '(foo bar)))])
      (assert-equal 'ok (car result))
      (assert-equal '(foo bar) (cadr result))))

  (define-test "try-compile-template returns err when incomplete"
    (let ([result (try-compile-template (new-template '($x)))])
      (assert-equal 'err (car result))
      (assert-equal '($x) (cadr result)))))

;;; ============================================================
;;; End-to-End Workflow Tests
;;; ============================================================

(test-group template-workflow

  (define-test "build factorial function incrementally"
    (let* ([t1 (new-template '(define $sig $body))]
           [t2 (fill-hole t1 '$sig '($name $args))]
           [t3 (fill-hole t2 '$name 'factorial)]
           [t4 (fill-hole t3 '$args 'n)]
           [t5 (fill-hole t4 '$body '(if (= n 0) 1 (* n (factorial (- n 1)))))])
      (assert-true (template-complete? t5))
      (assert-equal '(define (factorial n) (if (= n 0) 1 (* n (factorial (- n 1)))))
                    (compile-template t5))))

  (define-test "build let expression"
    (let* ([t1 (new-template '(let (($var $val)) $body))]
           [t2 (fill-hole t1 '$var 'x)]
           [t3 (fill-hole t2 '$val 10)]
           [t4 (fill-hole t3 '$body '(+ x 1))])
      (assert-equal '(let ((x 10)) (+ x 1)) (compile-template t4))))

  (define-test "build lambda expression"
    (let* ([t1 (new-template '(lambda ($params) $body))]
           [t2 (fill-hole t1 '$params 'x)]
           [t3 (fill-hole t2 '$body '(* x x))])
      (assert-equal '(lambda (x) (* x x)) (compile-template t3)))))

;;; ============================================================
;;; Utility Tests
;;; ============================================================

(test-group template-utilities

  (define-test "template-status reports holes"
    (let ([t (new-template '($a $b))])
      (assert-equal "Holes: $a, $b" (template-status t))))

  (define-test "template-status reports complete"
    (let ([t (new-template '(foo bar))])
      (assert-equal "Complete!" (template-status t))))

  (define-test "wrap-if-multiple wraps multiple tokens"
    (assert-equal '(a b c) (wrap-if-multiple '(a b c))))

  (define-test "wrap-if-multiple leaves single token unwrapped"
    (assert-equal 'a (wrap-if-multiple '(a)))))

;;; ============================================================
;;; Run All Tests
;;; ============================================================

(display "\n=== Template DSL Tests ===\n\n")
(run-all-tests)
