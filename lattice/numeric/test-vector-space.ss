(load "core/testing/test-framework.ss")
(load "lattice/numeric/vector-space.ss")

;;; ============================================================
;;; Vector Space Record Tests
;;; ============================================================

(test-group "vspace-record"

  (define-test "list-vspace is a vspace"
    (assert-true (vspace? list-vspace)))

  (define-test "scheme-vec-vspace is a vspace"
    (assert-true (vspace? scheme-vec-vspace)))

  (define-test "non-vspace is not a vspace"
    (assert-false (vspace? '(not a vspace)))
    (assert-false (vspace? 42))))

;;; ============================================================
;;; List VSpace Tests
;;; ============================================================

(test-group "list-vspace"

  (define-test "add"
    (assert-equal '(5 7 9) ((vspace-add list-vspace) '(1 2 3) '(4 5 6))))

  (define-test "sub"
    (assert-equal '(-3 -3 -3) ((vspace-sub list-vspace) '(1 2 3) '(4 5 6))))

  (define-test "scale"
    (assert-equal '(2 4 6) ((vspace-scale list-vspace) 2 '(1 2 3))))

  (define-test "madd"
    (assert-equal '(1 2 3) ((vspace-madd list-vspace) '(1 0 0) 1 '(0 2 3))))

  (define-test "madd-with-scalar"
    (assert-equal '(3 4 6) ((vspace-madd list-vspace) '(1 2 0) 2 '(1 1 3))))

  (define-test "norm"
    (assert-equal 5 ((vspace-norm list-vspace) '(3 4))))

  (define-test "norm-unit"
    (assert-equal 1 ((vspace-norm list-vspace) '(1))))

  (define-test "zero"
    (assert-equal '(0 0 0) ((vspace-zero list-vspace) 3)))

  (define-test "dim"
    (assert-equal 3 ((vspace-dim list-vspace) '(1 2 3)))))

;;; ============================================================
;;; Scheme Vector VSpace Tests
;;; ============================================================

(define (assert-vec-equal expected actual)
  (assert-true (and (vector? actual)
                    (= (vector-length expected) (vector-length actual))
                    (let loop ([i 0])
                      (if (= i (vector-length expected))
                          #t
                          (and (< (abs (- (vector-ref expected i) (vector-ref actual i))) 1e-10)
                               (loop (+ i 1))))))))

(test-group "scheme-vec-vspace"

  (define-test "add"
    (assert-vec-equal '#(5 7 9) ((vspace-add scheme-vec-vspace) '#(1 2 3) '#(4 5 6))))

  (define-test "sub"
    (assert-vec-equal '#(-3 -3 -3) ((vspace-sub scheme-vec-vspace) '#(1 2 3) '#(4 5 6))))

  (define-test "scale"
    (assert-vec-equal '#(2 4 6) ((vspace-scale scheme-vec-vspace) 2 '#(1 2 3))))

  (define-test "madd"
    (assert-vec-equal '#(1 2 3) ((vspace-madd scheme-vec-vspace) '#(1 0 0) 1 '#(0 2 3))))

  (define-test "norm"
    (let ([n ((vspace-norm scheme-vec-vspace) '#(3 4))])
      (assert-true (< (abs (- n 5.0)) 1e-10))))

  (define-test "zero"
    (assert-vec-equal '#(0 0 0) ((vspace-zero scheme-vec-vspace) 3)))

  (define-test "dim"
    (assert-equal 3 ((vspace-dim scheme-vec-vspace) '#(1 2 3)))))

;;; ============================================================
;;; Cross-VSpace Consistency
;;; ============================================================

(test-group "cross-vspace"

  (define-test "list and vec add agree"
    (let ([list-result ((vspace-add list-vspace) '(1 2 3) '(4 5 6))]
          [vec-result ((vspace-add scheme-vec-vspace) '#(1 2 3) '#(4 5 6))])
      (assert-equal (car list-result) (vector-ref vec-result 0))
      (assert-equal (cadr list-result) (vector-ref vec-result 1))
      (assert-equal (caddr list-result) (vector-ref vec-result 2))))

  (define-test "list and vec norm agree"
    (let ([list-norm ((vspace-norm list-vspace) '(3 4))]
          [vec-norm ((vspace-norm scheme-vec-vspace) '#(3 4))])
      (assert-true (< (abs (- list-norm vec-norm)) 1e-10)))))

(run-all-tests)
