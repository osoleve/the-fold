;;; lattice/algebra/test-field-ext.ss --- Tests for Algebraic Field Extensions
;;;
;;; Tests covering:
;;;   - Q(√2), Q(i) construction
;;;   - Element arithmetic
;;;   - Conjugation automorphism
;;;   - Norm and trace
;;;   - Splitting fields
;;;
;;; Run with: scheme --script lattice/algebra/test-field-ext.ss

(load "core/testing/test-framework.ss")
(load "lattice/algebra/field-ext.ss")

;;; ====
;;; Test Helper Functions
;;; ====

(define (approximately-equal? a b tol)
  (< (abs (- a b)) tol))

(define *test-tol* 1e-10)

;;; ====
;;; Q(√2) Tests
;;; ====

(test-group "Q(√2) Construction"

  (define-test "make-Q-sqrt creates extension"
    (let ([Q-sqrt2 (make-Q-sqrt 2)])
      (field? Q-sqrt2)))

  (define-test "ext-degree of Q(√2) is 2"
    (let ([Q-sqrt2 (make-Q-sqrt 2)])
      (= (ext-degree Q-sqrt2) 2)))

  (define-test "Q-sqrt-element creates valid element"
    (let* ([Q-sqrt2 (make-Q-sqrt 2)]
           [elem (Q-sqrt-element Q-sqrt2 3 2)])  ; 3 + 2√2
      (alg-ext-element? elem)))

  (define-test "Q-sqrt-real-part extracts a"
    (let* ([Q-sqrt2 (make-Q-sqrt 2)]
           [elem (Q-sqrt-element Q-sqrt2 3 2)])
      (= (Q-sqrt-real-part elem) 3)))

  (define-test "Q-sqrt-sqrt-part extracts b"
    (let* ([Q-sqrt2 (make-Q-sqrt 2)]
           [elem (Q-sqrt-element Q-sqrt2 3 2)])
      (= (Q-sqrt-sqrt-part elem) 2))))

;;; ====
;;; Q(√2) Arithmetic Tests
;;; ====

(test-group "Q(√2) Arithmetic"

  (define-test "addition: (1+√2) + (2+3√2) = 3+4√2"
    (let* ([Q-sqrt2 (make-Q-sqrt 2)]
           [a (Q-sqrt-element Q-sqrt2 1 1)]   ; 1 + √2
           [b (Q-sqrt-element Q-sqrt2 2 3)]   ; 2 + 3√2
           [sum ((field-add-op Q-sqrt2) a b)])
      (and (= (Q-sqrt-real-part sum) 3)
           (= (Q-sqrt-sqrt-part sum) 4))))

  (define-test "negation: -(1+√2) = -1-√2"
    (let* ([Q-sqrt2 (make-Q-sqrt 2)]
           [a (Q-sqrt-element Q-sqrt2 1 1)]
           [neg-a ((field-neg-fn Q-sqrt2) a)])
      (and (= (Q-sqrt-real-part neg-a) -1)
           (= (Q-sqrt-sqrt-part neg-a) -1))))

  (define-test "multiplication: (1+√2)(1-√2) = -1"
    ;; (1+√2)(1-√2) = 1 - 2 = -1
    (let* ([Q-sqrt2 (make-Q-sqrt 2)]
           [a (Q-sqrt-element Q-sqrt2 1 1)]   ; 1 + √2
           [b (Q-sqrt-element Q-sqrt2 1 -1)]  ; 1 - √2
           [prod ((field-mul-op Q-sqrt2) a b)])
      (and (= (Q-sqrt-real-part prod) -1)
           (= (Q-sqrt-sqrt-part prod) 0))))

  (define-test "multiplication: (√2)² = 2"
    (let* ([Q-sqrt2 (make-Q-sqrt 2)]
           [sqrt2 (Q-sqrt-element Q-sqrt2 0 1)]  ; √2
           [sq ((field-mul-op Q-sqrt2) sqrt2 sqrt2)])
      (and (= (Q-sqrt-real-part sq) 2)
           (= (Q-sqrt-sqrt-part sq) 0))))

  (define-test "multiplication: (1+√2)² = 3+2√2"
    ;; (1+√2)² = 1 + 2√2 + 2 = 3 + 2√2
    (let* ([Q-sqrt2 (make-Q-sqrt 2)]
           [a (Q-sqrt-element Q-sqrt2 1 1)]
           [sq ((field-mul-op Q-sqrt2) a a)])
      (and (= (Q-sqrt-real-part sq) 3)
           (= (Q-sqrt-sqrt-part sq) 2))))

  (define-test "division: (1+√2)/(1-√2) = -3-2√2"
    ;; (1+√2)/(1-√2) = (1+√2)²/(-1) = -(3+2√2)
    (let* ([Q-sqrt2 (make-Q-sqrt 2)]
           [a (Q-sqrt-element Q-sqrt2 1 1)]
           [b (Q-sqrt-element Q-sqrt2 1 -1)]
           [div ((field-div-fn Q-sqrt2) a b)])
      (and (= (Q-sqrt-real-part div) -3)
           (= (Q-sqrt-sqrt-part div) -2)))))

;;; ====
;;; Q(i) (Gaussian Rationals) Tests
;;; ====

(test-group "Q(i) Gaussian Rationals"

  (define-test "make-Q-i creates extension"
    (let ([Q-i (make-Q-i)])
      (field? Q-i)))

  (define-test "i² = -1"
    (let* ([Q-i (make-Q-i)]
           [i (Q-sqrt-element Q-i 0 1)]  ; i
           [i-sq ((field-mul-op Q-i) i i)])
      (and (= (Q-sqrt-real-part i-sq) -1)
           (= (Q-sqrt-sqrt-part i-sq) 0))))

  (define-test "(1+i)(1-i) = 2"
    (let* ([Q-i (make-Q-i)]
           [a (Q-sqrt-element Q-i 1 1)]   ; 1 + i
           [b (Q-sqrt-element Q-i 1 -1)]  ; 1 - i
           [prod ((field-mul-op Q-i) a b)])
      (and (= (Q-sqrt-real-part prod) 2)
           (= (Q-sqrt-sqrt-part prod) 0))))

  (define-test "1/(1+i) = (1-i)/2"
    ;; 1/(1+i) = (1-i)/((1+i)(1-i)) = (1-i)/2
    (let* ([Q-i (make-Q-i)]
           [one (Q-sqrt-element Q-i 1 0)]
           [a (Q-sqrt-element Q-i 1 1)]
           [inv ((field-div-fn Q-i) one a)])
      (and (= (Q-sqrt-real-part inv) 1/2)
           (= (Q-sqrt-sqrt-part inv) -1/2)))))

;;; ====
;;; Conjugation Tests
;;; ====

(test-group "Conjugation Automorphism"

  (define-test "conjugation of √2 is -√2"
    (let* ([Q-sqrt2 (make-Q-sqrt 2)]
           [conj (make-conjugation Q-sqrt2)]
           [sqrt2 (Q-sqrt-element Q-sqrt2 0 1)]
           [conj-sqrt2 (conj sqrt2)])
      (and (= (Q-sqrt-real-part conj-sqrt2) 0)
           (= (Q-sqrt-sqrt-part conj-sqrt2) -1))))

  (define-test "conjugation of 3+2√2 is 3-2√2"
    (let* ([Q-sqrt2 (make-Q-sqrt 2)]
           [conj (make-conjugation Q-sqrt2)]
           [elem (Q-sqrt-element Q-sqrt2 3 2)]
           [conj-elem (conj elem)])
      (and (= (Q-sqrt-real-part conj-elem) 3)
           (= (Q-sqrt-sqrt-part conj-elem) -2))))

  (define-test "conjugation fixes rational elements"
    (let* ([Q-sqrt2 (make-Q-sqrt 2)]
           [conj (make-conjugation Q-sqrt2)]
           [five (Q-sqrt-element Q-sqrt2 5 0)]
           [conj-five (conj five)])
      (and (= (Q-sqrt-real-part conj-five) 5)
           (= (Q-sqrt-sqrt-part conj-five) 0))))

  (define-test "conjugation is an involution (σ² = id)"
    (let* ([Q-sqrt2 (make-Q-sqrt 2)]
           [conj (make-conjugation Q-sqrt2)]
           [elem (Q-sqrt-element Q-sqrt2 3 2)]
           [double-conj (conj (conj elem))])
      (and (= (Q-sqrt-real-part double-conj) 3)
           (= (Q-sqrt-sqrt-part double-conj) 2)))))

;;; ====
;;; Norm and Trace Tests
;;; ====

(test-group "Norm and Trace"

  (define-test "trace of 3+2√2 is 6"
    (let* ([Q-sqrt2 (make-Q-sqrt 2)]
           [elem (Q-sqrt-element Q-sqrt2 3 2)])
      (= (Q-sqrt-trace elem) 6)))

  (define-test "trace of √2 is 0"
    (let* ([Q-sqrt2 (make-Q-sqrt 2)]
           [sqrt2 (Q-sqrt-element Q-sqrt2 0 1)])
      (= (Q-sqrt-trace sqrt2) 0)))

  (define-test "norm of 1+√2 in Q(√2) is -1"
    ;; N(1+√2) = (1+√2)(1-√2) = 1 - 2 = -1
    (let* ([Q-sqrt2 (make-Q-sqrt 2)]
           [elem (Q-sqrt-element Q-sqrt2 1 1)])
      (= (Q-sqrt-norm elem 2) -1)))

  (define-test "norm of 1+i in Q(i) is 2"
    ;; N(1+i) = (1+i)(1-i) = 1 - (-1) = 2
    (let* ([Q-i (make-Q-i)]
           [elem (Q-sqrt-element Q-i 1 1)])
      (= (Q-sqrt-norm elem -1) 2)))

  (define-test "norm of 3+2√2 is 1"
    ;; N(3+2√2) = 9 - 2×4 = 9 - 8 = 1
    (let* ([Q-sqrt2 (make-Q-sqrt 2)]
           [elem (Q-sqrt-element Q-sqrt2 3 2)])
      (= (Q-sqrt-norm elem 2) 1))))

;;; ====
;;; Galois Group Tests
;;; ====

(test-group "Galois Group"

  (define-test "galois-group-quadratic has 2 elements"
    (let* ([Q-sqrt2 (make-Q-sqrt 2)]
           [G (galois-group-quadratic Q-sqrt2)])
      (= (length G) 2)))

  (define-test "identity automorphism fixes all elements"
    (let* ([Q-sqrt2 (make-Q-sqrt 2)]
           [G (galois-group-quadratic Q-sqrt2)]
           [id (car G)]
           [elem (Q-sqrt-element Q-sqrt2 3 2)]
           [result (id elem)])
      (and (= (Q-sqrt-real-part result) 3)
           (= (Q-sqrt-sqrt-part result) 2))))

  (define-test "non-identity automorphism is conjugation"
    (let* ([Q-sqrt2 (make-Q-sqrt 2)]
           [G (galois-group-quadratic Q-sqrt2)]
           [sigma (cadr G)]
           [elem (Q-sqrt-element Q-sqrt2 3 2)]
           [result (sigma elem)])
      (and (= (Q-sqrt-real-part result) 3)
           (= (Q-sqrt-sqrt-part result) -2)))))

;;; ====
;;; Display Tests
;;; ====

(test-group "Element Display"

  (define-test "display zero"
    (let* ([Q-sqrt2 (make-Q-sqrt 2)]
           [zero (Q-sqrt-element Q-sqrt2 0 0)])
      (string=? (alg-ext->string zero) "0")))

  (define-test "display rational"
    (let* ([Q-sqrt2 (make-Q-sqrt 2)]
           [five (Q-sqrt-element Q-sqrt2 5 0)])
      (string=? (alg-ext->string five) "5")))

  (define-test "display pure sqrt"
    (let* ([Q-sqrt2 (make-Q-sqrt 2)]
           [sqrt2 (Q-sqrt-element Q-sqrt2 0 1)])
      (string=? (alg-ext->string sqrt2) "sqrt2")))

  (define-test "display mixed"
    (let* ([Q-sqrt2 (make-Q-sqrt 2)]
           [elem (Q-sqrt-element Q-sqrt2 3 2)])
      (string=? (alg-ext->string elem) "3 + 2·sqrt2"))))

;;; ====
;;; Splitting Field Tests
;;; ====

(test-group "Splitting Fields"

  (define-test "splitting field of x²-2 is Q(√2)"
    (let* ([poly (make-polynomial Q-field '(-2 0 1))]  ; x² - 2
           [split (splitting-field-quadratic Q-field poly)])
      (= (ext-degree split) 2)))

  (define-test "splitting field of x²-4 is Q (perfect square)"
    ;; x² - 4 = (x-2)(x+2), roots ±2 are in Q
    (let* ([poly (make-polynomial Q-field '(-4 0 1))]
           [split (splitting-field-quadratic Q-field poly)])
      (= (ext-degree split) 1)))

  (define-test "splitting field of x²+1 is Q(i)"
    (let* ([poly (make-polynomial Q-field '(1 0 1))]  ; x² + 1
           [split (splitting-field-quadratic Q-field poly)])
      (= (ext-degree split) 2))))

;;; ====
;;; Run All Tests
;;; ====

(run-all-tests)
