;;; lattice/algebra/test-poly-bridge.ss — Tests for Polynomial Bridge
;;;
;;; Tests for the unified polynomial bridge module.

(load "core/testing/test-framework.ss")

;;; IMPORTANT: Load numeric/polynomial.ss BEFORE poly-bridge.ss
;;; This is the recommended load order to avoid name collisions.
(unless (top-level-bound? 'doc) (load "core/base/prelude.ss"))
(load "lattice/numeric/polynomial.ss")
(load "lattice/algebra/poly-bridge.ss")

;;; ====
;;; Test: Conversion Functions
;;; ====

;;; Helper to extract coefficients from numeric polynomial
;;; Numeric poly is (poly #(...)) - coeffs in cadr
(define (num-poly-vec p) (cadr p))

(test-group "conversion"
  (define-test "numeric to algebra conversion"
    ;; Numeric: 3x^2 + 2x + 1 stored as #(3 2 1) (descending)
    (let* ([num-poly (poly-from-list '(3 2 1))]
           [alg-poly (numeric->algebra num-poly)]
           [coeffs (alg-coeffs alg-poly)])
      ;; Algebra should be ascending: (1 2 3)
      (assert-equal coeffs '(1 2 3))))

  (define-test "algebra to numeric conversion"
    ;; Algebra: 1 + 2x + 3x^2 stored as (1 2 3) (ascending)
    (let* ([alg-poly (alg-make Q-field '(1 2 3))]
           [num-poly (algebra->numeric alg-poly)]
           [coeffs (vector->list (num-poly-vec num-poly))])
      ;; Numeric should be descending: (3 2 1)
      (assert-equal coeffs '(3 2 1))))

  (define-test "round-trip numeric->algebra->numeric"
    (let* ([original (poly-from-list '(5 -3 0 2))]
           [alg-poly (numeric->algebra original)]
           [back (algebra->numeric alg-poly)]
           ;; Use vector length - 1 for degree
           [orig-deg (- (vector-length (num-poly-vec original)) 1)]
           [back-deg (- (vector-length (num-poly-vec back)) 1)])
      (assert-equal orig-deg back-deg)))

  (define-test "round-trip algebra->numeric->algebra"
    (let* ([original (alg-make Q-field '(2 0 -3 5))]
           [num-poly (algebra->numeric original)]
           [back (numeric->algebra num-poly)]
           [orig-deg (alg-degree original)]
           [back-deg (alg-degree back)])
      (assert-equal orig-deg back-deg)))

  (define-test "zero polynomial conversion"
    ;; Zero polynomial: critical edge case
    (let* ([z-num (poly-from-list '(0))]
           [z-alg (numeric->algebra z-num)]
           [z-back (algebra->numeric z-alg)])
      (assert-equal (alg-coeffs z-alg) '(0))
      (assert-equal (vector->list (num-poly-vec z-back)) '(0)))))

;;; ====
;;; Test: Prefixed Operations
;;; ====

(test-group "prefixed-operations"
  (define-test "alg-add works correctly"
    (let* ([p1 (alg-make Q-field '(1 2))]      ; 1 + 2x
           [p2 (alg-make Q-field '(3 4))]      ; 3 + 4x
           [sum (alg-add p1 p2)]
           [coeffs (alg-coeffs sum)])
      (assert-equal coeffs '(4 6))))  ; 4 + 6x

  (define-test "alg-mul works correctly"
    (let* ([p1 (alg-make Q-field '(1 1))]      ; 1 + x
           [p2 (alg-make Q-field '(1 1))]      ; 1 + x
           [prod (alg-mul p1 p2)]
           [coeffs (alg-coeffs prod)])
      ;; (1+x)^2 = 1 + 2x + x^2
      (assert-equal coeffs '(1 2 1))))

  (define-test "alg-divmod works correctly"
    (let* ([p1 (alg-make Q-field '(1 2 1))]    ; 1 + 2x + x^2 = (1+x)^2
           [p2 (alg-make Q-field '(1 1))]      ; 1 + x
           [qr (alg-divmod p1 p2)]
           [quot-deg (alg-degree (car qr))]
           [rem-deg (alg-degree (cdr qr))])
      ;; (1+x)^2 / (1+x) = (1+x) with remainder 0
      (assert-equal quot-deg 1)
      (assert-true (alg-zero? (cdr qr)))))

  (define-test "alg-gcd finds common factor"
    (let* ([p1 (alg-make Q-field '(-1 0 1))]   ; -1 + x^2 = (x-1)(x+1)
           [p2 (alg-make Q-field '(-1 1))]     ; -1 + x = (x-1)
           [gcd-poly (alg-gcd p1 p2)]
           [deg (alg-degree gcd-poly)])
      ;; GCD should be (x-1), degree 1
      (assert-equal deg 1))))

;;; ====
;;; Test: Bridge Operations
;;; ====

(test-group "bridge-operations"
  (define-test "bridge-gcd on numeric polynomials"
    ;; p1 = x^2 - 1 = (x-1)(x+1), p2 = x - 1
    (let* ([p1 (poly-from-list '(1 0 -1))]
           [p2 (poly-from-list '(1 -1))]
           [gcd-poly (bridge-gcd p1 p2)])
      ;; GCD should be (x-1), degree 1
      (assert-equal (- (vector-length (num-poly-vec gcd-poly)) 1) 1)))

  (define-test "bridge-divmod on numeric polynomials"
    ;; x^3 / x = x^2 with remainder 0
    (let* ([p1 (poly-from-list '(1 0 0 0))]    ; x^3
           [p2 (poly-from-list '(1 0))]        ; x
           [qr (bridge-divmod p1 p2)]
           [quot (car qr)]
           [rem (cdr qr)])
      (assert-equal (- (vector-length (num-poly-vec quot)) 1) 2)
      (assert-equal (- (vector-length (num-poly-vec rem)) 1) 0)))

  (define-test "bridge-simplify cancels common factors"
    ;; (x^2 - 1) / (x - 1) = (x+1)(x-1)/(x-1) should simplify to (x+1)
    (let* ([num (poly-from-list '(1 0 -1))]    ; x^2 - 1
           [den (poly-from-list '(1 -1))]       ; x - 1
           [simplified (bridge-simplify num den)]
           [num-simp (car simplified)]
           [den-simp (cdr simplified)])
      (assert-equal (- (vector-length (num-poly-vec num-simp)) 1) 1)
      (assert-equal (- (vector-length (num-poly-vec den-simp)) 1) 0)))

  (define-test "bridge-coprime? detects coprime polynomials"
    (let* ([p1 (poly-from-list '(1 1))]        ; x + 1
           [p2 (poly-from-list '(1 2))])        ; x + 2
      (assert-true (bridge-coprime? p1 p2))))

  (define-test "bridge-coprime? detects non-coprime polynomials"
    (let* ([p1 (poly-from-list '(1 0 -1))]     ; x^2 - 1
           [p2 (poly-from-list '(1 -1))])       ; x - 1
      (assert-false (bridge-coprime? p1 p2))))

  (define-test "bridge-simplify preserves coprime polynomials"
    ;; x and (x+1) are coprime - should remain unchanged
    (let* ([num (poly-from-list '(1 0))]        ; x
           [den (poly-from-list '(1 1))]         ; x + 1
           [simplified (bridge-simplify num den)]
           [num-simp (car simplified)]
           [den-simp (cdr simplified)])
      ;; Degrees should remain the same (degree 1 each)
      (assert-equal (- (vector-length (num-poly-vec num-simp)) 1) 1)
      (assert-equal (- (vector-length (num-poly-vec den-simp)) 1) 1))))

;;; ====
;;; Test: Factory Functions
;;; ====

(test-group "factory-functions"
  (define-test "make-numeric-from-roots"
    ;; Roots at 1 and -1: (x-1)(x+1) = x^2 - 1
    (let* ([p (make-numeric-from-roots '(1 -1))]
           [deg (- (vector-length (num-poly-vec p)) 1)])
      (assert-equal deg 2)))

  (define-test "make-numeric-from-ascending"
    ;; Coeffs (1 2 3) ascending should give 3x^2 + 2x + 1 in descending
    (let* ([p (make-numeric-from-ascending '(1 2 3))]
           [coeffs (vector->list (num-poly-vec p))])
      (assert-equal coeffs '(3 2 1))))

  (define-test "make-algebra-from-descending"
    ;; Coeffs (3 2 1) descending should give (1 2 3) ascending
    (let* ([p (make-algebra-from-descending Q-field '(3 2 1))]
           [coeffs (alg-coeffs p)])
      (assert-equal coeffs '(1 2 3)))))

;;; ====
;;; Test: String Conversion
;;; ====

(test-group "string-conversion"
  (define-test "alg->string produces output"
    (let* ([p (alg-make Q-field '(1 2 3))]
           [str (alg->string p 'x)])
      (assert-true (string? str))
      (assert-true (> (string-length str) 0))))

  (define-test "numeric->string produces output"
    (let* ([p (poly-from-list '(3 2 1))]
           [str (numeric->string p 'x)])
      (assert-true (string? str))
      (assert-true (> (string-length str) 0)))))

;;; ====
;;; Test: Compatibility Aliases
;;; ====

(test-group "compatibility-aliases"
  (define-test "sig-* aliases work"
    (let* ([p1 (sig-make-polynomial Q-field '(1 2))]
           [p2 (sig-make-polynomial Q-field '(3 4))]
           [sum (sig-poly-add p1 p2)]
           [coeffs (sig-poly-coeffs sum)])
      (assert-equal coeffs '(4 6))))

  (define-test "sig-poly-gcd works"
    (let* ([p1 (sig-make-polynomial Q-field '(-1 0 1))]
           [p2 (sig-make-polynomial Q-field '(-1 1))]
           [gcd-poly (sig-poly-gcd p1 p2)])
      (assert-equal (sig-poly-degree gcd-poly) 1))))

;;; ====
;;; Run All Tests
;;; ====

(run-all-tests)
