(load "lattice/algebra/field.ss")
(load "lattice/algebra/polynomial.ss")
(load "lattice/numeric/digital-filters.ss")

(doc 'module 'signal-poly)
(doc 'description "Polynomial algebra for signal processing: exact filter analysis, stability analysis, filter simplification, deconvolution")
(doc 'layer 'lattice)
(doc 'purity 'total)

;;; ====
;;; Field Definition for Signal Processing
;;; ====

;;; Q-field-sig : Field
;;; The field of rational numbers for exact filter arithmetic.
(define Q-field-sig
  (make-field
   '()                                    ; Infinite field
   +                                      ; Addition
   *                                      ; Multiplication
   0                                      ; Zero
   1                                      ; One
   -                                      ; Negation
   (lambda (a b) (/ a b))                 ; Division
   =))                                    ; Equality

;;; ====
;;; Algebra Polynomial Operations (Inline to Avoid Name Collision)
;;; ====

;;; Same pattern as poly-algebra.ss - prefix algebra operations with sig-

;;; sig-make-polynomial : Field × (List Coeff) → SigPolynomial
(define (sig-make-polynomial field coeffs)
  (list 'sig-polynomial field (sig-normalize-coeffs field coeffs)))

;;; sig-normalize-coeffs : Field × (List Coeff) → (List Coeff)
(define (sig-normalize-coeffs field coeffs)
  (let loop ([cs (reverse coeffs)])
    (cond
      [(null? cs) '(0)]
      [(= (car cs) 0) (loop (cdr cs))]
      [else (reverse cs)])))

;;; sig-poly-field : SigPolynomial → Field
(define (sig-poly-field p) (cadr p))

;;; sig-poly-coeffs : SigPolynomial → (List Coeff)
(define (sig-poly-coeffs p) (caddr p))

;;; sig-poly-degree : SigPolynomial → Nat
(define (sig-poly-degree p)
  (let ([coeffs (sig-poly-coeffs p)])
    (max 0 (- (length coeffs) 1))))

;;; sig-poly-zero? : SigPolynomial → Boolean
(define (sig-poly-zero? p)
  (let ([coeffs (sig-poly-coeffs p)])
    (and (= (length coeffs) 1)
         (= (car coeffs) 0))))

;;; sig-poly-leading : SigPolynomial → Coeff
(define (sig-poly-leading p)
  (let ([coeffs (sig-poly-coeffs p)])
    (list-ref coeffs (- (length coeffs) 1))))

;;; sig-poly-add : SigPolynomial × SigPolynomial → SigPolynomial
(define (sig-poly-add p1 p2)
  (let* ([F (sig-poly-field p1)]
         [c1 (sig-poly-coeffs p1)]
         [c2 (sig-poly-coeffs p2)]
         [n1 (length c1)]
         [n2 (length c2)]
         [n (max n1 n2)]
         [result (let loop ([i 0] [acc '()])
                   (if (>= i n)
                       (reverse acc)
                       (let ([a (if (< i n1) (list-ref c1 i) 0)]
                             [b (if (< i n2) (list-ref c2 i) 0)])
                         (loop (+ i 1) (cons (+ a b) acc)))))])
    (sig-make-polynomial F result)))

;;; sig-poly-sub : SigPolynomial × SigPolynomial → SigPolynomial
(define (sig-poly-sub p1 p2)
  (let* ([F (sig-poly-field p1)]
         [c1 (sig-poly-coeffs p1)]
         [c2 (sig-poly-coeffs p2)]
         [n1 (length c1)]
         [n2 (length c2)]
         [n (max n1 n2)]
         [result (let loop ([i 0] [acc '()])
                   (if (>= i n)
                       (reverse acc)
                       (let ([a (if (< i n1) (list-ref c1 i) 0)]
                             [b (if (< i n2) (list-ref c2 i) 0)])
                         (loop (+ i 1) (cons (- a b) acc)))))])
    (sig-make-polynomial F result)))

;;; sig-poly-mul : SigPolynomial × SigPolynomial → SigPolynomial
(define (sig-poly-mul p1 p2)
  (let* ([F (sig-poly-field p1)]
         [c1 (sig-poly-coeffs p1)]
         [c2 (sig-poly-coeffs p2)]
         [n1 (length c1)]
         [n2 (length c2)]
         [n (+ n1 n2 -1)]
         [result (make-list n 0)])
    (let loop1 ([i 0] [res result])
      (if (>= i n1)
          (sig-make-polynomial F res)
          (loop1 (+ i 1)
                 (let loop2 ([j 0] [r res])
                   (if (>= j n2)
                       r
                       (loop2 (+ j 1)
                              (list-set r (+ i j)
                                       (+ (list-ref r (+ i j))
                                          (* (list-ref c1 i)
                                             (list-ref c2 j))))))))))))

;;; list-set : (List α) × Nat × α → (List α)
(define (list-set lst idx val)
  (if (= idx 0)
      (cons val (cdr lst))
      (cons (car lst) (list-set (cdr lst) (- idx 1) val))))

;;; sig-poly-scale : SigPolynomial × Coeff → SigPolynomial
(define (sig-poly-scale p c)
  (let* ([F (sig-poly-field p)]
         [coeffs (sig-poly-coeffs p)]
         [scaled (map (lambda (a) (* a c)) coeffs)])
    (sig-make-polynomial F scaled)))

;;; sig-poly-divmod : SigPolynomial × SigPolynomial → (SigPolynomial . SigPolynomial)
(define (sig-poly-divmod p1 p2)
  (let ([F (sig-poly-field p1)])
    (if (sig-poly-zero? p2)
        (error 'sig-poly-divmod "division by zero polynomial")
        (sig-poly-divmod-iter p1 p2 (sig-poly-zero-over F)))))

;;; sig-poly-zero-over : Field → SigPolynomial
(define (sig-poly-zero-over F)
  (sig-make-polynomial F '(0)))

;;; sig-poly-one-over : Field → SigPolynomial
(define (sig-poly-one-over F)
  (sig-make-polynomial F '(1)))

;;; sig-poly-divmod-iter : SigPolynomial × SigPolynomial × SigPolynomial → (SigPolynomial . SigPolynomial)
(define (sig-poly-divmod-iter r d q)
  (let ([dr (sig-poly-degree r)]
        [dd (sig-poly-degree d)])
    (if (or (< dr dd) (sig-poly-zero? r))
        (cons q r)
        (let* ([F (sig-poly-field r)]
               [lr (sig-poly-leading r)]
               [ld (sig-poly-leading d)]
               [c (/ lr ld)]
               [deg-diff (- dr dd)]
               [term-coeffs (append (make-list deg-diff 0) (list c))]
               [term (sig-make-polynomial F term-coeffs)]
               [new-q (sig-poly-add q term)]
               [sub (sig-poly-mul term d)]
               [new-r (sig-poly-sub r sub)])
          (sig-poly-divmod-iter new-r d new-q)))))

;;; sig-poly-div : SigPolynomial × SigPolynomial → SigPolynomial
(define (sig-poly-div p1 p2)
  (car (sig-poly-divmod p1 p2)))

;;; sig-poly-mod : SigPolynomial × SigPolynomial → SigPolynomial
(define (sig-poly-mod p1 p2)
  (cdr (sig-poly-divmod p1 p2)))

;;; sig-poly-gcd : SigPolynomial × SigPolynomial → SigPolynomial
(define (sig-poly-gcd p1 p2)
  (if (sig-poly-zero? p2)
      (sig-poly-make-monic p1)
      (sig-poly-gcd p2 (sig-poly-mod p1 p2))))

;;; sig-poly-make-monic : SigPolynomial → SigPolynomial
(define (sig-poly-make-monic p)
  (if (sig-poly-zero? p)
      p
      (let* ([lc (sig-poly-leading p)]
             [inv-lc (/ 1 lc)])
        (sig-poly-scale p inv-lc))))

;;; ====
;;; Conversion Between Numeric and Signal Polynomials
;;; ====

;;; Access numeric polynomial coefficients (vector, descending order)
(define (num-poly-coeffs-sig p) (vector->list (cdr p)))

;;; numeric->signal-poly : NumericPoly → SigPolynomial
;;; Convert from numeric polynomial (descending vector) to signal polynomial (ascending list).
(define (numeric->signal-poly p)
  (let* ([coeffs (num-poly-coeffs-sig p)]
         [ascending (reverse coeffs)])
    (sig-make-polynomial Q-field-sig ascending)))

;;; signal->numeric-poly : SigPolynomial → NumericPoly
;;; Convert signal polynomial to numeric polynomial.
(define (signal->numeric-poly p)
  (let* ([coeffs (sig-poly-coeffs p)]
         [descending (reverse coeffs)]
         [vec (list->vector descending)])
    (cons 'polynomial vec)))

;;; ====
;;; Filter Polynomial Analysis
;;; ====

;;; filter-num-poly : IIR-Filter → SigPolynomial
;;; Get filter numerator as signal polynomial.
(define (filter-num-poly f)
  (let* ([b (iir-filter-b f)]
         [coeffs (vector->list b)])
    ;; IIR filter coefficients are already in order [b0, b1, ...]
    ;; which is descending powers of z^-1, equivalent to ascending powers of z
    (sig-make-polynomial Q-field-sig coeffs)))

;;; filter-den-poly : IIR-Filter → SigPolynomial
;;; Get filter denominator as signal polynomial.
(define (filter-den-poly f)
  (let* ([a (iir-filter-a f)]
         [coeffs (vector->list a)])
    (sig-make-polynomial Q-field-sig coeffs)))

;;; filter-coprime? : IIR-Filter → Boolean
;;; Check if numerator and denominator share common factors.
;;; Returns #t if filter is already minimal (no pole-zero cancellation possible).
(define (filter-coprime? f)
  (let* ([num (filter-num-poly f)]
         [den (filter-den-poly f)]
         [gcd-poly (sig-poly-gcd num den)])
    (<= (sig-poly-degree gcd-poly) 0)))

;;; filter-simplify : IIR-Filter → IIR-Filter
;;; Simplify filter by canceling common numerator/denominator factors.
(define (filter-simplify f)
  (let* ([num (filter-num-poly f)]
         [den (filter-den-poly f)]
         [gcd-poly (sig-poly-gcd num den)])
    (if (<= (sig-poly-degree gcd-poly) 0)
        f  ; Already simplified
        (let* ([num-simplified (sig-poly-div num gcd-poly)]
               [den-simplified (sig-poly-div den gcd-poly)]
               [b-coeffs (sig-poly-coeffs num-simplified)]
               [a-coeffs (sig-poly-coeffs den-simplified)])
          (make-iir-filter (list->vector b-coeffs)
                          (list->vector a-coeffs))))))

;;; ====
;;; Filter Stability Analysis
;;; ====

;;; For a digital filter to be stable, all poles must be inside the unit circle.
;;; Since we work with polynomial coefficients (not roots directly), we use
;;; the Jury stability test which is analogous to Routh-Hurwitz for discrete systems.

;;; jury-table : SigPolynomial → (List (List Number))
;;; Construct Jury table for stability analysis.
;;; Input polynomial should have positive leading coefficient.
(define (jury-table poly)
  (let* ([coeffs (sig-poly-coeffs poly)]
         [n (length coeffs)])
    (if (< n 2)
        (list coeffs)
        (jury-table-iter (list coeffs (reverse coeffs)) (- n 1)))))

;;; jury-table-iter : (List (List Number)) × Nat → (List (List Number))
(define (jury-table-iter rows remaining)
  (if (<= remaining 1)
      (reverse rows)
      (let* ([row1 (car rows)]
             [row2 (cadr rows)]
             [a0 (car row1)]
             [an (car row2)]
             [n (length row1)])
        (if (= a0 0)
            (reverse rows)  ; Degenerate case
            (let* ([new-row (jury-new-row row1 row2 a0 an)]
                   [new-row-rev (reverse new-row)])
              (jury-table-iter (cons new-row-rev (cons new-row rows))
                              (- remaining 1)))))))

;;; jury-new-row : (List Number) × (List Number) × Number × Number → (List Number)
(define (jury-new-row row1 row2 a0 an)
  (let ([n (length row1)])
    (let loop ([i 0] [acc '()])
      (if (>= i (- n 1))
          (reverse acc)
          (let* ([bi (list-ref row1 i)]
                 [bn-i (list-ref row2 (+ i 1))]
                 [val (- (* a0 bi) (* an bn-i))])
            (loop (+ i 1) (cons val acc)))))))

;;; filter-stable? : IIR-Filter → Boolean
;;; Check if digital filter is stable using Jury criterion.
;;; A discrete-time system is stable if all poles are inside unit circle.
(define (filter-stable? f)
  (let* ([den (filter-den-poly f)]
         [coeffs (sig-poly-coeffs den)]
         [len (length coeffs)]
         [deg (- len 1)]                 ; Polynomial degree
         [an (list-ref coeffs (- len 1))]  ; Leading coefficient
         [a0 (car coeffs)])              ; Constant term
    (cond
      [(= len 1) #t]  ; Constant polynomial - stable
      [(= len 2)
       ;; Linear: a0 + a1*z, pole at -a0/a1
       ;; Stable if |a0/a1| < 1, i.e., |a0| < |a1|
       (< (abs a0) (abs an))]
      [else
       ;; General case: check necessary Jury conditions
       ;; 1. P(1) > 0 (no pole at z=1)
       ;; 2. (-1)^deg * P(-1) > 0 (no pole at z=-1)
       ;; 3. |a0| < |an| (necessary for all poles inside unit circle)
       (let ([p1 (poly-eval-at-1 coeffs)]
             [pn1 (poly-eval-at-neg1 coeffs deg)])  ; Use degree, not length
         (and (> p1 0)
              (> pn1 0)
              (< (abs a0) (abs an))))])))

;;; poly-eval-at-1 : (List Number) → Number
;;; Evaluate polynomial at z = 1.
(define (poly-eval-at-1 coeffs)
  (apply + coeffs))

;;; poly-eval-at-neg1 : (List Number) × Nat → Number
;;; Evaluate (-1)^n * P(-1).
(define (poly-eval-at-neg1 coeffs n)
  (let ([pn1 (let loop ([cs coeffs] [sign 1] [sum 0])
               (if (null? cs)
                   sum
                   (loop (cdr cs) (- sign) (+ sum (* sign (car cs))))))])
    (if (even? n) pn1 (- pn1))))

;;; jury-first-column-positive? : SigPolynomial → Boolean
;;; Check if all first column elements of Jury table are positive.
(define (jury-first-column-positive? poly)
  (let* ([table (jury-table poly)]
         [first-col (map car table)])
    (or (null? first-col)
        (let ([sign (if (> (car first-col) 0) 1 -1)])
          (andmap (lambda (x) (> (* x sign) 0)) first-col)))))

;;; andmap : (α → Boolean) × (List α) → Boolean
(define (andmap pred lst)
  (or (null? lst)
      (and (pred (car lst))
           (andmap pred (cdr lst)))))

;;; ====
;;; Deconvolution via Polynomial Division
;;; ====

;;; deconvolve : Vector × Vector → (Vector . Vector)
;;; Deconvolution: given y = conv(x, h), find x given y and h.
;;; Returns (quotient . remainder).
;;; This is polynomial division in the signal domain.
(define (deconvolve y h)
  (let* ([y-poly (sig-make-polynomial Q-field-sig (vector->list y))]
         [h-poly (sig-make-polynomial Q-field-sig (vector->list h))]
         [qr (sig-poly-divmod y-poly h-poly)]
         [q-coeffs (sig-poly-coeffs (car qr))]
         [r-coeffs (sig-poly-coeffs (cdr qr))])
    (cons (list->vector q-coeffs)
          (list->vector r-coeffs))))

;;; deconvolve-exact? : Vector × Vector × Vector → Boolean
;;; Check if deconvolution is exact (no remainder).
(define (deconvolve-exact? y h x)
  (let* ([result (deconvolve y h)]
         [remainder (cdr result)])
    (vector-all-zero? remainder)))

;;; vector-all-zero? : Vector → Boolean
(define (vector-all-zero? v)
  (let ([n (vector-length v)])
    (let loop ([i 0])
      (or (>= i n)
          (and (< (abs (vector-ref v i)) 1e-10)
               (loop (+ i 1)))))))

;;; ====
;;; Filter Cascade/Parallel Combination
;;; ====

;;; filter-cascade : IIR-Filter × IIR-Filter → IIR-Filter
;;; Cascade (series) combination of two filters.
;;; H_total(z) = H1(z) * H2(z)
;;; Numerator = B1 * B2, Denominator = A1 * A2
(define (filter-cascade f1 f2)
  (let* ([num1 (filter-num-poly f1)]
         [den1 (filter-den-poly f1)]
         [num2 (filter-num-poly f2)]
         [den2 (filter-den-poly f2)]
         [num-total (sig-poly-mul num1 num2)]
         [den-total (sig-poly-mul den1 den2)]
         [b-coeffs (sig-poly-coeffs num-total)]
         [a-coeffs (sig-poly-coeffs den-total)])
    (make-iir-filter (list->vector b-coeffs)
                    (list->vector a-coeffs))))

;;; filter-parallel : IIR-Filter × IIR-Filter → IIR-Filter
;;; Parallel combination of two filters.
;;; H_total(z) = H1(z) + H2(z) = (B1*A2 + B2*A1) / (A1*A2)
(define (filter-parallel f1 f2)
  (let* ([num1 (filter-num-poly f1)]
         [den1 (filter-den-poly f1)]
         [num2 (filter-num-poly f2)]
         [den2 (filter-den-poly f2)]
         [num-total (sig-poly-add (sig-poly-mul num1 den2)
                                  (sig-poly-mul num2 den1))]
         [den-total (sig-poly-mul den1 den2)]
         [b-coeffs (sig-poly-coeffs num-total)]
         [a-coeffs (sig-poly-coeffs den-total)])
    (make-iir-filter (list->vector b-coeffs)
                    (list->vector a-coeffs))))

;;; ====
;;; Display Utilities
;;; ====

;;; sig-poly->string : SigPolynomial × Symbol → String
;;; Convert polynomial to string representation.
(define (sig-poly->string p var)
  (let* ([coeffs (sig-poly-coeffs p)]
         [n (length coeffs)]
         [var-str (symbol->string var)])
    (if (= n 0)
        "0"
        (let loop ([i 0] [first? #t] [acc ""])
          (if (>= i n)
              (if (string=? acc "") "0" acc)
              (let ([c (list-ref coeffs i)])
                (if (= c 0)
                    (loop (+ i 1) first? acc)
                    (let* ([sign (if (< c 0) " - " (if first? "" " + "))]
                           [abs-c (abs c)]
                           [coeff-str (if (and (= abs-c 1) (> i 0)) "" (number->string abs-c))]
                           [power-str (cond [(= i 0) ""]
                                           [(= i 1) var-str]
                                           [else (string-append var-str "^" (number->string i))])]
                           [term (string-append sign coeff-str
                                               (if (and (not (string=? coeff-str ""))
                                                        (not (string=? power-str "")))
                                                   "*" "")
                                               power-str)])
                      (loop (+ i 1) #f (string-append acc term))))))))))

;;; filter->string : IIR-Filter → String
;;; Display filter transfer function.
(define (filter->string f)
  (let* ([num (filter-num-poly f)]
         [den (filter-den-poly f)])
    (string-append "H(z) = (" (sig-poly->string num 'z) ") / ("
                   (sig-poly->string den 'z) ")")))
