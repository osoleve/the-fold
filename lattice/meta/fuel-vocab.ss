;;; lattice/meta/fuel-vocab.ss — Structured Fuel-Cost Vocabulary
;;; @module meta/fuel-vocab
;;; @requires prelude

(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
(require 'prelude)

(doc 'module 'meta/fuel-vocab)
(doc 'description "Structured vocabulary for fuel-cost annotations in doc forms.
Replaces free-text fuel-bound strings in manifests and ad-hoc cost comments
with machine-readable S-expression forms that can be validated, compared,
and used for budget estimation.

Fuel-cost forms:
  (constant k)        - Fixed cost independent of input size
  (linear n)          - O(n) in parameter n
  (quadratic n)       - O(n^2)
  (cubic n)           - O(n^3)
  (polynomial n k)    - O(n^k) for explicit degree k
  (exponential n)     - O(2^n)
  (logarithmic n)     - O(log n)
  (log-linear n)      - O(n log n)
  (product c1 c2)     - Product of two costs (e.g. matrix-mul on m x n * n x p)
  (max c1 c2)         - Maximum of two costs (branching)
  (sum c1 c2)         - Sum of two costs (sequential)")
(doc 'layer 'lattice)
(doc 'purity 'total)

;;; ====
;;; Vocabulary Tags
;;; ====

(doc 'section 'valid-tags)

(define *fuel-cost-tags*
  '(constant linear quadratic cubic polynomial exponential
    logarithmic log-linear product max sum))

;;; ====
;;; Validation
;;; ====

(doc 'section 'validation)

(doc fuel-cost-valid? 'type '(-> Any Bool))
(doc fuel-cost-valid? 'description "Validate a fuel-cost annotation form.
Returns #t if the form is a well-formed fuel-cost S-expression.")
(doc fuel-cost-valid? 'export #t)
(define (fuel-cost-valid? form)
  (and (pair? form)
       (symbol? (car form))
       (case (car form)
         [(constant)
          (and (= (length form) 2)
               (number? (cadr form))
               (>= (cadr form) 0))]
         [(linear quadratic cubic exponential logarithmic log-linear)
          (and (= (length form) 2)
               (symbol? (cadr form)))]
         [(polynomial)
          (and (= (length form) 3)
               (symbol? (cadr form))
               (number? (caddr form))
               (>= (caddr form) 0))]
         [(product max sum)
          (and (= (length form) 3)
               (fuel-cost-valid? (cadr form))
               (fuel-cost-valid? (caddr form)))]
         [else #f])))

;;; ====
;;; Ordering (for comparison)
;;; ====

(doc 'section 'comparison)

;;; fuel-cost-class-rank : Symbol -> Nat
;;; Maps complexity class tags to ordinal ranks for comparison.
;;; Lower rank = lower asymptotic complexity.
(define (fuel-cost-class-rank tag)
  (case tag
    [(constant)     0]
    [(logarithmic)  1]
    [(linear)       2]
    [(log-linear)   3]
    [(quadratic)    4]
    [(cubic)        5]
    [(polynomial)   6]
    [(exponential)  7]
    [else           8]))

(doc fuel-cost-compare 'type '(-> FuelCost FuelCost (Union -1 0 1)))
(doc fuel-cost-compare 'description "Compare two fuel-cost annotations by asymptotic complexity.
Returns -1 if a < b, 0 if same class, 1 if a > b.
For compound forms (product, max, sum), compares the dominant term.
For polynomial forms, compares by degree.")
(doc fuel-cost-compare 'export #t)
(define (fuel-cost-compare a b)
  (let ([ra (fuel-cost-dominant-rank a)]
        [rb (fuel-cost-dominant-rank b)])
    (cond
      [(< ra rb) -1]
      [(> ra rb)  1]
      [else
       ;; Same class -- compare constants or polynomial degrees
       (fuel-cost-compare-within-class a b)])))

;;; fuel-cost-dominant-rank : FuelCost -> Nat
;;; Extract the dominant asymptotic rank from a fuel-cost form,
;;; recursing into compound forms.
(define (fuel-cost-dominant-rank form)
  (case (car form)
    [(constant linear quadratic cubic exponential logarithmic log-linear)
     (fuel-cost-class-rank (car form))]
    [(polynomial)
     ;; Polynomial sits between cubic and exponential depending on degree
     (let ([k (caddr form)])
       (cond
         [(<= k 1) 2]   ; O(n^1) = linear
         [(= k 2)  4]   ; O(n^2) = quadratic
         [(= k 3)  5]   ; O(n^3) = cubic
         [else      6]))] ; O(n^k) for k>3 = polynomial
    [(product)
     (max (fuel-cost-dominant-rank (cadr form))
          (fuel-cost-dominant-rank (caddr form)))]
    [(max)
     (max (fuel-cost-dominant-rank (cadr form))
          (fuel-cost-dominant-rank (caddr form)))]
    [(sum)
     (max (fuel-cost-dominant-rank (cadr form))
          (fuel-cost-dominant-rank (caddr form)))]
    [else 8]))

;;; fuel-cost-compare-within-class : FuelCost -> FuelCost -> (Union -1 0 1)
;;; When two costs have the same asymptotic rank, compare within-class details.
(define (fuel-cost-compare-within-class a b)
  (cond
    ;; Both constant: compare the constant values
    [(and (eq? (car a) 'constant) (eq? (car b) 'constant))
     (cond
       [(< (cadr a) (cadr b)) -1]
       [(> (cadr a) (cadr b))  1]
       [else 0])]
    ;; Both polynomial with same dominant rank: compare degrees
    [(and (eq? (car a) 'polynomial) (eq? (car b) 'polynomial))
     (cond
       [(< (caddr a) (caddr b)) -1]
       [(> (caddr a) (caddr b))  1]
       [else 0])]
    ;; Otherwise: same class, call them equal
    [else 0]))

;;; ====
;;; Display
;;; ====

(doc 'section 'display)

(doc fuel-cost->string 'type '(-> FuelCost String))
(doc fuel-cost->string 'description "Convert a fuel-cost annotation to a human-readable string.
Examples: (constant 50) -> \"O(1) [50 fuel]\", (cubic n) -> \"O(n^3)\"")
(doc fuel-cost->string 'export #t)
(define (fuel-cost->string form)
  (case (car form)
    [(constant)
     (string-append "O(1) [" (number->string (cadr form)) " fuel]")]
    [(linear)
     (string-append "O(" (symbol->string (cadr form)) ")")]
    [(quadratic)
     (string-append "O(" (symbol->string (cadr form)) "^2)")]
    [(cubic)
     (string-append "O(" (symbol->string (cadr form)) "^3)")]
    [(polynomial)
     (string-append "O(" (symbol->string (cadr form))
                    "^" (number->string (caddr form)) ")")]
    [(exponential)
     (string-append "O(2^" (symbol->string (cadr form)) ")")]
    [(logarithmic)
     (string-append "O(log " (symbol->string (cadr form)) ")")]
    [(log-linear)
     (string-append "O(" (symbol->string (cadr form))
                    " log " (symbol->string (cadr form)) ")")]
    [(product)
     (string-append (fuel-cost->string (cadr form))
                    " * " (fuel-cost->string (caddr form)))]
    [(max)
     (string-append "max(" (fuel-cost->string (cadr form))
                    ", " (fuel-cost->string (caddr form)) ")")]
    [(sum)
     (string-append (fuel-cost->string (cadr form))
                    " + " (fuel-cost->string (caddr form)))]
    [else "???"]))

;;; ====
;;; Estimation
;;; ====

(doc 'section 'estimation)

(doc estimate-fuel 'type '(-> FuelCost Nat Nat))
(doc estimate-fuel 'description "Estimate fuel units from a fuel-cost annotation and input size n.
Returns an approximate integer fuel count. For compound forms, recurses.
For forms with a constant multiplier of 1 (i.e., we assume unit-coefficient
big-O), the estimate is the raw complexity function applied to n.")
(doc estimate-fuel 'export #t)
(define (estimate-fuel form n)
  (case (car form)
    [(constant)     (cadr form)]
    [(linear)       n]
    [(quadratic)    (* n n)]
    [(cubic)        (* n n n)]
    [(polynomial)   (expt n (caddr form))]
    [(exponential)  (expt 2 n)]
    [(logarithmic)  (if (<= n 1) 1 (ceiling (log n)))]
    [(log-linear)   (if (<= n 1) 1 (* n (ceiling (log n))))]
    [(product)      (* (estimate-fuel (cadr form) n)
                       (estimate-fuel (caddr form) n))]
    [(max)          (max (estimate-fuel (cadr form) n)
                         (estimate-fuel (caddr form) n))]
    [(sum)          (+ (estimate-fuel (cadr form) n)
                       (estimate-fuel (caddr form) n))]
    [else           0]))

;;; ====
;;; Convenience: fuel-cost form constructors
;;; ====

(doc 'section 'constructors)

(doc fuel/constant 'type '(-> Nat FuelCost))
(doc fuel/constant 'description "Construct a constant fuel-cost form")
(doc fuel/constant 'export #t)
(define (fuel/constant k) (list 'constant k))

(doc fuel/linear 'type '(-> Symbol FuelCost))
(doc fuel/linear 'description "Construct a linear fuel-cost form")
(doc fuel/linear 'export #t)
(define (fuel/linear var) (list 'linear var))

(doc fuel/quadratic 'type '(-> Symbol FuelCost))
(doc fuel/quadratic 'description "Construct a quadratic fuel-cost form")
(doc fuel/quadratic 'export #t)
(define (fuel/quadratic var) (list 'quadratic var))

(doc fuel/cubic 'type '(-> Symbol FuelCost))
(doc fuel/cubic 'description "Construct a cubic fuel-cost form")
(doc fuel/cubic 'export #t)
(define (fuel/cubic var) (list 'cubic var))

(doc fuel/polynomial 'type '(-> Symbol Nat FuelCost))
(doc fuel/polynomial 'description "Construct a polynomial fuel-cost form with explicit degree")
(doc fuel/polynomial 'export #t)
(define (fuel/polynomial var k) (list 'polynomial var k))

(doc fuel/exponential 'type '(-> Symbol FuelCost))
(doc fuel/exponential 'description "Construct an exponential fuel-cost form")
(doc fuel/exponential 'export #t)
(define (fuel/exponential var) (list 'exponential var))

(doc fuel/logarithmic 'type '(-> Symbol FuelCost))
(doc fuel/logarithmic 'description "Construct a logarithmic fuel-cost form")
(doc fuel/logarithmic 'export #t)
(define (fuel/logarithmic var) (list 'logarithmic var))

(doc fuel/log-linear 'type '(-> Symbol FuelCost))
(doc fuel/log-linear 'description "Construct a log-linear fuel-cost form")
(doc fuel/log-linear 'export #t)
(define (fuel/log-linear var) (list 'log-linear var))

(doc fuel/product 'type '(-> FuelCost FuelCost FuelCost))
(doc fuel/product 'description "Construct a product of two fuel-cost forms")
(doc fuel/product 'export #t)
(define (fuel/product a b) (list 'product a b))

(doc fuel/max 'type '(-> FuelCost FuelCost FuelCost))
(doc fuel/max 'description "Construct a max of two fuel-cost forms (branching)")
(doc fuel/max 'export #t)
(define (fuel/max a b) (list 'max a b))

(doc fuel/sum 'type '(-> FuelCost FuelCost FuelCost))
(doc fuel/sum 'description "Construct a sum of two fuel-cost forms (sequential)")
(doc fuel/sum 'export #t)
(define (fuel/sum a b) (list 'sum a b))
