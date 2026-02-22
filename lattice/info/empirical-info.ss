(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
;;; @module empirical-info
;;; @requires prelude entropy

(require 'prelude)
(require 'entropy)

(doc 'module 'empirical-info)
(doc 'description "Empirical information-theoretic measures from raw sample data")
(doc 'layer 'lattice)
(doc 'purity 'partial)

(doc 'note "Bridges info/ to statistics/ by computing entropy and mutual information
  directly from sample vectors, without requiring pre-computed probability distributions.
  Uses uniform binning for continuous data discretization.")

(doc 'section 'discretization)

(define (discretize-uniform xs num-bins)
  (doc 'export #t)
  (doc 'type '(-> (List Number) Nat (List Nat)))
  (doc 'description "Discretize continuous values into uniform bins.
   Returns list of bin indices (0 to num-bins-1).")
  (if (or (null? xs) (<= num-bins 0))
      '()
      (let* ([lo (apply min xs)]
             [hi (apply max xs)]
             [range (- hi lo)])
        (if (<= range 0)
            ;; All values identical: assign to bin 0
            (map (lambda (_) 0) xs)
            (let ([bin-width (/ range num-bins)])
              (map (lambda (x)
                     (let ([b (inexact->exact (floor (/ (- x lo) bin-width)))])
                       ;; Clamp to [0, num-bins-1]: the max value falls exactly on boundary
                       (min b (- num-bins 1))))
                   xs))))))

(define (default-num-bins n)
  (doc 'export #t)
  (doc 'type '(-> Nat Nat))
  (doc 'description "Sturges' rule for default bin count: ceil(1 + log2(n))")
  (if (<= n 1)
      1
      (inexact->exact (ceiling (+ 1 (log2 n))))))

(doc 'section 'empirical-entropy)

(define (entropy-empirical xs . opts)
  (doc 'export #t)
  (doc 'type '(-> (List Number) [Nat] Number))
  (doc 'description "Estimate Shannon entropy from continuous sample data.
   Discretizes via uniform binning then computes plug-in entropy.
   Optional second argument specifies number of bins (default: Sturges' rule).")
  (if (null? xs)
      0
      (let* ([num-bins (if (and (pair? opts) (integer? (car opts)))
                           (car opts)
                           (default-num-bins (length xs)))]
             [bins (discretize-uniform xs num-bins)])
        (sample-entropy bins))))

(doc 'section 'empirical-mutual-information)

(define (mutual-information-empirical xs ys . opts)
  (doc 'export #t)
  (doc 'type '(-> (List Number) (List Number) [Nat] Number))
  (doc 'description "Estimate mutual information I(X;Y) from two sample vectors.
   Discretizes both into uniform bins, builds joint distribution, computes
   I(X;Y) = H(X) + H(Y) - H(X,Y). Optional third argument is bin count.")
  (if (or (null? xs) (null? ys))
      0
      (let* ([n (length xs)]
             [num-bins (if (and (pair? opts) (integer? (car opts)))
                           (car opts)
                           (default-num-bins n))]
             [bx (discretize-uniform xs num-bins)]
             [by (discretize-uniform ys num-bins)]
             ;; Build joint frequency table
             [joint-counts (build-joint-counts bx by num-bins)]
             ;; Convert to probabilities
             [joint-probs (map (lambda (row)
                                 (map (lambda (c) (/ c n)) row))
                               joint-counts)]
             ;; Compute marginals
             [marginal-x (map (lambda (row) (fold-left + 0 row)) joint-probs)]
             [marginal-y (compute-column-sums joint-probs num-bins)])
        (mutual-information joint-probs marginal-x marginal-y))))

(define (build-joint-counts bx by num-bins)
  (doc 'export #f)
  (doc 'type '(-> (List Nat) (List Nat) Nat (List (List Nat))))
  (doc 'description "Build num-bins x num-bins joint frequency matrix from bin assignments")
  ;; Initialize count matrix as vector of vectors
  (let ([counts (make-vector (* num-bins num-bins) 0)])
    ;; Count joint occurrences
    (let loop ([xs bx] [ys by])
      (if (or (null? xs) (null? ys))
          ;; Convert flat vector to list-of-lists
          (let row-loop ([i 0] [rows '()])
            (if (= i num-bins)
                (reverse rows)
                (let col-loop ([j 0] [cols '()])
                  (if (= j num-bins)
                      (row-loop (+ i 1) (cons (reverse cols) rows))
                      (col-loop (+ j 1)
                                (cons (vector-ref counts (+ (* i num-bins) j))
                                      cols))))))
          (begin
            (let ([idx (+ (* (car xs) num-bins) (car ys))])
              (vector-set! counts idx (+ 1 (vector-ref counts idx))))
            (loop (cdr xs) (cdr ys)))))))

(define (compute-column-sums matrix num-cols)
  (doc 'export #f)
  (doc 'type '(-> (List (List Number)) Nat (List Number)))
  (doc 'description "Sum each column of a list-of-lists matrix")
  (map (lambda (j)
         (fold-left + 0
                    (map (lambda (row) (list-ref row j)) matrix)))
       (iota num-cols)))

(doc 'section 'conditional-entropy-empirical)

(define (conditional-entropy-empirical xs ys . opts)
  (doc 'export #t)
  (doc 'type '(-> (List Number) (List Number) [Nat] Number))
  (doc 'description "Estimate conditional entropy H(X|Y) from sample vectors.
   H(X|Y) = H(X,Y) - H(Y) = H(X) - I(X;Y).")
  (let* ([hx (apply entropy-empirical xs opts)]
         [mi (apply mutual-information-empirical xs ys opts)])
    (max 0 (- hx mi))))

(doc 'section 'normalized-mutual-information-empirical)

(define (nmi-empirical xs ys . opts)
  (doc 'export #t)
  (doc 'type '(-> (List Number) (List Number) [Nat] Number))
  (doc 'description "Normalized mutual information from sample vectors.
   NMI = I(X;Y) / sqrt(H(X) * H(Y)), in [0, 1].")
  (let* ([hx (apply entropy-empirical xs opts)]
         [hy (apply entropy-empirical ys opts)]
         [mi (apply mutual-information-empirical xs ys opts)])
    (if (or (<= hx 0) (<= hy 0))
        0
        (/ mi (sqrt (* hx hy))))))
