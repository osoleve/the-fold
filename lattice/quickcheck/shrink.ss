;;; lattice/quickcheck/shrink.ss — Shrink functions for counterexample minimization
;;; @module qc-shrink
;;; @requires prelude

(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
(require 'prelude)

(doc 'module 'qc-shrink)
(doc 'description "Shrinker functions for QuickCheck. A Shrinker a = a -> (List a), returning candidates ordered most-to-least aggressive.")
(doc 'layer 'lattice)
(doc 'purity 'total)

;;; ============================================================================
;;; Base shrinkers
;;; ============================================================================

(doc 'section 'base-shrinkers)

(define (shrink-int n)
  (doc 'export #t)
  (doc 'type '(-> Int (List Int)))
  (doc 'description "Binary search toward 0. n=100 produces (0 50 75 88 94 97 99)")
  (if (= n 0)
      '()
      ;; Always include 0 as first candidate, then binary search
      (let ([target 0])
        (let loop ([lo target] [hi n] [acc (list target)])
          (let ([mid (if (< n 0)
                         ;; Negative: search upward toward 0
                         (let ([m (quotient (+ lo hi) 2)])
                           (if (>= m hi) #f m))
                         ;; Positive: search upward from 0
                         (let ([m (+ lo (quotient (- hi lo) 2))])
                           (if (<= m lo) #f m)))])
            (if (not mid)
                (reverse acc)
                (loop mid hi (cons mid acc))))))))

(define (shrink-nat n)
  (doc 'export #t)
  (doc 'type '(-> Nat (List Nat)))
  (doc 'description "Like shrink-int but non-negative only")
  (filter (lambda (x) (>= x 0)) (shrink-int n)))

(define (shrink-bool b)
  (doc 'export #t)
  (doc 'type '(-> Boolean (List Boolean)))
  (doc 'description "#t -> (#f), #f -> ()")
  (if b '(#f) '()))

(define (shrink-float x)
  (doc 'export #t)
  (doc 'type '(-> Float (List Float)))
  (doc 'description "Halving toward 0.0")
  (if (= x 0.0)
      '()
      (let loop ([v (/ x 2.0)] [acc (list 0.0)])
        (if (< (abs v) 1e-10)
            (reverse acc)
            (loop (/ v 2.0) (cons v acc))))))

;;; ============================================================================
;;; Collection shrinkers
;;; ============================================================================

(doc 'section 'collection-shrinkers)

(define (shrink-list elem-shrinker lst)
  (doc 'export #t)
  (doc 'type '(-> (-> a (List a)) (List a) (List (List a))))
  (doc 'description "Remove progressively smaller chunks, then shrink elements in place")
  (let ([n (length lst)])
    (if (= n 0)
        '()
        (append
          ;; Phase 1: remove chunks of decreasing size
          (remove-chunks n lst)
          ;; Phase 2: shrink individual elements
          (shrink-elements elem-shrinker lst 0)))))

(define (remove-chunks n lst)
  (doc 'description "Generate sublists by removing chunks of halving size")
  (let outer ([k n])
    (if (<= k 0)
        '()
        (let inner ([start 0])
          (if (>= start (length lst))
              (outer (quotient k 2))
              ;; Remove k elements starting at position start
              (let ([removed (append (take start lst)
                                     (drop-at-most (+ start k) lst))])
                (if (equal? removed lst)
                    (inner (+ start k))
                    (cons removed (inner (+ start k))))))))))

(define (drop-at-most n lst)
  (doc 'description "Drop up to n elements from list")
  (if (or (<= n 0) (null? lst))
      lst
      (drop-at-most (- n 1) (cdr lst))))

(define (shrink-elements elem-shrinker lst idx)
  (doc 'description "Try shrinking each element in place, producing lists with one element shrunk")
  (if (null? lst)
      '()
      (let* ([head (car lst)]
             [tail (cdr lst)]
             [shrunk-heads (elem-shrinker head)]
             [with-shrunk-head (map (lambda (h) (cons h tail)) shrunk-heads)]
             [rest (map (lambda (tail-variant) (cons head tail-variant))
                        (shrink-elements elem-shrinker tail (+ idx 1)))])
        (append with-shrunk-head rest))))

(define (shrink-pair shrink-a shrink-b)
  (doc 'export #t)
  (doc 'type '(-> (-> a (List a)) (-> b (List b)) (-> (Pair a b) (List (Pair a b)))))
  (doc 'description "Shrink pair components independently")
  (lambda (p)
    (let ([a (car p)]
          [b (cdr p)])
      (append
        ;; Shrink first component
        (map (lambda (a2) (cons a2 b)) (shrink-a a))
        ;; Shrink second component
        (map (lambda (b2) (cons a b2)) (shrink-b b))))))

;;; ============================================================================
;;; Shrink combinators
;;; ============================================================================

(doc 'section 'combinators)

(define (shrink-nothing x)
  (doc 'export #t)
  (doc 'type '(-> a (List a)))
  (doc 'description "No shrinking — always returns empty list")
  '())

(define (shrink-map f-to f-from shrinker)
  (doc 'export #t)
  (doc 'type '(-> (-> a b) (-> b a) (-> a (List a)) (-> b (List b))))
  (doc 'description "Shrink through an isomorphism: convert to a, shrink, convert back")
  (lambda (b)
    (map f-to (shrinker (f-from b)))))

(define (shrink-filter pred shrinker)
  (doc 'export #t)
  (doc 'type '(-> (-> a Boolean) (-> a (List a)) (-> a (List a))))
  (doc 'description "Keep only shrink candidates satisfying predicate")
  (lambda (x)
    (filter pred (shrinker x))))

(define (shrink-one-of . shrinkers)
  (doc 'export #t)
  (doc 'type '(-> (-> a (List a)) ... (-> a (List a))))
  (doc 'description "Try all shrinkers, concatenate their candidates")
  (lambda (x)
    (apply append (map (lambda (s) (s x)) shrinkers))))
