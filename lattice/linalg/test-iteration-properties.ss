;;; lattice/linalg/test-iteration-properties.ss — QuickCheck properties for iteration macros

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'iteration)

;;; ============================================================================
;;; Generators
;;; ============================================================================

(define gen-int-small
  (gen-int-range -30 30))

(define gen-int-list
  (gen-list gen-int-small))

(define gen-same-length-pair
  (gen-bind (gen-int-range 0 30)
    (lambda (n)
      (gen-bind (gen-list-of n gen-int-small)
        (lambda (xs)
          (gen-map (lambda (ys) (cons xs ys))
                   (gen-list-of n gen-int-small)))))))

;;; ============================================================================
;;; Properties
;;; ============================================================================

(test-group iteration-properties

  (define-property "vec-map-idx identity body reproduces the original vector"
    gen-int-list
    (lambda (xs)
      (let* ([v (list->vector xs)]
             [mapped (vec-map-idx i v (vector-ref v i))])
        (equal? (vector->list mapped) xs)))
    'tests 240)

  (define-property "vec-fold sum matches fold-left on list view"
    gen-int-list
    (lambda (xs)
      (let* ([v (list->vector xs)]
             [sum-iter (vec-fold acc 0 x v (+ acc x))]
             [sum-list (fold-left + 0 xs)])
        (= sum-iter sum-list)))
    'tests 240)

  (define-property "vec-zip-map-idx pointwise addition matches list map"
    gen-same-length-pair
    (lambda (pair)
      (let* ([xs (car pair)]
             [ys (cdr pair)]
             [vx (list->vector xs)]
             [vy (list->vector ys)]
             [vz (vec-zip-map-idx i vx vy (+ (vector-ref vx i) (vector-ref vy i)))])
        (equal? (vector->list vz)
                (map + xs ys))))
    'tests 220)

  (define-property "vector iteration macro surface behaves consistently"
    gen-int-list
    (lambda (xs)
      (let* ([v (list->vector xs)]
             [n (vector-length v)]
             [sum-do (let ([acc 0])
                       (vec-do! i v
                         (set! acc (+ acc (vector-ref v i))))
                       acc)]
             [sum-fold (vec-fold acc 0 x v (+ acc x))]
             [sum-fold-idx (vec-fold-idx acc 0 i v (+ acc (vector-ref v i)))]
             [sum-fold-rev (vec-fold-reverse acc 0 i v (+ acc (vector-ref v i)))]
             [mapped (let ([out (make-vector n 0)])
                       (vec-map-idx! i v out (+ (vector-ref v i) i)))]
             [zipped (let ([out (make-vector n 0)])
                       (vec-zip-do! i v mapped
                         (vector-set! out i (+ (vector-ref v i) (vector-ref mapped i))))
                       out)]
             [any-pos? (vec-any? x v (> x 0))]
             [has-pos (let loop ([rest xs])
                        (and (pair? rest)
                             (or (> (car rest) 0)
                                 (loop (cdr rest)))))]
             [all-int? (vec-all? x v (integer? x))]
             [found-neg (vec-find-idx i v (< (vector-ref v i) 0))]
             [rev-idxs (let ([acc '()])
                         (vec-do-reverse! i v
                           (set! acc (cons i acc)))
                         (reverse acc))]
             [expected-rev (let loop ([i 0] [out '()])
                             (if (= i n)
                                 out
                                 (loop (+ i 1) (cons i out))))])
        (and (= sum-do sum-fold)
             (= sum-fold-idx sum-fold)
             (= sum-fold-rev sum-fold)
             (= (vector-length mapped) n)
             (= (vector-length zipped) n)
             (equal? rev-idxs expected-rev)
             (eq? any-pos? has-pos)
             all-int?
             (or (not found-neg)
                 (and (integer? found-neg)
                      (>= found-neg 0)
                      (< found-neg n)
                      (< (vector-ref v found-neg) 0))))))
    'tests 220)

  (define-property "matrix/range iteration macro surface behaves consistently"
    (gen-pure #t)
    (lambda (_)
      (let* ([r 2]
             [c 3]
             [data (vector 1 2 3 4 5 6)]
             [sum-m (matrix-fold-ij acc 0 i j r c data
                      (+ acc (vector-ref data (+ (* i c) j))))]
             [mapped (matrix-map-ij! i j r c data (make-vector (* r c) 0)
                      (+ (vector-ref data (+ (* i c) j)) 1))]
             [row-sum (let ([acc 0])
                        (matrix-row-do! j 1 c data
                          (set! acc (+ acc (vector-ref data (+ c j)))))
                        acc)]
             [col-sum (let ([acc 0])
                        (matrix-col-do! i 2 r c data
                          (set! acc (+ acc (vector-ref data (+ (* i c) 2)))))
                        acc)]
             [sum-do (let ([acc 0])
                       (matrix-do! i j r c
                         (set! acc (+ acc (vector-ref data (+ (* i c) j)))))
                       acc)]
             [range-sum-do (let ([acc 0])
                             (range-do! k 0 10
                               (set! acc (+ acc k)))
                             acc)]
             [range-sum-fold (range-fold acc 0 k 0 10 (+ acc k))]
             [dot (dot-product-loop k 3
                    (vector-ref data k)
                    (vector-ref data k))]
             [mm (let* ([m 2] [n 2] [inner 3]
                        [a (vector 1 2 3 4 5 6)]      ; 2x3
                        [b (vector 7 8 9 10 11 12)]   ; 3x2
                        [out (make-vector (* m n) 0)])
                   (matrix-mul-do! i j k m inner n
                     (let* ([aik (vector-ref a (+ (* i inner) k))]
                            [bkj (vector-ref b (+ (* k n) j))]
                            [idx (+ (* i n) j)])
                       (vector-set! out idx (+ (vector-ref out idx) (* aik bkj)))))
                   out)])
        (and (number? sum-m)
             (= sum-do 21)
             (= row-sum 15)
             (= col-sum 9)
             (= range-sum-do 45)
             (= range-sum-fold 45)
             (= dot 14)
             (equal? (vector->list mapped) '(2 3 4 5 6 7))
             (equal? (vector->list mm) '(58 64 139 154)))))
    'tests 20)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
