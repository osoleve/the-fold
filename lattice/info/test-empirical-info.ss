(load "core/testing/test-framework.ss")
(load "lattice/info/empirical-info.ss")

(test-group "empirical-info"

  (define-test "discretize-uniform: basic binning"
    (let ([bins (discretize-uniform '(0.0 0.25 0.5 0.75 1.0) 4)])
      ;; Values should map to bins 0, 1, 2, 3, 3 (max clamped)
      (assert-equal '(0 1 2 3 3) bins)))

  (define-test "discretize-uniform: identical values"
    (let ([bins (discretize-uniform '(5 5 5 5) 3)])
      (assert-equal '(0 0 0 0) bins)))

  (define-test "discretize-uniform: empty list"
    (assert-equal '() (discretize-uniform '() 5)))

  (define-test "default-num-bins: Sturges rule"
    ;; ceil(1 + log2(8)) = ceil(4) = 4
    (assert-equal 4 (default-num-bins 8))
    ;; ceil(1 + log2(16)) = ceil(5) = 5
    (assert-equal 5 (default-num-bins 16)))

  (define-test "entropy-empirical: uniform data has high entropy"
    ;; Integers 1..8 in 4 bins should have entropy = 2 bits
    (let ([h (entropy-empirical '(1 2 3 4 5 6 7 8) 4)])
      (assert-true (> h 1.5))))

  (define-test "entropy-empirical: constant data has zero entropy"
    (let ([h (entropy-empirical '(5 5 5 5 5) 3)])
      (assert-true (< (abs h) 1e-10))))

  (define-test "mutual-information-empirical: identical variables"
    ;; MI of a variable with itself equals its entropy
    (let* ([xs '(1 2 3 4 5 6 7 8)]
           [mi (mutual-information-empirical xs xs 4)]
           [hx (entropy-empirical xs 4)])
      (assert-true (< (abs (- mi hx)) 1e-10))))

  (define-test "mutual-information-empirical: non-negative"
    (let ([mi (mutual-information-empirical
                '(1 2 3 4 5 6 7 8)
                '(8 7 6 5 4 3 2 1) 4)])
      (assert-true (>= mi -1e-10))))

  (define-test "mutual-information-empirical: independent variables have low MI"
    ;; Two sequences that place values in different bins independently
    ;; should have near-zero MI (subject to finite sample bias)
    (let ([mi (mutual-information-empirical
                '(1 1 2 2 3 3 4 4 1 1 2 2 3 3 4 4)
                '(1 2 1 2 1 2 1 2 3 4 3 4 3 4 3 4) 4)])
      ;; For truly independent uniform data, MI should be near zero
      (assert-true (< mi 0.1))))

  (define-test "conditional-entropy-empirical: H(X|X) = 0"
    (let* ([xs '(1 2 3 4 5 6 7 8)]
           [h-cond (conditional-entropy-empirical xs xs 4)])
      (assert-true (< h-cond 1e-10))))

  (define-test "conditional-entropy-empirical: H(X|Y) <= H(X)"
    (let* ([xs '(1 2 3 4 5 6 7 8)]
           [ys '(2 1 4 3 6 5 8 7)]
           [hx (entropy-empirical xs 4)]
           [h-cond (conditional-entropy-empirical xs ys 4)])
      (assert-true (<= h-cond (+ hx 1e-10)))))

  (define-test "nmi-empirical: identical variables give 1"
    (let* ([xs '(1 2 3 4 5 6 7 8)]
           [nmi (nmi-empirical xs xs 4)])
      (assert-true (< (abs (- nmi 1.0)) 1e-10))))

  (define-test "nmi-empirical: bounded [0, 1]"
    (let ([nmi (nmi-empirical
                 '(1 2 3 4 5 6 7 8)
                 '(8 7 6 5 4 3 2 1) 4)])
      (assert-true (>= nmi -1e-10))
      (assert-true (<= nmi (+ 1.0 1e-10)))))

  ) ;; end group

(run-all-tests)
