(load "core/testing/test-framework.ss")
(load "lattice/info/partition-info.ss")

(test-group "partition-info"

  (define-test "partition-sizes: counts communities"
    (let ([sizes (partition-sizes '#(0 0 0 1 1 2))])
      (assert-equal '(3 2 1) sizes)))

  (define-test "partition-sizes: single community"
    (let ([sizes (partition-sizes '#(0 0 0 0))])
      (assert-equal '(4) sizes)))

  (define-test "partition-entropy: single community = 0"
    (let ([h (partition-entropy '#(0 0 0 0))])
      (assert-true (< (abs h) 1e-10))))

  (define-test "partition-entropy: uniform binary = 1 bit"
    ;; Two communities of equal size -> H = 1 bit
    (let ([h (partition-entropy '#(0 0 1 1))])
      (assert-true (< (abs (- h 1.0)) 1e-10))))

  (define-test "partition-entropy: uniform four-way = 2 bits"
    (let ([h (partition-entropy '#(0 1 2 3))])
      (assert-true (< (abs (- h 2.0)) 1e-10))))

  (define-test "partition-entropy: non-uniform"
    ;; 3 in community 0, 1 in community 1 -> H < 1
    (let ([h (partition-entropy '#(0 0 0 1))])
      (assert-true (> h 0))
      (assert-true (< h 1.0))))

  (define-test "partition-mi: identical partitions"
    ;; I(A;A) = H(A)
    (let* ([labels '#(0 0 1 1 2 2)]
           [mi (partition-mi labels labels)]
           [h (partition-entropy labels)])
      (assert-true (< (abs (- mi h)) 1e-10))))

  (define-test "partition-mi: independent partitions"
    ;; Partitions that are independent should have MI near 0
    ;; Cross-tabulation should be uniform
    (let* ([a '#(0 0 1 1 0 0 1 1)]
           [b '#(0 1 0 1 0 1 0 1)]
           [mi (partition-mi a b)])
      (assert-true (< (abs mi) 1e-10))))

  (define-test "partition-nmi: identical partitions = 1"
    (let* ([labels '#(0 0 1 1 2 2)]
           [nmi (partition-nmi labels labels)])
      (assert-true (< (abs (- nmi 1.0)) 1e-10))))

  (define-test "partition-nmi: independent partitions = 0"
    (let* ([a '#(0 0 1 1 0 0 1 1)]
           [b '#(0 1 0 1 0 1 0 1)]
           [nmi (partition-nmi a b)])
      (assert-true (< (abs nmi) 1e-10))))

  (define-test "partition-nmi: both single community"
    (let ([nmi (partition-nmi '#(0 0 0 0) '#(0 0 0 0))])
      ;; Both zero entropy -> convention: NMI = 1 (identical)
      (assert-true (< (abs (- nmi 1.0)) 1e-10))))

  (define-test "partition-nmi: one single community"
    (let ([nmi (partition-nmi '#(0 0 0 0) '#(0 0 1 1))])
      ;; One has zero entropy -> convention: NMI = 0
      (assert-true (< (abs nmi) 1e-10))))

  (define-test "partition-nmi: bounded [0, 1]"
    (let* ([a '#(0 0 0 1 1 2 2 2)]
           [b '#(0 1 1 1 2 2 0 0)]
           [nmi (partition-nmi a b)])
      (assert-true (>= nmi -1e-10))
      (assert-true (<= nmi (+ 1.0 1e-10)))))

  (define-test "partition-vi: identical partitions = 0"
    (let ([vi (partition-vi '#(0 0 1 1) '#(0 0 1 1))])
      (assert-true (< (abs vi) 1e-10))))

  (define-test "partition-vi: non-negative"
    (let ([vi (partition-vi '#(0 0 1 1 2) '#(0 1 1 2 2))])
      (assert-true (>= vi -1e-10))))

  (define-test "partition-vi: symmetric"
    (let* ([a '#(0 0 1 1 2)]
           [b '#(0 1 1 2 2)]
           [vi-ab (partition-vi a b)]
           [vi-ba (partition-vi b a)])
      (assert-true (< (abs (- vi-ab vi-ba)) 1e-10))))

  (define-test "partition-vi-normalized: bounded [0, 1]"
    (let ([nvi (partition-vi-normalized '#(0 0 1 1 2) '#(0 1 1 2 2))])
      (assert-true (>= nvi -1e-10))
      (assert-true (<= nvi (+ 1.0 1e-10)))))

  (define-test "partition-vi-normalized: identical = 0"
    (let ([nvi (partition-vi-normalized '#(0 0 1 1) '#(0 0 1 1))])
      (assert-true (< (abs nvi) 1e-10))))

  ) ;; end group

(run-all-tests)
