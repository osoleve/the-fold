;;; lattice/quickcheck/test-quickcheck.ss — Self-tests for QuickCheck

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)

;;; Helper — check all elements satisfy predicate
(define (for-all-list pred lst)
  (or (null? lst)
      (and (pred (car lst))
           (for-all-list pred (cdr lst)))))

;;; Helper — simple substring check
(define (string-contains? hay needle)
  (let ([hlen (string-length hay)]
        [nlen (string-length needle)])
    (let loop ([i 0])
      (cond
       [(> (+ i nlen) hlen) #f]
       [(string=? (substring hay i (+ i nlen)) needle) #t]
       [else (loop (+ i 1))]))))

;;; ============================================================================
;;; Generator basics
;;; ============================================================================

(test-group generator-basics

  (define-test "gen-pure always produces same value"
    (let* ([g (gen-pure 42)]
           [v1 (run-gen g 10 (make-pcg 1 1))]
           [v2 (run-gen g 99 (make-pcg 999 1))])
      (assert-equal 42 v1)
      (assert-equal 42 v2)))

  (define-test "gen-int range bounded by size"
    (let ([g gen-int]
          [rng (make-pcg 42 1)])
      ;; At size 5, values should be in [-5, 5]
      (let loop ([i 0] [r rng] [ok #t])
        (if (or (>= i 100) (not ok))
            (assert-true ok)
            (let* ([result (run-gen-with-state g 5 r)]
                   [v (car result)]
                   [r2 (cdr result)])
              (loop (+ i 1) r2 (and ok (<= -5 v 5))))))))

  (define-test "gen-nat produces non-negative values"
    (let ([g gen-nat]
          [rng (make-pcg 42 1)])
      (let loop ([i 0] [r rng] [ok #t])
        (if (or (>= i 100) (not ok))
            (assert-true ok)
            (let* ([result (run-gen-with-state g 10 r)]
                   [v (car result)]
                   [r2 (cdr result)])
              (loop (+ i 1) r2 (and ok (>= v 0))))))))

  (define-test "gen-bool produces booleans"
    (let ([g gen-bool]
          [rng (make-pcg 42 1)])
      (let loop ([i 0] [r rng] [ok #t])
        (if (or (>= i 100) (not ok))
            (assert-true ok)
            (let* ([result (run-gen-with-state g 10 r)]
                   [v (car result)]
                   [r2 (cdr result)])
              (loop (+ i 1) r2 (and ok (boolean? v))))))))

  (define-test "gen-bool coverage — both values appear"
    (let ([g gen-bool]
          [rng (make-pcg 42 1)])
      (let loop ([i 0] [r rng] [seen-t #f] [seen-f #f])
        (if (>= i 100)
            (begin
              (assert-true seen-t "expected #t to appear")
              (assert-true seen-f "expected #f to appear"))
            (let* ([result (run-gen-with-state g 10 r)]
                   [v (car result)]
                   [r2 (cdr result)])
              (loop (+ i 1) r2
                    (or seen-t (eq? v #t))
                    (or seen-f (eq? v #f))))))))

  (define-test "determinism — same seed gives same output"
    (let ([g gen-int])
      (assert-equal (run-gen g 10 (make-pcg 42 1))
                    (run-gen g 10 (make-pcg 42 1)))))
)

;;; ============================================================================
;;; Generator combinators
;;; ============================================================================

(test-group generator-combinators

  (define-test "gen-one-of selects from generators"
    (let* ([g (gen-one-of (list (gen-pure 'a) (gen-pure 'b) (gen-pure 'c)))]
           [rng (make-pcg 42 1)])
      (let loop ([i 0] [r rng] [seen '()])
        (if (>= i 100)
            (begin
              (assert-true (pair? (memq 'a seen)) "expected 'a")
              (assert-true (pair? (memq 'b seen)) "expected 'b")
              (assert-true (pair? (memq 'c seen)) "expected 'c"))
            (let* ([result (run-gen-with-state g 10 r)]
                   [v (car result)]
                   [r2 (cdr result)])
              (loop (+ i 1) r2 (if (memq v seen) seen (cons v seen))))))))

  (define-test "gen-elements selects from values"
    (let* ([g (gen-elements '(10 20 30))]
           [rng (make-pcg 42 1)])
      (let loop ([i 0] [r rng] [ok #t])
        (if (or (>= i 100) (not ok))
            (assert-true ok)
            (let* ([result (run-gen-with-state g 10 r)]
                   [v (car result)]
                   [r2 (cdr result)])
              (loop (+ i 1) r2 (and ok (pair? (memv v '(10 20 30))))))))))

  (define-test "gen-list bounded by size"
    (let* ([g (gen-list gen-nat)]
           [rng (make-pcg 42 1)]
           [size 5])
      (let loop ([i 0] [r rng] [ok #t])
        (if (or (>= i 50) (not ok))
            (assert-true ok)
            (let* ([result (run-gen-with-state g size r)]
                   [v (car result)]
                   [r2 (cdr result)])
              (loop (+ i 1) r2 (and ok (<= (length v) size))))))))

  (define-test "gen-list-of produces exact length"
    (let* ([g (gen-list-of 5 gen-nat)]
           [rng (make-pcg 42 1)]
           [v (run-gen g 10 rng)])
      (assert-equal 5 (length v))))

  (define-test "gen-pair produces pairs"
    (let* ([g (gen-pair gen-nat gen-bool)]
           [rng (make-pcg 42 1)]
           [v (run-gen g 10 rng)])
      (assert-true (pair? v))
      (assert-true (>= (car v) 0))
      (assert-true (boolean? (cdr v)))))

  (define-test "gen-map transforms output"
    (let* ([g (gen-map (lambda (n) (* n 2)) gen-nat)]
           [rng (make-pcg 42 1)]
           [v (run-gen g 10 rng)])
      (assert-true (= 0 (modulo v 2)))))

  (define-test "gen-bind chains generators"
    (let* ([g (gen-bind gen-nat
                        (lambda (n) (gen-list-of n (gen-pure 'x))))]
           [rng (make-pcg 42 1)])
      ;; At size 5, gen-nat gives 0-5, list-of gives that many 'x
      (let* ([result (run-gen-with-state g 5 rng)]
             [v (car result)])
        (assert-true (list? v))
        (assert-true (<= (length v) 5))
        (assert-true (for-all-list (lambda (x) (eq? x 'x)) v)))))

  (define-test "gen-string produces strings"
    (let* ([g gen-string]
           [rng (make-pcg 42 1)]
           [v (run-gen g 10 rng)])
      (assert-true (string? v))))

  (define-test "gen-resize overrides size"
    (let* ([g (gen-resize 3 gen-nat)]
           [rng (make-pcg 42 1)])
      ;; Even with large size, should be bounded by 3
      (let loop ([i 0] [r rng] [ok #t])
        (if (or (>= i 100) (not ok))
            (assert-true ok)
            (let* ([result (run-gen-with-state g 1000 r)]
                   [v (car result)]
                   [r2 (cdr result)])
              (loop (+ i 1) r2 (and ok (<= v 3))))))))

  (define-test "gen-frequency respects weights"
    ;; Heavily weight toward 'a, lightly toward 'b
    (let* ([g (gen-frequency (list (cons 99 (gen-pure 'a))
                                   (cons 1 (gen-pure 'b))))]
           [rng (make-pcg 42 1)])
      (let loop ([i 0] [r rng] [count-a 0])
        (if (>= i 200)
            ;; 'a should dominate
            (assert-true (> count-a 150) "expected 'a to dominate")
            (let* ([result (run-gen-with-state g 10 r)]
                   [v (car result)]
                   [r2 (cdr result)])
              (loop (+ i 1) r2 (if (eq? v 'a) (+ count-a 1) count-a)))))))
)

;;; ============================================================================
;;; Generator advanced/internals coverage
;;; ============================================================================

(test-group generator-advanced

  (define-test "make-gen, gen?, and gen-fn basics"
    (let* ([g (make-gen (lambda (size)
                          ((gen-fn (gen-pure (+ size 1))) size)))]
           [rng (make-pcg 42 1)]
           [v (run-gen g 7 rng)])
      (assert-true (gen? g))
      (assert-true (procedure? (gen-fn g)))
      (assert-equal 8 v)))

  (define-test "gen-sized can observe current size"
    (let* ([g (gen-sized (lambda (size) (gen-pure size)))]
           [rng (make-pcg 42 1)])
      (assert-equal 13 (run-gen g 13 rng))))

  (define-test "gen-scale transforms size before generation"
    (let* ([base (gen-sized (lambda (size) (gen-pure size)))]
           [g (gen-scale (lambda (size) (* 2 size)) base)]
           [rng (make-pcg 42 1)])
      (assert-equal 14 (run-gen g 7 rng))))

  (define-test "gen-float stays in [0, 1)"
    (let ([rng (make-pcg 42 1)])
      (let loop ([i 0] [r rng] [ok #t])
        (if (or (>= i 100) (not ok))
            (assert-true ok)
            (let* ([result (run-gen-with-state gen-float 10 r)]
                   [v (car result)]
                   [r2 (cdr result)])
              (loop (+ i 1) r2
                    (and ok
                         (number? v)
                         (<= 0.0 v)
                         (< v 1.0))))))))

  (define-test "gen-vector produces vectors with bounded length"
    (let* ([g (gen-vector gen-nat)]
           [rng (make-pcg 42 1)]
           [size 6]
           [v (run-gen g size rng)]
           [xs (vector->list v)])
      (assert-true (vector? v))
      (assert-true (<= (vector-length v) size))
      (assert-true (for-all-list (lambda (n) (>= n 0)) xs))))

  (define-test "gen-from-random ignores size and uses provided state computation"
    (let* ([g (gen-from-random (random-int-range 5 5))]
           [rng1 (make-pcg 1 1)]
           [rng2 (make-pcg 999 1)])
      (assert-equal 5 (run-gen g 0 rng1))
      (assert-equal 5 (run-gen g 100 rng2))))
)

;;; ============================================================================
;;; Shrink basics
;;; ============================================================================

(test-group shrink-basics

  (define-test "shrink-int 0 produces empty list"
    (assert-equal '() (shrink-int 0)))

  (define-test "shrink-int n contains 0"
    (assert-true (pair? (memv 0 (shrink-int 100)))))

  (define-test "shrink-int candidates all closer to 0"
    (let ([candidates (shrink-int 100)])
      (assert-true
        (for-all-list (lambda (c) (< (abs c) (abs 100)))
                      candidates))))

  (define-test "shrink-int negative contains 0"
    (assert-true (pair? (memv 0 (shrink-int -50)))))

  (define-test "shrink-int negative candidates closer to 0"
    (let ([candidates (shrink-int -50)])
      (assert-true
        (for-all-list (lambda (c) (< (abs c) (abs -50)))
                      candidates))))

  (define-test "shrink-nat produces non-negative"
    (assert-true
      (for-all-list (lambda (c) (>= c 0))
                    (shrink-nat 10))))

  (define-test "shrink-bool #t -> (#f)"
    (assert-equal '(#f) (shrink-bool #t)))

  (define-test "shrink-bool #f -> ()"
    (assert-equal '() (shrink-bool #f)))

  (define-test "shrink-list includes empty list"
    (let ([candidates (shrink-list shrink-int '(1 2 3))])
      (assert-true (pair? (member '() candidates)))))

  (define-test "shrink-list non-empty for non-empty input"
    (let ([candidates (shrink-list shrink-int '(5 10 15))])
      (assert-true (pair? candidates))))

  (define-test "shrink-nothing always empty"
    (assert-equal '() (shrink-nothing 42))
    (assert-equal '() (shrink-nothing '(1 2 3))))

  (define-test "shrink-filter removes invalid candidates"
    (let* ([s (shrink-filter (lambda (n) (> n 0)) shrink-int)]
           [candidates (s 10)])
      (assert-true (for-all-list (lambda (c) (> c 0)) candidates))))

  (define-test "shrink-pair shrinks components"
    (let* ([s (shrink-pair shrink-int shrink-int)]
           [candidates (s (cons 10 20))])
      (assert-true (pair? candidates))
      ;; Should contain pairs with first shrunk
      (assert-true (pair? (member (cons 0 20) candidates)))
      ;; Should contain pairs with second shrunk
      (assert-true (pair? (member (cons 10 0) candidates)))))
)

;;; ============================================================================
;;; Shrink internals/combinators coverage
;;; ============================================================================

(test-group shrink-advanced

  (define-test "shrink-float 0.0 produces empty list"
    (assert-equal '() (shrink-float 0.0)))

  (define-test "shrink-float includes 0.0 and moves toward zero"
    (let ([candidates (shrink-float 8.0)])
      (assert-true (pair? (member 0.0 candidates)))
      (assert-true (for-all-list (lambda (c) (< (abs c) 8.0))
                                 candidates))))

  (define-test "remove-chunks returns strictly smaller lists"
    (let ([candidates (remove-chunks 4 '(1 2 3 4))])
      (assert-true (pair? (member '() candidates)))
      (assert-true (for-all-list (lambda (lst) (< (length lst) 4))
                                 candidates))))

  (define-test "drop-at-most drops prefix safely"
    (assert-equal '(c d) (drop-at-most 2 '(a b c d)))
    (assert-equal '() (drop-at-most 10 '(a b))))

  (define-test "shrink-elements shrinks each position in place"
    (let ([candidates (shrink-elements shrink-int '(10 20) 0)])
      (assert-true (pair? (member '(0 20) candidates)))
      (assert-true (pair? (member '(10 0) candidates)))))

  (define-test "shrink-map transports shrinking through conversion"
    (let* ([s (shrink-map (lambda (n) (* 2 n))
                          (lambda (n) (quotient n 2))
                          shrink-int)]
           [candidates (s 20)])
      (assert-true (pair? (member 0 candidates)))
      (assert-true (for-all-list even? candidates))))

  (define-test "shrink-one-of combines candidate streams"
    (let* ([s (shrink-one-of shrink-nothing shrink-int)]
           [candidates (s 8)])
      (assert-equal (shrink-int 8) candidates)))
)

;;; ============================================================================
;;; Property runner
;;; ============================================================================

(test-group property-runner

  (define-test "true property succeeds"
    (let ([result (check-property gen-int (lambda (n) #t) 'tests 50)])
      (assert-true (qc-success? result))
      (assert-equal 50 (qc-success-num-tests result))))

  (define-test "false property finds counterexample"
    (let ([result (check-property gen-nat (lambda (n) #f) 'tests 50)])
      (assert-true (qc-failure? result))
      ;; Should fail on very first test
      (assert-equal 1 (qc-failure-num-tests result))))

  (define-test "shrinking finds minimal case"
    ;; Property: n < 10 — should shrink to 10
    (let ([result (check-property gen-nat
                                  (lambda (n) (< n 10))
                                  'tests 200
                                  'shrink shrink-nat
                                  'max-size 100)])
      (assert-true (qc-failure? result))
      (assert-equal 10 (qc-failure-shrunk result))))

  (define-test "list length shrinking"
    ;; Property: list length <= 3 — should shrink to 4-element list
    (let ([result (check-property (gen-list gen-nat)
                                  (lambda (lst) (<= (length lst) 3))
                                  'tests 200
                                  'shrink (lambda (lst) (shrink-list shrink-nat lst))
                                  'max-size 20)])
      (when (qc-failure? result)
        (assert-equal 4 (length (qc-failure-shrunk result))))))

  (define-test "seed reproducibility"
    (let* ([prop (lambda (n) (< n 50))]
           [r1 (check-property gen-nat prop 'tests 200 'seed 42 'max-size 100)]
           [r2 (check-property gen-nat prop 'tests 200 'seed 42 'max-size 100)])
      ;; Same seed should give same failure point
      (assert-true (qc-failure? r1))
      (assert-true (qc-failure? r2))
      (assert-equal (qc-failure-num-tests r1) (qc-failure-num-tests r2))
      (assert-equal (qc-failure-original r1) (qc-failure-original r2))))

  (define-test "qc-for-all works as bare runner"
    (let ([result (qc-for-all gen-bool (lambda (b) (boolean? b)) 'tests 50)])
      (assert-true (qc-success? result))))
)

;;; ============================================================================
;;; Runner internals/options coverage
;;; ============================================================================

(test-group runner-internals

  (define-test "parse-qc-opts and qc-opt extract values with defaults"
    (let* ([opts (parse-qc-opts (list 'tests 10 'seed 77 'max-size 50))])
      (assert-equal 10 (qc-opt opts 'tests -1))
      (assert-equal 77 (qc-opt opts 'seed -1))
      (assert-equal 50 (qc-opt opts 'max-size -1))
      (assert-equal 'fallback (qc-opt opts 'missing 'fallback))))

  (define-test "parse-qc-opts ignores dangling key without value"
    (let* ([opts (parse-qc-opts (list 'tests 10 'seed))])
      (assert-equal 10 (qc-opt opts 'tests -1))
      (assert-equal 'none (qc-opt opts 'seed 'none))))

  (define-test "size-at-test endpoints and singleton behavior"
    (assert-equal 0 (size-at-test 0 10 99))
    (assert-equal 99 (size-at-test 9 10 99))
    (assert-equal 77 (size-at-test 0 1 77))
    (assert-true (<= (size-at-test 2 10 20)
                     (size-at-test 3 10 20))))

  (define-test "shrink-loop finds minimal failing value in monotone property"
    (let* ([result (shrink-loop (lambda (n) (< n 10)) shrink-nat 42 1000)]
           [shrunk (car result)]
           [steps (cdr result)])
      (assert-equal 10 shrunk)
      (assert-true (> steps 0))))

  (define-test "shrink-loop keeps original when no shrinks still fail"
    (let* ([result (shrink-loop (lambda (n) (not (= n 5))) shrink-nat 5 100)]
           [shrunk (car result)]
           [steps (cdr result)])
      (assert-equal 5 shrunk)
      (assert-equal 0 steps)))

  (define-test "qc-failure accessors expose seed and shrink steps"
    (let* ([result (check-property gen-nat
                                   (lambda (n) #f)
                                   'tests 1
                                   'seed 77
                                   'shrink shrink-nat)])
      (assert-true (qc-failure? result))
      (assert-equal (make-pcg 77 1) (qc-failure-seed result))
      (assert-true (>= (qc-failure-shrink-steps result) 0))))

  (define-test "format-qc-failure includes key details"
    (let* ([result (qc-failure 3 (make-pcg 1 1) 99 10 4)]
           [msg (format-qc-failure result)])
      (assert-true (string? msg))
      (assert-true (string-contains? msg "Failed after 3"))
      (assert-true (string-contains? msg "Counterexample: 10"))
      (assert-true (string-contains? msg "Original:"))
      (assert-true (string-contains? msg "Shrink steps:"))))

  (define-test "assert-property macro accepts passing properties"
    (assert-property gen-nat (lambda (n) (>= n 0)) 'tests 50))
)

;;; ============================================================================
;;; Integration — define-property
;;; ============================================================================

(test-group integration

  (define-property "addition is commutative"
    (gen-pair gen-int gen-int)
    (lambda (p) (= (+ (car p) (cdr p)) (+ (cdr p) (car p))))
    'tests 200)

  (define-property "double negation is identity"
    gen-int
    (lambda (n) (= n (- 0 (- 0 n))))
    'tests 200)

  (define-property "list reverse is involution"
    (gen-list gen-int)
    (lambda (lst) (equal? lst (reverse (reverse lst))))
    'tests 100)

  (define-property "gen-int-range stays in range"
    (gen-int-range 10 20)
    (lambda (n) (<= 10 n 20))
    'tests 200)

  (define-property "gen-char produces printable ASCII"
    gen-char
    (lambda (c) (and (char? c)
                     (<= 32 (char->integer c) 126)))
    'tests 200)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
