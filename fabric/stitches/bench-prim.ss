;;; fabric/stitches/bench-prim.ss — Primitive Fuel Cost Benchmarks
;;;
;;; Wallclock benchmarks to validate that fuel costs reflect
;;; actual relative computational complexity.

(load "fabric/stitches/prelude.ss")
(load "fabric/stitches/block.ss")
(load "fabric/stitches/sha256.ss")
(load "fabric/stitches/prim.ss")

;;; ============================================================
;;; Timing Infrastructure
;;; ============================================================

(define *iterations* 100000)

;;; time-thunk : (-> Any) -> (values Any Nanoseconds)
;;; Execute thunk and return result with elapsed time in nanoseconds.
(define (time-thunk thunk)
  (let* ([start (current-time 'time-monotonic)]
         [result (thunk)]
         [end (current-time 'time-monotonic)]
         [elapsed-sec (- (time-second end) (time-second start))]
         [elapsed-ns (- (time-nanosecond end) (time-nanosecond start))]
         [total-ns (+ (* elapsed-sec 1000000000) elapsed-ns)])
        (values result total-ns)))

;;; bench : String (-> Any) Nat -> Void
;;; Run thunk n times and report timing.
(define (bench name thunk n)
  (let-values ([(_ ns) (time-thunk (lambda ()
                                           (let loop ([i n])
                                                (when (> i 0)
                                                      (thunk)
                                                      (loop (- i 1))))))])
              (let* ([ms (/ ns 1000000.0)]
                     [per-op-ns (/ ns n)])
                    (printf "  ~a: ~,2fms total, ~,1fns/op~n" name ms per-op-ns))))

;;; bench-prim : Symbol Args... -> Void
;;; Benchmark a specific primitive.
(define (bench-prim op . args)
  (let ([cost (prim-fuel-cost op)]
        [name (symbol->string op)])
       (bench (format "~a (cost ~a)" name cost)
              (lambda () (apply prim op args))
              *iterations*)))

;;; ============================================================
;;; Benchmark Suites by Tier
;;; ============================================================

(printf "~n=== Primitive Fuel Cost Benchmarks ===~n")
(printf "Iterations per test: ~a~n~n" *iterations*)

;; Prepare test data
(define test-list '(1 2 3 4 5 6 7 8 9 10))
(define test-vec (list->vector test-list))
(define test-bv (make-bytevector 32 42))
(define test-string "hello world")
(define test-block (make-block 'test (string->utf8 "payload") (vector)))

(printf "--- Tier 1 (Cost 1) - O(1) trivial ---~n")
(bench-prim 'number? 42)
(bench-prim 'symbol? 'foo)
(bench-prim 'null? '())
(bench-prim 'pair? '(1 . 2))
(bench-prim 'car '(1 . 2))
(bench-prim 'cdr '(1 . 2))
(bench-prim 'not #f)
(bench-prim 'eq? 42 42)
(bench-prim 'zero? 0)
(bench-prim 'block-tag test-block)

(printf "~n--- Tier 2 (Cost 2) - O(1) simple ---~n")
(bench-prim 'add 100 200)
(bench-prim 'sub 100 50)
(bench-prim 'mul 12 34)
(bench-prim 'neg 42)
(bench-prim 'lt? 10 20)
(bench-prim 'cons 1 2)
(bench-prim 'char->integer #\A)
(bench-prim 'vec-ref test-vec 5)
(bench-prim 'bv-ref test-bv 10)
(bench-prim 'string-ref test-string 5)
(bench-prim 'string-length test-string)
(bench-prim 'vec-length test-vec)

(printf "~n--- Tier 3 (Cost 3) - O(1) moderate ---~n")
(bench-prim 'div 100 7)
(bench-prim 'mod 100 7)
(bench-prim 'bitand #xFF00 #x0FF0)
(bench-prim 'bitor #xFF00 #x00FF)
(bench-prim 'bitxor #xAAAA #x5555)
(bench-prim 'shl 1 10)
(bench-prim 'shr 1024 5)
(bench-prim 'string=? "hello" "hello")
(bench-prim 'string<? "aaa" "bbb")
(bench-prim 'memq 5 test-list)
(bench-prim 'assq 'b '((a . 1) (b . 2) (c . 3)))

(printf "~n--- Tier 4 (Cost 5) - O(n) linear ---~n")
(bench-prim 'length test-list)
(bench-prim 'reverse test-list)
(bench-prim 'vec->list test-vec)
(bench-prim 'list->vec test-list)
(bench-prim 'string->list test-string)
(bench-prim 'list->string '(#\h # #\l #\l #\o))
(bench-prim 'symbol->string 'hello)
(bench-prim 'string->symbol "hello")

(printf "~n--- Tier 5 (Cost 10) - O(n) with allocation ---~n")
(bench-prim 'string-append "hello" " " "world")
(bench-prim 'substring test-string 0 5)
(bench-prim 'bv-slice test-bv 0 16)
(bench-prim 'string->utf8 test-string)
(bench-prim 'utf8->string (string->utf8 test-string))
(bench-prim 'block->bytes test-block)
(bench-prim 'bytes->block (block->bytes test-block))
(bench-prim 'string->number "12345")

(printf "~n--- Tier 6 (Cost 15) - Expensive conversions ---~n")
(bench-prim 'number->string 12345)

(printf "~n--- Tier 7 (Cost 100) - Cryptographic ---~n")
(bench-prim 'sha256 test-bv)

(printf "~n--- Tier 8 (Cost 110) - Serialization + crypto ---~n")
(bench-prim 'hash-block test-block)

;;; ============================================================
;;; Cross-Tier Comparison
;;; ============================================================

(printf "~n=== Cross-Tier Ratio Analysis ===~n")
(printf "Comparing representative ops from each tier...~n~n")

(define (measure-ns thunk n)
  (let-values ([(_ ns) (time-thunk (lambda ()
                                           (let loop ([i n])
                                                (when (> i 0)
                                                      (thunk)
                                                      (loop (- i 1))))))])
              ns))

(define tier1-ns (measure-ns (lambda () (prim 'number? 42)) *iterations*))
(define tier2-ns (measure-ns (lambda () (prim 'add 100 200)) *iterations*))
(define tier3-ns (measure-ns (lambda () (prim 'div 100 7)) *iterations*))
(define tier4-ns (measure-ns (lambda () (prim 'length test-list)) *iterations*))
(define tier5-ns (measure-ns (lambda () (prim 'string-append "a" "b")) *iterations*))
(define tier6-ns (measure-ns (lambda () (prim 'number->string 12345)) *iterations*))
(define tier7-ns (measure-ns (lambda () (prim 'sha256 test-bv)) *iterations*))
(define tier8-ns (measure-ns (lambda () (prim 'hash-block test-block)) *iterations*))

(define (ratio a b) (/ (* 1.0 a) b))

(printf "Tier | Fuel | Time (ns/op) | Ratio vs Tier1 | Expected Ratio~n")
(printf "-----|------|--------------|----------------|---------------~n")
(printf "  1  |   1  | ~10,1f      |    1.0x        |     1x~n" (/ tier1-ns *iterations*))
(printf "  2  |   2  | ~10,1f      | ~5,1fx        |     2x~n" (/ tier2-ns *iterations*) (ratio tier2-ns tier1-ns))
(printf "  3  |   3  | ~10,1f      | ~5,1fx        |     3x~n" (/ tier3-ns *iterations*) (ratio tier3-ns tier1-ns))
(printf "  4  |   5  | ~10,1f      | ~5,1fx        |     5x~n" (/ tier4-ns *iterations*) (ratio tier4-ns tier1-ns))
(printf "  5  |  10  | ~10,1f      | ~5,1fx        |    10x~n" (/ tier5-ns *iterations*) (ratio tier5-ns tier1-ns))
(printf "  6  |  15  | ~10,1f      | ~5,1fx        |    15x~n" (/ tier6-ns *iterations*) (ratio tier6-ns tier1-ns))
(printf "  7  | 100  | ~10,1f      | ~5,1fx        |   100x~n" (/ tier7-ns *iterations*) (ratio tier7-ns tier1-ns))
(printf "  8  | 110  | ~10,1f      | ~5,1fx        |   110x~n" (/ tier8-ns *iterations*) (ratio tier8-ns tier1-ns))

(printf "~n(Note: Tiers 1-6 are dominated by interpreter dispatch overhead.~n")
(printf " Crypto tiers 7-8 show the true cost differential.)~n")
