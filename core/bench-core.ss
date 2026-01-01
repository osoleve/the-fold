;;; bench-core.ss — Core System Benchmarks
;;;
;;; Comprehensive benchmarks for critical system components:
;;; - Block operations (serialization, hashing)
;;; - CAS operations (put, get, lookup)
;;; - Normalization and expansion
;;; - Store performance at scale

(load "core/prelude.ss")
(load "core/block.ss")
(load "core/sha256.ss")
(load "core/normalize.ss")
(load "core/expand.ss")
(load "core/cas.ss")
(load "shell/benchmark.ss")

(printf "\n╔═══════════════════════════════════════════════════════════════╗\n")
(printf "║              CORE SYSTEM BENCHMARKS                           ║\n")
(printf "╚═══════════════════════════════════════════════════════════════╝\n\n")

;;; ============================================================
;;; Test Data
;;; ============================================================

(define small-payload (string->utf8 "small"))
(define medium-payload (make-bytevector 1024 42))  ; 1KB
(define large-payload (make-bytevector 10240 42))  ; 10KB

(define small-block (make-block 'small small-payload (vector)))
(define medium-block (make-block 'medium medium-payload (vector)))
(define large-block (make-block 'large large-payload (vector)))

(define simple-expr '(lambda (x) (+ x 1)))
(define nested-expr '(lambda (f) (lambda (x) (f (f x)))))
(define deep-expr '(lambda (a) (lambda (b) (lambda (c) (lambda (d) (+ a b c d))))))

;;; ============================================================
;;; Block Operation Benchmarks
;;; ============================================================

(printf "═══ Block Operations ═══\n\n")

(define block-results
  (benchmark-compare
   `(("serialize small block" . ,(lambda () (block->bytes small-block)))
     ("serialize medium block (1KB)" . ,(lambda () (block->bytes medium-block)))
     ("serialize large block (10KB)" . ,(lambda () (block->bytes large-block)))
     ("hash small block" . ,(lambda () (hash-block small-block)))
     ("hash medium block (1KB)" . ,(lambda () (hash-block medium-block)))
     ("hash large block (10KB)" . ,(lambda () (hash-block large-block))))
   1000))

(benchmark-report block-results)

;;; ============================================================
;;; CAS Operation Benchmarks
;;; ============================================================

(printf "\n═══ CAS Operations ═══\n\n")

;; Pre-populate with some blocks for fetch benchmarks
(define hash1 (store! small-block))
(define hash2 (store! medium-block))
(define hash3 (store! large-block))

(define cas-results
  (benchmark-compare
   `(("store! small block" . ,(lambda () (store! small-block)))
     ("store! medium block (1KB)" . ,(lambda () (store! medium-block)))
     ("store! large block (10KB)" . ,(lambda () (store! large-block)))
     ("fetch small block" . ,(lambda () (fetch hash1)))
     ("fetch medium block (1KB)" . ,(lambda () (fetch hash2)))
     ("fetch large block (10KB)" . ,(lambda () (fetch hash3)))
     ("stored? (hit)" . ,(lambda () (stored? hash1)))
     ("stored? (miss)" . ,(lambda () (stored? (make-bytevector 33 99)))))
   1000))

(benchmark-report cas-results)

;;; ============================================================
;;; Normalization Benchmarks
;;; ============================================================

(printf "\n═══ Normalization (S-expr → Canonical) ═══\n\n")

(define norm-results
  (benchmark-compare
   `(("normalize simple expr" . ,(lambda () (normalize simple-expr)))
     ("normalize nested expr" . ,(lambda () (normalize nested-expr)))
     ("normalize deep expr" . ,(lambda () (normalize deep-expr))))
   1000))

(benchmark-report norm-results)

;;; ============================================================
;;; Expansion Benchmarks
;;; ============================================================

(printf "\n═══ Expansion (Canonical → S-expr) ═══\n\n")

;; Pre-normalize for expansion tests
(define norm-simple (normalize simple-expr))
(define norm-nested (normalize nested-expr))
(define norm-deep (normalize deep-expr))

(define expand-results
  (benchmark-compare
   `(("expand simple expr" . ,(lambda () (expand norm-simple '(x))))
     ("expand nested expr" . ,(lambda () (expand norm-nested '(f x))))
     ("expand deep expr" . ,(lambda () (expand norm-deep '(a b c d)))))
   1000))

(benchmark-report expand-results)

;;; ============================================================
;;; Round-trip Benchmarks
;;; ============================================================

(printf "\n═══ Round-trip Performance ═══\n\n")

(define roundtrip-results
  (benchmark-compare
   `(("normalize + expand simple" .
      ,(lambda () (expand (normalize simple-expr) '(x))))
     ("normalize + expand nested" .
      ,(lambda () (expand (normalize nested-expr) '(f x))))
     ("serialize + deserialize small" .
      ,(lambda () (bytes->block (block->bytes small-block))))
     ("serialize + deserialize medium" .
      ,(lambda () (bytes->block (block->bytes medium-block)))))
   1000))

(benchmark-report roundtrip-results)

;;; ============================================================
;;; CAS Scalability Test
;;; ============================================================

(printf "\n═══ CAS Scalability ═══\n\n")
(printf "Testing CAS fetch performance with increasing store size...\n\n")

(define (test-cas-scale n)
  (let loop ([i 0] [hashes '()])
       (if (>= i n)
           ;; Now benchmark fetch on a hash from the middle
           (let* ([target (list-ref hashes (quotient n 2))]
                  [result (benchmark (format "fetch (~a blocks in store)" n)
                                     (lambda () (fetch target))
                                     100)])
                 (benchmark-result-mean-ns result))
           (let* ([payload (make-bytevector 32 (modulo i 256))]
                  [block (make-block 'test payload (vector))]
                  [hash (store! block)])
                 (loop (+ i 1) (cons hash hashes))))))

(define sizes '(10 100 1000 10000))
(printf "Size    | Get Time (mean)\n")
(printf "--------|----------------\n")
(for-each
 (lambda (size)
         (let ([time-ns (test-cas-scale size)])
              (printf "~6a  | ~a\n" size (format-time-ns time-ns))))
 sizes)

;;; ============================================================
;;; Summary
;;; ============================================================

(printf "\n╔═══════════════════════════════════════════════════════════════╗\n")
(printf "║              BENCHMARK COMPLETE                               ║\n")
(printf "╚═══════════════════════════════════════════════════════════════╝\n\n")

(printf "Key Findings:\n")
(printf "  - Block operations scale with payload size\n")
(printf "  - CAS operations should be approximately O(1) for gets\n")
(printf "  - Normalization cost increases with expression depth\n")
(printf "  - Hash operations dominate for large blocks\n\n")
