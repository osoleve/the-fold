;;; core/autodiff/test-grad-store.ss --- Tests for Gradient Storage
;;;
;;; Tests for gradient storage, serialization, and checkpoint management.

(load "core/autodiff/grad-store.ss")

;;; ============================================================
;;; Test Framework
;;; ============================================================

(define *test-count* 0)
(define *pass-count* 0)
(define *fail-count* 0)

(define (test name expected actual)
  (set! *test-count* (+ *test-count* 1))
  (if (equal? expected actual)
      (begin
       (set! *pass-count* (+ *pass-count* 1))
       (display "✓ ")
       (display name)
       (newline))
      (begin
       (set! *fail-count* (+ *fail-count* 1))
       (display "✗ ")
       (display name)
       (newline)
       (display "  Expected: ")
       (write expected)
       (newline)
       (display "  Actual:   ")
       (write actual)
       (newline))))

(define (test-true name actual)
  (test name #t actual))

(define (test-false name actual)
  (test name #f actual))

(define (test-approx name expected actual epsilon)
  (test name #t (< (abs (- expected actual)) epsilon)))

(define (run-tests)
  (display "\n=== Gradient Storage Tests ===\n\n")
  
  ;;; ============================================================
  ;;; GradientSet Construction
  ;;; ============================================================
  
  (display "--- GradientSet Construction ---\n")
  
  (let ([dense (dense-grads '#(1.0 2.0 3.0))])
       (test-true "dense-grads creates dense set" (dense-grads? dense))
       (test-false "dense is not sparse" (sparse-grads? dense))
       (test "dense data is vector" '#(1.0 2.0 3.0) (grads-data dense)))
  
  (let ([sparse (sparse-grads '((0 . 1.0) (5 . 2.0) (10 . 3.0)))])
       (test-true "sparse-grads creates sparse set" (sparse-grads? sparse))
       (test-false "sparse is not dense" (dense-grads? sparse))
       (test "sparse size" 3 (grads-size sparse)))
  
  ;;; ============================================================
  ;;; Gradient Lookup
  ;;; ============================================================
  
  (display "\n--- Gradient Lookup ---\n")
  
  (let ([dense (dense-grads '#(1.0 2.0 3.0 4.0 5.0))])
       (test "grads-ref dense 0" 1.0 (grads-ref dense 0))
       (test "grads-ref dense 2" 3.0 (grads-ref dense 2))
       (test "grads-ref dense 4" 5.0 (grads-ref dense 4))
       (test "grads-ref dense out of bounds" 0 (grads-ref dense 10))
       (test "grads-size dense" 5 (grads-size dense)))
  
  (let ([sparse (sparse-grads '((0 . 1.5) (5 . 2.5) (100 . 3.5)))])
       (test "grads-ref sparse 0" 1.5 (grads-ref sparse 0))
       (test "grads-ref sparse 5" 2.5 (grads-ref sparse 5))
       (test "grads-ref sparse 100" 3.5 (grads-ref sparse 100))
       (test "grads-ref sparse missing" 0 (grads-ref sparse 50))
       (test "grads-size sparse" 3 (grads-size sparse)))
  
  ;;; ============================================================
  ;;; grads-ids
  ;;; ============================================================
  
  (display "\n--- grads-ids ---\n")
  
  (let ([dense (dense-grads '#(0 1.0 0 2.0 0))])
       (test "grads-ids dense (non-zero only)" '(1 3) (grads-ids dense)))
  
  (let ([sparse (sparse-grads '((2 . 1.0) (7 . 2.0) (15 . 3.0)))])
       (test "grads-ids sparse" '(2 7 15) (grads-ids sparse)))
  
  ;;; ============================================================
  ;;; Hashtable Conversion
  ;;; ============================================================
  
  (display "\n--- Hashtable Conversion ---\n")
  
  ;; Create a hashtable with some gradients
  (let* ([ht (make-hashtable equal-hash equal?)]
         [_ (begin
             (hashtable-set! ht 0 1.0)
             (hashtable-set! ht 1 2.0)
             (hashtable-set! ht 2 3.0))]
         [gs (hashtable->grads ht)])
        (test-true "dense format for high density" (dense-grads? gs))
        (test "round-trip value 0" 1.0 (grads-ref gs 0))
        (test "round-trip value 1" 2.0 (grads-ref gs 1))
        (test "round-trip value 2" 3.0 (grads-ref gs 2)))
  
  ;; Sparse hashtable
  (let* ([ht (make-hashtable equal-hash equal?)]
         [_ (begin
             (hashtable-set! ht 0 1.0)
             (hashtable-set! ht 100 2.0))]
         [gs (hashtable->grads ht)])
        (test-true "sparse format for low density" (sparse-grads? gs))
        (test "sparse round-trip 0" 1.0 (grads-ref gs 0))
        (test "sparse round-trip 100" 2.0 (grads-ref gs 100)))
  
  ;; Round-trip through hashtable
  (let* ([original (sparse-grads '((5 . 1.5) (10 . 2.5) (15 . 3.5)))]
         [ht (grads->hashtable original)]
         [restored (hashtable->grads ht)])
        (test "round-trip sparse->ht->sparse 5" 1.5 (grads-ref restored 5))
        (test "round-trip sparse->ht->sparse 10" 2.5 (grads-ref restored 10))
        (test "round-trip sparse->ht->sparse 15" 3.5 (grads-ref restored 15)))
  
  ;;; ============================================================
  ;;; Serialization
  ;;; ============================================================
  
  (display "\n--- Serialization ---\n")
  
  ;; Dense serialization round-trip
  (let* ([gs (dense-grads '#(1.0 2.0 3.0 4.0 5.0))]
         [bv (serialize-grads gs "test-dense" 12345)])
        (let-values ([(restored name ts) (deserialize-grads bv)])
                    (test "deserialize name" "test-dense" name)
                    (test "deserialize timestamp" 12345 ts)
                    (test-true "restored is dense" (dense-grads? restored))
                    (test "restored size" 5 (grads-size restored))
                    (test-approx "restored value 0" 1.0 (grads-ref restored 0) 0.0001)
                    (test-approx "restored value 4" 5.0 (grads-ref restored 4) 0.0001)))
  
  ;; Sparse serialization round-trip
  (let* ([gs (sparse-grads '((0 . 1.5) (50 . 2.5) (100 . 3.5)))]
         [bv (serialize-grads gs "test-sparse" 67890)])
        (let-values ([(restored name ts) (deserialize-grads bv)])
                    (test "sparse deserialize name" "test-sparse" name)
                    (test "sparse deserialize timestamp" 67890 ts)
                    (test-true "restored is sparse" (sparse-grads? restored))
                    (test "sparse restored size" 3 (grads-size restored))
                    (test-approx "sparse restored 0" 1.5 (grads-ref restored 0) 0.0001)
                    (test-approx "sparse restored 50" 2.5 (grads-ref restored 50) 0.0001)
                    (test-approx "sparse restored 100" 3.5 (grads-ref restored 100) 0.0001)))
  
  ;;; ============================================================
  ;;; Block Storage
  ;;; ============================================================
  
  (display "\n--- Block Storage ---\n")
  
  ;; Store and fetch dense gradients
  (let* ([gs (dense-grads '#(1.0 2.0 3.0))]
         [addr (store-grads! gs "checkpoint-1" 11111 #f #f)]
         [blk (fetch addr)])
        (test-true "stored block exists" (not (not blk)))
        (test "stored block tag" 'grad-store (block-tag blk)))
  
  ;; Fetch gradients back
  (let* ([ht (make-hashtable equal-hash equal?)]
         [_ (begin
             (hashtable-set! ht 0 10.0)
             (hashtable-set! ht 1 20.0)
             (hashtable-set! ht 2 30.0))]
         [addr (save-gradients ht "my-checkpoint")]
         [loaded (load-gradients addr)])
        (test-true "load-gradients returns hashtable" (hashtable? loaded))
        (test "loaded gradient 0" 10.0 (hashtable-ref loaded 0 0))
        (test "loaded gradient 1" 20.0 (hashtable-ref loaded 1 0))
        (test "loaded gradient 2" 30.0 (hashtable-ref loaded 2 0)))
  
  ;;; ============================================================
  ;;; Gradient Info
  ;;; ============================================================
  
  (display "\n--- Gradient Info ---\n")
  
  (let* ([gs (dense-grads '#(1.0 2.0 3.0 4.0 5.0))]
         [addr (store-grads! gs "info-test" 99999 #f #f)]
         [info (gradient-info addr)])
        (test-true "gradient-info returns alist" (list? info))
        (test "info name" "info-test" (cdr (assq 'name info)))
        (test "info timestamp" 99999 (cdr (assq 'timestamp info)))
        (test "info size" 5 (cdr (assq 'size info)))
        (test "info format" 'dense (cdr (assq 'format info)))
        (test-false "info has-previous" (cdr (assq 'has-previous info)))
        (test-false "info has-graph" (cdr (assq 'has-graph info))))
  
  ;;; ============================================================
  ;;; Versioning
  ;;; ============================================================
  
  (display "\n--- Versioning ---\n")
  
  (let* ([ht1 (make-hashtable equal-hash equal?)]
         [_ (hashtable-set! ht1 0 1.0)]
         [addr1 (save-gradients ht1 "v1")]
         [ht2 (make-hashtable equal-hash equal?)]
         [_ (hashtable-set! ht2 0 2.0)]
         [addr2 (save-gradients-versioned ht2 "v2" addr1)]
         [ht3 (make-hashtable equal-hash equal?)]
         [_ (hashtable-set! ht3 0 3.0)]
         [addr3 (save-gradients-versioned ht3 "v3" addr2)])
        (test "checkpoint-count" 3 (checkpoint-count addr3))
        (let ([history (checkpoint-history addr3)])
             (test "history length" 3 (length history))))
  
  ;;; ============================================================
  ;;; Gradient Arithmetic
  ;;; ============================================================
  
  (display "\n--- Gradient Arithmetic ---\n")
  
  ;; grads-add
  (let* ([gs1 (dense-grads '#(1.0 2.0 3.0))]
         [gs2 (dense-grads '#(4.0 5.0 6.0))]
         [sum (grads-add gs1 gs2)])
        (test-approx "grads-add 0" 5.0 (grads-ref sum 0) 0.0001)
        (test-approx "grads-add 1" 7.0 (grads-ref sum 1) 0.0001)
        (test-approx "grads-add 2" 9.0 (grads-ref sum 2) 0.0001))
  
  ;; grads-scale
  (let* ([gs (dense-grads '#(1.0 2.0 3.0))]
         [scaled (grads-scale gs 2.0)])
        (test-approx "grads-scale 0" 2.0 (grads-ref scaled 0) 0.0001)
        (test-approx "grads-scale 1" 4.0 (grads-ref scaled 1) 0.0001)
        (test-approx "grads-scale 2" 6.0 (grads-ref scaled 2) 0.0001))
  
  ;; grads-norm
  (let ([gs (dense-grads '#(3.0 4.0))])
       (test-approx "grads-norm 3-4-5 triangle" 5.0 (grads-norm gs) 0.0001))
  
  (let ([gs (sparse-grads '((0 . 3.0) (1 . 4.0)))])
       (test-approx "grads-norm sparse" 5.0 (grads-norm gs) 0.0001))
  
  ;; grads-clip
  (let* ([gs (dense-grads '#(30.0 40.0))]  ; norm = 50
         [clipped (grads-clip gs 10.0)])
        (test-approx "grads-clip norm" 10.0 (grads-norm clipped) 0.0001))
  
  (let* ([gs (dense-grads '#(3.0 4.0))]  ; norm = 5, below threshold
         [clipped (grads-clip gs 10.0)])
        (test-approx "grads-clip no-op" 5.0 (grads-norm clipped) 0.0001))
  
  ;;; ============================================================
  ;;; Null Reference Handling
  ;;; ============================================================
  
  (display "\n--- Null Reference Handling ---\n")
  
  (let ([null-ref (make-null-ref)])
       (test-true "make-null-ref creates null" (null-ref? null-ref))
       (test "null-ref size" address-size (bytevector-length null-ref)))
  
  (let ([non-null (make-bytevector address-size 1)])
       (test-false "non-null ref detected" (null-ref? non-null)))
  
  ;;; ============================================================
  ;;; Summary
  ;;; ============================================================
  
  (display "\n=== Test Summary ===\n")
  (display "Total:  ")
  (display *test-count*)
  (newline)
  (display "Passed: ")
  (display *pass-count*)
  (newline)
  (display "Failed: ")
  (display *fail-count*)
  (newline)
  
  (if (= *fail-count* 0)
      (display "\n✓ All tests passed!\n")
      (begin
       (display "\n✗ Some tests failed.\n")
       (exit 1))))

;; Run tests
(run-tests)
