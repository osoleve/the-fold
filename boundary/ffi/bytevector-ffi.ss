(load "core/base/prelude.ss")

(doc 'module 'bytevector-ffi)
(doc 'description "Zero-Copy Bytevector FFI — Enables passing Scheme bytevectors directly to Rust without copying")
(doc 'layer 'boundary)
(doc 'purity 'total)

(doc 'note "Chez Scheme allows bytevectors to be passed as u8* pointers to foreign procedures, eliminating element-by-element copying overhead")

(doc 'critical "Bytevectors passed to foreign code must not be relocated by GC during the call. Use lock-object/unlock-object for safety with callbacks or long-running operations.")

(doc 'provides '(
  "Type-safe bytevector constructors (f64, f32, i64, etc.)"
  "Indexed accessors that work in terms of elements, not bytes"
  "Direct FFI passing without copying"))

(doc 'usage "
  ;; Create f64 bytevector for 16 doubles
  (define mat (make-f64-bytevector 16))

  ;; Set values
  (f64-bv-set! mat 0 1.0)
  (f64-bv-set! mat 1 2.0)

  ;; Pass directly to Rust (no copy!)
  (rust-mat4-mul-bv mat-a mat-b result 10000)
")

(doc 'section 'f64-bytevector-operations)

;;; make-f64-bytevector : Nat → Bytevector
;;; Create a bytevector sized for N f64 values (N * 8 bytes)
(define (make-f64-bytevector n)
  (make-bytevector (* n 8) 0))

;;; f64-bv-length : Bytevector → Nat
;;; Number of f64 elements in bytevector
(define (f64-bv-length bv)
  (quotient (bytevector-length bv) 8))

;;; f64-bv-ref : Bytevector × Nat → Real
;;; Get f64 at index (not byte offset)
(define (f64-bv-ref bv i)
  (bytevector-ieee-double-native-ref bv (* i 8)))

;;; f64-bv-set! : Bytevector × Nat × Real → Void
;;; Set f64 at index (not byte offset)
(define (f64-bv-set! bv i val)
  (bytevector-ieee-double-native-set! bv (* i 8) val))

;;; vector->f64-bytevector : Vector → Bytevector
;;; Convert Scheme vector to f64 bytevector
(define (vector->f64-bytevector vec)
  (let* ([n (vector-length vec)]
         [bv (make-f64-bytevector n)])
        (do ([i 0 (+ i 1)])
            ((= i n) bv)
            (f64-bv-set! bv i (vector-ref vec i)))))

;;; f64-bytevector->vector : Bytevector → Vector
;;; Convert f64 bytevector to Scheme vector
(define (f64-bytevector->vector bv)
  (let* ([n (f64-bv-length bv)]
         [vec (make-vector n)])
        (do ([i 0 (+ i 1)])
            ((= i n) vec)
            (vector-set! vec i (f64-bv-ref bv i)))))

;;; list->f64-bytevector : List → Bytevector
;;; Convert list of numbers to f64 bytevector
(define (list->f64-bytevector lst)
  (let* ([n (length lst)]
         [bv (make-f64-bytevector n)])
        (let loop ([lst lst] [i 0])
             (if (null? lst)
                 bv
                 (begin
                  (f64-bv-set! bv i (car lst))
                  (loop (cdr lst) (+ i 1)))))))

;;; ====
;;; F32 Bytevector Operations
;;; ====

(define (make-f32-bytevector n)
  (make-bytevector (* n 4) 0))

(define (f32-bv-length bv)
  (quotient (bytevector-length bv) 4))

(define (f32-bv-ref bv i)
  (bytevector-ieee-single-native-ref bv (* i 4)))

(define (f32-bv-set! bv i val)
  (bytevector-ieee-single-native-set! bv (* i 4) val))

;;; ====
;;; I64 Bytevector Operations
;;; ====

(define (make-i64-bytevector n)
  (make-bytevector (* n 8) 0))

(define (i64-bv-length bv)
  (quotient (bytevector-length bv) 8))

(define (i64-bv-ref bv i)
  (bytevector-s64-native-ref bv (* i 8)))

(define (i64-bv-set! bv i val)
  (bytevector-s64-native-set! bv (* i 8) val))

;;; ====
;;; U64 Bytevector Operations
;;; ====

(define (make-u64-bytevector n)
  (make-bytevector (* n 8) 0))

(define (u64-bv-length bv)
  (quotient (bytevector-length bv) 8))

(define (u64-bv-ref bv i)
  (bytevector-u64-native-ref bv (* i 8)))

(define (u64-bv-set! bv i val)
  (bytevector-u64-native-set! bv (* i 8) val))

;;; ====
;;; Safe Locking for Long-Running FFI
;;; ====

;;; with-locked-bytevector : Bytevector × (→ α) → α
;;; Lock bytevector during thunk execution to prevent GC relocation.
;;; Use for callbacks or async operations where GC might run.
(define (with-locked-bytevector bv thunk)
  (lock-object bv)
  (dynamic-wind
   (lambda () #f)
   thunk
   (lambda () (unlock-object bv))))

;;; with-locked-bytevectors : (List Bytevector) × (→ α) → α
;;; Lock multiple bytevectors during thunk execution
(define (with-locked-bytevectors bvs thunk)
  (for-each lock-object bvs)
  (dynamic-wind
   (lambda () #f)
   thunk
   (lambda () (for-each unlock-object bvs))))

;;; ====
;;; Mat4 Bytevector Helpers
;;; ====

;;; make-mat4-bytevector : → Bytevector
;;; Create bytevector for 4x4 matrix (16 f64s = 128 bytes)
(define (make-mat4-bytevector)
  (make-f64-bytevector 16))

;;; mat4-identity! : Bytevector → Void
;;; Fill bytevector with identity matrix
(define (mat4-identity! bv)
  (do ([i 0 (+ i 1)])
      ((= i 16))
      (f64-bv-set! bv i 0.0))
  (f64-bv-set! bv 0 1.0)
  (f64-bv-set! bv 5 1.0)
  (f64-bv-set! bv 10 1.0)
  (f64-bv-set! bv 15 1.0))

;;; Points buffer for batch transforms
;;; Layout: [x0 y0 z0 w0 x1 y1 z1 w1 ...]
(define (make-points-bytevector n)
  (make-f64-bytevector (* n 4)))

(define (points-bv-ref bv i component)
  ;; component: 0=x, 1=y, 2=z, 3=w
  (f64-bv-ref bv (+ (* i 4) component)))

(define (points-bv-set! bv i x y z w)
  (let ([base (* i 4)])
       (f64-bv-set! bv base x)
       (f64-bv-set! bv (+ base 1) y)
       (f64-bv-set! bv (+ base 2) z)
       (f64-bv-set! bv (+ base 3) w)))

;;; ====
;;; Layer 2 Rust-Accelerated Bytevector Operations (fold-gu3t)
;;; ====
;;;
;;; These functions call into Rust for accelerated operations.
;;; Requires the rust-accel library to be loaded.

(load "boundary/ffi/rust-loader.ss")

;;; Registry flags for lazy loading
(define *bv-ffi-loaded* #f)
(define *str-ffi-loaded* #f)

;;; ensure-bv-ffi! : → Void
;;; Load bytevector FFI functions on first use.
(define (ensure-bv-ffi!)
  (unless *bv-ffi-loaded*
          (rust-load-fn! "fold_bv_hash" '(u8* u64) 'u64)
          (rust-load-fn! "fold_bv_compare" '(u8* u64 u8* u64) 'i64)
          (rust-load-fn! "fold_bv_copy" '(u8* u64 u8* u64 u64) 'buffer)
          (rust-load-fn! "fold_bv_fill" '(u8* u64 u8) 'buffer)
          (rust-load-fn! "fold_bv_equal" '(u8* u64 u8* u64) 'bool)
          (set! *bv-ffi-loaded* #t)))

;;; ensure-str-ffi! : → Void
;;; Load string FFI functions on first use.
(define (ensure-str-ffi!)
  (unless *str-ffi-loaded*
          (rust-load-fn! "fold_levenshtein" '(u8* u64 u8* u64) 'u64)
          (rust-load-fn! "fold_str_contains" '(u8* u64 u8* u64) 'bool)
          (rust-load-fn! "fold_str_index_of" '(u8* u64 u8* u64) 'i64)
          (rust-load-fn! "fold_str_last_index_of" '(u8* u64 u8* u64) 'i64)
          (rust-load-fn! "fold_str_upcase" '(u8* u64 u8* u64) 'buffer)
          (rust-load-fn! "fold_str_downcase" '(u8* u64 u8* u64) 'buffer)
          (rust-load-fn! "fold_str_starts_with" '(u8* u64 u8* u64) 'bool)
          (rust-load-fn! "fold_str_ends_with" '(u8* u64 u8* u64) 'bool)
          (set! *str-ffi-loaded* #t)))

;;; ====
;;; Bytevector Operations
;;; ====

;;; rust-bv-hash : Bytevector × Fuel → Result
;;; FNV-1a hash of bytevector contents.
;;; Returns (ok hash fuel-remaining) on success.
(define (rust-bv-hash bv fuel)
  (ensure-bv-ffi!)
  (rust-call "fold_bv_hash" bv (bytevector-length bv) fuel))

;;; rust-bv-compare : Bytevector × Bytevector × Fuel → Result
;;; Lexicographic comparison of two bytevectors.
;;; Returns (ok -1|0|1 fuel-remaining) on success.
(define (rust-bv-compare bv1 bv2 fuel)
  (ensure-bv-ffi!)
  (rust-call "fold_bv_compare"
             bv1 (bytevector-length bv1)
             bv2 (bytevector-length bv2)
             fuel))

;;; rust-bv-equal? : Bytevector × Bytevector × Fuel → Result
;;; Test if two bytevectors are equal.
;;; Returns (ok #t|#f fuel-remaining) on success.
(define (rust-bv-equal? bv1 bv2 fuel)
  (ensure-bv-ffi!)
  (rust-call "fold_bv_equal"
             bv1 (bytevector-length bv1)
             bv2 (bytevector-length bv2)
             fuel))

;;; rust-bv-copy! : Bytevector × Bytevector × Nat × Fuel → Result
;;; Copy n bytes from src to dst.
;;; Returns (ok bytes-written fuel-remaining) on success.
(define (rust-bv-copy! src dst n fuel)
  (ensure-bv-ffi!)
  (rust-call "fold_bv_copy"
             src (bytevector-length src)
             dst (bytevector-length dst)
             n
             fuel))

;;; rust-bv-fill! : Bytevector × Byte × Fuel → Result
;;; Fill bytevector with a value.
;;; Returns (ok bytes-written fuel-remaining) on success.
(define (rust-bv-fill! bv val fuel)
  (ensure-bv-ffi!)
  (rust-call "fold_bv_fill"
             bv (bytevector-length bv)
             val
             fuel))

;;; ====
;;; String Operations (Pre-validated UTF-8)
;;; ====
;;;
;;; These functions accept Scheme strings and convert them to UTF-8
;;; bytevectors for processing in Rust. The UTF-8 is pre-validated
;;; by Scheme's string->utf8.

;;; rust-levenshtein : String × String × Fuel → Result
;;; Compute Levenshtein edit distance between two strings.
;;; Returns (ok distance fuel-remaining) on success.
(define (rust-levenshtein s1 s2 fuel)
  (ensure-str-ffi!)
  (let ([bv1 (string->utf8 s1)]
        [bv2 (string->utf8 s2)])
       (rust-call "fold_levenshtein"
                  bv1 (bytevector-length bv1)
                  bv2 (bytevector-length bv2)
                  fuel)))

;;; rust-str-contains? : String × String × Fuel → Result
;;; Check if haystack contains needle.
;;; Returns (ok #t|#f fuel-remaining) on success.
(define (rust-str-contains? haystack needle fuel)
  (ensure-str-ffi!)
  (let ([bv-h (string->utf8 haystack)]
        [bv-n (string->utf8 needle)])
       (rust-call "fold_str_contains"
                  bv-h (bytevector-length bv-h)
                  bv-n (bytevector-length bv-n)
                  fuel)))

;;; rust-str-index-of : String × String × Fuel → Result
;;; Find first occurrence of needle in haystack.
;;; Returns (ok index fuel-remaining) where index is -1 if not found.
(define (rust-str-index-of haystack needle fuel)
  (ensure-str-ffi!)
  (let ([bv-h (string->utf8 haystack)]
        [bv-n (string->utf8 needle)])
       (rust-call "fold_str_index_of"
                  bv-h (bytevector-length bv-h)
                  bv-n (bytevector-length bv-n)
                  fuel)))

;;; rust-str-last-index-of : String × String × Fuel → Result
;;; Find last occurrence of needle in haystack.
;;; Returns (ok index fuel-remaining) where index is -1 if not found.
(define (rust-str-last-index-of haystack needle fuel)
  (ensure-str-ffi!)
  (let ([bv-h (string->utf8 haystack)]
        [bv-n (string->utf8 needle)])
       (rust-call "fold_str_last_index_of"
                  bv-h (bytevector-length bv-h)
                  bv-n (bytevector-length bv-n)
                  fuel)))

;;; rust-str-starts-with? : String × String × Fuel → Result
;;; Check if string starts with prefix.
;;; Returns (ok #t|#f fuel-remaining) on success.
(define (rust-str-starts-with? s prefix fuel)
  (ensure-str-ffi!)
  (let ([bv-s (string->utf8 s)]
        [bv-p (string->utf8 prefix)])
       (rust-call "fold_str_starts_with"
                  bv-s (bytevector-length bv-s)
                  bv-p (bytevector-length bv-p)
                  fuel)))

;;; rust-str-ends-with? : String × String × Fuel → Result
;;; Check if string ends with suffix.
;;; Returns (ok #t|#f fuel-remaining) on success.
(define (rust-str-ends-with? s suffix fuel)
  (ensure-str-ffi!)
  (let ([bv-s (string->utf8 s)]
        [bv-suf (string->utf8 suffix)])
       (rust-call "fold_str_ends_with"
                  bv-s (bytevector-length bv-s)
                  bv-suf (bytevector-length bv-suf)
                  fuel)))

;;; rust-str-upcase : String × Fuel → Result
;;; Convert string to uppercase (ASCII only).
;;; Returns (ok result-string fuel-remaining) on success.
(define (rust-str-upcase s fuel)
  (ensure-str-ffi!)
  (let* ([bv-src (string->utf8 s)]
         [len (bytevector-length bv-src)]
         [bv-dst (make-bytevector len)])
        (let ([result (rust-call "fold_str_upcase"
                                 bv-src len
                                 bv-dst len
                                 fuel)])
             (if (and (pair? result) (eq? (car result) 'ok))
                 ;; Convert result back to string
                 `(ok ,(utf8->string bv-dst) ,(caddr result))
                 result))))

;;; rust-str-downcase : String × Fuel → Result
;;; Convert string to lowercase (ASCII only).
;;; Returns (ok result-string fuel-remaining) on success.
(define (rust-str-downcase s fuel)
  (ensure-str-ffi!)
  (let* ([bv-src (string->utf8 s)]
         [len (bytevector-length bv-src)]
         [bv-dst (make-bytevector len)])
        (let ([result (rust-call "fold_str_downcase"
                                 bv-src len
                                 bv-dst len
                                 fuel)])
             (if (and (pair? result) (eq? (car result) 'ok))
                 ;; Convert result back to string
                 `(ok ,(utf8->string bv-dst) ,(caddr result))
                 result))))
