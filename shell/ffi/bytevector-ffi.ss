;;; shell/ffi/bytevector-ffi.ss — Zero-Copy Bytevector FFI
;;;
;;; Enables passing Scheme bytevectors directly to Rust without copying.
;;; Chez Scheme allows bytevectors to be passed as u8* pointers to foreign
;;; procedures, eliminating element-by-element copying overhead.
;;;
;;; CRITICAL: Bytevectors passed to foreign code must not be relocated by GC
;;; during the call. Use lock-object/unlock-object for safety with callbacks
;;; or long-running operations.
;;;
;;; Usage:
;;;   ;; Create f64 bytevector for 16 doubles
;;;   (define mat (make-f64-bytevector 16))
;;;
;;;   ;; Set values
;;;   (f64-bv-set! mat 0 1.0)
;;;   (f64-bv-set! mat 1 2.0)
;;;
;;;   ;; Pass directly to Rust (no copy!)
;;;   (rust-mat4-mul-bv mat-a mat-b result 10000)
;;;
;;; This module provides:
;;;   - Type-safe bytevector constructors (f64, f32, i64, etc.)
;;;   - Indexed accessors that work in terms of elements, not bytes
;;;   - Direct FFI passing without copying

(load "core/base/prelude.ss")

;;; ============================================================
;;; F64 Bytevector Operations
;;; ============================================================

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

;;; ============================================================
;;; F32 Bytevector Operations
;;; ============================================================

(define (make-f32-bytevector n)
  (make-bytevector (* n 4) 0))

(define (f32-bv-length bv)
  (quotient (bytevector-length bv) 4))

(define (f32-bv-ref bv i)
  (bytevector-ieee-single-native-ref bv (* i 4)))

(define (f32-bv-set! bv i val)
  (bytevector-ieee-single-native-set! bv (* i 4) val))

;;; ============================================================
;;; I64 Bytevector Operations
;;; ============================================================

(define (make-i64-bytevector n)
  (make-bytevector (* n 8) 0))

(define (i64-bv-length bv)
  (quotient (bytevector-length bv) 8))

(define (i64-bv-ref bv i)
  (bytevector-s64-native-ref bv (* i 8)))

(define (i64-bv-set! bv i val)
  (bytevector-s64-native-set! bv (* i 8) val))

;;; ============================================================
;;; U64 Bytevector Operations
;;; ============================================================

(define (make-u64-bytevector n)
  (make-bytevector (* n 8) 0))

(define (u64-bv-length bv)
  (quotient (bytevector-length bv) 8))

(define (u64-bv-ref bv i)
  (bytevector-u64-native-ref bv (* i 8)))

(define (u64-bv-set! bv i val)
  (bytevector-u64-native-set! bv (* i 8) val))

;;; ============================================================
;;; Safe Locking for Long-Running FFI
;;; ============================================================

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

;;; ============================================================
;;; Mat4 Bytevector Helpers
;;; ============================================================

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
