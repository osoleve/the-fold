;;; boundary/gpu/test-gpu-extended.ss — Extended GPU bridge tests
;;;
;;; Tests for: additional parameter types (f64, u64, i32), error conditions,
;;; and the 64-parameter limit. Created for fold-zxua.

(load "core/testing/test-framework.ss")
(load "boundary/gpu/runtime.ss")

;;; ====
;;; Test Kernels for Different Parameter Types
;;; ====

;;; Kernel that uses f64 (double precision)
(define *daxpy-source*
  "extern \"C\" __global__ void daxpy(double a, double *x, double *y, int n) {
    int i = blockIdx.x * blockDim.x + threadIdx.x;
    if (i < n) {
        y[i] = a * x[i] + y[i];
    }
}")

;;; Kernel that uses i32 (signed integers)
(define *add-offset-source*
  "extern \"C\" __global__ void add_offset(int *data, int offset, int n) {
    int i = blockIdx.x * blockDim.x + threadIdx.x;
    if (i < n) {
        data[i] = data[i] + offset;
    }
}")

;;; Kernel that uses u64 for large index computation
(define *scale-u64-source*
  "extern \"C\" __global__ void scale_u64(unsigned long long *data,
                                          unsigned long long scale,
                                          int n) {
    int i = blockIdx.x * blockDim.x + threadIdx.x;
    if (i < n) {
        data[i] = data[i] * scale;
    }
}")

;;; Kernel with many parameters (for limit testing)
(define *many-params-source*
  "extern \"C\" __global__ void many_params(
    float p0, float p1, float p2, float p3, float p4,
    float p5, float p6, float p7, float p8, float p9,
    float *out, int n) {
    int i = blockIdx.x * blockDim.x + threadIdx.x;
    if (i < n) {
        out[i] = p0 + p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9;
    }
}")

;;; ====
;;; Setup
;;; ====

(test-group "gpu-extended-setup"

  (define-test "ensure GPU initialized"
    (unless (gpu-available?)
      (gpu-init!))
    (assert-true (gpu-available?))))

;;; ====
;;; Parameter Type Tests (G01)
;;; ====

(test-group "gpu-parameter-types"

  (define-test "f64 (double precision) parameter type"
    ;; DAXPY: y = a*x + y using double precision
    (let* ([n 256]
           [byte-size (* n 8)]  ;; 8 bytes per double
           [x-host (make-bytevector byte-size)]
           [y-host (make-bytevector byte-size)])
      ;; Fill with test values
      (do ([i 0 (+ i 1)]) ((= i n))
        (bytevector-ieee-double-native-set! x-host (* i 8) 1.0)
        (bytevector-ieee-double-native-set! y-host (* i 8) 2.0))
      (let ([module (gpu-compile *daxpy-source*)])
        (let ([func (gpu-get-function module "daxpy")])
          (with-gpu-memory byte-size
            (lambda (d-x)
              (with-gpu-memory byte-size
                (lambda (d-y)
                  (gpu-copy-to! d-x x-host)
                  (gpu-copy-to! d-y y-host)
                  ;; y = 3.0 * 1.0 + 2.0 = 5.0
                  (gpu-launch! func
                    (ceiling (/ n 256)) 256
                    `((f64 3.0) (ptr ,d-x) (ptr ,d-y) (i32 ,n)))
                  (let ([result (gpu-copy-from d-y byte-size)])
                    ;; Check several values with double precision tolerance
                    (assert-true (< (abs (- (bytevector-ieee-double-native-ref result 0) 5.0)) 1e-10))
                    (assert-true (< (abs (- (bytevector-ieee-double-native-ref result 80) 5.0)) 1e-10))
                    (assert-true (< (abs (- (bytevector-ieee-double-native-ref result (* 8 (- n 1))) 5.0)) 1e-10))))))))
        (gpu-unload-module! module))))

  (define-test "i32 (signed integer) parameter type"
    ;; Add a signed offset to integer array
    (let* ([n 128]
           [byte-size (* n 4)]
           [data-host (make-bytevector byte-size)])
      ;; Fill with positive values
      (do ([i 0 (+ i 1)]) ((= i n))
        (bytevector-s32-native-set! data-host (* i 4) i))
      (let ([module (gpu-compile *add-offset-source*)])
        (let ([func (gpu-get-function module "add_offset")])
          (with-gpu-memory byte-size
            (lambda (d-data)
              (gpu-copy-to! d-data data-host)
              ;; Add negative offset: -10
              (gpu-launch! func
                (ceiling (/ n 256)) 256
                `((ptr ,d-data) (i32 -10) (i32 ,n)))
              (let ([result (gpu-copy-from d-data byte-size)])
                ;; data[0] = 0 + (-10) = -10
                (assert-equal -10 (bytevector-s32-native-ref result 0))
                ;; data[5] = 5 + (-10) = -5
                (assert-equal -5 (bytevector-s32-native-ref result 20))
                ;; data[20] = 20 + (-10) = 10
                (assert-equal 10 (bytevector-s32-native-ref result 80))))))
        (gpu-unload-module! module))))

  (define-test "u64 (unsigned 64-bit) parameter type"
    ;; Scale u64 values
    (let* ([n 64]
           [byte-size (* n 8)]
           [data-host (make-bytevector byte-size)])
      ;; Fill with u64 values
      (do ([i 0 (+ i 1)]) ((= i n))
        (bytevector-u64-native-set! data-host (* i 8) (+ i 1)))
      (let ([module (gpu-compile *scale-u64-source*)])
        (let ([func (gpu-get-function module "scale_u64")])
          (with-gpu-memory byte-size
            (lambda (d-data)
              (gpu-copy-to! d-data data-host)
              ;; Scale by 1000000000 (large u64)
              (gpu-launch! func
                (ceiling (/ n 256)) 256
                `((ptr ,d-data) (u64 1000000000) (i32 ,n)))
              (let ([result (gpu-copy-from d-data byte-size)])
                ;; data[0] = 1 * 1000000000 = 1000000000
                (assert-equal 1000000000 (bytevector-u64-native-ref result 0))
                ;; data[9] = 10 * 1000000000 = 10000000000
                (assert-equal 10000000000 (bytevector-u64-native-ref result 72))))))
        (gpu-unload-module! module))))

  (define-test "mixed parameter types in one kernel"
    ;; SAXPY already uses f32, ptr, u32 - test explicit u32 values
    (let* ([n 64]
           [byte-size (* n 4)]
           [x-host (make-bytevector byte-size)]
           [y-host (make-bytevector byte-size)])
      (do ([i 0 (+ i 1)]) ((= i n))
        (bytevector-ieee-single-native-set! x-host (* i 4) 2.0)
        (bytevector-ieee-single-native-set! y-host (* i 4) 3.0))
      (let ([module (gpu-compile
                      "extern \"C\" __global__ void saxpy(float a, float *x, float *y, unsigned int n) {
                         int i = blockIdx.x * blockDim.x + threadIdx.x;
                         if (i < n) { y[i] = a * x[i] + y[i]; }
                       }")])
        (let ([func (gpu-get-function module "saxpy")])
          (with-gpu-memory byte-size
            (lambda (d-x)
              (with-gpu-memory byte-size
                (lambda (d-y)
                  (gpu-copy-to! d-x x-host)
                  (gpu-copy-to! d-y y-host)
                  ;; y = 4.0 * 2.0 + 3.0 = 11.0
                  (gpu-launch! func
                    1 n
                    `((f32 4.0) (ptr ,d-x) (ptr ,d-y) (u32 ,n)))
                  (let ([result (gpu-copy-from d-y byte-size)])
                    (assert-true (< (abs (- (bytevector-ieee-single-native-ref result 0) 11.0)) 0.001))))))))
        (gpu-unload-module! module)))))

;;; ====
;;; Negative Tests (G02)
;;; ====

(test-group "gpu-error-conditions"

  (define-test "invalid CUDA source triggers compilation error"
    ;; Malformed CUDA source should error during compilation
    (assert-error
     (lambda ()
       (gpu-compile "this is not valid CUDA code { syntax error"))))

  (define-test "double-free on gpu-handle is detected"
    ;; Manually free, then try to free again
    (let ([handle (gpu-alloc 1024)])
      (gpu-free! handle)
      (assert-error
       (lambda () (gpu-free! handle)))))

  (define-test "double-unload on gpu-module is detected"
    (let ([module (gpu-compile
                    "extern \"C\" __global__ void noop() {}")])
      (gpu-unload-module! module)
      (assert-error
       (lambda () (gpu-unload-module! module)))))

  (define-test "use-after-free on gpu-handle is detected"
    (let ([handle (gpu-alloc 1024)])
      (gpu-free! handle)
      ;; Try to copy to freed handle
      (assert-error
       (lambda ()
         (gpu-copy-to! handle (make-bytevector 1024 0))))))

  (define-test "use-after-unload on gpu-function is detected"
    (let* ([module (gpu-compile
                     "extern \"C\" __global__ void noop() {}")]
           [func (gpu-get-function module "noop")])
      (gpu-unload-module! module)
      ;; Try to launch function from unloaded module
      (assert-error
       (lambda ()
         (gpu-launch! func 1 1 '())))))

  (define-test "get-function from unloaded module is detected"
    (let ([module (gpu-compile
                    "extern \"C\" __global__ void noop() {}")])
      (gpu-unload-module! module)
      (assert-error
       (lambda ()
         (gpu-get-function module "noop")))))

  (define-test "unknown parameter type is rejected"
    (let* ([module (gpu-compile
                     "extern \"C\" __global__ void noop() {}")]
           [func (gpu-get-function module "noop")])
      (assert-error
       (lambda ()
         (gpu-launch! func 1 1 '((unknown-type 42)))))
      (gpu-unload-module! module)))

  (define-test "ptr param with non-handle is rejected"
    (let* ([module (gpu-compile
                     "extern \"C\" __global__ void noop() {}")]
           [func (gpu-get-function module "noop")])
      (assert-error
       (lambda ()
         (gpu-launch! func 1 1 '((ptr 12345)))))  ;; number, not handle
      (gpu-unload-module! module))))

;;; ====
;;; Parameter Limit Tests (G03)
;;; ====

(test-group "gpu-parameter-limits"

  (define-test "kernel with 10 parameters works"
    (let* ([n 64]
           [byte-size (* n 4)]
           [out-host (make-bytevector byte-size 0)])
      (let ([module (gpu-compile *many-params-source*)])
        (let ([func (gpu-get-function module "many_params")])
          (with-gpu-memory byte-size
            (lambda (d-out)
              ;; p0..p9 = 1.0 each, sum = 10.0
              (gpu-launch! func
                1 n
                `((f32 1.0) (f32 1.0) (f32 1.0) (f32 1.0) (f32 1.0)
                  (f32 1.0) (f32 1.0) (f32 1.0) (f32 1.0) (f32 1.0)
                  (ptr ,d-out) (i32 ,n)))
              (let ([result (gpu-copy-from d-out byte-size)])
                (assert-true (< (abs (- (bytevector-ieee-single-native-ref result 0) 10.0)) 0.001))))))
        (gpu-unload-module! module))))

  (define-test "exactly 64 parameters is accepted"
    ;; Build a kernel that takes exactly 64 parameters total:
    ;; 63 f32 scalars + 1 ptr = 64 params
    (let* ([param-names (let loop ([i 0] [acc '()])
                          (if (= i 63)
                              (reverse acc)
                              (loop (+ i 1) (cons (format "float p~a" i) acc))))]
           [param-decl (apply string-append
                              (let loop ([ps param-names] [first? #t])
                                (if (null? ps)
                                    '()
                                    (cons (if first? (car ps) (string-append ", " (car ps)))
                                          (loop (cdr ps) #f)))))]
           [sum-expr (apply string-append
                            (let loop ([i 0])
                              (if (= i 63)
                                  '()
                                  (cons (if (= i 0)
                                            (format "p~a" i)
                                            (format " + p~a" i))
                                        (loop (+ i 1))))))]
           ;; 63 floats + 1 ptr = 64 total parameters
           [source (format "extern \"C\" __global__ void sum63(~a, float *out) {
                              if (threadIdx.x == 0) { *out = ~a; }
                            }" param-decl sum-expr)])
      (let ([module (gpu-compile source)])
        (let ([func (gpu-get-function module "sum63")])
          (with-gpu-memory 4  ;; single float output
            (lambda (d-out)
              ;; 63 floats of 1.0 each + ptr = 64 total parameters
              (let ([params (append
                             (map (lambda (i) '(f32 1.0)) (iota 63))
                             `((ptr ,d-out)))])
                (gpu-launch! func 1 1 params)
                (let ([result (gpu-copy-from d-out 4)])
                  ;; Sum of 63 ones = 63.0
                  (assert-true (< (abs (- (bytevector-ieee-single-native-ref result 0) 63.0)) 0.01)))))))
        (gpu-unload-module! module))))

  (define-test "65 parameters is rejected by bridge"
    ;; The bridge has a hard limit of 64 parameters
    ;; This test verifies the limit is enforced
    (let* ([param-names (let loop ([i 0] [acc '()])
                          (if (= i 65)
                              (reverse acc)
                              (loop (+ i 1) (cons (format "float p~a" i) acc))))]
           [param-decl (apply string-append
                              (let loop ([ps param-names] [first? #t])
                                (if (null? ps)
                                    '()
                                    (cons (if first? (car ps) (string-append ", " (car ps)))
                                          (loop (cdr ps) #f)))))]
           [source (format "extern \"C\" __global__ void too_many(~a) {}" param-decl)])
      (let ([module (gpu-compile source)])
        (let ([func (gpu-get-function module "too_many")])
          (assert-error
           (lambda ()
             (let ([params (map (lambda (i) '(f32 1.0)) (iota 65))])
               (gpu-launch! func 1 1 params)))))
        (gpu-unload-module! module)))))

;;; ====
;;; Memory Tests (additional coverage)
;;; ====

(test-group "gpu-memory-extended"

  (define-test "memory round-trip (f64)"
    (let* ([n 32]
           [byte-size (* n 8)]
           [data (make-bytevector byte-size 0)])
      (do ([i 0 (+ i 1)]) ((= i n))
        (bytevector-ieee-double-native-set! data (* i 8) (* i 3.14159265358979)))
      (with-gpu-memory byte-size
        (lambda (handle)
          (gpu-copy-to! handle data)
          (let ([result (gpu-copy-from handle byte-size)])
            (do ([i 0 (+ i 1)]) ((= i n))
              (let ([expected (bytevector-ieee-double-native-ref data (* i 8))]
                    [actual (bytevector-ieee-double-native-ref result (* i 8))])
                (assert-true (< (abs (- expected actual)) 1e-14)))))))))

  (define-test "memory round-trip (i32 with negatives)"
    (let* ([n 64]
           [byte-size (* n 4)]
           [data (make-bytevector byte-size 0)])
      (do ([i 0 (+ i 1)]) ((= i n))
        (bytevector-s32-native-set! data (* i 4) (- i 32)))  ;; -32 to +31
      (with-gpu-memory byte-size
        (lambda (handle)
          (gpu-copy-to! handle data)
          (let ([result (gpu-copy-from handle byte-size)])
            (do ([i 0 (+ i 1)]) ((= i n))
              (assert-equal (bytevector-s32-native-ref data (* i 4))
                            (bytevector-s32-native-ref result (* i 4))))))))))

;;; ====
;;; Cleanup and Run
;;; ====

(run-all-tests)

;; Clean up
(when (gpu-available?)
  (gpu-shutdown!))
