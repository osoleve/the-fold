;;; boundary/gpu/test-gpu.ss — End-to-end GPU bridge tests
;;;
;;; Tests: initialization, device info, memory round-trip, SAXPY kernel.

(load "core/testing/test-framework.ss")
(load "boundary/gpu/runtime.ss")

;;; SAXPY kernel source — the standard GPU hello world
(define *saxpy-source*
  "extern \"C\" __global__ void saxpy(float a, float *x, float *y, int n) {
    int i = blockIdx.x * blockDim.x + threadIdx.x;
    if (i < n) {
        y[i] = a * x[i] + y[i];
    }
}")

(test-group "gpu-bridge"

  (define-test "init and device info"
    (unless (gpu-available?)
      (gpu-init!))
    (assert-true (gpu-available?))
    (let ([info (gpu-device-info)])
      (assert-true (pair? (assq 'name info)))
      (assert-true (> (cdr (assq 'memory info)) 0))
      (display (format "  Device: ~a\n" (cdr (assq 'name info))))
      (display (format "  Memory: ~a MB\n"
                       (quotient (cdr (assq 'memory info)) (* 1024 1024))))
      (display (format "  Compute: ~a\n"
                       (cdr (assq 'compute-capability info))))))

  (define-test "memory round-trip (u64)"
    (let* ([data (make-bytevector 32 0)])
      (bytevector-u64-native-set! data 0 42)
      (bytevector-u64-native-set! data 8 123)
      (bytevector-u64-native-set! data 16 456)
      (bytevector-u64-native-set! data 24 789)
      (with-gpu-memory 32
        (lambda (handle)
          (gpu-copy-to! handle data)
          (let ([result (gpu-copy-from handle 32)])
            (assert-equal (bytevector-u64-native-ref data 0)
                          (bytevector-u64-native-ref result 0))
            (assert-equal (bytevector-u64-native-ref data 8)
                          (bytevector-u64-native-ref result 8))
            (assert-equal (bytevector-u64-native-ref data 16)
                          (bytevector-u64-native-ref result 16))
            (assert-equal (bytevector-u64-native-ref data 24)
                          (bytevector-u64-native-ref result 24)))))))

  (define-test "memory round-trip (f32)"
    (let* ([n 16]
           [byte-size (* n 4)]
           [data (make-bytevector byte-size 0)])
      ;; Fill with float values
      (do ([i 0 (+ i 1)]) ((= i n))
        (bytevector-ieee-single-native-set! data (* i 4) (inexact (* i 1.5))))
      (with-gpu-memory byte-size
        (lambda (handle)
          (gpu-copy-to! handle data)
          (let ([result (gpu-copy-from handle byte-size)])
            (do ([i 0 (+ i 1)]) ((= i n))
              (let ([expected (bytevector-ieee-single-native-ref data (* i 4))]
                    [actual (bytevector-ieee-single-native-ref result (* i 4))])
                (assert-true (< (abs (- expected actual)) 0.001)))))))))

  (define-test "compile CUDA source"
    (let ([module (gpu-compile *saxpy-source*)])
      (assert-true (gpu-module? module))
      (let ([func (gpu-get-function module "saxpy")])
        (assert-true (gpu-function? func)))
      (gpu-unload-module! module)))

  (define-test "SAXPY kernel end-to-end"
    (let* ([n 1024]
           [byte-size (* n 4)]
           [x-host (make-bytevector byte-size)]
           [y-host (make-bytevector byte-size)])
      ;; Fill x with 1.0, y with 2.0
      (do ([i 0 (+ i 1)]) ((= i n))
        (bytevector-ieee-single-native-set! x-host (* i 4) 1.0)
        (bytevector-ieee-single-native-set! y-host (* i 4) 2.0))

      ;; Compile SAXPY kernel
      (let ([module (gpu-compile *saxpy-source*)])
        (let ([func (gpu-get-function module "saxpy")])
          ;; Allocate device memory and run kernel
          (with-gpu-memory byte-size
            (lambda (d-x)
              (with-gpu-memory byte-size
                (lambda (d-y)
                  ;; Copy to device
                  (gpu-copy-to! d-x x-host)
                  (gpu-copy-to! d-y y-host)
                  ;; Launch: y = 3.0 * x + y → should be 5.0
                  (let ([grid-dim (ceiling (/ n 256))])
                    (gpu-launch! func
                      grid-dim    ;; grid
                      256         ;; block
                      `((f32 3.0) (ptr ,d-x) (ptr ,d-y) (u32 ,n))))
                  ;; Copy back and verify
                  (let ([result (gpu-copy-from d-y byte-size)])
                    (do ([i 0 (+ i 1)]) ((= i n))
                      (let ([val (bytevector-ieee-single-native-ref result (* i 4))])
                        ;; 3.0 * 1.0 + 2.0 = 5.0
                        (assert-true (< (abs (- val 5.0)) 0.001))))))))))
        (gpu-unload-module! module))))

  (define-test "SAXPY large (64K elements)"
    (let* ([n 65536]
           [byte-size (* n 4)]
           [x-host (make-bytevector byte-size)]
           [y-host (make-bytevector byte-size)])
      ;; Fill x with 2.0, y with 10.0
      (do ([i 0 (+ i 1)]) ((= i n))
        (bytevector-ieee-single-native-set! x-host (* i 4) 2.0)
        (bytevector-ieee-single-native-set! y-host (* i 4) 10.0))

      (let ([module (gpu-compile *saxpy-source*)])
        (let ([func (gpu-get-function module "saxpy")])
          (with-gpu-memory byte-size
            (lambda (d-x)
              (with-gpu-memory byte-size
                (lambda (d-y)
                  (gpu-copy-to! d-x x-host)
                  (gpu-copy-to! d-y y-host)
                  ;; y = 5.0 * 2.0 + 10.0 = 20.0
                  (let ([grid-dim (ceiling (/ n 256))])
                    (gpu-launch! func
                      grid-dim 256
                      `((f32 5.0) (ptr ,d-x) (ptr ,d-y) (u32 ,n))))
                  (let ([result (gpu-copy-from d-y byte-size)])
                    ;; Spot-check several indices
                    (assert-true (< (abs (- (bytevector-ieee-single-native-ref result 0) 20.0)) 0.001))
                    (assert-true (< (abs (- (bytevector-ieee-single-native-ref result 100) 20.0)) 0.001))
                    (assert-true (< (abs (- (bytevector-ieee-single-native-ref result (* 4 (- n 1))) 20.0)) 0.001))))))))
        (gpu-unload-module! module)))))

;; Clean up after all tests
(define (gpu-test-cleanup!)
  (when (gpu-available?)
    (gpu-shutdown!)))

(run-all-tests)
(gpu-test-cleanup!)
