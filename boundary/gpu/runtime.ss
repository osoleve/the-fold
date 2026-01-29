;;; boundary/gpu/runtime.ss — High-level Scheme API for GPU compute
;;;
;;; Provides gpu-init!, gpu-alloc, gpu-compile, gpu-launch! and friends.
;;; This is the primary interface for GPU operations from Scheme code.
;;; All code here is boundary layer — exempt from fuel model.

(load "boundary/gpu/ffi.ss")

(doc 'module 'gpu-runtime)
(doc 'description "High-level GPU compute API for The Fold")
(doc 'layer 'boundary)
(doc 'purity 'partial)

;;; ====
;;; State
;;; ====

(define *gpu-initialized* #f)
(define *gpu-context* #f)

;;; ====
;;; Initialization
;;; ====

;;; gpu-init! : → #t | error
;;; Initialize CUDA, create context on device 0.
(define (gpu-init!)
  (doc 'type (-> Boolean))
  (doc 'description "Initialize CUDA and create a context on device 0")
  (when *gpu-initialized*
    (error 'gpu-init! "GPU already initialized. Call gpu-shutdown! first."))
  (unless (gpu-load!)
    (error 'gpu-init! "Failed to load GPU bridge library"))
  (gpu-check! 'gpu-init! (gpu-ffi-init))
  ;; Create context on device 0 with default flags
  (let-values ([(r ctx) (call-with-gpu-ptr-out
                         (lambda (out) (gpu-ffi-ctx-create out 0 0)))])
    (gpu-check! 'gpu-init! r)
    (set! *gpu-context* ctx)
    (set! *gpu-initialized* #t)
    #t))

;;; gpu-available? : → Boolean
;;; Check if GPU bridge is loaded and initialized.
(define (gpu-available?)
  (doc 'type (-> Boolean))
  (and (gpu-ffi-available?) *gpu-initialized*))

;;; gpu-shutdown! : → void
;;; Destroy context and clean up.
(define (gpu-shutdown!)
  (doc 'type (-> void))
  (doc 'description "Destroy CUDA context and clean up GPU state")
  (when *gpu-context*
    (gpu-ffi-ctx-destroy *gpu-context*)
    (set! *gpu-context* #f)
    (set! *gpu-initialized* #f)))

;;; ====
;;; Device Info
;;; ====

;;; gpu-device-info : → alist
;;; Returns device information as an association list.
(define (gpu-device-info)
  (doc 'type (-> (List (Pair Symbol Any))))
  (doc 'description "Get GPU device info: name, memory, compute capability")
  (unless (gpu-available?)
    (error 'gpu-device-info "GPU not initialized"))
  ;; Device name
  (let ([name-buf (make-bytevector 256 0)])
    (gpu-check! 'gpu-device-info (gpu-ffi-device-name name-buf 256 0))
    (let ([name (let loop ([i 0])
                  (if (or (= i 256) (= (bytevector-u8-ref name-buf i) 0))
                      (let ([result (make-bytevector i)])
                        (bytevector-copy! name-buf 0 result 0 i)
                        (utf8->string result))
                      (loop (+ i 1))))])
      ;; Total memory
      (let-values ([(r mem) (call-with-gpu-u64-out
                             (lambda (out) (gpu-ffi-device-total-mem out 0)))])
        (gpu-check! 'gpu-device-info r)
        ;; Compute capability
        (call-with-gpu-alloc 8
          (lambda (addr)
            (let ([major-ptr addr]
                  [minor-ptr (+ addr 4)])
              (gpu-check! 'gpu-device-info
                          (gpu-ffi-device-compute-capability major-ptr minor-ptr 0))
              (let ([major (foreign-ref 'integer-32 major-ptr 0)]
                    [minor (foreign-ref 'integer-32 minor-ptr 0)])
                `((name . ,name)
                  (memory . ,mem)
                  (compute-capability . ,(+ major (* 0.1 minor))))))))))))

;;; ====
;;; Memory Management
;;; ====

;;; gpu-handle: tagged record for device memory
;;; (gpu-handle device-ptr size freed?)
(define (make-gpu-handle dptr size)
  (list 'gpu-handle dptr size #f))

(define (gpu-handle? x)
  (and (pair? x) (eq? (car x) 'gpu-handle)))

(define (gpu-handle-dptr h)
  (if (gpu-handle? h) (cadr h)
      (error 'gpu-handle-dptr "not a gpu-handle" h)))

(define (gpu-handle-size h)
  (if (gpu-handle? h) (caddr h)
      (error 'gpu-handle-size "not a gpu-handle" h)))

(define (gpu-handle-freed? h)
  (if (gpu-handle? h) (cadddr h)
      (error 'gpu-handle-freed? "not a gpu-handle" h)))

(define (gpu-handle-mark-freed! h)
  (set-car! (cdddr h) #t))

;;; gpu-alloc : Size → gpu-handle
;;; Allocate device memory. Returns a gpu-handle.
(define (gpu-alloc size)
  (doc 'type (-> Nat gpu-handle))
  (doc 'description "Allocate GPU device memory")
  (unless (gpu-available?)
    (error 'gpu-alloc "GPU not initialized"))
  (let-values ([(r dptr) (call-with-gpu-u64-out
                          (lambda (out) (gpu-ffi-mem-alloc out size)))])
    (gpu-check! 'gpu-alloc r)
    (make-gpu-handle dptr size)))

;;; gpu-free! : gpu-handle → void
;;; Free device memory. Prevents double-free.
(define (gpu-free! handle)
  (doc 'type (-> gpu-handle void))
  (doc 'description "Free GPU device memory")
  (unless (gpu-handle? handle)
    (error 'gpu-free! "not a gpu-handle" handle))
  (when (gpu-handle-freed? handle)
    (error 'gpu-free! "double free detected"))
  (gpu-check! 'gpu-free! (gpu-ffi-mem-free (gpu-handle-dptr handle)))
  (gpu-handle-mark-freed! handle))

;;; gpu-copy-to! : gpu-handle × bytevector → void
;;; Copy host bytevector to device memory.
(define (gpu-copy-to! handle bv)
  (doc 'type (-> gpu-handle Bytevector void))
  (doc 'description "Copy host data to GPU device memory")
  (unless (gpu-handle? handle)
    (error 'gpu-copy-to! "not a gpu-handle" handle))
  (when (gpu-handle-freed? handle)
    (error 'gpu-copy-to! "handle already freed"))
  (let ([size (bytevector-length bv)])
    (when (> size (gpu-handle-size handle))
      (error 'gpu-copy-to!
             (format "bytevector (~a bytes) exceeds allocation (~a bytes)"
                     size (gpu-handle-size handle))))
    (gpu-check! 'gpu-copy-to!
                (gpu-ffi-memcpy-h2d (gpu-handle-dptr handle) bv size))))

;;; gpu-copy-from : gpu-handle × size → bytevector
;;; Copy device memory to a new host bytevector.
(define (gpu-copy-from handle size)
  (doc 'type (-> gpu-handle Nat Bytevector))
  (doc 'description "Copy GPU device memory to host bytevector")
  (unless (gpu-handle? handle)
    (error 'gpu-copy-from "not a gpu-handle" handle))
  (when (gpu-handle-freed? handle)
    (error 'gpu-copy-from "handle already freed"))
  (when (> size (gpu-handle-size handle))
    (error 'gpu-copy-from
           (format "requested ~a bytes but allocation is ~a bytes"
                   size (gpu-handle-size handle))))
  (let ([bv (make-bytevector size 0)])
    (gpu-check! 'gpu-copy-from
                (gpu-ffi-memcpy-d2h bv (gpu-handle-dptr handle) size))
    bv))

;;; with-gpu-memory : Size × (gpu-handle → α) → α
;;; RAII pattern: allocate, run proc, guarantee free via dynamic-wind.
(define (with-gpu-memory size proc)
  (doc 'type (-> Nat (-> gpu-handle α) α))
  (doc 'description "Allocate GPU memory, run proc, guarantee free on exit")
  (let ([handle (gpu-alloc size)])
    (dynamic-wind
      (lambda () #f)
      (lambda () (proc handle))
      (lambda ()
        (unless (gpu-handle-freed? handle)
          (gpu-free! handle))))))

;;; ====
;;; Kernel Compilation
;;; ====

;;; gpu-module record: (gpu-module ptr)
(define (make-gpu-module ptr)
  (list 'gpu-module ptr))

(define (gpu-module? x)
  (and (pair? x) (eq? (car x) 'gpu-module)))

(define (gpu-module-ptr m)
  (if (gpu-module? m) (cadr m)
      (error 'gpu-module-ptr "not a gpu-module" m)))

;;; gpu-function record: (gpu-function ptr)
(define (make-gpu-function ptr)
  (list 'gpu-function ptr))

(define (gpu-function? x)
  (and (pair? x) (eq? (car x) 'gpu-function)))

(define (gpu-function-ptr f)
  (if (gpu-function? f) (cadr f)
      (error 'gpu-function-ptr "not a gpu-function" f)))

;;; gpu-compile : String → gpu-module
;;; Compile CUDA C source string to a loaded module via NVRTC.
(define (gpu-compile source)
  (doc 'type (-> String gpu-module))
  (doc 'description "Compile CUDA C source to GPU module via NVRTC")
  (unless (gpu-available?)
    (error 'gpu-compile "GPU not initialized"))
  ;; NVRTC expects null-terminated source, so append \0
  (let* ([src-bv (string->utf8 (string-append source "\x0;"))]
         [src-len (- (bytevector-length src-bv) 1)]  ;; don't count the null
         ;; Generous buffers for PTX and compile log
         [ptx-cap (max (* src-len 64) (* 1024 1024))]  ;; at least 1MB
         [ptx-buf (make-bytevector ptx-cap 0)]
         [log-cap (* 64 1024)]  ;; 64KB for compile log
         [log-buf (make-bytevector log-cap 0)])
    ;; Allocate length out-pointers (unsigned-64)
    (call-with-gpu-alloc 8
      (lambda (ptx-len-ptr)
        (foreign-set! 'unsigned-64 ptx-len-ptr 0 ptx-cap)
        (call-with-gpu-alloc 8
          (lambda (log-len-ptr)
            (foreign-set! 'unsigned-64 log-len-ptr 0 log-cap)
            (let ([r (gpu-ffi-compile src-bv src-len
                                      ptx-buf ptx-len-ptr
                                      log-buf log-len-ptr)])
              (unless (= r 0)
                ;; Extract compile log for the error message
                (let* ([actual-log-len (foreign-ref 'unsigned-64 log-len-ptr 0)]
                       [log-str (if (> actual-log-len 0)
                                    (let* ([len (min actual-log-len log-cap)]
                                           [result (make-bytevector len)])
                                      (bytevector-copy! log-buf 0 result 0 len)
                                      (utf8->string result))
                                    "")])
                  (error 'gpu-compile
                         (format "NVRTC compilation failed (code ~a):\n~a"
                                 r log-str))))
              ;; Load the PTX as a module
              (let ([ptx-len (foreign-ref 'unsigned-64 ptx-len-ptr 0)])
                ;; Ensure null-termination for cuModuleLoadData
                (when (< ptx-len ptx-cap)
                  (bytevector-u8-set! ptx-buf ptx-len 0))
                (let-values ([(r2 mod-ptr) (call-with-gpu-ptr-out
                                            (lambda (out)
                                              (gpu-ffi-module-load out ptx-buf)))])
                  (gpu-check! 'gpu-compile r2)
                  (make-gpu-module mod-ptr))))))))))

;;; gpu-unload-module! : gpu-module → void
;;; Unload a compiled GPU module.
(define (gpu-unload-module! module)
  (doc 'type (-> gpu-module void))
  (unless (gpu-module? module)
    (error 'gpu-unload-module! "not a gpu-module" module))
  (gpu-check! 'gpu-unload-module!
              (gpu-ffi-module-unload (gpu-module-ptr module))))

;;; gpu-get-function : gpu-module × String → gpu-function
;;; Get a kernel function handle from a compiled module.
(define (gpu-get-function module name)
  (doc 'type (-> gpu-module String gpu-function))
  (unless (gpu-module? module)
    (error 'gpu-get-function "not a gpu-module" module))
  (let ([name-bv (string->utf8 (string-append name "\x0;"))])
    (let-values ([(r fn-ptr) (call-with-gpu-ptr-out
                              (lambda (out)
                                (gpu-ffi-get-function out (gpu-module-ptr module)
                                                      name-bv)))])
      (gpu-check! 'gpu-get-function r)
      (make-gpu-function fn-ptr))))

;;; ====
;;; Parameter Packing
;;; ====

;;; pack-gpu-params : List → bytevector
;;; Pack kernel parameters into contiguous 8-byte slots.
;;; Each param is (type value):
;;;   f32  → IEEE single in low 4 bytes, zero-padded to 8
;;;   f64  → IEEE double (full 8 bytes)
;;;   u32  → unsigned-32 in low 4 bytes, zero-padded to 8
;;;   u64  → unsigned-64 (full 8 bytes)
;;;   i32  → signed-32 in low 4 bytes, zero-padded to 8
;;;   ptr  → gpu-handle device pointer as u64
(define (pack-gpu-params params)
  (let* ([n (length params)]
         [bv (make-bytevector (* n 8) 0)])
    (let loop ([ps params] [offset 0])
      (if (null? ps)
          bv
          (let* ([param (car ps)]
                 [type (car param)]
                 [val (cadr param)])
            (case type
              [(f32)
               (bytevector-ieee-single-native-set! bv offset (inexact val))]
              [(f64)
               (bytevector-ieee-double-native-set! bv offset (inexact val))]
              [(u32)
               (bytevector-u32-native-set! bv offset val)]
              [(u64)
               (bytevector-u64-native-set! bv offset val)]
              [(i32)
               (bytevector-s32-native-set! bv offset val)]
              [(ptr)
               (unless (gpu-handle? val)
                 (error 'pack-gpu-params "ptr param must be a gpu-handle" val))
               (bytevector-u64-native-set! bv offset (gpu-handle-dptr val))]
              [else
               (error 'pack-gpu-params (format "unknown param type: ~a" type))])
            (loop (cdr ps) (+ offset 8)))))))

;;; ====
;;; Kernel Launch
;;; ====

;;; normalize-grid-spec : (or Int (List Int Int Int)) → (values Int Int Int)
;;; Convert integer or 3-element list to grid/block dimensions.
(define (normalize-grid-spec spec)
  (cond
   [(integer? spec) (values spec 1 1)]
   [(and (list? spec) (= (length spec) 3))
    (values (car spec) (cadr spec) (caddr spec))]
   [else (error 'normalize-grid-spec
                "expected integer or (x y z) list" spec)]))

;;; gpu-launch! : gpu-function × grid × block × params → void
;;; Launch a kernel.
;;;   grid  = integer or (gx gy gz) list
;;;   block = integer or (bx by bz) list
;;;   params = list of (type value) pairs
(define (gpu-launch! func grid block params)
  (doc 'type (-> gpu-function (| Nat (List Nat Nat Nat))
                               (| Nat (List Nat Nat Nat))
                               (List (List Symbol Any))
                               void))
  (doc 'description "Launch a GPU kernel with the given grid, block, and parameters")
  (unless (gpu-function? func)
    (error 'gpu-launch! "not a gpu-function" func))
  (unless (gpu-available?)
    (error 'gpu-launch! "GPU not initialized"))
  (let-values ([(gx gy gz) (normalize-grid-spec grid)]
               [(bx by bz) (normalize-grid-spec block)])
    (let* ([params-bv (pack-gpu-params params)]
           [num-params (length params)])
      (gpu-check! 'gpu-launch!
                  (gpu-ffi-launch (gpu-function-ptr func)
                                  gx gy gz bx by bz
                                  0  ;; shared memory = 0 for Phase 0
                                  params-bv num-params)))))

;;; ====
;;; Load Message
;;; ====

(unless (top-level-bound? '*gpu-runtime-loaded*)
  (display "gpu/runtime.ss loaded.\n")
  (display "  (gpu-init!)              - Initialize GPU\n")
  (display "  (gpu-available?)         - Check GPU status\n")
  (display "  (gpu-device-info)        - Get device info\n")
  (display "  (gpu-compile src)        - Compile CUDA C source\n")
  (display "  (gpu-launch! fn g b ps)  - Launch kernel\n")
  (display "  (gpu-shutdown!)          - Clean up\n"))

(define *gpu-runtime-loaded* #t)
