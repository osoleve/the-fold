;;; boundary/gpu/ffi.ss — Chez Scheme FFI bindings for CUDA GPU bridge
;;;
;;; Low-level bindings to libfold_gpu.so. Follows established patterns
;;; from boundary/ffi/ffi-core.ss and posix-ffi.ss.
;;;
;;; All CUDA wrapper functions return int (CUresult).
;;; Values are returned via out-pointers allocated with foreign-alloc
;;; and freed via dynamic-wind for exception safety.

(load "core/base/prelude.ss")

(doc 'module 'gpu-ffi)
(doc 'description "CUDA GPU FFI Bindings — Wraps libfold_gpu.so for GPU compute")
(doc 'layer 'boundary)
(doc 'purity 'partial)

;;; ====
;;; Library Loading
;;; ====

(define *gpu-lib-loaded* #f)
(define *gpu-lib-path* #f)

(define *gpu-lib-paths*
  (list
   "boundary/gpu/libfold_gpu.so"
   "/usr/local/lib/libfold_gpu.so"))

(define (find-gpu-library)
  (doc 'type (-> (or String #f)))
  (doc 'description "Find the first existing GPU library path")
  (let loop ([paths *gpu-lib-paths*])
    (if (null? paths)
        #f
        (let ([path (car paths)])
          (if (file-exists? path)
              path
              (loop (cdr paths)))))))

;;; ====
;;; Foreign Procedure Bindings
;;; ====

;; Init & device info
(define gpu-ffi-init #f)
(define gpu-ffi-device-count #f)
(define gpu-ffi-device-get #f)
(define gpu-ffi-device-name #f)
(define gpu-ffi-device-total-mem #f)
(define gpu-ffi-device-compute-capability #f)

;; Context
(define gpu-ffi-ctx-create #f)
(define gpu-ffi-ctx-destroy #f)
(define gpu-ffi-ctx-sync #f)

;; Memory
(define gpu-ffi-mem-alloc #f)
(define gpu-ffi-mem-free #f)
(define gpu-ffi-memcpy-h2d #f)
(define gpu-ffi-memcpy-d2h #f)

;; Compilation
(define gpu-ffi-compile #f)

;; Module & kernel
(define gpu-ffi-module-load #f)
(define gpu-ffi-module-unload #f)
(define gpu-ffi-get-function #f)

;; Launch
(define gpu-ffi-launch #f)

;; Utility
(define gpu-ffi-error-string #f)

(define (bind-gpu-procedures!)
  (doc 'type (-> void))
  (doc 'description "Bind all GPU foreign procedures (call after library load)")

  ;; Initialization & device info
  (set! gpu-ffi-init
        (foreign-procedure "fold_gpu_init" () int))

  (set! gpu-ffi-device-count
        (foreign-procedure "fold_gpu_device_count" (void*) int))

  (set! gpu-ffi-device-get
        (foreign-procedure "fold_gpu_device_get" (void* int) int))

  (set! gpu-ffi-device-name
        (foreign-procedure "fold_gpu_device_name" (u8* int int) int))

  (set! gpu-ffi-device-total-mem
        (foreign-procedure "fold_gpu_device_total_mem" (void* int) int))

  (set! gpu-ffi-device-compute-capability
        (foreign-procedure "fold_gpu_device_compute_capability" (void* void* int) int))

  ;; Context
  (set! gpu-ffi-ctx-create
        (foreign-procedure "fold_gpu_ctx_create" (void* unsigned-32 int) int))

  (set! gpu-ffi-ctx-destroy
        (foreign-procedure "fold_gpu_ctx_destroy" (void*) int))

  (set! gpu-ffi-ctx-sync
        (foreign-procedure "fold_gpu_ctx_sync" () int))

  ;; Memory
  (set! gpu-ffi-mem-alloc
        (foreign-procedure "fold_gpu_mem_alloc" (void* unsigned-64) int))

  (set! gpu-ffi-mem-free
        (foreign-procedure "fold_gpu_mem_free" (unsigned-64) int))

  (set! gpu-ffi-memcpy-h2d
        (foreign-procedure "fold_gpu_memcpy_h2d" (unsigned-64 u8* unsigned-64) int))

  (set! gpu-ffi-memcpy-d2h
        (foreign-procedure "fold_gpu_memcpy_d2h" (u8* unsigned-64 unsigned-64) int))

  ;; Compilation (NVRTC)
  ;; Args: src, src_len, out_ptx, out_ptx_len_ptr, out_log, out_log_len_ptr
  (set! gpu-ffi-compile
        (foreign-procedure "fold_gpu_compile"
                           (u8* unsigned-64 u8* void* u8* void*) int))

  ;; Module & kernel
  (set! gpu-ffi-module-load
        (foreign-procedure "fold_gpu_module_load" (void* u8*) int))

  (set! gpu-ffi-module-unload
        (foreign-procedure "fold_gpu_module_unload" (void*) int))

  (set! gpu-ffi-get-function
        (foreign-procedure "fold_gpu_get_function" (void* void* u8*) int))

  ;; Launch
  ;; Args: func, grid_x/y/z, block_x/y/z, shared_mem, params_buf, num_params
  (set! gpu-ffi-launch
        (foreign-procedure "fold_gpu_launch"
                           (void* unsigned-32 unsigned-32 unsigned-32
                            unsigned-32 unsigned-32 unsigned-32
                            unsigned-32 u8* int)
                           int))

  ;; Utility
  (set! gpu-ffi-error-string
        (foreign-procedure "fold_gpu_error_string" (int) string)))

;;; ====
;;; Library Load Entry Point
;;; ====

(define (gpu-load!)
  (doc 'type (-> Boolean))
  (doc 'description "Load the GPU bridge library and bind procedures")
  (if *gpu-lib-loaded*
      #t
      (let ([path (find-gpu-library)])
        (if path
            (guard (ex [else
                        (display "GPU FFI: Failed to load library: ")
                        (display path)
                        (newline)
                        #f])
              (load-shared-object path)
              (set! *gpu-lib-loaded* #t)
              (set! *gpu-lib-path* path)
              (bind-gpu-procedures!)
              #t)
            (begin
              (display "GPU FFI: No GPU library found\n")
              (display "  Build with: cd boundary/gpu && make\n")
              #f)))))

(define (gpu-ffi-available?)
  (doc 'type (-> Boolean))
  (doc 'description "Check if GPU FFI is loaded")
  *gpu-lib-loaded*)

;;; ====
;;; Memory-Safe FFI Helpers
;;; ====

;;; call-with-gpu-alloc : Size × (Addr → α) → α
;;; Allocate foreign memory, run thunk, guarantee cleanup via dynamic-wind.
(define (call-with-gpu-alloc size thunk)
  (let ([addr (foreign-alloc size)])
    (dynamic-wind
      (lambda () #f)
      (lambda () (thunk addr))
      (lambda () (foreign-free addr)))))

;;; call-with-gpu-int-out : (Addr → int) → (values int int)
;;; Pattern for CUDA calls that write an int via out-pointer.
;;; Returns (values cuda-result out-value).
(define (call-with-gpu-int-out proc)
  (call-with-gpu-alloc 4
    (lambda (addr)
      (let ([r (proc addr)])
        (values r (foreign-ref 'integer-32 addr 0))))))

;;; call-with-gpu-u64-out : (Addr → int) → (values int unsigned-64)
;;; Pattern for CUDA calls that write a u64 via out-pointer.
;;; Returns (values cuda-result out-value).
(define (call-with-gpu-u64-out proc)
  (call-with-gpu-alloc 8
    (lambda (addr)
      (let ([r (proc addr)])
        (values r (foreign-ref 'unsigned-64 addr 0))))))

;;; call-with-gpu-ptr-out : (Addr → int) → (values int void*)
;;; Pattern for CUDA calls that write a pointer via out-pointer.
;;; Returns (values cuda-result out-value).
(define (call-with-gpu-ptr-out proc)
  (call-with-gpu-alloc 8
    (lambda (addr)
      (let ([r (proc addr)])
        (values r (foreign-ref 'void* addr 0))))))

;;; ====
;;; Error Checking
;;; ====

;;; gpu-check! : Symbol × Int → void
;;; Raise a Scheme error if the CUDA result code is non-zero.
(define (gpu-check! who result)
  (unless (= result 0)
    (let ([msg (if gpu-ffi-error-string
                   (gpu-ffi-error-string result)
                   (format "CUDA error ~a" result))])
      (error who (format "CUDA error (~a): ~a" result msg)))))

;;; ====
;;; Load Message
;;; ====

(unless (top-level-bound? '*gpu-ffi-loaded*)
  (display "gpu/ffi.ss loaded.\n")
  (display "  (gpu-load!)          - Load GPU bridge library\n")
  (display "  (gpu-ffi-available?) - Check if GPU FFI is loaded\n"))

(define *gpu-ffi-loaded* #t)
