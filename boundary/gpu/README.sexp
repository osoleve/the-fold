((name . "boundary/gpu/")
 (purpose . "CUDA GPU acceleration via Driver API and NVRTC")
 (layer . boundary)
 (purity . partial)

 (description . "Phase 0 of GPU acceleration for The Fold. Provides a C bridge
(libfold_gpu.so) wrapping CUDA Driver API and NVRTC for runtime kernel compilation,
with Chez Scheme FFI bindings and a high-level Scheme runtime API.")

 (modules
  ((name . "bridge.c")
   (purpose . "C shared library wrapping CUDA Driver API + NVRTC")
   (exports fold_gpu_init fold_gpu_device_count fold_gpu_device_get
            fold_gpu_device_name fold_gpu_device_total_mem
            fold_gpu_ctx_create fold_gpu_ctx_destroy fold_gpu_ctx_sync
            fold_gpu_mem_alloc fold_gpu_mem_free
            fold_gpu_memcpy_h2d fold_gpu_memcpy_d2h
            fold_gpu_compile fold_gpu_module_load fold_gpu_module_unload
            fold_gpu_get_function fold_gpu_launch fold_gpu_error_string)))

  ((name . "ffi.ss")
   (purpose . "Chez Scheme FFI bindings for libfold_gpu.so")
   (exports gpu-load! gpu-ffi-available?
            gpu-ffi-init gpu-ffi-device-count gpu-ffi-ctx-create
            gpu-ffi-mem-alloc gpu-ffi-memcpy-h2d gpu-ffi-memcpy-d2h
            gpu-ffi-compile gpu-ffi-module-load gpu-ffi-get-function
            gpu-ffi-launch gpu-ffi-error-string
            call-with-gpu-alloc call-with-gpu-int-out call-with-gpu-u64-out
            call-with-gpu-ptr-out gpu-check!))

  ((name . "runtime.ss")
   (purpose . "High-level Scheme API for GPU compute")
   (exports gpu-init! gpu-available? gpu-shutdown! gpu-device-info
            gpu-alloc gpu-free! gpu-copy-to! gpu-copy-from with-gpu-memory
            gpu-handle? gpu-handle-dptr gpu-handle-size
            gpu-compile gpu-unload-module! gpu-get-function
            gpu-module? gpu-function?
            gpu-launch! pack-gpu-params))

  ((name . "test-gpu.ss")
   (purpose . "End-to-end test suite including SAXPY kernel"))

 (build-instructions
  "cd boundary/gpu && make  # Produces libfold_gpu.so")

 (test-instructions
  "scheme --script boundary/gpu/test-gpu.ss")

 (dependencies
  (system "CUDA Toolkit 13+ (cuda.h, nvrtc.h, libcuda.so, libnvrtc.so)")
  (internal "core/base/prelude.ss" "core/testing/test-framework.ss"))

 (future-phases
  ((phase 1 . "Compute primitives (elementwise ops, reductions)")
   (phase 2 . "IR builder + CUDA C emitter → generate kernels from traces")
   (phase 3 . "Autodiff integration → GPU-accelerated gradients")
   (phase 4 . "Optic → access pattern compiler → optic-driven kernels")
   (phase 5 . "Kernel fusion, async streams, multi-GPU"))))
