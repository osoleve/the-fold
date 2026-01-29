/*
 * boundary/gpu/bridge.c — CUDA Driver API + NVRTC bridge for The Fold
 *
 * C shared library wrapping CUDA operations for Chez Scheme FFI.
 * Design: thin C glue that handles versioned symbols (#define cuMemAlloc → cuMemAlloc_v2)
 * and void** parameter packing that Chez FFI cannot express directly.
 *
 * All functions return CUresult (int) with out-pointers for values.
 * Boundary code — exempt from fuel model.
 */

#include <cuda.h>
#include <nvrtc.h>
#include <string.h>
#include <stdint.h>

/* ====================================================================
 * Initialization & Device Info
 * ==================================================================== */

static int gpu_initialized = 0;

int fold_gpu_init(void) {
    if (gpu_initialized) return CUDA_SUCCESS;
    CUresult r = cuInit(0);
    if (r == CUDA_SUCCESS) gpu_initialized = 1;
    return (int)r;
}

int fold_gpu_device_count(int *out) {
    if (!out) return CUDA_ERROR_INVALID_VALUE;
    return (int)cuDeviceGetCount(out);
}

int fold_gpu_device_get(int *out, int ordinal) {
    if (!out) return CUDA_ERROR_INVALID_VALUE;
    CUdevice dev;
    CUresult r = cuDeviceGet(&dev, ordinal);
    if (r == CUDA_SUCCESS) *out = (int)dev;
    return (int)r;
}

int fold_gpu_device_name(char *name_buf, int len, int device) {
    if (!name_buf) return CUDA_ERROR_INVALID_VALUE;
    return (int)cuDeviceGetName(name_buf, len, (CUdevice)device);
}

int fold_gpu_device_total_mem(unsigned long long *out, int device) {
    if (!out) return CUDA_ERROR_INVALID_VALUE;
    size_t bytes = 0;
    CUresult r = cuDeviceTotalMem(&bytes, (CUdevice)device);
    if (r == CUDA_SUCCESS) *out = (unsigned long long)bytes;
    return (int)r;
}

/* Compute capability */
int fold_gpu_device_compute_capability(int *major, int *minor, int device) {
    if (!major || !minor) return CUDA_ERROR_INVALID_VALUE;
    CUresult r = cuDeviceGetAttribute(major, CU_DEVICE_ATTRIBUTE_COMPUTE_CAPABILITY_MAJOR,
                                      (CUdevice)device);
    if (r != CUDA_SUCCESS) return (int)r;
    return (int)cuDeviceGetAttribute(minor, CU_DEVICE_ATTRIBUTE_COMPUTE_CAPABILITY_MINOR,
                                     (CUdevice)device);
}

/* ====================================================================
 * Context Management
 * ==================================================================== */

int fold_gpu_ctx_create(void **out, unsigned int flags, int device) {
    if (!out) return CUDA_ERROR_INVALID_VALUE;
    /* CUDA 13+: cuCtxCreate_v4 takes CUctxCreateParams* (NULL for defaults) */
    return (int)cuCtxCreate((CUcontext *)out, NULL, flags, (CUdevice)device);
}

int fold_gpu_ctx_destroy(void *ctx) {
    return (int)cuCtxDestroy((CUcontext)ctx);
}

int fold_gpu_ctx_sync(void) {
    return (int)cuCtxSynchronize();
}

/* ====================================================================
 * Memory Management
 * ==================================================================== */

int fold_gpu_mem_alloc(unsigned long long *out, unsigned long long size) {
    if (!out) return CUDA_ERROR_INVALID_VALUE;
    CUdeviceptr dptr = 0;
    CUresult r = cuMemAlloc(&dptr, (size_t)size);
    if (r == CUDA_SUCCESS) *out = (unsigned long long)dptr;
    return (int)r;
}

int fold_gpu_mem_free(unsigned long long dptr) {
    return (int)cuMemFree((CUdeviceptr)dptr);
}

int fold_gpu_memcpy_h2d(unsigned long long dst, const void *src, unsigned long long size) {
    if (!src) return CUDA_ERROR_INVALID_VALUE;
    return (int)cuMemcpyHtoD((CUdeviceptr)dst, src, (size_t)size);
}

int fold_gpu_memcpy_d2h(void *dst, unsigned long long src, unsigned long long size) {
    if (!dst) return CUDA_ERROR_INVALID_VALUE;
    return (int)cuMemcpyDtoH(dst, (CUdeviceptr)src, (size_t)size);
}

/* ====================================================================
 * NVRTC Compilation
 * ==================================================================== */

int fold_gpu_compile(const char *src, unsigned long long src_len,
                     char *out_ptx, unsigned long long *out_ptx_len,
                     char *out_log, unsigned long long *out_log_len) {
    if (!src || !out_ptx || !out_ptx_len || !out_log || !out_log_len)
        return -1;

    nvrtcProgram prog;
    nvrtcResult rc;

    rc = nvrtcCreateProgram(&prog, src, "fold_kernel.cu", 0, NULL, NULL);
    if (rc != NVRTC_SUCCESS) return (int)rc;

    /* Compile targeting compute_120 (GB10 is compute capability 12.1) */
    const char *opts[] = { "--gpu-architecture=compute_120" };
    rc = nvrtcCompileProgram(prog, 1, opts);

    /* Always retrieve the compile log */
    size_t log_size = 0;
    nvrtcGetProgramLogSize(prog, &log_size);
    if (log_size > 0 && log_size <= *out_log_len) {
        nvrtcGetProgramLog(prog, out_log);
        *out_log_len = (unsigned long long)log_size;
    } else if (log_size > *out_log_len) {
        /* Log too large for buffer — truncate */
        *out_log_len = 0;
    } else {
        *out_log_len = 0;
    }

    if (rc != NVRTC_SUCCESS) {
        nvrtcDestroyProgram(&prog);
        return (int)rc;
    }

    /* Get PTX */
    size_t ptx_size = 0;
    rc = nvrtcGetPTXSize(prog, &ptx_size);
    if (rc != NVRTC_SUCCESS) {
        nvrtcDestroyProgram(&prog);
        return (int)rc;
    }

    if (ptx_size > *out_ptx_len) {
        /* PTX too large for buffer */
        *out_ptx_len = (unsigned long long)ptx_size;
        nvrtcDestroyProgram(&prog);
        return -2; /* buffer overflow sentinel */
    }

    rc = nvrtcGetPTX(prog, out_ptx);
    if (rc == NVRTC_SUCCESS) {
        *out_ptx_len = (unsigned long long)ptx_size;
    }

    nvrtcDestroyProgram(&prog);
    return (int)rc;
}

/* ====================================================================
 * Module & Kernel Management
 * ==================================================================== */

int fold_gpu_module_load(void **out, const void *ptx) {
    if (!out || !ptx) return CUDA_ERROR_INVALID_VALUE;
    return (int)cuModuleLoadData((CUmodule *)out, ptx);
}

int fold_gpu_module_unload(void *module) {
    return (int)cuModuleUnload((CUmodule)module);
}

int fold_gpu_get_function(void **out, void *module, const char *name) {
    if (!out || !name) return CUDA_ERROR_INVALID_VALUE;
    return (int)cuModuleGetFunction((CUfunction *)out, (CUmodule)module, name);
}

/* ====================================================================
 * Kernel Launch
 * ==================================================================== */

/*
 * Launch a kernel with parameters packed as contiguous 8-byte slots.
 *
 * params_buf: contiguous buffer of num_params × 8-byte slots
 * The bridge builds void*[] pointing into params_buf at 8-byte offsets.
 * Synchronizes after launch (Phase 0 is synchronous).
 */
int fold_gpu_launch(void *func,
                    unsigned int grid_x, unsigned int grid_y, unsigned int grid_z,
                    unsigned int block_x, unsigned int block_y, unsigned int block_z,
                    unsigned int shared_mem,
                    const void *params_buf, int num_params) {
    if (!func) return CUDA_ERROR_INVALID_VALUE;

    /* Build void*[] array pointing into the contiguous parameter buffer */
    void *params[64]; /* max 64 kernel parameters — generous for Phase 0 */
    if (num_params > 64) return CUDA_ERROR_INVALID_VALUE;

    const uint8_t *buf = (const uint8_t *)params_buf;
    for (int i = 0; i < num_params; i++) {
        params[i] = (void *)(buf + i * 8);
    }

    CUresult r = cuLaunchKernel(
        (CUfunction)func,
        grid_x, grid_y, grid_z,
        block_x, block_y, block_z,
        shared_mem,
        NULL,       /* stream = default */
        params,
        NULL        /* extra = NULL */
    );

    if (r != CUDA_SUCCESS) return (int)r;

    /* Synchronous execution in Phase 0 */
    return (int)cuCtxSynchronize();
}

/* ====================================================================
 * Utility
 * ==================================================================== */

const char *fold_gpu_error_string(int error) {
    const char *str = NULL;
    CUresult r = cuGetErrorString((CUresult)error, &str);
    if (r != CUDA_SUCCESS || str == NULL) return "unknown error";
    return str;
}
