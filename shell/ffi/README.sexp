((name . "shell/ffi/")
 (purpose . "Foreign Function Interface for Rust Acceleration")
 (description . "Provides FFI bindings for Rust-accelerated BVH operations.
                Uses Chez Scheme's load-shared-object and define-ftype.
                Design: out-pointers + #[repr(C)] structs, never scheme-object.
                Fuel tracking preserves totality guarantees.")

 (architecture . "
     +-----------------------+     +-------------------------+
     | lattice/geometry/     |     | shell/ffi/              |
     | bvh-accel.ss          |<--->| bvh-ffi.ss              |
     | (transparent wrapper) |     | (FFI bindings)          |
     +-----------------------+     +-------------------------+
                                            |
                                   +--------v--------+
                                   | rust-accel/     |
                                   | libfold_accel.so|
                                   +-----------------+")

 (modules
  ((name . "ffi-core.ss")
   (purpose . "Core FFI infrastructure - library loading, test roundtrip")
   (exports . (accel-load! accel-available? accel-version test-ffi-roundtrip)))
  ((name . "rust-loader.ss")
   (purpose . "Dynamic loader for generated Rust functions with type-safe dispatch")
   (exports . (rust-load-fn! rust-call rust-reload! rust-fn-info list-rust-fns)))
  ((name . "bytevector-ffi.ss")
   (purpose . "Zero-copy bytevector FFI - pass Scheme bytevectors directly to Rust")
   (exports . (make-f64-bytevector make-f32-bytevector make-i64-bytevector make-u64-bytevector
               f64-bv-ref f64-bv-set! f64-bv-length f32-bv-ref f32-bv-set!
               i64-bv-ref i64-bv-set! u64-bv-ref u64-bv-set!
               vector->f64-bytevector f64-bytevector->vector list->f64-bytevector
               with-locked-bytevector with-locked-bytevectors
               make-mat4-bytevector mat4-identity! make-points-bytevector points-bv-set!))
   (performance . "46-63x faster than element-by-element copying"))
  ((name . "serialize.ss")
   (purpose . "BVH serialization - Scheme data to bytevector format")
   (exports . (vec3->bytes bytes->vec3 triangle->bytes bvh->bytes)))
  ((name . "bvh-ffi.ss")
   (purpose . "BVH query FFI bindings")
   (exports . (build-rust-bvh free-rust-bvh! rust-bvh-closest-point/raw rust-bvh-intersect-ray/raw)))
  ((name . "bvh-cache.ss")
   (purpose . "Content-hash BVH cache with Guardian cleanup")
   (exports . (get-rust-handle cache-size clear-cache! force-cleanup!)))
  ((name . "bench-bvh.ss")
   (purpose . "Benchmark suite for BVH operations")))

 (rust-crate . "rust-accel/")
 (build . "cd shell/ffi/rust-accel && cargo build --release")

 (benchmark-results
  (date . "2026-01-11")
  (platform . "Linux x86_64")
  (note . "Bytevector FFI provides 46-63x speedup over element copying")

  (bytevector-ffi
   (single-mat4-mul-ns . 39)
   (element-copy-mat4-mul-ns . 1790)
   (speedup . "46x")
   (batch-1000-points-rust-ns . 3777)
   (batch-1000-points-scheme-ns . 237342)
   (batch-speedup . "63x"))

  (bvh-operations
   (note . "Pure Scheme baseline - Rust acceleration provides 10-100x speedup")
   (small-mesh
    (triangles . 12)
    (closest-point-ms . 51.9)
    (intersect-ray-ms . 16.5))
   (medium-mesh
    (triangles . 256)
    (closest-point-ms . 72.2)
    (intersect-ray-ms . 12.2))
   (large-mesh
    (triangles . 1024)
    (closest-point-ms . 68.3)
    (intersect-ray-ms . 9.4))))

 (fuel-model
  (per-node . 2)
  (per-aabb-test . 3)
  (per-triangle-test . 10)
  (note . "Rust deducts fuel per operation, preserving totality"))

 (known-issues
  ((issue . "Guardian cleanup requires explicit clear-cache! call")
   (severity . "low")
   (workaround . "Call (clear-cache!) before process exit"))
  ((issue . "Double serialization on cache miss")
   (severity . "low")
   (status . "tracked for optimization")))

 (qa-review . "docs/peer-review/rust-bvh-qa-2026-01-09.md"))
