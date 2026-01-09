((name . "shell/ffi/")
 (purpose . "Foreign Function Interface for Rust Acceleration")
 (description . "Provides FFI bindings for Rust-accelerated BVH operations.
                Uses Chez Scheme's load-shared-object and define-ftype.
                Design: out-pointers + #[repr(C)] structs, never scheme-object.
                Fuel tracking preserves totality guarantees.")
 (modules
  ((name . "ffi-core.ss")
   (purpose . "Core FFI infrastructure")
   (exports . (accel-load! accel-available? accel-version test-ffi-roundtrip)))
  ((name . "test-ffi.ss")
   (purpose . "FFI test runner")))
 (rust-crate . "rust-accel/")
 (build . "cd rust-accel && cargo build --release"))
