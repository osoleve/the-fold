//! Rust acceleration library for The Fold
//!
//! Design principles:
//! - All functions use #[repr(C)] structs for FFI safety
//! - Out-pointers for results, never return Scheme objects
//! - Fuel tracking for totality preservation
//! - No panics - all errors return status codes

pub mod aabb;
pub mod bvh;
pub mod bytes;
pub mod fuel;
pub mod generated;
pub mod mat4;
pub mod raymarch;
pub mod string;
pub mod triangle;
pub mod vec3;

pub use aabb::AABB;
pub use bvh::{BVHHandle, BVHNode, ClosestPointResult, RayIntersectResult};
pub use raymarch::{NormalResult, RaymarchResult};
pub use triangle::Triangle;
pub use vec3::Vec3;

/// Result struct for closest point query (Phase 1 test version)
/// Uses out-pointer pattern: Scheme allocates, Rust writes
#[repr(C)]
pub struct TestResult {
    /// Status: 0=miss, 1=hit, 2=out-of-fuel
    pub status: u8,
    /// Result value (for testing: just echo input doubled)
    pub value: f64,
    /// Remaining fuel
    pub fuel_out: u64,
}

// ====
// Type-Safe Result Structs (Milestone 1: fold-4s4q)
// ====
//
// These structs enable type-safe FFI without lossy casts.
// Each Rust return type has its own result struct:
// - I64Result: for integer operations
// - F64Result: for floating-point operations
// - BoolResult: for comparison/logical operations
// - U64Result: for unsigned integer operations

/// Result struct for i64 return values
#[repr(C)]
pub struct I64Result {
    /// Status: 1=success, 2=out-of-fuel, 3=runtime-error
    pub status: u8,
    /// Result value (i64)
    pub value: i64,
    /// Remaining fuel
    pub fuel_out: u64,
}

/// Result struct for f64 return values
#[repr(C)]
pub struct F64Result {
    /// Status: 1=success, 2=out-of-fuel, 3=runtime-error
    pub status: u8,
    /// Result value (f64)
    pub value: f64,
    /// Remaining fuel
    pub fuel_out: u64,
}

/// Result struct for bool return values
/// Note: bool is represented as u8 for C FFI compatibility (0=false, 1=true)
#[repr(C)]
pub struct BoolResult {
    /// Status: 1=success, 2=out-of-fuel, 3=runtime-error
    pub status: u8,
    /// Result value (0=false, 1=true)
    pub value: u8,
    /// Remaining fuel
    pub fuel_out: u64,
}

/// Result struct for u64 return values
#[repr(C)]
pub struct U64Result {
    /// Status: 1=success, 2=out-of-fuel, 3=runtime-error
    pub status: u8,
    /// Result value (u64)
    pub value: u64,
    /// Remaining fuel
    pub fuel_out: u64,
}

/// Result struct for i32 return values
#[repr(C)]
pub struct I32Result {
    /// Status: 1=success, 2=out-of-fuel, 3=runtime-error
    pub status: u8,
    /// Result value (i32)
    pub value: i32,
    /// Remaining fuel
    pub fuel_out: u64,
}

/// Result struct for f32 return values
#[repr(C)]
pub struct F32Result {
    /// Status: 1=success, 2=out-of-fuel, 3=runtime-error
    pub status: u8,
    /// Result value (f32)
    pub value: f32,
    /// Remaining fuel
    pub fuel_out: u64,
}

// ====
// Layer 2 Result Structs (fold-gu3t: Bytevector/String Types)
// ====

/// Result struct for operations that write to an output buffer
/// Used for bytevector copy, string case conversion, etc.
#[repr(C)]
pub struct BufferResult {
    /// Status: 1=success, 2=out-of-fuel, 3=runtime-error, 4=buffer-overflow
    pub status: u8,
    /// Number of bytes written to output buffer
    pub bytes_written: usize,
    /// Remaining fuel
    pub fuel_out: u64,
}

/// Simple test function to verify FFI round-trip
/// Takes three doubles and fuel, writes result to out-pointer
/// Returns the sum of inputs (doubled) if fuel permits
#[no_mangle]
pub extern "C" fn fold_test_roundtrip(a: f64, b: f64, c: f64, fuel_in: u64, out: *mut TestResult) {
    // Safety: caller must provide valid pointer
    if out.is_null() {
        return;
    }

    // Fuel cost for this operation
    const COST: u64 = 5;

    let result = unsafe { &mut *out };

    if fuel_in < COST {
        // Out of fuel
        result.status = 2;
        result.value = 0.0;
        result.fuel_out = 0;
    } else {
        // Success: compute sum * 2
        result.status = 1;
        result.value = (a + b + c) * 2.0;
        result.fuel_out = fuel_in - COST;
    }
}

/// Get library version (for verification)
#[no_mangle]
pub extern "C" fn fold_accel_version() -> u32 {
    // Version 0.1.0 encoded as 0x000100
    0x000100
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_roundtrip_with_fuel() {
        let mut result = TestResult {
            status: 0,
            value: 0.0,
            fuel_out: 0,
        };

        fold_test_roundtrip(1.0, 2.0, 3.0, 100, &mut result);

        assert_eq!(result.status, 1);
        assert_eq!(result.value, 12.0); // (1+2+3)*2
        assert_eq!(result.fuel_out, 95); // 100 - 5
    }

    #[test]
    fn test_roundtrip_no_fuel() {
        let mut result = TestResult {
            status: 0,
            value: 0.0,
            fuel_out: 0,
        };

        fold_test_roundtrip(1.0, 2.0, 3.0, 2, &mut result);

        assert_eq!(result.status, 2); // out of fuel
        assert_eq!(result.fuel_out, 0);
    }
}
