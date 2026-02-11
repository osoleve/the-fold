//! bytes.rs - Bytevector operations for The Fold
//!
//! Layer 2 FFI: bulk operations on raw byte buffers.
//! All functions follow the out-pointer pattern with fuel tracking.
//!
//! Status codes:
//!   1 = success
//!   2 = out-of-fuel
//!   3 = runtime-error
//!   4 = buffer-overflow

use crate::{BufferResult, I64Result, U64Result};

/// Fuel costs for bytevector operations
pub mod cost {
    /// Base cost for hash operation
    pub const HASH_BASE: u64 = 10;
    /// Per-byte cost for hash
    pub const HASH_PER_BYTE: u64 = 1;
    /// Per-byte cost for compare
    pub const CMP_PER_BYTE: u64 = 1;
    /// Per-byte cost for copy/fill
    pub const COPY_PER_BYTE: u64 = 1;
}

/// FNV-1a hash for bytevectors
///
/// Fast, non-cryptographic hash suitable for hash tables.
/// Cost: HASH_BASE + len * HASH_PER_BYTE
#[no_mangle]
pub extern "C" fn fold_bv_hash(data: *const u8, len: usize, fuel_in: u64, out: *mut U64Result) {
    if out.is_null() {
        return;
    }
    let result = unsafe { &mut *out };

    let cost = cost::HASH_BASE + (len as u64).saturating_mul(cost::HASH_PER_BYTE);
    if fuel_in < cost {
        result.status = 2;
        result.value = 0;
        result.fuel_out = 0;
        return;
    }

    let bytes = if data.is_null() || len == 0 {
        &[][..]
    } else {
        unsafe { std::slice::from_raw_parts(data, len) }
    };

    // FNV-1a algorithm
    let mut hash: u64 = 0xcbf29ce484222325;
    for &byte in bytes {
        hash ^= byte as u64;
        hash = hash.wrapping_mul(0x100000001b3);
    }

    result.value = hash;
    result.status = 1;
    result.fuel_out = fuel_in - cost;
}

/// Compare two bytevectors lexicographically
///
/// Returns:
///   -1 if a < b
///    0 if a == b
///    1 if a > b
///
/// Cost: min(a_len, b_len) * CMP_PER_BYTE + 1
#[no_mangle]
pub extern "C" fn fold_bv_compare(
    a_data: *const u8,
    a_len: usize,
    b_data: *const u8,
    b_len: usize,
    fuel_in: u64,
    out: *mut I64Result,
) {
    if out.is_null() {
        return;
    }
    let result = unsafe { &mut *out };

    let cost = (a_len.min(b_len) as u64).saturating_mul(cost::CMP_PER_BYTE) + 1;
    if fuel_in < cost {
        result.status = 2;
        result.value = 0;
        result.fuel_out = 0;
        return;
    }

    let a = if a_data.is_null() || a_len == 0 {
        &[][..]
    } else {
        unsafe { std::slice::from_raw_parts(a_data, a_len) }
    };
    let b = if b_data.is_null() || b_len == 0 {
        &[][..]
    } else {
        unsafe { std::slice::from_raw_parts(b_data, b_len) }
    };

    result.value = match a.cmp(b) {
        std::cmp::Ordering::Less => -1,
        std::cmp::Ordering::Equal => 0,
        std::cmp::Ordering::Greater => 1,
    };
    result.status = 1;
    result.fuel_out = fuel_in - cost;
}

/// Copy bytes from src to dst
///
/// Copies exactly `n` bytes from src to dst.
/// Returns buffer-overflow (status=4) if n exceeds src_len or dst_len.
///
/// Cost: n * COPY_PER_BYTE + 1
#[no_mangle]
pub extern "C" fn fold_bv_copy(
    src_data: *const u8,
    src_len: usize,
    dst_data: *mut u8,
    dst_len: usize,
    n: usize,
    fuel_in: u64,
    out: *mut BufferResult,
) {
    if out.is_null() {
        return;
    }
    let result = unsafe { &mut *out };

    // Bounds check
    if n > src_len || n > dst_len {
        result.status = 4; // buffer overflow
        result.bytes_written = 0;
        result.fuel_out = fuel_in;
        return;
    }

    let cost = (n as u64).saturating_mul(cost::COPY_PER_BYTE) + 1;
    if fuel_in < cost {
        result.status = 2;
        result.bytes_written = 0;
        result.fuel_out = 0;
        return;
    }

    if !src_data.is_null() && !dst_data.is_null() && n > 0 {
        // Use copy (not copy_nonoverlapping) because src and dst may alias
        // the same Scheme bytevector — Scheme's bytevector-copy! allows overlap.
        unsafe {
            std::ptr::copy(src_data, dst_data, n);
        }
    }

    result.bytes_written = n;
    result.status = 1;
    result.fuel_out = fuel_in - cost;
}

/// Fill bytevector with a value
///
/// Fills `len` bytes starting at `data` with `value`.
///
/// Cost: len * COPY_PER_BYTE + 1
#[no_mangle]
pub extern "C" fn fold_bv_fill(
    data: *mut u8,
    len: usize,
    value: u8,
    fuel_in: u64,
    out: *mut BufferResult,
) {
    if out.is_null() {
        return;
    }
    let result = unsafe { &mut *out };

    let cost = (len as u64).saturating_mul(cost::COPY_PER_BYTE) + 1;
    if fuel_in < cost {
        result.status = 2;
        result.bytes_written = 0;
        result.fuel_out = 0;
        return;
    }

    if !data.is_null() && len > 0 {
        unsafe {
            std::ptr::write_bytes(data, value, len);
        }
    }

    result.bytes_written = len;
    result.status = 1;
    result.fuel_out = fuel_in - cost;
}

/// Check if two bytevectors are equal
///
/// Cost: min(a_len, b_len) * CMP_PER_BYTE + 1
#[no_mangle]
pub extern "C" fn fold_bv_equal(
    a_data: *const u8,
    a_len: usize,
    b_data: *const u8,
    b_len: usize,
    fuel_in: u64,
    out: *mut crate::BoolResult,
) {
    if out.is_null() {
        return;
    }
    let result = unsafe { &mut *out };

    // Quick length check (free)
    if a_len != b_len {
        result.value = 0;
        result.status = 1;
        result.fuel_out = fuel_in;
        return;
    }

    let cost = (a_len as u64).saturating_mul(cost::CMP_PER_BYTE) + 1;
    if fuel_in < cost {
        result.status = 2;
        result.value = 0;
        result.fuel_out = 0;
        return;
    }

    let a = if a_data.is_null() || a_len == 0 {
        &[][..]
    } else {
        unsafe { std::slice::from_raw_parts(a_data, a_len) }
    };
    let b = if b_data.is_null() || b_len == 0 {
        &[][..]
    } else {
        unsafe { std::slice::from_raw_parts(b_data, b_len) }
    };

    result.value = if a == b { 1 } else { 0 };
    result.status = 1;
    result.fuel_out = fuel_in - cost;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_bv_hash_basic() {
        let mut result = U64Result {
            status: 0,
            value: 0,
            fuel_out: 0,
        };
        let data = b"hello";
        fold_bv_hash(data.as_ptr(), data.len(), 1000, &mut result);

        assert_eq!(result.status, 1);
        assert!(result.value != 0); // Should produce non-zero hash
        assert!(result.fuel_out < 1000);
    }

    #[test]
    fn test_bv_hash_empty() {
        let mut result = U64Result {
            status: 0,
            value: 0,
            fuel_out: 0,
        };
        fold_bv_hash(std::ptr::null(), 0, 1000, &mut result);

        assert_eq!(result.status, 1);
        // Empty should hash to FNV offset basis
        assert_eq!(result.value, 0xcbf29ce484222325);
    }

    #[test]
    fn test_bv_hash_out_of_fuel() {
        let mut result = U64Result {
            status: 0,
            value: 0,
            fuel_out: 0,
        };
        let data = b"hello";
        fold_bv_hash(data.as_ptr(), data.len(), 1, &mut result);

        assert_eq!(result.status, 2); // out of fuel
    }

    #[test]
    fn test_bv_compare_less() {
        let mut result = I64Result {
            status: 0,
            value: 0,
            fuel_out: 0,
        };
        let a = b"abc";
        let b = b"abd";
        fold_bv_compare(a.as_ptr(), a.len(), b.as_ptr(), b.len(), 1000, &mut result);

        assert_eq!(result.status, 1);
        assert_eq!(result.value, -1); // abc < abd
    }

    #[test]
    fn test_bv_compare_equal() {
        let mut result = I64Result {
            status: 0,
            value: 0,
            fuel_out: 0,
        };
        let a = b"hello";
        let b = b"hello";
        fold_bv_compare(a.as_ptr(), a.len(), b.as_ptr(), b.len(), 1000, &mut result);

        assert_eq!(result.status, 1);
        assert_eq!(result.value, 0);
    }

    #[test]
    fn test_bv_compare_greater() {
        let mut result = I64Result {
            status: 0,
            value: 0,
            fuel_out: 0,
        };
        let a = b"b";
        let b = b"a";
        fold_bv_compare(a.as_ptr(), a.len(), b.as_ptr(), b.len(), 1000, &mut result);

        assert_eq!(result.status, 1);
        assert_eq!(result.value, 1);
    }

    #[test]
    fn test_bv_compare_length_matters() {
        let mut result = I64Result {
            status: 0,
            value: 0,
            fuel_out: 0,
        };
        let a = b"abc";
        let b = b"abcd";
        fold_bv_compare(a.as_ptr(), a.len(), b.as_ptr(), b.len(), 1000, &mut result);

        assert_eq!(result.status, 1);
        assert_eq!(result.value, -1); // abc < abcd
    }

    #[test]
    fn test_bv_copy_basic() {
        let mut result = BufferResult {
            status: 0,
            bytes_written: 0,
            fuel_out: 0,
        };
        let src = b"hello";
        let mut dst = [0u8; 10];
        fold_bv_copy(
            src.as_ptr(),
            src.len(),
            dst.as_mut_ptr(),
            dst.len(),
            5,
            1000,
            &mut result,
        );

        assert_eq!(result.status, 1);
        assert_eq!(result.bytes_written, 5);
        assert_eq!(&dst[..5], b"hello");
    }

    #[test]
    fn test_bv_copy_overflow() {
        let mut result = BufferResult {
            status: 0,
            bytes_written: 0,
            fuel_out: 0,
        };
        let src = b"hello";
        let mut dst = [0u8; 3];
        fold_bv_copy(
            src.as_ptr(),
            src.len(),
            dst.as_mut_ptr(),
            dst.len(),
            5, // Trying to copy more than dst can hold
            1000,
            &mut result,
        );

        assert_eq!(result.status, 4); // buffer overflow
    }

    #[test]
    fn test_bv_fill_basic() {
        let mut result = BufferResult {
            status: 0,
            bytes_written: 0,
            fuel_out: 0,
        };
        let mut data = [0u8; 5];
        fold_bv_fill(data.as_mut_ptr(), data.len(), 0xAB, 1000, &mut result);

        assert_eq!(result.status, 1);
        assert_eq!(result.bytes_written, 5);
        assert_eq!(data, [0xAB, 0xAB, 0xAB, 0xAB, 0xAB]);
    }

    #[test]
    fn test_bv_equal_same() {
        let mut result = crate::BoolResult {
            status: 0,
            value: 0,
            fuel_out: 0,
        };
        let a = b"hello";
        let b = b"hello";
        fold_bv_equal(a.as_ptr(), a.len(), b.as_ptr(), b.len(), 1000, &mut result);

        assert_eq!(result.status, 1);
        assert_eq!(result.value, 1); // true
    }

    #[test]
    fn test_bv_equal_different() {
        let mut result = crate::BoolResult {
            status: 0,
            value: 0,
            fuel_out: 0,
        };
        let a = b"hello";
        let b = b"world";
        fold_bv_equal(a.as_ptr(), a.len(), b.as_ptr(), b.len(), 1000, &mut result);

        assert_eq!(result.status, 1);
        assert_eq!(result.value, 0); // false
    }

    #[test]
    fn test_bv_equal_different_length() {
        let mut result = crate::BoolResult {
            status: 0,
            value: 0,
            fuel_out: 0,
        };
        let a = b"hello";
        let b = b"hello world";
        fold_bv_equal(a.as_ptr(), a.len(), b.as_ptr(), b.len(), 1000, &mut result);

        assert_eq!(result.status, 1);
        assert_eq!(result.value, 0); // false - different lengths
    }
}
