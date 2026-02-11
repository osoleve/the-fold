//! string.rs - String operations for The Fold
//!
//! Layer 2 FFI: string algorithms accelerated in Rust.
//!
//! IMPORTANT: All input strings are assumed to be valid UTF-8.
//! The boundary layer validates UTF-8 before calling these functions.
//! These functions operate on raw bytes for efficiency.
//!
//! Status codes:
//!   1 = success
//!   2 = out-of-fuel
//!   3 = runtime-error
//!   4 = buffer-overflow

use crate::{BoolResult, BufferResult, I64Result, U64Result};

/// Fuel costs for string operations
pub mod cost {
    /// Base cost for Levenshtein (plus O(m*n) per comparison)
    pub const LEVENSHTEIN_BASE: u64 = 10;
    /// Per-byte cost for search operations
    pub const SEARCH_PER_BYTE: u64 = 1;
    /// Per-byte cost for case conversion
    pub const CASE_CONVERT_PER_BYTE: u64 = 1;
}

/// Levenshtein edit distance using Wagner-Fischer algorithm
///
/// Computes the minimum number of single-character edits (insertions,
/// deletions, substitutions) needed to transform s1 into s2.
///
/// Space-optimized to O(min(m,n)) using two-row technique.
/// Time complexity: O(m*n)
///
/// Cost: LEVENSHTEIN_BASE + s1_len * s2_len
#[no_mangle]
pub extern "C" fn fold_levenshtein(
    s1_data: *const u8,
    s1_len: usize,
    s2_data: *const u8,
    s2_len: usize,
    fuel_in: u64,
    out: *mut U64Result,
) {
    if out.is_null() {
        return;
    }
    let result = unsafe { &mut *out };

    // Cost is O(m*n) - use saturating arithmetic to avoid overflow
    let cost = cost::LEVENSHTEIN_BASE + (s1_len as u64).saturating_mul(s2_len as u64);
    if fuel_in < cost {
        result.status = 2;
        result.value = 0;
        result.fuel_out = 0;
        return;
    }

    let s1 = if s1_data.is_null() || s1_len == 0 {
        &[][..]
    } else {
        unsafe { std::slice::from_raw_parts(s1_data, s1_len) }
    };
    let s2 = if s2_data.is_null() || s2_len == 0 {
        &[][..]
    } else {
        unsafe { std::slice::from_raw_parts(s2_data, s2_len) }
    };

    result.value = levenshtein_impl(s1, s2);
    result.status = 1;
    result.fuel_out = fuel_in - cost;
}

/// Internal Levenshtein implementation
fn levenshtein_impl(s1: &[u8], s2: &[u8]) -> u64 {
    // Handle empty strings
    if s1.is_empty() {
        return s2.len() as u64;
    }
    if s2.is_empty() {
        return s1.len() as u64;
    }

    // Ensure s1 is the shorter string for space optimization
    let (s1, s2) = if s1.len() <= s2.len() {
        (s1, s2)
    } else {
        (s2, s1)
    };

    let m = s1.len();
    let n = s2.len();

    // Two-row technique: only keep current and previous row
    let mut prev: Vec<usize> = (0..=m).collect();
    let mut curr = vec![0; m + 1];

    for j in 1..=n {
        curr[0] = j;
        for i in 1..=m {
            let cost = if s1[i - 1] == s2[j - 1] { 0 } else { 1 };
            curr[i] = (prev[i] + 1) // deletion
                .min(curr[i - 1] + 1) // insertion
                .min(prev[i - 1] + cost); // substitution
        }
        std::mem::swap(&mut prev, &mut curr);
    }

    prev[m] as u64
}

/// Check if haystack contains needle
///
/// Cost: haystack_len * SEARCH_PER_BYTE + 1
#[no_mangle]
pub extern "C" fn fold_str_contains(
    haystack_data: *const u8,
    haystack_len: usize,
    needle_data: *const u8,
    needle_len: usize,
    fuel_in: u64,
    out: *mut BoolResult,
) {
    if out.is_null() {
        return;
    }
    let result = unsafe { &mut *out };

    let cost = (haystack_len as u64).saturating_mul(cost::SEARCH_PER_BYTE) + 1;
    if fuel_in < cost {
        result.status = 2;
        result.value = 0;
        result.fuel_out = 0;
        return;
    }

    // Empty needle is always found
    if needle_len == 0 {
        result.value = 1;
        result.status = 1;
        result.fuel_out = fuel_in - cost;
        return;
    }

    // Needle larger than haystack - can't contain
    if needle_len > haystack_len {
        result.value = 0;
        result.status = 1;
        result.fuel_out = fuel_in - cost;
        return;
    }

    let haystack = if haystack_data.is_null() {
        &[][..]
    } else {
        unsafe { std::slice::from_raw_parts(haystack_data, haystack_len) }
    };
    let needle = if needle_data.is_null() {
        &[][..]
    } else {
        unsafe { std::slice::from_raw_parts(needle_data, needle_len) }
    };

    // Simple search - future: use memmem or SIMD
    for i in 0..=(haystack_len - needle_len) {
        if &haystack[i..i + needle_len] == needle {
            result.value = 1;
            result.status = 1;
            result.fuel_out = fuel_in - cost;
            return;
        }
    }

    result.value = 0;
    result.status = 1;
    result.fuel_out = fuel_in - cost;
}

/// Find first occurrence of needle in haystack
///
/// Returns index of first occurrence, or -1 if not found.
///
/// Cost: haystack_len * SEARCH_PER_BYTE + 1
#[no_mangle]
pub extern "C" fn fold_str_index_of(
    haystack_data: *const u8,
    haystack_len: usize,
    needle_data: *const u8,
    needle_len: usize,
    fuel_in: u64,
    out: *mut I64Result,
) {
    if out.is_null() {
        return;
    }
    let result = unsafe { &mut *out };

    let cost = (haystack_len as u64).saturating_mul(cost::SEARCH_PER_BYTE) + 1;
    if fuel_in < cost {
        result.status = 2;
        result.value = 0;
        result.fuel_out = 0;
        return;
    }

    // Empty needle always found at position 0
    if needle_len == 0 {
        result.value = 0;
        result.status = 1;
        result.fuel_out = fuel_in - cost;
        return;
    }

    // Needle larger than haystack
    if needle_len > haystack_len {
        result.value = -1;
        result.status = 1;
        result.fuel_out = fuel_in - cost;
        return;
    }

    let haystack = if haystack_data.is_null() {
        &[][..]
    } else {
        unsafe { std::slice::from_raw_parts(haystack_data, haystack_len) }
    };
    let needle = if needle_data.is_null() {
        &[][..]
    } else {
        unsafe { std::slice::from_raw_parts(needle_data, needle_len) }
    };

    // Simple search
    for i in 0..=(haystack_len - needle_len) {
        if &haystack[i..i + needle_len] == needle {
            result.value = i as i64;
            result.status = 1;
            result.fuel_out = fuel_in - cost;
            return;
        }
    }

    result.value = -1;
    result.status = 1;
    result.fuel_out = fuel_in - cost;
}

/// Find last occurrence of needle in haystack
///
/// Returns index of last occurrence, or -1 if not found.
///
/// Cost: haystack_len * SEARCH_PER_BYTE + 1
#[no_mangle]
pub extern "C" fn fold_str_last_index_of(
    haystack_data: *const u8,
    haystack_len: usize,
    needle_data: *const u8,
    needle_len: usize,
    fuel_in: u64,
    out: *mut I64Result,
) {
    if out.is_null() {
        return;
    }
    let result = unsafe { &mut *out };

    let cost = (haystack_len as u64).saturating_mul(cost::SEARCH_PER_BYTE) + 1;
    if fuel_in < cost {
        result.status = 2;
        result.value = 0;
        result.fuel_out = 0;
        return;
    }

    // Empty needle - last valid position
    if needle_len == 0 {
        result.value = haystack_len as i64;
        result.status = 1;
        result.fuel_out = fuel_in - cost;
        return;
    }

    // Needle larger than haystack
    if needle_len > haystack_len {
        result.value = -1;
        result.status = 1;
        result.fuel_out = fuel_in - cost;
        return;
    }

    let haystack = if haystack_data.is_null() {
        &[][..]
    } else {
        unsafe { std::slice::from_raw_parts(haystack_data, haystack_len) }
    };
    let needle = if needle_data.is_null() {
        &[][..]
    } else {
        unsafe { std::slice::from_raw_parts(needle_data, needle_len) }
    };

    // Search backwards
    for i in (0..=(haystack_len - needle_len)).rev() {
        if &haystack[i..i + needle_len] == needle {
            result.value = i as i64;
            result.status = 1;
            result.fuel_out = fuel_in - cost;
            return;
        }
    }

    result.value = -1;
    result.status = 1;
    result.fuel_out = fuel_in - cost;
}

/// Convert ASCII to uppercase
///
/// Non-ASCII bytes are copied unchanged.
/// Returns buffer-overflow (status=4) if dst_len < src_len.
///
/// Cost: src_len * CASE_CONVERT_PER_BYTE + 1
#[no_mangle]
pub extern "C" fn fold_str_upcase(
    src_data: *const u8,
    src_len: usize,
    dst_data: *mut u8,
    dst_len: usize,
    fuel_in: u64,
    out: *mut BufferResult,
) {
    if out.is_null() {
        return;
    }
    let result = unsafe { &mut *out };

    // Bounds check
    if src_len > dst_len {
        result.status = 4; // buffer overflow
        result.bytes_written = 0;
        result.fuel_out = fuel_in;
        return;
    }

    let cost = (src_len as u64).saturating_mul(cost::CASE_CONVERT_PER_BYTE) + 1;
    if fuel_in < cost {
        result.status = 2;
        result.bytes_written = 0;
        result.fuel_out = 0;
        return;
    }

    if src_data.is_null() || dst_data.is_null() || src_len == 0 {
        result.bytes_written = 0;
        result.status = 1;
        result.fuel_out = fuel_in - cost;
        return;
    }

    let src = unsafe { std::slice::from_raw_parts(src_data, src_len) };
    let dst = unsafe { std::slice::from_raw_parts_mut(dst_data, src_len) };

    for (i, &byte) in src.iter().enumerate() {
        dst[i] = byte.to_ascii_uppercase();
    }

    result.bytes_written = src_len;
    result.status = 1;
    result.fuel_out = fuel_in - cost;
}

/// Convert ASCII to lowercase
///
/// Non-ASCII bytes are copied unchanged.
/// Returns buffer-overflow (status=4) if dst_len < src_len.
///
/// Cost: src_len * CASE_CONVERT_PER_BYTE + 1
#[no_mangle]
pub extern "C" fn fold_str_downcase(
    src_data: *const u8,
    src_len: usize,
    dst_data: *mut u8,
    dst_len: usize,
    fuel_in: u64,
    out: *mut BufferResult,
) {
    if out.is_null() {
        return;
    }
    let result = unsafe { &mut *out };

    // Bounds check
    if src_len > dst_len {
        result.status = 4;
        result.bytes_written = 0;
        result.fuel_out = fuel_in;
        return;
    }

    let cost = (src_len as u64).saturating_mul(cost::CASE_CONVERT_PER_BYTE) + 1;
    if fuel_in < cost {
        result.status = 2;
        result.bytes_written = 0;
        result.fuel_out = 0;
        return;
    }

    if src_data.is_null() || dst_data.is_null() || src_len == 0 {
        result.bytes_written = 0;
        result.status = 1;
        result.fuel_out = fuel_in - cost;
        return;
    }

    let src = unsafe { std::slice::from_raw_parts(src_data, src_len) };
    let dst = unsafe { std::slice::from_raw_parts_mut(dst_data, src_len) };

    for (i, &byte) in src.iter().enumerate() {
        dst[i] = byte.to_ascii_lowercase();
    }

    result.bytes_written = src_len;
    result.status = 1;
    result.fuel_out = fuel_in - cost;
}

/// Check if string starts with prefix
///
/// Cost: prefix_len * SEARCH_PER_BYTE + 1
#[no_mangle]
pub extern "C" fn fold_str_starts_with(
    s_data: *const u8,
    s_len: usize,
    prefix_data: *const u8,
    prefix_len: usize,
    fuel_in: u64,
    out: *mut BoolResult,
) {
    if out.is_null() {
        return;
    }
    let result = unsafe { &mut *out };

    let cost = (prefix_len as u64).saturating_mul(cost::SEARCH_PER_BYTE) + 1;
    if fuel_in < cost {
        result.status = 2;
        result.value = 0;
        result.fuel_out = 0;
        return;
    }

    // Empty prefix always matches
    if prefix_len == 0 {
        result.value = 1;
        result.status = 1;
        result.fuel_out = fuel_in - cost;
        return;
    }

    // Prefix longer than string
    if prefix_len > s_len {
        result.value = 0;
        result.status = 1;
        result.fuel_out = fuel_in - cost;
        return;
    }

    let s = if s_data.is_null() {
        &[][..]
    } else {
        unsafe { std::slice::from_raw_parts(s_data, s_len) }
    };
    let prefix = if prefix_data.is_null() {
        &[][..]
    } else {
        unsafe { std::slice::from_raw_parts(prefix_data, prefix_len) }
    };

    result.value = if s.starts_with(prefix) { 1 } else { 0 };
    result.status = 1;
    result.fuel_out = fuel_in - cost;
}

/// Check if string ends with suffix
///
/// Cost: suffix_len * SEARCH_PER_BYTE + 1
#[no_mangle]
pub extern "C" fn fold_str_ends_with(
    s_data: *const u8,
    s_len: usize,
    suffix_data: *const u8,
    suffix_len: usize,
    fuel_in: u64,
    out: *mut BoolResult,
) {
    if out.is_null() {
        return;
    }
    let result = unsafe { &mut *out };

    let cost = (suffix_len as u64).saturating_mul(cost::SEARCH_PER_BYTE) + 1;
    if fuel_in < cost {
        result.status = 2;
        result.value = 0;
        result.fuel_out = 0;
        return;
    }

    // Empty suffix always matches
    if suffix_len == 0 {
        result.value = 1;
        result.status = 1;
        result.fuel_out = fuel_in - cost;
        return;
    }

    // Suffix longer than string
    if suffix_len > s_len {
        result.value = 0;
        result.status = 1;
        result.fuel_out = fuel_in - cost;
        return;
    }

    let s = if s_data.is_null() {
        &[][..]
    } else {
        unsafe { std::slice::from_raw_parts(s_data, s_len) }
    };
    let suffix = if suffix_data.is_null() {
        &[][..]
    } else {
        unsafe { std::slice::from_raw_parts(suffix_data, suffix_len) }
    };

    result.value = if s.ends_with(suffix) { 1 } else { 0 };
    result.status = 1;
    result.fuel_out = fuel_in - cost;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_levenshtein_same() {
        let mut result = U64Result {
            status: 0,
            _pad: [0; 7],
            value: 0,
            fuel_out: 0,
        };
        let s = b"hello";
        fold_levenshtein(s.as_ptr(), s.len(), s.as_ptr(), s.len(), 10000, &mut result);

        assert_eq!(result.status, 1);
        assert_eq!(result.value, 0);
    }

    #[test]
    fn test_levenshtein_kitten_sitting() {
        let mut result = U64Result {
            status: 0,
            _pad: [0; 7],
            value: 0,
            fuel_out: 0,
        };
        let s1 = b"kitten";
        let s2 = b"sitting";
        fold_levenshtein(
            s1.as_ptr(),
            s1.len(),
            s2.as_ptr(),
            s2.len(),
            10000,
            &mut result,
        );

        assert_eq!(result.status, 1);
        assert_eq!(result.value, 3); // kitten -> sitten -> sittin -> sitting
    }

    #[test]
    fn test_levenshtein_empty() {
        let mut result = U64Result {
            status: 0,
            _pad: [0; 7],
            value: 0,
            fuel_out: 0,
        };
        let s = b"hello";
        fold_levenshtein(std::ptr::null(), 0, s.as_ptr(), s.len(), 10000, &mut result);

        assert_eq!(result.status, 1);
        assert_eq!(result.value, 5); // Insert 5 chars
    }

    #[test]
    fn test_levenshtein_single_diff() {
        let mut result = U64Result {
            status: 0,
            _pad: [0; 7],
            value: 0,
            fuel_out: 0,
        };
        let s1 = b"cat";
        let s2 = b"bat";
        fold_levenshtein(
            s1.as_ptr(),
            s1.len(),
            s2.as_ptr(),
            s2.len(),
            10000,
            &mut result,
        );

        assert_eq!(result.status, 1);
        assert_eq!(result.value, 1);
    }

    #[test]
    fn test_levenshtein_out_of_fuel() {
        let mut result = U64Result {
            status: 0,
            _pad: [0; 7],
            value: 0,
            fuel_out: 0,
        };
        let s1 = b"hello";
        let s2 = b"world";
        fold_levenshtein(s1.as_ptr(), s1.len(), s2.as_ptr(), s2.len(), 1, &mut result);

        assert_eq!(result.status, 2); // out of fuel
    }

    #[test]
    fn test_str_contains_found() {
        let mut result = BoolResult {
            status: 0,
            value: 0,
            _pad: [0; 6],
            fuel_out: 0,
        };
        let haystack = b"hello world";
        let needle = b"world";
        fold_str_contains(
            haystack.as_ptr(),
            haystack.len(),
            needle.as_ptr(),
            needle.len(),
            1000,
            &mut result,
        );

        assert_eq!(result.status, 1);
        assert_eq!(result.value, 1); // true
    }

    #[test]
    fn test_str_contains_not_found() {
        let mut result = BoolResult {
            status: 0,
            value: 0,
            _pad: [0; 6],
            fuel_out: 0,
        };
        let haystack = b"hello world";
        let needle = b"foo";
        fold_str_contains(
            haystack.as_ptr(),
            haystack.len(),
            needle.as_ptr(),
            needle.len(),
            1000,
            &mut result,
        );

        assert_eq!(result.status, 1);
        assert_eq!(result.value, 0); // false
    }

    #[test]
    fn test_str_contains_empty_needle() {
        let mut result = BoolResult {
            status: 0,
            value: 0,
            _pad: [0; 6],
            fuel_out: 0,
        };
        let haystack = b"hello";
        fold_str_contains(
            haystack.as_ptr(),
            haystack.len(),
            std::ptr::null(),
            0,
            1000,
            &mut result,
        );

        assert_eq!(result.status, 1);
        assert_eq!(result.value, 1); // empty needle always found
    }

    #[test]
    fn test_str_index_of_found() {
        let mut result = I64Result {
            status: 0,
            _pad: [0; 7],
            value: 0,
            fuel_out: 0,
        };
        let haystack = b"hello world";
        let needle = b"world";
        fold_str_index_of(
            haystack.as_ptr(),
            haystack.len(),
            needle.as_ptr(),
            needle.len(),
            1000,
            &mut result,
        );

        assert_eq!(result.status, 1);
        assert_eq!(result.value, 6); // "world" starts at index 6
    }

    #[test]
    fn test_str_index_of_not_found() {
        let mut result = I64Result {
            status: 0,
            _pad: [0; 7],
            value: 0,
            fuel_out: 0,
        };
        let haystack = b"hello world";
        let needle = b"foo";
        fold_str_index_of(
            haystack.as_ptr(),
            haystack.len(),
            needle.as_ptr(),
            needle.len(),
            1000,
            &mut result,
        );

        assert_eq!(result.status, 1);
        assert_eq!(result.value, -1);
    }

    #[test]
    fn test_str_last_index_of() {
        let mut result = I64Result {
            status: 0,
            _pad: [0; 7],
            value: 0,
            fuel_out: 0,
        };
        let haystack = b"hello hello";
        let needle = b"hello";
        fold_str_last_index_of(
            haystack.as_ptr(),
            haystack.len(),
            needle.as_ptr(),
            needle.len(),
            1000,
            &mut result,
        );

        assert_eq!(result.status, 1);
        assert_eq!(result.value, 6); // Last "hello" starts at 6
    }

    #[test]
    fn test_str_upcase() {
        let mut result = BufferResult {
            status: 0,
            _pad: [0; 7],
            bytes_written: 0,
            fuel_out: 0,
        };
        let src = b"Hello World!";
        let mut dst = [0u8; 20];
        fold_str_upcase(
            src.as_ptr(),
            src.len(),
            dst.as_mut_ptr(),
            dst.len(),
            1000,
            &mut result,
        );

        assert_eq!(result.status, 1);
        assert_eq!(result.bytes_written, 12);
        assert_eq!(&dst[..12], b"HELLO WORLD!");
    }

    #[test]
    fn test_str_downcase() {
        let mut result = BufferResult {
            status: 0,
            _pad: [0; 7],
            bytes_written: 0,
            fuel_out: 0,
        };
        let src = b"Hello World!";
        let mut dst = [0u8; 20];
        fold_str_downcase(
            src.as_ptr(),
            src.len(),
            dst.as_mut_ptr(),
            dst.len(),
            1000,
            &mut result,
        );

        assert_eq!(result.status, 1);
        assert_eq!(result.bytes_written, 12);
        assert_eq!(&dst[..12], b"hello world!");
    }

    #[test]
    fn test_str_upcase_overflow() {
        let mut result = BufferResult {
            status: 0,
            _pad: [0; 7],
            bytes_written: 0,
            fuel_out: 0,
        };
        let src = b"hello";
        let mut dst = [0u8; 3]; // Too small
        fold_str_upcase(
            src.as_ptr(),
            src.len(),
            dst.as_mut_ptr(),
            dst.len(),
            1000,
            &mut result,
        );

        assert_eq!(result.status, 4); // buffer overflow
    }

    #[test]
    fn test_str_starts_with() {
        let mut result = BoolResult {
            status: 0,
            value: 0,
            _pad: [0; 6],
            fuel_out: 0,
        };
        let s = b"hello world";
        let prefix = b"hello";
        fold_str_starts_with(
            s.as_ptr(),
            s.len(),
            prefix.as_ptr(),
            prefix.len(),
            1000,
            &mut result,
        );

        assert_eq!(result.status, 1);
        assert_eq!(result.value, 1); // true
    }

    #[test]
    fn test_str_starts_with_no_match() {
        let mut result = BoolResult {
            status: 0,
            value: 0,
            _pad: [0; 6],
            fuel_out: 0,
        };
        let s = b"hello world";
        let prefix = b"world";
        fold_str_starts_with(
            s.as_ptr(),
            s.len(),
            prefix.as_ptr(),
            prefix.len(),
            1000,
            &mut result,
        );

        assert_eq!(result.status, 1);
        assert_eq!(result.value, 0); // false
    }

    #[test]
    fn test_str_ends_with() {
        let mut result = BoolResult {
            status: 0,
            value: 0,
            _pad: [0; 6],
            fuel_out: 0,
        };
        let s = b"hello world";
        let suffix = b"world";
        fold_str_ends_with(
            s.as_ptr(),
            s.len(),
            suffix.as_ptr(),
            suffix.len(),
            1000,
            &mut result,
        );

        assert_eq!(result.status, 1);
        assert_eq!(result.value, 1); // true
    }

    #[test]
    fn test_str_ends_with_no_match() {
        let mut result = BoolResult {
            status: 0,
            value: 0,
            _pad: [0; 6],
            fuel_out: 0,
        };
        let s = b"hello world";
        let suffix = b"hello";
        fold_str_ends_with(
            s.as_ptr(),
            s.len(),
            suffix.as_ptr(),
            suffix.len(),
            1000,
            &mut result,
        );

        assert_eq!(result.status, 1);
        assert_eq!(result.value, 0); // false
    }
}
