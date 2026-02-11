//! mat4.rs - 4x4 Matrix Operations for The Fold
//!
//! Layer 2 FFI: operations where computation >> FFI overhead.
//! 4x4 matrix multiply: 64 muls + 48 adds = ~112 ops.
//!
//! Design: Pass matrix as pointer to 16 f64s (row-major).
//! Result written to out-pointer for FFI safety.

use crate::F64Result;

/// Result struct for 4x4 matrix operations
/// Fields ordered for natural alignment, explicit padding for FFI safety
#[repr(C)]
pub struct Mat4Result {
    /// Matrix values in row-major order (m00, m01, m02, m03, m10, ...)
    pub m: [f64; 16],
    /// Remaining fuel
    pub fuel_out: u64,
    /// Status: 1=success, 2=out-of-fuel, 3=runtime-error
    pub status: u8,
    pub _pad: [u8; 7],
}

/// Result struct for vec4 operations (matrix-vector multiply)
/// Fields ordered for natural alignment, explicit padding for FFI safety
#[repr(C)]
pub struct Vec4Result {
    pub v: [f64; 4],
    pub fuel_out: u64,
    pub status: u8,
    pub _pad: [u8; 7],
}

/// Fuel cost constants for mat4 operations
pub mod cost {
    /// 4x4 matrix multiply: 64 muls + 48 adds
    pub const MAT4_MUL: u64 = 112;
    /// 4x4 matrix-vector multiply: 16 muls + 12 adds
    pub const MAT4_VEC_MUL: u64 = 28;
    /// 4x4 matrix transpose (just memory movement)
    pub const MAT4_TRANSPOSE: u64 = 16;
    /// 4x4 matrix determinant (expensive)
    pub const MAT4_DET: u64 = 100;
    /// 4x4 matrix inverse (very expensive)
    pub const MAT4_INVERSE: u64 = 250;
}

/// Matrix multiply: C = A * B
/// Both matrices in row-major order as flat [f64; 16]
///
/// Row-major indexing: m[i][j] = data[i*4 + j]
#[no_mangle]
pub extern "C" fn fold_mat4_mul(a: *const f64, b: *const f64, fuel_in: u64, out: *mut Mat4Result) {
    if out.is_null() || a.is_null() || b.is_null() {
        return;
    }

    let result = unsafe { &mut *out };

    if fuel_in < cost::MAT4_MUL {
        result.status = 2;
        result.fuel_out = 0;
        return;
    }

    // Safety: caller ensures valid pointers to 16 f64s
    let a = unsafe { std::slice::from_raw_parts(a, 16) };
    let b = unsafe { std::slice::from_raw_parts(b, 16) };

    // Fully unrolled matrix multiply (no loops for maximum performance)
    // C[i][j] = sum(k, A[i][k] * B[k][j])

    // Row 0
    result.m[0] = a[0] * b[0] + a[1] * b[4] + a[2] * b[8] + a[3] * b[12];
    result.m[1] = a[0] * b[1] + a[1] * b[5] + a[2] * b[9] + a[3] * b[13];
    result.m[2] = a[0] * b[2] + a[1] * b[6] + a[2] * b[10] + a[3] * b[14];
    result.m[3] = a[0] * b[3] + a[1] * b[7] + a[2] * b[11] + a[3] * b[15];

    // Row 1
    result.m[4] = a[4] * b[0] + a[5] * b[4] + a[6] * b[8] + a[7] * b[12];
    result.m[5] = a[4] * b[1] + a[5] * b[5] + a[6] * b[9] + a[7] * b[13];
    result.m[6] = a[4] * b[2] + a[5] * b[6] + a[6] * b[10] + a[7] * b[14];
    result.m[7] = a[4] * b[3] + a[5] * b[7] + a[6] * b[11] + a[7] * b[15];

    // Row 2
    result.m[8] = a[8] * b[0] + a[9] * b[4] + a[10] * b[8] + a[11] * b[12];
    result.m[9] = a[8] * b[1] + a[9] * b[5] + a[10] * b[9] + a[11] * b[13];
    result.m[10] = a[8] * b[2] + a[9] * b[6] + a[10] * b[10] + a[11] * b[14];
    result.m[11] = a[8] * b[3] + a[9] * b[7] + a[10] * b[11] + a[11] * b[15];

    // Row 3
    result.m[12] = a[12] * b[0] + a[13] * b[4] + a[14] * b[8] + a[15] * b[12];
    result.m[13] = a[12] * b[1] + a[13] * b[5] + a[14] * b[9] + a[15] * b[13];
    result.m[14] = a[12] * b[2] + a[13] * b[6] + a[14] * b[10] + a[15] * b[14];
    result.m[15] = a[12] * b[3] + a[13] * b[7] + a[14] * b[11] + a[15] * b[15];

    result.status = 1;
    result.fuel_out = fuel_in - cost::MAT4_MUL;
}

/// Matrix-vector multiply: v' = M * v
/// Matrix in row-major order, vector as [x, y, z, w]
#[no_mangle]
pub extern "C" fn fold_mat4_vec_mul(
    m: *const f64,
    v: *const f64,
    fuel_in: u64,
    out: *mut Vec4Result,
) {
    if out.is_null() || m.is_null() || v.is_null() {
        return;
    }

    let result = unsafe { &mut *out };

    if fuel_in < cost::MAT4_VEC_MUL {
        result.status = 2;
        result.fuel_out = 0;
        return;
    }

    let m = unsafe { std::slice::from_raw_parts(m, 16) };
    let v = unsafe { std::slice::from_raw_parts(v, 4) };

    // v'[i] = sum(j, M[i][j] * v[j])
    result.v[0] = m[0] * v[0] + m[1] * v[1] + m[2] * v[2] + m[3] * v[3];
    result.v[1] = m[4] * v[0] + m[5] * v[1] + m[6] * v[2] + m[7] * v[3];
    result.v[2] = m[8] * v[0] + m[9] * v[1] + m[10] * v[2] + m[11] * v[3];
    result.v[3] = m[12] * v[0] + m[13] * v[1] + m[14] * v[2] + m[15] * v[3];

    result.status = 1;
    result.fuel_out = fuel_in - cost::MAT4_VEC_MUL;
}

/// Batch matrix-vector multiply: transform N points through same matrix
/// This is where Layer 2 FFI really shines - amortize overhead across N points
#[no_mangle]
pub extern "C" fn fold_mat4_transform_points(
    m: *const f64,         // 4x4 matrix (16 f64s)
    points_in: *const f64, // N points (4*N f64s)
    n: u64,                // number of points
    points_out: *mut f64,  // output buffer (4*N f64s)
    fuel_in: u64,
    fuel_out: *mut u64,
    status: *mut u8,
) {
    if m.is_null()
        || points_in.is_null()
        || points_out.is_null()
        || fuel_out.is_null()
        || status.is_null()
    {
        return;
    }

    // Check for overflow in fuel calculation
    let total_cost = match cost::MAT4_VEC_MUL.checked_mul(n) {
        Some(c) => c,
        None => {
            // Overflow - return runtime error
            unsafe {
                *status = 3;
                *fuel_out = 0;
            }
            return;
        }
    };

    if fuel_in < total_cost {
        unsafe {
            *status = 2;
            *fuel_out = 0;
        }
        return;
    }

    // Check that n fits in usize (for 32-bit platforms)
    let n_usize = match usize::try_from(n) {
        Ok(n) => n,
        Err(_) => {
            // n too large for this platform
            unsafe {
                *status = 3;
                *fuel_out = 0;
            }
            return;
        }
    };

    let m = unsafe { std::slice::from_raw_parts(m, 16) };
    let points_in = unsafe { std::slice::from_raw_parts(points_in, 4 * n_usize) };
    let points_out = unsafe { std::slice::from_raw_parts_mut(points_out, 4 * n_usize) };

    // Process all points
    for i in 0..n_usize {
        let base = i * 4;
        let v0 = points_in[base];
        let v1 = points_in[base + 1];
        let v2 = points_in[base + 2];
        let v3 = points_in[base + 3];

        points_out[base] = m[0] * v0 + m[1] * v1 + m[2] * v2 + m[3] * v3;
        points_out[base + 1] = m[4] * v0 + m[5] * v1 + m[6] * v2 + m[7] * v3;
        points_out[base + 2] = m[8] * v0 + m[9] * v1 + m[10] * v2 + m[11] * v3;
        points_out[base + 3] = m[12] * v0 + m[13] * v1 + m[14] * v2 + m[15] * v3;
    }

    unsafe {
        *status = 1;
        *fuel_out = fuel_in - total_cost;
    }
}

/// Matrix transpose (in-place friendly)
#[no_mangle]
pub extern "C" fn fold_mat4_transpose(m: *const f64, fuel_in: u64, out: *mut Mat4Result) {
    if out.is_null() || m.is_null() {
        return;
    }

    let result = unsafe { &mut *out };

    if fuel_in < cost::MAT4_TRANSPOSE {
        result.status = 2;
        result.fuel_out = 0;
        return;
    }

    let m = unsafe { std::slice::from_raw_parts(m, 16) };

    // Transpose: swap rows and columns
    // m'[i][j] = m[j][i]
    result.m[0] = m[0];
    result.m[1] = m[4];
    result.m[2] = m[8];
    result.m[3] = m[12];
    result.m[4] = m[1];
    result.m[5] = m[5];
    result.m[6] = m[9];
    result.m[7] = m[13];
    result.m[8] = m[2];
    result.m[9] = m[6];
    result.m[10] = m[10];
    result.m[11] = m[14];
    result.m[12] = m[3];
    result.m[13] = m[7];
    result.m[14] = m[11];
    result.m[15] = m[15];

    result.status = 1;
    result.fuel_out = fuel_in - cost::MAT4_TRANSPOSE;
}

/// Matrix determinant (4x4)
/// Uses cofactor expansion along first row
#[no_mangle]
pub extern "C" fn fold_mat4_determinant(m: *const f64, fuel_in: u64, out: *mut F64Result) {
    if out.is_null() || m.is_null() {
        return;
    }

    let result = unsafe { &mut *out };

    if fuel_in < cost::MAT4_DET {
        result.status = 2;
        result.fuel_out = 0;
        return;
    }

    let m = unsafe { std::slice::from_raw_parts(m, 16) };

    // 3x3 subdeterminants (cofactors of first row)
    let sub0 = m[5] * (m[10] * m[15] - m[11] * m[14]) - m[6] * (m[9] * m[15] - m[11] * m[13])
        + m[7] * (m[9] * m[14] - m[10] * m[13]);

    let sub1 = m[4] * (m[10] * m[15] - m[11] * m[14]) - m[6] * (m[8] * m[15] - m[11] * m[12])
        + m[7] * (m[8] * m[14] - m[10] * m[12]);

    let sub2 = m[4] * (m[9] * m[15] - m[11] * m[13]) - m[5] * (m[8] * m[15] - m[11] * m[12])
        + m[7] * (m[8] * m[13] - m[9] * m[12]);

    let sub3 = m[4] * (m[9] * m[14] - m[10] * m[13]) - m[5] * (m[8] * m[14] - m[10] * m[12])
        + m[6] * (m[8] * m[13] - m[9] * m[12]);

    result.value = m[0] * sub0 - m[1] * sub1 + m[2] * sub2 - m[3] * sub3;
    result.status = 1;
    result.fuel_out = fuel_in - cost::MAT4_DET;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mat4_mul_identity() {
        // Identity matrix
        let identity: [f64; 16] = [
            1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0,
        ];

        let test_matrix: [f64; 16] = [
            1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0, 13.0, 14.0, 15.0, 16.0,
        ];

        let mut result = Mat4Result {
            m: [0.0; 16],
            fuel_out: 0,
            status: 0,
            _pad: [0; 7],
        };

        fold_mat4_mul(test_matrix.as_ptr(), identity.as_ptr(), 1000, &mut result);

        assert_eq!(result.status, 1);
        for i in 0..16 {
            assert!((result.m[i] - test_matrix[i]).abs() < 1e-10);
        }
    }

    #[test]
    fn test_mat4_mul_known_result() {
        // A = [[1,2],[3,4]] extended to 4x4
        let a: [f64; 16] = [
            1.0, 2.0, 0.0, 0.0, 3.0, 4.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0,
        ];

        // B = [[5,6],[7,8]] extended to 4x4
        let b: [f64; 16] = [
            5.0, 6.0, 0.0, 0.0, 7.0, 8.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0,
        ];

        // Expected: [[19,22],[43,50]] in top-left
        let mut result = Mat4Result {
            m: [0.0; 16],
            fuel_out: 0,
            status: 0,
            _pad: [0; 7],
        };

        fold_mat4_mul(a.as_ptr(), b.as_ptr(), 1000, &mut result);

        assert_eq!(result.status, 1);
        assert!((result.m[0] - 19.0).abs() < 1e-10); // 1*5 + 2*7
        assert!((result.m[1] - 22.0).abs() < 1e-10); // 1*6 + 2*8
        assert!((result.m[4] - 43.0).abs() < 1e-10); // 3*5 + 4*7
        assert!((result.m[5] - 50.0).abs() < 1e-10); // 3*6 + 4*8
    }

    #[test]
    fn test_mat4_determinant() {
        // Identity has det = 1
        let identity: [f64; 16] = [
            1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0,
        ];

        let mut result = F64Result {
            status: 0,
            _pad: [0; 7],
            value: 0.0,
            fuel_out: 0,
        };

        fold_mat4_determinant(identity.as_ptr(), 1000, &mut result);

        assert_eq!(result.status, 1);
        assert!((result.value - 1.0).abs() < 1e-10);
    }

    #[test]
    fn test_mat4_out_of_fuel() {
        let m: [f64; 16] = [0.0; 16];
        let mut result = Mat4Result {
            m: [0.0; 16],
            fuel_out: 0,
            status: 0,
            _pad: [0; 7],
        };

        fold_mat4_mul(m.as_ptr(), m.as_ptr(), 10, &mut result);

        assert_eq!(result.status, 2); // out of fuel
    }
}
