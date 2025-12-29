//! BigRational primitive operations for The Fold
//!
//! Provides high-performance arbitrary precision rational arithmetic
//! using the num-rational crate.
//!
//! These primitives are designed to be called from the evaluator
//! and complement the BigInt primitives.

use num_bigint::BigInt;
use num_rational::BigRational;
use num_traits::{One, Signed, Zero};
use std::cmp::Ordering;

/// Result type for BigRational operations that can fail
pub type BigRationalResult<T> = Result<T, BigRationalError>;

/// Errors that can occur in BigRational operations
#[derive(Debug, Clone, PartialEq)]
pub enum BigRationalError {
    DivisionByZero,
    ParseError(String),
}

impl std::fmt::Display for BigRationalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BigRationalError::DivisionByZero => write!(f, "division by zero"),
            BigRationalError::ParseError(s) => write!(f, "parse error: {}", s),
        }
    }
}

impl std::error::Error for BigRationalError {}

// ============================================================
// Construction
// ============================================================

/// Create BigRational from two BigInts (numerator/denominator)
/// Automatically reduces to lowest terms
pub fn bigrational_new(numer: BigInt, denom: BigInt) -> BigRationalResult<BigRational> {
    if denom.is_zero() {
        Err(BigRationalError::DivisionByZero)
    } else {
        Ok(BigRational::new(numer, denom))
    }
}

/// Create BigRational from a BigInt (as integer)
#[inline]
pub fn bigrational_from_bigint(n: BigInt) -> BigRational {
    BigRational::from(n)
}

/// Create BigRational from i64 (as integer)
#[inline]
pub fn bigrational_from_i64(n: i64) -> BigRational {
    BigRational::from(BigInt::from(n))
}

/// Create BigRational from two i64s (numerator/denominator)
pub fn bigrational_from_i64s(numer: i64, denom: i64) -> BigRationalResult<BigRational> {
    if denom == 0 {
        Err(BigRationalError::DivisionByZero)
    } else {
        Ok(BigRational::new(BigInt::from(numer), BigInt::from(denom)))
    }
}

/// Try to convert BigRational to f64
/// May lose precision for very large or precise rationals
#[inline]
pub fn bigrational_to_f64(r: &BigRational) -> f64 {
    use num_traits::ToPrimitive;
    // Compute as floating point division
    let numer = r.numer().to_f64().unwrap_or(f64::INFINITY);
    let denom = r.denom().to_f64().unwrap_or(f64::INFINITY);
    numer / denom
}

// ============================================================
// Accessors
// ============================================================

/// Get the numerator (always reduced)
#[inline]
pub fn bigrational_numer(r: &BigRational) -> BigInt {
    r.numer().clone()
}

/// Get the denominator (always reduced, always positive)
#[inline]
pub fn bigrational_denom(r: &BigRational) -> BigInt {
    r.denom().clone()
}

// ============================================================
// Predicates
// ============================================================

/// Check if BigRational is zero
#[inline]
pub fn bigrational_is_zero(r: &BigRational) -> bool {
    r.is_zero()
}

/// Check if BigRational is positive (> 0)
#[inline]
pub fn bigrational_is_positive(r: &BigRational) -> bool {
    r.is_positive()
}

/// Check if BigRational is negative (< 0)
#[inline]
pub fn bigrational_is_negative(r: &BigRational) -> bool {
    r.is_negative()
}

/// Check if BigRational is an integer (denominator = 1)
#[inline]
pub fn bigrational_is_integer(r: &BigRational) -> bool {
    r.is_integer()
}

// ============================================================
// Comparison
// ============================================================

/// Compare two BigRationals, returns -1, 0, or 1
#[inline]
pub fn bigrational_cmp(a: &BigRational, b: &BigRational) -> i32 {
    match a.cmp(b) {
        Ordering::Less => -1,
        Ordering::Equal => 0,
        Ordering::Greater => 1,
    }
}

/// Check equality
#[inline]
pub fn bigrational_eq(a: &BigRational, b: &BigRational) -> bool {
    a == b
}

/// Check less than
#[inline]
pub fn bigrational_lt(a: &BigRational, b: &BigRational) -> bool {
    a < b
}

/// Check less than or equal
#[inline]
pub fn bigrational_le(a: &BigRational, b: &BigRational) -> bool {
    a <= b
}

/// Check greater than
#[inline]
pub fn bigrational_gt(a: &BigRational, b: &BigRational) -> bool {
    a > b
}

/// Check greater than or equal
#[inline]
pub fn bigrational_ge(a: &BigRational, b: &BigRational) -> bool {
    a >= b
}

// ============================================================
// Arithmetic
// ============================================================

/// Add two BigRationals
#[inline]
pub fn bigrational_add(a: &BigRational, b: &BigRational) -> BigRational {
    a + b
}

/// Subtract two BigRationals
#[inline]
pub fn bigrational_sub(a: &BigRational, b: &BigRational) -> BigRational {
    a - b
}

/// Multiply two BigRationals
#[inline]
pub fn bigrational_mul(a: &BigRational, b: &BigRational) -> BigRational {
    a * b
}

/// Divide two BigRationals
pub fn bigrational_div(a: &BigRational, b: &BigRational) -> BigRationalResult<BigRational> {
    if b.is_zero() {
        Err(BigRationalError::DivisionByZero)
    } else {
        Ok(a / b)
    }
}

/// Negate a BigRational
#[inline]
pub fn bigrational_neg(r: &BigRational) -> BigRational {
    -r
}

/// Absolute value
#[inline]
pub fn bigrational_abs(r: &BigRational) -> BigRational {
    r.abs()
}

/// Reciprocal (1/r)
pub fn bigrational_recip(r: &BigRational) -> BigRationalResult<BigRational> {
    if r.is_zero() {
        Err(BigRationalError::DivisionByZero)
    } else {
        Ok(r.recip())
    }
}

/// Sign: -1, 0, or 1
#[inline]
pub fn bigrational_signum(r: &BigRational) -> i32 {
    if r.is_zero() {
        0
    } else if r.is_positive() {
        1
    } else {
        -1
    }
}

// ============================================================
// Rounding and Truncation
// ============================================================

/// Floor: largest integer <= r
#[inline]
pub fn bigrational_floor(r: &BigRational) -> BigInt {
    r.floor().to_integer()
}

/// Ceiling: smallest integer >= r
#[inline]
pub fn bigrational_ceil(r: &BigRational) -> BigInt {
    r.ceil().to_integer()
}

/// Truncate toward zero
#[inline]
pub fn bigrational_trunc(r: &BigRational) -> BigInt {
    r.trunc().to_integer()
}

/// Round to nearest integer (ties go to even)
#[inline]
pub fn bigrational_round(r: &BigRational) -> BigInt {
    r.round().to_integer()
}

/// Fractional part: r - trunc(r)
#[inline]
pub fn bigrational_fract(r: &BigRational) -> BigRational {
    r.fract()
}

// ============================================================
// Utilities
// ============================================================

/// Zero constant
#[inline]
pub fn bigrational_zero() -> BigRational {
    BigRational::zero()
}

/// One constant
#[inline]
pub fn bigrational_one() -> BigRational {
    BigRational::one()
}

/// Raise to integer power (can be negative)
pub fn bigrational_pow(base: &BigRational, exp: i32) -> BigRationalResult<BigRational> {
    if exp < 0 && base.is_zero() {
        Err(BigRationalError::DivisionByZero)
    } else {
        Ok(base.pow(exp))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_construction() {
        let r = bigrational_from_i64s(3, 4).unwrap();
        assert_eq!(bigrational_numer(&r), BigInt::from(3));
        assert_eq!(bigrational_denom(&r), BigInt::from(4));

        // Test reduction
        let r2 = bigrational_from_i64s(6, 8).unwrap();
        assert!(bigrational_eq(&r, &r2));

        // Test from integer
        let r3 = bigrational_from_i64(5);
        assert!(bigrational_is_integer(&r3));
        assert_eq!(bigrational_numer(&r3), BigInt::from(5));
        assert_eq!(bigrational_denom(&r3), BigInt::from(1));

        // Division by zero
        assert!(bigrational_from_i64s(1, 0).is_err());
    }

    #[test]
    fn test_arithmetic() {
        let half = bigrational_from_i64s(1, 2).unwrap();
        let third = bigrational_from_i64s(1, 3).unwrap();

        // 1/2 + 1/3 = 5/6
        let sum = bigrational_add(&half, &third);
        assert_eq!(bigrational_numer(&sum), BigInt::from(5));
        assert_eq!(bigrational_denom(&sum), BigInt::from(6));

        // 1/2 * 1/3 = 1/6
        let product = bigrational_mul(&half, &third);
        assert_eq!(bigrational_numer(&product), BigInt::from(1));
        assert_eq!(bigrational_denom(&product), BigInt::from(6));

        // 1/2 / 1/3 = 3/2
        let quotient = bigrational_div(&half, &third).unwrap();
        assert_eq!(bigrational_numer(&quotient), BigInt::from(3));
        assert_eq!(bigrational_denom(&quotient), BigInt::from(2));
    }

    #[test]
    fn test_comparison() {
        let half = bigrational_from_i64s(1, 2).unwrap();
        let third = bigrational_from_i64s(1, 3).unwrap();
        let also_half = bigrational_from_i64s(2, 4).unwrap();

        assert!(bigrational_gt(&half, &third));
        assert!(bigrational_lt(&third, &half));
        assert!(bigrational_eq(&half, &also_half));
        assert_eq!(bigrational_cmp(&half, &third), 1);
        assert_eq!(bigrational_cmp(&third, &half), -1);
        assert_eq!(bigrational_cmp(&half, &also_half), 0);
    }

    #[test]
    fn test_predicates() {
        let zero = bigrational_zero();
        let pos = bigrational_from_i64s(1, 2).unwrap();
        let neg = bigrational_from_i64s(-1, 2).unwrap();
        let int = bigrational_from_i64(5);

        assert!(bigrational_is_zero(&zero));
        assert!(!bigrational_is_zero(&pos));

        assert!(bigrational_is_positive(&pos));
        assert!(!bigrational_is_positive(&neg));
        assert!(!bigrational_is_positive(&zero));

        assert!(bigrational_is_negative(&neg));
        assert!(!bigrational_is_negative(&pos));

        assert!(bigrational_is_integer(&int));
        assert!(!bigrational_is_integer(&pos));
    }

    #[test]
    fn test_rounding() {
        let r = bigrational_from_i64s(7, 3).unwrap(); // 2.333...

        assert_eq!(bigrational_floor(&r), BigInt::from(2));
        assert_eq!(bigrational_ceil(&r), BigInt::from(3));
        assert_eq!(bigrational_trunc(&r), BigInt::from(2));
        assert_eq!(bigrational_round(&r), BigInt::from(2));

        // Negative
        let neg = bigrational_from_i64s(-7, 3).unwrap(); // -2.333...
        assert_eq!(bigrational_floor(&neg), BigInt::from(-3));
        assert_eq!(bigrational_ceil(&neg), BigInt::from(-2));
        assert_eq!(bigrational_trunc(&neg), BigInt::from(-2));
    }

    #[test]
    fn test_reciprocal() {
        let r = bigrational_from_i64s(3, 4).unwrap();
        let recip = bigrational_recip(&r).unwrap();
        assert_eq!(bigrational_numer(&recip), BigInt::from(4));
        assert_eq!(bigrational_denom(&recip), BigInt::from(3));

        // Reciprocal of zero
        let zero = bigrational_zero();
        assert!(bigrational_recip(&zero).is_err());
    }

    #[test]
    fn test_power() {
        let r = bigrational_from_i64s(2, 3).unwrap();

        // (2/3)^2 = 4/9
        let squared = bigrational_pow(&r, 2).unwrap();
        assert_eq!(bigrational_numer(&squared), BigInt::from(4));
        assert_eq!(bigrational_denom(&squared), BigInt::from(9));

        // (2/3)^-1 = 3/2
        let inv = bigrational_pow(&r, -1).unwrap();
        assert_eq!(bigrational_numer(&inv), BigInt::from(3));
        assert_eq!(bigrational_denom(&inv), BigInt::from(2));

        // (2/3)^0 = 1
        let one = bigrational_pow(&r, 0).unwrap();
        assert!(bigrational_is_integer(&one));
        assert_eq!(bigrational_numer(&one), BigInt::from(1));
    }

    #[test]
    fn test_to_f64() {
        let half = bigrational_from_i64s(1, 2).unwrap();
        assert!((bigrational_to_f64(&half) - 0.5).abs() < 1e-10);

        let third = bigrational_from_i64s(1, 3).unwrap();
        assert!((bigrational_to_f64(&third) - 0.333333333333).abs() < 1e-10);
    }

    #[test]
    fn test_large_rationals() {
        // Test with large numbers that would overflow i64
        let big_numer = BigInt::parse_bytes(b"123456789012345678901234567890", 10).unwrap();
        let big_denom = BigInt::parse_bytes(b"987654321098765432109876543210", 10).unwrap();

        let r = bigrational_new(big_numer.clone(), big_denom.clone()).unwrap();

        // Multiply and divide should be exact
        let doubled = bigrational_add(&r, &r);
        let halved = bigrational_div(&doubled, &bigrational_from_i64(2)).unwrap();
        assert!(bigrational_eq(&r, &halved));
    }
}
