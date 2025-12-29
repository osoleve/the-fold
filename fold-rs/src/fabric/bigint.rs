//! BigInt primitive operations for The Fold
//!
//! Provides high-performance arbitrary precision integer arithmetic
//! using the num-bigint crate.
//!
//! These primitives are designed to be called from the evaluator
//! and replace the slow Scheme-based implementation.

use num_bigint::BigInt;
use num_integer::Integer;
use num_traits::{One, Signed, Zero};
use std::cmp::Ordering;

/// Result type for BigInt operations that can fail
pub type BigIntResult<T> = Result<T, BigIntError>;

/// Errors that can occur in BigInt operations
#[derive(Debug, Clone, PartialEq)]
pub enum BigIntError {
    DivisionByZero,
    ParseError(String),
    Overflow,
}

impl std::fmt::Display for BigIntError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BigIntError::DivisionByZero => write!(f, "division by zero"),
            BigIntError::ParseError(s) => write!(f, "parse error: {}", s),
            BigIntError::Overflow => write!(f, "integer overflow"),
        }
    }
}

impl std::error::Error for BigIntError {}

// ============================================================
// Construction
// ============================================================

/// Create BigInt from i64
#[inline]
pub fn bigint_from_i64(n: i64) -> BigInt {
    BigInt::from(n)
}

/// Create BigInt from u64
#[inline]
pub fn bigint_from_u64(n: u64) -> BigInt {
    BigInt::from(n)
}

/// Try to convert BigInt to i64, returns None if out of range
#[inline]
pub fn bigint_to_i64(n: &BigInt) -> Option<i64> {
    use num_traits::ToPrimitive;
    n.to_i64()
}

/// Parse BigInt from decimal string
pub fn bigint_from_string(s: &str) -> BigIntResult<BigInt> {
    s.parse::<BigInt>()
        .map_err(|e| BigIntError::ParseError(e.to_string()))
}

/// Convert BigInt to decimal string
#[inline]
pub fn bigint_to_string(n: &BigInt) -> String {
    n.to_string()
}

// ============================================================
// Predicates
// ============================================================

/// Check if BigInt is zero
#[inline]
pub fn bigint_is_zero(n: &BigInt) -> bool {
    n.is_zero()
}

/// Check if BigInt is positive (> 0)
#[inline]
pub fn bigint_is_positive(n: &BigInt) -> bool {
    n.is_positive()
}

/// Check if BigInt is negative (< 0)
#[inline]
pub fn bigint_is_negative(n: &BigInt) -> bool {
    n.is_negative()
}

// ============================================================
// Comparison
// ============================================================

/// Compare two BigInts, returns -1, 0, or 1
#[inline]
pub fn bigint_cmp(a: &BigInt, b: &BigInt) -> i32 {
    match a.cmp(b) {
        Ordering::Less => -1,
        Ordering::Equal => 0,
        Ordering::Greater => 1,
    }
}

/// Check equality
#[inline]
pub fn bigint_eq(a: &BigInt, b: &BigInt) -> bool {
    a == b
}

/// Check less than
#[inline]
pub fn bigint_lt(a: &BigInt, b: &BigInt) -> bool {
    a < b
}

/// Check less than or equal
#[inline]
pub fn bigint_le(a: &BigInt, b: &BigInt) -> bool {
    a <= b
}

/// Check greater than
#[inline]
pub fn bigint_gt(a: &BigInt, b: &BigInt) -> bool {
    a > b
}

/// Check greater than or equal
#[inline]
pub fn bigint_ge(a: &BigInt, b: &BigInt) -> bool {
    a >= b
}

// ============================================================
// Arithmetic
// ============================================================

/// Add two BigInts
#[inline]
pub fn bigint_add(a: &BigInt, b: &BigInt) -> BigInt {
    a + b
}

/// Subtract two BigInts
#[inline]
pub fn bigint_sub(a: &BigInt, b: &BigInt) -> BigInt {
    a - b
}

/// Multiply two BigInts
#[inline]
pub fn bigint_mul(a: &BigInt, b: &BigInt) -> BigInt {
    a * b
}

/// Negate a BigInt
#[inline]
pub fn bigint_neg(n: &BigInt) -> BigInt {
    -n
}

/// Absolute value
#[inline]
pub fn bigint_abs(n: &BigInt) -> BigInt {
    n.abs()
}

/// Sign: -1, 0, or 1
#[inline]
pub fn bigint_signum(n: &BigInt) -> i32 {
    if n.is_zero() {
        0
    } else if n.is_positive() {
        1
    } else {
        -1
    }
}

// ============================================================
// Division
// ============================================================

/// Integer division (quotient), rounds toward zero
pub fn bigint_quotient(a: &BigInt, b: &BigInt) -> BigIntResult<BigInt> {
    if b.is_zero() {
        Err(BigIntError::DivisionByZero)
    } else {
        Ok(a / b)
    }
}

/// Remainder after division
pub fn bigint_remainder(a: &BigInt, b: &BigInt) -> BigIntResult<BigInt> {
    if b.is_zero() {
        Err(BigIntError::DivisionByZero)
    } else {
        Ok(a % b)
    }
}

/// Quotient and remainder together (more efficient than separate calls)
pub fn bigint_divmod(a: &BigInt, b: &BigInt) -> BigIntResult<(BigInt, BigInt)> {
    if b.is_zero() {
        Err(BigIntError::DivisionByZero)
    } else {
        Ok(a.div_rem(b))
    }
}

/// Floor division (rounds toward negative infinity)
pub fn bigint_div_floor(a: &BigInt, b: &BigInt) -> BigIntResult<BigInt> {
    if b.is_zero() {
        Err(BigIntError::DivisionByZero)
    } else {
        Ok(a.div_floor(b))
    }
}

/// Modulo (always non-negative if divisor is positive)
pub fn bigint_mod_floor(a: &BigInt, b: &BigInt) -> BigIntResult<BigInt> {
    if b.is_zero() {
        Err(BigIntError::DivisionByZero)
    } else {
        Ok(a.mod_floor(b))
    }
}

// ============================================================
// GCD and LCM
// ============================================================

/// Greatest common divisor (always non-negative)
#[inline]
pub fn bigint_gcd(a: &BigInt, b: &BigInt) -> BigInt {
    a.gcd(b)
}

/// Least common multiple (always non-negative)
#[inline]
pub fn bigint_lcm(a: &BigInt, b: &BigInt) -> BigInt {
    a.lcm(b)
}

/// Extended GCD: returns (gcd, x, y) where a*x + b*y = gcd
pub fn bigint_extended_gcd(a: &BigInt, b: &BigInt) -> (BigInt, BigInt, BigInt) {
    let result = a.extended_gcd(b);
    (result.gcd, result.x, result.y)
}

// ============================================================
// Bit Operations
// ============================================================

/// Bitwise AND
#[inline]
pub fn bigint_and(a: &BigInt, b: &BigInt) -> BigInt {
    a & b
}

/// Bitwise OR
#[inline]
pub fn bigint_or(a: &BigInt, b: &BigInt) -> BigInt {
    a | b
}

/// Bitwise XOR
#[inline]
pub fn bigint_xor(a: &BigInt, b: &BigInt) -> BigInt {
    a ^ b
}

/// Left shift
#[inline]
pub fn bigint_shl(n: &BigInt, shift: u32) -> BigInt {
    n << shift
}

/// Right shift (arithmetic)
#[inline]
pub fn bigint_shr(n: &BigInt, shift: u32) -> BigInt {
    n >> shift
}

/// Number of bits needed to represent the absolute value
#[inline]
pub fn bigint_bits(n: &BigInt) -> u64 {
    n.bits()
}

// ============================================================
// Exponentiation
// ============================================================

/// Raise to non-negative integer power
pub fn bigint_pow(base: &BigInt, exp: u32) -> BigInt {
    base.pow(exp)
}

/// Modular exponentiation: (base^exp) mod modulus
/// Returns None if modulus is zero
pub fn bigint_modpow(base: &BigInt, exp: &BigInt, modulus: &BigInt) -> BigIntResult<BigInt> {
    if modulus.is_zero() {
        Err(BigIntError::DivisionByZero)
    } else {
        Ok(base.modpow(exp, modulus))
    }
}

// ============================================================
// Utilities
// ============================================================

/// Zero constant
#[inline]
pub fn bigint_zero() -> BigInt {
    BigInt::zero()
}

/// One constant
#[inline]
pub fn bigint_one() -> BigInt {
    BigInt::one()
}

/// Check if a BigInt fits in i64
#[inline]
pub fn bigint_fits_i64(n: &BigInt) -> bool {
    bigint_to_i64(n).is_some()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_construction() {
        let n = bigint_from_i64(42);
        assert_eq!(bigint_to_i64(&n), Some(42));

        let big = bigint_from_string("123456789012345678901234567890").unwrap();
        assert!(bigint_to_i64(&big).is_none());
        assert_eq!(bigint_to_string(&big), "123456789012345678901234567890");
    }

    #[test]
    fn test_arithmetic() {
        let a = bigint_from_i64(100);
        let b = bigint_from_i64(42);

        assert_eq!(bigint_to_i64(&bigint_add(&a, &b)), Some(142));
        assert_eq!(bigint_to_i64(&bigint_sub(&a, &b)), Some(58));
        assert_eq!(bigint_to_i64(&bigint_mul(&a, &b)), Some(4200));
        assert_eq!(bigint_to_i64(&bigint_neg(&a)), Some(-100));
    }

    #[test]
    fn test_division() {
        let a = bigint_from_i64(100);
        let b = bigint_from_i64(7);

        let (q, r) = bigint_divmod(&a, &b).unwrap();
        assert_eq!(bigint_to_i64(&q), Some(14));
        assert_eq!(bigint_to_i64(&r), Some(2));

        // Division by zero
        let zero = bigint_zero();
        assert!(bigint_divmod(&a, &zero).is_err());
    }

    #[test]
    fn test_gcd_lcm() {
        let a = bigint_from_i64(48);
        let b = bigint_from_i64(18);

        assert_eq!(bigint_to_i64(&bigint_gcd(&a, &b)), Some(6));
        assert_eq!(bigint_to_i64(&bigint_lcm(&a, &b)), Some(144));
    }

    #[test]
    fn test_comparison() {
        let a = bigint_from_i64(100);
        let b = bigint_from_i64(42);
        let c = bigint_from_i64(100);

        assert_eq!(bigint_cmp(&a, &b), 1);
        assert_eq!(bigint_cmp(&b, &a), -1);
        assert_eq!(bigint_cmp(&a, &c), 0);

        assert!(bigint_gt(&a, &b));
        assert!(bigint_lt(&b, &a));
        assert!(bigint_eq(&a, &c));
    }

    #[test]
    fn test_large_numbers() {
        // Test multiplication that would overflow i64
        let a = bigint_from_string("999999999999999999999999999999").unwrap();
        let b = bigint_from_string("999999999999999999999999999999").unwrap();
        let product = bigint_mul(&a, &b);

        // Verify the result is correct
        assert_eq!(
            bigint_to_string(&product),
            "999999999999999999999999999998000000000000000000000000000001"
        );
    }

    #[test]
    fn test_division_performance() {
        // This would hang with the Scheme repeated-subtraction algorithm
        let a = bigint_from_string("10000000000000000000000000000000000000000").unwrap();
        let b = bigint_from_i64(7);

        let (q, r) = bigint_divmod(&a, &b).unwrap();

        // Verify correctness: a = b * q + r
        let reconstructed = bigint_add(&bigint_mul(&b, &q), &r);
        assert!(bigint_eq(&a, &reconstructed));
    }

    #[test]
    fn test_power() {
        let base = bigint_from_i64(2);
        let result = bigint_pow(&base, 100);

        // 2^100 is a specific known value
        assert_eq!(bigint_bits(&result), 101); // 101 bits needed
    }

    #[test]
    fn test_modpow() {
        let base = bigint_from_i64(3);
        let exp = bigint_from_i64(100);
        let modulus = bigint_from_i64(1000000007); // Common prime for modular arithmetic

        let result = bigint_modpow(&base, &exp, &modulus).unwrap();
        assert!(bigint_lt(&result, &modulus));
    }
}
