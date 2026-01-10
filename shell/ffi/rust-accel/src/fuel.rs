//! Fuel accounting for Rust accelerators
//!
//! Mirrors the Scheme fuel system:
//! - Each operation has a defined cost
//! - Fuel is checked before expensive operations
//! - Returns status=2 when fuel exhausted

/// Fuel cost constants
/// These should match the values in the accelerator manifest
pub mod costs {
    /// Cost per BVH node visited
    pub const NODE_VISIT: u64 = 2;
    /// Cost per AABB distance test
    pub const AABB_TEST: u64 = 3;
    /// Cost per triangle closest-point test
    pub const TRIANGLE_TEST: u64 = 10;
    /// Cost per triangle ray intersection test
    pub const TRIANGLE_RAY: u64 = 8;
    /// Base cost for any query
    pub const BASE_QUERY: u64 = 5;
}

/// Result status codes
/// These are written to the status field of result structs
pub mod status {
    /// No intersection found (miss)
    pub const MISS: u8 = 0;
    /// Intersection found (hit)
    pub const HIT: u8 = 1;
    /// Ran out of fuel before completion
    pub const OUT_OF_FUEL: u8 = 2;
}

/// Check if we have enough fuel, deduct if so
/// Returns true if operation can proceed
#[inline]
pub fn deduct(fuel: &mut u64, cost: u64) -> bool {
    if *fuel >= cost {
        *fuel -= cost;
        true
    } else {
        false
    }
}

/// Check fuel without deducting (for lookahead)
#[inline]
pub fn has_fuel(fuel: u64, cost: u64) -> bool {
    fuel >= cost
}

/// Fuel-tracked result type
/// Carries both the result value and remaining fuel
#[derive(Debug, Clone, Copy)]
pub enum FuelResult<T> {
    /// Operation completed successfully
    Ok(T, u64),
    /// Operation would have succeeded but no match found
    Miss(u64),
    /// Ran out of fuel before completion
    OutOfFuel,
}

impl<T> FuelResult<T> {
    /// Get remaining fuel (0 if out of fuel)
    pub fn fuel(&self) -> u64 {
        match self {
            FuelResult::Ok(_, fuel) => *fuel,
            FuelResult::Miss(fuel) => *fuel,
            FuelResult::OutOfFuel => 0,
        }
    }

    /// Get status code for FFI
    pub fn status(&self) -> u8 {
        match self {
            FuelResult::Ok(_, _) => status::HIT,
            FuelResult::Miss(_) => status::MISS,
            FuelResult::OutOfFuel => status::OUT_OF_FUEL,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_deduct_success() {
        let mut fuel = 100u64;
        assert!(deduct(&mut fuel, 10));
        assert_eq!(fuel, 90);
    }

    #[test]
    fn test_deduct_fail() {
        let mut fuel = 5u64;
        assert!(!deduct(&mut fuel, 10));
        assert_eq!(fuel, 5); // unchanged
    }

    #[test]
    fn test_fuel_result_status() {
        let ok: FuelResult<i32> = FuelResult::Ok(42, 50);
        assert_eq!(ok.status(), status::HIT);

        let miss: FuelResult<i32> = FuelResult::Miss(50);
        assert_eq!(miss.status(), status::MISS);

        let oof: FuelResult<i32> = FuelResult::OutOfFuel;
        assert_eq!(oof.status(), status::OUT_OF_FUEL);
    }
}
