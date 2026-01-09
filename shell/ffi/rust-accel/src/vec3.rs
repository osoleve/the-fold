//! Vec3 type matching Scheme's (vec3 x y z) representation
//!
//! Uses #[repr(C)] for FFI safety.
//! All operations are pure and inlined for performance.

use std::ops::{Add, Sub, Mul, Neg};

/// 3D vector with f64 components
/// Matches Scheme's (vec3 x y z)
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Vec3 {
    pub x: f64,
    pub y: f64,
    pub z: f64,
}

impl Vec3 {
    /// Create a new Vec3
    #[inline]
    pub const fn new(x: f64, y: f64, z: f64) -> Self {
        Self { x, y, z }
    }

    /// Zero vector
    #[inline]
    pub const fn zero() -> Self {
        Self { x: 0.0, y: 0.0, z: 0.0 }
    }

    /// Dot product
    #[inline]
    pub fn dot(self, other: Self) -> f64 {
        self.x * other.x + self.y * other.y + self.z * other.z
    }

    /// Cross product
    #[inline]
    pub fn cross(self, other: Self) -> Self {
        Self {
            x: self.y * other.z - self.z * other.y,
            y: self.z * other.x - self.x * other.z,
            z: self.x * other.y - self.y * other.x,
        }
    }

    /// Squared magnitude (avoids sqrt)
    #[inline]
    pub fn magnitude_sq(self) -> f64 {
        self.dot(self)
    }

    /// Magnitude (length)
    #[inline]
    pub fn magnitude(self) -> f64 {
        self.magnitude_sq().sqrt()
    }

    /// Normalize to unit length
    #[inline]
    pub fn normalize(self) -> Self {
        let mag = self.magnitude();
        if mag > 1e-10 {
            self * (1.0 / mag)
        } else {
            Self::zero()
        }
    }

    /// Scale by scalar
    #[inline]
    pub fn scale(self, s: f64) -> Self {
        Self {
            x: self.x * s,
            y: self.y * s,
            z: self.z * s,
        }
    }

    /// Distance to another point
    #[inline]
    pub fn distance(self, other: Self) -> f64 {
        (self - other).magnitude()
    }

    /// Squared distance (avoids sqrt)
    #[inline]
    pub fn distance_sq(self, other: Self) -> f64 {
        (self - other).magnitude_sq()
    }

    /// Linear interpolation
    #[inline]
    pub fn lerp(self, other: Self, t: f64) -> Self {
        self + (other - self).scale(t)
    }

    /// Component-wise minimum
    #[inline]
    pub fn min(self, other: Self) -> Self {
        Self {
            x: self.x.min(other.x),
            y: self.y.min(other.y),
            z: self.z.min(other.z),
        }
    }

    /// Component-wise maximum
    #[inline]
    pub fn max(self, other: Self) -> Self {
        Self {
            x: self.x.max(other.x),
            y: self.y.max(other.y),
            z: self.z.max(other.z),
        }
    }

    /// Get component by index (0=x, 1=y, 2=z)
    #[inline]
    pub fn get(self, axis: usize) -> f64 {
        match axis {
            0 => self.x,
            1 => self.y,
            _ => self.z,
        }
    }
}

impl Add for Vec3 {
    type Output = Self;
    #[inline]
    fn add(self, other: Self) -> Self {
        Self {
            x: self.x + other.x,
            y: self.y + other.y,
            z: self.z + other.z,
        }
    }
}

impl Sub for Vec3 {
    type Output = Self;
    #[inline]
    fn sub(self, other: Self) -> Self {
        Self {
            x: self.x - other.x,
            y: self.y - other.y,
            z: self.z - other.z,
        }
    }
}

impl Mul<f64> for Vec3 {
    type Output = Self;
    #[inline]
    fn mul(self, s: f64) -> Self {
        self.scale(s)
    }
}

impl Neg for Vec3 {
    type Output = Self;
    #[inline]
    fn neg(self) -> Self {
        Self {
            x: -self.x,
            y: -self.y,
            z: -self.z,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_dot() {
        let a = Vec3::new(1.0, 2.0, 3.0);
        let b = Vec3::new(4.0, 5.0, 6.0);
        assert_eq!(a.dot(b), 32.0); // 1*4 + 2*5 + 3*6
    }

    #[test]
    fn test_cross() {
        let a = Vec3::new(1.0, 0.0, 0.0);
        let b = Vec3::new(0.0, 1.0, 0.0);
        let c = a.cross(b);
        assert_eq!(c.z, 1.0);
    }

    #[test]
    fn test_normalize() {
        let v = Vec3::new(3.0, 0.0, 4.0);
        let n = v.normalize();
        assert!((n.magnitude() - 1.0).abs() < 1e-10);
    }
}
