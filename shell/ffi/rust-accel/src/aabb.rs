//! AABB (Axis-Aligned Bounding Box) for BVH acceleration
//!
//! Uses #[repr(C)] for FFI safety.
//! Provides intersection tests for ray traversal.

use crate::triangle::Triangle;
use crate::vec3::Vec3;

/// Axis-Aligned Bounding Box
/// Matches Scheme's (aabb min max)
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct AABB {
    pub min: Vec3,
    pub max: Vec3,
}

impl AABB {
    /// Create AABB from min/max corners
    #[inline]
    pub const fn new(min: Vec3, max: Vec3) -> Self {
        Self { min, max }
    }

    /// Create empty AABB (for accumulation)
    #[inline]
    pub fn empty() -> Self {
        Self {
            min: Vec3::new(f64::INFINITY, f64::INFINITY, f64::INFINITY),
            max: Vec3::new(f64::NEG_INFINITY, f64::NEG_INFINITY, f64::NEG_INFINITY),
        }
    }

    /// Create AABB containing a single point
    #[inline]
    pub fn from_point(p: Vec3) -> Self {
        Self { min: p, max: p }
    }

    /// Create AABB from a triangle
    #[inline]
    pub fn from_triangle(tri: &Triangle) -> Self {
        let min = tri.p1.min(tri.p2).min(tri.p3);
        let max = tri.p1.max(tri.p2).max(tri.p3);
        Self { min, max }
    }

    /// Union with another AABB
    #[inline]
    pub fn union(self, other: Self) -> Self {
        Self {
            min: self.min.min(other.min),
            max: self.max.max(other.max),
        }
    }

    /// Expand to include a point
    #[inline]
    pub fn expand_point(self, p: Vec3) -> Self {
        Self {
            min: self.min.min(p),
            max: self.max.max(p),
        }
    }

    /// Get center point
    #[inline]
    pub fn center(&self) -> Vec3 {
        (self.min + self.max) * 0.5
    }

    /// Get extents (half-widths)
    #[inline]
    pub fn extents(&self) -> Vec3 {
        (self.max - self.min) * 0.5
    }

    /// Get longest axis (0=x, 1=y, 2=z)
    #[inline]
    pub fn longest_axis(&self) -> usize {
        let d = self.max - self.min;
        if d.x >= d.y && d.x >= d.z {
            0
        } else if d.y >= d.z {
            1
        } else {
            2
        }
    }

    /// Check if point is inside AABB
    #[inline]
    pub fn contains(&self, p: Vec3) -> bool {
        p.x >= self.min.x
            && p.x <= self.max.x
            && p.y >= self.min.y
            && p.y <= self.max.y
            && p.z >= self.min.z
            && p.z <= self.max.z
    }

    /// Closest point on AABB surface to given point
    #[inline]
    pub fn closest_point(&self, p: Vec3) -> Vec3 {
        Vec3::new(
            p.x.clamp(self.min.x, self.max.x),
            p.y.clamp(self.min.y, self.max.y),
            p.z.clamp(self.min.z, self.max.z),
        )
    }

    /// Distance from point to AABB
    #[inline]
    pub fn distance_to_point(&self, p: Vec3) -> f64 {
        let closest = self.closest_point(p);
        p.distance(closest)
    }

    /// Ray-AABB intersection test
    /// Returns (tmin, tmax) if intersects, None otherwise
    pub fn intersect_ray(&self, origin: Vec3, direction: Vec3) -> Option<(f64, f64)> {
        let inv_dir = Vec3::new(
            if direction.x.abs() > 1e-10 {
                1.0 / direction.x
            } else {
                f64::INFINITY
            },
            if direction.y.abs() > 1e-10 {
                1.0 / direction.y
            } else {
                f64::INFINITY
            },
            if direction.z.abs() > 1e-10 {
                1.0 / direction.z
            } else {
                f64::INFINITY
            },
        );

        let t1 = (self.min.x - origin.x) * inv_dir.x;
        let t2 = (self.max.x - origin.x) * inv_dir.x;
        let t3 = (self.min.y - origin.y) * inv_dir.y;
        let t4 = (self.max.y - origin.y) * inv_dir.y;
        let t5 = (self.min.z - origin.z) * inv_dir.z;
        let t6 = (self.max.z - origin.z) * inv_dir.z;

        let tmin = t1.min(t2).max(t3.min(t4)).max(t5.min(t6));
        let tmax = t1.max(t2).min(t3.max(t4)).min(t5.max(t6));

        if tmax >= tmin && tmax >= 0.0 {
            Some((tmin.max(0.0), tmax))
        } else {
            None
        }
    }

    /// Check if ray intersects AABB (fast version, no distances)
    #[inline]
    pub fn ray_hits(&self, origin: Vec3, inv_dir: Vec3) -> bool {
        let t1 = (self.min.x - origin.x) * inv_dir.x;
        let t2 = (self.max.x - origin.x) * inv_dir.x;
        let t3 = (self.min.y - origin.y) * inv_dir.y;
        let t4 = (self.max.y - origin.y) * inv_dir.y;
        let t5 = (self.min.z - origin.z) * inv_dir.z;
        let t6 = (self.max.z - origin.z) * inv_dir.z;

        let tmin = t1.min(t2).max(t3.min(t4)).max(t5.min(t6));
        let tmax = t1.max(t2).min(t3.max(t4)).min(t5.max(t6));

        tmax >= tmin && tmax >= 0.0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_contains() {
        let aabb = AABB::new(Vec3::zero(), Vec3::new(1.0, 1.0, 1.0));
        assert!(aabb.contains(Vec3::new(0.5, 0.5, 0.5)));
        assert!(!aabb.contains(Vec3::new(1.5, 0.5, 0.5)));
    }

    #[test]
    fn test_ray_intersect() {
        let aabb = AABB::new(Vec3::new(-1.0, -1.0, -1.0), Vec3::new(1.0, 1.0, 1.0));

        // Ray hitting box
        let hit = aabb.intersect_ray(Vec3::new(0.0, 0.0, 5.0), Vec3::new(0.0, 0.0, -1.0));
        assert!(hit.is_some());

        // Ray missing box
        let miss = aabb.intersect_ray(Vec3::new(5.0, 5.0, 5.0), Vec3::new(0.0, 0.0, -1.0));
        assert!(miss.is_none());
    }

    #[test]
    fn test_closest_point() {
        let aabb = AABB::new(Vec3::zero(), Vec3::new(1.0, 1.0, 1.0));

        // Point outside
        let closest = aabb.closest_point(Vec3::new(2.0, 0.5, 0.5));
        assert!((closest.x - 1.0).abs() < 1e-10);

        // Point inside
        let closest = aabb.closest_point(Vec3::new(0.5, 0.5, 0.5));
        assert!((closest.x - 0.5).abs() < 1e-10);
    }
}
