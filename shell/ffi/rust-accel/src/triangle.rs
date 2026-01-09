//! Triangle type matching Scheme's (triangle3 p1 p2 p3) representation
//!
//! Uses #[repr(C)] for FFI safety.
//! Includes closest-point computation for BVH queries.

use crate::vec3::Vec3;

/// Triangle with 3 vertices
/// Matches Scheme's (triangle3 p1 p2 p3)
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct Triangle {
    pub p1: Vec3,
    pub p2: Vec3,
    pub p3: Vec3,
}

impl Triangle {
    /// Create a new triangle
    #[inline]
    pub const fn new(p1: Vec3, p2: Vec3, p3: Vec3) -> Self {
        Self { p1, p2, p3 }
    }

    /// Compute centroid (center of mass)
    #[inline]
    pub fn centroid(&self) -> Vec3 {
        (self.p1 + self.p2 + self.p3) * (1.0 / 3.0)
    }

    /// Compute normal (not normalized)
    #[inline]
    pub fn normal_unnormalized(&self) -> Vec3 {
        let e1 = self.p2 - self.p1;
        let e2 = self.p3 - self.p1;
        e1.cross(e2)
    }

    /// Compute normalized normal
    #[inline]
    pub fn normal(&self) -> Vec3 {
        self.normal_unnormalized().normalize()
    }

    /// Compute area
    #[inline]
    pub fn area(&self) -> f64 {
        self.normal_unnormalized().magnitude() * 0.5
    }

    /// Find closest point on triangle to given point
    /// Returns (closest_point, barycentric_coords)
    pub fn closest_point(&self, p: Vec3) -> (Vec3, f64) {
        // Edge vectors
        let ab = self.p2 - self.p1;
        let ac = self.p3 - self.p1;
        let ap = p - self.p1;

        // Precompute dot products
        let d1 = ab.dot(ap);
        let d2 = ac.dot(ap);

        // Check if P is in vertex region outside A
        if d1 <= 0.0 && d2 <= 0.0 {
            let dist = p.distance(self.p1);
            return (self.p1, dist);
        }

        // Check if P is in vertex region outside B
        let bp = p - self.p2;
        let d3 = ab.dot(bp);
        let d4 = ac.dot(bp);
        if d3 >= 0.0 && d4 <= d3 {
            let dist = p.distance(self.p2);
            return (self.p2, dist);
        }

        // Check if P is in edge region of AB
        let vc = d1 * d4 - d3 * d2;
        if vc <= 0.0 && d1 >= 0.0 && d3 <= 0.0 {
            let v = d1 / (d1 - d3);
            let closest = self.p1 + ab * v;
            let dist = p.distance(closest);
            return (closest, dist);
        }

        // Check if P is in vertex region outside C
        let cp = p - self.p3;
        let d5 = ab.dot(cp);
        let d6 = ac.dot(cp);
        if d6 >= 0.0 && d5 <= d6 {
            let dist = p.distance(self.p3);
            return (self.p3, dist);
        }

        // Check if P is in edge region of AC
        let vb = d5 * d2 - d1 * d6;
        if vb <= 0.0 && d2 >= 0.0 && d6 <= 0.0 {
            let w = d2 / (d2 - d6);
            let closest = self.p1 + ac * w;
            let dist = p.distance(closest);
            return (closest, dist);
        }

        // Check if P is in edge region of BC
        let va = d3 * d6 - d5 * d4;
        if va <= 0.0 && (d4 - d3) >= 0.0 && (d5 - d6) >= 0.0 {
            let w = (d4 - d3) / ((d4 - d3) + (d5 - d6));
            let closest = self.p2 + (self.p3 - self.p2) * w;
            let dist = p.distance(closest);
            return (closest, dist);
        }

        // P is inside the triangle
        let denom = 1.0 / (va + vb + vc);
        let v = vb * denom;
        let w = vc * denom;
        let closest = self.p1 + ab * v + ac * w;
        let dist = p.distance(closest);
        (closest, dist)
    }

    /// Ray-triangle intersection (Möller-Trumbore algorithm)
    /// Returns distance along ray, or None if no intersection
    pub fn intersect_ray(&self, origin: Vec3, direction: Vec3) -> Option<f64> {
        const EPSILON: f64 = 1e-10;

        let e1 = self.p2 - self.p1;
        let e2 = self.p3 - self.p1;
        let h = direction.cross(e2);
        let a = e1.dot(h);

        if a.abs() < EPSILON {
            return None; // Ray parallel to triangle
        }

        let f = 1.0 / a;
        let s = origin - self.p1;
        let u = f * s.dot(h);

        if !(0.0..=1.0).contains(&u) {
            return None;
        }

        let q = s.cross(e1);
        let v = f * direction.dot(q);

        if v < 0.0 || u + v > 1.0 {
            return None;
        }

        let t = f * e2.dot(q);

        if t > EPSILON {
            Some(t)
        } else {
            None // Intersection behind ray
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_centroid() {
        let tri = Triangle::new(
            Vec3::new(0.0, 0.0, 0.0),
            Vec3::new(3.0, 0.0, 0.0),
            Vec3::new(0.0, 3.0, 0.0),
        );
        let c = tri.centroid();
        assert!((c.x - 1.0).abs() < 1e-10);
        assert!((c.y - 1.0).abs() < 1e-10);
    }

    #[test]
    fn test_closest_point_inside() {
        let tri = Triangle::new(
            Vec3::new(0.0, 0.0, 0.0),
            Vec3::new(2.0, 0.0, 0.0),
            Vec3::new(1.0, 2.0, 0.0),
        );
        // Point directly above centroid
        let p = Vec3::new(1.0, 0.67, 1.0);
        let (closest, _) = tri.closest_point(p);
        assert!((closest.z).abs() < 1e-10); // Should be on triangle plane
    }

    #[test]
    fn test_ray_intersect() {
        let tri = Triangle::new(
            Vec3::new(-1.0, -1.0, 0.0),
            Vec3::new(1.0, -1.0, 0.0),
            Vec3::new(0.0, 1.0, 0.0),
        );
        // Ray from above, pointing down
        let t = tri.intersect_ray(Vec3::new(0.0, 0.0, 1.0), Vec3::new(0.0, 0.0, -1.0));
        assert!(t.is_some());
        assert!((t.unwrap() - 1.0).abs() < 1e-10);
    }
}
