//! Raymarching acceleration for mesh SDFs
//!
//! Implements sphere tracing for mesh SDFs with BVH acceleration.
//! Moves the entire raymarching loop to Rust for maximum performance.

use crate::bvh::{BVHHandle, BVHNode};
use crate::fuel::{self, costs};
use crate::triangle::Triangle;
use crate::vec3::Vec3;

/// Raymarching costs
pub mod raymarch_costs {
    pub const BASE: u64 = 10;
    pub const NORMAL_COMPUTE: u64 = 20; // Base cost for normal (6 BVH queries tracked separately)
}

/// Result of mesh raymarching query
/// Fields ordered for natural alignment, explicit padding for safety
#[repr(C)]
pub struct RaymarchResult {
    // f64 fields first (8-byte aligned)
    pub px: f64,
    pub py: f64,
    pub pz: f64,
    pub nx: f64,
    pub ny: f64,
    pub nz: f64,
    pub t: f64,
    pub fuel_out: u64,
    // u32 fields
    pub steps: u32,
    pub triangle_index: u32,
    // u8 fields with explicit padding
    pub status: u8,
    pub _pad: [u8; 7],
}

impl Default for RaymarchResult {
    fn default() -> Self {
        RaymarchResult {
            px: 0.0,
            py: 0.0,
            pz: 0.0,
            nx: 0.0,
            ny: 0.0,
            nz: 0.0,
            t: 0.0,
            fuel_out: 0,
            steps: 0,
            triangle_index: 0,
            status: 0,
            _pad: [0; 7],
        }
    }
}

/// Result of normal computation
#[repr(C)]
pub struct NormalResult {
    pub nx: f64,
    pub ny: f64,
    pub nz: f64,
    pub fuel_out: u64,
    pub status: u8,
    pub _pad: [u8; 7],
}

impl Default for NormalResult {
    fn default() -> Self {
        NormalResult {
            nx: 0.0,
            ny: 0.0,
            nz: 0.0,
            fuel_out: 0,
            status: 0,
            _pad: [0; 7],
        }
    }
}

/// Result of closest point query with triangle reference
#[derive(Clone, Copy)]
pub struct ClosestPointWithTri {
    pub point: Vec3,
    pub distance: f64,
    pub triangle: Triangle, // Store full triangle with embedded id
}

/// Find closest point on BVH surface with triangle.
/// Uses explicit stack instead of recursion to prevent stack overflow on deep BVHs.
///
/// Fuel is tracked solely via `&mut u64`. Returns None on fuel exhaustion,
/// Some(None) on miss, Some(Some(hit)) on success.
fn bvh_closest_point_with_index(
    handle: &BVHHandle,
    point: Vec3,
    fuel: &mut u64,
) -> Option<Option<ClosestPointWithTri>> {
    // Base cost for query
    if !fuel::deduct(fuel, costs::BASE_QUERY) {
        return None; // out of fuel
    }

    let mut best: Option<ClosestPointWithTri> = None;

    // Explicit stack for iterative traversal (prevents stack overflow on deep BVHs)
    let mut stack: Vec<&BVHNode> = Vec::with_capacity(64);
    stack.push(&handle.root);

    while let Some(node) = stack.pop() {
        // Cost to visit this node
        if !fuel::deduct(fuel, costs::NODE_VISIT) {
            return None;
        }

        // Cost to test AABB
        if !fuel::deduct(fuel, costs::AABB_TEST) {
            return None;
        }

        // Early exit if AABB can't improve result
        let dist_to_box = node.bbox().distance_to_point(point);
        if let Some(ref best_hit) = best {
            if dist_to_box >= best_hit.distance {
                continue; // Skip this subtree
            }
        }

        match node {
            BVHNode::Leaf { triangles, .. } => {
                for tri in triangles.iter() {
                    if !fuel::deduct(fuel, costs::TRIANGLE_TEST) {
                        return None;
                    }

                    let (closest, dist) = tri.closest_point(point);
                    let should_update = match best {
                        Some(ref b) => dist < b.distance,
                        None => true,
                    };

                    if should_update {
                        best = Some(ClosestPointWithTri {
                            point: closest,
                            distance: dist,
                            triangle: *tri,
                        });
                    }
                }
            }
            BVHNode::Internal { left, right, .. } => {
                // Push children in reverse order so closer child is processed first
                let left_dist = left.bbox().distance_to_point(point);
                let right_dist = right.bbox().distance_to_point(point);

                if left_dist <= right_dist {
                    stack.push(right);
                    stack.push(left);
                } else {
                    stack.push(left);
                    stack.push(right);
                }
            }
        }
    }

    Some(best)
}

/// Compute signed distance from point to mesh surface.
/// Uses standard convention: positive = outside, negative = inside.
/// Fuel tracked solely via `&mut u64`. Returns None on fuel exhaustion.
fn mesh_sdf(handle: &BVHHandle, point: Vec3, fuel: &mut u64) -> Option<(f64, u32)> {
    match bvh_closest_point_with_index(handle, point, fuel)? {
        Some(hit) => {
            let normal = hit.triangle.normal();
            let to_point = point - hit.point;
            let dot = normal.dot(to_point);

            // Standard SDF convention: positive outside, negative inside
            let signed_dist = if dot >= 0.0 {
                hit.distance
            } else {
                -hit.distance
            };

            Some((signed_dist, hit.triangle.id))
        }
        None => Some((1e10, 0)), // No triangles in BVH
    }
}

/// Core raymarching algorithm (sphere tracing)
pub fn raymarch_mesh(
    handle: &BVHHandle,
    origin: Vec3,
    direction: Vec3,
    max_steps: u32,
    max_distance: f64,
    threshold: f64,
    mut fuel: u64,
) -> (RaymarchResult, u64) {
    let mut result = RaymarchResult::default();

    // Base cost for raymarching
    if !fuel::deduct(&mut fuel, raymarch_costs::BASE) {
        result.status = 2; // out of fuel
        return (result, fuel);
    }

    let dir = direction.normalize();
    let mut t = 0.0;
    let mut steps = 0u32;

    while steps < max_steps && t < max_distance {
        let point = origin + dir * t;

        // Query SDF (deducts fuel via &mut fuel)
        match mesh_sdf(handle, point, &mut fuel) {
            Some((dist, tri_idx)) => {
                // Use abs(dist) to handle rays starting inside mesh
                let step = dist.abs();

                if step < threshold {
                    // Hit surface!
                    result.status = 1;
                    result.px = point.x;
                    result.py = point.y;
                    result.pz = point.z;
                    result.t = t;
                    result.steps = steps;
                    result.triangle_index = tri_idx;

                    // Compute normal at hit point
                    match compute_normal(handle, point, &mut fuel) {
                        Some(normal) => {
                            result.nx = normal.x;
                            result.ny = normal.y;
                            result.nz = normal.z;
                        }
                        None => {
                            // Out of fuel during normal computation
                            result.status = 2;
                        }
                    }

                    result.fuel_out = fuel;
                    return (result, fuel);
                }

                t += step;
                steps += 1;
            }
            None => {
                // Out of fuel
                result.status = 2;
                result.steps = steps;
                result.fuel_out = 0;
                return (result, 0);
            }
        }
    }

    // No hit (miss)
    result.status = 0;
    result.steps = steps;
    result.fuel_out = fuel;
    (result, fuel)
}

/// Compute surface normal using finite differences (central differences)
/// Requires 6 SDF queries
fn compute_normal(handle: &BVHHandle, point: Vec3, fuel: &mut u64) -> Option<Vec3> {
    // Base cost for normal computation
    if !fuel::deduct(fuel, raymarch_costs::NORMAL_COMPUTE) {
        return None;
    }

    let eps = 0.001;

    // +X and -X
    let (dx_pos, _) = mesh_sdf(handle, point + Vec3::new(eps, 0.0, 0.0), fuel)?;
    let (dx_neg, _) = mesh_sdf(handle, point - Vec3::new(eps, 0.0, 0.0), fuel)?;

    // +Y and -Y
    let (dy_pos, _) = mesh_sdf(handle, point + Vec3::new(0.0, eps, 0.0), fuel)?;
    let (dy_neg, _) = mesh_sdf(handle, point - Vec3::new(0.0, eps, 0.0), fuel)?;

    // +Z and -Z
    let (dz_pos, _) = mesh_sdf(handle, point + Vec3::new(0.0, 0.0, eps), fuel)?;
    let (dz_neg, _) = mesh_sdf(handle, point - Vec3::new(0.0, 0.0, eps), fuel)?;

    let gradient = Vec3::new(dx_pos - dx_neg, dy_pos - dy_neg, dz_pos - dz_neg);

    Some(gradient.normalize())
}

/// Compute mesh SDF normal at a point (standalone function)
pub fn mesh_sdf_normal(handle: &BVHHandle, point: Vec3, mut fuel: u64) -> (NormalResult, u64) {
    let mut result = NormalResult::default();

    match compute_normal(handle, point, &mut fuel) {
        Some(normal) => {
            result.status = 1;
            result.nx = normal.x;
            result.ny = normal.y;
            result.nz = normal.z;
            result.fuel_out = fuel;
        }
        None => {
            result.status = 2; // out of fuel
            result.fuel_out = 0;
        }
    }

    (result, fuel)
}

// ====
// FFI Exports
// ====

/// Complete mesh raymarching in a single FFI call
#[no_mangle]
pub extern "C" fn fold_raymarch_mesh(
    handle: *mut BVHHandle,
    // Ray
    ox: f64,
    oy: f64,
    oz: f64,
    dx: f64,
    dy: f64,
    dz: f64,
    // Params
    max_steps: u32,
    max_distance: f64,
    threshold: f64,
    // Fuel
    fuel_in: u64,
    out: *mut RaymarchResult,
) {
    // Safety: Initialize output on null pointer
    if out.is_null() {
        return;
    }

    let result_ptr = unsafe { &mut *out };
    *result_ptr = RaymarchResult::default();

    if handle.is_null() {
        result_ptr.status = 0; // miss
        return;
    }

    let bvh = unsafe { &*handle };
    let origin = Vec3::new(ox, oy, oz);
    let direction = Vec3::new(dx, dy, dz);

    let (result, _) = raymarch_mesh(
        bvh,
        origin,
        direction,
        max_steps,
        max_distance,
        threshold,
        fuel_in,
    );

    *result_ptr = result;
}

/// Compute mesh SDF normal at a point
#[no_mangle]
pub extern "C" fn fold_mesh_sdf_normal(
    handle: *mut BVHHandle,
    px: f64,
    py: f64,
    pz: f64,
    fuel_in: u64,
    out: *mut NormalResult,
) {
    // Safety: Initialize output on null pointer
    if out.is_null() {
        return;
    }

    let result_ptr = unsafe { &mut *out };
    *result_ptr = NormalResult::default();

    if handle.is_null() {
        result_ptr.status = 0; // fail
        return;
    }

    let bvh = unsafe { &*handle };
    let point = Vec3::new(px, py, pz);

    let (result, _) = mesh_sdf_normal(bvh, point, fuel_in);

    *result_ptr = result;
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::aabb::AABB;

    fn make_test_bvh() -> BVHHandle {
        // Simple BVH with one triangle in XY plane at z=0
        let tri = Triangle::new(
            Vec3::new(0.0, 0.0, 0.0),
            Vec3::new(2.0, 0.0, 0.0),
            Vec3::new(1.0, 2.0, 0.0),
        );
        let bbox = AABB::from_triangle(&tri);

        BVHHandle {
            root: BVHNode::Leaf {
                bbox,
                triangles: vec![tri],
            },
        }
    }

    #[test]
    fn test_raymarch_hit() {
        let bvh = make_test_bvh();
        let origin = Vec3::new(1.0, 1.0, 5.0); // Above triangle
        let direction = Vec3::new(0.0, 0.0, -1.0); // Pointing down

        let (result, _) = raymarch_mesh(&bvh, origin, direction, 100, 100.0, 0.001, 100000);

        assert_eq!(result.status, 1, "Expected hit");
        assert!((result.pz).abs() < 0.01, "Hit point should be near z=0");
        assert!((result.t - 5.0).abs() < 0.1, "t should be ~5");
    }

    #[test]
    fn test_raymarch_miss() {
        let bvh = make_test_bvh();
        let origin = Vec3::new(10.0, 10.0, 5.0); // Far from triangle
        let direction = Vec3::new(0.0, 0.0, -1.0);

        let (result, _) = raymarch_mesh(&bvh, origin, direction, 100, 100.0, 0.001, 100000);

        assert_eq!(result.status, 0, "Expected miss");
    }

    #[test]
    fn test_raymarch_fuel_exhaustion() {
        let bvh = make_test_bvh();
        let origin = Vec3::new(1.0, 1.0, 5.0);
        let direction = Vec3::new(0.0, 0.0, -1.0);

        // Very low fuel
        let (result, _) = raymarch_mesh(&bvh, origin, direction, 100, 100.0, 0.001, 10);

        assert_eq!(result.status, 2, "Expected out-of-fuel");
    }

    #[test]
    fn test_normal_computation() {
        let bvh = make_test_bvh();
        let point = Vec3::new(1.0, 1.0, 0.001); // Just above triangle

        let (result, _) = mesh_sdf_normal(&bvh, point, 100000);

        assert_eq!(result.status, 1, "Expected success");
        // Normal should point in +Z direction (away from triangle)
        assert!(result.nz > 0.9, "Normal should point up");
    }
}
