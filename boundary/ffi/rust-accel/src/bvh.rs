//! BVH (Bounding Volume Hierarchy) for spatial acceleration
//!
//! Provides fuel-tracked BVH traversal for closest-point and ray intersection queries.
//! Designed to match Scheme's BVH structure for transparent acceleration.

use crate::aabb::AABB;
use crate::fuel::{self, costs, FuelResult};
use crate::triangle::Triangle;
use crate::vec3::Vec3;

/// BVH node - either leaf with triangles or internal with children
pub enum BVHNode {
    Leaf {
        bbox: AABB,
        triangles: Vec<Triangle>,
    },
    Internal {
        bbox: AABB,
        left: Box<BVHNode>,
        right: Box<BVHNode>,
    },
}

impl BVHNode {
    /// Get bounding box
    pub fn bbox(&self) -> &AABB {
        match self {
            BVHNode::Leaf { bbox, .. } => bbox,
            BVHNode::Internal { bbox, .. } => bbox,
        }
    }

    /// Check if this is a leaf node
    pub fn is_leaf(&self) -> bool {
        matches!(self, BVHNode::Leaf { .. })
    }
}

/// Result of closest point query
#[repr(C)]
pub struct ClosestPointResult {
    pub status: u8,
    pub px: f64,
    pub py: f64,
    pub pz: f64,
    pub distance: f64,
    pub fuel_out: u64,
}

/// Result of ray intersection query
#[repr(C)]
pub struct RayIntersectResult {
    pub status: u8,
    pub t: f64,  // distance along ray
    pub nx: f64, // normal x
    pub ny: f64, // normal y
    pub nz: f64, // normal z
    pub fuel_out: u64,
}

/// BVH handle for FFI
/// Root is public for internal use by raymarch module
pub struct BVHHandle {
    pub root: BVHNode,
}

/// Build BVH from serialized data
/// Format: header (16 bytes) + nodes (64 bytes each) + triangles (72 bytes each)
pub fn bvh_from_bytes(data: &[u8]) -> Option<BVHHandle> {
    if data.len() < 16 {
        return None;
    }

    // Read header
    let magic = u32::from_ne_bytes([data[0], data[1], data[2], data[3]]);
    if magic != 0x42564801 {
        return None;
    }

    let num_nodes = u32::from_ne_bytes([data[4], data[5], data[6], data[7]]) as usize;
    let num_tris = u32::from_ne_bytes([data[8], data[9], data[10], data[11]]) as usize;

    let header_size = 16;
    let node_size = 64;
    let tri_size = 72;

    let nodes_start = header_size;
    let tris_start = nodes_start + num_nodes * node_size;

    // Parse triangles first, assigning stable indices
    let mut triangles = Vec::with_capacity(num_tris);
    for i in 0..num_tris {
        let offset = tris_start + i * tri_size;
        if offset + tri_size > data.len() {
            return None;
        }
        triangles.push(parse_triangle(&data[offset..offset + tri_size], i as u32));
    }

    // Parse nodes recursively starting from root (index 0)
    let root = parse_node(data, nodes_start, node_size, num_nodes, &triangles, 0, 0)?;

    Some(BVHHandle { root })
}

/// Maximum BVH parse depth to prevent stack overflow on crafted data.
/// A balanced BVH of 2^64 nodes would only need depth 64.
const MAX_PARSE_DEPTH: usize = 64;

fn parse_triangle(data: &[u8], id: u32) -> Triangle {
    let p1 = Vec3::new(
        f64::from_ne_bytes(data[0..8].try_into().unwrap()),
        f64::from_ne_bytes(data[8..16].try_into().unwrap()),
        f64::from_ne_bytes(data[16..24].try_into().unwrap()),
    );
    let p2 = Vec3::new(
        f64::from_ne_bytes(data[24..32].try_into().unwrap()),
        f64::from_ne_bytes(data[32..40].try_into().unwrap()),
        f64::from_ne_bytes(data[40..48].try_into().unwrap()),
    );
    let p3 = Vec3::new(
        f64::from_ne_bytes(data[48..56].try_into().unwrap()),
        f64::from_ne_bytes(data[56..64].try_into().unwrap()),
        f64::from_ne_bytes(data[64..72].try_into().unwrap()),
    );
    Triangle::new_indexed(p1, p2, p3, id)
}

fn parse_aabb(data: &[u8]) -> AABB {
    let min = Vec3::new(
        f64::from_ne_bytes(data[0..8].try_into().unwrap()),
        f64::from_ne_bytes(data[8..16].try_into().unwrap()),
        f64::from_ne_bytes(data[16..24].try_into().unwrap()),
    );
    let max = Vec3::new(
        f64::from_ne_bytes(data[24..32].try_into().unwrap()),
        f64::from_ne_bytes(data[32..40].try_into().unwrap()),
        f64::from_ne_bytes(data[40..48].try_into().unwrap()),
    );
    AABB::new(min, max)
}

fn parse_node(
    data: &[u8],
    nodes_start: usize,
    node_size: usize,
    num_nodes: usize,
    triangles: &[Triangle],
    index: usize,
    depth: usize,
) -> Option<BVHNode> {
    // Depth limit prevents stack overflow on cyclic or pathologically deep data
    if depth >= MAX_PARSE_DEPTH {
        return None;
    }

    // Validate index against declared node count
    if index >= num_nodes {
        return None;
    }

    let offset = nodes_start + index * node_size;
    if offset + node_size > data.len() {
        return None;
    }

    let node_data = &data[offset..offset + node_size];
    let node_type = node_data[0];
    let bbox = parse_aabb(&node_data[8..56]);

    if node_type == 1 {
        // Leaf
        let first_tri = u32::from_ne_bytes(node_data[56..60].try_into().unwrap()) as usize;
        let tri_count = u32::from_ne_bytes(node_data[60..64].try_into().unwrap()) as usize;

        // Bounds check: ensure triangle indices are valid
        if first_tri > triangles.len() || first_tri + tri_count > triangles.len() {
            return None; // Invalid triangle indices
        }

        let leaf_tris: Vec<Triangle> = triangles[first_tri..first_tri + tri_count].to_vec();

        Some(BVHNode::Leaf {
            bbox,
            triangles: leaf_tris,
        })
    } else {
        // Internal
        let left_idx = u32::from_ne_bytes(node_data[56..60].try_into().unwrap()) as usize;
        let right_idx = u32::from_ne_bytes(node_data[60..64].try_into().unwrap()) as usize;

        // Validate child indices: must be in bounds and not self-referential
        if left_idx >= num_nodes || right_idx >= num_nodes {
            return None;
        }
        if left_idx == index || right_idx == index {
            return None;
        }

        let left = parse_node(
            data,
            nodes_start,
            node_size,
            num_nodes,
            triangles,
            left_idx,
            depth + 1,
        )?;
        let right = parse_node(
            data,
            nodes_start,
            node_size,
            num_nodes,
            triangles,
            right_idx,
            depth + 1,
        )?;

        Some(BVHNode::Internal {
            bbox,
            left: Box::new(left),
            right: Box::new(right),
        })
    }
}

/// Find closest point on BVH surface to given point
pub fn bvh_closest_point(
    handle: &BVHHandle,
    point: Vec3,
    mut fuel: u64,
) -> FuelResult<(Vec3, f64)> {
    // Base cost for query
    if !fuel::deduct(&mut fuel, costs::BASE_QUERY) {
        return FuelResult::OutOfFuel;
    }

    let mut best: Option<(Vec3, f64)> = None;

    fn traverse(
        node: &BVHNode,
        point: Vec3,
        best: &mut Option<(Vec3, f64)>,
        fuel: &mut u64,
    ) -> bool {
        // Cost to visit this node
        if !fuel::deduct(fuel, costs::NODE_VISIT) {
            return false;
        }

        // Cost to test AABB
        if !fuel::deduct(fuel, costs::AABB_TEST) {
            return false;
        }

        // Early exit if AABB can't improve result
        let dist_to_box = node.bbox().distance_to_point(point);
        if let Some((_, best_dist)) = best {
            if dist_to_box >= *best_dist {
                return true; // Skip this subtree, continue search
            }
        }

        match node {
            BVHNode::Leaf { triangles, .. } => {
                for tri in triangles {
                    if !fuel::deduct(fuel, costs::TRIANGLE_TEST) {
                        return false;
                    }

                    let (closest, dist) = tri.closest_point(point);
                    match best {
                        Some((_, best_dist)) if dist < *best_dist => {
                            *best = Some((closest, dist));
                        }
                        None => {
                            *best = Some((closest, dist));
                        }
                        _ => {}
                    }
                }
                true
            }
            BVHNode::Internal { left, right, .. } => {
                // Traverse closer child first for better pruning
                let left_dist = left.bbox().distance_to_point(point);
                let right_dist = right.bbox().distance_to_point(point);

                let (first, second) = if left_dist <= right_dist {
                    (left.as_ref(), right.as_ref())
                } else {
                    (right.as_ref(), left.as_ref())
                };

                traverse(first, point, best, fuel) && traverse(second, point, best, fuel)
            }
        }
    }

    let completed = traverse(&handle.root, point, &mut best, &mut fuel);

    if !completed {
        // Ran out of fuel
        FuelResult::OutOfFuel
    } else {
        match best {
            Some((pt, dist)) => FuelResult::Ok((pt, dist), fuel),
            None => FuelResult::Miss(fuel),
        }
    }
}

/// Find ray intersection with BVH
pub fn bvh_intersect_ray(
    handle: &BVHHandle,
    origin: Vec3,
    direction: Vec3,
    mut fuel: u64,
) -> FuelResult<(f64, Vec3)> {
    // Base cost for query
    if !fuel::deduct(&mut fuel, costs::BASE_QUERY) {
        return FuelResult::OutOfFuel;
    }

    let dir_norm = direction.normalize();
    let inv_dir = Vec3::new(
        if dir_norm.x.abs() > 1e-10 {
            1.0 / dir_norm.x
        } else {
            f64::INFINITY
        },
        if dir_norm.y.abs() > 1e-10 {
            1.0 / dir_norm.y
        } else {
            f64::INFINITY
        },
        if dir_norm.z.abs() > 1e-10 {
            1.0 / dir_norm.z
        } else {
            f64::INFINITY
        },
    );

    let mut best: Option<(f64, Triangle)> = None;

    fn traverse(
        node: &BVHNode,
        origin: Vec3,
        dir: Vec3,
        inv_dir: Vec3,
        best: &mut Option<(f64, Triangle)>,
        fuel: &mut u64,
    ) -> bool {
        // Cost to visit node
        if !fuel::deduct(fuel, costs::NODE_VISIT) {
            return false;
        }

        // Cost to test AABB
        if !fuel::deduct(fuel, costs::AABB_TEST) {
            return false;
        }

        // Single AABB test: miss check + distance in one call
        let t_min = match node.bbox().intersect_ray_precomp(origin, inv_dir) {
            Some((t, _)) => t,
            None => return true, // Ray misses this subtree
        };

        // Early exit if AABB entry is beyond best hit
        if let Some((best_t, _)) = best {
            if t_min >= *best_t {
                return true; // Can't improve
            }
        }

        match node {
            BVHNode::Leaf { triangles, .. } => {
                for tri in triangles {
                    if !fuel::deduct(fuel, costs::TRIANGLE_RAY) {
                        return false;
                    }

                    if let Some(t) = tri.intersect_ray(origin, dir) {
                        match best {
                            Some((best_t, _)) if t < *best_t => {
                                *best = Some((t, *tri));
                            }
                            None => {
                                *best = Some((t, *tri));
                            }
                            _ => {}
                        }
                    }
                }
                true
            }
            BVHNode::Internal { left, right, .. } => {
                // Visit closer child first for better early termination
                let left_t = left
                    .bbox()
                    .intersect_ray_precomp(origin, inv_dir)
                    .map(|(t, _)| t);
                let right_t = right
                    .bbox()
                    .intersect_ray_precomp(origin, inv_dir)
                    .map(|(t, _)| t);

                match (left_t, right_t) {
                    (Some(lt), Some(rt)) if rt < lt => {
                        traverse(right, origin, dir, inv_dir, best, fuel)
                            && traverse(left, origin, dir, inv_dir, best, fuel)
                    }
                    _ => {
                        traverse(left, origin, dir, inv_dir, best, fuel)
                            && traverse(right, origin, dir, inv_dir, best, fuel)
                    }
                }
            }
        }
    }

    let completed = traverse(
        &handle.root,
        origin,
        dir_norm,
        inv_dir,
        &mut best,
        &mut fuel,
    );

    if !completed {
        FuelResult::OutOfFuel
    } else {
        match best {
            Some((t, tri)) => FuelResult::Ok((t, tri.normal()), fuel),
            None => FuelResult::Miss(fuel),
        }
    }
}

// ====
// FFI Exports
// ====

/// Build BVH from serialized bytevector
/// Returns handle pointer or null on failure
#[no_mangle]
pub extern "C" fn fold_bvh_build(data: *const u8, len: usize) -> *mut BVHHandle {
    if data.is_null() || len < 16 {
        return std::ptr::null_mut();
    }

    let bytes = unsafe { std::slice::from_raw_parts(data, len) };

    match bvh_from_bytes(bytes) {
        Some(handle) => Box::into_raw(Box::new(handle)),
        None => std::ptr::null_mut(),
    }
}

/// Free BVH handle
#[no_mangle]
pub extern "C" fn fold_bvh_drop(handle: *mut BVHHandle) {
    if !handle.is_null() {
        unsafe {
            drop(Box::from_raw(handle));
        }
    }
}

/// Query closest point on BVH surface
#[no_mangle]
pub extern "C" fn fold_bvh_closest_point(
    handle: *mut BVHHandle,
    px: f64,
    py: f64,
    pz: f64,
    fuel_in: u64,
    out: *mut ClosestPointResult,
) {
    // Safety: Initialize output on null pointer to avoid reading uninitialized memory
    if out.is_null() {
        return;
    }
    if handle.is_null() {
        unsafe {
            (*out).status = 0; // miss
            (*out).px = 0.0;
            (*out).py = 0.0;
            (*out).pz = 0.0;
            (*out).distance = 0.0;
            (*out).fuel_out = 0;
        }
        return;
    }

    let bvh = unsafe { &*handle };
    let point = Vec3::new(px, py, pz);

    let result = bvh_closest_point(bvh, point, fuel_in);

    unsafe {
        (*out).status = result.status();
        (*out).fuel_out = result.fuel();

        match result {
            FuelResult::Ok((pt, dist), _) => {
                (*out).px = pt.x;
                (*out).py = pt.y;
                (*out).pz = pt.z;
                (*out).distance = dist;
            }
            _ => {
                (*out).px = 0.0;
                (*out).py = 0.0;
                (*out).pz = 0.0;
                (*out).distance = 0.0;
            }
        }
    }
}

/// Query ray intersection with BVH
#[no_mangle]
pub extern "C" fn fold_bvh_intersect_ray(
    handle: *mut BVHHandle,
    ox: f64,
    oy: f64,
    oz: f64,
    dx: f64,
    dy: f64,
    dz: f64,
    fuel_in: u64,
    out: *mut RayIntersectResult,
) {
    // Safety: Initialize output on null pointer to avoid reading uninitialized memory
    if out.is_null() {
        return;
    }
    if handle.is_null() {
        unsafe {
            (*out).status = 0; // miss
            (*out).t = 0.0;
            (*out).nx = 0.0;
            (*out).ny = 0.0;
            (*out).nz = 0.0;
            (*out).fuel_out = 0;
        }
        return;
    }

    let bvh = unsafe { &*handle };
    let origin = Vec3::new(ox, oy, oz);
    let direction = Vec3::new(dx, dy, dz);

    let result = bvh_intersect_ray(bvh, origin, direction, fuel_in);

    unsafe {
        (*out).status = result.status();
        (*out).fuel_out = result.fuel();

        match result {
            FuelResult::Ok((t, normal), _) => {
                (*out).t = t;
                (*out).nx = normal.x;
                (*out).ny = normal.y;
                (*out).nz = normal.z;
            }
            _ => {
                (*out).t = 0.0;
                (*out).nx = 0.0;
                (*out).ny = 0.0;
                (*out).nz = 0.0;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_test_bvh() -> BVHHandle {
        // Simple BVH with one triangle
        let tri = Triangle::new(
            Vec3::new(0.0, 0.0, 0.0),
            Vec3::new(1.0, 0.0, 0.0),
            Vec3::new(0.0, 1.0, 0.0),
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
    fn test_closest_point() {
        let bvh = make_test_bvh();
        let point = Vec3::new(0.5, 0.5, 1.0); // Above the triangle

        let result = bvh_closest_point(&bvh, point, 1000);

        match result {
            FuelResult::Ok((pt, dist), _) => {
                // Closest point should be on the triangle (z ≈ 0)
                assert!(pt.z.abs() < 0.1);
                assert!((dist - 1.0).abs() < 0.1);
            }
            _ => panic!("Expected hit"),
        }
    }

    #[test]
    fn test_ray_intersect() {
        let bvh = make_test_bvh();
        let origin = Vec3::new(0.25, 0.25, 1.0);
        let direction = Vec3::new(0.0, 0.0, -1.0);

        let result = bvh_intersect_ray(&bvh, origin, direction, 1000);

        match result {
            FuelResult::Ok((t, _), _) => {
                assert!((t - 1.0).abs() < 0.1);
            }
            _ => panic!("Expected hit"),
        }
    }

    #[test]
    fn test_fuel_exhaustion() {
        let bvh = make_test_bvh();
        let point = Vec3::new(0.5, 0.5, 1.0);

        // Very low fuel should cause exhaustion
        let result = bvh_closest_point(&bvh, point, 2);

        assert!(matches!(result, FuelResult::OutOfFuel));
    }

    #[test]
    fn test_from_bytes_rejects_out_of_bounds_child() {
        // Craft a minimal BVH with an internal node pointing to index 99
        // when only 1 node exists. Should return None.
        let mut data = vec![0u8; 16 + 64]; // header + 1 node, 0 triangles

        // Header: magic
        data[0..4].copy_from_slice(&0x42564801u32.to_ne_bytes());
        // num_nodes = 1
        data[4..8].copy_from_slice(&1u32.to_ne_bytes());
        // num_tris = 0
        data[8..12].copy_from_slice(&0u32.to_ne_bytes());

        // Node at index 0: internal (type=0)
        data[16] = 0; // type = internal
                      // bbox bytes 8..56 (zeroed is fine)
                      // left_idx = 99 (out of bounds)
        data[16 + 56..16 + 60].copy_from_slice(&99u32.to_ne_bytes());
        // right_idx = 0 (self-ref, but should fail on left first)
        data[16 + 60..16 + 64].copy_from_slice(&0u32.to_ne_bytes());

        assert!(bvh_from_bytes(&data).is_none());
    }

    #[test]
    fn test_from_bytes_rejects_self_referential() {
        // Internal node where left child points back to itself
        let mut data = vec![0u8; 16 + 64 * 2 + 72]; // 2 nodes, 1 triangle

        // Header
        data[0..4].copy_from_slice(&0x42564801u32.to_ne_bytes());
        data[4..8].copy_from_slice(&2u32.to_ne_bytes()); // 2 nodes
        data[8..12].copy_from_slice(&1u32.to_ne_bytes()); // 1 triangle

        // Node 0: internal, left=0 (self-ref), right=1
        let n0 = 16;
        data[n0] = 0; // internal
        data[n0 + 56..n0 + 60].copy_from_slice(&0u32.to_ne_bytes()); // left=0 (self)
        data[n0 + 60..n0 + 64].copy_from_slice(&1u32.to_ne_bytes()); // right=1

        // Node 1: leaf, first_tri=0, tri_count=1
        let n1 = 16 + 64;
        data[n1] = 1; // leaf
        data[n1 + 56..n1 + 60].copy_from_slice(&0u32.to_ne_bytes());
        data[n1 + 60..n1 + 64].copy_from_slice(&1u32.to_ne_bytes());

        // Triangle at offset 16 + 128 (zeroed coords is fine for this test)

        assert!(bvh_from_bytes(&data).is_none());
    }
}
