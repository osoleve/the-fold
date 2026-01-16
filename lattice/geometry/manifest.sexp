(skill geometry
  (version "0.1.0")
  (tier 1)
  (path "lattice/geometry")
  (purity total)
  (stability stable)
  (fuel-bound "O(n log n) for BVH/octree operations, O(n) for raymarching")
  (deps (linalg topology))

  (description
   "3D computational geometry library providing primitives, spatial data structures,
    raymarching, and mesh topology analysis. Includes points, lines, rays, planes,
    triangles, spheres, AABBs, OBBs, transforms, distance queries, intersection tests,
    BVH and octree acceleration structures, mesh SDF computation, marching cubes
    isosurface extraction, and topological validation via Betti numbers.")

  (keywords (geometry 3d raymarching sdf bvh octree mesh intersection transform
             marching-cubes rendering ray-tracing spatial-data-structures
             topology homology betti-numbers manifold validation))
  (aliases (geom 3d-geometry spatial))

  (exports
   (geometry
    point3 line3 line3? line3-origin line3-direction
    ray3 ray3? ray3-origin ray3-direction ray3-point-at
    plane3 plane3? plane3-normal plane3-d plane3-from-point-normal plane3-from-points
    triangle3 triangle3? triangle3-p1 triangle3-p2 triangle3-p3
    circle circle? circle-center circle-radius
    sphere sphere? sphere-center sphere-radius
    aabb aabb? aabb-min aabb-max aabb-center aabb-extents
    obb obb? obb-center obb-axes obb-extents
    transform-identity transform-translation transform-scale
    transform-rotation-x transform-rotation-y transform-rotation-z
    transform-rotation-axis transform-from-quaternion
    transform-point transform-vector
    distance-point-point distance-point-plane distance-point-line distance-point-sphere
    intersect-ray-plane intersect-ray-sphere intersect-ray-aabb intersect-ray-triangle
    point-in-sphere? point-in-aabb? point-in-triangle?
    closest-point-on-line closest-point-on-plane closest-point-on-aabb
    triangle-area sphere-volume sphere-surface-area aabb-volume
    barycentric-coords triangle-normal aabb-merge aabb-from-points
    vec3-to-spherical vec3-to-cylindrical)

   (raymarch
    raymarch-params raymarch-params-max-steps raymarch-params-max-distance
    raymarch-params-hit-threshold default-raymarch-params
    raymarch raymarch-mesh sdf-normal mesh-sdf-normal
    raymarch-shadow raymarch-ao simple-shading render-pixel
    adaptive-step bvh-accelerated-raymarch)

   (octree
    octree-leaf octree-node octree-leaf? octree-node?
    octree-center octree-size octree-primitives octree-children
    octree-build subdivide-octants triangle-intersects-octant? aabb-overlaps?
    partition-triangles-octants octree-intersect-ray ray-intersects-octant?
    octree-depth octree-count-nodes octree-count-leaves octree-count-triangles)

   (bvh
    bvh-leaf bvh-node bvh-leaf? bvh-node?
    bvh-bbox bvh-primitives bvh-left bvh-right
    triangle-centroid compute-triangles-bbox longest-axis get-axis-coord
    bvh-build bvh-intersect-ray
    closest-point-on-segment closest-point-on-triangle bvh-closest-point
    bvh-depth bvh-count-nodes bvh-count-leaves bvh-count-triangles)

   (obj-loader
    parse-obj-line parse-vertex parse-face
    load-obj-from-string faces->triangles face->triangles get-vertex
    load-obj-file obj->mesh)

   (ascii-render
    ascii-ramp intensity->char rgb->ansi256 ansi-fg ansi-reset
    make-camera camera-pos camera-forward camera-right camera-up
    camera-half-width camera-half-height camera-ray
    render-pixel-color render-frame rotate-camera-around
    render-spinning-frames frames->ansi-animation save-frame)

   (mesh-sdf
    make-mesh mesh? mesh-triangles mesh-bvh
    mesh-sdf mesh-sdf-gradient
    make-mesh-cube make-mesh-sphere-ico
    subdivide-icosphere-triangle subdivide-icosphere-triangles
    mesh-intersect-ray mesh-triangle-count mesh-bvh-depth
    mesh-bvh-node-count mesh-bounds)

   (marching-cubes
    edge-table tri-table
    marching-cubes-grid marching-cubes-cube
    compute-cube-index compute-edge-intersections generate-cube-triangles
    marching-cubes marching-cubes-sphere marching-cubes-torus)

   (mesh-topology
    mesh->simplicial-complex triangles->simplicial-complex
    mesh-betti-numbers mesh-euler-characteristic mesh-f-vector
    mesh-connected-components mesh-genus
    mesh-edge-counts mesh-is-manifold? mesh-is-closed?
    mesh-edges-are-manifold? mesh-vertices-are-manifold?
    mesh-boundary-edges mesh-non-manifold-edges
    mesh-topology-summary
    mesh-is-watertight? mesh-is-sphere-topology? mesh-is-torus-topology?))

  (modules
   (geometry "geometry.ss"
    "Core 3D primitives: points, lines, rays, planes, triangles, circles, spheres,
     AABBs, OBBs. Includes transforms, distance queries, and intersection tests.")
   (raymarch "raymarch.ss"
    "Sphere tracing / raymarching for signed distance fields. Supports soft shadows,
     ambient occlusion, and BVH-accelerated mesh raymarching.")
   (octree "octree.ss"
    "Octree spatial partitioning for triangle meshes. Supports ray intersection
     queries and adaptive subdivision.")
   (bvh "bvh.ss"
    "Bounding Volume Hierarchy using AABBs. Provides fast ray intersection and
     closest point queries for triangle meshes.")
   (obj-loader "obj-loader.ss"
    "Wavefront OBJ file parser. Loads vertices and faces into triangle lists.")
   (ascii-render "ascii-render.ss"
    "ASCII/ANSI terminal renderer. Supports raytraced frames, camera rotation,
     and animated spinning renders.")
   (mesh-sdf "mesh-sdf.ss"
    "Mesh signed distance field computation. Wraps BVH for efficient point-to-mesh
     distance queries. Includes primitive mesh generators.")
   (marching-cubes "marching-cubes.ss"
    "Marching cubes isosurface extraction. Converts implicit surfaces (SDFs) to
     triangle meshes at specified resolution.")
   (mesh-topology "mesh-topology.ss"
    "Topological analysis of triangle meshes via homology. Computes Betti numbers,
     validates manifold properties, detects non-manifold edges, and verifies mesh
     topology (sphere, torus, watertight).")))
