(skill geometry
  (version "0.1.0")
  (tier 1)
  (path "lattice/geometry")
  (purity total)
  (stability stable)
  (fuel-bound "O(n log n) for BVH/octree operations, O(n) for raymarching")
  (deps (linalg topology optics fp))

  (description
   "2D/3D computational geometry library providing primitives, spatial data structures,
    raymarching, mesh generation, and mesh topology analysis. Includes points, lines,
    rays, planes, triangles, spheres, AABBs, OBBs, transforms, distance queries,
    intersection tests, BVH and octree acceleration structures, mesh SDF computation,
    marching cubes isosurface extraction, Delaunay triangulation with refinement,
    Laplacian mesh smoothing, adaptive refinement, boundary-constrained meshing,
    Voronoi diagrams (via Delaunay duality) with Lloyd relaxation,
    topological validation via Betti numbers, and 2D convex hull algorithms
    (Graham scan, Quickhull) with Minkowski operations.")

  (keywords (geometry 2d 3d raymarching sdf bvh octree mesh intersection transform
             marching-cubes rendering ray-tracing spatial-data-structures
             topology homology betti-numbers manifold validation
             delaunay triangulation mesh-generation refinement voronoi lloyd-relaxation
             laplacian-smoothing adaptive-refinement boundary-constrained polygon-mesh
             convex-hull graham-scan quickhull minkowski collision-detection
             optics lenses traversals prisms functional-access))
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
    load-obj-from-string faces->triangles face->triangles get-vertex)

   (ascii-render
    ascii-ramp intensity->char rgb->ansi256 ansi-fg ansi-reset
    make-camera camera-pos camera-forward camera-right camera-up
    camera-half-width camera-half-height camera-ray
    render-pixel-color render-frame rotate-camera-around
    render-spinning-frames)

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

   (mesh-gen
    ;; Points and triangles
    make-point2 point2-x point2-y
    make-tri2 tri2? tri2-p1 tri2-p2 tri2-p3 tri2-points
    tri2-area tri2-circumcenter tri2-circumradius-sq point-in-circumcircle?
    ;; Triangulation record (BREAKING: delaunay-triangulate now returns this)
    triangulation? triangulation-points triangulation-triangles
    triangulation-boundary triangle-neighbors
    delaunay-triangulate build-adjacency
    ;; Point location (O(√n) walking)
    orient2d barycentric-coords
    location? location-triangle location-bary
    locate-point interpolate-at
    ;; Quality metrics
    tri2-edge-lengths tri2-aspect-ratio tri2-angles tri2-min-angle tri2-max-angle
    mesh-quality-report refine-mesh
    ;; Laplacian smoothing
    smooth-mesh
    ;; Adaptive refinement
    adaptive-refine-mesh refine-mesh-uniform
    ;; Boundary-constrained meshing
    point-in-polygon? triangulate-polygon triangulate-polygon-adaptive
    ;; Utilities
    triangles-to-3d random-points-in-rect render-mesh-2d)

   (mesh-topology
    mesh->simplicial-complex triangles->simplicial-complex
    mesh-betti-numbers mesh-euler-characteristic mesh-f-vector
    mesh-connected-components mesh-genus
    mesh-edge-counts mesh-is-manifold? mesh-is-closed?
    mesh-edges-are-manifold? mesh-vertices-are-manifold?
    mesh-boundary-edges mesh-non-manifold-edges
    mesh-topology-summary
    mesh-is-watertight? mesh-is-sphere-topology? mesh-is-torus-topology?)

   (convex-hull
    cross-product-2d ccw? collinear? point2-distance-sq
    graham-scan quickhull convex-hull
    convex-hull-area convex-hull-perimeter convex-hull-centroid convex-hull-diameter
    point-in-convex-hull?
    extreme-point support-function
    minkowski-sum minkowski-difference hulls-intersect?)

   (voronoi
    voronoi-diagram voronoi? voronoi-sites voronoi-vertices voronoi-edges voronoi-cells
    voronoi-cell-polygon voronoi-cell-centroid voronoi-cell-unbounded?
    voronoi-neighbors voronoi-nearest-site
    voronoi-bounded voronoi-bounded? voronoi-bounded-sites voronoi-bounded-cells
    voronoi-bounded-cell voronoi-cell-areas voronoi-summary
    lloyd-step lloyd-relax render-voronoi)

   (shape-protocol
    ;; Shape protocols
    shape-intersect-ray shape-aabb shape-contains-point? shape-distance-point
    shape-center shape-volume shape-surface-area shape-normal-at
    ;; Scene operations
    scene-intersect-ray scene-aabb shapes-containing-point
    closest-shape scene-sdf scene-total-volume)

   (geometry-optics
    ;; Vec3 lenses
    vec3-x-lens vec3-y-lens vec3-z-lens
    ;; Line3 lenses
    line3-origin-lens line3-direction-lens
    ;; Ray3 lenses
    ray3-origin-lens ray3-direction-lens
    ray3-origin-x-lens ray3-origin-y-lens ray3-origin-z-lens
    ;; Plane3 lenses
    plane3-normal-lens plane3-d-lens
    ;; Triangle3 lenses and traversals
    triangle3-p1-lens triangle3-p2-lens triangle3-p3-lens triangle3-vertices-each
    ;; Circle lenses
    circle-center-lens circle-radius-lens
    ;; Sphere lenses
    sphere-center-lens sphere-radius-lens
    sphere-center-x-lens sphere-center-y-lens sphere-center-z-lens
    ;; AABB lenses and traversals
    aabb-min-lens aabb-max-lens aabb-corners-each
    ;; OBB lenses
    obb-center-lens obb-axes-lens obb-extents-lens
    ;; Type prisms
    prism-line3 prism-ray3 prism-plane3 prism-triangle3
    prism-sphere prism-aabb prism-circle
    ;; Traversals
    shapes-each triangles-each spheres-each all-triangle-vertices
    ;; Convenience
    translate-shape scale-sphere transform-triangle))

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
   (mesh-gen "mesh-gen.ss"
    "2D mesh generation with Delaunay triangulation (Bowyer-Watson), O(√n) point location
     via walking algorithm, barycentric interpolation, quality metrics (aspect ratio, angles),
     Ruppert refinement, Laplacian smoothing, adaptive area-based refinement, and
     boundary-constrained meshing (mesh inside polygon). BREAKING: delaunay-triangulate
     now returns a triangulation record with adjacency; use triangulation-triangles to
     extract the triangle list.")
   (mesh-topology "mesh-topology.ss"
    "Topological analysis of triangle meshes via homology. Computes Betti numbers,
     validates manifold properties, detects non-manifold edges, and verifies mesh
     topology (sphere, torus, watertight).")
   (convex-hull "convex-hull.ss"
    "2D convex hull algorithms: Graham scan O(n log n), Quickhull O(n log n) expected.
     Includes hull properties (area, perimeter, centroid, diameter), point-in-hull tests,
     extreme point queries, support functions, and Minkowski sum/difference for
     collision detection.")
   (voronoi "voronoi.ss"
    "Voronoi diagrams via Delaunay duality. Computes cells, neighbors, nearest site queries.
     Supports bounded Voronoi (clipped to rectangle) and Lloyd relaxation for uniform
     point distribution. Includes cell area computation and ASCII visualization.")
   (geometry-optics "geometry-optics.ss"
    "Composable optics for geometric primitives. Provides lenses for Vec3, rays, planes,
     triangles, spheres, AABBs, and OBBs. Includes traversals for multi-element structures
     (triangle vertices, AABB corners), type prisms for safe access, and convenience
     combinators (translate-shape, transform-triangle).")
   (shape-protocol "shape-protocol.ss"
    "Unified protocol interface for all shape types. Enables polymorphic operations
     (intersect-ray, aabb, contains-point, distance, center, volume, surface-area, normal)
     across heterogeneous shape collections. Supports scene-level operations like
     closest-hit raycast, combined AABB, SDF, and total volume computation.")))
