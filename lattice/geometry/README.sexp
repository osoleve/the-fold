(document
(title "Geometry Library with BVH/Octree Acceleration and Topology Analysis")
(version "1.1.0")
(created "2026-01-05")
(updated "2026-01-16")
(section
 (title "Overview")
 (content "The geometry library provides core 3D geometric primitives, transformations,\n    spatial acceleration structures, mesh generation, and topological analysis.\n\n    Key capabilities:\n    - Geometric primitives (points, lines, rays, planes, triangles, spheres, boxes)\n    - BVH and Octree spatial partitioning for efficient queries\n    - Mesh SDF computation with BVH acceleration\n    - Marching cubes isosurface extraction\n    - Mesh topology validation via homology (Betti numbers, manifold detection)\n    - ASCII/ANSI terminal rendering with raytracing"))
(section
 (title "Modules")
 (module
  (name "geometry.ss")
  (description "Core geometric primitives and operations")
  (exports
   (point3 vec3)
   (line3 line3-origin line3-direction)
   (ray3 ray3-origin ray3-direction ray3-point-at)
   (plane3 plane3-normal plane3-d)
   (triangle3 triangle3-p1 triangle3-p2 triangle3-p3)
   (sphere sphere-center sphere-radius)
   (aabb aabb-min aabb-max aabb-center aabb-extents)
   (transform-identity transform-translation transform-scale transform-rotation-x transform-rotation-y transform-rotation-z transform-rotation-axis transform-from-quaternion transform-point transform-vector)
   (distance-point-point distance-point-plane distance-point-sphere intersect-ray-plane intersect-ray-sphere intersect-ray-aabb intersect-ray-triangle)
   (triangle-normal barycentric-coords aabb-merge aabb-from-points)))
 (module
  (name "bvh.ss")
  (description "Bounding Volume Hierarchy for spatial acceleration")
  (exports
   (bvh-build)
   (bvh-intersect-ray)
   (bvh-closest-point)
   (bvh-depth bvh-count-nodes bvh-count-leaves bvh-count-triangles))
  (performance "BVH provides O(log n) average query time for ray-triangle intersection and\n     nearest-point queries. Best for non-uniform triangle distributions."))
 (module
  (name "octree.ss")
  (description "Octree spatial partitioning")
  (exports
   (octree-build)
   (octree-intersect-ray)
   (octree-depth octree-count-nodes octree-count-leaves octree-count-triangles))
  (performance "Octree provides uniform spatial subdivision. Simpler to build than BVH,\n     best for uniformly distributed triangles. Depth-first traversal ensures\n     efficient memory access patterns."))
 (module
  (name "mesh-sdf.ss")
  (description "Mesh Signed Distance Fields with BVH acceleration")
  (exports
   (make-mesh)
   (make-mesh-cube)
   (make-mesh-sphere-ico)
   (mesh-sdf)
   (mesh-sdf-gradient)
   (mesh-intersect-ray)
   (mesh-triangle-count mesh-bvh-depth mesh-bvh-node-count mesh-bounds))
  (note "Mesh SDF uses BVH internally for O(log n) distance queries. Signed distance\n     is negative inside the mesh, positive outside, and zero on the surface."))
 (module
  (name "raymarch.ss")
  (description "SDF raymarching with BVH acceleration")
  (exports
   (raymarch)
   (raymarch-mesh)
   (bvh-accelerated-raymarch)
   (sdf-normal)
   (mesh-sdf-normal)
   (raymarch-shadow)
   (raymarch-ao)
   (simple-shading)
   (render-pixel)
   (raymarch-params default-raymarch-params))
  (algorithm "Sphere tracing: iteratively step along ray by distance to nearest surface.\n     Guaranteed not to overshoot surfaces. Converges quickly for smooth SDFs."))
 (module
  (name "marching-cubes.ss")
  (description "Isosurface extraction from implicit functions")
  (exports
   (marching-cubes)
   (marching-cubes-grid)
   (marching-cubes-sphere)
   (marching-cubes-torus)
   (edge-table tri-table)
   (compute-cube-index)
   (generate-cube-triangles))
  (algorithm "Marching cubes extracts polygonal meshes from scalar fields by evaluating\n     the implicit function at grid vertices, classifying cube configurations,\n     and interpolating edge crossings. Produces watertight meshes from SDFs."))
 (module
  (name "mesh-topology.ss")
  (description "Topological analysis of triangle meshes via homology")
  (exports
   (mesh->simplicial-complex)
   (triangles->simplicial-complex)
   (mesh-betti-numbers)
   (mesh-euler-characteristic)
   (mesh-f-vector)
   (mesh-connected-components)
   (mesh-genus)
   (mesh-is-manifold?)
   (mesh-edges-are-manifold?)
   (mesh-vertices-are-manifold?)
   (mesh-is-closed?)
   (mesh-boundary-edges)
   (mesh-non-manifold-edges)
   (mesh-is-watertight?)
   (mesh-is-sphere-topology?)
   (mesh-is-torus-topology?)
   (mesh-topology-summary))
  (theory "Computes Betti numbers via Z₂ homology:\n     - B₀ = connected components (should be 1 for single solid)\n     - B₁ = handles/tunnels (genus-related)\n     - B₂ = enclosed voids (should be 0 for watertight mesh)\n\n     Manifold validation checks two conditions:\n     - Edge condition: every edge shared by 1-2 triangles\n     - Vertex condition: every vertex link is connected (detects pinch points)\n\n     Useful for validating marching cubes output, detecting mesh defects\n     before physics simulation, and classifying surface topology.")))
(section
 (title "Usage Examples")
 (example
  (name "Creating and querying a BVH")
  (code "(load \"lattice/geometry/bvh.ss\")\n\n     ;; Create some triangles\n     (define triangles\n       (list (triangle3 (vec3 0 0 0) (vec3 1 0 0) (vec3 0 1 0))\n             (triangle3 (vec3 2 0 0) (vec3 3 0 0) (vec3 2 1 0))))\n\n     ;; Build BVH (max 5 triangles per leaf)\n     (define bvh (bvh-build triangles 5))\n\n     ;; Ray-triangle intersection\n     (define ray (ray3 (vec3 0.25 0.25 -5) (vec3 0 0 1)))\n     (define hit (bvh-intersect-ray bvh ray))\n\n     (if hit\n         (let ([triangle (car hit)]\n               [t (cadr hit)])\n              (display \"Hit triangle at t=\")\n              (display t)\n              (newline))\n         (display \"No intersection\\n\"))"))
 (example
  (name "Creating a mesh and computing SDF")
  (code "(load \"lattice/geometry/mesh-sdf.ss\")\n\n     ;; Create a cube mesh\n     (define cube (make-mesh-cube 1.0))\n\n     ;; Query signed distance at various points\n     (define dist-inside (mesh-sdf cube (vec3 0 0 0)))      ; Negative\n     (define dist-outside (mesh-sdf cube (vec3 3 0 0)))     ; Positive\n     (define dist-surface (mesh-sdf cube (vec3 1 0 0)))     ; Near zero\n\n     ;; Compute surface normal\n     (define normal (mesh-sdf-gradient cube (vec3 1.1 0 0))) ; Points in +X\n\n     (display \"Inside:  \") (display dist-inside)  (newline)\n     (display \"Outside: \") (display dist-outside) (newline)\n     (display \"Surface: \") (display dist-surface) (newline)"))
 (example
  (name "Raymarching a mesh SDF")
  (code "(load \"lattice/geometry/raymarch.ss\")\n\n     ;; Create mesh\n     (define mesh (make-mesh-cube 1.0))\n\n     ;; Create ray\n     (define ray (ray3 (vec3 0 0 -5) (vec3 0 0 1)))\n\n     ;; Raymarch with default parameters\n     (define result (raymarch-mesh mesh ray default-raymarch-params))\n\n     (if result\n         (let ([hit-point (car result)]\n               [distance (cadr result)]\n               [steps (caddr result)])\n              (display \"Hit at: \") (write hit-point) (newline)\n              (display \"Distance: \") (display distance) (newline)\n              (display \"Steps: \") (display steps) (newline))\n         (display \"No hit\\n\"))"))
 (example
  (name "Using BVH-accelerated raymarching")
  (code "(load \"lattice/geometry/raymarch.ss\")\n\n     (define mesh (make-mesh-sphere-ico 1.0 0))\n     (define ray (ray3 (vec3 0 0 -5) (vec3 0 0 1)))\n\n     ;; Hybrid approach: BVH for coarse hit, then raymarch locally\n     (define result (bvh-accelerated-raymarch mesh ray default-raymarch-params))\n\n     (if result\n         (let ([hit-point (car result)])\n              ;; Compute shading\n              (define normal (mesh-sdf-normal mesh hit-point))\n              (define light-dir (vec3-normalize (vec3 1 1 -1)))\n              (define view-dir (vec3-normalize (vec3-scale (ray3-direction ray) -1)))\n              (define shading (simple-shading normal light-dir view-dir))\n\n              (display \"Shading: \") (display shading) (newline))\n         (display \"No hit\\n\"))"))
 (example
  (name "Creating an octree")
  (code "(load \"lattice/geometry/octree.ss\")\n\n     (define triangles\n       (list (triangle3 (vec3 0 0 0) (vec3 1 0 0) (vec3 0 1 0))\n             (triangle3 (vec3 2 0 0) (vec3 3 0 0) (vec3 2 1 0))))\n\n     ;; Build octree\n     ;; center: (0,0,0), half-size: 5.0, max-depth: 5, max-leaf-size: 10\n     (define octree (octree-build triangles (vec3 0 0 0) 5.0 5 10))\n\n     ;; Query statistics\n     (display \"Depth: \") (display (octree-depth octree)) (newline)\n     (display \"Nodes: \") (display (octree-count-nodes octree)) (newline)\n     (display \"Triangles: \") (display (octree-count-triangles octree)) (newline)"))
 (example
  (name "Validating mesh topology")
  (code "(load \"lattice/geometry/mesh-topology.ss\")\n\n     ;; Create a cube mesh\n     (define cube (make-mesh-cube 1.0))\n\n     ;; Compute Betti numbers\n     (define betti (mesh-betti-numbers cube))\n     (display \"Betti numbers: \") (write betti) (newline)\n     ;; => (1 0 1) — connected, no handles, one void\n\n     ;; Check if watertight (suitable for physics simulation)\n     (display \"Watertight? \") (display (mesh-is-watertight? cube)) (newline)\n     ;; => #t\n\n     ;; Verify sphere topology (genus 0)\n     (display \"Sphere topology? \") (display (mesh-is-sphere-topology? cube)) (newline)\n     ;; => #t\n\n     ;; Check manifold properties\n     (display \"Is manifold? \") (display (mesh-is-manifold? cube)) (newline)\n     ;; => #t (passes both edge and vertex conditions)\n\n     ;; Print full analysis\n     (mesh-topology-summary cube)"))
 (example
  (name "Detecting mesh defects")
  (code "(load \"lattice/geometry/mesh-topology.ss\")\n\n     ;; Identify boundary edges (mesh is open)\n     (define boundary (mesh-boundary-edges some-mesh))\n     (when (> (length boundary) 0)\n       (display \"Mesh has boundary edges — not watertight!\\n\"))\n\n     ;; Detect non-manifold edges (>2 triangles share edge)\n     (define bad-edges (mesh-non-manifold-edges some-mesh))\n     (when (> (length bad-edges) 0)\n       (display \"Non-manifold edges detected!\\n\"))\n\n     ;; Check for pinch points (vertex shared by disconnected surfaces)\n     (unless (mesh-vertices-are-manifold? some-mesh)\n       (display \"Mesh has pinch points — vertex link disconnected!\\n\"))")))
(section
 (title "Performance Characteristics")
 (comparison
  (aspect "Construction Time")
  (bvh "O(n log n) — Requires sorting triangles")
  (octree "O(n * depth) — Simpler, but can have duplicates"))
 (comparison
  (aspect "Query Time")
  (bvh "O(log n) average — Tighter bounds, fewer false positives")
  (octree "O(log n) average — Uniform subdivision, predictable"))
 (comparison
  (aspect "Memory Usage")
  (bvh "O(n) — Each triangle appears once")
  (octree "O(n * k) — Triangles can appear in multiple cells"))
 (comparison
  (aspect "Best Use Case")
  (bvh "Non-uniform distributions, high-quality scenes")
  (octree "Uniform distributions, dynamic scenes, spatial queries")))
(section
 (title "Testing")
 (content "Comprehensive test suites are provided for all modules:")
 (test-file "test-bvh.ss" "Tests BVH construction, ray intersection, closest point queries")
 (test-file "test-mesh-sdf.ss" "Tests mesh creation, SDF computation, signed distance correctness")
 (test-file "test-raymarch.ss" "Tests raymarching algorithm, shading, soft shadows, ambient occlusion")
 (test-file "test-mesh-topology.ss" "Tests mesh-to-SC conversion, Betti numbers, manifold validation,\n              genus detection, hourglass pinch-point detection")
 (run-tests "scheme --script lattice/geometry/test-mesh-topology.ss"))
(section
 (title "References")
 (paper "Physically Based Rendering" "Pharr, Jakob, Humphreys" "2016" "Comprehensive coverage of BVH construction and traversal")
 (paper "Real-Time Collision Detection" "Ericson" "2004" "Practical algorithms for spatial data structures and geometric queries")
 (paper "Sphere Tracing" "Hart" "1996" "Original paper on raymarching implicit surfaces using SDFs"))
(section
 (title "Future Enhancements")
 (enhancement "Surface Area Heuristic (SAH) for BVH" "Use SAH cost function for better split selection during BVH construction")
 (enhancement "SBVH (Spatial BVH)" "Hybrid BVH/octree approach with spatial splits for better quality")
 (enhancement "Mesh SDF caching" "Cache computed SDF values in a 3D grid for faster repeated queries")
 (enhancement "GPU acceleration" "Parallel BVH/octree traversal on GPU for real-time rendering")
 (enhancement "Icosphere vertex deduplication" "Fix floating-point vertex merging issues in icosphere construction")
 (enhancement "Wider range of primitives" "Support for CSG operations, implicit surfaces, bezier patches")
 (enhancement "Persistent homology" "Extend mesh-topology to support filtrations and persistence diagrams")))
