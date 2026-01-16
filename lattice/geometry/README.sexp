;;; lattice/geometry/README.sexp — Geometry and Spatial Acceleration Documentation

(document
 (title "Geometry Library with BVH/Octree Acceleration and Topology Analysis")
 (version "1.1.0")
 (created "2026-01-05")
 (updated "2026-01-16")

 (section
  (title "Overview")
  (content
   "The geometry library provides core 3D geometric primitives, transformations,
    spatial acceleration structures, mesh generation, and topological analysis.

    Key capabilities:
    - Geometric primitives (points, lines, rays, planes, triangles, spheres, boxes)
    - BVH and Octree spatial partitioning for efficient queries
    - Mesh SDF computation with BVH acceleration
    - Marching cubes isosurface extraction
    - Mesh topology validation via homology (Betti numbers, manifold detection)
    - ASCII/ANSI terminal rendering with raytracing"))

 (section
  (title "Modules")

  (module
   (name "geometry.ss")
   (description "Core geometric primitives and operations")
   (exports
    ;; Primitives
    (point3 vec3)
    (line3 line3-origin line3-direction)
    (ray3 ray3-origin ray3-direction ray3-point-at)
    (plane3 plane3-normal plane3-d)
    (triangle3 triangle3-p1 triangle3-p2 triangle3-p3)
    (sphere sphere-center sphere-radius)
    (aabb aabb-min aabb-max aabb-center aabb-extents)

    ;; Transformations
    (transform-identity transform-translation transform-scale
     transform-rotation-x transform-rotation-y transform-rotation-z
     transform-rotation-axis transform-from-quaternion
     transform-point transform-vector)

    ;; Distance & Intersection
    (distance-point-point distance-point-plane distance-point-sphere
     intersect-ray-plane intersect-ray-sphere intersect-ray-aabb
     intersect-ray-triangle)

    ;; Utilities
    (triangle-normal barycentric-coords aabb-merge aabb-from-points)))

  (module
   (name "bvh.ss")
   (description "Bounding Volume Hierarchy for spatial acceleration")
   (exports
    ;; Construction
    (bvh-build)  ; (List Triangle3) × Number → BVH

    ;; Queries
    (bvh-intersect-ray)     ; BVH × Ray3 → (Triangle3 Number) | #f
    (bvh-closest-point)     ; BVH × Point3 → (Point3 Number Triangle3) | #f

    ;; Statistics
    (bvh-depth bvh-count-nodes bvh-count-leaves bvh-count-triangles))

   (performance
    "BVH provides O(log n) average query time for ray-triangle intersection and
     nearest-point queries. Best for non-uniform triangle distributions."))

  (module
   (name "octree.ss")
   (description "Octree spatial partitioning")
   (exports
    ;; Construction
    (octree-build)  ; (List Triangle3) × Point3 × Number × Number × Number → Octree

    ;; Queries
    (octree-intersect-ray)  ; Octree × Ray3 → (Triangle3 Number) | #f

    ;; Statistics
    (octree-depth octree-count-nodes octree-count-leaves octree-count-triangles))

   (performance
    "Octree provides uniform spatial subdivision. Simpler to build than BVH,
     best for uniformly distributed triangles. Depth-first traversal ensures
     efficient memory access patterns."))

  (module
   (name "mesh-sdf.ss")
   (description "Mesh Signed Distance Fields with BVH acceleration")
   (exports
    ;; Mesh construction
    (make-mesh)            ; (List Triangle3) → Mesh
    (make-mesh-cube)       ; Number → Mesh
    (make-mesh-sphere-ico) ; Number × Number → Mesh

    ;; SDF queries
    (mesh-sdf)             ; Mesh × Point3 → Number (signed distance)
    (mesh-sdf-gradient)    ; Mesh × Point3 → Vec3 (normal)

    ;; Ray intersection
    (mesh-intersect-ray)   ; Mesh × Ray3 → (Point3 Number Triangle3) | #f

    ;; Statistics
    (mesh-triangle-count mesh-bvh-depth mesh-bvh-node-count mesh-bounds))

   (note
    "Mesh SDF uses BVH internally for O(log n) distance queries. Signed distance
     is negative inside the mesh, positive outside, and zero on the surface."))

  (module
   (name "raymarch.ss")
   (description "SDF raymarching with BVH acceleration")
   (exports
    ;; Raymarching
    (raymarch)                    ; SDF-Function × Ray3 × Params → Result | #f
    (raymarch-mesh)               ; Mesh × Ray3 × Params → Result | #f
    (bvh-accelerated-raymarch)    ; Mesh × Ray3 × Params → Result | #f

    ;; Normals
    (sdf-normal)                  ; SDF-Function × Point3 → Vec3
    (mesh-sdf-normal)             ; Mesh × Point3 → Vec3

    ;; Rendering effects
    (raymarch-shadow)             ; SDF × Ray3 × Number × Number × Params → Number
    (raymarch-ao)                 ; SDF × Point3 × Vec3 × Number → Number
    (simple-shading)              ; Vec3 × Vec3 × Vec3 → Number
    (render-pixel)                ; SDF × Ray3 × Vec3 × Params → Number

    ;; Parameters
    (raymarch-params default-raymarch-params))

   (algorithm
    "Sphere tracing: iteratively step along ray by distance to nearest surface.
     Guaranteed not to overshoot surfaces. Converges quickly for smooth SDFs."))

  (module
   (name "marching-cubes.ss")
   (description "Isosurface extraction from implicit functions")
   (exports
    ;; Core algorithm
    (marching-cubes)              ; SDF × Grid-params → (List Triangle3)
    (marching-cubes-grid)         ; SDF × Resolution × Bounds → (List Triangle3)

    ;; Primitive generators
    (marching-cubes-sphere)       ; Number × Resolution → (List Triangle3)
    (marching-cubes-torus)        ; Major × Minor × Resolution → (List Triangle3)

    ;; Internals (for custom implementations)
    (edge-table tri-table)        ; Lookup tables for cube configurations
    (compute-cube-index)          ; Sample values → Configuration index
    (generate-cube-triangles))    ; Cube → Triangles

   (algorithm
    "Marching cubes extracts polygonal meshes from scalar fields by evaluating
     the implicit function at grid vertices, classifying cube configurations,
     and interpolating edge crossings. Produces watertight meshes from SDFs."))

  (module
   (name "mesh-topology.ss")
   (description "Topological analysis of triangle meshes via homology")
   (exports
    ;; Conversion
    (mesh->simplicial-complex)     ; Mesh → SC (simplicial complex)
    (triangles->simplicial-complex) ; (List Triangle3) → SC

    ;; Topological invariants
    (mesh-betti-numbers)           ; Mesh → (B_0 B_1 B_2)
    (mesh-euler-characteristic)    ; Mesh → Integer
    (mesh-f-vector)                ; Mesh → (V E F)
    (mesh-connected-components)    ; Mesh → Integer
    (mesh-genus)                   ; Mesh → Integer | 'open | 'disconnected

    ;; Manifold validation
    (mesh-is-manifold?)            ; Mesh → Boolean (edge + vertex conditions)
    (mesh-edges-are-manifold?)     ; Mesh → Boolean (edge condition only)
    (mesh-vertices-are-manifold?)  ; Mesh → Boolean (vertex link condition)
    (mesh-is-closed?)              ; Mesh → Boolean (no boundary edges)
    (mesh-boundary-edges)          ; Mesh → (List Edge)
    (mesh-non-manifold-edges)      ; Mesh → (List Edge)

    ;; Topology predicates
    (mesh-is-watertight?)          ; Connected, closed, manifold
    (mesh-is-sphere-topology?)     ; B = (1,0,1), χ = 2
    (mesh-is-torus-topology?)      ; B = (1,2,1), χ = 0

    ;; Diagnostics
    (mesh-topology-summary))       ; Print comprehensive analysis

   (theory
    "Computes Betti numbers via Z₂ homology:
     - B₀ = connected components (should be 1 for single solid)
     - B₁ = handles/tunnels (genus-related)
     - B₂ = enclosed voids (should be 0 for watertight mesh)

     Manifold validation checks two conditions:
     - Edge condition: every edge shared by 1-2 triangles
     - Vertex condition: every vertex link is connected (detects pinch points)

     Useful for validating marching cubes output, detecting mesh defects
     before physics simulation, and classifying surface topology.")))

 (section
  (title "Usage Examples")

  (example
   (name "Creating and querying a BVH")
   (code
    "(load \"lattice/geometry/bvh.ss\")

     ;; Create some triangles
     (define triangles
       (list (triangle3 (vec3 0 0 0) (vec3 1 0 0) (vec3 0 1 0))
             (triangle3 (vec3 2 0 0) (vec3 3 0 0) (vec3 2 1 0))))

     ;; Build BVH (max 5 triangles per leaf)
     (define bvh (bvh-build triangles 5))

     ;; Ray-triangle intersection
     (define ray (ray3 (vec3 0.25 0.25 -5) (vec3 0 0 1)))
     (define hit (bvh-intersect-ray bvh ray))

     (if hit
         (let ([triangle (car hit)]
               [t (cadr hit)])
              (display \"Hit triangle at t=\")
              (display t)
              (newline))
         (display \"No intersection\\n\"))"))

  (example
   (name "Creating a mesh and computing SDF")
   (code
    "(load \"lattice/geometry/mesh-sdf.ss\")

     ;; Create a cube mesh
     (define cube (make-mesh-cube 1.0))

     ;; Query signed distance at various points
     (define dist-inside (mesh-sdf cube (vec3 0 0 0)))      ; Negative
     (define dist-outside (mesh-sdf cube (vec3 3 0 0)))     ; Positive
     (define dist-surface (mesh-sdf cube (vec3 1 0 0)))     ; Near zero

     ;; Compute surface normal
     (define normal (mesh-sdf-gradient cube (vec3 1.1 0 0))) ; Points in +X

     (display \"Inside:  \") (display dist-inside)  (newline)
     (display \"Outside: \") (display dist-outside) (newline)
     (display \"Surface: \") (display dist-surface) (newline)"))

  (example
   (name "Raymarching a mesh SDF")
   (code
    "(load \"lattice/geometry/raymarch.ss\")

     ;; Create mesh
     (define mesh (make-mesh-cube 1.0))

     ;; Create ray
     (define ray (ray3 (vec3 0 0 -5) (vec3 0 0 1)))

     ;; Raymarch with default parameters
     (define result (raymarch-mesh mesh ray default-raymarch-params))

     (if result
         (let ([hit-point (car result)]
               [distance (cadr result)]
               [steps (caddr result)])
              (display \"Hit at: \") (write hit-point) (newline)
              (display \"Distance: \") (display distance) (newline)
              (display \"Steps: \") (display steps) (newline))
         (display \"No hit\\n\"))"))

  (example
   (name "Using BVH-accelerated raymarching")
   (code
    "(load \"lattice/geometry/raymarch.ss\")

     (define mesh (make-mesh-sphere-ico 1.0 0))
     (define ray (ray3 (vec3 0 0 -5) (vec3 0 0 1)))

     ;; Hybrid approach: BVH for coarse hit, then raymarch locally
     (define result (bvh-accelerated-raymarch mesh ray default-raymarch-params))

     (if result
         (let ([hit-point (car result)])
              ;; Compute shading
              (define normal (mesh-sdf-normal mesh hit-point))
              (define light-dir (vec3-normalize (vec3 1 1 -1)))
              (define view-dir (vec3-normalize (vec3-scale (ray3-direction ray) -1)))
              (define shading (simple-shading normal light-dir view-dir))

              (display \"Shading: \") (display shading) (newline))
         (display \"No hit\\n\"))"))

  (example
   (name "Creating an octree")
   (code
    "(load \"lattice/geometry/octree.ss\")

     (define triangles
       (list (triangle3 (vec3 0 0 0) (vec3 1 0 0) (vec3 0 1 0))
             (triangle3 (vec3 2 0 0) (vec3 3 0 0) (vec3 2 1 0))))

     ;; Build octree
     ;; center: (0,0,0), half-size: 5.0, max-depth: 5, max-leaf-size: 10
     (define octree (octree-build triangles (vec3 0 0 0) 5.0 5 10))

     ;; Query statistics
     (display \"Depth: \") (display (octree-depth octree)) (newline)
     (display \"Nodes: \") (display (octree-count-nodes octree)) (newline)
     (display \"Triangles: \") (display (octree-count-triangles octree)) (newline)"))

  (example
   (name "Validating mesh topology")
   (code
    "(load \"lattice/geometry/mesh-topology.ss\")

     ;; Create a cube mesh
     (define cube (make-mesh-cube 1.0))

     ;; Compute Betti numbers
     (define betti (mesh-betti-numbers cube))
     (display \"Betti numbers: \") (write betti) (newline)
     ;; => (1 0 1) — connected, no handles, one void

     ;; Check if watertight (suitable for physics simulation)
     (display \"Watertight? \") (display (mesh-is-watertight? cube)) (newline)
     ;; => #t

     ;; Verify sphere topology (genus 0)
     (display \"Sphere topology? \") (display (mesh-is-sphere-topology? cube)) (newline)
     ;; => #t

     ;; Check manifold properties
     (display \"Is manifold? \") (display (mesh-is-manifold? cube)) (newline)
     ;; => #t (passes both edge and vertex conditions)

     ;; Print full analysis
     (mesh-topology-summary cube)"))

  (example
   (name "Detecting mesh defects")
   (code
    "(load \"lattice/geometry/mesh-topology.ss\")

     ;; Identify boundary edges (mesh is open)
     (define boundary (mesh-boundary-edges some-mesh))
     (when (> (length boundary) 0)
       (display \"Mesh has boundary edges — not watertight!\\n\"))

     ;; Detect non-manifold edges (>2 triangles share edge)
     (define bad-edges (mesh-non-manifold-edges some-mesh))
     (when (> (length bad-edges) 0)
       (display \"Non-manifold edges detected!\\n\"))

     ;; Check for pinch points (vertex shared by disconnected surfaces)
     (unless (mesh-vertices-are-manifold? some-mesh)
       (display \"Mesh has pinch points — vertex link disconnected!\\n\"))")))

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
  (content
   "Comprehensive test suites are provided for all modules:")

  (test-file "test-bvh.ss"
             "Tests BVH construction, ray intersection, closest point queries")
  (test-file "test-mesh-sdf.ss"
             "Tests mesh creation, SDF computation, signed distance correctness")
  (test-file "test-raymarch.ss"
             "Tests raymarching algorithm, shading, soft shadows, ambient occlusion")
  (test-file "test-mesh-topology.ss"
             "Tests mesh-to-SC conversion, Betti numbers, manifold validation,
              genus detection, hourglass pinch-point detection")

  (run-tests
   "scheme --script lattice/geometry/test-mesh-topology.ss"))

 (section
  (title "References")

  (paper "Physically Based Rendering" "Pharr, Jakob, Humphreys" "2016"
         "Comprehensive coverage of BVH construction and traversal")

  (paper "Real-Time Collision Detection" "Ericson" "2004"
         "Practical algorithms for spatial data structures and geometric queries")

  (paper "Sphere Tracing" "Hart" "1996"
         "Original paper on raymarching implicit surfaces using SDFs"))

 (section
  (title "Future Enhancements")

  (enhancement "Surface Area Heuristic (SAH) for BVH"
               "Use SAH cost function for better split selection during BVH construction")

  (enhancement "SBVH (Spatial BVH)"
               "Hybrid BVH/octree approach with spatial splits for better quality")

  (enhancement "Mesh SDF caching"
               "Cache computed SDF values in a 3D grid for faster repeated queries")

  (enhancement "GPU acceleration"
               "Parallel BVH/octree traversal on GPU for real-time rendering")

  (enhancement "Icosphere vertex deduplication"
               "Fix floating-point vertex merging issues in icosphere construction")

  (enhancement "Wider range of primitives"
               "Support for CSG operations, implicit surfaces, bezier patches")

  (enhancement "Persistent homology"
               "Extend mesh-topology to support filtrations and persistence diagrams")))
