;;; skills/geometry.sexp — Skill curation file

(skill geometry
  (description
    "2D/3D computational geometry library providing primitives, spatial data structures,\n    raymarching, mesh generation, and mesh topology analysis. Includes points, lines,\n    rays, planes, triangles, spheres, AABBs, OBBs, transforms, distance queries,\n    intersection tests, BVH and octree acceleration structures, mesh SDF computation,\n    marching cubes isosurface extraction, Delaunay triangulation with refinement,\n    Laplacian mesh smoothing, adaptive refinement, boundary-constrained meshing,\n    Voronoi diagrams (via Delaunay duality) with Lloyd relaxation,\n    topological validation via Betti numbers, and 2D convex hull algorithms\n    (Graham scan, Quickhull) with Minkowski operations.")

  (keywords (geometry 2d 3d raymarching sdf bvh octree mesh intersection transform marching-cubes rendering ray-tracing spatial-data-structures topology homology betti-numbers manifold validation delaunay triangulation mesh-generation refinement voronoi lloyd-relaxation laplacian-smoothing adaptive-refinement boundary-constrained polygon-mesh convex-hull graham-scan quickhull minkowski collision-detection optics lenses traversals prisms functional-access))

  (aliases (geom 3d-geometry spatial))

  (concepts (computational-geometry rendering-geometry mesh-processing))

  (modules
    geometry
    raymarch
    octree
    bvh
    obj-loader
    ascii-render
    mesh-sdf
    marching-cubes
    mesh-gen
    mesh-topology
    convex-hull
    voronoi
    geometry-optics
    shape-protocol
  )

  ;; Curated Q&A pairs for LLM context
  (prompts)

  ;; KG/search queries an agent should try
  (suggested-queries)
)
