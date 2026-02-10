(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
(require 'prelude)
(require 'geometry)

(doc 'module 'marching-cubes)
(doc 'description "Marching Cubes algorithm for extracting triangle meshes from implicit surfaces (SDFs)")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'provides "Extract triangle mesh approximating zero-level isosurface from SDF function")

(doc 'section 'marching-cubes-configuration)
(doc 'note "Edge table: which edges are intersected for each cube configuration; Edges numbered 0-11 (4 on bottom face, 4 on top face, 4 vertical)")

(doc 'export #t)
(define edge-table
  '#(#x0   #x109 #x203 #x30a #x406 #x50f #x605 #x70c
     #x80c #x905 #xa0f #xb06 #xc0a #xd03 #xe09 #xf00
     #x190 #x99  #x393 #x29a #x596 #x49f #x795 #x69c
     #x99c #x895 #xb9f #xa96 #xd9a #xc93 #xf99 #xe90
     #x230 #x339 #x33  #x13a #x636 #x73f #x435 #x53c
     #xa3c #xb35 #x83f #x936 #xe3a #xf33 #xc39 #xd30
     #x3a0 #x2a9 #x1a3 #xaa  #x7a6 #x6af #x5a5 #x4ac
     #xbac #xaa5 #x9af #x8a6 #xfaa #xea3 #xda9 #xca0
     #x460 #x569 #x663 #x76a #x66  #x16f #x265 #x36c
     #xc6c #xd65 #xe6f #xf66 #x86a #x963 #xa69 #xb60
     #x5f0 #x4f9 #x7f3 #x6fa #x1f6 #xff  #x3f5 #x2fc
     #xdfc #xcf5 #xfff #xef6 #x9fa #x8f3 #xbf9 #xaf0
     #x650 #x759 #x453 #x55a #x256 #x35f #x55  #x15c
     #xe5c #xf55 #xc5f #xd56 #xa5a #xb53 #x859 #x950
     #x7c0 #x6c9 #x5c3 #x4ca #x3c6 #x2cf #x1c5 #xcc
     #xfcc #xec5 #xdcf #xcc6 #xbca #xac3 #x9c9 #x8c0
     #x8c0 #x9c9 #xac3 #xbca #xcc6 #xdcf #xec5 #xfcc
     #xcc  #x1c5 #x2cf #x3c6 #x4ca #x5c3 #x6c9 #x7c0
     #x950 #x859 #xb53 #xa5a #xd56 #xc5f #xf55 #xe5c
     #x15c #x55  #x35f #x256 #x55a #x453 #x759 #x650
     #xaf0 #xbf9 #x8f3 #x9fa #xef6 #xfff #xcf5 #xdfc
     #x2fc #x3f5 #xff  #x1f6 #x6fa #x7f3 #x4f9 #x5f0
     #xb60 #xa69 #x963 #x86a #xf66 #xe6f #xd65 #xc6c
     #x36c #x265 #x16f #x66  #x76a #x663 #x569 #x460
     #xca0 #xda9 #xea3 #xfaa #x8a6 #x9af #xaa5 #xbac
     #x4ac #x5a5 #x6af #x7a6 #xaa  #x1a3 #x2a9 #x3a0
     #xd30 #xc39 #xf33 #xe3a #x936 #x83f #xb35 #xa3c
     #x53c #x435 #x73f #x636 #x13a #x33  #x339 #x230
     #xe90 #xf99 #xc93 #xd9a #xa96 #xb9f #x895 #x99c
     #x69c #x795 #x49f #x596 #x29a #x393 #x99  #x190
     #xf00 #xe09 #xd03 #xc0a #xb06 #xa0f #x905 #x80c
     #x70c #x605 #x50f #x406 #x30a #x203 #x109 #x0))

(doc 'note "Triangle table: simplified version (maps cube config to triangle count); full implementation would map to specific triangle configurations")

(doc 'export #t)
(define tri-table
  '#(0 1 1 2 1 2 2 3 1 2 2 3 2 3 3 2
     1 2 2 3 2 3 3 4 2 3 3 4 3 4 4 3
     1 2 2 3 2 3 3 4 2 3 3 4 3 4 4 3
     2 3 3 2 3 4 4 3 3 4 4 3 4 5 5 2
     1 2 2 3 2 3 3 4 2 3 3 4 3 4 4 3
     2 3 3 4 3 4 4 5 3 4 4 5 4 5 5 4
     2 3 3 4 3 4 2 3 3 4 4 5 4 5 3 2
     3 4 4 3 4 5 3 2 4 5 5 4 5 2 4 1
     1 2 2 3 2 3 3 4 2 3 3 4 3 4 4 3
     2 3 3 4 3 4 4 5 3 2 4 3 4 3 5 2
     2 3 3 4 3 4 4 5 3 4 4 5 4 5 5 4
     3 4 4 3 4 5 5 4 4 3 5 2 5 4 2 1
     2 3 3 4 3 4 4 5 3 4 4 5 2 3 3 2
     3 4 4 5 4 5 5 2 4 3 5 4 3 2 4 1
     3 4 4 5 4 5 3 4 4 5 5 2 3 4 2 1
     2 3 3 2 3 4 2 1 3 2 4 1 2 1 1 0))

(doc 'section 'grid-and-cube-utilities)

(define (marching-cubes-grid sdf-fn bounds resolution)
  (doc 'export #t)
  (doc 'type '(-> SDF-Function AABB Number (List Triangle3)))
  (doc 'description "Extract triangle mesh from SDF using marching cubes")
  (doc 'param 'sdf-fn "Implicit surface function (returns signed distance)")
  (doc 'param 'bounds "Bounding box for extraction")
  (doc 'param 'resolution "Number of grid cells per axis")
  (let* ([bmin (aabb-min bounds)]
         [bmax (aabb-max bounds)]
         [xmin (vec3-x bmin)] [ymin (vec3-y bmin)] [zmin (vec3-z bmin)]
         [xmax (vec3-x bmax)] [ymax (vec3-y bmax)] [zmax (vec3-z bmax)]
         [dx (/ (- xmax xmin) resolution)]
         [dy (/ (- ymax ymin) resolution)]
         [dz (/ (- zmax zmin) resolution)]
         [triangles '()])
        ;; Process each cube in the grid
        (do ([ix 0 (+ ix 1)])
            ((>= ix resolution) triangles)
            (do ([iy 0 (+ iy 1)])
                ((>= iy resolution))
                (do ([iz 0 (+ iz 1)])
                    ((>= iz resolution))
                    (let* ([x0 (+ xmin (* ix dx))]
                           [y0 (+ ymin (* iy dy))]
                           [z0 (+ zmin (* iz dz))]
                           [x1 (+ x0 dx)]
                           [y1 (+ y0 dy)]
                           [z1 (+ z0 dz)]
                           ;; Evaluate SDF at 8 cube corners
                           [corners (list
                                     (vec3 x0 y0 z0) (vec3 x1 y0 z0)
                                     (vec3 x1 y0 z1) (vec3 x0 y0 z1)
                                     (vec3 x0 y1 z0) (vec3 x1 y1 z0)
                                     (vec3 x1 y1 z1) (vec3 x0 y1 z1))]
                           [values (map sdf-fn corners)]
                           ;; Generate triangles for this cube
                           [cube-tris (marching-cubes-cube corners values)])
                          (set! triangles (append triangles cube-tris))))))))

(define (marching-cubes-cube corners values)
  (doc 'export #t)
  (doc 'type '(-> (List Point3) (List Number) (List Triangle3)))
  (doc 'description "Generate triangles for a single cube based on corner values")
  (let* ([cube-index (compute-cube-index values)]
         [edge-mask (vector-ref edge-table cube-index)])
        (if (or (= edge-mask 0) (= edge-mask #xfff))
            '()  ; Cube is entirely inside or outside
            ;; Compute edge intersections and generate triangles
            (let ([edge-points (compute-edge-intersections corners values edge-mask)])
                 ;; Simplified: generate triangles based on edge points
                 ;; Full implementation would use tri-table
                 (generate-cube-triangles edge-points cube-index)))))

(define (compute-cube-index values)
  (doc 'export #t)
  (doc 'type '(-> (List Number) Number))
  (doc 'description "Compute configuration index (0-255) based on which corners are inside")
  (let loop ([vals values] [i 0] [index 0])
       (if (null? vals)
           index
           (loop (cdr vals)
                 (+ i 1)
                 (if (< (car vals) 0)
                     (bitwise-ior index (bitwise-arithmetic-shift-left 1 i))
                     index)))))

(define (compute-edge-intersections corners values edge-mask)
  (doc 'export #t)
  (doc 'type '(-> (List Point3) (List Number) Number (List Point3)))
  (doc 'description "Compute intersection points on cube edges")
  (let ([edge-defs '((0 1) (1 2) (2 3) (3 0)  ; Bottom face
                     (4 5) (5 6) (6 7) (7 4)  ; Top face
                     (0 4) (1 5) (2 6) (3 7))]) ; Vertical edges
       (let loop ([edges edge-defs] [edge-idx 0] [points '()])
            (if (null? edges)
                (reverse points)
                (let* ([edge (car edges)]
                       [v0-idx (car edge)]
                       [v1-idx (cadr edge)])
                      (if (not (= 0 (bitwise-and edge-mask (bitwise-arithmetic-shift-left 1 edge-idx))))
                          ;; This edge is intersected
                          (let* ([p0 (list-ref corners v0-idx)]
                                 [p1 (list-ref corners v1-idx)]
                                 [val0 (list-ref values v0-idx)]
                                 [val1 (list-ref values v1-idx)]
                                 [t (/ (- val0) (- val1 val0))]  ; Linear interpolation
                                 [point (vec3-add p0 (vec3-scale (vec3-sub p1 p0) t))])
                                (loop (cdr edges) (+ edge-idx 1) (cons point points)))
                          (loop (cdr edges) (+ edge-idx 1) points)))))))

(define (generate-cube-triangles edge-points cube-index)
  (doc 'export #t)
  (doc 'type '(-> (List Point3) Number (List Triangle3)))
  (doc 'description "Generate triangles from edge intersection points; simplified version: just generates triangles from first 3+ points")
  (if (< (length edge-points) 3)
      '()
      (let ([num-tris (quotient (length edge-points) 3)])
           (let loop ([pts edge-points] [tris '()])
                (if (< (length pts) 3)
                    (reverse tris)
                    (let ([tri (triangle3 (car pts) (cadr pts) (caddr pts))])
                         (loop (cdddr pts) (cons tri tris))))))))

(doc 'section 'high-level-api)

(define (marching-cubes sdf-fn center size resolution)
  (doc 'export #t)
  (doc 'type '(-> SDF-Function Point3 Number Number (List Triangle3)))
  (doc 'description "Extract isosurface mesh from SDF")
  (doc 'param 'sdf-fn "Signed distance function")
  (doc 'param 'center "Center of extraction region")
  (doc 'param 'size "Half-size of extraction region")
  (doc 'param 'resolution "Grid resolution (cells per axis)")
  (let* ([bmin (vec3 (- (vec3-x center) size)
                     (- (vec3-y center) size)
                     (- (vec3-z center) size))]
         [bmax (vec3 (+ (vec3-x center) size)
                     (+ (vec3-y center) size)
                     (+ (vec3-z center) size))]
         [bounds (aabb bmin bmax)])
        (marching-cubes-grid sdf-fn bounds resolution)))

(define (marching-cubes-sphere center radius resolution)
  (doc 'export #t)
  (doc 'type '(-> Point3 Number Number (List Triangle3)))
  (doc 'description "Extract mesh for a sphere SDF")
  (let ([sdf-fn (lambda (p)
                        (- (vec3-length (vec3-sub p center)) radius))])
       (marching-cubes sdf-fn center (* radius 1.5) resolution)))

(define (marching-cubes-torus center major-radius minor-radius resolution)
  (doc 'export #t)
  (doc 'type '(-> Point3 Number Number Number (List Triangle3)))
  (doc 'description "Extract mesh for a torus SDF")
  (let ([sdf-fn (lambda (p)
                        (let* ([offset (vec3-sub p center)]
                               [x (vec3-x offset)]
                               [y (vec3-y offset)]
                               [z (vec3-z offset)]
                               [q (- (sqrt (+ (* x x) (* z z))) major-radius)])
                              (- (sqrt (+ (* q q) (* y y))) minor-radius)))])
       (marching-cubes sdf-fn center (+ major-radius minor-radius 1.0) resolution)))
