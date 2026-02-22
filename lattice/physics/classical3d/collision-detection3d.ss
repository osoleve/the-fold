(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
;;; @module collision-detection3d
;;; @requires prelude vec3 shapes3d
(require 'prelude)
(require 'vec3)
(require 'shapes3d)

(doc 'module 'collision-detection3d)
(doc 'description "3D Collision Detection - Comprehensive collision detection for 3D physics including: AABB3D (Axis-Aligned Bounding Box), Sphere primitives, Box3D (oriented box), Broad phase (spatial hash).")
(doc 'layer 'lattice)
(doc 'purity 'partial)
(doc 'note "Manifold structure: (list normal-vec3 penetration contact-vec3) where normal is unit vector from A toward B, penetration is positive overlap depth, contact is world-space contact point")

(doc 'section 'point-in-shape-tests)

;;; point-in-aabb3d? : Vec3 × AABB3D → Boolean
(define (point-in-aabb3d? point aabb)
  (let ([px (vec3-x point)]
        [py (vec3-y point)]
        [pz (vec3-z point)]
        [min-p (aabb3d-min aabb)]
        [max-p (aabb3d-max aabb)])
       (and (>= px (vec3-x min-p)) (<= px (vec3-x max-p))
            (>= py (vec3-y min-p)) (<= py (vec3-y max-p))
            (>= pz (vec3-z min-p)) (<= pz (vec3-z max-p)))))

;;; point-in-sphere3d? : Vec3 × Sphere3D → Boolean
(define (point-in-sphere3d? point sphere)
  (let ([d-sq (vec3-distance-sq point (sphere3d-center sphere))]
        [r (sphere3d-radius sphere)])
       (<= d-sq (* r r))))

;;; point-in-box3d? : Vec3 × Box3D → Boolean
;;; Test if point is inside axis-aligned box.
(define (point-in-box3d? point box)
  (let* ([center (box3d-center box)]
         [half (box3d-half-extents box)]
         [local (vec3-sub point center)])
        (and (<= (abs (vec3-x local)) (vec3-x half))
             (<= (abs (vec3-y local)) (vec3-y half))
             (<= (abs (vec3-z local)) (vec3-z half)))))

;;; ====
;;; Closest Point Utilities
;;; ====

;;; closest-point-on-segment-3d : Vec3 × Vec3 × Vec3 → Vec3
;;; Find closest point on line segment [v1, v2] to point.
(define (closest-point-on-segment-3d point v1 v2)
  (let* ([edge (vec3-sub v2 v1)]
         [edge-len-sq (vec3-magnitude-sq edge)]
         [to-point (vec3-sub point v1)]
         [t (if (< edge-len-sq 1e-10)
                0
                (/ (vec3-dot to-point edge) edge-len-sq))]
         [t-clamped (max 0 (min 1 t))])
        (vec3-add v1 (vec3-scale edge t-clamped))))

;;; closest-point-on-aabb3d : Vec3 × AABB3D → Vec3
;;; Find closest point on AABB to given point.
(define (closest-point-on-aabb3d point aabb)
  (let ([min-p (aabb3d-min aabb)]
        [max-p (aabb3d-max aabb)])
       (vec3 (max (vec3-x min-p) (min (vec3-x point) (vec3-x max-p)))
             (max (vec3-y min-p) (min (vec3-y point) (vec3-y max-p)))
             (max (vec3-z min-p) (min (vec3-z point) (vec3-z max-p))))))

;;; closest-point-on-box3d : Vec3 × Box3D → Vec3
;;; Find closest point on axis-aligned box to given point.
(define (closest-point-on-box3d point box)
  (let* ([center (box3d-center box)]
         [half (box3d-half-extents box)]
         [local (vec3-sub point center)]
         [clamped-x (max (- (vec3-x half)) (min (vec3-x local) (vec3-x half)))]
         [clamped-y (max (- (vec3-y half)) (min (vec3-y local) (vec3-y half)))]
         [clamped-z (max (- (vec3-z half)) (min (vec3-z local) (vec3-z half)))])
        (vec3-add center (vec3 clamped-x clamped-y clamped-z))))

;;; ====
;;; Collision Detection: Sphere vs Sphere
;;; ====

;;; sphere-sphere? : Sphere3D × Sphere3D → Boolean
;;; Test if two spheres overlap.
(define (sphere-sphere? a b)
  (let ([dist-sq (vec3-distance-sq (sphere3d-center a) (sphere3d-center b))]
        [r-sum (+ (sphere3d-radius a) (sphere3d-radius b))])
       (<= dist-sq (* r-sum r-sum))))

;;; sphere-sphere-manifold : Sphere3D × Sphere3D → Manifold | #f
;;; Generate collision manifold for two spheres.
;;; Returns (normal penetration contact) or #f if no collision.
(define (sphere-sphere-manifold a b)
  (let* ([center-a (sphere3d-center a)]
         [center-b (sphere3d-center b)]
         [radius-a (sphere3d-radius a)]
         [radius-b (sphere3d-radius b)]
         [diff (vec3-sub center-b center-a)]
         [dist-sq (vec3-magnitude-sq diff)]
         [r-sum (+ radius-a radius-b)])
        (if (> dist-sq (* r-sum r-sum))
            #f
            (let* ([dist (sqrt dist-sq)]
                   [normal (if (< dist 0.0001)
                               (vec3 1 0 0)  ; Arbitrary for coincident
                               (vec3-scale diff (/ 1 dist)))]
                   [penetration (- r-sum dist)]
                   [contact (vec3-add center-a
                                      (vec3-scale normal radius-a))])
                  (list normal penetration contact)))))

;;; ====
;;; Collision Detection: AABB3D vs AABB3D
;;; ====

;;; aabb3d-aabb3d? : AABB3D × AABB3D → Boolean
;;; Test if two AABBs overlap.
(define (aabb3d-aabb3d? a b)
  (let ([a-min (aabb3d-min a)]
        [a-max (aabb3d-max a)]
        [b-min (aabb3d-min b)]
        [b-max (aabb3d-max b)])
       (and (<= (vec3-x a-min) (vec3-x b-max))
            (>= (vec3-x a-max) (vec3-x b-min))
            (<= (vec3-y a-min) (vec3-y b-max))
            (>= (vec3-y a-max) (vec3-y b-min))
            (<= (vec3-z a-min) (vec3-z b-max))
            (>= (vec3-z a-max) (vec3-z b-min)))))

;;; aabb3d-aabb3d-manifold : AABB3D × AABB3D → Manifold | #f
;;; Generate collision manifold for two AABBs.
;;; Returns (normal penetration contact) or #f if no collision.
(define (aabb3d-aabb3d-manifold a b)
  (if (not (aabb3d-aabb3d? a b))
      #f
      (let* ([a-center (aabb3d-center a)]
             [b-center (aabb3d-center b)]
             [a-half (aabb3d-half-extents a)]
             [b-half (aabb3d-half-extents b)]
             ;; Calculate overlap on each axis
             [diff (vec3-sub b-center a-center)]
             [overlap-x (- (+ (vec3-x a-half) (vec3-x b-half))
                           (abs (vec3-x diff)))]
             [overlap-y (- (+ (vec3-y a-half) (vec3-y b-half))
                           (abs (vec3-y diff)))]
             [overlap-z (- (+ (vec3-z a-half) (vec3-z b-half))
                           (abs (vec3-z diff)))]
             ;; Find minimum overlap axis
             [min-overlap (min overlap-x (min overlap-y overlap-z))])
            (cond
             ;; X-axis has minimum overlap
             [(= min-overlap overlap-x)
              (let* ([sign-x (if (< (vec3-x diff) 0) -1 1)]
                     [normal (vec3 sign-x 0 0)]
                     [contact (vec3 (+ (vec3-x a-center)
                                       (* (vec3-x a-half) sign-x))
                                    (vec3-y b-center)
                                    (vec3-z b-center))])
                    (list normal overlap-x contact))]
             ;; Y-axis has minimum overlap
             [(= min-overlap overlap-y)
              (let* ([sign-y (if (< (vec3-y diff) 0) -1 1)]
                     [normal (vec3 0 sign-y 0)]
                     [contact (vec3 (vec3-x b-center)
                                    (+ (vec3-y a-center)
                                       (* (vec3-y a-half) sign-y))
                                    (vec3-z b-center))])
                    (list normal overlap-y contact))]
             ;; Z-axis has minimum overlap
             [else
              (let* ([sign-z (if (< (vec3-z diff) 0) -1 1)]
                     [normal (vec3 0 0 sign-z)]
                     [contact (vec3 (vec3-x b-center)
                                    (vec3-y b-center)
                                    (+ (vec3-z a-center)
                                       (* (vec3-z a-half) sign-z)))])
                    (list normal overlap-z contact))]))))

;;; ====
;;; Collision Detection: Sphere vs AABB3D
;;; ====

;;; sphere-aabb3d? : Sphere3D × AABB3D → Boolean
;;; Test if sphere and AABB overlap.
(define (sphere-aabb3d? sphere aabb)
  (let* ([center (sphere3d-center sphere)]
         [r (sphere3d-radius sphere)]
         [closest (closest-point-on-aabb3d center aabb)]
         [dist-sq (vec3-distance-sq center closest)])
        (<= dist-sq (* r r))))

;;; sphere-aabb3d-manifold : Sphere3D × AABB3D → Manifold | #f
;;; Generate collision manifold for sphere vs AABB.
(define (sphere-aabb3d-manifold sphere aabb)
  (let* ([center (sphere3d-center sphere)]
         [r (sphere3d-radius sphere)]
         [closest (closest-point-on-aabb3d center aabb)]
         [diff (vec3-sub center closest)]
         [dist-sq (vec3-magnitude-sq diff)])
        (if (> dist-sq (* r r))
            #f
            (let* ([dist (sqrt dist-sq)]
                   [normal (if (< dist 0.0001)
                               ;; Sphere center inside AABB
                               (sphere-inside-aabb-normal center aabb)
                               (vec3-scale diff (/ 1 dist)))]
                   [penetration (- r dist)])
                  (list normal penetration closest)))))

;;; sphere-inside-aabb-normal : Vec3 × AABB3D → Vec3
;;; Compute normal when sphere center is inside AABB.
(define (sphere-inside-aabb-normal center aabb)
  (let* ([min-p (aabb3d-min aabb)]
         [max-p (aabb3d-max aabb)]
         [cx (vec3-x center)]
         [cy (vec3-y center)]
         [cz (vec3-z center)]
         ;; Distance to each face
         [dx-min (- cx (vec3-x min-p))]
         [dx-max (- (vec3-x max-p) cx)]
         [dy-min (- cy (vec3-y min-p))]
         [dy-max (- (vec3-y max-p) cy)]
         [dz-min (- cz (vec3-z min-p))]
         [dz-max (- (vec3-z max-p) cz)]
         [min-dist (min dx-min (min dx-max (min dy-min (min dy-max (min dz-min dz-max)))))])
        (cond
         [(= min-dist dx-min) (vec3 -1 0 0)]
         [(= min-dist dx-max) (vec3 1 0 0)]
         [(= min-dist dy-min) (vec3 0 -1 0)]
         [(= min-dist dy-max) (vec3 0 1 0)]
         [(= min-dist dz-min) (vec3 0 0 -1)]
         [else (vec3 0 0 1)])))

;;; ====
;;; Collision Detection: Sphere vs Box3D
;;; ====

;;; sphere-box3d? : Sphere3D × Box3D → Boolean
;;; Test if sphere and box overlap.
(define (sphere-box3d? sphere box)
  (let* ([center (sphere3d-center sphere)]
         [r (sphere3d-radius sphere)]
         [closest (closest-point-on-box3d center box)]
         [dist-sq (vec3-distance-sq center closest)])
        (<= dist-sq (* r r))))

;;; sphere-box3d-manifold : Sphere3D × Box3D → Manifold | #f
;;; Generate collision manifold for sphere vs box.
(define (sphere-box3d-manifold sphere box)
  (let* ([center (sphere3d-center sphere)]
         [r (sphere3d-radius sphere)]
         [closest (closest-point-on-box3d center box)]
         [diff (vec3-sub center closest)]
         [dist-sq (vec3-magnitude-sq diff)])
        (if (> dist-sq (* r r))
            #f
            (let* ([dist (sqrt dist-sq)]
                   [normal (if (< dist 0.0001)
                               ;; Sphere center inside box
                               (sphere-inside-box-normal center box)
                               (vec3-scale diff (/ 1 dist)))]
                   [penetration (- r dist)])
                  (list normal penetration closest)))))

;;; sphere-inside-box-normal : Vec3 × Box3D → Vec3
;;; Compute normal when sphere center is inside box.
(define (sphere-inside-box-normal center box)
  (let* ([box-center (box3d-center box)]
         [half (box3d-half-extents box)]
         [local (vec3-sub center box-center)]
         ;; Distance to each face
         [dx-neg (+ (vec3-x local) (vec3-x half))]
         [dx-pos (- (vec3-x half) (vec3-x local))]
         [dy-neg (+ (vec3-y local) (vec3-y half))]
         [dy-pos (- (vec3-y half) (vec3-y local))]
         [dz-neg (+ (vec3-z local) (vec3-z half))]
         [dz-pos (- (vec3-z half) (vec3-z local))]
         [min-dist (min dx-neg (min dx-pos (min dy-neg (min dy-pos (min dz-neg dz-pos)))))])
        (cond
         [(= min-dist dx-neg) (vec3 -1 0 0)]
         [(= min-dist dx-pos) (vec3 1 0 0)]
         [(= min-dist dy-neg) (vec3 0 -1 0)]
         [(= min-dist dy-pos) (vec3 0 1 0)]
         [(= min-dist dz-neg) (vec3 0 0 -1)]
         [else (vec3 0 0 1)])))

;;; ====
;;; Collision Detection: Box3D vs Box3D (AABB version)
;;; ====

;;; box3d-box3d? : Box3D × Box3D → Boolean
;;; Test if two axis-aligned boxes overlap.
(define (box3d-box3d? a b)
  (aabb3d-aabb3d? (box3d-aabb a) (box3d-aabb b)))

;;; box3d-box3d-manifold : Box3D × Box3D → Manifold | #f
;;; Generate collision manifold for two axis-aligned boxes.
(define (box3d-box3d-manifold a b)
  (let* ([a-aabb (box3d-aabb a)]
         [b-aabb (box3d-aabb b)])
        (aabb3d-aabb3d-manifold a-aabb b-aabb)))

;;; ====
;;; Generic Shape Dispatch
;;; ====

;;; shape-type-3d : Shape3D → Symbol
(define (shape-type-3d s)
  (cond
   [(sphere3d? s) 'sphere3d]
   [(box3d? s) 'box3d]
   [(aabb3d? s) 'aabb3d]
   [else 'unknown]))

;;; shape-aabb-3d : Shape3D → AABB3D
;;; Get bounding AABB for any shape.
(define (shape-aabb-3d s)
  (cond
   [(sphere3d? s) (sphere3d-aabb s)]
   [(box3d? s) (box3d-aabb s)]
   [(aabb3d? s) s]
   [else (error 'shape-aabb-3d "Unknown shape type")]))

;;; shapes-collide-3d? : Shape3D × Shape3D → Boolean
;;; Test if two shapes overlap.
(define (shapes-collide-3d? a b)
  (let ([type-a (shape-type-3d a)]
        [type-b (shape-type-3d b)])
       (cond
        ;; Sphere-Sphere
        [(and (eq? type-a 'sphere3d) (eq? type-b 'sphere3d))
         (sphere-sphere? a b)]
        ;; Sphere-AABB
        [(and (eq? type-a 'sphere3d) (eq? type-b 'aabb3d))
         (sphere-aabb3d? a b)]
        [(and (eq? type-a 'aabb3d) (eq? type-b 'sphere3d))
         (sphere-aabb3d? b a)]
        ;; Sphere-Box
        [(and (eq? type-a 'sphere3d) (eq? type-b 'box3d))
         (sphere-box3d? a b)]
        [(and (eq? type-a 'box3d) (eq? type-b 'sphere3d))
         (sphere-box3d? b a)]
        ;; AABB-AABB
        [(and (eq? type-a 'aabb3d) (eq? type-b 'aabb3d))
         (aabb3d-aabb3d? a b)]
        ;; Box-Box
        [(and (eq? type-a 'box3d) (eq? type-b 'box3d))
         (box3d-box3d? a b)]
        ;; Box-AABB (convert box to AABB)
        [(and (eq? type-a 'box3d) (eq? type-b 'aabb3d))
         (aabb3d-aabb3d? (box3d-aabb a) b)]
        [(and (eq? type-a 'aabb3d) (eq? type-b 'box3d))
         (aabb3d-aabb3d? a (box3d-aabb b))]
        ;; Default: use AABB overlap
        [else (aabb3d-aabb3d? (shape-aabb-3d a) (shape-aabb-3d b))])))

;;; shapes-manifold-3d : Shape3D × Shape3D → Manifold | #f
;;; Generate collision manifold for any shape pair.
(define (shapes-manifold-3d a b)
  (let ([type-a (shape-type-3d a)]
        [type-b (shape-type-3d b)])
       (cond
        ;; Sphere-Sphere
        [(and (eq? type-a 'sphere3d) (eq? type-b 'sphere3d))
         (sphere-sphere-manifold a b)]
        ;; Sphere-AABB
        [(and (eq? type-a 'sphere3d) (eq? type-b 'aabb3d))
         (sphere-aabb3d-manifold a b)]
        [(and (eq? type-a 'aabb3d) (eq? type-b 'sphere3d))
         (let ([m (sphere-aabb3d-manifold b a)])
              (and m (list (vec3-neg (car m)) (cadr m) (caddr m))))]
        ;; Sphere-Box
        [(and (eq? type-a 'sphere3d) (eq? type-b 'box3d))
         (sphere-box3d-manifold a b)]
        [(and (eq? type-a 'box3d) (eq? type-b 'sphere3d))
         (let ([m (sphere-box3d-manifold b a)])
              (and m (list (vec3-neg (car m)) (cadr m) (caddr m))))]
        ;; AABB-AABB
        [(and (eq? type-a 'aabb3d) (eq? type-b 'aabb3d))
         (aabb3d-aabb3d-manifold a b)]
        ;; Box-Box
        [(and (eq? type-a 'box3d) (eq? type-b 'box3d))
         (box3d-box3d-manifold a b)]
        ;; Default: use AABB manifold
        [else (aabb3d-aabb3d-manifold (shape-aabb-3d a) (shape-aabb-3d b))])))

;;; ====
;;; Manifold Accessors
;;; ====

;;; manifold-normal : Manifold → Vec3
(define (manifold-normal m) (car m))

;;; manifold-penetration : Manifold → Number
(define (manifold-penetration m) (cadr m))

;;; manifold-contact : Manifold → Vec3
(define (manifold-contact m) (caddr m))

;;; ====
;;; Spatial Hash for Broad Phase
;;; ====

;;; cell-key-3d : (Int × Int × Int) → Integer
;;; Convert cell coordinates to a unique integer key.
(define (cell-key-3d cell)
  (let ([x (car cell)]
        [y (cadr cell)]
        [z (caddr cell)])
       ;; Use a large range to avoid collisions
       ;; Assumes coordinates are in reasonable range [-10000, 10000]
       (+ (* (+ x 100000) 200001 200001)
          (* (+ y 100000) 200001)
          (+ z 100000))))

;;; make-spatial-hash-3d : Number → SpatialHash3D
(define (make-spatial-hash-3d cell-size)
  (list 'spatial-hash-3d cell-size (make-eqv-hashtable)))

;;; spatial-hash-3d-cell-size : SpatialHash3D → Number
(define (spatial-hash-3d-cell-size sh) (list-ref sh 1))

;;; spatial-hash-3d-table : SpatialHash3D → Hashtable
(define (spatial-hash-3d-table sh) (list-ref sh 2))

;;; point-to-cell-3d : Vec3 × Number → (Int × Int × Int)
;;; Convert point to cell coordinates.
(define (point-to-cell-3d point cell-size)
  (list (exact (floor (/ (vec3-x point) cell-size)))
        (exact (floor (/ (vec3-y point) cell-size)))
        (exact (floor (/ (vec3-z point) cell-size)))))

;;; aabb-to-cells-3d : AABB3D × Number → (List (Int × Int × Int))
;;; Get all cells that an AABB overlaps.
(define (aabb-to-cells-3d aabb cell-size)
  (let* ([min-cell (point-to-cell-3d (aabb3d-min aabb) cell-size)]
         [max-cell (point-to-cell-3d (aabb3d-max aabb) cell-size)]
         [min-x (car min-cell)]
         [min-y (cadr min-cell)]
         [min-z (caddr min-cell)]
         [max-x (car max-cell)]
         [max-y (cadr max-cell)]
         [max-z (caddr max-cell)])
        (let x-loop ([x min-x] [cells '()])
             (if (> x max-x)
                 cells
                 (x-loop (+ x 1)
                         (let y-loop ([y min-y] [cells cells])
                              (if (> y max-y)
                                  cells
                                  (y-loop (+ y 1)
                                          (let z-loop ([z min-z] [cells cells])
                                               (if (> z max-z)
                                                   cells
                                                   (z-loop (+ z 1)
                                                           (cons (list x y z) cells))))))))))))

;;; spatial-hash-3d-insert! : SpatialHash3D × Any × AABB3D → Unit
;;; Insert object into spatial hash using its AABB.
(define (spatial-hash-3d-insert! sh obj aabb)
  (let* ([cell-size (spatial-hash-3d-cell-size sh)]
         [table (spatial-hash-3d-table sh)]
         [cells (aabb-to-cells-3d aabb cell-size)])
        (for-each (lambda (cell)
                          (let* ([key (cell-key-3d cell)]
                                 [existing (hashtable-ref table key '())])
                                (hashtable-set! table key (cons obj existing))))
                  cells)))

;;; spatial-hash-3d-query : SpatialHash3D × AABB3D → (List Any)
;;; Query for objects that may overlap with given AABB.
(define (spatial-hash-3d-query sh aabb)
  (let* ([cell-size (spatial-hash-3d-cell-size sh)]
         [table (spatial-hash-3d-table sh)]
         [cells (aabb-to-cells-3d aabb cell-size)]
         [results '()]
         [seen (make-eq-hashtable)])
        (for-each (lambda (cell)
                          (let* ([key (cell-key-3d cell)]
                                 [objs (hashtable-ref table key '())])
                                (for-each (lambda (obj)
                                                  (unless (hashtable-ref seen obj #f)
                                                          (hashtable-set! seen obj #t)
                                                          (set! results (cons obj results))))
                                          objs)))
                  cells)
        results))

;;; spatial-hash-3d-clear! : SpatialHash3D → Unit
;;; Clear all entries from spatial hash.
(define (spatial-hash-3d-clear! sh)
  (hashtable-clear! (spatial-hash-3d-table sh)))

;;; ====
;;; Broad Phase Collision Pair Generation
;;; ====

;;; pair-key-3d : Any × Any → String
;;; Create a canonical key for an unordered pair.
(define (pair-key-3d a b)
  ;; Use string representation for comparison
  (let ([sa (format "~s" a)]
        [sb (format "~s" b)])
       (if (string<? sa sb)
           (string-append sa "|" sb)
           (string-append sb "|" sa))))

;;; find-collision-pairs-3d : (List (Object × Shape3D)) × SpatialHash3D → (List (Object × Object))
;;; Find all potentially colliding pairs using spatial hash.
(define (find-collision-pairs-3d items sh)
  (spatial-hash-3d-clear! sh)
  ;; Insert all items
  (for-each (lambda (item)
                    (let ([obj (car item)]
                          [shape (cdr item)])
                         (spatial-hash-3d-insert! sh obj (shape-aabb-3d shape))))
            items)
  ;; Find pairs - use string keys for seen table
  (let ([pairs '()]
        [seen (make-hashtable string-hash string=?)])
       (for-each (lambda (item)
                         (let* ([obj-a (car item)]
                                [shape-a (cdr item)]
                                [candidates (spatial-hash-3d-query sh (shape-aabb-3d shape-a))])
                               (for-each (lambda (obj-b)
                                                 (let ([key (pair-key-3d obj-a obj-b)])
                                                      (when (and (not (eq? obj-a obj-b))
                                                                 (not (hashtable-ref seen key #f)))
                                                            (hashtable-set! seen key #t)
                                                            (set! pairs (cons (cons obj-a obj-b) pairs)))))
                                         candidates)))
                 items)
       pairs))

;;; narrow-phase-3d : (Object × Object) × (Object → Shape3D) → Manifold | #f
;;; Perform narrow phase collision detection on a pair.
(define (narrow-phase-3d pair get-shape)
  (let ([shape-a (get-shape (car pair))]
        [shape-b (get-shape (cdr pair))])
       (shapes-manifold-3d shape-a shape-b)))
