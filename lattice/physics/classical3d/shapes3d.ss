(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
;;; @module shapes3d
;;; @requires vec3
(require 'vec3)

(doc 'module 'shapes3d)
(doc 'description "3D Collision Shapes - Defines 3D shape primitives for collision detection: Sphere3D (center + radius), Box3D (center + half-extents, axis-aligned), AABB3D (min/max corners, axis-aligned bounding box).")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'section 'sphere3d)

;;; sphere3d : Vec3 × Number → Sphere3D
;;; Create a sphere with given center and radius.
(define (sphere3d center radius)
  (list 'sphere3d center radius))

;;; sphere3d? : Any → Boolean
(define (sphere3d? s)
  (and (pair? s) (eq? (car s) 'sphere3d)))

;;; sphere3d-center : Sphere3D → Vec3
(define (sphere3d-center s) (list-ref s 1))

;;; sphere3d-radius : Sphere3D → Number
(define (sphere3d-radius s) (list-ref s 2))

;;; sphere3d-diameter : Sphere3D → Number
(define (sphere3d-diameter s) (* 2 (sphere3d-radius s)))

;;; sphere3d-volume : Sphere3D → Number
;;; V = (4/3) * π * r³
(define (sphere3d-volume s)
  (let ([r (sphere3d-radius s)])
       (* (/ 4 3) 3.141592653589793 r r r)))

;;; sphere3d-surface-area : Sphere3D → Number
;;; A = 4 * π * r²
(define (sphere3d-surface-area s)
  (let ([r (sphere3d-radius s)])
       (* 4 3.141592653589793 r r)))

;;; sphere3d-aabb : Sphere3D → AABB3D
;;; Get axis-aligned bounding box.
(define (sphere3d-aabb s)
  (let ([c (sphere3d-center s)]
        [r (sphere3d-radius s)])
       (aabb3d (vec3-sub c (vec3 r r r))
               (vec3-add c (vec3 r r r)))))

;;; sphere3d-with-center : Sphere3D × Vec3 → Sphere3D
;;; Create new sphere with different center.
(define (sphere3d-with-center s new-center)
  (sphere3d new-center (sphere3d-radius s)))

;;; sphere3d-with-radius : Sphere3D × Number → Sphere3D
;;; Create new sphere with different radius.
(define (sphere3d-with-radius s new-radius)
  (sphere3d (sphere3d-center s) new-radius))

;;; sphere3d-contains-point? : Sphere3D × Vec3 → Boolean
;;; Check if point is inside or on sphere.
(define (sphere3d-contains-point? s p)
  (<= (vec3-distance-sq (sphere3d-center s) p)
      (* (sphere3d-radius s) (sphere3d-radius s))))

;;; ====
;;; Box3D (Axis-Aligned)
;;; ====

;;; box3d : Vec3 × Vec3 → Box3D
;;; Create a box with given center and half-extents.
;;; Half-extents = (half-width, half-height, half-depth).
(define (box3d center half-extents)
  (list 'box3d center half-extents))

;;; box3d? : Any → Boolean
(define (box3d? b)
  (and (pair? b) (eq? (car b) 'box3d)))

;;; box3d-center : Box3D → Vec3
(define (box3d-center b) (list-ref b 1))

;;; box3d-half-extents : Box3D → Vec3
(define (box3d-half-extents b) (list-ref b 2))

;;; box3d-extents : Box3D → Vec3
;;; Full extents (width, height, depth).
(define (box3d-extents b)
  (vec3-scale (box3d-half-extents b) 2))

;;; box3d-min : Box3D → Vec3
;;; Minimum corner.
(define (box3d-min b)
  (vec3-sub (box3d-center b) (box3d-half-extents b)))

;;; box3d-max : Box3D → Vec3
;;; Maximum corner.
(define (box3d-max b)
  (vec3-add (box3d-center b) (box3d-half-extents b)))

;;; box3d-volume : Box3D → Number
;;; V = 8 * hx * hy * hz
(define (box3d-volume b)
  (let ([h (box3d-half-extents b)])
       (* 8 (vec3-x h) (vec3-y h) (vec3-z h))))

;;; box3d-surface-area : Box3D → Number
;;; A = 8 * (hx*hy + hy*hz + hz*hx)
(define (box3d-surface-area b)
  (let* ([h (box3d-half-extents b)]
         [hx (vec3-x h)] [hy (vec3-y h)] [hz (vec3-z h)])
        (* 8 (+ (* hx hy) (* hy hz) (* hz hx)))))

;;; box3d-aabb : Box3D → AABB3D
;;; Get axis-aligned bounding box (same as box for axis-aligned box).
(define (box3d-aabb b)
  (aabb3d (box3d-min b) (box3d-max b)))

;;; box3d-with-center : Box3D × Vec3 → Box3D
;;; Create new box with different center.
(define (box3d-with-center b new-center)
  (box3d new-center (box3d-half-extents b)))

;;; box3d-with-half-extents : Box3D × Vec3 → Box3D
;;; Create new box with different half-extents.
(define (box3d-with-half-extents b new-half-extents)
  (box3d (box3d-center b) new-half-extents))

;;; box3d-contains-point? : Box3D × Vec3 → Boolean
;;; Check if point is inside or on box.
(define (box3d-contains-point? b p)
  (let ([c (box3d-center b)]
        [h (box3d-half-extents b)])
       (let ([dx (abs (- (vec3-x p) (vec3-x c)))]
             [dy (abs (- (vec3-y p) (vec3-y c)))]
             [dz (abs (- (vec3-z p) (vec3-z c)))])
            (and (<= dx (vec3-x h))
                 (<= dy (vec3-y h))
                 (<= dz (vec3-z h))))))

;;; box3d-corner : Box3D × Nat → Vec3
;;; Get one of the 8 corners by index (0-7).
;;; Index bits: bit0=x-sign, bit1=y-sign, bit2=z-sign
(define (box3d-corner b i)
  (let ([c (box3d-center b)]
        [h (box3d-half-extents b)])
       (let ([sx (if (= (bitwise-and i 1) 0) -1 1)]
             [sy (if (= (bitwise-and i 2) 0) -1 1)]
             [sz (if (= (bitwise-and i 4) 0) -1 1)])
            (vec3 (+ (vec3-x c) (* sx (vec3-x h)))
                  (+ (vec3-y c) (* sy (vec3-y h)))
                  (+ (vec3-z c) (* sz (vec3-z h)))))))

;;; box3d-corners : Box3D → (List Vec3)
;;; Get all 8 corners.
(define (box3d-corners b)
  (map (lambda (i) (box3d-corner b i)) (iota 8)))

;;; ====
;;; AABB3D (Axis-Aligned Bounding Box)
;;; ====

;;; aabb3d : Vec3 × Vec3 → AABB3D
;;; Create AABB from min and max corners.
(define (aabb3d min-corner max-corner)
  (list 'aabb3d min-corner max-corner))

;;; aabb3d? : Any → Boolean
(define (aabb3d? a)
  (and (pair? a) (eq? (car a) 'aabb3d)))

;;; aabb3d-min : AABB3D → Vec3
(define (aabb3d-min a) (list-ref a 1))

;;; aabb3d-max : AABB3D → Vec3
(define (aabb3d-max a) (list-ref a 2))

;;; aabb3d-center : AABB3D → Vec3
(define (aabb3d-center a)
  (vec3-scale (vec3-add (aabb3d-min a) (aabb3d-max a)) 0.5))

;;; aabb3d-half-extents : AABB3D → Vec3
(define (aabb3d-half-extents a)
  (vec3-scale (vec3-sub (aabb3d-max a) (aabb3d-min a)) 0.5))

;;; aabb3d-extents : AABB3D → Vec3
(define (aabb3d-extents a)
  (vec3-sub (aabb3d-max a) (aabb3d-min a)))

;;; aabb3d-volume : AABB3D → Number
(define (aabb3d-volume a)
  (let ([e (aabb3d-extents a)])
       (* (vec3-x e) (vec3-y e) (vec3-z e))))

;;; aabb3d-surface-area : AABB3D → Number
(define (aabb3d-surface-area a)
  (let* ([e (aabb3d-extents a)]
         [ex (vec3-x e)] [ey (vec3-y e)] [ez (vec3-z e)])
        (* 2 (+ (* ex ey) (* ey ez) (* ez ex)))))

;;; aabb3d-contains-point? : AABB3D × Vec3 → Boolean
(define (aabb3d-contains-point? a p)
  (let ([min-c (aabb3d-min a)]
        [max-c (aabb3d-max a)])
       (and (>= (vec3-x p) (vec3-x min-c)) (<= (vec3-x p) (vec3-x max-c))
            (>= (vec3-y p) (vec3-y min-c)) (<= (vec3-y p) (vec3-y max-c))
            (>= (vec3-z p) (vec3-z min-c)) (<= (vec3-z p) (vec3-z max-c)))))

;;; aabb3d-intersects? : AABB3D × AABB3D → Boolean
;;; Check if two AABBs intersect.
(define (aabb3d-intersects? a b)
  (let ([a-min (aabb3d-min a)] [a-max (aabb3d-max a)]
        [b-min (aabb3d-min b)] [b-max (aabb3d-max b)])
       (and (<= (vec3-x a-min) (vec3-x b-max)) (>= (vec3-x a-max) (vec3-x b-min))
            (<= (vec3-y a-min) (vec3-y b-max)) (>= (vec3-y a-max) (vec3-y b-min))
            (<= (vec3-z a-min) (vec3-z b-max)) (>= (vec3-z a-max) (vec3-z b-min)))))

;;; aabb3d-merge : AABB3D × AABB3D → AABB3D
;;; Create AABB that contains both inputs.
(define (aabb3d-merge a b)
  (aabb3d (vec3 (min (vec3-x (aabb3d-min a)) (vec3-x (aabb3d-min b)))
                (min (vec3-y (aabb3d-min a)) (vec3-y (aabb3d-min b)))
                (min (vec3-z (aabb3d-min a)) (vec3-z (aabb3d-min b))))
          (vec3 (max (vec3-x (aabb3d-max a)) (vec3-x (aabb3d-max b)))
                (max (vec3-y (aabb3d-max a)) (vec3-y (aabb3d-max b)))
                (max (vec3-z (aabb3d-max a)) (vec3-z (aabb3d-max b))))))

;;; aabb3d-expand : AABB3D × Number → AABB3D
;;; Expand AABB by delta in all directions.
(define (aabb3d-expand a delta)
  (let ([d (vec3 delta delta delta)])
       (aabb3d (vec3-sub (aabb3d-min a) d)
               (vec3-add (aabb3d-max a) d))))

;;; aabb3d-from-points : (List Vec3) → AABB3D
;;; Create smallest AABB containing all points.
(define (aabb3d-from-points points)
  (if (null? points)
      (aabb3d (vec3-zero) (vec3-zero))
      (let loop ([pts (cdr points)]
                 [min-p (car points)]
                 [max-p (car points)])
           (if (null? pts)
               (aabb3d min-p max-p)
               (let ([p (car pts)])
                    (loop (cdr pts)
                          (vec3 (min (vec3-x min-p) (vec3-x p))
                                (min (vec3-y min-p) (vec3-y p))
                                (min (vec3-z min-p) (vec3-z p)))
                          (vec3 (max (vec3-x max-p) (vec3-x p))
                                (max (vec3-y max-p) (vec3-y p))
                                (max (vec3-z max-p) (vec3-z p)))))))))

;;; ====
;;; Shape Utilities
;;; ====

;;; shape3d? : Any → Boolean
;;; Check if value is any 3D shape.
(define (shape3d? s)
  (or (sphere3d? s) (box3d? s) (aabb3d? s)))

;;; shape3d-aabb : Shape3D → AABB3D
;;; Get AABB for any shape.
(define (shape3d-aabb s)
  (cond
   [(sphere3d? s) (sphere3d-aabb s)]
   [(box3d? s) (box3d-aabb s)]
   [(aabb3d? s) s]
   [else (error 'shape3d-aabb "unknown shape type" s)]))

;;; shape3d-center : Shape3D → Vec3
;;; Get center of any shape.
(define (shape3d-center s)
  (cond
   [(sphere3d? s) (sphere3d-center s)]
   [(box3d? s) (box3d-center s)]
   [(aabb3d? s) (aabb3d-center s)]
   [else (error 'shape3d-center "unknown shape type" s)]))

;;; shape3d-contains-point? : Shape3D × Vec3 → Boolean
;;; Check if point is inside or on shape.
(define (shape3d-contains-point? s p)
  (cond
   [(sphere3d? s) (sphere3d-contains-point? s p)]
   [(box3d? s) (box3d-contains-point? s p)]
   [(aabb3d? s) (aabb3d-contains-point? s p)]
   [else (error 'shape3d-contains-point? "unknown shape type" s)]))
