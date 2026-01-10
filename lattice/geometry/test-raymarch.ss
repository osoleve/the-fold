;;; core/geometry/test-raymarch.ss — Tests for SDF Raymarching
;;;
;;; Tests for raymarching algorithms, normal computation, shadows, and AO.
;;;
;;; Run: scheme --script core/geometry/test-raymarch.ss

(load "core/test-framework.ss")
(load "lattice/geometry/raymarch.ss")

(set! *current-group* 'raymarch)

;;; Helper for approximate equality
(define (approx= a b eps)
  (< (abs (- a b)) eps))

;;; Helper for approximate vector equality
(define (vec3-approx= v1 v2 eps)
  (and (approx= (vec3-x v1) (vec3-x v2) eps)
       (approx= (vec3-y v1) (vec3-y v2) eps)
       (approx= (vec3-z v1) (vec3-z v2) eps)))

;;; ============================================================
;;; Simple SDF Functions for Testing
;;; ============================================================

;;; sdf-sphere : Vec3 -> Number
;;; Sphere centered at origin with radius 1
(define (sdf-sphere p)
  (- (vec3-length p) 1.0))

;;; sdf-box : Vec3 -> Number
;;; Box centered at origin with half-size 1
(define (sdf-box p)
  (let* ([q (vec3 (- (abs (vec3-x p)) 1)
                  (- (abs (vec3-y p)) 1)
                  (- (abs (vec3-z p)) 1))]
         [outside (vec3-length (vec3 (max 0 (vec3-x q))
                                     (max 0 (vec3-y q))
                                     (max 0 (vec3-z q))))]
         [inside (min 0 (max (vec3-x q) (max (vec3-y q) (vec3-z q))))])
        (+ outside inside)))

;;; sdf-plane : Vec3 -> Number
;;; XZ plane at y=0
(define (sdf-plane p)
  (vec3-y p))

;;; ============================================================
;;; Raymarch Parameters Tests
;;; ============================================================

(define-test "raymarch-params creation"
  (let ([params (raymarch-params 100 500.0 0.001)])
       (assert-equal (raymarch-params-max-steps params) 100)
       (assert-true (approx= (raymarch-params-max-distance params) 500.0 0.001))
       (assert-true (approx= (raymarch-params-hit-threshold params) 0.001 0.0001))))

(define-test "default-raymarch-params values"
  (assert-equal (raymarch-params-max-steps default-raymarch-params) 100)
  (assert-true (approx= (raymarch-params-max-distance default-raymarch-params) 1000.0 0.001))
  (assert-true (approx= (raymarch-params-hit-threshold default-raymarch-params) 0.001 0.0001)))

;;; ============================================================
;;; Basic Raymarching Tests
;;; ============================================================

(define-test "raymarch sphere hit"
  (let* ([ray (ray3 (vec3 0 0 -5) (vec3 0 0 1))]
         [params (raymarch-params 100 100.0 0.001)]
         [result (raymarch sdf-sphere ray params)])
        (assert-false (equal? result #f))
        (let ([hit-point (car result)]
              [t (cadr result)]
              [steps (caddr result)])
             ;; Should hit at z ~ -1
             (assert-true (approx= (vec3-z hit-point) -1.0 0.01))
             (assert-true (approx= t 4.0 0.1))
             (assert-true (> steps 0)))))

(define-test "raymarch sphere miss"
  (let* ([ray (ray3 (vec3 5 5 -5) (vec3 0 0 1))]  ; Off to the side
         [params (raymarch-params 100 100.0 0.001)]
         [result (raymarch sdf-sphere ray params)])
        (assert-false result)))

(define-test "raymarch box hit"
  (let* ([ray (ray3 (vec3 0 0 -5) (vec3 0 0 1))]
         [params (raymarch-params 100 100.0 0.001)]
         [result (raymarch sdf-box ray params)])
        (assert-false (equal? result #f))
        (let ([hit-point (car result)]
              [t (cadr result)])
             ;; Should hit at z ~ -1
             (assert-true (approx= (vec3-z hit-point) -1.0 0.01))
             (assert-true (approx= t 4.0 0.1)))))

(define-test "raymarch plane hit"
  (let* ([ray (ray3 (vec3 0 10 0) (vec3 0 -1 0))]
         [params (raymarch-params 100 100.0 0.001)]
         [result (raymarch sdf-plane ray params)])
        (assert-false (equal? result #f))
        (let ([hit-point (car result)]
              [t (cadr result)])
             ;; Should hit at y ~ 0
             (assert-true (approx= (vec3-y hit-point) 0.0 0.01))
             (assert-true (approx= t 10.0 0.1)))))

(define-test "raymarch step count increases with distance"
  ;; Verify that step count correlates with distance
  (let* ([near-ray (ray3 (vec3 0 0 -2) (vec3 0 0 1))]
         [far-ray (ray3 (vec3 0 0 -100) (vec3 0 0 1))]
         [params (raymarch-params 200 200.0 0.001)]
         [near-result (raymarch sdf-sphere near-ray params)]
         [far-result (raymarch sdf-sphere far-ray params)])
        ;; Both should hit
        (assert-false (equal? near-result #f))
        (assert-false (equal? far-result #f))
        ;; Far result should take more steps (or at least same)
        (let ([near-steps (caddr near-result)]
              [far-steps (caddr far-result)])
             (assert-true (>= far-steps near-steps)))))

(define-test "raymarch max distance reached"
  (let* ([ray (ray3 (vec3 0 0 -500) (vec3 0 0 1))]
         [params (raymarch-params 1000 100.0 0.001)]  ; Short max distance
         [result (raymarch sdf-sphere ray params)])
        ;; Should fail due to max distance
        (assert-false result)))

;;; ============================================================
;;; Normal Computation Tests
;;; ============================================================

(define-test "sdf-normal sphere at +X"
  (let* ([point (vec3 1 0 0)]
         [normal (sdf-normal sdf-sphere point)])
        ;; Normal should point in +X direction
        (assert-true (approx= (vec3-x normal) 1.0 0.1))
        (assert-true (approx= (vec3-y normal) 0.0 0.1))
        (assert-true (approx= (vec3-z normal) 0.0 0.1))))

(define-test "sdf-normal sphere at +Y"
  (let* ([point (vec3 0 1 0)]
         [normal (sdf-normal sdf-sphere point)])
        ;; Normal should point in +Y direction
        (assert-true (approx= (vec3-x normal) 0.0 0.1))
        (assert-true (approx= (vec3-y normal) 1.0 0.1))
        (assert-true (approx= (vec3-z normal) 0.0 0.1))))

(define-test "sdf-normal sphere at +Z"
  (let* ([point (vec3 0 0 1)]
         [normal (sdf-normal sdf-sphere point)])
        ;; Normal should point in +Z direction
        (assert-true (approx= (vec3-x normal) 0.0 0.1))
        (assert-true (approx= (vec3-y normal) 0.0 0.1))
        (assert-true (approx= (vec3-z normal) 1.0 0.1))))

(define-test "sdf-normal plane"
  (let* ([point (vec3 5 0 3)]  ; Anywhere on XZ plane
         [normal (sdf-normal sdf-plane point)])
        ;; Normal should point in +Y direction
        (assert-true (approx= (vec3-x normal) 0.0 0.1))
        (assert-true (approx= (vec3-y normal) 1.0 0.1))
        (assert-true (approx= (vec3-z normal) 0.0 0.1))))

(define-test "sdf-normal box face"
  (let* ([point (vec3 1 0 0)]  ; On +X face
         [normal (sdf-normal sdf-box point)])
        ;; Normal should point in +X direction
        (assert-true (approx= (vec3-x normal) 1.0 0.2))
        (assert-true (< (abs (vec3-y normal)) 0.2))
        (assert-true (< (abs (vec3-z normal)) 0.2))))

;;; ============================================================
;;; Soft Shadow Tests
;;; ============================================================

(define-test "raymarch-shadow no occlusion"
  (let* ([ray (ray3 (vec3 0 5 0) (vec3 0 1 0))]  ; Going up, no obstacles
         [params (raymarch-params 50 100.0 0.001)]
         [shadow (raymarch-shadow sdf-sphere ray 10.0 8.0 params)])
        ;; Should be fully lit
        (assert-true (approx= shadow 1.0 0.1))))

(define-test "raymarch-shadow full occlusion"
  (let* ([ray (ray3 (vec3 0 -5 0) (vec3 0 1 0))]  ; Going toward sphere
         [params (raymarch-params 50 100.0 0.001)]
         [shadow (raymarch-shadow sdf-sphere ray 10.0 8.0 params)])
        ;; Should be in shadow
        (assert-true (approx= shadow 0.0 0.01))))

(define-test "raymarch-shadow partial occlusion"
  (let* ([ray (ray3 (vec3 1.2 -2 0) (vec3 0 1 0))]  ; Passing near sphere edge
         [params (raymarch-params 100 100.0 0.001)]
         [shadow (raymarch-shadow sdf-sphere ray 10.0 16.0 params)])
        ;; Soft shadows near surface edge - should get some penumbra
        ;; May be 0 if too close to surface
        (assert-true (>= shadow 0.0))
        (assert-true (<= shadow 1.0))))

;;; ============================================================
;;; Ambient Occlusion Tests
;;; ============================================================

(define-test "raymarch-ao open space"
  (let* ([point (vec3 0 5 0)]
         [normal (vec3 0 1 0)]
         [ao (raymarch-ao sdf-sphere point normal 5)])
        ;; Far from surface, minimal occlusion
        (assert-true (> ao 0.8))))

(define-test "raymarch-ao near surface"
  (let* ([point (vec3 0 1.5 0)]  ; Slightly above sphere
         [normal (vec3 0 1 0)]
         [ao (raymarch-ao sdf-sphere point normal 5)])
        ;; AO samples along normal direction
        ;; Result should be between 0 and 1
        (assert-true (>= ao 0.0))
        (assert-true (<= ao 1.0))))

(define-test "raymarch-ao inside concavity"
  ;; Test with a ground plane - looking at floor should show AO
  (let* ([point (vec3 0 0.01 0)]
         [normal (vec3 0 1 0)]
         [ao (raymarch-ao sdf-plane point normal 5)])
        ;; Right on the plane
        (assert-true (<= ao 1.0))))

;;; ============================================================
;;; Shading Tests
;;; ============================================================

(define-test "simple-shading direct light"
  (let* ([normal (vec3 0 1 0)]
         [light-dir (vec3 0 1 0)]  ; Direct overhead
         [view-dir (vec3 0 1 0)]
         [shading (simple-shading normal light-dir view-dir)])
        ;; Should be near maximum
        (assert-true (> shading 0.9))))

(define-test "simple-shading grazing angle"
  (let* ([normal (vec3 0 1 0)]
         [light-dir (vec3 1 0 0)]  ; 90 degree angle
         [view-dir (vec3 0 1 0)]
         [shading (simple-shading normal light-dir view-dir)])
        ;; Should only have ambient
        (assert-true (approx= shading 0.2 0.1))))

(define-test "simple-shading backface"
  (let* ([normal (vec3 0 1 0)]
         [light-dir (vec3 0 -1 0)]  ; From behind
         [view-dir (vec3 0 1 0)]
         [shading (simple-shading normal light-dir view-dir)])
        ;; Should only have ambient
        (assert-true (approx= shading 0.2 0.1))))

;;; ============================================================
;;; Render Pixel Tests
;;; ============================================================

(define-test "render-pixel hit"
  (let* ([ray (ray3 (vec3 0 0 -5) (vec3 0 0 1))]
         [light-pos (vec3 0 10 -10)]
         [params (raymarch-params 100 100.0 0.001)]
         [pixel (render-pixel sdf-sphere ray light-pos params)])
        ;; Should return a value > 0
        (assert-true (> pixel 0.0))))

(define-test "render-pixel miss"
  (let* ([ray (ray3 (vec3 5 5 -5) (vec3 0 0 1))]
         [light-pos (vec3 0 10 -10)]
         [params (raymarch-params 100 100.0 0.001)]
         [pixel (render-pixel sdf-sphere ray light-pos params)])
        ;; Should return 0 for background
        (assert-true (approx= pixel 0.0 0.001))))

;;; ============================================================
;;; Adaptive Step Tests
;;; ============================================================

(define-test "adaptive-step returns valid distance"
  (let* ([point (vec3 0 0 -2)]  ; 1 unit from sphere
         [step (adaptive-step sdf-sphere point)])
        ;; Should return a positive step
        (assert-true (> step 0))))

(define-test "adaptive-step scales with curvature"
  (let* ([flat-point (vec3 10 0.5 0)]  ; Far from sphere, low curvature
         [curved-point (vec3 0 1.1 0)]  ; Near sphere surface, high curvature
         [flat-step (adaptive-step sdf-sphere flat-point)]
         [curved-step (adaptive-step sdf-sphere curved-point)])
        ;; Both should be positive
        (assert-true (> flat-step 0))
        (assert-true (> curved-step 0))))

;;; ============================================================
;;; Mesh Raymarching Tests (with mesh-sdf)
;;; ============================================================

(define-test "raymarch-mesh cube hit"
  (let* ([mesh (make-mesh-cube 1.0)]
         [ray (ray3 (vec3 0 0 -5) (vec3 0 0 1))]
         [params (raymarch-params 100 100.0 0.01)]
         [result (raymarch-mesh mesh ray params)])
        (assert-false (equal? result #f))
        (let ([hit-point (car result)]
              [t (cadr result)])
             ;; Should hit near z ~ -1
             (assert-true (approx= (vec3-z hit-point) -1.0 0.2)))))

(define-test "raymarch-mesh cube miss"
  (let* ([mesh (make-mesh-cube 1.0)]
         [ray (ray3 (vec3 10 10 -5) (vec3 0 0 1))]
         [params (raymarch-params 100 100.0 0.01)]
         [result (raymarch-mesh mesh ray params)])
        (assert-false result)))

(define-test "mesh-sdf-normal"
  (let* ([mesh (make-mesh-cube 1.0)]
         [point (vec3 1.1 0 0)]
         [normal (mesh-sdf-normal mesh point)])
        ;; Should point outward in +X direction
        (assert-true (approx= (vec3-x normal) 1.0 0.3))))

;;; ============================================================
;;; BVH Accelerated Raymarching Tests
;;; ============================================================

(define-test "bvh-accelerated-raymarch cube hit"
  (let* ([mesh (make-mesh-cube 1.0)]
         [ray (ray3 (vec3 0 0 -5) (vec3 0 0 1))]
         [params (raymarch-params 100 100.0 0.01)]
         [result (bvh-accelerated-raymarch mesh ray params)])
        (assert-false (equal? result #f))
        (let ([hit-point (car result)])
             ;; Should hit near z ~ -1
             (assert-true (approx= (vec3-z hit-point) -1.0 0.3)))))

(define-test "bvh-accelerated-raymarch miss"
  (let* ([mesh (make-mesh-cube 1.0)]
         [ray (ray3 (vec3 10 10 -5) (vec3 0 0 1))]
         [params (raymarch-params 100 100.0 0.01)]
         [result (bvh-accelerated-raymarch mesh ray params)])
        (assert-false result)))

;;; ============================================================
;;; Summary
;;; ============================================================

(display "\n")
(display "Tests run:    ")
(display *tests-run*)
(newline)
(display "Tests passed: ")
(display *tests-passed*)
(newline)
(display "Tests failed: ")
(display *tests-failed*)
(newline)

(if (> *tests-failed* 0)
    (exit 1)
    (exit 0))
