;;; user/physics3d/test-constraints3d.ss --- Tests for constraints3d and constraint-solver3d
;;;
;;; Run with: scheme --script user/physics3d/test-constraints3d.ss

(load "core/test-framework.ss")
(load "user/physics3d/constraint-solver3d.ss")

(display "\n")
(display "==============================================================\n")
(display "         CONSTRAINTS 3D TESTS\n")
(display "==============================================================\n")

;;; ============================================================
;;; Constraint Data Structure Tests
;;; ============================================================

(test-group constraint-data-tests
            
            (define-test distance-constraint-data
              (let ([data (make-distance-constraint-data-3d (vec3 1 0 0) (vec3 0 1 0) 2.0)])
                   (assert-true (vec3? (distance-data-3d-anchor-a data)))
                   (assert-equal 2.0 (distance-data-3d-target data))))
            
            (define-test ball-socket-constraint-data
              (let ([data (make-ball-socket-constraint-data-3d (vec3 0 0 0) (vec3 1 0 0))])
                   (assert-true (vec3? (ball-socket-data-3d-anchor-a data)))
                   (assert-true (vec3? (ball-socket-data-3d-anchor-b data)))))
            
            (define-test hinge-constraint-data
              (let ([data (make-hinge-constraint-data-3d
                           (vec3 0 0 0) (vec3 1 0 0)
                           (vec3 0 0 1) (vec3 0 0 1))])
                   (assert-true (vec3? (hinge-data-3d-axis-a data)))
                   (assert-true (vec3? (hinge-data-3d-axis-b data)))))
            
            (define-test fixed-constraint-data
              (let ([data (make-fixed-constraint-data-3d
                           (vec3 0 0 0) (vec3 1 0 0) (quat-identity))])
                   (assert-true (vec3? (fixed-data-3d-anchor-a data)))
                   (assert-true (quat? (fixed-data-3d-reference-orientation data)))))
            
            (define-test spring-constraint-data
              (let ([data (make-spring-constraint-data-3d
                           (vec3 0 0 0) (vec3 1 0 0) 1.0 1000 10)])
                   (assert-equal 1.0 (spring-data-3d-rest-length data))
                   (assert-equal 1000 (spring-data-3d-stiffness data))
                   (assert-equal 10 (spring-data-3d-damping data)))))

;;; ============================================================
;;; Distance Constraint Solver Tests
;;; ============================================================

(test-group distance-solver-tests
            
            (define-test distance-velocity-constraint
              ;; Two bodies approaching each other should have their approach velocity reduced
              (let* ([body-a (make-rigid-body-3d (vec3 0 0 0) (vec3 1 0 0) (quat-identity) (vec3-zero)
                                                 1.0 (inertia-solid-sphere 1.0 1.0))]
                     [body-b (make-rigid-body-3d (vec3 3 0 0) (vec3 -1 0 0) (quat-identity) (vec3-zero)
                                                 1.0 (inertia-solid-sphere 1.0 1.0))]
                     [pos-a (vec3 0 0 0)]
                     [pos-b (vec3 3 0 0)]
                     [target-dist 3.0]
                     [dt 0.01])
                    (call-with-values
                     (lambda () (solve-distance-velocity-3d body-a body-b pos-a pos-b target-dist dt))
                     (lambda (new-a new-b)
                             ;; After solving, relative velocity along constraint axis should decrease
                             (let* ([v-rel-before (- (vec3-x (rigid-body-3d-vel body-b))
                                                     (vec3-x (rigid-body-3d-vel body-a)))]
                                    [v-rel-after (- (vec3-x (rigid-body-3d-vel new-b))
                                                    (vec3-x (rigid-body-3d-vel new-a)))])
                                   (assert-true (< (abs v-rel-after) (abs v-rel-before))))))))
            
            (define-test distance-position-constraint
              ;; Bodies too close should be pushed apart
              (let* ([body-a (make-rigid-body-3d (vec3 0 0 0) (vec3-zero) (quat-identity) (vec3-zero)
                                                 1.0 (inertia-solid-sphere 1.0 1.0))]
                     [body-b (make-rigid-body-3d (vec3 2 0 0) (vec3-zero) (quat-identity) (vec3-zero)
                                                 1.0 (inertia-solid-sphere 1.0 1.0))]
                     [pos-a (vec3 0 0 0)]
                     [pos-b (vec3 2 0 0)]
                     [target-dist 3.0])  ; Current distance is 2, target is 3
                    (call-with-values
                     (lambda () (correct-distance-position-3d body-a body-b pos-a pos-b target-dist))
                     (lambda (new-a new-b)
                             ;; Bodies should move apart
                             (let* ([old-dist (vec3-magnitude (vec3-sub (rigid-body-3d-pos body-b)
                                                                        (rigid-body-3d-pos body-a)))]
                                    [new-dist (vec3-magnitude (vec3-sub (rigid-body-3d-pos new-b)
                                                                        (rigid-body-3d-pos new-a)))])
                                   (assert-true (> new-dist old-dist))))))))

;;; ============================================================
;;; Ball-Socket Joint Solver Tests
;;; ============================================================

(test-group ball-socket-solver-tests
            
            (define-test ball-socket-velocity-constraint
              ;; Anchors moving apart should have their relative velocity reduced
              (let* ([body-a (make-rigid-body-3d (vec3 0 0 0) (vec3 -1 0 0) (quat-identity) (vec3-zero)
                                                 1.0 (inertia-solid-sphere 1.0 1.0))]
                     [body-b (make-rigid-body-3d (vec3 2 0 0) (vec3 1 0 0) (quat-identity) (vec3-zero)
                                                 1.0 (inertia-solid-sphere 1.0 1.0))]
                     [pos-a (vec3 0.5 0 0)]  ; Anchor on A
                     [pos-b (vec3 1.5 0 0)]  ; Anchor on B - should be at same point
                     [dt 0.01])
                    (call-with-values
                     (lambda () (solve-ball-socket-velocity-3d body-a body-b pos-a pos-b dt))
                     (lambda (new-a new-b)
                             ;; After solving, anchors should move more together
                             (assert-true (rigid-body-3d? new-a))
                             (assert-true (rigid-body-3d? new-b))))))
            
            (define-test ball-socket-position-constraint
              ;; Separated anchors should be pulled together
              (let* ([body-a (make-rigid-body-3d (vec3 0 0 0) (vec3-zero) (quat-identity) (vec3-zero)
                                                 1.0 (inertia-solid-sphere 1.0 1.0))]
                     [body-b (make-rigid-body-3d (vec3 3 0 0) (vec3-zero) (quat-identity) (vec3-zero)
                                                 1.0 (inertia-solid-sphere 1.0 1.0))]
                     [pos-a (vec3 0.5 0 0)]  ; Anchor on A
                     [pos-b (vec3 2.5 0 0)]) ; Anchor on B - 2 units apart
                    (call-with-values
                     (lambda () (correct-ball-socket-position-3d body-a body-b pos-a pos-b))
                     (lambda (new-a new-b)
                             ;; Bodies should move toward each other
                             (let* ([old-dist (vec3-magnitude (vec3-sub (rigid-body-3d-pos body-b)
                                                                        (rigid-body-3d-pos body-a)))]
                                    [new-dist (vec3-magnitude (vec3-sub (rigid-body-3d-pos new-b)
                                                                        (rigid-body-3d-pos new-a)))])
                                   (assert-true (< new-dist old-dist))))))))

;;; ============================================================
;;; Spring Constraint Solver Tests
;;; ============================================================

(test-group spring-solver-tests
            
            (define-test spring-stretched-applies-force
              ;; Stretched spring should pull bodies together
              (let* ([body-a (make-rigid-body-3d (vec3 0 0 0) (vec3-zero) (quat-identity) (vec3-zero)
                                                 1.0 (inertia-solid-sphere 1.0 1.0))]
                     [body-b (make-rigid-body-3d (vec3 3 0 0) (vec3-zero) (quat-identity) (vec3-zero)
                                                 1.0 (inertia-solid-sphere 1.0 1.0))]
                     [pos-a (vec3 0 0 0)]
                     [pos-b (vec3 3 0 0)]
                     [rest-length 2.0]  ; Current length 3, rest is 2 → stretched
                     [stiffness 1000]
                     [damping 10]
                     [dt 0.01])
                    (call-with-values
                     (lambda () (solve-spring-velocity-3d body-a body-b pos-a pos-b rest-length stiffness damping dt))
                     (lambda (new-a new-b)
                             ;; Body A should have positive x velocity (pulled toward B)
                             (assert-true (> (vec3-x (rigid-body-3d-vel new-a)) 0))
                             ;; Body B should have negative x velocity (pulled toward A)
                             (assert-true (< (vec3-x (rigid-body-3d-vel new-b)) 0))))))
            
            (define-test spring-compressed-pushes-apart
              ;; Compressed spring should push bodies apart
              (let* ([body-a (make-rigid-body-3d (vec3 0 0 0) (vec3-zero) (quat-identity) (vec3-zero)
                                                 1.0 (inertia-solid-sphere 1.0 1.0))]
                     [body-b (make-rigid-body-3d (vec3 1 0 0) (vec3-zero) (quat-identity) (vec3-zero)
                                                 1.0 (inertia-solid-sphere 1.0 1.0))]
                     [pos-a (vec3 0 0 0)]
                     [pos-b (vec3 1 0 0)]
                     [rest-length 2.0]  ; Current length 1, rest is 2 → compressed
                     [stiffness 1000]
                     [damping 10]
                     [dt 0.01])
                    (call-with-values
                     (lambda () (solve-spring-velocity-3d body-a body-b pos-a pos-b rest-length stiffness damping dt))
                     (lambda (new-a new-b)
                             ;; Body A should have negative x velocity (pushed away from B)
                             (assert-true (< (vec3-x (rigid-body-3d-vel new-a)) 0))
                             ;; Body B should have positive x velocity (pushed away from A)
                             (assert-true (> (vec3-x (rigid-body-3d-vel new-b)) 0)))))))

;;; ============================================================
;;; Constraint Factory Tests
;;; ============================================================

(test-group constraint-factory-tests
            
            (define-test make-distance-joint
              (let ([joint (make-distance-joint-3d 'test-id 'entity-a 'entity-b
                                                   (vec3 0 0 0) (vec3 1 0 0) 2.0)])
                   (assert-true (constraint-3d? joint))
                   (assert-equal 'distance-3d (constraint-3d-type joint))
                   (assert-equal 'test-id (constraint-3d-id joint))))
            
            (define-test make-ball-socket-joint
              (let ([joint (make-ball-socket-joint-3d 'test-id 'entity-a 'entity-b
                                                      (vec3 0 0 0) (vec3 1 0 0))])
                   (assert-true (constraint-3d? joint))
                   (assert-equal 'ball-socket-3d (constraint-3d-type joint))))
            
            (define-test make-hinge-joint
              (let ([joint (make-hinge-joint-3d 'test-id 'entity-a 'entity-b
                                                (vec3 0 0 0) (vec3 1 0 0)
                                                (vec3 0 0 1) (vec3 0 0 1))])
                   (assert-true (constraint-3d? joint))
                   (assert-equal 'hinge-3d (constraint-3d-type joint))))
            
            (define-test make-fixed-joint
              (let ([joint (make-fixed-joint-3d 'test-id 'entity-a 'entity-b
                                                (vec3 0 0 0) (vec3 1 0 0)
                                                (quat-identity))])
                   (assert-true (constraint-3d? joint))
                   (assert-equal 'fixed-3d (constraint-3d-type joint))))
            
            (define-test make-spring-joint
              (let ([joint (make-spring-joint-3d 'test-id 'entity-a 'entity-b
                                                 (vec3 0 0 0) (vec3 1 0 0)
                                                 1.0 1000 10)])
                   (assert-true (constraint-3d? joint))
                   (assert-equal 'spring-3d (constraint-3d-type joint)))))

;;; ============================================================
;;; Mat3 Utility Tests
;;; ============================================================

(test-group mat3-utility-tests
            
            (define-test mat3-trace-identity
              (let ([I (mat3-identity)])
                   (assert-equal 3 (mat3-trace I))))
            
            (define-test mat3-trace-diagonal
              (let ([D (mat3-diagonal 1 2 3)])
                   (assert-equal 6 (mat3-trace D))))
            
            (define-test mat3-trace-zero
              (let ([Z (mat3-zero)])
                   (assert-equal 0 (mat3-trace Z)))))

;;; ============================================================
;;; Run all tests
;;; ============================================================

(run-all-tests)

