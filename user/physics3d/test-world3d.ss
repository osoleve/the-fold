;;; user/physics3d/test-world3d.ss --- Tests for world3d
;;;
;;; Run with: scheme --script user/physics3d/test-world3d.ss

(load "core/test-framework.ss")
(load "user/physics3d/world3d.ss")

(display "\n")
(display "==============================================================\n")
(display "         WORLD 3D TESTS\n")
(display "==============================================================\n")

;;; ============================================================
;;; Entity Tests
;;; ============================================================

(test-group entity-tests

            (define-test entity-creation
              (let* ([body (make-rigid-body-3d (vec3 1 2 3) (vec3 0 0 0)
                                               (quat-identity) (vec3 0 0 0)
                                               1.0 (inertia-solid-sphere 1.0 1.0))]
                     [shape (sphere3d (vec3 1 2 3) 0.5)]
                     [entity (make-entity-3d 'test-id body shape *default-material* #f)])
                    (assert-true (entity-3d? entity))
                    (assert-equal 'test-id (entity-3d-id entity))))

            (define-test entity-accessors
              (let* ([body (make-rigid-body-3d (vec3 5 6 7) (vec3 1 0 0)
                                               (quat-identity) (vec3 0 0 0)
                                               2.0 (inertia-solid-sphere 2.0 1.0))]
                     [shape (sphere3d (vec3 5 6 7) 1.0)]
                     [entity (make-entity-3d 'e1 body shape *bouncy-material* "user-data")])
                    (assert-equal (vec3 5 6 7) (entity-3d-pos entity))
                    (assert-equal (vec3 1 0 0) (entity-3d-vel entity))
                    (assert-equal "user-data" (entity-3d-user-data entity))))

            (define-test entity-with-body
              (let* ([body1 (make-rigid-body-3d (vec3 0 0 0) (vec3 0 0 0)
                                                (quat-identity) (vec3 0 0 0)
                                                1.0 (inertia-solid-sphere 1.0 1.0))]
                     [body2 (rigid-body-3d-with-pos body1 (vec3 10 0 0))]
                     [shape (sphere3d (vec3 0 0 0) 0.5)]
                     [entity (make-entity-3d 'e1 body1 shape *default-material* #f)]
                     [updated (entity-3d-with-body entity body2)])
                    (assert-equal (vec3 10 0 0) (entity-3d-pos updated))
                    (assert-equal 'e1 (entity-3d-id updated)))))

;;; ============================================================
;;; Material Tests
;;; ============================================================

(test-group material-tests

            (define-test default-material
              (assert-true (material? *default-material*))
              (assert-equal 0.3 (material-restitution *default-material*))
              (assert-equal 0.5 (material-friction *default-material*)))

            (define-test bouncy-material
              (assert-equal 0.9 (material-restitution *bouncy-material*)))

            (define-test sticky-material
              (assert-equal 0.0 (material-restitution *sticky-material*))
              (assert-equal 0.9 (material-friction *sticky-material*))))

;;; ============================================================
;;; World Creation and Entity Management
;;; ============================================================

(test-group world-management-tests

            (define-test world-creation
              (let ([world (make-world-3d (vec3 0 -9.8 0))])
                   (assert-true (world-3d? world))
                   (assert-equal (vec3 0 -9.8 0) (world-3d-gravity world))))

            (define-test add-get-entity
              (let* ([world (make-world-3d (vec3 0 -9.8 0))]
                     [entity (make-sphere-entity-3d 'ball (vec3 0 5 0) 0.5 1.0 *default-material*)])
                    (world-3d-add-entity! world entity)
                    (let ([retrieved (world-3d-get-entity world 'ball)])
                         (assert-true (entity-3d? retrieved))
                         (assert-equal 'ball (entity-3d-id retrieved)))))

            (define-test remove-entity
              (let* ([world (make-world-3d (vec3 0 -9.8 0))]
                     [entity (make-sphere-entity-3d 'ball (vec3 0 5 0) 0.5 1.0 *default-material*)])
                    (world-3d-add-entity! world entity)
                    (world-3d-remove-entity! world 'ball)
                    (assert-false (world-3d-get-entity world 'ball))))

            (define-test entity-list
              (let* ([world (make-world-3d (vec3 0 -9.8 0))]
                     [e1 (make-sphere-entity-3d 'ball1 (vec3 0 5 0) 0.5 1.0 *default-material*)]
                     [e2 (make-sphere-entity-3d 'ball2 (vec3 5 5 0) 0.5 1.0 *default-material*)])
                    (world-3d-add-entity! world e1)
                    (world-3d-add-entity! world e2)
                    (assert-equal 2 (length (world-3d-entity-list world)))))

            (define-test update-entity
              (let* ([world (make-world-3d (vec3 0 -9.8 0))]
                     [entity (make-sphere-entity-3d 'ball (vec3 0 5 0) 0.5 1.0 *default-material*)])
                    (world-3d-add-entity! world entity)
                    (world-3d-update-entity!
                     world 'ball
                     (lambda (e)
                             (let* ([body (entity-3d-body e)]
                                    [new-body (rigid-body-3d-with-vel body (vec3 10 0 0))])
                                   (entity-3d-with-body e new-body))))
                    (let ([updated (world-3d-get-entity world 'ball)])
                         (assert-equal (vec3 10 0 0) (entity-3d-vel updated))))))

;;; ============================================================
;;; Spatial Hash Tests
;;; ============================================================

(test-group spatial-hash-tests

            (define-test spatial-hash-creation
              (let ([hash (make-spatial-hash-3d 10)])
                   (assert-equal 10 (spatial-hash-3d-cell-size hash))))

            (define-test spatial-hash-insert-query
              (let ([hash (make-spatial-hash-3d 10)])
                   (spatial-hash-3d-insert! hash 'obj1
                                            (aabb3d (vec3 0 0 0) (vec3 5 5 5)))
                   (let ([result (spatial-hash-3d-query hash
                                                        (aabb3d (vec3 2 2 2) (vec3 3 3 3)))])
                        (assert-true (not (null? (filter (lambda (x) (eq? x 'obj1)) result)))))))

            (define-test spatial-hash-no-overlap
              (let ([hash (make-spatial-hash-3d 10)])
                   (spatial-hash-3d-insert! hash 'obj1
                                            (aabb3d (vec3 0 0 0) (vec3 5 5 5)))
                   (let ([result (spatial-hash-3d-query hash
                                                        (aabb3d (vec3 100 100 100) (vec3 110 110 110)))])
                        (assert-false (member 'obj1 result))))))

;;; ============================================================
;;; Time Accumulator Tests
;;; ============================================================

(test-group time-acc-tests

            (define-test time-acc-creation
              (let ([acc (make-time-acc-3d 0.016 10)])
                   (assert-equal 0.016 (time-acc-3d-fixed-dt acc))
                   (assert-equal 10 (time-acc-3d-max-substeps acc))
                   (assert-equal 0.0 (time-acc-3d-accumulated acc))))

            (define-test time-acc-add
              (let* ([acc (make-time-acc-3d 0.016 10)]
                     [acc2 (time-acc-3d-add acc 0.1)])
                    (assert-equal 0.1 (time-acc-3d-accumulated acc2))))

            (define-test time-acc-consume
              (let* ([acc (make-time-acc-3d 0.016 10)]
                     [acc2 (time-acc-3d-add acc 0.05)]  ; Enough for 3 steps
                     [result (time-acc-3d-consume acc2)]
                     [new-acc (car result)]
                     [consumed (cadr result)])
                    (assert-equal 0.016 consumed)
                    (assert-true (< (time-acc-3d-accumulated new-acc) 0.05)))))

;;; ============================================================
;;; Integration Tests
;;; ============================================================

(test-group integration-tests

            (define-test velocity-integration
              ;; Ball with initial velocity should move
              (let* ([world (make-world-3d (vec3 0 0 0))]  ; No gravity
                     [entity (make-sphere-entity-3d 'ball (vec3 0 0 0) 0.5 1.0 *default-material*)])
                    (world-3d-add-entity! world entity)
                    (set-velocity-3d! world 'ball (vec3 10 0 0))
                    (world-3d-fixed-step! world 0.1)
                    (let ([ball (world-3d-get-entity world 'ball)])
                         ;; Position should have moved in +x direction
                         (assert-true (> (vec3-x (entity-3d-pos ball)) 0)))))

            (define-test gravity-integration
              ;; Ball should fall under gravity
              (let* ([world (make-world-3d (vec3 0 -10 0))]
                     [entity (make-sphere-entity-3d 'ball (vec3 0 10 0) 0.5 1.0 *default-material*)])
                    (world-3d-add-entity! world entity)
                    (world-3d-fixed-step! world 0.1)
                    (let* ([ball (world-3d-get-entity world 'ball)]
                           [vel-y (vec3-y (entity-3d-vel ball))])
                          ;; Velocity should be negative (falling)
                          (assert-true (< vel-y 0))))))

;;; ============================================================
;;; Convenience Constructor Tests
;;; ============================================================

(test-group constructor-tests

            (define-test make-sphere-entity
              (let ([entity (make-sphere-entity-3d 'sphere (vec3 1 2 3) 0.5 2.0 *default-material*)])
                   (assert-true (entity-3d? entity))
                   (assert-true (sphere3d? (entity-3d-shape entity)))
                   (assert-equal (vec3 1 2 3) (entity-3d-pos entity))))

            (define-test make-box-entity
              (let ([entity (make-box-entity-3d 'box (vec3 0 0 0) (vec3 1 1 1) 3.0 *default-material*)])
                   (assert-true (entity-3d? entity))
                   (assert-true (box3d? (entity-3d-shape entity)))))

            (define-test make-static-sphere
              (let ([entity (make-static-sphere-3d 'static (vec3 0 0 0) 1.0 *default-material*)])
                   (assert-true (entity-3d-static? entity))))

            (define-test make-ground
              (let ([entity (make-ground-3d 'ground 0 100 *default-material*)])
                   (assert-true (entity-3d-static? entity))
                   (assert-true (aabb3d? (entity-3d-shape entity))))))

;;; ============================================================
;;; Entity Manipulation Tests
;;; ============================================================

(test-group manipulation-tests

            (define-test set-velocity
              (let* ([world (make-world-3d (vec3 0 0 0))]
                     [entity (make-sphere-entity-3d 'ball (vec3 0 0 0) 0.5 1.0 *default-material*)])
                    (world-3d-add-entity! world entity)
                    (set-velocity-3d! world 'ball (vec3 5 0 0))
                    (let ([ball (world-3d-get-entity world 'ball)])
                         (assert-equal (vec3 5 0 0) (entity-3d-vel ball)))))

            (define-test set-position
              (let* ([world (make-world-3d (vec3 0 0 0))]
                     [entity (make-sphere-entity-3d 'ball (vec3 0 0 0) 0.5 1.0 *default-material*)])
                    (world-3d-add-entity! world entity)
                    (set-position-3d! world 'ball (vec3 10 20 30))
                    (let ([ball (world-3d-get-entity world 'ball)])
                         (assert-equal (vec3 10 20 30) (entity-3d-pos ball)))))

            (define-test apply-impulse
              (let* ([world (make-world-3d (vec3 0 0 0))]
                     [entity (make-sphere-entity-3d 'ball (vec3 0 0 0) 0.5 1.0 *default-material*)])
                    (world-3d-add-entity! world entity)
                    (apply-impulse-3d! world 'ball (vec3 10 0 0))
                    (let ([ball (world-3d-get-entity world 'ball)])
                         ;; Velocity should have increased
                         (assert-true (> (vec3-x (entity-3d-vel ball)) 0))))))

;;; ============================================================
;;; World Query Tests
;;; ============================================================

(test-group query-tests

            (define-test query-point
              (let* ([world (make-world-3d (vec3 0 0 0))]
                     [e1 (make-sphere-entity-3d 'ball (vec3 0 0 0) 2.0 1.0 *default-material*)]
                     [e2 (make-sphere-entity-3d 'far (vec3 100 0 0) 1.0 1.0 *default-material*)])
                    (world-3d-add-entity! world e1)
                    (world-3d-add-entity! world e2)
                    (let ([result (world-3d-query-point world (vec3 1 0 0))])
                         ;; Should find 'ball but not 'far
                         (assert-equal 1 (length result))
                         (assert-equal 'ball (entity-3d-id (car result)))))))

;;; ============================================================
;;; Run all tests
;;; ============================================================

(run-all-tests)

