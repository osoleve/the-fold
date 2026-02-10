;;; lattice/physics/problems/test-physics-problems.ss — Tests for Physics Problems
;;; Run with: scheme --script lattice/physics/problems/test-physics-problems.ss

(load "core/lang/module.ss")
(load "core/testing/test-framework.ss")
(load "lattice/physics/problems/templates/spatial.ss")

;;; ============================================================
;;; Simulation Tests
;;; ============================================================

(test-group "simulation"
  (define-test "make-problem-config creates valid config"
    (let ([c (default-problem-config)])
      (assert-true (problem-config? c))
      (assert-equal 70 (problem-config-width c))
      (assert-equal 35 (problem-config-height c))))

  (define-test "make-ball creates entity"
    (let ([ball (make-ball 'test (vec2 0 5) 2.0 1.0 (vec2 1 0))])
      (assert-true (entity? ball))
      (assert-equal 'test (entity-id ball))))

  (define-test "setup-no-gravity-world creates world"
    (let* ([ball (make-ball 'a (vec2 0 5) 1.0 1.0 (vec2 0 0))]
           [world (setup-no-gravity-world (list ball))])
      (assert-true (world? world))))

  (define-test "simulate-step advances world"
    (let* ([ball (make-ball 'a (vec2 0 5) 1.0 1.0 (vec2 10 0))]
           [world (setup-no-gravity-world (list ball))]
           [world2 (simulate-step world 0.1)]
           [pos (get-entity-position world2 'a)])
      ;; Ball should have moved in x direction
      (assert-true (> (vec2-x pos) 0.5)))))

;;; ============================================================
;;; Physics Problem DSL Tests
;;; ============================================================

(test-group "physics-problem"
  (define-test "size-comparison-problem is valid"
    (assert-true (physics-problem? size-comparison-problem))
    (assert-equal 'size-comparison (problem-name size-comparison-problem))
    (assert-equal 'spatial (problem-category size-comparison-problem)))

  (define-test "distance-problem is valid"
    (assert-true (physics-problem? distance-problem))
    (assert-equal 'distance-estimation (problem-name distance-problem)))

  (define-test "count-problem is valid"
    (assert-true (physics-problem? count-problem))
    (assert-equal 'object-counting (problem-name count-problem)))

  (define-test "position-problem is valid"
    (assert-true (physics-problem? position-problem))
    (assert-equal 'position-comparison (problem-name position-problem))))

;;; ============================================================
;;; Spatial Template Tests
;;; ============================================================

(test-group "spatial-templates"
  (define-test "size-comparison params sample correctly"
    (let* ([rng (make-prng 42)]
           [params (param-set-sample size-comparison-params rng 100)])
      (assert-true (list? params))
      (assert-true (if (assq 'r1 params) #t #f))
      (assert-true (if (assq 'r2 params) #t #f))))

  (define-test "size-comparison setup creates world"
    (let* ([params '((r1 . 2.0) (r2 . 1.5) (x1 . -5.0) (x2 . 5.0) (y . 10.0))]
           [world (size-comparison-setup params)])
      (assert-true (world? world))))

  (define-test "size-comparison answer computes correctly"
    (let* ([params1 '((r1 . 3.0) (r2 . 1.5) (x1 . -5.0) (x2 . 5.0) (y . 10.0))]
           [params2 '((r1 . 1.5) (r2 . 3.0) (x1 . -5.0) (x2 . 5.0) (y . 10.0))]
           [params3 '((r1 . 2.0) (r2 . 2.1) (x1 . -5.0) (x2 . 5.0) (y . 10.0))])
      ;; A is larger
      (assert-equal 'A (size-comparison-answer #f #f params1))
      ;; B is larger
      (assert-equal 'B (size-comparison-answer #f #f params2))
      ;; Same (within tolerance)
      (assert-equal 'same (size-comparison-answer #f #f params3))))

  (define-test "distance answer computes correctly"
    (let ([params '((x1 . 0.0) (x2 . 3.0) (y1 . 0.0) (y2 . 4.0) (r . 1.0))])
      ;; 3-4-5 right triangle, distance = 5
      (assert-equal 5.0 (distance-answer #f #f params))))

  (define-test "count answer returns n"
    (let ([params '((n . 4) (spread . 10.0))])
      (assert-equal 4 (count-answer #f #f params))))

  (define-test "position answer computes correctly"
    ;; swap=0: A=left, B=right -> B is further right
    (let ([params1 '((x-left . -10.0) (x-right . 10.0) (y1 . 5.0) (y2 . 5.0) (r . 2.0) (swap . 0))])
      (assert-equal 'B (position-answer #f #f params1)))
    ;; swap=1: A=right, B=left -> A is further right
    (let ([params2 '((x-left . -10.0) (x-right . 10.0) (y1 . 5.0) (y2 . 5.0) (r . 2.0) (swap . 1))])
      (assert-equal 'A (position-answer #f #f params2)))))

;;; ============================================================
;;; Integration Tests
;;; ============================================================

(test-group "integration"
  (define-test "all-spatial-problems is populated"
    (assert-equal 4 (length all-spatial-problems))
    (assert-true (andmap physics-problem? all-spatial-problems)))

  (define-test "generate-problem-id creates unique ids"
    (let* ([rng (make-prng 42)]
           [id1 (generate-problem-id 'test rng)]
           [id2 (generate-problem-id 'test rng)])
      (assert-true (string? id1))
      (assert-true (string? id2))
      (assert-true (not (string=? id1 id2)))))

  (define-test "category-distractor-strategies returns strategies"
    (let ([spatial-strats (category-distractor-strategies 'spatial)]
          [velocity-strats (category-distractor-strategies 'velocity)])
      (assert-true (list? spatial-strats))
      (assert-true (list? velocity-strats))
      (assert-true (> (length spatial-strats) 0)))))

;;; ============================================================
;;; Helpers
;;; ============================================================

(define (andmap pred lst)
  (cond
    [(null? lst) #t]
    [(pred (car lst)) (andmap pred (cdr lst))]
    [else #f]))

;;; ============================================================
;;; Run Tests
;;; ============================================================

(display "\n=== Physics Problems Tests ===\n\n")
(run-all-tests)
