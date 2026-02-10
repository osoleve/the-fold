;;; lattice/fp/test-protocol-bundle.ss — Tests for Protocol Bundle System
;;;
;;; Run with: scheme --script lattice/fp/test-protocol-bundle.ss

(load "core/lang/module.ss")
(load "core/testing/test-framework.ss")
(load "lattice/fp/protocol-bundle.ss")

;;; ====
;;; Test Fixtures: Vehicle Types
;;; ====

;;; Car: (list 'car speed fuel color)
(define (make-car speed fuel color)
  (list 'car speed fuel color))

(define (car-speed c) (cadr c))
(define (car-fuel c) (caddr c))
(define (car-color c) (cadddr c))

(define (car-with-speed c s)
  (list 'car s (car-fuel c) (car-color c)))

(define (car-with-fuel c f)
  (list 'car (car-speed c) f (car-color c)))

(define (car-with-color c col)
  (list 'car (car-speed c) (car-fuel c) col))

;;; Bike: (list 'bike speed gear)
(define (make-bike speed gear)
  (list 'bike speed gear))

(define (bike-speed b) (cadr b))
(define (bike-gear b) (caddr b))

(define (bike-with-speed b s)
  (list 'bike s (bike-gear b)))

(define (bike-with-gear b g)
  (list 'bike (bike-speed b) g))

;;; Scooter: (list 'scooter speed battery)
;;; Note: Uses different naming - will test explicit implement-bundle!
(define (make-scooter speed battery)
  (list 'scooter speed battery))

(define (get-scooter-speed s) (cadr s))
(define (get-scooter-battery s) (caddr s))

(define (set-scooter-speed s spd)
  (list 'scooter spd (get-scooter-battery s)))

(define (set-scooter-battery s bat)
  (list 'scooter (get-scooter-speed s) bat))

;;; ====
;;; Define Test Protocols
;;; ====

(define-protocol (vehicle-speed v) "Get vehicle speed")
(define-protocol (vehicle-set-speed v s) "Set vehicle speed")

(define-protocol (vehicle-fuel v) "Get vehicle fuel level")
(define-protocol (vehicle-set-fuel v f) "Set vehicle fuel level")

;;; ====
;;; Define Test Bundle
;;; ====

(define-protocol-bundle vehicle-ops
  ((vehicle-speed vehicle-set-speed) "speed")
  ((vehicle-fuel vehicle-set-fuel) "fuel"))

;;; ====
;;; Tests: Bundle Definition
;;; ====

(test-group "Bundle Definition"
  (define-test "bundle is created correctly"
    (assert-true (bundle? vehicle-ops)))

  (define-test "bundle name is correct"
    (assert-equal 'vehicle-ops (bundle-name vehicle-ops)))

  (define-test "bundle has correct number of slots"
    (assert-equal 2 (bundle-slot-count vehicle-ops)))

  (define-test "bundle is registered"
    (assert-true (if (get-bundle 'vehicle-ops) #t #f)))

  (define-test "list-bundles includes our bundle"
    (assert-true (if (memq 'vehicle-ops (list-bundles)) #t #f))))

;;; ====
;;; Tests: Slot Structure
;;; ====

(test-group "Slot Structure"
  (let ([slots (bundle-slots vehicle-ops)])
    (define-test "slots is a list"
      (assert-true (list? slots)))

    (define-test "first slot has correct getter"
      (assert-equal 'vehicle-speed (slot-getter (car slots))))

    (define-test "first slot has correct setter"
      (assert-equal 'vehicle-set-speed (slot-setter (car slots))))

    (define-test "first slot has correct label"
      (assert-equal "speed" (slot-label (car slots))))

    (define-test "second slot has correct getter"
      (assert-equal 'vehicle-fuel (slot-getter (cadr slots))))

    (define-test "second slot has correct setter"
      (assert-equal 'vehicle-set-fuel (slot-setter (cadr slots))))))

;;; ====
;;; Tests: derive-bundle! without overrides
;;; ====

;;; Register car using naming convention
(derive-bundle! vehicle-ops 'car car)

(test-group "derive-bundle! without overrides"
  (define-test "getter protocol dispatches correctly"
    (let ([c (make-car 60 100 'red)])
      (assert-equal 60 (vehicle-speed c))))

  (define-test "setter protocol dispatches correctly"
    (let* ([c (make-car 60 100 'red)]
           [c2 (vehicle-set-speed c 80)])
      (assert-equal 80 (car-speed c2))))

  (define-test "fuel getter works"
    (let ([c (make-car 60 100 'red)])
      (assert-equal 100 (vehicle-fuel c))))

  (define-test "fuel setter works"
    (let* ([c (make-car 60 100 'red)]
           [c2 (vehicle-set-fuel c 50)])
      (assert-equal 50 (car-fuel c2))))

  (define-test "type is registered for protocol"
    (assert-true (type-implements? 'car 'vehicle-speed)))

  (define-test "type is registered for setter protocol"
    (assert-true (type-implements? 'car 'vehicle-set-speed))))

;;; ====
;;; Tests: derive-bundle! with overrides
;;; ====

;;; Bike has no fuel - override with constant
(derive-bundle! vehicle-ops 'bike bike
  ("fuel" (lambda (b) 0) (lambda (b f) b)))  ; Bikes have no fuel

(test-group "derive-bundle! with overrides"
  (define-test "speed getter uses convention"
    (let ([b (make-bike 25 3)])
      (assert-equal 25 (vehicle-speed b))))

  (define-test "speed setter uses convention"
    (let* ([b (make-bike 25 3)]
           [b2 (vehicle-set-speed b 30)])
      (assert-equal 30 (bike-speed b2))))

  (define-test "fuel getter uses override (constant)"
    (let ([b (make-bike 25 3)])
      (assert-equal 0 (vehicle-fuel b))))

  (define-test "fuel setter uses override (no-op)"
    (let* ([b (make-bike 25 3)]
           [b2 (vehicle-set-fuel b 100)])
      ;; No-op, should return same bike
      (assert-equal 3 (bike-gear b2)))))

;;; ====
;;; Tests: implement-bundle! explicit
;;; ====

;;; Scooter uses different naming conventions, so we use explicit
(implement-bundle! vehicle-ops 'scooter
  ("speed" get-scooter-speed set-scooter-speed)
  ("fuel" get-scooter-battery set-scooter-battery))  ; Treat battery as fuel

(test-group "implement-bundle! explicit"
  (define-test "speed getter works"
    (let ([s (make-scooter 20 80)])
      (assert-equal 20 (vehicle-speed s))))

  (define-test "speed setter works"
    (let* ([s (make-scooter 20 80)]
           [s2 (vehicle-set-speed s 25)])
      (assert-equal 25 (get-scooter-speed s2))))

  (define-test "fuel getter maps to battery"
    (let ([s (make-scooter 20 80)])
      (assert-equal 80 (vehicle-fuel s))))

  (define-test "fuel setter maps to battery"
    (let* ([s (make-scooter 20 80)]
           [s2 (vehicle-set-fuel s 60)])
      (assert-equal 60 (get-scooter-battery s2)))))

;;; ====
;;; Tests: Error Cases
;;; ====

(test-group "Error Cases"
  (define-test "implement-bundle! error on unknown label"
    ;; We'd need to test this by creating a bad call, but since it's compile-time
    ;; we can test the runtime helper directly
    (assert-error
     (lambda ()
       (implement-bundle-runtime! vehicle-ops 'test-type
         '(("unknown" (lambda (x) x) (lambda (x v) x)))))))

  (define-test "implement-bundle! error on missing slot"
    ;; Missing "fuel" slot
    (assert-error
     (lambda ()
       (implement-bundle-runtime! vehicle-ops 'test-type2
         '(("speed" (lambda (x) x) (lambda (x v) x))))))))

;;; ====
;;; Tests: Introspection
;;; ====

(test-group "Introspection"
  (define-test "bundle-types lists implementing types"
    (let ([types (bundle-types vehicle-ops)])
      (assert-true (if (memq 'car types) #t #f))
      (assert-true (if (memq 'bike types) #t #f))
      (assert-true (if (memq 'scooter types) #t #f))))

  (define-test "bundle-protocols lists all protocols"
    (let ([protos (bundle-protocols vehicle-ops)])
      (assert-equal 4 (length protos))
      (assert-true (if (memq 'vehicle-speed protos) #t #f))
      (assert-true (if (memq 'vehicle-set-speed protos) #t #f))
      (assert-true (if (memq 'vehicle-fuel protos) #t #f))
      (assert-true (if (memq 'vehicle-set-fuel protos) #t #f)))))

;;; ====
;;; Tests: Polymorphic Usage
;;; ====

(test-group "Polymorphic Usage"
  (define-test "mixed vehicle list speed access"
    (let* ([vehicles (list (make-car 60 100 'red)
                           (make-bike 25 3)
                           (make-scooter 20 80))]
           [speeds (map vehicle-speed vehicles)])
      (assert-equal '(60 25 20) speeds)))

  (define-test "mixed vehicle list fuel access"
    (let* ([vehicles (list (make-car 60 100 'red)
                           (make-bike 25 3)
                           (make-scooter 20 80))]
           [fuels (map vehicle-fuel vehicles)])
      (assert-equal '(100 0 80) fuels)))

  (define-test "total fuel across fleet"
    (let* ([vehicles (list (make-car 60 50 'red)
                           (make-car 80 30 'blue)
                           (make-scooter 20 20))]
           [total (apply + (map vehicle-fuel vehicles))])
      (assert-equal 100 total))))

;;; ====
;;; Tests: Helper Functions
;;; ====

(test-group "Helper Functions"
  (define-test "build-getter-name"
    (assert-equal 'foo-bar (build-getter-name 'foo "bar")))

  (define-test "build-setter-name"
    (assert-equal 'foo-with-bar (build-setter-name 'foo "bar")))

  (define-test "find-override finds match"
    (let ([overrides '(("a" ga sa) ("b" gb sb))])
      (assert-equal '("b" gb sb) (find-override "b" overrides))))

  (define-test "find-override returns #f on miss"
    (let ([overrides '(("a" ga sa))])
      (assert-false (find-override "c" overrides)))))

;;; ====
;;; Tests: derive-bundle! with 4+ overrides (fold-zxmc fix)
;;; ====

;;; Define protocols for a 4-slot bundle
(define-protocol (robot-x r) "Get robot X position")
(define-protocol (robot-set-x r x) "Set robot X position")
(define-protocol (robot-y r) "Get robot Y position")
(define-protocol (robot-set-y r y) "Set robot Y position")
(define-protocol (robot-charge r) "Get robot charge")
(define-protocol (robot-set-charge r c) "Set robot charge")
(define-protocol (robot-status r) "Get robot status")
(define-protocol (robot-set-status r s) "Set robot status")

;;; 4-slot bundle
(define-protocol-bundle robot-ops
  ((robot-x robot-set-x) "x")
  ((robot-y robot-set-y) "y")
  ((robot-charge robot-set-charge) "charge")
  ((robot-status robot-set-status) "status"))

;;; Drone: all slots use overrides (tests 4 simultaneous overrides)
;;; Structure: (list 'drone px py battery mode)
(define (make-drone px py battery mode)
  (list 'drone px py battery mode))

;;; derive-bundle! with 4 overrides - previously impossible!
(derive-bundle! robot-ops 'drone drone
  ("x" (lambda (d) (cadr d)) (lambda (d v) (list 'drone v (caddr d) (cadddr d) (car (cddddr d)))))
  ("y" (lambda (d) (caddr d)) (lambda (d v) (list 'drone (cadr d) v (cadddr d) (car (cddddr d)))))
  ("charge" (lambda (d) (cadddr d)) (lambda (d v) (list 'drone (cadr d) (caddr d) v (car (cddddr d)))))
  ("status" (lambda (d) (car (cddddr d))) (lambda (d v) (list 'drone (cadr d) (caddr d) (cadddr d) v))))

(test-group "derive-bundle! with 4+ overrides"
  (define-test "4-override derive works - x getter"
    (let ([d (make-drone 10 20 100 'active)])
      (assert-equal 10 (robot-x d))))

  (define-test "4-override derive works - y getter"
    (let ([d (make-drone 10 20 100 'active)])
      (assert-equal 20 (robot-y d))))

  (define-test "4-override derive works - charge getter"
    (let ([d (make-drone 10 20 100 'active)])
      (assert-equal 100 (robot-charge d))))

  (define-test "4-override derive works - status getter"
    (let ([d (make-drone 10 20 100 'active)])
      (assert-equal 'active (robot-status d))))

  (define-test "4-override derive works - x setter"
    (let* ([d (make-drone 10 20 100 'active)]
           [d2 (robot-set-x d 50)])
      (assert-equal 50 (robot-x d2))
      (assert-equal 20 (robot-y d2))))

  (define-test "4-override derive works - status setter"
    (let* ([d (make-drone 10 20 100 'active)]
           [d2 (robot-set-status d 'idle)])
      (assert-equal 'idle (robot-status d2))
      (assert-equal 100 (robot-charge d2)))))

;;; ====
;;; Tests: Composable Type Requirements (fold-zxlt)
;;; ====

;;; Define additional protocols for composition testing
(define-protocol (entity-health e) "Get entity health")
(define-protocol (entity-set-health e h) "Set entity health")

(define-protocol-bundle health-ops
  ((entity-health entity-set-health) "health"))

;;; Warrior: implements both vehicle-ops and health-ops
;;; Structure: (list 'warrior speed fuel health)
(define (make-warrior speed fuel health)
  (list 'warrior speed fuel health))

(define (warrior-speed w) (cadr w))
(define (warrior-fuel w) (caddr w))
(define (warrior-health w) (cadddr w))

(define (warrior-with-speed w s)
  (list 'warrior s (warrior-fuel w) (warrior-health w)))
(define (warrior-with-fuel w f)
  (list 'warrior (warrior-speed w) f (warrior-health w)))
(define (warrior-with-health w h)
  (list 'warrior (warrior-speed w) (warrior-fuel w) h))

(derive-bundle! vehicle-ops 'warrior warrior)
(derive-bundle! health-ops 'warrior warrior)

(test-group "implements-bundle?"
  (define-test "type implementing bundle returns true"
    (assert-true (implements-bundle? 'car vehicle-ops)))

  (define-test "type not implementing bundle returns false"
    (assert-false (implements-bundle? 'unknown-type vehicle-ops)))

  (define-test "warrior implements vehicle-ops"
    (assert-true (implements-bundle? 'warrior vehicle-ops)))

  (define-test "warrior implements health-ops"
    (assert-true (implements-bundle? 'warrior health-ops)))

  (define-test "car does not implement health-ops"
    (assert-false (implements-bundle? 'car health-ops))))

(test-group "compose-bundles"
  (define-test "compose creates new bundle"
    (let ([combined (compose-bundles 'combined-ops vehicle-ops health-ops)])
      (assert-true (bundle? combined))))

  (define-test "composed bundle has correct name"
    (let ([combined (compose-bundles 'test-combined vehicle-ops health-ops)])
      (assert-equal 'test-combined (bundle-name combined))))

  (define-test "composed bundle has all slots"
    (let ([combined (compose-bundles 'full-ops vehicle-ops health-ops)])
      (assert-equal 3 (bundle-slot-count combined))))  ; speed, fuel, health

  (define-test "warrior implements composed bundle"
    (let ([combined (compose-bundles 'warrior-reqs vehicle-ops health-ops)])
      (assert-true (implements-bundle? 'warrior combined))))

  (define-test "car does not implement composed bundle"
    (let ([combined (compose-bundles 'game-obj-ops vehicle-ops health-ops)])
      (assert-false (implements-bundle? 'car combined)))))

(test-group "assert-bundle!"
  (define-test "assert passes for valid type"
    (let ([c (make-car 60 100 'red)])
      (assert-equal c (assert-bundle! c vehicle-ops))))

  (define-test "assert returns the value"
    (let* ([c (make-car 60 100 'red)]
           [result (assert-bundle! c vehicle-ops)])
      (assert-equal 60 (vehicle-speed result))))

  (define-test "assert throws for invalid type"
    (assert-error (lambda () (assert-bundle! '(unknown-type) vehicle-ops))))

  (define-test "warrior passes assert for composed bundle"
    (let ([w (make-warrior 50 80 100)]
          [combined (compose-bundles 'w-test vehicle-ops health-ops)])
      (assert-equal w (assert-bundle! w combined)))))

(test-group "missing-protocols"
  (define-test "no missing protocols for valid type"
    (let ([missing (missing-protocols 'car vehicle-ops)])
      (assert-equal '() missing)))

  (define-test "lists missing protocols for invalid type"
    (let ([missing (missing-protocols 'car health-ops)])
      (assert-true (> (length missing) 0)))))

;;; ====
;;; Run Tests
;;; ====

(run-all-tests)
