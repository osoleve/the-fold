;;; lattice/fp/test-protocol-bundle.ss — Tests for Protocol Bundle System
;;;
;;; Run with: scheme --script lattice/fp/test-protocol-bundle.ss

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
;;; Run Tests
;;; ====

(run-all-tests)
