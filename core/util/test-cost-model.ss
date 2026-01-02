;;; core/util/test-cost-model.ss --- Tests for Cost Model Abstraction
;;;
;;; Tests:
;;;   - Cost model creation and accessors
;;;   - Built-in cost models
;;;   - Cost tracker accumulation
;;;   - Cost model composition

(load "core/util/cost-model.ss")

(display "Cost Model Tests\n")
(display "================\n\n")

(define tests-passed 0)
(define tests-failed 0)

(define (test name expected actual)
  (if (equal? expected actual)
      (begin
       (set! tests-passed (+ tests-passed 1))
       (display "  + ") (display name) (newline))
      (begin
       (set! tests-failed (+ tests-failed 1))
       (display "  X ") (display name)
       (display " - expected ") (write expected)
       (display ", got ") (write actual)
       (newline))))

;;; ============================================================
;;; Cost Model Creation Tests
;;; ============================================================

(display "Cost Model Creation:\n")

(let ([cm (make-cost-model
           'test-model
           (lambda (e) 5)
           (lambda (p) 10)
           3
           (lambda (xs) (fold-left + 0 xs)))])
     (test "cost-model?" #t (cost-model? cm))
     (test "cost-model-name" 'test-model (cost-model-name cm))
     (test "cost-model-apply-cost" 3 (cost-model-apply-cost cm))
     (test "eval-cost function works" 5 ((cost-model-eval-cost cm) 'any))
     (test "prim-cost function works" 10 ((cost-model-prim-cost cm) 'any))
     (test "aggregate function works" 15 ((cost-model-aggregate cm) '(5 10))))

(test "cost-model? on non-model" #f (cost-model? '(not a cost model)))
(test "cost-model? on number" #f (cost-model? 42))

;;; ============================================================
;;; Fuel Cost Model Tests
;;; ============================================================

(display "\nFuel Cost Model:\n")

(test "fuel-cost-model?" #t (cost-model? fuel-cost-model))
(test "fuel-cost-model name" 'fuel (cost-model-name fuel-cost-model))
(test "fuel eval-cost is 1" 1 (compute-eval-cost fuel-cost-model 'anything))
(test "fuel eval-cost for number" 1 (compute-eval-cost fuel-cost-model 42))
(test "fuel eval-cost for expr" 1 (compute-eval-cost fuel-cost-model '(+ 1 2)))
(test "fuel prim-cost is 0" 0 (compute-prim-cost fuel-cost-model 'add))
(test "fuel apply-cost is 1" 1 (cost-model-apply-cost fuel-cost-model))
(test "fuel aggregate sums" 10 (compute-aggregate fuel-cost-model '(1 2 3 4)))
(test "fuel aggregate empty" 0 (compute-aggregate fuel-cost-model '()))

;;; ============================================================
;;; Weighted Cost Model Tests
;;; ============================================================

(display "\nWeighted Cost Model:\n")

(test "weighted-cost-model?" #t (cost-model? weighted-cost-model))
(test "weighted-cost-model name" 'weighted (cost-model-name weighted-cost-model))
(test "weighted symbol cost" 1 (compute-eval-cost weighted-cost-model 'x))
(test "weighted number cost" 1 (compute-eval-cost weighted-cost-model 42))
(test "weighted lambda cost" 2 (compute-eval-cost weighted-cost-model '(fn (x) x)))
(test "weighted let cost" 2 (compute-eval-cost weighted-cost-model '(let ((x 1)) x)))
(test "weighted if cost" 2 (compute-eval-cost weighted-cost-model '(if #t 1 2)))
(test "weighted fix cost" 3 (compute-eval-cost weighted-cost-model '(fix f (fn (x) x))))
(test "weighted call cost" 3 (compute-eval-cost weighted-cost-model '(call f 1)))
(test "weighted apply-cost is 2" 2 (cost-model-apply-cost weighted-cost-model))

(test "weighted prim add cost" 1 (compute-prim-cost weighted-cost-model 'add))
(test "weighted prim div cost" 2 (compute-prim-cost weighted-cost-model 'div))
(test "weighted prim cons cost" 1 (compute-prim-cost weighted-cost-model 'cons))

;;; ============================================================
;;; Memory Cost Model Tests
;;; ============================================================

(display "\nMemory Cost Model:\n")

(test "memory-cost-model?" #t (cost-model? memory-cost-model))
(test "memory-cost-model name" 'memory (cost-model-name memory-cost-model))
(test "memory symbol cost" 0 (compute-eval-cost memory-cost-model 'x))
(test "memory pair cost" 1 (compute-eval-cost memory-cost-model '(a b)))
(test "memory prim cons cost" 2 (compute-prim-cost memory-cost-model 'cons))
(test "memory prim list cost" 3 (compute-prim-cost memory-cost-model 'list))
(test "memory prim append cost" 5 (compute-prim-cost memory-cost-model 'append))
(test "memory prim map cost" 4 (compute-prim-cost memory-cost-model 'map))
(test "memory prim add cost" 0 (compute-prim-cost memory-cost-model 'add))

;;; ============================================================
;;; Max Depth Cost Model Tests
;;; ============================================================

(display "\nMax Depth Cost Model:\n")

(test "max-depth-model?" #t (cost-model? max-depth-model))
(test "max-depth-model name" 'max-depth (cost-model-name max-depth-model))
(test "max-depth eval-cost" 1 (compute-eval-cost max-depth-model 'anything))
(test "max-depth aggregate takes max" 5 (compute-aggregate max-depth-model '(1 3 5 2)))
(test "max-depth aggregate empty" 0 (compute-aggregate max-depth-model '()))
(test "max-depth aggregate single" 7 (compute-aggregate max-depth-model '(7)))

;;; ============================================================
;;; Cost Tracker Tests
;;; ============================================================

(display "\nCost Tracker:\n")

(let ([ct (make-cost-tracker fuel-cost-model)])
     (test "cost-tracker?" #t (cost-tracker? ct))
     (test "cost-tracker model" fuel-cost-model (cost-tracker-model ct))
     (test "initial costs empty" '() (get-costs ct))
     (test "initial total is 0" 0 (get-total-cost ct)))

(test "cost-tracker? on non-tracker" #f (cost-tracker? '(not a tracker)))

;;; Cost accumulation
(display "\nCost Accumulation:\n")

(let* ([ct0 (make-cost-tracker fuel-cost-model)]
       [ct1 (track-cost ct0 'eval 5)]
       [ct2 (track-cost ct1 'eval 3)]
       [ct3 (track-cost ct2 'apply 2)])
      (test "single cost tracked" 5 (get-category-cost ct1 'eval))
      (test "costs accumulate" 8 (get-category-cost ct2 'eval))
      (test "separate category" 2 (get-category-cost ct3 'apply))
      (test "total cost" 10 (get-total-cost ct3))
      (test "missing category is 0" 0 (get-category-cost ct3 'nonexistent)))

;;; Multiple categories
(display "\nMultiple Categories:\n")

(let* ([ct0 (make-cost-tracker fuel-cost-model)]
       [ct1 (track-cost ct0 'eval 10)]
       [ct2 (track-cost ct1 'prim 5)]
       [ct3 (track-cost ct2 'apply 7)]
       [ct4 (track-cost ct3 'eval 3)])
      (test "eval accumulated" 13 (get-category-cost ct4 'eval))
      (test "prim tracked" 5 (get-category-cost ct4 'prim))
      (test "apply tracked" 7 (get-category-cost ct4 'apply))
      (test "total all categories" 25 (get-total-cost ct4)))

;;; Tracker reset
(display "\nTracker Reset:\n")

(let* ([ct0 (make-cost-tracker fuel-cost-model)]
       [ct1 (track-cost ct0 'eval 100)]
       [ct2 (reset-tracker ct1)])
      (test "reset clears costs" '() (get-costs ct2))
      (test "reset keeps model" fuel-cost-model (cost-tracker-model ct2))
      (test "original unchanged" 100 (get-category-cost ct1 'eval)))

;;; Max-depth tracker
(display "\nMax-Depth Tracker:\n")

(let* ([ct0 (make-cost-tracker max-depth-model)]
       [ct1 (track-cost ct0 'depth-a 3)]
       [ct2 (track-cost ct1 'depth-b 7)]
       [ct3 (track-cost ct2 'depth-c 5)])
      (test "max-depth total" 7 (get-total-cost ct3)))

;;; ============================================================
;;; Cost Model Composition Tests
;;; ============================================================

(display "\nCost Model Composition:\n")

;;; Combining models
(let ([combined (combine-cost-models fuel-cost-model weighted-cost-model)])
     (test "combined model?" #t (cost-model? combined))
     (test "combined name" 'fuel+weighted (cost-model-name combined))
     (test "combined eval-cost" 3 (compute-eval-cost combined '(fn (x) x)))  ; 1 + 2
     (test "combined apply-cost" 3 (cost-model-apply-cost combined))  ; 1 + 2
     (test "combined prim-cost" 2 (compute-prim-cost combined 'div)))  ; 0 + 2

;;; Scaling models
(let ([scaled (scale-cost-model fuel-cost-model 10)])
     (test "scaled model?" #t (cost-model? scaled))
     (test "scaled name" 'fuel*10 (cost-model-name scaled))
     (test "scaled eval-cost" 10 (compute-eval-cost scaled 'anything))
     (test "scaled apply-cost" 10 (cost-model-apply-cost scaled))
     (test "scaled prim-cost" 0 (compute-prim-cost scaled 'add)))  ; 0 * 10

;;; Scale weighted
(let ([scaled (scale-cost-model weighted-cost-model 3)])
     (test "scaled weighted lambda" 6 (compute-eval-cost scaled '(fn (x) x)))  ; 2 * 3
     (test "scaled weighted fix" 9 (compute-eval-cost scaled '(fix f (fn (x) x)))))  ; 3 * 3

;;; ============================================================
;;; Edge Cases
;;; ============================================================

(display "\nEdge Cases:\n")

;;; Empty lists
(test "aggregate empty list (fuel)" 0 (compute-aggregate fuel-cost-model '()))
(test "aggregate empty list (weighted)" 0 (compute-aggregate weighted-cost-model '()))
(test "aggregate empty list (max)" 0 (compute-aggregate max-depth-model '()))

;;; Single values
(test "aggregate single (fuel)" 42 (compute-aggregate fuel-cost-model '(42)))
(test "aggregate single (max)" 42 (compute-aggregate max-depth-model '(42)))

;;; Zero costs
(let* ([ct0 (make-cost-tracker fuel-cost-model)]
       [ct1 (track-cost ct0 'zero 0)]
       [ct2 (track-cost ct1 'zero 0)])
      (test "zero costs track" 0 (get-category-cost ct2 'zero))
      (test "zero costs total" 0 (get-total-cost ct2)))

;;; Boolean expressions
(test "weighted bool true" 1 (compute-eval-cost weighted-cost-model #t))
(test "weighted bool false" 1 (compute-eval-cost weighted-cost-model #f))

;;; String expressions
(test "weighted string" 1 (compute-eval-cost weighted-cost-model "hello"))

;;; ============================================================
;;; Immutability Tests
;;; ============================================================

(display "\nImmutability:\n")

(let* ([ct0 (make-cost-tracker fuel-cost-model)]
       [ct1 (track-cost ct0 'eval 10)])
      (test "original tracker unchanged" '() (get-costs ct0))
      (test "new tracker has cost" 10 (get-category-cost ct1 'eval)))

;;; ============================================================
;;; Summary
;;; ============================================================

(newline)
(display "================\n")
(display (format "Tests: ~a passed, ~a failed\n" tests-passed tests-failed))
(when (> tests-failed 0)
      (exit 1))
