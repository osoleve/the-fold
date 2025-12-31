;;; End-to-end test for .ss compatibility features
;;; Tests: top-level-bound?, unless, when, let with multiple body forms

;;; Test 1: top-level-bound? returns false for undefined symbol
(define test1 (top-level-bound? 'undefined-symbol))

;;; Test 2: top-level-bound? returns true for defined symbol
(define my-var 42)
(define test2 (top-level-bound? 'my-var))

;;; Test 3: unless with false condition evaluates body
(define test3 (unless #f 100))

;;; Test 4: unless with true condition returns nil
(define test4 (unless #t 999))

;;; Test 5: when with true condition evaluates body
(define test5 (when #t 200))

;;; Test 6: when with false condition returns nil
(define test6 (when #f 999))

;;; Test 7: unless with conditional load pattern
(define test7 (unless (top-level-bound? 'undefined-var) 300))

;;; Test 8: when with conditional pattern
(define test8 (when (top-level-bound? 'my-var) 400))

;;; Test 9: let with multiple body expressions (implicit begin)
(define test9
  (let ([a 1])
       (+ a 1)
       (+ a 2)
       (+ a 500)))

;;; Create result list
(prim 'vec-make
      test1    ; #f
      test2    ; #t
      test3    ; 100
      test4    ; ()
      test5    ; 200
      test6    ; ()
      test7    ; 300
      test8    ; 400
      test9)   ; 501
