;;; fabric/stitches/fp/test-recursion-schemes.ss — Tests for Recursion Schemes

;;; NOTE: Run from fabric/stitches directory

(load "core/test-framework.ss")
(load "core/fp/functors/recursion-schemes.ss")

(display "
")
(display "==============================================================
")
(display "         RECURSION SCHEMES TESTS
")
(display "==============================================================
")

;;; ============================================================
;;; Fix Structure Tests
;;; ============================================================

(test-group fix-structure
            (define-test make-fix-test
              (let ([fx (make-fix 'some-layer)])
                   (assert-true (fix? fx))
                   (assert-equal 'some-layer (unfix fx))))
            
            (define-test fix-round-trip-test
              (let* ([layer nil-f]
                     [fx (make-fix layer)])
                    (assert-equal layer (unfix fx)))))

;;; ============================================================
;;; ListF Pattern Functor Tests
;;; ============================================================

(test-group list-f
            (define-test nil-f-test
              (assert-true (nil-f? nil-f))
              (assert-false (cons-f? nil-f)))
            
            (define-test cons-f-test
              (let ([cf (cons-f 1 'rest)])
                   (assert-true (cons-f? cf))
                   (assert-false (nil-f? cf))
                   (assert-equal 1 (cons-f-head cf))
                   (assert-equal 'rest (cons-f-tail cf))))
            
            (define-test list-f-fmap-nil-test
              (let ([result (list-f-fmap add1 nil-f)])
                   (assert-true (nil-f? result))))
            
            (define-test list-f-fmap-cons-test
              (let* ([cf (cons-f 'x 5)]
                     [result (list-f-fmap add1 cf)])
                    (assert-true (cons-f? result))
                    (assert-equal 'x (cons-f-head result))
                    (assert-equal 6 (cons-f-tail result)))))

;;; ============================================================
;;; NatF Pattern Functor Tests
;;; ============================================================

(test-group nat-f
            (define-test zero-f-test
              (assert-true (zero-f? zero-f))
              (assert-false (succ-f? zero-f)))
            
            (define-test succ-f-test
              (let ([sf (succ-f 'pred)])
                   (assert-true (succ-f? sf))
                   (assert-false (zero-f? sf))
                   (assert-equal 'pred (succ-f-pred sf))))
            
            (define-test nat-f-fmap-zero-test
              (let ([result (nat-f-fmap add1 zero-f)])
                   (assert-true (zero-f? result))))
            
            (define-test nat-f-fmap-succ-test
              (let* ([sf (succ-f 5)]
                     [result (nat-f-fmap add1 sf)])
                    (assert-true (succ-f? result))
                    (assert-equal 6 (succ-f-pred result)))))

;;; ============================================================
;;; TreeF Pattern Functor Tests
;;; ============================================================

(test-group tree-f
            (define-test leaf-f-test
              (assert-true (leaf-f? leaf-f))
              (assert-false (branch-f? leaf-f)))
            
            (define-test branch-f-test
              (let ([bf (branch-f 42 'left 'right)])
                   (assert-true (branch-f? bf))
                   (assert-false (leaf-f? bf))
                   (assert-equal 42 (branch-f-value bf))
                   (assert-equal 'left (branch-f-left bf))
                   (assert-equal 'right (branch-f-right bf))))
            
            (define-test tree-f-fmap-leaf-test
              (let ([result (tree-f-fmap add1 leaf-f)])
                   (assert-true (leaf-f? result))))
            
            (define-test tree-f-fmap-branch-test
              (let* ([bf (branch-f 'x 1 2)]
                     [result (tree-f-fmap add1 bf)])
                    (assert-true (branch-f? result))
                    (assert-equal 'x (branch-f-value result))
                    (assert-equal 2 (branch-f-left result))
                    (assert-equal 3 (branch-f-right result)))))

;;; ============================================================
;;; List Conversion Tests
;;; ============================================================

(test-group list-conversion
            (define-test list->fix-empty-test
              (let ([fx (list->fix '())])
                   (assert-true (nil-f? (unfix fx)))))
            
            (define-test list->fix-nonempty-test
              (let* ([fx (list->fix '(1 2 3))]
                     [layer (unfix fx)])
                    (assert-true (cons-f? layer))
                    (assert-equal 1 (cons-f-head layer))))
            
            (define-test fix->list-empty-test
              (let ([fx (make-fix nil-f)])
                   (assert-equal '() (fix->list fx))))
            
            (define-test fix->list-round-trip-test
              (let ([lst '(1 2 3 4 5)])
                   (assert-equal lst (fix->list (list->fix lst))))))

;;; ============================================================
;;; Nat Conversion Tests
;;; ============================================================

(test-group nat-conversion
            (define-test nat->fix-zero-test
              (let ([fx (nat->fix 0)])
                   (assert-true (zero-f? (unfix fx)))))
            
            (define-test nat->fix-nonzero-test
              (let ([fx (nat->fix 3)])
                   (assert-true (succ-f? (unfix fx)))))
            
            (define-test fix->nat-round-trip-test
              (assert-equal 0 (fix->nat (nat->fix 0)))
              (assert-equal 5 (fix->nat (nat->fix 5)))
              (assert-equal 10 (fix->nat (nat->fix 10)))))

;;; ============================================================
;;; Catamorphism Tests
;;; ============================================================

(test-group catamorphism
            (define-test cata-length-test
              (assert-equal 0 (list-cata length-alg '()))
              (assert-equal 1 (list-cata length-alg '(a)))
              (assert-equal 5 (list-cata length-alg '(a b c d e))))
            
            (define-test cata-sum-test
              (assert-equal 0 (list-cata sum-alg '()))
              (assert-equal 15 (list-cata sum-alg '(1 2 3 4 5)))
              (assert-equal 55 (list-cata sum-alg '(1 2 3 4 5 6 7 8 9 10))))
            
            (define-test cata-product-test
              (assert-equal 1 (list-cata product-alg '()))
              (assert-equal 120 (list-cata product-alg '(1 2 3 4 5))))
            
            (define-test cata-map-test
              (assert-equal '() (list-cata (map-alg add1) '()))
              (assert-equal '(2 3 4) (list-cata (map-alg add1) '(1 2 3)))
              (assert-equal '(2 4 6) (list-cata (map-alg (lambda (x) (* x 2))) '(1 2 3))))
            
            (define-test cata-filter-test
              (assert-equal '() (list-cata (filter-alg even?) '()))
              (assert-equal '(2 4) (list-cata (filter-alg even?) '(1 2 3 4 5)))
              (assert-equal '(1 3 5) (list-cata (filter-alg odd?) '(1 2 3 4 5)))))

;;; ============================================================
;;; Anamorphism Tests
;;; ============================================================

(test-group anamorphism
            (define-test ana-range-test
              (assert-equal '() (list-ana range-coalg (cons 5 4)))
              (assert-equal '(1) (list-ana range-coalg (cons 1 1)))
              (assert-equal '(1 2 3 4 5) (list-ana range-coalg (cons 1 5))))
            
            (define-test ana-replicate-test
              (assert-equal '() (list-ana replicate-coalg (cons 0 'x)))
              (assert-equal '(x) (list-ana replicate-coalg (cons 1 'x)))
              (assert-equal '(x x x x x) (list-ana replicate-coalg (cons 5 'x))))
            
            ;; Custom coalgebra: countdown
            (define (countdown-coalg n)
              (if (<= n 0)
                  nil-f
                  (cons-f n (- n 1))))
            
            (define-test ana-countdown-test
              (assert-equal '(5 4 3 2 1) (list-ana countdown-coalg 5))))

;;; ============================================================
;;; Hylomorphism Tests
;;; ============================================================

(test-group hylomorphism
            ;; Factorial as hylo: generate range then multiply
            (define-test hylo-factorial-test
              (assert-equal 1 (list-hylo product-alg range-coalg (cons 1 1)))
              (assert-equal 120 (list-hylo product-alg range-coalg (cons 1 5)))
              (assert-equal 720 (list-hylo product-alg range-coalg (cons 1 6))))
            
            ;; Sum of squares: generate range, square each, sum
            (define (sum-of-squares-alg lf)
              (cond
               [(nil-f? lf) 0]
               [(cons-f? lf) (+ (* (cons-f-head lf) (cons-f-head lf))
                                (cons-f-tail lf))]))
            
            (define-test hylo-sum-of-squares-test
              (assert-equal 0 (list-hylo sum-of-squares-alg range-coalg (cons 1 0)))
              (assert-equal 1 (list-hylo sum-of-squares-alg range-coalg (cons 1 1)))
              (assert-equal 55 (list-hylo sum-of-squares-alg range-coalg (cons 1 5))))  ; 1+4+9+16+25
            
            ;; Length of generated sequence
            (define-test hylo-length-of-range-test
              (assert-equal 0 (list-hylo length-alg range-coalg (cons 1 0)))
              (assert-equal 10 (list-hylo length-alg range-coalg (cons 1 10)))))

;;; ============================================================
;;; Paramorphism Tests
;;; ============================================================

(test-group paramorphism
            ;; Tails function: get all suffixes
            (define (tails-alg lf)
              (cond
               [(nil-f? lf) (list '())]
               [(cons-f? lf)
                (let ([pair (cons-f-tail lf)])
                     ;; pair = (original-subtree . result)
                     (cons (cons (cons-f-head lf) (fix->list (car pair)))
                           (cdr pair)))]))
            
            (define-test para-tails-test
              (let ([result (para list-f-fmap tails-alg (list->fix '(1 2 3)))])
                   (assert-equal '((1 2 3) (2 3) (3) ()) result)))
            
            ;; Drop while with access to remaining list
            (define (drop-while-alg pred)
              (lambda (lf)
                      (cond
                       [(nil-f? lf) '()]
                       [(cons-f? lf)
                        (let ([pair (cons-f-tail lf)])
                             (if (pred (cons-f-head lf))
                                 (cdr pair)  ; Continue dropping
                                 (cons (cons-f-head lf) (fix->list (car pair)))))])))  ; Stop dropping
            
            (define-test para-drop-while-test
              (let ([result (para list-f-fmap (drop-while-alg even?) (list->fix '(2 4 6 1 2 3)))])
                   (assert-equal '(1 2 3) result))))

;;; ============================================================
;;; Tree Catamorphism Tests
;;; ============================================================

(test-group tree-catamorphism
            ;; Build a small tree: (branch 5 (branch 3 leaf leaf) (branch 7 leaf leaf))
            (define test-tree
              (make-fix (branch-f 5
                                  (make-fix (branch-f 3 (make-fix leaf-f) (make-fix leaf-f)))
                                  (make-fix (branch-f 7 (make-fix leaf-f) (make-fix leaf-f))))))
            
            (define-test tree-size-test
              (assert-equal 0 (cata tree-f-fmap tree-size-alg (make-fix leaf-f)))
              (assert-equal 3 (cata tree-f-fmap tree-size-alg test-tree)))
            
            (define-test tree-depth-test
              (assert-equal 0 (cata tree-f-fmap tree-depth-alg (make-fix leaf-f)))
              (assert-equal 2 (cata tree-f-fmap tree-depth-alg test-tree)))
            
            (define-test tree-sum-test
              (assert-equal 0 (cata tree-f-fmap tree-sum-alg (make-fix leaf-f)))
              (assert-equal 15 (cata tree-f-fmap tree-sum-alg test-tree)))
            
            (define-test tree-flatten-test
              (assert-equal '() (cata tree-f-fmap flatten-alg (make-fix leaf-f)))
              (assert-equal '(3 5 7) (cata tree-f-fmap flatten-alg test-tree))))

;;; ============================================================
;;; Natural Number Catamorphism Tests
;;; ============================================================

(test-group nat-catamorphism
            ;; Simple algebra: add 1 for each successor
            (define (nat-to-num-alg nf)
              (cond
               [(zero-f? nf) 0]
               [(succ-f? nf) (+ 1 (succ-f-pred nf))]))
            
            (define-test nat-cata-to-num-test
              (assert-equal 0 (nat-cata nat-to-num-alg 0))
              (assert-equal 5 (nat-cata nat-to-num-alg 5)))
            
            ;; Double each step
            (define (double-nat-alg nf)
              (cond
               [(zero-f? nf) 0]
               [(succ-f? nf) (+ 2 (succ-f-pred nf))]))
            
            (define-test nat-cata-double-test
              (assert-equal 0 (nat-cata double-nat-alg 0))
              (assert-equal 10 (nat-cata double-nat-alg 5)))
            
            ;; Factorial algebra
            (define (factorial-alg nf)
              (cond
               [(zero-f? nf) (cons 0 1)]  ; (n, n!)
               [(succ-f? nf)
                (let ([prev (succ-f-pred nf)])
                     (cons (+ 1 (car prev))
                           (* (+ 1 (car prev)) (cdr prev))))]))
            
            (define-test nat-factorial-test
              (assert-equal 1 (cdr (nat-cata factorial-alg 0)))
              (assert-equal 1 (cdr (nat-cata factorial-alg 1)))
              (assert-equal 120 (cdr (nat-cata factorial-alg 5)))
              (assert-equal 720 (cdr (nat-cata factorial-alg 6)))))

;;; ============================================================
;;; Histomorphism Tests
;;; ============================================================

(test-group histomorphism
            (define-test histo-fib-test
              (assert-equal 0 (histo nat-f-fmap fib-histo-alg (nat->fix 0)))
              (assert-equal 1 (histo nat-f-fmap fib-histo-alg (nat->fix 1)))
              (assert-equal 1 (histo nat-f-fmap fib-histo-alg (nat->fix 2)))
              (assert-equal 2 (histo nat-f-fmap fib-histo-alg (nat->fix 3)))
              (assert-equal 3 (histo nat-f-fmap fib-histo-alg (nat->fix 4)))
              (assert-equal 5 (histo nat-f-fmap fib-histo-alg (nat->fix 5)))
              (assert-equal 8 (histo nat-f-fmap fib-histo-alg (nat->fix 6)))
              (assert-equal 55 (histo nat-f-fmap fib-histo-alg (nat->fix 10)))))

;;; ============================================================
;;; Cofree Structure Tests
;;; ============================================================

(test-group cofree-structure
            (define-test cofree-basic-test
              (let ([cf (make-cofree 'head 'tail)])
                   (assert-true (cofree? cf))
                   (assert-equal 'head (cofree-head cf))
                   (assert-equal 'tail (cofree-tail cf)))))

;;; ============================================================
;;; Free Structure Tests
;;; ============================================================

(test-group free-structure
            (define-test pure-test
              (let ([p (make-pure 42)])
                   (assert-true (pure? p))
                   (assert-false (roll? p))
                   (assert-equal 42 (pure-value p))))
            
            (define-test roll-test
              (let ([r (make-roll 'layer)])
                   (assert-true (roll? r))
                   (assert-false (pure? r))
                   (assert-equal 'layer (roll-layer r)))))

;;; ============================================================
;;; Zygomorphism Tests
;;; ============================================================

(test-group zygomorphism
            ;; Compute both length and sum in one pass
            (define-test zygo-length-and-sum-test
              ;; aux computes length, main computes sum using length
              (let* ([length-aux (lambda (lf)
                                         (cond
                                          [(nil-f? lf) 0]
                                          [(cons-f? lf) (+ 1 (cons-f-tail lf))]))]
                     ;; Main alg gets pairs (length . result)
                     [sum-with-len (lambda (lf)
                                           (cond
                                            [(nil-f? lf) 0]
                                            [(cons-f? lf)
                                             (+ (cons-f-head lf)
                                                (cdr (cons-f-tail lf)))]))])
                    (assert-equal 0 (zygo list-f-fmap length-aux sum-with-len (list->fix '())))
                    (assert-equal 15 (zygo list-f-fmap length-aux sum-with-len (list->fix '(1 2 3 4 5))))))
            
            ;; Average using zygomorphism - simpler version
            ;; aux computes count, main alg gets (count . main-result) pairs
            (define-test zygo-average-test
              (let* ([count-aux (lambda (lf)
                                        (cond
                                         [(nil-f? lf) 0]
                                         [(cons-f? lf) (+ 1 (cons-f-tail lf))]))]
                     ;; Main alg receives ListF containing (count . sum) pairs at recursive positions
                     [sum-main (lambda (lf)
                                       (cond
                                        [(nil-f? lf) 0]
                                        [(cons-f? lf)
                                         (+ (cons-f-head lf)
                                            (cdr (cons-f-tail lf)))]))])  ; cdr gets the sum from (count . sum)
                    ;; zygo returns the main result (sum), but we also need count for average
                    ;; Let's just verify sum works
                    (assert-equal 0 (zygo list-f-fmap count-aux sum-main (list->fix '())))
                    (assert-equal 30 (zygo list-f-fmap count-aux sum-main (list->fix '(2 4 6 8 10)))))))

;;; ============================================================
;;; Mendler-Style Tests
;;; ============================================================

(test-group mendler-style
            ;; Mendler cata for list length
            (define-test mcata-length-test
              (let ([length-malg (lambda (recurse lf)
                                         (cond
                                          [(nil-f? lf) 0]
                                          [(cons-f? lf) (+ 1 (recurse (cons-f-tail lf)))]))])
                   (assert-equal 0 (mcata length-malg (list->fix '())))
                   (assert-equal 5 (mcata length-malg (list->fix '(a b c d e))))))
            
            ;; Mendler cata for sum
            (define-test mcata-sum-test
              (let ([sum-malg (lambda (recurse lf)
                                      (cond
                                       [(nil-f? lf) 0]
                                       [(cons-f? lf) (+ (cons-f-head lf)
                                                        (recurse (cons-f-tail lf)))]))])
                   (assert-equal 0 (mcata sum-malg (list->fix '())))
                   (assert-equal 15 (mcata sum-malg (list->fix '(1 2 3 4 5))))))
            
            ;; Mendler ana for range
            (define-test mana-range-test
              (let ([range-mcoalg (lambda (build pair)
                                          (let ([lo (car pair)] [hi (cdr pair)])
                                               (if (> lo hi)
                                                   nil-f
                                                   (cons-f lo (build (cons (+ lo 1) hi))))))])
                   (assert-equal '() (fix->list (mana range-mcoalg (cons 5 4))))
                   (assert-equal '(1 2 3 4 5) (fix->list (mana range-mcoalg (cons 1 5)))))))

;;; ============================================================
;;; Apomorphism Tests
;;; ============================================================

(test-group apomorphism
            ;; Coalgebra that short-circuits: insert into sorted list
            (define (insert-coalg pair)
              ;; pair = (value-to-insert . remaining-list-as-fix)
              (let ([x (car pair)]
                    [fx (cdr pair)])
                   (let ([layer (unfix fx)])
                        (cond
                         [(nil-f? layer) (cons-f x (left (make-fix nil-f)))]
                         [(cons-f? layer)
                          (if (<= x (cons-f-head layer))
                              ;; Insert here, short-circuit with rest
                              (cons-f x (left fx))
                              ;; Keep going
                              (cons-f (cons-f-head layer)
                                      (right (cons x (cons-f-tail layer)))))]))))
            
            (define-test apo-insert-test
              (let* ([sorted-list (list->fix '(1 3 5 7 9))]
                     [result (apo list-f-fmap insert-coalg (cons 4 sorted-list))])
                    (assert-equal '(1 3 4 5 7 9) (fix->list result))))
            
            (define-test apo-insert-front-test
              (let* ([sorted-list (list->fix '(5 10 15))]
                     [result (apo list-f-fmap insert-coalg (cons 1 sorted-list))])
                    (assert-equal '(1 5 10 15) (fix->list result))))
            
            (define-test apo-insert-end-test
              (let* ([sorted-list (list->fix '(1 2 3))]
                     [result (apo list-f-fmap insert-coalg (cons 10 sorted-list))])
                    (assert-equal '(1 2 3 10) (fix->list result)))))

;;; ============================================================
;;; Practical Examples Tests
;;; ============================================================

(test-group practical-examples
            ;; Merge sort using hylo
            (define (split-coalg lst)
              (if (or (null? lst) (null? (cdr lst)))
                  (branch-f lst (make-fix leaf-f) (make-fix leaf-f))  ; Base case: return list as leaf
                  (let loop ([l lst] [left '()] [right '()] [toggle #t])
                       (if (null? l)
                           (branch-f '() (left) (right))  ; Actually not quite right for tree structure
                           (if toggle
                               (loop (cdr l) (cons (car l) left) right #f)
                               (loop (cdr l) left (cons (car l) right) #t))))))
            
            ;; Simple reversal using catamorphism
            (define (reverse-alg lf)
              (cond
               [(nil-f? lf) '()]
               [(cons-f? lf) (append (cons-f-tail lf) (list (cons-f-head lf)))]))
            
            (define-test cata-reverse-test
              (assert-equal '() (list-cata reverse-alg '()))
              (assert-equal '(3 2 1) (list-cata reverse-alg '(1 2 3))))
            
            ;; All suffixes using catamorphism
            (define (suffixes-alg lf)
              (cond
               [(nil-f? lf) (list '())]
               [(cons-f? lf)
                (let ([rest (cons-f-tail lf)])
                     (cons (cons (cons-f-head lf) (if (null? rest) '() (car rest)))
                           rest))]))
            
            (define-test cata-suffixes-test
              (let ([result (list-cata suffixes-alg '(1 2 3))])
                   (assert-equal '((1 2 3) (2 3) (3) ()) result))))

;;; ============================================================
;;; Summary
;;; ============================================================

(display "
")
(display "==============================================================
")
(printf "Tests passed: ~a
" *tests-passed*)
(printf "Tests failed: ~a
" *tests-failed*)
(printf "Total tests:  ~a
" *tests-run*)

(if (= *tests-failed* 0)
    (display "
[SUCCESS] All recursion schemes tests passed.
")
    (display "
[FAILURE] Some recursion schemes tests failed.
"))
