;;; core/util/test-profile.ss — Tests for Inline Performance Profiler
;;;
;;; Tests profiler functionality:
;;;   - Call tree construction
;;;   - Fuel tracking
;;;   - Statistics extraction
;;;   - Hotspot detection

(load "core/util/profile.ss")

(display "Profiler Tests\n")
(display "====\n\n")

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

;;; ====
;;; Node Tests
;;; ====

(display "Node Creation:\n")

(let ([n (make-node 'test 100)])
     (test "node?" #t (node? n))
     (test "node-name" 'test (node-name n))
     (test "node-start-fuel" 100 (node-start-fuel n))
     (test "node-end-fuel #f" #f (node-end-fuel n))
     (test "node-call-count" 1 (node-call-count n)))

(display "\nNode Operations:\n")

(let* ([n (make-node 'test 100)]
       [closed (node-close n 50)])
      (test "node-close sets end-fuel" 50 (node-end-fuel closed))
      (test "node-fuel-consumed" 50 (node-fuel-consumed closed)))

(let* ([n (make-node 'test 100)]
       [inc (node-increment-calls n)])
      (test "node-increment-calls" 2 (node-call-count inc)))

(let* ([parent (make-node 'parent 100)]
       [child (make-node 'child 80)]
       [updated (node-add-child parent child)])
      (test "node-add-child" 1 (length (node-children updated))))

;;; ====
;;; Profiler State Tests
;;; ====

(display "\nProfiler State:\n")

(let ([p (make-profiler '(+ 1 2) empty-env 1000)])
     (test "profiler?" #t (profiler? p))
     (test "profiler-expr" '(+ 1 2) (profiler-expr p))
     (test "profiler-initial-fuel" 1000 (profiler-initial-fuel p))
     (test "profiler-status" 'ready (profiler-status p)))

(let* ([p (make-profiler '(+ 1 2) empty-env 1000)]
       [updated (profiler-set p 'status 'complete)])
      (test "profiler-set" 'complete (profiler-status updated)))

;;; ====
;;; Expression Classification Tests
;;; ====

(display "\nExpression Classification:\n")

(test "expr-name symbol" 'foo (expr-name 'foo))
(test "expr-name number" 'literal (expr-name 42))
(test "expr-name fn" 'lambda (expr-name '(fn (x) x)))
(test "expr-name fix" 'fib (expr-name '(fix fib (fn (n) n))))
(test "expr-name prim" 'add (expr-name '(prim 'add 1 2)))
(test "expr-name let" 'let (expr-name '(let ((x 1)) x)))
(test "expr-name if" 'if (expr-name '(if #t 1 2)))
(test "expr-name call" 'map (expr-name '(map f xs)))

;;; ====
;;; Profile Execution Tests
;;; ====

(display "\nProfile Execution:\n")

(let ([p (profile-expr 42 100)])
     (test "profile value status" 'complete (profiler-status p))
     (test "profile value result" 42 (profiler-expr p)))

(let ([p (profile-expr '(quote hello) 100)])
     (test "profile quote status" 'complete (profiler-status p)))

(let ([p (profile-expr '(fn (x) x) 100)])
     (test "profile lambda status" 'complete (profiler-status p)))

(let ([p (profile-expr '(prim 'add 1 2) 100)])
     (test "profile prim status" 'complete (profiler-status p))
     (test "profile prim result" 3 (profiler-expr p)))

(let ([p (profile-expr '(if #t 1 2) 100)])
     (test "profile if result" 1 (profiler-expr p)))

(let ([p (profile-expr '(let ((x 5)) x) 100)])
     (test "profile let result" 5 (profiler-expr p)))

;;; ====
;;; Statistics Tests
;;; ====

(display "\nStatistics:\n")

(let* ([p (profile-expr 42 100)]
       [stats (profile-stats p)])
      (test "stats total-fuel" 100 (cdr (assq 'total-fuel stats)))
      (test "stats has used-fuel" #t (pair? (assq 'used-fuel stats)))
      (test "stats has by-function" #t (pair? (assq 'by-function stats))))

(let* ([p (profile-expr '(prim 'add 1 2) 100)]
       [stats (profile-stats p)]
       [used (cdr (assq 'used-fuel stats))])
      (test "stats used-fuel > 0" #t (> used 0)))

;;; ====
;;; Hotspot Tests
;;; ====

(display "\nHotspots:\n")

(let* ([p (profile-expr '(let ((x 1)) x) 100)]
       [hotspots (find-hotspots p 5)])
      (test "find-hotspots returns list" #t (list? hotspots)))

;;; ====
;;; Call Tree Tests
;;; ====

(display "\nCall Tree:\n")

(let* ([n (node-close (make-node 'test 100) 50)]
       [flat (flatten-tree n)])
      (test "flatten-tree returns pairs" #t (pair? flat)))

(let* ([calls '((foo . 10) (bar . 20) (foo . 15))]
       [agg (aggregate-by-name calls)])
      (test "aggregate has foo" #t (pair? (assq 'foo agg)))
      (test "aggregate has bar" #t (pair? (assq 'bar agg))))

(let* ([n (node-close (make-node 'test 100) 50)]
       [rendered (render-tree n 0)])
      (test "render-tree produces string" #t (string? rendered)))

;;; ====
;;; Path Analysis Tests
;;; ====

(display "\nPath Analysis:\n")

(let ([path '((a . 10) (b . 20) (c . 30))])
     (test "path-fuel sums correctly" 60 (path-fuel path)))

(let* ([path '((a . 10) (b . 20))]
       [rendered (render-path path)])
      (test "render-path produces string" #t (string? rendered)))

;;; ====
;;; Summary
;;; ====

(newline)
(display "====\n")
(display (format "Tests: ~a passed, ~a failed\n" tests-passed tests-failed))
(when (> tests-failed 0)
      (exit 1))
