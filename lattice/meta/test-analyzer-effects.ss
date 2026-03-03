(load "core/testing/test-framework.ss")
(when (not (top-level-bound? 'meta-printf))
  (eval '(define meta-printf printf)))
(load "lattice/meta/lattice-walk.ss")
(load "lattice/meta/analyzer-effects.ss")

;;; Helper
(define (mr name deps sexps)
  (make-module-record name (string-append (symbol->string name) ".ss")
                      deps sexps ""))

;;; ============================================================================
;;; Effect Lattice Tests
;;; ============================================================================

(test-group "effect-lattice"

  (define-test "pure is identity"
    (assert-equal 'pure (effect-join 'pure 'pure))
    (assert-equal 'io (effect-join 'pure 'io))
    (assert-equal 'mutation (effect-join 'mutation 'pure)))

  (define-test "same effects join to themselves"
    (assert-equal 'io (effect-join 'io 'io))
    (assert-equal 'mutation (effect-join 'mutation 'mutation)))

  (define-test "unknown is optimistic"
    (assert-equal 'io (effect-join 'unknown 'io))
    (assert-equal 'mutation (effect-join 'mutation 'unknown)))

  (define-test "effects-join folds correctly"
    (assert-equal 'pure (effects-join '()))
    (assert-equal 'pure (effects-join '(pure pure pure)))
    (assert-equal 'io (effects-join '(pure io pure)))))

;;; ============================================================================
;;; Definition Extraction Tests
;;; ============================================================================

(test-group "extract-definitions"

  (define-test "extracts simple defines"
    (let ([defs (extract-definitions '((define (foo x) (+ x 1))
                                       (define bar 42)))])
      (assert-equal 2 (length defs))
      (assert-equal 'foo (caar defs))
      (assert-equal 'bar (caadr defs))))

  (define-test "skips non-define forms"
    (let ([defs (extract-definitions '((doc 'module 'test)
                                       (require 'prelude)
                                       (define (foo) 1)))])
      (assert-equal 1 (length defs))
      (assert-equal 'foo (caar defs))))

  (define-test "handles multi-body define"
    (let* ([defs (extract-definitions '((define (foo x) (display x) (+ x 1))))]
           [body (cdar defs)])
      ;; Should wrap in begin
      (assert-true (and (pair? body) (eq? (car body) 'begin))))))

;;; ============================================================================
;;; Effect Inference Tests
;;; ============================================================================

(test-group "infer-effect"

  (define db (make-eq-hashtable))

  (define-test "literals are pure"
    (assert-equal 'pure (infer-effect 42 db))
    (assert-equal 'pure (infer-effect "hello" db))
    (assert-equal 'pure (infer-effect #t db)))

  (define-test "symbol references are pure"
    (assert-equal 'pure (infer-effect 'x db))
    (assert-equal 'pure (infer-effect 'foo db)))

  (define-test "quote is pure"
    (assert-equal 'pure (infer-effect '(quote (a b c)) db)))

  (define-test "lambda is pure"
    (assert-equal 'pure (infer-effect '(lambda (x) (display x)) db)))

  (define-test "known pure application"
    (seed-primitives! db)
    (assert-equal 'pure (infer-effect '(+ 1 2) db))
    (assert-equal 'pure (infer-effect '(car (cons 1 2)) db)))

  (define-test "known io application"
    (assert-equal 'io (infer-effect '(display "hello") db)))

  (define-test "set! is mutation"
    (assert-equal 'mutation (infer-effect '(set! x 42) db)))

  (define-test "if propagates worst effect"
    (assert-equal 'io (infer-effect '(if #t (display "hi") 1) db)))

  (define-test "begin propagates worst effect"
    (assert-equal 'io (infer-effect '(begin 1 2 (display 3)) db)))

  (define-test "let infers binding + body effects"
    (assert-equal 'io
      (infer-effect '(let ([x (display "hi")]) x) db)))

  (define-test "unknown function gives unknown"
    (assert-equal 'unknown (infer-effect '(totally-unknown-fn 1 2) db)))

  (define-test "cond propagates worst effect"
    (assert-equal 'io
      (infer-effect '(cond [(= x 1) (display "one")] [else 0]) db)))

  (define-test "when/unless propagates effects"
    (assert-equal 'io
      (infer-effect '(when #t (display "hi")) db))))

;;; ============================================================================
;;; Cross-Module Effect Propagation (THE KEY TEST)
;;; ============================================================================

(test-group "cross-module-effects"

  (define-test "effects propagate from dependency to dependent"
    ;; Module A defines write-file (uses display → io)
    ;; Module B defines process (calls write-file)
    ;; Without topsort: process → unknown (write-file not in DB)
    ;; With topsort: process → io (write-file already analyzed as io)
    (let* ([a-sexps '((define (write-file x) (display x))
                       (define (pure-helper y) (+ y 1)))]
           [b-sexps '((define (process x) (write-file x))
                       (define (compute y) (pure-helper y)))]
           [a (mr 'a '() a-sexps)]
           [b (mr 'b '(a) b-sexps)]
           [analyzer (make-effect-analyzer)]
           [result (lattice-walk (list b a) (list analyzer))]
           [db (acc-ref (walk-state result) 'effects)])
      (assert-true (walk-ok? result))
      ;; write-file should be io (calls display)
      (assert-equal 'io (effect-db-lookup db 'write-file))
      ;; pure-helper should be pure (calls +)
      (assert-equal 'pure (effect-db-lookup db 'pure-helper))
      ;; CRITICAL: process should be io (not unknown!)
      ;; This is the whole point of topsort-based analysis.
      (assert-equal 'io (effect-db-lookup db 'process))
      ;; compute should be pure (calls pure-helper)
      (assert-equal 'pure (effect-db-lookup db 'compute))))

  (define-test "three-module chain propagates"
    ;; A: defines io-fn
    ;; B: defines wrapper (calls io-fn)
    ;; C: defines caller (calls wrapper)
    ;; All should resolve to io.
    (let* ([a (mr 'a '() '((define (io-fn) (display "x"))))]
           [b (mr 'b '(a) '((define (wrapper) (io-fn))))]
           [c (mr 'c '(b) '((define (caller) (wrapper))))]
           [analyzer (make-effect-analyzer)]
           [result (lattice-walk (list c b a) (list analyzer))]
           [db (acc-ref (walk-state result) 'effects)])
      (assert-true (walk-ok? result))
      (assert-equal 'io (effect-db-lookup db 'io-fn))
      (assert-equal 'io (effect-db-lookup db 'wrapper))
      (assert-equal 'io (effect-db-lookup db 'caller))))

  (define-test "local call chains resolve via fixed-point"
    ;; Within module A:
    ;;   bar calls baz (pure)
    ;;   foo calls bar
    ;; After fixed-point, foo should be pure (not unknown)
    (let* ([a (mr 'a '() '((define (baz x) (+ x 1))
                             (define (bar x) (baz x))
                             (define (foo x) (bar x))))]
           [analyzer (make-effect-analyzer)]
           [result (lattice-walk (list a) (list analyzer))]
           [db (acc-ref (walk-state result) 'effects)])
      (assert-true (walk-ok? result))
      (assert-equal 'pure (effect-db-lookup db 'baz))
      (assert-equal 'pure (effect-db-lookup db 'bar))
      (assert-equal 'pure (effect-db-lookup db 'foo))))

  (define-test "mixed pure and impure in same module"
    (let* ([a (mr 'a '() '((define (pure-fn x) (+ x 1))
                             (define (impure-fn x) (display x))
                             (define (calls-impure x) (impure-fn x))
                             (define (calls-pure x) (pure-fn x))))]
           [analyzer (make-effect-analyzer)]
           [result (lattice-walk (list a) (list analyzer))]
           [db (acc-ref (walk-state result) 'effects)])
      (assert-equal 'pure (effect-db-lookup db 'pure-fn))
      (assert-equal 'io (effect-db-lookup db 'impure-fn))
      (assert-equal 'io (effect-db-lookup db 'calls-impure))
      (assert-equal 'pure (effect-db-lookup db 'calls-pure)))))

;;; ============================================================================
;;; Fixed-Point Convergence Tests
;;; ============================================================================

(test-group "fixed-point"

  (define-test "mutual recursion in same module converges"
    ;; A and B call each other, both pure
    (let* ([a (mr 'mod '() '((define (fn-a x) (if (= x 0) 1 (fn-b (- x 1))))
                               (define (fn-b x) (if (= x 0) 2 (fn-a (- x 1))))))]
           [analyzer (make-effect-analyzer)]
           [result (lattice-walk (list a) (list analyzer))]
           [db (acc-ref (walk-state result) 'effects)])
      (assert-equal 'pure (effect-db-lookup db 'fn-a))
      (assert-equal 'pure (effect-db-lookup db 'fn-b))))

  (define-test "impure mutual recursion detected"
    ;; fn-a calls fn-b which calls display
    (let* ([a (mr 'mod '() '((define (fn-a x) (fn-b x))
                               (define (fn-b x) (begin (display x) (fn-a x)))))]
           [analyzer (make-effect-analyzer)]
           [result (lattice-walk (list a) (list analyzer))]
           [db (acc-ref (walk-state result) 'effects)])
      (assert-equal 'io (effect-db-lookup db 'fn-a))
      (assert-equal 'io (effect-db-lookup db 'fn-b)))))

(run-all-tests)
