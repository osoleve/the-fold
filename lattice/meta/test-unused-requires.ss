(load "core/testing/test-framework.ss")
(load "lattice/meta/unused-requires.ss")

;;; ====
;;; Test: extract-requires-from-sexps
;;; ====

(test-group "extract-requires"

  (define-test "empty file yields no requires"
    (assert-equal '() (extract-requires-from-sexps '())))

  (define-test "single require"
    (assert-equal '(prelude)
      (extract-requires-from-sexps
        '((require 'prelude)))))

  (define-test "multiple requires"
    (assert-equal '(prelude vec matrix)
      (extract-requires-from-sexps
        '((require 'prelude)
          (require 'vec)
          (require 'matrix)))))

  (define-test "guarded require in unless"
    (assert-equal '(prelude)
      (extract-requires-from-sexps
        '((unless (top-level-bound? 'require)
            (load "core/lang/module.ss"))
          (require 'prelude)))))

  (define-test "require in when block"
    (assert-equal '(foo)
      (extract-requires-from-sexps
        '((when some-condition
            (require 'foo))))))

  (define-test "require in begin block"
    (assert-equal '(foo bar)
      (extract-requires-from-sexps
        '((begin
            (require 'foo)
            (require 'bar))))))

  (define-test "ignores non-require forms"
    (assert-equal '(prelude)
      (extract-requires-from-sexps
        '((define x 10)
          (require 'prelude)
          (define (f y) (+ y 1))))))

  (define-test "ignores require inside define body"
    ;; Only top-level requires count
    (assert-equal '()
      (extract-requires-from-sexps
        '((define (f)
            (require 'bad))))))
)

;;; ====
;;; Test: collect-file-symbols
;;; ====

(test-group "collect-file-symbols"

  (define-test "collects symbols from define body"
    (let ([syms (collect-file-symbols
                  '((define (f x) (+ x 1))))])
      (assert-true (pair? (memq '+ syms)))
      (assert-true (pair? (memq 'x syms)))))

  (define-test "skips quoted symbols"
    (let ([syms (collect-file-symbols
                  '((define x 'hello)))])
      (assert-true (not (memq 'hello syms)))))

  (define-test "skips let binding names"
    (let ([syms (collect-file-symbols
                  '((let ([x 10]) (+ x 1))))])
      ;; x appears as a reference in the body, but the binding name itself
      ;; is skipped by the extractor — only the init value matters
      (assert-true (pair? (memq '+ syms)))))

  (define-test "skips lambda parameter names"
    (let ([syms (collect-file-symbols
                  '((lambda (x y) (+ x y))))])
      (assert-true (pair? (memq '+ syms)))))

  (define-test "skips doc forms"
    (let ([syms (collect-file-symbols
                  '((doc 'module 'foo)
                    (doc 'description "blah")
                    (define (f x) (vec-ref x 0))))])
      (assert-true (not (memq 'module syms)))
      (assert-true (not (memq 'description syms)))
      (assert-true (pair? (memq 'vec-ref syms)))))

  (define-test "skips require forms"
    (let ([syms (collect-file-symbols
                  '((require 'prelude)
                    (define (f x) (+ x 1))))])
      (assert-true (not (memq 'prelude syms)))
      (assert-true (pair? (memq '+ syms)))))

  (define-test "handles let with empty bindings"
    (let ([syms (collect-file-symbols
                  '((let () (vec-add a b))))])
      (assert-true (pair? (memq 'vec-add syms)))))

  (define-test "handles named let with empty bindings"
    (let ([syms (collect-file-symbols
                  '((let loop () (vec-add a b))))])
      (assert-true (pair? (memq 'vec-add syms)))))

  (define-test "collects symbols inside quasiquote unquotes"
    (let ([syms (collect-file-symbols
                  '((define (f x) `(result ,(vec-ref x 0)))))])
      (assert-true (pair? (memq 'vec-ref syms)))))

  (define-test "collects symbols inside syntax-rules templates"
    (let ([syms (collect-file-symbols
                  '((define-syntax my-mac
                      (syntax-rules ()
                        [(_ x) (vec-add x x)]))))])
      (assert-true (pair? (memq 'vec-add syms)))))

  (define-test "deduplicates"
    (let ([syms (collect-file-symbols
                  '((define (f x) (+ x (+ x 1)))))])
      ;; + appears twice in source but should be deduplicated
      (assert-equal 1 (length (filter (lambda (s) (eq? s '+)) syms)))))
)

;;; ====
;;; Test: analyze-file-requires
;;; ====

(test-group "analyze-file-requires"

  ;; Mock export lookup
  (define (mock-lookup mod)
    (case mod
      [(vec)    '(vec-ref vec-set vec-length vec-map)]
      [(matrix) '(matrix-ref matrix-set matrix-rows matrix-cols)]
      [(sparse) '(sparse-ref sparse-set sparse-nnz)]
      [(unknown-mod) '()]  ;; unknown exports
      [else '()]))

  (define-test "empty file"
    (let ([result (analyze-file-requires '() mock-lookup)])
      (assert-equal '() (analysis-unused result))
      (assert-equal '() (analysis-used result))
      (assert-equal '() (analysis-uncertain result))))

  (define-test "all requires used"
    (let* ([sexps '((require 'vec)
                    (require 'matrix)
                    (define (f m)
                      (let ([r (matrix-ref m 0 0)]
                            [v (vec-ref m 1)])
                        (+ r v))))]
           [result (analyze-file-requires sexps mock-lookup)])
      (assert-equal '() (analysis-unused result))
      (assert-true (pair? (memq 'vec (analysis-used result))))
      (assert-true (pair? (memq 'matrix (analysis-used result))))))

  (define-test "unused require detected"
    (let* ([sexps '((require 'vec)
                    (require 'matrix)
                    (require 'sparse)
                    (define (f v)
                      (vec-ref v 0)))]
           [result (analyze-file-requires sexps mock-lookup)])
      ;; vec is used (vec-ref), matrix and sparse are not
      (assert-true (pair? (memq 'vec (analysis-used result))))
      (assert-true (pair? (memq 'matrix (analysis-unused result))))
      (assert-true (pair? (memq 'sparse (analysis-unused result))))))

  (define-test "uncertain when exports unknown"
    (let* ([sexps '((require 'unknown-mod)
                    (define (f x) (+ x 1)))]
           [result (analyze-file-requires sexps mock-lookup)])
      (assert-true (pair? (memq 'unknown-mod (analysis-uncertain result))))
      (assert-equal '() (analysis-unused result))))

  (define-test "mixed: used, unused, and uncertain"
    (let* ([sexps '((require 'vec)
                    (require 'matrix)
                    (require 'unknown-mod)
                    (define (f v) (vec-ref v 0)))]
           [result (analyze-file-requires sexps mock-lookup)])
      (assert-equal '(vec) (analysis-used result))
      (assert-equal '(matrix) (analysis-unused result))
      (assert-equal '(unknown-mod) (analysis-uncertain result))))

  (define-test "prelude is always used (common symbols)"
    ;; If prelude exports filter-map, append-map, etc., and file uses them
    (let* ([prelude-lookup (lambda (mod)
                             (case mod
                               [(prelude) '(filter-map append-map for-each map)]
                               [else '()]))]
           [sexps '((require 'prelude)
                    (define (f xs) (filter-map car xs)))]
           [result (analyze-file-requires sexps prelude-lookup)])
      (assert-true (pair? (memq 'prelude (analysis-used result))))))
)

;;; ====
;;; Test: analyze-file-requires-detail
;;; ====

(test-group "analyze-file-requires-detail"

  (define (mock-lookup mod)
    (case mod
      [(vec) '(vec-ref vec-set vec-length vec-map)]
      [(matrix) '(matrix-ref matrix-set matrix-rows)]
      [else '()]))

  (define-test "detail shows which exports are used"
    (let* ([sexps '((require 'vec)
                    (define (f v)
                      (let ([a (vec-ref v 0)]
                            [b (vec-length v)])
                        (+ a b))))]
           [detail (analyze-file-requires-detail sexps mock-lookup)]
           [vec-info (cdr (assq 'vec detail))])
      (assert-equal 'used (cdr (assq 'status vec-info)))
      (assert-equal 4 (cdr (assq 'exports-total vec-info)))
      ;; vec-ref and vec-length should be in exports-used
      (let ([used (cdr (assq 'exports-used vec-info))])
        (assert-true (pair? (memq 'vec-ref used)))
        (assert-true (pair? (memq 'vec-length used)))
        (assert-true (not (memq 'vec-set used)))
        (assert-true (not (memq 'vec-map used))))))

  (define-test "detail for unused module shows zero used"
    (let* ([sexps '((require 'matrix)
                    (define (f x) (+ x 1)))]
           [detail (analyze-file-requires-detail sexps mock-lookup)]
           [matrix-info (cdr (assq 'matrix detail))])
      (assert-equal 'unused (cdr (assq 'status matrix-info)))
      (assert-equal '() (cdr (assq 'exports-used matrix-info)))
      (assert-equal 3 (cdr (assq 'exports-total matrix-info)))))
)

;;; ====
;;; Run
;;; ====

(run-all-tests)
