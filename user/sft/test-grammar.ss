;;; user/sft/test-grammar.ss — Tests for generative grammar DSL
(load "core/testing/test-framework.ss")
(load "user/sft/expand.ss")

(define rng0 (make-pcg 42 0))

;;; Helper
(define (string-contains haystack needle)
  (let ([h-len (string-length haystack)]
        [n-len (string-length needle)])
    (if (> n-len h-len) #f
        (let loop ([i 0])
          (cond
            [(> (+ i n-len) h-len) #f]
            [(string=? (substring haystack i (+ i n-len)) needle) #t]
            [else (loop (+ i 1))])))))

;;; ============================================================
;;; Grammar data types
;;; ============================================================

(test-group "grammar-data-types"

  (define-test "make-grammar creates tagged grammar"
    (let ([g (make-grammar '((root . "hello")))])
      (assert-true (grammar? g))))

  (define-test "grammar-ref finds rule"
    (let ([g (make-grammar '((root . "hello")))])
      (assert-equal "hello" (grammar-ref g 'root))))

  (define-test "grammar-ref errors on missing rule"
    (let ([g (make-grammar '((root . "hello")))])
      (assert-error (lambda () (grammar-ref g 'missing)))))

  (define-test "grammar-has-rule?"
    (let ([g (make-grammar '((root . "hello") (other . "world")))])
      (assert-true (grammar-has-rule? g 'root))
      (assert-false (grammar-has-rule? g 'nope))))

  (define-test "grammar-rule-names"
    (let ([g (make-grammar '((a . "1") (b . "2") (c . "3")))])
      (assert-equal '(a b c) (grammar-rule-names g))))

  (define-test "grammar-merge child shadows parent"
    (let* ([parent (make-grammar '((root . "parent") (shared . "from-parent")))]
           [child  (make-grammar '((root . "child")))]
           [merged (grammar-merge parent child)])
      (assert-equal "child" (grammar-ref merged 'root))
      (assert-equal "from-parent" (grammar-ref merged 'shared))))

  (define-test "grammar-merge preserves all parent rules"
    (let* ([parent (make-grammar '((a . "1") (b . "2") (c . "3")))]
           [child  (make-grammar '((b . "override")))]
           [merged (grammar-merge parent child)])
      (assert-equal "1" (grammar-ref merged 'a))
      (assert-equal "override" (grammar-ref merged 'b))
      (assert-equal "3" (grammar-ref merged 'c)))))

;;; ============================================================
;;; Basic expansion
;;; ============================================================

(test-group "expand-basic"

  (define-test "string literal"
    (let* ([g (make-grammar '((root . "hello world")))]
           [result (expand g 'root '() rng0)])
      (assert-equal "hello world" (car result))))

  (define-test "number literal"
    (let* ([g (make-grammar '((root . 42)))]
           [result (expand g 'root '() rng0)])
      (assert-equal "42" (car result))))

  (define-test "symbol literal"
    (let* ([g (make-grammar '((root . hello)))]
           [result (expand g 'root '() rng0)])
      (assert-equal "hello" (car result)))))

;;; ============================================================
;;; ref combinator
;;; ============================================================

(test-group "expand-ref"

  (define-test "ref expands named rule"
    (let* ([g (make-grammar '((root . (ref greeting))
                              (greeting . "hello")))]
           [result (expand g 'root '() rng0)])
      (assert-equal "hello" (car result))))

  (define-test "ref chains"
    (let* ([g (make-grammar '((root . (ref a))
                              (a . (ref b))
                              (b . "deep")))]
           [result (expand g 'root '() rng0)])
      (assert-equal "deep" (car result)))))

;;; ============================================================
;;; slot combinator
;;; ============================================================

(test-group "expand-slot"

  (define-test "slot from context"
    (let* ([g (make-grammar '((root . (slot name))))]
           [ctx '((name . "Alice"))]
           [result (expand g 'root ctx rng0)])
      (assert-equal "Alice" (car result))))

  (define-test "slot missing key returns empty"
    (let* ([g (make-grammar '((root . (slot missing))))]
           [result (expand g 'root '() rng0)])
      (assert-equal "" (car result))))

  (define-test "slot with numeric value"
    (let* ([g (make-grammar '((root . (slot count))))]
           [ctx '((count . 7))]
           [result (expand g 'root ctx rng0)])
      (assert-equal "7" (car result))))

  (define-test "slot with list value"
    (let* ([g (make-grammar '((root . (slot items))))]
           [ctx '((items . ("a" "b" "c")))]
           [result (expand g 'root ctx rng0)])
      (assert-equal "a b c" (car result)))))

;;; ============================================================
;;; seq combinator
;;; ============================================================

(test-group "expand-seq"

  (define-test "seq concatenates"
    (let* ([g (make-grammar '((root . (seq "hello" " " "world"))))]
           [result (expand g 'root '() rng0)])
      (assert-equal "hello world" (car result))))

  (define-test "seq with refs"
    (let* ([g (make-grammar '((root . (seq (ref a) "-" (ref b)))
                              (a . "foo")
                              (b . "bar")))]
           [result (expand g 'root '() rng0)])
      (assert-equal "foo-bar" (car result))))

  (define-test "seq with slots"
    (let* ([g (make-grammar '((root . (seq "Hi " (slot name) "!"))))]
           [ctx '((name . "Bob"))]
           [result (expand g 'root ctx rng0)])
      (assert-equal "Hi Bob!" (car result))))

  (define-test "empty seq"
    (let* ([g (make-grammar '((root . (seq))))]
           [result (expand g 'root '() rng0)])
      (assert-equal "" (car result)))))

;;; ============================================================
;;; alt combinator
;;; ============================================================

(test-group "expand-alt"

  (define-test "alt picks one option"
    (let* ([g (make-grammar '((root . (alt "a" "b" "c"))))]
           [result (expand g 'root '() rng0)]
           [text (car result)])
      (assert-true (or (string=? text "a")
                       (string=? text "b")
                       (string=? text "c")))))

  (define-test "alt is deterministic"
    (let* ([g (make-grammar '((root . (alt "a" "b" "c"))))]
           [r1 (car (expand g 'root '() (make-pcg 42 0)))]
           [r2 (car (expand g 'root '() (make-pcg 42 0)))])
      (assert-equal r1 r2)))

  (define-test "alt with different seeds gives different results"
    ;; Over many seeds, we should see variation
    (let* ([g (make-grammar '((root . (alt "a" "b" "c"))))]
           [results (map (lambda (seed)
                           (car (expand g 'root '() (make-pcg seed 0))))
                         '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19))]
           [unique (let loop ([items results] [seen '()])
                     (cond
                       [(null? items) seen]
                       [(member (car items) seen) (loop (cdr items) seen)]
                       [else (loop (cdr items) (cons (car items) seen))]))])
      ;; With 20 seeds and 3 options, we should see at least 2 distinct values
      (assert-true (>= (length unique) 2))))

  (define-test "single alt always returns that option"
    (let* ([g (make-grammar '((root . (alt "only"))))]
           [result (expand g 'root '() rng0)])
      (assert-equal "only" (car result))))

  (define-test "empty alt returns empty"
    (let* ([g (make-grammar '((root . (alt))))]
           [result (expand g 'root '() rng0)])
      (assert-equal "" (car result)))))

;;; ============================================================
;;; weighted combinator
;;; ============================================================

(test-group "expand-weighted"

  (define-test "weighted picks from options"
    (let* ([g (make-grammar '((root . (weighted (0.9 "likely") (0.1 "rare")))))]
           [result (expand g 'root '() rng0)]
           [text (car result)])
      (assert-true (or (string=? text "likely") (string=? text "rare")))))

  (define-test "heavily weighted option dominates"
    (let* ([g (make-grammar '((root . (weighted (99.0 "heavy") (1.0 "light")))))]
           [results (map (lambda (seed)
                           (car (expand g 'root '() (make-pcg seed 0))))
                         '(0 1 2 3 4 5 6 7 8 9))]
           [heavy-count (length (filter (lambda (s) (string=? s "heavy")) results))])
      ;; At least 7 of 10 should be "heavy" with 99:1 odds
      (assert-true (>= heavy-count 7)))))

;;; ============================================================
;;; maybe combinator
;;; ============================================================

(test-group "expand-maybe"

  (define-test "maybe with prob 1.0 always includes"
    (let* ([g (make-grammar '((root . (maybe 1.0 "always"))))]
           [results (map (lambda (seed)
                           (car (expand g 'root '() (make-pcg seed 0))))
                         '(0 1 2 3 4))])
      (assert-true (for-all (lambda (s) (string=? s "always")) results))))

  (define-test "maybe with prob 0.0 never includes"
    (let* ([g (make-grammar '((root . (maybe 0.0 "never"))))]
           [results (map (lambda (seed)
                           (car (expand g 'root '() (make-pcg seed 0))))
                         '(0 1 2 3 4))])
      (assert-true (for-all (lambda (s) (string=? s "")) results))))

  (define-test "maybe with 0.5 gives mix"
    (let* ([g (make-grammar '((root . (maybe 0.5 "half"))))]
           [results (map (lambda (seed)
                           (car (expand g 'root '() (make-pcg seed 0))))
                         (let loop ([i 0] [acc '()])
                           (if (>= i 40) acc (loop (+ i 1) (cons i acc)))))]
           [included (length (filter (lambda (s) (string=? s "half")) results))]
           [excluded (length (filter (lambda (s) (string=? s "")) results))])
      ;; Both should be non-zero over 40 samples
      (assert-true (> included 0))
      (assert-true (> excluded 0)))))

;;; ============================================================
;;; when combinator
;;; ============================================================

(test-group "expand-when"

  (define-test "when includes if key is truthy"
    (let* ([g (make-grammar '((root . (when has-verify "verified"))))]
           [ctx '((has-verify . #t))]
           [result (expand g 'root ctx rng0)])
      (assert-equal "verified" (car result))))

  (define-test "when excludes if key is false"
    (let* ([g (make-grammar '((root . (when has-verify "verified"))))]
           [ctx '((has-verify . #f))]
           [result (expand g 'root ctx rng0)])
      (assert-equal "" (car result))))

  (define-test "when excludes if key is missing"
    (let* ([g (make-grammar '((root . (when has-verify "verified"))))]
           [result (expand g 'root '() rng0)])
      (assert-equal "" (car result)))))

;;; ============================================================
;;; case-on combinator
;;; ============================================================

(test-group "expand-case-on"

  (define-test "case-on matches string value"
    (let* ([g (make-grammar '((root . (case-on family
                                        ("spec_to_code" "implement")
                                        ("bugfix" "fix")))))]
           [ctx '((family . "spec_to_code"))]
           [result (expand g 'root ctx rng0)])
      (assert-equal "implement" (car result))))

  (define-test "case-on matches second clause"
    (let* ([g (make-grammar '((root . (case-on family
                                        ("spec_to_code" "implement")
                                        ("bugfix" "fix")))))]
           [ctx '((family . "bugfix"))]
           [result (expand g 'root ctx rng0)])
      (assert-equal "fix" (car result))))

  (define-test "case-on falls through to else"
    (let* ([g (make-grammar '((root . (case-on family
                                        ("spec_to_code" "implement")
                                        (else "default")))))]
           [ctx '((family . "unknown"))]
           [result (expand g 'root ctx rng0)])
      (assert-equal "default" (car result))))

  (define-test "case-on with no match and no else returns empty"
    (let* ([g (make-grammar '((root . (case-on family
                                        ("spec_to_code" "implement")))))]
           [ctx '((family . "unknown"))]
           [result (expand g 'root ctx rng0)])
      (assert-equal "" (car result))))

  (define-test "case-on matches symbol context values"
    (let* ([g (make-grammar '((root . (case-on difficulty
                                        (easy "simple")
                                        (hard "complex")))))]
           [ctx '((difficulty . easy))]
           [result (expand g 'root ctx rng0)])
      (assert-equal "simple" (car result))))

  (define-test "case-on with nested expansion"
    (let* ([g (make-grammar '((root . (case-on family
                                        ("spec_to_code" (seq "Task: " (ref impl-hint)))))
                              (impl-hint . "implement correctly")))]
           [ctx '((family . "spec_to_code"))]
           [result (expand g 'root ctx rng0)])
      (assert-equal "Task: implement correctly" (car result)))))

;;; ============================================================
;;; join combinator
;;; ============================================================

(test-group "expand-join"

  (define-test "join formats list from context"
    (let* ([g (make-grammar '((root . (join ", " funcs))))]
           [ctx '((funcs . ("vec-add" "vec-sub" "vec-dot")))]
           [result (expand g 'root ctx rng0)])
      (assert-equal "vec-add, vec-sub, vec-dot" (car result))))

  (define-test "join with single item"
    (let* ([g (make-grammar '((root . (join ", " funcs))))]
           [ctx '((funcs . ("only")))]
           [result (expand g 'root ctx rng0)])
      (assert-equal "only" (car result))))

  (define-test "join falls back to rule expansion if not context list"
    (let* ([g (make-grammar '((root . (join ", " greeting))
                              (greeting . "hello")))]
           [result (expand g 'root '() rng0)])
      (assert-equal "hello" (car result)))))

;;; ============================================================
;;; Composition & integration
;;; ============================================================

(test-group "expand-composition"

  (define-test "nested combinators"
    (let* ([g (make-grammar
                '((root . (seq (ref greeting) " " (slot name) (maybe 1.0 "!")))
                  (greeting . (alt "Hello" "Hi" "Hey"))))]
           [ctx '((name . "World"))]
           [result (expand g 'root ctx rng0)]
           [text (car result)])
      ;; Should be "<greeting> World!"
      (assert-true (or (string=? text "Hello World!")
                       (string=? text "Hi World!")
                       (string=? text "Hey World!")))))

  (define-test "complex prompt-like grammar"
    (let* ([g (make-grammar
                '((prompt . (seq (ref task-mode) "\n\n"
                                 (slot body) "\n\n"
                                 (when has-verify (seq "Check: " (slot verify-expr)))))
                  (task-mode . (case-on family
                                 ("spec_to_code" (alt "Implement the contract."
                                                      "Build the function."))
                                 ("bugfix" "Fix the bug.")))))]
           [ctx '((family . "spec_to_code")
                  (body . "Write vec-add.")
                  (has-verify . #t)
                  (verify-expr . "(assert-equal 3 (vec-add 1 2))"))]
           [result (expand g 'prompt ctx rng0)]
           [text (car result)])
      ;; Should contain the task mode, body, and verify
      (assert-true (string-contains text "Write vec-add."))
      (assert-true (string-contains text "Check: (assert-equal 3 (vec-add 1 2))"))))

  (define-test "grammar-merge with expansion"
    (let* ([base (make-grammar
                   '((root . (seq (ref greeting) " " (ref target)))
                     (greeting . "Hello")
                     (target . "World")))]
           [override (make-grammar
                       '((greeting . "Bonjour")))]
           [merged (grammar-merge base override)]
           [result (expand merged 'root '() rng0)])
      (assert-equal "Bonjour World" (car result)))))

;;; ============================================================
;;; Determinism
;;; ============================================================

(test-group "determinism"

  (define-test "same seed same output across multiple expansions"
    (let ([g (make-grammar
               '((root . (seq (alt "A" "B" "C") "-"
                              (alt "1" "2" "3") "-"
                              (maybe 0.5 "X")))))])
      (let ([r1 (car (expand g 'root '() (make-pcg 12345 0)))]
            [r2 (car (expand g 'root '() (make-pcg 12345 0)))])
        (assert-equal r1 r2))))

  (define-test "expand-n produces n results"
    (let* ([g (make-grammar '((root . (alt "a" "b" "c"))))]
           [results (expand-n g 'root '() 0 5)])
      (assert-equal 5 (length results))
      (assert-true (for-all string? results))))

  (define-test "expand-to-string returns just the string"
    (let* ([g (make-grammar '((root . "hello")))]
           [text (expand-to-string g 'root '() 42)])
      (assert-equal "hello" text))))

;;; ============================================================
;;; Edge cases
;;; ============================================================

(test-group "edge-cases"

  (define-test "deeply nested refs"
    (let* ([g (make-grammar '((a . (ref b)) (b . (ref c)) (c . (ref d))
                              (d . (ref e)) (e . "bottom")))]
           [result (expand g 'a '() rng0)])
      (assert-equal "bottom" (car result))))

  (define-test "seq with all empty strings"
    (let* ([g (make-grammar '((root . (seq "" "" ""))))]
           [result (expand g 'root '() rng0)])
      (assert-equal "" (car result))))

  (define-test "when with complex body"
    (let* ([g (make-grammar '((root . (when active (seq "[" (slot name) "]")))))]
           [ctx '((active . #t) (name . "test"))]
           [result (expand g 'root ctx rng0)])
      (assert-equal "[test]" (car result)))))

;;; ============================================================
;;; Analysis
;;; ============================================================

(load "user/sft/analyze.ss")

(test-group "analysis"

  (define-test "reachable finds all connected rules"
    (let* ([g (make-grammar '((root . (seq (ref a) (ref b)))
                              (a . "hello")
                              (b . (ref c))
                              (c . "world")
                              (orphan . "unused")))]
           [reachable (grammar-reachable g 'root)])
      (assert-true (pair? (member 'root reachable)))
      (assert-true (pair? (member 'a reachable)))
      (assert-true (pair? (member 'b reachable)))
      (assert-true (pair? (member 'c reachable)))
      (assert-false (member 'orphan reachable))))

  (define-test "dead-rules finds orphans"
    (let* ([g (make-grammar '((root . (ref a))
                              (a . "used")
                              (dead1 . "unused")
                              (dead2 . "also unused")))]
           [dead (grammar-dead-rules g 'root)])
      (assert-equal 2 (length dead))
      (assert-true (pair? (member 'dead1 dead)))
      (assert-true (pair? (member 'dead2 dead)))))

  (define-test "no dead rules when all connected"
    (let* ([g (make-grammar '((root . (seq (ref a) (ref b)))
                              (a . "x")
                              (b . "y")))]
           [dead (grammar-dead-rules g 'root)])
      (assert-equal 0 (length dead))))

  (define-test "coverage counts rule hits"
    (let* ([g (make-grammar '((root . (seq (ref greeting) " " (ref target)))
                              (greeting . (alt "Hi" "Hey"))
                              (target . "World")))]
           [coverage (grammar-coverage g 'root '() 0 20)])
      ;; All three rules should be hit
      (assert-true (> (length coverage) 0))
      (let ([root-hits (cdr (assq 'root coverage))]
            [greeting-hits (cdr (assq 'greeting coverage))]
            [target-hits (cdr (assq 'target coverage))])
        (assert-equal 20 root-hits)
        (assert-equal 20 greeting-hits)
        (assert-equal 20 target-hits))))

  (define-test "expansion-trace returns visited rules"
    (let* ([g (make-grammar '((root . (seq (ref a) (ref b)))
                              (a . "x")
                              (b . (ref c))
                              (c . "y")))]
           [trace (expansion-trace g 'root '() rng0)])
      (assert-equal '(root a b c) trace))))

;;; ============================================================
;;; Base grammar
;;; ============================================================

(load "user/sft/base-grammar.ss")

(test-group "base-grammar"

  (define-test "spec_to_code prompt expands"
    (let* ([ctx (make-sft-context "spec_to_code" "implementation"
                  "vec-add" "Implement vec-add for 2D vectors."
                  "(assert-equal '(3 5) (vec-add '(1 2) '(2 3)))")]
           [text (expand-to-string *sft-base-grammar* 'prompt ctx 42)])
      (assert-true (string-contains text "Implement vec-add"))
      (assert-true (string-contains text "Task mode:"))))

  (define-test "bugfix prompt expands"
    (let* ([ctx (make-sft-context "bugfix" "repair"
                  "heap-pop" "Fix the off-by-one in heap-pop."
                  "(assert-true (heap-empty? (heap-pop (make-heap '(1)))))")]
           [text (expand-to-string *sft-base-grammar* 'prompt ctx 99)])
      (assert-true (string-contains text "heap-pop"))))

  (define-test "composition prompt with available functions"
    (let* ([ctx (make-sft-context "composition" "usage"
                  "vec-normalize" "Normalize a vector."
                  "(assert-true (< (abs (- (vec-length (vec-normalize v)) 1.0)) 0.001))"
                  '("vec-length" "vec-scale" "vec-dot"))]
           [text (expand-to-string *sft-base-grammar* 'prompt ctx 7)])
      (assert-true (string-contains text "Normalize a vector"))))

  (define-test "deterministic across runs"
    (let* ([ctx (make-sft-context "spec_to_code" "implementation"
                  "foo" "Implement foo.")]
           [t1 (expand-to-string *sft-base-grammar* 'prompt ctx 42)]
           [t2 (expand-to-string *sft-base-grammar* 'prompt ctx 42)])
      (assert-equal t1 t2)))

  (define-test "different seeds give different prompts"
    (let* ([ctx (make-sft-context "spec_to_code" "implementation"
                  "foo" "Implement foo.")]
           [results (expand-n *sft-base-grammar* 'prompt ctx 0 20)]
           [unique (let loop ([items results] [seen '()])
                     (cond
                       [(null? items) seen]
                       [(member (car items) seen) (loop (cdr items) seen)]
                       [else (loop (cdr items) (cons (car items) seen))]))])
      ;; Should see variation across 20 seeds
      (assert-true (>= (length unique) 3))))

  (define-test "no dead rules in base grammar"
    (let ([dead (grammar-dead-rules *sft-base-grammar* 'prompt)])
      ;; bug-report-line and composition-overlay are reachable through
      ;; family-overlay → bugfix-overlay → bug-report-line
      ;; Some rules may be dead if context doesn't trigger them,
      ;; but structurally they should all be reachable
      (assert-equal 0 (length dead)))))

(run-all-tests)
