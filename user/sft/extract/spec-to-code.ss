;;; user/sft/extract/spec-to-code.ss — Spec-to-code SFT sample extraction
;;;
;;; For each function in the walker IR, generates 1-3 spec-to-code samples:
;;;   1. Type+desc spec: Given type signature and description, implement
;;;   2. Test-as-spec:   Given test assertions as behavioral spec, implement
;;;   3. Skeleton:       Given function signature with <TODO> body, complete
;;;
;;; Requires walker.ss and emit.ss to be loaded first.
;;;
;;; Usage:
;;;   (load "user/sft/extract/walker.ss")
;;;   (load "user/sft/extract/emit.ss")
;;;   (load "user/sft/extract/spec-to-code.ss")
;;;   (define ir (walk-all-manifests))
;;;   (define samples (generate-spec-to-code ir))
;;;   (emit-jsonl-split samples "data/spec-to-code/")

;;; ====
;;; Dependencies — walker.ss and emit.ss must be loaded first
;;; ====

(unless (top-level-bound? 'walk-all-manifests)
  (error 'spec-to-code "walker.ss must be loaded first"))
(unless (top-level-bound? 'make-sample)
  (error 'spec-to-code "emit.ss must be loaded first"))

;;; ====
;;; Helpers
;;; ====

(define (s2c/skill-description skill-name)
  ;; Human-readable skill category description for prompt context.
  ;; Maps skill names to short domain descriptions.
  (let ([table '((linalg . "linear algebra")
                 (data . "data structures")
                 (algebra . "abstract algebra")
                 (random . "pseudo-random number generation and distributions")
                 (numeric . "numerical methods")
                 (geometry . "computational geometry")
                 (diffgeo . "differential geometry")
                 (autodiff . "automatic differentiation")
                 (fp . "functional programming")
                 (optics . "optics (lenses, prisms, traversals)")
                 (clp . "constraint logic programming")
                 (query . "query DSL and pattern matching")
                 (dsl . "domain-specific languages")
                 (info . "information theory")
                 (number-theory . "number theory")
                 (topology . "algebraic topology")
                 (crypto . "cryptographic primitives")
                 (optimization . "mathematical optimization")
                 (statistics . "statistical methods")
                 (game-theory . "game theory and mechanism design")
                 (control-systems . "control theory")
                 (sim . "simulation and dynamics")
                 (tiles . "board game SDK")
                 (egraph . "equality saturation")
                 (sat . "SAT and MaxSAT solving")
                 (meta . "lattice navigation and search")
                 (automata . "finite automata and state machines")
                 (pipeline . "agent workflows")
                 (validation . "input validation")
                 (physics/diff . "differentiable 2D physics")
                 (physics/diff3d . "differentiable 3D physics")
                 (physics/classical . "classical 2D physics")
                 (physics/classical3d . "classical 3D physics")
                 (physics/lenses . "physics optic integration")
                 (dataset . "dataset construction")
                 (markdown . "markdown processing")
                 (parallel . "parallel computation")
                 (template . "template DSL")
                 (info-stat . "information statistics"))])
    (let ([entry (assq skill-name table)])
      (if entry (cdr entry)
          (symbol->string skill-name)))))

(define (s2c/format-type-sig type-sig)
  ;; Format a type signature S-expression as a readable string.
  (if (not type-sig) ""
      (emit/sexp->string type-sig)))

(define (s2c/format-tests-for-prompt tests max-tests)
  ;; Format test forms as a code block for inclusion in prompts.
  ;; Returns a string with up to max-tests test examples.
  (if (or (not tests) (null? tests)) ""
      (let* ([selected (if (> (length tests) max-tests)
                           (list-head tests max-tests)
                           tests)]
             [formatted (map (lambda (t) (emit/sexp->pretty-string t))
                             selected)])
        (apply string-append
               (map (lambda (s) (string-append s "\n")) formatted)))))

(define (s2c/build-verify-expr ir)
  ;; Build a verify expression from the function's tests.
  ;; Returns ONLY assertion checks — NO target function definition.
  ;; The ground truth (function define) is injected separately by
  ;; verify-expr-from-parts in verify.ss, preventing oracle leakage.
  (let ([tests (cdr (assq 'tests ir))])
    (if (or (not tests) (null? tests))
        #f  ; No tests available for verification
        ;; Extract ALL assertion bodies from define-test forms
        (let ([assertions (append-map (lambda (t)
                                        (s2c/extract-assertions t))
                                      tests)])
          (if (null? assertions)
              #f
              ;; (and check1 check2 ...) short-circuits on first #f.
              `(and ,@assertions))))))

(define (s2c/assertion->check assertion)
  ;; Convert test framework assertion to self-contained check expression.
  ;; (assert-equal expected actual) → (equal? actual expected)
  ;; (assert-true expr)             → (eq? #t expr)
  ;; (assert-false expr)            → (eq? #f expr)
  ;; (assert-ok expr)               → (pair? expr)   ; ok values are (ok ...)
  ;; (assert-error expr)            → We skip these (can't easily self-contain)
  (and (pair? assertion)
       (case (car assertion)
         [(assert-equal)
          (and (pair? (cdr assertion)) (pair? (cddr assertion))
               `(equal? ,(caddr assertion) ,(cadr assertion)))]
         [(assert-true)
          (and (pair? (cdr assertion))
               `(eq? #t ,(cadr assertion)))]
         [(assert-false)
          (and (pair? (cdr assertion))
               `(eq? #f ,(cadr assertion)))]
         [(assert-ok)
          (and (pair? (cdr assertion))
               `(and (pair? ,(cadr assertion))
                     (eq? (car ,(cadr assertion)) 'ok)))]
         [else #f])))

(define (s2c/extract-assertion test-form)
  ;; Extract a single self-contained check from a test form.
  ;; LEGACY: kept for backward compatibility. Prefer s2c/extract-assertions.
  (let ([results (s2c/extract-assertions test-form)])
    (if (null? results) #f (car results))))

(define (s2c/extract-assertions test-form)
  ;; Extract ALL self-contained check expressions from a test form.
  ;; Returns a list of expressions using only standard primitives.
  ;; Strips any (define ...) forms from test bodies to prevent oracle leakage.
  (cond
    [(and (pair? test-form) (eq? (car test-form) 'define-test)
          (pair? (cdr test-form)) (pair? (cddr test-form)))
     ;; (define-test "name" body...) — collect ALL assertions, skip defines
     (let ([body (cddr test-form)])
       (filter-map
         (lambda (form)
           (and (pair? form)
                (not (eq? (car form) 'define))  ; strip target-fn definitions
                (memq (car form) '(assert-equal assert-true assert-false
                                    assert-ok))
                (s2c/assertion->check form)))
         body))]
    [(and (pair? test-form) (eq? (car test-form) 'test)
          (pair? (cdr test-form)) (pair? (cddr test-form)) (pair? (cdddr test-form)))
     ;; (test "name" expected actual) → (equal? actual expected)
     (list `(equal? ,(cadddr test-form) ,(caddr test-form)))]
    [else '()]))

(define (s2c/skeleton-body formals)
  ;; Build a skeleton body showing the function signature.
  ;; Returns a string like "(define (make-vec n init) <TODO>)"
  "<TODO>")

;;; ====
;;; Sample generators
;;; ====

(define (s2c/type-spec-sample ir)
  ;; Variant 1: Type signature + description → implement
  ;; Requires: type signature AND description
  (let ([type-sig (cdr (assq 'type ir))]
        [desc (cdr (assq 'description ir))]
        [name (cdr (assq 'name ir))]
        [skill (cdr (assq 'skill ir))]
        [module (cdr (assq 'module ir))]
        [file (cdr (assq 'file ir))]
        [clean-define (cdr (assq 'clean-define ir))]
        [tests (cdr (assq 'tests ir))])
    (and type-sig desc
         (let* ([difficulty (emit/ir-difficulty ir)]
                [skill-desc (s2c/skill-description skill)]
                [id (emit/next-id
                      (format "~a_spec_to_code_~a" skill name))]
                [prompt-body
                  (format "You are implementing ~a utilities in Fold-native Scheme.\n\nTarget module: ~a\nFunction: `~a`\nType: ~a\nSpec: ~a\n\nWrite exactly one Scheme function definition for `~a`.\nReturn only code, no explanation."
                          skill-desc file name
                          (s2c/format-type-sig type-sig)
                          desc name)]
                [gt-str (emit/sexp->pretty-string clean-define)]
                [ve (s2c/build-verify-expr ir)]
                [ve-str (if ve (emit/sexp->pretty-string ve) "")]
                [tags (emit/make-tags skill module "spec-to-code" name)]
                [sample (make-sample id "spec_to_code" "implementation"
                                     difficulty file
                                     (if (pair? tests) file "")
                                     name prompt-body gt-str ve-str tags)])
           ;; Attach sexp fields for in-process verification
           (append sample
                   `((ground_truth_sexp . ,clean-define)
                     (verify_sexp . ,ve)))))))

(define (s2c/test-spec-sample ir)
  ;; Variant 2: Test assertions as behavioral spec → implement
  ;; Requires: at least one test
  (let ([tests (cdr (assq 'tests ir))]
        [name (cdr (assq 'name ir))]
        [skill (cdr (assq 'skill ir))]
        [module (cdr (assq 'module ir))]
        [file (cdr (assq 'file ir))]
        [desc (cdr (assq 'description ir))]
        [clean-define (cdr (assq 'clean-define ir))])
    (and (pair? tests)
         (let* ([difficulty (emit/ir-difficulty ir)]
                [skill-desc (s2c/skill-description skill)]
                [id (emit/next-id
                      (format "~a_test_spec_~a" skill name))]
                [test-block (s2c/format-tests-for-prompt tests 3)]
                [prompt-body
                  (string-append
                    (format "You are implementing ~a utilities in Fold-native Scheme.\n\nTarget module: ~a\nFunction: `~a`\n" skill-desc file name)
                    (if desc (format "Brief: ~a\n\n" desc) "\n")
                    (format "Your implementation must pass these tests:\n```scheme\n~a```\n\nWrite exactly one Scheme function definition for `~a`.\nReturn only code, no explanation."
                            test-block name))]
                [gt-str (emit/sexp->pretty-string clean-define)]
                [ve (s2c/build-verify-expr ir)]
                [ve-str (if ve (emit/sexp->pretty-string ve) "")]
                [tags (emit/make-tags skill module "test-spec" name)]
                [sample (make-sample id "spec_to_code" "implementation"
                                     difficulty file file
                                     name prompt-body gt-str ve-str tags)])
           (append sample
                   `((ground_truth_sexp . ,clean-define)
                     (verify_sexp . ,ve)))))))

(define (s2c/skeleton-sample ir)
  ;; Variant 3: Skeleton with <TODO> body → complete
  ;; Requires: non-trivial formals (not just a value definition)
  (let ([formals (cdr (assq 'formals ir))]
        [name (cdr (assq 'name ir))]
        [skill (cdr (assq 'skill ir))]
        [module (cdr (assq 'module ir))]
        [file (cdr (assq 'file ir))]
        [desc (cdr (assq 'description ir))]
        [type-sig (cdr (assq 'type ir))]
        [clean-define (cdr (assq 'clean-define ir))]
        [tests (cdr (assq 'tests ir))])
    (and (or (pair? formals) (symbol? formals))  ; has parameters
         (let* ([difficulty (emit/ir-difficulty ir)]
                [flat-formals (walker/flatten-formals formals)]
                [skeleton (format "(define (~a~a) <TODO>)"
                                  name
                                  (apply string-append
                                    (map (lambda (f)
                                           (string-append " " (symbol->string f)))
                                         flat-formals)))]
                [skill-desc (s2c/skill-description skill)]
                [id (emit/next-id
                      (format "~a_skeleton_~a" skill name))]
                [prompt-body
                  (string-append
                    (format "Complete the following Fold Scheme function skeleton.\n\nModule: ~a (~a)\n\n```scheme\n~a\n```\n"
                            file skill-desc skeleton)
                    (if type-sig
                        (format "\nType: ~a\n" (s2c/format-type-sig type-sig))
                        "")
                    (if desc (format "Spec: ~a\n" desc) "")
                    "\nReplace `<TODO>` with the implementation.\nReturn only the complete function definition.")]
                [gt-str (emit/sexp->pretty-string clean-define)]
                [ve (s2c/build-verify-expr ir)]
                [ve-str (if ve (emit/sexp->pretty-string ve) "")]
                [tags (emit/make-tags skill module "skeleton" name)]
                [sample (make-sample id "spec_to_code" "implementation"
                                     difficulty file
                                     (if (pair? tests) file "")
                                     name prompt-body gt-str ve-str tags)])
           (append sample
                   `((ground_truth_sexp . ,clean-define)
                     (verify_sexp . ,ve)))))))

;;; ====
;;; Main generator
;;; ====

(define (generate-spec-to-code ir-records . opts)
  ;; Generate spec-to-code samples from walker IR records.
  ;; Options: first arg = #t for exported-only (default #t)
  (let* ([exported-only? (if (pair? opts) (car opts) #t)]
         [candidates (if exported-only?
                         (ir-exported-only ir-records)
                         ir-records)]
         [_ (emit/reset-counter!)]
         [_ (printf "Generating spec-to-code samples from ~a candidates...\n"
                     (length candidates))])
    (let loop ([remaining candidates] [acc '()] [counts '(0 0 0)])
      (if (null? remaining)
          (begin
            (printf "Generated ~a spec-to-code samples:\n" (length acc))
            (printf "  Type+desc:  ~a\n" (car counts))
            (printf "  Test-spec:  ~a\n" (cadr counts))
            (printf "  Skeleton:   ~a\n" (caddr counts))
            (reverse acc))
          (let* ([ir (car remaining)]
                 [s1 (guard (exn [else #f]) (s2c/type-spec-sample ir))]
                 [s2 (guard (exn [else #f]) (s2c/test-spec-sample ir))]
                 [s3 (guard (exn [else #f]) (s2c/skeleton-sample ir))]
                 [new-samples (filter values (list s1 s2 s3))]
                 [new-counts (list
                               (+ (car counts) (if s1 1 0))
                               (+ (cadr counts) (if s2 1 0))
                               (+ (caddr counts) (if s3 1 0)))])
            (loop (cdr remaining)
                  (append (reverse new-samples) acc)
                  new-counts))))))

;;; ====
;;; REPL interface
;;; ====

(printf "spec-to-code.ss loaded.\n")
(printf "  (generate-spec-to-code ir)       - Generate from walker IR (exported only)\n")
(printf "  (generate-spec-to-code ir #f)    - Generate from ALL functions\n")
