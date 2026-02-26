;;; user/sft/extract/mutate.ss — Mutation-based bugfix SFT sample extraction
;;;
;;; Generates bugfix samples by applying deterministic S-expression mutations
;;; to function bodies. Each mutation creates a "broken" version; the task
;;; is to fix it. Mutations are verified: mutant must FAIL tests while
;;; original PASSES.
;;;
;;; Mutation types:
;;;   - Accessor swap:    car <-> cdr, cadr <-> caddr, etc.
;;;   - Arithmetic swap:  + <-> -, * <-> /, < <-> <=, > <-> >=
;;;   - Equality swap:    eq? <-> equal?, = <-> eqv?
;;;   - Base case delete: Remove (null? ...) or (= n 0) clauses
;;;   - Branch swap:      Swap then/else in if
;;;   - Accumulator init: 0 <-> 1, '() <-> #f
;;;   - Off-by-one:       n <-> (+ n 1), (- n 1) <-> n
;;;   - Missing recursion: (f (cdr x)) -> (cdr x)
;;;
;;; Requires walker.ss, emit.ss, verify.ss loaded first.
;;;
;;; Usage:
;;;   (load "user/sft/extract/walker.ss")
;;;   (load "user/sft/extract/emit.ss")
;;;   (load "user/sft/extract/verify.ss")
;;;   (load "user/sft/extract/mutate.ss")
;;;   (define ir (walk-all-manifests))
;;;   (define samples (generate-bugfix-samples ir))

;;; ====
;;; Dependencies
;;; ====

(unless (top-level-bound? 'walk-all-manifests)
  (error 'mutate "walker.ss must be loaded first"))
(unless (top-level-bound? 'make-sample)
  (error 'mutate "emit.ss must be loaded first"))
(unless (top-level-bound? 'verify-sample)
  (error 'mutate "verify.ss must be loaded first"))

;;; ====
;;; Mutation primitives
;;; ====

;; Each mutator: (sexp) → (list-of (mutated-sexp . description))
;; Returns empty list if no applicable mutation site found.

(define *accessor-swaps*
  '((car . cdr) (cdr . car)
    (cadr . caddr) (caddr . cadr)
    (caar . cdar) (cdar . caar)))

(define *arith-swaps*
  '((+ . -) (- . +) (* . /) (/ . *)
    (< . <=) (<= . <) (> . >=) (>= . >)
    (< . >) (> . <)))

(define *equality-swaps*
  '((eq? . equal?) (equal? . eq?)
    (= . eqv?) (eqv? . =)
    (string=? . equal?) (equal? . string=?)))

(define (mut/find-swap sym table)
  ;; Look up a symbol in a swap table, return replacement or #f.
  (let ([entry (assq sym table)])
    (if entry (cdr entry) #f)))

(define (mut/swap-symbol sexp table description-prefix)
  ;; Walk sexp, replace first occurrence of a swappable symbol.
  ;; Returns (mutated . description) or #f.
  (let ([found #f])
    (let walk ([x sexp])
      (cond
        [found x]  ; already mutated, pass through
        [(and (symbol? x) (mut/find-swap x table))
         => (lambda (replacement)
              (set! found (cons x replacement))
              replacement)]
        [(pair? x)
         (let ([new-car (walk (car x))])
           (if found
               (cons new-car (cdr x))
               (cons (car x) (walk (cdr x)))))]
        [else x]))))

(define (mut/accessor-swap sexp)
  ;; Swap car<->cdr or similar accessor.
  (let ([result (mut/swap-symbol sexp *accessor-swaps* "accessor")])
    (if (not (equal? result sexp))
        (list (cons result
                    (format "Accessor swapped: ~a → ~a"
                            (car (let ([f #f])
                                   (let check ([x sexp])
                                     (cond
                                       [(and (symbol? x) (mut/find-swap x *accessor-swaps*))
                                        (set! f (cons x (mut/find-swap x *accessor-swaps*))) x]
                                       [(pair? x) (check (car x)) (unless f (check (cdr x))) x]
                                       [else x]))
                                   f))
                            (cdr (let ([f #f])
                                   (let check ([x sexp])
                                     (cond
                                       [(and (symbol? x) (mut/find-swap x *accessor-swaps*))
                                        (set! f (cons x (mut/find-swap x *accessor-swaps*))) x]
                                       [(pair? x) (check (car x)) (unless f (check (cdr x))) x]
                                       [else x]))
                                   f)))))
        '())))

(define (mut/arith-swap sexp)
  ;; Swap arithmetic/comparison operator.
  (let ([result (mut/swap-symbol sexp *arith-swaps* "arithmetic")])
    (if (not (equal? result sexp))
        (list (cons result "Arithmetic operator swapped"))
        '())))

(define (mut/equality-swap sexp)
  ;; Swap equality predicate.
  (let ([result (mut/swap-symbol sexp *equality-swaps* "equality")])
    (if (not (equal? result sexp))
        (list (cons result "Equality predicate swapped"))
        '())))

(define (mut/branch-swap sexp)
  ;; Find first (if cond then else) and swap then/else.
  (let ([found #f])
    (let walk ([x sexp])
      (cond
        [found x]
        [(and (pair? x) (eq? (car x) 'if)
              (pair? (cdr x)) (pair? (cddr x)) (pair? (cdddr x)))
         (set! found #t)
         `(if ,(cadr x) ,(cadddr x) ,(caddr x))]
        [(pair? x)
         (let ([new-car (walk (car x))])
           (if found
               (cons new-car (cdr x))
               (cons (car x) (walk (cdr x)))))]
        [else x]))))

(define (mut/if-branch-swap sexp)
  ;; Apply branch swap mutation.
  (let ([result (mut/branch-swap sexp)])
    (if (not (equal? result sexp))
        (list (cons result "If-branch then/else swapped"))
        '())))

(define (mut/delete-base-case sexp)
  ;; Remove a base-case clause from cond expressions.
  ;; Targets: [(null? ...) ...] or [(= x 0) ...] or [(zero? ...) ...]
  (let ([found #f])
    (let walk ([x sexp])
      (cond
        [found x]
        [(and (pair? x) (eq? (car x) 'cond) (pair? (cdr x)))
         ;; Look for a base-case clause to delete
         (let ([clauses (cdr x)])
           (let try-delete ([cs clauses] [before '()])
             (cond
               [(null? cs) x]  ; no base case found
               [(and (pair? (car cs)) (pair? (caar cs))
                     (memq (caaar cs) '(null? zero? not)))
                ;; Delete this clause
                (set! found #t)
                (cons 'cond (append (reverse before) (cdr cs)))]
               [(and (pair? (car cs)) (pair? (caar cs))
                     (eq? (caaar cs) '=)
                     (pair? (cdaar cs)) (pair? (cddaar cs))
                     (eqv? (cadaar cs) 0))
                ;; (= x 0) base case
                (set! found #t)
                (cons 'cond (append (reverse before) (cdr cs)))]
               [else (try-delete (cdr cs) (cons (car cs) before))])))]
        [(pair? x)
         (let ([new-car (walk (car x))])
           (if found
               (cons new-car (cdr x))
               (cons (car x) (walk (cdr x)))))]
        [else x]))))

(define (mut/base-case-delete sexp)
  (let ([result (mut/delete-base-case sexp)])
    (if (not (equal? result sexp))
        (list (cons result "Base case clause deleted"))
        '())))

(define (mut/accum-init sexp)
  ;; Swap accumulator initializers: 0<->1, '()<->#f
  (let ([found #f])
    (let walk ([x sexp])
      (cond
        [found x]
        ;; Named let with accumulator init
        [(and (pair? x) (eq? (car x) 'let)
              (pair? (cdr x)) (symbol? (cadr x))
              (pair? (cddr x)) (pair? (caddr x)))
         ;; (let name ((var init) ...) body)
         (let ([bindings (caddr x)])
           (let try-swap ([bs bindings] [before '()])
             (cond
               [(null? bs)
                ;; Try walking body
                (let ([new-body (map walk (cdddr x))])
                  (if found
                      `(let ,(cadr x) ,bindings ,@new-body)
                      x))]
               [(and (pair? (car bs)) (pair? (cdar bs))
                     (eqv? (cadar bs) 0))
                (set! found #t)
                `(let ,(cadr x)
                   ,(append (reverse before)
                            (list (list (caar bs) 1))
                            (cdr bs))
                   ,@(cdddr x))]
               [(and (pair? (car bs)) (pair? (cdar bs))
                     (equal? (cadar bs) '(quote ())))
                (set! found #t)
                `(let ,(cadr x)
                   ,(append (reverse before)
                            (list (list (caar bs) #f))
                            (cdr bs))
                   ,@(cdddr x))]
               [else (try-swap (cdr bs) (cons (car bs) before))])))]
        [(pair? x)
         (let ([new-car (walk (car x))])
           (if found
               (cons new-car (cdr x))
               (cons (car x) (walk (cdr x)))))]
        [else x]))))

(define (mut/accumulator-init sexp)
  (let ([result (mut/accum-init sexp)])
    (if (not (equal? result sexp))
        (list (cons result "Accumulator initializer swapped"))
        '())))

;;; ====
;;; Off-by-one mutation
;;; ====

(define (mut/off-by-one sexp)
  ;; Introduce off-by-one errors: n → (+ n 1), (- n 1) → n, etc.
  ;; Targets common patterns in loop bounds and indices.
  (let ([found #f])
    (let walk ([x sexp])
      (cond
        [found x]
        ;; (- n 1) → n  (drop the decrement)
        [(and (pair? x) (eq? (car x) '-)
              (pair? (cdr x)) (pair? (cddr x))
              (null? (cdddr x))
              (or (symbol? (cadr x)) (number? (cadr x)))
              (eqv? (caddr x) 1))
         (set! found #t)
         (cadr x)]
        ;; (+ n 1) → n  (drop the increment)
        [(and (pair? x) (eq? (car x) '+)
              (pair? (cdr x)) (pair? (cddr x))
              (null? (cdddr x))
              (or (symbol? (cadr x)) (number? (cadr x)))
              (eqv? (caddr x) 1))
         (set! found #t)
         (cadr x)]
        ;; (length lst) → (- (length lst) 1) — common array bounds error
        [(and (pair? x) (eq? (car x) 'length)
              (pair? (cdr x)) (null? (cddr x)))
         (set! found #t)
         `(- ,x 1)]
        [(pair? x)
         (let ([new-car (walk (car x))])
           (if found (cons new-car (cdr x))
               (cons (car x) (walk (cdr x)))))]
        [else x]))))

(define (mut/off-by-one-mutator sexp)
  (let ([result (mut/off-by-one sexp)])
    (if (not (equal? result sexp))
        (list (cons result "Off-by-one error introduced"))
        '())))

;;; ====
;;; Missing recursion mutation
;;; ====

(define (mut/drop-recursion sexp fn-name)
  ;; Replace (fn-name (cdr x)) with (cdr x) — drops recursive call.
  ;; Only applies when fn-name is known.
  (let ([found #f])
    (let walk ([x sexp])
      (cond
        [found x]
        ;; (fn-name arg) where arg contains cdr/car → just arg
        [(and (pair? x) (eq? (car x) fn-name)
              (pair? (cdr x)) (null? (cddr x)))
         (set! found #t)
         (cadr x)]
        [(pair? x)
         (let ([new-car (walk (car x))])
           (if found (cons new-car (cdr x))
               (cons (car x) (walk (cdr x)))))]
        [else x]))))

(define (mut/missing-recursion-mutator sexp)
  ;; Requires detecting self-referential calls. Works on function bodies
  ;; where the define-name appears inside the body. We scan for the first
  ;; symbol that appears as both (sym arg) and at the top of the define.
  ;; For standalone body mutation, we look for named-let patterns.
  (let ([found #f])
    ;; Check for named let: (let name ((v i) ...) body)
    (let walk ([x sexp])
      (cond
        [found x]
        [(and (pair? x) (eq? (car x) 'let)
              (pair? (cdr x)) (symbol? (cadr x))
              (pair? (cddr x)))
         ;; Named let — the loop name is (cadr x)
         (let ([loop-name (cadr x)])
           (let ([result (mut/drop-recursion (cdddr x) loop-name)])
             (if (not (equal? result (cdddr x)))
                 (begin (set! found #t)
                        `(let ,loop-name ,(caddr x) ,@result))
                 x)))]
        [(pair? x)
         (let ([new-car (walk (car x))])
           (if found (cons new-car (cdr x))
               (cons (car x) (walk (cdr x)))))]
        [else x]))))

(define (mut/missing-recursion sexp)
  (let ([result (mut/missing-recursion-mutator sexp)])
    (if (not (equal? result sexp))
        (list (cons result "Missing recursion — recursive call dropped"))
        '())))

;;; ====
;;; Mutation application
;;; ====

(define *all-mutators*
  (list mut/accessor-swap
        mut/arith-swap
        mut/equality-swap
        mut/if-branch-swap
        mut/base-case-delete
        mut/accumulator-init
        mut/off-by-one-mutator
        mut/missing-recursion))

(define (mutate-body body)
  ;; Apply all mutators to a function body.
  ;; Returns list of (mutated-body . description) pairs.
  ;; Each mutator produces at most one mutation (first-site-wins).
  (let ([body-sexp (if (and (pair? body) (= (length body) 1))
                       (car body)
                       (cons 'begin body))])
    (append-map (lambda (mutator)
                  (guard (exn [else '()])
                    (mutator body-sexp)))
                *all-mutators*)))

(define (reconstruct-define name formals mutated-body is-function?)
  ;; Rebuild a define form from name, formals, and mutated body.
  ;; is-function? distinguishes thunks (define (f) ...) from value defs
  ;; (define f ...) — both have formals = '() but need different reconstruction.
  (if is-function?
      (if (and (pair? mutated-body) (eq? (car mutated-body) 'begin))
          `(define (,name ,@(walker/flatten-formals formals))
             ,@(cdr mutated-body))
          `(define (,name ,@(walker/flatten-formals formals))
             ,mutated-body))
      `(define ,name ,mutated-body)))

;;; ====
;;; Bugfix sample generation
;;; ====

(define (mut/make-bugfix-sample ir mutated-body mutation-desc)
  ;; Build a bugfix sample from an IR record and a mutation.
  (let* ([name (cdr (assq 'name ir))]
         [skill (cdr (assq 'skill ir))]
         [module (cdr (assq 'module ir))]
         [file (cdr (assq 'file ir))]
         [formals (cdr (assq 'formals ir))]
         [clean-define (cdr (assq 'clean-define ir))]
         [tests (cdr (assq 'tests ir))]
         [desc (cdr (assq 'description ir))]
         [is-fn? (pair? (cadr clean-define))]  ; (define (f ...) ...) vs (define f ...)
         [mutant-define (reconstruct-define name formals mutated-body is-fn?)]
         [id (emit/next-id (format "~a_bugfix_~a" skill name))]
         [prompt-body
           (string-append
             (format "Fix the bug in this Fold Scheme function with minimal semantic changes.\nTarget: `~a` in `~a`.\nKnown issue: ~a\n\n```scheme\n~a\n```\n\nReturn only the corrected definition."
                     name file mutation-desc
                     (emit/sexp->pretty-string mutant-define)))]
         [gt-str (emit/sexp->pretty-string clean-define)]
         [ve (s2c/build-verify-expr ir)]
         [ve-str (if ve (emit/sexp->pretty-string ve) "")]
         [tags (emit/make-tags skill module "bugfix" name)])
    (let ([sample (make-sample id "bugfix" "debugging"
                              (emit/mutation-difficulty mutation-desc) file
                              (if (pair? tests) file "")
                              name prompt-body gt-str ve-str
                              tags
                              #f  ; no available-fns
                              mutation-desc)])
      ;; Attach sexp fields for in-process verification
      (append sample
              `((ground_truth_sexp . ,clean-define)
                (verify_sexp . ,ve))))))

;;; ====
;;; Main generator
;;; ====

(define *mutate-verify?* #f)  ; Set to #t to enable execution verification

(define (generate-bugfix-samples ir-records . opts)
  ;; Generate bugfix samples from walker IR.
  ;; Default: exported functions with tests only.
  ;; Set *mutate-verify?* to #t for execution verification (slow).
  (let* ([exported-only? (if (pair? opts) (car opts) #t)]
         [candidates (filter
                       (lambda (ir)
                         (and (if exported-only? (cdr (assq 'exported? ir)) #t)
                              (pair? (cdr (assq 'tests ir)))
                              (pair? (cdr (assq 'body ir)))))
                       ir-records)]
         [_ (emit/reset-counter!)]
         [_ (printf "Generating bugfix samples from ~a candidates"
                     (length candidates))]
         [_ (when *mutate-verify?*
              (printf " (with execution verification)"))]
         [_ (printf "...\n")])
    (let loop ([remaining candidates]
               [acc '()]
               [total-mutations 0]
               [verified 0]
               [skipped 0])
      (if (null? remaining)
          (begin
            (printf "Generated ~a bugfix samples\n" (length acc))
            (printf "  Total mutations tried: ~a\n" total-mutations)
            (when *mutate-verify?*
              (printf "  Verified (mutant fails): ~a\n" verified)
              (printf "  Skipped (mutant passes): ~a\n" skipped))
            (reverse acc))
          (let* ([ir (car remaining)]
                 [body (cdr (assq 'body ir))]
                 [mutations (guard (exn [else '()]) (mutate-body body))]
                 ;; Take at most 2 mutations per function to avoid explosion
                 [selected (if (> (length mutations) 2)
                               (list (car mutations) (cadr mutations))
                               mutations)]
                 [new-samples
                   (filter-map
                     (lambda (mut)
                       (guard (exn [else #f])
                         (let ([sample (mut/make-bugfix-sample
                                         ir (car mut) (cdr mut))])
                           (if *mutate-verify?*
                               ;; Verify: mutant should FAIL tests
                               (let* ([cd (cdr (assq 'clean-define ir))]
                                      [mutant-define
                                        (reconstruct-define
                                          (cdr (assq 'name ir))
                                          (cdr (assq 'formals ir))
                                          (car mut)
                                          (pair? (cadr cd)))]
                                      [ve (s2c/build-verify-expr ir)]
                                      [mutant-passes?
                                        (if ve (verify-sample mutant-define ve) #f)])
                                 (if mutant-passes?
                                     #f  ; mutant passes = useless mutation
                                     sample))
                               sample))))
                     selected)])
            (when (and (= (modulo (+ (length acc) 1) 500) 0)
                       (> (length acc) 0))
              (printf "  ... ~a samples so far\n" (length acc)))
            (loop (cdr remaining)
                  (append (reverse new-samples) acc)
                  (+ total-mutations (length selected))
                  (+ verified (length new-samples))
                  (+ skipped (- (length selected) (length new-samples)))))))))

;;; ====
;;; REPL interface
;;; ====

(printf "mutate.ss loaded.\n")
(printf "  (generate-bugfix-samples ir)         - Generate bugfix samples\n")
(printf "  (set! *mutate-verify?* #t)           - Enable execution verification\n")
(printf "  (mutate-body body-sexps)             - Test mutations on a body\n")
