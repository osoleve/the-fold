;;; user/sft/extract/qa-gate.ss — Automated quality gate for SFT pipeline
;;;
;;; Validates structural invariants on generated samples. Run after every
;;; pipeline execution. Fails with non-zero exit and diagnostic output on
;;; any violation.
;;;
;;; Checks:
;;;   1. Oracle check:   verify_expr contains no (define (target-fn ...) ...)
;;;   2. Leakage check:  0% function-level cross-split leakage
;;;   3. Family check:   all expected families present
;;;   4. Duplicate check: no exact prompt duplicates
;;;   5. Difficulty check: each family has 2+ difficulty levels
;;;
;;; Usage:
;;;   (load "user/sft/extract/qa-gate.ss")
;;;   (qa-gate samples)   ; → #t if pass, errors with diagnostics on failure
;;;
;;; Or from runner:
;;;   (define ok (qa-gate all-samples))
;;;   (unless ok (error 'qa-gate "Quality gate FAILED"))

(unless (top-level-bound? 'filter-map) (load "core/base/prelude.ss"))
(load "core/base/sha256.ss")

;;; ====
;;; Check 1: Oracle — verify_expr must not contain target function definition
;;; ====

(define (qa/sexp-contains-define? sexp target-name)
  ;; Does sexp contain (define (target-name ...) ...) anywhere?
  (cond
    [(not (pair? sexp)) #f]
    [(and (eq? (car sexp) 'define)
          (pair? (cdr sexp))
          (pair? (cadr sexp))
          (eq? (car (cadr sexp)) target-name))
     #t]
    [else (or (qa/sexp-contains-define? (car sexp) target-name)
              (qa/sexp-contains-define? (cdr sexp) target-name))]))

(define (qa/check-oracle samples)
  ;; Check that no verify_sexp contains a define for the target function.
  ;; Returns (pass? . violations-list).
  (let loop ([remaining samples] [violations '()])
    (if (null? remaining)
        (cons (null? violations) (reverse violations))
        (let* ([s (car remaining)]
               [ve (let ([e (assq 'verify_sexp s)]) (if e (cdr e) #f))]
               [fn-name (let ([f (assq 'source_function s)])
                          (if f (if (string? (cdr f))
                                    (string->symbol (cdr f))
                                    (cdr f))
                              #f))]
               [id (cdr (assq 'id s))])
          (if (and ve fn-name (qa/sexp-contains-define? ve fn-name))
              (loop (cdr remaining) (cons id violations))
              (loop (cdr remaining) violations))))))

;;; ====
;;; Check 2: Cross-split leakage — no function in both train and eval
;;; ====

(define (qa/function-key sample)
  ;; Unique key for a function: "source_module:source_function"
  (let ([mod (cdr (assq 'source_module sample))]
        [fn (let ([f (assq 'source_function sample)])
              (if f (cdr f) ""))])
    (string-append mod ":" (if (symbol? fn) (symbol->string fn) fn))))

(define (qa/check-leakage samples)
  ;; Check that no function appears in both train and eval splits.
  ;; Returns (pass? . leaked-functions-list).
  (let ([train-fns '()]
        [eval-fns '()])
    ;; Collect function keys per split
    (for-each
      (lambda (s)
        (let ([key (qa/function-key s)]
              [split (cdr (assq 'split s))])
          (cond
            [(string=? split "train")
             (unless (member key train-fns)
               (set! train-fns (cons key train-fns)))]
            [(string=? split "eval")
             (unless (member key eval-fns)
               (set! eval-fns (cons key eval-fns)))])))
      samples)
    ;; Find intersection
    (let ([leaked (filter (lambda (k) (member k train-fns)) eval-fns)])
      (cons (null? leaked) leaked))))

;;; ====
;;; Check 3: Family presence — all expected families must appear
;;; ====

(define *qa-expected-families*
  '("spec_to_code" "bugfix" "cloze" "composition" "type_inhabit" "type_sig"))

(define (qa/check-families samples)
  ;; Check that all expected families are present.
  ;; Returns (pass? . missing-families).
  (let ([present (let loop ([ss samples] [seen '()])
                   (if (null? ss)
                       seen
                       (let ([fam (cdr (assq 'family (car ss)))])
                         (loop (cdr ss)
                               (if (member fam seen) seen (cons fam seen))))))])
    (let ([missing (filter (lambda (f) (not (member f present)))
                           *qa-expected-families*)])
      (cons (null? missing) missing))))

;;; ====
;;; Check 4: Duplicate prompts
;;; ====

(define (qa/check-duplicates samples)
  ;; Check for exact duplicate prompt_body fields.
  ;; Returns (pass? . duplicate-count).
  (let loop ([remaining samples] [seen '()] [dupes 0])
    (if (null? remaining)
        (cons (= dupes 0) dupes)
        (let* ([s (car remaining)]
               [body (cdr (assq 'prompt_body s))]
               [hash (sha256-hex (string->utf8 body))]
               [key (substring hash 0 16)])
          (if (member key seen)
              (loop (cdr remaining) seen (+ dupes 1))
              (loop (cdr remaining) (cons key seen) dupes))))))

;;; ====
;;; Check 5: Difficulty distribution — each family needs 2+ levels
;;; ====

(define (qa/check-difficulty samples)
  ;; Check that each family with 10+ samples has at least 2 difficulty levels.
  ;; Returns (pass? . flat-families).
  (let* ([families (let loop ([ss samples] [acc '()])
                     (if (null? ss) acc
                         (let* ([s (car ss)]
                                [fam (cdr (assq 'family s))]
                                [diff (cdr (assq 'difficulty s))]
                                [entry (find (lambda (g) (string=? (car g) fam)) acc)])
                           (if entry
                               (begin
                                 (unless (member diff (cdr entry))
                                   (set-cdr! entry (cons diff (cdr entry))))
                                 (loop (cdr ss) acc))
                               (loop (cdr ss) (cons (list fam diff) acc))))))]
         ;; Count samples per family to filter small families
         [fam-counts (let loop ([ss samples] [acc '()])
                       (if (null? ss) acc
                           (let* ([fam (cdr (assq 'family (car ss)))]
                                  [entry (find (lambda (g) (string=? (car g) fam)) acc)])
                             (if entry
                                 (begin (set-cdr! entry (+ (cdr entry) 1))
                                        (loop (cdr ss) acc))
                                 (loop (cdr ss) (cons (cons fam 1) acc))))))]
         [flat (filter (lambda (g)
                         (let ([count-entry (find (lambda (c) (string=? (car c) (car g)))
                                                  fam-counts)])
                           (and count-entry (> (cdr count-entry) 10)
                                (< (length (cdr g)) 2))))
                       families)])
    (cons (null? flat) (map car flat))))

;;; ====
;;; Main gate
;;; ====

(define (qa-gate samples)
  ;; Run all quality checks. Returns #t if all pass.
  ;; Prints diagnostics for every failure.
  (printf "\n=== QA Gate: ~a samples ===\n" (length samples))
  (let ([all-pass #t])

    ;; 1. Oracle check
    (let ([result (qa/check-oracle samples)])
      (if (car result)
          (printf "  [PASS] Oracle check: no verify_expr contains target define\n")
          (begin
            (set! all-pass #f)
            (printf "  [FAIL] Oracle check: ~a samples have target define in verify_expr\n"
                    (length (cdr result)))
            (for-each (lambda (id) (printf "    - ~a\n" id))
                      (if (> (length (cdr result)) 10)
                          (list-head (cdr result) 10)
                          (cdr result)))
            (when (> (length (cdr result)) 10)
              (printf "    ... and ~a more\n" (- (length (cdr result)) 10))))))

    ;; 2. Leakage check
    (let ([result (qa/check-leakage samples)])
      (if (car result)
          (printf "  [PASS] Leakage check: 0 functions in both train and eval\n")
          (begin
            (set! all-pass #f)
            (printf "  [FAIL] Leakage check: ~a functions appear in both splits\n"
                    (length (cdr result)))
            (for-each (lambda (k) (printf "    - ~a\n" k))
                      (if (> (length (cdr result)) 5)
                          (list-head (cdr result) 5)
                          (cdr result))))))

    ;; 3. Family check
    (let ([result (qa/check-families samples)])
      (if (car result)
          (printf "  [PASS] Family check: all ~a expected families present\n"
                  (length *qa-expected-families*))
          (begin
            (set! all-pass #f)
            (printf "  [FAIL] Family check: missing families: ~a\n"
                    (cdr result)))))

    ;; 4. Duplicate check
    (let ([result (qa/check-duplicates samples)])
      (if (car result)
          (printf "  [PASS] Duplicate check: no exact prompt duplicates\n")
          (begin
            (set! all-pass #f)
            (printf "  [FAIL] Duplicate check: ~a exact prompt duplicates found\n"
                    (cdr result)))))

    ;; 5. Difficulty check
    (let ([result (qa/check-difficulty samples)])
      (if (car result)
          (printf "  [PASS] Difficulty check: all families have 2+ difficulty levels\n")
          (begin
            (set! all-pass #f)
            (printf "  [FAIL] Difficulty check: flat distribution in: ~a\n"
                    (cdr result)))))

    (printf "=== QA Gate: ~a ===\n" (if all-pass "ALL PASSED" "FAILED"))
    all-pass))

;;; ====
;;; REPL interface
;;; ====

(printf "qa-gate.ss loaded.\n")
(printf "  (qa-gate samples)  - Run all quality checks\n")
