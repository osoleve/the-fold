;;; user/sft/extract/verify.ss — Execution verification for SFT samples
;;;
;;; Two verification modes:
;;;   1. verify-batch-sessions (recommended) — subprocess-isolated per-module.
;;;      Groups samples by source_module, spawns scheme --script per group.
;;;      ~280 samples/s. No environment poisoning. 91.6% pass rate.
;;;   2. verify-batch (legacy) — in-process eval. Fast (~6000/s) but
;;;      loading 271 modules poisons Chez built-ins like current-time.
;;;
;;; Usage:
;;;   (load "user/sft/extract/verify.ss")
;;;   (verify-batch-sessions samples)   ; isolated per-module (recommended)
;;;   (verify-batch samples)            ; in-process (fast, legacy)
;;;   (verify-sample gt-sexp ve-sexp)   ; quick single-sample check

(unless (top-level-bound? 'filter-map) (load "core/base/prelude.ss"))

;;; ====
;;; Configuration
;;; ====

(define *verify-timeout* 10)       ; seconds (subprocess only)
(define *verify-use-subprocess* #f) ; #t to force subprocess isolation

;;; ====
;;; In-process verification (fast path)
;;; ====

(define (verify-expr-from-parts ground-truth verify-expr)
  ;; Build a self-contained verification expression.
  ;; Both inputs are S-expressions.
  (if verify-expr
      `(let ()
         ,ground-truth
         ,verify-expr)
      ;; No verify-expr: just check that ground-truth doesn't error
      `(begin ,ground-truth #t)))

(define (verify/eval-in-process expr)
  ;; Evaluate a verification expression in-process.
  ;; Returns #t on success, #f on any error.
  ;; Safe because verify expressions use (let () ...) — lexical scope,
  ;; no global side effects.
  (guard (exn [else #f])
    (eq? #t (eval expr))))

(define (verify/eval-in-process-detail expr)
  ;; Detailed in-process eval. Returns alist.
  (guard (exn [else `((pass . #f)
                      (error . ,(if (condition? exn)
                                    (condition-message exn)
                                    (format "~a" exn))))])
    (let ([result (eval expr)])
      `((pass . ,(eq? #t result))
        (result . ,result)))))

;;; ====
;;; Subprocess verification (isolated fallback)
;;; ====

(define (verify/sexp->string expr)
  (let ([port (open-output-string)])
    (write expr port)
    (get-output-string port)))

(define (verify/run-subprocess expr-string)
  ;; Run via scheme --script. Returns (exit-code . stdout-string).
  (let ([tmp-script (format "/tmp/fold-verify-~a.ss" (random 999999))]
        [tmp-out (format "/tmp/fold-verify-~a.out" (random 999999))])
    (dynamic-wind
      (lambda () #f)
      (lambda ()
        (call-with-output-file tmp-script
          (lambda (port)
            (put-string port "(display\n")
            (put-string port expr-string)
            (put-string port "\n)\n(newline)\n"))
          'replace)
        (let* ([cmd (format "timeout ~a scheme --script ~a > ~a 2>/dev/null"
                            *verify-timeout* tmp-script tmp-out)]
               [exit-code (system cmd)]
               [stdout (if (file-exists? tmp-out)
                           (call-with-input-file tmp-out
                             (lambda (port)
                               (let loop ([acc ""])
                                 (let ([line (get-line port)])
                                   (if (eof-object? line) acc
                                       (loop (if (string=? acc "")
                                                  line
                                                  (string-append acc "\n" line))))))))
                           "")])
          (cons exit-code stdout)))
      (lambda ()
        (when (file-exists? tmp-script) (delete-file tmp-script))
        (when (file-exists? tmp-out) (delete-file tmp-out))))))

(define (verify/trim s)
  (let* ([len (string-length s)]
         [start (let loop ([i 0])
                  (if (and (< i len) (char-whitespace? (string-ref s i)))
                      (loop (+ i 1)) i))]
         [end (let loop ([i len])
                (if (and (> i start) (char-whitespace? (string-ref s (- i 1))))
                    (loop (- i 1)) i))])
    (if (>= start end) "" (substring s start end))))

;;; ====
;;; Public API — dispatches to fast or subprocess path
;;; ====

(define (verify-sample ground-truth-sexp verify-sexp)
  ;; Returns #t if the sample passes verification.
  (let ([expr (verify-expr-from-parts ground-truth-sexp verify-sexp)])
    (if *verify-use-subprocess*
        (guard (exn [else #f])
          (let* ([result (verify/run-subprocess (verify/sexp->string expr))]
                 [exit-code (car result)]
                 [stdout (cdr result)])
            (and (= exit-code 0)
                 (string=? (verify/trim stdout) "#t"))))
        (verify/eval-in-process expr))))

(define (verify-sample-detail ground-truth-sexp verify-sexp)
  ;; Detailed result alist.
  (let ([expr (verify-expr-from-parts ground-truth-sexp verify-sexp)])
    (if *verify-use-subprocess*
        (guard (exn [else `((pass . #f)
                            (error . ,(if (condition? exn)
                                          (condition-message exn)
                                          (format "~a" exn))))])
          (let* ([result (verify/run-subprocess (verify/sexp->string expr))]
                 [exit-code (car result)]
                 [stdout (cdr result)])
            `((pass . ,(and (= exit-code 0)
                            (string=? (verify/trim stdout) "#t")))
              (exit-code . ,exit-code)
              (stdout . ,stdout))))
        (verify/eval-in-process-detail expr))))

;;; ====
;;; Session-based verification (isolated per-module)
;;; ====
;;;
;;; Instead of loading all modules into one process (which poisons
;;; Chez built-ins like current-time), we spin up a named ./fold session
;;; per source module. Each session loads only its own require chain.
;;; No blacklists needed — sessions are isolated.

(define *verify-session-timeout* 15000)  ; ms per subprocess

(define (verify/group-by-module samples)
  ;; Group samples by source_module (file path).
  ;; Returns ((file sample ...) ...) alist.
  (let loop ([ss samples] [groups '()])
    (if (null? ss)
        (map (lambda (g) (cons (car g) (reverse (cdr g)))) groups)
        (let* ([s (car ss)]
               [file (cdr (assq 'source_module s))]
               [entry (find (lambda (g) (string=? (car g) file)) groups)])
          (if entry
              (begin (set-cdr! entry (cons s (cdr entry)))
                     (loop (cdr ss) groups))
              (loop (cdr ss)
                    (cons (cons file (list s)) groups)))))))

(define (verify/module-name-from-ir sample)
  ;; Extract the require-able module name from a sample's IR metadata.
  ;; Returns a symbol. May need namespacing for collision modules.
  (let ([mod (assq 'module sample)])
    (if (and mod (cdr mod) (symbol? (cdr mod)))
        (cdr mod)
        ;; Fallback: extract from path like "lattice/linalg/vec.ss" -> vec
        (let* ([path (cdr (assq 'source_module sample))]
               [base (let loop ([i (- (string-length path) 1)])
                       (cond [(< i 0) path]
                             [(char=? (string-ref path i) #\/)
                              (substring path (+ i 1) (string-length path))]
                             [else (loop (- i 1))]))]
               [name (if (and (> (string-length base) 3)
                              (string=? (substring base (- (string-length base) 3)
                                                   (string-length base))
                                        ".ss"))
                         (substring base 0 (- (string-length base) 3))
                         base)])
          (string->symbol name)))))

(define (verify/read-file-contents path)
  ;; Read entire file as string. Returns "" on error.
  (guard (exn [else ""])
    (if (file-exists? path)
        (call-with-input-file path
          (lambda (port)
            (let loop ([acc ""])
              (let ([line (get-line port)])
                (if (eof-object? line) acc
                    (loop (if (string=? acc "")
                               line
                               (string-append acc "\n" line))))))))
        "")))

(define (verify/run-module-subprocess module-name source-file samples)
  ;; Verify all samples from a module using a standalone scheme --script subprocess.
  ;; Each subprocess loads only the target module via require — no daemon needed.
  ;; source-file is the raw path (e.g. "lattice/egraph/union-find.ss") for fallback.
  ;; Returns list of booleans parallel to samples.
  (if (null? samples) '()
      (let* ([result-file (format "/tmp/fold-vresult-~a.dat" (random 99999))]
             [script-file (format "/tmp/fold-vbatch-~a.ss" (random 99999))]
             [exprs (map (lambda (s)
                           (let* ([gt (cdr (assq 'ground_truth_sexp s))]
                                  [ve (let ([e (assq 'verify_sexp s)])
                                        (if e (cdr e) #f))])
                             (verify-expr-from-parts gt ve)))
                         samples)])
        (dynamic-wind
          (lambda ()
            ;; Write a self-contained verification script
            (call-with-port
              (open-file-output-port script-file
                (file-options no-fail) (buffer-mode block)
                (make-transcoder (utf-8-codec)))
              (lambda (port)
                ;; Stub daemon-only symbols before bootstrapping
                (put-string port "(when (not (top-level-bound? 'meta-printf)) (eval '(define meta-printf printf)))\n")
                ;; Bootstrap module system
                (put-string port "(load \"core/lang/module.ss\")\n")
                ;; Load target module — try require first, fall back to load
                (put-string port (format "(guard (exn [else (load ~s)])\n  (require '~a))\n"
                                         source-file module-name))
                ;; Install test helpers
                (put-string port "(define (error-result? x) (and (pair? x) (eq? (car x) 'error)))\n")
                (put-string port "(define (approx= a b) (< (abs (- a b)) 1e-6))\n")
                ;; Evaluate each verify expression via eval inside guard
                (put-string port "(let ([results (list\n")
                (for-each
                  (lambda (e)
                    (put-string port "  (guard (exn [else #f]) (eq? #t (eval '")
                    (write e port)
                    (put-string port ")))\n"))
                  exprs)
                (put-string port ")])\n")
                ;; Write results to output file
                (put-string port (format "  (call-with-port\n"))
                (put-string port (format "    (open-file-output-port ~s\n" result-file))
                (put-string port "      (file-options no-fail) (buffer-mode block)\n")
                (put-string port "      (make-transcoder (utf-8-codec)))\n")
                (put-string port "    (lambda (p) (write results p) (newline p))))\n"))))
          (lambda ()
            ;; Run as standalone subprocess — completely isolated
            (let* ([cmd (format "timeout ~a scheme --script ~a 2>/dev/null"
                                (quotient *verify-session-timeout* 1000)
                                script-file)]
                   [exit-code (system cmd)])
              (if (and (= exit-code 0) (file-exists? result-file))
                  (guard (exn [else (map (lambda (_) #f) samples)])
                    (let* ([content (verify/read-file-contents result-file)]
                           [parsed (read (open-input-string content))])
                      (if (and (list? parsed) (= (length parsed) (length samples)))
                          parsed
                          (map (lambda (_) #f) samples))))
                  ;; Subprocess error — all fail
                  (map (lambda (_) #f) samples))))
          (lambda ()
            (when (file-exists? script-file) (delete-file script-file))
            (when (file-exists? result-file) (delete-file result-file)))))))

;;; ====
;;; Legacy: in-process preloading (kept for quick single-module checks)
;;; ====

(define (verify/install-test-helpers!)
  ;; Define common test-local helpers that leak into verify expressions.
  (unless (top-level-bound? 'error-result?)
    (eval '(define (error-result? x) (and (pair? x) (eq? (car x) 'error)))))
  (unless (top-level-bound? 'approx=)
    (eval '(define (approx= a b) (< (abs (- a b)) 1e-6)))))

(define *verify-load-blacklist*
  '("lattice/rewrite/verify.ss"
    "lattice/fp/meta/dsl.ss"))

(define (verify/preload-modules! samples)
  ;; LEGACY: Pre-load source modules into current process.
  ;; Prefer verify-batch-sessions for production use (no poisoning risk).
  (verify/install-test-helpers!)
  (let* ([files (let loop ([ss samples] [acc '()])
                  (if (null? ss) (reverse acc)
                      (let ([f (cdr (assq 'source_module (car ss)))])
                        (loop (cdr ss)
                              (if (member f acc) acc (cons f acc))))))]
         [safe-files (filter (lambda (f)
                               (not (member f *verify-load-blacklist*)))
                             files)]
         [loaded 0]
         [skipped (- (length files) (length safe-files))])
    (for-each (lambda (f)
                (guard (exn [else #f])
                  (load f)
                  (set! loaded (+ loaded 1))))
              safe-files)
    (printf "Preloaded ~a/~a source modules (~a blacklisted)\n"
            loaded (length files) skipped)))

;;; ====
;;; Batch verification — subprocess-isolated per-module (recommended)
;;; ====

(define (verify-batch-sessions samples . opts)
  ;; Verify samples using isolated per-module subprocesses.
  ;; Groups by source_module, runs a standalone scheme --script per module.
  ;; Each subprocess loads only its own require chain — zero poisoning.
  ;; Does NOT use the daemon — works from any context.
  ;;
  ;; Options: first arg = #t for verbose (print failures).
  ;; Returns (passed-list . failed-count).
  (let* ([verbose? (and (pair? opts) (car opts))]
         [total (length samples)]
         [groups (verify/group-by-module samples)]
         [_ (printf "Verifying ~a samples across ~a module groups...\n"
                    total (length groups))]
         [start-time (current-time)])
    (let loop ([remaining-groups groups]
               [all-passed '()]
               [all-failed 0]
               [modules-done 0])
      (if (null? remaining-groups)
          (let* ([end-time (current-time)]
                 [elapsed-ns (- (time-nanosecond end-time)
                                (time-nanosecond start-time))]
                 [elapsed-s (+ (- (time-second end-time)
                                  (time-second start-time))
                               (/ elapsed-ns 1e9))])
            (printf "Verification: ~a/~a passed (~a failed) in ~,1fs (~,1f samples/s)\n"
                    (length all-passed) total all-failed
                    elapsed-s
                    (if (> elapsed-s 0) (/ total elapsed-s) 0.0))
            (printf "  ~a module subprocesses used\n" (length groups))
            (cons (reverse all-passed) all-failed))
          (let* ([group (car remaining-groups)]
                 [file (car group)]
                 [group-samples (cdr group)]
                 [mod-name (verify/module-name-from-ir (car group-samples))]
                 ;; Run all samples for this module in one subprocess
                 [results
                   (guard (exn [else (map (lambda (_) #f) group-samples)])
                     (verify/run-module-subprocess mod-name file group-samples))]
                 ;; Collect pass/fail
                 [group-passed
                   (let zip ([ss group-samples] [rs results] [acc '()])
                     (if (or (null? ss) (null? rs))
                         (reverse acc)
                         (let ([s (car ss)] [ok? (car rs)])
                           (when (and verbose? (not ok?))
                             (printf "  FAIL: ~a\n" (cdr (assq 'id s))))
                           (zip (cdr ss) (cdr rs)
                                (if ok? (cons s acc) acc)))))]
                 [group-failed (- (length group-samples) (length group-passed))])
            ;; Progress
            (when (or (> group-failed 0) (= (modulo (+ modules-done 1) 20) 0))
              (printf "  [~a/~a] ~a: ~a/~a passed\n"
                      (+ modules-done 1) (length groups)
                      mod-name
                      (length group-passed) (length group-samples)))
            (loop (cdr remaining-groups)
                  (append (reverse group-passed) all-passed)
                  (+ all-failed group-failed)
                  (+ modules-done 1)))))))

;;; ====
;;; Batch verification — in-process (fast, legacy)
;;; ====

(define (verify-batch samples . opts)
  ;; In-process batch verification. Fast (~6000/s) but subject to
  ;; environment poisoning when modules redefine Chez built-ins.
  ;; Use verify-batch-sessions for production runs.
  ;;
  ;; Options: first arg = #t for verbose, second arg = #t for preload.
  ;; Returns (passed-list . failed-count).
  (let* ([verbose? (and (pair? opts) (car opts))]
         [preload? (if (and (pair? opts) (pair? (cdr opts))) (cadr opts) #f)]
         [_ (when preload? (verify/preload-modules! samples))]
         [total (length samples)]
         [start-time (current-time)])
    (let loop ([remaining samples] [passed '()] [failed 0] [n 0])
      (if (null? remaining)
          (let* ([end-time (current-time)]
                 [elapsed-ns (- (time-nanosecond end-time)
                                (time-nanosecond start-time))]
                 [elapsed-s (+ (- (time-second end-time)
                                  (time-second start-time))
                               (/ elapsed-ns 1e9))])
            (printf "Verification: ~a/~a passed (~a failed) in ~,1fs (~,1f samples/s)\n"
                    (length passed) total failed
                    elapsed-s
                    (if (> elapsed-s 0) (/ total elapsed-s) 0.0))
            (cons (reverse passed) failed))
          (let* ([sample (car remaining)]
                 [gt (cdr (assq 'ground_truth_sexp sample))]
                 [ve (let ([e (assq 'verify_sexp sample)])
                       (if e (cdr e) #f))]
                 [ok? (verify-sample gt ve)])
            (when (and verbose? (not ok?))
              (printf "  FAIL: ~a\n" (cdr (assq 'id sample))))
            (when (= (modulo (+ n 1) 1000) 0)
              (printf "  ... verified ~a/~a\n" (+ n 1) total))
            (loop (cdr remaining)
                  (if ok? (cons sample passed) passed)
                  (if ok? failed (+ failed 1))
                  (+ n 1)))))))

;;; ====
;;; String-based verification (for JSONL records with string fields)
;;; ====

(define (verify-sample-from-strings ground-truth-str verify-expr-str)
  ;; Verify from string fields (as stored in JSONL).
  ;; Parses strings to S-expressions, then verifies.
  (guard (exn [else #f])
    (let* ([gt (read (open-input-string ground-truth-str))]
           [ve (if (or (not verify-expr-str)
                       (string=? verify-expr-str ""))
                   #f
                   (read (open-input-string verify-expr-str)))])
      (verify-sample gt ve))))

;;; ====
;;; JSONL → sexp field adapter
;;; ====

(define (verify/prepare-jsonl-samples samples)
  ;; Convert JSONL-loaded samples (string fields) to verification-ready format
  ;; (sexp fields). Adds ground_truth_sexp and verify_sexp parsed from
  ;; ground_truth and verify_expr strings.
  ;; Samples that fail to parse are excluded.
  (filter-map
    (lambda (s)
      (guard (ex [else #f])
        (let* ([gt-str (cdr (assq 'ground_truth s))]
               [ve-str (let ([e (assq 'verify_expr s)])
                         (if e (cdr e) ""))]
               [gt-sexp (read (open-input-string gt-str))]
               [ve-sexp (if (or (not (string? ve-str))
                                (string=? ve-str ""))
                            #f
                            (read (open-input-string ve-str)))])
          (append s
                  `((ground_truth_sexp . ,gt-sexp)
                    (verify_sexp . ,ve-sexp))))))
    samples))

;;; ====
;;; REPL interface
;;; ====

(printf "verify.ss loaded.\n")
(printf "  (verify-batch-sessions samples)       - Session-isolated verify (recommended)\n")
(printf "  (verify-batch-sessions samples #t)    - Session-isolated + verbose\n")
(printf "  (verify-batch samples)                - In-process verify (fast, legacy)\n")
(printf "  (verify-batch samples #t #t)          - In-process + verbose + preload\n")
(printf "  (verify-sample gt-sexp ve-sexp)       - Quick single-sample check\n")
(printf "  (verify-sample-from-strings gt ve)    - Verify from JSONL strings\n")
