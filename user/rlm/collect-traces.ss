;;; user/rlm/collect-traces.ss — Trajectory Collector
;;;
;;; Reads bench-results-*.sexp files, extracts successful trajectories
;;; from CAS, and produces JSONL training data. Generalizes sft-extract.ss
;;; to work with ANY benchmark (GSM8K, DROP, OOLONG, Intellect3, etc.).
;;;
;;; Usage:
;;;   scheme --script user/rlm/collect-traces.ss [results-file ...]
;;;
;;; If no files specified, scans user/rlm/bench-results-*.sexp for all results.
;;;
;;; Output: user/rlm/training/collected-traces.jsonl

(load "boundary/pipeline/rlm2-drive.ss")
(load "boundary/io/json.ss")

;;; ====
;;; Configuration
;;; ====

(define *collect-output-path* "user/rlm/training/collected-traces.jsonl")
(define *collect-context-budget* 8000)

;;; ====
;;; Results File Reader
;;; ====

(define (collect-read-results path)
  ;; Read a bench-results sexp file. Returns the full sexp or #f.
  (guard (ex [else
              (display (format "  WARN: cannot read ~a: ~a\n" path
                (if (message-condition? ex) (condition-message ex) ex)))
              #f])
    (with-input-from-file path read)))

(define (collect-extract-winners results-data)
  ;; Extract (trajectory-hash mode label) from correct results.
  ;; Handles both flat (gsm8k/drop) and nested (oolong/pairs) formats.
  ;; results-data may be (benchmark-results (field ...) ...) — skip tag if symbol.
  (let* ([results-data (if (and (pair? results-data)
                                (symbol? (car results-data)))
                           (cdr results-data)
                           results-data)]
         [mode-raw (assq 'mode results-data)]
         [mode (if mode-raw (cdr mode-raw) "unknown")]
         [model-raw (assq 'model results-data)]
         [model (if model-raw (cdr model-raw) "unknown")])
    (cond
      ;; Flat format: (benchmark-results (problems ((correct . #t) ...)))
      [(assq 'problems results-data)
       => (lambda (p)
            ;; (problems (list-of-alists)) — car unwraps the extra nesting
            (let loop ([probs (car (cdr p))] [i 0] [acc '()])
              (if (null? probs)
                  (reverse acc)
                  (let* ([prob (car probs)]
                         [correct? (let ([c (assq 'correct prob)])
                                     (and c (cdr c)))]
                         [traj (let ([t (assq 'trajectory prob)])
                                 (and t (cdr t)))]
                         [label (format "~a-~a-~a" mode model i)])
                    (loop (cdr probs) (+ i 1)
                          (if (and correct? traj (string? traj))
                              (cons (list traj label mode) acc)
                              acc))))))]
      ;; Nested format: (benchmark-results (oolong (...)) (pairs (...)))
      [(assq 'oolong results-data)
       => (lambda (o)
            (let* ([oolong-results (cdr o)]
                   [pairs-raw (assq 'pairs results-data)]
                   [pairs-results (if pairs-raw (cdr pairs-raw) '())]
                   [all (append oolong-results pairs-results)])
              (let loop ([items all] [i 0] [acc '()])
                (if (null? items)
                    (reverse acc)
                    (let* ([item (car items)]
                           [correct? (let ([c (assq 'correct item)])
                                       (and c (cdr c)))]
                           ;; Pairs uses F1 instead of correct
                           [f1-ok? (let ([f (assq 'f1 item)])
                                     (and f (> (cdr f) 0.5)))]
                           [winner? (or correct? f1-ok?)]
                           [traj (let ([t (assq 'trajectory item)])
                                   (and t (cdr t)))]
                           [lbl (let ([l (assq 'label item)])
                                  (if l (cdr l)
                                      (format "oolong-~a" i)))]
                           [label (format "~a-~a" model lbl)])
                      (loop (cdr items) (+ i 1)
                            (if (and winner? traj (string? traj))
                                (cons (list traj label "oolong") acc)
                                acc)))))))]
      [else '()])))

;;; ====
;;; CAS Helpers (from sft-extract.ss, generalized)
;;; ====

(define (collect-fetch-block hex)
  (fetch-persistent (hex->hash hex)))

(define (collect-decode-payload blk)
  (guard (ex [else #f])
    (read (open-input-string (utf8->string (block-payload blk))))))

;;; ====
;;; Step Chain Walker
;;; ====

(define (collect-walk-step-chain last-step-hash)
  ;; Walk backward from last step, return chronological list of step payloads.
  (let loop ([hash last-step-hash] [steps '()])
    (let ([blk (fetch-persistent hash)])
      (cond
        [(not blk) steps]
        [(not (eq? (block-tag blk) 'rlm2/step))
         (display (format "  SKIP: tag ~a (not rlm2/step)\n" (block-tag blk)))
         '()]
        [else
         (let* ([payload (collect-decode-payload blk)]
                [step-refs (block-refs blk)]
                [prev (if (> (vector-length step-refs) 0)
                          (vector-ref step-refs 0)
                          #f)]
                [augmented (cons (cons 'step-hash (hash->hex hash))
                                payload)]
                [steps* (cons augmented steps)])
           (if prev
               (loop prev steps*)
               steps*))]))))

(define (collect-walk-trajectory traj-hex)
  ;; Returns: (values config-data steps) or (values #f '())
  (let ([traj-blk (collect-fetch-block traj-hex)])
    (cond
      [(not traj-blk)
       (display (format "  SKIP: trajectory ~a not found\n"
                        (substring traj-hex 0 (min 16 (string-length traj-hex)))))
       (values #f '())]
      [else
       (let* ([traj-data (collect-decode-payload traj-blk)]
              [refs (block-refs traj-blk)]
              [nrefs (vector-length refs)])
         (cond
           [(not traj-data)
            (display "  SKIP: cannot decode trajectory\n")
            (values #f '())]
           [(not (eq? (rlm2-trajectory-status traj-data) 'completed))
            (display (format "  SKIP: status ~a\n" (rlm2-trajectory-status traj-data)))
            (values #f '())]
           [(< nrefs 1)
            (display "  SKIP: no step chain\n")
            (values #f '())]
           [else
            (let* ([config-hex (rlm2-trajectory-config-hash traj-data)]
                   [config-blk (collect-fetch-block config-hex)])
              (cond
                [(not config-blk)
                 (display "  SKIP: config not found\n")
                 (values #f '())]
                [else
                 (let ([config-data (collect-decode-payload config-blk)]
                       [last-step-hash (vector-ref refs 0)])
                   (values config-data
                           (collect-walk-step-chain last-step-hash)))]))]))])))

;;; ====
;;; State Reconstruction (approximate)
;;; ====

(define (collect-unquote-key arg)
  (if (and (pair? arg) (eq? (car arg) 'quote) (pair? (cdr arg)))
      (cadr arg)
      arg))

(define (collect-build-initial-state task-text max-fuel)
  ;; Approximate initial state — we don't know exact input size,
  ;; but that doesn't matter much for HUD rendering.
  (let ([env (rlm-env-put
               (rlm-env-put '() 'input "input-hash" 'chunks 50000)
               'task "task-hash" 'text (string-length task-text))])
    (make-rlm2-state task-text '() env '() '() '() '()
                     #f max-fuel 0)))

(define (collect-apply-action state action note fuel-used step-hash)
  (let* ([action-type (if (pair? action) (car action) 'unknown)]
         [env (rlm2-state-env state)]
         [env* (case action-type
                 [(store)
                  (if (>= (length action) 3)
                      (rlm-env-put env (collect-unquote-key (cadr action))
                                   "computed" 'result 0)
                      env)]
                 [(eval)
                  (rlm-env-put env 'last-result "computed" 'result 0)]
                 [(map-chunks)
                  (rlm-env-put env 'map-result "computed" 'result 0)]
                 [else env])]
         [plan* (if (eq? action-type 'plan!)
                    (if (>= (length action) 2) (cdr action) '())
                    (rlm2-state-plan state))]
         [loaded* (if (eq? action-type 'load)
                      (if (>= (length action) 2)
                          (cons (cadr action) (rlm2-state-loaded state))
                          (rlm2-state-loaded state))
                      (rlm2-state-loaded state))]
         [notes* (if (and (string? note) (> (string-length note) 0))
                     (cons note (rlm2-state-notes state))
                     (rlm2-state-notes state))]
         [episodic* (cons (cons (rlm2-state-step state) step-hash)
                          (rlm2-state-episodic state))]
         [journal* (if (and (eq? action-type 'journal)
                            (>= (length action) 3))
                       (cons (cons (collect-unquote-key (cadr action))
                                   (caddr action))
                             (rlm2-state-journal state))
                       (rlm2-state-journal state))])
    (make-rlm2-state
      (rlm2-state-task state)
      plan* env* loaded* notes* episodic* journal*
      #f
      (max 0 (- (rlm2-state-fuel state) fuel-used))
      (+ (rlm2-state-step state) 1))))

;;; ====
;;; Training Example Generator
;;; ====

(define (collect-format-action action)
  (let ([port (open-output-string)])
    (write action port)
    (get-output-string port)))

(define (collect-make-example sys-prompt hud action-str source step-num
                              traj-hex phase)
  `((messages . (((role . "system") (content . ,sys-prompt))
                 ((role . "user") (content . ,hud))
                 ((role . "assistant") (content . ,action-str))))
    (source . ,source)
    (step . ,step-num)
    (phase . ,phase)
    (trajectory . ,traj-hex)))

(define (collect-make-reflect-example sys-prompt reflect-prompt note
                                      source step-num traj-hex)
  `((messages . (((role . "system") (content . ,sys-prompt))
                 ((role . "user") (content . ,reflect-prompt))
                 ((role . "assistant") (content . ,note))))
    (source . ,source)
    (step . ,step-num)
    (phase . "reflect")
    (trajectory . ,traj-hex)))

(define (collect-extract-trajectory traj-hex label)
  (display (format "\n  Extracting: ~a (~a...)\n" label
                   (substring traj-hex 0 (min 16 (string-length traj-hex)))))

  (let-values ([(config-data steps) (collect-walk-trajectory traj-hex)])
    (if (not config-data)
        (begin (display "    -> 0 examples (skipped)\n") '())
        (let* ([sys-prompt-base (rlm2-config-system-prompt config-data)]
               [sys-prompt (rlm2-build-system-prompt sys-prompt-base)]
               [max-fuel (rlm2-config-max-fuel config-data)]
               ;; Try to reconstruct task from first step context
               [initial-state (collect-build-initial-state
                                "task" max-fuel)])
          (let loop ([remaining steps]
                     [state initial-state]
                     [examples '()])
            (if (null? remaining)
                (let ([result (reverse examples)])
                  (display (format "    -> ~a examples\n" (length result)))
                  result)
                (let* ([step-rec (car remaining)]
                       [action (cdr (assq 'action step-rec))]
                       [note (cdr (assq 'note step-rec))]
                       [fuel-used (cdr (assq 'fuel-used step-rec))]
                       [step-num (cdr (assq 'step step-rec))]
                       [step-hash (cdr (assq 'step-hash step-rec))]
                       ;; Render HUD
                       [hud (rlm2-render-state state *collect-context-budget*)]
                       [action-str (collect-format-action action)]
                       ;; Act example
                       [act-example (collect-make-example
                                      sys-prompt hud action-str
                                      label step-num traj-hex "act")]
                       ;; Reflect example (skip mechanical actions)
                       [reflect-examples
                        (if (and (string? note) (> (string-length note) 0)
                                 (pair? action)
                                 (not (memq (car action) '(submit noop))))
                            (list (collect-make-reflect-example
                                    sys-prompt
                                    (format "You just executed: ~a\nBriefly note what you learned."
                                            action-str)
                                    note label step-num traj-hex))
                            '())]
                       [state* (collect-apply-action
                                 state action note fuel-used step-hash)])
                  (loop (cdr remaining) state*
                        (append reflect-examples
                                (cons act-example examples))))))))))

;;; ====
;;; JSONL Writer
;;; ====

(define (collect-write-jsonl! examples output-path)
  (call-with-port
    (open-file-output-port output-path
      (file-options no-fail)
      (buffer-mode block)
      (make-transcoder (utf-8-codec)))
    (lambda (port)
      (for-each
        (lambda (example)
          (put-string port (json->string example))
          (put-string port "\n"))
        examples))))

;;; ====
;;; File Scanner
;;; ====

(define (collect-find-result-files)
  ;; Find all bench-results-*.sexp files
  (let ([entries (directory-list "user/rlm")])
    (filter (lambda (f)
              (and (>= (string-length f) 14)
                   (string=? (substring f 0 14) "bench-results-")
                   (let ([len (string-length f)])
                     (string=? (substring f (- len 5) len) ".sexp"))))
            entries)))

;;; ====
;;; Main Driver
;;; ====

(define (collect-traces!)
  (let* ([args (cdr (command-line))]
         [files (if (null? args)
                    (map (lambda (f) (string-append "user/rlm/" f))
                         (collect-find-result-files))
                    args)])

    (display "Trajectory Collector\n")
    (display "====================\n")
    (display (format "Scanning ~a result files...\n\n" (length files)))
    (flush-output-port)

    (let loop ([remaining files]
               [all-winners '()]
               [all-examples '()]
               [n-files 0]
               [n-skipped 0])
      (if (null? remaining)
          ;; Done scanning — report and write
          (begin
            (display (format "\n=== Collection Summary ===\n"))
            (display (format "Files scanned: ~a (~a had results)\n"
                             (+ n-files n-skipped) n-files))
            (display (format "Winning trajectories: ~a\n" (length all-winners)))
            (display (format "Training examples: ~a (act + reflect)\n"
                             (length all-examples)))

            ;; Action distribution
            (let ([act-count 0] [reflect-count 0])
              (for-each
                (lambda (ex)
                  (let ([phase (cdr (assq 'phase ex))])
                    (if (string=? phase "act")
                        (set! act-count (+ act-count 1))
                        (set! reflect-count (+ reflect-count 1)))))
                all-examples)
              (display (format "  Act: ~a | Reflect: ~a\n" act-count reflect-count)))

            (if (null? all-examples)
                (display "No examples to write.\n")
                (begin
                  (collect-write-jsonl! all-examples *collect-output-path*)
                  (display (format "Written to: ~a\n" *collect-output-path*))))
            (length all-examples))

          ;; Process next file
          (let* ([path (car remaining)]
                 [data (collect-read-results path)])
            (if (not data)
                (loop (cdr remaining) all-winners all-examples
                      n-files (+ n-skipped 1))
                (let ([winners (collect-extract-winners data)])
                  (display (format "~a: ~a winners\n" path (length winners)))
                  (flush-output-port)

                  (let inner ([ws winners] [examples all-examples])
                    (if (null? ws)
                        (loop (cdr remaining)
                              (append all-winners winners)
                              examples
                              (+ n-files 1) n-skipped)
                        (let* ([w (car ws)]
                               [traj-hex (car w)]
                               [label (cadr w)]
                               [new-examples
                                (guard (ex [else
                                           (display (format "    ERROR: ~a\n"
                                             (if (message-condition? ex)
                                                 (condition-message ex)
                                                 ex)))
                                           '()])
                                  (collect-extract-trajectory traj-hex label))])
                          (inner (cdr ws)
                                 (append examples new-examples))))))))))))

;;; ====
;;; Entry Point
;;; ====

(collect-traces!)
