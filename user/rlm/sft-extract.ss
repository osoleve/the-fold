;;; user/rlm/sft-extract.ss -- SFT Training Data Extractor
;;;
;;; Walks CAS trajectory chains from successful Qwen3 OOLONG runs,
;;; reconstructs approximate state at each step, renders HUDs, and
;;; produces JSONL training data for Nemotron SFT.
;;;
;;; Run: scheme --script user/rlm/sft-extract.ss

(load "boundary/pipeline/rlm2-drive.ss")
(load "boundary/io/json.ss")

;;; ====
;;; Configuration
;;; ====

(define *sft-output-path* "user/rlm/sft-training-data.jsonl")
(define *sft-context-budget* 8000)

;; Successful Qwen3 trajectory hashes
;; Source: user/rlm/bench-results-2026-02-06T20:08:50Z.sexp
(define *sft-trajectories*
  '(;; v2 trajectories (primary -- known CAS format)
    ("0081da091a0a9a1bd7ecfb9061d95d54eabab2dcdbe53a74e561e7fd6154fe43c3"
     "qwen3-oolong-100-v2")
    ("0045f4c41e6b78dbbcc531b55ea952c88c69dd91d06bc2b640a6acb27baf4cb8bb"
     "qwen3-oolong-200-v2")
    ;; v1 trajectories (may have different CAS format -- extracted if compatible)
    ("00022c1157e0ab1929bfd2ef28844558dd8052a7f74c581861bb25aa4077f136a3"
     "qwen3-oolong-100-v1")
    ("007546a3c553b8d8dd48d2cf592fe2c4225097d21012351876b155c8815466da5b"
     "qwen3-oolong-200-v1")))

;; OOLONG task description (same for all runs, from bench.ss)
(define *oolong-task*
  "Find the total sum of all 'value' fields from records where type is 'alpha' AND region is 'north'. Report only the numeric total.")

;;; ====
;;; CAS Helpers
;;; ====

(define (sft-fetch-block hex)
  (fetch-persistent (hex->hash hex)))

(define (sft-decode-payload blk)
  (guard (ex [else #f])
    (read (open-input-string (utf8->string (block-payload blk))))))

;;; ====
;;; Step Chain Walker
;;; ====

(define (sft-skip! msg)
  (display (format "  SKIP: ~a\n" msg))
  (values #f '()))

(define (sft-walk-step-chain config-data last-step-hash)
  ;; Walk backward from last step, return chronological list
  (let loop ([hash last-step-hash] [steps '()])
    (let ([blk (fetch-persistent hash)])
      (cond
        [(not blk)
         (display (format "  WARN: broken chain at ~a\n" (hash->hex hash)))
         (values config-data steps)]
        [(not (eq? (block-tag blk) 'rlm2/step))
         (display (format "  SKIP: incompatible tag ~a\n" (block-tag blk)))
         (values #f '())]
        [else
         (let* ([payload (sft-decode-payload blk)]
                [step-refs (block-refs blk)]
                [prev (if (> (vector-length step-refs) 0)
                          (vector-ref step-refs 0)
                          #f)]
                [augmented (cons (cons 'step-hash (hash->hex hash))
                                payload)]
                [steps* (cons augmented steps)])
           (if prev
               (loop prev steps*)
               (values config-data steps*)))]))))

(define (sft-walk-trajectory traj-hex)
  ;; Returns: (values config-data steps) or (values #f '()) on error
  (let ([traj-blk (sft-fetch-block traj-hex)])
    (cond
      [(not traj-blk)
       (sft-skip! (format "trajectory ~a not found"
                          (substring traj-hex 0 16)))]
      [else
       (let* ([traj-data (sft-decode-payload traj-blk)]
              [refs (block-refs traj-blk)]
              [nrefs (vector-length refs)])
         (cond
           [(not traj-data)
            (sft-skip! "cannot decode trajectory payload")]
           [(not (eq? (rlm2-trajectory-status traj-data) 'completed))
            (sft-skip! (format "status ~a (not completed)"
                               (rlm2-trajectory-status traj-data)))]
           [(< nrefs 2)
            (sft-skip! (format "no step chain (nrefs=~a)" nrefs))]
           [else
            (let* ([config-hex (rlm2-trajectory-config-hash traj-data)]
                   [config-blk (sft-fetch-block config-hex)])
              (cond
                [(not config-blk)
                 (sft-skip! "config block not found")]
                [else
                 (let ([config-data (sft-decode-payload config-blk)]
                       [last-step-hash (vector-ref refs 0)])
                   (sft-walk-step-chain config-data
                                        last-step-hash))]))]))])))

;;; ====
;;; State Reconstruction
;;; ====
;;;
;;; Approximate: we know actions and notes exactly, but not
;;; observation values. The HUD renders env as metadata summaries
;;; (key, type, size) so approximate values are sufficient.

(define (sft-build-initial-state task-text max-fuel input-size)
  ;; Build an approximate initial env matching what rlm2-run creates
  (let* ([env (rlm-env-put
                (rlm-env-put '() 'input "input-hash" 'chunks input-size)
                'task "task-hash" 'text (string-length task-text))])
    (make-rlm2-state task-text '() env '() '() '() '()
                     #f max-fuel 0)))

(define (sft-unquote-key arg)
  ;; Extract symbol from possibly quoted arg: (quote foo) -> foo, bar -> bar
  (if (and (pair? arg) (eq? (car arg) 'quote) (pair? (cdr arg)))
      (cadr arg)
      arg))

(define (sft-apply-action-to-state state action note fuel-used step-hash)
  ;; Apply approximate state changes based on action type.
  (let* ([action-type (if (pair? action) (car action) 'unknown)]
         [env (rlm2-state-env state)]
         ;; Update env based on action
         [env* (case action-type
                 [(store)
                  (if (>= (length action) 3)
                      (rlm-env-put env (sft-unquote-key (cadr action))
                                   "computed" 'result 0)
                      env)]
                 [(eval)
                  (let ([key (string->symbol
                               (format "step-~a-result"
                                       (rlm2-state-step state)))])
                    (rlm-env-put env key "computed" 'result 0))]
                 [(map-chunks)
                  (rlm-env-put env 'map-result "computed" 'result 0)]
                 [else env])]
         ;; Update plan
         [plan* (if (eq? action-type 'plan!)
                    (if (>= (length action) 2) (cdr action) '())
                    (rlm2-state-plan state))]
         ;; Update loaded
         [loaded* (if (eq? action-type 'load)
                      (if (>= (length action) 2)
                          (cons (cadr action) (rlm2-state-loaded state))
                          (rlm2-state-loaded state))
                      (rlm2-state-loaded state))]
         ;; Accumulate notes
         [notes* (if (and (string? note) (> (string-length note) 0))
                     (cons note (rlm2-state-notes state))
                     (rlm2-state-notes state))]
         ;; Accumulate episodic
         [episodic* (cons (cons (rlm2-state-step state) step-hash)
                          (rlm2-state-episodic state))]
         ;; Update journal
         [journal* (if (and (eq? action-type 'journal)
                            (>= (length action) 3))
                       (cons (cons (sft-unquote-key (cadr action))
                                   (caddr action))
                             (rlm2-state-journal state))
                       (rlm2-state-journal state))])
    (make-rlm2-state
      (rlm2-state-task state)
      plan* env* loaded* notes* episodic* journal*
      #f  ; last-result approximate
      (max 0 (- (rlm2-state-fuel state) fuel-used))
      (+ (rlm2-state-step state) 1))))

;;; ====
;;; Training Tuple Generator
;;; ====

(define (sft-format-action action)
  (let ([port (open-output-string)])
    (write action port)
    (get-output-string port)))

(define (sft-make-training-example sys-prompt hud action-str
                                    source step-num traj-hex)
  `((messages . (((role . "system") (content . ,sys-prompt))
                 ((role . "user") (content . ,hud))
                 ((role . "assistant") (content . ,action-str))))
    (source . ,source)
    (step . ,step-num)
    (trajectory . ,traj-hex)))

(define (sft-string-contains? s sub)
  (let ([slen (string-length s)]
        [sublen (string-length sub)])
    (if (> sublen slen)
        #f
        (let loop ([i 0])
          (cond
            [(> (+ i sublen) slen) #f]
            [(string=? (substring s i (+ i sublen)) sub) #t]
            [else (loop (+ i 1))])))))

(define (sft-extract-from-trajectory traj-hex label)
  ;; Extract training examples from one trajectory.
  (display (format "\nExtracting: ~a (~a...)\n" label
                   (substring traj-hex 0 16)))

  (let-values ([(config-data steps) (sft-walk-trajectory traj-hex)])
    (if (not config-data)
        (begin (display "  -> 0 examples (skipped)\n") '())
        (let* ([sys-prompt-base (rlm2-config-system-prompt config-data)]
               [sys-prompt (rlm2-build-system-prompt sys-prompt-base)]
               [max-fuel (rlm2-config-max-fuel config-data)]
               [input-size (if (sft-string-contains? label "200")
                               110000 55000)]
               [initial-state (sft-build-initial-state
                                *oolong-task* max-fuel input-size)])
          (let loop ([remaining steps]
                     [state initial-state]
                     [examples '()])
            (if (null? remaining)
                (let ([result (reverse examples)])
                  (display (format "  -> ~a examples\n" (length result)))
                  result)
                (let* ([step-rec (car remaining)]
                       [action (cdr (assq 'action step-rec))]
                       [note (cdr (assq 'note step-rec))]
                       [fuel-used (cdr (assq 'fuel-used step-rec))]
                       [step-num (cdr (assq 'step step-rec))]
                       [step-hash (cdr (assq 'step-hash step-rec))]
                       ;; Render HUD from current state
                       [hud (rlm2-render-state state *sft-context-budget*)]
                       [action-str (sft-format-action action)]
                       [example (sft-make-training-example
                                  sys-prompt hud action-str
                                  label step-num traj-hex)]
                       [state* (sft-apply-action-to-state
                                 state action note fuel-used step-hash)])
                  (loop (cdr remaining) state*
                        (cons example examples)))))))))

;;; ====
;;; JSONL Writer
;;; ====

(define (sft-write-jsonl! examples output-path)
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
;;; Top-Level Driver
;;; ====

(define (sft-extract-all!)
  (display "SFT Trajectory Extractor\n")
  (display "========================\n")

  (let loop ([trajectories *sft-trajectories*]
             [all-examples '()]
             [n-success 0]
             [n-skip 0])
    (if (null? trajectories)
        (begin
          (display (format "\n--- Summary ---\n"))
          (display (format "Trajectories: ~a extracted, ~a skipped\n"
                           n-success n-skip))
          (display (format "Total examples: ~a\n" (length all-examples)))
          (when (> n-skip 0)
            (display (format "  (v1 trajectories use rlm/step tag -- no action stored)\n")))
          (if (null? all-examples)
              (display "No examples to write.\n")
              (begin
                (sft-write-jsonl! all-examples *sft-output-path*)
                (display (format "Written to: ~a\n" *sft-output-path*))))
          all-examples)

        (let* ([entry (car trajectories)]
               [traj-hex (car entry)]
               [label (cadr entry)]
               [examples (guard (ex [else
                                     (display (format "  ERROR: ~a\n"
                                       (if (message-condition? ex)
                                           (condition-message ex)
                                           ex)))
                                     '()])
                           (sft-extract-from-trajectory traj-hex label))])
          (if (null? examples)
              (loop (cdr trajectories) all-examples
                    n-success (+ n-skip 1))
              (loop (cdr trajectories)
                    (append all-examples examples)
                    (+ n-success 1) n-skip))))))

;;; ====
;;; Main
;;; ====

(sft-extract-all!)
