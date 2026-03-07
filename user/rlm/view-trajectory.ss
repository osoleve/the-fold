;;; user/rlm/view-trajectory.ss — Trajectory Step Viewer
;;;
;;; Replays a completed trajectory from CAS, showing exactly what the
;;; model saw (HUD) and did (action + observation) at each step.
;;;
;;; Usage:
;;;   scheme --script user/rlm/view-trajectory.ss <results-file> [episode-index]
;;;
;;; Examples:
;;;   # View episode 0 (first) from a results file:
;;;   scheme --script user/rlm/view-trajectory.ss user/rlm/bench-results-mastermind-2026-03-01T13:35:25Z.sexp
;;;
;;;   # View episode 3:
;;;   scheme --script user/rlm/view-trajectory.ss user/rlm/bench-results-mastermind-2026-03-01T13:35:25Z.sexp 3
;;;
;;;   # List all episodes in a results file:
;;;   scheme --script user/rlm/view-trajectory.ss user/rlm/bench-results-mastermind-2026-03-01T13:35:25Z.sexp list
;;;
;;;   # View by raw trajectory hash:
;;;   scheme --script user/rlm/view-trajectory.ss --hash 009f7795...

(load "boundary/pipeline/rlm2-drive.ss")

;;; Reuse infrastructure from collect-traces.ss
;;; (CAS helpers, step chain walker, state reconstruction)

(define (tv-fetch-block hex)
  (fetch-persistent (hex->hash hex)))

(define (tv-decode-payload blk)
  (guard (ex [else #f])
    (read (open-input-string (utf8->string (block-payload blk))))))

(define (tv-walk-step-chain last-step-hash)
  (let loop ([hash last-step-hash] [steps '()])
    (let ([blk (fetch-persistent hash)])
      (cond
        [(not blk) steps]
        [(not (eq? (block-tag blk) 'rlm2/step)) steps]
        [else
         (let* ([payload (tv-decode-payload blk)]
                [step-refs (block-refs blk)]
                [prev (if (> (vector-length step-refs) 0)
                          (vector-ref step-refs 0) #f)]
                [augmented (cons (cons 'step-hash (hash->hex hash)) payload)]
                [steps* (cons augmented steps)])
           (if prev (loop prev steps*) steps*))]))))

(define (tv-walk-trajectory traj-hex)
  (let ([traj-blk (tv-fetch-block traj-hex)])
    (cond
      [(not traj-blk)
       (display (format "Trajectory ~a not found in CAS\n" traj-hex))
       (values #f '())]
      [else
       (let* ([traj-data (tv-decode-payload traj-blk)]
              [refs (block-refs traj-blk)]
              [nrefs (vector-length refs)])
         (cond
           [(not traj-data)
            (display "Cannot decode trajectory\n")
            (values #f '())]
           [(< nrefs 1)
            (display "No step chain in trajectory\n")
            (values #f '())]
           [else
            (let* ([config-hex (rlm2-trajectory-config-hash traj-data)]
                   [config-blk (tv-fetch-block config-hex)])
              (if (not config-blk)
                  (begin (display "Config not found\n") (values #f '()))
                  (values (tv-decode-payload config-blk)
                          (tv-walk-step-chain (vector-ref refs 0)))))]))])))

;;; ====
;;; State Reconstruction (from collect-traces.ss)
;;; ====

(define (tv-unquote-key arg)
  (if (and (pair? arg) (eq? (car arg) 'quote) (pair? (cdr arg)))
      (cadr arg) arg))

(define (tv-build-initial-state task-text max-fuel)
  (let ([env (rlm-env-put
               (rlm-env-put '() 'input "input-hash" 'chunks 50000)
               'task "task-hash" 'text (string-length task-text))])
    (make-rlm2-state task-text '() env '() '() '() '()
                     #f max-fuel 0)))

(define (tv-apply-action state action note fuel-used step-hash obs-ok)
  (let* ([action-type (if (pair? action) (car action) 'unknown)]
         [env (rlm2-state-env state)]
         [env* (case action-type
                 [(store) (if (>= (length action) 3)
                              (rlm-env-put env (tv-unquote-key (cadr action))
                                           "computed" 'result 0)
                              env)]
                 [(eval) (rlm-env-put env 'last-result "computed" 'result 0)]
                 [else env])]
         [plan* (if (eq? action-type 'plan!)
                    (if (>= (length action) 2) (cdr action) '())
                    (rlm2-state-plan state))]
         [loaded* (if (eq? action-type 'load)
                      (if (>= (length action) 2)
                          (cons (tv-unquote-key (cadr action))
                                (rlm2-state-loaded state))
                          (rlm2-state-loaded state))
                      (rlm2-state-loaded state))]
         [notes* (if (and (string? note) (> (string-length note) 0))
                     (cons note (rlm2-state-notes state))
                     (rlm2-state-notes state))]
         [episodic* (cons (list (rlm2-state-step state) step-hash action-type obs-ok)
                          (rlm2-state-episodic state))]
         [journal* (if (and (eq? action-type 'journal)
                            (>= (length action) 3))
                       (cons (cons (tv-unquote-key (cadr action))
                                   (caddr action))
                             (rlm2-state-journal state))
                       (rlm2-state-journal state))])
    (make-rlm2-state
      (rlm2-state-task state) plan* env* loaded* notes*
      episodic* journal* #f
      (max 0 (- (rlm2-state-fuel state) fuel-used))
      (+ (rlm2-state-step state) 1))))

;;; ====
;;; Display
;;; ====

(define (tv-bar ch n)
  (make-string n ch))

(define (tv-section header text)
  (let ([pad (tv-bar #\- (max 0 (- 50 (string-length header) 4)))])
    (format "~%-- ~a ~a~%~a~%" header pad text)))

(define (tv-display-step step-rec state config-data step-num total)
  (let* ([action (cdr (assq 'action step-rec))]
         [obs-ok (cdr (assq 'observation-ok step-rec))]
         [obs-val (cdr (assq 'observation-value step-rec))]
         [note (cdr (assq 'note step-rec))]
         [fuel-used (cdr (assq 'fuel-used step-rec))]
         [action-type (if (pair? action) (car action) 'unknown)]
         [fuel (rlm2-state-fuel state)]
         [max-fuel (rlm2-config-max-fuel config-data)]
         ;; Render HUD as the model would see it
         [hud (rlm2-render-state state 5000)])

    (display (format "~%~a~% STEP ~a/~a  |  ~a  |  fuel: ~a/~a  |  ok: ~a~%~a~%"
      (tv-bar #\= 55)
      step-num (- total 1) action-type fuel max-fuel obs-ok
      (tv-bar #\= 55)))

    (display (tv-section "HUD (model input)" hud))
    (display (tv-section "ACTION" (format "~s" action)))
    (display (tv-section "OBSERVATION"
      (if (string? obs-val) obs-val (format "~a" obs-val))))
    (display (tv-section "REFLECT NOTE" note))

    (flush-output-port)))

;;; ====
;;; Results File Reader
;;; ====

(define (tv-read-results path)
  (guard (ex [else
              (display (format "Cannot read ~a: ~a\n" path
                (if (message-condition? ex) (condition-message ex) ex)))
              #f])
    (with-input-from-file path read)))

(define (tv-extract-episodes results-data)
  ;; Returns list of (label seed status reward trajectory-hash)
  (let* ([data (if (and (pair? results-data) (symbol? (car results-data)))
                   (cdr results-data) results-data)]
         [eps-raw (assq 'episodes data)])
    (if (not eps-raw) '()
        ;; episodes value is ((ep1 ep2 ...)) — unwrap
        (let loop ([eps (cadr eps-raw)] [i 0] [acc '()])
          (if (or (null? eps) (not (pair? eps))) (reverse acc)
              (let* ([ep (car eps)]
                     [label (let ([l (assq 'label ep)]) (if l (cdr l) (format "ep-~a" i)))]
                     [seed (let ([s (assq 'seed ep)]) (if s (cdr s) 0))]
                     [status (let ([s (assq 'game-status ep)]) (if s (cdr s) "unknown"))]
                     [reward (let ([r (assq 'reward ep)]) (if r (cdr r) 0))]
                     [traj (let ([t (assq 'trajectory ep)]) (if t (cdr t) #f))])
                (loop (cdr eps) (+ i 1)
                      (cons (list i label seed status reward traj) acc))))))))

(define (tv-list-episodes path)
  (let ([data (tv-read-results path)])
    (when data
      (let ([episodes (tv-extract-episodes data)])
        (display (format "~%Episodes in ~a:~%" path))
        (display (format "~a~%" (tv-bar #\- 60)))
        (for-each
          (lambda (ep)
            (let ([idx (car ep)] [label (cadr ep)] [seed (caddr ep)]
                  [status (cadddr ep)] [reward (car (cddddr ep))]
                  [traj (cadr (cddddr ep))])
              (display (format "  [~a] ~a  seed=~a  ~a  reward=~a  ~a~%"
                idx label seed status reward
                (if traj
                    (string-append "traj=" (substring traj 0 (min 16 (string-length traj))) "...")
                    "no-trajectory")))))
          episodes)
        (display (format "~%Use: scheme --script user/rlm/view-trajectory.ss ~a <index>~%" path))))))

(define (tv-view-episode traj-hex label)
  (display (format "~%Viewing trajectory: ~a~%" label))
  (display (format "Hash: ~a~%" traj-hex))
  (display (format "~a~%" (tv-bar #\= 55)))

  (let-values ([(config-data steps) (tv-walk-trajectory traj-hex)])
    (if (not config-data)
        (display "Failed to load trajectory\n")
        (let* ([sys-prompt (rlm2-config-system-prompt config-data)]
               [max-fuel (rlm2-config-max-fuel config-data)]
               [task-text (let ([t (assq 'task (car steps))])
                            (if t "task" "task"))]
               [state (tv-build-initial-state task-text max-fuel)]
               [total (length steps)])

          (display (format "Steps: ~a  |  Max fuel: ~a~%" total max-fuel))
          (display (format "System prompt: ~a chars~%"
            (string-length (rlm2-build-system-prompt sys-prompt))))

          (let loop ([remaining steps] [state state] [i 0])
            (when (pair? remaining)
              (let* ([step-rec (car remaining)]
                     [action (cdr (assq 'action step-rec))]
                     [obs-ok (cdr (assq 'observation-ok step-rec))]
                     [note (cdr (assq 'note step-rec))]
                     [fuel-used (cdr (assq 'fuel-used step-rec))]
                     [step-hash (cdr (assq 'step-hash step-rec))])
                (tv-display-step step-rec state config-data i total)
                (let ([state* (tv-apply-action state action note
                                fuel-used step-hash obs-ok)])
                  (loop (cdr remaining) state* (+ i 1))))))

          (display (format "~%~a~% END OF TRAJECTORY~%~a~%"
            (tv-bar #\= 55) (tv-bar #\= 55)))))))

;;; ====
;;; Main
;;; ====

(let ([args (cdr (command-line))])
  (cond
    [(null? args)
     (display "Usage:\n")
     (display "  scheme --script user/rlm/view-trajectory.ss <results-file> [index|list]\n")
     (display "  scheme --script user/rlm/view-trajectory.ss --hash <trajectory-hash>\n")]

    ;; Direct trajectory hash
    [(and (>= (length args) 2)
          (string=? (car args) "--hash"))
     (tv-view-episode (cadr args) "direct")]

    ;; Results file
    [(null? (cdr args))
     ;; No index — list episodes
     (tv-list-episodes (car args))]

    [(string=? (cadr args) "list")
     (tv-list-episodes (car args))]

    [else
     (let* ([path (car args)]
            [idx (string->number (cadr args))]
            [data (tv-read-results path)])
       (when data
         (let ([episodes (tv-extract-episodes data)])
           (cond
             [(not idx)
              (display (format "Invalid index: ~a\n" (cadr args)))]
             [(>= idx (length episodes))
              (display (format "Episode ~a not found (max: ~a)\n" idx (- (length episodes) 1)))]
             [else
              (let* ([ep (list-ref episodes idx)]
                     [label (cadr ep)]
                     [traj (cadr (cddddr ep))])
                (if (not traj)
                    (display (format "Episode ~a has no trajectory\n" idx))
                    (tv-view-episode traj label)))]))))]))
