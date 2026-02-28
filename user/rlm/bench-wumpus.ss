;;; user/rlm/bench-wumpus.ss — Hunt the Wumpus Episode Runner
;;;
;;; Runs game episodes via RLM v2. The agent navigates the lattice cave
;;; system, avoiding pits and hunting the wumpus by eval'ing game commands.
;;;
;;; Run: RLM_INTEGRATION=1 scheme --script user/rlm/bench-wumpus.ss
;;; Env: RLM_MODEL, RLM_PORT, RLM_HOST, RLM_EPISODES (default 10)

(unless (top-level-bound? 'rlm2-run)
  (load "boundary/pipeline/rlm2-drive.ss"))

;;; ====
;;; System prompt — game rules for the agent
;;; ====

(define *wumpus-system-prompt*
  (string-append
    "## Hunt the Wumpus\n\n"
    "You are hunting a wumpus in a cave system. The caves are the skills of "
    "The Fold's lattice — real rooms with real names. Tunnels connect skills "
    "that share dependencies.\n\n"
    "### Your Tools\n\n"
    "Use `(eval ...)` to interact with the game:\n"
    "- `(eval (wumpus-move! 'room-name))` — Move through a tunnel to an adjacent room\n"
    "- `(eval (wumpus-shoot! '(room1 room2 ...)))` — Fire an arrow through 1-3 adjacent rooms\n"
    "- `(eval (wumpus-look!))` — Re-examine your surroundings (costs 1 move)\n"
    "- `(eval (wumpus-done?))` — Check if the game is over\n"
    "- `(submit (wumpus-result))` — Submit your result when the game ends\n\n"
    "### Senses\n\n"
    "Each observation shows your room, tunnels, and senses:\n"
    "- **stench** — The wumpus is in an adjacent room\n"
    "- **draft** — A bottomless pit is in an adjacent room\n"
    "- No senses means adjacent rooms are safe\n\n"
    "### Rules\n\n"
    "- You have 3 arrows and 30 moves\n"
    "- Moving into the wumpus room: you die (eaten)\n"
    "- Moving into a pit room: you die (fell)\n"
    "- Shooting: name 1-3 rooms forming a path from your room. "
    "If the wumpus is in any room along the path, you win!\n"
    "- If you miss, the wumpus may move to an adjacent room\n"
    "- Invalid moves (non-adjacent rooms) waste a move\n\n"
    "### Strategy\n\n"
    "- Use `(think ...)` to reason about wumpus location based on senses\n"
    "- Use `(journal ...)` and `(plan! ...)` to track visited rooms and deductions\n"
    "- Navigate toward stench, then shoot into the suspected room\n"
    "- Avoid rooms adjacent to draft signals\n\n"
    "### Submission\n\n"
    "When the game ends (won, eaten, fell, or out of moves), call:\n"
    "`(submit (wumpus-result))`\n\n"
    "This returns `(status reward)` — your final score.\n"))

;;; ====
;;; Runner
;;; ====

(define (run-wumpus-episode provider seed i total)
  (let* ([max-steps 60]
         [max-fuel 120000]
         [label (format "wumpus-~a" i)]
         [task "Hunt the wumpus! Navigate the caves, sense danger, and shoot the wumpus. Use (eval (wumpus-look!)) to see your surroundings, (eval (wumpus-move! 'room)) to move, and (eval (wumpus-shoot! '(rooms...))) to fire. When the game ends, (submit (wumpus-result))."]
         [setup `((eval (load "lattice/pipeline/wumpus-session.ss"))
                  (eval (wumpus-init! ,seed)))])

    (display (format "\n=== [~a/~a] ~a (seed ~a) ===\n" (+ i 1) total label seed))
    (flush-output-port)

    (guard (ex [else
                (display (format "  ERROR: ~a\n"
                          (if (message-condition? ex)
                              (condition-message ex)
                              ex)))
                `((label . ,label)
                  (seed . ,seed)
                  (status . "error")
                  (reward . 0)
                  (trajectory . #f))])
      (let-values ([(result ms)
                    (wall-clock-ms
                      (lambda ()
                        (let ([config (append
                                        (make-rlm2-config
                                          provider *wumpus-system-prompt*
                                          max-steps max-fuel
                                          2000  ; chunk-size
                                          1     ; max-depth
                                          3     ; loop-window
                                          12000 ; context-budget (larger for game)
                                          #f    ; no verifier
                                          1024) ; max-tokens
                                        (list '()     ; few-shot (empty)
                                              setup))])
                          (rlm2-run config task ""))))])
        (let* ([status (rlm2-run-result-status result)]
               [output (format "~a" (rlm2-run-result-output result))]
               [traj (rlm2-run-result-trajectory-hash result)]
               ;; Parse submitted result: expect (game-status reward)
               [submitted? (eq? status 'completed)]
               [game-status (and submitted?
                                 (let ([out (rlm2-run-result-output result)])
                                   (if (and (pair? out) (symbol? (car out)))
                                       (car out)
                                       'unknown)))]
               [reward (and submitted?
                            (let ([out (rlm2-run-result-output result)])
                              (if (and (pair? out) (pair? (cdr out))
                                       (number? (cadr out)))
                                  (cadr out)
                                  0)))])
          (display (format "  RLM: ~a | Game: ~a | Reward: ~a | Time: ~a ms\n"
                           status
                           (or game-status "n/a")
                           (or reward 0)
                           ms))
          (display (format "  Output: ~a\n"
                           (if (> (string-length output) 200)
                               (string-append (substring output 0 200) "...")
                               output)))
          (flush-output-port)

          `((label . ,label)
            (seed . ,seed)
            (rlm-status . ,(symbol->string status))
            (game-status . ,(if game-status (symbol->string game-status) "n/a"))
            (reward . ,(or reward 0))
            (time-ms . ,ms)
            (output . ,output)
            (trajectory . ,traj)))))))

;;; wall-clock-ms : (-> a) -> (values a Nat)
(define (wall-clock-ms thunk)
  (let* ([t0 (current-time)]
         [result (thunk)]
         [t1 (current-time)]
         [ms (+ (* 1000 (- (time-second t1) (time-second t0)))
                (quotient (- (time-nanosecond t1) (time-nanosecond t0))
                          1000000))])
    (values result ms)))

;;; ====
;;; Main
;;; ====

(define (run-wumpus-suite)
  (let* ([model-id (or (getenv "RLM_MODEL")
                       "/models/Qwen3.5-27B-NVFP4")]
         [host (or (getenv "RLM_HOST") "localhost")]
         [port (or (and (getenv "RLM_PORT")
                        (string->number (getenv "RLM_PORT")))
                   8000)]
         [n-episodes (or (and (getenv "RLM_EPISODES")
                              (string->number (getenv "RLM_EPISODES")))
                         10)]
         [provider (make-rlm-provider
                     (format "http://~a:~a/v1/chat/completions" host port)
                     model-id #f 'openai)]
         [seeds (let loop ([i 0] [acc '()])
                  (if (= i n-episodes) (reverse acc)
                      (loop (+ i 1) (cons (* (+ i 1) 7919) acc))))])

    (display (format "Hunt the Wumpus — Episode Runner\n"))
    (display (format "=================================\n"))
    (display (format "Model: ~a | Host: ~a:~a | Episodes: ~a\n\n"
                     model-id host port n-episodes))
    (flush-output-port)

    (let loop ([remaining seeds] [i 0] [results '()])
      (if (null? remaining)
          ;; Done — report
          (let* ([results (reverse results)]
                 [n-won (length (filter (lambda (r)
                                          (let ([gs (assq 'game-status r)])
                                            (and gs (string=? (cdr gs) "won"))))
                                        results))]
                 [n-eaten (length (filter (lambda (r)
                                           (let ([gs (assq 'game-status r)])
                                             (and gs (string=? (cdr gs) "eaten"))))
                                         results))]
                 [n-fell (length (filter (lambda (r)
                                          (let ([gs (assq 'game-status r)])
                                            (and gs (string=? (cdr gs) "fell"))))
                                        results))]
                 [n-timeout (length (filter (lambda (r)
                                             (let ([gs (assq 'game-status r)])
                                               (and gs (string=? (cdr gs) "timeout"))))
                                           results))]
                 [total-reward (apply + (map (lambda (r)
                                               (let ([rw (assq 'reward r)])
                                                 (if rw (cdr rw) 0)))
                                             results))]
                 [total-ms (apply + (map (lambda (r)
                                           (let ([t (assq 'time-ms r)])
                                             (if t (cdr t) 0)))
                                         results))]
                 [results-file (format "user/rlm/bench-results-wumpus-~a.sexp"
                                       (rlm2-current-iso8601))])

            (display (format "\n\n========================================\n"))
            (display (format "RESULTS: ~a episodes\n" n-episodes))
            (display (format "  Won: ~a | Eaten: ~a | Fell: ~a | Timeout: ~a\n"
                             n-won n-eaten n-fell n-timeout))
            (display (format "  Total reward: ~a | Avg: ~a\n"
                             total-reward
                             (if (> n-episodes 0)
                                 (/ (round (* 100 (/ total-reward n-episodes))) 100.0)
                                 0)))
            (display (format "  Total time: ~a ms (~a ms avg)\n"
                             total-ms
                             (if (> n-episodes 0) (quotient total-ms n-episodes) 0)))

            (call-with-output-file results-file
              (lambda (port)
                (pretty-print `(benchmark-results
                                 (model ,model-id)
                                 (mode "wumpus")
                                 (timestamp ,(rlm2-current-iso8601))
                                 (episodes ,results))
                              port))
              'replace)
            (display (format "Results saved to ~a\n" results-file)))

          ;; Run next episode
          (let ([result (run-wumpus-episode provider (car remaining) i n-episodes)])
            (loop (cdr remaining) (+ i 1) (cons result results)))))))

;; Auto-run when invoked as script
(when (getenv "RLM_INTEGRATION")
  (run-wumpus-suite))
