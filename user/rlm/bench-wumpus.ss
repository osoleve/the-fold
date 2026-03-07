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
    "- You have 3 arrows and 25 moves\n"
    "- Moving into the wumpus room: you die (eaten)\n"
    "- Moving into a pit room: you die (fell)\n"
    "- Shooting: name 1-3 rooms forming a path from your room. "
    "If the wumpus is in any room along the path, you win!\n"
    "- If you miss, the wumpus may move to an adjacent room\n"
    "- Invalid moves (non-adjacent rooms) waste a move\n\n"
    "### Strategy\n\n"
    "**CRITICAL SAFETY RULE:** NEVER move into an unvisited room when you sense "
    "stench or draft. Entering a stench-candidate room kills you. "
    "Use arrows to probe — arrows are your safe information-gathering tool.\n\n"
    "**Triangulation procedure:**\n"
    "1. Sense stench → mark ALL current tunnels as wumpus candidates.\n"
    "2. Retreat to a VISITED (safe) room. If no stench there, eliminate that "
    "room's non-overlapping tunnels from suspects.\n"
    "3. Gather stench data from 2-3 rooms. Intersect candidate sets to narrow "
    "to 1-2 suspects.\n"
    "4. Shoot the suspects. Do NOT walk to them.\n\n"
    "**Pit avoidance:** Same rule — never enter unvisited rooms when sensing draft. "
    "Retreat and approach from a different direction.\n\n"
    "Use `(think ...)` to track candidate sets. "
    "The `(visited ...)` list shows where you've safely been.\n\n"
    "### Submission\n\n"
    "When the game ends (won, eaten, fell, or out of moves), call:\n"
    "`(submit (wumpus-result))`\n\n"
    "This returns `(status reward)` — your final score.\n"))

;;; ====
;;; Prompt Fitting
;;; ====
;;; Run separately: WUMPUS_FIT=1 RLM_INTEGRATION=1 scheme --script user/rlm/bench-wumpus.ss
;;; Saves fitted prompt to user/rlm/wumpus-prompt.txt. The episode runner
;;; reads WUMPUS_PROMPT=<file> to use it — same fitted prompt across all boxes.

(define *wumpus-fit-instruction*
  (string-append
    "You are about to play Hunt the Wumpus. Below are the full game rules.\n\n"
    "Restate these rules as a concise briefing TO YOURSELF — in your own words, "
    "in whatever format helps you play well. Include:\n"
    "- Exact command syntax (eval, submit)\n"
    "- The triangulation strategy for finding the wumpus\n"
    "- How to avoid pits\n"
    "- What each sense means\n\n"
    "Do NOT add any rules that aren't listed. Do NOT embellish. "
    "Be precise and concise — this will be your only reference during play.\n\n"
    "---\n\n"))

(define (wumpus-fit-prompt provider)
  (display "Fitting prompt...\n")
  (flush-output-port)
  (let* ([messages (list
                     (rlm2-make-msg "user"
                       (string-append *wumpus-fit-instruction*
                                      *wumpus-system-prompt*)))]
         [response (rlm-chat provider messages 1024 0.3)])
    (cond
      [(rlm-chat-ok? response)
       (let ([fitted (rlm-chat-text response)])
         (display (format "  Fitted prompt: ~a chars (original: ~a)\n"
                          (string-length fitted)
                          (string-length *wumpus-system-prompt*)))
         (flush-output-port)
         fitted)]
      [else
       (display (format "  Fit failed (~a), using original prompt\n"
                        (rlm-chat-error-msg response)))
       (flush-output-port)
       *wumpus-system-prompt*])))

;;; Load system prompt: WUMPUS_PROMPT file > raw *wumpus-system-prompt*
(define (wumpus-load-system-prompt)
  (let ([prompt-file (getenv "WUMPUS_PROMPT")])
    (if (and prompt-file (file-exists? prompt-file))
        (let ([text (call-with-input-file prompt-file
                      (lambda (p) (get-string-all p)))])
          (display (format "Using fitted prompt from ~a (~a chars)\n" prompt-file (string-length text)))
          (flush-output-port)
          text)
        (begin
          (display (format "Using raw system prompt (~a chars)\n" (string-length *wumpus-system-prompt*)))
          (flush-output-port)
          *wumpus-system-prompt*))))

;;; ====
;;; Runner
;;; ====

(define (run-wumpus-episode provider system-prompt seed i total)
  (let* ([max-steps 60]
         [max-fuel 120000]
         [label (format "wumpus-~a" i)]
         [task "Hunt the wumpus! Navigate the caves, sense danger, and shoot the wumpus. Use (eval (wumpus-look!)) to see your surroundings, (eval (wumpus-move! 'room)) to move, and (eval (wumpus-shoot! '(rooms...))) to fire. When the game ends, (submit (wumpus-result))."]
         [setup `((eval (load "lattice/pipeline/wumpus-session.ss"))
                  (eval (wumpus-init! ,seed))
                  (plan! ((STENCH . NEVER-enter-unvisited-rooms--use-arrows-to-probe)
                          (DRAFT . NEVER-enter-unvisited-rooms--retreat-and-reroute)
                          (triangulate . sense-from-2-3-safe-rooms-then-shoot-intersection)
                          (track-candidates . use-think-after-each-observation))))])

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
                                          provider system-prompt
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
               [output-str (format "~a" (rlm2-run-result-output result))]
               [traj (rlm2-run-result-trajectory-hash result)]
               ;; Parse submitted result: expect "(status reward)" string
               [submitted? (eq? status 'completed)]
               [parsed (and submitted?
                            (guard (ex [else #f])
                              (read (open-input-string output-str))))]
               [game-status (if (and (pair? parsed) (symbol? (car parsed)))
                                (car parsed) 'unknown)]
               [reward (if (and (pair? parsed) (pair? (cdr parsed))
                                (number? (cadr parsed)))
                           (cadr parsed) 0)])
          (display (format "  RLM: ~a | Game: ~a | Reward: ~a | Time: ~a ms\n"
                           status game-status reward ms))
          (display (format "  Output: ~a\n"
                           (if (> (string-length output-str) 200)
                               (string-append (substring output-str 0 200) "...")
                               output-str)))
          (flush-output-port)

          `((label . ,label)
            (seed . ,seed)
            (rlm-status . ,(symbol->string status))
            (game-status . ,(symbol->string game-status))
            (reward . ,reward)
            (time-ms . ,ms)
            (output . ,output-str)
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

    (let ([system-prompt (wumpus-load-system-prompt)])

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
                                 (fitted-prompt ,system-prompt)
                                 (episodes ,results))
                              port))
              'replace)
            (display (format "Results saved to ~a\n" results-file)))

          ;; Run next episode
          (let ([result (run-wumpus-episode provider system-prompt (car remaining) i n-episodes)])
            (loop (cdr remaining) (+ i 1) (cons result results))))))))

;; WUMPUS_FIT=1 — fit prompt and save to file, then exit
;; RLM_INTEGRATION=1 — run episodes (optionally with WUMPUS_PROMPT=<file>)
(when (getenv "WUMPUS_FIT")
  (let* ([host (or (getenv "RLM_HOST") "localhost")]
         [port (or (and (getenv "RLM_PORT") (string->number (getenv "RLM_PORT"))) 8000)]
         [model-id (or (getenv "RLM_MODEL") "/models/Qwen3.5-27B-NVFP4")]
         [provider (make-rlm-provider
                     (format "http://~a:~a/v1/chat/completions" host port)
                     model-id #f 'openai)]
         [fitted (wumpus-fit-prompt provider)]
         [out-file (or (getenv "WUMPUS_PROMPT") "user/rlm/wumpus-prompt.txt")])
    (call-with-output-file out-file
      (lambda (p) (put-string p fitted))
      'replace)
    (display (format "Saved to ~a\n" out-file))))

(when (getenv "RLM_INTEGRATION")
  (run-wumpus-suite))
