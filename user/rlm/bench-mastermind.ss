;;; user/rlm/bench-mastermind.ss — Mastermind Episode Runner
;;;
;;; Runs code-breaking episodes via RLM v2. The agent must deduce a
;;; secret 4-color code by guessing and interpreting (exact, misplaced)
;;; feedback. Tests persistent state tracking — without (think ...),
;;; early feedback scrolls out of the 3-turn window.
;;;
;;; Run: RLM_INTEGRATION=1 scheme --script user/rlm/bench-mastermind.ss
;;; Env: RLM_MODEL, RLM_PORT, RLM_HOST, RLM_EPISODES (default 10)

(unless (top-level-bound? 'rlm2-run)
  (load "boundary/pipeline/rlm2-drive.ss"))

;;; ====
;;; System prompt
;;; ====

(define *mastermind-system-prompt*
  (string-append
    "## Mastermind\n\n"
    "You are playing Mastermind — a code-breaking deduction game.\n"
    "A secret code of 4 colors has been chosen (colors may repeat).\n"
    "You have 10 guesses to deduce the exact code.\n\n"
    "### Your Tools\n\n"
    "Use `(eval ...)` to interact with the game:\n"
    "- `(eval (mastermind-guess! '(c1 c2 c3 c4)))` — Submit a guess of 4 colors\n"
    "- `(eval (mastermind-done?))` — Check if the game is over\n"
    "- `(submit (mastermind-result))` — Submit your result when the game ends\n\n"
    "### Feedback\n\n"
    "After each guess you receive:\n"
    "- **exact** — Number of colors in the correct position\n"
    "- **misplaced** — Number of correct colors in wrong positions\n"
    "Colors are NOT double-counted between exact and misplaced.\n\n"
    "### Available Colors\n\n"
    "`red`, `blue`, `green`, `yellow`, `white`, `orange`\n\n"
    "### Strategy\n\n"
    "**CRITICAL:** Colors CAN repeat. The secret might be (blue blue red blue).\n"
    "Never assume all 4 positions have different colors.\n\n"
    "**CRITICAL:** When exact=N and misplaced=0, you know N colors are in the "
    "right position and the OTHER colors are NOT in the code at all. But you "
    "don't know WHICH N are the exact ones. There are C(4,N) possible pairs. "
    "You MUST consider all possibilities, not pick one.\n\n"
    "You MUST use `(think ...)` after EVERY guess to record:\n"
    "1. The guess you made and the feedback received\n"
    "2. ALL possible interpretations of the feedback\n"
    "3. Which interpretations are eliminated by prior guesses\n\n"
    "Without tracking feedback in (think ...), earlier results will scroll "
    "out of your context window and you'll lose critical deduction data.\n\n"
    "**Approach:** Start with information-gathering guesses. On each guess, "
    "enumerate all possible exact-position sets consistent with the feedback, "
    "then cross-reference with prior data to eliminate impossibilities.\n\n"
    "**ONE ACTION PER TURN.** Do NOT combine eval and think in a begin block. "
    "After each `(eval (mastermind-guess! ...))`, you will see the feedback in "
    "your observation. Use `(think ...)` on the NEXT turn to analyze it. "
    "This prevents truncation and ensures your analysis uses real feedback.\n\n"
    "### Submission\n\n"
    "When the game ends (won or out of guesses), call:\n"
    "`(submit (mastermind-result))`\n\n"
    "This returns `(status reward)` — your final score.\n"))

;;; ====
;;; Prompt Fitting
;;; ====

(define *mastermind-fit-instruction*
  (string-append
    "You are about to play Mastermind. Below are the full game rules.\n\n"
    "Restate these rules as a concise briefing TO YOURSELF — in your own words, "
    "in whatever format helps you play well. Include:\n"
    "- Exact command syntax (eval, submit)\n"
    "- How feedback works (exact vs misplaced, no double-counting)\n"
    "- Why you MUST use (think ...) to track results\n"
    "- Your deduction strategy\n\n"
    "Do NOT add any rules that aren't listed. Do NOT embellish. "
    "Be precise and concise — this will be your only reference during play.\n\n"
    "---\n\n"))

(define (mastermind-fit-prompt provider)
  (display "Fitting prompt...\n")
  (flush-output-port)
  (let* ([messages (list
                     (rlm2-make-msg "user"
                       (string-append *mastermind-fit-instruction*
                                      *mastermind-system-prompt*)))]
         [response (rlm-chat provider messages 1024 0.3)])
    (cond
      [(rlm-chat-ok? response)
       (let ([fitted (rlm-chat-text response)])
         (display (format "  Fitted prompt: ~a chars (original: ~a)\n"
                          (string-length fitted)
                          (string-length *mastermind-system-prompt*)))
         (flush-output-port)
         fitted)]
      [else
       (display (format "  Fit failed (~a), using original prompt\n"
                        (rlm-chat-error-msg response)))
       (flush-output-port)
       *mastermind-system-prompt*])))

(define (mastermind-load-system-prompt)
  (let ([prompt-file (getenv "MASTERMIND_PROMPT")])
    (if (and prompt-file (file-exists? prompt-file))
        (let ([text (call-with-input-file prompt-file
                      (lambda (p) (get-string-all p)))])
          (display (format "Using fitted prompt from ~a (~a chars)\n" prompt-file (string-length text)))
          (flush-output-port)
          text)
        (begin
          (display (format "Using raw system prompt (~a chars)\n" (string-length *mastermind-system-prompt*)))
          (flush-output-port)
          *mastermind-system-prompt*))))

;;; ====
;;; Runner
;;; ====

(define (run-mastermind-episode provider system-prompt seed i total)
  (let* ([max-steps 30]
         [max-fuel 200000]
         [label (format "mastermind-~a" i)]
         [task "Break the secret code! Guess 4 colors and use the feedback to deduce the answer. Use (eval (mastermind-guess! '(c1 c2 c3 c4))) to guess. After each guess, use (think ...) on the NEXT turn to analyze the feedback — one action per turn, never combine eval+think in begin. When the game ends, (submit (mastermind-result))."]
         [setup `((eval (load "lattice/pipeline/mastermind-session.ss"))
                  (eval (mastermind-init! ,seed))
                  ;; Demo: try an invalid color so the model learns the syntax + valid colors
                  (eval (mastermind-guess! '(purple purple purple purple)))
                  ;; Record the lessons in the HUD
                  (journal 'demo "Tried (mastermind-guess! '(purple purple purple purple)) — REJECTED. purple is not a valid color. Valid colors: red blue green yellow white orange")
                  (journal 'warning "COLORS CAN REPEAT! The secret might be (orange orange green orange). When narrowing the last position, ALWAYS consider colors already used elsewhere.")
                  (plan! ()))])

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
                                          5000  ; context-budget
                                          #f    ; no verifier
                                          512)  ; max-tokens
                                        (list '()     ; few-shot (empty)
                                              setup))])
                          (rlm2-run config task ""))))])
        (let* ([status (rlm2-run-result-status result)]
               [output-str (format "~a" (rlm2-run-result-output result))]
               [traj (rlm2-run-result-trajectory-hash result)]
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

(define (run-mastermind-suite)
  (let* ([model-id (or (getenv "RLM_MODEL")
                       "/models/Qwen3.5-27B-NVFP4")]
         [host (or (getenv "RLM_HOST") "localhost")]
         [port (or (and (getenv "RLM_PORT")
                        (string->number (getenv "RLM_PORT")))
                   8000)]
         [n-episodes (or (and (getenv "RLM_EPISODES")
                              (string->number (getenv "RLM_EPISODES")))
                         10)]
         [seed-offset (or (and (getenv "RLM_SEED_OFFSET")
                              (string->number (getenv "RLM_SEED_OFFSET")))
                         0)]
         [provider (make-rlm-provider
                     (format "http://~a:~a/v1/chat/completions" host port)
                     model-id #f 'openai)]
         [seeds (let loop ([i 0] [acc '()])
                  (if (= i n-episodes) (reverse acc)
                      (loop (+ i 1) (cons (+ seed-offset (* (+ i 1) 6173)) acc))))])

    (display (format "Mastermind — Episode Runner\n"))
    (display (format "===========================\n"))
    (display (format "Model: ~a | Host: ~a:~a | Episodes: ~a\n\n"
                     model-id host port n-episodes))
    (flush-output-port)

    (let ([system-prompt (mastermind-load-system-prompt)])

    (let loop ([remaining seeds] [i 0] [results '()])
      (if (null? remaining)
          (let* ([results (reverse results)]
                 [n-won (length (filter (lambda (r)
                                          (let ([gs (assq 'game-status r)])
                                            (and gs (string=? (cdr gs) "won"))))
                                        results))]
                 [n-lost (length (filter (lambda (r)
                                          (let ([gs (assq 'game-status r)])
                                            (and gs (string=? (cdr gs) "lost"))))
                                        results))]
                 [n-other (- (length results) n-won n-lost)]
                 [total-reward (apply + (map (lambda (r)
                                               (let ([rw (assq 'reward r)])
                                                 (if rw (cdr rw) 0)))
                                             results))]
                 [total-ms (apply + (map (lambda (r)
                                           (let ([t (assq 'time-ms r)])
                                             (if t (cdr t) 0)))
                                         results))]
                 [results-file (format "user/rlm/bench-results-mastermind-~a.sexp"
                                       (rlm2-current-iso8601))])

            (display (format "\n\n========================================\n"))
            (display (format "RESULTS: ~a episodes\n" n-episodes))
            (display (format "  Won: ~a | Lost: ~a | Other: ~a\n"
                             n-won n-lost n-other))
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
                                 (mode "mastermind")
                                 (timestamp ,(rlm2-current-iso8601))
                                 (fitted-prompt ,system-prompt)
                                 (episodes ,results))
                              port))
              'replace)
            (display (format "Results saved to ~a\n" results-file)))

          (let ([result (run-mastermind-episode provider system-prompt (car remaining) i n-episodes)])
            (loop (cdr remaining) (+ i 1) (cons result results))))))))

;; MASTERMIND_FIT=1 — fit prompt and save to file, then exit
;; RLM_INTEGRATION=1 — run episodes
(when (getenv "MASTERMIND_FIT")
  (let* ([host (or (getenv "RLM_HOST") "localhost")]
         [port (or (and (getenv "RLM_PORT") (string->number (getenv "RLM_PORT"))) 8000)]
         [model-id (or (getenv "RLM_MODEL") "/models/Qwen3.5-27B-NVFP4")]
         [provider (make-rlm-provider
                     (format "http://~a:~a/v1/chat/completions" host port)
                     model-id #f 'openai)]
         [fitted (mastermind-fit-prompt provider)]
         [out-file (or (getenv "MASTERMIND_PROMPT") "user/rlm/mastermind-prompt.txt")])
    (call-with-output-file out-file
      (lambda (p) (put-string p fitted))
      'replace)
    (display (format "Saved to ~a\n" out-file))))

(when (getenv "RLM_INTEGRATION")
  (run-mastermind-suite))
