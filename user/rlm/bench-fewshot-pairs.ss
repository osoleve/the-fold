;;; user/rlm/bench-fewshot-pairs.ss -- Few-shot OOLONG-Pairs benchmark
;;;
;;; Tests whether Nemotron can discover and use lattice tools for the
;;; Pairs task. Few-shot examples teach action format (5 aggregation)
;;; and search→load→use workflow (5 diverse domains: optimization,
;;; game theory, graphics, refactoring, strings). No constraint solver
;;; hints — the model must discover those independently.
;;;
;;; Run: scheme --script user/rlm/bench-fewshot-pairs.ss

(load "boundary/pipeline/rlm2-drive.ss")
(load "user/rlm/bench.ss")

;;; ====
;;; Few-Shot Examples
;;; ====
;;;
;;; Two groups:
;;;   A. 5 aggregation examples (action format basics — peek, think, map-chunks, submit)
;;;   B. 5 lattice discovery examples (search→load→use across unrelated domains)
;;; The lattice examples teach the *workflow* of discovering tools, not the tools
;;; themselves. No mention of constraint solvers, sets, or pairing.

(define *fewshot-budget* 8000)

;; Step-0 renderer (fresh state)
(define (fewshot-render-example task env-spec fuel action)
  (let* ([env (fold-left
                (lambda (env entry)
                  (rlm-env-put env (car entry) "synthetic"
                               (cadr entry) (caddr entry)))
                '()
                env-spec)]
         [state (make-rlm2-state task '() env '() '() '() '()
                                 #f fuel 0)]
         [hud (rlm2-render-state state *fewshot-budget*)]
         [action-str (let ([p (open-output-string)])
                       (write action p)
                       (get-output-string p))])
    (cons hud action-str)))

;; Mid-trajectory renderer (state with notes, loaded modules, step > 0)
(define (fewshot-render-at-step task env-spec loaded notes last-result fuel step action)
  (let* ([env (fold-left
                (lambda (env entry)
                  (rlm-env-put env (car entry) "synthetic"
                               (cadr entry) (caddr entry)))
                '()
                env-spec)]
         [state (make-rlm2-state task '() env loaded notes '() '()
                                 last-result fuel step)]
         [hud (rlm2-render-state state *fewshot-budget*)]
         [action-str (let ([p (open-output-string)])
                       (write action p)
                       (get-output-string p))])
    (cons hud action-str)))

;;; --- Group A: Aggregation format basics ---

(define *aggregation-examples*
  (list
    ;; 1. Simple peek
    (fewshot-render-example
      "Count records where type is 'gamma'."
      '((input chunks 55000) (task text 40))
      20000
      '(peek 'input 500))

    ;; 2. Think-then-act
    (fewshot-render-example
      "Find the average value for region 'east' records."
      '((input chunks 55000) (task text 50))
      20000
      '(think "I need both sum and count to compute an average. Let me peek at the format first."))

    ;; 3. Map-chunks
    (fewshot-render-example
      "Sum all 'value' fields where type is 'beta' AND region is 'south'."
      '((input chunks 55000) (task text 70))
      15000
      '(map-chunks 'input
        "(let ([lines (split-lines *chunk*)])\n  (apply + (map (lambda (line)\n    (if (and (string-contains? line \"type: beta\")\n             (string-contains? line \"region: south\"))\n        (let ([v (extract-after line \"value: \")])\n          (if v (string->number v) 0))\n        0))\n    lines)))"))

    ;; 4. Store + submit
    (fewshot-render-example
      "Report the total count."
      '((input chunks 55000) (task text 25) (map-result result 0))
      18000
      '(begin
         (store 'total (apply + (retrieve 'map-result)))
         (submit (retrieve 'total))))

    ;; 5. Full aggregation pipeline
    (fewshot-render-example
      "Find the total sum of all 'value' fields from records where type is 'alpha' AND region is 'north'. Report only the numeric total."
      '((input chunks 55000) (task text 130))
      20000
      '(begin
         (map-chunks 'input
           "(let ([lines (split-lines *chunk*)])\n       (apply + (map (lambda (line)\n                       (if (and (string-contains? line \\\"type: alpha\\\")\n                                (string-contains? line \\\"region: north\\\"))\n                           (let ([v (extract-after line \\\"value: \\\")])\n                             (if v (string->number v) 0))\n                           0))\n                     lines)))")
         (store 'total (apply + (retrieve 'map-result)))
         (submit (retrieve 'total))))))

;;; --- Group B: Lattice discovery (search→load→use) ---

(define *discovery-examples*
  (list
    ;; 1. Gradient optimizer — search for tools (step 0)
    (fewshot-render-example
      "Find the minimum of f(x) = x^3 - 6x^2 + 9x + 1 on the interval [0, 5]. Report the x value and minimum."
      '((task text 95))
      20000
      '(search "gradient descent minimize optimization"))

    ;; 2. Game theory — load discovered module (step 1, after search)
    (fewshot-render-at-step
      "Compute the mixed-strategy Nash equilibrium for a 2-player game with payoffs [[3,0],[5,1]] vs [[3,5],[0,1]]."
      '((task text 110))
      '()  ; not yet loaded
      '("search results: optimization (gradient-descent, newton-method, l-bfgs), fp/game-theory (nash-equilibrium, dominated-strategy, mixed-strategy, payoff-matrix)")
      "search returned 47 matches across 8 modules"
      18000
      1
      '(load 'fp/game-theory))

    ;; 3. Declarative graphics — eval with loaded module (step 2)
    (fewshot-render-at-step
      "Generate the vertices of a regular hexagon centered at origin with radius 1. Return as a list of (x y) pairs."
      '((task text 85))
      '(geometry/shapes)
      '("search results: geometry/shapes (regular-polygon, circle-points, ngon-vertices), geometry/transforms (rotate, translate)"
        "loaded geometry/shapes")
      "module geometry/shapes loaded (14 exports)"
      16000
      2
      '(begin
         (eval "(map (lambda (i) (let ([a (* i (/ 6.283185 6))]) (list (cos a) (sin a)))) (iota 6))")
         (submit (retrieve 'last-result))))

    ;; 4. Refactoring — search for analysis tools (step 0)
    (fewshot-render-example
      "Find all functions exported by the statistics module that perform hypothesis testing."
      '((task text 80))
      15000
      '(begin
         (search "module exports list introspect")
         (think "I need introspection tools to examine module exports. Let me see what the lattice provides.")))

    ;; 5. String manipulation — map-chunks for extraction (step 0)
    (fewshot-render-example
      "Extract all lines containing email addresses from the document and return them as a list."
      '((input chunks 45000) (task text 60))
      20000
      '(map-chunks 'input
        "(filter (lambda (line) (string-contains? line \"@\")) (split-lines *chunk*))"))))

(define *fewshot-examples*
  (append *aggregation-examples* *discovery-examples*))

(define *few-shot-msgs*
  (apply append
    (map (lambda (pair)
           (list `((role . "user") (content . ,(car pair)))
                 `((role . "assistant") (content . ,(cdr pair)))))
         *fewshot-examples*)))

;;; ====
;;; Runner
;;; ====

(define (run-fewshot-pairs!)
  (display "Few-Shot OOLONG-Pairs Benchmark (Nemotron)\n")
  (display "==========================================\n\n")
  (display (format "Few-shot: ~a examples (~a aggregation + ~a discovery, no constraint hints)\n\n"
                   (length *fewshot-examples*)
                   (length *aggregation-examples*)
                   (length *discovery-examples*)))

  (let* ([provider (rlm-provider-vllm
                     "nvidia/NVIDIA-Nemotron-3-Nano-30B-A3B-NVFP4" 8000)]
         [entries (generate-pairs-entries 15 3)]
         [haystack (build-pairs-haystack entries 30000)]
         [set-a (compute-condition-set entries *condition-a-cat* 'before *cutoff-day*)]
         [set-b (compute-condition-set entries *condition-b-cat* 'after *cutoff-day*)]
         [expected-pairs (compute-pairs set-a set-b)]
         [cutoff-date (day->date-string *cutoff-day*)]
         [task (format "Find all pairs (A, B) where A < B (alphabetical), user A has at least one 'science' entry dated before ~a, and user B has at least one 'technology' entry dated after ~a. List each pair as (UNNN, UMMM), one per line, sorted ascending."
                       cutoff-date cutoff-date)]
         [config (append
                   (make-rlm2-config provider *pairs-system-prompt*
                                     10 40000 2000 1 4 8000 #f)
                   (list *few-shot-msgs*))])

    (display (format "Users: 15 (3 entries/user) | Haystack: ~a chars\n"
                     (string-length haystack)))
    (display (format "Set A (science before ~a): ~a users\n" cutoff-date (length set-a)))
    (display (format "Set B (tech after ~a): ~a users\n" cutoff-date (length set-b)))
    (display (format "Expected pairs: ~a\n" (length expected-pairs)))
    (display (format "Expected pairs: ~a\n\n" expected-pairs))
    (flush-output-port)

    (display "--- Running ---\n")
    (flush-output-port)

    (let-values ([(result ms) (wall-clock-ms (lambda () (rlm2-run config task haystack)))])
      (let* ([status (rlm2-run-result-status result)]
             [output (format "~a" (rlm2-run-result-output result))]
             [traj (rlm2-run-result-trajectory-hash result)])
        (let-values ([(f1 prec rec tp pred exp)
                      (compute-f1 expected-pairs output)])
          (display (format "\n  Status: ~a | Time: ~ams\n" status ms))
          (display (format "  F1: ~,2f (P=~,2f R=~,2f TP=~a pred=~a exp=~a)\n"
                           f1 prec rec tp pred exp))
          (display (format "  Output: ~a\n" (if (> (string-length output) 500)
                                                 (string-append (substring output 0 500) "...")
                                                 output)))
          (display (format "  Trajectory: ~a\n\n" traj))

          ;; Save results
          (let* ([ts (rlm2-current-iso8601)]
                 [outpath (format "user/rlm/bench-results-fewshot-pairs-~a.sexp" ts)])
            (call-with-port
              (open-file-output-port outpath
                (file-options no-fail) (buffer-mode block)
                (make-transcoder (utf-8-codec)))
              (lambda (port)
                (pretty-print
                  `(benchmark-results
                     (model "nvidia/NVIDIA-Nemotron-3-Nano-30B-A3B-NVFP4")
                     (mode "few-shot-pairs")
                     (n-fewshot ,(length *fewshot-examples*))
                     (timestamp ,ts)
                     (pairs
                       ((label . "OOLONG-Pairs 15 users (few-shot)")
                        (n-users . 15)
                        (entries-per-user . 3)
                        (haystack-chars . ,(string-length haystack))
                        (expected-pairs . ,(length expected-pairs))
                        (status . ,status)
                        (time-ms . ,ms)
                        (f1 . ,f1)
                        (precision . ,prec)
                        (recall . ,rec)
                        (tp . ,tp)
                        (predicted . ,pred)
                        (output . ,output)
                        (trajectory . ,traj))))
                  port)))
            (display (format "Results: ~a\n" outpath))))))))

;;; ====
;;; Main
;;; ====

(run-fewshot-pairs!)
