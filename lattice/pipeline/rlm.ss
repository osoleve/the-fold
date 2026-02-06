(load "lattice/pipeline/stage.ss")
(load "lattice/pipeline/effects.ss")

(doc 'module 'pipeline/rlm)
(doc 'description "Recursive Language Model types and pure operations. Defines RLM environments, configurations, trajectory records, and context chunking — all as plain s-expressions suitable for CAS storage and fuel-bounded evaluation.")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'dependencies '(pipeline/stage.ss pipeline/effects.ss))

;;; ====
;;; RLM Environment (Pure)
;;; ====
;;;
;;; An RLM environment maps symbolic keys to metadata entries.
;;; Values themselves live in CAS — the env tracks what's available
;;; without materializing content. This is the core RLM insight:
;;; context is navigable state, not prompt stuffing.
;;;
;;; Entry: (key type size hash)
;;;   key  : Symbol — human-readable name
;;;   type : Symbol — 'text, 'sexpr, 'chunks, 'result, 'sub-result
;;;   size : Nat    — character count or item count
;;;   hash : String — CAS hash (hex) of the stored value

(doc 'section 'rlm-environment)

(doc 'type '(-> RlmEnv))
(doc 'description "Create an empty RLM environment")
(define (make-rlm-env) '())

(doc 'type '(-> RlmEnv Symbol Any Symbol RlmEnv))
(doc 'description "Add or update an entry in the environment. value-hash is the CAS hex hash.")
(define (rlm-env-put env key value-hash tag size)
  (let ([entry (list key tag size value-hash)])
    (cons entry (rlm-env-remove-key env key))))

(doc 'type '(-> RlmEnv Symbol (Maybe RlmEntry)))
(doc 'description "Look up an entry by key. Returns (key type size hash) or #f.")
(define (rlm-env-get env key)
  (let loop ([e env])
    (cond
      [(null? e) #f]
      [(eq? (caar e) key) (car e)]
      [else (loop (cdr e))])))

(doc 'type '(-> RlmEnv (List (List Symbol Symbol Nat))))
(doc 'description "List all keys with their types and sizes")
(define (rlm-env-keys env)
  (map (lambda (entry)
         (list (car entry)     ; key
               (cadr entry)    ; type
               (caddr entry))) ; size
       env))

(doc 'type '(-> RlmEnv Symbol RlmEnv))
(doc 'description "Remove a key from the environment")
(define (rlm-env-remove-key env key)
  (filter (lambda (entry) (not (eq? (car entry) key))) env))

(doc 'type '(-> RlmEnv String))
(doc 'description "Generate a human-readable summary of environment contents for model prompts")
(define (rlm-env-summary env)
  (if (null? env)
      "[Environment is empty]"
      (let ([lines (map (lambda (entry)
                          (format "  ~a (~a, ~a chars)"
                                  (car entry)    ; key
                                  (cadr entry)   ; type
                                  (caddr entry))); size
                        env)])
        (string-append "Available context:\n"
                       (apply string-append
                              (map (lambda (l) (string-append l "\n")) lines))))))

(doc 'type '(-> RlmEnv Symbol String))
(doc 'description "Get the CAS hash for a key, or #f if not present")
(define (rlm-env-hash env key)
  (let ([entry (rlm-env-get env key)])
    (if entry (cadddr entry) #f)))

;;; ====
;;; RLM Configuration
;;; ====

(doc 'section 'rlm-config)

(doc 'type '(-> RlmProvider String Nat Nat Nat Nat Nat RlmConfig))
(doc 'description "Create an RLM configuration")
(define (make-rlm-config provider system-prompt max-steps max-fuel
                         chunk-size max-depth loop-window)
  (list 'rlm-config provider system-prompt max-steps max-fuel
        chunk-size max-depth loop-window))

(define (rlm-config? x)
  (and (pair? x) (eq? (car x) 'rlm-config)))

(define (rlm-config-provider cfg)      (list-ref cfg 1))
(define (rlm-config-system-prompt cfg) (list-ref cfg 2))
(define (rlm-config-max-steps cfg)     (list-ref cfg 3))
(define (rlm-config-max-fuel cfg)      (list-ref cfg 4))
(define (rlm-config-chunk-size cfg)    (list-ref cfg 5))
(define (rlm-config-max-depth cfg)     (list-ref cfg 6))
(define (rlm-config-loop-window cfg)   (list-ref cfg 7))

;;; Sensible defaults
(define (make-rlm-config-default provider system-prompt)
  (make-rlm-config provider system-prompt
                    20     ; max-steps
                    50000  ; max-fuel
                    2000   ; chunk-size (chars)
                    2      ; max-depth (root + 1 sub-level)
                    3))    ; loop-window (detect repeat in last 3)

;;; ====
;;; RLM Step Record
;;; ====

(doc 'section 'rlm-step-record)

(doc 'type '(-> Nat String String String Any Alist Nat RlmStepRecord))
(doc 'description "Create an immutable record of a single RLM step")
(define (make-rlm-step-record step-num prompt-sent response-received
                              code-extracted eval-result env-delta fuel-used)
  (list 'rlm-step step-num prompt-sent response-received
        code-extracted eval-result env-delta fuel-used))

(define (rlm-step? x)
  (and (pair? x) (eq? (car x) 'rlm-step)))

(define (rlm-step-num r)             (list-ref r 1))
(define (rlm-step-prompt r)          (list-ref r 2))
(define (rlm-step-response r)        (list-ref r 3))
(define (rlm-step-code r)            (list-ref r 4))
(define (rlm-step-result r)          (list-ref r 5))
(define (rlm-step-env-delta r)       (list-ref r 6))
(define (rlm-step-fuel-used r)       (list-ref r 7))

;;; ====
;;; Chunk Manifest
;;; ====

(doc 'section 'chunk-manifest)

(doc 'type '(-> Symbol Nat Nat (List String) ChunkManifest))
(doc 'description "Metadata for a chunked text entry in the environment")
(define (make-chunk-manifest key total-chars chunk-count chunk-hashes)
  (list 'chunk-manifest key total-chars chunk-count chunk-hashes))

(define (chunk-manifest? x)
  (and (pair? x) (eq? (car x) 'chunk-manifest)))

(define (chunk-manifest-key m)          (list-ref m 1))
(define (chunk-manifest-total-chars m)  (list-ref m 2))
(define (chunk-manifest-count m)        (list-ref m 3))
(define (chunk-manifest-hashes m)       (list-ref m 4))

;;; ====
;;; RLM Trajectory
;;; ====

(doc 'section 'rlm-trajectory)

(doc 'type '(-> RlmConfig String String Nat Nat Symbol String String Nat RlmTrajectory))
(doc 'description "Summary record for a complete RLM run")
(define (make-rlm-trajectory config-hash input-hash output-hash
                             steps total-fuel status
                             started finished depth)
  (list 'rlm-trajectory config-hash input-hash output-hash
        steps total-fuel status started finished depth))

(define (rlm-trajectory? x)
  (and (pair? x) (eq? (car x) 'rlm-trajectory)))

(define (rlm-trajectory-config-hash t) (list-ref t 1))
(define (rlm-trajectory-input-hash t)  (list-ref t 2))
(define (rlm-trajectory-output-hash t) (list-ref t 3))
(define (rlm-trajectory-steps t)       (list-ref t 4))
(define (rlm-trajectory-total-fuel t)  (list-ref t 5))
(define (rlm-trajectory-status t)      (list-ref t 6))
(define (rlm-trajectory-started t)     (list-ref t 7))
(define (rlm-trajectory-finished t)    (list-ref t 8))
(define (rlm-trajectory-depth t)       (list-ref t 9))

;;; ====
;;; Response Parsing (Pure)
;;; ====
;;;
;;; Extract code blocks and markers from model responses.
;;; The model can emit:
;;;   ```scheme or ```fold  — executable code
;;;   ```fold-template      — template DSL (processed via tp-batch)
;;;   DONE(value) or FINAL(value) — completion marker
;;;   Everything else is 'thought — logged but not executed.

(doc 'section 'response-parsing)

(doc 'type '(-> String (List RlmParsedBlock)))
(doc 'description "Parse a model response into typed blocks: code, template, done, thought")
(define (rlm-parse-response text)
  (let loop ([lines (string-split-lines text)]
             [acc '()]
             [in-block #f]
             [block-type #f]
             [block-lines '()])
    (cond
      [(null? lines)
       ;; End of input — flush any open block
       (let ([final (if in-block
                        (cons (make-parsed-block block-type
                                (lines->string (reverse block-lines)))
                              acc)
                        acc)])
         (reverse (coalesce-thoughts final)))]
      ;; Opening fence
      [(and (not in-block) (code-fence-open? (car lines)))
       (let ([type (fence-block-type (car lines))])
         (loop (cdr lines) acc #t type '()))]
      ;; Closing fence
      [(and in-block (code-fence-close? (car lines)))
       (let ([block (make-parsed-block block-type
                      (lines->string (reverse block-lines)))])
         (loop (cdr lines) (cons block acc) #f #f '()))]
      ;; Inside a code block
      [in-block
       (loop (cdr lines) acc #t block-type (cons (car lines) block-lines))]
      ;; DONE/FINAL marker
      [(done-marker? (car lines))
       (let ([value (extract-done-value (car lines))])
         (loop (cdr lines) (cons (list 'done value) acc) #f #f '()))]
      ;; Plain text (thought)
      [else
       (loop (cdr lines) (cons (list 'thought (car lines)) acc) #f #f '())])))

;;; Parsed block constructors
(define (make-parsed-block type content)
  (cond
    [(eq? type 'template) (list 'template content)]
    [else                 (list 'code content)]))

;;; Fence detection
(define (code-fence-open? line)
  (let ([trimmed (string-trim-left line)])
    (and (>= (string-length trimmed) 3)
         (string=? (substring trimmed 0 3) "```"))))

(define (code-fence-close? line)
  (let ([trimmed (string-trim-left line)])
    (and (>= (string-length trimmed) 3)
         (string=? (substring trimmed 0 3) "```")
         (<= (string-length trimmed) 4))))  ; just ``` or ```\n

(define (fence-block-type line)
  (let* ([trimmed (string-trim-left line)]
         [lang (substring trimmed 3 (string-length trimmed))]
         [lang-trimmed (string-trim lang)])
    (cond
      [(or (string=? lang-trimmed "scheme")
           (string=? lang-trimmed "fold"))
       'code]
      [(string=? lang-trimmed "fold-template")
       'template]
      [else 'code])))  ; default to code for unknown languages

;;; DONE/FINAL marker
;;; done-marker? : String -> Bool
;;; Matches: DONE, DONE(value), FINAL, FINAL(value)
;;; Does NOT match: DONE with analysis, FINALLY, DONESTUFF
(define (done-marker? line)
  (let ([trimmed (string-trim line)])
    (or (string=? trimmed "DONE")
        (string=? trimmed "FINAL")
        (and (>= (string-length trimmed) 5)
             (string=? (substring trimmed 0 5) "DONE("))
        (and (>= (string-length trimmed) 6)
             (string=? (substring trimmed 0 6) "FINAL(")))))

(define (extract-done-value line)
  (let* ([trimmed (string-trim line)]
         [paren-start (string-index trimmed #\()])
    (if paren-start
        (let ([paren-end (string-rindex trimmed #\))])
          (if paren-end
              (substring trimmed (+ paren-start 1) paren-end)
              trimmed))
        trimmed)))

;;; Coalesce adjacent thought lines into single thought blocks
(define (coalesce-thoughts blocks)
  (let loop ([bs blocks] [acc '()] [pending-thoughts '()])
    (cond
      [(null? bs)
       (if (null? pending-thoughts)
           (reverse acc)
           (reverse (cons (list 'thought (lines->string (reverse pending-thoughts)))
                          acc)))]
      [(and (pair? (car bs)) (eq? (caar bs) 'thought))
       (loop (cdr bs) acc (cons (cadar bs) pending-thoughts))]
      [else
       (if (null? pending-thoughts)
           (loop (cdr bs) (cons (car bs) acc) '())
           (loop (cdr bs)
                 (cons (car bs)
                       (cons (list 'thought (lines->string (reverse pending-thoughts)))
                             acc))
                 '()))])))

;;; Block predicates
(define (rlm-block-code? b)     (and (pair? b) (eq? (car b) 'code)))
(define (rlm-block-template? b) (and (pair? b) (eq? (car b) 'template)))
(define (rlm-block-done? b)     (and (pair? b) (eq? (car b) 'done)))
(define (rlm-block-thought? b)  (and (pair? b) (eq? (car b) 'thought)))
(define (rlm-block-content b)   (cadr b))

;;; ====
;;; String Utilities (pure, minimal)
;;; ====

(define (string-split-lines s)
  (let loop ([chars (string->list s)]
             [current '()]
             [lines '()])
    (cond
      [(null? chars)
       (reverse (cons (list->string (reverse current)) lines))]
      [(char=? (car chars) #\newline)
       (loop (cdr chars) '() (cons (list->string (reverse current)) lines))]
      [else
       (loop (cdr chars) (cons (car chars) current) lines)])))

(define (lines->string lines)
  (if (null? lines)
      ""
      (let loop ([ls (cdr lines)]
                 [result (car lines)])
        (if (null? ls)
            result
            (loop (cdr ls) (string-append result "\n" (car ls)))))))

(define (string-trim-left s)
  (let ([len (string-length s)])
    (let loop ([i 0])
      (cond
        [(>= i len) ""]
        [(char-whitespace? (string-ref s i)) (loop (+ i 1))]
        [else (substring s i len)]))))

(define (string-trim-right s)
  (let ([len (string-length s)])
    (let loop ([i (- len 1)])
      (cond
        [(< i 0) ""]
        [(char-whitespace? (string-ref s i)) (loop (- i 1))]
        [else (substring s 0 (+ i 1))]))))

(define (string-trim s)
  (string-trim-left (string-trim-right s)))

(define (string-index s ch)
  (let ([len (string-length s)])
    (let loop ([i 0])
      (cond
        [(>= i len) #f]
        [(char=? (string-ref s i) ch) i]
        [else (loop (+ i 1))]))))

(define (string-rindex s ch)
  (let loop ([i (- (string-length s) 1)])
    (cond
      [(< i 0) #f]
      [(char=? (string-ref s i) ch) i]
      [else (loop (- i 1))])))

;;; ====
;;; Loop Detection Helpers (Pure)
;;; ====

(doc 'section 'loop-detection)

(doc 'type '(-> String String String))
(doc 'description "Create a fingerprint for loop detection from env summary + emitted code")
(define (rlm-step-fingerprint env-summary code)
  (string-append env-summary "|||" code))

(doc 'type '(-> (List String) String Nat Boolean))
(doc 'description "Check if the current fingerprint appears in the recent history window")
(define (rlm-loop-detected? history current-fingerprint window-size)
  (let ([recent (take-up-to window-size history)])
    (member current-fingerprint recent)))

(define (take-up-to n lst)
  (let loop ([remaining lst] [k n] [acc '()])
    (if (or (= k 0) (null? remaining))
        (reverse acc)
        (loop (cdr remaining) (- k 1) (cons (car remaining) acc)))))

;;; ====
;;; Pipeline Effect Constructor
;;; ====

(doc 'section 'rlm-effect)

(doc 'type '(-> RlmConfig String Nat (Stage ctx i o)))
(doc 'description "Create an RLM effect stage for pipeline composition")
(define (rlm-effect config system-prompt max-steps)
  (effect 'rlm (list 'run config system-prompt max-steps)))

;;; rlm-effect? : Effect -> Boolean
(define (rlm-effect? e)
  (and (stage-effect? e)
       (eq? (stage-effect-type e) 'rlm)))
