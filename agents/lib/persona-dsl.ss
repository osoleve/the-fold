;;; persona-dsl.ss - Enhanced Persona Prompt DSL
;;;
;;; A monadic DSL for building agent persona prompts with:
;;;   - Do-notation for clean composition
;;;   - Choice tracking for reproducibility
;;;   - Safe fragment loading with error handling
;;;   - Schema validation for persona structure
;;;   - Persona composition utilities
;;;
;;; Built on the Free monad DSL toolkit (fabric/stitches/fp/dsl.ss)
;;;
;;; USAGE:
;;;   (load "agents/lib/persona-dsl.ss")
;;;
;;;   (define my-persona
;;;     (prompt-do
;;;       (style <- (pick '("warm" "direct" "playful")))
;;;       (emit "You are MyAgent.")
;;;       (emit-line "Your voice is " style ".")
;;;       (approaches <- (pick-n 2 '("analyze" "question" "synthesize")))
;;;       (emit-list "Your approaches:" approaches)
;;;       (done)))
;;;
;;;   (run-persona my-persona)  ; => "You are MyAgent.\nYour voice is warm.\n..."

(load "fabric/stitches/fp/dsl.ss")
(load "thimble/string-utils.ss")

;;; ============================================================
;;; Prompt DSL Instructions
;;; ============================================================

;;; emit : String -> Prompt ()
;;; Emit text to the prompt output.
(define (emit text)
  (dsl-emit 'emit text))

;;; emit-line : String ... -> Prompt ()
;;; Emit text followed by newline.
(define (emit-line . parts)
  (dsl-emit 'emit-line parts))

;;; emit-blank : -> Prompt ()
;;; Emit a blank line.
(define (emit-blank)
  (dsl-emit 'emit-blank '()))

;;; emit-section : String -> String -> Prompt ()
;;; Emit a section header with underline.
(define (emit-section title)
  (dsl-emit 'emit-section title))

;;; emit-list : String -> List String -> Prompt ()
;;; Emit a bulleted list with header.
(define (emit-list header items)
  (dsl-emit 'emit-list (cons header items)))

;;; emit-numbered : String -> List String -> Prompt ()
;;; Emit a numbered list with header.
(define (emit-numbered header items)
  (dsl-emit 'emit-numbered (cons header items)))

;;; pick : List a -> Prompt a
;;; Randomly select one item from a list.
(define (pick options)
  (dsl-request 'pick options))

;;; pick-n : Int -> List a -> Prompt (List a)
;;; Randomly select n distinct items from a list.
(define (pick-n n options)
  (dsl-request 'pick-n (cons n options)))

;;; pick-weighted : List (Int . a) -> Prompt a
;;; Select based on weights.
(define (pick-weighted weighted-options)
  (dsl-request 'pick-weighted weighted-options))

;;; maybe-emit : String -> Float -> Prompt ()
;;; Emit text with given probability (0.0-1.0).
(define (maybe-emit text prob)
  (dsl-emit 'maybe-emit (cons prob text)))

;;; conditional : Bool -> Prompt () -> Prompt ()
;;; Conditionally execute a prompt action.
(define (conditional test action)
  (if test action (dsl-pure '())))

;;; done : -> Prompt ()
;;; Signal completion (no-op, for readability).
(define (done)
  (dsl-pure '()))

;;; ============================================================
;;; Prompt Do-Notation
;;; ============================================================

;;; prompt-do : Monadic do-notation for prompt building
;;; Usage:
;;;   (prompt-do
;;;     (x <- (pick options))
;;;     (emit x)
;;;     (done))
(define-syntax prompt-do
  (syntax-rules (<-)
                [(_ expr)
                 expr]
                [(_ (var <- action) rest ...)
                 (dsl-bind action (lambda (var) (prompt-do rest ...)))]
                [(_ action rest ...)
                 (dsl-bind action (lambda (_) (prompt-do rest ...)))]))

;;; ============================================================
;;; Prompt Interpreter
;;; ============================================================

;;; Interpreter state (using mutable cells)
(define (make-prompt-state)
  (vector '() '() #f))  ; output, choices, seed

(define (prompt-state-output state)
  (vector-ref state 0))

(define (prompt-state-output-set! state val)
  (vector-set! state 0 val))

(define (prompt-state-choices state)
  (vector-ref state 1))

(define (prompt-state-choices-set! state val)
  (vector-set! state 1 val))

(define (prompt-state-seed state)
  (vector-ref state 2))

(define (prompt-state-seed-set! state val)
  (vector-set! state 2 val))

;;; make-prompt-interpreter : -> (Interpreter . State)
;;; Create an interpreter that collects output and tracks choices.
(define (make-prompt-interpreter)
  (let ([state (make-prompt-state)])
       (cons
        (make-interpreter
         (lambda (tag payload)
                 (case tag
                       [(emit)
                        (prompt-state-output-set!
                         state
                         (cons payload (prompt-state-output state)))
                        '()]
                       
                       [(emit-line)
                        (let ([text (apply string-append payload)])
                             (prompt-state-output-set!
                              state
                              (cons "\n" (cons text (prompt-state-output state)))))
                        '()]
                       
                       [(emit-blank)
                        (prompt-state-output-set!
                         state
                         (cons "\n" (prompt-state-output state)))
                        '()]
                       
                       [(emit-section)
                        (let* ([title payload]
                               [underline (make-string (string-length title) #\─)])
                              (prompt-state-output-set!
                               state
                               (cons "\n" (cons underline (cons "\n" (cons title (prompt-state-output state)))))))
                        '()]
                       
                       [(emit-list)
                        (let* ([header (car payload)]
                               [items (cdr payload)]
                               [bullet-lines (let loop ([lst items] [acc '()])
                                                  (if (null? lst)
                                                      (reverse acc)
                                                      (loop (cdr lst)
                                                            (cons (string-append "• " (car lst) "\n") acc))))])
                              (prompt-state-output-set!
                               state
                               (append (reverse (cons "\n" bullet-lines))
                                       (cons "\n" (cons header (prompt-state-output state))))))
                        '()]
                       
                       [(emit-numbered)
                        (let* ([header (car payload)]
                               [items (cdr payload)]
                               [numbered-lines (let loop ([lst items] [n 1] [acc '()])
                                                    (if (null? lst)
                                                        (reverse acc)
                                                        (loop (cdr lst) (+ n 1)
                                                              (cons (string-append (number->string n) ". " (car lst) "\n") acc))))])
                              (prompt-state-output-set!
                               state
                               (append (reverse (cons "\n" numbered-lines))
                                       (cons "\n" (cons header (prompt-state-output state))))))
                        '()]
                       
                       [(pick)
                        (let* ([options payload]
                               [idx (random (length options))]
                               [selected (list-ref options idx)])
                              (prompt-state-choices-set!
                               state
                               (cons (cons 'pick selected) (prompt-state-choices state)))
                              selected)]
                       
                       [(pick-n)
                        (let* ([n (car payload)]
                               [options (cdr payload)]
                               [selected (shuffle-take options n)])
                              (prompt-state-choices-set!
                               state
                               (cons (cons 'pick-n selected) (prompt-state-choices state)))
                              selected)]
                       
                       [(pick-weighted)
                        (let* ([options payload]
                               [total-weight (apply + (map car options))]
                               [r (random total-weight)]
                               [selected (weighted-select options r 0)])
                              (prompt-state-choices-set!
                               state
                               (cons (cons 'pick-weighted selected) (prompt-state-choices state)))
                              selected)]
                       
                       [(maybe-emit)
                        (let ([prob (car payload)]
                              [text (cdr payload)])
                             (when (< (random 1.0) prob)
                                   (prompt-state-output-set!
                                    state
                                    (cons text (prompt-state-output state)))))
                        '()]
                       
                       [else
                        (error 'prompt-interpreter "Unknown instruction" tag)])))
        state)))

;;; Helper: Fisher-Yates shuffle and take n
(define (shuffle-take lst n)
  (let* ([vec (list->vector lst)]
         [len (vector-length vec)])
        (let loop ([i (- len 1)])
             (when (> i 0)
                   (let* ([j (random (+ i 1))]
                          [temp (vector-ref vec i)])
                         (vector-set! vec i (vector-ref vec j))
                         (vector-set! vec j temp)
                         (loop (- i 1)))))
        (let take-loop ([i 0] [acc '()])
             (if (or (>= i n) (>= i len))
                 (reverse acc)
                 (take-loop (+ i 1) (cons (vector-ref vec i) acc))))))

;;; Helper: Weighted selection
(define (weighted-select options r current-sum)
  (if (null? options)
      (cdar (last-pair options))
      (let* ([opt (car options)]
             [weight (car opt)]
             [value (cdr opt)]
             [next-sum (+ current-sum weight)])
            (if (< r next-sum)
                value
                (weighted-select (cdr options) r next-sum)))))

;;; Helper: last-pair
(define (last-pair lst)
  (if (null? (cdr lst))
      lst
      (last-pair (cdr lst))))

;;; ============================================================
;;; Running Personas
;;; ============================================================

;;; run-persona : Prompt a -> String
;;; Execute a persona prompt builder and return the generated prompt.
(define (run-persona program)
  (let* ([interp+state (make-prompt-interpreter)]
         [interp (car interp+state)]
         [state (cdr interp+state)]
         [_ (run-dsl interp program)]
         [output (prompt-state-output state)])
        (apply string-append (reverse output))))

;;; run-persona-traced : Prompt a -> (String . Choices)
;;; Execute and return both prompt and choice log.
(define (run-persona-traced program)
  (let* ([interp+state (make-prompt-interpreter)]
         [interp (car interp+state)]
         [state (cdr interp+state)]
         [_ (run-dsl interp program)]
         [output (prompt-state-output state)]
         [choices (prompt-state-choices state)])
        (cons (apply string-append (reverse output))
              (reverse choices))))

;;; run-persona-safe : Prompt a -> Result String
;;; Execute with error handling.
(define (run-persona-safe program)
  (let ([interp+state (make-prompt-interpreter)])
       (run-dsl-safe (car interp+state) program)))

;;; ============================================================
;;; Choice Replay (Reproducibility)
;;; ============================================================

;;; make-replay-interpreter : List Choice -> Interpreter
;;; Create interpreter that replays recorded choices.
(define (make-replay-interpreter choices)
  (let ([state (make-prompt-state)]
        [choice-queue choices])
       (cons
        (make-interpreter
         (lambda (tag payload)
                 (case tag
                       [(emit emit-line emit-blank emit-section emit-list emit-numbered maybe-emit)
                        ;; Handle output same as normal interpreter
                        ((interpreter-handler (car (make-prompt-interpreter))) tag payload)]
                       
                       [(pick pick-n pick-weighted)
                        ;; Replay from recorded choices
                        (if (null? choice-queue)
                            (error 'replay-interpreter "Ran out of recorded choices")
                            (let ([recorded (car choice-queue)])
                                 (set! choice-queue (cdr choice-queue))
                                 (cdr recorded)))]
                       
                       [else
                        (error 'replay-interpreter "Unknown instruction" tag)])))
        state)))

;;; run-persona-with-choices : Prompt a -> List Choice -> String
;;; Replay a persona with specific choices.
(define (run-persona-with-choices program choices)
  (let* ([interp+state (make-replay-interpreter choices)]
         [interp (car interp+state)]
         [state (cdr interp+state)]
         [_ (run-dsl interp program)]
         [output (prompt-state-output state)])
        (apply string-append (reverse output))))

;;; ============================================================
;;; Safe Fragment Loading
;;; ============================================================

(define *fragment-cache* (make-eq-hashtable))

;;; try-load-fragment : Symbol -> Prompt (Result Fragment)
;;; Attempt to load a fragment, returning Result.
(define (try-load-fragment name)
  (dsl-pure
   (let ([cached (hashtable-ref *fragment-cache* name #f)])
        (if cached
            (dsl-ok cached)
            (guard (ex [else (dsl-err 'fragment-not-found
                                      (string-append "Failed to load: " (symbol->string name)))])
                   (let ([path (find-fragment-path name)])
                        (if path
                            (begin
                             (load path)
                             (hashtable-set! *fragment-cache* name #t)
                             (dsl-ok #t))
                            (dsl-err 'fragment-not-found
                                     (string-append "Fragment not found: " (symbol->string name))))))))))

;;; find-fragment-path : Symbol -> String | #f
(define (find-fragment-path name)
  (let* ([filename (string-append (symbol->string name) ".ss")]
         [candidates (list
                      (string-append "agents/personas/fragments/" filename)
                      (string-append "/home/oso/the-fold/agents/personas/fragments/" filename))])
        (let loop ([paths candidates])
             (if (null? paths)
                 #f
                 (if (file-exists? (car paths))
                     (car paths)
                     (loop (cdr paths)))))))

;;; with-fragment : Symbol -> (-> Prompt a) -> Prompt a -> Prompt a
;;; Use fragment if available, otherwise use fallback.
(define (with-fragment name use-fragment fallback)
  (prompt-do
   (result <- (try-load-fragment name))
   (if (dsl-ok? result)
       (use-fragment)
       fallback)))

;;; ============================================================
;;; Persona Validation
;;; ============================================================

;;; Validation result
(define (validation-ok) (dsl-ok 'valid))
(define (validation-err field msg)
  (dsl-err field msg))

;;; make-persona-schema : List (Symbol . Type) -> Schema
(define (make-persona-schema fields)
  (list 'persona-schema fields))

(define (persona-schema? x)
  (and (pair? x) (eq? (car x) 'persona-schema)))

(define (persona-schema-fields schema)
  (cadr schema))

;;; validate-field : Any -> Symbol -> Result
(define (validate-field value field-type)
  (case field-type
        [(required-string)
         (if (and (string? value) (> (string-length value) 0))
             (dsl-ok value)
             (dsl-err 'invalid-field "Expected non-empty string"))]
        [(optional-string)
         (if (or (not value) (string? value))
             (dsl-ok value)
             (dsl-err 'invalid-field "Expected string or #f"))]
        [(string-list)
         (if (and (list? value) (every string? value))
             (dsl-ok value)
             (dsl-err 'invalid-field "Expected list of strings"))]
        [(positive-int)
         (if (and (integer? value) (> value 0))
             (dsl-ok value)
             (dsl-err 'invalid-field "Expected positive integer"))]
        [else
         (dsl-ok value)]))

;;; every : (a -> Bool) -> List a -> Bool
(define (every pred lst)
  (or (null? lst)
      (and (pred (car lst))
           (every pred (cdr lst)))))

;;; ============================================================
;;; Persona Composition
;;; ============================================================

;;; compose-prompts : Prompt a -> Prompt b -> Prompt b
;;; Sequence two prompts, keeping output from both.
(define (compose-prompts p1 p2)
  (prompt-do
   (_ <- p1)
   p2))

;;; merge-personas : List Prompt -> Prompt ()
;;; Merge multiple persona prompts into one.
(define (merge-personas prompts)
  (if (null? prompts)
      (done)
      (compose-prompts (car prompts) (merge-personas (cdr prompts)))))

;;; inject-trait : Symbol -> Prompt a -> Prompt a
;;; Tag a prompt section for later filtering.
(define (inject-trait trait-name prompt)
  (prompt-do
   (emit-line "<!-- trait: " (symbol->string trait-name) " -->")
   prompt
   (emit-line "<!-- /trait -->")))

;;; ============================================================
;;; Convenience Constructors
;;; ============================================================

;;; persona-header : String -> String -> Prompt ()
;;; Standard persona header.
(define (persona-header name role)
  (prompt-do
   (emit-line "You are " name ".")
   (emit-blank)
   (emit-section "Your Role")
   (emit role)))

;;; persona-voice : List String -> Prompt ()
;;; Voice/style section with random selection.
(define (persona-voice styles)
  (prompt-do
   (style <- (pick styles))
   (emit-blank)
   (emit-section "Your Voice")
   (emit style)))

;;; persona-approaches : Int -> List String -> Prompt ()
;;; Approaches section with n random selections.
(define (persona-approaches n options)
  (prompt-do
   (selected <- (pick-n n options))
   (emit-blank)
   (emit-list "Your Approaches:" selected)))

;;; persona-triggers : Int -> List String -> Prompt ()
;;; When-to-post section.
(define (persona-triggers n options)
  (prompt-do
   (selected <- (pick-n n options))
   (emit-blank)
   (emit-list "When to Contribute:" selected)))

;;; persona-boundaries : List String -> Prompt ()
;;; Boundaries/constraints section.
(define (persona-boundaries items)
  (prompt-do
   (emit-blank)
   (emit-list "Critical Boundaries:" items)))

;;; persona-signoff : List String -> Prompt ()
;;; Random signoff.
(define (persona-signoff options)
  (prompt-do
   (signoff <- (pick options))
   (emit-blank)
   (emit-line "Sign Off: " signoff ".")))

;;; ============================================================
;;; Debug Utilities
;;; ============================================================

;;; persona-peek : Prompt a -> (Symbol . Any)
;;; Inspect first instruction of a persona prompt.
(define persona-peek dsl-peek)

;;; persona-count-choices : Prompt a -> Int -> Int
;;; Count choice points in a persona (approximation).
(define (persona-count-choices program fuel)
  (dsl-count-instructions program fuel))

;;; persona-pretty-print : Prompt a -> String
;;; Pretty print first instruction.
(define persona-pretty-print dsl-pretty-print)
