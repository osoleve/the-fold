;;; @module chronicle
;;; @requires prelude combinators
(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
(require 'prelude)
(require 'combinators)

(doc 'module 'chronicle)
(doc 'description "Chronicle: A Narrative DSL - Next-generation narrative engine")
(doc 'layer 'lattice)
(doc 'purity 'partial)

(doc 'note "Dogfoods:
  - Quasiquotation (dsl/quasi.ss) for template expansion
  - Parser combinators (fp/parser.ss) for DSL parsing
  - Maybe/Either monads (fp/combinators.ss) for error handling
  - Optionally: Probability monad for procedural narrative")

(doc 'note "Core Concepts:
  - Scene: A narrative moment with text, choices, and effects
  - Chronicle: A graph of scenes connected by transitions
  - State: Player variables, flags, inventory
  - Guard: First-class predicate on state
  - Effect: State transformation
  - Template: Quasiquoted text with state interpolation")

(doc 'note "DSL Example:
  (chronicle \"my-story\"
    (scene 'intro
      (text \"Welcome, ${player-name}. The door awaits.\")
      (choice \"Open the door\" -> 'hallway)
      (choice \"Turn back\" -> 'ending
        :when (not (flag? 'committed))))
    (scene 'hallway
      (on-enter (set! 'committed #t))
      (text \"A long corridor stretches before you.\")
      (choice \"Continue\" -> 'ending)))")

(doc 'section 'core-types)

(doc 'note "Chronicle : A complete narrative")
(define-record-type chronicle%
  (fields id title scenes start-scene meta))

(define make-chronicle make-chronicle%)
(define chronicle? chronicle%?)
(define chronicle-id chronicle%-id)
(define chronicle-title chronicle%-title)
(define chronicle-scenes chronicle%-scenes)
(define chronicle-start-scene chronicle%-start-scene)
(define chronicle-meta chronicle%-meta)

;;; Scene : A narrative moment
(define-record-type scene%
  (fields id text choices on-enter on-exit meta))

(define make-scene make-scene%)
(define scene? scene%?)
(define scene-id scene%-id)
(define scene-text scene%-text)
(define scene-choices scene%-choices)
(define scene-on-enter scene%-on-enter)
(define scene-on-exit scene%-on-exit)
(define scene-meta scene%-meta)

;;; Choice : A player option
(define-record-type choice%
  (fields label target guard effects meta))

(define make-choice make-choice%)
(define choice? choice%?)
(define choice-label choice%-label)
(define choice-target choice%-target)
(define choice-guard choice%-guard)
(define choice-effects choice%-effects)
(define choice-meta choice%-meta)

(doc 'section 'state-management)
(doc 'note "State is an immutable record with vars, flags, inventory")

(define-record-type cstate%
  (fields vars flags inventory meta))

(define make-cstate make-cstate%)
(define cstate? cstate%?)
(define cstate-vars cstate%-vars)
(define cstate-flags cstate%-flags)
(define cstate-inventory cstate%-inventory)
(define cstate-meta cstate%-meta)

;;; Empty state constructor
(define (empty-state)
  (doc 'export #t)
  (make-cstate '() '() '() '()))

;;; State accessors
(define (state-get state key default)
  (doc 'export #t)
  (let ([p (assq key (cstate-vars state))])
       (if p (cdr p) default)))

(define (state-set state key value)
  (doc 'export #t)
  (let* ([vars (cstate-vars state)]
         [vars* (alist-set vars key value)])
        (make-cstate vars* (cstate-flags state)
                     (cstate-inventory state) (cstate-meta state))))

(define (state-flag? state flag)
  (doc 'export #t)
  (let ([p (assq flag (cstate-flags state))])
       (and p (cdr p))))

(define (state-set-flag state flag value)
  (doc 'export #t)
  (let* ([flags (cstate-flags state)]
         [flags* (alist-set flags flag value)])
        (make-cstate (cstate-vars state) flags*
                     (cstate-inventory state) (cstate-meta state))))

(define (state-flag! state flag)
  (doc 'export #t)
  (state-set-flag state flag #t))

(define (state-unflag! state flag)
  (doc 'export #t)
  (state-set-flag state flag #f))

(define (state-has-item? state item)
  (doc 'export #t)
  (and (memq item (cstate-inventory state)) #t))

(define (state-add-item state item)
  (doc 'export #t)
  (if (state-has-item? state item)
      state
      (make-cstate (cstate-vars state) (cstate-flags state)
                   (cons item (cstate-inventory state))
                   (cstate-meta state))))

(define (state-remove-item state item)
  (doc 'export #t)
  (make-cstate (cstate-vars state) (cstate-flags state)
               (remq item (cstate-inventory state))
               (cstate-meta state)))

;;; Alist helper
(define (alist-set alist key value)
  (doc 'export #t)
  (let ([p (assq key alist)])
       (if p
           (cons (cons key value) (remq p alist))
           (cons (cons key value) alist))))

(doc 'section 'guards)
(doc 'note "A guard is a function: State -> Boolean")
(doc 'note "Guards compose with combinators")

(define (guard-always) (lambda (s) #t))
(define (guard-never) (lambda (s) #f))

(define (guard-flag flag)
  (doc 'export #t)
  (lambda (s) (state-flag? s flag)))

(define (guard-not-flag flag)
  (doc 'export #t)
  (lambda (s) (not (state-flag? s flag))))

(define (guard-has-item item)
  (doc 'export #t)
  (lambda (s) (state-has-item? s item)))

(define (guard-var-eq key value)
  (doc 'export #t)
  (lambda (s) (equal? (state-get s key #f) value)))

(define (guard-var>= key threshold)
  (doc 'export #t)
  (lambda (s)
          (let ([v (state-get s key 0)])
               (and (number? v) (>= v threshold)))))

(define (guard-var<= key threshold)
  (doc 'export #t)
  (lambda (s)
          (let ([v (state-get s key 0)])
               (and (number? v) (<= v threshold)))))

;;; Guard combinators
(define (guard-and . guards)
  (doc 'export #t)
  (lambda (s)
          (let loop ([gs guards])
               (or (null? gs)
                   (and ((car gs) s)
                        (loop (cdr gs)))))))

(define (guard-or . guards)
  (doc 'export #t)
  (lambda (s)
          (let loop ([gs guards])
               (and (not (null? gs))
                    (or ((car gs) s)
                        (loop (cdr gs)))))))

(define (guard-not g)
  (doc 'export #t)
  (lambda (s) (not (g s))))

;;; Evaluate a guard (for compatibility with data-based guards)
(define (guard-check guard state)
  (doc 'export #t)
  (cond
   [(procedure? guard) (guard state)]
   [(eq? guard #t) #t]
   [(eq? guard #f) #f]
   [else #t]))

;;; ====
;;; Effects - State Transformations
;;; ====

;;; An effect is a function: State -> State
;;; Effects compose sequentially

(define (effect-none) (lambda (s) s))

(define (effect-set key value)
  (doc 'export #t)
  (lambda (s) (state-set s key value)))

(define (effect-inc key delta)
  (doc 'export #t)
  (lambda (s)
          (let ([v (state-get s key 0)])
               (state-set s key (+ v delta)))))

(define (effect-flag flag)
  (doc 'export #t)
  (lambda (s) (state-flag! s flag)))

(define (effect-unflag flag)
  (doc 'export #t)
  (lambda (s) (state-unflag! s flag)))

(define (effect-add-item item)
  (doc 'export #t)
  (lambda (s) (state-add-item s item)))

(define (effect-remove-item item)
  (doc 'export #t)
  (lambda (s) (state-remove-item s item)))

;;; Effect combinators
(define (effect-seq . effects)
  (doc 'export #t)
  (lambda (s)
          (let loop ([st s] [effs effects])
               (if (null? effs)
                   st
                   (loop ((car effs) st) (cdr effs))))))

(define (effect-when guard effect)
  (doc 'export #t)
  (lambda (s)
          (if (guard-check guard s)
              (effect s)
              s)))

;;; Apply effect to state
(define (apply-effect effect state)
  (doc 'export #t)
  (if (procedure? effect)
      (effect state)
      state))

(define (apply-effects effects state)
  (doc 'export #t)
  (let loop ([s state] [effs effects])
       (if (null? effs)
           s
           (loop (apply-effect (car effs) s) (cdr effs)))))

;;; ====
;;; Templates - Text with State Interpolation
;;; ====

;;; Templates use quasiquotation for dynamic text.
;;; A template is either:
;;;   - A string literal
;;;   - A thunk: State -> String
;;;   - A template expression: (template expr)

;;; Simple string interpolation using ${key} syntax
;;; Expands ${key} to (state-get state 'key "")
(define (expand-template-string str state)
  (doc 'export #t)
  (let loop ([chars (string->list str)] [result '()] [in-var #f] [var-chars '()])
       (cond
        [(null? chars)
         (if in-var
             ;; Unterminated ${
             (list->string (reverse (append var-chars (cons #\{ (cons #\$ result)))))
             (list->string (reverse result)))]
        [(and (not in-var) (char=? (car chars) #\$)
              (pair? (cdr chars)) (char=? (cadr chars) #\{))
         (loop (cddr chars) result #t '())]
        [(and in-var (char=? (car chars) #\}))
         (let* ([var-name (string->symbol (list->string (reverse var-chars)))]
                [value (state-get state var-name "")]
                [value-str (if (string? value) value (format "~a" value))])
               (loop (cdr chars)
                     (append (reverse (string->list value-str)) result)
                     #f '()))]
        [in-var
         (loop (cdr chars) result #t (cons (car chars) var-chars))]
        [else
         (loop (cdr chars) (cons (car chars) result) #f '())])))

;;; Template evaluation
(define (eval-template template state)
  (doc 'export #t)
  (cond
   [(string? template)
    (expand-template-string template state)]
   [(procedure? template)
    (template state)]
   [else
    (format "~a" template)]))

;;; ====
;;; Chronicle Lookup
;;; ====

(define (chronicle-scene chronicle sid)
  (doc 'export #t)
  (let ([scenes (chronicle-scenes chronicle)])
       (let loop ([ss scenes])
            (cond
             [(null? ss) #f]
             [(eq? (scene-id (car ss)) sid) (car ss)]
             [else (loop (cdr ss))]))))

(define (chronicle-has-scene? chronicle sid)
  (doc 'export #t)
  (and (chronicle-scene chronicle sid) #t))

;;; ====
;;; Runtime State
;;; ====

;;; A Run tracks the current playthrough
(define-record-type crun%
  (fields chronicle-id scene-id state transcript done? message))

(define make-crun make-crun%)
(define crun? crun%?)
(define crun-chronicle-id crun%-chronicle-id)
(define crun-scene-id crun%-scene-id)
(define crun-state crun%-state)
(define crun-transcript crun%-transcript)
(define crun-done? crun%-done?)
(define crun-message crun%-message)

;;; Run transformers (functional updates)
(define (crun-with-scene run scene-id)
  (doc 'export #t)
  (make-crun (crun-chronicle-id run) scene-id
             (crun-state run) (crun-transcript run)
             (crun-done? run) (crun-message run)))

(define (crun-with-state run state)
  (doc 'export #t)
  (make-crun (crun-chronicle-id run) (crun-scene-id run)
             state (crun-transcript run)
             (crun-done? run) (crun-message run)))

(define (crun-with-done run done?)
  (doc 'export #t)
  (make-crun (crun-chronicle-id run) (crun-scene-id run)
             (crun-state run) (crun-transcript run)
             done? (crun-message run)))

(define (crun-with-message run msg)
  (doc 'export #t)
  (make-crun (crun-chronicle-id run) (crun-scene-id run)
             (crun-state run) (crun-transcript run)
             (crun-done? run) msg))

(define (crun-append-transcript run entry)
  (doc 'export #t)
  (make-crun (crun-chronicle-id run) (crun-scene-id run)
             (crun-state run) (cons entry (crun-transcript run))
             (crun-done? run) (crun-message run)))

;;; ====
;;; Runtime Engine
;;; ====

;;; Start a new run (enters the first scene, applying on-enter effects)
(define (chronicle-start chronicle . state-opt)
  (doc 'export #t)
  (let* ([state (if (null? state-opt) (empty-state) (car state-opt))]
         [run (make-crun (chronicle-id chronicle)
                         (chronicle-start-scene chronicle)
                         state
                         '()
                         #f
                         #f)])
        ;; Enter the first scene to apply on-enter effects
        (chronicle-enter-scene chronicle run)))

;;; Enter a scene (applies on-enter effects)
(define (chronicle-enter-scene chronicle run)
  (doc 'export #t)
  (let ([scene (chronicle-scene chronicle (crun-scene-id run))])
       (if (not scene)
           (crun-with-message run (format "Missing scene: ~a" (crun-scene-id run)))
           (let* ([run1 (crun-with-message run #f)]
                  [effects (scene-on-enter scene)]
                  [state1 (apply-effects (or effects '()) (crun-state run1))])
                 (crun-with-state run1 state1)))))

;;; Get visible choices for current scene
(define (chronicle-visible-choices chronicle run)
  (doc 'export #t)
  (let ([scene (chronicle-scene chronicle (crun-scene-id run))])
       (if (not scene)
           '()
           (filter (lambda (c)
                           (guard-check (choice-guard c) (crun-state run)))
                   (scene-choices scene)))))

;;; Make a choice
(define (chronicle-choose chronicle run choice-num)
  (doc 'export #t)
  (let ([choices (chronicle-visible-choices chronicle run)])
       (cond
        [(or (< choice-num 1) (> choice-num (length choices)))
         (crun-with-message run "Invalid choice.")]
        [else
         (let* ([choice (list-ref choices (- choice-num 1))]
                [target (choice-target choice)]
                [effects (choice-effects choice)]
                [run1 (crun-with-message run #f)]
                ;; Apply choice effects
                [state1 (apply-effects (or effects '()) (crun-state run1))]
                [run2 (crun-with-state run1 state1)]
                ;; Move to target scene
                [run3 (if (chronicle-has-scene? chronicle target)
                          (crun-with-scene run2 target)
                          (crun-with-message run2 (format "Missing scene: ~a" target)))]
                ;; Enter new scene
                [run4 (if (crun-message run3)
                          run3
                          (chronicle-enter-scene chronicle run3))])
               run4)])))

;;; Render current scene
(define (chronicle-render chronicle run)
  (doc 'export #t)
  (let ([scene (chronicle-scene chronicle (crun-scene-id run))])
       (if (not scene)
           (format "Error: Scene ~a not found" (crun-scene-id run))
           (let* ([state (crun-state run)]
                  [text (eval-template (scene-text scene) state)]
                  [choices (chronicle-visible-choices chronicle run)]
                  [choice-strs (map (lambda (c i)
                                            (format "  ~a. ~a"
                                                    (+ i 1)
                                                    (eval-template (choice-label c) state)))
                                    choices
                                    (iota (length choices)))])
                 (string-append
                  text "\n\n"
                  (if (null? choice-strs)
                      "[No choices available]"
                      (apply string-append
                             (intersperse "\n" choice-strs))))))))

;; iota and intersperse are provided by prelude and combinators.ss respectively

;;; ====
;;; DSL Constructors (S-expression Based)
;;; ====

;;; These constructors create Chronicle structures from S-expressions.
;;; Later, parser combinators will generate these from text syntax.

;;; Build a scene from spec
;;; (scene-spec 'id "text" ((choice-spec ...) ...) on-enter on-exit meta)
(define (build-scene id text choices on-enter on-exit meta)
  (doc 'export #t)
  (make-scene id text choices on-enter on-exit meta))

;;; Build a choice from spec
(define (build-choice label target guard effects meta)
  (doc 'export #t)
  (make-choice label target guard effects meta))

;;; Build a chronicle from specs
(define (build-chronicle id title scenes start meta)
  (doc 'export #t)
  (make-chronicle id title scenes start meta))

;;; ====
;;; DSL Macros (High-Level Syntax)
;;; ====

;;; Macro: define-chronicle
;;; Usage:
;;;   (define-chronicle my-story "My Story"
;;;     (start 'intro)
;;;     (scene 'intro
;;;       "Welcome to the story."
;;;       (-> "Continue" 'next))
;;;     (scene 'next
;;;       "The end."
;;;       (-> "Finish" 'ending :when (flag? 'ready))))

;;; Parse scene definition
(define (parse-scene-def def)
  (doc 'export #t)
  (let* ([id (cadr def)]
         [rest (cddr def)]
         [text (car rest)]
         [choices-and-meta (cdr rest)])
        ;; Separate choices from metadata
        (let loop ([items choices-and-meta]
                   [choices '()]
                   [on-enter '()]
                   [on-exit '()]
                   [meta '()])
             (if (null? items)
                 (build-scene id text (reverse choices) on-enter on-exit meta)
                 (let ([item (car items)])
                      (cond
                       [(and (pair? item) (eq? (car item) '->))
                        (loop (cdr items)
                              (cons (parse-choice-def item) choices)
                              on-enter on-exit meta)]
                       [(and (pair? item) (eq? (car item) 'on-enter))
                        (loop (cdr items) choices
                              (append on-enter (map parse-effect-expr (cdr item)))
                              on-exit meta)]
                       [(and (pair? item) (eq? (car item) 'on-exit))
                        (loop (cdr items) choices
                              on-enter
                              (append on-exit (map parse-effect-expr (cdr item)))
                              meta)]
                       [else
                        (loop (cdr items) choices on-enter on-exit
                              (cons item meta))]))))))

;;; Parse choice definition
;;; (-> "label" 'target :when guard :do effect ...)
(define (parse-choice-def def)
  (doc 'export #t)
  (let* ([label (cadr def)]
         [target (caddr def)]
         [rest (cdddr def)])
        (let loop ([items rest] [guard (guard-always)] [effects '()] [meta '()])
             (if (null? items)
                 (build-choice label target guard (reverse effects) meta)
                 (let ([item (car items)])
                      (cond
                       [(eq? item ':when)
                        (loop (cddr items)
                              (parse-guard-expr (cadr items))
                              effects meta)]
                       [(eq? item ':do)
                        (loop (cddr items)
                              guard
                              (cons (parse-effect-expr (cadr items)) effects)
                              meta)]
                       [else
                        (loop (cdr items) guard effects (cons item meta))]))))))

;;; Parse guard expression
(define (parse-guard-expr expr)
  (doc 'export #t)
  (cond
   [(eq? expr #t) (guard-always)]
   [(eq? expr #f) (guard-never)]
   [(and (pair? expr) (eq? (car expr) 'flag?))
    (guard-flag (cadr expr))]
   [(and (pair? expr) (eq? (car expr) 'not-flag?))
    (guard-not-flag (cadr expr))]
   [(and (pair? expr) (eq? (car expr) 'has-item?))
    (guard-has-item (cadr expr))]
   [(and (pair? expr) (eq? (car expr) 'var=))
    (guard-var-eq (cadr expr) (caddr expr))]
   [(and (pair? expr) (eq? (car expr) 'var>=))
    (guard-var>= (cadr expr) (caddr expr))]
   [(and (pair? expr) (eq? (car expr) 'var<=))
    (guard-var<= (cadr expr) (caddr expr))]
   [(and (pair? expr) (eq? (car expr) 'and))
    (apply guard-and (map parse-guard-expr (cdr expr)))]
   [(and (pair? expr) (eq? (car expr) 'or))
    (apply guard-or (map parse-guard-expr (cdr expr)))]
   [(and (pair? expr) (eq? (car expr) 'not))
    (guard-not (parse-guard-expr (cadr expr)))]
   [else (guard-always)]))

;;; Parse effect expression
(define (parse-effect-expr expr)
  (doc 'export #t)
  (cond
   [(and (pair? expr) (eq? (car expr) 'set!))
    (effect-set (cadr expr) (caddr expr))]
   [(and (pair? expr) (eq? (car expr) 'inc!))
    (effect-inc (cadr expr) (caddr expr))]
   [(and (pair? expr) (eq? (car expr) 'flag!))
    (effect-flag (cadr expr))]
   [(and (pair? expr) (eq? (car expr) 'unflag!))
    (effect-unflag (cadr expr))]
   [(and (pair? expr) (eq? (car expr) 'add-item!))
    (effect-add-item (cadr expr))]
   [(and (pair? expr) (eq? (car expr) 'remove-item!))
    (effect-remove-item (cadr expr))]
   [(and (pair? expr) (eq? (car expr) 'seq))
    (apply effect-seq (map parse-effect-expr (cdr expr)))]
   [else (effect-none)]))

;;; Parse chronicle definition
(define (parse-chronicle-def def)
  (doc 'export #t)
  (let* ([name (cadr def)]
         [title (caddr def)]
         [body (cdddr def)])
        (let loop ([items body] [scenes '()] [start #f] [meta '()])
             (if (null? items)
                 (build-chronicle name title (reverse scenes)
                                  (or start (and (pair? scenes) (scene-id (car (reverse scenes)))))
                                  meta)
                 (let ([item (car items)])
                      (cond
                       [(and (pair? item) (eq? (car item) 'start))
                        (loop (cdr items) scenes (cadr item) meta)]
                       [(and (pair? item) (eq? (car item) 'scene))
                        (loop (cdr items)
                              (cons (parse-scene-def item) scenes)
                              start meta)]
                       [(and (pair? item) (eq? (car item) 'meta))
                        (loop (cdr items) scenes start
                              (append meta (cdr item)))]
                       [else
                        (loop (cdr items) scenes start (cons item meta))]))))))

;;; Top-level macro
(define-syntax define-chronicle
  (syntax-rules ()
                [(_ name title body ...)
                 (define name
                   (parse-chronicle-def '(chronicle name title body ...)))]))

;;; ====
;;; Exports Summary
;;; ====
;;;
;;; Types:
;;;   chronicle, scene, choice, cstate, crun
;;;
;;; State:
;;;   empty-state, state-get, state-set, state-flag?, state-set-flag,
;;;   state-flag!, state-unflag!, state-has-item?, state-add-item,
;;;   state-remove-item
;;;
;;; Guards:
;;;   guard-always, guard-never, guard-flag, guard-not-flag,
;;;   guard-has-item, guard-var-eq, guard-var>=, guard-var<=,
;;;   guard-and, guard-or, guard-not, guard-check
;;;
;;; Effects:
;;;   effect-none, effect-set, effect-inc, effect-flag, effect-unflag,
;;;   effect-add-item, effect-remove-item, effect-seq, effect-when,
;;;   apply-effect, apply-effects
;;;
;;; Templates:
;;;   expand-template-string, eval-template
;;;
;;; Runtime:
;;;   chronicle-start, chronicle-enter-scene, chronicle-visible-choices,
;;;   chronicle-choose, chronicle-render
;;;
;;; DSL:
;;;   define-chronicle, parse-chronicle-def, parse-scene-def,
;;;   parse-choice-def, parse-guard-expr, parse-effect-expr
