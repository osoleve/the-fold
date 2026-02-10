;;; @module statechart
;;; @requires prelude combinators

(require 'prelude)
(require 'combinators)

(doc 'module 'statechart)
(doc 'description "Hierarchical state machine (statechart) implementation following Harel's statechart semantics with extensions for simulation")
(doc 'features "Hierarchical/nested states, parallel/orthogonal regions, entry/exit actions, guard conditions, event-driven transitions, history states, DSL syntax, interpreter/executor, validation and reachability analysis")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'section 'state-types)
(doc 'description "States can be: Atomic (simple), Composite (hierarchical), Parallel (concurrent), History (previous state reference), Initial (default entry), Final (terminal)")

(doc 'note "State record: (state <id> <type> <entry-action> <exit-action> <substates> <transitions> <parent> <data>)")

(define (make-state id type entry-action exit-action substates transitions parent data)
  (doc 'export #t)
  (doc 'type '(-> Symbol Symbol (Option Action) (Option Action) (List State) (List Transition) (Option Symbol) (List α) State))
  (list 'state id type entry-action exit-action substates transitions parent data))

(define (state? x)
  (doc 'export #t)
  (doc 'type '(-> α Boolean))
  (and (list? x)
       (>= (length x) 9)
       (eq? (car x) 'state)))

(define (state-id s)
  (doc 'export #t)
  (doc 'type '(-> State Symbol))
  (list-ref s 1))

(define (state-type s)
  (doc 'export #t)
  (doc 'type '(-> State Symbol))
  (list-ref s 2))

(define (state-entry-action s)
  (doc 'export #t)
  (doc 'type '(-> State (Option Action)))
  (list-ref s 3))

(define (state-exit-action s)
  (doc 'export #t)
  (doc 'type '(-> State (Option Action)))
  (list-ref s 4))

(define (state-substates s)
  (doc 'export #t)
  (doc 'type '(-> State (List State)))
  (list-ref s 5))

(define (state-transitions s)
  (doc 'export #t)
  (doc 'type '(-> State (List Transition)))
  (list-ref s 6))

(define (state-parent s)
  (doc 'export #t)
  (doc 'type '(-> State (Option Symbol)))
  (list-ref s 7))

(define (state-data s)
  (doc 'export #t)
  (doc 'type '(-> State (List α)))
  (list-ref s 8))

(doc 'section 'state-predicates)

(define (atomic-state? s)
  (doc 'export #t)
  (doc 'type '(-> α Boolean))
  (and (state? s) (eq? (state-type s) 'atomic)))

(define (composite-state? s)
  (doc 'export #t)
  (doc 'type '(-> α Boolean))
  (and (state? s) (eq? (state-type s) 'composite)))

(define (parallel-state? s)
  (doc 'export #t)
  (doc 'type '(-> α Boolean))
  (and (state? s) (eq? (state-type s) 'parallel)))

(define (history-state? s)
  (doc 'export #t)
  (doc 'type '(-> α Boolean))
  (and (state? s)
       (or (eq? (state-type s) 'history-shallow)
           (eq? (state-type s) 'history-deep))))

(define (shallow-history-state? s)
  (doc 'export #t)
  (doc 'type '(-> α Boolean))
  (and (state? s) (eq? (state-type s) 'history-shallow)))

(define (deep-history-state? s)
  (doc 'export #t)
  (doc 'type '(-> α Boolean))
  (and (state? s) (eq? (state-type s) 'history-deep)))

(define (initial-state? s)
  (doc 'export #t)
  (doc 'type '(-> α Boolean))
  (and (state? s) (eq? (state-type s) 'initial)))

(define (final-state? s)
  (doc 'export #t)
  (doc 'type '(-> α Boolean))
  (and (state? s) (eq? (state-type s) 'final)))

;;; --- Simple State Constructors ---

;;; atomic : Symbol → State
;;; Create an atomic (simple) state.
(define (atomic id)
  (doc 'export #t)
  (make-state id 'atomic #f #f '() '() #f '()))

;;; atomic-with : Symbol × Action × Action → State
;;; Create an atomic state with entry/exit actions.
(define (atomic-with id entry exit)
  (doc 'export #t)
  (make-state id 'atomic entry exit '() '() #f '()))

;;; composite : Symbol × (List State) → State
;;; Create a composite state containing substates.
(define (composite id substates)
  (doc 'export #t)
  (let* ([substates-with-parent
          (map (lambda (s) (set-state-parent s id)) substates)])
        (make-state id 'composite #f #f substates-with-parent '() #f '())))

;;; composite-with : Symbol × (List State) × Action × Action → State
;;; Create a composite state with entry/exit actions.
(define (composite-with id substates entry exit)
  (doc 'export #t)
  (let* ([substates-with-parent
          (map (lambda (s) (set-state-parent s id)) substates)])
        (make-state id 'composite entry exit substates-with-parent '() #f '())))

;;; parallel : Symbol × (List State) → State
;;; Create a parallel state with orthogonal regions.
;;; Each region is itself a composite state.
(define (parallel id regions)
  (doc 'export #t)
  (let* ([regions-with-parent
          (map (lambda (r) (set-state-parent r id)) regions)])
        (make-state id 'parallel #f #f regions-with-parent '() #f '())))

;;; parallel-with : Symbol × (List State) × Action × Action → State
;;; Create a parallel state with entry/exit actions.
(define (parallel-with id regions entry exit)
  (doc 'export #t)
  (let* ([regions-with-parent
          (map (lambda (r) (set-state-parent r id)) regions)])
        (make-state id 'parallel entry exit regions-with-parent '() #f '())))

;;; region : Symbol × (List State) → State
;;; Create a region (used within parallel states).
(define (region id substates)
  (doc 'export #t)
  (composite id substates))

;;; Counter for generating unique initial state IDs
(define *initial-counter* 0)

;;; initial : Symbol → State
;;; Create an initial pseudo-state.
(define (initial target-id)
  (doc 'export #t)
  (set! *initial-counter* (+ *initial-counter* 1))
  (let ([id (string->symbol
             (string-append "__initial__"
                            (number->string *initial-counter*)))])
       (make-state id 'initial #f #f '()
                   (list (make-transition #f target-id #f #f))
                   #f
                   (list (cons 'target target-id)))))

;;; final : Symbol → State
;;; Create a final pseudo-state.
(define (final id)
  (doc 'export #t)
  (make-state id 'final #f #f '() '() #f '()))

;;; history-shallow : Symbol → State
;;; Create a shallow history pseudo-state.
(define (history-shallow id)
  (doc 'export #t)
  (make-state id 'history-shallow #f #f '() '() #f '()))

;;; history-deep : Symbol → State
;;; Create a deep history pseudo-state.
(define (history-deep id)
  (doc 'export #t)
  (make-state id 'history-deep #f #f '() '() #f '()))

;;; --- State Modifiers ---

;;; set-state-parent : State × Symbol → State
(define (set-state-parent s parent-id)
  (doc 'export #t)
  (make-state (state-id s) (state-type s)
              (state-entry-action s) (state-exit-action s)
              (state-substates s) (state-transitions s)
              parent-id (state-data s)))

;;; add-transition : State × Transition → State
(define (add-transition s trans)
  (doc 'export #t)
  (make-state (state-id s) (state-type s)
              (state-entry-action s) (state-exit-action s)
              (state-substates s)
              (cons trans (state-transitions s))
              (state-parent s) (state-data s)))

;;; set-entry-action : State × Action → State
(define (set-entry-action s action)
  (doc 'export #t)
  (make-state (state-id s) (state-type s)
              action (state-exit-action s)
              (state-substates s) (state-transitions s)
              (state-parent s) (state-data s)))

;;; set-exit-action : State × Action → State
(define (set-exit-action s action)
  (doc 'export #t)
  (make-state (state-id s) (state-type s)
              (state-entry-action s) action
              (state-substates s) (state-transitions s)
              (state-parent s) (state-data s)))

;;; Load zipper-based navigation (after state types are defined)
;;; This provides efficient O(depth) ancestor/LCA operations.
(load "lattice/automata/statechart-zipper.ss")

;;; ====
;;; Transition Types
;;; ====
;;;
;;; A transition is:
;;;   (transition <event> <target> <guard> <action>)
;;;
;;; - event: Symbol or #f for eventless transitions
;;; - target: Symbol (state id) or #f for internal transitions
;;; - guard: (Context -> Boolean) or #f
;;; - action: (Context Event -> Context) or #f

;;; make-transition : (Option Symbol) × (Option Symbol) × (Option Guard) × (Option Action) → Transition
(define (make-transition event target guard action)
  (doc 'export #t)
  (list 'transition event target guard action))

;;; transition? : α → Boolean
(define (transition? x)
  (doc 'export #t)
  (and (list? x)
       (= (length x) 5)
       (eq? (car x) 'transition)))

;;; transition-event : Transition → (Option Symbol)
(define (transition-event t) (list-ref t 1))
(doc 'export #t)
;;; transition-target : Transition → (Option Symbol)
(define (transition-target t) (list-ref t 2))
(doc 'export #t)
;;; transition-guard : Transition → (Option Guard)
(define (transition-guard t) (list-ref t 3))
(doc 'export #t)
;;; transition-action : Transition → (Option Action)
(define (transition-action t) (list-ref t 4))
(doc 'export #t)

;;; Transition predicates
;;; internal-transition? : α → Boolean
(define (internal-transition? t)
  (doc 'export #t)
  (and (transition? t) (not (transition-target t))))

;;; external-transition? : α → Boolean
(define (external-transition? t)
  (doc 'export #t)
  (and (transition? t) (transition-target t)))

;;; eventless-transition? : α → Boolean
(define (eventless-transition? t)
  (doc 'export #t)
  (and (transition? t) (not (transition-event t))))

;;; guarded-transition? : α → Boolean
(define (guarded-transition? t)
  (doc 'export #t)
  (and (transition? t) (transition-guard t)))

;;; --- Transition Constructors ---

;;; on : Symbol × Symbol → Transition
;;; Simple transition: on event, go to target.
(define (on event target)
  (doc 'export #t)
  (make-transition event target #f #f))

;;; on-do : Symbol × Symbol × Action → Transition
;;; Transition with action.
(define (on-do event target action)
  (doc 'export #t)
  (make-transition event target #f action))

;;; on-guard : Symbol × Symbol × Guard → Transition
;;; Guarded transition.
(define (on-guard event target guard)
  (doc 'export #t)
  (make-transition event target guard #f))

;;; on-guard-do : Symbol × Symbol × Guard × Action → Transition
;;; Fully specified transition.
(define (on-guard-do event target guard action)
  (doc 'export #t)
  (make-transition event target guard action))

;;; always : Symbol → Transition
;;; Eventless (automatic) transition.
(define (always target)
  (doc 'export #t)
  (make-transition #f target #f #f))

;;; always-guard : Symbol × Guard → Transition
;;; Guarded eventless transition.
(define (always-guard target guard)
  (doc 'export #t)
  (make-transition #f target guard #f))

;;; internal : Symbol × Action → Transition
;;; Internal transition (no state change).
(define (internal event action)
  (doc 'export #t)
  (make-transition event #f #f action))

;;; internal-guard : Symbol × Guard × Action → Transition
;;; Guarded internal transition.
(define (internal-guard event guard action)
  (doc 'export #t)
  (make-transition event #f guard action))

;;; ====
;;; Events
;;; ====
;;;
;;; An event is: (event <type> <payload>)

;;; make-event : Symbol × α → Event
(define (make-event type payload)
  (doc 'export #t)
  (list 'event type payload))

;;; event? : α → Boolean
(define (event? x)
  (doc 'export #t)
  (and (list? x)
       (= (length x) 3)
       (eq? (car x) 'event)))

;;; event-type : Event → Symbol
(define (event-type e) (list-ref e 1))
(doc 'export #t)
;;; event-payload : Event → α
(define (event-payload e) (list-ref e 2))
(doc 'export #t)

;;; evt : Symbol → Event
;;; Create a simple event with no payload.
(define (evt type)
  (doc 'export #t)
  (make-event type '()))

;;; evt-data : Symbol × α → Event
;;; Create an event with payload data.
(define (evt-data type data)
  (doc 'export #t)
  (make-event type data))

;;; ====
;;; Statechart
;;; ====
;;;
;;; A statechart is the top-level container:
;;;   (statechart <id> <root-state> <context> <history>)
;;;
;;; - root-state: the outermost state (usually composite)
;;; - context: user-defined context data
;;; - history: map of state-id -> previously active substates

;;; make-statechart : Symbol × State × α → Statechart
(define (make-statechart id root-state context)
  (doc 'export #t)
  (list 'statechart id root-state context (make-history)))

;;; statechart? : α → Boolean
(define (statechart? x)
  (doc 'export #t)
  (and (list? x)
       (= (length x) 5)
       (eq? (car x) 'statechart)))

;;; statechart-id : Statechart → Symbol
(define (statechart-id sc) (list-ref sc 1))
(doc 'export #t)
;;; statechart-root : Statechart → State
(define (statechart-root sc) (list-ref sc 2))
(doc 'export #t)
;;; statechart-context : Statechart → α
(define (statechart-context sc) (list-ref sc 3))
(doc 'export #t)
;;; statechart-history : Statechart → History
(define (statechart-history sc) (list-ref sc 4))
(doc 'export #t)

;;; Update statechart components
;;; set-statechart-context : Statechart × α → Statechart
(define (set-statechart-context sc ctx)
  (doc 'export #t)
  (list 'statechart (statechart-id sc) (statechart-root sc) ctx (statechart-history sc)))

;;; set-statechart-history : Statechart × History → Statechart
(define (set-statechart-history sc hist)
  (doc 'export #t)
  (list 'statechart (statechart-id sc) (statechart-root sc) (statechart-context sc) hist))

;;; --- History Management ---

;;; make-history : → History
(define (make-history)
  (doc 'export #t)
  '())

;;; history-get : History × Symbol → (Option (List Symbol))
(define (history-get hist state-id)
  (doc 'export #t)
  (let ([entry (assoc state-id hist)])
       (if entry (cdr entry) #f)))

;;; history-set : History × Symbol × (List Symbol) → History
(define (history-set hist state-id active-substates)
  (doc 'export #t)
  (cons (cons state-id active-substates)
        (filter (lambda (e) (not (eq? (car e) state-id))) hist)))

;;; ====
;;; Statechart DSL
;;; ====
;;;
;;; The DSL provides a declarative way to define statecharts.
;;;
;;; Example:
;;; (define-statechart traffic-light
;;;   (composite 'running
;;;     (list
;;;       (initial 'red)
;;;       (atomic 'red)
;;;       (atomic 'yellow)
;;;       (atomic 'green)))
;;;   (transitions
;;;     ('red 'timer -> 'green)
;;;     ('green 'timer -> 'yellow)
;;;     ('yellow 'timer -> 'red)))

;;; statechart : Symbol × State → Statechart
;;; Create a statechart with default context.
(define (statechart id root-state)
  (doc 'export #t)
  (make-statechart id (resolve-initial-states root-state) '()))

;;; statechart-ctx : Symbol × State × α → Statechart
;;; Create a statechart with initial context.
(define (statechart-ctx id root-state context)
  (doc 'export #t)
  (make-statechart id (resolve-initial-states root-state) context))

;;; define-statechart macro simulation
;;; Since we're in pure Scheme, we use a function-based builder.

;;; with-transitions : State × (List (List Symbol)) → State
;;; Add transitions to a state based on source/event/target triples.
(define (with-transitions s trans-specs)
  (doc 'export #t)
  (let loop ([state s] [specs trans-specs])
       (if (null? specs)
           state
           (let* ([spec (car specs)]
                  [source (car spec)]
                  [event (cadr spec)]
                  [target (caddr spec)]
                  [guard (if (> (length spec) 3) (list-ref spec 3) #f)]
                  [action (if (> (length spec) 4) (list-ref spec 4) #f)]
                  [trans (make-transition event target guard action)])
                 (loop (add-transition-to-substate state source trans)
                       (cdr specs))))))

;;; add-transition-to-substate : State × Symbol × Transition → State
;;; Add a transition to a substate identified by id.
(define (add-transition-to-substate state source-id trans)
  (doc 'export #t)
  (cond
   [(eq? (state-id state) source-id)
    (add-transition state trans)]
   [(or (composite-state? state) (parallel-state? state))
    (make-state (state-id state) (state-type state)
                (state-entry-action state) (state-exit-action state)
                (map (lambda (sub)
                             (add-transition-to-substate sub source-id trans))
                     (state-substates state))
                (state-transitions state)
                (state-parent state) (state-data state))]
   [else state]))

;;; resolve-initial-states : State → State
;;; Ensure initial pseudo-states are properly linked.
(define (resolve-initial-states state)
  (doc 'export #t)
  (cond
   [(or (atomic-state? state) (history-state? state)
        (initial-state? state) (final-state? state))
    state]
   [(composite-state? state)
    (make-state (state-id state) (state-type state)
                (state-entry-action state) (state-exit-action state)
                (map resolve-initial-states (state-substates state))
                (state-transitions state)
                (state-parent state) (state-data state))]
   [(parallel-state? state)
    (make-state (state-id state) (state-type state)
                (state-entry-action state) (state-exit-action state)
                (map resolve-initial-states (state-substates state))
                (state-transitions state)
                (state-parent state) (state-data state))]
   [else state]))

;;; ====
;;; State Lookup and Navigation
;;; ====

;;; find-state : State × Symbol → (Option State)
;;; Find a state by id within a state hierarchy.
(define (find-state root id)
  (doc 'export #t)
  (cond
   [(eq? (state-id root) id) (just root)]
   [(or (composite-state? root) (parallel-state? root))
    (find-state-in-list (state-substates root) id)]
   [else nothing]))

;;; find-state-in-list : (List State) × Symbol → (Option State)
(define (find-state-in-list states id)
  (doc 'export #t)
  (if (null? states)
      nothing
      (let ([found (find-state (car states) id)])
           (if (just? found)
               found
               (find-state-in-list (cdr states) id)))))

;;; get-initial-substate : State → (Option State)
;;; Get the initial substate of a composite state.
(define (get-initial-substate state)
  (doc 'export #t)
  (if (or (composite-state? state) (parallel-state? state))
      (let ([initial (find-if initial-state? (state-substates state))])
           (if initial
               (let ([target-id (cdr (assoc 'target (state-data initial)))])
                    (find-state state target-id))
               ;; Default to first non-pseudo state
               (let ([first-real (find-if (lambda (s)
                                                  (not (or (initial-state? s)
                                                           (history-state? s))))
                                          (state-substates state))])
                    (if first-real (just first-real) nothing))))
      nothing))

;;; get-ancestors : State × Symbol → (List State)
;;; Get all ancestor states from root down to immediate parent (outermost first).
;;; Uses zipper for O(depth) performance via crumbs.
(define (get-ancestors root child-id)
  (doc 'export #t)
  (let* ([sz (state->zipper root)]
         [found (state-zipper-find sz child-id)])
    (if (nothing? found)
        '()
        ;; state-zipper-ancestors returns parent-to-root (innermost first)
        ;; But original API was root-to-parent (outermost first), so reverse
        (reverse (state-zipper-ancestors (from-just found))))))

;;; lca : State × Symbol × Symbol → (Option State)
;;; Find the Lowest Common Ancestor of two states.
;;; Uses zipper for efficient path extraction and comparison.
(define (lca root id1 id2)
  (doc 'export #t)
  (let ([sz (state->zipper root)])
    (state-zipper-lca sz id1 id2)))

;;; is-descendant? : State × Symbol × Symbol → Boolean
;;; Check if child-id is a descendant of parent-id.
;;; Uses zipper for efficient ancestor lookup.
(define (is-descendant? root parent-id child-id)
  (doc 'export #t)
  (let ([sz (state->zipper root)])
    (state-zipper-is-descendant? sz parent-id child-id)))

;;; ====
;;; Configuration (Active State Set)
;;; ====
;;;
;;; A configuration is the set of currently active states.
;;; For flat machines: single state.
;;; For hierarchical: active state plus all ancestors.
;;; For parallel: multiple active states per region.

;;; make-configuration : (List Symbol) → Configuration
(define (make-configuration active-states)
  (list 'configuration active-states))

;;; configuration? : α → Boolean
(define (configuration? x)
  (and (list? x)
       (= (length x) 2)
       (eq? (car x) 'configuration)))

;;; configuration-states : Configuration → (List Symbol)
(define (configuration-states cfg)
  (list-ref cfg 1))

;;; configuration-add : Configuration × Symbol → Configuration
(define (configuration-add cfg state-id)
  (make-configuration
   (if (member state-id (configuration-states cfg))
       (configuration-states cfg)
       (cons state-id (configuration-states cfg)))))

;;; configuration-remove : Configuration × Symbol → Configuration
(define (configuration-remove cfg state-id)
  (make-configuration
   (filter (lambda (id) (not (eq? id state-id)))
           (configuration-states cfg))))

;;; configuration-contains? : Configuration × Symbol → Boolean
(define (configuration-contains? cfg state-id)
  (if (member state-id (configuration-states cfg)) #t #f))

;;; configuration-empty : → Configuration
(define (configuration-empty)
  (make-configuration '()))

;;; enter-initial : State × Symbol → Configuration
;;; Enter a state and all required substates for initial configuration.
(define (enter-initial root sid)
  (let ([maybe-state (find-state root sid)])
       (if (nothing? maybe-state)
           (configuration-empty)
           (let ([state (from-just maybe-state)])
                (cond
                 [(atomic-state? state)
                  (make-configuration (list sid))]
                 [(final-state? state)
                  (make-configuration (list sid))]
                 [(composite-state? state)
                  (let ([init (get-initial-substate state)])
                       (if (just? init)
                           (let ([sub-cfg (enter-initial root (state-id (from-just init)))])
                                (configuration-add sub-cfg sid))
                           (make-configuration (list sid))))]
                 [(parallel-state? state)
                  ;; Enter all regions
                  (let loop ([regions (state-substates state)]
                             [cfg (make-configuration (list sid))])
                       (if (null? regions)
                           cfg
                           (let* ([region (car regions)]
                                  [init (get-initial-substate region)])
                                 (if (just? init)
                                     (let ([sub-cfg (enter-initial root (state-id (from-just init)))])
                                          (loop (cdr regions)
                                                (fold-left configuration-add
                                                           (configuration-add cfg (state-id region))
                                                           (configuration-states sub-cfg))))
                                     (loop (cdr regions)
                                           (configuration-add cfg (state-id region)))))))]
                 [else (configuration-empty)])))))

;;; ====
;;; Statechart Interpreter
;;; ====
;;;
;;; The interpreter manages statechart execution:
;;;   - Current configuration (active states)
;;;   - Context (user data)
;;;   - History tracking
;;;   - Event processing

;;; make-interpreter : Statechart → Interpreter
(define (make-interpreter statechart)
  (let* ([root (statechart-root statechart)]
         [init-cfg (get-initial-configuration root)])
        (list 'interpreter
              statechart
              init-cfg
              (statechart-context statechart)
              (make-history))))

;;; interpreter? : α → Boolean
(define (interpreter? x)
  (and (list? x)
       (= (length x) 5)
       (eq? (car x) 'interpreter)))

;;; interpreter-statechart : Interpreter → Statechart
(define (interpreter-statechart interp) (list-ref interp 1))
;;; interpreter-configuration : Interpreter → Configuration
(define (interpreter-configuration interp) (list-ref interp 2))
;;; interpreter-context : Interpreter → α
(define (interpreter-context interp) (list-ref interp 3))
;;; interpreter-history : Interpreter → History
(define (interpreter-history interp) (list-ref interp 4))

;;; set-interpreter-configuration : Interpreter × Configuration → Interpreter
(define (set-interpreter-configuration interp cfg)
  (list 'interpreter
        (interpreter-statechart interp)
        cfg
        (interpreter-context interp)
        (interpreter-history interp)))

;;; set-interpreter-context : Interpreter × α → Interpreter
(define (set-interpreter-context interp ctx)
  (list 'interpreter
        (interpreter-statechart interp)
        (interpreter-configuration interp)
        ctx
        (interpreter-history interp)))

;;; set-interpreter-history : Interpreter × History → Interpreter
(define (set-interpreter-history interp hist)
  (list 'interpreter
        (interpreter-statechart interp)
        (interpreter-configuration interp)
        (interpreter-context interp)
        hist))

;;; get-initial-configuration : State → Configuration
;;; Compute the initial configuration for a statechart.
(define (get-initial-configuration root)
  (cond
   [(composite-state? root)
    (let ([init (get-initial-substate root)])
         (if (just? init)
             (let ([sub-cfg (enter-initial root (state-id (from-just init)))])
                  (configuration-add sub-cfg (state-id root)))
             (make-configuration (list (state-id root)))))]
   [(parallel-state? root)
    (let loop ([regions (state-substates root)]
               [cfg (make-configuration (list (state-id root)))])
         (if (null? regions)
             cfg
             (let* ([region (car regions)]
                    [init (get-initial-substate region)])
                   (if (just? init)
                       (let ([sub-cfg (enter-initial root (state-id (from-just init)))])
                            (loop (cdr regions)
                                  (fold-left configuration-add
                                             (configuration-add cfg (state-id region))
                                             (configuration-states sub-cfg))))
                       (loop (cdr regions)
                             (configuration-add cfg (state-id region)))))))]
   [(atomic-state? root)
    (make-configuration (list (state-id root)))]
   [else (configuration-empty)]))

;;; ====
;;; Transition Selection and Execution
;;; ====

;;; find-enabled-transition : Interpreter × State × (Option Event) → (Option Transition)
;;; Find a transition enabled by the given event in the given state.
(define (find-enabled-transition interp state event)
  (let ([ctx (interpreter-context interp)]
        [transitions (state-transitions state)])
       (find-if (lambda (t)
                        (and (transition-matches-event? t event)
                             (transition-guard-passes? t ctx event)))
                transitions)))

;;; transition-matches-event? : Transition × (Option Event) → Boolean
(define (transition-matches-event? trans event)
  (cond
   ;; Eventless transition only matches when event is #f
   [(not (transition-event trans))
    (not event)]
   ;; No event provided - only matches eventless transitions (already handled above)
   [(not event) #f]
   ;; Normal matching: transition event matches event type
   [else (eq? (transition-event trans) (event-type event))]))

;;; transition-guard-passes? : Transition × α × (Option Event) → Boolean
(define (transition-guard-passes? trans ctx event)
  (let ([guard (transition-guard trans)])
       (or (not guard)
           (guard ctx event))))

;;; select-transitions : Interpreter × (Option Event) → (List (State × Transition))
;;; Select all enabled transitions for the current configuration.
(define (select-transitions interp event)
  (let* ([root (statechart-root (interpreter-statechart interp))]
         [cfg (interpreter-configuration interp)]
         [active-ids (configuration-states cfg)])
        ;; Check transitions from innermost to outermost states
        (let loop ([ids active-ids] [selected '()])
             (if (null? ids)
                 selected
                 (let* ([sid (car ids)]
                        [maybe-state (find-state root sid)])
                       (if (nothing? maybe-state)
                           (loop (cdr ids) selected)
                           (let* ([state (from-just maybe-state)]
                                  [trans (find-enabled-transition interp state event)])
                                 (if trans
                                     ;; Found a transition - add it and skip ancestors
                                     (let ([ancestors (map state-id (get-ancestors root sid))])
                                          (loop (filter (lambda (id) (not (member id ancestors)))
                                                        (cdr ids))
                                                (cons (cons state trans) selected)))
                                     (loop (cdr ids) selected)))))))))

;;; execute-transition : Interpreter × State × Transition × (Option Event) → Interpreter
;;; Execute a single transition.
(define (execute-transition interp source-state trans event)
  (let* ([root (statechart-root (interpreter-statechart interp))]
         [ctx (interpreter-context interp)]
         [source-id (state-id source-state)]
         [target-id (transition-target trans)]
         [action (transition-action trans)])
        (cond
         ;; Internal transition - no state change
         [(not target-id)
          (if action
              (set-interpreter-context interp (action ctx event))
              interp)]
         ;; External transition
         [else
          (let* ([maybe-target (find-state root target-id)])
                (if (nothing? maybe-target)
                    interp  ; Target not found, no change
                    (let* ([target-state (from-just maybe-target)]
                           ;; Execute exit actions (source to LCA)
                           [interp1 (execute-exit-actions interp source-id target-id)]
                           ;; Execute transition action
                           [ctx1 (if action
                                     (action (interpreter-context interp1) event)
                                     (interpreter-context interp1))]
                           [interp2 (set-interpreter-context interp1 ctx1)]
                           ;; Execute entry actions (LCA to target)
                           [interp3 (execute-entry-actions interp2 source-id target-id)]
                           ;; Update configuration
                           [new-cfg (compute-new-configuration root
                                                               (interpreter-configuration interp3)
                                                               source-id
                                                               target-id
                                                               (interpreter-history interp3))])
                          (set-interpreter-configuration interp3 new-cfg))))])))

;;; execute-exit-actions : Interpreter × Symbol × Symbol → Interpreter
;;; Execute exit actions from source up to (but not including) LCA.
(define (execute-exit-actions interp source-id target-id)
  (let* ([root (statechart-root (interpreter-statechart interp))]
         [lca-maybe (lca root source-id target-id)]
         [lca-id (if (just? lca-maybe) (state-id (from-just lca-maybe)) #f)]
         [source-ancestors (get-ancestors root source-id)])
        (let loop ([states (cons source-id (map state-id source-ancestors))]
                   [interp interp]
                   [history (interpreter-history interp)])
             (if (null? states)
                 (set-interpreter-history interp history)
                 (let ([sid (car states)])
                      (if (and lca-id (eq? sid lca-id))
                          (set-interpreter-history interp history)
                          (let ([maybe-state (find-state root sid)])
                               (if (nothing? maybe-state)
                                   (loop (cdr states) interp history)
                                   (let* ([state (from-just maybe-state)]
                                          [exit-action (state-exit-action state)]
                                          ;; Save history for composite states
                                          [new-history (if (composite-state? state)
                                                           (save-history history state (interpreter-configuration interp))
                                                           history)]
                                          [new-ctx (if exit-action
                                                       (exit-action (interpreter-context interp))
                                                       (interpreter-context interp))])
                                         (loop (cdr states)
                                               (set-interpreter-context interp new-ctx)
                                               new-history))))))))))

;;; execute-entry-actions : Interpreter × Symbol × Symbol → Interpreter
;;; Execute entry actions from LCA down to target.
(define (execute-entry-actions interp source-id target-id)
  (let* ([root (statechart-root (interpreter-statechart interp))]
         [lca-maybe (lca root source-id target-id)]
         [lca-id (if (just? lca-maybe) (state-id (from-just lca-maybe)) #f)]
         [target-ancestors (reverse (get-ancestors root target-id))])
        (let loop ([states (append
                            (filter (lambda (s)
                                            (or (not lca-id)
                                                (not (eq? (state-id s) lca-id))))
                                    target-ancestors)
                            (let ([t (find-state root target-id)])
                                 (if (just? t) (list (from-just t)) '())))]
                   [interp interp])
             (if (null? states)
                 interp
                 (let* ([state (car states)]
                        [entry-action (state-entry-action state)]
                        [new-ctx (if entry-action
                                     (entry-action (interpreter-context interp))
                                     (interpreter-context interp))])
                       (loop (cdr states) (set-interpreter-context interp new-ctx)))))))

;;; save-history : History × State × Configuration → History
(define (save-history hist state cfg)
  (let* ([sid (state-id state)]
         [substates (state-substates state)]
         [active-subs (filter (lambda (sub)
                                      (configuration-contains? cfg (state-id sub)))
                              substates)])
        (if (null? active-subs)
            hist
            (history-set hist sid (map state-id active-subs)))))

;;; compute-new-configuration : State × Configuration × Symbol × Symbol × History → Configuration
;;; Compute the new configuration after a transition.
(define (compute-new-configuration root old-cfg source-id target-id history)
  (let* ([lca-maybe (lca root source-id target-id)]
         [lca-id (if (just? lca-maybe) (state-id (from-just lca-maybe)) #f)]
         ;; Handle history states
         [resolved-target (resolve-history-target root target-id history)]
         ;; Enter target and get its configuration (target + substates)
         [target-cfg (enter-initial root resolved-target)]
         ;; Get ancestors of target
         [target-ancestors (get-ancestors root resolved-target)]
         ;; Get ancestors of source
         [source-ancestors (get-ancestors root source-id)]
         ;; States to remove: source, its descendants, and ancestors between source and LCA
         ;; source-ancestors is from outermost to innermost, so we need to:
         ;; 1. Find where LCA is in the list
         ;; 2. Take everything after the LCA (these are between LCA and source)
         [ancestors-to-remove
          (if lca-id
              (let ([after-lca (dropwhile (lambda (a) (not (eq? (state-id a) lca-id)))
                                          source-ancestors)])
                   ;; after-lca starts with LCA, skip it to get ancestors between LCA and source
                   (if (null? after-lca) '() (cdr after-lca)))
              source-ancestors)]
         [states-to-remove
          (append (cons source-id (get-descendant-ids root source-id))
                  (map state-id ancestors-to-remove))])
        ;; Build new configuration:
        ;; 1. Start with old configuration
        ;; 2. Remove source, its descendants, and exited ancestors
        ;; 3. Add target configuration and ancestors
        (let* ([cfg-without-source
                (fold-left (lambda (cfg id) (configuration-remove cfg id))
                           old-cfg
                           states-to-remove)]
               ;; Add target ancestors
               [cfg-with-ancestors
                (fold-left (lambda (cfg ancestor)
                                   (configuration-add cfg (state-id ancestor)))
                           cfg-without-source
                           target-ancestors)]
               ;; Add target configuration
               [final-cfg
                (fold-left (lambda (cfg id)
                                   (configuration-add cfg id))
                           cfg-with-ancestors
                           (configuration-states target-cfg))])
              final-cfg)))

;;; takewhile : (α → Boolean) × (List α) → (List α)
;;; Take elements from the list while predicate is true.
(define (takewhile pred lst)
  (cond
   [(null? lst) '()]
   [(pred (car lst)) (cons (car lst) (takewhile pred (cdr lst)))]
   [else '()]))

;;; dropwhile : (α → Boolean) × (List α) → (List α)
;;; Drop elements from the list while predicate is true.
(define (dropwhile pred lst)
  (cond
   [(null? lst) '()]
   [(pred (car lst)) (dropwhile pred (cdr lst))]
   [else lst]))

;;; get-descendant-ids : State × Symbol → (List Symbol)
;;; Get all descendant state IDs of a given state.
(define (get-descendant-ids root parent-id)
  (let ([maybe-state (find-state root parent-id)])
       (if (nothing? maybe-state)
           '()
           (let ([state (from-just maybe-state)])
                (if (or (composite-state? state) (parallel-state? state))
                    (let ([substates (state-substates state)])
                         (append-map (lambda (sub)
                                             (cons (state-id sub)
                                                   (get-descendant-ids root (state-id sub))))
                                     substates))
                    '())))))

;;; resolve-history-target : State × Symbol × History → Symbol
;;; If target is a history state, resolve to actual target.
(define (resolve-history-target root target-id history)
  (let ([maybe-state (find-state root target-id)])
       (if (nothing? maybe-state)
           target-id
           (let ([state (from-just maybe-state)])
                (cond
                 [(shallow-history-state? state)
                  (let ([parent-id (state-parent state)])
                       (if parent-id
                           (let ([saved (history-get history parent-id)])
                                (if saved (car saved) target-id))
                           target-id))]
                 [(deep-history-state? state)
                  ;; Deep history - fully restore hierarchy
                  (let ([parent-id (state-parent state)])
                       (if parent-id
                           (let ([saved (history-get history parent-id)])
                                (if saved
                                    ;; Find deepest saved state
                                    (let loop ([ids saved])
                                         (if (null? ids)
                                             target-id
                                             (let ([id (car ids)])
                                                  (let ([saved-deep (history-get history id)])
                                                       (if saved-deep
                                                           (loop saved-deep)
                                                           id)))))
                                    target-id))
                           target-id))]
                 [else target-id])))))

;;; ====
;;; Event Processing
;;; ====

;;; step : Interpreter × (Option Event) → Interpreter
;;; Process a single event and return the new interpreter state.
(define (step interp event)
  (doc 'export #t)
  (let* ([transitions (select-transitions interp event)]
         ;; Execute all selected transitions
         [new-interp (fold-left (lambda (i st-trans)
                                        (execute-transition i (car st-trans) (cdr st-trans) event))
                                interp
                                transitions)])
        ;; Process eventless transitions
        (process-eventless-transitions new-interp 100)))

;;; process-eventless-transitions : Interpreter × Nat → Interpreter
;;; Process any enabled eventless (always) transitions.
(define (process-eventless-transitions interp fuel)
  (if (<= fuel 0)
      interp
      (let ([transitions (select-transitions interp #f)])
           (if (null? transitions)
               interp
               (let ([new-interp (fold-left (lambda (i st-trans)
                                                    (execute-transition i (car st-trans) (cdr st-trans) #f))
                                            interp
                                            transitions)])
                    (process-eventless-transitions new-interp (- fuel 1)))))))

;;; send : Interpreter × (Option Event) → Interpreter
;;; Alias for step - send an event to the statechart.
(doc send 'export #t)
(define send step)

;;; send-event : Interpreter × Symbol → Interpreter
;;; Send a simple event (no payload).
(define (send-event interp event-type)
  (step interp (evt event-type)))

;;; send-event-data : Interpreter × Symbol × α → Interpreter
;;; Send an event with payload.
(define (send-event-data interp event-type data)
  (step interp (evt-data event-type data)))

;;; run-events : Interpreter × (List Event) → Interpreter
;;; Process a sequence of events.
(define (run-events interp events)
  (fold-left step interp events))

;;; ====
;;; State Queries
;;; ====

;;; in-state? : Interpreter × Symbol → Boolean
;;; Check if a state is currently active.
(define (in-state? interp state-id)
  (configuration-contains? (interpreter-configuration interp) state-id))

;;; active-states : Interpreter → (List Symbol)
;;; Get all currently active state ids.
(define (active-states interp)
  (configuration-states (interpreter-configuration interp)))

;;; is-final? : Interpreter → Boolean
;;; Check if the statechart has reached a final state.
(define (is-final? interp)
  (let* ([root (statechart-root (interpreter-statechart interp))]
         [active (active-states interp)])
        (exists (lambda (id)
                        (let ([s (find-state root id)])
                             (and (just? s) (final-state? (from-just s)))))
                active)))

;;; current-context : Interpreter → α
;;; Get the current context.
(define (current-context interp)
  (interpreter-context interp))

;;; ====
;;; Validation and Analysis
;;; ====

;;; validate-statechart : Statechart → (List (List Symbol))
;;; Validate a statechart for structural correctness.
(define (validate-statechart sc)
  (doc 'export #t)
  (let* ([root (statechart-root sc)]
         [errors '()])
        (append
         (validate-state root root)
         (validate-transitions root root)
         (validate-initial-states root))))

;;; validate-state : State × State → (List (List Symbol))
(define (validate-state root state)
  (let ([errors '()])
       (cond
        [(composite-state? state)
         (if (null? (state-substates state))
             (cons (list 'error 'empty-composite (state-id state)) errors)
             (append errors
                     (append-map (lambda (s) (validate-state root s))
                                 (state-substates state))))]
        [(parallel-state? state)
         (if (< (length (state-substates state)) 2)
             (cons (list 'error 'parallel-needs-regions (state-id state)) errors)
             (append errors
                     (append-map (lambda (s) (validate-state root s))
                                 (state-substates state))))]
        [else errors])))

;;; validate-transitions : State × State → (List (List Symbol))
(define (validate-transitions root state)
  (doc 'export #t)
  (let ([errors '()])
       (append
        ;; Validate this state's transitions
        (append-map (lambda (t)
                            (let ([target (transition-target t)])
                                 (if (and target (nothing? (find-state root target)))
                                     (list (list 'error 'invalid-target target (state-id state)))
                                     '())))
                    (state-transitions state))
        ;; Validate substates
        (if (or (composite-state? state) (parallel-state? state))
            (append-map (lambda (s) (validate-transitions root s))
                        (state-substates state))
            '()))))

;;; validate-initial-states : State → (List (List Symbol))
(define (validate-initial-states root)
  (let ([errors '()])
       (cond
        [(composite-state? root)
         (let ([has-initial (find-if initial-state? (state-substates root))])
              (append
               (if (not has-initial)
                   (list (list 'warning 'no-initial-state (state-id root)))
                   '())
               (append-map validate-initial-states (state-substates root))))]
        [(parallel-state? root)
         (append-map (lambda (region)
                             (let ([has-initial (find-if initial-state? (state-substates region))])
                                  (append
                                   (if (not has-initial)
                                       (list (list 'warning 'no-initial-in-region (state-id region)))
                                       '())
                                   (validate-initial-states region))))
                     (state-substates root))]
        [else '()])))

;;; reachable-states : Statechart → (List Symbol)
;;; Find all reachable states from the initial configuration.
(define (reachable-states sc)
  (doc 'export #t)
  (let* ([root (statechart-root sc)]
         [interp (make-interpreter sc)]
         [initial-active (active-states interp)])
        (collect-reachable root initial-active '() 1000)))

;;; collect-reachable : State × (List Symbol) × (List Symbol) × Nat → (List Symbol)
(define (collect-reachable root frontier visited fuel)
  (if (or (<= fuel 0) (null? frontier))
      visited
      (let ([sid (car frontier)])
           (if (member sid visited)
               (collect-reachable root (cdr frontier) visited (- fuel 1))
               (let* ([maybe-state (find-state root sid)]
                      [new-visited (cons sid visited)])
                     (if (nothing? maybe-state)
                         (collect-reachable root (cdr frontier) new-visited (- fuel 1))
                         (let* ([state (from-just maybe-state)]
                                ;; Add substates
                                [substate-ids (if (or (composite-state? state) (parallel-state? state))
                                                  (map state-id (state-substates state))
                                                  '())]
                                ;; Add transition targets
                                [target-ids (filter identity
                                                    (map transition-target (state-transitions state)))]
                                [new-frontier (append substate-ids target-ids (cdr frontier))])
                               (collect-reachable root new-frontier new-visited (- fuel 1)))))))))

;;; unreachable-states : Statechart → (List Symbol)
;;; Find states that cannot be reached.
(define (unreachable-states sc)
  (doc 'export #t)
  (let* ([root (statechart-root sc)]
         [all-states (collect-all-state-ids root)]
         [reachable (reachable-states sc)])
        (filter (lambda (id) (not (member id reachable))) all-states)))

;;; collect-all-state-ids : State → (List Symbol)
(define (collect-all-state-ids state)
  (cons (state-id state)
        (if (or (composite-state? state) (parallel-state? state))
            (append-map collect-all-state-ids (state-substates state))
            '())))

;;; ====
;;; Visualization
;;; ====

;;; statechart->dot : Statechart → String
;;; Generate a DOT (Graphviz) representation.
(define (statechart->dot sc)
  (let ([root (statechart-root sc)])
       (string-append
        "digraph " (symbol->string (statechart-id sc)) " {\n"
        "  rankdir=TB;\n"
        "  compound=true;\n"
        (state->dot root "  ")
        "}\n")))

;;; state->dot : State × String → String
(define (state->dot state indent)
  (cond
   [(atomic-state? state)
    (string-append indent (symbol->string (state-id state))
                   " [shape=box];\n"
                   (transitions->dot state indent))]
   [(final-state? state)
    (string-append indent (symbol->string (state-id state))
                   " [shape=doublecircle];\n")]
   [(initial-state? state)
    (string-append indent (symbol->string (state-id state))
                   " [shape=point];\n"
                   (transitions->dot state indent))]
   [(history-state? state)
    (string-append indent (symbol->string (state-id state))
                   " [shape=circle,label=\""
                   (if (deep-history-state? state) "H*" "H")
                   "\"];\n")]
   [(composite-state? state)
    (string-append indent "subgraph cluster_" (symbol->string (state-id state)) " {\n"
                   indent "  label=\"" (symbol->string (state-id state)) "\";\n"
                   (apply string-append
                          (map (lambda (s) (state->dot s (string-append indent "  ")))
                               (state-substates state)))
                   (transitions->dot state indent)
                   indent "}\n")]
   [(parallel-state? state)
    (string-append indent "subgraph cluster_" (symbol->string (state-id state)) " {\n"
                   indent "  label=\"" (symbol->string (state-id state)) " [parallel]\";\n"
                   indent "  style=dashed;\n"
                   (apply string-append
                          (map (lambda (s) (state->dot s (string-append indent "  ")))
                               (state-substates state)))
                   (transitions->dot state indent)
                   indent "}\n")]
   [else ""]))

;;; transitions->dot : State × String → String
(define (transitions->dot state indent)
  (apply string-append
         (map (lambda (t)
                      (if (transition-target t)
                          (string-append indent
                                         (symbol->string (state-id state))
                                         " -> "
                                         (symbol->string (transition-target t))
                                         (if (transition-event t)
                                             (string-append " [label=\"" (symbol->string (transition-event t)) "\"]")
                                             "")
                                         ";\n")
                          ""))
              (state-transitions state))))

;;; ====
;;; Helper Functions
;;; ====

;;; find-if : (α → Boolean) × (List α) → (Option α)
(define (find-if pred lst)
  (cond
   [(null? lst) #f]
   [(pred (car lst)) (car lst)]
   [else (find-if pred (cdr lst))]))

;;; exists : (α → Boolean) × (List α) → Boolean
(define (exists pred lst)
  (and (not (null? lst))
       (or (pred (car lst))
           (exists pred (cdr lst)))))

;;; from-just-or : (Option α) × α → α
(define (from-just-or maybe default)
  (if (just? maybe)
      (from-just maybe)
      default))

;;; ====
;;; Exports Summary
;;; ====
;;;
;;; State Constructors:
;;;   atomic, atomic-with, composite, composite-with
;;;   parallel, parallel-with, region, initial, final
;;;   history-shallow, history-deep
;;;
;;; State Predicates:
;;;   state?, atomic-state?, composite-state?, parallel-state?
;;;   history-state?, shallow-history-state?, deep-history-state?
;;;   initial-state?, final-state?
;;;
;;; Transition Constructors:
;;;   on, on-do, on-guard, on-guard-do
;;;   always, always-guard, internal, internal-guard
;;;   make-transition
;;;
;;; Event Constructors:
;;;   evt, evt-data, make-event
;;;
;;; Statechart:
;;;   statechart, statechart-ctx, make-statechart
;;;   with-transitions
;;;
;;; Interpreter:
;;;   make-interpreter, step, send, send-event, send-event-data
;;;   run-events
;;;
;;; Queries:
;;;   in-state?, active-states, is-final?, current-context
;;;   find-state, get-ancestors, lca
;;;
;;; Validation:
;;;   validate-statechart, reachable-states, unreachable-states
;;;
;;; Visualization:
;;;   statechart->dot
