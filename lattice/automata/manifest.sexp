;;; lattice/automata/manifest.sexp — Automata and State Machines Skill Manifest

(skill automata
  (version "0.1.0")
  (path "lattice/automata")
  (purity partial)  ; Uses counter for unique IDs
  (stability stable)
  (fuel-bound "O(n) for state traversal, O(n^2) for reachability analysis")
  (deps (data fp))  ; Depends on data structures and fp parsers

  (description
   "Hierarchical state machine (statechart) implementation following Harel's
    semantics. Supports nested states, parallel regions, history states,
    guarded transitions, entry/exit actions, and event-driven execution.
    Includes DSL for declarative definition and validation utilities.")

  (keywords (automata statechart state-machine fsm dfa nfa
             hierarchical-state parallel-region history-state
             harel transition guard action event))
  (aliases (state-machines fsm statecharts))

  (concepts
    (concept automata-theory
      (description "Hierarchical state machines (statecharts) following Harel's semantics: nested states, parallel regions, guarded transitions, and event-driven execution.")
      (parent computation)
      (synonyms automata state-machines fsm statecharts state-machine statechart harel hierarchical-state parallel-region)))

  (exports
   (statechart
    ;; State types and constructors
    make-state state? state-id state-type state-entry-action
    state-exit-action state-substates state-transitions state-parent state-data
    atomic-state? composite-state? parallel-state? history-state?
    shallow-history-state? deep-history-state? initial-state? final-state?
    atomic atomic-with composite composite-with parallel parallel-with
    region initial final history-shallow history-deep
    set-state-parent add-transition set-entry-action set-exit-action
    ;; Transitions
    make-transition transition? transition-event transition-target
    transition-guard transition-action internal-transition? external-transition?
    eventless-transition? guarded-transition?
    on on-do on-guard on-guard-do always always-guard internal internal-guard
    ;; Events
    make-event event? event-type event-payload evt evt-data
    ;; Statechart structure
    make-statechart statechart? statechart-id statechart-root
    statechart-context statechart-history set-statechart-context
    set-statechart-history statechart statechart-ctx
    make-history history-get history-set
    ;; DSL and navigation
    with-transitions add-transition-to-substate resolve-initial-states
    find-state find-state-in-list get-initial-substate
    get-ancestors lca is-descendant?
    ;; Interpreter
    step send
    ;; Validation
    validate-statechart reachable-states unreachable-states
    validate-transitions)

   (statechart-zipper
    state->tree tree->state make-state-zipper state->zipper zipper->state
    state-zipper-state state-zipper-id state-zipper-type
    state-zipper-set state-zipper-modify
    state-zipper-up state-zipper-down state-zipper-left state-zipper-right
    state-zipper-root state-zipper-nth-child
    state-zipper-at-root? state-zipper-at-leaf?
    state-zipper-find state-zipper-ancestors state-zipper-path
    state-zipper-depth state-zipper-lca state-zipper-is-descendant?
    state-zipper-exit-path state-zipper-entry-path
    state-zipper-preorder state-zipper-collect state-zipper-collect-ids))

  (modules
   (statechart "statechart.ss" "Full statechart implementation: states, transitions, interpreter, validation")
   (statechart-zipper "statechart-zipper.ss" "Zipper-based navigation and modification of statechart hierarchies")))
