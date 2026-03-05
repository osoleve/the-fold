;;; skills/automata.sexp — Skill curation file

(skill automata
  (description
    "Hierarchical state machine (statechart) implementation following Harel's\n    semantics. Supports nested states, parallel regions, history states,\n    guarded transitions, entry/exit actions, and event-driven execution.\n    Includes DSL for declarative definition and validation utilities.")

  (keywords (automata statechart state-machine fsm dfa nfa hierarchical-state parallel-region history-state harel transition guard action event))

  (aliases (state-machines fsm statecharts))

  (concepts (automata-theory))

  (modules
    statechart
    statechart-zipper
  )

  ;; Curated Q&A pairs for LLM context
  (prompts)

  ;; KG/search queries an agent should try
  (suggested-queries)
)
