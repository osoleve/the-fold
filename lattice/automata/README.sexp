((name "automata")
 (purpose "State machine DSL with Harel statecharts")
 (description
  "Hierarchical state machine implementation following Harel's
   statechart semantics. Supports nested states, parallel regions,
   history states, guarded transitions, and entry/exit actions.
   Includes DSL for declarative definition, interpreter for
   execution, and validation/analysis utilities.")
 (modules
  ((statechart.ss "Full statechart implementation: states, transitions, interpreter, validation")))
 (dependencies (base fp/meta))
 (load-order "statechart.ss"))
