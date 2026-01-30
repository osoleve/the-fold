((name "automata")
(purpose "State machine DSL with Harel statecharts")
(description "Hierarchical state machine implementation following Harel's\n   statechart semantics. Supports nested states, parallel regions,\n   history states, guarded transitions, and entry/exit actions.\n   Includes DSL for declarative definition, interpreter for\n   execution, and validation/analysis utilities.")
(modules 
  ((statechart.ss "Full statechart implementation: states, transitions, interpreter, validation")))
(dependencies (base fp/meta))
(load-order "statechart.ss"))
