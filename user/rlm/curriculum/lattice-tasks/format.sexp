;;; format.sexp — Schema for lattice development task records
;;;
;;; Each task file contains a (tasks ...) form wrapping one or more (task ...) records.
;;; Fields are positional within named pairs for readability.

(schema
  (version 1)
  (fields
    (id          String   "Unique identifier: module-group/module/task-name")
    (module      Symbol   "Module name as used by (require 'module)")
    (tier        Nat      "Dependency tier in the lattice DAG (0 = no lattice deps)")
    (type        Symbol   "One of: implement fix extend optimize refactor test debug compose")
    (description String   "Natural language task description — what the model should do")
    (context     String   "What's available: loaded modules, session state, constraints")
    (before      String   "Starting code state — stub, buggy impl, or existing code to modify")
    (test-file   String   "Path to test file that verifies the solution")
    (test-forms  (Maybe (List String)) "Specific test expressions if not whole file")
    (available-modules (List Symbol) "Modules the model may require for this task")
    (hints       (List String) "Optional pedagogical hints, ordered from vague to specific")
    (reference-solution String "One known-good solution (the actual implementation)")
    (alternatives (List String) "Other acceptable solutions if any")
    (source-commit (Maybe String) "Git commit SHA this task derives from")
    (difficulty  Nat      "1-10 scale within the module's tier"))

  (type-descriptions
    (implement "Write a function from type signature + description + tests")
    (fix       "Given buggy code + failing test, find and fix the defect")
    (extend    "Add a new function or capability to an existing module")
    (optimize  "Make working code faster while preserving test correctness")
    (refactor  "Restructure code without changing observable behavior")
    (test      "Write tests for an existing implementation")
    (debug     "Identify and explain a bug without necessarily fixing it")
    (compose   "Use multiple loaded modules together to solve a problem")))
