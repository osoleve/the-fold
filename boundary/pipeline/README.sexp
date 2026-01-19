((name "pipeline")
 (purpose "Effect interpretation for agent pipeline execution")
 (description
  "Impure shell that interprets effects from core/pipeline/.
   Executes LLM calls, shell commands, Fold IPC, HTTP requests,
   and other IO-bound operations. Manages pipeline state,
   handles checkpoints, and provides error recovery.")
 (modules
  ((interpreter.ss "Main effect interpreter: dispatch, state, logging, metrics")))
 (dependencies (core/pipeline fs))
 (load-order "interpreter.ss"))
