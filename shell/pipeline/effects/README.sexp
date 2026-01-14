;;; shell/pipeline/effects/README.sexp — Effect Handler Modules
;;;
;;; This directory contains focused modules that implement effect handlers
;;; for the pipeline interpreter. Each module handles a specific category
;;; of effects.

((directory "shell/pipeline/effects")
 (purpose "Modular effect handlers for pipeline interpreter")
 (authority builder)  ; Shell code - may be modified by Builders+

 (modules
  ((name "llm.ss")
   (lines 431)
   (description "LLM API calls via Anthropic Claude API")
   (effects (llm)))

  ((name "shell.ss")
   (lines 140)
   (description "Shell command execution via open-process-ports")
   (effects (shell)))

  ((name "http.ss")
   (lines 221)
   (description "HTTP requests via curl")
   (effects (http)))

  ((name "fold.ss")
   (lines 136)
   (description "Fold REPL daemon IPC via file-based protocol")
   (effects (fold)))

  ((name "discord.ss")
   (lines 314)
   (description "Discord integration via outbox queue")
   (effects (discord)))

  ((name "misc.ss")
   (lines 476)
   (description "Miscellaneous effect handlers")
   (effects (log store bbs git await))))

 (parent-module "shell/pipeline/interpreter.ss")
 (refactored-from "Monolithic interpreter.ss (1675 lines)")
 (refactored-to "Modular design: interpreter (364 lines) + 6 focused modules (2211 lines)")
 (reduction "78% reduction in interpreter.ss size")

 (notes
  "Effect handlers are self-contained modules with their own dependencies."
  "Each handler module can be tested and modified independently."
  "JSON parsing is currently duplicated across modules - consider extraction."
  "Shell execution utilities are shared between modules via load."
  "All modules follow defensive shell code patterns (guard, error handling)."))
