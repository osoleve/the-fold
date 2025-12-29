;;; playpen/quill/quill.ss — Quill: Text Adventure SDK (loader)
;;;
;;; Quill is a deterministic text-adventure and narrative engine for The Fold.
;;; It is REPL-first and intended for:
;;;   - Educational materials (lessons, exercises, rubrics, progress)
;;;   - Complex narrative engines (dialogue, quests, rules, timeline)
;;;
;;; This file defines the Quill load order and public entrypoints.
;;;
;;; Usage:
;;;   (load "thimble/repl.ss")
;;;   (load "playpen/quill/quill.ss")
;;;
;;; Then:
;;;   (quill-run story)                  ; interactive
;;;   (define run (quill-start story))   ; non-interactive
;;;   (define-values (run2 out) (quill-step story run "1"))
;;;
;;; Notes:
;;; - Quill is designed to run without needing an active session/login.
;;; - Parsing uses fabric/stitches/parse.ss (parser combinators).
;;; - Persistence and advanced narrative features land in later modules.

;;; ============================================================
;;; Dependencies
;;; ============================================================

;; Useful string utilities (string-trim, string-blank?, etc).
(load "thimble/string-utils.ss")

;;; ============================================================
;;; Quill Modules (load order)
;;; ============================================================

(load "playpen/quill/types.ss")
(load "playpen/quill/state.ss")
(load "playpen/quill/parse.ss")
(load "playpen/quill/validate.ss")
(load "playpen/quill/dsl.ss")
(load "playpen/quill/render.ss")
(load "playpen/quill/runtime.ss")
(load "playpen/quill/repl.ss")

;;; Optional startup message (respects *quiet* convention used by thimble/repl.ss)
(when (and (top-level-bound? '*quiet*) (not *quiet*))
      (display "Quill loaded — text adventure SDK\n"))
