(load "core/types/sig-check.ss")

(doc 'module 'type-annotation-check)
(doc 'description "DEPRECATED: Backwards-compatibility stub. This file has been merged into sig-check.ss. Load sig-check.ss for all signature parsing and validation functionality. This stub preserves the script interface for backwards compatibility.")
(doc 'layer 'core)

(doc 'note "Usage: scheme --script core/types/type-annotation-check.ss [file ...] OR scheme --script core/types/type-annotation-check.ss (checks all core files)")

(doc 'note "All type annotation checking functions are now defined in sig-check.ss: check-file, check-type-kind, type-name-mapping, normalize-type-name, sig-type->kind-type, collect-type-vars, reset-counters!, record-parse-error!, record-kind-error!, record-valid!, report-results, sig-check-main")

(doc 'section 'script-entry-point)

(let ([args (cdr (command-line))])
     (exit (sig-check-main args)))

