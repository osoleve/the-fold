;;; boundary/provenance/README.sexp — Provenance Tracking Module
;;;
;;; Log every optic application to the CAS for full data lineage.
;;; Natural fit with The Fold's content-addressed philosophy.

(module provenance
  (path "boundary/provenance")
  (layer boundary)

  (description
   "Provenance tracking via optic instrumentation. Every traced optic
operation creates a CAS block recording what happened: the source
structure, result, operation type, timestamp, and agent identity.

This enables:
  - Full audit trail for any CAS block
  - 'Explain this value' queries (trace-lineage)
  - Debugging complex transformations
  - Regulatory compliance (data lineage)

The provenance record is itself a CAS block, forming a linked
audit trail where each record references the source and result
blocks it describes.")

  (modules
   ("provenance.ss"
    "Core provenance infrastructure: record creation, storage, optic registry")
   ("traced-optics.ss"
    "Wrapped optic operations that automatically record provenance")
   ("query.ss"
    "Query API for explaining values and tracing lineage"))

  (usage
   ";;; Basic traced operations
    (load \"boundary/provenance/traced-optics.ss\")
    (load \"boundary/provenance/query.ss\")

    ;; Use traced operations instead of raw optics
    (define data '(1 . 2))
    (traced-view data lens-fst)        ; Records: viewed '1' from '(1 . 2)'
    (traced-set lens-fst 99 data)      ; Records: set 99 at fst, got '(99 . 2)'

    ;; Chain transformations with full tracking
    (define result
      (traced-set lens-fst 10
        (traced-set lens-snd 20 '(0 . 0))))

    ;; Explain how a value was created
    (explain result)
    ;; => Lineage for 00abc123...
    ;;    Step 1: set via lens-snd (lens)
    ;;      Source: 00def456...
    ;;      Result: 00789abc...
    ;;    Step 2: set via lens-fst (lens)
    ;;      ...

    ;; Query by operation type
    (find-provenance-by-operation 'set)

    ;; Set identity for audit trail
    (with-agent-id 'my-agent
      (lambda ()
        (traced-set lens-fst 42 data)))")

  (block-schema
   ((tag . provenance/record)
    (payload . "((operation . <symbol>)
                 (optic-name . <symbol|#f>)
                 (optic-type . <symbol>)
                 (source-hash . <hex-string>)
                 (result-hash . <hex-string>)
                 (value-hash . <hex-string|#f>)
                 (timestamp . <iso8601-string>)
                 (agent-id . <symbol|#f>)
                 (session-id . <string|#f>)
                 (version . 1))")
    (refs . "[source-block, result-block, value-block?]")))

  (exports
   ;; Configuration
   (*provenance-enabled?* provenance-enable! provenance-disable!)
   (*current-agent-id* *current-session-id*)
   (with-agent-id with-session-id)

   ;; Optic Registry
   (register-optic! lookup-optic-name lookup-optic list-registered-optics)

   ;; Traced Operations
   (traced-view traced-preview traced-to-list)
   (traced-set traced-over)
   (traced-iso-view traced-iso-review traced-iso-over)
   (traced-lens-view traced-lens-set traced-lens-over)
   (traced-prism-preview traced-prism-review traced-prism-set)
   (traced-traversal-to-list traced-traversal-over traced-traversal-set)
   (traced-affine-preview traced-affine-set traced-affine-over)
   (traced->>> with-tracing without-tracing)

   ;; Query API
   (provenance-for-result provenance-for-source latest-provenance)
   (trace-lineage trace-descendants)
   (explain-value explain-hash)
   (provenance-summary lineage-summary)
   (provenance-get-source provenance-get-result provenance-get-value)
   (find-provenance-by-optic find-provenance-by-operation find-provenance-by-agent)
   (provenance-stats)

   ;; REPL Integration
   (explain explain-last show-lineage))

  (related
   (lattice/fp/optics "The optics tower being instrumented")
   (boundary/history "Similar CAS-based audit trail pattern")
   (boundary/storage/identity "Agent identity integration")))
