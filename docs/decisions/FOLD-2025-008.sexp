;;; FOLD-2025-008: API Design Style
;;; Settled: 2025-12-29

((id . "FOLD-2025-008")
 (date . "2025-12-29T19:30:00Z")
 (settled-by . outsider)
 (category . taste)
 (context . "When designing APIs, should we favor explicit/verbose calls
where intent is always clear, or terse/implicit calls that rely on context?")
 (decision . "Fluent, case by case - neither dogmatically verbose nor
reflexively terse. Each API should feel natural for its domain.")
 (rationale . "Rigid rules produce awkward APIs. Mathematical operations
should be terse; configuration should be explicit. Judge by how it reads.")
 (implications . ("No universal naming convention enforced"
                  "Review APIs for fluency, not adherence to style rules"
                  "Domain experts should find the API unsurprising")))
