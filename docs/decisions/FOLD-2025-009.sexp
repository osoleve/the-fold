;;; FOLD-2025-009: Safety vs Convenience Tradeoff
;;; Settled: 2025-12-29

((id . "FOLD-2025-009")
 (date . "2025-12-29T19:30:00Z")
 (settled-by . outsider)
 (category . taste)
 (context . "When safety and convenience conflict, which should win?")
 (decision . "Favor safety - make dangerous things hard, even if annoying")
 (rationale . "Convenience is a one-time cost; bugs from unsafe defaults
recur forever. The Fold is infrastructure; it must be trustworthy.")
 (implications . ("Destructive operations require explicit confirmation"
                  "Default to immutable; mutation must be opted into"
                  "Fail closed rather than open on ambiguity"
                  "Type system should catch errors early")))
