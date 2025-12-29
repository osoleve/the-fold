;;; FOLD-2025-014: Handling Weird But Valid Code
;;; Settled: 2025-12-29

((id . "FOLD-2025-014")
 (date . "2025-12-29T19:30:00Z")
 (settled-by . outsider)
 (category . taste)
 (context . "When someone does something weird but technically valid,
should we allow silently, warn, or refuse?")
 (decision . "Warn but proceed - note the oddity, continue anyway")
 (rationale . "Sometimes weird is intentional. Refusing blocks legitimate
uses. Silent acceptance hides potential bugs. Warnings are the middle path:
inform without blocking.")
 (implications . ("Type system may emit warnings for suspicious patterns"
                  "Warnings go to stderr, don't interrupt flow"
                  "Lint tools can be stricter than core"
                  "'Weird' patterns should be documented when warned")))
