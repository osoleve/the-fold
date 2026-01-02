((name "blocks")
 (purpose "Block system and content-addressed storage")
 (description "The Block Machine - everything in The Fold is a Block.
Blocks consist of {tag, payload, refs[]} and are content-addressed
via cryptographic hashing. This module provides block construction,
normalization (alpha-conversion to de Bruijn indices), and CAS operations.")
 (modules
  ((block.ss "Block construction and accessors")
   (cas.ss "Content-addressed store operations")
   (cas-gc.ss "Garbage collection for CAS")
   (normalize.ss "S-expression to de Bruijn index conversion")
   (expand.ss "de Bruijn indices back to S-expressions")))
 (dependencies (base))
 (key-concepts
  ((content-addressing "Hash IS identity - same content = same hash")
   (alpha-normalization "Variable names don't affect hash")
   (de-bruijn-indices "Canonical variable representation"))))
