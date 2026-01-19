## Appendix D: Manifest Schema


```scheme
;; Complete manifest schema
(skill <name:symbol>
  ;; Required fields
  (version <semver:string>)           ; "major.minor.patch"
  (tier <n:nat>)                      ; 0 = foundational
  (path <path:string>)                ; Relative to project root
  (purity <p:purity>)                 ; total | partial | effect
  (stability <s:stability>)           ; stable | experimental
  (fuel-bound <bound:string>)         ; Big-O notation
  (deps (<dep:symbol> ...))           ; Direct dependencies

  ;; Optional fields
  (description <desc:string>)         ; Human-readable
  (keywords (<kw:symbol> ...))        ; Search tags
  (aliases (<alias:symbol> ...))      ; Alternative names

  ;; API specification
  (exports
    (<module:symbol> <export:symbol>+ ) ...)

  ;; Module listing
  (modules
    (<name:symbol> <file:string> <desc:string>) ...))

;; Purity levels
<purity> ::= total    ; Pure, terminating
           | partial  ; Pure, may diverge
           | effect   ; Has side effects

;; Stability levels
<stability> ::= stable       ; API frozen
              | experimental ; API may change
```

---
