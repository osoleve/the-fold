## Appendix C: Kind Grammar


```bnf
<kind>       ::= "*"                           ; Type kind
               | "(" "⇒" <kind> <kind> ")"     ; Kind arrow
               | "Constraint"                   ; Constraint kind
               | "Row"                          ; Row kind
               | "(" "κ∀" "(" <kvar>+ ")" <kind> ")"  ; Kind polymorphism
               | "(" "Πκ" "(" <kbinding> ")" <kind> ")" ; Dependent kind
               | "□"                            ; Sort
               | "(" "□" <nat> ")"              ; Leveled sort

<kbinding>   ::= "(" <kvar> ":" <kind> ")"

<kvar>       ::= "κ" <identifier>
```

---
