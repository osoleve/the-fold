## Appendix B: Type Grammar


```bnf
<type>       ::= <base-type>
               | <compound-type>
               | <dependent-type>
               | <polymorphic-type>
               | <special-type>

<base-type>  ::= "Nat" | "Int" | "Bool" | "Char" | "Symbol"
               | "String" | "Bytes" | "Unit" | "Void" | "Hash"

<compound-type> ::= "(" "→" <type> <type> ")"
                  | "(" "×" <type>+ ")"
                  | "(" "+" <variant>+ ")"
                  | "(" "List" <type> ")"
                  | "(" "Vector" <type> ")"
                  | "(" "Block" <symbol> <type> ")"
                  | "(" "Ref" <type> ")"

<variant>    ::= "(" <tag> <type> ")"

<dependent-type> ::= "(" "Π" "(" <binding>+ ")" <type> ")"
                   | "(" "Σ" "(" <binding>+ ")" <type> ")"
                   | "(" "=" <type> <term> <term> ")"

<binding>    ::= "(" <var> ":" <type> ")"

<polymorphic-type> ::= "(" "∀" "(" <tvar>+ ")" <type> ")"
                     | "(" "μ" <tvar> <type> ")"
                     | <tvar>

<special-type> ::= "?" | "(" "?" <name> ")"
                 | "(" "Cap" <name> <type> ")"

<tvar>       ::= <identifier>
<tag>        ::= <identifier>
<name>       ::= <identifier>
```

---
