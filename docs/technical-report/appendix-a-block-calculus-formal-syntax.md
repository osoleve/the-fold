## Appendix A: Block Calculus Formal Syntax


```
e ::= x                                  ; Variable
    | (λ x : τ . e)                      ; Typed abstraction
    | (e₁ e₂)                            ; Application
    | (let x : τ = e₁ in e₂)             ; Let binding
    | (fix x : τ . e)                    ; Recursive binding
    | c                                  ; Constant
    | (if e₁ e₂ e₃)                      ; Conditional
    | (prim op e*)                       ; Primitive operation
    | (make-block τ e_tag e_payload e_refs)  ; Block construction
    | (block-tag e)                      ; Tag accessor
    | (block-payload e)                  ; Payload accessor
    | (block-refs e)                     ; Refs accessor
    | (hash e)                           ; Hash computation
    | (store! e)                         ; CAS store
    | (fetch e)                          ; CAS fetch
    | (quote e)                          ; Quotation
    | (eval e)                           ; Evaluation

τ ::= Nat | Int | Bool | ...             ; Base types
    | (→ τ₁ τ₂)                          ; Function type
    | (× τ₁ τ₂)                          ; Product type
    | (+ (l₁ τ₁) ... (lₙ τₙ))            ; Sum type
    | (∀ α . τ)                          ; Universal type
    | (μ α . τ)                          ; Recursive type
    | (Block τ_tag τ_payload)            ; Block type
    | (Ref τ)                            ; Reference type
    | α                                  ; Type variable
    | ?                                  ; Hole

v ::= (λ x : τ . e)                      ; Abstraction value
    | c                                  ; Constant value
    | (block v_tag v_payload v_refs)     ; Block value
```

**Reduction Rules**:

```
((λ x : τ . e) v) →β e[v/x]

(let x : τ = v in e) → e[v/x]

(fix x : τ . e) → e[(fix x : τ . e)/x]

(if true e₂ e₃) → e₂

(if false e₂ e₃) → e₃

(block-tag (block t p r)) → t

(block-payload (block t p r)) → p

(block-refs (block t p r)) → r

(eval (quote e)) → e
```

---
