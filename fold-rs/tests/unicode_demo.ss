;; Unicode Symbol Aliases Demo for fold-rs Parser
;; This file demonstrates that Unicode mathematical symbols are automatically
;; converted to their ASCII equivalents for .ss compatibility

;; Lambda (λ → lambda)
(λ (x) (× x 2))

;; Logical operators (∧ → and, ∨ → or, ¬ → not)
(∧ #t #f)
(∨ #t #f)
(¬ #t)

;; Comparison operators
(≤ 1 2)
(≥ 3 2)
(≠ 1 2)

;; Arithmetic operators
(× 3 4)
(÷ 10 2)

;; Mathematical constant
(× 2 π)

;; Arrow operators (for match/case expressions)
(→ 'input 'output)
(← 'value 'source)

;; Complex nested expression mixing Unicode and ASCII
(λ (x y)
  (∧ (≤ x y)
     (∨ (≠ x 0)
        (≠ y 0))))

;; This demonstrates that you can use mathematical notation in Scheme code
;; and it will be parsed correctly by fold-rs
