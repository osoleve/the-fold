; ============================================================
; Logic Extension Functions
; Canonical versions - no duplicates
; ============================================================

; xor: Exclusive or
; (xor #t #f) => #t
; (xor #t #t) => #f
(xor (fn (a b)
         (if a (not b) b)))

; implies: Logical implication (a => b)
; (implies #t #f) => #f
; (implies #f #t) => #t
(implies (fn (a b)
             (if a b #t)))

; iff: If and only if (biconditional)
; (iff #t #t) => #t
; (iff #t #f) => #f
(iff (fn (a b)
         (= a b)))

; nand: Not and
; (nand #t #t) => #f
; (nand #t #f) => #t
(nand (fn (a b)
          (not (if a b #f))))

; nor: Not or
; (nor #f #f) => #t
; (nor #t #f) => #f
(nor (fn (a b)
         (not (if a #t b))))
