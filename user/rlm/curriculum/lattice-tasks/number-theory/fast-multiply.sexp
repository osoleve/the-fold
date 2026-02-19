;;; fast-multiply.sexp — Development tasks for lattice/number-theory/fast-multiply.ss
;;;
;;; Module: fast-multiply (tier 0, depends on prelude)
;;; 699 lines, ~15 exported functions
;;; Fast integer multiplication via limb-list representation. Schoolbook
;;; O(n²), Karatsuba O(n^1.585), Toom-Cook O(n^1.465). Limbs are
;;; least-significant-first lists of base-B digits.
;;;
;;; Git history:
;;;   a3abed54 refactor: Reorganize into core/lattice/shell three-layer architecture
;;;   710fc341 feat(module): migrate lattice/number-theory/ to require

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement limbs->integer
  ;; ──────────────────────────────────────────────
  ;; Convert limb list to integer.

  (task
    (id "number-theory/fast-multiply/limbs-to-integer-impl")
    (module fast-multiply)
    (tier 0)
    (type implement)
    (description "Implement limbs->integer: convert a limb list to an integer. Limbs are stored least-significant first, so the list (l0 l1 l2 ...) represents the number l0 + l1*base + l2*base² + ... . Use a named let loop with three accumulators: the remaining list, the current multiplier (starts at 1, multiplied by base each step), and the running result (starts at 0). At each step, add car(ls) * multiplier to result, then multiply multiplier by base.")
    (context "Available: null?, car, cdr, +, *, let. Named let loop with three accumulators.")
    (before "(define (limbs->integer limbs base)
  ;; TODO: l0 + l1*base + l2*base^2 + ...
  0)")
    (test-file "lattice/number-theory/test-fast-multiply.ss")
    (test-forms (";; Base 10: (3 2 1) = 3 + 20 + 100 = 123
(assert-equal 123 (limbs->integer '(3 2 1) 10))"
                 ";; Base 10: (0) = 0
(assert-equal 0 (limbs->integer '(0) 10))"
                 ";; Base 2: (1 0 1) = 1 + 0 + 4 = 5
(assert-equal 5 (limbs->integer '(1 0 1) 2))"
                 ";; Single limb
(assert-equal 7 (limbs->integer '(7) 10))"))
    (available-modules (prelude))
    (hints ("Named let: (let loop ([ls limbs] [multiplier 1] [result 0]) ...)"
            "Base case: (null? ls) -> result"
            "Step: (loop (cdr ls) (* multiplier base) (+ result (* (car ls) multiplier)))"))
    (reference-solution "(define (limbs->integer limbs base)
  (let loop ([ls limbs] [multiplier 1] [result 0])
    (if (null? ls)
        result
        (loop (cdr ls)
              (* multiplier base)
              (+ result (* (car ls) multiplier))))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement integer->limbs
  ;; ──────────────────────────────────────────────
  ;; Convert integer to limb list.

  (task
    (id "number-theory/fast-multiply/integer-to-limbs-impl")
    (module fast-multiply)
    (tier 0)
    (type implement)
    (description "Implement integer->limbs: convert a non-negative integer to a limb list (least-significant first). Repeatedly extract the remainder mod base (the lowest limb) and divide by base (shift right by one digit). Collect limbs in reverse order, then reverse at the end. Special case: if n is 0, return '(0). Use a named let loop with n decreasing and limbs accumulating.")
    (context "Available: =, modulo, quotient, cons, reverse, let. Named let with two accumulators.")
    (before "(define (integer->limbs n base)
  ;; TODO: repeatedly extract mod and divide
  '(0))")
    (test-file "lattice/number-theory/test-fast-multiply.ss")
    (test-forms (";; 123 in base 10: (3 2 1)
(assert-equal '(3 2 1) (integer->limbs 123 10))"
                 ";; 0 → (0)
(assert-equal '(0) (integer->limbs 0 10))"
                 ";; 5 in base 2: (1 0 1)
(assert-equal '(1 0 1) (integer->limbs 5 2))"
                 ";; 255 in base 16: (15 15) = 0xFF
(assert-equal '(15 15) (integer->limbs 255 16))"))
    (available-modules (prelude))
    (hints ("Guard: (= n 0) -> '(0)"
            "Named let: (let loop ([n n] [limbs '()]) ...)"
            "Base: (= n 0) -> (reverse limbs)"
            "Step: (loop (quotient n base) (cons (modulo n base) limbs))"))
    (reference-solution "(define (integer->limbs n base)
  (if (= n 0)
      '(0)
      (let loop ([n n] [limbs '()])
        (if (= n 0)
            (reverse limbs)
            (loop (quotient n base)
                  (cons (modulo n base) limbs))))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement limbs-normalize
  ;; ──────────────────────────────────────────────
  ;; Strip trailing (high-order) zeros.

  (task
    (id "number-theory/fast-multiply/limbs-normalize-impl")
    (module fast-multiply)
    (tier 0)
    (type implement)
    (description "Implement limbs-normalize: remove trailing zeros from a limb list. Since limbs are least-significant first, trailing zeros are the high-order zeros that don't contribute to the value. For example, (1 2 0 0) normalizes to (1 2). The result must always have at least one element: (0 0 0) normalizes to (0). Strategy: reverse the list, drop leading zeros (but keep at least one element), then reverse back.")
    (context "Available: reverse, null?, =, car, cdr, and, not, let, cond. Reverse-strip-reverse pattern.")
    (before "(define (limbs-normalize limbs)
  ;; TODO: remove trailing zeros, keep at least one element
  limbs)")
    (test-file "lattice/number-theory/test-fast-multiply.ss")
    (test-forms (";; Remove trailing zeros
(assert-equal '(1 2 3) (limbs-normalize '(1 2 3 0 0)))"
                 ";; All zeros → (0)
(assert-equal '(0) (limbs-normalize '(0 0 0)))"
                 ";; Already normalized
(assert-equal '(1 2 3) (limbs-normalize '(1 2 3)))"
                 ";; Single zero
(assert-equal '(0) (limbs-normalize '(0)))"))
    (available-modules (prelude))
    (hints ("Reverse the list first: work on high-order end"
            "Drop leading zeros: (let loop ([ls (reverse limbs)]) ...)"
            "Keep at least one: (cond [(null? ls) '(0)] [(and (= (car ls) 0) (not (null? (cdr ls)))) (loop (cdr ls))] [else (reverse ls)])"))
    (reference-solution "(define (limbs-normalize limbs)
  (let loop ([ls (reverse limbs)])
    (cond
      [(null? ls) '(0)]
      [(and (= (car ls) 0) (not (null? (cdr ls))))
       (loop (cdr ls))]
      [else (reverse ls)])))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement limbs-shift
  ;; ──────────────────────────────────────────────
  ;; Left shift by k limbs (multiply by base^k).

  (task
    (id "number-theory/fast-multiply/limbs-shift-impl")
    (module fast-multiply)
    (tier 0)
    (type implement)
    (description "Implement limbs-shift: shift a limb list left by k positions, equivalent to multiplying by base^k. Since limbs are least-significant first, prepend k zeros to the front of the list. Special case: if the number is zero (single limb = 0), return as-is (shifting zero gives zero). Use make-list to create k zeros and append to prepend them.")
    (context "Available: length, =, car, and, append, make-list, if. One guard + one append.")
    (before "(define (limbs-shift limbs k)
  ;; TODO: prepend k zeros (unless zero)
  limbs)")
    (test-file "lattice/number-theory/test-fast-multiply.ss")
    (test-forms (";; Shift (1 2 3) by 2: (0 0 1 2 3) = 123 * 100 = 12300 in base 10
(assert-equal '(0 0 1 2 3) (limbs-shift '(1 2 3) 2))"
                 ";; Shift by 0: no change
(assert-equal '(1 2 3) (limbs-shift '(1 2 3) 0))"
                 ";; Shift zero: stays zero
(assert-equal '(0) (limbs-shift '(0) 5))"
                 ";; Shift single digit
(assert-equal '(0 0 0 7) (limbs-shift '(7) 3))"))
    (available-modules (prelude))
    (hints ("Zero check: (and (= 1 (length limbs)) (= 0 (car limbs))) → return limbs"
            "Otherwise: (append (make-list k 0) limbs)"))
    (reference-solution "(define (limbs-shift limbs k)
  (if (and (= 1 (length limbs)) (= 0 (car limbs)))
      limbs
      (append (make-list k 0) limbs)))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Implement limbs-add
  ;; ──────────────────────────────────────────────
  ;; Addition with carry propagation.

  (task
    (id "number-theory/fast-multiply/limbs-add-impl")
    (module fast-multiply)
    (tier 0)
    (type implement)
    (description "Implement limbs-add: add two limb lists with carry propagation. Process limbs from least-significant to most-significant, adding corresponding limbs plus any carry from the previous position. When the sum at a position >= base, the carry is sum div base and the digit is sum mod base. Handle four cases: both lists exhausted (emit final carry if > 0), only a exhausted, only b exhausted, and both have elements. Build the result in reverse and normalize at the end.")
    (context "Available: null?, and, car, cdr, +, quotient, modulo, >, cons, reverse, limbs-normalize, cond. Named let with four accumulators (a, b, carry, result).")
    (before "(define (limbs-add a b base)
  ;; TODO: digit-by-digit addition with carry
  '(0))")
    (test-file "lattice/number-theory/test-fast-multiply.ss")
    (test-forms (";; 123 + 78 = 201 in base 10
(let ([result (limbs-add '(3 2 1) '(8 7) 10)])
  (assert-equal 201 (limbs->integer result 10)))"
                 ";; 99 + 1 = 100 (carry propagation)
(let ([result (limbs-add '(9 9) '(1) 10)])
  (assert-equal 100 (limbs->integer result 10)))"
                 ";; 0 + 0 = 0
(assert-equal '(0) (limbs-add '(0) '(0) 10))"
                 ";; 5 + 7 = 12 in base 10 (single digit with carry)
(let ([result (limbs-add '(5) '(7) 10)])
  (assert-equal 12 (limbs->integer result 10)))"))
    (available-modules (prelude))
    (hints ("Named let: (let loop ([a a] [b b] [carry 0] [result '()]) ...)"
            "Both exhausted: (limbs-normalize (reverse (if (> carry 0) (cons carry result) result)))"
            "One exhausted: process remaining list with carry"
            "Both present: (let ([sum (+ (car a) (car b) carry)]) (loop (cdr a) (cdr b) (quotient sum base) (cons (modulo sum base) result)))"))
    (reference-solution "(define (limbs-add a b base)
  (let loop ([a a] [b b] [carry 0] [result '()])
    (cond
      [(and (null? a) (null? b))
       (limbs-normalize
        (reverse (if (> carry 0) (cons carry result) result)))]
      [(null? a)
       (loop '() (cdr b)
             (quotient (+ (car b) carry) base)
             (cons (modulo (+ (car b) carry) base) result))]
      [(null? b)
       (loop (cdr a) '()
             (quotient (+ (car a) carry) base)
             (cons (modulo (+ (car a) carry) base) result))]
      [else
       (let ([sum (+ (car a) (car b) carry)])
         (loop (cdr a) (cdr b)
               (quotient sum base)
               (cons (modulo sum base) result)))])))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

) ;; end tasks
