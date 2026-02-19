;;; dft.sexp — Development tasks for lattice/numeric/dft.ss
;;;
;;; Module: dft (tier 0, depends on prelude, complex, vec)
;;; 343 lines, ~23 exported functions
;;; Discrete Fourier Transform: naive O(N^2) DFT, Cooley-Tukey radix-2 FFT,
;;; inverse transforms, bit reversal, spectral analysis utilities,
;;; real-valued transforms, zero padding.
;;;
;;; Git history:
;;;   a3abed54 refactor: Reorganize into core/lattice/shell three-layer architecture
;;;   d1ae3197 fix: Patch 12 P1 algorithmic and mathematical bugs in lattice

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement power-of-2?
  ;; ──────────────────────────────────────────────
  ;; The classic bitwise trick. Foundation for FFT dispatch.

  (task
    (id "numeric/dft/power-of-2-impl")
    (module dft)
    (tier 0)
    (type implement)
    (description "Implement power-of-2?: check if an integer n is a power of 2. Use the classic bitwise trick: n is a power of 2 if and only if n > 0 AND (n & (n-1)) = 0. The expression n & (n-1) clears the lowest set bit. For powers of 2 (which have exactly one bit set), this gives 0. For non-powers of 2, at least one bit remains. In Scheme, bitwise-and performs the AND operation.")
    (context "Available: prelude (>, =, -, and), bitwise-and (bitwise AND of two integers). This is a pure arithmetic predicate.")
    (before "(define (power-of-2? n)
  (doc 'type '(-> Integer Boolean))
  (doc 'description \"Check if n is a power of 2\")
  ;; TODO: bitwise trick using n & (n-1)
  #f)")
    (test-file "lattice/numeric/test-dft.ss")
    (test-forms ("(assert-true (power-of-2? 1))"
                 "(assert-true (power-of-2? 2))"
                 "(assert-true (power-of-2? 4))"
                 "(assert-true (power-of-2? 8))"
                 "(assert-true (power-of-2? 1024))"
                 "(assert-false (power-of-2? 0))"
                 "(assert-false (power-of-2? 3))"
                 "(assert-false (power-of-2? 6))"
                 "(assert-false (power-of-2? 15))"))
    (available-modules (prelude))
    (hints ("The key insight: a power of 2 in binary is 1 followed by zeros: 1, 10, 100, 1000..."
            "n-1 flips the lowest set bit and all bits below it"
            "So n & (n-1) is zero only when n has exactly one bit set"
            "Don't forget: n must be positive (0 is not a power of 2)"))
    (reference-solution "(define (power-of-2? n)
  (and (> n 0)
       (= 0 (bitwise-and n (- n 1)))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement bit-reverse
  ;; ──────────────────────────────────────────────
  ;; Essential preprocessing for iterative FFT.

  (task
    (id "numeric/dft/bit-reverse-impl")
    (module dft)
    (tier 0)
    (type implement)
    (description "Implement bit-reverse: reverse the bit pattern of an integer i using exactly nbits bits. For example, with 3 bits: 1 (001) becomes 4 (100), 3 (011) becomes 6 (110), 5 (101) stays 5 (101). This is used in the Cooley-Tukey FFT to reorder input elements. Algorithm: loop nbits times. At each step, extract the lowest bit of i (using remainder by 2), append it to the result (result = result*2 + bit), then shift i right (quotient by 2).")
    (context "Available: prelude (=, +, *, quotient, remainder, -). Pure integer arithmetic.")
    (before "(define (bit-reverse i nbits)
  (doc 'type '(-> Integer Integer Integer))
  (doc 'description \"Reverse the bits of index i using nbits bits\")
  ;; TODO: extract bits from i, build reversed result
  0)")
    (test-file "lattice/numeric/test-dft.ss")
    (test-forms (";; 3 bits: 0 (000) -> 0 (000)
(assert-equal 0 (bit-reverse 0 3))"
                 ";; 3 bits: 1 (001) -> 4 (100)
(assert-equal 4 (bit-reverse 1 3))"
                 ";; 3 bits: 2 (010) -> 2 (010)
(assert-equal 2 (bit-reverse 2 3))"
                 ";; 3 bits: 3 (011) -> 6 (110)
(assert-equal 6 (bit-reverse 3 3))"
                 ";; 3 bits: 4 (100) -> 1 (001)
(assert-equal 1 (bit-reverse 4 3))"
                 ";; 0 bits: always 0
(assert-equal 0 (bit-reverse 5 0))"))
    (available-modules (prelude))
    (hints ("Named let with i, result=0, bits=nbits as loop variables"
            "Base case: bits = 0, return result"
            "Each step: extract lowest bit = (remainder i 2)"
            "Shift result left and add bit: result = result*2 + bit"
            "Shift i right: i = (quotient i 2)"
            "Decrement bits"))
    (reference-solution "(define (bit-reverse i nbits)
  (let loop ([i i] [result 0] [bits nbits])
    (if (= bits 0)
        result
        (loop (quotient i 2)
              (+ (* result 2) (remainder i 2))
              (- bits 1)))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 3))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement next-power-of-2
  ;; ──────────────────────────────────────────────
  ;; Simple but practical utility for zero padding.

  (task
    (id "numeric/dft/next-power-of-2-impl")
    (module dft)
    (tier 0)
    (type implement)
    (description "Implement next-power-of-2: find the smallest power of 2 that is greater than or equal to n. Start with p=1 and keep doubling until p >= n. For example: next-power-of-2(3) = 4, next-power-of-2(8) = 8, next-power-of-2(9) = 16. This is used to pad signals to FFT-friendly lengths.")
    (context "Available: prelude (>=, *). Simple loop.")
    (before "(define (next-power-of-2 n)
  (doc 'type '(-> Integer Integer))
  (doc 'description \"Find the next power of 2 >= n\")
  ;; TODO: double until >= n
  1)")
    (test-file "lattice/numeric/test-dft.ss")
    (test-forms ("(assert-equal 1 (next-power-of-2 1))"
                 "(assert-equal 4 (next-power-of-2 3))"
                 "(assert-equal 8 (next-power-of-2 5))"
                 "(assert-equal 8 (next-power-of-2 8))"
                 "(assert-equal 16 (next-power-of-2 9))"
                 "(assert-equal 1024 (next-power-of-2 1000))"))
    (available-modules (prelude))
    (hints ("Named let starting with p = 1"
            "If p >= n, return p"
            "Otherwise loop with p * 2"))
    (reference-solution "(define (next-power-of-2 n)
  (let loop ([p 1])
    (if (>= p n)
        p
        (loop (* 2 p)))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement magnitude-spectrum
  ;; ──────────────────────────────────────────────
  ;; Extract magnitudes from complex DFT output.

  (task
    (id "numeric/dft/magnitude-spectrum-impl")
    (module dft)
    (tier 0)
    (type implement)
    (description "Implement magnitude-spectrum: given a vector of complex numbers (DFT output), compute the magnitude of each element. The magnitude of a complex number z = a + bi is |z| = sqrt(a^2 + b^2). Create a new result vector of the same length, and for each element, store (complex-magnitude element). This converts the complex frequency-domain representation into a real-valued amplitude spectrum.")
    (context "Available: prelude (=, +, make-vector, vector-length, vector-ref, vector-set!), complex-magnitude (returns |z| for a complex number). Uses imperative do loop for efficiency.")
    (before "(define (magnitude-spectrum X)
  (doc 'type '(-> (Vector Complex) (Vector Number)))
  (doc 'description \"Compute magnitude spectrum from DFT output\")
  ;; TODO: map complex-magnitude over the vector
  (make-vector (vector-length X) 0))")
    (test-file "lattice/numeric/test-dft.ss")
    (test-forms (";; Impulse DFT has flat magnitude spectrum
(let* ([x (vector (make-complex 1 0) (make-complex 0 0)
                  (make-complex 0 0) (make-complex 0 0))]
       [X (dft x)]
       [mag (magnitude-spectrum X)])
  (assert-true (< (abs (- (vector-ref mag 0) 1)) 1e-10))
  (assert-true (< (abs (- (vector-ref mag 1) 1)) 1e-10)))"
                 ";; Single complex number
(let ([mag (magnitude-spectrum (vector (make-complex 3 4)))])
  (assert-true (< (abs (- (vector-ref mag 0) 5)) 1e-10)))"
                 ";; Empty vector
(assert-equal 0 (vector-length (magnitude-spectrum (vector))))"))
    (available-modules (prelude complex))
    (hints ("Allocate result vector: (make-vector n) where n = (vector-length X)"
            "Use a do loop from i=0 to n: (do ([i 0 (+ i 1)]) ((= i n) result) ...)"
            "At each step: (vector-set! result i (complex-magnitude (vector-ref X i)))"))
    (reference-solution "(define (magnitude-spectrum X)
  (let* ([n (vector-length X)]
         [result (make-vector n)])
    (do ([i 0 (+ i 1)])
        ((= i n) result)
        (vector-set! result i (complex-magnitude (vector-ref X i))))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Implement real->complex-vec
  ;; ──────────────────────────────────────────────
  ;; Bridge between real signals and complex DFT.

  (task
    (id "numeric/dft/real-to-complex-impl")
    (module dft)
    (tier 0)
    (type implement)
    (description "Implement real->complex-vec: convert a vector of real numbers into a vector of complex numbers by setting each imaginary part to 0. For each real value r in the input, create (make-complex r 0) in the output. This is the standard way to prepare a real-valued signal for DFT processing, since DFT operates on complex numbers.")
    (context "Available: prelude (=, +, make-vector, vector-length, vector-ref, vector-set!), make-complex (creates a complex number from real and imaginary parts).")
    (before "(define (real->complex-vec v)
  (doc 'type '(-> (Vector Number) (Vector Complex)))
  (doc 'description \"Convert real-valued vector to complex vector\")
  ;; TODO: wrap each real value as (make-complex val 0)
  (make-vector (vector-length v)))")
    (test-file "lattice/numeric/test-dft.ss")
    (test-forms (";; Simple conversion
(let* ([v (vector 1 2 3 4)]
       [cv (real->complex-vec v)])
  (assert-equal 4 (vector-length cv))
  (assert-true (< (abs (- (complex-real (vector-ref cv 0)) 1)) 1e-10))
  (assert-true (< (abs (complex-imag (vector-ref cv 2))) 1e-10)))"
                 ";; Round-trip: real->complex->dft->idft->real should recover input
(let* ([v (vector 1 2 3 4)]
       [X (dft-real v)]
       [recovered (idft-real X)])
  (assert-true (< (abs (- (vector-ref recovered 0) 1)) 1e-10))
  (assert-true (< (abs (- (vector-ref recovered 3) 4)) 1e-10)))"
                 ";; Empty vector
(assert-equal 0 (vector-length (real->complex-vec (vector))))"))
    (available-modules (prelude complex))
    (hints ("Get n = (vector-length v)"
            "Allocate result = (make-vector n)"
            "Loop from i=0 to n"
            "At each step: (vector-set! result i (make-complex (vector-ref v i) 0))"
            "Return result when i = n"))
    (reference-solution "(define (real->complex-vec v)
  (let* ([n (vector-length v)]
         [result (make-vector n)])
    (do ([i 0 (+ i 1)])
        ((= i n) result)
        (vector-set! result i (make-complex (vector-ref v i) 0)))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

) ;; end tasks
