;;; vectors.ss --- Fixed-Size Mutable Vectors
;;;
;;; Vectors provide O(1) random access, unlike lists.
;;;
;;; Run with: scheme --script docs/examples/data-structures/vectors.ss

;;; ====
;;; Creating Vectors
;;; ====

(display "=== Creating Vectors ===\n")

;;; Using vector function with symbols
(define v1 (vector 'a 'b 'c 'd 'e))
(format #t "from symbols: ~a~%" v1)

;;; vector function
(define v2 (vector 1 2 3 4 5))
(format #t "from vector fn: ~a~%" v2)

;;; make-vector creates a vector of given size
(define v3 (make-vector 5 0))  ; 5 zeros
(format #t "make-vector 5 0: ~a~%" v3)

;;; list->vector conversion
(define v4 (list->vector '(x y z)))
(format #t "from list: ~a~%" v4)

;;; ====
;;; Accessing Elements
;;; ====

(display "\n=== Accessing Elements ===\n")

;;; vector-ref for O(1) access (0-indexed)
(format #t "v1[0] = ~a~%" (vector-ref v1 0))
(format #t "v1[2] = ~a~%" (vector-ref v1 2))
(format #t "v1[4] = ~a~%" (vector-ref v1 4))

;;; vector-length
(format #t "length of v1 = ~a~%" (vector-length v1))

;;; ====
;;; Modifying Vectors (Mutation)
;;; ====

(display "\n=== Mutation ===\n")

;;; vector-set! modifies in place
(define mutable (vector 'a 'b 'c))
(format #t "before: ~a~%" mutable)
(vector-set! mutable 1 'CHANGED)
(format #t "after vector-set! 1 'CHANGED: ~a~%" mutable)

;;; vector-fill! sets all elements
(define filled (make-vector 4 0))
(vector-fill! filled 'x)
(format #t "after fill: ~a~%" filled)

;;; ====
;;; Vector Operations
;;; ====

(display "\n=== Vector Operations ===\n")

;;; vector->list conversion
(format #t "v1 as list: ~a~%" (vector->list v1))

;;; vector-copy
(define original (vector 1 2 3))
(define copy (vector-copy original))
(format #t "original: ~a, copy: ~a~%" original copy)

;;; Modifying copy doesn't affect original
(vector-set! copy 0 999)
(format #t "after modifying copy: original=~a, copy=~a~%" original copy)

;;; ====
;;; Iterating Over Vectors
;;; ====

(display "\n=== Iteration ===\n")

;;; vector-for-each
(display "elements of v2: ")
(vector-for-each (lambda (x) (format #t "~a " x)) v2)
(newline)

;;; vector-map
(define doubled (vector-map (lambda (x) (* x 2)) v2))
(format #t "doubled v2: ~a~%" doubled)

;;; Manual iteration with index
(display "with indices: ")
(let ([len (vector-length v1)])
  (let loop ([i 0])
    (when (< i len)
      (format #t "[~a]=~a " i (vector-ref v1 i))
      (loop (+ i 1)))))
(newline)

;;; ====
;;; When to Use Vectors vs Lists
;;; ====

(display "\n=== Vectors vs Lists ===\n")

(display "
Use VECTORS when:
  - You need fast random access by index
  - You know the size upfront
  - You need to mutate elements in place
  - Working with numeric data (performance)

Use LISTS when:
  - You're building data incrementally (cons)
  - You process sequentially (car/cdr)
  - You need structural recursion
  - Size varies or is unknown
")

;;; ====
;;; Practical Example: Sieve of Eratosthenes
;;; ====

(display "=== Example: Prime Sieve ===\n")

(define (sieve-primes limit)
  ;; Create vector where #t means "is prime" (initially all true)
  (let ([is-prime (make-vector (+ limit 1) #t)])
    ;; 0 and 1 are not prime
    (vector-set! is-prime 0 #f)
    (vector-set! is-prime 1 #f)
    ;; Sieve
    (let loop ([i 2])
      (when (<= (* i i) limit)
        (when (vector-ref is-prime i)
          ;; Mark multiples as not prime
          (let mark ([j (* i i)])
            (when (<= j limit)
              (vector-set! is-prime j #f)
              (mark (+ j i)))))
        (loop (+ i 1))))
    ;; Collect primes
    (let collect ([i 2] [primes '()])
      (if (> i limit)
          (reverse primes)
          (collect (+ i 1)
                   (if (vector-ref is-prime i)
                       (cons i primes)
                       primes))))))

(format #t "primes up to 50: ~a~%" (sieve-primes 50))

(display "\nDone!\n")
