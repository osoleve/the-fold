(load "core/base/prelude.ss")
(load "lattice/fp/control/state.ss")

(doc 'module 'prng)
(doc 'description "Pseudorandom Number Generation — Pure, deterministic PRNGs using the State monad. All generators are fully reproducible given the same seed.")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'features "PCG (Permuted Congruential Generator) - high quality, fast; Xorshift128+ - fast, good statistical properties; Splitmix64 - excellent for seeding other generators; State monad integration for composable random computations")

(doc 'section 'bit-manipulation)

(doc "Scheme uses arbitrary precision integers, but we simulate 64-bit and 32-bit operations for PRNG algorithms.")

(doc "Masks for fixed-width arithmetic")
(doc mask-32 'export #t)
(define mask-32 (- (expt 2 32) 1))
(doc mask-64 'export #t)
(define mask-64 (- (expt 2 64) 1))

(define (u32 n)
  (doc 'export #t)
  (doc 'type '(-> Int Int))
  (doc 'description "Truncate to unsigned 32-bit")
  (bitwise-and n mask-32))

(define (u64 n)
  (doc 'export #t)
  (doc 'type '(-> Int Int))
  (doc 'description "Truncate to unsigned 64-bit")
  (bitwise-and n mask-64))

(define (rotl32 x k)
  (doc 'export #t)
  (doc 'type '(-> Int Int Int))
  (doc 'description "32-bit left rotation")
  (let ([x (u32 x)]
        [k (modulo k 32)])
       (u32 (bitwise-ior (ash x k)
                         (ash x (- k 32))))))

(define (rotr32 x k)
  (doc 'export #t)
  (doc 'type '(-> Int Int Int))
  (doc 'description "32-bit right rotation")
  (rotl32 x (- 32 k)))

(define (rotl64 x k)
  (doc 'export #t)
  (doc 'type '(-> Int Int Int))
  (doc 'description "64-bit left rotation")
  (let ([x (u64 x)]
        [k (modulo k 64)])
       (u64 (bitwise-ior (ash x k)
                         (ash x (- k 64))))))

(define (rotr64 x k)
  (doc 'export #t)
  (doc 'type '(-> Int Int Int))
  (doc 'description "64-bit right rotation")
  (rotl64 x (- 64 k)))

(doc 'section 'splitmix64)

(doc "Simple, high-quality generator often used to initialize other generators from a single seed. State: single 64-bit integer")

(define (make-splitmix seed)
  (doc 'export #t)
  (list 'splitmix (u64 seed)))

(define (splitmix? x)
  (doc 'export #t)
  (and (pair? x) (eq? (car x) 'splitmix)))

(define (splitmix-state sm)
  (doc 'export #t)
  (cadr sm))

(define (splitmix-next sm)
  (doc 'export #t)
  (let* ([s (u64 (+ (splitmix-state sm) #x9e3779b97f4a7c15))]
         [z (u64 (* (bitwise-xor s (ash s -30))
                    #xbf58476d1ce4e5b9))]
         [z (u64 (* (bitwise-xor z (ash z -27))
                    #x94d049bb133111eb))]
         [z (bitwise-xor z (ash z -31))])
        (cons z (make-splitmix s))))

(doc splitmix-random 'export #t)
(define splitmix-random
  (make-state splitmix-next))

(doc 'section 'pcg)

(doc "High-quality, statistically excellent generator. We implement PCG-XSH-RR (32-bit output, 64-bit state). State: (state . inc) where both are 64-bit")

(define (make-pcg seed stream)
  (doc 'export #t)
  (doc 'type '(-> Int Int RNG))
  (doc 'description "Create PCG state from seed and stream id. Different stream IDs give independent sequences")
  (let* ([inc (u64 (bitwise-ior (ash stream 1) 1))]
         [state0 0]
         [state1 (u64 (+ (* state0 #x5851f42d4c957f2d) inc))]
         [state2 (u64 (+ state1 (u64 seed)))]
         [state3 (u64 (+ (* state2 #x5851f42d4c957f2d) inc))])
        (list 'pcg state3 inc)))

(define (pcg? x)
  (doc 'export #t)
  (and (pair? x) (eq? (car x) 'pcg)))

(define (pcg-state p) (cadr p))
(doc 'export #t)

(define (pcg-inc p) (caddr p))
(doc 'export #t)

(define (pcg-next p)
  (doc 'export #t)
  (let* ([state (pcg-state p)]
         [inc (pcg-inc p)]
         ;; XSH-RR output function
         [xorshifted (u32 (ash
                           (bitwise-xor (ash state -18) state)
                           -27))]
         [rot (ash state -59)]
         [output (rotr32 xorshifted rot)]
         ;; LCG step
         [new-state (u64 (+ (* state #x5851f42d4c957f2d) inc))])
        (cons output (list 'pcg new-state inc))))

(doc pcg-random 'export #t)
(define pcg-random
  (make-state pcg-next))


(doc 'section 'xorshift128+-generator)

(doc "Xorshift128+ Generator")



(define (make-xorshift128 seed)
  (doc 'export #t)
  (let* ([sm0 (make-splitmix seed)]
         [r1 (splitmix-next sm0)]
         [s0 (car r1)]
         [sm1 (cdr r1)]
         [r2 (splitmix-next sm1)]
         [s1 (car r2)])
        ;; Ensure non-zero state
        (list 'xorshift128
              (if (= s0 0) 1 s0)
              (if (= s1 0) 1 s1))))

(define (xorshift128? x)
  (doc 'export #t)
  (and (pair? x) (eq? (car x) 'xorshift128)))

(define (xorshift128-s0 xs) (cadr xs))
(doc 'export #t)

(define (xorshift128-s1 xs) (caddr xs))
(doc 'export #t)

(define (xorshift128-next xs)
  (doc 'export #t)
  (let* ([s0 (xorshift128-s0 xs)]
         [s1 (xorshift128-s1 xs)]
         [result (u64 (+ s0 s1))]
         ;; Xorshift operations
         [s1-new (bitwise-xor s0 s1)]
         [new-s0 (u64 (bitwise-xor
                       (bitwise-xor (rotl64 s0 24) s1-new)
                       (ash s1-new 16)))]
         [new-s1 (rotl64 s1-new 37)])
        (cons result (list 'xorshift128 new-s0 new-s1))))

(doc xorshift128-random 'export #t)
(define xorshift128-random
  (make-state xorshift128-next))


(doc 'section 'uniform-random-number-generation)

(doc "Uniform Random Number Generation")



(define (random-u32-from gen-state)
  (doc 'export #t)
  (cond
   [(pcg? gen-state) pcg-random]
   [(splitmix? gen-state)
    (state-map u32 splitmix-random)]
   [(xorshift128? gen-state)
    (state-map u32 xorshift128-random)]
   [else (error 'random-u32 "unknown generator type" gen-state)]))

(define (random-u64-from gen-state)
  (doc 'export #t)
  (cond
   [(pcg? gen-state)
    ;; PCG produces 32 bits, combine two
    (state-bind pcg-random
                (lambda (hi)
                        (state-bind pcg-random
                                    (lambda (lo)
                                            (state-pure
                                             (u64 (bitwise-ior (ash hi 32) lo)))))))]
   [(splitmix? gen-state) splitmix-random]
   [(xorshift128? gen-state) xorshift128-random]
   [else (error 'random-u64 "unknown generator type" gen-state)]))

(doc random-float 'export #t)
(define random-float
  (make-state
   (lambda (gen)
           (cond
            [(pcg? gen)
             ;; PCG produces 32 bits
             (let* ([result (pcg-next gen)]
                    [bits (car result)]
                    [new-gen (cdr result)]
                    [scaled (/ bits (expt 2.0 32))])
                   (cons scaled new-gen))]
            [(splitmix? gen)
             ;; Splitmix produces 64 bits, use top 53 for precision
             (let* ([result (splitmix-next gen)]
                    [bits (car result)]
                    [new-gen (cdr result)]
                    [scaled (/ (ash bits -11) (expt 2.0 53))])
                   (cons scaled new-gen))]
            [(xorshift128? gen)
             ;; Xorshift128+ produces 64 bits, use top 53 for precision
             (let* ([result (xorshift128-next gen)]
                    [bits (car result)]
                    [new-gen (cdr result)]
                    [scaled (/ (ash bits -11) (expt 2.0 53))])
                   (cons scaled new-gen))]
            [else (error 'random-float "unknown generator" gen)]))))

(define (random-float-range lo hi)
  (doc 'export #t)
  (state-map (lambda (u) (+ lo (* u (- hi lo))))
             random-float))

(define (random-int-range lo hi)
  (doc 'export #t)
  (if (> lo hi)
      (error 'random-int-range "lo must be <= hi" lo hi)
      (let* ([range (+ (- hi lo) 1)]
             [threshold (modulo (- (expt 2 64)) range)])
            (make-state
             (lambda (gen)
                     (let loop ([g gen])
                          (let* ([result (cond
                                          [(pcg? g)
                                           (let* ([r1 (pcg-next g)]
                                                  [r2 (pcg-next (cdr r1))])
                                                 (cons (bitwise-ior
                                                        (ash (car r1) 32)
                                                        (car r2))
                                                       (cdr r2)))]
                                          [(splitmix? g) (splitmix-next g)]
                                          [(xorshift128? g) (xorshift128-next g)]
                                          [else (error 'random-int-range "unknown gen" g)])]
                                 [bits (car result)]
                                 [new-gen (cdr result)])
                                (if (>= bits threshold)
                                    (cons (+ lo (modulo bits range)) new-gen)
                                    (loop new-gen)))))))))

(doc random-bool 'export #t)
(define random-bool
  (state-map (lambda (n) (odd? n))
             (make-state
              (lambda (gen)
                      (cond
                       [(pcg? gen) (pcg-next gen)]
                       [(splitmix? gen) (splitmix-next gen)]
                       [(xorshift128? gen) (xorshift128-next gen)]
                       [else (error 'random-bool "unknown generator" gen)])))))


(doc 'section 'sampling-utilities)

(doc "Sampling Utilities")



(define (random-element lst)
  (doc 'export #t)
  (if (null? lst)
      (error 'random-element "empty list")
      (state-map (lambda (i) (list-ref lst i))
                 (random-int-range 0 (- (length lst) 1)))))

(define (shuffle lst)
  (doc 'export #t)
  (let ([vec (list->vector lst)]
        [n (length lst)])
       (letrec ([shuffle-step
                 (lambda (i)
                         (if (>= i (- n 1))
                             (state-pure (vector->list vec))
                             (state-bind (random-int-range i (- n 1))
                                         (lambda (j)
                                                 ;; Swap vec[i] and vec[j]
                                                 (let ([tmp (vector-ref vec i)])
                                                      (vector-set! vec i (vector-ref vec j))
                                                      (vector-set! vec j tmp)
                                                      (shuffle-step (+ i 1)))))))])
               (shuffle-step 0))))

(define (sample k lst)
  (doc 'export #t)
  (let ([n (length lst)])
       (cond
        [(> k n) (error 'sample "k > list length" k n)]
        [(= k n) (shuffle lst)]
        [(= k 0) (state-pure '())]
        [else
         ;; Reservoir sampling
         (let ([reservoir (take k lst)]
               [rest (drop k lst)])
              (letrec ([fill-reservoir
                        (lambda (res rem i)
                                (if (null? rem)
                                    (state-pure res)
                                    (state-bind (random-int-range 0 i)
                                                (lambda (j)
                                                        (if (< j k)
                                                            (fill-reservoir
                                                             (list-set res j (car rem))
                                                             (cdr rem)
                                                             (+ i 1))
                                                            (fill-reservoir
                                                             res
                                                             (cdr rem)
                                                             (+ i 1)))))))])
                      (fill-reservoir reservoir rest k)))])))

(define (list-set lst i val)
  (if (= i 0)
      (cons val (cdr lst))
      (cons (car lst) (list-set (cdr lst) (- i 1) val))))

(define (weighted-choice weighted-list)
  (if (null? weighted-list)
      (error 'weighted-choice "empty list")
      (let* ([total (fold-left + 0 (map cdr weighted-list))])
            (if (<= total 0)
                (error 'weighted-choice "total weight must be positive" total)
                (state-bind (random-float-range 0 total)
                            (lambda (r)
                                    (state-pure
                                     (let loop ([lst weighted-list] [acc 0])
                                          (let ([new-acc (+ acc (cdar lst))])
                                               (if (or (< r new-acc) (null? (cdr lst)))
                                                   (caar lst)
                                                   (loop (cdr lst) new-acc)))))))))))


(doc 'section 'generating-multiple-random-values)

(doc "Generating Multiple Random Values")



(define (random-list n gen)
  (doc 'export #t)
  (if (<= n 0)
      (state-pure '())
      (state-bind gen
                  (lambda (x)
                          (state-bind (random-list (- n 1) gen)
                                      (lambda (xs)
                                              (state-pure (cons x xs))))))))

(define (random-vector n gen)
  (state-map list->vector (random-list n gen)))


(doc 'section 'convenience-functions-run-with-default-generator)

(doc "Convenience Functions (Run with Default Generator)")



(define (with-random seed computation)
  (eval-state computation (make-pcg seed 1)))

(define (with-random-stream seed stream computation)
  (eval-state computation (make-pcg seed stream)))


(doc 'section 'generator-statistics-for-testing)

(doc "Generator Statistics (for Testing)")



(define (gen-advance n gen)
  (if (<= n 0)
      gen
      (let ([next (cond
                   [(pcg? gen) pcg-next]
                   [(splitmix? gen) splitmix-next]
                   [(xorshift128? gen) xorshift128-next]
                   [else (error 'gen-advance "unknown generator" gen)])])
           (gen-advance (- n 1) (cdr (next gen))))))


(doc 'section 'generator-splitting-for-parallel-simulations)

(doc "Generator Splitting (for Parallel Simulations)")



(define (gen-split gen)
  (cond
   [(pcg? gen)
    ;; PCG split: derive two new generators with different stream IDs
    ;; Use the current state to seed two new PCGs with distinct streams
    (let* ([state (pcg-state gen)]
           [inc (pcg-inc gen)]
           ;; Generate two seeds from current state
           [seed1 (u64 (+ state #x9e3779b97f4a7c15))]
           [seed2 (u64 (+ seed1 #x9e3779b97f4a7c15))]
           ;; Create two new PCGs with different stream IDs
           [stream1 (u64 (+ inc 2))]
           [stream2 (u64 (+ inc 4))])
          (cons (make-pcg seed1 stream1)
                (make-pcg seed2 stream2)))]
   
   [(splitmix? gen)
    ;; Splitmix split: generate two seeds and create new generators
    (let* ([r1 (splitmix-next gen)]
           [seed1 (car r1)]
           [g1 (cdr r1)]
           [r2 (splitmix-next g1)]
           [seed2 (car r2)])
          (cons (make-splitmix seed1)
                (make-splitmix seed2)))]
   
   [(xorshift128? gen)
    ;; Xorshift split: use splitmix to derive new states
    (let* ([combined (u64 (+ (xorshift128-s0 gen) (xorshift128-s1 gen)))]
           [sm (make-splitmix combined)]
           [r1 (splitmix-next sm)]
           [r2 (splitmix-next (cdr r1))]
           [r3 (splitmix-next (cdr r2))]
           [r4 (splitmix-next (cdr r3))])
          (cons (list 'xorshift128
                      (if (= (car r1) 0) 1 (car r1))
                      (if (= (car r2) 0) 1 (car r2)))
                (list 'xorshift128
                      (if (= (car r3) 0) 1 (car r3))
                      (if (= (car r4) 0) 1 (car r4)))))]
   
   [else (error 'gen-split "unknown generator type" gen)]))

(define random-split
  (make-state
   (lambda (gen)
           (let ([children (gen-split gen)])
                ;; Return one child, use the other as new state
                (cons children (car children))))))


(doc 'section 'random-bytes-generation)

(doc "Random Bytes Generation")



(define (random-bytes n)
  (if (<= n 0)
      (state-pure (make-bytevector 0))
      (make-state
       (lambda (gen)
               (let ([bv (make-bytevector n)])
                    (let loop ([i 0] [g gen])
                         (if (>= i n)
                             (cons bv g)
                             ;; Generate bytes 4 or 8 at a time
                             (cond
                              [(pcg? g)
                               (let* ([r (pcg-next g)]
                                      [val (car r)]
                                      [ng (cdr r)])
                                     ;; PCG gives 32 bits = 4 bytes
                                     (let byte-loop ([j 0] [v val])
                                          (if (or (>= (+ i j) n) (>= j 4))
                                              (loop (+ i 4) ng)
                                              (begin
                                               (bytevector-u8-set! bv (+ i j) (bitwise-and v #xff))
                                               (byte-loop (+ j 1) (ash v -8))))))]
                              
                              [(or (splitmix? g) (xorshift128? g))
                               (let* ([next (if (splitmix? g) splitmix-next xorshift128-next)]
                                      [r (next g)]
                                      [val (car r)]
                                      [ng (cdr r)])
                                     ;; 64 bits = 8 bytes
                                     (let byte-loop ([j 0] [v val])
                                          (if (or (>= (+ i j) n) (>= j 8))
                                              (loop (+ i 8) ng)
                                              (begin
                                               (bytevector-u8-set! bv (+ i j) (bitwise-and v #xff))
                                               (byte-loop (+ j 1) (ash v -8))))))]
                              
                              [else (error 'random-bytes "unknown generator" g)]))))))))


(doc 'section 'generator-serialization-for-resumable-sessions)

(doc "Generator Serialization (for Resumable Sessions)")



(define (gen-serialize gen)
  (cond
   [(pcg? gen)
    `(pcg ,(pcg-state gen) ,(pcg-inc gen))]
   
   [(splitmix? gen)
    `(splitmix ,(splitmix-state gen))]
   
   [(xorshift128? gen)
    `(xorshift128 ,(xorshift128-s0 gen) ,(xorshift128-s1 gen))]
   
   [else (error 'gen-serialize "unknown generator type" gen)]))

(define (gen-deserialize sexpr)
  (case (car sexpr)
        [(pcg)
         (list 'pcg (cadr sexpr) (caddr sexpr))]
        
        [(splitmix)
         (list 'splitmix (cadr sexpr))]
        
        [(xorshift128)
         (list 'xorshift128 (cadr sexpr) (caddr sexpr))]
        
        [else (error 'gen-deserialize "unknown generator type" (car sexpr))]))

(define (gen->string gen)
  (format "~s" (gen-serialize gen)))

(define (string->gen str)
  (gen-deserialize (safe-parse-gen-state str)))

(define (safe-parse-gen-state str)
  (let* ([trimmed (string-trim str)]
         [len (string-length trimmed)])
        ;; Must start with ( and end with )
        (unless (and (> len 2)
                     (char=? (string-ref trimmed 0) #\()
                     (char=? (string-ref trimmed (- len 1)) #\)))
                (error 'safe-parse-gen-state "invalid format: expected parenthesized expression" str))
        ;; Extract content between parens
        (let* ([content (string-trim (substring trimmed 1 (- len 1)))]
               [tokens (split-on-whitespace content)])
              (when (null? tokens)
                    (error 'safe-parse-gen-state "invalid format: empty expression" str))
              (let ([tag (car tokens)]
                    [args (cdr tokens)])
                   (cond
                    [(string=? tag "pcg")
                     (unless (= (length args) 2)
                             (error 'safe-parse-gen-state "pcg requires exactly 2 arguments" str))
                     (list 'pcg
                           (safe-parse-integer (car args) str)
                           (safe-parse-integer (cadr args) str))]
                    [(string=? tag "splitmix")
                     (unless (= (length args) 1)
                             (error 'safe-parse-gen-state "splitmix requires exactly 1 argument" str))
                     (list 'splitmix
                           (safe-parse-integer (car args) str))]
                    [(string=? tag "xorshift128")
                     (unless (= (length args) 2)
                             (error 'safe-parse-gen-state "xorshift128 requires exactly 2 arguments" str))
                     (list 'xorshift128
                           (safe-parse-integer (car args) str)
                           (safe-parse-integer (cadr args) str))]
                    [else
                     (error 'safe-parse-gen-state "unknown generator type" tag)])))))

(define (safe-parse-integer s original-input)
  (let ([len (string-length s)])
       (when (= len 0)
             (error 'safe-parse-gen-state "invalid integer: empty string" original-input))
       ;; Check all characters are digits
       (let loop ([i 0])
            (when (< i len)
                  (let ([c (string-ref s i)])
                       (unless (char<=? #\0 c #\9)
                               (error 'safe-parse-gen-state "invalid integer: non-digit character" s))
                       (loop (+ i 1)))))
       ;; Convert to integer
       (string->number s)))

(define (split-on-whitespace s)
  (let loop ([i 0] [start #f] [tokens '()])
       (if (>= i (string-length s))
           (reverse (if start
                        (cons (substring s start i) tokens)
                        tokens))
           (let ([c (string-ref s i)])
                (if (or (char=? c #\space) (char=? c #\tab) (char=? c #\newline))
                    (loop (+ i 1)
                          #f
                          (if start
                              (cons (substring s start i) tokens)
                              tokens))
                    (loop (+ i 1)
                          (or start i)
                          tokens))))))

(define (string-trim s)
  (let* ([len (string-length s)]
         [start (let loop ([i 0])
                     (if (and (< i len)
                              (char-whitespace? (string-ref s i)))
                         (loop (+ i 1))
                         i))]
         [end (let loop ([i len])
                   (if (and (> i start)
                            (char-whitespace? (string-ref s (- i 1))))
                       (loop (- i 1))
                       i))])
        (substring s start end)))

(define (char-whitespace? c)
  (or (char=? c #\space)
      (char=? c #\tab)
      (char=? c #\newline)
      (char=? c #\return)))

