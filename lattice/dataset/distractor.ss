;;; lattice/dataset/distractor.ss — Plausible Wrong Answer Generation
;;; @module distractor
;;; @requires prelude random parameter

(load "core/base/prelude.ss")
(load "lattice/random/prng.ss")
(load "lattice/dataset/parameter.ss")

(doc 'module 'distractor)
(doc 'description "Strategies for generating plausible wrong answers (distractors) for multiple choice problems. Distractors should be wrong but reasonable - common mistakes, off-by-one errors, sign flips, etc.")
(doc 'layer 'lattice)
(doc 'purity 'partial)

;;; ============================================================
;;; Part 1: Distractor Strategy Types
;;; ============================================================
;;;
;;; Different strategies for generating wrong answers:
;;;   - nearby: perturb correct answer by small amount
;;;   - sign-flip: negate or flip direction
;;;   - off-by-one: common counting errors
;;;   - formula-error: wrong formula application
;;;   - unit-error: wrong units or scaling
;;;   - random-in-range: random value in valid range

(doc 'section 'strategies)

;;; make-distractor-strategy : Symbol × (Any × RNG → Any) → DistractorStrategy
(define (make-distractor-strategy name generator)
  (doc 'export #t)
  (list 'distractor-strategy name generator))

(define (distractor-strategy? x)
  (and (pair? x) (eq? (car x) 'distractor-strategy)))

(define (distractor-strategy-name ds) (list-ref ds 1))
(define (distractor-strategy-generator ds) (list-ref ds 2))

;;; ============================================================
;;; Part 2: Built-in Strategies
;;; ============================================================

(doc 'section 'builtin)

;;; distractor-nearby : Number × Number → DistractorStrategy
;;; Perturb by relative epsilon (e.g., 0.1 = ±10%)
;;; For zero values, uses absolute perturbation to avoid returning zero
(define (distractor-nearby epsilon)
  (doc 'export #t)
  (make-distractor-strategy
   'nearby
   (lambda (correct rng)
     (if (number? correct)
         (let* ([sign (if (< (prng-next-float rng) 0.5) 1 -1)]
                ;; Use max of relative and absolute delta to handle zero
                [base (if (zero? correct) 1.0 (abs correct))]
                [delta (* base epsilon (+ 0.5 (prng-next-float rng)))])
           (+ correct (* sign delta)))
         correct))))

;;; distractor-sign-flip : DistractorStrategy
;;; Negate the value (common physics error)
(doc distractor-sign-flip 'export #t)
(define distractor-sign-flip
  (make-distractor-strategy
   'sign-flip
   (lambda (correct rng)
     (if (number? correct)
         (- correct)
         correct))))

;;; distractor-off-by-one : DistractorStrategy
;;; Add or subtract 1 (counting errors)
(doc distractor-off-by-one 'export #t)
(define distractor-off-by-one
  (make-distractor-strategy
   'off-by-one
   (lambda (correct rng)
     (if (number? correct)
         (+ correct (if (< (prng-next-float rng) 0.5) 1 -1))
         correct))))

;;; distractor-double-half : DistractorStrategy
;;; Double or halve (factor of 2 errors)
(doc distractor-double-half 'export #t)
(define distractor-double-half
  (make-distractor-strategy
   'double-half
   (lambda (correct rng)
     (if (and (number? correct) (not (zero? correct)))
         (if (< (prng-next-float rng) 0.5)
             (* correct 2)
             (/ correct 2))
         correct))))

;;; distractor-wrong-sign-convention : DistractorStrategy
;;; Common physics error: using wrong sign convention
(doc distractor-wrong-sign-convention 'export #t)
(define distractor-wrong-sign-convention
  (make-distractor-strategy
   'wrong-sign-convention
   (lambda (correct rng)
     (if (number? correct)
         (- correct)  ; Sign flip
         correct))))

;;; distractor-order-swap : DistractorStrategy
;;; For comparisons - swap greater/less than
(doc distractor-order-swap 'export #t)
(define distractor-order-swap
  (make-distractor-strategy
   'order-swap
   (lambda (correct rng)
     (cond
       [(eq? correct 'greater) 'less]
       [(eq? correct 'less) 'greater]
       [(eq? correct 'A) 'B]
       [(eq? correct 'B) 'A]
       [else correct]))))

;;; distractor-random-choice : (List Any) → DistractorStrategy
;;; Pick random from list (excluding correct)
(define (distractor-random-choice choices)
  (doc 'export #t)
  (make-distractor-strategy
   'random-choice
   (lambda (correct rng)
     (let ([others (filter (lambda (c) (not (equal? c correct))) choices)])
       (if (null? others)
           correct
           (list-ref others (prng-next-int rng (length others))))))))

;;; distractor-formula-variant : (Any → Any) → DistractorStrategy
;;; Apply a wrong formula
(define (distractor-formula-variant wrong-formula)
  (doc 'export #t)
  (make-distractor-strategy
   'formula-variant
   (lambda (correct rng)
     (wrong-formula correct))))

;;; ============================================================
;;; Part 3: Distractor Generation
;;; ============================================================

(doc 'section 'generation)

;;; apply-distractor : DistractorStrategy × Any × RNG → Any
;;; Apply a strategy to generate one distractor
(define (apply-distractor strategy correct rng)
  (doc 'export #t)
  ((distractor-strategy-generator strategy) correct rng))

;;; generate-distractors : Any × (List DistractorStrategy) × Nat × RNG → (List Any)
;;; Generate n distinct distractors using given strategies
(define (generate-distractors correct strategies n rng)
  (doc 'export #t)
  (doc 'description "Generate n distinct wrong answers using given strategies")
  (let loop ([remaining n]
             [used-strategies strategies]
             [results '()]
             [attempts 0])
    (cond
      [(<= remaining 0) (reverse results)]
      [(>= attempts (* n 10))
       ;; Too many attempts, fill with nearby values
       (let ([filler (generate-filler-distractors correct remaining rng results)])
         (reverse (append (reverse filler) results)))]
      [(null? used-strategies)
       ;; Ran out of strategies, cycle through again
       (loop remaining strategies results attempts)]
      [else
       (let* ([strategy (car used-strategies)]
              [distractor (apply-distractor strategy correct rng)])
         (if (or (equal? distractor correct)
                 (member-approx? distractor results))
             ;; Duplicate, try next strategy
             (loop remaining (cdr used-strategies) results (+ attempts 1))
             ;; Valid distractor
             (loop (- remaining 1)
                   (cdr used-strategies)
                   (cons distractor results)
                   0)))])))

;;; member-approx? : Any × (List Any) → Boolean
;;; Check membership with approximate equality for numbers
(define (member-approx? x lst)
  (ormap (lambda (y) (approx-equal? x y)) lst))

;;; approx-equal? : Any × Any → Boolean
(define (approx-equal? a b)
  (cond
    [(and (number? a) (number? b))
     (< (abs (- a b)) (* 0.01 (max (abs a) (abs b) 1)))]
    [else (equal? a b)]))

;;; generate-filler-distractors : Any × Nat × RNG × (List Any) → (List Any)
;;; Generate fallback distractors when strategies are exhausted
(define (generate-filler-distractors correct n rng existing)
  (let loop ([remaining n] [results '()] [attempts 0])
    (cond
      [(<= remaining 0) results]
      [(>= attempts (* n 20)) results]  ; Give up
      [else
       (let ([val (if (number? correct)
                      (* correct (+ 0.5 (* 1.5 (prng-next-float rng))))
                      correct)])
         (if (or (equal? val correct)
                 (member-approx? val existing)
                 (member-approx? val results))
             (loop remaining results (+ attempts 1))
             (loop (- remaining 1) (cons val results) 0)))])))

;;; ============================================================
;;; Part 4: Domain-Specific Strategies
;;; ============================================================
;;;
;;; Pre-built strategy sets for different problem types.

(doc 'section 'domain-strategies)

;;; numeric-distractors : (List DistractorStrategy)
;;; Standard strategies for numeric answers
(doc numeric-distractors 'export #t)
(define numeric-distractors
  (list (distractor-nearby 0.15)
        (distractor-nearby 0.30)
        distractor-sign-flip
        distractor-double-half))

;;; counting-distractors : (List DistractorStrategy)
;;; Strategies for counting problems
(doc counting-distractors 'export #t)
(define counting-distractors
  (list distractor-off-by-one
        (distractor-nearby 0.5)
        distractor-double-half))

;;; comparison-distractors : (List DistractorStrategy)
;;; Strategies for A vs B comparisons
(doc comparison-distractors 'export #t)
(define comparison-distractors
  (list distractor-order-swap
        (distractor-random-choice '(A B same cannot-determine))
        (distractor-random-choice '(A B same cannot-determine))))

;;; velocity-distractors : (List DistractorStrategy)
;;; Strategies for velocity problems (physics)
(doc velocity-distractors 'export #t)
(define velocity-distractors
  (list (distractor-nearby 0.2)
        distractor-sign-flip
        distractor-double-half
        (distractor-nearby 0.4)))

;;; ============================================================
;;; Part 5: Multiple Choice Assembly
;;; ============================================================

(doc 'section 'assembly)

;;; format-numeric-option : Number → String
;;; Format a number for display as an option
(define (format-numeric-option n)
  (doc 'export #t)
  (if (integer? n)
      (number->string (inexact->exact n))
      (let ([s (number->string (exact->inexact n))])
        ;; Truncate to reasonable precision
        (if (> (string-length s) 8)
            (let ([dot-pos (string-find s #\.)])
              (if dot-pos
                  (substring s 0 (min (string-length s) (+ dot-pos 3)))
                  (substring s 0 8)))
            s))))

;;; string-find : String × Char → Nat | #f
(define (string-find s c)
  (let loop ([i 0])
    (cond
      [(>= i (string-length s)) #f]
      [(char=? (string-ref s i) c) i]
      [else (loop (+ i 1))])))

;;; make-numeric-options : Number × (List DistractorStrategy) × RNG × (Number → String) → (List (Symbol . String)) × Symbol
;;; Generate options with correct answer randomly placed
;;; Returns (options . correct-label)
;;; Ensures uniqueness after formatting (handles integer rounding collisions)
(define (make-numeric-options correct strategies rng formatter)
  (doc 'export #t)
  (let* ([correct-str (formatter correct)]
         ;; Generate unique formatted distractors
         [distractor-strs (generate-unique-formatted-distractors
                           correct correct-str strategies 3 rng formatter)]
         [all-strs (cons correct-str distractor-strs)]
         [sorted (list-sort string<? all-strs)]
         [labels '(A B C D)]
         [options (map cons labels sorted)]
         [correct-label (find-label-for-string options correct-str)])
    (cons options correct-label)))

;;; generate-unique-formatted-distractors : Number × String × Strategies × Nat × RNG × Formatter → (List String)
;;; Generate n distractor strings that are unique from correct and each other
(define (generate-unique-formatted-distractors correct correct-str strategies n rng formatter)
  (let loop ([remaining n]
             [used-strategies strategies]
             [results '()]
             [attempts 0])
    (cond
      [(<= remaining 0) (reverse results)]
      [(>= attempts (* n 20))
       ;; Exhausted attempts, fill with incremental values
       (let ([fillers (generate-incremental-fillers correct correct-str remaining results formatter)])
         (reverse (append (reverse fillers) results)))]
      [(null? used-strategies)
       ;; Cycle through strategies again
       (loop remaining strategies results attempts)]
      [else
       (let* ([strategy (car used-strategies)]
              [distractor (apply-distractor strategy correct rng)]
              [distractor-str (formatter distractor)])
         (if (or (string=? distractor-str correct-str)
                 (member distractor-str results))
             ;; Duplicate formatted string, try next
             (loop remaining (cdr used-strategies) results (+ attempts 1))
             ;; Unique formatted string
             (loop (- remaining 1)
                   (cdr used-strategies)
                   (cons distractor-str results)
                   0)))])))

;;; generate-incremental-fillers : Number × String × Nat × (List String) × Formatter → (List String)
;;; Generate fallback distractors by adding/subtracting fixed amounts
(define (generate-incremental-fillers correct correct-str n existing formatter)
  (let loop ([delta 1] [results '()] [remaining n])
    (if (<= remaining 0)
        results
        (let* ([plus-val (+ correct delta)]
               [minus-val (- correct delta)]
               [plus-str (formatter plus-val)]
               [minus-str (formatter minus-val)]
               [all-existing (append existing results (list correct-str))])
          (cond
            ;; Try plus first
            [(and (> remaining 0)
                  (not (member plus-str all-existing)))
             (loop delta (cons plus-str results) (- remaining 1))]
            ;; Try minus
            [(and (> remaining 0)
                  (positive? minus-val)  ; Don't go negative for counts
                  (not (member minus-str (append existing results (list correct-str)))))
             (loop delta (cons minus-str results) (- remaining 1))]
            ;; Increase delta
            [else (loop (+ delta 1) results remaining)])))))

;;; find-label-for-string : (List (Symbol . String)) × String → Symbol
(define (find-label-for-string options target)
  (let loop ([opts options])
    (if (null? opts)
        'A  ; Fallback
        (if (string=? (cdar opts) target)
            (caar opts)
            (loop (cdr opts))))))

;;; make-symbolic-options : Symbol × (List Symbol) × RNG → (List (Symbol . String)) × Symbol
;;; Generate options for symbolic answers (A, B, same, etc.)
(define (make-symbolic-options correct all-choices rng)
  (doc 'export #t)
  (let* ([others (filter (lambda (c) (not (eq? c correct))) all-choices)]
         [distractors (take-up-to 3 (shuffle-list others rng))]
         [all-values (cons correct distractors)]
         [shuffled (shuffle-list all-values rng)]
         [labels '(A B C D)]
         [options (map (lambda (label val)
                        (cons label (symbol->string val)))
                      labels shuffled)]
         [correct-label (car (filter (lambda (l)
                                       (string=? (cdr (assq l options))
                                                (symbol->string correct)))
                                    labels))])
    (cons options correct-label)))

;;; shuffle-list : (List a) × RNG → (List a)
;;; Proper Fisher-Yates shuffle
(define (shuffle-list lst rng)
  (let* ([v (list->vector lst)]
         [n (vector-length v)])
    (let loop ([i (- n 1)])
      (when (> i 0)
        (let* ([j (prng-next-int rng (+ i 1))]
               [tmp (vector-ref v i)])
          (vector-set! v i (vector-ref v j))
          (vector-set! v j tmp)
          (loop (- i 1)))))
    (vector->list v)))

;;; take-up-to : Nat × (List a) → (List a)
(define (take-up-to n xs)
  (if (or (<= n 0) (null? xs))
      '()
      (cons (car xs) (take-up-to (- n 1) (cdr xs)))))
