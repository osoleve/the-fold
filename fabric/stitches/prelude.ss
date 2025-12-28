;;; fabric/stitches/prelude.ss — Shared Utilities for Core Modules
;;;
;;; Common pure functions used across core/ modules.
;;; This is the ONLY place for these implementations.
;;;
;;; This is Core code: pure, total, assumes perfect input.
;;;
;;; Dependencies: NONE (this is the foundation)
;;;
;;; See fabric/stitches/MODULES.md for dependency graph.

;;; ============================================================
;;; List Predicates
;;; ============================================================

;;; andmap : (α → Bool) × (List α) → Bool
;;; Apply predicate to all elements, return true if all pass.
;;; Returns #t for empty list (vacuous truth).
(define (andmap pred lst)
  (or (null? lst)
      (and (pred (car lst))
           (andmap pred (cdr lst)))))

;;; ormap : (α → Bool) × (List α) → Bool
;;; Apply predicate to all elements, return true if any pass.
;;; Returns #f for empty list.
(define (ormap pred lst)
  (and (pair? lst)
       (or (pred (car lst))
           (ormap pred (cdr lst)))))

;;; ============================================================
;;; List Utilities
;;; ============================================================

;;; unique : (List α) → (List α)
;;; Remove duplicates, preserving first occurrence order.
;;; Uses eq? for comparison.
(define (unique lst)
  (let loop ([lst lst] [seen '()] [acc '()])
    (cond
      [(null? lst) (reverse acc)]
      [(memq (car lst) seen) (loop (cdr lst) seen acc)]
      [else (loop (cdr lst) (cons (car lst) seen) (cons (car lst) acc))])))

;;; filter : (α → Bool) × (List α) → (List α)
;;; Keep only elements satisfying predicate.
(define (filter pred lst)
  (cond
    [(null? lst) '()]
    [(pred (car lst)) (cons (car lst) (filter pred (cdr lst)))]
    [else (filter pred (cdr lst))]))

;;; fold-left : (β × α → β) × β × (List α) → β
;;; Left-associative fold.
(define (fold-left f acc lst)
  (if (null? lst)
      acc
      (fold-left f (f acc (car lst)) (cdr lst))))

;;; fold-right : (α × β → β) × β × (List α) → β
;;; Right-associative fold.
(define (fold-right f acc lst)
  (if (null? lst)
      acc
      (f (car lst) (fold-right f acc (cdr lst)))))

;;; zip : (List α) × (List β) → (List (Pair α β))
;;; Zip two lists together. Stops at shorter list.
(define (zip xs ys)
  (if (or (null? xs) (null? ys))
      '()
      (cons (cons (car xs) (car ys))
            (zip (cdr xs) (cdr ys)))))

;;; iota : Nat → (List Nat)
;;; Generate list [0, 1, ..., n-1].
(define (iota n)
  (let loop ([i 0] [acc '()])
    (if (= i n)
        (reverse acc)
        (loop (+ i 1) (cons i acc)))))

;;; take : Nat × (List α) → (List α)
;;; Take first n elements.
(define (take n lst)
  (if (or (= n 0) (null? lst))
      '()
      (cons (car lst) (take (- n 1) (cdr lst)))))

;;; drop : Nat × (List α) → (List α)
;;; Drop first n elements.
(define (drop n lst)
  (if (or (= n 0) (null? lst))
      lst
      (drop (- n 1) (cdr lst))))

;;; ============================================================
;;; Collection Utilities (Missing Functional Primitives)
;;; ============================================================

;;; identity : α → α
;;; The identity function - returns its argument unchanged.
(define (identity x) x)

;;; flatten : (List (List α)) → (List α)
;;; Flatten a list of lists into a single list.
(define (flatten lst-of-lists)
  (fold-right append '() lst-of-lists))

;;; partition : (α → Bool) × (List α) → (List (List α) (List α))
;;; Partition list into two lists: those satisfying predicate, and those that don't.
(define (partition pred lst)
  (let loop ([lst lst] [yes '()] [no '()])
    (cond
      [(null? lst) (list (reverse yes) (reverse no))]
      [(pred (car lst)) (loop (cdr lst) (cons (car lst) yes) no)]
      [else (loop (cdr lst) yes (cons (car lst) no))])))

;;; group-by : (α → β) × (List α) → (List (Pair β (List α)))
;;; Group consecutive elements by key function result.
(define (group-by key-fn lst)
  (if (null? lst)
      '()
      (let ([first-key (key-fn (car lst))])
        (let loop ([remaining lst] [current-key first-key] [current-group '()] [result '()])
          (cond
            [(null? remaining)
             (reverse (cons (cons current-key (reverse current-group)) result))]
            [(equal? (key-fn (car remaining)) current-key)
             (loop (cdr remaining) current-key (cons (car remaining) current-group) result)]
            [else
             (let ([new-key (key-fn (car remaining))])
               (loop (cdr remaining) new-key (list (car remaining)) 
                     (cons (cons current-key (reverse current-group)) result)))])))))

;;; distinct-by : (α → β) × (List α) → (List α)
;;; Remove duplicates based on key function, preserving first occurrence order.
(define (distinct-by key-fn lst)
  (let loop ([lst lst] [seen '()] [result '()])
    (cond
      [(null? lst) (reverse result)]
      [else
       (let* ([elem (car lst)]
              [key (key-fn elem)])
         (if (member key seen)
             (loop (cdr lst) seen result)
             (loop (cdr lst) (cons key seen) (cons elem result))))])))

;;; ============================================================
;;; Result Type (Standardized Error Handling)
;;; ============================================================

;;; All core modules use:
;;;   (ok value)           — success
;;;   (error tag details)  — failure
;;;
;;; This provides a standard way to handle errors functionally.

;;; ok? : Any → Bool
;;; Is this a successful result?
(define (ok? result)
  (and (pair? result) (eq? (car result) 'ok)))

;;; error? : Any → Bool
;;; Is this an error result?
(define (error? result)
  (and (pair? result) (eq? (car result) 'error)))

;;; unwrap-ok : Result → Value
;;; Extract value from (ok value). Assumes result is ok.
(define (unwrap-ok result)
  (cadr result))

;;; unwrap-error : Result → (tag . details)
;;; Extract error from (error tag ...). Assumes result is error.
(define (unwrap-error result)
  (cdr result))

;;; result-map : (α → β) × Result α → Result β
;;; Apply function to ok value, pass through errors.
(define (result-map f result)
  (if (ok? result)
      `(ok ,(f (unwrap-ok result)))
      result))

;;; result-bind : Result α × (α → Result β) → Result β
;;; Chain results, short-circuiting on error.
(define (result-bind result f)
  (if (ok? result)
      (f (unwrap-ok result))
      result))

;;; result-sequence : (List (Result α)) → Result (List α)
;;; Convert list of results to result of list.
;;; Short-circuits on first error.
(define (result-sequence results)
  (if (null? results)
      '(ok ())
      (let ([first (car results)])
        (if (error? first)
            first
            (let ([rest (result-sequence (cdr results))])
              (if (error? rest)
                  rest
                  `(ok ,(cons (unwrap-ok first) (unwrap-ok rest)))))))))

;;; ============================================================
;;; String Utilities
;;; ============================================================

;;; string-join : (List String) × String → String
;;; Join strings with separator.
(define (string-join strs sep)
  (if (null? strs)
      ""
      (fold-left (lambda (acc s) (string-append acc sep s))
                 (car strs)
                 (cdr strs))))

;;; ============================================================
;;; Debug Utilities
;;; ============================================================

;;; trace : String × α → α
;;; Print a debug message and return the value unchanged.
;;; Useful for debugging without changing code structure.
(define (trace msg val)
  (display msg)
  (display ": ")
  (write val)
  (newline)
  val)
