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
