;;; lattice/data/stack.ss — LIFO Stack
;;;
;;; Immutable functional stack using simple list representation.
;;; All operations return new structures without modifying originals.
;;;
;;; Stack α = (List α)
;;;
;;; TIER: 0 (no lattice dependencies)

;;; stack-empty : Stack
;;; The empty stack.
(define stack-empty '())

;;; stack-empty? : Stack → Boolean
;;; Check if stack is empty.
(define (stack-empty? stack)
  (null? stack))

;;; stack-push : α Stack → Stack
;;; Push element onto stack. Returns new stack.
(define (stack-push elem stack)
  (cons elem stack))

;;; stack-pop : Stack → (Values Stack α)
;;; Pop top element from stack. Returns (new-stack, element).
;;; Error if stack is empty.
(define (stack-pop stack)
  (if (null? stack)
      (error 'stack-pop "Cannot pop from empty stack")
      (values (cdr stack) (car stack))))

;;; stack-peek : Stack → α
;;; Get top element without removing. Error if empty.
(define (stack-peek stack)
  (if (null? stack)
      (error 'stack-peek "Cannot peek empty stack")
      (car stack)))

;;; stack-size : Stack → Nat
;;; Get number of elements in stack.
(define (stack-size stack)
  (length stack))

;;; stack->list : (Stack α) → (List α)
;;; Convert stack to list (top to bottom).
(define (stack->list stack)
  stack)

;;; list->stack : (List α) → (Stack α)
;;; Convert list to stack (first element becomes top).
(define (list->stack lst)
  lst)
