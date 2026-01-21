;;; lattice/data/stack.ss — LIFO Stack
;;; @module stack
;;; @requires prelude

(load "core/base/prelude.ss")

(doc 'module 'stack)
(doc 'description "LIFO Stack")
(doc 'layer 'lattice)
(doc 'tier 0)
(doc 'purity 'total)
(doc 'note "Immutable functional stack using simple list representation")

(define stack-empty '())
(doc stack-empty 'type 'Stack)
(doc stack-empty 'description "The empty stack")

(define (stack-empty? stack)
  (doc 'type (-> Stack Boolean))
  (doc 'description "Check if stack is empty")
  (null? stack))

(define (stack-push elem stack)
  (doc 'type (-> α Stack Stack))
  (doc 'description "Push element onto stack")
  (cons elem stack))

(define (stack-pop stack)
  (doc 'type (-> Stack (Values Stack α)))
  (doc 'description "Pop top element from stack")
  (if (null? stack)
      (error 'stack-pop "Cannot pop from empty stack")
      (values (cdr stack) (car stack))))

(define (stack-peek stack)
  (doc 'type (-> Stack α))
  (doc 'description "Get top element without removing")
  (if (null? stack)
      (error 'stack-peek "Cannot peek empty stack")
      (car stack)))

(define (stack-size stack)
  (doc 'type (-> Stack Nat))
  (doc 'description "Get number of elements in stack")
  (length stack))

(define (stack->list stack)
  (doc 'type (-> Stack (List α)))
  (doc 'description "Convert stack to list (top to bottom)")
  stack)

(define (list->stack lst)
  (doc 'type (-> (List α) Stack))
  (doc 'description "Convert list to stack (first element becomes top)")
  lst)
