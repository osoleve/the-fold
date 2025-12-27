;;; playpen/loom/spell/pipe.ss — Enhanced Pipe Macros
;;;
;;; Extends the basic -> threading macro with conditionals, bindings, and
;;; mutation variants. The pipe macro is foundational to the Spell DSL.
;;;
;;; This is part of Spell: the DSL for weaving game logic.
;;;
;;; Exports:
;;;   ->     Enhanced threading macro with :when, :unless, :let, :as
;;;   ->!    Mutation threading for side-effecting operations
;;;   ->>    Thread-last variant (value at end of argument list)

;;; ============================================================
;;; Enhanced Threading Macro (->)
;;; ============================================================

;;; -> : Value × Forms... -> Value
;;; Thread a value through a series of transformations.
;;;
;;; Basic usage (same as original):
;;;   (-> x (f a) (g b))  =>  (g (f x a) b)
;;;   (-> x f g)          =>  (g (f x))
;;;
;;; Conditional threading:
;;;   (-> x (:when pred) (f a))   =>  (if pred (f x a) x)
;;;   (-> x (:unless pred) (f a)) =>  (if pred x (f x a))
;;;
;;; Bindings:
;;;   (-> x (:let name) (f name)) =>  (let ([name x]) (f name))
;;;   (-> x (:as name) ...)       =>  Same as :let but for debugging
;;;
;;; Example:
;;;   (-> entity
;;;       (:when (entity-alive? entity))
;;;       (entity-damage 10)
;;;       (:let e)
;;;       (entity-add-effect e 'poisoned))

(define-syntax ->
  (syntax-rules (:when :unless :let :as)
    ;; Base case: single value
    [(_ val) val]

    ;; Conditional threading: only apply if predicate is true
    [(_ val (:when pred) (fn args ...) rest ...)
     (let ([v val])
       (-> (if pred (fn v args ...) v) rest ...))]

    [(_ val (:when pred) fn rest ...)
     (let ([v val])
       (-> (if pred (fn v) v) rest ...))]

    ;; Unless: apply if predicate is false
    [(_ val (:unless pred) (fn args ...) rest ...)
     (let ([v val])
       (-> (if pred v (fn v args ...)) rest ...))]

    [(_ val (:unless pred) fn rest ...)
     (let ([v val])
       (-> (if pred v (fn v)) rest ...))]

    ;; Let binding: bind current value to name for use in later forms
    [(_ val (:let name) rest ...)
     (let ([name val])
       (-> name rest ...))]

    ;; As: alias for :let (semantic clarity)
    [(_ val (:as name) rest ...)
     (let ([name val])
       (-> name rest ...))]

    ;; Standard threading with arguments
    [(_ val (fn args ...) rest ...)
     (-> (fn val args ...) rest ...)]

    ;; Standard threading without arguments
    [(_ val fn rest ...)
     (-> (fn val) rest ...)]))


;;; ============================================================
;;; Mutation Threading Macro (->!)
;;; ============================================================

;;; ->! : Object × (Mutator Args...)... -> Void
;;; Chain side-effecting operations on a mutable object.
;;; Unlike ->, this doesn't thread the value; it passes the same
;;; object to each mutator in sequence.
;;;
;;; Example:
;;;   (->! canvas
;;;        (canvas-set! 0 0 #\#)
;;;        (canvas-set! 1 0 #\#)
;;;        (canvas-set! 2 0 #\#))
;;;
;;; Expands to:
;;;   (begin
;;;     (canvas-set! canvas 0 0 #\#)
;;;     (canvas-set! canvas 1 0 #\#)
;;;     (canvas-set! canvas 2 0 #\#))

(define-syntax ->!
  (syntax-rules ()
    [(_ obj) (void)]
    [(_ obj (fn args ...) rest ...)
     (begin
       (fn obj args ...)
       (->! obj rest ...))]))


;;; ============================================================
;;; Thread-Last Macro (->>)
;;; ============================================================

;;; ->> : Value × Forms... -> Value
;;; Thread a value as the LAST argument to each form.
;;; Useful for functions that take the collection/target last.
;;;
;;; Example:
;;;   (->> items
;;;        (filter predicate)
;;;        (map transform)
;;;        (fold-left + 0))
;;;
;;; Expands to:
;;;   (fold-left + 0
;;;     (map transform
;;;       (filter predicate items)))

(define-syntax ->>
  (syntax-rules ()
    [(_ val) val]
    [(_ val (fn args ...) rest ...)
     (->> (fn args ... val) rest ...)]
    [(_ val fn rest ...)
     (->> (fn val) rest ...)]))


;;; ============================================================
;;; Conditional Forms
;;; ============================================================

;;; when-> : Pred × Value × Forms... -> Value
;;; Only thread if predicate is true, otherwise return value unchanged.
;;;
;;; Example:
;;;   (when-> (> damage 0) entity (entity-damage damage))

(define-syntax when->
  (syntax-rules ()
    [(_ pred val forms ...)
     (if pred
         (-> val forms ...)
         val)]))

;;; unless-> : Pred × Value × Forms... -> Value
;;; Only thread if predicate is false.
(define-syntax unless->
  (syntax-rules ()
    [(_ pred val forms ...)
     (if pred
         val
         (-> val forms ...))]))
