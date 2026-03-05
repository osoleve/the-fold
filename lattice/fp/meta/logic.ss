;;; @module logic
;;; @requires prelude combinators stream

(require 'prelude)
(require 'combinators)
(require 'stream)

(doc 'module 'logic)
(doc 'purity 'partial)  ; Global mutable counter for logic variable identity
(doc 'description "Logic Variables Logic variables are placeholders that can be unified with values. Each variable has a unique identifier.")
(doc 'layer 'lattice)
(define *lvar-counter* 0)

;;; make-lvar : Symbol → LVar
;;; Create a new logic variable with an optional name hint.
(define (make-lvar name)
  (doc 'export #t)
  (set! *lvar-counter* (+ *lvar-counter* 1))
  (list 'lvar name *lvar-counter*))

;;; lvar? : α → Bool
(define (lvar? x)
  (doc 'export #t)
  (and (pair? x) (eq? (car x) 'lvar)))

;;; lvar-name : LVar → Symbol
(define (lvar-name v)
  (doc 'export #t)
  (list-ref v 1))

;;; lvar-id : LVar → Nat
(define (lvar-id v)
  (doc 'export #t)
  (list-ref v 2))

;;; lvar=? : LVar × LVar → Bool
(define (lvar=? v1 v2)
  (doc 'export #t)
  (and (lvar? v1) (lvar? v2) (= (lvar-id v1) (lvar-id v2))))

;;; ====
;;; Substitutions
;;; ====
;;;
;;; A substitution is a mapping from logic variables to values.
;;; We represent it as an association list.

;;; empty-subst : Substitution
(define empty-subst '())

;;; extend-subst : LVar × α × Substitution → Substitution
;;; Add a binding to a substitution.
(define (extend-subst var val subst)
  (doc 'export #t)
  (cons (cons var val) subst))

;;; lookup-subst : LVar × Substitution → (Maybe α)
;;; Look up a variable in a substitution.
(define (lookup-subst var subst)
  (doc 'export #t)
  (let loop ([s subst])
       (cond
        [(null? s) nothing]
        [(lvar=? (caar s) var) (just (cdar s))]
        [else (loop (cdr s))])))

;;; walk : α × Substitution → α
;;; Follow substitution chain to find the value of a term.
;;; If term is an unbound variable, return it.
(define (walk term subst)
  (doc 'export #t)
  (if (lvar? term)
      (let ([binding (lookup-subst term subst)])
           (if (nothing? binding)
               term
               (walk (from-just binding) subst)))
      term))

;;; walk* : α × Substitution → α
;;; Deeply walk a term, substituting all variables.
(define (walk* term subst)
  (doc 'export #t)
  (let ([v (walk term subst)])
       (cond
        [(lvar? v) v]
        [(pair? v) (cons (walk* (car v) subst) (walk* (cdr v) subst))]
        [else v])))

;;; ====
;;; Unification
;;; ====
;;;
;;; Unification finds a substitution that makes two terms equal.

;;; unify : α × α × Substitution → (Maybe Substitution)
;;; Attempt to unify two terms under a substitution.
(define (unify u v subst)
  (doc 'export #t)
  (let ([u (walk u subst)]
        [v (walk v subst)])
       (cond
        ;; Both are the same variable
        [(and (lvar? u) (lvar? v) (lvar=? u v)) (just subst)]
        ;; u is a variable - bind it
        [(lvar? u) (just (extend-subst u v subst))]
        ;; v is a variable - bind it
        [(lvar? v) (just (extend-subst v u subst))]
        ;; Both are pairs - unify recursively
        [(and (pair? u) (pair? v))
         (let ([s1 (unify (car u) (car v) subst)])
              (if (nothing? s1)
                  nothing
                  (unify (cdr u) (cdr v) (from-just s1))))]
        ;; Both are equal atoms
        [(equal? u v) (just subst)]
        ;; Cannot unify
        [else nothing])))

;;; ====
;;; Goal Monad
;;; ====
;;;
;;; A goal is a function from substitution to a stream of substitutions.
;;; Success produces substitutions, failure produces empty stream.
;;;
;;; Goal : Substitution -> Stream Substitution

;;; succeed : Substitution → (Stream Substitution)
;;; The goal that always succeeds with current substitution.
(define (succeed subst)
  (doc 'export #t)
  (stream-cons subst (lambda () stream-nil)))

;;; fail : Substitution → (Stream Substitution)
;;; The goal that always fails.
(define (fail subst)
  (doc 'export #t)
  stream-nil)

;;; == : α × α → Goal
;;; Unification goal: succeed if terms can be unified.
(define (== u v)
  (doc 'export #t)
  (lambda (subst)
          (let ([result (unify u v subst)])
               (if (nothing? result)
                   stream-nil
                   (stream-cons (from-just result) (lambda () stream-nil))))))

;;; =/= : α × α → Goal
;;; Disequality goal: succeed if terms cannot be unified.
(define (=/= u v)
  (doc 'export #t)
  (lambda (subst)
          (let ([result (unify u v subst)])
               (if (nothing? result)
                   (stream-cons subst (lambda () stream-nil))
                   stream-nil))))

;;; ====
;;; Goal Combinators
;;; ====

;;; conj : Goal × Goal → Goal
;;; Conjunction: both goals must succeed.
(define (conj g1 g2)
  (doc 'export #t)
  (lambda (subst)
          (stream-flatmap g2 (g1 subst))))

;;; disj : Goal × Goal → Goal
;;; Disjunction: either goal may succeed.
(define (disj g1 g2)
  (doc 'export #t)
  (lambda (subst)
          (stream-interleave (g1 subst) (g2 subst))))

;;; conj* : (List Goal) → Goal
;;; Conjunction of multiple goals.
(define (conj* goals)
  (doc 'export #t)
  (if (null? goals)
      succeed
      (conj (car goals) (conj* (cdr goals)))))

;;; disj* : (List Goal) → Goal
;;; Disjunction of multiple goals.
(define (disj* goals)
  (doc 'export #t)
  (if (null? goals)
      fail
      (disj (car goals) (disj* (cdr goals)))))

;;; conde : (List (List Goal)) → Goal
;;; Conditional: try each clause (conjunction of goals).
(define (conde clauses)
  (doc 'export #t)
  (disj* (map conj* clauses)))

;;; ====
;;; Fresh Variables
;;; ====

;;; call/fresh : (LVar → Goal) → Goal
;;; Create a fresh logic variable and pass it to a goal function.
(define (call/fresh f)
  (doc 'export #t)
  (lambda (subst)
          (let ([var (make-lvar '_)])
               ((f var) subst))))

;;; fresh1 : (LVar → Goal) → Goal
(define fresh1 call/fresh)

;;; fresh2 : (LVar → LVar → Goal) → Goal
(define (fresh2 f)
  (doc 'export #t)
  (call/fresh (lambda (a)
                      (call/fresh (lambda (b)
                                          (f a b))))))

;;; fresh3 : (LVar → LVar → LVar → Goal) → Goal
(define (fresh3 f)
  (doc 'export #t)
  (call/fresh (lambda (a)
                      (call/fresh (lambda (b)
                                          (call/fresh (lambda (c)
                                                              (f a b c))))))))

;;; fresh4 : (LVar → LVar → LVar → LVar → Goal) → Goal
(define (fresh4 f)
  (doc 'export #t)
  (call/fresh (lambda (a)
                      (call/fresh (lambda (b)
                                          (call/fresh (lambda (c)
                                                              (call/fresh (lambda (d)
                                                                                  (f a b c d))))))))))

;;; ====
;;; Running Goals
;;; ====

;;; run-goal : Nat × Goal → (List Substitution)
;;; Run a goal and collect up to n solutions.
(define (run-goal n goal)
  (doc 'export #t)
  (stream->list n (goal empty-subst)))

;;; run* : Nat × (LVar → Goal) → (List α)
;;; Run with fresh variable and reify results.
(define (run* n f)
  (doc 'export #t)
  (let ([var (make-lvar 'q)])
       (map (lambda (subst) (reify var subst))
            (run-goal n (f var)))))

;;; ====
;;; Reification
;;; ====
;;;
;;; Reification converts logic variables to readable values.

;;; reify : α × Substitution → α
;;; Walk a term and replace unbound variables with symbols.
(define (reify term subst)
  (doc 'export #t)
  (let ([v (walk* term subst)])
       (reify-s v (build-reify-subst v empty-subst))))

;;; build-reify-subst : α × Substitution → Substitution
(define (build-reify-subst term subst)
  (doc 'export #t)
  (let ([v (walk term subst)])
       (cond
        [(lvar? v)
         (let ([n (length subst)])
              (extend-subst v (reify-name n) subst))]
        [(pair? v)
         (build-reify-subst (cdr v)
                            (build-reify-subst (car v) subst))]
        [else subst])))

;;; reify-name : Nat → Symbol
(define (reify-name n)
  (doc 'export #t)
  (string->symbol (string-append "_." (number->string n))))

;;; reify-s : α × Substitution → α
(define (reify-s term subst)
  (doc 'export #t)
  (let ([v (walk term subst)])
       (cond
        [(lvar? v) v]  ; Should have been replaced
        [(pair? v) (cons (reify-s (car v) subst) (reify-s (cdr v) subst))]
        [else v])))

;;; ====
;;; List Relations
;;; ====

;;; conso : α × (List α) × (List α) → Goal
;;; Relational cons: (cons head tail) == list
(define (conso head tail lst)
  (doc 'export #t)
  (== (cons head tail) lst))

;;; nullo : α → Goal
;;; Succeed if argument is null.
(define (nullo x)
  (doc 'export #t)
  (== x '()))

;;; pairo : α → Goal
;;; Succeed if argument is a pair.
(define (pairo x)
  (doc 'export #t)
  (fresh2 (lambda (a d)
                  (conso a d x))))

;;; caro : (List α) × α → Goal
;;; Relational car: (car lst) == val
(define (caro lst val)
  (doc 'export #t)
  (fresh1 (lambda (d)
                  (conso val d lst))))

;;; cdro : (List α) × (List α) → Goal
;;; Relational cdr: (cdr lst) == val
(define (cdro lst val)
  (doc 'export #t)
  (fresh1 (lambda (a)
                  (conso a val lst))))

;;; appendo : (List α) × (List α) × (List α) → Goal
;;; Relational append: (append l1 l2) == out
(define (appendo l1 l2 out)
  (doc 'export #t)
  (disj
   (conj (nullo l1) (== l2 out))
   (fresh3 (lambda (head tail result)
                   (conj* (list
                           (conso head tail l1)
                           (conso head result out)
                           (appendo tail l2 result)))))))

;;; membero : α × (List α) → Goal
;;; Relational member: x is a member of lst
(define (membero x lst)
  (doc 'export #t)
  (fresh2 (lambda (head tail)
                  (conj (conso head tail lst)
                        (disj (== head x)
                              (membero x tail))))))

;;; lengtho : (List α) × Peano → Goal
;;; Relational length: (length lst) == n (using Peano numerals)
(define (lengtho lst n)
  (doc 'export #t)
  (disj
   (conj (nullo lst) (== n 'zero))
   (fresh3 (lambda (head tail m)
                   (conj* (list
                           (conso head tail lst)
                           (== n (list 'succ m))
                           (lengtho tail m)))))))

;;; reverseo : Value → Value → Goal
;;; Relational reverse: (reverse lst) == out
(define (reverseo lst out)
  (doc 'export #t)
  (reverse-acc lst '() out))

;;; reverse-acc : Value × Value × Value → Goal
(define (reverse-acc lst acc out)
  (doc 'export #t)
  (disj
   (conj (nullo lst) (== acc out))
   (fresh2 (lambda (head tail)
                   (conj* (list
                           (conso head tail lst)
                           (reverse-acc tail (cons head acc) out)))))))

;;; ====
;;; Arithmetic Relations (Peano)
;;; ====
;;;
;;; We use Peano numerals: zero, (succ zero), (succ (succ zero)), ...

;;; zeroo : Peano → Goal
(define (zeroo n)
  (doc 'export #t)
  (== n 'zero))

;;; succo : Peano × Peano → Goal
(define (succo n m)
  (doc 'export #t)
  (== m (list 'succ n)))

;;; pluso : Peano × Peano × Peano → Goal
;;; Relational addition: x + y == z
(define (pluso x y z)
  (doc 'export #t)
  (disj
   (conj (zeroo x) (== y z))
   (fresh2 (lambda (x-1 z-1)
                   (conj* (list
                           (succo x-1 x)
                           (succo z-1 z)
                           (pluso x-1 y z-1)))))))

;;; minuso : Peano × Peano × Peano → Goal
;;; Relational subtraction: x - y == z (x >= y)
(define (minuso x y z)
  (doc 'export #t)
  (pluso z y x))

;;; lesso : Peano × Peano → Goal
;;; Less than relation (strict).
(define (lesso x y)
  (doc 'export #t)
  (fresh1 (lambda (diff)
                  (conj (succo diff y)  ; y has a successor component
                        (minuso y x diff)))))

;;; ====
;;; Convenience: Converting Numbers
;;; ====

;;; nat->peano : Nat → Peano
(define (nat->peano n)
  (doc 'export #t)
  (if (<= n 0)
      'zero
      (list 'succ (nat->peano (- n 1)))))

;;; peano->nat : Peano → Nat
(define (peano->nat p)
  (doc 'export #t)
  (if (eq? p 'zero)
      0
      (+ 1 (peano->nat (cadr p)))))

;;; ====
;;; Occurs Check
;;; ====
;;;
;;; The occurs check prevents creating circular structures.

;;; occurs? : LVar × α × Substitution → Bool
;;; Check if variable occurs in term.
(define (occurs? var term subst)
  (doc 'export #t)
  (let ([v (walk term subst)])
       (cond
        [(lvar? v) (lvar=? var v)]
        [(pair? v) (or (occurs? var (car v) subst)
                       (occurs? var (cdr v) subst))]
        [else #f])))

;;; unify-check : α × α × Substitution → (Maybe Substitution)
;;; Unify with occurs check.
(define (unify-check u v subst)
  (doc 'export #t)
  (let ([u (walk u subst)]
        [v (walk v subst)])
       (cond
        [(and (lvar? u) (lvar? v) (lvar=? u v)) (just subst)]
        [(lvar? u)
         (if (occurs? u v subst)
             nothing
             (just (extend-subst u v subst)))]
        [(lvar? v)
         (if (occurs? v u subst)
             nothing
             (just (extend-subst v u subst)))]
        [(and (pair? u) (pair? v))
         (let ([s1 (unify-check (car u) (car v) subst)])
              (if (nothing? s1)
                  nothing
                  (unify-check (cdr u) (cdr v) (from-just s1))))]
        [(equal? u v) (just subst)]
        [else nothing])))

;;; ==/check : α × α → Goal
;;; Unification goal with occurs check.
(define (==/check u v)
  (doc 'export #t)
  (lambda (subst)
          (let ([result (unify-check u v subst)])
               (if (nothing? result)
                   stream-nil
                   (stream-cons (from-just result) (lambda () stream-nil))))))

;;; ====
;;; Constraint Store (Simple)
;;; ====
;;;
;;; For more complex logic programming, we'd want constraints.
;;; This is a minimal implementation.

;;; make-constraint-store : Substitution × (List Constraint) → ConstraintStore
(define (make-constraint-store subst constraints)
  (doc 'export #t)
  (list 'cstore subst constraints))

;;; cstore-subst : ConstraintStore → Substitution
(define (cstore-subst cs)
  (doc 'export #t)
  (list-ref cs 1))

;;; cstore-constraints : ConstraintStore → (List Constraint)
(define (cstore-constraints cs)
  (doc 'export #t)
  (list-ref cs 2))

;;; ====
;;; Debugging Utilities
;;; ====

;;; show-subst : Substitution → (List (Symbol × α))
;;; String representation
(define (show-subst subst)
  (doc 'export #t)
  (map (lambda (binding)
               (cons (lvar-name (car binding)) (cdr binding)))
       subst))

;;; trace-goal : String × Goal → Goal
;;; Wrap a goal with tracing output.
(define (trace-goal label goal)
  (doc 'export #t)
  (lambda (subst)
          (display (string-append "TRACE " label ": "))
          (display (show-subst subst))
          (newline)
          (goal subst)))
