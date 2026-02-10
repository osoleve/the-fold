;;; @module rewrite/proof-tactics
;;; @requires prelude engine rewrite/laws

(require 'prelude)
(require 'engine)
(require 'rewrite/laws)

(doc 'module 'rewrite/proof-tactics)
(doc 'description "Proof Tactics Wrapping Rewrite Strategies")
(doc 'layer 'lattice)

(doc 'description "Provides proof tactics that expose engine.ss strategies to users for
equational reasoning. Tactics transform proof goals into subgoals,
building on the rewrite engine's pattern matching and strategy combinators.

Core Tactics:
  tactic-refl        : Prove x = x reflexively
  tactic-sym         : Transform y = x goal to x = y
  tactic-trans       : Split x = z into x = mid, mid = z
  tactic-rewrite     : Apply a named law from the registry

Strategy-Based Tactics:
  tactic-topdown     : Apply strategy top-down through expression
  tactic-bottomup    : Apply strategy bottom-up through expression
  tactic-try         : Apply tactic, succeed even if it fails
  tactic-repeat      : Apply tactic until fixed point

Automation:
  tactic-auto        : Try common tactics automatically
  tactic-search      : Bounded search with cycle detection

Tactic Combinators:
  tactic-then        : Sequential composition
  tactic-orelse      : Try first, then second on failure

This is Lattice code: pure, total, assumes perfect input.")

;;; ====
;;; Proof Goal Structure
;;; ====
;;;
;;; A proof goal represents an equality to be proved.
;;; Structure: (goal lhs rhs context)
;;;   - lhs: Left-hand side expression
;;;   - rhs: Right-hand side expression
;;;   - context: Association list of assumptions (name . type)
;;;
;;; The goal represents: prove that lhs = rhs under context.

;;; make-proof-goal : Expr x Expr x Context -> Goal
(define (make-proof-goal lhs rhs ctx)
  `(goal ,lhs ,rhs ,ctx))

;;; proof-goal? : Any -> Boolean
(define (proof-goal? g)
  (and (pair? g)
       (eq? (car g) 'goal)
       (= (length g) 4)))

;;; goal-lhs : Goal -> Expr
(define (goal-lhs g)
  (if (proof-goal? g) (cadr g) #f))

;;; goal-rhs : Goal -> Expr
(define (goal-rhs g)
  (if (proof-goal? g) (caddr g) #f))

;;; goal-context : Goal -> Context
(define (goal-context g)
  (if (proof-goal? g) (cadddr g) '()))

;;; goal-trivial? : Goal -> Boolean
;;; Check if goal is syntactically trivial (lhs = rhs).
(define (goal-trivial? g)
  (and (proof-goal? g)
       (equal? (goal-lhs g) (goal-rhs g))))

;;; ====
;;; Tactic Results
;;; ====
;;;
;;; A tactic returns either:
;;;   (tactic-success subgoals builder) - success with new subgoals
;;;   (tactic-failure reason)           - failure with explanation
;;;
;;; The builder is a function that takes proofs of subgoals and
;;; constructs the proof of the original goal.

;;; tactic-success : (List Goal) x Builder -> TacticResult
(define (tactic-success subgoals builder)
  `(ok ,subgoals ,builder))

;;; tactic-failure : Any -> TacticResult
(define (tactic-failure reason)
  `(error ,reason))

;;; tactic-success? : TacticResult -> Boolean
(define (tactic-success? r)
  (and (pair? r) (eq? (car r) 'ok)))

;;; tactic-failure? : TacticResult -> Boolean
(define (tactic-failure? r)
  (and (pair? r) (eq? (car r) 'error)))

;;; tactic-subgoals : TacticResult -> (List Goal)
(define (tactic-subgoals r)
  (if (tactic-success? r) (cadr r) '()))

;;; tactic-builder : TacticResult -> Builder
(define (tactic-builder r)
  (if (tactic-success? r) (caddr r) (lambda args 'error)))

;;; tactic-error : TacticResult -> Any
(define (tactic-error r)
  (if (tactic-failure? r) (cadr r) #f))

;;; ====
;;; Core Tactics
;;; ====

;;; tactic-refl : Goal -> TacticResult
;;; Prove x = x using reflexivity.
;;; Succeeds if lhs and rhs are syntactically equal.
(define (tactic-refl goal)
  (if (not (proof-goal? goal))
      (tactic-failure '(not-a-goal))
      (let ([lhs (goal-lhs goal)]
            [rhs (goal-rhs goal)])
           (if (equal? lhs rhs)
               ;; No subgoals, proof is (refl lhs)
               (tactic-success '()
                               (lambda () `(refl ,lhs)))
               (tactic-failure `(sides-not-equal ,lhs ,rhs))))))

;;; tactic-sym : Goal -> TacticResult
;;; Transform goal y = x into subgoal x = y.
;;; The proof uses symmetry: if we prove x = y, sym gives y = x.
(define (tactic-sym goal)
  (if (not (proof-goal? goal))
      (tactic-failure '(not-a-goal))
      (let* ([lhs (goal-lhs goal)]   ; y
             [rhs (goal-rhs goal)]   ; x
             [ctx (goal-context goal)]
             [new-goal (make-proof-goal rhs lhs ctx)])  ; x = y
            (tactic-success
             (list new-goal)
             (lambda (proofs)
                     (if (null? proofs)
                         'error
                         `(sym ,(car proofs))))))))

;;; tactic-trans : Expr -> Goal -> TacticResult
;;; Split goal x = z into subgoals x = mid and mid = z.
;;; Uses transitivity to chain the proofs.
(define (tactic-trans mid)
  (lambda (goal)
          (if (not (proof-goal? goal))
              (tactic-failure '(not-a-goal))
              (let* ([lhs (goal-lhs goal)]   ; x
                     [rhs (goal-rhs goal)]   ; z
                     [ctx (goal-context goal)]
                     [goal1 (make-proof-goal lhs mid ctx)]  ; x = mid
                     [goal2 (make-proof-goal mid rhs ctx)]) ; mid = z
                    (tactic-success
                     (list goal1 goal2)
                     (lambda (proofs)
                             (if (< (length proofs) 2)
                                 'error
                                 `(trans ,(car proofs) ,(cadr proofs)))))))))

;;; tactic-rewrite : Symbol -> Goal -> TacticResult
;;; Apply a named law from the registry to rewrite the goal.
;;; Rewrites the lhs using the law and checks if it equals rhs.
(define (tactic-rewrite law-name)
  (lambda (goal)
          (if (not (proof-goal? goal))
              (tactic-failure '(not-a-goal))
              (let ([law (get-law law-name)])
                   (if (not law)
                       (tactic-failure `(unknown-law ,law-name))
                       (let* ([lhs (goal-lhs goal)]
                              [rhs (goal-rhs goal)]
                              [ctx (goal-context goal)]
                              [result (apply-rule law lhs)])
                             (if (not result)
                                 (tactic-failure `(law-did-not-apply ,law-name ,lhs))
                                 (let ([rewritten (car result)]
                                       [bindings (cdr result)])
                                      (if (equal? rewritten rhs)
                                          ;; Direct match - no subgoals
                                          (tactic-success
                                           '()
                                           (lambda () `(rewrite ,law-name ,bindings)))
                                          ;; Need to prove rewritten = rhs
                                          (let ([new-goal (make-proof-goal rewritten rhs ctx)])
                                               (tactic-success
                                                (list new-goal)
                                                (lambda (proofs)
                                                        `(trans (rewrite ,law-name ,bindings)
                                                          ,(if (null? proofs) '(refl) (car proofs)))))))))))))))

;;; ====
;;; Strategy-Based Tactics
;;; ====
;;;
;;; These tactics wrap engine.ss strategies, applying them within
;;; the proof framework.

;;; tactic-topdown : Strategy -> Goal -> TacticResult
;;; Apply a rewrite strategy top-down through the lhs expression.
(define (tactic-topdown strategy)
  (lambda (goal)
          (if (not (proof-goal? goal))
              (tactic-failure '(not-a-goal))
              (let* ([lhs (goal-lhs goal)]
                     [rhs (goal-rhs goal)]
                     [ctx (goal-context goal)]
                     [topdown-strat (topdown strategy)]
                     [result (topdown-strat lhs)])
                    (if (not result)
                        (tactic-failure '(strategy-did-not-apply-topdown))
                        (if (equal? result rhs)
                            ;; Direct match
                            (tactic-success
                             '()
                             (lambda () `(topdown-rewrite ,lhs ,result)))
                            ;; Need subgoal for result = rhs
                            (let ([new-goal (make-proof-goal result rhs ctx)])
                                 (tactic-success
                                  (list new-goal)
                                  (lambda (proofs)
                                          `(trans (topdown-rewrite ,lhs ,result)
                                            ,(if (null? proofs) '(refl) (car proofs))))))))))))

;;; tactic-bottomup : Strategy -> Goal -> TacticResult
;;; Apply a rewrite strategy bottom-up through the lhs expression.
(define (tactic-bottomup strategy)
  (lambda (goal)
          (if (not (proof-goal? goal))
              (tactic-failure '(not-a-goal))
              (let* ([lhs (goal-lhs goal)]
                     [rhs (goal-rhs goal)]
                     [ctx (goal-context goal)]
                     [bottomup-strat (bottomup strategy)]
                     [result (bottomup-strat lhs)])
                    (if (not result)
                        (tactic-failure '(strategy-did-not-apply-bottomup))
                        (if (equal? result rhs)
                            (tactic-success
                             '()
                             (lambda () `(bottomup-rewrite ,lhs ,result)))
                            (let ([new-goal (make-proof-goal result rhs ctx)])
                                 (tactic-success
                                  (list new-goal)
                                  (lambda (proofs)
                                          `(trans (bottomup-rewrite ,lhs ,result)
                                            ,(if (null? proofs) '(refl) (car proofs))))))))))))

;;; tactic-try : Tactic -> Goal -> TacticResult
;;; Apply tactic, succeed even if it fails (goal unchanged).
(define (tactic-try t)
  (lambda (goal)
          (let ([result (t goal)])
               (if (tactic-success? result)
                   result
                   ;; Tactic failed - return goal unchanged
                   (tactic-success
                    (list goal)
                    (lambda (proofs)
                            (if (null? proofs) '(refl) (car proofs))))))))

;;; *tactic-repeat-fuel* : Nat
;;; Default fuel limit for tactic-repeat to ensure termination.
(define *tactic-repeat-fuel* 100)

;;; tactic-repeat : Tactic [x Nat] -> Tactic
;;; Apply tactic repeatedly until it fails or reaches fixed point.
;;; Uses fuel to ensure termination.
(define tactic-repeat
  (case-lambda
   [(t) (tactic-repeat t *tactic-repeat-fuel*)]
   [(t fuel)
    (lambda (goal)
            (let loop ([current-goal goal]
                       [remaining fuel]
                       [proof-steps '()])
                 (if (<= remaining 0)
                     ;; Out of fuel - return current state
                     (tactic-success
                      (if (goal-trivial? current-goal) '() (list current-goal))
                      (lambda (proofs)
                              (fold-proofs proof-steps proofs)))
                     (let ([result (t current-goal)])
                          (if (tactic-failure? result)
                              ;; Tactic failed - return current accumulated result
                              (tactic-success
                               (if (goal-trivial? current-goal) '() (list current-goal))
                               (lambda (proofs)
                                       (fold-proofs proof-steps proofs)))
                              ;; Tactic succeeded
                              (let ([subgoals (tactic-subgoals result)]
                                    [builder (tactic-builder result)])
                                   (if (null? subgoals)
                                       ;; Goal fully solved
                                       (tactic-success
                                        '()
                                        (lambda ()
                                                (fold-proofs (cons builder proof-steps) '())))
                                       ;; Continue with first subgoal
                                       (let ([next-goal (car subgoals)])
                                            (if (equal? next-goal current-goal)
                                                ;; Fixed point reached
                                                (tactic-success
                                                 (if (goal-trivial? current-goal) '() (list current-goal))
                                                 (lambda (proofs)
                                                         (fold-proofs proof-steps proofs)))
                                                ;; Continue iteration
                                                (loop next-goal
                                                      (- remaining 1)
                                                      (cons builder proof-steps)))))))))))]))

;;; fold-proofs : (List Builder) x (List Proof) -> Proof
;;; Fold builders into a single proof.
;;; Builders may be thunks (no subgoals) or take a list of proofs.
(define (fold-proofs builders proofs)
  (if (null? builders)
      (if (null? proofs) '(refl) (car proofs))
      (let ([b (car builders)]
            [rest (fold-proofs (cdr builders) proofs)])
           (if (procedure? b)
               ;; Try calling as thunk first, fall back to passing rest
               (guard (exn [else (b (list rest))])
                      (b))
               rest))))

;;; ====
;;; Tactic Combinators
;;; ====

;;; tactic-then : Tactic x Tactic -> Tactic
;;; Sequential composition: apply t1, then apply t2 to all subgoals.
(define (tactic-then t1 t2)
  (lambda (goal)
          (let ([r1 (t1 goal)])
               (if (tactic-failure? r1)
                   r1
                   (let* ([subgoals (tactic-subgoals r1)]
                          [builder1 (tactic-builder r1)])
                         (if (null? subgoals)
                             ;; No subgoals, t1 solved it
                             r1
                             ;; Apply t2 to each subgoal
                             (let ([results (map t2 subgoals)])
                                  (if (ormap tactic-failure? results)
                                      (tactic-failure
                                       `(subgoal-failed ,(filter tactic-failure? results)))
                                      ;; Collect all new subgoals
                                      (let* ([all-subgoals (append-map tactic-subgoals results)]
                                             [builders (map tactic-builder results)]
                                             [combined-builder
                                              (lambda (proofs)
                                                      (build-combined-proof builder1 builders proofs))])
                                            (tactic-success all-subgoals combined-builder))))))))))

;;; build-combined-proof : Builder x (List Builder) x (List Proof) -> Proof
;;; Helper to build proof from nested builders.
(define (build-combined-proof outer-builder inner-builders proofs)
  (let* ([num-builders (length inner-builders)]
         [proofs-per (if (zero? num-builders) 0 (quotient (length proofs) num-builders))]
         [inner-proofs
          (let loop ([bs inner-builders] [ps proofs] [acc '()])
               (if (null? bs)
                   (reverse acc)
                   ;; FIX (fold-ntfv): Last builder takes all remaining proofs
                   ;; Previous code used quotient which drops remainder
                   (let* ([b (car bs)]
                          [last-builder? (null? (cdr bs))]
                          [these (if (or last-builder? (<= (length ps) proofs-per))
                                     ps
                                     (take-n ps proofs-per))]
                          [rest (if (or last-builder? (<= (length ps) proofs-per))
                                    '()
                                    (drop-n ps proofs-per))])
                        (loop (cdr bs) rest
                              (cons (if (procedure? b) (b these) '(refl)) acc)))))])
        (if (procedure? outer-builder)
            (outer-builder inner-proofs)
            '(refl))))

;;; take-n : (List a) x Nat -> (List a)
(define (take-n lst n)
  (if (or (null? lst) (<= n 0))
      '()
      (cons (car lst) (take-n (cdr lst) (- n 1)))))

;;; drop-n : (List a) x Nat -> (List a)
(define (drop-n lst n)
  (if (or (null? lst) (<= n 0))
      lst
      (drop-n (cdr lst) (- n 1))))

;;; tactic-orelse : Tactic x Tactic -> Tactic
;;; Try first tactic, if fails try second.
(define (tactic-orelse t1 t2)
  (lambda (goal)
          (let ([r1 (t1 goal)])
               (if (tactic-success? r1)
                   r1
                   (t2 goal)))))

;;; tactic-id : Tactic
;;; Identity tactic: always succeeds with goal unchanged.
(define tactic-id
  (lambda (goal)
          (tactic-success
           (list goal)
           (lambda (proofs)
                   (if (null? proofs) '(refl) (car proofs))))))

;;; tactic-fail : Tactic
;;; Failure tactic: always fails.
(define tactic-fail
  (lambda (goal)
          (tactic-failure 'explicit-fail)))

;;; ====
;;; Automation Tactics
;;; ====

;;; ====
;;; Auto-Law Configuration
;;; ====
;;;
;;; The auto-law system determines which laws tactic-auto will try.
;;; It consists of:
;;;   - A base set of manually curated laws (*auto-laws-base*)
;;;   - A set of categories to auto-include (*auto-law-categories*)
;;;   - Ad-hoc additions via register-auto-law!
;;;
;;; The (auto-laws) function dynamically computes the full set.

;;; *auto-laws-base* : (List Symbol)
;;; Core simplification laws always included in auto-laws.
(define *auto-laws-base*
  '(monoid-left-id
    monoid-right-id
    arith-add-left-id
    arith-add-right-id
    arith-mul-left-id
    arith-mul-right-id
    arith-mul-zero-left
    arith-mul-zero-right
    functor-id
    applicative-id
    monad-left-id
    monad-right-id))

;;; *auto-law-categories* : (List Symbol)
;;; Categories whose laws are auto-included.
;;; Fusion rules eliminate intermediate structures - always beneficial.
(define *auto-law-categories*
  '(fusion))

;;; *auto-laws-extra* : (List Symbol)
;;; Additional laws added via register-auto-law!
(define *auto-laws-extra* '())

;;; register-auto-law! : Symbol -> void
;;; Add a law to the auto-law set by name.
(define (register-auto-law! law-name)
  (unless (memq law-name *auto-laws-extra*)
          (set! *auto-laws-extra* (cons law-name *auto-laws-extra*))))

;;; register-auto-category! : Symbol -> void
;;; Add a category to auto-include in auto-laws.
(define (register-auto-category! category)
  (unless (memq category *auto-law-categories*)
          (set! *auto-law-categories* (cons category *auto-law-categories*))))

;;; unregister-auto-category! : Symbol -> void
;;; Remove a category from auto-inclusion.
(define (unregister-auto-category! category)
  (set! *auto-law-categories*
        (filter (lambda (c) (not (eq? c category))) *auto-law-categories*)))

;;; auto-laws : -> (List Symbol)
;;; Compute the complete set of auto-laws dynamically.
;;; Combines: base laws + laws from auto-categories + extra laws.
;;; Removes duplicates while preserving order (base laws tried first).
(define (auto-laws)
  (let* ([base *auto-laws-base*]
         [category-laws (append-map (lambda (cat)
                                            (map rule-name (laws-by-category cat)))
                                    *auto-law-categories*)]
         [extra *auto-laws-extra*]
         [all (append base category-laws extra)])
        ;; Remove duplicates, keeping first occurrence
        (let loop ([lst all] [seen '()] [acc '()])
             (cond
              [(null? lst) (reverse acc)]
              [(memq (car lst) seen) (loop (cdr lst) seen acc)]
              [else (loop (cdr lst)
                          (cons (car lst) seen)
                          (cons (car lst) acc))]))))

;;; auto-laws-by-category : Symbol -> (List Symbol)
;;; Get auto-law names from a specific category.
(define (auto-laws-by-category category)
  (map rule-name (laws-by-category category)))

;;; *auto-laws* : (List Symbol)
;;; Backward compatibility: static snapshot of auto-laws.
;;; Note: Use (auto-laws) for dynamic computation.
(define *auto-laws*
  '(monoid-left-id
    monoid-right-id
    arith-add-left-id
    arith-add-right-id
    arith-mul-left-id
    arith-mul-right-id
    arith-mul-zero-left
    arith-mul-zero-right
    functor-id
    applicative-id
    monad-left-id
    monad-right-id))

;;; tactic-auto : Goal -> TacticResult
;;; Try common tactics automatically:
;;; 1. Reflexivity
;;; 2. All laws from (auto-laws) - includes base laws, fusion rules, etc.
(define (tactic-auto goal)
  (if (not (proof-goal? goal))
      (tactic-failure '(not-a-goal))
      ;; Try reflexivity first
      (let ([refl-result (tactic-refl goal)])
           (if (tactic-success? refl-result)
               refl-result
               ;; Try each auto-law (dynamically computed)
               (let try-laws ([laws (auto-laws)])
                    (if (null? laws)
                        (tactic-failure '(auto-exhausted))
                        (let ([result ((tactic-rewrite (car laws)) goal)])
                             (if (tactic-success? result)
                                 result
                                 (try-laws (cdr laws))))))))))

;;; ====
;;; Bounded Search
;;; ====

;;; *search-max-depth* : Nat
;;; Default maximum depth for search.
(define *search-max-depth* 10)

;;; expr->key : Expr -> String
;;; Convert expression to string key for cycle detection.
(define (expr->key expr)
  (format "~s" expr))

;;; tactic-search : Nat -> Goal -> TacticResult
;;; Bounded depth-first search with cycle detection.
;;; Tries to find a proof path from lhs to rhs.
(define tactic-search
  (case-lambda
   [(goal) ((tactic-search *search-max-depth*) goal)]
   [(depth)
    (lambda (goal)
            (if (not (proof-goal? goal))
                (tactic-failure '(not-a-goal))
                (let ([lhs (goal-lhs goal)]
                      [rhs (goal-rhs goal)]
                      [ctx (goal-context goal)])
                     (search-dfs lhs rhs ctx depth (make-eq-hashtable)))))]))

;;; search-dfs : Expr x Expr x Context x Nat x Hashtable -> TacticResult
;;; Depth-first search from current expression to target.
(define (search-dfs current target ctx depth visited)
  (cond
   ;; Reached target
   [(equal? current target)
    (tactic-success '() (lambda () `(refl ,current)))]
   
   ;; Out of depth
   [(<= depth 0)
    (tactic-failure `(search-depth-exhausted ,current ,target))]
   
   ;; Cycle detection
   [(hashtable-ref visited (expr->key current) #f)
    (tactic-failure `(cycle-detected ,current))]
   
   [else
    ;; Mark as visited
    (hashtable-set! visited (expr->key current) #t)
    
    ;; Try each law
    (let try-laws ([laws (all-laws)])
         (if (null? laws)
             (tactic-failure `(search-failed ,current ,target))
             (let* ([law (car laws)]
                    [result (apply-rule law current)])
                   (if (not result)
                       (try-laws (cdr laws))
                       (let ([rewritten (car result)]
                             [bindings (cdr result)])
                            ;; Recursively search from rewritten expression
                            (let ([sub-result (search-dfs rewritten target ctx (- depth 1) visited)])
                                 (if (tactic-success? sub-result)
                                     ;; Found a path
                                     (tactic-success
                                      '()
                                      (lambda ()
                                              `(trans (rewrite ,(rule-name law) ,bindings)
                                                ,((tactic-builder sub-result)))))
                                     ;; Try next law
                                     (try-laws (cdr laws)))))))))]))

;;; ====
;;; Strategy Lifting
;;; ====
;;;
;;; These utilities lift engine.ss strategies into the tactic world.

;;; strategy->tactic : Strategy -> Tactic
;;; Lift a rewrite strategy to a tactic.
;;; The strategy is applied to the lhs of the goal.
(define (strategy->tactic strategy)
  (lambda (goal)
          (if (not (proof-goal? goal))
              (tactic-failure '(not-a-goal))
              (let* ([lhs (goal-lhs goal)]
                     [rhs (goal-rhs goal)]
                     [ctx (goal-context goal)]
                     [result (strategy lhs)])
                    (if (not result)
                        (tactic-failure '(strategy-did-not-apply))
                        (if (equal? result rhs)
                            (tactic-success
                             '()
                             (lambda () `(strategy-proof ,lhs ,result)))
                            (let ([new-goal (make-proof-goal result rhs ctx)])
                                 (tactic-success
                                  (list new-goal)
                                  (lambda (proofs)
                                          `(trans (strategy-proof ,lhs ,result)
                                            ,(if (null? proofs) '(refl) (car proofs))))))))))))

;;; law->tactic : Symbol -> Tactic
;;; Create a tactic from a named law.
(define (law->tactic name)
  (let ([law (get-law name)])
       (if law
           (strategy->tactic (rule->strategy law))
           (lambda (goal) (tactic-failure `(unknown-law ,name))))))

;;; simplify-tactic : Strategy -> Tactic
;;; Create a tactic that simplifies using a strategy exhaustively.
(define (simplify-tactic strategy)
  (strategy->tactic (innermost strategy)))

;;; ====
;;; Convenience Tactics
;;; ====

;;; tactic-simplify : Goal -> TacticResult
;;; Apply all simplification laws exhaustively.
(define tactic-simplify
  (simplify-tactic (rules->strategy (all-laws))))

;;; tactic-arith : Goal -> TacticResult
;;; Apply arithmetic simplification.
(define tactic-arith
  (simplify-tactic arith-simplify))

;;; tactic-monoid : Goal -> TacticResult
;;; Apply monoid simplification.
(define tactic-monoid
  (simplify-tactic monoid-simplify))

;;; tactic-functor : Goal -> TacticResult
;;; Apply functor simplification.
(define tactic-functor
  (simplify-tactic functor-simplify))

;;; tactic-monad : Goal -> TacticResult
;;; Apply monad simplification.
(define tactic-monad
  (simplify-tactic monad-simplify))

;; ormap, filter provided by prelude

;;; ====
;;; Proof Execution
;;; ====

;;; prove-with-tactics : Expr x Expr x (List Tactic) -> (Result Proof Error)
;;; Execute a sequence of tactics to prove lhs = rhs.
(define (prove-with-tactics lhs rhs tactics)
  (let ([initial-goal (make-proof-goal lhs rhs '())])
       (let loop ([current-goal initial-goal]
                  [remaining tactics]
                  [builders '()])
            (cond
             ;; Goal is trivial
             [(goal-trivial? current-goal)
              `(ok ,(fold-proofs (reverse builders) '()))]
             
             ;; No more tactics
             [(null? remaining)
              (if (goal-trivial? current-goal)
                  `(ok ,(fold-proofs (reverse builders) '()))
                  `(error (remaining-goal ,current-goal)))]
             
             ;; Apply next tactic
             [else
              (let* ([tactic (car remaining)]
                     [result (tactic current-goal)])
                    (if (tactic-failure? result)
                        `(error ,(tactic-error result))
                        (let ([subgoals (tactic-subgoals result)]
                              [builder (tactic-builder result)])
                             (if (null? subgoals)
                                 ;; Tactic solved the goal
                                 `(ok ,(fold-proofs (cons builder (reverse builders)) '()))
                                 ;; Continue with first subgoal
                                 (loop (car subgoals)
                                       (cdr remaining)
                                       (cons builder builders))))))]))))
