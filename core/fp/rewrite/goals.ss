;;; core/fp/rewrite/goals.ss --- Goal Types and Management for Proof Sketcher
;;;
;;; Provides goal types for the proof sketcher component of the rewrite system.
;;; Goals represent propositions to be proved, typically algebraic equalities
;;; or structural properties like associativity and identity.
;;;
;;; Goal Types:
;;;   - eq-goal:     Prove lhs = rhs in some context
;;;   - assoc-goal:  Prove associativity of an operation
;;;   - left-id-goal:  Prove left identity for an operation
;;;   - right-id-goal: Prove right identity for an operation
;;;   - inv-goal:    Prove inverse property for an operation
;;;
;;; Goals can be decomposed into simpler eq-goals for verification.
;;; The proof sketcher uses these goals to guide law application.
;;;
;;; This is Core code: pure, total, assumes perfect input.
;;;
;;; Dependencies:
;;;   - core/base/prelude.ss
;;;   - core/fp/rewrite/rule.ss (for metavariable patterns)

(load "core/base/prelude.ss")
(load "core/fp/rewrite/rule.ss")

;;; ============================================================
;;; Goal Structure
;;; ============================================================

;;; Goals are represented as alists with a 'type field distinguishing them.
;;; Common fields:
;;;   type     - Symbol identifying goal kind
;;;   context  - List of (name . type) bindings (hypotheses)
;;;
;;; Type-specific fields are documented with each constructor.

;;; ============================================================
;;; Goal Predicates
;;; ============================================================

;;; goal? : Any -> Boolean
;;; Check if x is a well-formed goal structure.
(define (goal? x)
  (and (pair? x)
       (assq 'type x)
       (assq 'context x)))

;;; goal-type : Goal -> Symbol
;;; Extract the goal type.
(define (goal-type g)
  (cdr (assq 'type g)))

;;; goal-context : Goal -> (List (name . type))
;;; Extract the goal's hypothesis context.
(define (goal-context g)
  (cdr (assq 'context g)))

;;; ============================================================
;;; Equality Goals
;;; ============================================================

;;; eq-goal: Prove that lhs = rhs
;;; Structure:
;;;   type    -> 'eq
;;;   context -> hypotheses
;;;   lhs     -> left-hand side expression
;;;   rhs     -> right-hand side expression
;;;   carrier -> optional type annotation for the equality

;;; make-eq-goal : Context x Expr x Expr [x Type] -> Goal
;;; Create a goal to prove lhs = rhs.
(define (make-eq-goal ctx lhs rhs . opts)
  (let ([carrier (if (null? opts) #f (car opts))])
       `((type . eq)
         (context . ,ctx)
         (lhs . ,lhs)
         (rhs . ,rhs)
         (carrier . ,carrier))))

;;; eq-goal? : Any -> Boolean
(define (eq-goal? g)
  (and (goal? g) (eq? (goal-type g) 'eq)))

;;; eq-goal-lhs : Goal -> Expr
(define (eq-goal-lhs g)
  (cdr (assq 'lhs g)))

;;; eq-goal-rhs : Goal -> Expr
(define (eq-goal-rhs g)
  (cdr (assq 'rhs g)))

;;; eq-goal-carrier : Goal -> Type | #f
(define (eq-goal-carrier g)
  (let ([c (assq 'carrier g)])
       (if c (cdr c) #f)))

;;; ============================================================
;;; Associativity Goals
;;; ============================================================

;;; assoc-goal: Prove that op is associative
;;; Property: (op (op x y) z) = (op x (op y z))
;;; Structure:
;;;   type    -> 'assoc
;;;   context -> hypotheses
;;;   op      -> the binary operation

;;; make-assoc-goal : Context x Symbol -> Goal
;;; Create a goal to prove op is associative.
(define (make-assoc-goal ctx op)
  `((type . assoc)
    (context . ,ctx)
    (op . ,op)))

;;; assoc-goal? : Any -> Boolean
(define (assoc-goal? g)
  (and (goal? g) (eq? (goal-type g) 'assoc)))

;;; assoc-goal-op : Goal -> Symbol
(define (assoc-goal-op g)
  (cdr (assq 'op g)))

;;; ============================================================
;;; Identity Goals
;;; ============================================================

;;; left-id-goal: Prove that e is a left identity for op
;;; Property: (op e x) = x
;;; Structure:
;;;   type     -> 'left-id
;;;   context  -> hypotheses
;;;   op       -> the binary operation
;;;   identity -> the identity element

;;; make-left-id-goal : Context x Symbol x Expr -> Goal
;;; Create a goal to prove e is a left identity for op.
(define (make-left-id-goal ctx op identity)
  `((type . left-id)
    (context . ,ctx)
    (op . ,op)
    (identity . ,identity)))

;;; left-id-goal? : Any -> Boolean
(define (left-id-goal? g)
  (and (goal? g) (eq? (goal-type g) 'left-id)))

;;; left-id-goal-op : Goal -> Symbol
(define (left-id-goal-op g)
  (cdr (assq 'op g)))

;;; left-id-goal-identity : Goal -> Expr
(define (left-id-goal-identity g)
  (cdr (assq 'identity g)))

;;; right-id-goal: Prove that e is a right identity for op
;;; Property: (op x e) = x
;;; Structure:
;;;   type     -> 'right-id
;;;   context  -> hypotheses
;;;   op       -> the binary operation
;;;   identity -> the identity element

;;; make-right-id-goal : Context x Symbol x Expr -> Goal
;;; Create a goal to prove e is a right identity for op.
(define (make-right-id-goal ctx op identity)
  `((type . right-id)
    (context . ,ctx)
    (op . ,op)
    (identity . ,identity)))

;;; right-id-goal? : Any -> Boolean
(define (right-id-goal? g)
  (and (goal? g) (eq? (goal-type g) 'right-id)))

;;; right-id-goal-op : Goal -> Symbol
(define (right-id-goal-op g)
  (cdr (assq 'op g)))

;;; right-id-goal-identity : Goal -> Expr
(define (right-id-goal-identity g)
  (cdr (assq 'identity g)))

;;; make-id-goal : Context x Symbol x Expr x Symbol -> Goal
;;; Create an identity goal with specified side ('left or 'right).
(define (make-id-goal ctx op identity side)
  (case side
        [(left)  (make-left-id-goal ctx op identity)]
        [(right) (make-right-id-goal ctx op identity)]
        [else    (make-left-id-goal ctx op identity)]))

;;; id-goal? : Any -> Boolean
;;; Check if goal is any kind of identity goal.
(define (id-goal? g)
  (or (left-id-goal? g) (right-id-goal? g)))

;;; ============================================================
;;; Inverse Goals
;;; ============================================================

;;; inv-goal: Prove that inv is an inverse operation for op with identity e
;;; Properties:
;;;   Left inverse:  (op (inv x) x) = e
;;;   Right inverse: (op x (inv x)) = e
;;; Structure:
;;;   type     -> 'inv
;;;   context  -> hypotheses
;;;   op       -> the binary operation
;;;   inv      -> the inverse operation
;;;   identity -> the identity element
;;;   side     -> 'left, 'right, or 'both

;;; make-inv-goal : Context x Symbol x Symbol x Expr x Symbol -> Goal
;;; Create a goal to prove inv is an inverse for op.
(define (make-inv-goal ctx op inv identity side)
  `((type . inv)
    (context . ,ctx)
    (op . ,op)
    (inv . ,inv)
    (identity . ,identity)
    (side . ,side)))

;;; inv-goal? : Any -> Boolean
(define (inv-goal? g)
  (and (goal? g) (eq? (goal-type g) 'inv)))

;;; inv-goal-op : Goal -> Symbol
(define (inv-goal-op g)
  (cdr (assq 'op g)))

;;; inv-goal-inv : Goal -> Symbol
(define (inv-goal-inv g)
  (cdr (assq 'inv g)))

;;; inv-goal-identity : Goal -> Expr
(define (inv-goal-identity g)
  (cdr (assq 'identity g)))

;;; inv-goal-side : Goal -> Symbol
(define (inv-goal-side g)
  (cdr (assq 'side g)))

;;; ============================================================
;;; Goal Decomposition
;;; ============================================================

;;; decompose-goal : Goal -> (List eq-goal)
;;; Expand a high-level goal into concrete equality goals with metavariables.
;;;
;;; Associativity decomposes to:
;;;   (op (op (?x) (?y)) (?z)) = (op (?x) (op (?y) (?z)))
;;;
;;; Left identity decomposes to:
;;;   (op e (?x)) = (?x)
;;;
;;; Right identity decomposes to:
;;;   (op (?x) e) = (?x)
;;;
;;; Inverse decomposes to one or two equalities depending on side.

(define (decompose-goal goal)
  (let ([ctx (goal-context goal)]
        [type (goal-type goal)])
       (case type
             [(eq)
              ;; Already a concrete equality goal
              (list goal)]
             
             [(assoc)
              ;; (op (op x y) z) = (op x (op y z))
              (let ([op (assoc-goal-op goal)])
                   (list (make-eq-goal ctx
                                       `(,op (,op (?x) (?y)) (?z))
                                       `(,op (?x) (,op (?y) (?z))))))]
             
             [(left-id)
              ;; (op e x) = x
              (let ([op (left-id-goal-op goal)]
                    [e (left-id-goal-identity goal)])
                   (list (make-eq-goal ctx
                                       `(,op ,e (?x))
                                       '(?x))))]
             
             [(right-id)
              ;; (op x e) = x
              (let ([op (right-id-goal-op goal)]
                    [e (right-id-goal-identity goal)])
                   (list (make-eq-goal ctx
                                       `(,op (?x) ,e)
                                       '(?x))))]
             
             [(inv)
              ;; Inverse properties
              (let ([op (inv-goal-op goal)]
                    [inv-op (inv-goal-inv goal)]
                    [e (inv-goal-identity goal)]
                    [side (inv-goal-side goal)])
                   (case side
                         [(left)
                          ;; (op (inv x) x) = e
                          (list (make-eq-goal ctx
                                              `(,op (,inv-op (?x)) (?x))
                                              e))]
                         [(right)
                          ;; (op x (inv x)) = e
                          (list (make-eq-goal ctx
                                              `(,op (?x) (,inv-op (?x)))
                                              e))]
                         [(both)
                          ;; Both left and right inverse
                          (list (make-eq-goal ctx
                                              `(,op (,inv-op (?x)) (?x))
                                              e)
                                (make-eq-goal ctx
                                              `(,op (?x) (,inv-op (?x)))
                                              e))]
                         [else '()]))]
             
             [else '()])))

;;; ============================================================
;;; Goal Utilities
;;; ============================================================

;;; goal->string : Goal -> String
;;; Format a goal for display.
(define (goal->string g)
  (case (goal-type g)
        [(eq)
         (format "~a = ~a" (eq-goal-lhs g) (eq-goal-rhs g))]
        [(assoc)
         (format "~a is associative" (assoc-goal-op g))]
        [(left-id)
         (format "~a is left identity for ~a"
                 (left-id-goal-identity g)
                 (left-id-goal-op g))]
        [(right-id)
         (format "~a is right identity for ~a"
                 (right-id-goal-identity g)
                 (right-id-goal-op g))]
        [(inv)
         (format "~a is ~a inverse for ~a with identity ~a"
                 (inv-goal-inv g)
                 (inv-goal-side g)
                 (inv-goal-op g)
                 (inv-goal-identity g))]
        [else
         (format "Unknown goal: ~a" g)]))

;;; goal-vars : Goal -> (List Symbol)
;;; Collect metavariable names from decomposed goal.
(define (goal-vars g)
  (let ([eqs (decompose-goal g)])
       (apply append
              (map (lambda (eq)
                           (append (pattern-vars (eq-goal-lhs eq))
                                   (pattern-vars (eq-goal-rhs eq))))
                   eqs))))

;;; goal-vars-unique : Goal -> (List Symbol)
;;; Collect unique metavariable names.
(define (goal-vars-unique g)
  (let ([vars (goal-vars g)])
       (let loop ([vs vars] [seen '()] [acc '()])
            (cond
             [(null? vs) (reverse acc)]
             [(memq (car vs) seen) (loop (cdr vs) seen acc)]
             [else (loop (cdr vs) (cons (car vs) seen) (cons (car vs) acc))]))))

;;; ============================================================
;;; Goal Comparison
;;; ============================================================

;;; goals-equal? : Goal x Goal -> Boolean
;;; Check if two goals are structurally equal.
(define (goals-equal? g1 g2)
  (and (eq? (goal-type g1) (goal-type g2))
       (case (goal-type g1)
             [(eq)
              (and (equal? (eq-goal-lhs g1) (eq-goal-lhs g2))
                   (equal? (eq-goal-rhs g1) (eq-goal-rhs g2)))]
             [(assoc)
              (eq? (assoc-goal-op g1) (assoc-goal-op g2))]
             [(left-id)
              (and (eq? (left-id-goal-op g1) (left-id-goal-op g2))
                   (equal? (left-id-goal-identity g1) (left-id-goal-identity g2)))]
             [(right-id)
              (and (eq? (right-id-goal-op g1) (right-id-goal-op g2))
                   (equal? (right-id-goal-identity g1) (right-id-goal-identity g2)))]
             [(inv)
              (and (eq? (inv-goal-op g1) (inv-goal-op g2))
                   (eq? (inv-goal-inv g1) (inv-goal-inv g2))
                   (equal? (inv-goal-identity g1) (inv-goal-identity g2))
                   (eq? (inv-goal-side g1) (inv-goal-side g2)))]
             [else #f])))

;;; ============================================================
;;; Goal Transformation
;;; ============================================================

;;; flip-eq-goal : eq-goal -> eq-goal
;;; Swap lhs and rhs of an equality goal (symmetry).
(define (flip-eq-goal g)
  (if (eq-goal? g)
      (make-eq-goal (goal-context g)
                    (eq-goal-rhs g)
                    (eq-goal-lhs g)
                    (eq-goal-carrier g))
      g))

;;; add-hypothesis : Goal x (name . type) -> Goal
;;; Add a hypothesis to a goal's context.
(define (add-hypothesis g hyp)
  (let ([type (goal-type g)]
        [old-ctx (goal-context g)])
       ;; Rebuild goal with extended context
       (map (lambda (pair)
                    (if (eq? (car pair) 'context)
                        (cons 'context (cons hyp old-ctx))
                        pair))
            g)))

;;; ============================================================
;;; Standard Algebraic Goals
;;; ============================================================

;;; These convenience constructors create goals for common algebraic structures.

;;; monoid-goals : Context x Symbol x Expr -> (List Goal)
;;; Create all three monoid law goals for operation op with identity e.
(define (monoid-goals ctx op identity)
  (list (make-assoc-goal ctx op)
        (make-left-id-goal ctx op identity)
        (make-right-id-goal ctx op identity)))

;;; group-goals : Context x Symbol x Symbol x Expr -> (List Goal)
;;; Create all group law goals: monoid laws plus inverse laws.
(define (group-goals ctx op inv identity)
  (append (monoid-goals ctx op identity)
          (list (make-inv-goal ctx op inv identity 'both))))

;;; semigroup-goals : Context x Symbol -> (List Goal)
;;; Create semigroup goal (just associativity).
(define (semigroup-goals ctx op)
  (list (make-assoc-goal ctx op)))
