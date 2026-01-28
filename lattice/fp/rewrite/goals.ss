(load "core/base/prelude.ss")
(load "lattice/fp/rewrite/rule.ss")

(doc 'module 'goals)
(doc 'description "Goal Types and Management for Proof Sketcher

Provides goal types for the proof sketcher component of the rewrite system.
Goals represent propositions to be proved, typically algebraic equalities
or structural properties like associativity and identity.

Goal Types:
  - eq-goal:     Prove lhs = rhs in some context
  - assoc-goal:  Prove associativity of an operation
  - left-id-goal:  Prove left identity for an operation
  - right-id-goal: Prove right identity for an operation
  - inv-goal:    Prove inverse property for an operation

Goals can be decomposed into simpler eq-goals for verification.
The proof sketcher uses these goals to guide law application.")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'section 'goal-structure)
(doc 'description "Goals are represented as alists with a 'type field distinguishing them.
Common fields:
  type     - Symbol identifying goal kind
  context  - List of (name . type) bindings (hypotheses)

Type-specific fields are documented with each constructor.")

(doc 'section 'goal-predicates)

(define (goal? x)
  (doc 'type '(-> Any Boolean))
  (doc 'description "Check if x is a well-formed goal structure.")
  (and (pair? x)
       (assq 'type x)
       (assq 'context x)))

(define (goal-type g)
  (doc 'type '(-> Goal Symbol))
  (doc 'description "Extract the goal type.")
  (cdr (assq 'type g)))

(define (goal-context g)
  (doc 'type '(-> Goal (List (Pair Symbol Type))))
  (doc 'description "Extract the goal's hypothesis context.")
  (cdr (assq 'context g)))

(doc 'section 'equality-goals)
(doc 'description "eq-goal: Prove that lhs = rhs
Structure:
  type    -> 'eq
  context -> hypotheses
  lhs     -> left-hand side expression
  rhs     -> right-hand side expression
  carrier -> optional type annotation for the equality")

(define (make-eq-goal ctx lhs rhs . opts)
  (doc 'type '(-> Context Expr Expr (Optional Type) Goal))
  (doc 'description "Create a goal to prove lhs = rhs.")
  (let ([carrier (if (null? opts) #f (car opts))])
       `((type . eq)
         (context . ,ctx)
         (lhs . ,lhs)
         (rhs . ,rhs)
         (carrier . ,carrier))))

(define (eq-goal? g)
  (doc 'type '(-> Any Boolean))
  (and (goal? g) (eq? (goal-type g) 'eq)))

(define (eq-goal-lhs g)
  (doc 'type '(-> Goal Expr))
  (cdr (assq 'lhs g)))

(define (eq-goal-rhs g)
  (doc 'type '(-> Goal Expr))
  (cdr (assq 'rhs g)))

(define (eq-goal-carrier g)
  (doc 'type '(-> Goal (Union Type Boolean)))
  (let ([c (assq 'carrier g)])
       (if c (cdr c) #f)))

(doc 'section 'associativity-goals)
(doc 'description "assoc-goal: Prove that op is associative
Property: (op (op x y) z) = (op x (op y z))
Structure:
  type    -> 'assoc
  context -> hypotheses
  op      -> the binary operation")

(define (make-assoc-goal ctx op)
  (doc 'type '(-> Context Symbol Goal))
  (doc 'description "Create a goal to prove op is associative.")
  `((type . assoc)
    (context . ,ctx)
    (op . ,op)))

(define (assoc-goal? g)
  (doc 'type '(-> Any Boolean))
  (and (goal? g) (eq? (goal-type g) 'assoc)))

(define (assoc-goal-op g)
  (doc 'type '(-> Goal Symbol))
  (cdr (assq 'op g)))

(doc 'section 'identity-goals)
(doc 'description "left-id-goal: Prove that e is a left identity for op
Property: (op e x) = x
Structure:
  type     -> 'left-id
  context  -> hypotheses
  op       -> the binary operation
  identity -> the identity element")

(define (make-left-id-goal ctx op identity)
  (doc 'type '(-> Context Symbol Expr Goal))
  (doc 'description "Create a goal to prove e is a left identity for op.")
  `((type . left-id)
    (context . ,ctx)
    (op . ,op)
    (identity . ,identity)))

(define (left-id-goal? g)
  (doc 'type '(-> Any Boolean))
  (and (goal? g) (eq? (goal-type g) 'left-id)))

(define (left-id-goal-op g)
  (doc 'type '(-> Goal Symbol))
  (cdr (assq 'op g)))

(define (left-id-goal-identity g)
  (doc 'type '(-> Goal Expr))
  (cdr (assq 'identity g)))

(doc 'description "right-id-goal: Prove that e is a right identity for op
Property: (op x e) = x
Structure:
  type     -> 'right-id
  context  -> hypotheses
  op       -> the binary operation
  identity -> the identity element")

(define (make-right-id-goal ctx op identity)
  (doc 'type '(-> Context Symbol Expr Goal))
  (doc 'description "Create a goal to prove e is a right identity for op.")
  `((type . right-id)
    (context . ,ctx)
    (op . ,op)
    (identity . ,identity)))

(define (right-id-goal? g)
  (doc 'type '(-> Any Boolean))
  (and (goal? g) (eq? (goal-type g) 'right-id)))

(define (right-id-goal-op g)
  (doc 'type '(-> Goal Symbol))
  (cdr (assq 'op g)))

(define (right-id-goal-identity g)
  (doc 'type '(-> Goal Expr))
  (cdr (assq 'identity g)))

(define (make-id-goal ctx op identity side)
  (doc 'type '(-> Context Symbol Expr Symbol Goal))
  (doc 'description "Create an identity goal with specified side ('left or 'right).")
  (case side
        [(left)  (make-left-id-goal ctx op identity)]
        [(right) (make-right-id-goal ctx op identity)]
        [else    (make-left-id-goal ctx op identity)]))

(define (id-goal? g)
  (doc 'type '(-> Any Boolean))
  (doc 'description "Check if goal is any kind of identity goal.")
  (or (left-id-goal? g) (right-id-goal? g)))

(doc 'section 'inverse-goals)
(doc 'description "inv-goal: Prove that inv is an inverse operation for op with identity e
Properties:
  Left inverse:  (op (inv x) x) = e
  Right inverse: (op x (inv x)) = e
Structure:
  type     -> 'inv
  context  -> hypotheses
  op       -> the binary operation
  inv      -> the inverse operation
  identity -> the identity element
  side     -> 'left, 'right, or 'both")

(define (make-inv-goal ctx op inv identity side)
  (doc 'type '(-> Context Symbol Symbol Expr Symbol Goal))
  (doc 'description "Create a goal to prove inv is an inverse for op.")
  `((type . inv)
    (context . ,ctx)
    (op . ,op)
    (inv . ,inv)
    (identity . ,identity)
    (side . ,side)))

(define (inv-goal? g)
  (doc 'type '(-> Any Boolean))
  (and (goal? g) (eq? (goal-type g) 'inv)))

(define (inv-goal-op g)
  (doc 'type '(-> Goal Symbol))
  (cdr (assq 'op g)))

(define (inv-goal-inv g)
  (doc 'type '(-> Goal Symbol))
  (cdr (assq 'inv g)))

(define (inv-goal-identity g)
  (doc 'type '(-> Goal Expr))
  (cdr (assq 'identity g)))

(define (inv-goal-side g)
  (doc 'type '(-> Goal Symbol))
  (cdr (assq 'side g)))

(doc 'section 'goal-decomposition)

(define (decompose-goal goal)
  (doc 'type '(-> Goal (List eq-goal)))
  (doc 'description "Expand a high-level goal into concrete equality goals with metavariables.

Associativity decomposes to:
  (op (op (?x) (?y)) (?z)) = (op (?x) (op (?y) (?z)))

Left identity decomposes to:
  (op e (?x)) = (?x)

Right identity decomposes to:
  (op (?x) e) = (?x)

Inverse decomposes to one or two equalities depending on side.")
  (let ([ctx (goal-context goal)]
        [type (goal-type goal)])
       (case type
             [(eq)
              (list goal)]

             [(assoc)
              (let ([op (assoc-goal-op goal)])
                   (list (make-eq-goal ctx
                                       `(,op (,op (?x) (?y)) (?z))
                                       `(,op (?x) (,op (?y) (?z))))))]

             [(left-id)
              (let ([op (left-id-goal-op goal)]
                    [e (left-id-goal-identity goal)])
                   (list (make-eq-goal ctx
                                       `(,op ,e (?x))
                                       '(?x))))]

             [(right-id)
              (let ([op (right-id-goal-op goal)]
                    [e (right-id-goal-identity goal)])
                   (list (make-eq-goal ctx
                                       `(,op (?x) ,e)
                                       '(?x))))]

             [(inv)
              (let ([op (inv-goal-op goal)]
                    [inv-op (inv-goal-inv goal)]
                    [e (inv-goal-identity goal)]
                    [side (inv-goal-side goal)])
                   (case side
                         [(left)
                          (list (make-eq-goal ctx
                                              `(,op (,inv-op (?x)) (?x))
                                              e))]
                         [(right)
                          (list (make-eq-goal ctx
                                              `(,op (?x) (,inv-op (?x)))
                                              e))]
                         [(both)
                          (list (make-eq-goal ctx
                                              `(,op (,inv-op (?x)) (?x))
                                              e)
                                (make-eq-goal ctx
                                              `(,op (?x) (,inv-op (?x)))
                                              e))]
                         [else '()]))]

             [else '()])))

(doc 'section 'goal-utilities)

(define (goal->string g)
  (doc 'type '(-> Goal String))
  (doc 'description "Format a goal for display.")
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

(define (goal-vars g)
  (doc 'type '(-> Goal (List Symbol)))
  (doc 'description "Collect metavariable names from decomposed goal.")
  (let ([eqs (decompose-goal g)])
       (append-map (lambda (eq)
                           (append (pattern-vars (eq-goal-lhs eq))
                                   (pattern-vars (eq-goal-rhs eq))))
                   eqs)))

(define (goal-vars-unique g)
  (doc 'type '(-> Goal (List Symbol)))
  (doc 'description "Collect unique metavariable names.")
  (let ([vars (goal-vars g)])
       (let loop ([vs vars] [seen '()] [acc '()])
            (cond
             [(null? vs) (reverse acc)]
             [(memq (car vs) seen) (loop (cdr vs) seen acc)]
             [else (loop (cdr vs) (cons (car vs) seen) (cons (car vs) acc))]))))

(doc 'section 'goal-comparison)

(define (goals-equal? g1 g2)
  (doc 'type '(-> Goal Goal Boolean))
  (doc 'description "Check if two goals are structurally equal.")
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

(doc 'section 'goal-transformation)

(define (flip-eq-goal g)
  (doc 'type '(-> eq-goal eq-goal))
  (doc 'description "Swap lhs and rhs of an equality goal (symmetry).")
  (if (eq-goal? g)
      (make-eq-goal (goal-context g)
                    (eq-goal-rhs g)
                    (eq-goal-lhs g)
                    (eq-goal-carrier g))
      g))

(define (add-hypothesis g hyp)
  (doc 'type '(-> Goal (Pair Symbol Type) Goal))
  (doc 'description "Add a hypothesis to a goal's context.")
  (let ([type (goal-type g)]
        [old-ctx (goal-context g)])
       (map (lambda (pair)
                    (if (eq? (car pair) 'context)
                        (cons 'context (cons hyp old-ctx))
                        pair))
            g)))

(doc 'section 'standard-algebraic-goals)
(doc 'description "Convenience constructors for common algebraic structures.")

(define (monoid-goals ctx op identity)
  (doc 'type '(-> Context Symbol Expr (List Goal)))
  (doc 'description "Create all three monoid law goals for operation op with identity e.")
  (list (make-assoc-goal ctx op)
        (make-left-id-goal ctx op identity)
        (make-right-id-goal ctx op identity)))

(define (group-goals ctx op inv identity)
  (doc 'type '(-> Context Symbol Symbol Expr (List Goal)))
  (doc 'description "Create all group law goals: monoid laws plus inverse laws.")
  (append (monoid-goals ctx op identity)
          (list (make-inv-goal ctx op inv identity 'both))))

(define (semigroup-goals ctx op)
  (doc 'type '(-> Context Symbol (List Goal)))
  (doc 'description "Create semigroup goal (just associativity).")
  (list (make-assoc-goal ctx op)))
