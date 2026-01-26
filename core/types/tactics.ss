(load "core/base/prelude.ss")
(load "core/types/types.ss")
(load "core/types/dep-types.ss")
(load "core/types/dep-infer.ss")

(doc 'module 'tactics)
(doc 'description "Proof tactics and automation for constructing proof terms. Tactics transform proof goals into simpler subgoals until the proof is complete. Includes reflexivity, symmetry, transitivity, congruence, assumption, and combinators")
(doc 'layer 'core)

(doc 'section 'proof-goals)

(doc make-goal 'type (-> Context Type Goal))
(doc make-goal 'description "Create a proof goal with context and proposition")
(doc make-goal 'export #t)
(define (make-goal ctx prop)
  `(goal ,ctx ,prop))

(define (goal? g)
  (and (pair? g) (eq? (car g) 'goal)))

(define (goal-context g)
  (if (goal? g) (cadr g) '()))

(define (goal-proposition g)
  (if (goal? g) (caddr g) #f))

;;; ====
;;; Tactic Results
;;; ====

;;; A tactic returns either:
;;;   (ok subgoals proof-builder)  - success with new subgoals and proof builder
;;;   (error reason)               - failure with reason
;;;
;;; proof-builder is a function that takes proofs of subgoals and builds
;;; the proof of the original goal.

(define (tactic-success subgoals builder)
  `(ok ,subgoals ,builder))

(define (tactic-failure reason)
  `(error ,reason))

(define (tactic-success? r)
  (and (pair? r) (eq? (car r) 'ok)))

(define (tactic-failure? r)
  (and (pair? r) (eq? (car r) 'error)))

(define (tactic-subgoals r)
  (if (tactic-success? r) (cadr r) '()))

(define (tactic-builder r)
  (if (tactic-success? r) (caddr r) (lambda args 'error)))

(define (tactic-error r)
  (if (tactic-failure? r) (cadr r) #f))

(doc 'section 'core-tactics)

(doc tactic-reflexivity 'type (-> Goal TacticResult))
(doc tactic-reflexivity 'description "Prove x = x using refl")
(doc tactic-reflexivity 'export #t)
(define (tactic-reflexivity goal)
  (let ([prop (goal-proposition goal)])
       (if (not (equality-type? prop))
           (tactic-failure `(not-an-equality-goal ,prop))
           (let ([A (equality-carrier prop)]
                 [lhs (equality-lhs prop)]
                 [rhs (equality-rhs prop)])
                (if (not (equal? lhs rhs))
                    ;; Try normalizing both sides
                    (let ([lhs-norm (nbe-type-normalize-closed lhs)]
                          [rhs-norm (nbe-type-normalize-closed rhs)])
                         (if (equal? lhs-norm rhs-norm)
                             ;; Sides are definitionally equal after normalization
                             (tactic-success '() (lambda () `(refl ,A ,lhs)))
                             (tactic-failure `(sides-not-equal ,lhs ,rhs))))
                    ;; Direct syntactic equality
                    (tactic-success '() (lambda () `(refl ,A ,lhs))))))))

;;; symmetry : Goal → TacticResult
;;; Transform a goal y = x into x = y.
;;; Produces a subgoal for x = y and builds sym proof.
(define (tactic-symmetry goal)
  (let ([prop (goal-proposition goal)]
        [ctx (goal-context goal)])
       (if (not (equality-type? prop))
           (tactic-failure `(not-an-equality-goal ,prop))
           (let* ([A (equality-carrier prop)]
                  [y (equality-lhs prop)]
                  [x (equality-rhs prop)]
                  ;; New goal: prove x = y
                  [new-prop `(= ,A ,x ,y)]
                  [new-goal (make-goal ctx new-prop)])
                 (tactic-success
                  (list new-goal)
                  (lambda (proofs)
                          (if (null? proofs)
                              'error
                              `(sym ,A ,(car proofs)))))))))

;;; transitivity : Term → Tactic
;;; Split x = z into x = y and y = z for a given intermediate term y.
(define (tactic-transitivity middle)
  (lambda (goal)
          (let ([prop (goal-proposition goal)]
                [ctx (goal-context goal)])
               (if (not (equality-type? prop))
                   (tactic-failure `(not-an-equality-goal ,prop))
                   (let* ([A (equality-carrier prop)]
                          [x (equality-lhs prop)]
                          [z (equality-rhs prop)]
                          ;; Subgoal 1: prove x = middle
                          [prop1 `(= ,A ,x ,middle)]
                          [goal1 (make-goal ctx prop1)]
                          ;; Subgoal 2: prove middle = z
                          [prop2 `(= ,A ,middle ,z)]
                          [goal2 (make-goal ctx prop2)])
                         (tactic-success
                          (list goal1 goal2)
                          (lambda (proofs)
                                  (if (< (length proofs) 2)
                                      'error
                                      `(trans ,A ,(car proofs) ,(cadr proofs))))))))))

;;; congruence : Expr → Tactic
;;; Given goal f(x) = f(y), produce subgoal x = y.
;;; f must be a function expression.
(define (tactic-congruence f)
  (lambda (goal)
          (let ([prop (goal-proposition goal)]
                [ctx (goal-context goal)])
               (if (not (equality-type? prop))
                   (tactic-failure `(not-an-equality-goal ,prop))
                   (let* ([B (equality-carrier prop)]
                          [fx (equality-lhs prop)]
                          [fy (equality-rhs prop)])
                         ;; Check that fx and fy are applications of f
                         (if (not (and (pair? fx) (pair? fy)
                                       (equal? (car fx) f)
                                       (equal? (car fy) f)))
                             (tactic-failure `(not-congruence-goal ,f ,prop))
                             (let* ([x (cadr fx)]
                                    [y (cadr fy)]
                                    ;; Infer the domain type (simplified: just use 'A)
                                    ;; In a full system, we'd use type inference
                                    [A 'A]
                                    [new-prop `(= ,A ,x ,y)]
                                    [new-goal (make-goal ctx new-prop)])
                                   (tactic-success
                                    (list new-goal)
                                    (lambda (proofs)
                                            (if (null? proofs)
                                                'error
                                                `(cong ,f ,(car proofs))))))))))))

;;; assumption : Goal → TacticResult
;;; Look for a hypothesis in context that proves the goal.
(define (tactic-assumption goal)
  (let ([prop (goal-proposition goal)]
        [ctx (goal-context goal)])
       (let loop ([hyps ctx])
            (if (null? hyps)
                (tactic-failure `(no-matching-assumption ,prop))
                (let* ([hyp (car hyps)]
                       [name (car hyp)]
                       [type (cdr hyp)])
                      (if (equal? type prop)
                          ;; Found a matching hypothesis
                          (tactic-success '() (lambda () name))
                          (loop (cdr hyps))))))))

;;; intro : Symbol → Tactic
;;; For a goal Π(x:A).B, introduce x into context and prove B.
(define (tactic-intro name)
  (lambda (goal)
          (let ([prop (goal-proposition goal)]
                [ctx (goal-context goal)])
               (cond
                [(pi-type? prop)
                 (let* ([var (pi-var prop)]
                        [A (pi-domain prop)]
                        [B (pi-codomain prop)]
                        ;; Add (name : A) to context
                        [new-ctx (cons (cons name A) ctx)]
                        ;; Substitute name for var in B
                        [new-prop (dep-subst-type B var name)]
                        [new-goal (make-goal new-ctx new-prop)])
                       (tactic-success
                        (list new-goal)
                        (lambda (proofs)
                                (if (null? proofs)
                                    'error
                                    `(fn (,name) ,(car proofs))))))]
                ;; For arrow types, similar treatment
                [(function-type? prop)
                 (let* ([A (car (function-param-types prop))]
                        [B (function-return-type prop)]
                        [new-ctx (cons (cons name A) ctx)]
                        [new-goal (make-goal new-ctx B)])
                       (tactic-success
                        (list new-goal)
                        (lambda (proofs)
                                (if (null? proofs)
                                    'error
                                    `(fn (,name) ,(car proofs))))))]
                [else
                 (tactic-failure `(not-a-pi-or-arrow-goal ,prop))]))))

;;; trivial : Goal → TacticResult
;;; Try to solve obvious goals (like #t, reflexive equalities).
(define (tactic-trivial goal)
  (let ([prop (goal-proposition goal)])
       (cond
        ;; Literal true
        [(eq? prop #t)
         (tactic-success '() (lambda () #t))]
        ;; Unit type
        [(eq? prop 'Unit)
         (tactic-success '() (lambda () 'unit))]
        ;; Try reflexivity
        [(equality-type? prop)
         (tactic-reflexivity goal)]
        [else
         (tactic-failure `(not-trivial ,prop))])))

;;; ====
;;; Tactic Combinators
;;; ====

;;; then : Tactic × Tactic → Tactic
;;; Apply first tactic, then apply second to all subgoals.
(define (tactic-then t1 t2)
  (lambda (goal)
          (let ([r1 (t1 goal)])
               (if (tactic-failure? r1)
                   r1
                   (let* ([subgoals (tactic-subgoals r1)]
                          [builder1 (tactic-builder r1)]
                          ;; Apply t2 to each subgoal
                          [results (map t2 subgoals)])
                         ;; Check if all succeeded
                         (if (ormap tactic-failure? results)
                             (tactic-failure `(subgoal-failed ,(filter tactic-failure? results)))
                             ;; Collect all new subgoals
                             (let* ([all-subgoals (apply append (map tactic-subgoals results))]
                                    [builders (map tactic-builder results)]
                                    ;; Build combined proof
                                    [combined-builder
                                     (lambda (proofs)
                                             ;; Distribute proofs to each builder
                                             (let distribute ([bs builders] [ps proofs] [acc '()])
                                                  (if (null? bs)
                                                      (builder1 (reverse acc))
                                                      (let* ([b (car bs)]
                                                             ;; Count subgoals this builder needs
                                                             ;; (simplified: assume 1 for now)
                                                             [n 1]
                                                             [these-proofs (take n ps)]
                                                             [rest-proofs (drop n ps)]
                                                             [proof (apply b these-proofs)])
                                                            (distribute (cdr bs) rest-proofs
                                                                        (cons proof acc))))))])
                                   (tactic-success all-subgoals combined-builder))))))))

;;; orelse : Tactic × Tactic → Tactic
;;; Try first tactic, if it fails try second.
(define (tactic-orelse t1 t2)
  (lambda (goal)
          (let ([r1 (t1 goal)])
               (if (tactic-success? r1)
                   r1
                   (t2 goal)))))

;;; try : Tactic → Tactic
;;; Try tactic, succeed even if it fails (no subgoals, identity builder).
(define (tactic-try t)
  (lambda (goal)
          (let ([r (t goal)])
               (if (tactic-success? r)
                   r
                   ;; Return goal unchanged
                   (tactic-success (list goal) (lambda (proofs) (car proofs)))))))

;;; repeat : Tactic → Tactic
;;; Apply tactic repeatedly until it fails.
(define (tactic-repeat t)
  (lambda (goal)
          (let ([result (t goal)])
               (if (tactic-failure? result)
                   ;; Tactic failed, return goal unchanged
                   (tactic-success (list goal) (lambda (ps) (car ps)))
                   ;; Tactic succeeded, recurse on subgoals
                   (let* ([subgoals (tactic-subgoals result)]
                          [builder (tactic-builder result)])
                         (if (null? subgoals)
                             ;; No subgoals means proof complete
                             result
                             ;; Apply repeat to each subgoal
                             (let ([repeated ((tactic-repeat t) (car subgoals))])
                                  (if (tactic-failure? repeated)
                                      result  ; Return current result if repeat fails
                                      repeated))))))))

;;; id : Tactic
;;; Identity tactic: succeed with goal unchanged.
(define tactic-id
  (lambda (goal)
          (tactic-success (list goal) (lambda (proofs) (car proofs)))))

;;; fail : Tactic
;;; Always fail.
(define tactic-fail
  (lambda (goal)
          (tactic-failure 'explicit-fail)))

;;; ====
;;; Proof State
;;; ====

;;; A proof state tracks remaining goals and a partial proof.
;;; Structure: (proof-state goals proof)

(define (make-proof-state goals)
  `(proof-state ,goals))

(define (proof-state? ps)
  (and (pair? ps) (eq? (car ps) 'proof-state)))

(define (proof-state-goals ps)
  (if (proof-state? ps) (cadr ps) '()))

(define (proof-state-complete? ps)
  (null? (proof-state-goals ps)))

;;; apply-tactic : Tactic × ProofState → ProofState | Error
;;; Apply a tactic to the first goal in the proof state.
(define (apply-tactic tactic ps)
  (let ([goals (proof-state-goals ps)])
       (if (null? goals)
           `(error no-goals)
           (let* ([goal (car goals)]
                  [rest (cdr goals)]
                  [result (tactic goal)])
                 (if (tactic-failure? result)
                     `(error ,(tactic-error result))
                     (let ([new-goals (append (tactic-subgoals result) rest)])
                          (make-proof-state new-goals)))))))

;;; ====
;;; Auto Tactic
;;; ====

;;; auto : Tactic
;;; Try a combination of basic tactics automatically.
(define tactic-auto
  (tactic-repeat
   (tactic-orelse
    tactic-trivial
    (tactic-orelse
     tactic-assumption
     (tactic-orelse
      tactic-reflexivity
      tactic-id)))))

;;; ====
;;; Helper Functions
;;; ====

;; take, drop, filter, ormap provided by prelude

;;; ====
;;; Proof Execution
;;; ====

;;; prove : Type × Context × TacticScript → (Result ProofTerm Error)
;;; Execute a tactic script to prove a proposition.
;;; TacticScript is a list of tactics to apply in sequence.
(define (prove prop ctx tactics)
  (let* ([initial-goal (make-goal ctx prop)]
         [initial-state (make-proof-state (list initial-goal))])
        (let loop ([state initial-state]
                   [remaining-tactics tactics]
                   [builders '()])
             (cond
              ;; All goals solved
              [(proof-state-complete? state)
               ;; Build the proof term
               (if (null? builders)
                   '(ok unit)  ; No proof needed
                   `(ok ,(apply-builders (reverse builders))))]
              ;; No more tactics
              [(null? remaining-tactics)
               `(error (remaining-goals ,(proof-state-goals state)))]
              ;; Apply next tactic
              [else
               (let* ([tactic (car remaining-tactics)]
                      [result (apply-tactic tactic state)])
                     (if (and (pair? result) (eq? (car result) 'error))
                         result  ; Tactic failed
                         (loop result (cdr remaining-tactics) builders)))]))))

;;; apply-builders : (List Builder) → ProofTerm
;;; Placeholder for building proof terms from tactic builders.
(define (apply-builders builders)
  (if (null? builders)
      'unit
      ((car builders) '())))

;;; ====
;;; Quick Proof Helpers
;;; ====

;;; qed-reflexivity : Type × Term → ProofTerm
;;; Quickly prove x = x.
(define (qed-reflexivity A x)
  `(refl ,A ,x))

;;; qed-symmetry : ProofTerm → ProofTerm
;;; Given p : x = y, produce sym(p) : y = x.
(define (qed-symmetry p)
  `(sym _ ,p))

;;; qed-transitivity : ProofTerm × ProofTerm → ProofTerm
;;; Given p : x = y and q : y = z, produce trans(p,q) : x = z.
(define (qed-transitivity p q)
  `(trans _ ,p ,q))

;;; qed-congruence : Term × ProofTerm → ProofTerm
;;; Given f and p : x = y, produce cong(f,p) : f(x) = f(y).
(define (qed-congruence f p)
  `(cong ,f ,p))
