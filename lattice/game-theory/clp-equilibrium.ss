(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
;;; @module game-theory/clp-equilibrium
;;; @requires prelude clp normal-form
(require 'prelude)
(require 'clp)
(require 'normal-form)

(doc 'module 'game-theory/clp-equilibrium)
(doc 'description "Bridge between CLP(FD) constraint solving and game theory equilibrium finding.
Encodes Nash equilibrium conditions as CLP constraints, enabling constraint-based
pruning and composition with arbitrary user constraints.

Instead of brute-force enumeration, the CLP approach prunes strategy spaces via
arc consistency: when one player's strategy is fixed, the opponent's domain narrows
to best responses. Composable with arbitrary additional constraints.")
(doc 'layer 'lattice)
(doc 'purity 'total)

;;; ============================================================
;;; Best-Response Propagators
;;; ============================================================

(doc 'section 'propagators)

(doc 'type '(-> Game LVar LVar (-> CStore (Maybe CStore))))
(doc 'description "Propagator: p1 must be a best response to p2.
When p2 is ground, narrows p1 to best responses.
When p1 is ground, prunes p2 values where p1 isn't a best response.
When both ground, verifies the constraint.")
(define (br-p1-propagator game p1 p2)
  (lambda (cs)
    (let ([p1-val (cstore-get-value cs p1)]
          [p2-val (cstore-get-value cs p2)])
      (cond
       ;; Both ground — verify
       [(and p1-val p2-val)
        (if (pair? (memv p1-val (best-response-p1 game p2-val))) cs #f)]
       ;; p2 ground — narrow p1 to best responses
       [p2-val
        (cstore-narrow-domain cs p1
          (domain-from-list (best-response-p1 game p2-val)))]
       ;; p1 ground — prune p2 values where p1 isn't a best response
       [p1-val
        (let* ([p2-dom (cstore-get-domain cs p2)]
               [p2-vals (if p2-dom (domain->list p2-dom)
                            (iota (game-num-strategies game 1)))])
          (let ([valid (filter
                        (lambda (j) (pair? (memv p1-val (best-response-p1 game j))))
                        p2-vals)])
            (if (null? valid) #f
                (cstore-narrow-domain cs p2 (domain-from-list valid)))))]
       ;; Neither ground — wait for labeling
       [else cs]))))

(doc 'type '(-> Game LVar LVar (-> CStore (Maybe CStore))))
(doc 'description "Propagator: p2 must be a best response to p1.
Mirror of br-p1-propagator for Player 2.")
(define (br-p2-propagator game p1 p2)
  (lambda (cs)
    (let ([p1-val (cstore-get-value cs p1)]
          [p2-val (cstore-get-value cs p2)])
      (cond
       [(and p1-val p2-val)
        (if (pair? (memv p2-val (best-response-p2 game p1-val))) cs #f)]
       [p1-val
        (cstore-narrow-domain cs p2
          (domain-from-list (best-response-p2 game p1-val)))]
       [p2-val
        (let* ([p1-dom (cstore-get-domain cs p1)]
               [p1-vals (if p1-dom (domain->list p1-dom)
                            (iota (game-num-strategies game 0)))])
          (let ([valid (filter
                        (lambda (i) (pair? (memv p2-val (best-response-p2 game i))))
                        p1-vals)])
            (if (null? valid) #f
                (cstore-narrow-domain cs p1 (domain-from-list valid)))))]
       [else cs]))))

;;; ============================================================
;;; Constraint Goals
;;; ============================================================

(doc 'section 'goals)

(doc 'type '(-> Game LVar LVar Goal))
(doc 'description "Goal: p1 is a best response to p2.
Registers a persistent constraint that re-propagates during labeling.")
(define (clp-best-response-p1 game p1 p2)
  (doc 'export #t)
  (lambda (cs)
    (let ([cs1 (post-constraint cs 'br-p1 (list p1 p2)
                (br-p1-propagator game p1 p2))])
      (if cs1 (clp-succeed cs1) (clp-fail cs)))))

(doc 'type '(-> Game LVar LVar Goal))
(doc 'description "Goal: p2 is a best response to p1.")
(define (clp-best-response-p2 game p1 p2)
  (doc 'export #t)
  (lambda (cs)
    (let ([cs1 (post-constraint cs 'br-p2 (list p1 p2)
                (br-p2-propagator game p1 p2))])
      (if cs1 (clp-succeed cs1) (clp-fail cs)))))

(doc 'type '(-> Game LVar LVar Goal))
(doc 'description "Goal: mutual best response (Nash equilibrium condition).
Both p1 is a best response to p2 AND p2 is a best response to p1.")
(define (clp-mutual-best-response game p1 p2)
  (doc 'export #t)
  (clp-conj (clp-best-response-p1 game p1 p2)
            (clp-best-response-p2 game p1 p2)))

;;; ============================================================
;;; Dominance Pruning
;;; ============================================================

(doc 'section 'dominance)

(doc 'type '(-> Game LVar LVar Goal))
(doc 'description "Goal: prune strictly dominated strategies from both players' domains.
Narrows the search space before Nash constraint propagation.")
(define (clp-dominance-prune game p1 p2)
  (doc 'export #t)
  (let* ([dominated (find-dominated-strategies game)]
         [dom-p1 (car dominated)]
         [dom-p2 (cdr dominated)]
         [n1 (game-num-strategies game 0)]
         [n2 (game-num-strategies game 1)]
         [valid-p1 (filter (lambda (i) (not (pair? (memv i dom-p1)))) (iota n1))]
         [valid-p2 (filter (lambda (j) (not (pair? (memv j dom-p2)))) (iota n2))])
    (clp-conj
     (if (< (length valid-p1) n1)
         (goal-in-domain p1 valid-p1)
         clp-succeed)
     (if (< (length valid-p2) n2)
         (goal-in-domain p2 valid-p2)
         clp-succeed))))

;;; ============================================================
;;; Problem Setup
;;; ============================================================

(doc 'section 'setup)

(doc 'type '(-> Game (Values Goal (List LVar))))
(doc 'description "Set up a CLP problem for finding pure Nash equilibria.
Returns (values goal vars) where goal encodes domain constraints,
Nash conditions, and labeling. Solve with run-clp.")
(define (game->clp-nash game)
  (doc 'export #t)
  (let ([p1 (make-lvar 'p1)]
        [p2 (make-lvar 'p2)]
        [n1 (game-num-strategies game 0)]
        [n2 (game-num-strategies game 1)])
    (values
     (clp-conj*
      (list (goal-in-range p1 0 (- n1 1))
            (goal-in-range p2 0 (- n2 1))
            (clp-mutual-best-response game p1 p2)
            (goal-label (list p1 p2))))
     (list p1 p2))))

(doc 'type '(-> Game (-> LVar LVar Goal) (Values Goal (List LVar))))
(doc 'description "Set up a CLP problem for Nash equilibria with additional constraints.
extra-goal-fn receives (p1 p2) and returns a Goal to conjoin with Nash conditions.")
(define (game->clp-constrained-nash game extra-goal-fn)
  (doc 'export #t)
  (let ([p1 (make-lvar 'p1)]
        [p2 (make-lvar 'p2)]
        [n1 (game-num-strategies game 0)]
        [n2 (game-num-strategies game 1)])
    (values
     (clp-conj*
      (list (goal-in-range p1 0 (- n1 1))
            (goal-in-range p2 0 (- n2 1))
            (extra-goal-fn p1 p2)
            (clp-mutual-best-response game p1 p2)
            (goal-label (list p1 p2))))
     (list p1 p2))))

;;; ============================================================
;;; Convenience Solvers
;;; ============================================================

(doc 'section 'solvers)

(doc 'type '(-> Game (List (Pair Nat Nat))))
(doc 'description "Find all pure Nash equilibria of a game via CLP.
Returns list of (i . j) strategy index pairs.")
(define (clp-find-pure-nash game)
  (doc 'export #t)
  (call-with-values
   (lambda () (game->clp-nash game))
   (lambda (goal vars)
     (let ([solutions (run-clp 100 (lambda () goal) vars)])
       (map (lambda (sol) (cons (car sol) (cadr sol))) solutions)))))

(doc 'type '(-> Game (-> LVar LVar Goal) (List (Pair Nat Nat))))
(doc 'description "Find pure Nash equilibria satisfying additional constraints.
extra-goal-fn receives (p1 p2) CLP variables and returns a Goal.")
(define (clp-constrained-nash game extra-goal-fn)
  (doc 'export #t)
  (call-with-values
   (lambda () (game->clp-constrained-nash game extra-goal-fn))
   (lambda (goal vars)
     (let ([solutions (run-clp 100 (lambda () goal) vars)])
       (map (lambda (sol) (cons (car sol) (cadr sol))) solutions)))))

(doc 'type '(-> Game (Pair Nat Nat) (Pair Symbol Symbol)))
(doc 'description "Convert a Nash equilibrium index pair to strategy names.
Takes a game and (i . j) pair, returns (name1 . name2).")
(define (nash-solution->names game profile)
  (doc 'export #t)
  (let ([s1 (game%-strategies1 game)]
        [s2 (game%-strategies2 game)])
    (cons (vector-ref s1 (car profile))
          (vector-ref s2 (cdr profile)))))

(doc 'type '(-> Game (List (Pair Nat Nat)) (List (Pair Symbol Symbol))))
(doc 'description "Convert a list of Nash equilibria to named strategy pairs.")
(define (nash-solutions->names game profiles)
  (doc 'export #t)
  (map (lambda (p) (nash-solution->names game p)) profiles))
