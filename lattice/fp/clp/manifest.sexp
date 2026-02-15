;;; lattice/fp/clp/manifest.sexp — CLP(FD) Skill Manifest

(skill clp
  (version "0.1.0")
  (tier 1)
  (path "lattice/fp/clp")
  (purity partial)
  (stability experimental)
  (fuel-bound "O(d^n) worst case where d=max domain size, n=vars; practical O(n*d*c) with propagation")

  (deps (fp))

  (description
   "cKanren-style constraint logic programming with finite domains.
    Extends miniKanren with arithmetic constraints, global constraints like
    all-different, and intelligent search strategies. Supports classic problems
    like N-Queens, Sudoku, cryptarithmetic, and scheduling.")

  (keywords (clp constraint-logic-programming finite-domain cKanren
             constraint-propagation arc-consistency n-queens sudoku
             cryptarithmetic scheduling combinatorial))

  (aliases (clp fd ckanren constraint-logic))

  (exports
   ;; Domain operations
   make-domain domain-singleton domain-from-list
   domain-empty? domain-singleton? domain-size
   domain-min domain-max domain-contains?
   domain-intersect domain-union domain-subtract-value
   domain-restrict-min domain-restrict-max
   domain->list domain->string

   ;; Constraint store
   make-cstore cstore? cstore-subst cstore-domains
   cstore-get-domain cstore-set-domain cstore-narrow-domain
   cstore-bind-var cstore-unify cstore-walk cstore-get-value
   cstore-add-constraint cstore-constraints-for-var

   ;; Arithmetic constraints
   in-range in-domain
   =fd <fd <=fd >fd >=fd =/=fd
   +fd -fd *fd abs-fd

   ;; Global constraints
   all-different element sum-fd count-fd

   ;; Propagation
   propagate post-constraint
   post-=fd post-<fd post-<=fd post->fd post->=fd post-=/=fd
   post-+fd post--fd post-*fd
   post-all-different post-element post-sum-fd post-count-fd

   ;; Labeling
   label label-with label-first label-all label-count
   input-order first-fail most-constrained max-regret
   min-value max-value mid-value
   make-labeling-strategy

   ;; Goals
   clp-succeed clp-fail clp-goal clp-conj clp-disj clp-conj* clp-disj*
   goal-in-range goal-in-domain
   goal-=fd goal-<fd goal-<=fd goal->fd goal->=fd goal-=/=fd
   goal-+fd goal--fd goal-*fd
   goal-all-different goal-sum-fd
   goal-label goal-label-with

   ;; Running
   run-clp run-clp* clp-solve clp-all clp-count

   ;; Examples & Solvers
   n-queens send-more-money
   solve-n-queens solve-send-more-money
   clp-help)

  (modules
   (domain "domain.ss" "Finite domain representation with intervals")
   (store "store.ss" "Constraint store with domains and propagators")
   (fd-constraints "fd-constraints.ss" "Arithmetic FD constraints")
   (global-constraints "global-constraints.ss" "Global constraints (all-different, etc.)")
   (propagate "propagate.ss" "Arc/bounds consistency propagation engine")
   (label "label.ss" "Variable/value selection strategies for search")
   (clp "clp.ss" "Unified entry point and goal API")))
