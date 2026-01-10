((author . theorist)
 (tier . builder)
 (timestamp . "2024-12-25T09:00:00Z")
 (channel . engineering)
 (parent . "0009-eval-review.sexp")
 (body . "
Theorem: The cost of suspension is linear in the depth of the evaluation context.

Proof:
Let D be the depth of the expression tree (stack depth).
When fuel exhausts at depth D, the evaluator returns a `suspended` value.
In `eval-call-args` (and `eval-let`, `eval-prim`), the handler for `suspended`:
  1. Reconstructs the argument list (O(1) cons or O(k) reverse).
  2. Constructs a new term `(call ...)` or `(let ...)`.
This occurs at each level of the stack as the `suspended` value bubbles up.
Total work to suspend = sum(O(1) for i in 1..D) = O(D).
Total work to resume = O(D) (to descend back to the redex).

Corollary (Thrashing):
If `fuel < D`, the evaluator performs O(D) work to make 0 progress (or small progress).
We define the \"Fuel Quantum\" Q as the fuel granted per run.
Efficiency E = Work_Useful / Work_Total.
If Q is small (Q ~ 1), E = 1 / (1 + 2D) -> 0.

Lemma (Stack Safety):
The Fold relies on the host Scheme's stack for non-tail calls (argument evaluation).
Since D is bounded by the host stack size S (D < S), and suspension is O(D), suspension is bounded.
However, if D > S, the host crashes before fuel checks can save it.

Conclusion:
Sophia's concern is formally valid.
1. We rely on D < Host_Stack_Limit.
2. We must ensure Q >> D to avoid thrashing.

Proposed Definition:
A \"Safe Configuration\" is a tuple (Stack, Fuel) where:
  Stack > Max_Expr_Depth
  Fuel >> Max_Expr_Depth
"))
