((name "parallel")
(purpose "Evaluation strategies and parallel algorithm skeletons")
(description "Pure strategy layer for parallel evaluation in The Fold.\nStrategies describe task decomposition as S-expression plan data.\nPlans are interpreted by boundary/parallel/strategy-eval.ss against\nthe work-stealing thread pool.\n\nThis separation keeps strategies pure, composable, and content-addressable.")
(modules
  ((strategies.ss "Strategy types, core strategies, and combinators.\n     Types: Thunk (make-thunk, thunk-id, thunk-work)\n            Plan (plan:seq, plan:par, plan:chunk, plan:tree, plan:race, plan:pipe)\n            Strategy (make-strategy, strategy-name, strategy-transform)\n     Core strategies: strategy/seq, strategy/par, strategy/par-chunk,\n                      strategy/par-buffer, strategy/speculate, strategy/par-list\n     Combinators: strategy-compose, strategy-when-beneficial, using\n     Fuel: fuel-budget-full, fuel-budget-proportional")
  (patterns.ss "Parallel algorithm skeletons:\n     - par-divide-and-conquer: generic D&C with custom divisible?/divide/combine\n     - par-reduce: balanced binary tree reduction\n     - par-scan: Blelloch-style parallel prefix scan (up-sweep + down-sweep)\n     - par-pipeline: multi-stage streaming pipeline\n     - par-sort: parallel merge sort skeleton")))
(tests
  ((test-strategies.ss "46 tests for plan construction, strategy application, combinators, fuel budgeting")
  (test-patterns.ss "24 tests for algorithm skeleton plan shapes")))
(dependencies (fp)))
