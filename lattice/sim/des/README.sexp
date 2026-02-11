(directory des
  (description "Discrete Event Simulation SDK — pure, composable, stream-based")
  (layer lattice)
  (purity total)

  (modules
    (des/event    "Event types and time-ordered event queue (min-heap)")
    (des/world    "World state container with optics, entity CRUD, metrics, RNG")
    (des/engine   "DES loop: pop event, advance clock, dispatch handler → stream")
    (des/schedule "Deterministic and stochastic event scheduling combinators")
    (des/replicate "Run N independent replications, collect summary statistics"))

  (design-notes
    "The DES engine is a codata pattern: simulation = stream-unfold over (world × event-queue)."
    "Each world state is immutable — every intermediate step is a snapshot."
    "Stochastic events thread RNG state purely via (State RNG a) from lattice/random."
    "Handler dispatch is a simple alist lookup: O(n) in handler count, O(1) typical."
    "Event queue is a leftist heap: O(log n) schedule/pop, O(1) peek, O(log n) merge.")

  (usage
    "Build a simulation: define handlers (world event -> world), create config,"
    "make initial world with entities and seed events, call (des-run config world)."
    "See test-des.ss for M/M/1 queuing system example."))
