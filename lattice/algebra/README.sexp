((name "algebra")
 (purpose "Algebraic structures and abstract algebra")
 (description
  "Pure implementation of fundamental algebraic structures.
   Provides groups, rings, and their laws for use in numeric
   and symbolic computation. Enables generic programming over
   algebraic types.")
 (modules
  ((group.ss "Group theory: groups, monoids, semigroups, and their operations")
   (ring.ss "Ring theory: rings, fields, ideals, and ring homomorphisms")))
 (dependencies (base linalg))
 (load-order "group.ss -> ring.ss"))
