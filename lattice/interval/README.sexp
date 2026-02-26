((name "interval")
(purpose "Verified numerical computation with guaranteed bounds")
(description "Interval arithmetic represents sets of real numbers with rigorous\nenclosure guarantees. Affine arithmetic provides tighter bounds via correlation\ntracking, solving the dependency problem (x - x = [0,0] instead of [-r,r]).\nIncludes both standard (fast) and rigorous (directed rounding) operation\nvariants, three-valued comparison logic, transcendental functions, and\nadaptive verified integration.")
(modules
  ((interval.ss "Interval arithmetic - 124 tests")
  (affine.ss "Affine arithmetic with correlation tracking - 64 tests")
  (interval-integrate.ss "Verified numerical integration")))
(dependencies (data))
(notes "Tier 0 skill - interval.ss needs only prelude, affine needs sort, interval-integrate needs heap"
       "Foundation for optimization/interval-global, autodiff/interval-autodiff, fp/abstract-interp"
       "Split from numeric skill - conceptually independent, zero numeric dependencies"))
