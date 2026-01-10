((name "benchmarks")
 (purpose "Performance benchmarking suite for algorithm and system validation")
 (description
  "Comprehensive performance benchmarks measuring execution time, memory
   usage, and scalability characteristics. Used for performance regression
   testing, optimization validation, and algorithm comparison. Benchmarks
   generate statistical reports with mean, median, standard deviation, and
   percentiles.")
 (modules
  ((graph-algorithms.ss "Benchmarks for graph algorithms across multiple structures and sizes:
                         - Chain, star, tree, cyclic, dense, sparse graphs
                         - Scalability testing (10, 50, 100, 500, 1000 nodes)
                         - Traversal, shortest path, topological sort, cycle detection
                         - Memory allocation tracking and performance comparison")))
 (usage
  "Load and run benchmarks in REPL:
     (load \"shell/benchmarks/graph-algorithms.ss\")
     (run-all-benchmarks)          ; Complete suite
     (run-traversal-benchmarks)    ; Specific category
     (run-scalability-tests)       ; Test algorithm scaling

   Or execute directly:
     scheme --script shell/benchmarks/graph-algorithms.ss")
 (dependencies (core/data/graph-algorithms shell/tools/benchmark shell/fs))
 (output
  "Benchmarks produce detailed reports including:
   - Execution time statistics (mean, median, stddev)
   - Memory usage profiles
   - Scalability curves
   - Comparison tables across implementations")
 (adding-benchmarks
  "To add new benchmarks:
   1. Create benchmark file in shell/benchmarks/
   2. Use shell/tools/benchmark.ss for timing and statistics
   3. Generate multiple test cases (small, medium, large)
   4. Include memory tracking with allocation counters
   5. Output results in consistent format for comparison
   6. Document expected performance characteristics"))
