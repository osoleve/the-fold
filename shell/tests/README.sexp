((name "tests")
 (purpose "Comprehensive test suite for shell layer")
 (description
  "Complete test coverage for shell subsystems including IO, validation,
   graphics, tools, REPL, and utilities. Tests verify defensive code,
   error handling, capability enforcement, and integration with core.
   Uses core/test-framework.ss for unified test API.")
 (structure
  ((block-system
    "test-block-index.ss           - Block indexing and querying
     test-block-navigator.ss       - Block navigation UI
     test-block-query.ss           - Basic block queries
     test-block-query-advanced.ss  - Advanced query features")
   (persistence-storage
    "test-store-api.ss             - Store API functionality
     test-duckie-persist.ss        - DUCKIE persistence
     test-universe-serialize.ss    - Universe serialization")
   (filesystem-validation
    "test-fs.ss                    - Filesystem operations
     test-validate.ss              - Input validation
     test-text.ss                  - Text sanitization (glitchling quarantine)")
   (repl-commands
    "test-repl-integration.ss      - REPL integration tests
     test-commands.ss              - Command system basics
     test-commands-demo.ss         - Command examples
     test-commands-advanced.ss     - Advanced command features")
   (graphics-visual
    "test-color.ss                 - Color system
     test-graphics-primitives.ss   - Graphics primitives (lines, circles, fills)
     test-layout.ss                - Layout system (canvas, grid)
     test-layout-color.ss          - Layout color schemes
     test-layers.ss                - Layer-based rendering with z-ordering
     test-transforms.ss            - 2D transform matrices
     test-turtle.ss                - Turtle graphics interpreter
     test-turtle-svg.ss            - SVG export for turtle graphics
     test-animation.ss             - Animation timeline and keyframes
     test-particles.ss             - Particle systems
     test-svg-renderer.ss          - SVG output generation
     test-easing.ss                - Animation easing functions")
   (developer-tools
    "test-archextract.ss           - Architecture extraction
     test-benchmark.ss             - Benchmarking harness
     test-call-graph.ss            - Call graph analysis
     test-complexity.ss            - Code complexity metrics
     test-concept-map.ss           - Concept mapping
     test-dead-code.ss             - Unused binding detection
     test-dep-slice.ss             - Dependency slicing
     test-fuel-analysis.ss         - Fuel consumption tracking
     test-introspect.ss            - Introspection tools
     test-jump.ss                  - Code navigation (jump-to-definition)
     test-memory.ss                - Memory profiling
     test-meta.ss                  - Metadata system
     test-module-deps.ss           - Module dependency graphs
     test-navigator.ss             - Code navigation
     test-project-status.ss        - Project status reporting
     test-refactor.ss              - Safe automated refactoring
     test-refactor-integrated.ss   - Refactoring with index integration
     test-scaffold.ss              - Code scaffolding
     test-string-utils.ss          - String utilities (86 tests)
     test-test-finder.ss           - Test discovery
     test-timing.ss                - Timing utilities
     test-toolkit.ss               - Toolkit functions
     test-type-search.ss           - Type-driven function search
     test-watch.ss                 - File watching
     test-watch-integration.ss     - Watch integration")
   (git-integration
    "test-git.ss                   - Git operations
     test-git-workflow.ss          - Git workflow helpers")
   (test-infrastructure
    "test-runner.ss                - Main test runner (runs all tests)
     test-tdd.ss                   - TDD workflow")))
 (usage
  "Run all tests:
     scheme --script shell/tests/test-runner.ss

   Run individual test:
     scheme --script shell/tests/test-color.ss
     scheme --script shell/tests/test-string-utils.ss

   Run category (via test-runner.ss structure):
     Load test-runner.ss and run specific test sets

   In REPL:
     (load \"shell/tests/test-benchmark.ss\")
     (run-tests)")
 (test-framework
  "All tests use core/test-framework.ss:
   - (test-group \"name\" tests...) for organization
   - (test-equal expected actual) for assertions
   - (test-assert condition) for boolean checks
   - (test-error expr) for expected failures
   - Automatic pass/fail counting and reporting")
 (conventions
  "Test file naming:
   - All tests use test-*.ss prefix (standardized 2026-01-08)
   - Tests live adjacent to code they test when possible
   - shell/tests/ holds shell-specific integration tests

   Test structure:
   - Load dependencies first
   - Define test helper functions
   - Organize tests in test-group blocks
   - Include both positive and negative tests
   - Test error handling and edge cases")
 (adding-tests
  "To add new shell tests:
   1. Create test-<feature>.ss in shell/tests/
   2. Load core/test-framework.ss
   3. Load module under test and dependencies
   4. Write test-group blocks with test assertions
   5. Add to test-runner.ss if integration needed
   6. Run individually to verify
   7. Run full suite to check for regressions")
 (dependencies (core/test-framework))
 (notes
  "All test files now use test-*.ss naming convention (2026-01-08).

   Some tests have restrictive permissions (marked with ----).
   These may require special environment setup or credentials.")
 (see-also ("core/test-framework.ss" "Test framework implementation")
            ("core/run-tests.ss" "Core test runner")
            ("shell/examples/" "Example code with demonstrations")
            ("INDEX.txt" "Detailed test index with categorization")))
