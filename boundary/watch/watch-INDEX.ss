(define-syntax doc
  (syntax-rules ()
    [(_ args ...) (void)]))

(doc 'module 'watch-INDEX)
(doc 'description "Index of Watch System Files - Lists all components of the file watching and auto-reload system. Use this as a reference for what exists and where to find it.")
(doc 'layer 'boundary)
(doc 'purity 'total)
(doc 'dependencies '())

(doc 'section 'core-implementation)

(doc 'note "boundary/watch/watch.ss - Main implementation of the watch system")
(doc 'note "Lines: ~650")
(doc 'note "Exports:")
(doc 'note "  - watch-file, watch-dir, auto-reload, auto-test")
(doc 'note "  - stop-watcher!, stop-watching, list-watchers, watch-help")
(doc 'note "  - glob-match?, file-mtime")
(doc 'note "  - *watch-poll-interval*, *watch-debounce-delay*")
(doc 'note "Dependencies: None (uses only standard R6RS Scheme)")
(doc 'note "Load with: (load \"boundary/watch/watch.ss\")")

(doc 'section 'daemon-integration)

;;; boundary/watch-daemon-integration.ss
;;;   Integration with REPL daemon for development workflows
;;;   Lines: ~400
;;;   Exports:
;;;     - dev-mode-on
;;;     - dev-mode-off
;;;     - dev-status
;;;     - watch-tests
;;;     - stop-test-watchers
;;;     - smart-reload
;;;     - watch-with-smart-reload
;;;     - auto-dev-mode
;;;     - dev-help
;;;   Dependencies:
;;;     - boundary/watch/watch.ss
;;;   Load with:
;;;     (load "boundary/watch-daemon-integration.ss")
;;;   Features:
;;;     - One-command development mode
;;;     - Auto-reload for core modules
;;;     - Test auto-runner
;;;     - Smart reload (module + dependents)
;;;     - Module dependency graph

(doc 'section 'testing)

;;; boundary/test-watch.ss
;;;   Comprehensive test suite for watch system
;;;   Lines: ~300
;;;   Tests:
;;;     - Glob pattern matching (7 tests)
;;;     - File modification time (2 tests)
;;;     - Path scanning (3 tests)
;;;     - Watcher creation (6 tests)
;;;     - Change detection (2 tests)
;;;     - Directory watching (1 test)
;;;     - Multiple watchers (2 tests)
;;;     - Stop watchers (1 test)
;;;     - Debouncing (1 test)
;;;   Total: 25 test cases
;;;   Dependencies:
;;;     - boundary/watch/watch.ss
;;;   Run with:
;;;     scheme --script boundary/test-watch.ss
;;;   Notes:
;;;     - Creates temporary test directory
;;;     - Tests watcher lifecycle
;;;     - Tests change detection
;;;     - Cleans up after tests

;;; boundary/test-watch-integration.ss
;;;   Integration test for complete watch system
;;;   Lines: ~250
;;;   Tests:
;;;     - Loading watch system
;;;     - Verifying exports
;;;     - Configuration values
;;;     - Watcher lifecycle
;;;     - Daemon integration
;;;     - Examples loading
;;;     - Glob matching
;;;     - Cleanup
;;;   Total: 10 integration tests
;;;   Dependencies:
;;;     - boundary/watch/watch.ss
;;;     - boundary/watch-daemon-integration.ss
;;;     - boundary/watch-example.ss
;;;   Run with:
;;;     scheme --script boundary/test-watch-integration.ss
;;;   Notes:
;;;     - End-to-end validation
;;;     - Verifies all components work together
;;;     - Quick smoke test

(doc 'section 'examples)

;;; boundary/watch-example.ss
;;;   Comprehensive examples of watch system usage
;;;   Lines: ~500
;;;   Examples:
;;;     1. Auto-reload single module
;;;     2. Auto-run tests on change
;;;     3. Watch directory with pattern
;;;     4. Rebuild on change
;;;     5. Multiple watchers with different actions
;;;     6. Custom action with validation
;;;     7. Debouncing visualization
;;;   Exports:
;;;     - example-1-auto-reload
;;;     - example-2-auto-test
;;;     - example-3-watch-core
;;;     - example-4-rebuild-on-change
;;;     - example-5-multi-watch
;;;     - example-6-custom-action
;;;     - example-7-debounce-demo
;;;     - watch-examples-help
;;;   Dependencies:
;;;     - boundary/watch/watch.ss
;;;   Load with:
;;;     (load "boundary/watch-example.ss")
;;;   Usage:
;;;     (watch-examples-help)  ; Show all examples
;;;     (example-1-auto-reload) ; Run example 1

(doc 'section 'documentation)

;;; boundary/watch-README.ss
;;;   Complete documentation and API reference
;;;   Lines: ~600
;;;   Sections:
;;;     - Overview
;;;     - Architecture
;;;     - Basic usage
;;;     - API reference (all functions)
;;;     - Configuration
;;;     - Glob patterns
;;;     - Daemon integration
;;;     - Advanced usage
;;;     - Visual feedback
;;;     - Troubleshooting
;;;     - Examples
;;;     - Implementation notes
;;;     - Future enhancements
;;;     - See also
;;;   Load with:
;;;     (load "boundary/watch-README.ss")  ; Read as documentation
;;;   Notes:
;;;     - Comprehensive API documentation
;;;     - Architecture decisions explained
;;;     - Troubleshooting guide
;;;     - Implementation details

;;; boundary/watch-quickstart.ss
;;;   Quick start guide with copy-paste examples
;;;   Lines: ~300
;;;   Sections:
;;;     - Step-by-step getting started
;;;     - Configuration
;;;     - Common patterns
;;;     - Typical workflow
;;;     - Tips
;;;     - Troubleshooting
;;;   Load with:
;;;     (load "boundary/watch-quickstart.ss")
;;;   Notes:
;;;     - Copy-paste ready snippets
;;;     - Fast onboarding
;;;     - Typical workflows
;;;     - Best practices

;;; boundary/watch-SUMMARY.ss
;;;   Implementation summary and overview
;;;   Lines: ~500
;;;   Sections:
;;;     - What was built
;;;     - Files created
;;;     - Key features
;;;     - Architecture decisions
;;;     - Integration points
;;;     - Configuration
;;;     - Testing
;;;     - Usage examples
;;;     - Future enhancements
;;;     - Performance characteristics
;;;     - Compatibility
;;;     - Conclusion
;;;   Load with:
;;;     (load "boundary/watch-SUMMARY.ss")  ; Read as documentation
;;;   Notes:
;;;     - High-level overview
;;;     - Design decisions explained
;;;     - Performance characteristics
;;;     - Complete feature list

;;; boundary/watch/watch-INDEX.ss
;;;   This file - index of all watch system components
;;;   Lines: ~300
;;;   Sections:
;;;     - Core implementation
;;;     - Daemon integration
;;;     - Testing
;;;     - Examples
;;;     - Documentation
;;;     - Quick reference
;;;   Notes:
;;;     - Navigation aid
;;;     - Complete file listing
;;;     - Quick reference

(doc 'section 'quick-reference)

(doc 'note "Getting Started:")
(doc 'note "  1. (load \"boundary/watch/watch.ss\")")
(doc 'note "  2. (auto-reload \"your-module.ss\")")
(doc 'note "  3. Or: (load \"boundary/watch-daemon-integration.ss\")")
(doc 'note "         (dev-mode-on)")

(doc 'note "Basic Commands:")
(doc 'note "  (watch-file path action)           - Watch single file")
(doc 'note "  (watch-dir path pattern action)    - Watch directory")
(doc 'note "  (auto-reload module-path)          - Auto-reload module")
(doc 'note "  (auto-test test-path)              - Auto-run tests")
(doc 'note "  (stop-watching)                    - Stop all watchers")
(doc 'note "  (list-watchers)                    - Show active watchers")

(doc 'note "Development Mode:")
(doc 'note "  (dev-mode-on)                      - Enable dev mode")
(doc 'note "  (dev-mode-off)                     - Disable dev mode")
(doc 'note "  (dev-status)                       - Show status")
(doc 'note "  (watch-tests)                      - Watch all tests")
(doc 'note "  (stop-test-watchers)               - Stop test watchers")

(doc 'note "Configuration:")
(doc 'note "  *watch-poll-interval*              - Polling frequency (ms)")
(doc 'note "  *watch-debounce-delay*             - Debounce delay (ms)")

(doc 'note "Help:")
(doc 'note "  (watch-help)                       - Watch system help")
(doc 'note "  (dev-help)                         - Dev mode help")
(doc 'note "  (watch-examples-help)              - Examples help")
(doc 'note "  (load \"boundary/watch-quickstart.ss\") - Quick start guide")

(doc 'section 'file-organization)

(doc 'note "All files are in boundary/ directory:")
(doc 'note "  boundary/watch.ss                    - Core implementation")
(doc 'note "  boundary/watch-daemon-integration.ss - Daemon integration")
(doc 'note "  boundary/watch-example.ss            - Examples")
(doc 'note "  boundary/watch-quickstart.ss         - Quick start guide")
(doc 'note "  boundary/watch-README.ss             - Full documentation")
(doc 'note "  boundary/watch-SUMMARY.ss            - Implementation summary")
(doc 'note "  boundary/watch-INDEX.ss              - This file")
(doc 'note "  boundary/test-watch.ss               - Test suite")
(doc 'note "  boundary/test-watch-integration.ss   - Integration tests")

(doc 'note "Total: 9 files, ~3000+ lines")
(doc 'note "Core: 1 file (watch.ss)")
(doc 'note "Extensions: 2 files (daemon, examples)")
(doc 'note "Tests: 2 files (unit, integration)")
(doc 'note "Documentation: 4 files (README, quickstart, summary, index)")

(doc 'section 'dependency-graph)

(doc 'note "watch.ss - No dependencies (standalone)")
(doc 'note "watch-daemon-integration.ss - Depends on watch.ss")
(doc 'note "watch-example.ss - Depends on watch.ss")
(doc 'note "test-watch.ss - Depends on watch.ss")
(doc 'note "test-watch-integration.ss - Depends on watch.ss, watch-daemon-integration.ss, watch-example.ss")
(doc 'note "watch-README.ss - Documentation only")
(doc 'note "watch-quickstart.ss - Documentation only")
(doc 'note "watch-SUMMARY.ss - Documentation only")
(doc 'note "watch-INDEX.ss - Documentation only (this file)")

(doc 'section 'integration-with-the-fold)

(doc 'note "The watch system integrates with:")
(doc 'note "  REPL Daemon - Runs in daemon process, background threads, persistent state")
(doc 'note "  Filesystem - Uses file-exists?, mkdir, delete-file")
(doc 'note "  Testing Framework - Auto-run test files")
(doc 'note "  Module System - Auto-reload with (load ...)")
(doc 'note "  Development Workflow - TDD support, live coding, fast feedback loop")

(doc 'note "Future integration possibilities:")
(doc 'note "  - Forum system, Build system, CAS, Git hooks")

(doc 'section 'usage-statistics)

(doc 'note "Lines of code: ~3500 total (650 implementation, 400 daemon, 500 examples, 550 tests, 1400 docs)")
(doc 'note "Test coverage: 42 total scenarios (25 unit, 10 integration, 7 examples)")
(doc 'note "Public API: 17 exports (7 core functions, 8 daemon integration, 2 config vars)")
(doc 'note "Documentation: 4 files (README, quickstart, summary, index)")

(doc 'section 'conclusion)

(doc 'note "The watch system is complete and ready to use.")
(doc 'note "To get started:")
(doc 'note "  1. Load boundary/watch/watch.ss")
(doc 'note "  2. Try (auto-reload \"your-module.ss\")")
(doc 'note "  3. Or use (dev-mode-on) for full workflow")
(doc 'note "For help:")
(doc 'note "  - (watch-help) for command reference")
(doc 'note "  - (load \"boundary/watch-quickstart.ss\") for quick start")
(doc 'note "  - (load \"boundary/watch-README.ss\") for full documentation")
(doc 'note "For examples:")
(doc 'note "  - (load \"boundary/watch-example.ss\")")
(doc 'note "  - (watch-examples-help)")
(doc 'note "To test:")
(doc 'note "  - scheme --script boundary/test-watch.ss")
(doc 'note "  - scheme --script boundary/test-watch-integration.ss")

(doc 'note "End of index.")
