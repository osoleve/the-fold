((name "tools")
 (purpose "Developer tools for code analysis, formatting, and scaffolding")
 (description
  "Shell-level development tools providing IDE-like functionality.
   Includes refactoring, code coverage, formatting, dead code detection,
   module dependency analysis, benchmarking, scaffolding, documentation
   generation, and type-driven search. All tools handle IO defensively
   and integrate with the shell subsystems.")
 (modules
  ((archextract.ss "Archive extraction and inspection utilities")
   (ast-format.ss "AST-aware formatter with style profiles (compact/verbose/diff-friendly)")
   (benchmark.ss "Benchmarking harness with statistical analysis")
   (coverage.ss "Code coverage tracking and reporting")
   (dead-code.ss "Unused binding and dead code detection")
   (docgen.ss "API documentation generator from source comments")
   (export.ss "Module export analysis and reporting")
   (format.ss "Code formatter with consistent indentation styles (legacy)")
   (init-project.ss "Project initialization and setup")
   (markdown.ss "Markdown parsing and rendering utilities")
   (module-deps.ss "Module dependency graph analysis and cycle detection")
   (refactor.ss "Safe automated refactorings: rename, extract, inline")
   (refactor-integrated.ss "Refactoring with index and call graph integration")
   (scaffold.ss "Code scaffolding from templates with variable substitution")
   (string-utils.ss "String manipulation utilities for tools")
   (type-search.ss "Type-driven function search with pattern matching")
   (validate.ss "Input validation utilities")))
 (dependencies (base lang/index lens))
 (load-order "string-utils.ss first, then any order"))
