((name "examples")
 (purpose "Interactive demonstrations and example code for shell features")
 (description
  "Working examples showcasing shell subsystems and common patterns.
   Each example is runnable and demonstrates best practices for using
   shell APIs. Examples serve as both documentation and starting points
   for new features.")
 (modules
  ((commands-example.ss "Custom command registration and usage patterns")
   (core-playground.ss "Interactive experimentation with core functions")
   (demo-turtle.ss "Turtle graphics demonstration with shape drawing")
   (fuel-analysis-demo.ss "Fuel consumption tracking and profiling examples")
   (graphics-primitives-demo.ss "Graphics primitives showcase: lines, circles, fills")
   (scaffold-examples.ss "Code generation from templates with variable substitution")
   (string-utils-example.ss "String manipulation: split, join, trim, case conversion")
   (universe-example.ss "Universe (environment) and state management patterns")
   (watch-example.ss "File watching with auto-reload on changes")))
 (categories
  ((command-system "commands-example.ss")
   (graphics-visual "demo-turtle.ss" "graphics-primitives-demo.ss")
   (string-utilities "string-utils-example.ss")
   (performance "fuel-analysis-demo.ss")
   (scaffolding "scaffold-examples.ss")
   (persistence "universe-example.ss")
   (watch-system "watch-example.ss")
   (experimental "core-playground.ss")))
 (usage
  "Load examples in REPL:
     (load \"shell/examples/demo-turtle.ss\")
     (load \"shell/examples/graphics-primitives-demo.ss\")

   Execute directly:
     scheme --script shell/examples/string-utils-example.ss

   Most examples include interactive functions - check file comments
   for available commands and demonstrations.")
 (for-builders
  "Examples are ideal references when building new features:
   - Study patterns for similar functionality
   - Copy-paste starter code and adapt
   - See how to structure user-facing APIs
   - Learn defensive programming patterns
   - Understand shell/core boundaries")
 (for-players
  "Players can run examples to explore Fold capabilities:
   - Load examples in REPL to see what's possible
   - Modify examples in user/ directory
   - Experiment with parameters and variations
   - Use as templates for own creations")
 (see-also ("shell/tests/" "Full test suite with more code examples")
            ("shell/tools/" "Developer tools used by examples")
            ("shell/ui/" "Graphics and visualization subsystem")
            ("INDEX.txt" "Detailed index with descriptions")))
