;;; shell/scaffold-examples.ss — Example Usage of Scaffolding System
;;;
;;; This file demonstrates how to use the scaffolding system effectively.
;;; Run these examples in the REPL after loading scaffold.ss.
;;;
;;; Usage:
;;;   (load "shell/repl.ss")
;;;   (load "shell/tools/scaffold.ss")
;;;   ;; Then try examples below

;;; ====
;;; Example 1: Create a Simple Shell Module
;;; ====

;;; Generate a utility module for string manipulation
(define (example-1-shell-module)
  (scaffold 'shell-module "string-helpers"
            '((description . "String manipulation utilities")
              (dependencies . "text.ss"))))

;;; This creates:
;;;   shell/string-helpers.ss
;;;   shell/test-string-helpers.ss
;;;
;;; Next steps:
;;;   1. Edit shell/string-helpers.ss to add your functions
;;;   2. Edit shell/test-string-helpers.ss to add tests
;;;   3. Run: scheme --script shell/test-string-helpers.ss

;;; ====
;;; Example 2: Create a Pure Core Module
;;; ====

;;; Generate a core module for mathematical operations
(define (example-2-core-module)
  (scaffold 'core-module "math-pure"
            '((description . "Pure mathematical operations")
              (dependencies . "prelude.ss"))))

;;; This creates:
;;;   core/math-pure.ss
;;;   core/test-math-pure.ss
;;;
;;; Remember:
;;;   - Core functions must be PURE (no IO)
;;;   - Core functions must be TOTAL (always terminate)
;;;   - Use fuel parameter for recursion
;;;   - No defensive code (Shell validates)

;;; ====
;;; Example 3: Create a Toolkit Analysis Tool
;;; ====

;;; Generate a tool to analyze code complexity
(define (example-3-analysis-tool)
  (scaffold 'tool "complexity-analyzer"
            '((category . analysis)
              (description . "Analyze code complexity metrics"))))

;;; This creates:
;;;   shell/complexity-analyzer.ss
;;;   shell/test-complexity-analyzer.ss
;;;
;;; After generation:
;;;   1. Implement the analysis logic
;;;   2. Add to shell/toolkit.ss registry:
;;;      (complexity-analyzer analysis "Analyze complexity" "complexity-analyzer.ss")
;;;   3. Test with: scheme --script shell/test-complexity-analyzer.ss

;;; ====
;;; Example 4: Create a Toolkit Introspection Tool
;;; ====

;;; Generate a tool to inspect block relationships
(define (example-4-introspection-tool)
  (scaffold 'tool "block-relationships"
            '((category . introspection)
              (description . "Visualize block reference relationships"))))

;;; Categories:
;;;   - building:      Code building and maintenance
;;;   - introspection: System examination and analysis
;;;   - debugging:     Debugging and troubleshooting
;;;   - analysis:      Performance and quality analysis

;;; ====
;;; Example 5: Create a Playground Game
;;; ====

;;; Generate a new game for the playpen
(define (example-5-playground-game)
  (scaffold 'playground "word-transform"
            '((type . game)
              (description . "Transform words using lambda functions"))))

;;; This creates:
;;;   playpen/templates/word-transform.ss
;;;
;;; Playground types:
;;;   - game:       Interactive games
;;;   - tool:       Interactive utilities
;;;   - experiment: Experimental creations
;;;
;;; After generation:
;;;   1. Implement game logic
;;;   2. Load in REPL: (load "user/templates/word-transform.ss")
;;;   3. Test gameplay

;;; ====
;;; Example 6: Create a Test Suite
;;; ====

;;; Generate tests for an existing module
(define (example-6-test-suite)
  (scaffold 'test-suite "my-module-tests"
            '((description . "Tests for my-module")
              (target-file . "shell/my-module.ss")
              (target-file-dir . "shell"))))

;;; Variables needed:
;;;   - description:    What is being tested
;;;   - target-file:    File being tested
;;;   - target-file-dir: Directory for test file

;;; ====
;;; Example 7: Create a Forum Post Draft
;;; ====

;;; Generate a structured forum post
(define (example-7-forum-post)
  (scaffold 'forum-post "architecture-proposal"
            '((channel . design)
              (title . "Proposed Architecture for Feature X")
              (body . "I propose we implement Feature X using...\n\nRationale:\n- Point 1\n- Point 2"))))

;;; This creates:
;;;   forum/drafts/architecture-proposal.ss
;;;
;;; To post:
;;;   1. Edit the draft file
;;;   2. In REPL:
;;;      (let ([body (read-text-file (fs) "forum/drafts/architecture-proposal.ss")])
;;;        (msg 'design "Proposed Architecture for Feature X" body))

;;; ====
;;; Example 8: Interactive Scaffolding
;;; ====

;;; Use the interactive wizard for guided creation
(define (example-8-interactive)
  (scaffold-interactive))

;;; The wizard will:
;;;   1. Show available templates
;;;   2. Prompt for template choice
;;;   3. Prompt for name
;;;   4. Prompt for each variable
;;;   5. Generate files

;;; ====
;;; Example 9: Custom Template
;;; ====

;;; Define a custom template for documentation files
(define (example-9-custom-template)
  (define-template
    'doc-file
    "Generate a documentation file"
    '((topic . ("Topic being documented" . "Feature"))
      (audience . ("Target audience" . "Developers")))
    '(((path . "docs/{{NAME}}.md")
       (content . "# {{TOPIC}}

Author: {{AUTHOR}}
Date: {{TIMESTAMP}}
Audience: {{AUDIENCE}}

## Overview

TODO: Add overview

## Details

TODO: Add details

## Examples

TODO: Add examples
"))))
  
  ;; Use the custom template
  (scaffold 'doc-file "scaffolding-guide"
            '((topic . "Scaffolding System")
              (audience . "Claude Code users"))))

;;; ====
;;; Example 10: Batch Scaffolding
;;; ====

;;; Generate multiple related modules at once
(define (example-10-batch-scaffolding)
  ;; Create a suite of related utilities
  (for-each
   (lambda (spec)
           (let ([name (car spec)]
                 [desc (cadr spec)])
                (scaffold 'shell-module name `((description . ,desc)))))
   '(("json-utils" "JSON parsing and generation")
     ("xml-utils" "XML parsing and generation")
     ("csv-utils" "CSV parsing and generation"))))

;;; This creates 6 files:
;;;   shell/json-utils.ss + test-json-utils.ss
;;;   shell/xml-utils.ss + test-xml-utils.ss
;;;   shell/csv-utils.ss + test-csv-utils.ss

;;; ====
;;; Example 11: Convention-Based Naming
;;; ====

;;; Using consistent naming conventions
(define (example-11-conventions)
  ;; Tool names should be descriptive
  (scaffold 'tool "dependency-visualizer"
            '((category . introspection)))
  
  ;; Core modules use simple, clear names
  (scaffold 'core-module "tree"
            '((description . "Pure tree operations")))
  
  ;; Shell modules can be more specific
  (scaffold 'shell-module "git-integration"
            '((description . "Git operations wrapper"))))

;;; Naming conventions:
;;;   - Core modules:   Simple, mathematical (list, tree, graph)
;;;   - Shell modules:  Descriptive, purpose-driven (git-integration)
;;;   - Tools:          Action-oriented (dependency-visualizer)
;;;   - Playgrounds:    Creative, playful (lambda-kombat)

;;; ====
;;; Example 12: Integration Checklist
;;; ====

;;; After scaffolding, complete these steps:
(define (post-scaffold-checklist module-type)
  (display "\nPost-Scaffolding Checklist:\n")
  (display "====\n\n")
  
  (case module-type
        [(shell-module)
         (display "1. [ ] Implement functions in shell/<name>.ss\n")
         (display "2. [ ] Add tests in shell/test-<name>.ss\n")
         (display "3. [ ] Run tests: scheme --script shell/test-<name>.ss\n")
         (display "4. [ ] Document usage in module header\n")
         (display "5. [ ] Add to shell/repl.ss if needed\n")
         (display "6. [ ] Commit changes\n")]
        
        [(core-module)
         (display "1. [ ] Implement pure functions in core/<name>.ss\n")
         (display "2. [ ] Ensure totality (use fuel for recursion)\n")
         (display "3. [ ] Add tests in core/test-<name>.ss\n")
         (display "4. [ ] Run tests: scheme --script core/test-<name>.ss\n")
         (display "5. [ ] Update core/MODULES.md dependency graph\n")
         (display "6. [ ] Commit changes\n")]
        
        [(tool)
         (display "1. [ ] Implement tool logic in shell/<name>.ss\n")
         (display "2. [ ] Add to shell/toolkit.ss registry\n")
         (display "3. [ ] Add tests in shell/test-<name>.ss\n")
         (display "4. [ ] Run tests: scheme --script shell/test-<name>.ss\n")
         (display "5. [ ] Test in REPL: (load \"shell/toolkit.ss\") then use tool\n")
         (display "6. [ ] Update toolkit help documentation\n")
         (display "7. [ ] Commit changes\n")]
        
        [(playground)
         (display "1. [ ] Implement game/tool logic in playpen/templates/<name>.ss\n")
         (display "2. [ ] Test in REPL: (load \"user/templates/<name>.ss\")\n")
         (display "3. [ ] Play/use the creation\n")
         (display "4. [ ] Document how to use in file header\n")
         (display "5. [ ] Add to shell/repl.ss if loading by default\n")
         (display "6. [ ] Commit changes\n")]
        
        [else
         (display "1. [ ] Review generated files\n")
         (display "2. [ ] Implement TODOs\n")
         (display "3. [ ] Test thoroughly\n")
         (display "4. [ ] Document\n")
         (display "5. [ ] Commit\n")]))

;;; ====
;;; Help Functions
;;; ====

;;; Show all examples
(define (show-all-examples)
  (display "\nScaffolding Examples:\n")
  (display "====\n\n")
  (display "1. (example-1-shell-module)        - Shell utility module\n")
  (display "2. (example-2-core-module)         - Pure core module\n")
  (display "3. (example-3-analysis-tool)       - Analysis toolkit tool\n")
  (display "4. (example-4-introspection-tool)  - Introspection tool\n")
  (display "5. (example-5-playground-game)     - Playground game\n")
  (display "6. (example-6-test-suite)          - Test suite\n")
  (display "7. (example-7-forum-post)          - Forum post draft\n")
  (display "8. (example-8-interactive)         - Interactive wizard\n")
  (display "9. (example-9-custom-template)     - Custom template\n")
  (display "10. (example-10-batch-scaffolding) - Batch generation\n")
  (display "11. (example-11-conventions)       - Naming conventions\n")
  (display "12. (post-scaffold-checklist 'shell-module) - Checklist\n")
  (display "\nRun any example function to try it!\n"))

(display "\nScaffolding examples loaded.\n")
(display "Usage: (show-all-examples)\n")
