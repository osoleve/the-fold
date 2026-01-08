;;; shell/scaffold.ss — Code Scaffolding and Templating System
;;;
;;; Generate boilerplate code from templates for rapid development.
;;; Supports variable substitution, interactive prompts, and validation.
;;;
;;; This is Shell code: IO-heavy, user interaction, file creation.
;;;
;;; Usage:
;;;   (scaffold 'shell-module "my-module" '((description . "Does things")))
;;;   (scaffold 'core-module "my-pure-fn" '((description . "Pure computation")))
;;;   (scaffold 'tool "my-tool" '((category . building)))
;;;   (list-templates)
;;;   (scaffold-interactive)
;;;
;;; Dependencies:
;;;   shell/fs.ss (for file operations)
;;;   shell/text.ss (for text utilities)
;;;   core/prelude.ss (for string utilities)
;;;
;;; NOTE: string utilities (string-join, string-split, string-trim, string-contains?,
;;;       string-upcase, string-downcase, etc.) are provided by core/prelude.ss.
;;;       string-search is unique to this module.

(load "core/base/prelude.ss")

;;; ============================================================
;;; Template Registry
;;; ============================================================

;;; Template structure:
;;;   ((name . <symbol>)
;;;    (description . <string>)
;;;    (variables . ((var-name . (prompt . default)) ...))
;;;    (files . (((path . <string-template>)
;;;               (content . <string-template>)) ...)))
;;;
;;; String templates support these substitutions:
;;;   {{NAME}}         - module name (as provided)
;;;   {{NAME-UPPER}}   - module name in UPPERCASE
;;;   {{NAME-LOWER}}   - module name in lowercase
;;;   {{DESCRIPTION}}  - description from variables
;;;   {{AUTHOR}}       - author from session or variables
;;;   {{TIMESTAMP}}    - current timestamp
;;;   {{YEAR}}         - current year
;;;   {{VAR-NAME}}     - custom variable from options

(define *scaffold-templates* '())

;;; define-template : Symbol × String × Alist × (List Alist) → void
;;; Register a new template in the registry.
(define (define-template name description variables files)
  (let ([existing (assq name *scaffold-templates*)])
       (if existing
           (set! *scaffold-templates*
                 (cons `(,name . ((description . ,description)
                                  (variables . ,variables)
                                  (files . ,files)))
                       (filter (lambda (x) (not (eq? (car x) name)))
                               *scaffold-templates*)))
           (set! *scaffold-templates*
                 (cons `(,name . ((description . ,description)
                                  (variables . ,variables)
                                  (files . ,files)))
                       *scaffold-templates*)))))

;;; get-template : Symbol → Alist | #f
(define (get-template name)
  (let ([entry (assq name *scaffold-templates*)])
       (if entry (cdr entry) #f)))

;;; ============================================================
;;; String Substitution
;;; ============================================================

;;; substitute-vars : String × Alist → String
;;; Replace {{VAR}} placeholders with values from bindings.
(define (substitute-vars template bindings)
  (let loop ([str template])
       (let ([start (string-search "{{" str)])
            (if (not start)
                str
                (let ([end (string-search "}}" str)])
                     (if (not end)
                         str  ; Malformed template, return as-is
                         (let* ([var-name (substring str (+ start 2) end)]
                                [value (cdr (assoc var-name bindings))]
                                [before (substring str 0 start)]
                                [after (substring str (+ end 2) (string-length str))])
                               (loop (string-append before value after)))))))))

;;; string-search : String × String → Nat | #f
;;; Find first occurrence of needle in haystack.
(define (string-search needle haystack)
  (let ([nlen (string-length needle)]
        [hlen (string-length haystack)])
       (let loop ([i 0])
            (cond
             [(> (+ i nlen) hlen) #f]
             [(string=? needle (substring haystack i (+ i nlen))) i]
             [else (loop (+ i 1))]))))

;;; ============================================================
;;; Variable Resolution
;;; ============================================================

;;; build-bindings : String × Alist × Alist → Alist
;;; Build substitution bindings from name, template vars, and options.
(define (build-bindings name template-vars options)
  (let* ([base `(("NAME" . ,name)
                 ("NAME-UPPER" . ,(string-upcase name))
                 ("NAME-LOWER" . ,(string-downcase name))
                 ("TIMESTAMP" . ,(current-timestamp))
                 ("YEAR" . ,(timestamp-year (current-timestamp))))]
         [from-session (if (session-exists?)
                           (let ([session (read-session)])
                                `(("AUTHOR" . ,(symbol->string (cdr (assq 'name session))))))
                           '())]
         [from-options (map (lambda (opt)
                                    (cons (string-upcase (symbol->string (car opt)))
                                          (if (symbol? (cdr opt))
                                              (symbol->string (cdr opt))
                                              (cdr opt))))
                            options)]
         [from-template (map (lambda (var)
                                     (let* ([var-name (car var)]
                                            [var-spec (cdr var)]
                                            [default (cdr var-spec)]
                                            [opt-value (assq var-name options)])
                                           (cons (string-upcase (symbol->string var-name))
                                                 (if opt-value
                                                     (if (symbol? (cdr opt-value))
                                                         (symbol->string (cdr opt-value))
                                                         (cdr opt-value))
                                                     default))))
                             template-vars)])
        (append base from-session from-options from-template)))

;;; current-timestamp : → String
;;; Get current timestamp in ISO format.
(define (current-timestamp)
  (let* ([t (current-time)]
         [d (time-utc->date t)])
        (format "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d UTC"
                (date-year d)
                (date-month d)
                (date-day d)
                (date-hour d)
                (date-minute d)
                (date-second d))))

;;; timestamp-year : String → String
;;; Extract year from timestamp.
(define (timestamp-year ts)
  (substring ts 0 4))

;;; ============================================================
;;; Scaffolding Engine
;;; ============================================================

;;; scaffold : Symbol × String × Alist → void
;;; Generate code from a template.
(define (scaffold template-name name . options)
  (let ([opts (if (null? options) '() (car options))]
        [template (get-template template-name)])
       (if (not template)
           (begin
            (display (format "Error: Unknown template '~a'\n" template-name))
            (display "Available templates:\n")
            (list-templates))
           (let* ([description (cdr (assq 'description template))]
                  [variables (cdr (assq 'variables template))]
                  [files (cdr (assq 'files template))]
                  [bindings (build-bindings name variables opts)])
                 (display (format "Scaffolding ~a: ~a\n" template-name name))
                 (for-each
                  (lambda (file-spec)
                          (let* ([path-template (cdr (assq 'path file-spec))]
                                 [content-template (cdr (assq 'content file-spec))]
                                 [path (substitute-vars path-template bindings)]
                                 [content (substitute-vars content-template bindings)])
                                (display (format "  Creating ~a\n" path))
                                (let ([fs (mint-fs-capability ".")])
                                     (write-text-file! fs path content))))
                  files)
                 (display "Done.\n")))))

;;; list-templates : → void
;;; Display all available templates.
(define (list-templates)
  (display "\nAvailable Scaffolding Templates:\n")
  (display "=================================\n\n")
  (for-each
   (lambda (entry)
           (let* ([name (car entry)]
                  [spec (cdr entry)]
                  [description (cdr (assq 'description spec))])
                 (display (format "  ~a~a~a\n"
                                  name
                                  (make-string (max 1 (- 20 (string-length (symbol->string name)))) #\space)
                                  description))))
   *scaffold-templates*)
  (display "\nUsage: (scaffold 'template-name \"module-name\" '((option . value) ...))\n")
  (display "       (scaffold-interactive)\n\n"))

;;; scaffold-interactive : → void
;;; Interactive scaffolding wizard.
(define (scaffold-interactive)
  (display "\n=== Interactive Scaffolding Wizard ===\n\n")
  (list-templates)
  (display "\nEnter template name (or 'quit'): ")
  (let ([template-name (read)])
       (if (eq? template-name 'quit)
           (display "Cancelled.\n")
           (let ([template (get-template template-name)])
                (if (not template)
                    (begin
                     (display (format "Unknown template: ~a\n" template-name))
                     (scaffold-interactive))
                    (begin
                     (display "\nEnter module/component name: ")
                     (let ([name (symbol->string (read))])
                          (display "\nEnter values for template variables (press Enter for defaults):\n")
                          (let* ([variables (cdr (assq 'variables template))]
                                 [options (map (lambda (var)
                                                       (let* ([var-name (car var)]
                                                              [var-spec (cdr var)]
                                                              [prompt (car var-spec)]
                                                              [default (cdr var-spec)])
                                                             (display (format "  ~a [~a]: " prompt default))
                                                             (let ([input (get-line (current-input-port))])
                                                                  (cons var-name
                                                                        (if (or (eof-object? input)
                                                                                (string=? input ""))
                                                                            default
                                                                            input)))))
                                               variables)])
                                (scaffold template-name name options)))))))))

;;; ============================================================
;;; Built-in Templates
;;; ============================================================

;;; Shell Module Template
(define-template
  'shell-module
  "Complete shell module with tests and documentation"
  '((description . ("Module description" . "A new shell module"))
    (dependencies . ("Dependencies (comma-separated)" . "fs.ss, text.ss")))
  '(((path . "shell/{{NAME}}.ss")
     (content . ";;; shell/{{NAME}}.ss — {{DESCRIPTION}}
;;;
;;; This is Shell code: impure, defensive, handles failure.
;;;
;;; Dependencies:
;;;   {{DEPENDENCIES}}
;;;
;;; Usage:
;;;   (load \"shell/{{NAME}}.ss\")
;;;
;;; Created: {{TIMESTAMP}}
;;; Author: {{AUTHOR}}

;;; ============================================================
;;; Module Interface
;;; ============================================================

;;; TODO: Define your functions here

;;; {{NAME}}-example : String → String
;;; Example function for {{NAME}}.
(define ({{NAME}}-example input)
  (string-append \"{{NAME}}: \" input))

;;; ============================================================
;;; Exported Functions
;;; ============================================================

;;; Add your main module functions here.
"))
    ((path . "shell/test-{{NAME}}.ss")
     (content . ";;; Test harness for shell/{{NAME}}.ss
;;;
;;; Usage:
;;;   scheme --script shell/test-{{NAME}}.ss

;;; Load dependencies
(load \"core/block.ss\")
(load \"shell/fs.ss\")
(load \"shell/{{NAME}}.ss\")

(define (test name expected actual)
  (display \"  \")
  (display name)
  (display \": \")
  (if (equal? expected actual)
      (display \"✓\")
      (begin
        (display \"✗\\n    expected: \")
        (display expected)
        (display \"\\n    got: \")
        (display actual)))
  (newline))

(display \"{{DESCRIPTION}} Tests\\n\")
(display \"==========================================\\n\\n\")

;;; Test 1: Basic functionality
(display \"Test 1: Basic example\\n\")
(test \"example works\"
      \"{{NAME}}: hello\"
      ({{NAME}}-example \"hello\"))

(display \"\\n✓ All tests complete.\\n\")
"))))

;;; Core Module Template
(define-template
  'core-module
  "Pure core module with totality guarantees and tests"
  '((description . ("Module description" . "A new core module"))
    (dependencies . ("Dependencies" . "prelude.ss")))
  '(((path . "core/{{NAME}}.ss")
     (content . ";;; core/{{NAME}}.ss — {{DESCRIPTION}}
;;;
;;; This is Core code: pure, total, assumes perfect input.
;;;
;;; Dependencies:
;;;   {{DEPENDENCIES}}
;;;
;;; See core/MODULES.md for full dependency graph.
;;;
;;; Created: {{TIMESTAMP}}
;;; Author: {{AUTHOR}}

(load \"prelude.ss\")

;;; ============================================================
;;; Core Functions
;;; ============================================================

;;; {{NAME}}-identity : α → α
;;; Identity function for {{NAME}}.
;;; This is a total function — always terminates.
(define ({{NAME}}-identity x)
  x)

;;; TODO: Add your pure, total functions here.
;;; Remember:
;;;   - No IO
;;;   - Always terminate (use fuel for recursion)
;;;   - No defensive code (Shell validates input)
;;;   - Type signatures in comments
"))
    ((path . "core/test-{{NAME}}.ss")
     (content . ";;; Test harness for {{NAME}}.ss
;;;
;;; Uses the unified test framework.

;;; Load test framework if not already loaded
(unless (top-level-bound? 'define-test)
  (load \"test-framework.ss\"))

(load \"{{NAME}}.ss\")

(display \"{{DESCRIPTION}} Tests\\n\")
(display \"===================\\n\\n\")

;;; ============================================================
;;; Basic Tests
;;; ============================================================

(test-group {{NAME}}-basic
  (define-test identity
    (assert-equal 42 ({{NAME}}-identity 42))
    (assert-equal 'hello ({{NAME}}-identity 'hello))))

;;; ============================================================
;;; Tests run immediately when defined via define-test
;;; ============================================================
"))))

;;; Tool Template (for toolkit integration)
(define-template
  'tool
  "New toolkit tool with integration into toolkit.ss"
  '((description . ("Tool description" . "A new development tool"))
    (category . ("Category (building/introspection/debugging/analysis)" . "building")))
  '(((path . "shell/{{NAME}}.ss")
     (content . ";;; shell/{{NAME}}.ss — {{DESCRIPTION}}
;;;
;;; Part of The Fold Development Toolkit.
;;;
;;; This is Shell code: coordinates analysis and presents results.
;;;
;;; Usage:
;;;   (load \"shell/{{NAME}}.ss\")
;;;   ({{NAME}}-help)
;;;
;;; Created: {{TIMESTAMP}}
;;; Author: {{AUTHOR}}

;;; ============================================================
;;; Tool Interface
;;; ============================================================

;;; {{NAME}}-run : FS → void
;;; Main entry point for the {{NAME}} tool.
(define ({{NAME}}-run fs)
  (display \"Running {{NAME}}...\\n\")
  ;; TODO: Implement tool logic
  (display \"Done.\\n\"))

;;; {{NAME}}-help : → void
;;; Display help for {{NAME}}.
(define ({{NAME}}-help)
  (display \"\\n{{DESCRIPTION}}\\n\")
  (display \"\\nUsage:\\n\")
  (display \"  ({{NAME}}-run (fs))   Run the tool\\n\")
  (display \"  ({{NAME}}-help)       Show this help\\n\")
  (display \"\\n\"))
"))
    ((path . "shell/test-{{NAME}}.ss")
     (content . ";;; Test harness for shell/{{NAME}}.ss

(load \"core/block.ss\")
(load \"shell/fs.ss\")
(load \"shell/{{NAME}}.ss\")

(define (test name expected actual)
  (display \"  \")
  (display name)
  (display \": \")
  (if (equal? expected actual)
      (display \"✓\")
      (begin
        (display \"✗\\n    expected: \")
        (display expected)
        (display \"\\n    got: \")
        (display actual)))
  (newline))

(display \"{{DESCRIPTION}} Tests\\n\")
(display \"==========================================\\n\\n\")

;;; TODO: Add tests for your tool

(display \"\\n✓ All tests complete.\\n\")
"))))

;;; Test Suite Template
(define-template
  'test-suite
  "Standalone test file using test framework"
  '((description . ("What is being tested" . "Module tests"))
    (target-file . ("File being tested" . "module.ss")))
  '(((path . "{{TARGET-FILE-DIR}}/test-{{NAME}}.ss")
     (content . ";;; Test harness for {{TARGET-FILE}}
;;;
;;; Uses the unified test framework.

;;; Load test framework if not already loaded
(unless (top-level-bound? 'define-test)
  (load \"test-framework.ss\"))

(load \"{{TARGET-FILE}}\")

(display \"{{DESCRIPTION}}\\n\")
(display \"===================\\n\\n\")

;;; ============================================================
;;; Test Groups
;;; ============================================================

(test-group {{NAME}}-tests
  (define-test example
    (assert-true #t)
    (assert-equal 42 42)))

;;; ============================================================
;;; Tests run immediately when defined via define-test
;;; ============================================================
"))))

;;; Playground Creation Template
(define-template
  'playground
  "New playpen creation or game"
  '((description . ("Playground description" . "A new creation"))
    (type . ("Type (game/tool/experiment)" . "game")))
  '(((path . "user/templates/{{NAME}}.ss")
     (content . ";;; user/templates/{{NAME}}.ss — {{DESCRIPTION}}
;;;
;;; This is a template for user creations.
;;;
;;; Type: {{TYPE}}
;;;
;;; Created: {{TIMESTAMP}}
;;; Author: {{AUTHOR}}

;;; ============================================================
;;; Dependency Loading (with guards for idempotency)
;;; ============================================================

;;; Load dependencies if not already loaded
(define (load-if-needed path)
  (guard (exc [else #f])  ; Silently skip if already loaded
    (load path)))

(load-if-needed \"core/blocks/block.ss\")
(load-if-needed \"core/base/sha256.ss\")
(load-if-needed \"shell/fs.ss\")
(load-if-needed \"shell/ui/text.ss\")

;;; ============================================================
;;; State
;;; ============================================================

(define *{{NAME}}-state* #f)

;;; make-{{NAME}}-state : → Alist
(define (make-{{NAME}}-state)
  '((mode . active)
    (score . 0)))

;;; ============================================================
;;; Main Interface
;;; ============================================================

;;; {{NAME}}-start : → void
;;; Start the {{TYPE}}.
(define ({{NAME}}-start)
  (set! *{{NAME}}-state* (make-{{NAME}}-state))
  (display \"\\n=== {{DESCRIPTION}} ===\\n\\n\")
  ;; TODO: Implement your {{TYPE}} logic
  (display \"Welcome to {{NAME}}!\\n\"))

;;; {{NAME}}-help : → void
;;; Display help.
(define ({{NAME}}-help)
  (display \"\\n{{DESCRIPTION}}\\n\")
  (display \"\\nCommands:\\n\")
  (display \"  ({{NAME}}-start)  Start the {{TYPE}}\\n\")
  (display \"  ({{NAME}}-help)   Show this help\\n\")
  (display \"\\n\"))
"))))

;;; ============================================================
;;; Help Function
;;; ============================================================

;;; scaffold-help : → void
;;; Display comprehensive scaffolding help.
(define (scaffold-help)
  (display "\n")
  (display "╔══════════════════════════════════════════════════════════════════════════╗\n")
  (display "║                   SCAFFOLDING SYSTEM HELP                                ║\n")
  (display "╚══════════════════════════════════════════════════════════════════════════╝\n")
  (display "\n")
  (display "The scaffolding system generates boilerplate code from templates.\n")
  (display "\n")
  (display "BASIC USAGE:\n")
  (display "  (scaffold 'template-name \"module-name\" '((option . value) ...))\n")
  (display "\n")
  (display "INTERACTIVE MODE:\n")
  (display "  (scaffold-interactive)    ; Guided wizard\n")
  (display "\n")
  (display "TEMPLATE MANAGEMENT:\n")
  (display "  (list-templates)                              ; Show all templates\n")
  (display "  (define-template name desc vars files)        ; Register new template\n")
  (display "\n")
  (display "EXAMPLES:\n")
  (display "  ; Create a shell module\n")
  (display "  (scaffold 'shell-module \"my-module\"\n")
  (display "            '((description . \"Does cool things\")))\n")
  (display "\n")
  (display "  ; Create a core module\n")
  (display "  (scaffold 'core-module \"my-pure-fn\"\n")
  (display "            '((description . \"Pure computation\")))\n")
  (display "\n")
  (display "  ; Add a toolkit tool\n")
  (display "  (scaffold 'tool \"my-tool\"\n")
  (display "            '((category . introspection)\n")
  (display "              (description . \"Analyzes things\")))\n")
  (display "\n")
  (display "  ; Create a playground game\n")
  (display "  (scaffold 'playground \"my-game\"\n")
  (display "            '((type . game)))\n")
  (display "\n")
  (display "VARIABLE SUBSTITUTION:\n")
  (display "  Templates support these built-in variables:\n")
  (display "    {{NAME}}         - Module name as provided\n")
  (display "    {{NAME-UPPER}}   - Uppercase module name\n")
  (display "    {{NAME-LOWER}}   - Lowercase module name\n")
  (display "    {{DESCRIPTION}}  - Description from options\n")
  (display "    {{AUTHOR}}       - Author from session\n")
  (display "    {{TIMESTAMP}}    - Current timestamp\n")
  (display "    {{YEAR}}         - Current year\n")
  (display "    {{CUSTOM-VAR}}   - Any custom variable from options\n")
  (display "\n")
  (display "After scaffolding, manually:\n")
  (display "  1. Review generated files\n")
  (display "  2. Add to toolkit.ss if creating a tool\n")
  (display "  3. Run tests: scheme --script <test-file>.ss\n")
  (display "  4. Update documentation\n")
  (display "\n"))

;;; ============================================================
;;; Validation
;;; ============================================================

;;; validate-scaffold : String × Alist → Boolean
;;; Validate that required files can be created.
(define (validate-scaffold name options)
  ;; TODO: Check for:
  ;;   - Name conflicts (file exists)
  ;;   - Valid characters in name
  ;;   - Required options present
  #t)

;;; ============================================================
;;; Module Initialization
;;; ============================================================

(display "Scaffolding system loaded.\n")
(display "Usage: (list-templates) or (scaffold-interactive)\n")
