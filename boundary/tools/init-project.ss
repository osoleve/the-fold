(load "core/base/prelude.ss")

(doc 'module 'init-project)
(doc 'description "Interactive wizard to set up new Fold projects with best practices")
(doc 'layer 'boundary)
(doc 'purity 'partial)

(doc 'section 'input-validation-security)

(define (valid-project-name? name)
  (doc 'type "String → Boolean")
  (doc 'description "Project names must be safe for use in shell commands and file paths")
  (doc 'note "Only alphanumeric characters, hyphens, and underscores allowed")
  (let ([len (string-length name)])
       (and (> len 0)
            (<= len 64)  ; Reasonable max length
            ;; Must start with letter
            (char-alphabetic? (string-ref name 0))
            ;; Only safe characters allowed
            (let loop ([i 0])
                 (if (>= i len)
                     #t
                     (let ([c (string-ref name i)])
                          (if (or (char-alphabetic? c)
                                  (char-numeric? c)
                                  (char=? c #\-)
                                  (char=? c #\_))
                              (loop (+ i 1))
                              #f)))))))

;;; valid-path-segment? : String → Boolean
;;; Path segments must not contain shell metacharacters or path traversal.
(define (valid-path-segment? s)
  (let ([len (string-length s)])
       (and (> len 0)
            (<= len 256)
            ;; No dangerous characters
            (not (string-contains? s ";"))
            (not (string-contains? s "&"))
            (not (string-contains? s "|"))
            (not (string-contains? s "$"))
            (not (string-contains? s "`"))
            (not (string-contains? s "\""))
            (not (string-contains? s "'"))
            (not (string-contains? s "\\"))
            (not (string-contains? s ".."))
            ;; No newlines or control characters
            (let loop ([i 0])
                 (if (>= i len)
                     #t
                     (let ([c (string-ref s i)])
                          (if (< (char->integer c) 32)
                              #f
                              (loop (+ i 1)))))))))

;;; ====
;;; Configuration
;;; ====

(define *project-types*
  '((core-module . "Pure Fold core module")
    (boundary-tool . "Boundary tool or utility")
    (application . "Complete application")
    (library . "Reusable library")
    (playground . "Experimental playground")))

;;; ====
;;; Main Initialization
;;; ====

;;; init-project : String → Bool
;;; Initialize new project with defaults.
;;; SECURITY: Validates project name to prevent command injection.
(define (init-project name)
  ;; SECURITY: Validate project name
  (unless (valid-project-name? name)
          (display (format "ERROR: Invalid project name: ~a\n" name))
          (display "Project names must start with a letter and contain only\n")
          (display "letters, numbers, hyphens, and underscores.\n")
          (error 'init-project "Invalid project name" name))
  (let ([config (make-default-config name)])
       (create-project config)))

;;; init-project-interactive : → Bool
;;; Interactive wizard for project setup.
;;; SECURITY: Validates project name to prevent command injection.
(define (init-project-interactive)
  (display "\n")
  (display "╔══════════════════════════════════════════════════════════════╗\n")
  (display "║              THE FOLD PROJECT INITIALIZATION                 ║\n")
  (display "╚══════════════════════════════════════════════════════════════╝\n")
  (display "\n")

  (let* ([name (prompt "Project name")]
         ;; SECURITY: Validate name immediately
         [_ (unless (valid-project-name? name)
                    (display "\nERROR: Invalid project name.\n")
                    (display "Names must start with a letter and contain only\n")
                    (display "letters, numbers, hyphens, and underscores.\n")
                    (error 'init-project-interactive "Invalid project name" name))]
         [type (prompt-choice "Project type" (map car *project-types*))]
         [description (prompt "Description")]
         [author (prompt "Author")]
         [with-tests? (prompt-bool "Include tests?" #t)]
         [with-git? (prompt-bool "Initialize git?" #t)]
         [with-ci? (prompt-bool "Add CI/CD config?" #f)]
         [config (make-config name type description author
                              with-tests? with-git? with-ci?)])
        
        (display "\n")
        (display "Creating project...\n")
        (create-project config)))

;;; make-config : String × Symbol × String × String × Bool × Bool × Bool → Alist
(define (make-config name type desc author tests? git? ci?)
  `((name . ,name)
    (type . ,type)
    (description . ,desc)
    (author . ,author)
    (with-tests . ,tests?)
    (with-git . ,git?)
    (with-ci . ,ci?)))

;;; make-default-config : String → Alist
(define (make-default-config name)
  (make-config name 'boundary-tool "A Fold project" "Unknown" #t #t #f))

;;; ====
;;; Project Creation
;;; ====

;;; create-project : Alist → Bool
(define (create-project config)
  (let ([name (assq-ref config 'name)])
       ;; Create directory structure
       (create-directories config)
       
       ;; Create files
       (create-project-files config)
       
       ;; Initialize git
       (when (assq-ref config 'with-git)
             (init-git config))
       
       ;; Display completion message
       (display-completion name)
       #t))

;;; create-directories : Alist → void
(define (create-directories config)
  (let ([name (assq-ref config 'name)]
        [type (assq-ref config 'type)])
       (display (format "Creating directory structure for ~a...\n" name))
       
       ;; Base directories
       (make-directory name)
       
       (case type
             [(core-module)
              (make-directory (path-join name "core"))
              (make-directory (path-join name "tests"))]
             [(boundary-tool)
              (make-directory (path-join name "boundary"))
              (make-directory (path-join name "tests"))]
             [(application)
              (make-directory (path-join name "core"))
              (make-directory (path-join name "boundary"))
              (make-directory (path-join name "tests"))
              (make-directory (path-join name "docs"))]
             [(library)
              (make-directory (path-join name "src"))
              (make-directory (path-join name "tests"))
              (make-directory (path-join name "examples"))]
             [(playground)
              (make-directory (path-join name "experiments"))])))

;;; create-project-files : Alist → void
(define (create-project-files config)
  (let ([name (assq-ref config 'name)])
       (display "Creating project files...\n")
       
       ;; README
       (create-readme config)
       
       ;; CLAUDE.md (instructions for Claude Code)
       (create-claude-md config)
       
       ;; Main module file
       (create-main-file config)
       
       ;; Test file (if requested)
       (when (assq-ref config 'with-tests)
             (create-test-file config))
       
       ;; .gitignore (if git enabled)
       (when (assq-ref config 'with-git)
             (create-gitignore config))
       
       ;; CI config (if requested)
       (when (assq-ref config 'with-ci)
             (create-ci-config config))))

;;; create-readme : Alist → void
(define (create-readme config)
  (let* ([name (assq-ref config 'name)]
         [desc (assq-ref config 'description)]
         [path (path-join name "README.md")]
         [content (format-readme name desc)])
        (write-file path content)
        (display (format "  Created ~a\n" path))))

;;; format-readme : String × String → String
(define (format-readme name desc)
  (string-append
   "# " name "\n\n"
   "> " desc "\n\n"
   "## Installation\n\n"
   "Load the module in your Scheme environment:\n\n"
   "```scheme\n"
   "(load \"" name ".ss\")\n"
   "```\n\n"
   "## Usage\n\n"
   "TODO: Add usage examples\n\n"
   "## Development\n\n"
   "Run tests:\n\n"
   "```bash\n"
   "scheme --script run-tests.ss\n"
   "```\n\n"
   "## License\n\n"
   "See LICENSE file.\n"))

;;; create-claude-md : Alist → void
(define (create-claude-md config)
  (let* ([name (assq-ref config 'name)]
         [desc (assq-ref config 'description)]
         [path (path-join name "CLAUDE.md")]
         [content (format-claude-md name desc)])
        (write-file path content)
        (display (format "  Created ~a\n" path))))

;;; format-claude-md : String × String → String
(define (format-claude-md name desc)
  (string-append
   "# CLAUDE.md\n\n"
   "Instructions for Claude Code when working with this project.\n\n"
   "## Project Overview\n\n"
   "**" name "**: " desc "\n\n"
   "## Architecture\n\n"
   "TODO: Describe project architecture\n\n"
   "## Development Guidelines\n\n"
   "- Write pure functions in core/\n"
   "- Handle effects in boundary/\n"
   "- Include tests for all new functionality\n"
   "- Follow The Fold's conventions\n\n"
   "## Testing\n\n"
   "Run tests with:\n\n"
   "```bash\n"
   "scheme --script run-tests.ss\n"
   "```\n"))

;;; create-main-file : Alist → void
(define (create-main-file config)
  (let* ([name (assq-ref config 'name)]
         [type (assq-ref config 'type)]
         [desc (assq-ref config 'description)]
         [path (case type
                     [(core-module) (path-join name "core" (string-append name ".ss"))]
                     [(boundary-tool) (path-join name "boundary" (string-append name ".ss"))]
                     [else (path-join name (string-append name ".ss"))])]
         [content (format-main-file name desc type)])
        (write-file path content)
        (display (format "  Created ~a\n" path))))

;;; format-main-file : String × String × Symbol → String
(define (format-main-file name desc type)
  (string-append
   ";;; " (case type
                [(core-module) "core/"]
                [(boundary-tool) "boundary/"]
                [else ""]) name ".ss — " desc "\n"
   ";;;\n"
   ";;; TODO: Add detailed description\n"
   ";;;\n"
   (case type
         [(core-module) ";;; This is Core code: pure, typed, total.\n"]
         [(boundary-tool) ";;; This is Boundary code: impure, effectful.\n"]
         [else ""])
   "\n"
   ";;; ====\n"
   ";;; Main Implementation\n"
   ";;; ====\n"
   "\n"
   ";;; TODO: Implement functionality\n"
   "\n"
   "(display \"" name " loaded\\n\")\n"))

;;; create-test-file : Alist → void
(define (create-test-file config)
  (let* ([name (assq-ref config 'name)]
         [path (path-join name "tests" (string-append "test-" name ".ss"))]
         [content (format-test-file name)])
        (write-file path content)
        (display (format "  Created ~a\n" path))))

;;; format-test-file : String → String
(define (format-test-file name)
  (string-append
   ";;; tests/test-" name ".ss — Tests for " name "\n\n"
   "(load \"../" name ".ss\")\n\n"
   ";;; Test assertions\n"
   "(define (assert condition msg)\n"
   "  (unless condition\n"
   "    (error 'test msg)))\n\n"
   ";;; TODO: Add tests\n\n"
   "(display \"All tests passed\\n\")\n"))

;;; create-gitignore : Alist → void
(define (create-gitignore config)
  (let* ([name (assq-ref config 'name)]
         [path (path-join name ".gitignore")]
         [content (format-gitignore)])
        (write-file path content)
        (display (format "  Created ~a\n" path))))

;;; format-gitignore : → String
(define (format-gitignore)
  "*.so\n*.o\n*.log\n*.tmp\n.fold-repl/\n.DS_Store\n")

;;; create-ci-config : Alist → void
(define (create-ci-config config)
  (let* ([name (assq-ref config 'name)]
         [path (path-join name ".github" "workflows" "ci.yml")]
         [content (format-ci-config name)])
        (make-directory (path-join name ".github"))
        (make-directory (path-join name ".github" "workflows"))
        (write-file path content)
        (display (format "  Created ~a\n" path))))

;;; format-ci-config : String → String
(define (format-ci-config name)
  (string-append
   "name: CI\n\n"
   "on: [push, pull_request]\n\n"
   "jobs:\n"
   "  test:\n"
   "    runs-on: ubuntu-latest\n"
   "    steps:\n"
   "      - uses: actions/checkout@v2\n"
   "      - name: Install Chez Scheme\n"
   "        run: sudo apt-get install chezscheme\n"
   "      - name: Run tests\n"
   "        run: scheme --script run-tests.ss\n"))

;;; ====
;;; Git Initialization
;;; ====

;;; init-git : Alist → void
;;; SECURITY: Project name is validated at init-project entry point.
(define (init-git config)
  (let ([name (assq-ref config 'name)])
       ;; SECURITY: Double-check name is valid before shell use
       (unless (valid-project-name? name)
               (error 'init-git "Invalid project name" name))
       (display "Initializing git repository...\n")
       (system (format "cd ~a && git init" name))
       (system (format "cd ~a && git add ." name))
       (system (format "cd ~a && git commit -m \"Initial commit\"" name))))

;;; ====
;;; Completion Display
;;; ====

;;; display-completion : String → void
(define (display-completion name)
  (display "\n")
  (display "╔══════════════════════════════════════════════════════════════╗\n")
  (display "║                  PROJECT CREATED                             ║\n")
  (display "╚══════════════════════════════════════════════════════════════╝\n")
  (display "\n")
  (display (format "Project '~a' created successfully!\n\n" name))
  (display "Next steps:\n")
  (display (format "  1. cd ~a\n" name))
  (display "  2. Start coding!\n")
  (display "  3. Run tests: scheme --script run-tests.ss\n")
  (display "\n"))

;;; ====
;;; Interactive Prompts
;;; ====

;;; prompt : String → String
(define (prompt question)
  (display (format "~a: " question))
  (flush-output-port)
  (get-line (current-input-port)))

;;; prompt-bool : String × Bool → Bool
(define (prompt-bool question default)
  (let ([default-str (if default "Y/n" "y/N")])
       (display (format "~a [~a]: " question default-str))
       (flush-output-port)
       (let ([response (get-line (current-input-port))])
            (cond
             [(string=? response "") default]
             [(or (string=? response "y") (string=? response "Y")) #t]
             [(or (string=? response "n") (string=? response "N")) #f]
             [else default]))))

;;; prompt-choice : String × (List Symbol) → Symbol
(define (prompt-choice question choices)
  (display (format "~a:\n" question))
  (for-each
   (lambda (choice i)
           (display (format "  ~a. ~a\n" i choice)))
   choices
   (enumerate choices))
  (display "Choice: ")
  (flush-output-port)
  (let ([response (get-line (current-input-port))])
       (let ([idx (string->number response)])
            (if (and idx (>= idx 1) (<= idx (length choices)))
                (list-ref choices (- idx 1))
                (car choices)))))

;;; enumerate : (List α) → (List Nat)
(define (enumerate lst)
  (let loop ([i 1] [l lst] [result '()])
       (if (null? l)
           (reverse result)
           (loop (+ i 1) (cdr l) (cons i result)))))

;;; ====
;;; Utility Functions
;;; ====

;; assq-ref is provided by prelude (uses eq? for symbol keys)

;;; path-join : String* → String
(define (path-join . parts)
  (string-join parts "/"))

;;; NOTE: string-join provided by core/prelude.ss

;;; make-directory : Path → void
;;; SECURITY: Validates path to prevent command injection.
(define (make-directory path)
  ;; SECURITY: Validate path before shell use
  (unless (valid-path-segment? path)
          (error 'make-directory "Invalid path" path))
  (guard (e [else (void)])
         (system (format "mkdir -p ~a" path))))

;;; write-file : Path × String → void
(define (write-file path content)
  (call-with-output-file path
                         (lambda (port)
                                 (display content port))
                         'replace))

(display "\n")
(display "Project initialization wizard loaded.\n")
(display "\n")
(display "Usage:\n")
(display "  (init-project \"name\")           - Initialize with defaults\n")
(display "  (init-project-interactive)       - Interactive wizard\n")
(display "\n")
(display "Try:\n")
(display "  (init-project-interactive)\n")
(display "\n")
