;;; shell/tools/test-init-project.ss — Tests for Project Initialization
;;;
;;; Run from project root: scheme --script shell/tools/test-init-project.ss

(load "shell/tools/init-project.ss")

(define test-count 0)
(define pass-count 0)
(define fail-count 0)

;;; Test assertion helper
(define (assert-equal name actual expected)
  (set! test-count (+ test-count 1))
  (if (equal? actual expected)
      (begin
       (set! pass-count (+ pass-count 1))
       (printf "  ✓ ~a\n" name))
      (begin
       (set! fail-count (+ fail-count 1))
       (printf "  ✗ ~a\n" name)
       (printf "    Expected: ~s\n" expected)
       (printf "    Actual:   ~s\n" actual))))

(define (assert-true name condition)
  (set! test-count (+ test-count 1))
  (if condition
      (begin
       (set! pass-count (+ pass-count 1))
       (printf "  ✓ ~a\n" name))
      (begin
       (set! fail-count (+ fail-count 1))
       (printf "  ✗ ~a\n" name)
       (printf "    Expected: #t\n")
       (printf "    Actual:   #f\n"))))

;;; ====
;;; Test 1: Project Types Configuration
;;; ====

(printf "\n=== Test 1: Project Types Configuration ===\n")

(assert-true "project types is alist" (list? *project-types*))
(assert-true "has core-module" (assq 'core-module *project-types*))
(assert-true "has shell-tool" (assq 'shell-tool *project-types*))
(assert-true "has application" (assq 'application *project-types*))
(assert-true "has library" (assq 'library *project-types*))
(assert-true "has playground" (assq 'playground *project-types*))

;;; ====
;;; Test 2: Make Config
;;; ====

(printf "\n=== Test 2: Make Config ===\n")

(let ([config (make-config "test-proj" 'shell-tool "A test" "Alice" #t #t #f)])
     (assert-true "config is alist" (list? config))
     (assert-equal "config name" (assoc-ref config 'name) "test-proj")
     (assert-equal "config type" (assoc-ref config 'type) 'shell-tool)
     (assert-equal "config description" (assoc-ref config 'description) "A test")
     (assert-equal "config author" (assoc-ref config 'author) "Alice")
     (assert-equal "config with-tests" (assoc-ref config 'with-tests) #t)
     (assert-equal "config with-git" (assoc-ref config 'with-git) #t)
     (assert-equal "config with-ci" (assoc-ref config 'with-ci) #f))

;;; ====
;;; Test 3: Make Default Config
;;; ====

(printf "\n=== Test 3: Make Default Config ===\n")

(let ([config (make-default-config "my-project")])
     (assert-equal "default config name" (assoc-ref config 'name) "my-project")
     (assert-equal "default config type" (assoc-ref config 'type) 'shell-tool)
     (assert-true "default has tests" (assoc-ref config 'with-tests))
     (assert-true "default has git" (assoc-ref config 'with-git)))

;;; ====
;;; Test 4: Assoc-Ref
;;; ====

(printf "\n=== Test 4: Assoc-Ref ===\n")

(let ([alist '((a . 1) (b . 2) (c . 3))])
     (assert-equal "assoc-ref finds a" (assoc-ref alist 'a) 1)
     (assert-equal "assoc-ref finds b" (assoc-ref alist 'b) 2)
     (assert-equal "assoc-ref missing" (assoc-ref alist 'missing) #f))

;;; ====
;;; Test 5: Path Join
;;; ====

(printf "\n=== Test 5: Path Join ===\n")

(assert-equal "join two paths"
              (path-join "dir" "file.ss")
              "dir/file.ss")
(assert-equal "join three paths"
              (path-join "a" "b" "c")
              "a/b/c")
(assert-equal "join one path"
              (path-join "single")
              "single")

;;; ====
;;; Test 6: Enumerate
;;; ====

(printf "\n=== Test 6: Enumerate ===\n")

(assert-equal "enumerate empty" (enumerate '()) '())
(assert-equal "enumerate three" (enumerate '(a b c)) '(1 2 3))
(assert-equal "enumerate one" (enumerate '(x)) '(1))

;;; ====
;;; Test 7: Format README
;;; ====

(printf "\n=== Test 7: Format README ===\n")

(let ([readme (format-readme "TestProj" "A test project")])
     (assert-true "readme contains name"
                  (string-contains? readme "TestProj"))
     (assert-true "readme contains description"
                  (string-contains? readme "A test project"))
     (assert-true "readme contains installation"
                  (string-contains? readme "Installation"))
     (assert-true "readme contains usage"
                  (string-contains? readme "Usage")))

;;; ====
;;; Test 8: Format CLAUDE.md
;;; ====

(printf "\n=== Test 8: Format CLAUDE.md ===\n")

(let ([claude-md (format-claude-md "TestProj" "A test project")])
     (assert-true "claude-md contains name"
                  (string-contains? claude-md "TestProj"))
     (assert-true "claude-md contains description"
                  (string-contains? claude-md "A test project"))
     (assert-true "claude-md contains guidelines"
                  (string-contains? claude-md "Guidelines")))

;;; ====
;;; Test 9: Format Main File
;;; ====

(printf "\n=== Test 9: Format Main File ===\n")

(let ([core-file (format-main-file "test" "Test module" 'core-module)])
     (assert-true "core file has header"
                  (string-contains? core-file "core/"))
     (assert-true "core file marked as pure"
                  (string-contains? core-file "pure")))

(let ([shell-file (format-main-file "test" "Test tool" 'shell-tool)])
     (assert-true "shell file has header"
                  (string-contains? shell-file "shell/"))
     (assert-true "shell file marked as impure"
                  (string-contains? shell-file "impure")))

;;; ====
;;; Test 10: Format Test File
;;; ====

(printf "\n=== Test 10: Format Test File ===\n")

(let ([test-file (format-test-file "myproj")])
     (assert-true "test file has header"
                  (string-contains? test-file "test-myproj.ss"))
     (assert-true "test file has load statement"
                  (string-contains? test-file "load"))
     (assert-true "test file has assert"
                  (string-contains? test-file "assert")))

;;; ====
;;; Test 11: Format Gitignore
;;; ====

(printf "\n=== Test 11: Format Gitignore ===\n")

(let ([gitignore (format-gitignore)])
     (assert-true "gitignore has .so"
                  (string-contains? gitignore ".so"))
     (assert-true "gitignore has .log"
                  (string-contains? gitignore ".log"))
     (assert-true "gitignore has .fold-repl"
                  (string-contains? gitignore ".fold-repl")))

;;; ====
;;; Test 12: Format CI Config
;;; ====

(printf "\n=== Test 12: Format CI Config ===\n")

(let ([ci-config (format-ci-config "test-proj")])
     (assert-true "ci config has name"
                  (string-contains? ci-config "name: CI"))
     (assert-true "ci config has test job"
                  (string-contains? ci-config "test:"))
     (assert-true "ci config has scheme install"
                  (string-contains? ci-config "chezscheme")))

;;; ====
;;; Test 13: Init Project (File Creation)
;;; ====

(printf "\n=== Test 13: Init Project (File Creation) ===\n")

(let ([test-proj-name "test-init-proj-temp"])
     ;; Clean up if exists
     (when (file-exists? test-proj-name)
           (system (format "rm -rf ~a" test-proj-name)))
     
     ;; Create project
     (guard (e [else
                (printf "    Init error: ~a\n"
                        (if (condition? e) (condition-message e) e))
                (assert-true "init-project failed" #f)])
            (init-project test-proj-name)
            
            ;; Check directory was created
            (assert-true "project directory created"
                         (file-exists? test-proj-name))
            
            ;; Check files exist
            (when (file-exists? test-proj-name)
                  (assert-true "README.md created"
                               (file-exists? (path-join test-proj-name "README.md")))
                  (assert-true "CLAUDE.md created"
                               (file-exists? (path-join test-proj-name "CLAUDE.md")))
                  (assert-true ".gitignore created"
                               (file-exists? (path-join test-proj-name ".gitignore")))))
     
     ;; Clean up
     (when (file-exists? test-proj-name)
           (system (format "rm -rf ~a" test-proj-name))))

;;; ====
;;; Test 14: Create Directories for Different Types
;;; ====

(printf "\n=== Test 14: Create Directories for Different Types ===\n")

;; Test core-module type
(let* ([config (make-config "temp-core" 'core-module "Test" "Me" #f #f #f)]
       [test-dir "temp-core"])
      (when (file-exists? test-dir)
            (system (format "rm -rf ~a" test-dir)))
      
      (guard (e [else (void)])
             (create-directories config)
             (assert-true "core-module creates core dir"
                          (file-exists? (path-join test-dir "core"))))
      
      (when (file-exists? test-dir)
            (system (format "rm -rf ~a" test-dir))))

;; Test application type
(let* ([config (make-config "temp-app" 'application "Test" "Me" #f #f #f)]
       [test-dir "temp-app"])
      (when (file-exists? test-dir)
            (system (format "rm -rf ~a" test-dir)))
      
      (guard (e [else (void)])
             (create-directories config)
             (assert-true "application creates docs dir"
                          (file-exists? (path-join test-dir "docs"))))
      
      (when (file-exists? test-dir)
            (system (format "rm -rf ~a" test-dir))))

;;; ====
;;; Test 15: Write File Function
;;; ====

(printf "\n=== Test 15: Write File Function ===\n")

(let ([temp-file "/tmp/test-write-file.txt"])
     (write-file temp-file "test content")
     (assert-true "file written" (file-exists? temp-file))
     
     (let ([content (call-with-input-file temp-file get-string-all)])
          (assert-equal "content correct" content "test content"))
     
     (when (file-exists? temp-file)
           (delete-file temp-file)))

;;; ====
;;; Test Summary
;;; ====

(printf "\n====\n")
(printf "Test Results:\n")
(printf "  Total:  ~a\n" test-count)
(printf "  Passed: ~a\n" pass-count)
(printf "  Failed: ~a\n" fail-count)

(if (= fail-count 0)
    (printf "\n✓ All tests passed!\n\n")
    (printf "\n✗ Some tests failed\n\n"))

;;; Return success/failure
(= fail-count 0)
