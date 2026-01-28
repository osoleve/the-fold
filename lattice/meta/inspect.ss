(load "lattice/meta/dag.ss")
(load "boundary/io/process.ss")

(doc 'module 'inspect)
(doc 'description "Skill introspection providing detailed information for agent consumption")
(doc 'layer 'lattice)
(doc 'purity 'partial)

;;; ====
;;; Skill Description
;;; ====

;;; lattice-describe : Symbol -> void
;;; Pretty-print full skill description
(define (lattice-describe skill-name)
  (if (not (kg-initialized?))
      (begin
        (printf "Knowledge graph not initialized.\n")
        (printf "Run (lattice-init!) first.\n"))
      (let ([data (kg-skill-data skill-name)])
           (if (not data)
               (printf "Skill not found: ~a\n" skill-name)
           (let ([name (cdr (assq 'name data))]
                 [version (cdr (or (assq 'version data) '(version . "0.0.0")))]
                 [tier (cdr (or (assq 'tier data) '(tier . 0)))]
                 [path (cdr (or (assq 'path data) '(path . "")))]
                 [purity (cdr (or (assq 'purity data) '(purity . unknown)))]
                 [stability (cdr (or (assq 'stability data) '(stability . experimental)))]
                 [fuel-bound (cdr (or (assq 'fuel-bound data) '(fuel-bound . "O(?)")))]
                 [description (cdr (or (assq 'description data) '(description . "")))]
                 [keywords (cdr (or (assq 'keywords data) '(keywords . ())))]
                 [aliases (cdr (or (assq 'aliases data) '(aliases . ())))]
                 [deps (kg-deps skill-name)]
                 [uses (kg-uses skill-name)]
                 [modules (kg-modules skill-name)])
                
                (printf "~a\n" (make-string 60 #\=))
                (printf "~a v~a\n" name version)
                (printf "~a\n\n" (make-string 60 #\=))
                
                (when (and description (not (string=? description "")))
                      (printf "~a\n\n" (string-trim description)))
                
                (printf "Tier:      ~a\n" tier)
                (printf "Purity:    ~a\n" purity)
                (printf "Stability: ~a\n" stability)
                (printf "Fuel:      ~a\n" fuel-bound)
                (printf "Path:      ~a\n\n" path)
                
                (unless (null? keywords)
                        (printf "Keywords:  ~a\n" keywords))
                (unless (null? aliases)
                        (printf "Aliases:   ~a\n" aliases))
                
                (printf "\nDependencies: ")
                (if (null? deps)
                    (printf "(none)\n")
                    (printf "~a\n" deps))
                
                (printf "Used by:      ")
                (if (null? uses)
                    (printf "(none)\n")
                    (printf "~a\n" uses))
                
                (printf "\nModules (~a):\n" (length modules))
                (for-each
                 (lambda (mod)
                         (let ([mod-name (car mod)])
                              (printf "  - ~a\n" mod-name)))
                 modules))))))

;;; string-trim : String -> String
;;; Remove leading/trailing whitespace and collapse internal whitespace
(define (string-trim str)
  (if (not (string? str))
      ""
      (let* ([chars (string->list str)]
             [trimmed (trim-whitespace chars)])
            (list->string trimmed))))

(define (trim-whitespace chars)
  ;; Remove leading whitespace
  (let ([chars (let loop ([cs chars])
                    (if (and (pair? cs) (char-whitespace? (car cs)))
                        (loop (cdr cs))
                        cs))])
       ;; Remove trailing whitespace
       (reverse
        (let loop ([cs (reverse chars)])
             (if (and (pair? cs) (char-whitespace? (car cs)))
                 (loop (cdr cs))
                 cs)))))

;;; ====
;;; Export Listing
;;; ====

;;; lattice-skill-exports : Symbol -> (List Symbol)
;;; Get all exports from a skill
(define (lattice-skill-exports skill-name)
  (let* ([data (kg-skill-data skill-name)]
         [exports-raw (if data
                          (cdr (or (assq 'exports data) '(exports . ())))
                          '())])
        ;; Flatten export groups
        (append-map (lambda (group)
                            (if (and (pair? group) (list? group))
                                (cdr group)  ; Skip module name
                                '()))
                    (if (list? exports-raw) exports-raw '()))))

;;; lattice-all-exports : -> (List (skill . (exports ...)))
;;; Get all exports grouped by skill
(define (lattice-all-exports)
  (map (lambda (skill-name)
               (cons skill-name (lattice-skill-exports skill-name)))
       (kg-skills)))

;;; lattice-exports-pretty : Symbol -> void
;;; Pretty-print exports for a skill
(define (lattice-exports-pretty skill-name)
  (let ([data (kg-skill-data skill-name)])
       (if (not data)
           (printf "Skill not found: ~a\n" skill-name)
           (let ([exports-raw (cdr (or (assq 'exports data) '(exports . ())))])
                (printf "Exports for ~a\n" skill-name)
                (printf "~a\n\n" (make-string 40 #\-))
                (for-each
                 (lambda (group)
                         (when (and (pair? group) (list? group))
                               (printf "~a:\n" (car group))
                               (for-each
                                (lambda (exp)
                                        (printf "  ~a\n" exp))
                                (cdr group))
                               (printf "\n")))
                 (if (list? exports-raw) exports-raw '()))))))

;;; ====
;;; Module Listing
;;; ====

;;; lattice-modules-detail : Symbol -> void
;;; Pretty-print module details for a skill
(define (lattice-modules-detail skill-name)
  (let ([data (kg-skill-data skill-name)])
       (if (not data)
           (printf "Skill not found: ~a\n" skill-name)
           (let ([modules-raw (cdr (or (assq 'modules data) '(modules . ())))])
                (printf "Modules for ~a\n" skill-name)
                (printf "~a\n\n" (make-string 40 #\-))
                (for-each
                 (lambda (mod)
                         (when (and (pair? mod) (>= (length mod) 3))
                               (let ([name (car mod)]
                                     [file (cadr mod)]
                                     [desc (caddr mod)])
                                    (printf "~a\n" name)
                                    (printf "  File: ~a\n" file)
                                    (printf "  ~a\n\n" desc))))
                 (if (list? modules-raw) modules-raw '()))))))

;;; ====
;;; Source Location
;;; ====

;;; lattice-source : Symbol -> String | #f
;;; Get source file for an export (if discoverable)
(define (lattice-source export-name)
  (let loop ([skills (kg-skills)])
       (if (null? skills)
           #f
           (let* ([skill-name (car skills)]
                  [data (kg-skill-data skill-name)]
                  [path (if data (cdr (or (assq 'path data) '(path . ""))) "")]
                  [exports-raw (if data
                                   (cdr (or (assq 'exports data) '(exports . ())))
                                   '())])
                 ;; Search through export groups
                 (let find-in-groups ([groups exports-raw])
                      (if (null? groups)
                          (loop (cdr skills))  ; Not in this skill, try next
                          (let ([group (car groups)])
                               (if (and (pair? group) (list? group))
                                   (if (memq export-name (cdr group))
                                       ;; Found it! Get module file
                                       (let* ([mod-name (car group)]
                                              [modules-raw (cdr (or (assq 'modules data) '(modules . ())))]
                                              [mod-entry (find (lambda (m)
                                                                       (and (pair? m)
                                                                            (eq? (car m) mod-name)))
                                                               modules-raw)])
                                             (if (and mod-entry (>= (length mod-entry) 2))
                                                 (string-append path "/" (cadr mod-entry))
                                                 path))
                                       (find-in-groups (cdr groups)))
                                   (find-in-groups (cdr groups))))))))))

;;; find helper
(define (find pred lst)
  (cond
   [(null? lst) #f]
   [(pred (car lst)) (car lst)]
   [else (find pred (cdr lst))]))

;;; ====
;;; Agent-Friendly Info
;;; ====

;;; lattice-info : Symbol -> Alist | #f
;;; Get structured info about a skill for agent consumption
(define (lattice-info skill-name)
  (let ([data (kg-skill-data skill-name)])
       (if (not data)
           #f
           (let ([deps (kg-deps skill-name)]
                 [uses (kg-uses skill-name)]
                 [modules (kg-modules skill-name)]
                 [exports (lattice-skill-exports skill-name)])
                `((name . ,skill-name)
                  (version . ,(cdr (or (assq 'version data) '(version . "0.0.0"))))
                  (tier . ,(cdr (or (assq 'tier data) '(tier . 0))))
                  (purity . ,(cdr (or (assq 'purity data) '(purity . unknown))))
                  (stability . ,(cdr (or (assq 'stability data) '(stability . experimental))))
                  (path . ,(cdr (or (assq 'path data) '(path . ""))))
                  (description . ,(cdr (or (assq 'description data) '(description . ""))))
                  (dependencies . ,deps)
                  (dependents . ,uses)
                  (module-count . ,(length modules))
                  (export-count . ,(length exports))
                  (keywords . ,(cdr (or (assq 'keywords data) '(keywords . ()))))
                  (aliases . ,(cdr (or (assq 'aliases data) '(aliases . ())))))))))

;;; lattice-summary : -> void
;;; Print one-line summary for each skill
(define (lattice-summary)
  (if (not (kg-initialized?))
      (begin
        (printf "Knowledge graph not initialized.\n")
        (printf "Run (lattice-init!) first.\n"))
      (begin
        (printf "~20a ~8a ~8a ~6a ~a\n" "Skill" "Tier" "Purity" "Mods" "Description")
        (printf "~20a ~8a ~8a ~6a ~a\n" "----" "----" "----" "----" "----")
        (for-each
         (lambda (skill-name)
                 (let ([info (lattice-info skill-name)])
                      (when info
                            (printf "~20a ~8a ~8a ~6a ~a\n"
                                    (cdr (assq 'name info))
                                    (cdr (assq 'tier info))
                                    (cdr (assq 'purity info))
                                    (cdr (assq 'module-count info))
                                    (truncate-string (cdr (assq 'description info)) 40)))))
         (kg-skills)))))

;;; truncate-string : String Int -> String
(define (truncate-string str max-len)
  (let ([s (string-trim str)])
       (if (<= (string-length s) max-len)
           s
           (string-append (substring s 0 (- max-len 3)) "..."))))

;;; ====
;;; Convenience Functions (for REPL)
;;; ====

;;; li : Symbol -> void
;;; Quick skill inspection
(define (li skill-name)
  (lattice-describe skill-name))

;;; le : Symbol -> void
;;; Quick exports list
(define (le skill-name)
  (lattice-exports-pretty skill-name))

;;; lm : Symbol -> void
;;; Quick modules list
(define (lm skill-name)
  (lattice-modules-detail skill-name))

;;; ====
;;; Test Discovery
;;; ====

;;; find-test-files : String -> (List String)
;;; Find all test-*.ss files in a directory (non-recursive)
;;; Filters out directories that might match the pattern
(define (find-test-files dir)
  (guard (e [else '()])
         (let ([entries (directory-list dir)])
              (filter (lambda (f)
                              (and (string-starts-with? f "test-")
                                   (string-ends-with? f ".ss")
                                   ;; Verify it's a file, not a directory
                                   (file-regular? (string-append dir "/" f))))
                      entries))))

;;; lattice-tests : Symbol -> (List String)
;;; Get list of test files for a skill
;;; Returns full paths to test-*.ss files in the skill's directory
(define (lattice-tests skill-name)
  (let ([data (kg-skill-data skill-name)])
       (if (not data)
           (begin
             (printf "Skill not found: ~a\n" skill-name)
             '())
           (let* ([path (cdr (or (assq 'path data) '(path . "")))]
                  [test-files (find-test-files path)])
                 (map (lambda (f) (string-append path "/" f))
                      test-files)))))

;;; lattice-tests-pretty : Symbol -> void
;;; Pretty-print test files for a skill
(define (lattice-tests-pretty skill-name)
  (let ([tests (lattice-tests skill-name)])
       (if (null? tests)
           (printf "No tests found for ~a\n" skill-name)
           (begin
             (printf "Tests for ~a (~a files)\n" skill-name (length tests))
             (printf "~a\n\n" (make-string 40 #\-))
             (for-each (lambda (t) (printf "  ~a\n" t)) tests)))))

;;; lattice-tests-run : Symbol -> Alist
;;; Run tests for a skill and return structured results
;;; Result: ((total . N) (passed . N) (failed . N) (files . ((path . status) ...)))
;;; This is a shell-boundary operation - invokes external Scheme process
(define (lattice-tests-run skill-name)
  (let ([tests (lattice-tests skill-name)])
       (if (null? tests)
           `((total . 0) (passed . 0) (failed . 0) (files . ()))
           (let loop ([remaining tests]
                      [passed 0]
                      [failed 0]
                      [results '()])
                (if (null? remaining)
                    `((total . ,(length tests))
                      (passed . ,passed)
                      (failed . ,failed)
                      (files . ,(reverse results)))
                    (let* ([test-file (car remaining)]
                           [result (run-test-file test-file)]
                           [success? (eq? result 'ok)])
                          (loop (cdr remaining)
                                (if success? (+ passed 1) passed)
                                (if success? failed (+ failed 1))
                                (cons (cons test-file result) results))))))))

;;; parse-test-result-line : String -> Alist | #f
;;; Parse the structured result line: [TEST-RESULT total=N passed=N failed=N]
;;; Returns alist with 'total, 'passed, 'failed or #f if not found
;;; Uses string-last-index-of to find the LAST result line (in case tests
;;; print debug output containing result lines, or run nested tests)
(define (parse-test-result-line output)
  (let ([start (string-last-index-of output "[TEST-RESULT ")])
       (if (not start)
           #f
           (let* ([end (string-index-of-after output "]" start)]
                  [line (if end
                            (substring output start (+ end 1))
                            #f)])
                 (and line
                      (let ([total (parse-kv line "total=")]
                            [passed (parse-kv line "passed=")]
                            [failed (parse-kv line "failed=")])
                           (and total passed failed
                                `((total . ,total)
                                  (passed . ,passed)
                                  (failed . ,failed)))))))))

;;; parse-kv : String String -> Integer | #f
;;; Extract integer value after key= from string
(define (parse-kv str key)
  (let ([pos (string-index-of str key)])
       (if (not pos)
           #f
           (let* ([start (+ pos (string-length key))]
                  [rest (substring str start (string-length str))]
                  [digits (extract-leading-digits rest)])
                 (and (not (string=? digits ""))
                      (string->number digits))))))

;;; extract-leading-digits : String -> String
;;; Extract leading digit characters from string
(define (extract-leading-digits str)
  (let loop ([i 0] [acc '()])
       (if (>= i (string-length str))
           (list->string (reverse acc))
           (let ([c (string-ref str i)])
                (if (char-numeric? c)
                    (loop (+ i 1) (cons c acc))
                    (list->string (reverse acc)))))))

;;; string-index-of-after : String String Integer -> Integer | #f
;;; Find substring in string starting search at given position
(define (string-index-of-after haystack needle start)
  (let* ([rest (substring haystack start (string-length haystack))]
         [idx (string-index-of rest needle)])
        (and idx (+ start idx))))

;;; truncate-error-output : String -> String
;;; Truncate error output to last N characters for readability
(define (truncate-error-output output)
  (let ([max-len 500])
       (if (<= (string-length output) max-len)
           output
           (string-append "...\n" (substring output
                                             (- (string-length output) max-len)
                                             (string-length output))))))

;;; run-test-file : String -> 'ok | (error . String)
;;; Run a single test file and return result
;;; Primary: parse structured [TEST-RESULT ...] line
;;; Fallback: check exit code
(define (run-test-file path)
  (guard (e [else `(error . ,(format "~a" e))])
         (let* ([cmd (format "scheme --script '~a' 2>&1" (shell-escape path))]
                [result (shell-capture-result cmd)]
                [ok? (process-ok? result)]
                [output (process-stdout result)]
                [parsed (parse-test-result-line output)])
               (cond
                ;; Primary: structured result line (most reliable)
                [parsed
                 (if (= 0 (cdr (assq 'failed parsed)))
                     'ok
                     `(error . ,(format "~a/~a tests failed"
                                        (cdr (assq 'failed parsed))
                                        (cdr (assq 'total parsed)))))]
                ;; Fallback: exit code
                [ok? 'ok]
                ;; Error case: truncate large output
                [else `(error . ,(truncate-error-output output))]))))

;;; lattice-tests-run-pretty : Symbol -> void
;;; Run tests and display results
(define (lattice-tests-run-pretty skill-name)
  (printf "Running tests for ~a...\n\n" skill-name)
  (let* ([results (lattice-tests-run skill-name)]
         [total (cdr (assq 'total results))]
         [passed (cdr (assq 'passed results))]
         [failed (cdr (assq 'failed results))]
         [files (cdr (assq 'files results))])
        (if (= total 0)
            (printf "No tests found.\n")
            (begin
              (for-each
               (lambda (entry)
                       (let ([path (car entry)]
                             [status (cdr entry)])
                            (if (eq? status 'ok)
                                (printf "  ✓ ~a\n" path)
                                (printf "  ✗ ~a\n    ~a\n" path (cdr status)))))
               files)
              (printf "\n~a\n" (make-string 40 #\-))
              (printf "Total: ~a  Passed: ~a  Failed: ~a\n" total passed failed)
              (if (= failed 0)
                  (printf "✓ All tests passed!\n")
                  (printf "✗ ~a test~a failed\n" failed (if (= failed 1) "" "s")))))))

;;; lt : Symbol -> void
;;; Quick test file listing (follows li, le, lm pattern)
(define (lt skill-name)
  (lattice-tests-pretty skill-name))

;;; ltr : Symbol -> void
;;; Quick test runner (lt + run)
(define (ltr skill-name)
  (lattice-tests-run-pretty skill-name))

;;; lattice-all-tests : -> (List (Pair Symbol (List String)))
;;; Get test files for all skills
(define (lattice-all-tests)
  (map (lambda (skill-name)
               (cons skill-name (lattice-tests skill-name)))
       (kg-skills)))

;;; lattice-tests-summary : -> void
;;; Print summary of test coverage across all skills
(define (lattice-tests-summary)
  (if (not (kg-initialized?))
      (begin
        (printf "Knowledge graph not initialized.\n")
        (printf "Run (lattice-init!) first, or (kg-build!) for just the graph.\n"))
      (begin
        (printf "Test Coverage Summary\n")
        (printf "~a\n\n" (make-string 50 #\=))
        (let* ([all-tests (lattice-all-tests)]
               [with-tests (filter (lambda (e) (not (null? (cdr e)))) all-tests)]
               [without-tests (filter (lambda (e) (null? (cdr e))) all-tests)])
              (printf "Skills with tests: ~a/~a\n\n" (length with-tests) (length all-tests))
              (for-each
               (lambda (entry)
                       (printf "  ~20a ~a test file~a\n"
                               (car entry)
                               (length (cdr entry))
                               (if (= 1 (length (cdr entry))) "" "s")))
               (sort (lambda (a b) (> (length (cdr a)) (length (cdr b)))) with-tests))
              (when (not (null? without-tests))
                    (printf "\nSkills without tests:\n")
                    (for-each
                     (lambda (entry) (printf "  ~a\n" (car entry)))
                     without-tests))))))

;;; ====
;;; REPL Interface
;;; ====

(meta-printf "inspect.ss loaded.\n")
(meta-printf "  (lattice-describe 'skill)     - Full description\n")
(meta-printf "  (lattice-skill-exports 'skill) - Export list\n")
(meta-printf "  (lattice-modules-detail 'skill) - Module details\n")
(meta-printf "  (lattice-source 'export)      - Source location\n")
(meta-printf "  (lattice-info 'skill)         - Structured info\n")
(meta-printf "  (lattice-summary)             - All skills summary\n")
(meta-printf "  (lattice-tests 'skill)        - List test files\n")
(meta-printf "  (lattice-tests-run 'skill)    - Run skill tests\n")
(meta-printf "  (lattice-tests-summary)       - Test coverage overview\n")
(meta-printf "  (li 'skill), (le 'skill)      - Quick inspection\n")
(meta-printf "  (lt 'skill), (ltr 'skill)     - Quick test listing/running\n")
