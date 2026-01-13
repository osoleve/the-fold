;;; shell/project-status.ss --- Project Health Dashboard
;;;
;;; A unified view of project health: tests, code, git status.
;;; Fulfills the wishlist item: forum/wishlist/0006-projectstatus-tool.sexp
;;;
;;; This is Shell code: uses IO, runs system commands.
;;;
;;; Dependencies:
(load "core/base/prelude.ss")

;;; NOTE: string-trim, string-split provided by core/prelude.ss
;;;
;;; Provides:
;;;   (count-tests path)      - Count test files and test cases
;;;   (count-lines path)      - Count lines of code (excluding blanks/comments)
;;;   (git-status-info)       - Get structured git status
;;;   (recent-commits n)      - Get last n commits
;;;   (project-status)        - Aggregate all into health dashboard
;;;   (project-status-report) - Pretty-print the dashboard

;;; ============================================================
;;; Path and File Utilities
;;; ============================================================

;;; normalize-path : String -> String
;;; Ensure path uses forward slashes for consistency.
(define (normalize-path path)
  (let loop ([chars (string->list path)] [acc '()])
       (if (null? chars)
           (list->string (reverse acc))
           (loop (cdr chars)
                 (cons (if (char=? (car chars) #\\) #\/ (car chars))
                       acc)))))

;;; path-join : String x String -> String
;;; Join path components.
(define (path-join base name)
  (string-append (normalize-path base) "/" name))

;;; ends-with? : String x String -> Boolean
;;; Check if string ends with suffix.
(define (ends-with? str suffix)
  (let ([slen (string-length suffix)]
        [len (string-length str)])
       (and (>= len slen)
            (string=? suffix (substring str (- len slen) len)))))

;;; starts-with? : String x String -> Boolean
;;; Check if string starts with prefix.
(define (starts-with? str prefix)
  (let ([plen (string-length prefix)]
        [len (string-length str)])
       (and (>= len plen)
            (string=? prefix (substring str 0 plen)))))

;;; ============================================================
;;; Directory Traversal
;;; ============================================================

;;; list-directory-recursive : String -> (List String)
;;; Recursively list all files in a directory.
(define (list-directory-recursive path)
  (if (file-directory? path)
      (let ([entries (directory-list path)])
           (apply append
                  (map (lambda (entry)
                               (let ([full-path (path-join path entry)])
                                    (if (file-directory? full-path)
                                        (list-directory-recursive full-path)
                                        (list full-path))))
                       entries)))
      (list path)))

;;; filter-files : (String -> Boolean) x (List String) -> (List String)
;;; Filter file list by predicate.
(define (filter-files pred files)
  (let loop ([files files] [acc '()])
       (if (null? files)
           (reverse acc)
           (loop (cdr files)
                 (if (pred (car files))
                     (cons (car files) acc)
                     acc)))))

;;; ============================================================
;;; Test Counting
;;; ============================================================

;;; is-test-file? : String -> Boolean
;;; Check if a file is a test file (test-*.ss or *-test.ss).
(define (is-test-file? path)
  (let ([name (path-basename path)])
       (and (ends-with? name ".ss")
            (or (starts-with? name "test-")
                (ends-with? name "-test.ss")))))

;;; path-basename : String -> String
;;; Extract filename from path.
(define (path-basename path)
  (let ([normalized (normalize-path path)])
       (let loop ([i (- (string-length normalized) 1)])
            (cond
             [(< i 0) normalized]
             [(char=? (string-ref normalized i) #\/)
              (substring normalized (+ i 1) (string-length normalized))]
             [else (loop (- i 1))]))))

;;; count-test-forms-in-file : String -> Number
;;; Count (define-test ...) or (test-* ...) forms in a file.
;;; Also counts simple test assertions like (test-valid ...) or (test-pred ...).
(define (count-test-forms-in-file path)
  (guard (exn [else 0])
         (call-with-input-file path
                               (lambda (port)
                                       (let loop ([count 0])
                                            (let ([line (get-line port)])
                                                 (if (eof-object? line)
                                                     count
                                                     (let ([trimmed (string-trim line)])
                                                          (loop (if (or (starts-with? trimmed "(define-test")
                                                                        (starts-with? trimmed "(test-")
                                                                        (starts-with? trimmed "(Test ")
                                                                        (starts-with? trimmed "(display \"Test"))
                                                                    (+ count 1)
                                                                    count))))))))))

;;; count-tests : String -> Alist
;;; Count test files and test cases in a directory tree.
;;; Returns: ((files . N) (cases . M))
(define (count-tests path)
  (let* ([all-files (list-directory-recursive path)]
         [test-files (filter-files is-test-file? all-files)]
         [test-count (apply + (map count-test-forms-in-file test-files))])
        `((files . ,(length test-files))
          (cases . ,test-count))))

;;; ============================================================
;;; Line Counting
;;; ============================================================

;;; is-scheme-file? : String -> Boolean
;;; Check if a file is a Scheme source file.
(define (is-scheme-file? path)
  (ends-with? path ".ss"))

;;; is-blank-or-comment? : String -> Boolean
;;; Check if a line is blank or a comment.
(define (is-blank-or-comment? line)
  (let ([trimmed (string-trim line)])
       (or (= (string-length trimmed) 0)
           (and (> (string-length trimmed) 0)
                (char=? (string-ref trimmed 0) #\;)))))

;;; count-code-lines-in-file : String -> Number
;;; Count non-blank, non-comment lines in a file.
(define (count-code-lines-in-file path)
  (guard (exn [else 0])
         (call-with-input-file path
                               (lambda (port)
                                       (let loop ([count 0])
                                            (let ([line (get-line port)])
                                                 (if (eof-object? line)
                                                     count
                                                     (loop (if (is-blank-or-comment? line)
                                                               count
                                                               (+ count 1))))))))))

;;; count-lines : String -> Alist
;;; Count lines of code in a directory tree.
;;; Returns: ((files . N) (lines . M) (code-lines . K))
(define (count-lines path)
  (let* ([all-files (list-directory-recursive path)]
         [scheme-files (filter-files is-scheme-file? all-files)]
         [code-lines (apply + (map count-code-lines-in-file scheme-files))])
        `((files . ,(length scheme-files))
          (code-lines . ,code-lines))))

;;; ============================================================
;;; Git Operations
;;; ============================================================

;;; run-command : String -> String
;;; Run a shell command and capture its output.
(define (run-command cmd)
  (let ([tmp-file ".fold-tmp-cmd-output"])
       (system (string-append cmd " > " tmp-file " 2>&1"))
       (guard (exn [else ""])
              (let ([result (call-with-input-file tmp-file
                                                  (lambda (port)
                                                          (let loop ([lines '()])
                                                               (let ([line (get-line port)])
                                                                    (if (eof-object? line)
                                                                        (apply string-append
                                                                               (map (lambda (l) (string-append l "\n"))
                                                                                    (reverse lines)))
                                                                        (loop (cons line lines)))))))])
                   (when (file-exists? tmp-file)
                         (delete-file tmp-file))
                   result))))

;;; parse-git-status : String -> Alist
;;; Parse git status output into structured data.
;;; NOTE: uses string-split from core/prelude.ss
(define (parse-git-status output)
  (let ([lines (string-split output #\newline)])
       (let loop ([lines lines]
                  [modified '()]
                  [staged '()]
                  [untracked '()])
            (if (null? lines)
                `((status . ,(if (and (null? modified)
                                      (null? staged)
                                      (null? untracked))
                                 "clean"
                                 "dirty"))
                  (modified . ,(reverse modified))
                  (staged . ,(reverse staged))
                  (untracked . ,(reverse untracked)))
                (let* ([line (car lines)]
                       [len (string-length line)])
                      (if (< len 3)
                          (loop (cdr lines) modified staged untracked)
                          (let ([status-char (string-ref line 0)]
                                [work-char (string-ref line 1)]
                                [file-part (if (>= len 3) (substring line 3 len) "")])
                               (cond
                                [(char=? status-char #\?) ; Untracked
                                 (loop (cdr lines) modified staged (cons file-part untracked))]
                                [(char=? status-char #\M) ; Staged modification
                                 (loop (cdr lines) modified (cons file-part staged) untracked)]
                                [(char=? work-char #\M) ; Unstaged modification
                                 (loop (cdr lines) (cons file-part modified) staged untracked)]
                                [(char=? status-char #\A) ; Staged new file
                                 (loop (cdr lines) modified (cons file-part staged) untracked)]
                                [(char=? status-char #\D) ; Staged deletion
                                 (loop (cdr lines) modified (cons file-part staged) untracked)]
                                [else
                                 (loop (cdr lines) modified staged untracked)]))))))))

;;; git-status-info : -> Alist
;;; Get structured git status information.
(define (git-status-info)
  (let ([output (run-command "git status --porcelain")])
       (parse-git-status output)))

;;; recent-commits : Number -> (List Alist)
;;; Get the last n commits with hash, message, and time.
;;; Note: We make separate calls for each field to avoid Windows cmd
;;; issues with percent signs in format strings.
(define (recent-commits n)
  (let* ([hashes-cmd (string-append "git log -" (number->string n) " --format=%h")]
         [subjects-cmd (string-append "git log -" (number->string n) " --format=%s")]
         [times-cmd (string-append "git log -" (number->string n) " --format=%cr")]
         [hashes (filter (lambda (s) (> (string-length s) 0))
                         (string-split (run-command hashes-cmd) #\newline))]
         [subjects (filter (lambda (s) (> (string-length s) 0))
                           (string-split (run-command subjects-cmd) #\newline))]
         [times (filter (lambda (s) (> (string-length s) 0))
                        (string-split (run-command times-cmd) #\newline))])
        (map (lambda (h s t)
                     `((hash . ,h)
                       (message . ,s)
                       (time . ,t)))
             hashes subjects times)))

;;; parse-commit-line : String -> Alist
;;; Parse a git log line in format "hash|subject|relative-time".
(define (parse-commit-line line)
  (let ([parts (string-split line #\|)])
       (if (>= (length parts) 3)
           `((hash . ,(car parts))
             (message . ,(cadr parts))
             (time . ,(caddr parts)))
           `((hash . "unknown")
             (message . ,line)
             (time . "unknown")))))

;;; ============================================================
;;; Project Status Aggregator
;;; ============================================================

;;; project-status : -> Alist
;;; Get comprehensive project status.
(define (project-status)
  (let* ([cwd (current-directory)]
         [test-info (count-tests cwd)]
         [line-info (count-lines cwd)]
         [git-info (git-status-info)]
         [commits (recent-commits 1)]
         [last-commit (if (null? commits) #f (car commits))])
        `((files . ,(cdr (assq 'files line-info)))
          (code-lines . ,(cdr (assq 'code-lines line-info)))
          (test-files . ,(cdr (assq 'files test-info)))
          (test-cases . ,(cdr (assq 'cases test-info)))
          (git-status . ,(cdr (assq 'status git-info)))
          (modified-files . ,(cdr (assq 'modified git-info)))
          (staged-files . ,(cdr (assq 'staged git-info)))
          (untracked-files . ,(cdr (assq 'untracked git-info)))
          (last-commit . ,(if last-commit
                              (cdr (assq 'time last-commit))
                              "unknown"))
          (last-commit-message . ,(if last-commit
                                      (cdr (assq 'message last-commit))
                                      "unknown")))))

;;; ============================================================
;;; Pretty Printing
;;; ============================================================

;;; project-status-report : -> void
;;; Display a formatted project status report.
(define (project-status-report)
  (let ([status (project-status)])
       (display "\n")
       (display "+==============================================================+\n")
       (display "|                     PROJECT STATUS                           |\n")
       (display "+==============================================================+\n")
       (display "\n")
       
       ;; Code Statistics
       (display "  CODE:\n")
       (display (format "    Scheme files:  ~a\n" (cdr (assq 'files status))))
       (display (format "    Lines of code: ~a\n" (cdr (assq 'code-lines status))))
       (display "\n")
       
       ;; Test Statistics
       (display "  TESTS:\n")
       (display (format "    Test files:    ~a\n" (cdr (assq 'test-files status))))
       (display (format "    Test cases:    ~a\n" (cdr (assq 'test-cases status))))
       (display "\n")
       
       ;; Git Status
       (let ([git-status (cdr (assq 'git-status status))]
             [modified (cdr (assq 'modified-files status))]
             [staged (cdr (assq 'staged-files status))]
             [untracked (cdr (assq 'untracked-files status))])
            (display "  GIT:\n")
            (display (format "    Status:        ~a\n" git-status))
            (display (format "    Modified:      ~a\n" (length modified)))
            (display (format "    Staged:        ~a\n" (length staged)))
            (display (format "    Untracked:     ~a\n" (length untracked)))
            
            ;; Show modified files if any
            (unless (null? modified)
                    (display "    Modified files:\n")
                    (for-each (lambda (f)
                                      (display (format "      - ~a\n" f)))
                              (take-up-to 5 modified))
                    (when (> (length modified) 5)
                          (display (format "      ... and ~a more\n" (- (length modified) 5))))))
       (display "\n")
       
       ;; Last Commit
       (display "  LAST COMMIT:\n")
       (display (format "    Time:    ~a\n" (cdr (assq 'last-commit status))))
       (display (format "    Message: ~a\n" (cdr (assq 'last-commit-message status))))
       (display "\n")
       (display "+==============================================================+\n")))

;;; take-up-to : Number x List -> List
;;; Take up to n elements from a list.
(define (take-up-to n lst)
  (if (or (null? lst) (<= n 0))
      '()
      (cons (car lst) (take-up-to (- n 1) (cdr lst)))))

;;; ============================================================
;;; Quick Commands
;;; ============================================================

;;; status : -> void
;;; Alias for project-status-report.
(define (status)
  (project-status-report))

;;; health : -> Symbol
;;; Quick health check: returns 'healthy, 'dirty, or 'unknown.
(define (health)
  (let ([status (project-status)])
       (let ([git (cdr (assq 'git-status status))]
             [tests (cdr (assq 'test-cases status))])
            (cond
             [(string=? git "clean") 'healthy]
             [(string=? git "dirty") 'dirty]
             [else 'unknown]))))
