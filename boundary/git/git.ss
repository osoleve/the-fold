;;; boundary/git.ss — Git Operations
;;;
;;; Git operations for the Fold.
;;;
;;; This is Shell code: uses IO for git commands.

;;; ====
;;; Security Utilities
;;; ====

;;; shell-escape : String → String
;;; Escape a string for safe use in shell single quotes.
;;; Single quotes prevent all shell interpretation. To include a single quote
;;; inside single quotes, we end the quote, add an escaped quote, and restart.
(define (shell-escape str)
  (let ([len (string-length str)])
       (let loop ([i 0]
                  [chars '()])
            (if (>= i len)
                (list->string (reverse chars))
                (let ([c (string-ref str i)])
                     (if (char=? c #\')
                         (loop (+ i 1)
                               (append (reverse (string->list "'\\''")) chars))
                         (loop (+ i 1) (cons c chars))))))))

;;; safe-branch-name? : String → Boolean
;;; SECURITY: Validate git branch name to prevent injection.
;;; Git branch names cannot contain: space, ~, ^, :, \, ?, *, [, @{, ..
(define (safe-branch-name? name)
  (and (string? name)
       (> (string-length name) 0)
       (<= (string-length name) 256)
       (not (string-contains-char? name #\space))
       (not (string-contains-char? name #\~))
       (not (string-contains-char? name #\^))
       (not (string-contains-char? name #\:))
       (not (string-contains-char? name #\\))
       (not (string-contains-char? name #\?))
       (not (string-contains-char? name #\*))
       (not (string-contains-char? name #\[))
       (not (string-contains? name ".."))
       (not (string-contains? name "@{"))
       ;; Cannot start or end with / or .
       (not (char=? (string-ref name 0) #\/))
       (not (char=? (string-ref name 0) #\.))
       (not (char=? (string-ref name (- (string-length name) 1)) #\/))
       (not (char=? (string-ref name (- (string-length name) 1)) #\.))))

;;; string-contains-char? : String → Char → Boolean
(define (string-contains-char? str ch)
  (let loop ([i 0])
       (cond
        [(>= i (string-length str)) #f]
        [(char=? (string-ref str i) ch) #t]
        [else (loop (+ i 1))])))

;;; string-contains? : String → String → Boolean
(define (string-contains? haystack needle)
  (let ([hlen (string-length haystack)]
        [nlen (string-length needle)])
       (let loop ([i 0])
            (cond
             [(> (+ i nlen) hlen) #f]
             [(string=? (substring haystack i (+ i nlen)) needle) #t]
             [else (loop (+ i 1))]))))

;;; ====
;;; Git Status
;;; ====

;;; git-status : → void
;;; Show current git status.
(define (git-status)
  (system "git status")
  (void))

;;; git-diff : → void
;;; Show uncommitted changes.
(define (git-diff)
  (system "git diff")
  (void))

;;; git-log : [Nat] → void
;;; Show recent commits.
;;; SECURITY: Validates count is a positive integer.
(define (git-log . args)
  (let ([n (if (null? args) 5 (car args))])
       (unless (and (integer? n) (> n 0) (<= n 1000))
               (error 'git-log "Count must be a positive integer <= 1000" n))
       (system (format "git log --oneline -~a" n))
       (void)))

;;; ====
;;; Git Commit
;;; ====

;;; commit! : String → void
;;; Stage all changes and commit with message.
;;; SECURITY: Message is properly escaped with single quotes.
(define (commit! message)
  (unless (string? message)
          (error 'commit! "Commit message must be a string" message))
  ;; Stage all changes
  (system "git add -A")
  ;; Commit with properly escaped message
  (system (format "git commit -m '~a'" (shell-escape message)))
  (display (format "Committed: ~a\n" message)))

;;; truncate-commit-title : String → String
;;; Get first line of commit message, truncated for title.
(define (truncate-commit-title msg)
  (let* ([first-line (car (string-split msg #\newline))]
         [max-len 50])
        (if (<= (string-length first-line) max-len)
            first-line
            (string-append (substring first-line 0 (- max-len 3)) "..."))))

;;; ====
;;; Git Push
;;; ====

;;; push! : → void
;;; Push to origin.
(define (push!)
  (system "git push")
  (display "Pushed to origin.\n"))

;;; commit-and-push! : String → void
;;; Stage, commit, and push in one operation.
(define (commit-and-push! message)
  (commit! message)
  (push!))

;;; ====
;;; Git Branch
;;; ====

;;; git-branch : → void
;;; List branches.
(define (git-branch)
  (system "git branch -a")
  (void))

;;; git-checkout! : String → void
;;; Switch to a branch.
;;; SECURITY: Branch name is validated to prevent injection.
(define (git-checkout! branch)
  (unless (safe-branch-name? branch)
          (error 'git-checkout! "Invalid branch name" branch))
  (system (format "git checkout '~a'" (shell-escape branch)))
  (void))

;;; create-branch! : String → void
;;; Create and switch to new branch.
;;; SECURITY: Branch name is validated to prevent injection.
(define (create-branch! branch)
  (unless (safe-branch-name? branch)
          (error 'create-branch! "Invalid branch name" branch))
  (system (format "git checkout -b '~a'" (shell-escape branch)))
  (void))
