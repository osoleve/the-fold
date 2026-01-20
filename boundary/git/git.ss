;;; Local doc macro - git.ss is self-contained, no prelude dependency
(define-syntax doc
  (syntax-rules ()
    [(_ args ...) (void)]))

(doc 'module 'git)
(doc 'description "Git Operations — Git operations for the Fold.")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'dependencies '())
(doc 'note "Self-contained module. Uses IO for git commands.")

(doc 'section 'security-utilities)

(define (shell-escape str)
  (doc 'type (-> String String))
  (doc 'description "Escape a string for safe use in shell single quotes. Single quotes prevent all shell interpretation. To include a single quote inside single quotes, we end the quote, add an escaped quote, and restart.")
  (doc 'export #t)
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

(define (safe-branch-name? name)
  (doc 'type (-> String Bool))
  (doc 'description "SECURITY: Validate git branch name to prevent injection. Git branch names cannot contain: space, ~, ^, :, \\, ?, *, [, @{, ..")
  (doc 'export #t)
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

(define (string-contains-char? str ch)
  (doc 'type (-> String Char Bool))
  (doc 'description "Check if string contains character.")
  (let loop ([i 0])
       (cond
        [(>= i (string-length str)) #f]
        [(char=? (string-ref str i) ch) #t]
        [else (loop (+ i 1))])))

(define (string-contains? haystack needle)
  (doc 'type (-> String String Bool))
  (doc 'description "Check if haystack contains needle substring.")
  (let ([hlen (string-length haystack)]
        [nlen (string-length needle)])
       (let loop ([i 0])
            (cond
             [(> (+ i nlen) hlen) #f]
             [(string=? (substring haystack i (+ i nlen)) needle) #t]
             [else (loop (+ i 1))]))))

(doc 'section 'git-status)

(define (git-status)
  (doc 'type (-> Void))
  (doc 'description "Show current git status.")
  (doc 'export #t)
  (system "git status")
  (void))

(define (git-diff)
  (doc 'type (-> Void))
  (doc 'description "Show uncommitted changes.")
  (doc 'export #t)
  (system "git diff")
  (void))

(define (git-log . args)
  (doc 'type (-> (Maybe Nat) Void))
  (doc 'description "Show recent commits. SECURITY: Validates count is a positive integer.")
  (doc 'export #t)
  (let ([n (if (null? args) 5 (car args))])
       (unless (and (integer? n) (> n 0) (<= n 1000))
               (error 'git-log "Count must be a positive integer <= 1000" n))
       (system (format "git log --oneline -~a" n))
       (void)))

(doc 'section 'git-commit)

(define (commit! message)
  (doc 'type (-> String Void))
  (doc 'description "Stage all changes and commit with message. SECURITY: Message is properly escaped with single quotes.")
  (doc 'export #t)
  (unless (string? message)
          (error 'commit! "Commit message must be a string" message))
  ;; Stage all changes
  (system "git add -A")
  ;; Commit with properly escaped message
  (system (format "git commit -m '~a'" (shell-escape message)))
  (display (format "Committed: ~a\n" message)))

(define (truncate-commit-title msg)
  (doc 'type (-> String String))
  (doc 'description "Get first line of commit message, truncated for title.")
  (let* ([first-line (car (string-split msg #\newline))]
         [max-len 50])
        (if (<= (string-length first-line) max-len)
            first-line
            (string-append (substring first-line 0 (- max-len 3)) "..."))))

(doc 'section 'git-push)

(define (push!)
  (doc 'type (-> Void))
  (doc 'description "Push to origin.")
  (doc 'export #t)
  (system "git push")
  (display "Pushed to origin.\n"))

(define (commit-and-push! message)
  (doc 'type (-> String Void))
  (doc 'description "Stage, commit, and push in one operation.")
  (doc 'export #t)
  (commit! message)
  (push!))

(doc 'section 'git-branch)

(define (git-branch)
  (doc 'type (-> Void))
  (doc 'description "List branches.")
  (doc 'export #t)
  (system "git branch -a")
  (void))

(define (git-checkout! branch)
  (doc 'type (-> String Void))
  (doc 'description "Switch to a branch. SECURITY: Branch name is validated to prevent injection.")
  (doc 'export #t)
  (unless (safe-branch-name? branch)
          (error 'git-checkout! "Invalid branch name" branch))
  (system (format "git checkout '~a'" (shell-escape branch)))
  (void))

(define (create-branch! branch)
  (doc 'type (-> String Void))
  (doc 'description "Create and switch to new branch. SECURITY: Branch name is validated to prevent injection.")
  (doc 'export #t)
  (unless (safe-branch-name? branch)
          (error 'create-branch! "Invalid branch name" branch))
  (system (format "git checkout -b '~a'" (shell-escape branch)))
  (void))
