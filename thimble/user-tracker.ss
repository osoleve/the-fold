;;; thimble/user-tracker.ss — User Login and Activity Tracking
;;;
;;; Tracks user login timestamps to determine when to show activity summaries.
;;; Stores data in .fold-users/ directory, persisting across daemon restarts.
;;;
;;; This is Shell code: manages mutable state (user tracking database).
;;;
;;; Dependencies:
;;;   - prelude.ss (loaded by session-manager.ss)

;;; ============================================================
;;; User Tracking Storage
;;; ============================================================

(define *user-dir* ".fold-users")

;;; In-memory cache of user data
;;; Maps username (symbol) → user-record alist
(define *user-cache* (make-eq-hashtable))

;;; ensure-user-dir! : → void
(define (ensure-user-dir!)
  (unless (file-exists? *user-dir*)
          (mkdir *user-dir*)))

;;; user-file-path : Symbol → String
(define (user-file-path username)
  (string-append *user-dir* "/" (symbol->string username) ".user"))

;;; ============================================================
;;; User Record Structure
;;; ============================================================

;;; A user record contains:
;;;   - name: Symbol (the username)
;;;   - last-login: Number (Unix timestamp of last hi call)
;;;   - total-logins: Number (count of logins)
;;;   - tiers-used: (List Symbol) (list of tiers used)

;;; make-user-record : Symbol → Alist
(define (make-user-record name)
  (let ([now (time-second (current-time))])
       (list (cons 'name name)
             (cons 'last-login now)
             (cons 'total-logins 1)
             (cons 'tiers-used '()))))

;;; ============================================================
;;; User File I/O
;;; ============================================================

;;; load-user-record : Symbol → Alist | #f
(define (load-user-record username)
  ;; Check cache first
  (or (hashtable-ref *user-cache* username #f)
      ;; Try disk
      (let ([path (user-file-path username)])
           (guard (e [else #f])
                  (and (file-exists? path)
                       (let ([data (call-with-input-file path read)])
                            (when (list? data)
                                  (hashtable-set! *user-cache* username data))
                            data))))))

;;; save-user-record! : Alist → void
(define (save-user-record! record)
  (ensure-user-dir!)
  (let* ([name (cdr (assq 'name record))]
         [path (user-file-path name)])
        ;; Update cache
        (hashtable-set! *user-cache* name record)
        ;; Persist to disk
        (call-with-output-file path
                               (lambda (p) (write record p))
                               'replace)))

;;; ============================================================
;;; User Tracking API
;;; ============================================================

;;; record-user-login! : Symbol × Symbol → Alist
;;; Record that a user logged in. Returns the user record.
;;; Updates last-login timestamp and increments login count.
(define (record-user-login! username tier)
  (let* ([now (time-second (current-time))]
         [existing (load-user-record username)]
         [record (if existing
                     ;; Update existing record
                     (let ([new-record (map (lambda (pair)
                                                    (case (car pair)
                                                          [(last-login) (cons 'last-login now)]
                                                          [(total-logins)
                                                           (cons 'total-logins (+ 1 (cdr pair)))]
                                                          [(tiers-used)
                                                           (cons 'tiers-used
                                                                 (if (memq tier (cdr pair))
                                                                     (cdr pair)
                                                                     (cons tier (cdr pair))))]
                                                          [else pair]))
                                            existing)])
                          new-record)
                     ;; Create new record
                     (let ([new-rec (make-user-record username)])
                          (set-cdr! (assq 'tiers-used new-rec) (list tier))
                          new-rec))])
        (save-user-record! record)
        record))

;;; get-user-last-login : Symbol → Number | #f
;;; Get the last login timestamp for a user.
(define (get-user-last-login username)
  (let ([record (load-user-record username)])
       (and record (cdr (assq 'last-login record)))))

;;; get-user-record : Symbol → Alist | #f
;;; Get the full user record.
(define (get-user-record username)
  (load-user-record username))

;;; user-login-recent? : Symbol × Number → Boolean
;;; Check if user logged in within the last N seconds.
(define (user-login-recent? username seconds)
  (let ([last-login (get-user-last-login username)])
       (and last-login
            (let ([now (time-second (current-time))])
                 (< (- now last-login) seconds)))))

;;; ============================================================
;;; Activity Summary
;;; ============================================================

;;; get-user-activity-summary : FS × Symbol → Alist
;;; Gather activity summary for a user.
;;; Returns:
;;;   - total-posts: Number of posts across all channels
;;;   - channels-active: List of channels user posted in
;;;   - recent-posts: List of 5 most recent posts
;;;   - last-login: Timestamp of previous login
;;;   - total-logins: Number of times logged in
;;; Note: Gracefully handles corrupted blocks in store.
(define (get-user-activity-summary fs username)
  (let ([user-record (load-user-record username)])
       ;; Try to collect posts, but handle errors gracefully
       (guard (e [else
                  ;; If there's an error reading posts, return basic info
                  `((total-posts . 0)
                    (channels-active . ())
                    (recent-posts . ())
                    (last-login . ,(and user-record (cdr (assq 'last-login user-record))))
                    (total-logins . ,(if user-record (cdr (assq 'total-logins user-record)) 0)))])
              (let* ([channels (list-channels fs)]
                     [all-posts '()]
                     [active-channels '()])
                    ;; Collect posts from each channel
                    (for-each
                     (lambda (channel)
                             (guard (e [else #f])  ; Skip channels with errors
                                    (let ([posts (posts-by-author fs channel username)])
                                         (when (not (null? posts))
                                               (set! active-channels (cons channel active-channels))
                                               (set! all-posts (append posts all-posts))))))
                     channels)
                    ;; Sort by timestamp (newest first)
                    (let* ([sorted-posts (list-sort
                                          (lambda (a b)
                                                  (string>? (cdr (assq 'timestamp a))
                                                            (cdr (assq 'timestamp b))))
                                          all-posts)]
                           [recent (if (> (length sorted-posts) 5)
                                       (list-head sorted-posts 5)
                                       sorted-posts)])
                          `((total-posts . ,(length all-posts))
                            (channels-active . ,(reverse active-channels))
                            (recent-posts . ,recent)
                            (last-login . ,(and user-record (cdr (assq 'last-login user-record))))
                            (total-logins . ,(if user-record (cdr (assq 'total-logins user-record)) 0))))))))

;;; list-head : List × Nat → List
;;; Take first n elements of a list.
(define (list-head lst n)
  (if (or (null? lst) (= n 0))
      '()
      (cons (car lst) (list-head (cdr lst) (- n 1)))))

;;; ============================================================
;;; Display Activity Summary
;;; ============================================================

;;; display-activity-summary : Alist → void
;;; Display a formatted activity summary.
(define (display-activity-summary summary username)
  (let ([total-posts (cdr (assq 'total-posts summary))]
        [channels (cdr (assq 'channels-active summary))]
        [recent (cdr (assq 'recent-posts summary))]
        [logins (cdr (assq 'total-logins summary))])
       
       (display "\n")
       (display "  ┌────────────────────────────────────────────────────────────────┐\n")
       (display (format "  │ Welcome back, ~a!~a│\n"
                        username
                        (make-string (max 0 (- 48 (string-length (symbol->string username)))) #\space)))
       (display "  └────────────────────────────────────────────────────────────────┘\n")
       (display "\n")
       
       (display (format "  Sessions: ~a total logins\n" logins))
       (display (format "  Activity: ~a posts across ~a channel~a\n"
                        total-posts
                        (length channels)
                        (if (= (length channels) 1) "" "s")))
       
       (when (not (null? channels))
             (display (format "  Channels: ~a\n"
                              (apply string-append
                                     (let loop ([chs channels] [first #t])
                                          (if (null? chs)
                                              '()
                                              (cons (format "~a~a"
                                                            (if first "" ", ")
                                                            (car chs))
                                                    (loop (cdr chs) #f))))))))
       
       (when (not (null? recent))
             (display "\n")
             (display "  Recent activity:\n")
             (for-each
              (lambda (post)
                      (let ([channel (cdr (assq 'channel post))]
                            [timestamp (cdr (assq 'timestamp post))]
                            [body (cdr (assq 'body post))])
                           ;; Truncate body for display
                           (let ([preview (if (> (string-length body) 50)
                                              (string-append (substring body 0 47) "...")
                                              body)])
                                (display (format "    • [~a] ~a\n" channel preview)))))
              recent))
       (display "\n")))
