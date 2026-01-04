;;; forum/coordination.ss — Agent Coordination Primitives
;;;
;;; Provides tools for agent-to-agent coordination:
;;; - Work claims (who's working on what)
;;; - Notification queue (@mention delivery)
;;; - Quick reactions (ack/question/done)
;;; - Status tracking
;;;
;;; This is Shell code: uses IO, manages state files.
;;;
;;; Dependencies (must be loaded before this file):
;;;   core/block.ss
;;;   core/sha256.ss
;;;   shell/fs.ss
;;;   forum/chat.ss

;;; ============================================================
;;; Work Claims System
;;; ============================================================

(define *work-claims-file* ".fold-repl/work-claims.json")
(define *notifications-dir* ".fold-repl/notifications")

;;; ensure-notifications-dir! : → void
(define (ensure-notifications-dir!)
  (unless (file-exists? *notifications-dir*)
          (mkdir *notifications-dir*)))

;;; read-work-claims : → Alist
;;; Read current work claims from disk.
;;; Returns alist: ((item . "beads-042") (agent . "Praxis") (claimed-at . "timestamp"))
(define (read-work-claims)
  (if (file-exists? *work-claims-file*)
      (call-with-input-file *work-claims-file* read)
      '()))

;;; write-work-claims! : Alist → void
(define (write-work-claims! claims)
  (let ([dir (string-append (path-parent *work-claims-file*))])
       (unless (file-exists? dir)
               (mkdir dir)))
  (call-with-output-file *work-claims-file*
                         (lambda (port)
                                 (write claims port)
                                 (newline port))
                         'replace))

;;; claim-work : String → void
;;; Claim work on an item (typically a beads ID).
;;; Announces to chat and updates work claims file.
;;;
;;; Example: (claim-work "beads-042")
(define (claim-work item)
  (let* ([session (read-session)]
         [_ (unless session
                    (error 'claim-work "No active session. Use (hi tier name txt) first."))]
         [agent (cdr (assq 'name session))]
         [claims (read-work-claims)]
         [existing (assoc item claims)]
         [_ (when existing
                  (let ([current-agent (cdr (assq 'agent (cdr existing)))])
                       (unless (eq? current-agent agent)
                               (display (format "Warning: ~a already claimed by ~a\n"
                                                item current-agent)))))]
         [timestamp (current-timestamp)]
         [new-claim `((item . ,item)
                      (agent . ,agent)
                      (claimed-at . ,timestamp))]
         [updated-claims (cons (cons item new-claim)
                               (filter (lambda (c) (not (equal? (car c) item)))
                                       claims))])
        (write-work-claims! updated-claims)
        ;; Announce to chat
        (chat (format "📋 Claiming work on ~a" item))
        (display (format "✓ Claimed ~a\n" item))))

;;; release-work : String → void
;;; Release a work claim.
;;;
;;; Example: (release-work "beads-042")
(define (release-work item)
  (let* ([session (read-session)]
         [_ (unless session
                    (error 'release-work "No active session. Use (hi tier name txt) first."))]
         [agent (cdr (assq 'name session))]
         [claims (read-work-claims)]
         [existing (assoc item claims)])
        (if existing
            (let ([claimed-by (cdr (assq 'agent (cdr existing)))])
                 (if (eq? claimed-by agent)
                     (begin
                      (write-work-claims! (filter (lambda (c) (not (equal? (car c) item)))
                                                  claims))
                      (chat (format "✅ Released work on ~a" item))
                      (display (format "✓ Released ~a\n" item)))
                     (display (format "Error: ~a is claimed by ~a, not you\n"
                                      item claimed-by))))
            (display (format "Warning: ~a is not currently claimed\n" item)))))

;;; who-is-working-on : String → void
;;; Query who is working on an item or topic.
;;;
;;; Example: (who-is-working-on "type system")
;;;          (who-is-working-on "beads-042")
(define (who-is-working-on query)
  (let ([claims (read-work-claims)])
       (if (null? claims)
           (display "No active work claims.\n")
           (let ([matches (filter (lambda (c)
                                          (let ([item (cdr (assq 'item (cdr c)))])
                                               (string-contains? item query)))
                                  claims)])
                (if (null? matches)
                    (display (format "No claims matching '~a'\n" query))
                    (begin
                     (display (format "Work claims matching '~a':\n" query))
                     (for-each
                      (lambda (claim)
                              (let* ([data (cdr claim)]
                                     [item (cdr (assq 'item data))]
                                     [agent (cdr (assq 'agent data))]
                                     [time (cdr (assq 'claimed-at data))])
                                    (display (format "  • ~a → ~a (since ~a)\n"
                                                     item agent time))))
                      matches)))))))

;;; show-all-claims : → void
;;; Display all active work claims.
(define (show-all-claims)
  (let ([claims (read-work-claims)])
       (if (null? claims)
           (display "No active work claims.\n")
           (begin
            (display "Active work claims:\n")
            (for-each
             (lambda (claim)
                     (let* ([data (cdr claim)]
                            [item (cdr (assq 'item data))]
                            [agent (cdr (assq 'agent data))]
                            [time (cdr (assq 'claimed-at data))])
                           (display (format "  • ~a → ~a (since ~a)\n"
                                            item agent time))))
             claims)))))

;;; string-contains? : String × String → Boolean
(define (string-contains? str substr)
  (let ([str-len (string-length str)]
        [sub-len (string-length substr)])
       (let loop ([i 0])
            (cond
             [(> (+ i sub-len) str-len) #f]
             [(string=? (substring str i (+ i sub-len)) substr) #t]
             [else (loop (+ i 1))]))))

;;; path-parent : String → String
;;; Get parent directory path.
(define (path-parent path)
  (let ([parts (string-split path #\/)])
       (if (<= (length parts) 1)
           "."
           (string-join (reverse (cdr (reverse parts))) "/"))))

;;; string-join : (List String) × String → String
(define (string-join lst sep)
  (if (null? lst)
      ""
      (if (null? (cdr lst))
          (car lst)
          (string-append (car lst) sep (string-join (cdr lst) sep)))))

;;; string-split : String × Char → (List String)
(define (string-split str delim)
  (let loop ([chars (string->list str)]
             [current '()]
             [result '()])
       (cond
        [(null? chars)
         (reverse (if (null? current)
                      result
                      (cons (list->string (reverse current)) result)))]
        [(char=? (car chars) delim)
         (loop (cdr chars)
               '()
               (if (null? current)
                   result
                   (cons (list->string (reverse current)) result)))]
        [else
         (loop (cdr chars)
               (cons (car chars) current)
               result)])))

;;; ============================================================
;;; Notification Queue
;;; ============================================================

;;; notify-user! : String × String × String × String → void
;;; Write a notification for a user.
;;; Agent: who mentioned them
;;; Context: the message or topic
;;; Message-hash: reference to the chat message
(define (notify-user! username agent context message-hash)
  (ensure-notifications-dir!)
  (let* ([user-file (format "~a/~a.sexp" *notifications-dir* username)]
         [existing (if (file-exists? user-file)
                       (call-with-input-file user-file read)
                       '())]
         [notification `((from . ,agent)
                         (context . ,context)
                         (message-hash . ,message-hash)
                         (timestamp . ,(current-timestamp)))]
         [updated (cons notification existing)])
        (call-with-output-file user-file
                               (lambda (port)
                                       (write updated port)
                                       (newline port))
                               'replace)))

;;; read-notifications : → (List Alist)
;;; Read notifications for current user.
(define (read-notifications)
  (let ([session (read-session)])
       (if (not session)
           '()
           (let* ([username (symbol->string (cdr (assq 'name session)))]
                  [user-file (format "~a/~a.sexp" *notifications-dir* username)])
                 (if (file-exists? user-file)
                     (call-with-input-file user-file read)
                     '())))))

;;; show-notifications : → void
;;; Display notifications for current user.
(define (show-notifications)
  (let ([notifs (read-notifications)])
       (if (null? notifs)
           (display "No new notifications.\n")
           (begin
            (display (format "You have ~a notification~a:\n"
                             (length notifs)
                             (if (= (length notifs) 1) "" "s")))
            (for-each
             (lambda (n)
                     (let ([from (cdr (assq 'from n))]
                           [context (cdr (assq 'context n))]
                           [hash (cdr (assq 'message-hash n))]
                           [time (cdr (assq 'timestamp n))])
                          (display (format "  📬 ~a mentioned you (~a)\n" from time))
                          (display (format "     \"~a\"\n"
                                           (truncate-string context 60)))
                          (display (format "     [~a]\n" (substring hash 0 6)))))
             (reverse notifs))))))  ; Show oldest first

;;; clear-notifications : → void
;;; Clear all notifications for current user.
(define (clear-notifications)
  (let ([session (read-session)])
       (when session
             (let* ([username (symbol->string (cdr (assq 'name session)))]
                    [user-file (format "~a/~a.sexp" *notifications-dir* username)])
                   (when (file-exists? user-file)
                         (delete-file user-file)
                         (display "Notifications cleared.\n"))))))

;;; ============================================================
;;; Quick Reactions
;;; ============================================================

;;; react : String × Symbol → Bytevector
;;; Add a quick reaction to a message.
;;; Hash-prefix: the message to react to
;;; Reaction: 'ack, 'question, 'done, 'thanks, 'help
;;;
;;; Example: (react "abc123" 'ack)
(define (react hash-prefix reaction)
  (let ([session (read-session)])
       (unless session
               (error 'react "No active session. Use (hi tier name txt) first."))
       
       (unless (memq reaction '(ack question done thanks help))
               (error 'react "Invalid reaction. Use 'ack, 'question, 'done, 'thanks, or 'help"))
       
       (let* ([author (cdr (assq 'name session))]
              [tier (cdr (assq 'tier session))]
              [fs (mint-fs-capability ".store")]
              [reaction-symbol (reaction->symbol reaction)]
              [full-meta `((author . ,author)
                           (tier . ,tier)
                           (timestamp . ,(current-timestamp))
                           (channel . chat)
                           (reaction . ,reaction)
                           (target-hash . ,hash-prefix)
                           (body . ,reaction-symbol))]
              [prev-head (fs-read-head fs 'chat)]
              [refs (if prev-head (list prev-head) '())]
              [blk (make-post-block full-meta refs)]
              [hash (fs-store! fs blk)])
             (fs-write-head! fs 'chat hash)
             (fs-pin! fs hash)
             (display (format "~a ~a [~a]\n" reaction-symbol hash-prefix author))
             hash)))

;;; reaction->symbol : Symbol → String
(define (reaction->symbol r)
  (case r
        [(ack) "✓"]
        [(question) "?"]
        [(done) "✅"]
        [(thanks) "🙏"]
        [(help) "🆘"]
        [else "•"]))

;;; ============================================================
;;; Exports
;;; ============================================================

;; All functions are implicitly exported in this file
