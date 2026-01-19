;;; boundary/bbs/bbs.ss — BBS Entry Point
;;;
;;; CAS-native bulletin board system for issue tracking and posts.
;;; Replaces beads with a native implementation using The Fold's CAS.
;;;
;;; Usage:
;;;   (load "boundary/bbs/bbs.ss")
;;;   (bbs-init!)
;;;
;;;   ;; Issues
;;;   (bbs-create "Issue title" 'priority 2 'type 'task)
;;;   (bbs-show 'fold-001)
;;;   (bbs-list)
;;;   (bbs-update 'fold-001 'status 'in_progress)
;;;   (bbs-close 'fold-001 'reason "Done!")
;;;
;;;   ;; Posts (changelogs, notes, announcements)
;;;   (post-create "Title" "Body..." 'changelog)
;;;   (post-show 'post-1)
;;;   (post-list)
;;;
;;; This is Shell code: loads all BBS modules.

(load "boundary/bbs/ops.ss")
(load "boundary/bbs/posts.ss")

;;; ====
;;; Initialization
;;; ====

;;; *bbs-initialized* : Boolean
;;; Track whether BBS has been initialized.
(define *bbs-initialized* #f)

;;; *bbs-posts-initialized* : Boolean
;;; Track whether post index has been initialized.
(define *bbs-posts-initialized* #f)

;;; bbs-init! : -> Int
;;; Initialize the BBS system (issues and posts).
;;; Tries cached index first, rebuilds only if cache is stale.
(define (bbs-init!)
  (if *bbs-initialized*
      (hashtable-size (let () *bbs-by-status*))  ; Return count without reinit
      (begin
        (printf "Initializing BBS...~n")
        ;; Initialize issues
        (let ([count (if (bbs-load-index-cache!)
                         (begin
                           (printf "  (issues loaded from cache)~n")
                           (bbs-issue-count))
                         (bbs-rebuild-indices!))])
          (set! *bbs-initialized* #t)
          (printf "  Loaded ~a issues~n" count)
          ;; Initialize posts
          (bbs-init-posts!)
          count))))

;;; bbs-init-posts! : -> Int
;;; Initialize the post index.
;;; Tries cached index first, rebuilds only if cache is stale.
(define (bbs-init-posts!)
  (if *bbs-posts-initialized*
      (post-index-count)  ; Return count without reinit
      (let ([count (if (post-load-index-cache!)
                       (begin
                         (printf "  (posts loaded from cache)~n")
                         (post-index-count))
                       (post-rebuild-indices!))])
        (set! *bbs-posts-initialized* #t)
        (printf "  Loaded ~a posts~n" count)
        count)))

;;; bbs-init-posts-quiet! : -> Int
;;; Initialize post index silently (for startup).
(define (bbs-init-posts-quiet!)
  (unless *bbs-posts-initialized*
    (unless (post-load-index-cache!)
      (post-rebuild-indices!))
    (set! *bbs-posts-initialized* #t))
  (post-index-count))

;;; bbs-init-quiet! : -> Int
;;; Initialize BBS silently (for startup).
;;; Tries cached index first for fast startup.
(define (bbs-init-quiet!)
  (unless *bbs-initialized*
    (unless (bbs-load-index-cache!)
      (bbs-rebuild-indices!))
    (set! *bbs-initialized* #t))
  ;; Also init posts silently
  (bbs-init-posts-quiet!)
  (hashtable-size *bbs-by-status*))

;;; ====
;;; Display Functions
;;; ====

;;; bbs-show : String | Symbol -> Void
;;; Display an issue.
(define (bbs-show id)
  (let* ([id-str (normalize-id id)]
         [data (bbs-fetch-issue-data id-str)])
    (if data
        (begin
          (printf "~a~n" (make-string 60 #\-))
          (printf "ID:          ~a~n" (cdr (assq 'id data)))
          (printf "Title:       ~a~n" (cdr (assq 'title data)))
          (printf "Status:      ~a~n" (cdr (assq 'status data)))
          (printf "Priority:    P~a~n" (cdr (assq 'priority data)))
          (printf "Type:        ~a~n" (cdr (assq 'type data)))
          (let ([labels (cdr (assq 'labels data))])
            (unless (null? labels)
              (printf "Labels:      ~a~n" labels)))
          (printf "Created:     ~a~n" (cdr (assq 'created-at data)))
          (printf "Created by:  ~a~n" (cdr (assq 'created-by data)))
          (printf "Version:     ~a~n" (cdr (assq 'version data)))
          (let ([desc (cdr (assq 'description data))])
            (unless (string=? desc "")
              (printf "~nDescription:~n~a~n" desc)))
          ;; Show blockers with status
          (let ([blockers (bbs-blockers-with-status id-str)])
            (unless (null? blockers)
              (printf "~nBlocked by:  ")
              (for-each
               (lambda (b)
                 (let ([id (car b)] [status (cdr b)])
                   (case status
                     [(open in_progress) (printf "~a " id)]
                     [(closed) (printf "~a[closed] " id)]
                     [(missing) (printf "~a[missing] " id)])))
               blockers)
              (printf "~n")))
          ;; Show blocking
          (let ([blocking (bbs-blocking id-str)])
            (unless (null? blocking)
              (printf "Blocks:      ~a~n" blocking)))
          (printf "~a~n" (make-string 60 #\-)))
        (printf "Issue not found: ~a~n" id-str))))

;;; bbs-list : -> Void
;;; List issues.
;;;
;;; Keyword arguments:
;;;   'status - Filter by status (default: show all open)
;;;   'limit - Maximum number to show (default: 20)
(define (bbs-list . args)
  (let* ([status (get-keyword-arg args 'status 'open)]
         [limit (get-keyword-arg args 'limit 20)]
         [ids (if (eq? status 'all)
                  (bbs-all-ids)
                  (bbs-issues-by-status status))]
         [to-show (take-n limit ids)])
    (printf "~a issues (~a status)~n" (length ids) status)
    (printf "~a~n" (make-string 60 #\-))
    (for-each
     (lambda (id)
       (let ([data (bbs-fetch-issue-data id)])
         (when data
           (printf "~a  P~a  [~a]  ~a~n"
                   (pad-right id 12)
                   (cdr (assq 'priority data))
                   (pad-right (symbol->string (cdr (assq 'status data))) 11)
                   (truncate-str (cdr (assq 'title data)) 40)))))
     to-show)
    (when (> (length ids) limit)
      (printf "... and ~a more~n" (- (length ids) limit)))))

;;; bbs-ready : -> Void
;;; Show issues ready to work on (open and not blocked).
(define (bbs-ready . args)
  (let* ([limit (get-keyword-arg args 'limit 20)]
         [ids (bbs-ready-issues)]
         [to-show (take-n limit ids)])
    (printf "~a ready issues~n" (length ids))
    (printf "~a~n" (make-string 60 #\-))
    (for-each
     (lambda (id)
       (let ([data (bbs-fetch-issue-data id)])
         (when data
           (printf "~a  P~a  ~a~n"
                   (pad-right id 12)
                   (cdr (assq 'priority data))
                   (truncate-str (cdr (assq 'title data)) 45)))))
     to-show)
    (when (> (length ids) limit)
      (printf "... and ~a more~n" (- (length ids) limit)))))

;;; bbs-blocked : -> Void
;;; Show blocked issues.
(define (bbs-blocked)
  (let ([ids (bbs-blocked-issues)])
    (printf "~a blocked issues~n" (length ids))
    (printf "~a~n" (make-string 60 #\-))
    (for-each
     (lambda (id)
       (let ([data (bbs-fetch-issue-data id)]
             [blockers (bbs-blockers id)])
         (when data
           (printf "~a  ~a~n" id (cdr (assq 'title data)))
           (printf "         blocked by: ~a~n" blockers))))
     ids)))

;;; bbs-history : String | Symbol -> Void
;;; Show version history of an issue.
(define (bbs-history id)
  (let* ([id-str (normalize-id id)]
         [history (bbs-issue-history-data id-str)])
    (if (null? history)
        (printf "Issue not found: ~a~n" id-str)
        (begin
          (printf "History for ~a (~a versions)~n" id-str (length history))
          (printf "~a~n" (make-string 60 #\-))
          (for-each
           (lambda (data)
             (printf "v~a  [~a]  P~a  ~a~n"
                     (cdr (assq 'version data))
                     (cdr (assq 'status data))
                     (cdr (assq 'priority data))
                     (truncate-str (cdr (assq 'title data)) 40)))
           (reverse history))))))

;;; bbs-find : String -> Void
;;; Simple substring search in issue titles.
(define (bbs-find query)
  (let* ([query-lower (string-downcase query)]
         [matches (filter
                   (lambda (id)
                     (let ([data (bbs-fetch-issue-data id)])
                       (and data
                            (string-contains-ci?
                             (cdr (assq 'title data))
                             query-lower))))
                   (bbs-all-ids))])
    (printf "~a matches for '~a'~n" (length matches) query)
    (printf "~a~n" (make-string 60 #\-))
    (for-each
     (lambda (id)
       (let ([data (bbs-fetch-issue-data id)])
         (when data
           (printf "~a  [~a]  ~a~n"
                   (pad-right id 12)
                   (pad-right (symbol->string (cdr (assq 'status data))) 11)
                   (cdr (assq 'title data))))))
     (take-n 20 matches))))

;;; bbs-find-exact : String | Symbol -> Void
;;; Look up an issue by exact ID.
(define (bbs-find-exact id)
  (bbs-show id))

;;; ====
;;; Convenience Aliases
;;; ====

;;; bbs-add-blocker : String|Symbol String|Symbol -> Void
;;; Add a dependency where blocker blocks blocked.
;;; Alias for bbs-dep with clearer naming.
;;; Example: (bbs-add-blocker 'fold-001 'fold-002) means fold-001 blocks fold-002
(define (bbs-add-blocker blocker blocked)
  (bbs-dep blocker blocked))

;;; bbs-remove-blocker : String|Symbol String|Symbol -> Void
;;; Remove a dependency.
;;; Alias for bbs-undep with clearer naming.
(define (bbs-remove-blocker blocker blocked)
  (bbs-undep blocker blocked))

;;; bbs-gc : -> Void
;;; Garbage collect stale dependencies (where either issue is missing).
(define (bbs-gc)
  (let ([removed (bbs-gc-deps!)])
    (if (null? removed)
        (printf "No stale dependencies found.~n")
        (begin
          (printf "Removed ~a stale dependencies:~n" (length removed))
          (for-each
           (lambda (d)
             (printf "  ~a -> ~a~n" (car d) (cdr d)))
           removed)))))

;;; ====
;;; Label and Type Filtering
;;; ====

;;; bbs-by-label : Symbol -> (List String)
;;; Get issue IDs that have a specific label.
;;; Returns list of IDs (not displayed).
(define (bbs-by-label label)
  (filter
   (lambda (id)
     (let ([data (bbs-fetch-issue-data id)])
       (and data
            (let ([labels (cdr (assq 'labels data))])
              (memq label labels)))))
   (bbs-all-ids)))

;;; bbs-list-by-label : Symbol -> Void
;;; List issues with a specific label.
(define (bbs-list-by-label label . args)
  (let* ([limit (get-keyword-arg args 'limit 20)]
         [ids (bbs-by-label label)]
         [to-show (take-n limit ids)])
    (printf "~a issues with label '~a'~n" (length ids) label)
    (printf "~a~n" (make-string 60 #\-))
    (for-each
     (lambda (id)
       (let ([data (bbs-fetch-issue-data id)])
         (when data
           (printf "~a  P~a  [~a]  ~a~n"
                   (pad-right id 12)
                   (cdr (assq 'priority data))
                   (pad-right (symbol->string (cdr (assq 'status data))) 11)
                   (truncate-str (cdr (assq 'title data)) 35)))))
     to-show)
    (when (> (length ids) limit)
      (printf "... and ~a more~n" (- (length ids) limit)))))

;;; bbs-by-type : Symbol -> (List String)
;;; Get issue IDs of a specific type (task, bug, feature, epic).
(define (bbs-by-type type)
  (filter
   (lambda (id)
     (let ([data (bbs-fetch-issue-data id)])
       (and data
            (eq? (cdr (assq 'type data)) type))))
   (bbs-all-ids)))

;;; bbs-list-by-type : Symbol -> Void
;;; List issues of a specific type.
(define (bbs-list-by-type type . args)
  (let* ([limit (get-keyword-arg args 'limit 20)]
         [ids (bbs-by-type type)]
         [to-show (take-n limit ids)])
    (printf "~a ~a issues~n" (length ids) type)
    (printf "~a~n" (make-string 60 #\-))
    (for-each
     (lambda (id)
       (let ([data (bbs-fetch-issue-data id)])
         (when data
           (printf "~a  P~a  [~a]  ~a~n"
                   (pad-right id 12)
                   (cdr (assq 'priority data))
                   (pad-right (symbol->string (cdr (assq 'status data))) 11)
                   (truncate-str (cdr (assq 'title data)) 35)))))
     to-show)
    (when (> (length ids) limit)
      (printf "... and ~a more~n" (- (length ids) limit)))))

;;; ====
;;; Enhanced Search
;;; ====

;;; bbs-search : String -> Void
;;; Search issues by title AND description (case-insensitive).
;;; More comprehensive than bbs-find which only searches titles.
(define (bbs-search query . args)
  (let* ([limit (get-keyword-arg args 'limit 20)]
         [query-lower (string-downcase query)]
         [matches (filter
                   (lambda (id)
                     (let ([data (bbs-fetch-issue-data id)])
                       (and data
                            (or (string-contains-ci?
                                 (cdr (assq 'title data))
                                 query-lower)
                                (string-contains-ci?
                                 (cdr (assq 'description data))
                                 query-lower)))))
                   (bbs-all-ids))]
         [to-show (take-n limit matches)])
    (printf "~a matches for '~a' (title+description)~n" (length matches) query)
    (printf "~a~n" (make-string 60 #\-))
    (for-each
     (lambda (id)
       (let ([data (bbs-fetch-issue-data id)])
         (when data
           (printf "~a  [~a]  ~a~n"
                   (pad-right id 12)
                   (pad-right (symbol->string (cdr (assq 'status data))) 11)
                   (truncate-str (cdr (assq 'title data)) 40)))))
     to-show)
    (when (> (length matches) limit)
      (printf "... and ~a more~n" (- (length matches) limit)))))

;;; bbs-labels : -> (List Symbol)
;;; Get all unique labels in use across all issues.
(define (bbs-labels)
  (let ([all-labels '()])
    (for-each
     (lambda (id)
       (let ([data (bbs-fetch-issue-data id)])
         (when data
           (let ([labels (cdr (assq 'labels data))])
             (for-each
              (lambda (label)
                (unless (memq label all-labels)
                  (set! all-labels (cons label all-labels))))
              labels)))))
     (bbs-all-ids))
    (sort (lambda (a b)
            (string<? (symbol->string a) (symbol->string b)))
          all-labels)))

;;; bbs-label-report : -> Void
;;; Show all labels and their issue counts.
(define (bbs-label-report)
  (let ([labels (bbs-labels)])
    (printf "~a labels in use~n" (length labels))
    (printf "~a~n" (make-string 40 #\-))
    (for-each
     (lambda (label)
       (let ([count (length (bbs-by-label label))])
         (printf "  ~a: ~a~n" (pad-right (symbol->string label) 20) count)))
     labels)))

;;; ====
;;; String Utilities
;;; ====

;;; take-n : Int (List a) -> (List a)
(define (take-n n lst)
  (if (or (<= n 0) (null? lst))
      '()
      (cons (car lst) (take-n (- n 1) (cdr lst)))))

;;; pad-right : String Int -> String
(define (pad-right str width)
  (let ([len (string-length str)])
    (if (>= len width)
        str
        (string-append str (make-string (- width len) #\space)))))

;;; truncate-str : String Int -> String
(define (truncate-str str max-len)
  (if (<= (string-length str) max-len)
      str
      (string-append (substring str 0 (- max-len 3)) "...")))

;;; string-contains-ci? : String String -> Boolean
;;; Case-insensitive substring search.
(define (string-contains-ci? haystack needle)
  (let ([h-len (string-length haystack)]
        [n-len (string-length needle)])
    (and (<= n-len h-len)
         (let loop ([i 0])
           (cond
            [(> (+ i n-len) h-len) #f]
            [(string-prefix-ci? haystack i needle) #t]
            [else (loop (+ i 1))])))))

;;; string-prefix-ci? : String Int String -> Boolean
(define (string-prefix-ci? str start prefix)
  (let ([p-len (string-length prefix)])
    (let loop ([i 0])
      (cond
       [(>= i p-len) #t]
       [(>= (+ start i) (string-length str)) #f]
       [(char-ci=? (string-ref str (+ start i))
                   (string-ref prefix i))
        (loop (+ i 1))]
       [else #f]))))

;;; string-downcase : String -> String
(define (string-downcase str)
  (list->string
   (map char-downcase (string->list str))))


;;; ====
;;; Print Help
;;; ====

(printf "bbs.ss loaded.~n")
(printf "  (bbs-init!)                     - Initialize BBS~n")
(printf "  (bbs-create \"title\" ...)        - Create issue~n")
(printf "  (bbs-show 'id)                  - Show issue~n")
(printf "  (bbs-list)                      - List open issues~n")
(printf "  (bbs-list 'status 'closed)      - List closed issues~n")
(printf "  (bbs-update 'id 'status 'done)  - Update issue~n")
(printf "  (bbs-close 'id 'reason \"...\")   - Close issue~n")
(printf "  (bbs-dep 'blocker 'blocked)     - Add dependency~n")
(printf "  (bbs-ready)                     - Show ready issues~n")
(printf "  (bbs-blocked)                   - Show blocked issues~n")
(printf "  (bbs-find \"query\")              - Search issue titles~n")
(printf "  (bbs-search \"query\")            - Search titles + descriptions~n")
(printf "  (bbs-history 'id)               - Show version history~n")
(printf "  (bbs-list-by-label 'label)      - Filter by label~n")
(printf "  (bbs-list-by-type 'type)        - Filter by type~n")
(printf "  (bbs-label-report)              - Show all labels~n")
