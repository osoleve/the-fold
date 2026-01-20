(load "boundary/bbs/ops.ss")
(load "boundary/bbs/posts.ss")

(doc 'module 'bbs)
(doc 'description "BBS Entry Point - CAS-native bulletin board system for issue tracking and posts")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'note "Usage:
  (bbs-init!)

  ;; Issues
  (bbs-create \"Issue title\" 'priority 2 'type 'task)
  (bbs-show 'fold-001)
  (bbs-list)
  (bbs-update 'fold-001 'status 'in_progress)
  (bbs-close 'fold-001 'reason \"Done!\")

  ;; Posts (changelogs, notes, announcements)
  (post-create \"Title\" \"Body...\" 'changelog)
  (post-show 'post-1)
  (post-list)")

(doc 'section 'initialization)

(doc *bbs-initialized* 'type 'Boolean)
(doc *bbs-initialized* 'description "Track whether BBS has been initialized")
(define *bbs-initialized* #f)

(doc *bbs-posts-initialized* 'type 'Boolean)
(doc *bbs-posts-initialized* 'description "Track whether post index has been initialized")
(define *bbs-posts-initialized* #f)

(define (bbs-init!)
  (doc 'type (-> Int))
  (doc 'description "Initialize the BBS system (issues and posts)")
  (doc 'note "Tries cached index first, rebuilds only if cache is stale")
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

(define (bbs-init-posts!)
  (doc 'type (-> Int))
  (doc 'description "Initialize the post index")
  (doc 'note "Tries cached index first, rebuilds only if cache is stale")
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

(define (bbs-init-posts-quiet!)
  (doc 'type (-> Int))
  (doc 'description "Initialize post index silently (for startup)")
  (unless *bbs-posts-initialized*
    (unless (post-load-index-cache!)
      (post-rebuild-indices!))
    (set! *bbs-posts-initialized* #t))
  (post-index-count))

(define (bbs-init-quiet!)
  (doc 'type (-> Int))
  (doc 'description "Initialize BBS silently (for startup)")
  (doc 'note "Tries cached index first for fast startup")
  (unless *bbs-initialized*
    (unless (bbs-load-index-cache!)
      (bbs-rebuild-indices!))
    (set! *bbs-initialized* #t))
  ;; Also init posts silently
  (bbs-init-posts-quiet!)
  (hashtable-size *bbs-by-status*))

(doc 'section 'display-functions)

(define (bbs-show id)
  (doc 'type (-> (Or String Symbol) Void))
  (doc 'description "Display an issue")
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

(define (bbs-list . args)
  (doc 'type (-> Void))
  (doc 'description "List issues")
  (doc 'param 'status "Filter by status (default: show all open)")
  (doc 'param 'limit "Maximum number to show (default: 20)")
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

(define (bbs-ready . args)
  (doc 'type (-> Void))
  (doc 'description "Show issues ready to work on (open and not blocked)")
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

(define (bbs-blocked)
  (doc 'type (-> Void))
  (doc 'description "Show blocked issues")
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

(define (bbs-history id)
  (doc 'type (-> (Or String Symbol) Void))
  (doc 'description "Show version history of an issue")
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

(define (bbs-show-blockers id)
  (doc 'type (-> (Or String Symbol) Void))
  (doc 'description "Show blockers for an issue with their current status")
  (let* ([id-str (normalize-id id)]
         [blockers (bbs-blockers-with-status id-str)])
    (if (null? blockers)
        (printf "~a has no blockers~n" id-str)
        (begin
          (printf "Blockers for ~a:~n" id-str)
          (for-each
           (lambda (b)
             (let* ([blocker-id (car b)]
                    [status (cdr b)]
                    [status-str (case status
                                  [(open) "[open]"]
                                  [(in_progress) "[in progress]"]
                                  [(closed) "[closed]"]
                                  [(missing) "[not found]"]
                                  [else (format "[~a]" status)])])
               (printf "  ~a  ~a~n" (pad-right blocker-id 14) status-str)))
           blockers)))))

(define (bbs-find query)
  (doc 'type (-> String Void))
  (doc 'description "Simple substring search in issue titles")
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

(define (bbs-find-exact id)
  (doc 'type (-> (Or String Symbol) Void))
  (doc 'description "Look up an issue by exact ID")
  (bbs-show id))

(doc 'section 'convenience-aliases)

(define (bbs-add-blocker blocker blocked)
  (doc 'type (-> (Or String Symbol) (Or String Symbol) Void))
  (doc 'description "Add a dependency where blocker blocks blocked")
  (doc 'note "Alias for bbs-dep with clearer naming")
  (doc 'example "(bbs-add-blocker 'fold-001 'fold-002) means fold-001 blocks fold-002")
  (bbs-dep blocker blocked))

(define (bbs-remove-blocker blocker blocked)
  (doc 'type (-> (Or String Symbol) (Or String Symbol) Void))
  (doc 'description "Remove a dependency")
  (doc 'note "Alias for bbs-undep with clearer naming")
  (bbs-undep blocker blocked))

(define (bbs-gc)
  (doc 'type (-> Void))
  (doc 'description "Garbage collect stale dependencies (where either issue is missing)")
  (let ([removed (bbs-gc-deps!)])
    (if (null? removed)
        (printf "No stale dependencies found.~n")
        (begin
          (printf "Removed ~a stale dependencies:~n" (length removed))
          (for-each
           (lambda (d)
             (printf "  ~a -> ~a~n" (car d) (cdr d)))
           removed)))))

(doc 'section 'label-and-type-filtering)

(define (bbs-by-label label)
  (doc 'type (-> Symbol (List String)))
  (doc 'description "Get issue IDs that have a specific label")
  (doc 'returns "List of IDs (not displayed)")
  (filter
   (lambda (id)
     (let ([data (bbs-fetch-issue-data id)])
       (and data
            (let ([labels (cdr (assq 'labels data))])
              (memq label labels)))))
   (bbs-all-ids)))

(define (bbs-list-by-label label . args)
  (doc 'type (-> Symbol Void))
  (doc 'description "List issues with a specific label")
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

(define (bbs-by-type type)
  (doc 'type (-> Symbol (List String)))
  (doc 'description "Get issue IDs of a specific type (task, bug, feature, epic)")
  (filter
   (lambda (id)
     (let ([data (bbs-fetch-issue-data id)])
       (and data
            (eq? (cdr (assq 'type data)) type))))
   (bbs-all-ids)))

(define (bbs-list-by-type type . args)
  (doc 'type (-> Symbol Void))
  (doc 'description "List issues of a specific type")
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

(doc 'section 'enhanced-search)

(define (bbs-search query . args)
  (doc 'type (-> String Void))
  (doc 'description "Search issues by title AND description (case-insensitive)")
  (doc 'note "More comprehensive than bbs-find which only searches titles")
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

(define (bbs-labels)
  (doc 'type (-> (List Symbol)))
  (doc 'description "Get all unique labels in use across all issues")
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

(define (bbs-label-report)
  (doc 'type (-> Void))
  (doc 'description "Show all labels and their issue counts")
  (let ([labels (bbs-labels)])
    (printf "~a labels in use~n" (length labels))
    (printf "~a~n" (make-string 40 #\-))
    (for-each
     (lambda (label)
       (let ([count (length (bbs-by-label label))])
         (printf "  ~a: ~a~n" (pad-right (symbol->string label) 20) count)))
     labels)))

(doc 'section 'string-utilities)

(define (take-n n lst)
  (doc 'type (-> Int (List a) (List a)))
  (if (or (<= n 0) (null? lst))
      '()
      (cons (car lst) (take-n (- n 1) (cdr lst)))))

(define (pad-right str width)
  (doc 'type (-> String Int String))
  (let ([len (string-length str)])
    (if (>= len width)
        str
        (string-append str (make-string (- width len) #\space)))))

(define (truncate-str str max-len)
  (doc 'type (-> String Int String))
  (if (<= (string-length str) max-len)
      str
      (string-append (substring str 0 (- max-len 3)) "...")))

(define (string-contains-ci? haystack needle)
  (doc 'type (-> String String Boolean))
  (doc 'description "Case-insensitive substring search")
  (let ([h-len (string-length haystack)]
        [n-len (string-length needle)])
    (and (<= n-len h-len)
         (let loop ([i 0])
           (cond
            [(> (+ i n-len) h-len) #f]
            [(string-prefix-ci? haystack i needle) #t]
            [else (loop (+ i 1))])))))

(define (string-prefix-ci? str start prefix)
  (doc 'type (-> String Int String Boolean))
  (let ([p-len (string-length prefix)])
    (let loop ([i 0])
      (cond
       [(>= i p-len) #t]
       [(>= (+ start i) (string-length str)) #f]
       [(char-ci=? (string-ref str (+ start i))
                   (string-ref prefix i))
        (loop (+ i 1))]
       [else #f]))))

(define (string-downcase str)
  (doc 'type (-> String String))
  (list->string
   (map char-downcase (string->list str))))

(doc 'section 'print-help)

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
