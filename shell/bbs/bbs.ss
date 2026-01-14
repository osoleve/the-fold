;;; shell/bbs/bbs.ss — BBS Entry Point
;;;
;;; CAS-native bulletin board system for issue tracking.
;;; Replaces beads with a native implementation using The Fold's CAS.
;;;
;;; Usage:
;;;   (load "shell/bbs/bbs.ss")
;;;   (bbs-init!)
;;;
;;;   (bbs-create "Issue title" 'priority 2 'type 'task)
;;;   (bbs-show 'fold-001)
;;;   (bbs-list)
;;;   (bbs-update 'fold-001 'status 'in_progress)
;;;   (bbs-close 'fold-001 'reason "Done!")
;;;
;;; This is Shell code: loads all BBS modules.

(load "shell/bbs/ops.ss")

;;; ====
;;; Initialization
;;; ====

;;; bbs-init! : -> Int
;;; Initialize the BBS system.
;;; Loads indices from disk and returns the number of issues.
(define (bbs-init!)
  (printf "Initializing BBS...~n")
  (let ([count (bbs-rebuild-indices!)])
    (printf "  Loaded ~a issues~n" count)
    count))

;;; ====
;;; Display Functions
;;; ====

;;; bbs-show : String | Symbol -> Void
;;; Display an issue.
(define (bbs-show id)
  (let* ([id-str (if (symbol? id) (symbol->string id) id)]
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
          ;; Show blockers
          (let ([blockers (bbs-blockers id-str)])
            (unless (null? blockers)
              (printf "~nBlocked by:  ~a~n" blockers)))
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
                  (map car *bbs-issues*)
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
  (let* ([id-str (if (symbol? id) (symbol->string id) id)]
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
                   (map car *bbs-issues*))])
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
(printf "  (bbs-find \"query\")              - Search issues~n")
(printf "  (bbs-history 'id)               - Show version history~n")
