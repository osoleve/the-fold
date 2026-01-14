;;; shell/bbs/index.ss — BBS In-Memory Indices
;;;
;;; Maintains in-memory indices for fast issue lookups.
;;; Rebuilt on initialization from head files.
;;;
;;; Indices:
;;;   *bbs-issues*     - ((id . hash) ...)
;;;   *bbs-by-status*  - hashtable: status -> (id ...)
;;;   *bbs-deps*       - ((blocker-id . blocked-id) ...)
;;;
;;; This is Shell code: impure (maintains mutable state).

(load "shell/bbs/store.ss")
(load "shell/bbs/counter.ss")

;;; ====
;;; Global State
;;; ====

;;; All issues: ((id . hash) ...)
(define *bbs-issues* '())

;;; Issues by status: hashtable status -> (id ...)
(define *bbs-by-status* (make-eq-hashtable))

;;; Issues by priority: hashtable priority -> (id ...)
(define *bbs-by-priority* (make-eqv-hashtable))

;;; Dependencies: ((blocker-id . blocked-id) ...)
(define *bbs-deps* '())

;;; Is the index initialized?
(define *bbs-initialized* #f)

;;; ====
;;; Index Building
;;; ====

;;; bbs-rebuild-indices! : -> Int
;;; Rebuild all indices from head files.
;;; Returns the number of issues indexed.
(define (bbs-rebuild-indices!)
  (set! *bbs-issues* '())
  (set! *bbs-by-status* (make-eq-hashtable))
  (set! *bbs-by-priority* (make-eqv-hashtable))
  (set! *bbs-deps* '())

  (let* ([ids (bbs-list-heads)]
         [count 0])
    ;; Sync counter to avoid ID collisions
    (bbs-sync-counter-from-heads! ids)

    ;; Index each issue
    (for-each
     (lambda (id)
       (let ([hash (bbs-read-head id)])
         (when hash
           (let ([blk (bbs-fetch hash)])
             (when blk
               (let ([data (issue-block-data blk)])
                 (when data
                   ;; Add to main index
                   (set! *bbs-issues* (cons (cons id hash) *bbs-issues*))
                   (set! count (+ count 1))

                   ;; Index by status
                   (let* ([status (cdr (assq 'status data))]
                          [existing (hashtable-ref *bbs-by-status* status '())])
                     (hashtable-set! *bbs-by-status* status (cons id existing)))

                   ;; Index by priority
                   (let* ([priority (cdr (assq 'priority data))]
                          [existing (hashtable-ref *bbs-by-priority* priority '())])
                     (hashtable-set! *bbs-by-priority* priority (cons id existing))))))))))
     ids)

    ;; Load dependencies from disk
    (bbs-load-deps!)

    (set! *bbs-initialized* #t)
    count))

;;; ====
;;; Index Updates
;;; ====

;;; bbs-index-add! : String Bytevector -> Void
;;; Add a new issue to the index.
(define (bbs-index-add! id hash)
  (let ([blk (bbs-fetch hash)])
    (when blk
      (let ([data (issue-block-data blk)])
        (when data
          ;; Add to main index
          (set! *bbs-issues* (cons (cons id hash) *bbs-issues*))

          ;; Index by status
          (let* ([status (cdr (assq 'status data))]
                 [existing (hashtable-ref *bbs-by-status* status '())])
            (hashtable-set! *bbs-by-status* status (cons id existing)))

          ;; Index by priority
          (let* ([priority (cdr (assq 'priority data))]
                 [existing (hashtable-ref *bbs-by-priority* priority '())])
            (hashtable-set! *bbs-by-priority* priority (cons id existing))))))))

;;; bbs-index-update! : String Bytevector Symbol Symbol Int Int -> Void
;;; Update an issue in the index (handles status/priority changes).
(define (bbs-index-update! id new-hash old-status new-status old-priority new-priority)
  ;; Update main index
  (set! *bbs-issues*
        (cons (cons id new-hash)
              (filter (lambda (entry) (not (string=? (car entry) id)))
                      *bbs-issues*)))

  ;; Update status index if changed
  (unless (eq? old-status new-status)
    ;; Remove from old status
    (let ([old-list (hashtable-ref *bbs-by-status* old-status '())])
      (hashtable-set! *bbs-by-status* old-status
                      (filter (lambda (x) (not (string=? x id))) old-list)))
    ;; Add to new status
    (let ([new-list (hashtable-ref *bbs-by-status* new-status '())])
      (hashtable-set! *bbs-by-status* new-status (cons id new-list))))

  ;; Update priority index if changed
  (unless (= old-priority new-priority)
    ;; Remove from old priority
    (let ([old-list (hashtable-ref *bbs-by-priority* old-priority '())])
      (hashtable-set! *bbs-by-priority* old-priority
                      (filter (lambda (x) (not (string=? x id))) old-list)))
    ;; Add to new priority
    (let ([new-list (hashtable-ref *bbs-by-priority* new-priority '())])
      (hashtable-set! *bbs-by-priority* new-priority (cons id new-list)))))

;;; ====
;;; Index Queries
;;; ====

;;; bbs-all-issues : -> (List (String . Bytevector))
;;; Get all issues as (id . hash) pairs.
(define (bbs-all-issues)
  *bbs-issues*)

;;; bbs-issues-by-status : Symbol -> (List String)
;;; Get issue IDs with a given status.
(define (bbs-issues-by-status status)
  (hashtable-ref *bbs-by-status* status '()))

;;; bbs-issues-by-priority : Int -> (List String)
;;; Get issue IDs with a given priority.
(define (bbs-issues-by-priority priority)
  (hashtable-ref *bbs-by-priority* priority '()))

;;; bbs-issue-count : -> Int
;;; Get total number of issues.
(define (bbs-issue-count)
  (length *bbs-issues*))

;;; bbs-issue-exists? : String -> Boolean
;;; Check if an issue exists in the index.
(define (bbs-issue-exists? id)
  (let ([id-str (if (symbol? id) (symbol->string id) id)])
    (assoc id-str *bbs-issues*)))

;;; bbs-issue-hash : String|Symbol -> Bytevector | #f
;;; Get the current hash for an issue ID.
(define (bbs-issue-hash id)
  (let* ([id-str (if (symbol? id) (symbol->string id) id)]
         [entry (assoc id-str *bbs-issues*)])
    (if entry (cdr entry) #f)))

;;; ====
;;; Dependency Management
;;; ====

(define *bbs-deps-file* ".bbs/deps")

;;; bbs-save-deps! : -> Void
;;; Persist dependencies to disk.
(define (bbs-save-deps!)
  (unless (file-exists? ".bbs")
    (mkdir ".bbs"))
  (call-with-output-file *bbs-deps-file*
    (lambda (port)
      (write *bbs-deps* port)
      (newline port))
    '(replace)))

;;; bbs-load-deps! : -> Void
;;; Load dependencies from disk.
(define (bbs-load-deps!)
  (guard (e [else (set! *bbs-deps* '())])
    (if (file-exists? *bbs-deps-file*)
        (set! *bbs-deps*
              (call-with-input-file *bbs-deps-file*
                (lambda (port)
                  (let ([data (read port)])
                    (if (eof-object? data) '() data)))))
        (set! *bbs-deps* '()))))

;;; bbs-add-dep! : String String -> Void
;;; Add a dependency: blocker-id blocks blocked-id.
(define (bbs-add-dep! blocker-id blocked-id)
  (unless (assoc blocker-id
                 (filter (lambda (d) (string=? (cdr d) blocked-id)) *bbs-deps*))
    (set! *bbs-deps* (cons (cons blocker-id blocked-id) *bbs-deps*))
    (bbs-save-deps!)))

;;; bbs-remove-dep! : String String -> Void
;;; Remove a dependency.
(define (bbs-remove-dep! blocker-id blocked-id)
  (set! *bbs-deps*
        (filter (lambda (d)
                  (not (and (string=? (car d) blocker-id)
                            (string=? (cdr d) blocked-id))))
                *bbs-deps*))
  (bbs-save-deps!))

;;; bbs-blockers : String -> (List String)
;;; Get IDs of issues that block the given issue.
(define (bbs-blockers id)
  (map car
       (filter (lambda (d) (string=? (cdr d) id)) *bbs-deps*)))

;;; bbs-blocking : String -> (List String)
;;; Get IDs of issues that the given issue blocks.
(define (bbs-blocking id)
  (map cdr
       (filter (lambda (d) (string=? (car d) id)) *bbs-deps*)))

;;; bbs-is-blocked? : String -> Boolean
;;; Check if an issue is blocked by any open issues.
(define (bbs-is-blocked? id)
  (let ([blockers (bbs-blockers id)])
    (any (lambda (blocker-id)
           (let ([data (bbs-fetch-issue-data blocker-id)])
             (and data
                  (not (eq? (cdr (assq 'status data)) 'closed)))))
         blockers)))

;;; bbs-blocked-issues : -> (List String)
;;; Get all issues that are blocked.
(define (bbs-blocked-issues)
  (filter bbs-is-blocked?
          (bbs-issues-by-status 'open)))

;;; bbs-ready-issues : -> (List String)
;;; Get all open issues that are not blocked.
(define (bbs-ready-issues)
  (filter (lambda (id) (not (bbs-is-blocked? id)))
          (bbs-issues-by-status 'open)))

;;; any : (a -> Bool) (List a) -> Bool
(define (any pred lst)
  (cond
   [(null? lst) #f]
   [(pred (car lst)) #t]
   [else (any pred (cdr lst))]))

;;; ====
;;; Statistics
;;; ====

;;; bbs-stats : -> Alist
;;; Get statistics about the issue database.
(define (bbs-stats)
  `((total . ,(length *bbs-issues*))
    (open . ,(length (bbs-issues-by-status 'open)))
    (in_progress . ,(length (bbs-issues-by-status 'in_progress)))
    (closed . ,(length (bbs-issues-by-status 'closed)))
    (blocked . ,(length (bbs-blocked-issues)))
    (ready . ,(length (bbs-ready-issues)))
    (deps . ,(length *bbs-deps*))))
