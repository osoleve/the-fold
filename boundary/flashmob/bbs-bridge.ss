(load "boundary/flashmob/flashmob.ss")
(load "boundary/bbs/bbs.ss")

(doc 'module 'flashmob-bbs-bridge)
(doc 'description "Flashmob to BBS Integration - Creates BBS issues from flashmob findings and links them for traceability")
(doc 'layer 'boundary)
(doc 'purity 'partial)

(doc 'usage "
  (flashmob-to-bbs)                ; All top findings
  (flashmob-to-bbs 'count 5)       ; Top 5
  (flashmob-to-bbs 'severity 'high); High severity only
")

(doc 'section 'finding-to-issue-conversion)

(doc flashmob-finding-to-issue 'type '(-> Alist String))
(doc flashmob-finding-to-issue 'description "Create a BBS issue from a finding. Returns the new issue ID.")
(define (flashmob-finding-to-issue finding)
  (let* ([title (cdr (assq 'title finding))]
         [file (cdr (assq 'file finding))]
         [line (cdr (assq 'line finding))]
         [severity (cdr (assq 'severity finding))]
         [category (cdr (assq 'category finding))]
         [description (cdr (assq 'description finding))]
         [agent-id (cdr (assq 'agent-id finding))]
         [confidence (cdr (assq 'confidence finding))]
         ;; Map severity to priority
         [priority (bbs-severity-to-priority severity)]
         ;; Map category to type
         [type (bbs-category-to-type category)]
         ;; Build description with finding details
         [full-desc (format "~a~n~nLocation: ~a:~a~nSeverity: ~a~nCategory: ~a~nConfidence: ~a~nFound by: ~a~nSource: flashmob"
                           (if (string=? description "") "No details" description)
                           file line
                           severity category
                           confidence agent-id)]
         ;; Labels from category and severity
         [labels (list category severity 'flashmob)])
    (bbs-create title
                'description full-desc
                'priority priority
                'type type
                'labels labels
                'created-by (format "flashmob/~a" agent-id))))

(doc bbs-severity-to-priority 'type '(-> Symbol Int))
(doc bbs-severity-to-priority 'description "Map flashmob severity to BBS priority: critical->P0, high->P1, medium->P2, low->P3, info->P4")
(define (bbs-severity-to-priority severity)
  (case severity
    [(critical) 0]
    [(high) 1]
    [(medium) 2]
    [(low) 3]
    [(info) 4]
    [else 2]))  ; Default medium

(doc bbs-category-to-type 'type '(-> Symbol Symbol))
(doc bbs-category-to-type 'description "Map flashmob category to BBS issue type")
(define (bbs-category-to-type category)
  (case category
    [(security) 'bug]
    [(performance) 'task]
    [(correctness) 'bug]
    [(style) 'task]
    [(documentation) 'task]
    [else 'task]))

(doc 'section 'batch-operations)

(doc flashmob-to-bbs 'type '(-> &rest (List String)))
(doc flashmob-to-bbs 'description "Create BBS issues from triage results")
(doc flashmob-to-bbs 'param "'count - Maximum number of issues to create (default: 10)")
(doc flashmob-to-bbs 'param "'severity - Only create for this severity or higher")
(doc flashmob-to-bbs 'param "'category - Only create for this category")
(doc flashmob-to-bbs 'param "'dry-run - If #t, show what would be created without creating")
(doc flashmob-to-bbs 'returns "List of created issue IDs")
(define (flashmob-to-bbs . args)
  (unless *flashmob-session-triage*
    (error 'flashmob-to-bbs "No triage results. Run (flashmob-triage) first"))
  (let* ([count (fm-get-keyword args 'count 10)]
         [severity-filter (fm-get-keyword args 'severity #f)]
         [category-filter (fm-get-keyword args 'category #f)]
         [dry-run (fm-get-keyword args 'dry-run #f)]
         ;; Get ranked findings (selected by triage)
         [selected (cdr (assq 'selected *flashmob-session-triage*))]
         ;; Apply filters
         [filtered (bbs-bridge-filter-findings selected severity-filter category-filter)]
         ;; Take count
         [to-create (bbs-bridge-take count filtered)])
    (if dry-run
        (begin
          (printf "Would create ~a BBS issues:~n" (length to-create))
          (for-each
           (lambda (f)
             (printf "  [~a ~a] ~a~n"
                     (cdr (assq 'severity f))
                     (cdr (assq 'category f))
                     (cdr (assq 'title f))))
           to-create)
          '())
        (begin
          (printf "Creating ~a BBS issues...~n" (length to-create))
          (bbs-init!)  ; Ensure BBS is initialized
          (let ([created-ids
                 (map (lambda (f)
                        (let ([id (flashmob-finding-to-issue f)])
                          (printf "  Created: ~a~n" id)
                          id))
                      to-create)])
            (printf "Done. Created ~a issues.~n" (length created-ids))
            created-ids)))))

(doc bbs-bridge-filter-findings 'type '(-> (List Alist) Symbol Symbol (List Alist)))
(doc bbs-bridge-filter-findings 'description "Filter findings by severity and/or category")
(define (bbs-bridge-filter-findings findings severity-filter category-filter)
  (filter
   (lambda (f)
     (and (or (not severity-filter)
              (bbs-bridge-severity>=? (cdr (assq 'severity f)) severity-filter))
          (or (not category-filter)
              (eq? (cdr (assq 'category f)) category-filter))))
   findings))

(doc bbs-bridge-severity>=? 'type '(-> Symbol Symbol Boolean))
(doc bbs-bridge-severity>=? 'description "Is severity1 >= severity2 in priority?")
(define (bbs-bridge-severity>=? s1 s2)
  (let ([order '(critical high medium low info)])
    (<= (bbs-bridge-position-of s1 order)
        (bbs-bridge-position-of s2 order))))

(doc bbs-bridge-position-of 'type '(-> a (List a) Int))
(define (bbs-bridge-position-of x lst)
  (let loop ([l lst] [i 0])
    (cond
      [(null? l) 999]
      [(eq? (car l) x) i]
      [else (loop (cdr l) (+ i 1))])))

(doc bbs-bridge-take 'type '(-> Int (List a) (List a)))
(define (bbs-bridge-take n lst)
  (if (or (<= n 0) (null? lst))
      '()
      (cons (car lst) (bbs-bridge-take (- n 1) (cdr lst)))))

(doc 'section 'traceability)

(doc flashmob-issues-for-finding 'type '(-> Alist (List String)))
(doc flashmob-issues-for-finding 'description "Find BBS issues created from a finding. Searches by title match within flashmob-labeled issues only.")
(doc flashmob-issues-for-finding 'note "Optimized to search only issues with 'flashmob label instead of all issues")
(define (flashmob-issues-for-finding finding)
  (let ([title (cdr (assq 'title finding))])
    ;; Only search issues with 'flashmob label (created by flashmob-to-bbs)
    ;; This reduces O(N) from all issues to just flashmob-created issues
    (filter
     (lambda (id)
       (let ([data (bbs-fetch-issue-data id)])
         (and data
              (string=? (cdr (assq 'title data)) title))))
     (bbs-by-label 'flashmob))))

(doc flashmob-link-finding-to-issue 'type '(-> Alist String Void))
(doc flashmob-link-finding-to-issue 'description "Add a comment to an issue linking back to the finding")
(define (flashmob-link-finding-to-issue finding issue-id)
  (let* ([file (cdr (assq 'file finding))]
         [line (cdr (assq 'line finding))]
         [confidence (cdr (assq 'confidence finding))]
         [comment-text (format "Linked from flashmob finding~nLocation: ~a:~a~nConfidence: ~a"
                               file line confidence)])
    (bbs-comment issue-id comment-text
                 'content-type 'text
                 'author "flashmob")))
