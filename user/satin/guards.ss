;;; playpen/satin/guards.ss — Guard expression compilation
;;;
;;; Guards are boolean expressions that control choice visibility
;;; and effect triggers. They compile to (lambda (state) -> bool).

;;; ====
;;; Guard Forms
;;; ====

;;; Valid guard forms:
;;;   #t | #f                    ; literal boolean
;;;   (flag? <sym>)              ; check flag
;;;   (has-item? <sym>)          ; check inventory
;;;   (var <sym>)                ; get variable (truthy test)
;;;   (var= <sym> <val>)         ; variable equals value
;;;   (var>= <sym> <num>)        ; variable >= number
;;;   (var> <sym> <num>)         ; variable > number
;;;   (var<= <sym> <num>)        ; variable <= number
;;;   (var< <sym> <num>)         ; variable < number
;;;   (not <guard>)              ; negation
;;;   (and <guard> ...)          ; conjunction
;;;   (or <guard> ...)           ; disjunction
;;;   (visited? <node-id>)       ; check node visited
;;;   (quest-active? <id>)       ; check quest status
;;;   (quest-complete? <id>)     ; check quest complete
;;;   (exercise-passed? <id>)    ; check exercise passed

;;; ====
;;; Guard Recognition
;;; ====

(define (satin-guard-form? expr)
  (cond
   [(boolean? expr) #t]
   [(procedure? expr) #t]
   [(not (pair? expr)) #f]
   [else
    (let ([tag (car (strip-span expr))])
         (memq tag '(flag? has-item? var var= var>= var> var<= var<
                     not and or visited? quest-active? quest-complete?
                     exercise-passed?)))]))

;;; ====
;;; Guard Compilation
;;; ====

;;; satin-compile-guard : GuardExpr → (State → Bool)
;;; Compile a guard expression to a predicate function.
(define (satin-compile-guard expr)
  (cond
   ;; Literal booleans
   [(eq? expr #t) (lambda (_s) #t)]
   [(eq? expr #f) (lambda (_s) #f)]
   
   ;; Already a procedure
   [(procedure? expr) expr]
   
   ;; Compound forms
   [(pair? expr)
    (let ([tag (car (strip-span expr))]
          [args (cdr (strip-span expr))])
         (case tag
               ;; Flag check
               [(flag?)
                (let ([flag (car args)])
                     (lambda (s) (and (quill-state-flag? s flag) #t)))]
               
               ;; Item check
               [(has-item?)
                (let ([item (car args)])
                     (lambda (s) (and (quill-state-has-item? s item) #t)))]
               
               ;; Variable access (truthy test)
               [(var)
                (let ([k (car args)])
                     (lambda (s)
                             (let ([v (quill-state-var s k)])
                                  (and v (not (eq? v 0)) (not (eq? v #f))))))]
               
               ;; Variable equality
               [(var=)
                (let ([k (car args)]
                      [v (cadr args)])
                     (lambda (s) (equal? (quill-state-var s k) v)))]
               
               ;; Variable comparisons
               [(var>=)
                (let ([k (car args)]
                      [n (cadr args)])
                     (lambda (s) (>= (or (quill-state-var s k) 0) n)))]
               
               [(var>)
                (let ([k (car args)]
                      [n (cadr args)])
                     (lambda (s) (> (or (quill-state-var s k) 0) n)))]
               
               [(var<=)
                (let ([k (car args)]
                      [n (cadr args)])
                     (lambda (s) (<= (or (quill-state-var s k) 0) n)))]
               
               [(var<)
                (let ([k (car args)]
                      [n (cadr args)])
                     (lambda (s) (< (or (quill-state-var s k) 0) n)))]
               
               ;; Visited check
               [(visited?)
                (let ([node-id (car args)])
                     (lambda (s) (and (quill-state-flag? s (satin-visited-flag node-id)) #t)))]
               
               ;; Quest status checks
               [(quest-active?)
                (let ([quest-id (car args)])
                     (lambda (s)
                             (let ([status (quill-state-meta-get s 'quests '())])
                                  (eq? (quill-state-alist-get status quest-id 'inactive) 'active))))]
               
               [(quest-complete?)
                (let ([quest-id (car args)])
                     (lambda (s)
                             (let ([status (quill-state-meta-get s 'quests '())])
                                  (eq? (quill-state-alist-get status quest-id 'inactive) 'complete))))]
               
               ;; Exercise check
               [(exercise-passed?)
                (let ([ex-id (car args)])
                     (lambda (s)
                             (let ([status (quill-state-meta-get s 'education '())])
                                  (eq? (quill-state-alist-get status ex-id 'unseen) 'passed))))]
               
               ;; Boolean combinators
               [(not)
                (let ([p (satin-compile-guard (car args))])
                     (lambda (s) (not (p s))))]
               
               [(and)
                (let ([ps (map satin-compile-guard args)])
                     (lambda (s) (andmap (lambda (p) (p s)) ps)))]
               
               [(or)
                (let ([ps (map satin-compile-guard args)])
                     (lambda (s) (ormap (lambda (p) (p s)) ps)))]
               
               ;; Unknown form - default to true
               [else (lambda (_s) #t)]))]
   
   ;; Unknown - default to true
   [else (lambda (_s) #t)]))

;;; ====
;;; Helper Functions
;;; ====

;;; Generate flag name for node visited tracking
(define (satin-visited-flag node-id)
  (string->symbol (string-append "visited:" (symbol->string node-id))))

;;; Alist lookup helper (should be in state.ss but duplicated for clarity)
(define (quill-state-alist-get alist key default)
  (let ([p (assq key alist)])
       (if p (cdr p) default)))
