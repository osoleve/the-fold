;;; persona-prompt-gen.ss - Helper functions for persona prompt DSL
;;;
;;; Provides primitives for building variable persona prompts:
;;;   (choice opt1 opt2 ...) - randomly select one string option
;;;   (cond [(test) val] ... [else default]) - native Scheme conditional
;;;   (load-fragment 'name) - load reusable personality fragment
;;;   string-append - native Scheme string concatenation
;;;
;;; Persona DSL files load fragments and use these helpers to build prompts.
;;;
;;; USAGE IN PERSONA FILES:
;;;   (load "agents/lib/persona-prompt-gen.ss")
;;;   (load-fragment 'response-postures)     ; Auto-resolves to correct path
;;;   (define persona-prompt
;;;     (string-append
;;;       "You are name. "
;;;       (choice "Option A" "Option B")
;;;       "..."))
;;;   persona-prompt  ; Return the prompt

;; Determine fragments directory - tries multiple fallback paths
(define (get-fragments-dir)
  (let* ([cwd (current-directory)]
         [candidates (list
           ;; Try relative to current working directory
           (string-append cwd "/agents/personas/fragments/")
           ;; Try relative to a common project root
           "/home/oso/the-fold/agents/personas/fragments/"
           ;; Try relative to Fold root (if cwd is inside project)
           (let loop ([path cwd])
             (if (string=? path "/")
                 #f
                 (let ([candidate (string-append path "/agents/personas/fragments/")])
                   (if (file-exists? candidate)
                       candidate
                       (loop (let ([last-slash (string-rindex path #\/)])
                               (if last-slash
                                   (substring path 0 last-slash)
                                   "/"))))))))])
    ;; Return first valid path
    (let loop ([paths candidates])
      (if (null? paths)
          (error 'get-fragments-dir
                 "Could not locate fragments directory. Set FOLD_FRAGMENTS environment variable.")
          (let ([path (car paths)])
            (if (and path (file-exists? path))
                path
                (loop (cdr paths))))))))

;; Cache for loaded fragments to avoid redundant loads
(define *fragment-cache* (make-eq-hashtable))

;; Random choice - select one string from a list
(define (choice . options)
  (if (null? options)
      (error 'choice "choice requires at least one option")
      (list-ref options (random (length options)))))

;; Random choice without replacement - select n distinct items from a list
;; Returns a list of n items (or fewer if list has fewer than n items)
;;
;; Examples:
;;   (choose-n 2 '("a" "b" "c"))     => ("c" "a") or ("b" "c"), etc.
;;   (choose-n 3 '("x" "y"))         => ("x" "y") - returns all 2 items
;;   (string-append (choose-n 2 '("a" "b" "c")))  => "ac" or "ba", etc.
(define (choose-n n items)
  (cond
    ;; Edge cases
    [(< n 0)
     (error 'choose-n "choose-n count must be non-negative")]
    [(= n 0)
     '()]
    [(null? items)
     '()]
    [(>= n (length items))
     ;; Return all items in random order (Fisher-Yates shuffle)
     (shuffle-list items)]
    [else
     ;; Return n items without replacement
     (let ([shuffled (shuffle-list items)])
       (take shuffled n))]))

;; Fisher-Yates shuffle - randomize list order
(define (shuffle-list lst)
  (let ([vec (list->vector lst)])
    (let loop ([i (- (vector-length vec) 1)])
      (if (<= i 0)
          (vector->list vec)
          (let* ([j (random (+ i 1))]
                 [temp (vector-ref vec i)])
            (vector-set! vec i (vector-ref vec j))
            (vector-set! vec j temp)
            (loop (- i 1)))))))

;; Take first n elements from a list
(define (take lst n)
  (cond
    [(= n 0) '()]
    [(null? lst) '()]
    [else (cons (car lst) (take (cdr lst) (- n 1)))]))

;; Fragment loading helper - auto-locates fragments directory
;; Caches loaded fragments to prevent redundant disk access
(define (load-fragment name)
  (let ([cached (hashtable-ref *fragment-cache* name #f)])
    (if cached
        cached
        (let* ([fragments-dir (get-fragments-dir)]
               [filename (string-append fragments-dir (symbol->string name) ".ss")])
          (guard (ex [else
                      (error 'load-fragment
                             (string-append "Failed to load fragment: " (symbol->string name)
                                          "\nLooking in: " filename))])
            (load filename)
            (hashtable-set! *fragment-cache* name #t)
            #t)))))
