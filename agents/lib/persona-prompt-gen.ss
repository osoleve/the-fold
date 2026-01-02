;;; persona-prompt-gen.ss - Helper functions for persona prompt DSL
;;;
;;; Provides primitives for building variable persona prompts:
;;;   (choice opt1 opt2 ...) - randomly select one string option
;;;   (weighted-choice '(weight val) ...) - select option based on weights
;;;   (maybe val [prob]) - optionally include val (default 50% chance)
;;;   (interpolate template 'key val ...) - replace {key} in template
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

(load "shell/tools/string-utils.ss")
(load "shell/tools/markdown.ss")

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

;; Weighted choice - select based on integer weights
;; Usage: (weighted-choice '(80 "Common") '(20 "Rare"))
(define (weighted-choice . options)
  (if (null? options)
      (error 'weighted-choice "weighted-choice requires at least one option")
      (let* ([total-weight (apply + (map car options))]
             [r (random total-weight)])
            (let loop ([opts options] [current-sum 0])
                 (if (null? opts)
                     (cadr (car (last-pair options))) ;; Fallback (shouldn't happen)
                     (let* ([opt (car opts)]
                            [w (car opt)]
                            [val (cadr opt)]
                            [next-sum (+ current-sum w)])
                           (if (< r next-sum)
                               val
                               (loop (cdr opts) next-sum))))))))

;; Maybe - optionally return val or empty string
;; Usage: (maybe "Sometimes this appears") -> 50% chance
;;        (maybe "Rarely appears" 0.1) -> 10% chance
(define maybe
  (case-lambda
   [(val) (if (< (random 1.0) 0.5) val "")]
   [(val prob) (if (< (random 1.0) prob) val "")]))

;; String interpolation - simple template replacement
;; Usage: (interpolate "Hello {name}, you are {role}." 'name "Bob" 'role "Builder")
(define (interpolate template . args)
  (let loop ([result template] [pairs (let recur ([lst args])
                                           (if (null? lst)
                                               '()
                                               (cons (cons (car lst) (cadr lst))
                                                     (recur (cddr lst)))))])
       (if (null? pairs)
           result
           (let* ([key (symbol->string (caar pairs))]
                  [val (cdar pairs)]
                  [placeholder (string-append "{" key "}")])
                 (loop (string-replace result placeholder val) (cdr pairs))))))

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

;; Context Injection
(define *context* (make-hashtable string-hash string=?))

(define (context-ref key . default)
  (let ([key-str (symbol->string key)])
       (or (hashtable-ref *context* key-str #f)
           (getenv key-str)
           (if (null? default) #f (car default)))))

(define (with-context ctx-alist thunk)
  (let ([old-context (hashtable-copy *context* #t)])
       (for-each (lambda (pair)
                         (hashtable-set! *context*
                                         (symbol->string (car pair))
                                         (cdr pair)))
                 ctx-alist)
       (let ([result (thunk)])
            (set! *context* old-context)
            result)))

;; Namespaced Fragments
;; Usage: (define my-style (fragment (warm "Warm") (cool "Cool")))
;;        (fragment-ref my-style 'warm)
(define-syntax fragment
  (syntax-rules ()
                [(_ (key val) ...)
                 (let ([ht (make-eq-hashtable)])
                      (hashtable-set! ht 'key val) ...
                      ht)]))

(define (fragment-ref frag key . default)
  (hashtable-ref frag key (if (null? default) #f (car default))))

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
