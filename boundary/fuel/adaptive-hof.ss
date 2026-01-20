(load "boundary/fuel/adaptive-allocator.ss")

(doc 'module 'adaptive-hof)
(doc 'description "HOFs with runtime-adaptive fuel allocation. Uses Kalman filtering to refine cost estimates as elements are processed.")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'dependencies '(boundary/fuel/adaptive-allocator))

(doc 'note "Features: Per-element fuel allocation (not batch), log-space Kalman filter for heavy-tailed costs, accumulates cost across retries (no filter pollution), returns stats alongside results for observability")

(doc 'section 'options-parsing)

(define (normalize-opts opts)
  (doc 'type (-> (Union Alist Plist) Alist))
  (doc 'description "Normalize options to alist format. Accepts alist '((key . value) ...) or plist '(key value ...) and returns alist.")
  (doc 'note "Distinguishes by checking if first element is a pair with a symbol car (alist) or just a symbol (plist)")
  (doc 'export #t)
  (cond
   [(null? opts) '()]
   [(and (pair? (car opts))
         (symbol? (caar opts)))
    opts]  ; Already an alist
   [(symbol? (car opts))
    (plist->alist opts)]
   [else
    (error 'normalize-opts "invalid options format" opts)]))

(define (plist->alist plist)
  (doc 'type (-> Plist Alist))
  (doc 'description "Convert property list to association list. '(key1 val1 key2 val2) → '((key1 . val1) (key2 . val2))")
  (doc 'export #t)
  (let loop ([remaining plist] [acc '()])
       (cond
        [(null? remaining) (reverse acc)]
        [(null? (cdr remaining))
         (error 'plist->alist "odd-length property list")]
        [else
         (let ([key (car remaining)]
               [val (cadr remaining)])
              (loop (cddr remaining)
                    (cons (cons key val) acc)))])))

(define (get-opt opts key default)
  (doc 'type (-> Alist Symbol Any Any))
  (doc 'description "Get option value or default")
  (let ([entry (assq key opts)])
       (if entry (cdr entry) default)))

(doc 'section 'adaptive-evaluation)

(define (adaptive-eval-element alloc expr env max-retries)
  (doc 'type (-> Allocator Expr Env Nat (Values (Union Result Error) Allocator)))
  (doc 'description "Evaluate a single element with adaptive retry. Accumulates fuel across retries, updates filter only on success.")
  (doc 'returns "(values result-or-error updated-allocator)")
  (doc 'export #t)
  (let loop ([fuel-limit (allocator-request-fuel alloc)]
             [accumulated-cost 0]
             [retries 0]
             [alloc alloc])
       (if (> retries max-retries)
           (values `(error max-retries-exceeded ,expr ,accumulated-cost) alloc)
           (let* ([alloc-tracked (allocator-mark-allocated alloc fuel-limit)]
                  [result (eval-expr expr env fuel-limit)])
                 (case (result-status result)
                       [(ok)
                        (let* ([step-cost (- fuel-limit (result-remaining-fuel result))]
                               [total-cost (+ accumulated-cost step-cost)]
                               [alloc-updated (allocator-observe alloc-tracked total-cost)])
                              (values `(ok ,(result-value result) ,total-cost) alloc-updated))]
                       [(suspended)
                        (loop (* 2 fuel-limit)
                              (+ accumulated-cost fuel-limit)
                              (+ retries 1)
                              alloc-tracked)]
                       [else
                        (values result alloc-tracked)])))))

(doc 'section 'adaptive-map)

(define (adaptive-map f xs . opts)
  (doc 'type (-> Expr (List a) Options (Values (List b) Alist)))
  (doc 'description "Map with adaptive fuel allocation. The function f should be a Core DSL expression: Lambda (fn (x) body) or variable name bound in env")
  (doc 'param 'f "Core DSL expression (lambda or variable)")
  (doc 'param 'xs "List of elements to map over")
  (doc 'param 'opts "Options as alist or plist: initial-estimate (default 100), confidence (default 2.0), process-noise Q (default 0.1), measurement-noise R (default 0.5), max-retries (default 10), env (default empty-env)")
  (doc 'returns "(values results stats) where stats is an alist with efficiency metrics")
  (doc 'note "For Scheme-native functions, use adaptive-map-native instead")
  (doc 'export #t)
  (let* ([opts (normalize-opts (if (null? opts) '() (car opts)))]
         [initial-estimate (get-opt opts 'initial-estimate 100)]
         [confidence (get-opt opts 'confidence 2.0)]
         [Q (get-opt opts 'process-noise 0.1)]
         [R (get-opt opts 'measurement-noise 0.5)]
         [max-retries (get-opt opts 'max-retries 10)]
         [env (get-opt opts 'env empty-env)]
         [alloc (make-adaptive-allocator initial-estimate Q R confidence)])

        (define (make-call elem)
          (doc 'type (-> Any Expr))
          (doc 'description "Build call expression for f applied to elem. f is passed directly (not quoted) for proper closure handling, element is quoted as runtime value.")
          `(call ,f (quote ,elem)))

        (let loop ([remaining xs]
                   [results '()]
                   [alloc alloc])
             (if (null? remaining)
                 (values (reverse results) (allocator-summary alloc))
                 (let ([elem (car remaining)])
                      (let-values ([(result alloc*)
                                    (adaptive-eval-element alloc
                                                           (make-call elem)
                                                           env
                                                           max-retries)])
                                  (case (car result)
                                        [(ok)
                                         (loop (cdr remaining)
                                               (cons (cadr result) results)
                                               alloc*)]
                                        [else
                                         (values `(error ,result
                                                   (completed . ,(reverse results))
                                                   (remaining . ,(length (cdr remaining))))
                                                 (allocator-summary alloc*))])))))))

(doc 'section 'adaptive-map-native)

(define (adaptive-map-native f xs . opts)
  (doc 'type (-> (-> a b) (List a) Options (Values (List b) Alist)))
  (doc 'description "Map with adaptive fuel allocation for native Scheme functions. This version applies the function directly, measuring wall time as cost. Useful when function isn't in Core DSL.")
  (doc 'param 'opts "Options as alist or plist: initial-estimate (default 100), confidence (default 2.0), process-noise Q (default 0.1), measurement-noise R (default 0.5)")
  (doc 'export #t)
  (let* ([opts (normalize-opts (if (null? opts) '() (car opts)))]
         [initial-estimate (get-opt opts 'initial-estimate 100)]
         [confidence (get-opt opts 'confidence 2.0)]
         [Q (get-opt opts 'process-noise 0.1)]
         [R (get-opt opts 'measurement-noise 0.5)]
         [alloc (make-adaptive-allocator initial-estimate Q R confidence)])

        (let loop ([remaining xs]
                   [results '()]
                   [alloc alloc])
             (if (null? remaining)
                 (values (reverse results) (allocator-summary alloc))
                 (let* ([elem (car remaining)]
                        [start-time (get-time-micros)]
                        [result (f elem)]
                        [end-time (get-time-micros)]
                        [cost (max 1 (time-diff-micros end-time start-time))]
                        [alloc* (allocator-observe alloc cost)])
                       (loop (cdr remaining)
                             (cons result results)
                             alloc*))))))

(doc 'section 'adaptive-filter)

(define (adaptive-filter pred xs . opts)
  (doc 'type (-> (-> a Bool) (List a) Alist (Values (List a) Alist)))
  (doc 'description "Filter with adaptive fuel allocation")
  (doc 'export #t)
  (let-values ([(results stats)
                (apply adaptive-map
                       (lambda (x) (cons (pred x) x))
                       xs
                       opts)])
              (if (and (pair? results) (eq? (car results) 'error))
                  (values results stats)
                  (values (map cdr (filter (lambda (p) (car p)) results))
                          stats))))

(define (adaptive-filter-native pred xs . opts)
  (doc 'type (-> (-> a Bool) (List a) Alist (Values (List a) Alist)))
  (doc 'description "Filter with native Scheme predicates")
  (doc 'export #t)
  (let-values ([(results stats)
                (apply adaptive-map-native
                       (lambda (x) (cons (pred x) x))
                       xs
                       opts)])
              (values (map cdr (filter (lambda (p) (car p)) results))
                      stats)))

(doc 'section 'adaptive-fold)

(define (adaptive-fold-left f init xs . opts)
  (doc 'type (-> Expr b (List a) Options (Values b Alist)))
  (doc 'description "Left fold with adaptive fuel allocation. The function f should be a Core DSL expression taking (acc, elem).")
  (doc 'param 'opts "Options as alist or plist: initial-estimate (default 100), confidence (default 2.0), process-noise Q (default 0.1), measurement-noise R (default 0.5), max-retries (default 10), env (default empty-env)")
  (doc 'export #t)
  (let* ([opts (normalize-opts (if (null? opts) '() (car opts)))]
         [initial-estimate (get-opt opts 'initial-estimate 100)]
         [confidence (get-opt opts 'confidence 2.0)]
         [Q (get-opt opts 'process-noise 0.1)]
         [R (get-opt opts 'measurement-noise 0.5)]
         [max-retries (get-opt opts 'max-retries 10)]
         [env (get-opt opts 'env empty-env)]
         [alloc (make-adaptive-allocator initial-estimate Q R confidence)])

        (define (make-call acc elem)
          (doc 'type (-> Any Any Expr))
          (doc 'description "Build call expression for f applied to accumulator and element. f is passed directly (not quoted) for proper closure handling. acc and elem are quoted as runtime values.")
          `(call ,f (quote ,acc) (quote ,elem)))

        (let loop ([remaining xs]
                   [acc init]
                   [alloc alloc])
             (if (null? remaining)
                 (values acc (allocator-summary alloc))
                 (let ([elem (car remaining)])
                      (let-values ([(result alloc*)
                                    (adaptive-eval-element alloc
                                                           (make-call acc elem)
                                                           env
                                                           max-retries)])
                                  (case (car result)
                                        [(ok)
                                         (loop (cdr remaining)
                                               (cadr result)
                                               alloc*)]
                                        [else
                                         (values `(error ,result
                                                   (accumulated . ,acc)
                                                   (remaining . ,(length remaining)))
                                                 (allocator-summary alloc*))])))))))

(define (adaptive-fold-left-native f init xs . opts)
  (doc 'type (-> (-> b a b) b (List a) Options (Values b Alist)))
  (doc 'description "Left fold with native Scheme functions")
  (doc 'param 'opts "Options as alist or plist: initial-estimate (default 100), confidence (default 2.0), process-noise Q (default 0.1), measurement-noise R (default 0.5)")
  (doc 'export #t)
  (let* ([opts (normalize-opts (if (null? opts) '() (car opts)))]
         [initial-estimate (get-opt opts 'initial-estimate 100)]
         [confidence (get-opt opts 'confidence 2.0)]
         [Q (get-opt opts 'process-noise 0.1)]
         [R (get-opt opts 'measurement-noise 0.5)]
         [alloc (make-adaptive-allocator initial-estimate Q R confidence)])

        (let loop ([remaining xs]
                   [acc init]
                   [alloc alloc])
             (if (null? remaining)
                 (values acc (allocator-summary alloc))
                 (let* ([elem (car remaining)]
                        [start-time (get-time-micros)]
                        [new-acc (f acc elem)]
                        [end-time (get-time-micros)]
                        [cost (max 1 (time-diff-micros end-time start-time))]
                        [alloc* (allocator-observe alloc cost)])
                       (loop (cdr remaining)
                             new-acc
                             alloc*))))))

(doc 'section 'time-utilities)
(doc 'note "For native function timing - Chez Scheme specific")

(define (get-time-micros)
  (doc 'type (-> Nat))
  (doc 'description "Get current time in microseconds")
  (doc 'export #t)
  (let ([t (current-time 'time-utc)])
       (+ (* (time-second t) 1000000)
          (quotient (time-nanosecond t) 1000))))

(define (time-diff-micros end start)
  (doc 'type (-> Nat Nat Nat))
  (doc 'description "Difference between two times in microseconds")
  (doc 'export #t)
  (max 0 (- end start)))

(doc 'section 'convenience)

(define (adaptive-eval expr)
  (doc 'type (-> Expr (Values Any Alist)))
  (doc 'description "Evaluate a single expression with adaptive fuel allocation. Useful for one-off evaluations where you don't know the cost.")
  (doc 'export #t)
  (let ([alloc (default-adaptive-allocator)])
       (let-values ([(result alloc*)
                     (adaptive-eval-element alloc expr empty-env 10)])
                   (values (if (eq? (car result) 'ok)
                               (cadr result)
                               result)
                           (allocator-summary alloc*)))))
