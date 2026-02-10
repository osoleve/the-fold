;;; lattice/fp/analysis/fusion-detect.ss — Fusion Pattern Detection
;;; @module fusion-detect
;;; @requires prelude

(require 'prelude)

(doc 'module 'fusion-detect)
(doc 'description "Static Fusion Pattern Detection

Analyzes S-expressions to detect fusion opportunities:
  - map-map fusion: (map f (map g xs)) -> (map (compose f g) xs)
  - filter-map fusion: (map f (filter p xs)) -> (filter-map p f xs)
  - map-filter fusion: (filter p (map f xs)) -> (filter-map (compose p f) f xs)
  - fold-map fusion: (foldl f z (map g xs)) -> (foldl (fn (a x) (f a (g x))) z xs)
  - concat-map fusion: (flatten (map f xs)) -> (flatMap f xs)
  - stream-map-map fusion: (stream-map f (stream-map g s)) -> (stream-map (compose f g) s)")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'dependencies '(core/base/prelude.ss))
(doc 'exports '(detect-fusion-static fusion-opportunity? fusion-type
                fusion-location fusion-before fusion-after fusion-savings
                fusion-confidence))

(doc 'section 'fusion-opportunity-data-structure)
(doc 'note "A FusionOpportunity captures a detected fusion pattern:
  (fusion-opportunity
    (type . Symbol)           ; Rule name: map-map-fuse, filter-map-fuse, etc.
    (location . Position)     ; Path in expression tree (list of indices)
    (before . Expr)           ; Original expression fragment
    (after . Expr)            ; Suggested rewrite
    (savings . Nat)           ; Estimated traversal savings (0-100)
    (confidence . Symbol))    ; safe or check-effects")

(define (make-fusion-opportunity type location before after savings confidence)
  (doc 'type '(-> Symbol Position Expr Expr Nat Symbol FusionOpportunity))
  (doc 'description "Create a new fusion opportunity record.")
  `(fusion-opportunity
    (type . ,type)
    (location . ,location)
    (before . ,before)
    (after . ,after)
    (savings . ,savings)
    (confidence . ,confidence)))

(define (fusion-opportunity? x)
  (doc 'type '(-> Any Boolean))
  (doc 'description "Check if x is a FusionOpportunity.")
  (and (pair? x)
       (eq? (car x) 'fusion-opportunity)
       (pair? (assq 'type (cdr x)))
       (pair? (assq 'location (cdr x)))
       #t))

(define (fusion-type opp)
  (doc 'type '(-> FusionOpportunity Symbol))
  (cdr (assq 'type (cdr opp))))

(define (fusion-location opp)
  (doc 'type '(-> FusionOpportunity Position))
  (cdr (assq 'location (cdr opp))))

(define (fusion-before opp)
  (doc 'type '(-> FusionOpportunity Expr))
  (cdr (assq 'before (cdr opp))))

(define (fusion-after opp)
  (doc 'type '(-> FusionOpportunity Expr))
  (cdr (assq 'after (cdr opp))))

(define (fusion-savings opp)
  (doc 'type '(-> FusionOpportunity Nat))
  (cdr (assq 'savings (cdr opp))))

(define (fusion-confidence opp)
  (doc 'type '(-> FusionOpportunity Symbol))
  (cdr (assq 'confidence (cdr opp))))

(doc 'section 'position-utilities)
(doc 'note "Positions are paths into an expression tree:
  () means root
  (0) means first element (after head)
  (1 0) means second element's first sub-element")

(define (expr-at-position expr pos)
  (doc 'type '(-> Expr Position Expr))
  (doc 'description "Extract subexpression at given position.")
  (if (null? pos)
      expr
      (if (and (pair? expr) (> (length expr) (car pos)))
          (expr-at-position (list-ref expr (car pos)) (cdr pos))
          expr)))

(doc 'section 'pure-function-detection)

(define *pure-hof*
  '(map filter foldl foldr fold-left fold-right
    stream-map stream-filter flatten flatMap
    andmap ormap for-all exists))
(doc *pure-hof* 'description "Known pure higher-order functions")

(define *pure-primitives*
  '(+ - * / = < > <= >= eq? equal? not and or
    car cdr cons list append reverse length
    null? pair? number? symbol? string? boolean?
    abs min max expt sqrt sin cos tan log exp
    string-length string-ref substring string-append
    char=? char<? char>?))
(doc *pure-primitives* 'description "Known pure primitives (for confidence estimation)")

(define (known-pure? sym)
  (doc 'type '(-> Symbol Boolean))
  (doc 'description "Check if a function symbol is known to be pure.")
  (or (memq sym *pure-hof*)
      (memq sym *pure-primitives*)))

(define (estimate-purity expr)
  (doc 'type '(-> Expr Symbol))
  (doc 'description "Estimate if an expression is pure: safe, likely-pure, or check-effects
This is called on the function argument of map/filter/fold patterns.
For (map f (map g xs)), we call (estimate-purity f) and (estimate-purity g).")
  (cond
   ;; Lambda expressions are safe (they're pure by construction)
   [(and (pair? expr) (eq? (car expr) 'lambda)) 'safe]
   [(and (pair? expr) (eq? (car expr) 'fn)) 'safe]
   ;; Symbol that's a known pure function
   [(and (symbol? expr) (known-pure? expr)) 'safe]
   ;; Symbol that's a known effectful function
   [(and (symbol? expr) (memq expr '(set! display write newline read)))
    'check-effects]
   ;; Unknown symbol (function name not in known lists)
   [(symbol? expr) 'likely-pure]
   ;; Application of known pure function
   [(and (pair? expr) (symbol? (car expr)) (known-pure? (car expr))) 'safe]
   ;; Application of known effectful function
   [(and (pair? expr) (symbol? (car expr))
         (memq (car expr) '(set! display write newline read)))
    'check-effects]
   ;; Other applications
   [(pair? expr) 'likely-pure]
   ;; Other atoms (numbers, strings, etc.) are safe
   [else 'safe]))

(define (combine-confidence c1 c2)
  (doc 'type '(-> Symbol Symbol Symbol))
  (doc 'description "Combine two confidence levels (pessimistic).")
  (cond
   [(or (eq? c1 'check-effects) (eq? c2 'check-effects)) 'check-effects]
   [(or (eq? c1 'likely-pure) (eq? c2 'likely-pure)) 'likely-pure]
   [else 'safe]))

(doc 'section 'pattern-matchers)
(doc 'note "Each matcher returns #f or (bindings . confidence)")

(define (match-map-map expr)
  (doc 'type '(-> Expr (U False (Pair Bindings Confidence))))
  (doc 'description "Pattern: (map f (map g xs))")
  (and (pair? expr)
       (eq? (car expr) 'map)
       (= (length expr) 3)
       (let ([f (cadr expr)]
             [inner (caddr expr)])
            (and (pair? inner)
                 (eq? (car inner) 'map)
                 (= (length inner) 3)
                 (let ([g (cadr inner)]
                       [xs (caddr inner)])
                      (cons `((f . ,f) (g . ,g) (xs . ,xs))
                            (combine-confidence (estimate-purity f)
                                                (estimate-purity g))))))))

(define (match-filter-map expr)
  (doc 'type '(-> Expr (U False (Pair Bindings Confidence))))
  (doc 'description "Pattern: (map f (filter p xs))")
  (and (pair? expr)
       (eq? (car expr) 'map)
       (= (length expr) 3)
       (let ([f (cadr expr)]
             [inner (caddr expr)])
            (and (pair? inner)
                 (eq? (car inner) 'filter)
                 (= (length inner) 3)
                 (let ([p (cadr inner)]
                       [xs (caddr inner)])
                      (cons `((f . ,f) (p . ,p) (xs . ,xs))
                            (combine-confidence (estimate-purity f)
                                                (estimate-purity p))))))))

(define (match-map-filter expr)
  (doc 'type '(-> Expr (U False (Pair Bindings Confidence))))
  (doc 'description "Pattern: (filter p (map f xs))")
  (and (pair? expr)
       (eq? (car expr) 'filter)
       (= (length expr) 3)
       (let ([p (cadr expr)]
             [inner (caddr expr)])
            (and (pair? inner)
                 (eq? (car inner) 'map)
                 (= (length inner) 3)
                 (let ([f (cadr inner)]
                       [xs (caddr inner)])
                      (cons `((p . ,p) (f . ,f) (xs . ,xs))
                            (combine-confidence (estimate-purity f)
                                                (estimate-purity p))))))))

(define (match-fold-map expr)
  (doc 'type '(-> Expr (U False (Pair Bindings Confidence))))
  (doc 'description "Pattern: (foldl f z (map g xs)) or (fold-left f z (map g xs))")
  (and (pair? expr)
       (memq (car expr) '(foldl fold-left foldr fold-right))
       (= (length expr) 4)
       (let ([fold-op (car expr)]
             [f (cadr expr)]
             [z (caddr expr)]
             [inner (cadddr expr)])
            (and (pair? inner)
                 (eq? (car inner) 'map)
                 (= (length inner) 3)
                 (let ([g (cadr inner)]
                       [xs (caddr inner)])
                      (cons `((fold-op . ,fold-op) (f . ,f) (z . ,z) (g . ,g) (xs . ,xs))
                            (combine-confidence (estimate-purity f)
                                                (estimate-purity g))))))))

(define (match-concat-map expr)
  (doc 'type '(-> Expr (U False (Pair Bindings Confidence))))
  (doc 'description "Pattern: (flatten (map f xs)) or (apply append (map f xs))")
  (cond
   ;; (flatten (map f xs))
   [(and (pair? expr)
         (eq? (car expr) 'flatten)
         (= (length expr) 2)
         (let ([inner (cadr expr)])
              (and (pair? inner)
                   (eq? (car inner) 'map)
                   (= (length inner) 3)
                   (let ([f (cadr inner)]
                         [xs (caddr inner)])
                        (cons `((f . ,f) (xs . ,xs))
                              (estimate-purity f))))))
    => identity]
   ;; (apply append (map f xs))
   [(and (pair? expr)
         (eq? (car expr) 'apply)
         (= (length expr) 3)
         (eq? (cadr expr) 'append)
         (let ([inner (caddr expr)])
              (and (pair? inner)
                   (eq? (car inner) 'map)
                   (= (length inner) 3)
                   (let ([f (cadr inner)]
                         [xs (caddr inner)])
                        (cons `((f . ,f) (xs . ,xs))
                              (estimate-purity f))))))
    => identity]
   [else #f]))

(define (match-stream-map-map expr)
  (doc 'type '(-> Expr (U False (Pair Bindings Confidence))))
  (doc 'description "Pattern: (stream-map f (stream-map g s))")
  (and (pair? expr)
       (eq? (car expr) 'stream-map)
       (= (length expr) 3)
       (let ([f (cadr expr)]
             [inner (caddr expr)])
            (and (pair? inner)
                 (eq? (car inner) 'stream-map)
                 (= (length inner) 3)
                 (let ([g (cadr inner)]
                       [s (caddr inner)])
                      (cons `((f . ,f) (g . ,g) (s . ,s))
                            (combine-confidence (estimate-purity f)
                                                (estimate-purity g))))))))

(doc 'section 'rewrite-templates)

(define (build-map-map-fused bindings)
  (doc 'type '(-> Bindings Expr))
  (doc 'description "(map f (map g xs)) -> (map (compose f g) xs)")
  (let ([f (cdr (assq 'f bindings))]
        [g (cdr (assq 'g bindings))]
        [xs (cdr (assq 'xs bindings))])
       `(map (compose ,f ,g) ,xs)))

(define (build-filter-map-fused bindings)
  (doc 'type '(-> Bindings Expr))
  (doc 'description "(map f (filter p xs)) -> (filter-map p f xs)")
  (let ([f (cdr (assq 'f bindings))]
        [p (cdr (assq 'p bindings))]
        [xs (cdr (assq 'xs bindings))])
       `(filter-map ,p ,f ,xs)))

(define (build-map-filter-fused bindings)
  (doc 'type '(-> Bindings Expr))
  (doc 'description "(filter p (map f xs)) -> (filter-map (compose p f) f xs)
Note: Requires mapping first, then filtering - slightly different semantics")
  (let ([p (cdr (assq 'p bindings))]
        [f (cdr (assq 'f bindings))]
        [xs (cdr (assq 'xs bindings))])
       `(filter-map (compose ,p ,f) ,f ,xs)))

(define (build-fold-map-fused bindings)
  (doc 'type '(-> Bindings Expr))
  (doc 'description "(foldl f z (map g xs)) -> (foldl (lambda (a x) (f a (g x))) z xs)")
  (let ([fold-op (cdr (assq 'fold-op bindings))]
        [f (cdr (assq 'f bindings))]
        [z (cdr (assq 'z bindings))]
        [g (cdr (assq 'g bindings))]
        [xs (cdr (assq 'xs bindings))])
       ;; For left folds: (lambda (acc x) (f acc (g x)))
       ;; For right folds: (lambda (x acc) (f (g x) acc))
       (if (memq fold-op '(foldl fold-left))
           `(,fold-op (lambda (acc x) (,f acc (,g x))) ,z ,xs)
           `(,fold-op (lambda (x acc) (,f (,g x) acc)) ,z ,xs))))

(define (build-concat-map-fused bindings)
  (doc 'type '(-> Bindings Expr))
  (doc 'description "(flatten (map f xs)) -> (flatMap f xs)")
  (let ([f (cdr (assq 'f bindings))]
        [xs (cdr (assq 'xs bindings))])
       `(flatMap ,f ,xs)))

(define (build-stream-map-map-fused bindings)
  (doc 'type '(-> Bindings Expr))
  (doc 'description "(stream-map f (stream-map g s)) -> (stream-map (compose f g) s)")
  (let ([f (cdr (assq 'f bindings))]
        [g (cdr (assq 'g bindings))]
        [s (cdr (assq 's bindings))])
       `(stream-map (compose ,f ,g) ,s)))

(doc 'section 'fusion-rules-table)
(doc 'note "Each rule: (name matcher builder savings)
savings: estimated % reduction in list traversals")

(define *fusion-rules*
  `((map-map-fuse        ,match-map-map        ,build-map-map-fused        50)
    (filter-map-fuse     ,match-filter-map     ,build-filter-map-fused     50)
    (map-filter-fuse     ,match-map-filter     ,build-map-filter-fused     50)
    (fold-map-fuse       ,match-fold-map       ,build-fold-map-fused       50)
    (concat-map-fuse     ,match-concat-map     ,build-concat-map-fused     50)
    (stream-map-map-fuse ,match-stream-map-map ,build-stream-map-map-fused 50)))

(doc 'section 'core-detection-logic)

(define (try-all-rules expr pos)
  (doc 'type '(-> Expr Position (List FusionOpportunity)))
  (doc 'description "Try all fusion rules on an expression at given position.")
  (let loop ([rules *fusion-rules*] [opps '()])
       (if (null? rules)
           opps
           (let* ([rule (car rules)]
                  [name (car rule)]
                  [matcher (cadr rule)]
                  [builder (caddr rule)]
                  [savings (cadddr rule)]
                  [match-result (matcher expr)])
                 (if match-result
                     (let* ([bindings (car match-result)]
                            [confidence (cdr match-result)]
                            [after (builder bindings)]
                            [opp (make-fusion-opportunity
                                  name pos expr after savings confidence)])
                           (loop (cdr rules) (cons opp opps)))
                     (loop (cdr rules) opps))))))

(define (fusion-detect-at-depth expr pos fuel)
  (doc 'type '(-> Expr Position Nat (List FusionOpportunity)))
  (doc 'description "Recursively detect fusion opportunities with depth limit.
Traverses ALL elements of lists (including index 0) to find patterns
nested inside special forms like let, lambda, etc.
Note: Prefixed to avoid collision with parallel-detect.ss")
  (if (<= fuel 0)
      '()
      (let* ([here (try-all-rules expr pos)]
             [children
              (if (pair? expr)
                  ;; Traverse ALL elements starting from index 0
                  ;; This ensures we find patterns inside let bindings, etc.
                  (let loop ([items expr] [i 0] [acc '()])
                       (if (or (null? items) (not (pair? items)))
                           acc
                           (let ([child-opps (fusion-detect-at-depth
                                              (car items)
                                              (append pos (list i))
                                              (- fuel 1))])
                                (loop (cdr items) (+ i 1) (append acc child-opps)))))
                  '())])
            (append here children))))

(define *fusion-detect-fuel* 100)
(doc *fusion-detect-fuel* 'type 'Nat)
(doc *fusion-detect-fuel* 'description "Default recursion depth for fusion detection.
Can be overridden by passing an optional fuel parameter to detect-fusion-static.")

(define detect-fusion-static
  (case-lambda
   [(expr) (fusion-detect-at-depth expr '() *fusion-detect-fuel*)]
   [(expr fuel) (fusion-detect-at-depth expr '() fuel)]))
(doc detect-fusion-static 'type '(-> Expr [Nat] (List FusionOpportunity)))
(doc detect-fusion-static 'description "Main entry point: detect all fusion opportunities in an expression.
Optional fuel parameter allows handling larger ASTs (default: 100).")

(doc 'section 'convenience-functions)

(define (count-opportunities opps)
  (doc 'type '(-> (List FusionOpportunity) Nat))
  (doc 'description "Count total opportunities.")
  (length opps))

(define (total-savings opps)
  (doc 'type '(-> (List FusionOpportunity) Nat))
  (doc 'description "Sum up potential savings (capped at 100).")
  (min 100 (fold-left (lambda (acc opp) (+ acc (fusion-savings opp))) 0 opps)))

(define (opportunities-by-type opps)
  (doc 'type '(-> (List FusionOpportunity) ((Symbol . (List FusionOpportunity)) ...)))
  (doc 'description "Group opportunities by fusion type.")
  (let ([types '(map-map-fuse filter-map-fuse map-filter-fuse
                 fold-map-fuse concat-map-fuse stream-map-map-fuse)])
       (map (lambda (t)
                    (cons t (filter (lambda (o) (eq? (fusion-type o) t)) opps)))
            types)))

(define (safe-opportunities opps)
  (doc 'type '(-> (List FusionOpportunity) (List FusionOpportunity)))
  (doc 'description "Filter to only safe (known-pure) opportunities.")
  (filter (lambda (o) (eq? (fusion-confidence o) 'safe)) opps))

(doc 'section 'display-utilities)

(define (format-opportunity opp)
  (doc 'type '(-> FusionOpportunity String))
  (doc 'description "Format an opportunity for display.")
  (format "[~a] at ~a\n  Before: ~a\n  After:  ~a\n  Savings: ~a%  Confidence: ~a"
          (fusion-type opp)
          (fusion-location opp)
          (fusion-before opp)
          (fusion-after opp)
          (fusion-savings opp)
          (fusion-confidence opp)))

(define (display-opportunities opps)
  (doc 'type '(-> (List FusionOpportunity) Void))
  (doc 'description "Display all opportunities to current output port.")
  (if (null? opps)
      (display "No fusion opportunities detected.\n")
      (begin
       (display (format "Found ~a fusion opportunities:\n\n" (length opps)))
       (for-each
        (lambda (opp)
                (display (format-opportunity opp))
                (display "\n\n"))
        opps))))

(doc 'section 'summary-report)

(define (fusion-summary opps)
  (doc 'type '(-> (List FusionOpportunity) Summary))
  (doc 'description "Generate a summary report of opportunities.")
  (let* ([by-type (opportunities-by-type opps)]
         [safe (safe-opportunities opps)]
         [unsafe (filter (lambda (o) (not (eq? (fusion-confidence o) 'safe))) opps)])
        `(fusion-summary
          (total . ,(length opps))
          (safe . ,(length safe))
          (needs-review . ,(length unsafe))
          (estimated-savings . ,(total-savings opps))
          (by-type . ,(map (lambda (pair) (cons (car pair) (length (cdr pair)))) by-type)))))
