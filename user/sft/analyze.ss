;;; user/sft/analyze.ss — Grammar static analysis
;;;
;;; Dead rule detection, reachability, coverage sampling.

(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
(require 'prelude)
(require 'state)
(require 'prng)

(load "user/sft/grammar.ss")
(load "user/sft/expand.ss")

;;; --- Reachability ---

(define (body-refs body)
  (doc 'description "Extract all rule names referenced by a body expression")
  (cond
    [(not (pair? body)) '()]
    [(eq? (car body) 'ref) (list (cadr body))]
    [(eq? (car body) 'case-on)
     ;; (case-on key (val body) ...)
     (apply append (map (lambda (clause) (body-refs (cadr clause)))
                        (cddr body)))]
    [else (apply append (map body-refs (cdr body)))]))

(define (grammar-reachable grammar start)
  (doc 'type '(-> Grammar Symbol (List Symbol)))
  (doc 'description "All rule names reachable from start rule (BFS)")
  (let loop ([queue (list start)] [visited '()])
    (if (null? queue)
        visited
        (let ([current (car queue)]
              [rest (cdr queue)])
          (if (member current visited)
              (loop rest visited)
              (let* ([body (if (grammar-has-rule? grammar current)
                               (grammar-ref grammar current)
                               '())]
                     [refs (body-refs body)]
                     [new-refs (filter (lambda (r) (not (member r visited)))
                                      refs)])
                (loop (append rest new-refs)
                      (cons current visited))))))))

(define (grammar-dead-rules grammar start)
  (doc 'type '(-> Grammar Symbol (List Symbol)))
  (doc 'description "Rules defined but not reachable from start")
  (let ([reachable (grammar-reachable grammar start)]
        [all-rules (grammar-rule-names grammar)])
    (filter (lambda (r) (not (member r reachable)))
            all-rules)))

;;; --- Coverage sampling ---

(define (grammar-coverage grammar start ctx seed n)
  (doc 'type '(-> Grammar Symbol Alist Int Int Alist))
  (doc 'description "Sample n expansions, return (rule-name . hit-count) alist")
  (let* ([rules (grammar-rule-names grammar)]
         [counts (map (lambda (r) (cons r 0)) rules)])
    ;; For each sample, track which rules get expanded
    ;; We do this by wrapping grammar-ref with a counter
    (let loop ([i 0] [counts counts])
      (if (>= i n)
          (filter (lambda (p) (> (cdr p) 0)) counts)
          (let* ([rng (make-pcg (+ seed i) 0)]
                 [touched (expansion-trace grammar start ctx rng)]
                 [new-counts (map (lambda (p)
                                    (if (member (car p) touched)
                                        (cons (car p) (+ (cdr p) 1))
                                        p))
                                  counts)])
            (loop (+ i 1) new-counts))))))

(define (expansion-trace grammar start ctx rng)
  (doc 'description "Return list of rule names visited during expansion")
  (let ([visited '()])
    (define (trace-expand rule-name rng)
      (set! visited (cons rule-name visited))
      (let ([body (grammar-ref grammar rule-name)])
        (trace-body body rng)))
    (define (trace-body body rng)
      (cond
        [(not (pair? body)) rng]
        [(eq? (car body) 'ref)
         (trace-expand (cadr body) rng)]
        [(eq? (car body) 'seq)
         (let loop ([items (cdr body)] [rng rng])
           (if (null? items) rng
               (loop (cdr items) (trace-body (car items) rng))))]
        [(eq? (car body) 'alt)
         (let* ([options (cdr body)]
                [n (length options)]
                [result (run-state (random-int-range 0 (- n 1)) rng)]
                [idx (car result)]
                [rng2 (cdr result)])
           (trace-body (list-ref options idx) rng2))]
        [(eq? (car body) 'weighted)
         (let* ([entries (cdr body)]
                [total (fold-left + 0 (map car entries))]
                [_ (when (<= total 0)
                     (error 'expansion-trace "weights must sum to positive value" total))]
                [result (run-state (random-float-range 0.0 total) rng)]
                [r (car result)]
                [rng2 (cdr result)]
                [chosen (let loop ([es entries] [acc 0.0])
                          (if (null? (cdr es))
                              (cadar es)
                              (let ([new-acc (+ acc (caar es))])
                                (if (< r new-acc) (cadar es)
                                    (loop (cdr es) new-acc)))))])
           (trace-body chosen rng2))]
        [(eq? (car body) 'maybe)
         (let* ([result (run-state random-float rng)]
                [f (car result)]
                [rng2 (cdr result)])
           (if (< f (cadr body))
               (trace-body (caddr body) rng2)
               rng2))]
        [(eq? (car body) 'when)
         (let ([entry (assq (cadr body) ctx)])
           (if (and entry (cdr entry))
               (trace-body (caddr body) rng)
               rng))]
        [(eq? (car body) 'case-on)
         (let* ([entry (assq (cadr body) ctx)]
                [val (if entry (cdr entry) #f)])
           (let loop ([cs (cddr body)])
             (cond
               [(null? cs) rng]
               [(eq? (caar cs) 'else)
                (trace-body (cadar cs) rng)]
               [(equal? (caar cs) val)
                (trace-body (cadar cs) rng)]
               [(and (string? val) (eq? (caar cs) (string->symbol val)))
                (trace-body (cadar cs) rng)]
               [(and (symbol? val) (string? (caar cs))
                     (string=? (caar cs) (symbol->string val)))
                (trace-body (cadar cs) rng)]
               [else (loop (cdr cs))])))]
        [(eq? (car body) 'join)
         ;; join looks up context key as list, or falls back to expanding rule
         (let ([entry (assq (caddr body) ctx)])
           (if (and entry (list? (cdr entry)))
               rng  ; context list — no rule expansion needed
               (trace-expand (caddr body) rng)))]
        [else rng]))
    (trace-expand start rng)
    (reverse visited)))
