;;; user/sft/expand.ss — Grammar expansion interpreter
;;;
;;; (expand grammar rule-name ctx rng) → (string . rng')
;;;
;;; Threads RNG explicitly — deterministic given seed.
;;; Uses lattice/random/prng State monads via run-state.

(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
(require 'prelude)
(require 'state)
(require 'prng)

(load "user/sft/grammar.ss")

;;; --- Core expansion ---

(define (expand grammar rule-name ctx rng)
  (doc 'type '(-> Grammar Symbol Alist RNG (Pair String RNG)))
  (doc 'description "Expand a named rule, returning (string . rng')")
  (let ([body (grammar-ref grammar rule-name)])
    (expand-body grammar body ctx rng)))

(define (expand-body grammar body ctx rng)
  (doc 'type '(-> Grammar Body Alist RNG (Pair String RNG)))
  (cond
    [(string? body) (cons body rng)]
    [(number? body) (cons (number->string body) rng)]
    [(symbol? body) (cons (symbol->string body) rng)]
    [(null? body)   (cons "" rng)]
    [(pair? body)
     (case (car body)
       [(ref)      (expand-ref grammar (cadr body) ctx rng)]
       [(slot)     (expand-slot (cadr body) ctx rng)]
       [(seq)      (expand-seq grammar (cdr body) ctx rng)]
       [(alt)      (expand-alt grammar (cdr body) ctx rng)]
       [(weighted) (expand-weighted grammar (cdr body) ctx rng)]
       [(maybe)    (expand-maybe grammar (cadr body) (caddr body) ctx rng)]
       [(when)     (expand-when grammar (cadr body) (caddr body) ctx rng)]
       [(case-on)  (expand-case-on grammar (cadr body) (cddr body) ctx rng)]
       [(join)     (expand-join grammar (cadr body) (caddr body) ctx rng)]
       [else       (error 'expand-body "unknown combinator" (car body))])]
    [else (cons (format "~a" body) rng)]))

;;; --- Combinator handlers ---

(define (expand-ref grammar rule-name ctx rng)
  (expand grammar rule-name ctx rng))

(define (expand-slot key ctx rng)
  (let ([entry (assq key ctx)])
    (cond
      [(not entry) (cons "" rng)]
      [(string? (cdr entry)) (cons (cdr entry) rng)]
      [(list? (cdr entry))
       ;; Lists become space-separated strings
       (cons (apply string-append
               (let loop ([items (cdr entry)] [acc '()])
                 (if (null? items)
                     (reverse acc)
                     (loop (cdr items)
                           (if (null? acc)
                               (list (format "~a" (car items)))
                               (cons (format "~a" (car items))
                                     (cons " " acc)))))))
             rng)]
      [else (cons (format "~a" (cdr entry)) rng)])))

(define (expand-seq grammar items ctx rng)
  (let loop ([remaining items] [parts '()] [rng rng])
    (if (null? remaining)
        (cons (apply string-append (reverse parts)) rng)
        (let* ([result (expand-body grammar (car remaining) ctx rng)]
               [text (car result)]
               [rng2 (cdr result)])
          (loop (cdr remaining) (cons text parts) rng2)))))

(define (expand-alt grammar options ctx rng)
  (if (null? options)
      (cons "" rng)
      (let* ([n (length options)]
             [result (run-state (random-int-range 0 (- n 1)) rng)]
             [idx (car result)]
             [rng2 (cdr result)])
        (expand-body grammar (list-ref options idx) ctx rng2))))

(define (expand-weighted grammar entries ctx rng)
  ;; entries: ((weight body) (weight body) ...)
  (if (null? entries)
      (cons "" rng)
      (let* ([total (fold-left + 0 (map car entries))]
             [result (run-state (random-float-range 0.0 total) rng)]
             [r (car result)]
             [rng2 (cdr result)]
             [body (let loop ([es entries] [acc 0.0])
                     (if (null? (cdr es))
                         (cadar es)  ; last entry fallback
                         (let ([new-acc (+ acc (caar es))])
                           (if (< r new-acc)
                               (cadar es)
                               (loop (cdr es) new-acc)))))])
        (expand-body grammar body ctx rng2))))

(define (expand-maybe grammar prob body ctx rng)
  (let* ([result (run-state random-float rng)]
         [f (car result)]
         [rng2 (cdr result)])
    (if (< f prob)
        (expand-body grammar body ctx rng2)
        (cons "" rng2))))

(define (expand-when grammar key body ctx rng)
  (let ([entry (assq key ctx)])
    (if (and entry (cdr entry))
        (expand-body grammar body ctx rng)
        (cons "" rng))))

(define (expand-case-on grammar key clauses ctx rng)
  ;; clauses: ((val1 body1) (val2 body2) ... (else body-else))
  (let* ([entry (assq key ctx)]
         [val (if entry (cdr entry) #f)])
    (let loop ([cs clauses])
      (cond
        [(null? cs) (cons "" rng)]
        [(eq? (caar cs) 'else)
         (expand-body grammar (cadar cs) ctx rng)]
        [(equal? (caar cs) val)
         (expand-body grammar (cadar cs) ctx rng)]
        ;; Also match string-to-symbol for convenience
        [(and (string? val) (eq? (caar cs) (string->symbol val)))
         (expand-body grammar (cadar cs) ctx rng)]
        [(and (symbol? val) (string? (caar cs)) (string=? (caar cs) (symbol->string val)))
         (expand-body grammar (cadar cs) ctx rng)]
        [else (loop (cdr cs))]))))

(define (expand-join grammar sep items-rule ctx rng)
  ;; items-rule should be a rule name whose body is (alt ...) or similar
  ;; For join, we look up the context key as a list and join with sep
  (let ([entry (assq items-rule ctx)])
    (if (and entry (list? (cdr entry)))
        (cons (let loop ([items (cdr entry)] [acc ""])
                (if (null? items)
                    acc
                    (loop (cdr items)
                          (if (string=? acc "")
                              (format "~a" (car items))
                              (string-append acc sep (format "~a" (car items)))))))
              rng)
        ;; If not a context list, just expand the rule
        (expand grammar items-rule ctx rng))))

;;; --- Convenience ---

(define (expand-to-string grammar rule-name ctx seed)
  (doc 'type '(-> Grammar Symbol Alist Int String))
  (doc 'description "Expand a rule with a seed, returning just the string")
  (car (expand grammar rule-name ctx (make-pcg seed 0))))

(define (expand-n grammar rule-name ctx seed n)
  (doc 'type '(-> Grammar Symbol Alist Int Int (List String)))
  (doc 'description "Generate n expansions with consecutive seeds")
  (let loop ([i 0] [acc '()])
    (if (>= i n)
        (reverse acc)
        (loop (+ i 1)
              (cons (expand-to-string grammar rule-name ctx (+ seed i))
                    acc)))))
