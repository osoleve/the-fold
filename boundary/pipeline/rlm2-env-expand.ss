;;; @module rlm2-env-expand
;;; @description Environment reference expansion and code splitting for RLM eval.
;;; Loaded by rlm2-drive.ss — requires rlm2 state accessors.

;;; ====
;;; Env Reference Expansion (ported from v1)
;;; ====
;;;
;;; Inside (eval ...) and (store ...) expressions, the model may write
;;; (retrieve 'key) to reference env values. We pre-expand these to
;;; their actual values before sending to IPC.

;;; Bare symbol expansion: after (store 'entries val), models can write
;;; (filter pred entries) instead of (filter pred (retrieve 'entries)).
;;; The driver auto-inlines non-chunked stored values (<50K chars).
;;; Scope tracking prevents expanding lambda-bound variables.
(define (rlm2-expand-env-refs expr env)
  (rlm2-expand-env-refs* expr env '()))

(define (rlm2-expand-env-refs* expr env bound)
  (cond
    ;; Bare symbol matching a non-chunked env key (not locally bound)
    [(and (symbol? expr)
          (not (memq expr bound))
          (let ([entry (rlm-env-get env expr)])
            (and entry
                 (not (eq? (cadr entry) 'chunks))
                 (< (caddr entry) 50000))))
     (let ([val (rlm-env-fetch env expr)])
       (if val (rlm2-quote-if-needed val) expr))]
    ;; (retrieve 'key) -> expanded value
    [(and (pair? expr)
          (eq? (car expr) 'retrieve)
          (pair? (cdr expr))
          (rlm2-literal-arg? (cadr expr)))
     (let* ([key (rlm2-unquote-key (cadr expr))]
            [val (rlm-env-fetch env key)])
       (if val
           (rlm2-quote-if-needed val)
           expr))]
    ;; (peek 'key n) -> expanded preview
    [(and (pair? expr)
          (eq? (car expr) 'peek)
          (>= (length (cdr expr)) 2)
          (rlm2-literal-arg? (cadr expr)))
     (let* ([key (rlm2-unquote-key (cadr expr))]
            [n (caddr expr)]
            [val (rlm-env-peek env key n)])
       (if val (rlm2-quote-if-needed val) expr))]
    ;; (grep 'key pattern k) -> expanded results
    [(and (pair? expr)
          (eq? (car expr) 'grep)
          (>= (length (cdr expr)) 2)
          (for-all rlm2-literal-arg? (cdr expr)))
     (let* ([key (rlm2-unquote-key (cadr expr))]
            [pattern (caddr expr)]
            [k (if (>= (length (cdr expr)) 3) (cadddr expr) 5)]
            [results (rlm-env-grep env key pattern k)])
       (if results
           (rlm2-quote-if-needed (map car results))
           expr))]
    ;; Also handle v1-style (rlm-env-get 'key) for compatibility in eval blocks
    [(and (pair? expr)
          (memq (car expr) '(rlm-env-get env-get))
          (pair? (cdr expr))
          (rlm2-literal-arg? (cadr expr)))
     (let* ([key (rlm2-unquote-key (cadr expr))]
            [val (rlm-env-fetch env key)])
       (if val
           (rlm2-quote-if-needed val)
           expr))]
    ;; Don't expand inside quote
    [(and (pair? expr) (eq? (car expr) 'quote))
     expr]
    ;; Lambda: track params as locally bound
    [(and (pair? expr) (eq? (car expr) 'lambda) (pair? (cdr expr)))
     (let* ([params (cadr expr)]
            [param-syms (cond
                          [(list? params) params]
                          [(symbol? params) (list params)]  ; rest-arg
                          [else '()])]
            [bound* (append param-syms bound)])
       (cons 'lambda
             (cons params
                   (map (lambda (e) (rlm2-expand-env-refs* e env bound*))
                        (cddr expr)))))]
    ;; Recurse into sub-expressions
    [(pair? expr)
     (cons (rlm2-expand-env-refs* (car expr) env bound)
           (rlm2-expand-env-refs* (cdr expr) env bound))]
    [else expr]))

(define (rlm2-literal-arg? x)
  (or (number? x) (string? x) (boolean? x) (char? x)
      (null? x)
      (and (pair? x) (eq? (car x) 'quote))))

(define (rlm2-unquote-key arg)
  (if (and (pair? arg) (eq? (car arg) 'quote))
      (cadr arg)
      arg))

(define (rlm2-quote-if-needed val)
  (if (or (number? val) (string? val) (boolean? val) (char? val))
      val
      (list 'quote val)))

;;; ====
;;; Code Splitting (ported from v1)
;;; ====

(define (rlm2-split-code-exprs code-text)
  (guard (ex [else (list code-text)])
    (let ([port (open-input-string code-text)])
      (let loop ([exprs '()])
        (let skip-ws ()
          (let ([c (peek-char port)])
            (cond
              [(eof-object? c) (void)]
              [(char-whitespace? c) (read-char port) (skip-ws)]
              [(char=? c #\;)
               (let skip-comment ()
                 (let ([c (read-char port)])
                   (unless (or (eof-object? c) (char=? c #\newline))
                     (skip-comment))))
               (skip-ws)]
              [else (void)])))
        (if (eof-object? (peek-char port))
            (reverse exprs)
            (let ([expr (read port)])
              (if (eof-object? expr)
                  (reverse exprs)
                  (loop (cons (format "~s" expr) exprs)))))))))
