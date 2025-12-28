;;; legacy-eval.ss - Minimal legacy evaluator for parity tests

(define fold-root (or (getenv "FOLD_ROOT") ".."))
(define stitches-dir (string-append fold-root "/fabric/stitches"))
(source-directories (cons stitches-dir (source-directories)))
(load (string-append stitches-dir "/eval.ss"))
(load (string-append stitches-dir "/sha256.ss"))

(define (read-all)
  (let loop ([acc '()])
    (let ([expr (read)])
      (if (eof-object? expr)
          (reverse acc)
          (loop (cons expr acc))))))

(define (eval-seq exprs env fuel)
  (if (null? exprs)
      `(ok () ,fuel)
      (let ([result (eval-expr (car exprs) env fuel)])
        (cond
          [(eq? (car result) 'ok)
           (if (null? (cdr exprs))
               result
               (eval-seq (cdr exprs) env (caddr result)))]
          [else result]))))

(define (env-or-default name default)
  (let ([value (getenv name)])
    (if value (string->number value) default)))

(define fuel (env-or-default "FOLD_FUEL" 10000))
(define exprs (read-all))
(define result (eval-seq exprs empty-env fuel))

(cond
  [(eq? (car result) 'ok)
   (display "=> ")
   (write (cadr result))
   (newline)]
  [(eq? (car result) 'suspended)
   (display "[suspended - out of fuel]\n")]
  [else
   (display "ERROR: ")
   (write result)
   (newline)])
