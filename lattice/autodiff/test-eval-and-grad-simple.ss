(load "core/lang/eval.ss")

(display "Testing eval-and-grad...\n")

; First test: create a traced variable manually
(display "Creating traced variable...\n")
(define tape (make-reverse-tape))
(define x-var (make-traced-var 5 tape))
(display "Traced variable created: ") (display x-var) (newline)
(display "Is it traced? ") (display (traced? x-var)) (newline)
(display "Value: ") (display (traced-value x-var)) (newline)

; Test env-extend
(display "Extending environment...\n")
(define env* (env-extend empty-env 'x x-var))
(display "Looking up x...\n")
(define lookup-result (env-lookup env* 'x))
(display "Lookup result: ") (display lookup-result) (newline)

; Test evaluation
(display "Evaluating 'x...\n")
(define eval-result (eval-expr-traced 'x env* 1000 tape))
(display "Eval result: ") (display eval-result) (newline)

(display "\nNow testing eval-and-grad...\n")
(call-with-values
 (lambda () (eval-and-grad 'x empty-env '(x) '(5) 1000))
 (lambda (val grads)
         (display "Value: ") (display val) (newline)
         (display "Grads: ") (display grads) (newline)))
