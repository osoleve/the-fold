;;; playpen/quill/repl.ss — Quill REPL helpers
;;;
;;; Minimal interactive runner. Advanced tooling (inspectors, debugging)
;;; lands in a dedicated tooling file later.

(define (quill-run story . state-opt)
  (quill-assert-valid! story)
  (let ([run (apply quill-start story state-opt)])
    ;; Apply on-enter for the start node once.
    (let loop ([r (quill-enter-node story run)])
      (display (quill-render story r))
      (newline)
      (if (quill-run-done? r)
          r
          (begin
            (display "> ")
            (flush-output-port (current-output-port))
            (let ([line (get-line (current-input-port))])
              (if (eof-object? line)
                  (quill-run-with-done r #t)
                  (let-values ([(r2 _out) (quill-step story r line)])
                    (loop r2)))))))))
