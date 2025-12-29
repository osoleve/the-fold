;;; playpen/quill/parse.ss — Quill command parsing
;;;
;;; This parser is intentionally small. It provides a stable intent model
;;; for the runtime while leaving room for richer parsing later.
;;;
;;; Depends on fabric/stitches/parse.ss (parser combinators).

(load "fabric/stitches/parse.ss")

;;; Helpers
(define (quill-parse-complete parser input)
  (let* ([trimmed (string-trim input)]
         [result (run-parser (seq-left parser spaces) trimmed)])
    (if (and (parse-ok? result)
             (string=? (result-remaining result) ""))
        (result-value result)
        #f)))

;;; Intent parsers
(define quill-intent-parser
  (choice
    (list
      ;; number => choose
      (map-p quill-intent-choose nat)

      ;; look / l
      (map-p (lambda (_) (quill-intent-look)) (alt (string-p "look") (string-p "l")))

      ;; help / ?
      (map-p (lambda (_) (quill-intent-help)) (alt (string-p "help") (string-p "?")))

      ;; quit / q
      (map-p (lambda (_) (quill-intent-quit)) (alt (string-p "quit") (string-p "q"))))))

(define (quill-parse s)
  (cond
    [(or (not s) (not (string? s)) (string-blank? s))
     (quill-intent-unknown "")]
    [else
     (or (quill-parse-complete quill-intent-parser s)
         (quill-intent-unknown (string-trim s)))]))
