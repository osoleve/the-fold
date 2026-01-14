;;; playpen/quill/render.ss — Quill rendering
;;;
;;; Rendering is pure: given a Story + Run, produce a display string.
;;; Advanced formatting (wrapping, styles, transcript views) comes later.

(define *quill-wrap-width* 72)

;;; ====
;;; Text shaping helpers
;;; ====

(define (quill-alist-ref alist key default)
  (let ([p (assq key alist)])
       (if p (cdr p) default)))

(define (quill-first-line text)
  (let ([idx (string-index-of text "\n")])
       (if idx (substring text 0 idx) text)))

(define (quill-split-lines text)
  (string-split text "\n"))

(define (quill-split-words text)
  (let loop ([chars (string->list text)] [cur '()] [words '()])
       (cond
        [(null? chars)
         (reverse (if (null? cur) words (cons (list->string (reverse cur)) words)))]
        [(char-whitespace? (car chars))
         (loop (cdr chars) '()
               (if (null? cur) words (cons (list->string (reverse cur)) words)))]
        [else
         (loop (cdr chars) (cons (car chars) cur) words)])))

(define (quill-wrap-words words width)
  (cond
   [(or (not width) (<= width 0))
    (list (string-join words " "))]
   [else
    (let loop ([ws words] [line ""] [lines '()])
         (cond
          [(null? ws)
           (reverse (if (string-blank? line) lines (cons line lines)))]
          [else
           (let* ([w (car ws)]
                  [candidate (if (string-blank? line) w (string-append line " " w))])
                 (if (> (string-length candidate) width)
                     (loop (cdr ws) w (if (string-blank? line) lines (cons line lines)))
                     (loop (cdr ws) candidate lines)))]))]))

(define (quill-wrap-text text width)
  (let loop ([lines (quill-split-lines text)] [acc '()])
       (cond
        [(null? lines) (string-join (reverse acc) "\n")]
        [(string-blank? (car lines))
         (loop (cdr lines) (cons "" acc))]
        [else
         (let* ([words (quill-split-words (car lines))]
                [wrapped (quill-wrap-words words width)]
                [block (string-join wrapped "\n")])
               (loop (cdr lines) (cons block acc)))])))

(define (quill-prefix-lines prefix lines)
  (let ([indent (make-string (string-length prefix) #\space)])
       (if (null? lines)
           (list prefix)
           (cons (string-append prefix (car lines))
                 (map (lambda (line) (string-append indent line)) (cdr lines))))))

;;; ====
;;; Template rendering
;;; ====

(define *quill-template-missing* '__quill-missing)

(define (quill-state-var-entry state key)
  (assq key (quill-state-section state 'vars)))

(define (quill-state-flag-entry state key)
  (assq key (quill-state-section state 'flags)))

(define (quill-state-meta-entry state key)
  (assq key (quill-state-section state 'meta)))

(define (quill-template-split token)
  (let* ([trimmed (string-trim token)]
         [idx (string-index-of trimmed ":")])
        (if idx
            (list (string-downcase (string-trim (substring trimmed 0 idx)))
                  (string-trim (substring trimmed (+ idx 1) (string-length trimmed))))
            (list "" trimmed))))

(define (quill-template-value->string value)
  (cond
   [(string? value) value]
   [(symbol? value) (symbol->string value)]
   [(boolean? value) (if value "true" "false")]
   [else (format "~a" value)]))

(define (quill-template-var state name)
  (let* ([sym (and (not (string-blank? name)) (string->symbol name))]
         [entry (and sym (quill-state-var-entry state sym))])
        (if entry
            (cdr entry)
            (let* ([sym2 (and (not (string-blank? name)) (string->symbol (string-downcase name)))]
                   [entry2 (and sym2 (quill-state-var-entry state sym2))])
                  (if entry2 (cdr entry2) *quill-template-missing*)))))

(define (quill-template-flag state name)
  (let* ([sym (and (not (string-blank? name)) (string->symbol name))]
         [entry (and sym (quill-state-flag-entry state sym))])
        (if entry (and (cdr entry) #t) *quill-template-missing*)))

(define (quill-template-meta state name)
  (let* ([sym (and (not (string-blank? name)) (string->symbol name))]
         [entry (and sym (quill-state-meta-entry state sym))])
        (if entry (cdr entry) *quill-template-missing*)))

(define (quill-template-inventory state)
  (let ([inv (reverse (quill-state-inventory state))])
       (if (null? inv)
           "empty"
           (string-join (map (lambda (x) (format "~a" x)) inv) ", "))))

(define (quill-template-flag-list state)
  (let* ([flags (quill-state-section state 'flags)]
         [active (filter (lambda (p) (cdr p)) flags)])
        (if (null? active)
            "none"
            (string-join (map (lambda (p) (format "~a" (car p))) active) ", "))))

(define (quill-template-value token story run node choice)
  (let* ([state (quill-run-state run)]
         [parts (quill-template-split token)]
         [prefix (car parts)]
         [name (cadr parts)])
        (cond
         [(string=? prefix "var") (quill-template-var state name)]
         [(string=? prefix "flag") (quill-template-flag state name)]
         [(string=? prefix "meta") (quill-template-meta state name)]
         [(string=? prefix "story")
          (cond
           [(string=? (string-downcase name) "id") (quill-story-id story)]
           [(string=? (string-downcase name) "title") (quill-story-title story)]
           [else *quill-template-missing*])]
         [(string=? prefix "node")
          (if node
              (cond
               [(string=? (string-downcase name) "id") (quill-node-id node)]
               [(string=? (string-downcase name) "title") (quill-node-title node)]
               [else *quill-template-missing*])
              *quill-template-missing*)]
         [else
          (cond
           [(string=? (string-downcase name) "inventory") (quill-template-inventory state)]
           [(string=? (string-downcase name) "inv") (quill-template-inventory state)]
           [(string=? (string-downcase name) "inv-count")
            (length (quill-state-inventory state))]
           [(string=? (string-downcase name) "flags") (quill-template-flag-list state)]
           [(string=? (string-downcase name) "clock")
            (quill-state-meta-get state 'clock 0)]
           [(string=? (string-downcase name) "time")
            (quill-state-meta-get state 'clock 0)]
           [(string=? (string-downcase name) "node")
            (if node (quill-node-id node) *quill-template-missing*)]
           [(string=? (string-downcase name) "node-id")
            (if node (quill-node-id node) *quill-template-missing*)]
           [(string=? (string-downcase name) "node-title")
            (if node (quill-node-title node) *quill-template-missing*)]
           [(string=? (string-downcase name) "story") (quill-story-id story)]
           [(string=? (string-downcase name) "story-id") (quill-story-id story)]
           [(string=? (string-downcase name) "story-title") (quill-story-title story)]
           [(string=? (string-downcase name) "turn")
            (+ 1 (length (quill-run-transcript run)))]
           [else (quill-template-var state name)])])))

(define (quill-render-template text story run node choice)
  (let loop ([s text] [acc ""])
       (let ([start (string-index-of s "{{")])
            (if (not start)
                (string-append acc s)
                (let* ([after (substring s (+ start 2) (string-length s))]
                       [end (string-index-of after "}}")])
                      (if (not end)
                          (string-append acc s)
                          (let* ([token (substring after 0 end)]
                                 [value (quill-template-value token story run node choice)]
                                 [rendered (if (eq? value *quill-template-missing*)
                                               (string-append "{{" token "}}")
                                               (quill-template-value->string value))])
                                (loop (substring after (+ end 2) (string-length after))
                                      (string-append acc (substring s 0 start) rendered)))))))))

(define (quill-node-body->string body story run node)
  (let* ([state (quill-run-state run)]
         [raw (cond
               [(string? body) body]
               [(procedure? body) (body state)]
               [else (format "~a" body)])])
        (quill-render-template (format "~a" raw) story run node #f)))

(define (quill-choice-visible? choice state)
  (let ((g (quill-choice-guard choice)))
       (cond
        ((not g) #t)
        ((eq? g #t) #t)
        ((procedure? g) (and (g state) #t))
        (else #t))))

(define (quill-visible-choices node state)
  (filter (lambda (c) (quill-choice-visible? c state))
          (quill-node-choices node)))

(define (quill-choice->lines choice i story run node width)
  (let* ([prefix (string-append "  " (number->string i) ". ")]
         [label-raw (format "~a" (quill-choice-label choice))]
         [label (quill-render-template label-raw story run node choice)]
         [avail (if (and width (> width (string-length prefix)))
                    (- width (string-length prefix))
                    width)]
         [wrapped (quill-wrap-text label avail)]
         [lines (quill-split-lines wrapped)])
        (quill-prefix-lines prefix lines)))

(define (quill-choices->string choices story run node width)
  (let loop ([cs choices] [i 1] [acc '()])
       (if (null? cs)
           (if (null? acc) "" (string-append (string-join (reverse acc) "\n") "\n"))
           (let* ([lines (quill-choice->lines (car cs) i story run node width)]
                  [block (string-join lines "\n")])
                 (loop (cdr cs) (+ i 1) (cons block acc))))))

(define (quill-dialogue-choice->lines choice i story run node width)
  (let* ([prefix (string-append "  " (number->string i) ". ")]
         [label-raw (format "~a" (quill-dialogue-choice-label choice))]
         [label (quill-render-template label-raw story run #f choice)]
         [avail (if (and width (> width (string-length prefix)))
                    (- width (string-length prefix))
                    width)]
         [wrapped (quill-wrap-text label avail)]
         [lines (quill-split-lines wrapped)])
        (quill-prefix-lines prefix lines)))

(define (quill-dialogue-choices->string choices story run node width)
  (let loop ([cs choices] [i 1] [acc '()])
       (if (null? cs)
           (if (null? acc) "" (string-append (string-join (reverse acc) "\n") "\n"))
           (let* ([lines (quill-dialogue-choice->lines (car cs) i story run node width)]
                  [block (string-join lines "\n")])
                 (loop (cdr cs) (+ i 1) (cons block acc))))))

(define (quill-render-dialogue story run node width)
  (let* ([speaker-raw (format "~a" (quill-dialogue-node-speaker node))]
         [speaker (quill-render-template speaker-raw story run #f node)]
         [text-raw (format "~a" (quill-dialogue-node-text node))]
         [text (quill-render-template text-raw story run #f node)]
         [choices (quill-dialogue-visible-choices node (quill-run-state run))]
         [choices-text (if (null? choices)
                           ""
                           (quill-dialogue-choices->string choices story run node width))])
        (string-append
         (if (and speaker (not (string-blank? speaker)))
             (string-append speaker "\n" (make-string (string-length (quill-first-line speaker)) #\-) "\n\n")
             "")
         (quill-wrap-text text width)
         "\n\n"
         choices-text
         (if (null? choices)
             "\n(Type 'quit' to exit dialogue)\n"
             "\n(Choose a number, or 'help')\n"))))

(define (quill-render story run . width-opt)
  (let* ([node (quill-story-node story (quill-run-node-id run))]
         [width (if (null? width-opt) *quill-wrap-width* (car width-opt))]
         [state (quill-run-state run)]
         [msg (quill-run-message run)])
        (cond
         [(not node)
          (string-append "Quill error: missing node "
                         (format "~a" (quill-run-node-id run))
                         "\n")]
         [else
          (let* ([dialogue-node (quill-dialogue-current-node story run)]
                 [title-raw (format "~a" (quill-node-title node))]
                 [title (quill-render-template title-raw story run node #f)]
                 [body (quill-node-body->string (quill-node-body node) story run node)]
                 [choices (quill-visible-choices node state)]
                 [choices-text (if (null? choices) "" (quill-choices->string choices story run node width))]
                 [msg-text (and msg (not (string-blank? msg))
                                (quill-render-template (format "~a" msg) story run node #f))]
                 [msg-block (if msg-text (string-append (quill-wrap-text msg-text width) "\n\n") "")])
                (string-append
                 msg-block
                 (if dialogue-node
                     (quill-render-dialogue story run dialogue-node width)
                     (string-append
                      (if (and title (not (string-blank? title)))
                          (string-append title "\n" (make-string (string-length (quill-first-line title)) #\-) "\n\n")
                          "")
                      (quill-wrap-text body width)
                      "\n\n"
                      choices-text
                      (cond
                       [(quill-run-done? run) "\n[The End]\n"]
                       [(null? choices) "\n(Type 'quit' to exit)\n"]
                       [else "\n(Choose a number, or 'help')\n"]))))) ])))

;;; ====
;;; Transcript rendering
;;; ====

(define (quill-indent-lines lines indent)
  (map (lambda (line) (string-append indent line)) lines))

(define (quill-render-transcript-entry entry index)
  (let* ([node (quill-alist-ref entry 'node #f)]
         [input (quill-alist-ref entry 'input "")]
         [intent (quill-alist-ref entry 'intent '())]
         [output (quill-alist-ref entry 'output "")]
         [output-lines (quill-indent-lines (quill-split-lines (format "~a" output)) "    ")])
        (string-append
         "Turn " (number->string index) "\n"
         "  node: " (format "~a" node) "\n"
         "  input: " (format "~s" input) "\n"
         "  intent: " (format "~a" intent) "\n"
         "  output:\n"
         (string-join output-lines "\n"))))

(define (quill-render-transcript run)
  (let loop ([entries (reverse (quill-run-transcript run))] [i 1] [acc '()])
       (if (null? entries)
           (if (null? acc) "" (string-join (reverse acc) "\n\n"))
           (loop (cdr entries) (+ i 1)
                 (cons (quill-render-transcript-entry (car entries) i) acc)))))
