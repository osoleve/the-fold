;;; tag-parser.ss
;;; Extract and parse agent consultation tags from forum posts
;;;
;;; Supports tags like:
;;;   @opus architecture
;;;   @pedagogue help explain category theory
;;;   @archivist research prior work on DSLs
;;;
;;; Tag format:
;;;   @agent-name topic-keywords
;;;   where agent-name is lowercase with hyphens
;;;   and topic-keywords can be multiple words

(define (extract-agent-tags text)
  "Extract @agent topic tags from post body text.
   Returns: ((agent . \"topic\") ...) or empty list if no tags found"
  (let* ([lines (string-split text #\newline)]
         [tags '()])
        
        (for-each
         (lambda (line)
                 ;; Match @agent-name followed by text until end of line or another tag
                 (let loop ([pos 0])
                      (let ([at-pos (string-index line #\@ pos)])
                           (if at-pos
                               (let* ([after-at (substring line (+ at-pos 1) (string-length line))]
                                      ;; Extract agent name: lowercase alphanumeric + hyphens
                                      [match (regexp-match "^([a-z][a-z0-9-]*) (.+)$" after-at)])
                                     (if match
                                         (let ([agent (cadr match)]
                                               [topic (caddr match)])
                                              ;; Only capture known agents
                                              (if (member agent '("opus" "pedagogue" "archivist"))
                                                  (set! tags (cons (cons agent topic) tags))))
                                         #f)
                                     (loop (+ at-pos 1)))
                               #f))))
         lines)
        
        ;; Return in reverse order (first found first)
        (reverse tags)))

(define (extract-mentions text)
  "Extract @username mentions from post body.
   Returns: (\"username1\" \"username2\" ...) or empty list"
  (let* ([pattern (regexp "(@[a-z0-9_][a-z0-9_-]*)" (re-icase))]
         [matches (regexp-match-all pattern text)])
        (map
         (lambda (match)
                 (let ([mention (car match)])
                      (substring mention 1 (string-length mention))))  ;; Remove the @ symbol
         matches)))

(define (validate-tag tag-string)
  "Check if tag string is safe (no shell escapes, path traversal, etc).
   Returns: tag-string if valid, #f otherwise"
  (cond
   [(> (string-length tag-string) 100) #f]  ;; Too long
   [(string-contains tag-string "..") #f]   ;; Path traversal
   [(string-contains tag-string "/") #f]    ;; Path separator
   [(string-contains tag-string "$(") #f]   ;; Command substitution
   [(string-contains tag-string "`") #f]    ;; Backtick substitution
   [(string-contains tag-string ";") #f]    ;; Shell command separator
   [(string-contains tag-string "&") #f]    ;; Shell background/redirect
   [(string-contains tag-string "|") #f]    ;; Shell pipe
   [(string-contains tag-string ">") #f]    ;; Shell redirect
   [(string-contains tag-string "<") #f]    ;; Shell redirect
   [(string-contains text "\\") #f]         ;; Escape character
   [else tag-string]))

(define (extract-agent-consultation-tags text)
  "Extract all agent consultation tags and return as structured data.
   Returns: ((agent . \"opus\") (topic . \"architecture and strategy\") ...) or #f"
  (let ([tags (extract-agent-tags text)])
       (if (null? tags)
           #f
           (map
            (lambda (tag-pair)
                    (let ([agent (car tag-pair)]
                          [topic (cdr tag-pair)])
                         `((agent . ,agent)
                           (topic . ,topic)
                           (valid? . ,(validate-tag topic)))))
            tags))))

;;; Functions are available after loading:
;;; - extract-agent-tags
;;; - extract-mentions
;;; - validate-tag
;;; - extract-agent-consultation-tags
