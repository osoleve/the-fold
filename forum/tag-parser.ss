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
  "Extract @agent topic tags from post body text"
  (let ([words (string-split text #\space)])
       (let loop ([words words] [tags '()])
            (if (null? words)
                (reverse tags)
                (let ([word (car words)])
                     (if (and (> (string-length word) 1)
                              (char=? (string-ref word 0) #\@))
                         ;; Found @word
                         (let* ([agent-candidate (substring word 1 (string-length word))]
                                ;; Remove trailing punctuation if any
                                [agent (if (and (> (string-length agent-candidate) 0)
                                                (member (string-ref agent-candidate
                                                                    (- (string-length agent-candidate) 1))
                                                        '(#\, #\. #\: #\; #\! #\?)))
                                           (substring agent-candidate 0 (- (string-length agent-candidate) 1))
                                           agent-candidate)])
                               (if (member agent '("opus" "pedagogue" "archivist"))
                                   ;; Valid agent found
                                   (let ([topic (string-join (cdr words) " ")])
                                        (reverse (cons (cons agent topic) tags)))
                                   ;; Not a known agent, keep looking
                                   (loop (cdr words) tags)))
                         ;; Not @-prefixed, keep looking
                         (loop (cdr words) tags)))))))

(define (extract-mentions text)
  "Extract @username mentions from post body"
  '())

(define (validate-tag tag-string)
  "Check if tag string is safe"
  (if (> (string-length tag-string) 100)
      #f
      tag-string))

(define (extract-agent-consultation-tags text)
  "Extract all agent consultation tags and return as structured data"
  (let ([tags (extract-agent-tags text)])
       (if (null? tags)
           #f
           (map
            (lambda (tag-pair)
                    (let ([agent (car tag-pair)]
                          [topic (cdr tag-pair)])
                         (list (cons 'agent agent)
                               (cons 'topic topic)
                               (cons 'valid? (validate-tag topic)))))
            tags))))

;;; Functions are available after loading:
;;; - extract-agent-tags
;;; - extract-mentions
;;; - validate-tag
;;; - extract-agent-consultation-tags
