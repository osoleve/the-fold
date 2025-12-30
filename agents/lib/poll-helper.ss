;;; poll-helper.ss
;;; Helper script to poll for agent requests and output clean text format
;;; Usage: scheme --script poll-helper.ss <timestamp>

(load "forum/polling-queries.ss")

(define (main)
  (let* ([args (command-line-arguments)]
         [last-check (if (null? args) 0 (string->number (car args)))])
        
        (let ([posts (find-posts-with-agent-tags last-check)])
             (for-each
              (lambda (post)
                      (let ([tags (cdr (assoc 'tags post))])
                           (for-each
                            (lambda (tag)
                                    ;; tag is (agent-name . topic)
                                    (display "AGENT_REQUEST: ")
                                    (display (car tag))
                                    (newline))
                            tags)))
              posts))))

(main)
