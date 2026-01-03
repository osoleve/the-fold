;;; poll-helper.ss
;;; Helper script to poll for agent requests and output clean text format
;;; Usage: scheme --script poll-helper.ss <timestamp>
;;;
;;; Output format (one per request):
;;;   AGENT_REQUEST: <agent-name>
;;;   TOPIC: <topic>
;;;   AUTHOR: <author>
;;;   BODY: <body>
;;;   ---

(load "forum/polling-queries.ss")

(define (main)
  (let* ([args (command-line-arguments)]
         [last-check (if (null? args)
                         0
                         (or (string->number (car args)) 0))])  ; Handle invalid input
        
        (let ([posts (find-posts-with-agent-tags last-check)])
             (for-each
              (lambda (post)
                      (let ([tags (cdr (assoc 'tags post))]
                            [author (cdr (assoc 'author post))]
                            [body (cdr (assoc 'body post))]
                            [channel (cdr (assoc 'channel post))])
                           (for-each
                            (lambda (tag)
                                    ;; tag is (agent-name . topic)
                                    ;; Output full context for the agent
                                    (display "AGENT_REQUEST: ")
                                    (display (car tag))
                                    (newline)
                                    (display "TOPIC: ")
                                    (display (or (cdr tag) channel "unknown"))
                                    (newline)
                                    (display "AUTHOR: ")
                                    (display (or author "unknown"))
                                    (newline)
                                    (display "BODY: ")
                                    (display (or body ""))
                                    (newline)
                                    (display "---")
                                    (newline))
                            tags)))
              posts))))

(main)
