(load "boundary/flashmob/flashmob.ss")

(doc 'module 'flashmob/report)
(doc 'description "Flashmob JSON Report Export - Exports triage results to JSON format for compatibility with external tools")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'note "Usage: (flashmob-report) prints JSON to stdout, (flashmob-report-to-file filename) writes to file, (flashmob-report-data) returns Scheme data")

;;; ====
;;; Report Data Generation
;;; ====

;;; flashmob-report-data : -> Alist
;;; Generate report data structure.
(define (flashmob-report-data)
  (unless *flashmob-session-triage*
    (error 'flashmob-report-data "No triage results"))
  (let* ([triage *flashmob-session-triage*]
         [strategy (cdr (assq 'strategy triage))]
         [ranking (cdr (assq 'ranking triage))]
         [selected (cdr (assq 'selected triage))]
         [consensus (cdr (assq 'consensus triage))]
         [credits (cdr (assq 'credits triage))]
         [power (cdr (assq 'power triage))])
    `((version . "1.0.0")
      (generated-at . ,(report-timestamp))
      (session . ,*flashmob-current-session*)
      (strategy . ,strategy)
      (summary . ((total-findings . ,(length *flashmob-session-findings*))
                  (selected-count . ,(length selected))
                  (agent-count . ,(length *flashmob-session-agents*))
                  (consensus . ,consensus)))
      (findings . ,(map report-finding-to-json selected))
      (ranking . ,(map report-finding-summary ranking))
      (attribution . ,(map report-credit-to-json credits))
      (power-indices . ,(map report-power-to-json power))
      (agents . ,(map report-agent-to-json *flashmob-session-agents*)))))

;;; report-finding-to-json : Alist -> Alist
;;; Convert finding to JSON-friendly format.
(define (report-finding-to-json f)
  `((title . ,(cdr (assq 'title f)))
    (file . ,(cdr (assq 'file f)))
    (line . ,(cdr (assq 'line f)))
    (severity . ,(symbol->string (cdr (assq 'severity f))))
    (category . ,(symbol->string (cdr (assq 'category f))))
    (confidence . ,(cdr (assq 'confidence f)))
    (agent . ,(symbol->string (cdr (assq 'agent-id f))))
    (description . ,(cdr (assq 'description f)))))

;;; report-finding-summary : Alist -> Alist
;;; Compact finding summary for ranking list.
(define (report-finding-summary f)
  `((title . ,(cdr (assq 'title f)))
    (file . ,(cdr (assq 'file f)))
    (line . ,(cdr (assq 'line f)))
    (severity . ,(symbol->string (cdr (assq 'severity f))))))

;;; report-credit-to-json : (Cons Symbol Real) -> Alist
(define (report-credit-to-json c)
  `((agent . ,(symbol->string (car c)))
    (credit . ,(cdr c))))

;;; report-power-to-json : (Cons Symbol Real) -> Alist
(define (report-power-to-json p)
  `((agent . ,(symbol->string (car p)))
    (power . ,(cdr p))))

;;; report-agent-to-json : Symbol -> Alist
(define (report-agent-to-json agent-id)
  (let ([data (flashmob-get-agent agent-id)])
    (if data
        `((id . ,(symbol->string agent-id))
          (type . ,(cdr (assq 'agent-type data)))
          (expertise . ,(map (lambda (e)
                               `(,(symbol->string (car e)) . ,(cdr e)))
                             (cdr (assq 'expertise data)))))
        `((id . ,(symbol->string agent-id))
          (type . "unknown")
          (expertise . ())))))

;;; report-timestamp : -> String
(define (report-timestamp)
  (let ([t (current-date)])
    (format "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
            (date-year t)
            (date-month t)
            (date-day t)
            (date-hour t)
            (date-minute t)
            (date-second t))))

;;; ====
;;; JSON Serialization
;;; ====

;;; json-serialize : Any -> String
;;; Convert Scheme value to JSON string.
(define (json-serialize val)
  (cond
    [(null? val) "null"]
    [(eq? val #t) "true"]
    [(eq? val #f) "false"]
    [(number? val) (json-number->string val)]
    [(string? val) (json-escape-string val)]
    [(symbol? val) (json-escape-string (symbol->string val))]
    [(pair? val)
     (if (json-alist? val)
         (json-serialize-object val)
         (json-serialize-array val))]
    [(vector? val)
     (json-serialize-array (vector->list val))]
    [else (json-escape-string (format "~s" val))]))

;;; json-alist? : Any -> Boolean
;;; Check if value is an association list.
(define (json-alist? val)
  (and (pair? val)
       (pair? (car val))
       (or (symbol? (caar val))
           (string? (caar val)))))

;;; json-serialize-object : Alist -> String
(define (json-serialize-object alist)
  (string-append
   "{"
   (string-join
    (map (lambda (pair)
           (string-append
            (json-escape-string
             (if (symbol? (car pair))
                 (symbol->string (car pair))
                 (car pair)))
            ": "
            (json-serialize (cdr pair))))
         alist)
    ", ")
   "}"))

;;; json-serialize-array : List -> String
(define (json-serialize-array lst)
  (string-append
   "["
   (string-join (map json-serialize lst) ", ")
   "]"))

;;; json-escape-string : String -> String
;;; Properly escapes all JSON special characters including Unicode.
(define (json-escape-string str)
  (string-append
   "\""
   (let loop ([chars (string->list str)] [acc '()])
     (if (null? chars)
         (list->string (reverse acc))
         (let* ([c (car chars)]
                [code (char->integer c)])
           (cond
             ;; Required JSON escapes
             [(char=? c #\") (loop (cdr chars) (append (list #\" #\\) acc))]
             [(char=? c #\\) (loop (cdr chars) (append (list #\\ #\\) acc))]
             [(char=? c #\newline) (loop (cdr chars) (append (list #\n #\\) acc))]
             [(char=? c #\return) (loop (cdr chars) (append (list #\r #\\) acc))]
             [(char=? c #\tab) (loop (cdr chars) (append (list #\t #\\) acc))]
             ;; Other control characters (0x00-0x1F) as \uXXXX
             [(< code 32)
              (loop (cdr chars)
                    (append (reverse (string->list (json-unicode-escape code))) acc))]
             ;; DEL and above 0x7F - escape non-ASCII for safety
             [(> code 127)
              (loop (cdr chars)
                    (append (reverse (string->list (json-unicode-escape code))) acc))]
             ;; Printable ASCII - pass through
             [else (loop (cdr chars) (cons c acc))]))))
   "\""))

;;; json-unicode-escape : Int -> String
;;; Format a code point as \uXXXX (handles BMP only; surrogates for >0xFFFF).
(define (json-unicode-escape code)
  (if (> code #xFFFF)
      ;; Surrogate pair for characters outside BMP
      (let* ([adjusted (- code #x10000)]
             [high (+ #xD800 (bitwise-arithmetic-shift-right adjusted 10))]
             [low (+ #xDC00 (bitwise-and adjusted #x3FF))])
        (string-append (json-unicode-escape high) (json-unicode-escape low)))
      (format "\\u~4,'0x" code)))

;;; json-number->string : Number -> String
(define (json-number->string n)
  (if (exact? n)
      (number->string n)
      (let ([s (format "~,6f" n)])
        ;; Trim trailing zeros after decimal
        (let loop ([i (- (string-length s) 1)])
          (cond
            [(< i 0) s]
            [(char=? (string-ref s i) #\0)
             (loop (- i 1))]
            [(char=? (string-ref s i) #\.)
             (substring s 0 i)]
            [else
             (substring s 0 (+ i 1))])))))

;;; ====
;;; Output Functions
;;; ====

;;; flashmob-report : -> Void
;;; Print JSON report to stdout.
(define (flashmob-report)
  (let ([data (flashmob-report-data)])
    (display (json-serialize data))
    (newline)))

;;; flashmob-report-to-file : String -> Void
;;; Write JSON report to file.
(define (flashmob-report-to-file filename)
  (let ([data (flashmob-report-data)]
        [json (json-serialize (flashmob-report-data))])
    (call-with-output-file filename
      (lambda (port)
        (put-string port json)
        (newline port))
      '(replace))
    (printf "Report written to: ~a~n" filename)))

;;; flashmob-report-pretty : -> Void
;;; Print human-readable report summary.
(define (flashmob-report-pretty)
  (unless *flashmob-session-triage*
    (error 'flashmob-report-pretty "No triage results"))
  (let* ([data (flashmob-report-data)]
         [summary (cdr (assq 'summary data))]
         [findings (cdr (assq 'findings data))]
         [attribution (cdr (assq 'attribution data))])
    (printf "~a~n" (make-string 60 #\=))
    (printf "FLASHMOB QA REPORT~n")
    (printf "~a~n" (make-string 60 #\=))
    (printf "Session:    ~a~n" (cdr (assq 'session data)))
    (printf "Strategy:   ~a~n" (cdr (assq 'strategy data)))
    (printf "Generated:  ~a~n" (cdr (assq 'generated-at data)))
    (printf "~n")
    (printf "Summary:~n")
    (printf "  Total findings:  ~a~n" (cdr (assq 'total-findings summary)))
    (printf "  Selected:        ~a~n" (cdr (assq 'selected-count summary)))
    (printf "  Agents:          ~a~n" (cdr (assq 'agent-count summary)))
    (printf "  Consensus:       ~,1f%~n" (* 100 (cdr (assq 'consensus summary))))
    (printf "~n")
    (printf "Top Findings:~n")
    (printf "~a~n" (make-string 60 #\-))
    (for-each
     (lambda (f i)
       (printf "~a. [~a] ~a:~a - ~a~n"
               (+ i 1)
               (cdr (assq 'severity f))
               (cdr (assq 'file f))
               (cdr (assq 'line f))
               (cdr (assq 'title f))))
     findings
     (iota (length findings)))
    (printf "~n")
    (printf "Attribution:~n")
    (printf "~a~n" (make-string 40 #\-))
    (for-each
     (lambda (a)
       (printf "  ~a: ~,1f%~n"
               (cdr (assq 'agent a))
               (* 100 (cdr (assq 'credit a)))))
     (sort (lambda (a b)
             (> (cdr (assq 'credit a)) (cdr (assq 'credit b))))
           attribution))
    (printf "~a~n" (make-string 60 #\=))))

;; iota is provided by prelude (via flashmob.ss chain)
