;;; boundary/pipeline/effects/http.ss — HTTP Effect Handler
;;;
;;; Handles HTTP requests using curl.
;;;
;;; This is Shell code: handles IO, may fail, contains defensive logic.

(load "lattice/pipeline/stage.ss")
(load "lattice/pipeline/effects.ss")
(load "lattice/pipeline/context.ss")
(load "boundary/pipeline/effects/shell.ss")
(load "boundary/io/json.ss")

;;; ====
;;; HTTP Effect Interpretation
;;; ====

;;; interpret-http-effect : Payload -> Context -> State -> Input -> (Result . State)
(define (interpret-http-effect payload ctx state input)
  (let ([op (car payload)]
        [url-template (cadr payload)])
       (case op
             [(get)
              (let* ([url (expand-template-with-ctx url-template ctx input)]
                     [result (http-fetch-get url)])
                    (if (http-result-ok? result)
                        (cons (stage-ok (http-result-body result)) state)
                        (cons (stage-err 'http-error
                                         (http-result-error result)
                                         result)
                              state)))]
             [(get-json)
              (let* ([url (expand-template-with-ctx url-template ctx input)]
                     [result (http-fetch-get url)])
                    (if (http-result-ok? result)
                        (let ([parsed (parse-json-string (http-result-body result))])
                             (if parsed
                                 (cons (stage-ok parsed) state)
                                 (cons (stage-err 'json-parse-error
                                                  "Failed to parse HTTP response as JSON"
                                                  (http-result-body result))
                                       state)))
                        (cons (stage-err 'http-error
                                         (http-result-error result)
                                         result)
                              state)))]
             [else
              (cons (stage-err 'unknown-http-op
                               (format "Unknown HTTP op: ~a" op)
                               payload)
                    state)])))

;;; ====
;;; Helper Functions
;;; ====

;;; expand-template-with-ctx : String -> Context -> Input -> String
(define (expand-template-with-ctx template ctx input)
  (let ([bindings (append (list (cons "input" input))
                          (map (lambda (p) (cons (symbol->string (car p)) (cdr p)))
                               (ctx-env ctx)))])
       (expand-template template bindings)))

;;; ====
;;; URL Validation (Security Critical)
;;; ====

;;; *allowed-url-schemes* : List of String
;;; Only these URL schemes are allowed for HTTP fetching.
(define *allowed-url-schemes* '("http" "https"))

;;; *blocked-hosts* : List of String
;;; Hostnames that are always blocked (case-insensitive).
(define *blocked-hosts* '("localhost" "localhost.localdomain"))

;;; url-scheme : String -> String | #f
;;; Extract the scheme from a URL (e.g., "https" from "https://example.com").
(define (url-scheme url)
  (let ([idx (string-index url #\:)])
       (if idx
           (string-downcase (substring url 0 idx))
           #f)))

;;; url-host : String -> String | #f
;;; Extract the host from a URL.
(define (url-host url)
  (let ([start (string-search url "://")])
       (if start
           (let* ([after-scheme (+ start 3)]
                  [rest (substring url after-scheme (string-length url))]
                  [end-host (or (string-index rest #\/)
                                (string-index rest #\:)
                                (string-index rest #\?)
                                (string-length rest))])
                 (string-downcase (substring rest 0 end-host)))
           #f)))

;;; string-index : String -> Char -> Integer | #f
;;; Find first occurrence of char in string.
(define (string-index str ch)
  (let ([len (string-length str)])
       (let loop ([i 0])
            (cond
             [(>= i len) #f]
             [(char=? (string-ref str i) ch) i]
             [else (loop (+ i 1))]))))

;;; string-search : String -> String -> Integer | #f
;;; Find first occurrence of needle in haystack.
(define (string-search haystack needle)
  (let ([hlen (string-length haystack)]
        [nlen (string-length needle)])
       (let loop ([i 0])
            (cond
             [(> (+ i nlen) hlen) #f]
             [(string=? (substring haystack i (+ i nlen)) needle) i]
             [else (loop (+ i 1))]))))

;;; string-downcase : String -> String
;;; Convert string to lowercase.
(define (string-downcase s)
  (list->string (map char-downcase (string->list s))))

;;; private-ip? : String -> Boolean
;;; Check if a hostname looks like a private/internal IP address.
(define (private-ip? host)
  (or (string=? host "127.0.0.1")
      (string-prefix? host "10.")
      (string-prefix? host "192.168.")
      ;; 172.16.0.0 - 172.31.255.255
      (and (string-prefix? host "172.")
           (let ([rest (substring host 4 (string-length host))])
                (let ([dot (string-index rest #\.)])
                     (and dot
                          (let ([second-octet (string->number (substring rest 0 dot))])
                               (and second-octet
                                    (>= second-octet 16)
                                    (<= second-octet 31)))))))
      ;; IPv6 loopback
      (string=? host "::1")
      (string-prefix? host "[::1]")))

;;; string-prefix? : String -> String -> Boolean
;;; Check if string starts with prefix.
(define (string-prefix? str prefix)
  (let ([slen (string-length str)]
        [plen (string-length prefix)])
       (and (>= slen plen)
            (string=? (substring str 0 plen) prefix))))

;;; validate-url : String -> (ok String) | (err String)
;;; Validate a URL for safe fetching. Returns (ok url) or (err message).
(define (validate-url url)
  (let ([scheme (url-scheme url)]
        [host (url-host url)])
       (cond
        [(not scheme)
         (list 'err "Invalid URL: no scheme found")]
        [(not (member scheme *allowed-url-schemes*))
         (list 'err (format "Blocked URL scheme: ~a (only http/https allowed)" scheme))]
        [(not host)
         (list 'err "Invalid URL: no host found")]
        [(member host *blocked-hosts*)
         (list 'err (format "Blocked host: ~a" host))]
        [(private-ip? host)
         (list 'err (format "Blocked private/internal IP: ~a" host))]
        [else
         (list 'ok url)])))

;;; ====
;;; HTTP Implementation
;;; ====

;;; http-fetch-get : String -> HTTPResult
;;; Fetch content from a URL using curl.
;;; Returns HTTPResult with body on success, error on failure.
;;; SECURITY: URL is validated to prevent SSRF attacks.
(define (http-fetch-get url)
  (guard (ex [else
              (list 'http-result #f #f
                    (format "http-fetch-get error: ~a"
                            (if (message-condition? ex)
                                (condition-message ex)
                                "unknown error")))])
         ;; Validate URL before fetching
         (let ([validation (validate-url url)])
              (if (eq? (car validation) 'err)
                  (list 'http-result #f #f (cadr validation))
                  ;; Use curl with silent mode and fail on HTTP errors
                  (let ([result (shell-exec (format "curl -sS -f ~s" url))])
                       (if (shell-result-ok? result)
                           (list 'http-result #t (shell-result-stdout result) #f)
                           (list 'http-result #f #f (shell-result-stderr result))))))))

(define (http-result-ok? r) (list-ref r 1))
(define (http-result-body r) (list-ref r 2))
(define (http-result-error r) (list-ref r 3))
