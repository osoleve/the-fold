(load "boundary/io/process.ss")
(load "boundary/flashmob/flashmob.ss")

;;; Module: flashmob/llm-review
;;; Description: LLM-powered code review integration for flashmob.
;;;              Routes to local llama-server (Nemotron) when available,
;;;              falls back to Gemini 3 Flash.
;;; Layer: boundary
;;;
;;; Usage:
;;;   (llm-review-backend)            ; => 'nemotron or 'gemini or #f
;;;   (llm-review-file "path/to/file.ss")
;;;   (llm-review-files '("file1.ss" "file2.ss"))

;;;; ============================================================
;;;; Configuration
;;;; ============================================================

(define *llm-review-system-prompt*
  "You are a senior QA engineer reviewing Scheme code for The Fold, a content-addressable homoiconic system.

Focus areas (in priority order):
1. CORRECTNESS: Logic bugs, off-by-one, null/nil handling, type mismatches
2. EDGE CASES: Empty lists, zero values, boundary conditions
3. ERROR HANDLING: Missing guards, unchecked returns, assertion gaps
4. PERFORMANCE: O(n^2) when O(n) possible, redundant traversals
5. STYLE: Only if it affects readability/maintainability

Output format (one finding per line):
LINE_NUMBER|SEVERITY|CATEGORY|TITLE|DESCRIPTION

Where:
- LINE_NUMBER: Integer line number
- SEVERITY: critical, high, medium, low, or info
- CATEGORY: correctness, security, performance, style, or documentation
- TITLE: Brief summary (max 60 chars)
- DESCRIPTION: Detailed explanation

Example:
280|critical|performance|Shapley value O(2^N) complexity|The game-shapley-attribution function iterates all 2^N coalitions, causing exponential blowup with more than 15 agents
45|medium|correctness|Division by zero possible|simple-consensus divides by total without checking for empty agent list

If no issues found, output exactly: NO_ISSUES_FOUND")

(define *llm-review-timeout* 120000)  ; 2 minutes
(define *llm-review-temperature* 0.3)

;;;; ============================================================
;;;; Backend Detection
;;;; ============================================================

;;; llm-review-check-llama : -> Boolean
;;; Check if llama-server is running by calling curl on its health endpoint.
(define (llm-review-check-llama)
  (let ([result (shell-capture-result
                 "curl -s -o /dev/null -w '%{http_code}' http://localhost:30000/health 2>/dev/null")])
    (and (process-ok? result)
         (string=? (string-trim (process-stdout result)) "200"))))

;;; llm-review-check-gemini : -> Boolean
;;; Check if gemini CLI is available.
(define (llm-review-check-gemini)
  (shell-run-quiet "which gemini"))

;;; llm-review-backend : -> (U Symbol Boolean)
;;; Detect the best available LLM backend.
;;; Returns 'nemotron if llama-server is running, 'gemini if available, #f if neither.
(define (llm-review-backend)
  (cond
    [(llm-review-check-llama) 'nemotron]
    [(llm-review-check-gemini) 'gemini]
    [else #f]))

;;;; ============================================================
;;;; LLM Invocation
;;;; ============================================================

;;; llm-review-call-nemotron : String -> String
;;; Call local llama-server with Nemotron for code review.
(define (llm-review-call-nemotron code-content)
  (let* ([payload (llm-review-build-nemotron-payload code-content)]
         [temp-file "/tmp/llm-review-payload.json"]
         [_ (call-with-output-file temp-file
              (lambda (port) (put-string port payload))
              '(replace))]
         [cmd (format "curl -s -X POST http://localhost:30000/v1/chat/completions -H 'Content-Type: application/json' -d @~a"
                      temp-file)]
         [result (shell-capture-result cmd)])
    (if (process-ok? result)
        (llm-review-extract-nemotron-response (process-stdout result))
        (error 'llm-review-call-nemotron
               (format "Nemotron call failed: ~a" (process-stderr result))))))

;;; llm-review-build-nemotron-payload : String -> String
;;; Build JSON payload for llama-server OpenAI-compatible API.
(define (llm-review-build-nemotron-payload code-content)
  (string-append
   "{"
   "\"model\": \"nemotron\","
   "\"messages\": ["
   "{\"role\": \"system\", \"content\": " (llm-json-escape *llm-review-system-prompt*) "},"
   "{\"role\": \"user\", \"content\": " (llm-json-escape (string-append "Review this Scheme code:\n\n" code-content)) "}"
   "],"
   "\"temperature\": " (number->string *llm-review-temperature*) ","
   "\"max_tokens\": 8192"  ; Extra tokens for reasoning models
   "}"))

;;; llm-review-extract-nemotron-response : String -> String
;;; Extract content from OpenAI-format JSON response.
;;; Nemotron in reasoning mode puts chain-of-thought in "reasoning_content"
;;; and final answer in "content". We try content first, fall back to reasoning.
(define (llm-review-extract-nemotron-response json-str)
  ;; Try "content" first
  (let ([content (llm-extract-json-field json-str "\"content\":")])
    (if (and content (> (string-length content) 0))
        content
        ;; Fall back to reasoning_content if content is empty
        (let ([reasoning (llm-extract-json-field json-str "\"reasoning_content\":")])
          (if reasoning
              ;; Try to extract structured output from reasoning
              (llm-extract-findings-from-reasoning reasoning)
              json-str)))))

;;; llm-extract-json-field : String String -> (U String Boolean)
;;; Extract a JSON string field value.
(define (llm-extract-json-field json-str field-key)
  (let ([field-start (llm-string-search json-str field-key)])
    (if field-start
        (let* ([after-key (substring json-str
                                     (+ field-start (string-length field-key))
                                     (string-length json-str))]
               [trimmed (string-trim after-key)]
               [quote-start (if (and (> (string-length trimmed) 0)
                                     (char=? (string-ref trimmed 0) #\"))
                                1 0)]
               [content-end (llm-find-json-string-end trimmed quote-start)])
          (if content-end
              (llm-json-unescape (substring trimmed quote-start content-end))
              #f))
        #f)))

;;; llm-extract-findings-from-reasoning : String -> String
;;; Try to find structured findings in reasoning content.
;;; Look for patterns like "LINE|SEVERITY|..." or return as-is.
(define (llm-extract-findings-from-reasoning reasoning)
  ;; Look for lines that match our expected output format
  (let* ([lines (llm-string-split reasoning #\newline)]
         [findings (filter (lambda (line)
                            (let ([parts (llm-string-split line #\|)])
                              (>= (length parts) 4)))
                          lines)])
    (if (null? findings)
        ;; No structured findings found - return the reasoning
        ;; which might contain useful info
        reasoning
        ;; Return just the structured findings
        (llm-string-join findings "\n"))))

;;; llm-review-call-gemini : String -> String
;;; Call Gemini 3 Flash for code review.
(define (llm-review-call-gemini code-content)
  (let* ([prompt (string-append *llm-review-system-prompt*
                                "\n\n---\n\nReview this Scheme code:\n\n"
                                code-content)]
         [temp-file "/tmp/llm-review-prompt.txt"]
         [_ (call-with-output-file temp-file
              (lambda (port) (put-string port prompt))
              '(replace))]
         [cmd (format "cat '~a' | gemini -m gemini-3-flash-preview" temp-file)]
         [result (shell-capture-result cmd)])
    (if (process-ok? result)
        (process-stdout result)
        (error 'llm-review-call-gemini
               (format "Gemini call failed: ~a" (process-stderr result))))))

;;;; ============================================================
;;;; Response Parsing
;;;; ============================================================

;;; llm-review-parse-findings : String String -> (List Alist)
;;; Parse LLM response into finding alists.
(define (llm-review-parse-findings response file-path)
  (let ([lines (llm-string-split response #\newline)])
    (llm-filter-map
     (lambda (line)
       (llm-review-parse-finding-line (string-trim line) file-path))
     lines)))

;;; llm-review-parse-finding-line : String String -> (U Alist Boolean)
;;; Parse a single finding line. Returns #f if not a valid finding.
(define (llm-review-parse-finding-line line file-path)
  (cond
    [(string=? line "") #f]
    [(string=? line "NO_ISSUES_FOUND") #f]
    [(llm-string-prefix? line "#") #f]
    [else
     (let ([parts (llm-string-split line #\|)])
       (if (>= (length parts) 5)
           (let ([line-num (string->number (string-trim (car parts)))]
                 [severity (string->symbol (string-trim (cadr parts)))]
                 [category (string->symbol (string-trim (caddr parts)))]
                 [title (string-trim (cadddr parts))]
                 [description (string-trim (llm-string-join (cddddr parts) "|"))])
             (if (and line-num
                      (memq severity '(critical high medium low info))
                      (memq category '(correctness security performance style documentation)))
                 `((file . ,file-path)
                   (line . ,line-num)
                   (severity . ,severity)
                   (category . ,category)
                   (title . ,title)
                   (description . ,description)
                   (confidence . 0.85))
                 #f))
           #f))]))

;;;; ============================================================
;;;; Review API
;;;; ============================================================

;;; llm-review-file : String [Symbol] -> (List Alist)
;;; Review a single file using the best available LLM backend.
(define (llm-review-file file-path . args)
  (let* ([backend (if (and (pair? args) (symbol? (car args)))
                      (car args)
                      (llm-review-backend))]
         [_ (unless backend
              (error 'llm-review-file "No LLM backend available"))]
         [code-content (call-with-input-file file-path
                         (lambda (port)
                           (get-string-all port)))]
         [_ (printf "Reviewing ~a with ~a...~n" file-path backend)]
         [response (case backend
                     [(nemotron) (llm-review-call-nemotron code-content)]
                     [(gemini) (llm-review-call-gemini code-content)]
                     [else (error 'llm-review-file "Unknown backend")])]
         [findings (llm-review-parse-findings response file-path)])
    (printf "  Found ~a potential issues~n" (length findings))
    findings))

;;; llm-review-files : (List String) [Symbol] -> (List Alist)
;;; Review multiple files, adding findings to current flashmob session.
(define (llm-review-files files . args)
  (let ([backend (llm-review-backend)]
        [agent-id (if (and (pair? args) (symbol? (car args)))
                      (car args)
                      (if (eq? (llm-review-backend) 'nemotron)
                          'nemotron-qa
                          'gemini-qa))])
    (unless backend
      (error 'llm-review-files "No LLM backend available"))
    (printf "Using ~a backend~n" backend)
    (printf "Agent: ~a~n~n" agent-id)

    (unless *flashmob-current-session*
      (flashmob-start files))

    (let ([all-findings
           (apply append
                  (map (lambda (file)
                         (guard (exn [else
                                      (printf "  Error reviewing ~a: ~a~n" file
                                              (if (message-condition? exn)
                                                  (condition-message exn)
                                                  "unknown"))
                                      '()])
                           (let ([findings (llm-review-file file backend)])
                             (for-each
                              (lambda (f)
                                (flashmob-add-finding
                                 'file (cdr (assq 'file f))
                                 'line (cdr (assq 'line f))
                                 'severity (cdr (assq 'severity f))
                                 'category (cdr (assq 'category f))
                                 'confidence (cdr (assq 'confidence f))
                                 'agent agent-id
                                 'title (cdr (assq 'title f))
                                 'description (cdr (assq 'description f))))
                              findings)
                             findings)))
                       files))])
      (printf "~nTotal findings: ~a~n" (length all-findings))
      all-findings)))

;;;; ============================================================
;;;; Utility Functions
;;;; ============================================================

;;; llm-json-escape : String -> String
(define (llm-json-escape str)
  (string-append
   "\""
   (let loop ([chars (string->list str)] [acc '()])
     (if (null? chars)
         (list->string (reverse acc))
         (let ([c (car chars)])
           (cond
             [(char=? c #\") (loop (cdr chars) (append '(#\" #\\) acc))]
             [(char=? c #\\) (loop (cdr chars) (append '(#\\ #\\) acc))]
             [(char=? c #\newline) (loop (cdr chars) (append '(#\n #\\) acc))]
             [(char=? c #\return) (loop (cdr chars) (append '(#\r #\\) acc))]
             [(char=? c #\tab) (loop (cdr chars) (append '(#\t #\\) acc))]
             [else (loop (cdr chars) (cons c acc))]))))
   "\""))

;;; llm-json-unescape : String -> String
(define (llm-json-unescape str)
  (let loop ([chars (string->list str)] [acc '()] [escape? #f])
    (if (null? chars)
        (list->string (reverse acc))
        (let ([c (car chars)])
          (if escape?
              (loop (cdr chars)
                    (cons (case c
                            [(#\n) #\newline]
                            [(#\r) #\return]
                            [(#\t) #\tab]
                            [(#\" #\\) c]
                            [else c])
                          acc)
                    #f)
              (if (char=? c #\\)
                  (loop (cdr chars) acc #t)
                  (loop (cdr chars) (cons c acc) #f)))))))

;;; llm-find-json-string-end : String Int -> (U Int Boolean)
(define (llm-find-json-string-end str start)
  (let loop ([i start] [escape? #f])
    (if (>= i (string-length str))
        #f
        (let ([c (string-ref str i)])
          (cond
            [escape? (loop (+ i 1) #f)]
            [(char=? c #\\) (loop (+ i 1) #t)]
            [(char=? c #\") i]
            [else (loop (+ i 1) #f)])))))

;;; llm-string-search : String String -> (U Int Boolean)
(define (llm-string-search haystack needle)
  (let ([h-len (string-length haystack)]
        [n-len (string-length needle)])
    (let loop ([i 0])
      (cond
        [(> (+ i n-len) h-len) #f]
        [(string=? (substring haystack i (+ i n-len)) needle) i]
        [else (loop (+ i 1))]))))

;;; llm-string-prefix? : String String -> Boolean
(define (llm-string-prefix? str prefix)
  (and (>= (string-length str) (string-length prefix))
       (string=? (substring str 0 (string-length prefix)) prefix)))

;;; llm-string-split : String Char -> (List String)
(define (llm-string-split str delim)
  (let loop ([chars (string->list str)] [current '()] [result '()])
    (cond
      [(null? chars)
       (reverse (cons (list->string (reverse current)) result))]
      [(char=? (car chars) delim)
       (loop (cdr chars) '() (cons (list->string (reverse current)) result))]
      [else
       (loop (cdr chars) (cons (car chars) current) result)])))

;;; llm-string-join : (List String) String -> String
(define (llm-string-join strs delim)
  (if (null? strs)
      ""
      (let loop ([rest (cdr strs)] [acc (car strs)])
        (if (null? rest)
            acc
            (loop (cdr rest) (string-append acc delim (car rest)))))))

;;; llm-filter-map : (a -> (U b Boolean)) (List a) -> (List b)
(define (llm-filter-map f lst)
  (let loop ([items lst] [acc '()])
    (if (null? items)
        (reverse acc)
        (let ([result (f (car items))])
          (if result
              (loop (cdr items) (cons result acc))
              (loop (cdr items) acc))))))

;;; cddddr : (List a) -> (List a)
(define (cddddr lst)
  (cdr (cdr (cdr (cdr lst)))))

;;;; ============================================================
;;;; Load Message
;;;; ============================================================

(printf "llm-review.ss loaded.~n")
(printf "  (llm-review-backend)           - Check available backend~n")
(printf "  (llm-review-file \"path\")       - Review a single file~n")
(printf "  (llm-review-files '(paths...)) - Review files, add to flashmob~n")
