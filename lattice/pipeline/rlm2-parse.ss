(load "lattice/pipeline/rlm2.ss")

(doc 'module 'pipeline/rlm2-parse)
(doc 'description "RLM v2 action parser: S-expression parsing with fuzzy fallback. Parses model text output into validated actions.")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'dependencies '(pipeline/rlm2.ss))

;;; ====
;;; Primary Parser
;;; ====
;;;
;;; Strategy:
;;; 1. Try `read` on the raw text. If it produces a valid action, done.
;;; 2. If `read` fails, try to extract the first balanced (...) from the text.
;;; 3. If extraction + validation succeeds, use it.
;;; 4. Otherwise, wrap the entire output as (think "...").
;;;
;;; Returns: (parse-result action thought raw)
;;;   action  — the validated action (always present, at worst (think "..."))
;;;   thought — extracted think text if a (begin (think ...) action) was parsed,
;;;             or #f if no explicit think
;;;   raw     — the original model output string

(doc 'section 'rlm2-parse)

(doc 'type '(-> String (Maybe String) Action String Rlm2ParseResult))
(doc 'description "Parse result: action + optional thought + raw text")
(define (make-rlm2-parse-result action thought raw)
  (list 'rlm2-parse-result action thought raw))

(define (rlm2-parse-result? x)
  (and (pair? x) (eq? (car x) 'rlm2-parse-result)))

(define (rlm2-parse-result-action r)  (list-ref r 1))
(define (rlm2-parse-result-thought r) (list-ref r 2))
(define (rlm2-parse-result-raw r)     (list-ref r 3))

;;; ====
;;; Main entry point
;;; ====

(doc 'type '(-> String Rlm2ParseResult))
(doc 'description "Parse model text output into a validated action. Always succeeds — worst case wraps as (think ...).")
(define (rlm2-parse-response text)
  (let ([trimmed (rlm2-parse-trim text)])
    (cond
      ;; Empty output -> think
      [(string=? trimmed "")
       (make-rlm2-parse-result (list 'think "") #f text)]
      ;; Try direct read
      [(rlm2-try-read-action trimmed)
       => (lambda (result)
            (rlm2-extract-thought-and-action result text))]
      ;; Try fuzzy extraction
      [(rlm2-extract-balanced trimmed)
       => (lambda (candidate)
            (cond
              [(rlm2-try-read-action candidate)
               => (lambda (result)
                    (let ([pre (rlm2-text-before-paren trimmed)])
                      (rlm2-extract-thought-and-action
                       result text
                       (if (string=? pre "") #f pre))))]
              [else
               (make-rlm2-parse-result (list 'think trimmed) trimmed text)]))]
      ;; Total fallback
      [else
       (make-rlm2-parse-result (list 'think trimmed) trimmed text)])))

;;; ====
;;; Helpers
;;; ====

;;; Try to read an S-expression from a string and validate it as an action.
;;; Returns the validated action or #f.
;;; Rejects input with significant trailing content after the first expression
;;; (whitespace and comments are tolerated).
(define (rlm2-try-read-action str)
  (guard (exn [#t #f])
    (let* ([port (open-input-string str)]
           [expr (read port)])
      (if (eof-object? expr)
          #f
          ;; Check for trailing content
          (let ([rest (rlm2-port-rest-trimmed port)])
            (if (and (not (string=? rest ""))
                     (not (rlm2-only-comments? rest)))
                #f  ; significant trailing garbage — reject
                (let ([validation (rlm2-validate-action expr)])
                  (if (rlm2-validation-ok? validation)
                      (rlm2-validation-value validation)
                      #f))))))))

;;; Read remaining content from a port, trimmed.
(define (rlm2-port-rest-trimmed port)
  (let loop ([chars '()])
    (let ([c (read-char port)])
      (if (eof-object? c)
          (rlm2-parse-trim (list->string (reverse chars)))
          (loop (cons c chars))))))

;;; Check if a string contains only comments (line comments starting with ;,
;;; block comments #|...|#, datum comments #;) and whitespace.
;;; Used to tolerate trailing explanations from models.
(define (rlm2-only-comments? str)
  (let ([len (string-length str)])
    (let loop ([i 0])
      (cond
        [(>= i len) #t]
        ;; Whitespace — skip
        [(char-whitespace? (string-ref str i))
         (loop (+ i 1))]
        ;; Line comment: ; to end of line
        [(char=? (string-ref str i) #\;)
         (let skip-line ([j (+ i 1)])
           (cond
             [(>= j len) #t]
             [(char=? (string-ref str j) #\newline) (loop (+ j 1))]
             [else (skip-line (+ j 1))]))]
        ;; #| block comment |# with nesting
        [(and (char=? (string-ref str i) #\#)
              (< (+ i 1) len)
              (char=? (string-ref str (+ i 1)) #\|))
         (let skip-block ([j (+ i 2)] [depth 1])
           (cond
             [(>= j len) #f]  ; unclosed block comment
             [(and (char=? (string-ref str j) #\|)
                   (< (+ j 1) len)
                   (char=? (string-ref str (+ j 1)) #\#))
              (if (= depth 1)
                  (loop (+ j 2))
                  (skip-block (+ j 2) (- depth 1)))]
             [(and (char=? (string-ref str j) #\#)
                   (< (+ j 1) len)
                   (char=? (string-ref str (+ j 1)) #\|))
              (skip-block (+ j 2) (+ depth 1))]
             [else (skip-block (+ j 1) depth)]))]
        ;; #; datum comment — skip next datum via read
        [(and (char=? (string-ref str i) #\#)
              (< (+ i 1) len)
              (char=? (string-ref str (+ i 1)) #\;))
         (guard (ex [else #f])
           (let* ([port (open-input-string (substring str (+ i 2) len))]
                  [datum (read port)]
                  ;; Calculate how many chars were consumed
                  [rest (rlm2-port-rest-trimmed port)])
             (if (eof-object? datum)
                 #f
                 (rlm2-only-comments? rest))))]
        ;; Anything else — not a comment
        [else #f]))))

;;; Extract the first balanced parenthesized expression from text.
;;; Returns the substring or #f.
(define (rlm2-extract-balanced text)
  (let ([len (string-length text)])
    (let find-open ([i 0])
      (cond
        [(>= i len) #f]
        [(char=? (string-ref text i) #\()
         (let scan ([j (+ i 1)] [depth 1] [in-string #f] [escape #f])
           (cond
             [(>= j len) #f]  ; unclosed
             [escape
              (scan (+ j 1) depth in-string #f)]
             [(char=? (string-ref text j) #\\)
              (scan (+ j 1) depth in-string #t)]
             [in-string
              (if (char=? (string-ref text j) #\")
                  (scan (+ j 1) depth #f #f)
                  (scan (+ j 1) depth #t #f))]
             [(char=? (string-ref text j) #\")
              (scan (+ j 1) depth #t #f)]
             [(char=? (string-ref text j) #\()
              (scan (+ j 1) (+ depth 1) #f #f)]
             [(char=? (string-ref text j) #\))
              (if (= depth 1)
                  (substring text i (+ j 1))
                  (scan (+ j 1) (- depth 1) #f #f))]
             [else
              (scan (+ j 1) depth #f #f)]))]
        [else (find-open (+ i 1))]))))

;;; Get text before the first open paren (for capturing pre-action thought)
(define (rlm2-text-before-paren text)
  (let ([len (string-length text)])
    (let loop ([i 0])
      (cond
        [(>= i len) text]
        [(char=? (string-ref text i) #\()
         (rlm2-parse-trim (substring text 0 i))]
        [else (loop (+ i 1))]))))

;;; Given a parsed action and raw text, separate think from action.
;;; If the action is (begin (think ...) rest...), extract the think.
;;; If the action is (think ...) alone, it IS the thought.
;;; Optional pre-text argument for fuzzy extraction preamble.
(define rlm2-extract-thought-and-action
  (case-lambda
    [(action raw)
     (rlm2-extract-thought-and-action action raw #f)]
    [(action raw pre-text)
     (cond
       ;; (begin (think "...") actual-action ...)
       [(and (rlm2-begin? action)
             (not (null? (rlm2-begin-actions action)))
             (rlm2-think? (car (rlm2-begin-actions action))))
        (let* ([think-act (car (rlm2-begin-actions action))]
               [thought (rlm2-think-text think-act)]
               [rest (cdr (rlm2-begin-actions action))]
               ;; Combine pre-text with explicit think
               [full-thought (if pre-text
                                 (string-append pre-text "\n" thought)
                                 thought)])
          (if (= (length rest) 1)
              (make-rlm2-parse-result (car rest) full-thought raw)
              (make-rlm2-parse-result (cons 'begin rest) full-thought raw)))]
       ;; (think "...") alone
       [(rlm2-think? action)
        (let ([thought (rlm2-think-text action)])
          (make-rlm2-parse-result action
                                  (if pre-text
                                      (string-append pre-text "\n" thought)
                                      thought)
                                  raw))]
       ;; Action with pre-text preamble
       [pre-text
        (make-rlm2-parse-result action pre-text raw)]
       ;; Clean action, no thought
       [else
        (make-rlm2-parse-result action #f raw)])]))

;;; Trim whitespace from both ends (pure, no dependency on v1 string utils)
(define (rlm2-parse-trim s)
  (let ([len (string-length s)])
    (let ([start (let loop ([i 0])
                   (cond
                     [(>= i len) len]
                     [(char-whitespace? (string-ref s i)) (loop (+ i 1))]
                     [else i]))])
      (if (>= start len)
          ""
          (let ([end (let loop ([i (- len 1)])
                       (cond
                         [(< i start) start]
                         [(char-whitespace? (string-ref s i)) (loop (- i 1))]
                         [else (+ i 1)]))])
            (substring s start end))))))
