(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
;;; @requires rlm2
(require 'rlm2)

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

(doc 'type '(-> Action (Maybe String) String Rlm2ParseResult))
(doc 'description "Parse result: action + optional thought + raw text + optional failure info + optional alias info")
(define (make-rlm2-parse-result action thought raw . rest)
  (let ([candidate (if (pair? rest) (car rest) #f)]
        [failure-reason (if (and (pair? rest) (pair? (cdr rest))) (cadr rest) #f)]
        [alias-info (if (and (pair? rest) (pair? (cdr rest)) (pair? (cddr rest)))
                        (caddr rest) #f)])
    (list 'rlm2-parse-result action thought raw candidate failure-reason alias-info)))

(define (rlm2-parse-result? x)
  (and (pair? x) (eq? (car x) 'rlm2-parse-result)))

(define (rlm2-parse-result-action r)         (list-ref r 1))
(define (rlm2-parse-result-thought r)        (list-ref r 2))
(define (rlm2-parse-result-raw r)            (list-ref r 3))
(define (rlm2-parse-result-candidate r)      (if (> (length r) 4) (list-ref r 4) #f))
(define (rlm2-parse-result-failure-reason r) (if (> (length r) 5) (list-ref r 5) #f))
(define (rlm2-parse-result-alias-info r)     (if (> (length r) 6) (list-ref r 6) #f))

(doc 'type '(-> Rlm2ParseResult (Pair Symbol Symbol) Rlm2ParseResult))
(doc 'description "Return a new parse result with alias-info set")
(define (rlm2-parse-result-with-alias r alias-info)
  (list 'rlm2-parse-result
        (rlm2-parse-result-action r)
        (rlm2-parse-result-thought r)
        (rlm2-parse-result-raw r)
        (rlm2-parse-result-candidate r)
        (rlm2-parse-result-failure-reason r)
        alias-info))

(doc 'type '(-> (Pair Symbol Symbol) String))
(doc 'description "Format a correction note for when an alias was used, to help the model learn canonical names")
(define (rlm2-alias-correction-note alias-info)
  (format "[alias] '~a' resolved to '~a'. Use (~a ...) directly."
          (car alias-info) (cdr alias-info) (cdr alias-info)))

;;; ====
;;; Think normalization
;;; ====

;;; Recover a (think ...) that read/validation couldn't handle.
;;; Two cases:
;;;   1. Truncated quoted: (think "long text that got cut off...
;;;   2. Unquoted bare text: (think G8: analysis of feedback...
;;; Returns a (think content) action or #f.
(define (rlm2-try-partial-think text)
  (let ([len (string-length text)])
    (and (> len 8)
         (char=? (string-ref text 0) #\()
         (string=? (substring text 1 6) "think")
         (char-whitespace? (string-ref text 6))
         ;; Find start of content after (think<ws>
         (let find-content ([i 7])
           (cond
             [(>= i len) (list 'think "")]
             [(char-whitespace? (string-ref text i)) (find-content (+ i 1))]
             ;; Quoted content — extract after opening quote
             [(char=? (string-ref text i) #\")
              (let ([start (+ i 1)])
                (if (>= start len)
                    (list 'think "")
                    (let* ([raw (substring text start len)]
                           [rlen (string-length raw)])
                      (list 'think
                        (cond
                          ;; Strip trailing ") — complete but read failed for other reasons
                          [(and (>= rlen 2)
                                (char=? (string-ref raw (- rlen 1)) #\))
                                (char=? (string-ref raw (- rlen 2)) #\"))
                           (substring raw 0 (- rlen 2))]
                          ;; Strip trailing " — almost complete
                          [(and (>= rlen 1)
                                (char=? (string-ref raw (- rlen 1)) #\"))
                           (substring raw 0 (- rlen 1))]
                          ;; Truncated mid-content — use as-is
                          [else raw])))))]
             ;; Unquoted bare text — model wrote (think G8: analysis...)
             ;; Grab everything from here to end as content
             [else
              (list 'think (substring text i len))])))))

(define (rlm2-unwrap-nested-think result)
  ;; Safety net: if parsed action is (think "(think \"...\")"), unwrap one level.
  ;; Primary defense is rlm2-try-partial-think; this catches the remaining case
  ;; where read succeeded but the model intentionally double-wrapped.
  (let ([action (rlm2-parse-result-action result)])
    (if (and (pair? action)
             (eq? (car action) 'think)
             (>= (length action) 2)
             (string? (cadr action))
             (let ([t (rlm2-parse-trim (cadr action))])
               (and (>= (string-length t) 7)
                    (string=? (substring t 0 6) "(think"))))
        ;; Try to parse the inner think
        (let ([inner (guard (ex [else #f])
                       (read (open-input-string (cadr action))))])
          (if (and (pair? inner) (eq? (car inner) 'think) (>= (length inner) 2))
              (make-rlm2-parse-result
                (list 'think (cadr inner))
                (rlm2-parse-result-thought result)
                (rlm2-parse-result-raw result)
                (rlm2-parse-result-candidate result)
                (rlm2-parse-result-failure-reason result))
              result))
        result)))

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
       (make-rlm2-parse-result (list 'think "") #f text #f "empty-input")]
      [else
       (let ([result (rlm2-parse-response-inner trimmed text)])
         ;; Unwrap double-nested thinks: model sometimes writes
         ;; (think "(think \"actual content\")") — strip the inner wrapper
         (rlm2-unwrap-nested-think result))])))

;;; Inner dispatch: direct read → partial begin → partial think → fuzzy
(define (rlm2-parse-response-inner trimmed text)
  (let ([direct (rlm2-try-read-action trimmed)])
    (if (rlm2-try-ok? direct)
        ;; Direct read succeeded — pass through alias info if present
        (let ([result (rlm2-extract-thought-and-action
                        (rlm2-try-action direct) text)]
              [alias-info (rlm2-try-alias-info direct)])
          (if alias-info
              (rlm2-parse-result-with-alias result alias-info)
              result))
        ;; Direct read failed — try partial recovery before fuzzy
        (let ([partial (rlm2-try-partial-begin trimmed)])
          (if partial
              ;; Recovered actions from a truncated begin block
              (rlm2-extract-thought-and-action partial text)
              ;; Try partial think recovery — prevents double-wrapping
              ;; when (think "long text...) is truncated
              (let ([partial-think (rlm2-try-partial-think trimmed)])
                (if partial-think
                    (make-rlm2-parse-result partial-think #f text
                                           #f "partial-think-recovery")
                    ;; No partial recovery — fuzzy extraction
                    (rlm2-parse-fuzzy trimmed text direct))))))))

;;; Fuzzy extraction: find balanced parens in text, try to read+validate
(define (rlm2-parse-fuzzy trimmed text direct)
  (let ([balanced (rlm2-extract-balanced trimmed)])
    (if (not balanced)
        ;; No balanced parens — use direct failure info
        (make-rlm2-parse-result
          (list 'think trimmed) trimmed text
          (rlm2-try-candidate direct)
          (or (rlm2-try-reason direct) "no-balanced-sexp"))
        ;; Found balanced parens — try reading that
        (let ([fuzzy (rlm2-try-read-action balanced)])
          (if (rlm2-try-ok? fuzzy)
              (let* ([pre (rlm2-text-before-paren trimmed)]
                     [result (rlm2-extract-thought-and-action
                               (rlm2-try-action fuzzy) text
                               (if (string=? pre "") #f pre))]
                     [alias-info (rlm2-try-alias-info fuzzy)])
                (if alias-info
                    (rlm2-parse-result-with-alias result alias-info)
                    result))
              ;; Balanced parens found but validation failed
              (make-rlm2-parse-result
                (list 'think trimmed) trimmed text
                (rlm2-try-candidate fuzzy)
                (or (rlm2-try-reason fuzzy)
                    "balanced-extraction-read-failed")))))))


;;; ====
;;; Action Alias Table
;;; ====
;;;
;;; Maps common synonyms (emitted by smaller models) to canonical action names.
;;; Used before validation so that e.g. (lookup "foo") becomes (search "foo").

(doc 'section 'rlm2-alias)

(doc 'type 'Alist)
(doc 'description "Maps alias symbols to canonical action symbols.
Targets must be members of *rlm2-action-types* in rlm2.ss.")
(define *rlm2-action-aliases*
  '(;; search aliases
    (find     . search)
    (help     . search)
    ;; eval aliases
    (run      . eval)
    (execute  . eval)
    ;; load aliases
    (import   . load)
    (require  . load)
    (use      . load)
    ;; submit aliases
    (answer   . submit)
    (respond  . submit)
    (output   . submit)
    ;; inspect aliases
    (look     . inspect)
    (view     . inspect)
    (info     . inspect)
    (describe . inspect)
    (show     . inspect)
    ;; exports aliases
    (list-exports . exports)
    ;; symbols aliases
    (list     . symbols)
    (dir      . symbols)
    ;; peek aliases
    (get      . peek)
    (fetch    . peek)
    (cat      . peek)))

(doc 'type '(-> Symbol (Maybe Symbol)))
(doc 'description "Look up the canonical action name for an alias. Returns #f if not an alias.")
(define (rlm2-alias-lookup sym)
  (let ([entry (assq sym *rlm2-action-aliases*)])
    (and entry (cdr entry))))

(doc 'type '(-> Any (Values Any (Maybe (Pair Symbol Symbol)))))
(doc 'description "Resolve aliases in an action expression. Returns (values resolved-expr alias-info).
alias-info is (alias . canonical) if rewriting occurred, #f otherwise.")
(define (rlm2-resolve-alias expr)
  (if (not (and (pair? expr) (symbol? (car expr))))
      (values expr #f)
      (let* ([head (car expr)]
             [args (cdr expr)]
             [canonical (rlm2-alias-lookup head)])
        (if canonical
            (values (cons canonical args) (cons head canonical))
            (values expr #f)))))

;;; ====
;;; Helpers
;;; ====

;;; Try to read an S-expression from a string and validate it as an action.
;;; Returns a tagged result:
;;;   (parse-ok action)              — valid action
;;;   (parse-ok action alias-info)   — valid action after alias resolution
;;;   (parse-fail candidate reason)  — failure with diagnostics
;;; Rejects input with significant trailing content after the first expression
;;; (whitespace and comments are tolerated).
(doc 'description "Try to read and validate an action from a string. Returns (parse-ok action [alias-info]) or (parse-fail candidate reason).")

(define (rlm2-try-ok? r) (eq? (car r) 'parse-ok))
(define (rlm2-try-action r) (cadr r))
(define (rlm2-try-alias-info r)
  (and (rlm2-try-ok? r) (> (length r) 2) (caddr r)))
(define (rlm2-try-candidate r) (cadr r))
(define (rlm2-try-reason r) (caddr r))

;;; Validate an expression, trying alias resolution on failure.
;;; Returns a try-result: (parse-ok action alias-info) or (parse-fail candidate reason).
(define (rlm2-validate-or-alias expr)
  (let ([validation (rlm2-validate-action expr)])
    (if (rlm2-validation-ok? validation)
        (list 'parse-ok (rlm2-validation-value validation) #f)
        ;; Validation failed — try alias resolution before giving up
        (let-values ([(resolved alias-info) (rlm2-resolve-alias-recursive expr)])
          (if alias-info
              (let ([v2 (rlm2-validate-action resolved)])
                (if (rlm2-validation-ok? v2)
                    (list 'parse-ok (rlm2-validation-value v2) alias-info)
                    (list 'parse-fail expr (rlm2-validation-error v2))))
              (list 'parse-fail expr (rlm2-validation-error validation)))))))

(define (rlm2-try-read-action str)
  (guard (exn [#t
               (list 'parse-fail #f
                     (format "read-error: ~a"
                       (if (message-condition? exn)
                           (condition-message exn)
                           exn)))])
    (let* ([port (open-input-string str)]
           [expr (read port)])
      (if (eof-object? expr)
          (list 'parse-fail #f "empty-read")
          ;; Check for trailing content
          (let ([rest (rlm2-port-rest-trimmed port)])
            (if (and (not (string=? rest ""))
                     (not (rlm2-only-comments? rest)))
                (list 'parse-fail expr
                      (format "trailing-content: ~a"
                        (if (> (string-length rest) 100)
                            (string-append (substring rest 0 97) "...")
                            rest)))
                (rlm2-validate-or-alias expr)))))))

;;; Also resolve aliases inside begin blocks (recursive)
(define (rlm2-resolve-alias-recursive expr)
  (if (not (and (pair? expr) (symbol? (car expr))))
      (values expr #f)
      (if (eq? (car expr) 'begin)
          ;; Resolve aliases in each child of a begin
          (let loop ([children (cdr expr)]
                     [resolved-children '()]
                     [any-alias? #f]
                     [first-alias #f])
            (if (null? children)
                (let ([result (cons 'begin (reverse resolved-children))])
                  (values result first-alias))
                (let-values ([(child-resolved child-alias) (rlm2-resolve-alias (car children))])
                  (loop (cdr children)
                        (cons child-resolved resolved-children)
                        (or any-alias? (and child-alias #t))
                        (or first-alias child-alias)))))
          ;; Non-begin: resolve at top level
          (rlm2-resolve-alias expr))))

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

;;; Try to recover completed sub-expressions from a truncated (begin ...) block.
;;; When max_tokens cuts the model output mid-begin, we lose the closing paren
;;; but may have several complete sub-expressions inside. This function reads
;;; them one at a time and reconstructs a valid begin.
;;; Returns a validated action or #f.
(define (rlm2-try-partial-begin text)
  (let ([len (string-length text)])
    ;; Must start with (begin followed by whitespace
    (and (> len 6)
         (char=? (string-ref text 0) #\()
         (let ([prefix (substring text 1 (min 6 len))])
           (string=? prefix "begin"))
         (> len 6)
         (char-whitespace? (string-ref text 6))
         ;; Read sub-expressions from the body, stopping at first failure
         (let ([port (open-input-string (substring text 6 len))])
           (let loop ([exprs '()])
             (guard (ex [#t
                         ;; read threw — truncated sexp. Use what we have.
                         (rlm2-partial-begin-result exprs)])
               (let ([expr (read port)])
                 (if (eof-object? expr)
                     ;; Clean EOF — but we're here because the outer read
                     ;; failed (trailing content or unclosed begin paren),
                     ;; so build from what we collected
                     (rlm2-partial-begin-result exprs)
                     (loop (cons expr exprs))))))))))

;;; Given collected sub-expressions (in reverse order), build and validate
;;; a begin action. Returns validated action or #f.
(define (rlm2-partial-begin-result exprs)
  (let ([exprs (reverse exprs)])
    (cond
      [(null? exprs) #f]
      ;; Single expr — validate directly
      [(null? (cdr exprs))
       (let ([v (rlm2-validate-action (car exprs))])
         (and (rlm2-validation-ok? v)
              (rlm2-validation-value v)))]
      ;; Multiple exprs — wrap in begin, validate
      [else
       (let* ([candidate (cons 'begin exprs)]
              [v (rlm2-validate-action candidate)])
         (and (rlm2-validation-ok? v)
              (rlm2-validation-value v)))])))

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
