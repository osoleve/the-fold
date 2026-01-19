;;; boundary/history/blocks.ss — History Entry Block Creation
;;;
;;; Creates CAS blocks for REPL history entries.
;;; Each entry records a command, its type, result, and links to previous.
;;;
;;; Block Tag: history/entry
;;;
;;; Command Types:
;;;   definition  - Modifies environment (define, define-syntax). Replayable.
;;;   effect      - I/O side effects (load, write, display). Skip on safe replay.
;;;   expression  - Pure evaluation. Replay if needed for result.
;;;
;;; This is Shell code: uses Core block primitives.

(load "core/base/prelude.ss")
(load "core/blocks/block.ss")
(load "core/blocks/cas.ss")

;;; ====
;;; Block Tag
;;; ====

(define HISTORY-ENTRY 'history/entry)

;;; ====
;;; Command Classification
;;; ====

;;; *definition-forms* : (List Symbol)
;;; Forms that define bindings (modify environment state).
(define *definition-forms*
  '(define define-syntax define-record-type
    define-property define-ftype library
    module))

;;; *effect-forms* : (List Symbol)
;;; Forms that perform side effects (should skip on safe replay).
(define *effect-forms*
  '(load require import
    display write newline printf format
    put-string put-char put-datum put-bytevector
    call-with-output-file call-with-input-file
    with-output-to-file with-input-from-file
    open-output-file open-input-file
    delete-file rename-file mkdir
    set! set-car! set-cdr!
    vector-set! bytevector-u8-set!
    hashtable-set! hashtable-delete!
    read get-line get-string-all))

;;; classify-command : String -> Symbol
;;; Classify a command string as 'definition, 'effect, or 'expression.
;;;
;;; Strategy:
;;;   1. Parse the command string to extract the head form
;;;   2. Check if head matches definition or effect forms
;;;   3. Default to 'expression for pure computation
(define (classify-command cmd-str)
  (guard (e [else 'expression])  ; Parse errors → treat as expression
    (let* ([port (open-input-string cmd-str)]
           [expr (read port)])
      (classify-expr expr))))

;;; classify-expr : Sexpr -> Symbol
;;; Classify a parsed expression.
(define (classify-expr expr)
  (cond
    [(not (pair? expr)) 'expression]
    [(memq (car expr) *definition-forms*) 'definition]
    [(memq (car expr) *effect-forms*) 'effect]
    ;; Check for begin with definitions
    [(eq? (car expr) 'begin)
     (let ([types (map classify-expr (cdr expr))])
       (cond
         [(memq 'definition types) 'definition]
         [(memq 'effect types) 'effect]
         [else 'expression]))]
    ;; Check for let/letrec that might contain definitions
    [(memq (car expr) '(let let* letrec letrec*))
     (if (and (pair? (cdr expr))
              (list? (cadr expr)))
         'expression  ; Pure binding forms
         'expression)]
    [else 'expression]))

;;; ====
;;; History Entry Block
;;; ====

;;; make-history-entry-block : String Int String Symbol Symbol String Symbol String Int (Option Bytevector) -> Block
;;; Create a history entry block.
;;;
;;; Arguments:
;;;   session-id    - Session identifier (e.g., "cli-123")
;;;   index         - Command index in session (0-based)
;;;   command       - The command string
;;;   cmd-type      - 'definition | 'effect | 'expression
;;;   result-type   - 'success | 'error
;;;   result-hash   - Hash of the result value (hex string)
;;;   defined-name  - Symbol defined (or #f if not a definition)
;;;   timestamp     - ISO 8601 timestamp
;;;   version       - Schema version (currently 1)
;;;   prev-hash     - Hash of previous entry (or #f for first)
(define (make-history-entry-block session-id index command cmd-type result-type
                                   result-hash defined-name timestamp version prev-hash)
  (let* ([payload-data `((session-id . ,session-id)
                         (index . ,index)
                         (command . ,command)
                         (cmd-type . ,cmd-type)
                         (result-type . ,result-type)
                         (result-hash . ,result-hash)
                         (defined-name . ,defined-name)
                         (timestamp . ,timestamp)
                         (version . ,version))]
         [payload (string->utf8 (format "~s" payload-data))]
         [refs (if prev-hash
                   (vector prev-hash)
                   (vector))])
    (make-block HISTORY-ENTRY payload refs)))

;;; ====
;;; Block Data Extraction
;;; ====

;;; history-entry-data : Block -> Alist | #f
;;; Extract history entry data from a block.
(define (history-entry-data blk)
  (if (and (block? blk) (eq? (block-tag blk) HISTORY-ENTRY))
      (guard (e [else #f])
        (read (open-input-string (utf8->string (block-payload blk)))))
      #f))

;;; history-entry-prev : Block -> Bytevector | #f
;;; Get the previous entry hash from a history entry block.
(define (history-entry-prev blk)
  (let ([refs (block-refs blk)])
    (if (> (vector-length refs) 0)
        (vector-ref refs 0)
        #f)))

;;; ====
;;; Convenience Accessors
;;; ====

;;; history-entry-field : Block Symbol -> Any | #f
;;; Get a specific field from a history entry block.
(define (history-entry-field blk field)
  (let ([data (history-entry-data blk)])
    (and data (cdr (assq field data)))))

(define (history-entry-session-id blk) (history-entry-field blk 'session-id))
(define (history-entry-index blk) (history-entry-field blk 'index))
(define (history-entry-command blk) (history-entry-field blk 'command))
(define (history-entry-cmd-type blk) (history-entry-field blk 'cmd-type))
(define (history-entry-result-type blk) (history-entry-field blk 'result-type))
(define (history-entry-result-hash blk) (history-entry-field blk 'result-hash))
(define (history-entry-defined-name blk) (history-entry-field blk 'defined-name))
(define (history-entry-timestamp blk) (history-entry-field blk 'timestamp))

;;; ====
;;; Defined Name Extraction
;;; ====

;;; extract-defined-name : String -> Symbol | #f
;;; Extract the name being defined from a definition command.
(define (extract-defined-name cmd-str)
  (guard (e [else #f])
    (let* ([port (open-input-string cmd-str)]
           [expr (read port)])
      (extract-defined-name-from-expr expr))))

;;; extract-defined-name-from-expr : Sexpr -> Symbol | #f
(define (extract-defined-name-from-expr expr)
  (cond
    [(not (pair? expr)) #f]
    [(memq (car expr) '(define define-syntax))
     (let ([form (cadr expr)])
       (if (pair? form)
           (car form)   ; (define (foo x) ...) -> foo
           form))]      ; (define foo ...) -> foo
    [(eq? (car expr) 'begin)
     ;; Return first defined name in begin
     (let loop ([forms (cdr expr)])
       (if (null? forms)
           #f
           (or (extract-defined-name-from-expr (car forms))
               (loop (cdr forms)))))]
    [else #f]))
