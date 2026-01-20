(load "core/base/prelude.ss")
(load "core/blocks/block.ss")
(load "core/blocks/cas.ss")

(doc 'module 'boundary/history/blocks)
(doc 'description "History entry block creation for REPL commands")
(doc 'layer 'boundary)
(doc 'purity 'total)
(doc 'dependencies '(core/base/prelude core/blocks/block core/blocks/cas))
(doc 'note "Creates CAS blocks for history entries with command classification")

(doc 'section 'block-tag)

(doc HISTORY-ENTRY 'type 'symbol)
(doc HISTORY-ENTRY 'description "Block tag for history entries")
(define HISTORY-ENTRY 'history/entry)

(doc 'section 'command-classification)

(doc *definition-forms* 'type '(List Symbol))
(doc *definition-forms* 'description "Forms that define bindings (modify environment)")
(define *definition-forms*
  '(define define-syntax define-record-type
    define-property define-ftype library
    module))

(doc *effect-forms* 'type '(List Symbol))
(doc *effect-forms* 'description "Forms with side effects (skip on safe replay)")
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

(define (classify-command cmd-str)
  (doc 'type (-> String Symbol))
  (doc 'description "Classify command as 'definition, 'effect, or 'expression")
  (doc 'export #t)
  (doc 'note "Parses command string and checks head form against known lists")
  (guard (e [else 'expression])
    (let* ([port (open-input-string cmd-str)]
           [expr (read port)])
      (classify-expr expr))))

(define (classify-expr expr)
  (doc 'type (-> Sexpr Symbol))
  (doc 'description "Classify a parsed expression")
  (cond
    [(not (pair? expr)) 'expression]
    [(memq (car expr) *definition-forms*) 'definition]
    [(memq (car expr) *effect-forms*) 'effect]
    [(eq? (car expr) 'begin)
     (let ([types (map classify-expr (cdr expr))])
       (cond
         [(memq 'definition types) 'definition]
         [(memq 'effect types) 'effect]
         [else 'expression]))]
    [(memq (car expr) '(let let* letrec letrec*))
     (if (and (pair? (cdr expr))
              (list? (cadr expr)))
         'expression
         'expression)]
    [else 'expression]))

(doc 'section 'history-entry-block)

(define (make-history-entry-block session-id index command cmd-type result-type
                                   result-hash defined-name timestamp version prev-hash)
  (doc 'type (-> String Int String Symbol Symbol String Symbol String Int (Option Bytevector) Block))
  (doc 'description "Create a history entry block")
  (doc 'export #t)
  (doc 'param 'session-id "Session identifier")
  (doc 'param 'index "Command index (0-based)")
  (doc 'param 'command "Command string")
  (doc 'param 'cmd-type "'definition | 'effect | 'expression")
  (doc 'param 'result-type "'success | 'error")
  (doc 'param 'result-hash "Hash of result value (hex)")
  (doc 'param 'defined-name "Symbol defined (or #f)")
  (doc 'param 'timestamp "ISO 8601 timestamp")
  (doc 'param 'version "Schema version (currently 1)")
  (doc 'param 'prev-hash "Hash of previous entry (or #f)")
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

(doc 'section 'block-data-extraction)

(define (history-entry-data blk)
  (doc 'type (-> Block (Option Alist)))
  (doc 'description "Extract history entry data from a block")
  (doc 'export #t)
  (if (and (block? blk) (eq? (block-tag blk) HISTORY-ENTRY))
      (guard (e [else #f])
        (read (open-input-string (utf8->string (block-payload blk)))))
      #f))

(define (history-entry-prev blk)
  (doc 'type (-> Block (Option Bytevector)))
  (doc 'description "Get the previous entry hash from a history entry block")
  (doc 'export #t)
  (let ([refs (block-refs blk)])
    (if (> (vector-length refs) 0)
        (vector-ref refs 0)
        #f)))

(doc 'section 'convenience-accessors)

(define (history-entry-field blk field)
  (doc 'type (-> Block Symbol Any))
  (doc 'description "Get a specific field from a history entry block")
  (doc 'export #t)
  (let ([data (history-entry-data blk)])
    (and data (cdr (assq field data)))))

(doc history-entry-session-id 'type (-> Block (Option String)))
(doc history-entry-session-id 'export #t)
(define (history-entry-session-id blk) (history-entry-field blk 'session-id))

(doc history-entry-index 'type (-> Block (Option Int)))
(doc history-entry-index 'export #t)
(define (history-entry-index blk) (history-entry-field blk 'index))

(doc history-entry-command 'type (-> Block (Option String)))
(doc history-entry-command 'export #t)
(define (history-entry-command blk) (history-entry-field blk 'command))

(doc history-entry-cmd-type 'type (-> Block (Option Symbol)))
(doc history-entry-cmd-type 'export #t)
(define (history-entry-cmd-type blk) (history-entry-field blk 'cmd-type))

(doc history-entry-result-type 'type (-> Block (Option Symbol)))
(doc history-entry-result-type 'export #t)
(define (history-entry-result-type blk) (history-entry-field blk 'result-type))

(doc history-entry-result-hash 'type (-> Block (Option String)))
(doc history-entry-result-hash 'export #t)
(define (history-entry-result-hash blk) (history-entry-field blk 'result-hash))

(doc history-entry-defined-name 'type (-> Block (Option Symbol)))
(doc history-entry-defined-name 'export #t)
(define (history-entry-defined-name blk) (history-entry-field blk 'defined-name))

(doc history-entry-timestamp 'type (-> Block (Option String)))
(doc history-entry-timestamp 'export #t)
(define (history-entry-timestamp blk) (history-entry-field blk 'timestamp))

(doc 'section 'defined-name-extraction)

(define (extract-defined-name cmd-str)
  (doc 'type (-> String (Option Symbol)))
  (doc 'description "Extract the name being defined from a definition command")
  (doc 'export #t)
  (guard (e [else #f])
    (let* ([port (open-input-string cmd-str)]
           [expr (read port)])
      (extract-defined-name-from-expr expr))))

(define (extract-defined-name-from-expr expr)
  (doc 'type (-> Sexpr (Option Symbol)))
  (doc 'description "Extract defined name from parsed expression")
  (cond
    [(not (pair? expr)) #f]
    [(memq (car expr) '(define define-syntax))
     (let ([form (cadr expr)])
       (if (pair? form)
           (car form)
           form))]
    [(eq? (car expr) 'begin)
     (let loop ([forms (cdr expr)])
       (if (null? forms)
           #f
           (or (extract-defined-name-from-expr (car forms))
               (loop (cdr forms)))))]
    [else #f]))
