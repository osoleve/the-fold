;;; boundary/repl/repl-worker-socket.ss — Frame-Based REPL Worker
;;;
;;; Per-session worker that reads IPC frames from stdin and writes
;;; response frames to stdout. Spawned by repl-daemon-socket.ss.
;;;
;;; Protocol: Length-prefixed s-expression frames on stdin/stdout.
;;; Same eval logic as repl-worker.ss, different transport.

(load "core/base/sha256.ss")
(load "core/blocks/cas.ss")
(load "core/blocks/normalize.ss")
(load "lattice/ipc/protocol.ss")

(define *quiet* #t)
(load "boundary/repl/error-context.ss")

;;; ====
;;; User Environment Isolation
;;; ====

;;; After all system modules load, we copy the interaction-environment into
;;; *user-env*. User eval runs with (parameterize ([interaction-environment
;;; *user-env*]) ...), so user (define ...) only modifies *user-env* — the
;;; system namespace stays clean. This prevents (define normalize ...) from
;;; clobbering internal module bindings (fold-zxzj).
(define *system-env* #f)  ; Set after repl.ss loads
(define *user-env*   #f)  ; Set after repl.ss loads

(define (session-reset!)
  (set! *user-env* (copy-environment *system-env*))
  (hashtable-clear! *procedure-formals*)
  "Session namespace reset. User definitions cleared.")

;;; ====
;;; Eval Timeout (POSIX alarm-based)
;;; ====

;;; Prevents runaway evals from permanently blocking the session.
;;; Uses POSIX alarm(2) for wall-clock timeout. When the alarm fires,
;;; SIGALRM is delivered and the registered handler raises an error
;;; that propagates up through the guard in process-eval!.
(load-shared-object #f)  ; resolve from current process image (portable)
(define posix-alarm (foreign-procedure "alarm" (unsigned-int) unsigned-int))
(define *eval-timeout-seconds* 90)

(define (with-eval-timeout thunk)
  (posix-alarm *eval-timeout-seconds*)
  (dynamic-wind
    (lambda () #f)
    thunk
    (lambda () (posix-alarm 0))))

;;; ====
;;; Content Addressing (same as repl-worker.ss)
;;; ====

;;; Belt-and-suspenders: capture internal references at load time.
;;; The primary protection is *user-env* isolation above, but these
;;; closures provide defense-in-depth for content addressing.
(define *worker-normalize* normalize)
(define *worker-sha256*    sha256)
(define *worker-hash->hex* hash->hex)

(define (extract-definition-body expr)
  (cond
   [(and (pair? expr) (eq? (car expr) 'define))
    (let ([form (cadr expr)])
      (cond
       [(pair? form)
        (cons 'fn (cons (cdr form) (cddr expr)))]
       [else
        (caddr expr)]))]
   [else expr]))

(define (content-address value)
  (let* ([body-to-hash (if (pair? value)
                           (extract-definition-body value)
                           value)]
         [normalized (if (pair? body-to-hash)
                         (*worker-normalize* body-to-hash)
                         body-to-hash)]
         [serialized (string->utf8 (format "~s" normalized))]
         [hash (*worker-sha256* serialized)])
    (*worker-hash->hex* hash)))

(define (definition? expr)
  (and (pair? expr)
       (memq (car expr) '(define define-syntax))))

(define (definition-name expr)
  (let ([form (cadr expr)])
    (if (pair? form) (car form) form)))

(define (definition-formals expr)
  ;; (define (name a b c) body) → (a b c)
  ;; (define name (lambda (a b c) body)) → (a b c)
  (guard (e [else #f])
    (let ([form (cadr expr)])
      (cond
        [(pair? form) (cdr form)]
        [(and (symbol? form)
              (pair? (cddr expr))
              (pair? (caddr expr))
              (eq? (car (caddr expr)) 'lambda))
         (cadr (caddr expr))]
        [else #f]))))

;;; Procedure formals cache — maps symbol → formals list for user-defined fns
(define *procedure-formals* (make-hashtable symbol-hash eq?))

;;; ====
;;; Evaluation (same core logic as repl-worker.ss)
;;; ====

(define (scheme-eval-string str)
  (let ([port (open-input-string str)])
    (parameterize ([interaction-environment *user-env*])
      (let loop ([last-result (void)]
                 [last-def-name #f]
                 [last-def-expr #f])
        (let ([expr (read port)])
          (if (eof-object? expr)
              (values last-result last-def-name last-def-expr)
              (let ([is-def (definition? expr)]
                    [result (eval expr)])
                (when is-def
                  (let ([name (definition-name expr)])
                    (when (and (top-level-bound? name)
                               (procedure? (top-level-value name)))
                      (let ([formals (definition-formals expr)])
                        (when formals
                          (hashtable-set! *procedure-formals* name formals))))))
                (loop result
                      (if is-def (definition-name expr) last-def-name)
                      (if is-def expr last-def-expr)))))))))


(define (condition->string c)
  (let ([who (and (who-condition? c) (condition-who c))]
        [msg (and (message-condition? c) (condition-message c))]
        [irritants (and (irritants-condition? c) (condition-irritants c))])
    (cond
     [(and msg (null? (or irritants '())))
      (if who (format "~a: ~a" who msg) msg)]
     [(and msg irritants)
      (if who
          (format "~a: ~a ~s" who msg irritants)
          (format "~a ~s" msg irritants))]
     [(and who irritants) (format "~a: ~s" who irritants)]
     [who (format "error in ~a" who)]
     [msg msg]
     [irritants (format "error: ~s" irritants)]
     [else "unknown error"])))

(define (suggest-for-unbound sym)
  (guard (ex [else ""])
    (if (and (top-level-bound? 'lattice-export-source)
             (top-level-bound? 'kg-initialized?)
             (kg-initialized?))
        (let ([skill (lattice-export-source sym)])
          (if skill
              (format "\n  Hint: exported by '~a' skill. Try: (li '~a) to find the module"
                      skill skill)
              ""))
        "")))

(define (format-condition e)
  (let ([base-msg
         (if (condition? e)
             (guard (e2 [else (condition->string e)])
               (if (message-condition? e)
                   (let ([template (condition-message e)]
                         [irritants (if (irritants-condition? e)
                                        (condition-irritants e)
                                        '())])
                     (if (null? irritants)
                         template
                         (guard (e3 [else
                                     (let ([who (and (who-condition? e) (condition-who e))])
                                       (if who
                                           (format "~a: ~a ~s" who template irritants)
                                           (format "~a ~s" template irritants)))])
                           (apply format template irritants))))
                   (condition->string e)))
             (format "~a" e))])
    ;; Enrich "not bound" errors with lattice suggestions
    (if (and (condition? e)
             (message-condition? e)
             (string=? (condition-message e) "variable ~:s is not bound")
             (irritants-condition? e)
             (pair? (condition-irritants e)))
        (let* ([raw-sym (car (condition-irritants e))]
               [sym (string->symbol (symbol->string raw-sym))])
          (string-append base-msg (suggest-for-unbound sym)))
        base-msg)))

;;; ====
;;; Procedure repr (Python-style __repr__ for bare symbol lookups)
;;; ====

(define (last-symbol-expr str)
  ;; Parse the input string, return the last expression if it's a bare symbol.
  (guard (e [else #f])
    (let ([port (open-input-string str)])
      (let loop ([last #f])
        (let ([expr (read port)])
          (if (eof-object? expr)
              (and (symbol? last) last)
              (loop expr)))))))

(define (procedure-repr sym)
  ;; Build a rich repr for a procedure bound to sym.
  (let ([docstring (and (top-level-bound? 'get-docstring)
                        (get-docstring sym))]
        [source (and (top-level-bound? 'lattice-export-source)
                     (lattice-export-source sym))])
    (cond
      ;; Lattice export with docstring: type sig + description + provenance
      [docstring
       (string-append docstring
                      (if source
                          (format "\n  [~a] (require '~a)"
                                  source
                                  (or (and (top-level-bound? '*export-module-map*)
                                           (hashtable-ref *export-module-map* sym #f))
                                      sym))
                          ""))]
      ;; User-defined with captured formals
      [(hashtable-ref *procedure-formals* sym #f)
       => (lambda (formals)
            (format "~a : ~a → ..." sym formals))]
      ;; Fallback: standard Chez repr
      [else (format "~s" (top-level-value sym))])))

(define (format-eval-result str result)
  ;; If the result is a procedure and the input was a bare symbol, show repr.
  (let ([sym (last-symbol-expr str)])
    (if (and sym (procedure? result))
        (procedure-repr sym)
        (format "~s" result))))

(define (scheme-eval-and-capture session-id str)
  (let ([output-port (open-output-string)]
        [error-port (open-output-string)])
    (let-values ([(result def-name def-expr)
                  (parameterize ([current-output-port output-port]
                                 [current-error-port error-port]
                                 [*current-session-id* session-id])
                    (scheme-eval-string str))])
      (let* ([stdout-str (get-output-string output-port)]
             [stderr-str (get-output-string error-port)]
             [output (cond
                      [(and (> (string-length stdout-str) 0)
                            (> (string-length stderr-str) 0))
                       (string-append stdout-str "\nstderr: " stderr-str)]
                      [(> (string-length stderr-str) 0)
                       (string-append "stderr: " stderr-str)]
                      [else stdout-str])])
        (cond
         [def-expr
           ;; Compute content-address for CAS but don't return it to user
           (content-address def-expr)
           output]
         [(and (eq? result (void)) (> (string-length output) 0))
          output]
         [(> (string-length output) 0)
          (string-append output
                         (if (eq? result (void))
                             ""
                             (string-append "\n=> " (format-eval-result str result))))]
         [(not (eq? result (void)))
          (format-eval-result str result)]
         [else ""])))))

;;; ====
;;; Frame I/O on stdin/stdout
;;; ====

;;; Cache binary ports once — (standard-input-port) and (standard-output-port)
;;; create FRESH ports on each call per R6RS. Multiple ports on the same fd
;;; have independent buffers, so buffered data from port #1 is invisible to
;;; port #2. This causes payload reads to block forever after header reads.
(define *stdin-binary* (standard-input-port))
(define *stdout-binary* (standard-output-port))

;;; read-frame-stdin : → Bytevector | #f
;;; Read a complete length-prefixed frame from stdin (binary mode).
(define (read-frame-stdin)
  (let ([header (get-bytevector-n *stdin-binary* 4)])
    (if (or (eof-object? header) (< (bytevector-length header) 4))
        #f
        (let ([payload-len (bytevector-u32-ref header 0 (endianness big))])
          (if (> payload-len (* 16 1024 1024))
              #f  ; Oversized
              (let ([payload (get-bytevector-n *stdin-binary* payload-len)])
                (if (or (eof-object? payload)
                        (< (bytevector-length payload) payload-len))
                    #f
                    ;; Reconstruct full frame
                    (let ([frame (make-bytevector (+ 4 payload-len))])
                      (bytevector-copy! header 0 frame 0 4)
                      (bytevector-copy! payload 0 frame 4 payload-len)
                      frame))))))))

;;; write-frame-stdout : Bytevector → Void
;;; Write a complete frame to stdout (binary mode).
(define (write-frame-stdout frame)
  (put-bytevector *stdout-binary* frame)
  (flush-output-port *stdout-binary*))

;;; ====
;;; Graceful Exit Flag
;;; ====

;;; Set by (bye) during eval. Checked after response is sent.
;;; This allows the worker to finish sending its response before exiting.
(define *bye-requested* #f)

;;; ====
;;; Request Processing
;;; ====

;;; ====
;;; v2 Command Handlers
;;; ====

;;; Capture stdout from a thunk, return as string
(define (capture-output thunk)
  (let ([out (open-output-string)])
    (parameterize ([current-output-port out])
      (thunk))
    (get-output-string out)))

;;; ====
;;; LSP Lazy Loading
;;; ====

(define *lsp-loaded?* #f)

(define (ensure-lsp!)
  (unless *lsp-loaded?*
    (parameterize ([current-output-port (current-error-port)])
      (guard (ex [else
                  (display (format "LSP load failed: ~a\n"
                                   (if (message-condition? ex)
                                       (condition-message ex) ex))
                           (current-error-port))])
        (load "boundary/lsp/capabilities.ss")))
    ;; Try to refresh the symbol index
    (when (top-level-bound? 'index-refresh!)
      (guard (ex [else #f])
        (parameterize ([current-output-port (current-error-port)])
          (index-refresh!))))
    (set! *lsp-loaded?* #t)))

;;; ====
;;; LSP Command Handlers
;;; ====

(define (process-lsp-lookup! msg)
  (ensure-lsp!)
  (let* ([req-id (ipc-message-id msg)]
         [args (ipc-message-args msg)]
         [sym-name (cond [(and args (assq 'symbol args)) => cdr]
                         [else ""])]
         [sym-str (if (symbol? sym-name) (symbol->string sym-name) sym-name)])
    (guard (ex [else
                (let* ([err-str (format "LSP lookup error: ~a"
                                        (if (message-condition? ex)
                                            (condition-message ex) ex))]
                       [frame (ipc-encode-frame
                                (ipc-make-error req-id 'lsp-error err-str))])
                  (write-frame-stdout frame))])
      (let* ([info (if (top-level-bound? 'lookup-symbol-info)
                       (lookup-symbol-info sym-str)
                       #f)]
             [doc-type (if (top-level-bound? 'lookup-doc-type)
                           (lookup-doc-type sym-str)
                           #f)]
             [text (cond
                     [(and info doc-type)
                      (format "~a\nType: ~a\n~a" sym-str doc-type
                              (format "~s" info))]
                     [info (format "~a\n~s" sym-str info)]
                     [doc-type (format "~a\nType: ~a" sym-str doc-type)]
                     [else (format "No information found for '~a'" sym-str)])]
             [resp (ipc-make-data-result req-id text
                     `((symbol . ,sym-str)
                       (found . ,(if (or info doc-type) #t #f))))]
             [frame (ipc-encode-frame resp)])
        (write-frame-stdout frame)))))

(define (process-lsp-definition! msg)
  (ensure-lsp!)
  (let* ([req-id (ipc-message-id msg)]
         [args (ipc-message-args msg)]
         [sym-name (cond [(and args (assq 'symbol args)) => cdr]
                         [else ""])]
         [sym-str (if (symbol? sym-name) (symbol->string sym-name) sym-name)])
    (guard (ex [else
                (let* ([err-str (format "LSP definition error: ~a"
                                        (if (message-condition? ex)
                                            (condition-message ex) ex))]
                       [frame (ipc-encode-frame
                                (ipc-make-error req-id 'lsp-error err-str))])
                  (write-frame-stdout frame))])
      (let* ([info (if (top-level-bound? 'lookup-symbol-info)
                       (lookup-symbol-info sym-str)
                       #f)]
             [file (and info (assq 'file info))]
             [line (and info (assq 'line info))]
             [text (cond
                     [(and file line)
                      (format "~a:~a" (cdr file) (cdr line))]
                     [file (format "~a" (cdr file))]
                     [else (format "Definition not found for '~a'" sym-str)])]
             [resp (ipc-make-data-result req-id text
                     `((symbol . ,sym-str)
                       (file . ,(and file (cdr file)))
                       (line . ,(and line (cdr line)))))]
             [frame (ipc-encode-frame resp)])
        (write-frame-stdout frame)))))

(define (process-lsp-symbols! msg)
  (ensure-lsp!)
  (let* ([req-id (ipc-message-id msg)]
         [args (ipc-message-args msg)]
         [query (cond [(and args (assq 'query args)) => cdr]
                      [else ""])]
         [query-str (if (symbol? query) (symbol->string query) query)])
    (guard (ex [else
                (let* ([err-str (format "LSP symbols error: ~a"
                                        (if (message-condition? ex)
                                            (condition-message ex) ex))]
                       [frame (ipc-encode-frame
                                (ipc-make-error req-id 'lsp-error err-str))])
                  (write-frame-stdout frame))])
      (let* ([matches (if (top-level-bound? 'find-symbols-matching)
                          (find-symbols-matching query-str)
                          '())]
             [top-10 (if (> (length matches) 10)
                         (let take ([ms matches] [n 10] [acc '()])
                           (if (or (null? ms) (= n 0))
                               (reverse acc)
                               (take (cdr ms) (- n 1)
                                     (cons (car ms) acc))))
                         matches)]
             [text (if (null? top-10)
                       (format "No symbols matching '~a'" query-str)
                       (let fmt-loop ([syms top-10] [acc ""])
                         (if (null? syms)
                             acc
                             (let* ([sym (car syms)]
                                    [name (if (pair? sym)
                                              (let ([n (assq 'name sym)])
                                                (if n (cdr n) (format "~a" sym)))
                                              (format "~a" sym))]
                                    [file (and (pair? sym) (assq 'file sym))]
                                    [line (and (pair? sym) (assq 'line sym))]
                                    [loc (cond
                                           [(and file line)
                                            (format "  ~a:~a" (cdr file) (cdr line))]
                                           [file (format "  ~a" (cdr file))]
                                           [else ""])]
                                    [entry (format "~a~a\n" name loc)])
                               (fmt-loop (cdr syms)
                                         (string-append acc entry))))))]
             [resp (ipc-make-data-result req-id text
                     `((query . ,query-str)
                       (count . ,(length top-10))))]
             [frame (ipc-encode-frame resp)])
        (write-frame-stdout frame)))))

(define (process-lsp-outline! msg)
  (ensure-lsp!)
  (let* ([req-id (ipc-message-id msg)]
         [args (ipc-message-args msg)]
         [file (cond [(and args (assq 'file args)) => cdr]
                     [else ""])]
         [file-str (if (symbol? file) (symbol->string file) file)])
    (guard (ex [else
                (let* ([err-str (format "LSP outline error: ~a"
                                        (if (message-condition? ex)
                                            (condition-message ex) ex))]
                       [frame (ipc-encode-frame
                                (ipc-make-error req-id 'lsp-error err-str))])
                  (write-frame-stdout frame))])
      ;; Use index-find with empty string to get all symbols, then
      ;; filter by file. Or just read file and extract define forms.
      (let* ([text (if (file-exists? file-str)
                       (let* ([content (call-with-input-file file-str
                                         get-string-all)]
                              [defs (extract-definitions content)])
                         (if (null? defs)
                             (format "No definitions found in ~a" file-str)
                             (let fmt-loop ([ds defs] [acc ""])
                               (if (null? ds) acc
                                   (fmt-loop (cdr ds)
                                             (string-append acc (car ds) "\n"))))))
                       (format "File not found: ~a" file-str))]
             [resp (ipc-make-data-result req-id text
                     `((file . ,file-str)))]
             [frame (ipc-encode-frame resp)])
        (write-frame-stdout frame)))))

;;; Extract top-level define names from file content (lightweight parser)
(define (extract-definitions content)
  (let ([port (open-input-string content)])
    (let loop ([defs '()] [line-num 1])
      (let ([line (guard (ex [else #f])
                    (let read-line ([acc '()])
                      (let ([c (read-char port)])
                        (cond
                          [(eof-object? c)
                           (if (null? acc) c (list->string (reverse acc)))]
                          [(char=? c #\newline) (list->string (reverse acc))]
                          [else (read-line (cons c acc))]))))])
        (cond
          [(or (not line) (eof-object? line)) (reverse defs)]
          ;; Match (define (name ...) or (define name
          [(and (>= (string-length line) 8)
                (string=? (substring line 0 8) "(define "))
           (let* ([rest (substring line 8 (string-length line))]
                  [name (cond
                          [(and (> (string-length rest) 0)
                                (char=? (string-ref rest 0) #\())
                           ;; (define (name ...) → extract name
                           (let name-loop ([i 1] [acc '()])
                             (cond
                               [(>= i (string-length rest))
                                (list->string (reverse acc))]
                               [(or (char=? (string-ref rest i) #\space)
                                    (char=? (string-ref rest i) #\)))
                                (list->string (reverse acc))]
                               [else (name-loop (+ i 1)
                                                (cons (string-ref rest i) acc))]))]
                          [else
                           ;; (define name → extract name
                           (let name-loop ([i 0] [acc '()])
                             (cond
                               [(>= i (string-length rest))
                                (list->string (reverse acc))]
                               [(char-whitespace? (string-ref rest i))
                                (list->string (reverse acc))]
                               [else (name-loop (+ i 1)
                                                (cons (string-ref rest i) acc))]))])])
             (loop (cons (format "  ~a :~a" name line-num) defs)
                   (+ line-num 1)))]
          [else (loop defs (+ line-num 1))])))))

(define (process-search! msg)
  (let* ([req-id (ipc-message-id msg)]
         [args (ipc-message-args msg)]
         [query (cond [(and args (assq 'query args)) => cdr]
                      [else ""])]
         [limit (cond [(and args (assq 'limit args)) => cdr]
                      [else 20])]
         [results (lattice-find-data query limit)]
         [text (if (null? results)
                   "No matches found."
                   (format "~s" results))]
         [resp (ipc-make-data-result req-id text
                 `((query . ,query) (limit . ,limit)))]
         [frame (ipc-encode-frame resp)])
    (write-frame-stdout frame)))

(define (process-inspect! msg)
  (let* ([req-id (ipc-message-id msg)]
         [args (ipc-message-args msg)]
         [skill (cond [(and args (assq 'skill args))
                       => (lambda (p) (if (string? (cdr p))
                                          (string->symbol (cdr p))
                                          (cdr p)))]
                      [else 'unknown])]
         [data (lattice-describe-data skill)]
         [text (if data
                   (format "~s" data)
                   (format "Skill '~a' not found." skill))]
         [resp (ipc-make-data-result req-id text
                 `((skill . ,skill)))]
         [frame (ipc-encode-frame resp)])
    (write-frame-stdout frame)))

(define (process-exports! msg)
  (let* ([req-id (ipc-message-id msg)]
         [args (ipc-message-args msg)]
         [skill (cond [(and args (assq 'skill args))
                       => (lambda (p) (if (string? (cdr p))
                                          (string->symbol (cdr p))
                                          (cdr p)))]
                      [else 'unknown])]
         [data (lattice-exports-data skill)]
         [text (if data
                   (format "~s" data)
                   (format "Skill '~a' not found." skill))]
         [resp (ipc-make-data-result req-id text
                 `((skill . ,skill)))]
         [frame (ipc-encode-frame resp)])
    (write-frame-stdout frame)))

;;; ====
;;; Capability Layer Lazy Loading
;;; ====

(define *cap-loaded?* #f)

(define (ensure-cap!)
  (unless *cap-loaded?*
    (parameterize ([current-output-port (current-error-port)])
      (guard (ex [else
                  (display (format "Capability layer load failed: ~a\n"
                                   (if (message-condition? ex)
                                       (condition-message ex) ex))
                           (current-error-port))])
        (load "boundary/capability/fold-cap.ss")))
    (set! *cap-loaded?* #t)))

;;; ====
;;; Env/Memory Command Handlers
;;; ====

(define (process-env-store! session-id msg)
  (ensure-cap!)
  (let* ([req-id (ipc-message-id msg)]
         [args (ipc-message-args msg)]
         [key (cond [(and args (assq 'key args))
                     => (lambda (p) (if (string? (cdr p))
                                        (string->symbol (cdr p))
                                        (cdr p)))]
                    [else 'unnamed])]
         [expr-str (cond [(and args (assq 'expression args)) => cdr]
                         [else "#f"])])
    (guard (ex [else
                (let* ([err-str (format "env-store error: ~a"
                                        (if (message-condition? ex)
                                            (condition-message ex) ex))]
                       [frame (ipc-encode-frame
                                (ipc-make-error req-id 'env-error err-str))])
                  (write-frame-stdout frame))])
      ;; Capture stdout to prevent display/print from corrupting IPC frame stream
      (let* ([value (let ([out (open-output-string)])
                      (parameterize ([current-output-port out]
                                     [interaction-environment *user-env*])
                        (eval (read (open-input-string expr-str)))))]
             [result (fold-cap-env-store! session-id key value 'sexpr)]
             [text (car result)]
             [env-summary (cdr result)]
             [resp `((type . result) (v . 2) (id . ,req-id)
                     (value . ,text)
                     (env . ,env-summary))]
             [frame (ipc-encode-frame resp)])
        (write-frame-stdout frame)))))

(define (process-env-retrieve! session-id msg)
  (ensure-cap!)
  (let* ([req-id (ipc-message-id msg)]
         [args (ipc-message-args msg)]
         [key (cond [(and args (assq 'key args))
                     => (lambda (p) (if (string? (cdr p))
                                        (string->symbol (cdr p))
                                        (cdr p)))]
                    [else 'unnamed])])
    (guard (ex [else
                (let* ([err-str (format "env-retrieve error: ~a"
                                        (if (message-condition? ex)
                                            (condition-message ex) ex))]
                       [frame (ipc-encode-frame
                                (ipc-make-error req-id 'env-error err-str))])
                  (write-frame-stdout frame))])
      (let* ([value (fold-cap-env-fetch session-id key)]
             [text (if value (format "~s" value) (format "Key '~a' not found" key))]
             [resp (ipc-make-data-result req-id text
                     `((key . ,key) (found . ,(if value #t #f))))]
             [frame (ipc-encode-frame resp)])
        (write-frame-stdout frame)))))

(define (process-env-peek! session-id msg)
  (ensure-cap!)
  (let* ([req-id (ipc-message-id msg)]
         [args (ipc-message-args msg)]
         [key (cond [(and args (assq 'key args))
                     => (lambda (p) (if (string? (cdr p))
                                        (string->symbol (cdr p))
                                        (cdr p)))]
                    [else 'unnamed])]
         [n (cond [(and args (assq 'n args)) => cdr]
                  [else 500])])
    (guard (ex [else
                (let* ([err-str (format "env-peek error: ~a"
                                        (if (message-condition? ex)
                                            (condition-message ex) ex))]
                       [frame (ipc-encode-frame
                                (ipc-make-error req-id 'env-error err-str))])
                  (write-frame-stdout frame))])
      (let* ([value (fold-cap-env-peek session-id key n)]
             [text (or value (format "Key '~a' not found" key))]
             [resp (ipc-make-data-result req-id text
                     `((key . ,key) (found . ,(if value #t #f))))]
             [frame (ipc-encode-frame resp)])
        (write-frame-stdout frame)))))

(define (process-env-grep! session-id msg)
  (ensure-cap!)
  (let* ([req-id (ipc-message-id msg)]
         [args (ipc-message-args msg)]
         [key (cond [(and args (assq 'key args))
                     => (lambda (p) (if (string? (cdr p))
                                        (string->symbol (cdr p))
                                        (cdr p)))]
                    [else 'unnamed])]
         [pattern (cond [(and args (assq 'pattern args)) => cdr]
                        [else ""])]
         [k (cond [(and args (assq 'k args)) => cdr]
                  [else 5])])
    (guard (ex [else
                (let* ([err-str (format "env-grep error: ~a"
                                        (if (message-condition? ex)
                                            (condition-message ex) ex))]
                       [frame (ipc-encode-frame
                                (ipc-make-error req-id 'env-error err-str))])
                  (write-frame-stdout frame))])
      (let* ([results (fold-cap-env-grep session-id key pattern k)]
             [text (if results
                       (let fmt ([rs results] [acc ""])
                         (if (null? rs) acc
                             (fmt (cdr rs)
                                  (string-append acc
                                    (if (string=? acc "") "" "\n---\n")
                                    (if (pair? (car rs))
                                        (car (car rs))
                                        (format "~a" (car rs)))))))
                       (format "Key '~a' not found or not chunked" key))]
             [resp (ipc-make-data-result req-id text
                     `((key . ,key)
                       (pattern . ,pattern)
                       (count . ,(if results (length results) 0))))]
             [frame (ipc-encode-frame resp)])
        (write-frame-stdout frame)))))

(define (process-env-keys! session-id msg)
  (ensure-cap!)
  (let* ([req-id (ipc-message-id msg)]
         [keys (fold-cap-env-keys session-id)]
         [text (if (null? keys)
                   "Environment is empty"
                   (let fmt ([ks keys] [acc ""])
                     (if (null? ks) acc
                         (let ([entry (car ks)])
                           (fmt (cdr ks)
                                (string-append acc
                                  (format "  ~a (~a, ~a)\n"
                                          (car entry)
                                          (cadr entry)
                                          (caddr entry))))))))]
         [resp (ipc-make-data-result req-id text
                 `((keys . ,keys)))]
         [frame (ipc-encode-frame resp)])
    (write-frame-stdout frame)))

(define (process-env-ingest! session-id msg)
  (ensure-cap!)
  (let* ([req-id (ipc-message-id msg)]
         [args (ipc-message-args msg)]
         [key (cond [(and args (assq 'key args))
                     => (lambda (p) (if (string? (cdr p))
                                        (string->symbol (cdr p))
                                        (cdr p)))]
                    [else 'unnamed])]
         [text (cond [(and args (assq 'text args)) => cdr]
                     [else ""])]
         [chunk-size (cond [(and args (assq 'chunk-size args)) => cdr]
                           [else 2000])])
    (guard (ex [else
                (let* ([err-str (format "env-ingest error: ~a"
                                        (if (message-condition? ex)
                                            (condition-message ex) ex))]
                       [frame (ipc-encode-frame
                                (ipc-make-error req-id 'env-error err-str))])
                  (write-frame-stdout frame))])
      (let* ([env-summary (fold-cap-env-ingest! session-id key text chunk-size)]
             [resp `((type . result) (v . 2) (id . ,req-id)
                     (value . ,(format "Ingested ~a chars as '~a'"
                                       (string-length text) key))
                     (env . ,env-summary))]
             [frame (ipc-encode-frame resp)])
        (write-frame-stdout frame)))))

(define (process-memory-store! msg)
  (ensure-cap!)
  (let* ([req-id (ipc-message-id msg)]
         [args (ipc-message-args msg)]
         [key (cond [(and args (assq 'key args))
                     => (lambda (p) (if (string? (cdr p))
                                        (string->symbol (cdr p))
                                        (cdr p)))]
                    [else 'unnamed])]
         [text (cond [(and args (assq 'text args)) => cdr]
                     [else ""])])
    (guard (ex [else
                (let* ([err-str (format "memory-store error: ~a"
                                        (if (message-condition? ex)
                                            (condition-message ex) ex))]
                       [frame (ipc-encode-frame
                                (ipc-make-error req-id 'memory-error err-str))])
                  (write-frame-stdout frame))])
      (fold-cap-memorize! key text)
      (let* ([resp (ipc-make-data-result req-id
                     (format "Saved to persistent memory under '~a'" key)
                     `((key . ,key)))]
             [frame (ipc-encode-frame resp)])
        (write-frame-stdout frame)))))

(define (process-memory-search! msg)
  (ensure-cap!)
  (let* ([req-id (ipc-message-id msg)]
         [args (ipc-message-args msg)]
         [query (cond [(and args (assq 'query args)) => cdr]
                      [else ""])]
         [k (cond [(and args (assq 'k args)) => cdr]
                  [else 5])])
    (guard (ex [else
                (let* ([err-str (format "memory-search error: ~a"
                                        (if (message-condition? ex)
                                            (condition-message ex) ex))]
                       [frame (ipc-encode-frame
                                (ipc-make-error req-id 'memory-error err-str))])
                  (write-frame-stdout frame))])
      (let* ([results (fold-cap-remember query k)]
             [text (if (null? results)
                       "No matching memories found."
                       (let fmt ([rs results] [acc ""])
                         (if (null? rs) acc
                             (let ([entry (car rs)])
                               (fmt (cdr rs)
                                    (string-append acc
                                      (format "(~a ~s ~a)\n"
                                              (car entry)
                                              (cadr entry)
                                              (if (>= (length entry) 3)
                                                  (caddr entry) ""))))))))]
             [resp (ipc-make-data-result req-id text
                     `((query . ,query)
                       (count . ,(length results))))]
             [frame (ipc-encode-frame resp)])
        (write-frame-stdout frame)))))

;;; ====
;;; Request Processing (v1 + v2 dispatch)
;;; ====

(define (process-eval! session-id msg)
  (let* ([req-id (ipc-message-id msg)]
         [expr (ipc-message-get msg 'expr)])
    (guard (ex [else
                (capture-error! ex)
                (let* ([err-str (format-condition ex)]
                       [err-msg (ipc-make-error req-id 'eval-error err-str)]
                       [frame (ipc-encode-frame err-msg)])
                  (write-frame-stdout frame))])
      (let ([result (with-eval-timeout
                      (lambda () (scheme-eval-and-capture session-id expr)))])
        (let* ([resp (ipc-make-result req-id result)]
               [frame (ipc-encode-frame resp)])
          (write-frame-stdout frame))))))

(define (process-request! session-id msg)
  (let ([cmd (ipc-message-cmd msg)])
    (case cmd
      ;; Core
      [(eval)           (process-eval! session-id msg)]
      ;; Lattice meta
      [(search)         (process-search! msg)]
      [(inspect)        (process-inspect! msg)]
      [(exports)        (process-exports! msg)]
      ;; LSP
      [(lsp-lookup)     (process-lsp-lookup! msg)]
      [(lsp-definition) (process-lsp-definition! msg)]
      [(lsp-symbols)    (process-lsp-symbols! msg)]
      [(lsp-outline)    (process-lsp-outline! msg)]
      ;; Env
      [(env-store)      (process-env-store! session-id msg)]
      [(env-retrieve)   (process-env-retrieve! session-id msg)]
      [(env-peek)       (process-env-peek! session-id msg)]
      [(env-grep)       (process-env-grep! session-id msg)]
      [(env-keys)       (process-env-keys! session-id msg)]
      [(env-ingest)     (process-env-ingest! session-id msg)]
      ;; Memory
      [(memory-store)   (process-memory-store! msg)]
      [(memory-search)  (process-memory-search! msg)]
      ;; Default
      [else             (process-eval! session-id msg)])))

;;; ====
;;; Worker Loop
;;; ====

(define (worker-loop session-id)
  (let ([frame (read-frame-stdin)])
    (when frame
      (let ([continue?
             (guard (ex [else
                         ;; Decode error — skip this frame
                         (display (format "Worker ~a: decode error\n" session-id)
                                  (current-error-port))
                         #t])
               (let ([msg (ipc-decode-frame frame)])
                 (case (ipc-message-type msg)
                   [(request)
                    (process-request! session-id msg)
                    (not *bye-requested*)]
                   [(ping)
                    (write-frame-stdout (ipc-encode-frame (ipc-make-pong)))
                    #t]
                   [(shutdown)
                    (write-frame-stdout (ipc-encode-frame (ipc-make-shutdown-ack)))
                    #f]  ; stop looping
                   [else #t])))])
        (when continue?
          (worker-loop session-id))))))

;;; ====
;;; Startup
;;; ====

(define (require-session-id args)
  (if (and (pair? args) (pair? (cdr args)))
      (cadr args)
      (begin
       (display "Usage: scheme --script boundary/repl/repl-worker-socket.ss <session-id>\n"
                (current-error-port))
       (exit 1))))

(define (start-socket-worker!)
  (let ([session-id (require-session-id (command-line))])
    ;; Load the REPL environment so workers have access to Fold commands.
    ;; Redirect stdout to stderr during init to keep the binary frame
    ;; protocol clean — many modules print load banners on stdout.
    (parameterize ([current-output-port (current-error-port)])
      (load "boundary/repl/repl.ss"))
    ;; Snapshot system environment, then create isolated user namespace.
    ;; User (define ...) goes into *user-env*, system bindings stay clean.
    (set! *system-env* (interaction-environment))
    (set! *user-env* (copy-environment *system-env*))
    ;; Register SIGALRM handler for eval timeouts (fold-zxyw).
    ;; When posix-alarm fires, SIGALRM (14) is delivered and this handler
    ;; raises an error that propagates up through the guard in process-eval!,
    ;; returning a timeout error to the client instead of blocking forever.
    (register-signal-handler 14  ; SIGALRM
      (lambda (sig)
        (error 'eval-timeout
               (format "Evaluation timed out after ~a seconds" *eval-timeout-seconds*))))
    ;; Run the frame-based worker loop with structured cleanup.
    ;; dynamic-wind ensures flush on any exit path: normal return,
    ;; shutdown frame, EOF, or uncaught exception.
    (dynamic-wind
      (lambda () #f)
      (lambda () (worker-loop session-id))
      (lambda ()
        (guard (ex [else #f])
          (flush-output-port *stdout-binary*))
        (display (format "Worker ~a exiting.\n" session-id)
                 (current-error-port))))))

(start-socket-worker!)
