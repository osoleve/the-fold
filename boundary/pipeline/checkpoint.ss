(load "lattice/pipeline/stage.ss")
(load "lattice/pipeline/effects.ss")
(load "lattice/pipeline/context.ss")

(define-syntax doc
  (syntax-rules ()
    [(_ . rest) (void)]))

(doc 'module 'checkpoint)
(doc 'description "Checkpoint effect handler - handles pipeline checkpoint persistence and restoration")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'note "This is Shell code: handles IO, may fail, contains defensive logic")

(doc 'section 'checkpoint-configuration)

(define *checkpoint-dir* ".fold-checkpoints")
(doc *checkpoint-dir* 'description "Directory for pipeline checkpoints")

(doc 'section 'checkpoint-effect-interpretation)

(define (interpret-checkpoint-effect payload ctx state input)
  (doc 'description "Interpret checkpoint effect")
  (doc 'type '(-> Payload Context State Input (Pair Result State)))
  (doc 'param 'payload "Effect payload")
  (doc 'param 'ctx "Pipeline context")
  (doc 'param 'state "Current state")
  (doc 'param 'input "Current input")
  (let ([op (car payload)])
       (case op
             [(save)
              (let* ([name (cadr payload)]
                     [new-state (state-set-checkpoint state name input)]
                     [run-id (ctx-run-id ctx)])
                    ;; Persist to CAS
                    (when run-id
                          (persist-checkpoint run-id name input))
                    (cons (stage-ok input) new-state))]
             [(save-value)
              (let* ([name (cadr payload)]
                     [value (caddr payload)]
                     [new-state (state-set-checkpoint state name value)])
                    (cons (stage-ok input) new-state))]
             [(restore)
              (let* ([name (cadr payload)]
                     [value (state-get-checkpoint state name)])
                    (if value
                        (cons (stage-ok value) state)
                        ;; Try loading from CAS
                        (let ([run-id (ctx-run-id ctx)])
                             (if run-id
                                 (let ([persisted (load-checkpoint run-id name)])
                                      (if persisted
                                          (cons (stage-ok persisted) state)
                                          (cons (stage-err 'checkpoint-not-found
                                                           (format "No checkpoint: ~a" name)
                                                           name)
                                                state)))
                                 (cons (stage-err 'checkpoint-not-found
                                                  (format "No checkpoint: ~a" name)
                                                  name)
                                       state)))))]
             [(exists)
              (let* ([name (cadr payload)]
                     [value (state-get-checkpoint state name)])
                    (cons (stage-ok (if value #t #f)) state))]
             [(clear)
              (let* ([name (cadr payload)]
                     [new-state (state-set-checkpoint state name #f)])
                    (cons (stage-ok '()) new-state))]
             [else
              (cons (stage-err 'unknown-checkpoint-op
                               (format "Unknown checkpoint op: ~a" op)
                               payload)
                    state)])))

(doc 'section 'checkpoint-persistence)

(define (persist-checkpoint run-id name value)
  (doc 'description "Persist a checkpoint value to disk. Checkpoints are stored as: .fold-checkpoints/<run-id>/<name>.sexp")
  (doc 'type '(-> RunId Name Value Void))
  (doc 'param 'run-id "Pipeline run ID")
  (doc 'param 'name "Checkpoint name")
  (doc 'param 'value "Value to persist")
  (guard (ex [else (void)])  ; Silently fail if persistence fails
         (let ([run-dir (string-append *checkpoint-dir* "/" run-id)])
              (ensure-directory! run-dir)
              (let ([checkpoint-file (string-append run-dir "/" (symbol->string name) ".sexp")])
                   (call-with-output-file checkpoint-file
                                          (lambda (p)
                                                  (pretty-print value p)))))))

(define (load-checkpoint run-id name)
  (doc 'description "Load a checkpoint value from disk. Returns #f if checkpoint doesn't exist")
  (doc 'type '(-> RunId Name (Maybe Any)))
  (doc 'param 'run-id "Pipeline run ID")
  (doc 'param 'name "Checkpoint name")
  (guard (ex [else #f])
         (let ([checkpoint-file (string-append *checkpoint-dir* "/" run-id "/" (symbol->string name) ".sexp")])
              (if (file-exists? checkpoint-file)
                  (call-with-input-file checkpoint-file read)
                  #f))))

(doc 'section 'directory-utilities)

(define (path-directory path)
  (doc 'description "Get directory portion of path")
  (doc 'type '(-> String String))
  (doc 'param 'path "File path")
  (let ([idx (string-rindex path #\/)])
       (if idx
           (substring path 0 idx)
           ".")))

(define (string-rindex str ch)
  (doc 'description "Find last occurrence of char in string")
  (doc 'type '(-> String Char (Maybe Integer)))
  (doc 'param 'str "String to search")
  (doc 'param 'ch "Character to find")
  (let loop ([i (- (string-length str) 1)])
       (cond
        [(< i 0) #f]
        [(char=? (string-ref str i) ch) i]
        [else (loop (- i 1))])))

(define (ensure-directory! path)
  (doc 'description "Create directory and all parent directories if they don't exist")
  (doc 'type '(-> String Void))
  (doc 'param 'path "Directory path to create")
  (unless (file-exists? path)
          (let ([parent (path-directory path)])
               (when (and (not (string=? parent "."))
                          (not (string=? parent path)))
                     (ensure-directory! parent)))
          (guard (ex [else (void)])  ; Ignore if already exists
                 (mkdir path))))
