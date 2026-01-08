;;; shell/pipeline/checkpoint.ss — Checkpoint Effect Handler
;;;
;;; Handles pipeline checkpoint persistence and restoration.
;;;
;;; This is Shell code: handles IO, may fail, contains defensive logic.

(load "core/pipeline/stage.ss")
(load "core/pipeline/effects.ss")
(load "core/pipeline/context.ss")

;;; ============================================================
;;; Checkpoint Configuration
;;; ============================================================

;;; *checkpoint-dir* : String
;;; Directory for pipeline checkpoints.
(define *checkpoint-dir* ".fold-checkpoints")

;;; ============================================================
;;; Checkpoint Effect Interpretation
;;; ============================================================

;;; interpret-checkpoint-effect : Payload -> Context -> State -> Input -> (Result . State)
(define (interpret-checkpoint-effect payload ctx state input)
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

;;; ============================================================
;;; Checkpoint Persistence
;;; ============================================================

;;; persist-checkpoint : RunId -> Name -> Value -> ()
;;; Persist a checkpoint value to disk.
;;; Checkpoints are stored as: .fold-checkpoints/<run-id>/<name>.sexp
(define (persist-checkpoint run-id name value)
  (guard (ex [else (void)])  ; Silently fail if persistence fails
         (let ([run-dir (string-append *checkpoint-dir* "/" run-id)])
              (ensure-directory! run-dir)
              (let ([checkpoint-file (string-append run-dir "/" (symbol->string name) ".sexp")])
                   (call-with-output-file checkpoint-file
                                          (lambda (p)
                                                  (pretty-print value p)))))))

;;; load-checkpoint : RunId -> Name -> Any
;;; Load a checkpoint value from disk.
;;; Returns #f if checkpoint doesn't exist.
(define (load-checkpoint run-id name)
  (guard (ex [else #f])
         (let ([checkpoint-file (string-append *checkpoint-dir* "/" run-id "/" (symbol->string name) ".sexp")])
              (if (file-exists? checkpoint-file)
                  (call-with-input-file checkpoint-file read)
                  #f))))

;;; ============================================================
;;; Directory Utilities
;;; ============================================================

;;; path-directory : String -> String
;;; Get directory portion of path.
(define (path-directory path)
  (let ([idx (string-rindex path #\/)])
       (if idx
           (substring path 0 idx)
           ".")))

;;; string-rindex : String -> Char -> Maybe Integer
;;; Find last occurrence of char in string.
(define (string-rindex str ch)
  (let loop ([i (- (string-length str) 1)])
       (cond
        [(< i 0) #f]
        [(char=? (string-ref str i) ch) i]
        [else (loop (- i 1))])))

;;; ensure-directory! : String -> ()
;;; Create directory and all parent directories if they don't exist.
(define (ensure-directory! path)
  (unless (file-exists? path)
          (let ([parent (path-directory path)])
               (when (and (not (string=? parent "."))
                          (not (string=? parent path)))
                     (ensure-directory! parent)))
          (guard (ex [else (void)])  ; Ignore if already exists
                 (mkdir path))))
