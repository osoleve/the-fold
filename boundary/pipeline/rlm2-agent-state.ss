;;; @module rlm2-agent-state
;;; @description Agent state persistence for continuous agent lifecycle.
;;; Loaded by rlm2-drive.ss — requires CAS storage and rlm2 state types.

;;; ====
;;; Agent State Persistence (CAS-backed, for continuous agents)
;;; ====
;;;
;;; Follows the same pattern as rlm2-memory:
;;;   Head pointer: .store/heads/rlm2-agent/{agent-id}.head
;;;   CAS block:    tag=rlm2/agent-state, payload=serialized state,
;;;                 refs=(vector prev-state-hash) or empty

(define *rlm2-agent-heads-dir* ".store/heads/rlm2-agent")

(define (rlm2-agent-head-path agent-id)
  (string-append *rlm2-agent-heads-dir* "/" agent-id ".head"))

(define (rlm2-ensure-agent-heads-dir!)
  (unless (file-exists? ".store")
    (mkdir ".store"))
  (unless (file-exists? ".store/heads")
    (mkdir ".store/heads"))
  (unless (file-exists? *rlm2-agent-heads-dir*)
    (mkdir *rlm2-agent-heads-dir*)))

(define (rlm2-read-agent-head agent-id)
  (let ([path (rlm2-agent-head-path agent-id)])
    (guard (e [else #f])
      (if (file-exists? path)
          (let* ([content (call-with-input-file path
                            (lambda (port) (get-line port)))]
                 [trimmed (string-trim content)])
            (if (>= (string-length trimmed) 64)
                (hex->hash trimmed)
                #f))
          #f))))

(define (rlm2-write-agent-head! agent-id hash)
  (rlm2-ensure-agent-heads-dir!)
  (let ([path (rlm2-agent-head-path agent-id)])
    (with-file-lock path
      (lambda ()
        (call-with-atomic-output-file path
          (lambda (port)
            (put-string port (hash->hex hash))
            (newline port))
          '(replace))))))

(define (rlm2-checkpoint-state! agent-id state prev-hash)
  "Serialize state to a CAS block. Returns the hex hash string."
  (let* ([payload (string->utf8 (format "~s" state))]
         [refs (if prev-hash
                   (vector (hex->hash prev-hash))
                   (vector))]
         [blk (make-block 'rlm2/agent-state payload refs)]
         [hash (store-persistent! blk)]
         [hex (hash->hex hash)])
    (rlm2-write-agent-head! agent-id hash)
    hex))

(define (rlm2-restore-state agent-id)
  "Load agent state from CAS. Returns state or #f."
  (let ([head (rlm2-read-agent-head agent-id)])
    (if (not head)
        #f
        (let ([blk (fetch-persistent head)])
          (if (not blk)
              (begin
                (format (current-error-port)
                  "[RLM] WARNING: agent ~a head points to missing block ~a~%"
                  agent-id (hash->hex head))
                #f)
              (guard (ex [else
                          (format (current-error-port)
                            "[RLM] WARNING: failed to deserialize agent ~a state: ~a~%"
                            agent-id (if (message-condition? ex)
                                         (condition-message ex) ex))
                          #f])
                (let ([data (read (open-input-string
                                   (utf8->string (block-payload blk))))])
                  (if (rlm2-state? data)
                      data
                      (begin
                        (format (current-error-port)
                          "[RLM] WARNING: agent ~a state block is not a valid rlm2-state~%"
                          agent-id)
                        #f)))))))))

