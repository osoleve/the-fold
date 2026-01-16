;;; shell/bbs/heads.ss — BBS Head File Management
;;;
;;; Manages named heads for issues in .store/heads/bbs/
;;; Each issue has a head file containing its current block hash.
;;;
;;; Head files contain 66-character hex strings:
;;;   - 2 chars: version byte (00)
;;;   - 64 chars: SHA-256 hash
;;;
;;; This is Shell code: impure (filesystem IO).

(load "core/base/prelude.ss")
(load "core/blocks/cas.ss")
(load "shell/io/atomic.ss")

;;; ====
;;; Configuration
;;; ====

(define *bbs-heads-dir* ".store/heads/bbs")

;;; ====
;;; Path Utilities
;;; ====

;;; bbs-head-path : String -> String
;;; Get the filesystem path for an issue's head file.
(define (bbs-head-path id)
  (string-append *bbs-heads-dir* "/" id ".head"))

;;; bbs-ensure-heads-dir! : -> Void
;;; Ensure the heads directory exists.
(define (bbs-ensure-heads-dir!)
  (unless (file-exists? ".store")
    (mkdir ".store"))
  (unless (file-exists? ".store/heads")
    (mkdir ".store/heads"))
  (unless (file-exists? *bbs-heads-dir*)
    (mkdir *bbs-heads-dir*)))

;;; ====
;;; Read/Write Operations
;;; ====

;;; bbs-read-head : String -> Bytevector | #f
;;; Read the current hash for an issue ID.
;;; Returns #f if the head file doesn't exist.
(define (bbs-read-head id)
  (let ([path (bbs-head-path id)])
    (guard (e [else #f])
      (if (file-exists? path)
          (let* ([content (call-with-input-file path
                           (lambda (port) (get-line port)))]
                 [trimmed (string-trim content)]
                 ;; Strip 0x prefix if present
                 [hex (if (and (> (string-length trimmed) 2)
                               (string=? (substring trimmed 0 2) "0x"))
                          (substring trimmed 2 (string-length trimmed))
                          trimmed)])
            ;; Accept 64-char (32-byte hash) or 66-char (33-byte CAS address)
            (let ([len (string-length hex)])
              (if (or (= len 64) (= len 66))
                  (hex->hash hex)
                  #f)))
          #f))))

;;; bbs-write-head! : String Bytevector -> Void
;;; Write the current hash for an issue ID.
;;; Uses atomic write-then-rename to prevent corruption.
(define (bbs-write-head! id hash)
  (bbs-ensure-heads-dir!)
  (let ([path (bbs-head-path id)]
        [hex (hash->hex hash)])
    (call-with-atomic-output-file path
      (lambda (port)
        (put-string port hex)
        (newline port))
      '(replace))))

;;; bbs-delete-head! : String -> Void
;;; Delete the head file for an issue (for closing/deleting).
(define (bbs-delete-head! id)
  (let ([path (bbs-head-path id)])
    (when (file-exists? path)
      (delete-file path))))

;;; bbs-head-exists? : String -> Boolean
;;; Check if a head file exists for an issue ID.
(define (bbs-head-exists? id)
  (file-exists? (bbs-head-path id)))

;;; ====
;;; Compare-and-Swap (OCC)
;;; ====

;;; bbs-cas-head! : String Bytevector Bytevector -> Boolean
;;; Atomically update head if current value matches expected.
;;; Returns #t if successful, #f if current value differs.
(define (bbs-cas-head! id expected-hash new-hash)
  (let ([current (bbs-read-head id)])
    (if (or (not current)
            (not (bytevector=? current expected-hash)))
        #f  ; CAS failed - current doesn't match expected
        (begin
          (bbs-write-head! id new-hash)
          #t))))

;;; ====
;;; Listing Operations
;;; ====

;;; bbs-list-heads : -> (List String)
;;; List all issue IDs that have head files.
(define (bbs-list-heads)
  (guard (e [else '()])
    (if (file-exists? *bbs-heads-dir*)
        (let ([entries (directory-list *bbs-heads-dir*)])
          (filter-map
           (lambda (entry)
             (let ([len (string-length entry)])
               (if (and (> len 5)
                        (string=? (substring entry (- len 5) len) ".head"))
                   (substring entry 0 (- len 5))
                   #f)))
           entries))
        '())))

;;; filter-map : (a -> b | #f) (List a) -> (List b)
;;; Map and filter in one pass.
(define (filter-map f lst)
  (let loop ([lst lst] [acc '()])
    (if (null? lst)
        (reverse acc)
        (let ([result (f (car lst))])
          (loop (cdr lst)
                (if result (cons result acc) acc))))))

;;; ====
;;; String Utilities
;;; ====

;;; string-trim : String -> String
;;; Remove leading and trailing whitespace.
(define (string-trim str)
  (let* ([len (string-length str)]
         [start (let loop ([i 0])
                  (if (>= i len)
                      len
                      (if (char-whitespace? (string-ref str i))
                          (loop (+ i 1))
                          i)))]
         [end (let loop ([i (- len 1)])
                (if (< i start)
                    start
                    (if (char-whitespace? (string-ref str i))
                        (loop (- i 1))
                        (+ i 1))))])
    (substring str start end)))
