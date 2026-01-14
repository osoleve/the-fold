;;; shell/bbs/counter.ss — BBS ID Generation
;;;
;;; Generates sequential human-readable IDs for issues.
;;; Format: fold-XXXX where XXXX is base36 encoded.
;;;
;;; Counter is stored in .bbs/counter
;;;
;;; This is Shell code: impure (filesystem IO).

(load "core/base/prelude.ss")

;;; ====
;;; Configuration
;;; ====

(define *bbs-counter-file* ".bbs/counter")
(define *bbs-id-prefix* "fold-")

;;; ====
;;; Base36 Encoding
;;; ====

;;; Base36 alphabet (0-9, a-z)
(define *base36-chars* "0123456789abcdefghijklmnopqrstuvwxyz")

;;; int->base36 : Int -> String
;;; Convert an integer to base36 string (at least 3 chars).
(define (int->base36 n)
  (if (= n 0)
      "000"
      (let loop ([n n] [acc '()])
        (if (= n 0)
            (let ([result (list->string acc)])
              ;; Pad to at least 3 characters
              (if (< (string-length result) 3)
                  (string-append (make-string (- 3 (string-length result)) #\0)
                                 result)
                  result))
            (loop (quotient n 36)
                  (cons (string-ref *base36-chars* (modulo n 36))
                        acc))))))

;;; base36->int : String -> Int
;;; Convert a base36 string to integer.
(define (base36->int str)
  (let ([len (string-length str)])
    (let loop ([i 0] [acc 0])
      (if (>= i len)
          acc
          (let* ([c (string-ref str i)]
                 [val (cond
                       [(char<=? #\0 c #\9) (- (char->integer c) (char->integer #\0))]
                       [(char<=? #\a c #\z) (+ 10 (- (char->integer c) (char->integer #\a)))]
                       [(char<=? #\A c #\Z) (+ 10 (- (char->integer c) (char->integer #\A)))]
                       [else 0])])
            (loop (+ i 1) (+ (* acc 36) val)))))))

;;; ====
;;; Counter Operations
;;; ====

;;; bbs-ensure-counter-dir! : -> Void
;;; Ensure .bbs directory exists.
(define (bbs-ensure-counter-dir!)
  (unless (file-exists? ".bbs")
    (mkdir ".bbs")))

;;; bbs-read-counter : -> Int
;;; Read the current counter value.
;;; Returns 0 if counter file doesn't exist.
(define (bbs-read-counter)
  (guard (e [else 0])
    (if (file-exists? *bbs-counter-file*)
        (call-with-input-file *bbs-counter-file*
          (lambda (port)
            (let ([line (get-line port)])
              (if (eof-object? line)
                  0
                  (string->number line)))))
        0)))

;;; bbs-write-counter! : Int -> Void
;;; Write the counter value.
(define (bbs-write-counter! n)
  (bbs-ensure-counter-dir!)
  (call-with-output-file *bbs-counter-file*
    (lambda (port)
      (put-string port (number->string n))
      (newline port))
    '(replace)))

;;; bbs-next-id! : -> String
;;; Generate the next issue ID and increment counter.
;;; Returns "fold-XXXX" format.
(define (bbs-next-id!)
  (let* ([current (bbs-read-counter)]
         [next (+ current 1)]
         [id (string-append *bbs-id-prefix* (int->base36 next))])
    (bbs-write-counter! next)
    id))

;;; bbs-id->number : String -> Int | #f
;;; Extract the numeric part from an ID.
(define (bbs-id->number id)
  (let ([prefix-len (string-length *bbs-id-prefix*)])
    (if (and (> (string-length id) prefix-len)
             (string=? (substring id 0 prefix-len) *bbs-id-prefix*))
        (base36->int (substring id prefix-len (string-length id)))
        #f)))

;;; bbs-sync-counter-from-heads! : (List String) -> Void
;;; Sync counter to be at least as high as the highest existing ID.
;;; Call this during initialization to avoid ID collisions.
(define (bbs-sync-counter-from-heads! ids)
  (let ([max-num (fold-left
                  (lambda (acc id)
                    (let ([num (bbs-id->number id)])
                      (if (and num (> num acc))
                          num
                          acc)))
                  0
                  ids)])
    (when (> max-num (bbs-read-counter))
      (bbs-write-counter! max-num))))
