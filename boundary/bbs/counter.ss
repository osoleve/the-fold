(load "core/base/prelude.ss")
(load "boundary/io/atomic.ss")
(load "boundary/io/file-lock.ss")

(doc 'module 'boundary/bbs/counter)
(doc 'description "BBS ID Generation - Generates sequential human-readable IDs for issues. Format: fold-XXXX where XXXX is base36 encoded. Counter is stored in .bbs/counter")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'dependencies '(core/base/prelude boundary/io/atomic boundary/io/file-lock))
(doc 'note "Lock-aware design: Public functions (bbs-write-counter!, bbs-next-id!) acquire locks. Internal functions (%bbs-write-counter!) for use when lock already held")

(doc 'section 'configuration)

(doc *bbs-counter-file* 'type 'String)
(doc *bbs-counter-file* 'description "Path to counter file")
(define *bbs-counter-file* ".bbs/counter")

(doc *bbs-id-prefix* 'type 'String)
(doc *bbs-id-prefix* 'description "ID prefix for issues")
(define *bbs-id-prefix* "fold-")

(doc 'section 'base36-encoding)

(doc *base36-chars* 'type 'String)
(doc *base36-chars* 'description "Base36 alphabet (0-9, a-z)")
(define *base36-chars* "0123456789abcdefghijklmnopqrstuvwxyz")

(define (int->base36 n)
  (doc 'type '(-> Int String))
  (doc 'description "Convert an integer to base36 string (at least 3 chars)")
  (doc 'export #t)
  (if (= n 0)
      "000"
      (let loop ([n n] [acc '()])
        (if (= n 0)
            (let ([result (list->string acc)])
              (if (< (string-length result) 3)
                  (string-append (make-string (- 3 (string-length result)) #\0)
                                 result)
                  result))
            (loop (quotient n 36)
                  (cons (string-ref *base36-chars* (modulo n 36))
                        acc))))))

(define (base36->int str)
  (doc 'type '(-> String Int))
  (doc 'description "Convert a base36 string to integer")
  (doc 'export #t)
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

(doc 'section 'counter-operations)

(define (bbs-ensure-counter-dir!)
  (doc 'type '(-> Void))
  (doc 'description "Ensure .bbs directory exists")
  (doc 'export #t)
  (unless (file-exists? ".bbs")
    (mkdir ".bbs")))

(define (bbs-read-counter)
  (doc 'type '(-> Int))
  (doc 'description "Read the current counter value. Returns 0 if counter file doesn't exist")
  (doc 'export #t)
  (guard (e [else 0])
    (if (file-exists? *bbs-counter-file*)
        (call-with-input-file *bbs-counter-file*
          (lambda (port)
            (let ([line (get-line port)])
              (if (eof-object? line)
                  0
                  (string->number line)))))
        0)))

(define (%bbs-write-counter! n)
  (doc 'type '(-> Int Void))
  (doc 'description "INTERNAL: Write the counter value (caller must hold lock). Uses atomic write-then-rename to prevent corruption")
  (bbs-ensure-counter-dir!)
  (call-with-atomic-output-file *bbs-counter-file*
    (lambda (port)
      (put-string port (number->string n))
      (newline port))
    '(replace)))

(define (bbs-write-counter! n)
  (doc 'type '(-> Int Void))
  (doc 'description "PUBLIC: Write the counter value with locking. Uses atomic write-then-rename to prevent corruption")
  (doc 'export #t)
  (with-file-lock *bbs-counter-file*
    (lambda ()
      (%bbs-write-counter! n))))

(define (bbs-next-id!)
  (doc 'type '(-> String))
  (doc 'description "Generate the next issue ID and increment counter. Returns 'fold-XXXX' format. Uses file locking to prevent race conditions with concurrent calls")
  (doc 'export #t)
  (with-file-lock *bbs-counter-file*
    (lambda ()
      (let* ([current (bbs-read-counter)]
             [next (+ current 1)]
             [id (string-append *bbs-id-prefix* (int->base36 next))])
        (%bbs-write-counter! next)
        id))))

(define (bbs-id->number id)
  (doc 'type '(-> String (Option Int)))
  (doc 'description "Extract the numeric part from an ID")
  (doc 'export #t)
  (let ([prefix-len (string-length *bbs-id-prefix*)])
    (if (and (> (string-length id) prefix-len)
             (string=? (substring id 0 prefix-len) *bbs-id-prefix*))
        (base36->int (substring id prefix-len (string-length id)))
        #f)))

(define (bbs-sync-counter-from-heads! ids)
  (doc 'type '(-> (List String) Void))
  (doc 'description "Sync counter to be at least as high as the highest existing ID. Call this during initialization to avoid ID collisions")
  (doc 'export #t)
  (with-file-lock *bbs-counter-file*
    (lambda ()
      (let ([max-num (fold-left
                      (lambda (acc id)
                        (let ([num (bbs-id->number id)])
                          (if (and num (> num acc))
                              num
                              acc)))
                      0
                      ids)])
        (when (> max-num (bbs-read-counter))
          (%bbs-write-counter! max-num))))))
