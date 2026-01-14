;;; shell/validate.ss — Validation utilities for blocks and hashes
;;;
;;; Defensive validation functions that check structural invariants:
;;;   - Block structure (tag, payload, refs are correct types)
;;;   - Hash format (33-byte versioned addresses)
;;;   - Reference validity (all refs are valid hashes)
;;;   - Hex string format (66 hex characters)
;;;
;;; This is Shell code: defensive, validates before calling Core.
;;; Core assumes perfect input, so Shell must validate at boundaries.
;;;
;;; All validators return a Result type (from prelude.ss):
;;;   - (ok #t) for valid input
;;;   - (error message) for validation failures
;;;
;;; Dependencies:
;;;   - core/prelude.ss
;;;   - core/block.ss
;;;
;;; NOTE: All shell files should be run from the ccverse root directory.

;;; Set up source-directories to find core modules
(source-directories (cons "core" (source-directories)))

(load "base/prelude.ss")
(load "blocks/block.ss")

;;; ====
;;; Hash Validation
;;; ====

;;; hash? : Any -> Boolean
;;; Predicate: is this a valid hash (33-byte address bytevector)?
(define (hash? obj)
  (and (bytevector? obj)
       (= (bytevector-length obj) address-size)))

;;; validate-hash : Any -> Result Bool
;;; Validate that an object is a proper hash.
;;; Returns (ok #t) if valid, (error message) otherwise.
(define (validate-hash obj)
  (cond
   [(not (bytevector? obj))
    '(error "hash must be a bytevector")]
   [(not (= (bytevector-length obj) address-size))
    `(error ,(string-append "hash must be exactly "
                            (number->string address-size)
                            " bytes, got "
                            (number->string (bytevector-length obj))))]
   [else '(ok #t)]))

;;; hex-char? : Char -> Boolean
;;; Check if character is a valid hexadecimal digit.
(define (hex-char? c)
  (or (char<=? #\0 c #\9)
      (char<=? #\a c #\f)
      (char<=? #\A c #\F)))

;;; hex-string? : Any -> Boolean
;;; Predicate: is this a valid 66-character hex string (for versioned addresses)?
(define (hex-string? obj)
  (and (string? obj)
       (= (string-length obj) (* 2 address-size))
       (let loop ([i 0])
            (or (= i (string-length obj))
                (and (hex-char? (string-ref obj i))
                     (loop (+ i 1)))))))

;;; validate-hex-string : Any -> Result Bool
;;; Validate that a string is a proper hex hash string.
;;; Returns (ok #t) if valid, (error message) otherwise.
(define (validate-hex-string obj)
  (cond
   [(not (string? obj))
    '(error "hex hash must be a string")]
   [(not (= (string-length obj) (* 2 address-size)))
    `(error ,(string-append "hex hash must be exactly "
                            (number->string (* 2 address-size))
                            " characters, got "
                            (number->string (string-length obj))))]
   [else
    (let loop ([i 0])
         (cond
          [(= i (string-length obj)) '(ok #t)]
          [(not (hex-char? (string-ref obj i)))
           `(error ,(string-append "invalid hex character at position "
                                   (number->string i)
                                   ": "
                                   (string (string-ref obj i))))]
          [else (loop (+ i 1))]))]))

;;; ====
;;; Reference Validation
;;; ====

;;; validate-refs : Any -> Result Bool
;;; Validate that refs is a vector of valid hashes.
;;; Returns (ok #t) if valid, (error message) otherwise.
(define (validate-refs obj)
  (cond
   [(not (vector? obj))
    '(error "refs must be a vector")]
   [else
    (let loop ([i 0])
         (cond
          [(= i (vector-length obj)) '(ok #t)]
          [else
           (let ([ref (vector-ref obj i)])
                (let ([result (validate-hash ref)])
                     (if (error? result)
                         `(error ,(string-append "ref at index "
                                                 (number->string i)
                                                 ": "
                                                 (cadr result)))
                         (loop (+ i 1)))))]))]))

;;; refs-all-valid? : Vector -> Boolean
;;; Predicate: are all refs valid hashes?
(define (refs-all-valid? refs)
  (and (vector? refs)
       (let loop ([i 0])
            (or (= i (vector-length refs))
                (and (hash? (vector-ref refs i))
                     (loop (+ i 1)))))))

;;; ====
;;; Block Validation
;;; ====

;;; validate-block : Any -> Result Bool
;;; Validate that an object is a well-formed block.
;;; Checks:
;;;   - Is a block record
;;;   - Tag is a symbol
;;;   - Payload is a bytevector
;;;   - Refs is a vector of valid hashes
;;; Returns (ok #t) if valid, (error message) otherwise.
(define (validate-block obj)
  (cond
   [(not (block? obj))
    '(error "not a block")]
   [(not (symbol? (block-tag obj)))
    '(error "block tag must be a symbol")]
   [(not (bytevector? (block-payload obj)))
    '(error "block payload must be a bytevector")]
   [else
    (validate-refs (block-refs obj))]))

;;; block-valid? : Any -> Boolean
;;; Predicate: is this a valid block?
(define (block-valid? obj)
  (and (block? obj)
       (symbol? (block-tag obj))
       (bytevector? (block-payload obj))
       (refs-all-valid? (block-refs obj))))

;;; ====
;;; Tag Validation
;;; ====

;;; validate-tag : Any -> Result Bool
;;; Validate that a tag is a symbol.
;;; Returns (ok #t) if valid, (error message) otherwise.
(define (validate-tag obj)
  (if (symbol? obj)
      '(ok #t)
      '(error "tag must be a symbol")))

;;; tag-valid? : Any -> Boolean
;;; Predicate: is this a valid tag?
(define (tag-valid? obj)
  (symbol? obj))

;;; ====
;;; Payload Validation
;;; ====

;;; validate-payload : Any -> Result Bool
;;; Validate that a payload is a bytevector.
;;; Returns (ok #t) if valid, (error message) otherwise.
(define (validate-payload obj)
  (if (bytevector? obj)
      '(ok #t)
      '(error "payload must be a bytevector")))

;;; payload-valid? : Any -> Boolean
;;; Predicate: is this a valid payload?
(define (payload-valid? obj)
  (bytevector? obj))

;;; ====
;;; Serialization Validation
;;; ====

;;; validate-serialized : Any -> Result Bool
;;; Validate that a bytevector could be a serialized block.
;;; Performs basic structural checks without full deserialization.
;;; Returns (ok #t) if valid, (error message) otherwise.
(define (validate-serialized obj)
  (cond
   ((not (bytevector? obj))
    '(error "serialized block must be a bytevector"))
   ((< (bytevector-length obj) 12)
    '(error "serialized block too short (minimum 12 bytes for headers)"))
   (else
    (validate-serialized-structure obj))))

;;; Helper: validate the structure of a serialized block
(define (validate-serialized-structure obj)
  (let ((tag-len (bytes-le->u32 obj 0)))
       (cond
        ((> tag-len 1000000)
         '(error "tag length unreasonably large"))
        ((< (bytevector-length obj) (+ 4 tag-len 4 4))
         '(error "serialized block truncated"))
        (else
         (validate-serialized-payload obj tag-len)))))

;;; Helper: validate payload portion
(define (validate-serialized-payload obj tag-len)
  (let ((payload-len (bytes-le->u32 obj (+ 4 tag-len))))
       (cond
        ((> payload-len 100000000)
         '(error "payload length unreasonably large"))
        (else
         (validate-serialized-refs obj tag-len payload-len)))))

;;; Helper: validate refs portion
(define (validate-serialized-refs obj tag-len payload-len)
  (let* ((refs-offset (+ 4 tag-len 4 payload-len))
         (have-refs-count (>= (bytevector-length obj) (+ refs-offset 4))))
        (cond
         ((not have-refs-count)
          '(error "serialized block truncated at refs count"))
         (else
          (let* ((refs-count (bytes-le->u32 obj refs-offset))
                 (expected-len (+ refs-offset 4 (* refs-count address-size))))
                (cond
                 ((> refs-count 1000000)
                  '(error "refs count unreasonably large"))
                 ((not (= (bytevector-length obj) expected-len))
                  `(error ,(string-append "serialized block length mismatch: expected "
                                          (number->string expected-len)
                                          ", got "
                                          (number->string (bytevector-length obj)))))
                 (else '(ok #t))))))))
