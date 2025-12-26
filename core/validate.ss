;;; core/validate.ss — Validation utilities for blocks and hashes
;;;
;;; Pure, total validation functions that check structural invariants:
;;;   - Block structure (tag, payload, refs are correct types)
;;;   - Hash format (32-byte bytevectors)
;;;   - Reference validity (all refs are valid hashes)
;;;   - Hex string format (64 hex characters)
;;;
;;; These are defensive checks for Shell/boundary code.
;;; Core assumes perfect input, but Shell must validate before calling Core.
;;;
;;; All validators return either:
;;;   - #t for valid input
;;;   - A string describing the validation error

(load "block.ss")

;;; ============================================================
;;; Hash Validation
;;; ============================================================

;;; hash? : Any → Boolean
;;; Predicate: is this a valid hash (32-byte bytevector)?
(define (hash? obj)
  (and (bytevector? obj)
       (= (bytevector-length obj) hash-size)))

;;; validate-hash : Any → Boolean | String
;;; Validate that an object is a proper hash.
;;; Returns #t if valid, error string otherwise.
(define (validate-hash obj)
  (cond
    [(not (bytevector? obj))
     "hash must be a bytevector"]
    [(not (= (bytevector-length obj) hash-size))
     (string-append "hash must be exactly "
                    (number->string hash-size)
                    " bytes, got "
                    (number->string (bytevector-length obj)))]
    [else #t]))

;;; hex-char? : Char → Boolean
;;; Check if character is a valid hexadecimal digit.
(define (hex-char? c)
  (or (char<=? #\0 c #\9)
      (char<=? #\a c #\f)
      (char<=? #\A c #\F)))

;;; hex-string? : Any → Boolean
;;; Predicate: is this a valid 64-character hex string (for SHA-256)?
(define (hex-string? obj)
  (and (string? obj)
       (= (string-length obj) (* 2 hash-size))
       (let loop ([i 0])
         (or (= i (string-length obj))
             (and (hex-char? (string-ref obj i))
                  (loop (+ i 1)))))))

;;; validate-hex-string : Any → Boolean | String
;;; Validate that a string is a proper hex hash string.
;;; Returns #t if valid, error string otherwise.
(define (validate-hex-string obj)
  (cond
    [(not (string? obj))
     "hex hash must be a string"]
    [(not (= (string-length obj) (* 2 hash-size)))
     (string-append "hex hash must be exactly "
                    (number->string (* 2 hash-size))
                    " characters, got "
                    (number->string (string-length obj)))]
    [else
     (let loop ([i 0])
       (cond
         [(= i (string-length obj)) #t]
         [(not (hex-char? (string-ref obj i)))
          (string-append "invalid hex character at position "
                        (number->string i)
                        ": "
                        (string (string-ref obj i)))]
         [else (loop (+ i 1))]))]))

;;; ============================================================
;;; Reference Validation
;;; ============================================================

;;; validate-refs : Any → Boolean | String
;;; Validate that refs is a vector of valid hashes.
;;; Returns #t if valid, error string otherwise.
(define (validate-refs obj)
  (cond
    [(not (vector? obj))
     "refs must be a vector"]
    [else
     (let loop ([i 0])
       (cond
         [(= i (vector-length obj)) #t]
         [else
          (let ([ref (vector-ref obj i)])
            (let ([result (validate-hash ref)])
              (if (string? result)
                  (string-append "ref at index "
                                (number->string i)
                                ": "
                                result)
                  (loop (+ i 1)))))]))]))

;;; refs-all-valid? : Vector → Boolean
;;; Predicate: are all refs valid hashes?
(define (refs-all-valid? refs)
  (and (vector? refs)
       (let loop ([i 0])
         (or (= i (vector-length refs))
             (and (hash? (vector-ref refs i))
                  (loop (+ i 1)))))))

;;; ============================================================
;;; Block Validation
;;; ============================================================

;;; validate-block : Any → Boolean | String
;;; Validate that an object is a well-formed block.
;;; Checks:
;;;   - Is a block record
;;;   - Tag is a symbol
;;;   - Payload is a bytevector
;;;   - Refs is a vector of valid hashes
;;; Returns #t if valid, error string otherwise.
(define (validate-block obj)
  (cond
    [(not (block? obj))
     "not a block"]
    [(not (symbol? (block-tag obj)))
     "block tag must be a symbol"]
    [(not (bytevector? (block-payload obj)))
     "block payload must be a bytevector"]
    [else
     (validate-refs (block-refs obj))]))

;;; block-valid? : Any → Boolean
;;; Predicate: is this a valid block?
(define (block-valid? obj)
  (and (block? obj)
       (symbol? (block-tag obj))
       (bytevector? (block-payload obj))
       (refs-all-valid? (block-refs obj))))

;;; ============================================================
;;; Tag Validation
;;; ============================================================

;;; validate-tag : Any → Boolean | String
;;; Validate that a tag is a symbol.
;;; Returns #t if valid, error string otherwise.
(define (validate-tag obj)
  (if (symbol? obj)
      #t
      "tag must be a symbol"))

;;; tag-valid? : Any → Boolean
;;; Predicate: is this a valid tag?
(define (tag-valid? obj)
  (symbol? obj))

;;; ============================================================
;;; Payload Validation
;;; ============================================================

;;; validate-payload : Any → Boolean | String
;;; Validate that a payload is a bytevector.
;;; Returns #t if valid, error string otherwise.
(define (validate-payload obj)
  (if (bytevector? obj)
      #t
      "payload must be a bytevector"))

;;; payload-valid? : Any → Boolean
;;; Predicate: is this a valid payload?
(define (payload-valid? obj)
  (bytevector? obj))

;;; ============================================================
;;; Serialization Validation
;;; ============================================================

;;; validate-serialized : Any → Boolean | String
;;; Validate that a bytevector could be a serialized block.
;;; Performs basic structural checks without full deserialization.
;;; Returns #t if valid, error string otherwise.
(define (validate-serialized obj)
  (cond
    [(not (bytevector? obj))
     "serialized block must be a bytevector"]
    [(< (bytevector-length obj) 12)
     "serialized block too short (minimum 12 bytes for headers)"]
    [else
     ;; Check if we can at least read the tag length
     (let ([tag-len (bytes-le->u32 obj 0)])
       (cond
         [(> tag-len 1000000)
          "tag length unreasonably large"]
         [(< (bytevector-length obj) (+ 4 tag-len 4 4))
          "serialized block truncated"]
         [else
          ;; Check payload length
          (let ([payload-len (bytes-le->u32 obj (+ 4 tag-len))])
            (cond
              [(> payload-len 100000000)
               "payload length unreasonably large"]
              [else
               ;; Check refs count
               (let* ([refs-offset (+ 4 tag-len 4 payload-len)]
                      [_ (if (< (bytevector-length obj) (+ refs-offset 4))
                             (error 'validate-serialized "truncated at refs count")
                             #f)]
                      [refs-count (bytes-le->u32 obj refs-offset)]
                      [expected-len (+ refs-offset 4 (* refs-count hash-size))])
                 (cond
                   [(> refs-count 1000000)
                    "refs count unreasonably large"]
                   [(not (= (bytevector-length obj) expected-len))
                    (string-append "serialized block length mismatch: expected "
                                  (number->string expected-len)
                                  ", got "
                                  (number->string (bytevector-length obj)))]
                   [else #t]))])]))]))
