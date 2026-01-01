; ============================================================
; Encoding Utilities
; Hex, Base64, and byte manipulation
; ============================================================

; ============================================
; Hex Encoding
; ============================================

; hex-chars: Hex digit characters
(hex-chars "0123456789abcdef")

; nibble->hex: Convert nibble (0-15) to hex char
(nibble->hex (fn (n)
                 (string-ref hex-chars (mod n 16))))

; hex->nibble: Convert hex char to nibble
(hex->nibble (fn (c)
                 (let ((code (char->integer c)))
                      (if (and (>= code 48) (<= code 57))   ; 0-9
                          (- code 48)
                          (if (and (>= code 97) (<= code 102)) ; a-f
                              (+ (- code 97) 10)
                              (if (and (>= code 65) (<= code 70)) ; A-F
                                  (+ (- code 65) 10)
                                  0))))))

; byte->hex: Convert byte (0-255) to hex string
(byte->hex (fn (b)
               (list->string (list (nibble->hex (/ b 16))
                                   (nibble->hex (mod b 16))))))

; hex->byte: Convert hex string to byte
(hex->byte (fn (s)
               (let ((chars (string->list s)))
                    (if (< (length chars) 2)
                        0
                        (+ (* (hex->nibble (car chars)) 16)
                           (hex->nibble (cadr chars)))))))

; bytes->hex: Convert list of bytes to hex string
(bytes->hex (fn (bytes)
                (string-join (map byte->hex bytes) "")))

; hex->bytes: Convert hex string to list of bytes
(hex->bytes (fn (s)
                (let ((pairs (chunks 2 (string->list s))))
                     (map (fn (pair) (hex->byte (list->string pair))) pairs))))

; string->bytes: Convert string to list of byte values
(string->bytes (fn (s)
                   (map char->integer (string->list s))))

; bytes->string: Convert list of byte values to string
(bytes->string (fn (bytes)
                   (list->string (map integer->char bytes))))

; string->hex: Convert string to hex representation
(string->hex (fn (s)
                 (bytes->hex (string->bytes s))))

; hex->string: Convert hex representation to string
(hex->string (fn (h)
                 (bytes->string (hex->bytes h))))

; ============================================
; Base64 Encoding
; ============================================

; b64-alphabet: Base64 alphabet
(b64-alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

; b64-char: Get base64 character for index
(b64-char (fn (i)
              (if (< i 0) #\=
                  (if (>= i 64) #\=
                      (string-ref b64-alphabet i)))))

; b64-index: Get index for base64 character
(b64-index (fn (c)
               (if (eq? c #\=)
                   (- 0 1)
                   (let ((find (fix find
                                    (fn (i)
                                        (if (>= i 64)
                                            (- 0 1)
                                            (if (eq? (string-ref b64-alphabet i) c)
                                                i
                                                (find (+ i 1))))))))
                        (find 0)))))

; b64-encode-triple: Encode 3 bytes to 4 base64 chars
(b64-encode-triple (fn (b1 b2 b3)
                       (let ((n (+ (* b1 65536) (* b2 256) b3)))
                            (list (b64-char (/ n 262144))
                                  (b64-char (mod (/ n 4096) 64))
                                  (b64-char (mod (/ n 64) 64))
                                  (b64-char (mod n 64))))))

; b64-encode-bytes: Encode list of bytes to base64 string
(b64-encode-bytes (fn (bytes)
                      (let ((encode-group (fn (group)
                                              (let ((len (length group)))
                                                   (if (= len 3)
                                                       (b64-encode-triple (car group) (cadr group) (caddr group))
                                                       (if (= len 2)
                                                           (let ((result (b64-encode-triple (car group) (cadr group) 0)))
                                                                (list (car result) (cadr result) (caddr result) #\=))
                                                           (if (= len 1)
                                                               (let ((result (b64-encode-triple (car group) 0 0)))
                                                                    (list (car result) (cadr result) #\= #\=))
                                                               '())))))))
                           (list->string (concat (map encode-group (chunks 3 bytes)))))))

; b64-decode-quad: Decode 4 base64 chars to bytes
(b64-decode-quad (fn (c1 c2 c3 c4)
                     (let ((i1 (b64-index c1))
                           (i2 (b64-index c2))
                           (i3 (b64-index c3))
                           (i4 (b64-index c4)))
                          (let ((n (+ (* i1 262144) (* i2 4096)
                                      (* (if (< i3 0) 0 i3) 64)
                                      (if (< i4 0) 0 i4))))
                               (if (eq? c3 #\=)
                                   (list (/ n 65536))
                                   (if (eq? c4 #\=)
                                       (list (/ n 65536) (mod (/ n 256) 256))
                                       (list (/ n 65536) (mod (/ n 256) 256) (mod n 256))))))))

; b64-decode-bytes: Decode base64 string to list of bytes
(b64-decode-bytes (fn (s)
                      (let ((chars (string->list s)))
                           (let ((groups (chunks 4 chars)))
                                (concat (map (fn (g)
                                                 (if (< (length g) 4)
                                                     '()
                                                     (b64-decode-quad (car g) (cadr g) (caddr g) (cadddr g))))
                                             groups))))))

; b64-encode: Encode string to base64
(b64-encode (fn (s)
                (b64-encode-bytes (string->bytes s))))

; b64-decode: Decode base64 to string
(b64-decode (fn (s)
                (bytes->string (b64-decode-bytes s))))

; ============================================
; Hash and Checksum Functions
; ============================================

; djb2-hash: DJB2 hash algorithm
(djb2-hash (fn (s)
               (foldl (fn (hash c)
                          (mod (+ (* hash 33) (char->integer c)) 4294967296))
                      5381
                      (string->list s))))

; fnv1a-hash: FNV-1a hash algorithm
(fnv1a-hash (fn (s)
                (foldl (fn (hash c)
                           (let ((xored (bitxor hash (char->integer c))))
                                (mod (* xored 16777619) 4294967296)))
                       2166136261
                       (string->list s))))

; simple-checksum: Simple additive checksum
(simple-checksum (fn (bytes)
                     (mod (sum-list bytes) 256)))

; xor-checksum: XOR checksum
(xor-checksum (fn (bytes)
                  (foldl bitxor 0 bytes)))

; fletcher16: Fletcher-16 checksum
(fletcher16 (fn (bytes)
                (let ((result (foldl (fn (acc b)
                                         (let ((s1 (+ (car acc) b))
                                               (s2 (+ (cdr acc) (+ (car acc) b))))
                                              (cons (mod s1 255) (mod s2 255))))
                                     (cons 0 0)
                                     bytes)))
                     (+ (* (cdr result) 256) (car result)))))

; adler32: Adler-32 checksum
(adler32 (fn (bytes)
             (let ((result (foldl (fn (acc b)
                                      (let ((a (+ (car acc) b))
                                            (b-acc (+ (cdr acc) (+ (car acc) b))))
                                           (cons (mod a 65521) (mod b-acc 65521))))
                                  (cons 1 0)
                                  bytes)))
                  (+ (* (cdr result) 65536) (car result)))))

; hash-combine: Combine two hashes
(hash-combine (fn (h1 h2)
                  (bitxor h1 (+ h2 2654435769 (shl h1 6) (shr h1 2)))))

; hash-list: Hash a list of values
(hash-list (fn (values)
               (foldl (fn (h v) (hash-combine h (djb2-hash (value->string v))))
                      0
                      values)))

; string-hash: Hash a string (alias for djb2)
(string-hash djb2-hash)
