;;; lattice/meta/type-search.ss — Type-Aware Search
;;;
;;; Adds type-aware filtering to the lattice search.
;;; Uses the existing BM25 search and filters results by type signature.
;;;
;;; Usage:
;;;   (lf-type "Monad")           ; Search exports mentioning Monad in type
;;;   (lf-input "Matrix")         ; Functions that take Matrix as input
;;;   (lf-output "Maybe")         ; Functions that return Maybe
;;;
;;; Requires lattice/meta/search.ss to be loaded first.

;;; ============================================================
;;; Type Signature Utilities
;;; ============================================================

;;; has-type-sig? : String -> Bool
;;; Check if a docstring contains a type signature (has : and ->)
(define (has-type-sig? docstring)
  (and (string? docstring)
       (string-contains? docstring ":")
       (or (string-contains? docstring "→")
           (string-contains? docstring "->"))))

;;; string-contains? : String × String -> Bool
(define (string-contains? haystack needle)
  (let ([h-len (string-length haystack)]
        [n-len (string-length needle)])
    (and (<= n-len h-len)
         (let loop ([i 0])
           (cond
             [(> (+ i n-len) h-len) #f]
             [(string=? (substring haystack i (+ i n-len)) needle) #t]
             [else (loop (+ i 1))])))))

;;; extract-type-sig : String -> String | #f
;;; Extract type signature from docstring (part between : and description)
(define (extract-type-sig docstring)
  (if (not (string? docstring))
      #f
      (let ([colon-pos (string-find docstring #\:)])
        (if (not colon-pos)
            #f
            (let* ([after-colon (substring docstring (+ colon-pos 1)
                                           (string-length docstring))]
                   [trimmed (string-trim-left-ws after-colon)])
              ;; Return up to end of type (heuristic: stop at lowercase word after type)
              trimmed)))))

;;; string-find : String × Char -> Int | #f
(define (string-find str ch)
  (let ([len (string-length str)])
    (let loop ([i 0])
      (cond
        [(>= i len) #f]
        [(char=? (string-ref str i) ch) i]
        [else (loop (+ i 1))]))))

;;; string-trim-left-ws : String -> String
(define (string-trim-left-ws str)
  (let ([len (string-length str)])
    (let loop ([i 0])
      (cond
        [(>= i len) ""]
        [(char-whitespace? (string-ref str i)) (loop (+ i 1))]
        [else (substring str i len)]))))

;;; in-input-position? : String × String -> Bool
;;; Check if query appears before the arrow in a type signature
(define (in-input-position? type-sig query)
  (let ([arrow-pos (or (string-find-substr type-sig "→")
                       (string-find-substr type-sig "->"))])
    (if (not arrow-pos)
        #f
        (let ([before-arrow (string-downcase (substring type-sig 0 arrow-pos))])
          (string-contains? before-arrow (string-downcase query))))))

;;; in-output-position? : String × String -> Bool
;;; Check if query appears after the arrow in a type signature
(define (in-output-position? type-sig query)
  (let ([arrow-pos (or (string-find-substr type-sig "→")
                       (string-find-substr type-sig "->"))])
    (if (not arrow-pos)
        #f
        (let* ([arrow-len (if (string-find-substr type-sig "→") 1 2)]
               [after-arrow (string-downcase
                            (substring type-sig (+ arrow-pos arrow-len)
                                       (string-length type-sig)))])
          (string-contains? after-arrow (string-downcase query))))))

;;; string-find-substr : String × String -> Int | #f
(define (string-find-substr haystack needle)
  (let ([h-len (string-length haystack)]
        [n-len (string-length needle)])
    (if (> n-len h-len)
        #f
        (let loop ([i 0])
          (cond
            [(> (+ i n-len) h-len) #f]
            [(string=? (substring haystack i (+ i n-len)) needle) i]
            [else (loop (+ i 1))])))))

;;; ============================================================
;;; Type-Aware Search (uses existing BM25 infrastructure)
;;; ============================================================

;;; lf-type : String -> Void
;;; Search for functions with matching type signatures
(define (lf-type query)
  (let* ([results (lattice-find query)]  ; Use existing BM25 search
         [filtered (filter-by-type-sig results query)])
    (print-type-results filtered query)))

;;; lf-input : String -> Void
;;; Search for functions that take query type as input
(define (lf-input query)
  (let* ([results (lattice-find query)]
         [filtered (filter (lambda (r)
                            (let ([doc (get-docstring (result-name r))])
                              (and doc
                                   (has-type-sig? doc)
                                   (in-input-position? doc query))))
                          results)])
    (print-type-results filtered query)))

;;; lf-output : String -> Void
;;; Search for functions that return query type
(define (lf-output query)
  (let* ([results (lattice-find query)]
         [filtered (filter (lambda (r)
                            (let ([doc (get-docstring (result-name r))])
                              (and doc
                                   (has-type-sig? doc)
                                   (in-output-position? doc query))))
                          results)])
    (print-type-results filtered query)))

;;; filter-by-type-sig : (List Result) × String -> (List Result)
;;; Keep only results that have type signatures containing query
(define (filter-by-type-sig results query)
  (let ([q-lower (string-downcase query)])
    (filter (lambda (r)
              (let ([doc (get-docstring (result-name r))])
                (and doc
                     (has-type-sig? doc)
                     (string-contains? (string-downcase doc) q-lower))))
            results)))

;;; result-name : Result -> Symbol
;;; Extract the name from a search result
(define (result-name r)
  (if (pair? r)
      (if (symbol? (car r)) (car r) (cadr r))
      r))

;;; ============================================================
;;; Output
;;; ============================================================

;;; print-type-results : (List Result) × String -> Void
(define (print-type-results results query)
  (if (null? results)
      (printf "No type matches for '~a'.\n" query)
      (begin
        (printf "Type matches for '~a' (~a results):\n" query (length results))
        (for-each
         (lambda (r)
           (let* ([name (result-name r)]
                  [doc (get-docstring name)]
                  [sig (if doc (extract-type-sig doc) #f)])
             (printf "  ~a\n" name)
             (when sig
               (printf "    ~a\n" (truncate-string sig 70)))))
         (take-n 10 results)))))

;;; truncate-string : String × Int -> String
(define (truncate-string str max-len)
  (if (<= (string-length str) max-len)
      str
      (string-append (substring str 0 (- max-len 3)) "...")))

;;; take-n : Int × (List α) -> (List α)
(define (take-n n lst)
  (if (or (<= n 0) (null? lst))
      '()
      (cons (car lst) (take-n (- n 1) (cdr lst)))))

;;; ============================================================
;;; REPL Interface
;;; ============================================================

(printf "type-search.ss loaded.\n")
(printf "  (lf-type \"query\")      - Search types for query\n")
(printf "  (lf-input \"Type\")      - Functions taking Type as input\n")
(printf "  (lf-output \"Type\")     - Functions returning Type\n")
