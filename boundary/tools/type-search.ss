(load "core/base/prelude.ss")

(doc 'module 'type-search)
(doc 'description "Type-driven search with pattern matching, type variables, and combinator suggestions")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(load "boundary/tools/string-utils.ss")

(doc 'section 'typed-index)

(doc 'note "Pre-parsed type signatures for fast searching")
(define *typed-index* (make-eq-hashtable))

;;; build-typed-index! : -> Void
;;; Parse all signatures in the symbol index at once.
;;; Call this after index-refresh! for faster type searches.
(define (build-typed-index!)
  (hashtable-clear! *typed-index*)
  (let ([count 0])
       (vector-for-each
        (lambda (sym)
                (let* ([entry (hashtable-ref *symbol-index* sym #f)]
                       [sig-str (and entry (cdr (assq 'signature entry)))])
                      (when sig-str
                            (let ([parsed (parse-signature sig-str)])
                                 (when parsed
                                       (hashtable-set! *typed-index* sym parsed)
                                       (set! count (+ count 1)))))))
        (hashtable-keys *symbol-index*))
       count))

;;; typed-index-get : Symbol -> ParsedType | #f
;;; Get pre-parsed type for a symbol.
(define (typed-index-get sym)
  (hashtable-ref *typed-index* sym #f))

;;; typed-index-size : -> Nat
;;; Number of symbols with parsed types.
(define (typed-index-size)
  (hashtable-size *typed-index*))

;;; ====
;;; Type Pattern Representation
;;; ====

;;; Type patterns extend types with pattern variables:
;;;   - _ : matches any type
;;;   - a, b, c, ... : type variables (match consistently)
;;;   - Exact types must match exactly

;;; type-pattern? : Any -> Boolean
(define (type-pattern? t)
  (cond
   [(eq? t '_) #t]  ; Wildcard
   [(symbol? t) #t] ; Variable or base type
   [(not (pair? t)) #f]
   [(eq? (car t) '->) (andmap type-pattern? (cdr t))]
   [(eq? (car t) '×) (andmap type-pattern? (cdr t))]
   [(eq? (car t) '+) #t]  ; Sum types
   [(eq? (car t) 'List) (and (= (length t) 2) (type-pattern? (cadr t)))]
   [(eq? (car t) 'Vector) (and (= (length t) 2) (type-pattern? (cadr t)))]
   [(eq? (car t) '∀) #t]  ; Polymorphic
   [(eq? (car t) 'Cap) #t]  ; Capability
   [else #f]))

;;; type-variable? : Symbol -> Boolean
;;; Type variables start with lowercase.
(define (type-variable? s)
  (and (symbol? s)
       (let ([str (symbol->string s)])
            (and (> (string-length str) 0)
                 (char-lower-case? (string-ref str 0))
                 (not (memq s '(a an the)))))))  ; Exclude articles

;;; base-type? : Symbol -> Boolean
(define (base-type? s)
  (memq s '(Nat Int Bool Char Symbol String Bytes Unit Void Hash)))

;;; ====
;;; Type Matching
;;; ====

;;; type-match? : Pattern × Type × Env -> Env | #f
;;; Match a type pattern against a concrete type.
;;; Returns updated environment binding type variables, or #f.
(define (type-match? pattern type env)
  (cond
   ;; Wildcard matches anything
   [(eq? pattern '_) env]
   
   ;; Type variable - check binding or create new
   [(type-variable? pattern)
    (let ([bound (assq pattern env)])
         (cond
          [(not bound) (cons (cons pattern type) env)]  ; New binding
          [(type-equal? (cdr bound) type) env]          ; Consistent
          [else #f]))]                                  ; Inconsistent
   
   ;; Base type - must match exactly
   [(base-type? pattern)
    (if (eq? pattern type) env #f)]
   
   ;; Compound patterns
   [(and (pair? pattern) (pair? type))
    (cond
     ;; Function types
     [(and (eq? (car pattern) '->) (eq? (car type) '->))
      (match-list (cdr pattern) (cdr type) env)]
     
     ;; Product types
     [(and (eq? (car pattern) '×) (eq? (car type) '×))
      (match-list (cdr pattern) (cdr type) env)]
     
     ;; List types
     [(and (eq? (car pattern) 'List) (eq? (car type) 'List))
      (type-match? (cadr pattern) (cadr type) env)]
     
     ;; Vector types
     [(and (eq? (car pattern) 'Vector) (eq? (car type) 'Vector))
      (type-match? (cadr pattern) (cadr type) env)]
     
     ;; Polymorphic - strip and match body
     [(and (eq? (car pattern) '∀) (eq? (car type) '∀))
      (type-match? (caddr pattern) (caddr type) env)]
     
     ;; Capability types
     [(and (eq? (car pattern) 'Cap) (eq? (car type) 'Cap))
      (and (equal? (cadr pattern) (cadr type))
           (type-match? (caddr pattern) (caddr type) env))]
     
     [else #f])]
   
   ;; Type constructor application
   [(and (pair? pattern) (symbol? (car pattern))
         (pair? type) (eq? (car pattern) (car type)))
    (match-list (cdr pattern) (cdr type) env)]
   
   [else #f]))

;;; match-list : (List Pattern) × (List Type) × Env -> Env | #f
;;; Match lists of patterns against types.
(define (match-list patterns types env)
  (cond
   [(and (null? patterns) (null? types)) env]
   [(or (null? patterns) (null? types)) #f]
   [else
    (let ([new-env (type-match? (car patterns) (car types) env)])
         (and new-env
              (match-list (cdr patterns) (cdr types) new-env)))]))

;;; type-equal? : Type × Type -> Boolean
;;; Check if two types are equal.
(define (type-equal? t1 t2)
  (equal? t1 t2))

;;; ====
;;; Signature Parsing
;;; ====

;;; parse-signature : String -> Type | #f
;;; Parse a type signature string into a type expression.
;;; Handles both S-expr format (-> a b) and infix format (a -> b).
(define (parse-signature sig-str)
  (guard (e [else #f])
         ;; First try direct S-expr parse
         (let ([port (open-input-string sig-str)])
              (let ([type (read port)])
                   (cond
                    [(eof-object? type) #f]
                    ;; If we got a valid S-expr type, use it
                    [(and (pair? type) (eq? (car type) '->)) type]
                    [(and (pair? type) (eq? (car type) '∀)) type]
                    [(and (pair? type) (eq? (car type) 'List)) type]
                    [(and (pair? type) (eq? (car type) 'Vector)) type]
                    [(and (pair? type) (eq? (car type) '×)) type]
                    ;; Simple type (symbol)
                    [(symbol? type) type]
                    ;; Otherwise, try converting infix format
                    [else (parse-infix-signature sig-str)])))))

;;; parse-infix-signature : String -> Type | #f
;;; Convert infix signature format (A -> B -> C) to S-expr (-> A B C).
(define (parse-infix-signature sig-str)
  (guard (e [else #f])
         ;; Replace Unicode arrow with ASCII
         (let* ([normalized (string-replace-arrow sig-str)]
                ;; Split on ->
                [parts (string-split-arrow normalized)])
               (if (< (length parts) 2)
                   #f  ; Not a function type
                   ;; Parse each part as a type
                   (let ([parsed-parts (map parse-type-component parts)])
                        (if (any not parsed-parts)
                            #f
                            (cons '-> parsed-parts)))))))

;;; string-replace-arrow : String -> String
;;; Replace Unicode arrows with ASCII.
(define (string-replace-arrow str)
  (let ([result (make-string (string-length str))]
        [len (string-length str)])
       (let loop ([i 0] [j 0])
            (cond
             [(>= i len) (substring result 0 j)]
             ;; Check for Unicode arrow (→ is 3 bytes in UTF-8)
             [(and (< (+ i 2) len)
                   (char=? (string-ref str i) #\→))
              (string-set! result j #\-)
              (string-set! result (+ j 1) #\>)
              (loop (+ i 1) (+ j 2))]
             [else
              (string-set! result j (string-ref str i))
              (loop (+ i 1) (+ j 1))]))))

;;; string-split-arrow : String -> (List String)
;;; Split string on " -> " delimiter.
;;; NOTE: Uses string-split-flex from boundary/tools/string-utils.ss
(define (string-split-arrow str)
  (map string-trim (string-split-flex str " -> ")))

;;; NOTE: string-find-substring can use string-index-of from prelude.ss
;;;       Keeping local alias for clarity in this module.
(define string-find-substring string-index-of)

;;; string-prefix-at? : String × String × Nat -> Bool
;;; NOTE: This is a specialized function - checks prefix at a specific position.
;;;       Not a duplicate of string-starts-with? which always checks from position 0.
(define (string-prefix-at? str prefix pos)
  (let ([p-len (string-length prefix)])
       (let loop ([i 0])
            (cond
             [(>= i p-len) #t]
             [(>= (+ pos i) (string-length str)) #f]
             [(char=? (string-ref str (+ pos i)) (string-ref prefix i))
              (loop (+ i 1))]
             [else #f]))))

;;; parse-type-component : String -> Type | #f
;;; Parse a single type component (may be compound like (List a) or simple like Nat).
(define (parse-type-component str)
  (let ([trimmed (string-trim str)])
       (guard (e [else (string->symbol trimmed)])
              (let ([port (open-input-string trimmed)])
                   (let ([type (read port)])
                        (if (eof-object? type)
                            (string->symbol trimmed)
                            type))))))

;;; normalize-signature : String -> Type | #f
;;; Parse and normalize a signature for matching.
(define (normalize-signature sig-str)
  (let ([parsed (parse-signature sig-str)])
       (and parsed
            (normalize-type parsed))))

;;; normalize-type : Type -> Type
;;; Normalize a type for matching (strip extras).
(define (normalize-type t)
  (cond
   [(not (pair? t)) t]
   [(eq? (car t) '∀)
    ;; Keep polymorphic structure
    `(∀ ,(cadr t) ,(normalize-type (caddr t)))]
   [else
    (cons (car t) (map normalize-type (cdr t)))]))

;;; ensure-typed-index! : -> Void
(define (ensure-typed-index!)
  (when (and (= (typed-index-size) 0)
             (> (hashtable-size *symbol-index*) 0))
        (build-typed-index!)))

;;; ====
;;; Search Engine
;;; ====

;;; search-by-type : Pattern -> (List Entry)
;;; Search the symbol index for matching types.
;;; Uses pre-parsed typed index for speed when available.
(define (search-by-type pattern)
  (let ([results '()])
       (guard (e [else '()])
              (ensure-typed-index!)
              (if (> (typed-index-size) 0)
                  ;; Use pre-parsed typed index (fast path)
                  (vector-for-each
                   (lambda (sym)
                           (let ([sig (typed-index-get sym)])
                                (when (and sig (type-match? pattern sig '()))
                                      (let ([entry (hashtable-ref *symbol-index* sym #f)])
                                           (when entry
                                                 (set! results (cons entry results)))))))
                   (hashtable-keys *typed-index*))
                  ;; Fall back to parsing on demand (slow path)
                  (vector-for-each
                   (lambda (sym)
                           (let* ([entry (hashtable-ref *symbol-index* sym #f)]
                                  [sig-str (and entry (cdr (assq 'signature entry)))])
                                 (when sig-str
                                       (let ([sig (parse-signature sig-str)])
                                            (when (and sig (type-match? pattern sig '()))
                                                  (set! results (cons entry results)))))))
                   (hashtable-keys *symbol-index*))))
       results))

;;; search-by-return : Type -> (List Entry)
;;; Find functions returning a specific type.
;;; Uses pre-parsed typed index for speed when available.
(define (search-by-return return-type)
  (let ([results '()])
       (guard (e [else '()])
              (ensure-typed-index!)
              (if (> (typed-index-size) 0)
                  ;; Use pre-parsed typed index (fast path)
                  (vector-for-each
                   (lambda (sym)
                           (let ([sig (typed-index-get sym)])
                                (when (and sig (pair? sig) (eq? (car sig) '->))
                                      (let ([ret (last (cdr sig))])
                                           (when (type-match? return-type ret '())
                                                 (let ([entry (hashtable-ref *symbol-index* sym #f)])
                                                      (when entry
                                                            (set! results (cons entry results)))))))))
                   (hashtable-keys *typed-index*))
                  ;; Fall back to parsing on demand (slow path)
                  (vector-for-each
                   (lambda (sym)
                           (let* ([entry (hashtable-ref *symbol-index* sym #f)]
                                  [sig-str (and entry (cdr (assq 'signature entry)))])
                                 (when sig-str
                                       (let ([sig (parse-signature sig-str)])
                                            (when (and sig (pair? sig) (eq? (car sig) '->))
                                                  (let ([ret (last (cdr sig))])
                                                       (when (type-match? return-type ret '())
                                                             (set! results (cons entry results)))))))))
                   (hashtable-keys *symbol-index*))))
       results))

;;; search-by-arg : Type -> (List Entry)
;;; Find functions taking a specific argument type.
;;; Uses pre-parsed typed index for speed when available.
(define (search-by-arg arg-type)
  (let ([results '()])
       (guard (e [else '()])
              (ensure-typed-index!)
              (if (> (typed-index-size) 0)
                  ;; Use pre-parsed typed index (fast path)
                  (vector-for-each
                   (lambda (sym)
                           (let ([sig (typed-index-get sym)])
                                (when (and sig (pair? sig) (eq? (car sig) '->))
                                      (let ([args (butlast (cdr sig))])
                                           (when (any (lambda (arg) (type-match? arg-type arg '())) args)
                                                 (let ([entry (hashtable-ref *symbol-index* sym #f)])
                                                      (when entry
                                                            (set! results (cons entry results)))))))))
                   (hashtable-keys *typed-index*))
                  ;; Fall back to parsing on demand (slow path)
                  (vector-for-each
                   (lambda (sym)
                           (let* ([entry (hashtable-ref *symbol-index* sym #f)]
                                  [sig-str (and entry (cdr (assq 'signature entry)))])
                                 (when sig-str
                                       (let ([sig (parse-signature sig-str)])
                                            (when (and sig (pair? sig) (eq? (car sig) '->))
                                                  (let ([args (butlast (cdr sig))])
                                                       (when (any (lambda (arg) (type-match? arg-type arg '())) args)
                                                             (set! results (cons entry results)))))))))
                   (hashtable-keys *symbol-index*))))
       results))

;;; last provided by prelude

;;; butlast : (List a) -> (List a)
(define (butlast lst)
  (if (null? (cdr lst))
      '()
      (cons (car lst) (butlast (cdr lst)))))

;;; any : (a -> Bool) × (List a) -> Bool
(define (any pred lst)
  (cond
   [(null? lst) #f]
   [(pred (car lst)) #t]
   [else (any pred (cdr lst))]))

;;; ====
;;; Combinator Suggestions
;;; ====

;;; suggest-combinators : Type -> (List Suggestion)
;;; Suggest ways to construct a value of the given type.
(define (suggest-combinators target-type)
  (let ([suggestions '()])
       
       ;; Direct matches - values/functions returning this type
       (let ([direct (search-by-return target-type)])
            (for-each
             (lambda (entry)
                     (set! suggestions
                           (cons (make-suggestion 'direct entry target-type)
                                 suggestions)))
             (take 5 direct)))
       
       ;; Composition matches - A -> B and B -> C when we want A -> C
       (when (and (pair? target-type)
                  (eq? (car target-type) '->))
             (let* ([args (butlast (cdr target-type))]
                    [ret (last (cdr target-type))])
                   ;; Find functions returning ret
                   (for-each
                    (lambda (entry)
                            (let* ([sig-str (cdr (assq 'signature entry))]
                                   [sig (and sig-str (parse-signature sig-str))])
                                  (when (and sig (pair? sig) (eq? (car sig) '->))
                                        ;; Check if we can find something to produce the arg
                                        (let ([needed-arg (car (butlast (cdr sig)))])
                                             (let ([producers (search-by-return needed-arg)])
                                                  (when (pair? producers)
                                                        (set! suggestions
                                                              (cons (make-suggestion
                                                                     'compose
                                                                     (list (car producers) entry)
                                                                     target-type)
                                                                    suggestions))))))))
                    (take 3 (search-by-return ret)))))
       
       suggestions))

;;; make-suggestion : Symbol × Any × Type -> Suggestion
(define (make-suggestion kind data target)
  `((kind . ,kind)
    (data . ,data)
    (target . ,target)))

;;; ====
;;; REPL Interface
;;; ====

;;; type-search : String -> void
;;; Search for functions matching a type pattern.
(define (type-search pattern-str)
  (display "\n")
  (printf "  Type Search: ~a\n" pattern-str)
  (display "  --------------------------------\n\n")
  
  ;; Ensure index is loaded
  (unless (> (hashtable-size *symbol-index*) 0)
          (display "  Index not loaded. Running index-refresh!...\n")
          (index-refresh!))
  
  (let ([pattern (parse-signature pattern-str)])
       (if (not pattern)
           (printf "  Invalid type pattern: ~a\n" pattern-str)
           (let ([results (search-by-type pattern)])
                (if (null? results)
                    (display "  No matching functions found.\n")
                    (begin
                     (printf "  Found ~a matches:\n\n" (length results))
                     (for-each display-search-result
                               (take 15 results))
                     (when (> (length results) 15)
                           (printf "\n  ... and ~a more\n"
                                   (- (length results) 15))))))))
  (display "\n"))

;;; type-search-return : String -> void
;;; Search for functions returning a specific type.
(define (type-search-return type-str)
  (display "\n")
  (printf "  Functions returning: ~a\n" type-str)
  (display "  --------------------------------\n\n")
  
  (unless (> (hashtable-size *symbol-index*) 0)
          (index-refresh!))
  
  (let ([return-type (parse-signature type-str)])
       (if (not return-type)
           (printf "  Invalid type: ~a\n" type-str)
           (let ([results (search-by-return return-type)])
                (if (null? results)
                    (display "  No matching functions found.\n")
                    (begin
                     (printf "  Found ~a matches:\n\n" (length results))
                     (for-each display-search-result
                               (take 15 results)))))))
  (display "\n"))

;;; type-search-arg : String -> void
;;; Search for functions taking a specific argument type.
(define (type-search-arg type-str)
  (display "\n")
  (printf "  Functions taking: ~a\n" type-str)
  (display "  --------------------------------\n\n")
  
  (unless (> (hashtable-size *symbol-index*) 0)
          (index-refresh!))
  
  (let ([arg-type (parse-signature type-str)])
       (if (not arg-type)
           (printf "  Invalid type: ~a\n" type-str)
           (let ([results (search-by-arg arg-type)])
                (if (null? results)
                    (display "  No matching functions found.\n")
                    (begin
                     (printf "  Found ~a matches:\n\n" (length results))
                     (for-each display-search-result
                               (take 15 results)))))))
  (display "\n"))

;;; type-search-suggest : String -> void
;;; Suggest ways to construct a value of a type.
(define (type-search-suggest type-str)
  (display "\n")
  (printf "  Suggestions for constructing: ~a\n" type-str)
  (display "  --------------------------------\n\n")
  
  (unless (> (hashtable-size *symbol-index*) 0)
          (index-refresh!))
  
  (let ([target (parse-signature type-str)])
       (if (not target)
           (printf "  Invalid type: ~a\n" type-str)
           (let ([suggestions (suggest-combinators target)])
                (if (null? suggestions)
                    (display "  No suggestions found.\n")
                    (for-each display-suggestion suggestions)))))
  (display "\n"))

;;; display-search-result : Entry -> void
(define (display-search-result entry)
  (let ([name (cdr (assq 'name entry))]
        [sig (cdr (assq 'signature entry))]
        [file (cdr (assq 'file entry))]
        [line (cdr (assq 'line entry))])
       (printf "    ~a" name)
       (when sig
             (printf " : ~a" sig))
       (display "\n")
       (printf "      ~a:~a\n" file line)))

;;; display-suggestion : Suggestion -> void
(define (display-suggestion sug)
  (let ([kind (cdr (assq 'kind sug))]
        [data (cdr (assq 'data sug))])
       (case kind
             [(direct)
              (display "  Direct:\n")
              (display-search-result data)]
             [(compose)
              (display "  Composition:\n")
              (for-each
               (lambda (entry)
                       (display "    ")
                       (display-search-result entry))
               data)])))

;;; ====
;;; Help
;;; ====

;;; type-search-help : -> void
(define (type-search-help)
  (display "\n")
  (display "  --- TYPE-DRIVEN SEARCH COMMANDS ---------------------------------\n")
  (display "\n")
  (display "  Basic Search:\n")
  (display "    (type-search \"(-> Nat Nat)\")        Functions Nat -> Nat\n")
  (display "    (type-search \"(-> a a)\")            Identity-like functions\n")
  (display "    (type-search \"(-> (List a) Nat)\")   List to Nat functions\n")
  (display "\n")
  (display "  Targeted Search:\n")
  (display "    (type-search-return \"Bool\")         Functions returning Bool\n")
  (display "    (type-search-arg \"String\")          Functions taking String\n")
  (display "\n")
  (display "  Suggestions:\n")
  (display "    (type-search-suggest \"(-> Nat Bool)\")  Ways to build Nat -> Bool\n")
  (display "\n")
  (display "  Type Pattern Syntax:\n")
  (display "    (-> T1 T2)           Function type\n")
  (display "    (List T)             List of T\n")
  (display "    (× T1 T2)            Product (tuple)\n")
  (display "    a, b, c              Type variables (match consistently)\n")
  (display "    _                    Wildcard (matches anything)\n")
  (display "    Nat, Bool, String    Base types\n")
  (display "\n")
  (display "  Performance:\n")
  (display "    (build-typed-index!)           Pre-parse types for faster search\n")
  (display "    (type-search-stats)            Show index statistics\n")
  (display "\n")
  (display "  Help:\n")
  (display "    (type-search-help)             Show this help\n")
  (display "\n"))

;;; ====
;;; Stats
;;; ====

;;; type-search-stats : -> void
(define (type-search-stats)
  (display "\n")
  (display "  Type Search Statistics\n")
  (display "  --------------------------------\n\n")
  
  (let ([total 0]
        [with-sig 0])
       (vector-for-each
        (lambda (sym)
                (set! total (+ total 1))
                (let* ([entry (hashtable-ref *symbol-index* sym #f)]
                       [sig (and entry (cdr (assq 'signature entry)))])
                      (when sig
                            (set! with-sig (+ with-sig 1)))))
        (hashtable-keys *symbol-index*))
       
       (printf "  Total symbols indexed: ~a\n" total)
       (printf "  Symbols with signatures: ~a (~a%)\n"
               with-sig
               (if (> total 0)
                   (quotient (* with-sig 100) total)
                   0))
       (let ([typed-count (typed-index-size)])
            (printf "  Pre-parsed types: ~a~a\n"
                    typed-count
                    (if (> typed-count 0) " (fast path enabled)" " (run build-typed-index!)"))))
  (display "\n"))

;;; ====
;;; Initialization
;;; ====

(display "\n")
(display "  Type-Driven Search Loaded\n")
(display "  --------------------------------\n")
(display "  Use (type-search-help) for available commands.\n")
(display "\n")
