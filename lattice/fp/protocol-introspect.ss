(load "lattice/fp/protocol.ss")

(doc 'module 'protocol-introspect)
(doc 'description "Protocol introspection and visualization tools. Provides reverse lookups (type → protocols), documentation storage, and ASCII visualization of protocol/type relationships.")
(doc 'layer 'lattice)
(doc 'purity 'partial)
(doc 'dependencies '(lattice/fp/protocol.ss))

;;; ============================================================
;;; Protocol Documentation Registry
;;; ============================================================

(doc 'section 'documentation)

(define *protocol-docs* (make-hashtable symbol-hash eq?))
(doc *protocol-docs* 'type '(Hashtable Symbol ProtocolDoc))
(doc *protocol-docs* 'description "Registry mapping protocol names to their documentation records")

;;; ProtocolDoc structure:
;;;   (protocol-doc name docstring signature module)
(define (make-protocol-doc name docstring signature module)
  (list 'protocol-doc name docstring signature module))

(define (protocol-doc? x)
  (and (pair? x) (eq? 'protocol-doc (car x))))

(define (protocol-doc-name doc) (list-ref doc 1))
(define (protocol-doc-docstring doc) (list-ref doc 2))
(define (protocol-doc-signature doc) (list-ref doc 3))
(define (protocol-doc-module doc) (list-ref doc 4))

;;; register-protocol-doc! : Symbol × String × (Or Signature #f) × (Or Symbol #f) → Void
;;; Register documentation for a protocol.
(define (register-protocol-doc! name docstring signature module)
  (hashtable-set! *protocol-docs* name
    (make-protocol-doc name docstring signature module)))

;;; get-protocol-doc : Symbol → ProtocolDoc | #f
(define (get-protocol-doc name)
  (hashtable-ref *protocol-docs* name #f))

;;; protocol-docstring : Symbol → String | #f
;;; Get just the docstring for a protocol.
(define (protocol-docstring name)
  (let ([doc (get-protocol-doc name)])
    (and doc (protocol-doc-docstring doc))))

;;; ============================================================
;;; Reverse Lookup: Type → Protocols
;;; ============================================================

(doc 'section 'reverse-lookup)

;;; protocols-for-type : Symbol → (List Symbol)
;;; List all protocols that a type implements.
;;; Scans the entire protocol registry (O(protocols × types)).
(define (protocols-for-type type-tag)
  (doc 'type '(-> Symbol (List Symbol)))
  (doc 'description "Find all protocols implemented by a given type tag")
  (filter
    (lambda (proto)
      (type-implements? type-tag proto))
    (list-protocols)))

;;; all-type-tags : → (List Symbol)
;;; Collect all unique type tags across all protocols.
(define (all-type-tags)
  (doc 'type '(-> (List Symbol)))
  (doc 'description "List all type tags that implement any protocol")
  (let ([seen (make-hashtable symbol-hash eq?)])
    (for-each
      (lambda (proto)
        (for-each
          (lambda (type-tag)
            (hashtable-set! seen type-tag #t))
          (protocol-implementations proto)))
      (list-protocols))
    (vector->list (hashtable-keys seen))))

;;; ============================================================
;;; Protocol Description
;;; ============================================================

(doc 'section 'describe)

;;; protocol-describe : Symbol → String
;;; Generate a rich description of a protocol.
(define (protocol-describe proto-name)
  (doc 'type '(-> Symbol String))
  (doc 'description "Generate human-readable description of a protocol")
  (let* ([doc (get-protocol-doc proto-name)]
         [impls (protocol-implementations proto-name)]
         [docstring (if doc (protocol-doc-docstring doc) "(no documentation)")]
         [sig (if doc (protocol-doc-signature doc) #f)]
         [mod (if doc (protocol-doc-module doc) #f)])
    (string-append
      "Protocol: " (symbol->string proto-name) "\n"
      (make-string 60 #\─) "\n"
      (if sig
          (string-append "Signature: " (format "~a" sig) "\n")
          "")
      (if mod
          (string-append "Module:    " (symbol->string mod) "\n")
          "")
      "Doc:       " docstring "\n"
      "\n"
      "Implementations (" (number->string (length impls)) "):\n"
      (if (null? impls)
          "  (none)\n"
          (apply string-append
            (map (lambda (t) (string-append "  • " (symbol->string t) "\n")) impls))))))

;;; type-describe : Symbol → String
;;; Generate a description of what protocols a type implements.
(define (type-describe type-tag)
  (doc 'type '(-> Symbol String))
  (doc 'description "Generate human-readable description of a type's protocol implementations")
  (let ([protos (protocols-for-type type-tag)])
    (string-append
      "Type: " (symbol->string type-tag) "\n"
      (make-string 60 #\─) "\n"
      "Implements (" (number->string (length protos)) " protocols):\n"
      (if (null? protos)
          "  (none)\n"
          (apply string-append
            (map (lambda (p)
                   (let ([doc (get-protocol-doc p)])
                     (string-append
                       "  • " (symbol->string p)
                       (if (and doc (protocol-doc-docstring doc))
                           (string-append " — " (truncate-string (protocol-doc-docstring doc) 40))
                           "")
                       "\n")))
                 protos))))))

;;; truncate-string : String × Number → String
(define (truncate-string s max-len)
  (if (<= (string-length s) max-len)
      s
      (string-append (substring s 0 (- max-len 3)) "...")))

;;; ============================================================
;;; Protocol Matrix Visualization
;;; ============================================================

(doc 'section 'visualization)

;;; protocol-matrix : [(List Symbol)] [(List Symbol)] → String
;;; Generate an ASCII matrix showing which types implement which protocols.
;;; Rows = protocols, Columns = types.
;;; Optional type-tags/proto-names to limit display.
(define (protocol-matrix . args)
  (doc 'type '(-> (Optional (List Symbol)) (Optional (List Symbol)) String))
  (doc 'description "ASCII matrix of protocol/type relationships. ✓ = implements")
  (let* ([types (if (and (pair? args) (pair? (car args)))
                    (car args)
                    (all-type-tags))]
         [protos (if (and (pair? args) (pair? (cdr args)) (pair? (cadr args)))
                     (cadr args)
                     (list-protocols))])
    (if (or (null? types) (null? protos))
        "(no protocols or types registered)\n"
        (build-matrix-string protos types))))

;;; build-matrix-string : (List Symbol) × (List Symbol) → String
(define (build-matrix-string protos types)
  (let* ([proto-col-width (min 25 (+ 2 (apply max (map symbol-length protos))))]
         [type-col-width 12]
         [header (build-header types type-col-width proto-col-width)]
         [rows (map (lambda (p) (build-row p types type-col-width proto-col-width)) protos)])
    (string-append
      header
      (make-string (+ proto-col-width (* type-col-width (length types))) #\─) "\n"
      (apply string-append rows))))

;;; build-header : (List Symbol) × Number × Number → String
(define (build-header types type-width proto-width)
  (string-append
    (pad-right "" proto-width)
    (apply string-append
      (map (lambda (t) (pad-right (truncate-symbol t (- type-width 1)) type-width)) types))
    "\n"))

;;; build-row : Symbol × (List Symbol) × Number × Number → String
(define (build-row proto types type-width proto-width)
  (string-append
    (pad-right (truncate-symbol proto (- proto-width 1)) proto-width)
    (apply string-append
      (map (lambda (t)
             (pad-right (if (type-implements? t proto) "✓" "·") type-width))
           types))
    "\n"))

;;; pad-right : String × Number → String
(define (pad-right s width)
  (let ([len (string-length s)])
    (if (>= len width)
        (substring s 0 width)
        (string-append s (make-string (- width len) #\space)))))

;;; symbol-length : Symbol → Number
(define (symbol-length s)
  (string-length (symbol->string s)))

;;; truncate-symbol : Symbol × Number → String
(define (truncate-symbol s max-len)
  (truncate-string (symbol->string s) max-len))

;;; ============================================================
;;; Protocol Graph (Text-based)
;;; ============================================================

;;; protocol-graph : → String
;;; Show a text-based graph of protocol → type relationships.
(define (protocol-graph)
  (doc 'type '(-> String))
  (doc 'description "Text-based graph showing protocol → type relationships")
  (let ([protos (list-protocols)])
    (if (null? protos)
        "(no protocols registered)\n"
        (apply string-append
          (map
            (lambda (p)
              (let ([impls (protocol-implementations p)])
                (string-append
                  (symbol->string p) "\n"
                  (if (null? impls)
                      "  └── (none)\n"
                      (format-impl-tree impls))
                  "\n")))
            protos)))))

;;; format-impl-tree : (List Symbol) → String
(define (format-impl-tree impls)
  (let loop ([remaining impls] [result ""])
    (cond
      [(null? remaining) result]
      [(null? (cdr remaining))
       (string-append result "  └── " (symbol->string (car remaining)) "\n")]
      [else
       (loop (cdr remaining)
             (string-append result "  ├── " (symbol->string (car remaining)) "\n"))])))

;;; ============================================================
;;; Statistics
;;; ============================================================

(doc 'section 'statistics)

;;; protocol-stats : → AssocList
;;; Return statistics about the protocol system.
(define (protocol-stats)
  (doc 'type '(-> (List (Symbol . Number))))
  (doc 'description "Statistics about registered protocols and types")
  (let* ([protos (list-protocols)]
         [types (all-type-tags)]
         [impl-counts (map (lambda (p) (length (protocol-implementations p))) protos)]
         [total-impls (if (null? impl-counts) 0 (apply + impl-counts))]
         [proto-per-type (map (lambda (t) (length (protocols-for-type t))) types)])
    `((protocols . ,(length protos))
      (types . ,(length types))
      (total-implementations . ,total-impls)
      (avg-impls-per-protocol . ,(if (null? protos) 0 (/ total-impls (length protos))))
      (avg-protos-per-type . ,(if (null? types) 0 (/ total-impls (length types)))))))

;;; print-protocol-stats : → Void
(define (print-protocol-stats)
  (let ([stats (protocol-stats)])
    (display "Protocol System Statistics\n")
    (display (make-string 40 #\─))
    (newline)
    (for-each
      (lambda (pair)
        (display (format "  ~a: ~a\n" (car pair) (cdr pair))))
      stats)))

;;; ============================================================
;;; Enhanced define-protocol (optional upgrade)
;;; ============================================================

;;; define-protocol+ : Like define-protocol but also registers documentation.
;;; Usage: (define-protocol+ (name obj args...) docstring)
;;;        (define-protocol+ (name obj args...) docstring signature)
;;;        (define-protocol+ (name obj args...) docstring signature module)
(define-syntax define-protocol+
  (syntax-rules ()
    ;; With just docstring
    [(_ (name obj args ...) docstring)
     (begin
       (define-protocol (name obj args ...) docstring)
       (register-protocol-doc! 'name docstring #f #f))]
    ;; With docstring and signature
    [(_ (name obj args ...) docstring signature)
     (begin
       (define-protocol (name obj args ...) docstring)
       (register-protocol-doc! 'name docstring 'signature #f))]
    ;; With docstring, signature, and module
    [(_ (name obj args ...) docstring signature module)
     (begin
       (define-protocol (name obj args ...) docstring)
       (register-protocol-doc! 'name docstring 'signature 'module))]))

;;; ============================================================
;;; REPL Helpers
;;; ============================================================

(doc 'section 'repl)

;;; pi : Symbol → Void (Protocol Info)
;;; Quick REPL command to describe a protocol.
(define (pi proto-name)
  (display (protocol-describe proto-name)))

;;; ti : Symbol → Void (Type Info)
;;; Quick REPL command to describe a type.
(define (ti type-tag)
  (display (type-describe type-tag)))

;;; pm : → Void (Protocol Matrix)
;;; Quick REPL command to show protocol matrix.
(define (pm)
  (display (protocol-matrix)))

;;; ============================================================
;;; Load Message
;;; ============================================================

(display "protocol-introspect.ss loaded.\n")
(display "  Describe:   (pi 'protocol), (ti 'type-tag)\n")
(display "  Matrix:     (pm), (protocol-matrix [types] [protos])\n")
(display "  Graph:      (protocol-graph)\n")
(display "  Stats:      (print-protocol-stats)\n")
(display "  Reverse:    (protocols-for-type 'type-tag)\n")
(display "  Define+:    (define-protocol+ (name obj) \"doc\" sig mod)\n")
