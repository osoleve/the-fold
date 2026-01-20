(load "core/base/prelude.ss")
(load "lattice/fp/meta/combinators.ss")

(doc 'module 'reader)
(doc 'description "Reader Extensions and Custom Notation - Domain-specific syntax via reader macros")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'note "Core Capability:
  #vec[1 2 3]           → (vector 1 2 3)
  #mat[[1 2][3 4]]      → (matrix '((1 2) (3 4)))
  #sql{SELECT * FROM t} → (sql-query ...)
  #re/pattern/flags     → (regex \"pattern\" 'flags)")

(doc 'note "Reader Dispatch:
  (define-reader-macro #\\v
    (lambda (port) (read-vector-literal port)))")

(doc 'note "Actual IO happens in shell; this provides the framework")

(doc 'section 'readtable-data-structure)
(doc 'note "A readtable maps dispatch characters to reader macros.
Readtables can be stacked for scoped extensions")

(define (make-readtable)
  (doc 'type (-> Readtable))
  (doc 'description "Create an empty readtable")
  (list 'readtable '()))

;;; readtable? : α → Boolean
(define (readtable? x)
  (and (pair? x) (eq? (car x) 'readtable)))

;;; readtable-entries : Readtable → (List (Char . Macro))
(define (readtable-entries rt)
  (if (readtable? rt) (cadr rt) '()))

;;; readtable-lookup : Readtable × Char → (Option Macro)
(define (readtable-lookup rt char)
  (let ([entry (assv char (readtable-entries rt))])
       (if entry (just (cdr entry)) nothing)))

;;; readtable-extend : Readtable × Char × Macro → Readtable
;;; Add or replace a reader macro in the readtable.
(define (readtable-extend rt char macro)
  (let* ([entries (readtable-entries rt)]
         [filtered (filter (lambda (e) (not (char=? (car e) char))) entries)]
         [new-entries (cons (cons char macro) filtered)])
        (list 'readtable new-entries)))

;;; readtable-remove : Readtable × Char → Readtable
;;; Remove a reader macro from the readtable.
(define (readtable-remove rt char)
  (let* ([entries (readtable-entries rt)]
         [filtered (filter (lambda (e) (not (char=? (car e) char))) entries)])
        (list 'readtable filtered)))

;;; ====
;;; Readtable Stack
;;; ====
;;;
;;; Support scoped readtable extensions via a stack.
;;; (with-readtable ...) pushes; exiting pops.

;;; make-readtable-stack : → Stack
(define (make-readtable-stack)
  (list (make-readtable)))

;;; readtable-stack? : α → Boolean
(define (readtable-stack? x)
  (and (pair? x) (andmap readtable? x)))

;;; readtable-stack-push : Stack × Readtable → Stack
(define (readtable-stack-push stack rt)
  (cons rt stack))

;;; readtable-stack-pop : Stack → (Option Stack)
(define (readtable-stack-pop stack)
  (if (null? (cdr stack))
      nothing  ; Can't pop the base readtable
      (just (cdr stack))))

;;; readtable-stack-current : Stack → Readtable
(define (readtable-stack-current stack)
  (car stack))

;;; readtable-stack-lookup : Stack × Char → (Option Macro)
;;; Look up a char in the stack, searching from top to bottom.
(define (readtable-stack-lookup stack char)
  (let loop ([remaining stack])
       (if (null? remaining)
           nothing
           (let ([result (readtable-lookup (car remaining) char)])
                (if (just? result)
                    result
                    (loop (cdr remaining)))))))

;;; ====
;;; Reader Macro Representation
;;; ====
;;;
;;; A reader macro is a structure containing:
;;;   - transform: the transformation function
;;;   - delimiter: optional custom delimiter char
;;;   - name: for debugging

;;; make-reader-macro : Symbol × Procedure × (Option Char) → ReaderMacro
(define (make-reader-macro name transform delimiter)
  (list 'reader-macro name transform delimiter))

;;; reader-macro? : α → Boolean
(define (reader-macro? x)
  (and (pair? x) (eq? (car x) 'reader-macro)))

;;; reader-macro-name : ReaderMacro → Symbol
(define (reader-macro-name rm)
  (cadr rm))

;;; reader-macro-transform : ReaderMacro → Procedure
(define (reader-macro-transform rm)
  (caddr rm))

;;; reader-macro-delimiter : ReaderMacro → (Option Char)
(define (reader-macro-delimiter rm)
  (cadddr rm))

;;; ====
;;; Reader Dispatch Characters
;;; ====
;;;
;;; Standard dispatch characters:
;;;   #v[...]  - vector literal
;;;   #m[...]  - matrix literal
;;;   #r/...   - regex literal
;;;   #{...}   - custom block
;;;   #[...]   - bracket notation
;;;   #<...>   - angle notation

;;; dispatch-char? : Char → Boolean
;;; Check if a character can be used as a dispatch character.
(define (dispatch-char? c)
  (and (char? c)
       (not (char-whitespace? c))
       (not (memv c '(#\( #\) #\' #\" #\; #\`)))))

;;; ====
;;; Token Representation
;;; ====
;;;
;;; Tokens are the output of reader macros, ready for parsing.

;;; make-reader-token : Symbol × α × SourceLoc → Token
(define (make-reader-token type value source-loc)
  (list 'reader-token type value source-loc))

;;; reader-token? : α → Boolean
(define (reader-token? x)
  (and (pair? x) (eq? (car x) 'reader-token)))

;;; reader-token-type : Token → Symbol
(define (reader-token-type tok)
  (cadr tok))

;;; reader-token-value : Token → α
(define (reader-token-value tok)
  (caddr tok))

;;; reader-token-source : Token → SourceLoc
(define (reader-token-source tok)
  (cadddr tok))

;;; ====
;;; Source Location (from quasi.ss, re-exported)
;;; ====

;;; make-source-loc : String × Int × Int → SourceLoc
(define (make-reader-source-loc file line column)
  (list 'source-loc file line column))

;;; source-loc-file : SourceLoc → String
(define (reader-source-loc-file loc)
  (cadr loc))

;;; source-loc-line : SourceLoc → Int
(define (reader-source-loc-line loc)
  (caddr loc))

;;; source-loc-column : SourceLoc → Int
(define (reader-source-loc-column loc)
  (cadddr loc))

;;; no-reader-source : SourceLoc
(define no-reader-source
  (make-reader-source-loc "<reader>" 0 0))

;;; ====
;;; Delimiter Pairs
;;; ====
;;;
;;; Standard delimiter pairs for reader macros.

;;; delimiter-pairs : (List (Char . Char))
(define delimiter-pairs
  '((#\[ . #\])
    (#\{ . #\})
    (#\< . #\>)
    (#\( . #\))))

;;; delimiter-close : Char → (Option Char)
(define (delimiter-close open)
  (let ([pair (assv open delimiter-pairs)])
       (if pair (just (cdr pair)) nothing)))

;;; delimiter-open : Char → (Option Char)
(define (delimiter-open close)
  (let ([pair (find (lambda (p) (char=? (cdr p) close)) delimiter-pairs)])
       (if pair (just (car pair)) nothing)))

;;; ====
;;; Reader State
;;; ====
;;;
;;; The reader state tracks position and readtable stack.

;;; make-reader-state : String × Int × Int × Stack → ReaderState
(define (make-reader-state filename line column stack)
  (list 'reader-state filename line column stack))

;;; reader-state? : α → Boolean
(define (reader-state? x)
  (and (pair? x) (eq? (car x) 'reader-state)))

;;; reader-state-filename : ReaderState → String
(define (reader-state-filename rs)
  (cadr rs))

;;; reader-state-line : ReaderState → Int
(define (reader-state-line rs)
  (caddr rs))

;;; reader-state-column : ReaderState → Int
(define (reader-state-column rs)
  (cadddr rs))

;;; reader-state-stack : ReaderState → Stack
(define (reader-state-stack rs)
  (car (cddddr rs)))

;;; reader-state-source-loc : ReaderState → SourceLoc
(define (reader-state-source-loc rs)
  (make-reader-source-loc (reader-state-filename rs)
                          (reader-state-line rs)
                          (reader-state-column rs)))

;;; reader-state-update-position : ReaderState × Int × Int → ReaderState
(define (reader-state-update-position rs line column)
  (make-reader-state (reader-state-filename rs)
                     line column
                     (reader-state-stack rs)))

;;; reader-state-push-readtable : ReaderState × Readtable → ReaderState
(define (reader-state-push-readtable rs rt)
  (make-reader-state (reader-state-filename rs)
                     (reader-state-line rs)
                     (reader-state-column rs)
                     (readtable-stack-push (reader-state-stack rs) rt)))

;;; reader-state-pop-readtable : ReaderState → ReaderState
(define (reader-state-pop-readtable rs)
  (let ([result (readtable-stack-pop (reader-state-stack rs))])
       (if (just? result)
           (make-reader-state (reader-state-filename rs)
                              (reader-state-line rs)
                              (reader-state-column rs)
                              (from-just result))
           rs)))

;;; ====
;;; Standard Reader Macros
;;; ====

;;; vector-reader-macro : ReaderMacro
;;; #v[1 2 3] → (vector 1 2 3)
(define vector-reader-macro
  (make-reader-macro 'vector
                     (lambda (contents source-loc)
                             `(vector ,@contents))
                     #\[))

;;; matrix-reader-macro : ReaderMacro
;;; #m[[1 2][3 4]] → (matrix '((1 2) (3 4)))
(define matrix-reader-macro
  (make-reader-macro 'matrix
                     (lambda (contents source-loc)
                             `(matrix (quote ,contents)))
                     #\[))

;;; regex-reader-macro : ReaderMacro
;;; #r/pattern/flags → (regex "pattern" 'flags)
(define regex-reader-macro
  (make-reader-macro 'regex
                     (lambda (contents source-loc)
                             (if (and (pair? contents)
                                      (string? (car contents)))
                                 (let ([pattern (car contents)]
                                       [flags (if (null? (cdr contents)) '() (cadr contents))])
                                      `(regex ,pattern (quote ,flags)))
                                 `(regex "" '())))
                     #\/))

;;; set-reader-macro : ReaderMacro
;;; #s{1 2 3} → (set 1 2 3)
(define set-reader-macro
  (make-reader-macro 'set
                     (lambda (contents source-loc)
                             `(set ,@contents))
                     #\{))

;;; hash-reader-macro : ReaderMacro
;;; #h{a 1 b 2} → (hash 'a 1 'b 2)
;;; Odd-length input is an error (keys must have values).
(define hash-reader-macro
  (make-reader-macro 'hash
                     (lambda (contents source-loc)
                             ;; Validate even-length (key-value pairs)
                             (when (odd? (length contents))
                                   (error 'hash-reader-macro
                                          "hash literal requires key-value pairs (even count)"
                                          contents))
                             ;; Convert pairs: a 1 b 2 → 'a 1 'b 2
                             (let convert ([items contents] [result '()])
                                  (cond
                                   [(null? items) `(hash ,@(reverse result))]
                                   [else
                                    (let ([key (car items)]
                                          [val (cadr items)])
                                         (convert (cddr items)
                                                  (cons val (cons (if (symbol? key) `(quote ,key) key)
                                                                  result))))])))
                     #\{))

;;; ====
;;; Default Readtable
;;; ====

;;; default-readtable : Readtable
;;; The standard readtable with common extensions.
(define default-readtable
  (let ([rt (make-readtable)])
       (fold-left
        (lambda (rt pair)
                (readtable-extend rt (car pair) (cdr pair)))
        rt
        `((#\v . ,vector-reader-macro)
          (#\m . ,matrix-reader-macro)
          (#\r . ,regex-reader-macro)
          (#\s . ,set-reader-macro)
          (#\h . ,hash-reader-macro)))))

;;; ====
;;; Reader Macro Expansion
;;; ====
;;;
;;; These functions transform reader macro syntax to S-expressions.
;;; Actual IO is handled by the boundary layer.

;;; expand-reader-macro : Char × α × SourceLoc × Readtable → (Option Sexp)
;;; Look up and apply a reader macro.
(define (expand-reader-macro dispatch-char contents source-loc readtable)
  (let ([result (readtable-lookup readtable dispatch-char)])
       (if (just? result)
           (let* ([macro (from-just result)]
                  [transform (reader-macro-transform macro)])
                 (just (transform contents source-loc)))
           nothing)))

;;; expand-reader-macro-stack : Char × α × SourceLoc × Stack → (Option Sexp)
;;; Look up and apply a reader macro using the stack.
(define (expand-reader-macro-stack dispatch-char contents source-loc stack)
  (let ([result (readtable-stack-lookup stack dispatch-char)])
       (if (just? result)
           (let* ([macro (from-just result)]
                  [transform (reader-macro-transform macro)])
                 (just (transform contents source-loc)))
           nothing)))

;;; ====
;;; Reader Macro Registration
;;; ====
;;;
;;; define-reader-macro returns an updated readtable.
;;; Actual macro is used in shell for syntactic sugar.

;;; register-reader-macro : Readtable × Char × Symbol × Procedure → Readtable
(define (register-reader-macro readtable char name transform)
  (readtable-extend readtable char
                    (make-reader-macro name transform nothing)))

;;; register-reader-macro-delimited : Readtable × Char × Symbol × Procedure × Char → Readtable
(define (register-reader-macro-delimited readtable char name transform delimiter)
  (readtable-extend readtable char
                    (make-reader-macro name transform (just delimiter))))

;;; ====
;;; Reader Macro Syntax DSL
;;; ====
;;;
;;; A DSL for defining reader macros declaratively.
;;;
;;; (reader-macro-spec name
;;;   (dispatch char)
;;;   (delimiter open close)
;;;   (transform (contents loc) body))

;;; reader-macro-spec? : α → Boolean
(define (reader-macro-spec? x)
  (and (pair? x) (eq? (car x) 'reader-macro-spec)))

;;; parse-reader-macro-spec : Sexp → (Option ReaderMacroSpec)
(define (parse-reader-macro-spec spec)
  (if (and (reader-macro-spec? spec)
           (>= (length spec) 2)
           (symbol? (cadr spec)))
      (let ([name (cadr spec)]
            [clauses (cddr spec)])
           (let loop ([remaining clauses]
                      [dispatch nothing]
                      [delimiter nothing]
                      [transform nothing])
                (if (null? remaining)
                    (if (just? dispatch)
                        (just (list 'parsed-spec name
                                    (from-just dispatch)
                                    delimiter
                                    (if (just? transform) (from-just transform) #f)))
                        nothing)
                    (let ([clause (car remaining)])
                         (cond
                          [(and (pair? clause) (eq? (car clause) 'dispatch))
                           (loop (cdr remaining)
                                 (just (cadr clause))
                                 delimiter transform)]
                          [(and (pair? clause) (eq? (car clause) 'delimiter))
                           (loop (cdr remaining)
                                 dispatch
                                 (just (cons (cadr clause) (caddr clause)))
                                 transform)]
                          [(and (pair? clause) (eq? (car clause) 'transform))
                           (loop (cdr remaining)
                                 dispatch delimiter
                                 (just (cdr clause)))]
                          [else (loop (cdr remaining) dispatch delimiter transform)])))))
      nothing))

;;; ====
;;; Escaped Content Parsing
;;; ====
;;;
;;; Parse escaped sequences within reader macro content.

;;; unescape-string : String → String
;;; Process escape sequences in a string.
(define (unescape-string s)
  (let loop ([chars (string->list s)] [result '()])
       (cond
        [(null? chars) (list->string (reverse result))]
        [(and (char=? (car chars) #\\)
              (not (null? (cdr chars))))
         (let ([next (cadr chars)])
              (loop (cddr chars)
                    (cons (case next
                                [(#\n) #\newline]
                                [(#\t) #\tab]
                                [(#\r) #\return]
                                [(#\\) #\\]
                                [(#\/) #\/]
                                [(#\") #\"]   ; Escaped double-quote
                                [(#\') #\']   ; Escaped single-quote
                                [else next])
                          result)))]
        [else (loop (cdr chars) (cons (car chars) result))])))

;;; ====
;;; Reader Macro Composition
;;; ====
;;;
;;; Compose multiple readtables.

;;; compose-readtables : (List Readtable) → Readtable
;;; Merge readtables, later entries override earlier.
(define (compose-readtables readtables)
  (fold-left
   (lambda (acc rt)
           (fold-left
            (lambda (acc entry)
                    (readtable-extend acc (car entry) (cdr entry)))
            acc
            (readtable-entries rt)))
   (make-readtable)
   readtables))

;;; ====
;;; Reader Errors
;;; ====

;;; make-reader-error : String × SourceLoc → ReaderError
(define (make-reader-error message source-loc)
  (list 'reader-error message source-loc))

;;; reader-error? : α → Boolean
(define (reader-error? x)
  (and (pair? x) (eq? (car x) 'reader-error)))

;;; reader-error-message : ReaderError → String
(define (reader-error-message err)
  (cadr err))

;;; reader-error-source : ReaderError → SourceLoc
(define (reader-error-source err)
  (caddr err))

;;; ====
;;; Unit Annotation Reader
;;; ====
;;;
;;; Support for unit annotations: 5#m/s

;;; unit-reader-macro : ReaderMacro
;;; Parse unit annotations.
(define unit-reader-macro
  (make-reader-macro 'unit
                     (lambda (contents source-loc)
                             ;; contents is (number unit-string)
                             (if (and (pair? contents)
                                      (number? (car contents))
                                      (pair? (cdr contents))
                                      (string? (cadr contents)))
                                 `(with-unit ,(car contents) (quote ,(string->symbol (cadr contents))))
                                 contents))
                     nothing))

;;; ====
;;; Exports
;;; ====

;;; All public functions are defined at top level.
;;; Main entry points:
;;;   - make-readtable, readtable-extend, readtable-lookup
;;;   - make-readtable-stack, readtable-stack-push, readtable-stack-pop
;;;   - make-reader-macro, reader-macro-transform
;;;   - expand-reader-macro, expand-reader-macro-stack
;;;   - register-reader-macro, register-reader-macro-delimited
;;;   - default-readtable
