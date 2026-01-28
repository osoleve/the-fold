(load "lattice/fp/parsing/parser.ss")
(load "lattice/dsl/chronicle/chronicle.ss")

(doc 'module 'chronicle-parser)
(doc 'description "Parser Combinator Parser for Chronicle - Demonstrates dogfooding by using the parser combinator library to parse a textual Chronicle DSL syntax")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'note "Textual Syntax Example:
  chronicle \"The Quest\" {
    start: intro

    scene intro {
      \"You stand before the ancient temple.\"

      -> \"Enter the temple\" => main_hall
      -> \"Examine surroundings\" => intro [do: flag!(examined)]
    }

    scene main_hall {
      \"The hall echoes with your footsteps.\"

      -> \"Search for treasure\" => treasure_room [when: flag?(examined)]
      -> \"Leave\" => ending
    }
  }")

(doc 'note "This parser produces S-expression AST that can be compiled using parse-chronicle-def from chronicle.ss")

(doc 'section 'lexical-primitives)

;;; Skip whitespace and comments
(define whitespace
  (many (parser-or space
                   (parser-then (string-parser "//")
                                (many-till any-char newline-char)))))

;;; Skip required whitespace
(define whitespace1
  (some (parser-or space
                   (parser-then (string-parser "//")
                                (many-till any-char newline-char)))))

;;; Lexeme: parser followed by whitespace
(define (lexeme p)
  (parser-left p whitespace))

;;; Symbol: parse a keyword/identifier followed by whitespace
(define (symbol str)
  (lexeme (string-parser str)))

(doc 'section 'identifiers-and-strings)

;;; identifier-start : Parser Char
(define identifier-start
  (satisfy (lambda (c) (or (char-alphabetic? c) (char=? c #\_)))
           "identifier start"))

;;; identifier-char : Parser Char
(define identifier-char
  (satisfy (lambda (c) (or (char-alphabetic? c)
                           (char-numeric? c)
                           (char=? c #\_)
                           (char=? c #\-)))
           "identifier character"))

;;; identifier : Parser Symbol
(define identifier
  (lexeme
   (parser-bind identifier-start
                (lambda (first)
                        (parser-bind (many identifier-char)
                                     (lambda (rest)
                                             (parser-pure (string->symbol (list->string (cons first rest))))))))))

;;; quoted-string : Parser String
;;; Parse a double-quoted string with escape sequences
(define quoted-string
  (lexeme
   (between (char #\")
            (char #\")
            (parser-map list->string
                        (many (parser-or
                               ;; Escape sequences
                               (parser-then (char #\\)
                                            (choice
                                             (list
                                              (parser-map (lambda (_) #\newline) (char #\n))
                                              (parser-map (lambda (_) #\tab) (char #\t))
                                              (parser-map (lambda (_) #\") (char #\"))
                                              (parser-map (lambda (_) #\\) (char #\\)))))
                               ;; Regular characters
                               (satisfy (lambda (c) (and (not (char=? c #\"))
                                                         (not (char=? c #\\))))
                                        "string character")))))))

(doc 'section 'guard-expressions)

;;; Parse guard expressions like:
;;;   flag?(name)
;;;   not-flag?(name)
;;;   has-item?(name)
;;;   var>=(name, value)
;;;   and(g1, g2)
;;;   or(g1, g2)
;;;   not(g)

;;; For simplicity, we parse guards without complex nesting for now.
;;; Supports: flag?, not-flag?, has-item?, var>=, var<=, var=, true, false
;;; Compound: and/or are parsed as prefix operators

(define parse-guard
  (make-parser
   (lambda (state)
           (run-parser (parse-guard-impl) state))))

;;; Actual guard parser (delayed to allow forward reference)
(define (parse-guard-impl)
  (choice
   (list
    ;; and(g1, g2, ...)
    (parser-bind (symbol "and")
                 (lambda (_)
                         (parser-bind (between (symbol "(") (symbol ")")
                                               (sep-by (parse-guard-impl) (symbol ",")))
                                      (lambda (guards)
                                              (parser-pure (cons 'and guards))))))
    
    ;; or(g1, g2, ...)
    (parser-bind (symbol "or")
                 (lambda (_)
                         (parser-bind (between (symbol "(") (symbol ")")
                                               (sep-by (parse-guard-impl) (symbol ",")))
                                      (lambda (guards)
                                              (parser-pure (cons 'or guards))))))
    
    ;; not(g)
    (parser-bind (symbol "not")
                 (lambda (_)
                         (parser-bind (between (symbol "(") (symbol ")")
                                               (parse-guard-impl))
                                      (lambda (g)
                                              (parser-pure (list 'not g))))))
    
    ;; flag?(name)
    (parser-bind (symbol "flag?")
                 (lambda (_)
                         (parser-bind (between (symbol "(") (symbol ")") identifier)
                                      (lambda (name)
                                              (parser-pure (list 'flag? name))))))
    
    ;; not-flag?(name)
    (parser-bind (symbol "not-flag?")
                 (lambda (_)
                         (parser-bind (between (symbol "(") (symbol ")") identifier)
                                      (lambda (name)
                                              (parser-pure (list 'not-flag? name))))))
    
    ;; has-item?(name)
    (parser-bind (symbol "has-item?")
                 (lambda (_)
                         (parser-bind (between (symbol "(") (symbol ")") identifier)
                                      (lambda (name)
                                              (parser-pure (list 'has-item? name))))))
    
    ;; var>=(name, value)
    (parser-bind (symbol "var>=")
                 (lambda (_)
                         (parser-then (symbol "(")
                                      (parser-bind identifier
                                                   (lambda (name)
                                                           (parser-then (symbol ",")
                                                                        (parser-bind parse-number
                                                                                     (lambda (val)
                                                                                             (parser-then (symbol ")")
                                                                                                          (parser-pure (list 'var>= name val)))))))))))
    
    ;; var<=(name, value)
    (parser-bind (symbol "var<=")
                 (lambda (_)
                         (parser-then (symbol "(")
                                      (parser-bind identifier
                                                   (lambda (name)
                                                           (parser-then (symbol ",")
                                                                        (parser-bind parse-number
                                                                                     (lambda (val)
                                                                                             (parser-then (symbol ")")
                                                                                                          (parser-pure (list 'var<= name val)))))))))))
    
    ;; var=(name, value)
    (parser-bind (symbol "var=")
                 (lambda (_)
                         (parser-then (symbol "(")
                                      (parser-bind identifier
                                                   (lambda (name)
                                                           (parser-then (symbol ",")
                                                                        (parser-bind parse-value
                                                                                     (lambda (val)
                                                                                             (parser-then (symbol ")")
                                                                                                          (parser-pure (list 'var= name val)))))))))))
    
    ;; true
    (parser-then (symbol "true") (parser-pure #t))
    
    ;; false
    (parser-then (symbol "false") (parser-pure #f)))))

(doc 'section 'effect-expressions)

;;; Parse effects like:
;;;   set!(name, value)
;;;   inc!(name, delta)
;;;   flag!(name)
;;;   unflag!(name)
;;;   add-item!(name)
;;;   remove-item!(name)

(define parse-effect
  (choice
   (list
    ;; set!(name, value)
    (parser-bind (symbol "set!")
                 (lambda (_)
                         (parser-then (symbol "(")
                                      (parser-bind identifier
                                                   (lambda (name)
                                                           (parser-then (symbol ",")
                                                                        (parser-bind parse-value
                                                                                     (lambda (val)
                                                                                             (parser-then (symbol ")")
                                                                                                          (parser-pure (list 'set! name val)))))))))))
    
    ;; inc!(name, delta)
    (parser-bind (symbol "inc!")
                 (lambda (_)
                         (parser-then (symbol "(")
                                      (parser-bind identifier
                                                   (lambda (name)
                                                           (parser-then (symbol ",")
                                                                        (parser-bind parse-number
                                                                                     (lambda (delta)
                                                                                             (parser-then (symbol ")")
                                                                                                          (parser-pure (list 'inc! name delta)))))))))))
    
    ;; flag!(name)
    (parser-bind (symbol "flag!")
                 (lambda (_)
                         (parser-bind (between (symbol "(") (symbol ")") identifier)
                                      (lambda (name)
                                              (parser-pure (list 'flag! name))))))
    
    ;; unflag!(name)
    (parser-bind (symbol "unflag!")
                 (lambda (_)
                         (parser-bind (between (symbol "(") (symbol ")") identifier)
                                      (lambda (name)
                                              (parser-pure (list 'unflag! name))))))
    
    ;; add-item!(name)
    (parser-bind (symbol "add-item!")
                 (lambda (_)
                         (parser-bind (between (symbol "(") (symbol ")") identifier)
                                      (lambda (name)
                                              (parser-pure (list 'add-item! name))))))
    
    ;; remove-item!(name)
    (parser-bind (symbol "remove-item!")
                 (lambda (_)
                         (parser-bind (between (symbol "(") (symbol ")") identifier)
                                      (lambda (name)
                                              (parser-pure (list 'remove-item! name)))))))))

(doc 'section 'values)

;;; parse-number : Parser Number
(define parse-number
  (lexeme
   (parser-bind (optional (char #\-) #f)
                (lambda (neg?)
                        (parser-bind (some digit)
                                     (lambda (digits)
                                             (let ([n (string->number (list->string digits))])
                                                  (parser-pure (if neg? (- n) n)))))))))

;;; parse-value : Parser Any
(define parse-value
  (choice
   (list
    parse-number
    quoted-string
    (parser-then (symbol "true") (parser-pure #t))
    (parser-then (symbol "false") (parser-pure #f))
    identifier)))

(doc 'section 'choice-definition)

;;; Parse choice like:
;;;   -> "label" => target
;;;   -> "label" => target [when: guard]
;;;   -> "label" => target [do: effect]
;;;   -> "label" => target [when: guard, do: effect]

(define parse-choice-modifiers
  (optional
   (between (symbol "[") (symbol "]")
            (sep-by
             (parser-or
              ;; when: guard
              (parser-bind (symbol "when:")
                           (lambda (_)
                                   (parser-bind parse-guard
                                                (lambda (g)
                                                        (parser-pure (cons 'when g))))))
              ;; do: effect
              (parser-bind (symbol "do:")
                           (lambda (_)
                                   (parser-bind parse-effect
                                                (lambda (e)
                                                        (parser-pure (cons 'do e)))))))
             (symbol ",")))
   '()))

(define parse-choice
  (parser-bind (symbol "->")
               (lambda (_)
                       (parser-bind quoted-string
                                    (lambda (label)
                                            (parser-bind (symbol "=>")
                                                         (lambda (_)
                                                                 (parser-bind identifier
                                                                              (lambda (target)
                                                                                      (parser-bind parse-choice-modifiers
                                                                                                   (lambda (mods)
                                                                                                           (let ([guard-mod (assq 'when mods)]
                                                                                                                 [do-mod (assq 'do mods)])
                                                                                                                ;; Build flat list: (-> label target :when guard :do effect)
                                                                                                                (parser-pure
                                                                                                                 (append
                                                                                                                  (list '-> label target)
                                                                                                                  (if guard-mod (list ':when (cdr guard-mod)) '())
                                                                                                                  (if do-mod (list ':do (cdr do-mod)) '())))))))))))))))

(doc 'section 'scene-definition)

;;; Parse scene like:
;;;   scene name {
;;;     "text"
;;;     on-enter { effects... }
;;;     -> choices...
;;;   }

(define parse-on-enter
  (parser-bind (symbol "on-enter")
               (lambda (_)
                       (parser-bind (between (symbol "{") (symbol "}")
                                             (sep-by parse-effect (optional (symbol ";") '())))
                                    (lambda (effects)
                                            (parser-pure (cons 'on-enter effects)))))))

(define parse-scene-body
  (parser-bind quoted-string
               (lambda (text)
                       (parser-bind (many parse-on-enter)
                                    (lambda (on-enters)
                                            (parser-bind (many parse-choice)
                                                         (lambda (choices)
                                                                 ;; Flatten on-enter effects
                                                                 (let ([all-effects (append-map cdr on-enters)])
                                                                      (parser-pure (list text all-effects choices))))))))))

(define parse-scene
  (parser-bind (symbol "scene")
               (lambda (_)
                       (parser-bind identifier
                                    (lambda (name)
                                            (parser-bind (between (symbol "{") (symbol "}") parse-scene-body)
                                                         (lambda (body)
                                                                 (let ([text (car body)]
                                                                       [on-enter (cadr body)]
                                                                       [choices (caddr body)])
                                                                      (parser-pure
                                                                       `(scene ,name ,text
                                                                         ,@(if (null? on-enter) '() (list `(on-enter ,@on-enter)))
                                                                         ,@choices))))))))))

(doc 'section 'chronicle-definition)

;;; Parse chronicle like:
;;;   chronicle "title" {
;;;     start: scene_name
;;;     scene ... { }
;;;     scene ... { }
;;;   }

(define parse-start-decl
  (parser-bind (symbol "start:")
               (lambda (_)
                       (parser-bind identifier
                                    (lambda (name)
                                            (parser-pure (list 'start name)))))))

(define parse-chronicle-body
  (parser-bind (optional parse-start-decl #f)
               (lambda (start)
                       (parser-bind (many parse-scene)
                                    (lambda (scenes)
                                            (parser-pure (cons start scenes)))))))

(define parse-chronicle
  (parser-bind (symbol "chronicle")
               (lambda (_)
                       (parser-bind quoted-string
                                    (lambda (title)
                                            (parser-bind (between (symbol "{") (symbol "}") parse-chronicle-body)
                                                         (lambda (body)
                                                                 (let ([start (car body)]
                                                                       [scenes (cdr body)])
                                                                      (parser-pure
                                                                       `(chronicle ,(string->symbol (string-replace title " " "-"))
                                                                         ,title
                                                                         ,@(if start (list start) '())
                                                                         ,@scenes))))))))))

(doc 'section 'main-entry-point)

;;; parse-chronicle-text : String -> Either Error Chronicle
;;; Parse textual chronicle DSL and compile to Chronicle structure.
(define (parse-chronicle-text input)
  (let ([result (parse-all (parser-then whitespace parse-chronicle) input)])
       (if (right? result)
           (let ([ast (from-right result)])
                ;; Compile AST using parse-chronicle-def from chronicle.ss
                (right (parse-chronicle-def ast)))
           result)))

;;; Helper: replace character in string
(define (string-replace str old new)
  (list->string
   (map (lambda (c) (if (char=? c (string-ref old 0)) (string-ref new 0) c))
        (string->list str))))

(doc 'section 'convenience-parse-and-run)

;;; chronicle-from-text : String -> Chronicle
;;; Parse text and return chronicle (or error).
(define (chronicle-from-text input)
  (let ([result (parse-chronicle-text input)])
       (if (right? result)
           (from-right result)
           (error 'chronicle-from-text
                  (format-error (from-left result))))))

(doc 'section 'exports-summary)
;;;
;;; Main Entry Points:
;;;   parse-chronicle-text : String -> Either Error Chronicle
;;;   chronicle-from-text : String -> Chronicle
;;;
;;; Parser Combinators (for extension):
;;;   parse-chronicle, parse-scene, parse-choice
;;;   parse-guard, parse-effect, parse-value
;;;   identifier, quoted-string, parse-number
