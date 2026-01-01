;;; playpen/quill/parse.ss — Quill command parsing
;;;
;;; This parser is intentionally small. It provides a stable intent model
;;; for the runtime while leaving room for richer parsing later.
;;;
;;; Depends on fabric/stitches/parse.ss (parser combinators).

(load "core/parse.ss")
(load "shell/string-utils.ss")

;;; Helpers
(define (quill-parse-complete parser input)
  (let* ([trimmed (string-trim input)]
         [result (run-parser (seq-left parser spaces) trimmed)])
        (if (and (parse-ok? result)
                 (string=? (result-remaining result) ""))
            (result-value result)
            #f)))

;;; ============================================================
;;; Token-ish helpers (using combinators)
;;; ============================================================

;;; word-boundary : Parser Unit
;;; Succeed only if next char is whitespace or we're at EOF.
;;; Does not consume input (lookahead).
(define word-boundary
  (lambda (input)
          (if (= (string-length input) 0)
              (success '() input)
              (if (char-whitespace? (string-ref input 0))
                  (success '() input)
                  (failure "word boundary" (string-length input))))))

;;; kw : String -> Parser String
;;; Match a keyword and require a boundary after it.
(define (kw s)
  (seq-left (string-p s) word-boundary))

;;; rest-of-line : Parser String
(define rest-of-line
  (lambda (input)
          (success (string-trim input) "")))

;;; quoted-string : Parser String
;;; Parse "..." (no escapes yet).
(define quoted-string
  (between (string-p "\"")
           (string-p "\"")
           (map-p list->string (many (satisfy (lambda (c) (not (char=? c #\"))) "string-char")))))

;;; text-arg : Parser String
;;; Parse either a quoted string or the remaining text.
(define text-arg
  (alt quoted-string rest-of-line))

;;; wordish : Parser String
;;; A word-like token for directions and short commands.
(define wordish
  (map-p list->string (many1 (alt alpha (alt digit (char #\-))))))

;;; lex-word : Parser String
(define lex-word (lexeme wordish))

;;; ============================================================
;;; Intent parsers
;;; ============================================================

(define p-choose
  (bind nat (lambda (n)
                    (bind word-boundary (lambda (_)
                                                (pure (quill-intent-choose n)))))))

(define p-look
  (map-p (lambda (_) (quill-intent-look))
         (choice (list (kw "look") (kw "l")))))

(define p-help
  (map-p (lambda (_) (quill-intent-help))
         (choice (list (kw "help") (kw "?") (kw "h")))))

(define p-quit
  (map-p (lambda (_) (quill-intent-quit))
         (choice (list (kw "quit") (kw "q") (kw "exit")))))

(define p-inventory
  (map-p (lambda (_) (quill-intent-inventory))
         (choice (list (kw "inventory") (kw "inv") (kw "i")))))

(define p-examine
  (bind (choice (list (kw "examine") (kw "x")))
        (lambda (_)
                (bind spaces1 (lambda (_)
                                      (bind text-arg (lambda (obj)
                                                             (pure (quill-intent-examine obj)))))))))

(define (p-direction-intent s dir)
  (map-p (lambda (_) (quill-intent-move dir)) (kw s)))

(define p-direction
  (choice
   (list
    (p-direction-intent "north" 'north)
    (p-direction-intent "n" 'north)
    (p-direction-intent "south" 'south)
    (p-direction-intent "s" 'south)
    (p-direction-intent "east" 'east)
    (p-direction-intent "e" 'east)
    (p-direction-intent "west" 'west)
    (p-direction-intent "w" 'west)
    (p-direction-intent "up" 'up)
    (p-direction-intent "u" 'up)
    (p-direction-intent "down" 'down)
    (p-direction-intent "d" 'down)
    (p-direction-intent "in" 'in)
    (p-direction-intent "out" 'out))))

(define p-go
  (bind (choice (list (kw "go") (kw "move")))
        (lambda (_)
                (bind spaces1 (lambda (_)
                                      (bind (lexeme p-direction) (lambda (intent)
                                                                         (pure intent))))))))

(define p-take
  (bind (choice (list (kw "take") (kw "get")))
        (lambda (_)
                (bind spaces1 (lambda (_)
                                      (bind text-arg (lambda (obj)
                                                             (pure (quill-intent-take obj)))))))))

(define p-drop
  (bind (kw "drop")
        (lambda (_)
                (bind spaces1 (lambda (_)
                                      (bind text-arg (lambda (obj)
                                                             (pure (quill-intent-drop obj)))))))))

(define p-use
  (bind (kw "use")
        (lambda (_)
                (bind spaces1 (lambda (_)
                                      (bind text-arg (lambda (obj)
                                                             (pure (quill-intent-use obj #f)))))))))

(define p-say
  (bind (choice (list (kw "say") (kw "talk")))
        (lambda (_)
                (bind spaces1 (lambda (_)
                                      (bind rest-of-line (lambda (txt)
                                                                 (pure (quill-intent-say txt)))))))))

(define quill-intent-parser
  (choice
   (list
    p-choose
    p-direction
    p-go
    p-look
    p-examine
    p-inventory
    p-take
    p-drop
    p-use
    p-say
    p-help
    p-quit)))

(define (quill-parse s)
  (cond
   [(or (not s) (not (string? s)) (string-blank? s))
    (quill-intent-unknown "")]
   [else
    (or (quill-parse-complete quill-intent-parser s)
        (quill-intent-unknown (string-trim s)))]))
