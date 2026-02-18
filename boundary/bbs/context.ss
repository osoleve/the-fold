(load "core/base/prelude.ss")

(doc 'module 'boundary/bbs/context)
(doc 'description "BBS Board Context - Dynamic parameter for current board scope")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'dependencies '(core/base/prelude))
(doc 'note "All board-scoped operations implicitly use (current-board).
The with-board macro provides clean dynamic scoping via parameterize.")

(doc 'section 'current-board-parameter)

(doc *current-board* 'type '(Parameter (Option Board)))
(doc *current-board* 'description "Dynamic parameter holding the current board record. #f when no board context is active")
(define *current-board* (make-parameter #f))

(doc 'section 'context-accessors)

(define (current-board)
  (doc 'type '(-> Board))
  (doc 'description "Get the current board record. Errors if no board context is active")
  (doc 'export #t)
  (or (*current-board*)
      (error 'current-board "No board context active. Use with-board or set *current-board*")))

(define (current-board-slug)
  (doc 'type '(-> String))
  (doc 'description "Get the slug of the current board")
  (doc 'export #t)
  (board-slug (current-board)))

(define (current-board-type)
  (doc 'type '(-> Symbol))
  (doc 'description "Get the type of the current board ('tracker | 'forum | 'feed)")
  (doc 'export #t)
  (board-board-type (current-board)))

(define (current-board-id-prefix)
  (doc 'type '(-> String))
  (doc 'description "Get the ID prefix of the current board")
  (doc 'export #t)
  (board-id-prefix (current-board)))

(define (board-context?)
  (doc 'type '(-> Boolean))
  (doc 'description "Check if a board context is currently active")
  (doc 'export #t)
  (and (*current-board*) #t))

(doc 'section 'with-board-syntax)

(define-syntax with-board
  (syntax-rules ()
    [(_ board-record body ...)
     (parameterize ([*current-board* board-record])
       body ...)]))
