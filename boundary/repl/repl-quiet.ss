(doc 'module 'repl-quiet)
(doc 'description "Quiet REPL loader - loads the REPL without the startup banner")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'usage "(load \"boundary/repl/repl-quiet.ss\")")

(define *quiet* #t)
(load "boundary/repl/repl.ss")
