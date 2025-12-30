; ============================================================
; Core Monad Utilities
; State, Reader, and Writer monads
; Canonical versions - no duplicates
; ============================================================

; ============================================
; State Monad
; ============================================

; state-return: Return value in state monad
; (state-return 5) => (fn (s) (list 5 s))
(state-return (fn (x)
                  (fn (s) (list x s))))

; state-bind: Bind for state monad
; Threads state through computations
(state-bind (fn (m f)
                (fn (s)
                    (let ((result (m s)))
                         ((f (car result)) (cadr result))))))

; state-get: Get current state
; (state-get s) => (list s s)
(state-get (fn (s) (list s s)))

; state-put: Set new state
; (state-put x) => (fn (s) (list '() x))
(state-put (fn (new-state)
               (fn (s) (list '() new-state))))

; state-modify: Modify state with function
; (state-modify inc) => (fn (s) (list '() (inc s)))
(state-modify (fn (f)
                  (fn (s) (list '() (f s)))))

; run-state: Run state monad with initial state
; Returns (list value final-state)
(run-state (fn (m init-state)
               (m init-state)))

; eval-state: Evaluate state monad, returning only value
(eval-state (fn (m init-state)
                (car (m init-state))))

; exec-state: Execute state monad, returning only final state
(exec-state (fn (m init-state)
                (cadr (m init-state))))

; ============================================
; Reader Monad
; ============================================

; reader-return: Return value in reader monad
; (reader-return 5) => (fn (env) 5)
(reader-return (fn (x)
                   (fn (env) x)))

; reader-bind: Bind for reader monad
; Threads environment through computations
(reader-bind (fn (m f)
                 (fn (env)
                     ((f (m env)) env))))

; reader-ask: Get environment
; reader-ask => (fn (env) env)
(reader-ask (fn (env) env))

; reader-local: Run with modified environment
; (reader-local f m) runs m with environment modified by f
(reader-local (fn (f m)
                  (fn (env) (m (f env)))))

; run-reader: Run reader with environment
(run-reader (fn (m env)
                (m env)))

; ============================================
; Writer Monad
; ============================================

; writer-return: Return value in writer monad
; (writer-return 5) => (list 5 '())
(writer-return (fn (x)
                   (list x '())))

; writer-bind: Bind for writer monad (log is list)
; Accumulates log entries
(writer-bind (fn (m f)
                 (let ((result (f (car m))))
                      (list (car result) (append (cadr m) (cadr result))))))

; writer-tell: Write to log
; (writer-tell "message") => (list '() (list "message"))
(writer-tell (fn (w)
                 (list '() (list w))))

; run-writer: Run writer, returning (value log) pair
; Identity function - writer values are already in the right form
(run-writer id)

; --- Module Exports ---
; (see exports.ss for exported symbols)
