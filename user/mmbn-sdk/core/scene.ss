;;; user/mmbn-sdk/core/scene.ss — Scene State Machine for MMBN-style games
;;; @module mmbn/scene
;;; @requires protocol
;;; Load via: (load "user/mmbn-sdk/mmbn.ss")

;; Dependencies loaded by mmbn.ss

(doc 'module 'mmbn/scene)
(doc 'description "Scene management system. Scenes are discrete game states
(title screen, overworld, battle, menu, dialogue) that handle input, update,
and render independently. The scene manager handles transitions between scenes.")
(doc 'layer 'user)
(doc 'purity 'partial)

;;; ============================================================
;;; Scene Protocol
;;; ============================================================

(doc 'section 'scene-protocol)

(doc 'note "Scenes implement a lifecycle:
  - enter: Called when scene becomes active
  - exit: Called when scene becomes inactive
  - update: Called each frame with delta time
  - render: Called to produce display output
  - handle-input: Called with input events")

(define-protocol (scene-enter s game-state)
  "Initialize scene, returns updated game state")

(define-protocol (scene-exit s game-state)
  "Cleanup scene, returns updated game state")

(define-protocol (scene-update s dt game-state)
  "Update scene logic, returns (values scene game-state transition?)")

(define-protocol (scene-render s game-state buffer)
  "Render scene to pixel buffer")

(define-protocol (scene-handle-input s input game-state)
  "Handle input event, returns (values scene game-state)")

;;; ============================================================
;;; Scene Transitions
;;; ============================================================

(doc 'section 'transitions)

(define-record-type transition%
  (fields type target data))

(define (transition-push target . data)
  (doc 'type '(-> Symbol Data... Transition))
  (doc 'description "Push new scene onto stack")
  (doc 'export #t)
  (make-transition% 'push target (if (null? data) #f (car data))))

(define (transition-pop . data)
  (doc 'type '(-> Data... Transition))
  (doc 'description "Pop current scene, return to previous")
  (doc 'export #t)
  (make-transition% 'pop #f (if (null? data) #f (car data))))

(define (transition-replace target . data)
  (doc 'type '(-> Symbol Data... Transition))
  (doc 'description "Replace current scene")
  (doc 'export #t)
  (make-transition% 'replace target (if (null? data) #f (car data))))

(define (transition-none)
  (doc 'type '(-> Transition))
  (doc 'description "No transition")
  #f)

(define (transition? x)
  (transition%? x))

;;; ============================================================
;;; Scene Manager
;;; ============================================================

(doc 'section 'scene-manager)

(define-record-type scene-manager%
  (fields stack scene-registry))

(define (make-scene-manager)
  (doc 'type '(-> SceneManager))
  (doc 'description "Create scene manager with empty stack")
  (doc 'export #t)
  (make-scene-manager% '() (make-hashtable symbol-hash eq?)))

(define (manager-register! mgr name scene-factory)
  (doc 'type '(-> SceneManager Symbol (-> Data Scene) Void))
  (doc 'description "Register a scene factory by name")
  (doc 'export #t)
  (hashtable-set! (scene-manager%-scene-registry mgr) name scene-factory))

(define (manager-create-scene mgr name data)
  (doc 'type '(-> SceneManager Symbol Data Scene))
  (let ([factory (hashtable-ref (scene-manager%-scene-registry mgr) name #f)])
    (unless factory
      (error 'manager-create-scene "Unknown scene type" name))
    (factory data)))

(define (manager-current mgr)
  (doc 'type '(-> SceneManager (Maybe Scene)))
  (let ([stack (scene-manager%-stack mgr)])
    (if (null? stack) #f (car stack))))

(define (manager-push mgr scene)
  (doc 'type '(-> SceneManager Scene SceneManager))
  (make-scene-manager%
   (cons scene (scene-manager%-stack mgr))
   (scene-manager%-scene-registry mgr)))

(define (manager-pop mgr)
  (doc 'type '(-> SceneManager SceneManager))
  (let ([stack (scene-manager%-stack mgr)])
    (make-scene-manager%
     (if (null? stack) '() (cdr stack))
     (scene-manager%-scene-registry mgr))))

(define (manager-replace mgr scene)
  (doc 'type '(-> SceneManager Scene SceneManager))
  (let ([stack (scene-manager%-stack mgr)])
    (make-scene-manager%
     (cons scene (if (null? stack) '() (cdr stack)))
     (scene-manager%-scene-registry mgr))))

;;; ============================================================
;;; Transition Processing
;;; ============================================================

(doc 'section 'transition-processing)

(define (manager-process-transition mgr trans game-state)
  (doc 'type '(-> SceneManager Transition GameState (Values SceneManager GameState)))
  (doc 'description "Process a scene transition")
  (doc 'export #t)
  (if (not trans)
      (values mgr game-state)
      (let ([type (transition%-type trans)]
            [target (transition%-target trans)]
            [data (transition%-data trans)])
        (case type
          [(push)
           ;; Exit current, push new, enter new
           (let* ([current (manager-current mgr)]
                  [gs1 (if current (scene-exit current game-state) game-state)]
                  [new-scene (manager-create-scene mgr target data)]
                  [mgr2 (manager-push mgr new-scene)]
                  [gs2 (scene-enter new-scene gs1)])
             (values mgr2 gs2))]
          [(pop)
           ;; Exit current, pop, enter previous
           (let* ([current (manager-current mgr)]
                  [gs1 (if current (scene-exit current game-state) game-state)]
                  [mgr2 (manager-pop mgr)]
                  [prev (manager-current mgr2)]
                  [gs2 (if prev (scene-enter prev gs1) gs1)])
             (values mgr2 gs2))]
          [(replace)
           ;; Exit current, replace with new, enter new
           (let* ([current (manager-current mgr)]
                  [gs1 (if current (scene-exit current game-state) game-state)]
                  [new-scene (manager-create-scene mgr target data)]
                  [mgr2 (manager-replace mgr new-scene)]
                  [gs2 (scene-enter new-scene gs1)])
             (values mgr2 gs2))]
          [else (values mgr game-state)]))))

;;; ============================================================
;;; Scene Update Loop
;;; ============================================================

(doc 'section 'update-loop)

(define (manager-update mgr dt game-state)
  (doc 'type '(-> SceneManager Number GameState (Values SceneManager GameState)))
  (doc 'description "Update current scene and process any transitions")
  (doc 'export #t)
  (let ([current (manager-current mgr)])
    (if (not current)
        (values mgr game-state)
        (let-values ([(scene2 gs2 trans) (scene-update current dt game-state)])
          (let ([mgr2 (if (eq? scene2 current)
                          mgr
                          (manager-replace mgr scene2))])
            (manager-process-transition mgr2 trans gs2))))))

(define (manager-handle-input mgr input game-state)
  (doc 'type '(-> SceneManager Input GameState (Values SceneManager GameState)))
  (doc 'description "Send input to current scene")
  (doc 'export #t)
  (let ([current (manager-current mgr)])
    (if (not current)
        (values mgr game-state)
        (let-values ([(scene2 gs2) (scene-handle-input current input game-state)])
          (values (if (eq? scene2 current)
                      mgr
                      (manager-replace mgr scene2))
                  gs2)))))

(define (manager-render mgr game-state buffer)
  (doc 'type '(-> SceneManager GameState PixelBuffer Void))
  (doc 'description "Render current scene to buffer")
  (doc 'export #t)
  (let ([current (manager-current mgr)])
    (when current
      (scene-render current game-state buffer))))

;;; ============================================================
;;; Input Types
;;; ============================================================

(doc 'section 'input)

(define input-up    'up)
(define input-down  'down)
(define input-left  'left)
(define input-right 'right)
(define input-a     'a)      ; Confirm / Attack
(define input-b     'b)      ; Cancel / Dodge
(define input-l     'l)      ; Open chip menu
(define input-r     'r)      ; Open chip menu
(define input-start 'start)  ; Pause menu
(define input-select 'select)

(define (input? x)
  (memq x '(up down left right a b l r start select)))

;;; ============================================================
;;; Base Scene Implementations
;;; ============================================================

(doc 'section 'base-scenes)

;; Simple scene that does nothing (for testing/placeholder)
(define (make-null-scene name)
  (doc 'type '(-> Symbol Scene))
  (doc 'export #t)
  (list 'null-scene name))

(implement-protocol! 'scene-enter 'null-scene
  (lambda (s gs) gs))

(implement-protocol! 'scene-exit 'null-scene
  (lambda (s gs) gs))

(implement-protocol! 'scene-update 'null-scene
  (lambda (s dt gs) (values s gs #f)))

(implement-protocol! 'scene-render 'null-scene
  (lambda (s gs buf) (void)))

(implement-protocol! 'scene-handle-input 'null-scene
  (lambda (s input gs) (values s gs)))

(display "mmbn/scene.ss loaded.\n")
(display "  (make-scene-manager)           — Create scene manager\n")
(display "  (manager-register! m n f)      — Register scene factory\n")
(display "  (transition-push 'battle data) — Transition to new scene\n")
(display "  (transition-pop)               — Return to previous scene\n")
