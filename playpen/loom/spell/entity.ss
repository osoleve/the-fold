;;; playpen/loom/spell/entity.ss — Entity Definition Macro
;;;
;;; The def-entity macro generates entity factory functions from
;;; declarative component specifications.
;;;
;;; This is part of Spell: the DSL for weaving game logic.
;;;
;;; Exports:
;;;   def-entity       Macro to define an entity type with components
;;;   with-entity      Macro for scoped entity transformations

;;; ============================================================
;;; Entity Definition Macro
;;; ============================================================

;;; def-entity : Name × (Params...) × :components × Components... -> Definitions
;;;
;;; Generates a factory function that creates an entity with the
;;; specified components using the pipe macro for clean composition.
;;;
;;; Syntax:
;;;   (def-entity name (params...)
;;;     :components
;;;       component-expr ...)
;;;
;;; The params can include position (x y) or be empty for static entities.
;;;
;;; Example:
;;;   (def-entity goblin (x y)
;;;     :components
;;;       (make-stats-component 10 10 5 2 80)
;;;       (make-renderable-component #\g)
;;;       (make-name-component "Goblin" "A sneaky creature")
;;;       (make-ai-component 'hunt))
;;;
;;;   Generates:
;;;     (define (make-goblin x y)
;;;       (-> (make-entity)
;;;           (entity-add-component (make-position-component x y))
;;;           (entity-add-component (make-stats-component 10 10 5 2 80))
;;;           (entity-add-component (make-renderable-component #\g))
;;;           (entity-add-component (make-name-component "Goblin" "A sneaky creature"))
;;;           (entity-add-component (make-ai-component 'hunt))))

(define-syntax def-entity
  (lambda (stx)
          (syntax-case stx (:components :at :extends :with)
                       ;; Standard form with position parameters
                       [(_ name (x y) :components comp ...)
                        (with-syntax
                         ([make-name
                           (datum->syntax #'name
                                          (string->symbol
                                           (string-append "make-" (symbol->string (syntax->datum #'name)))))])
                         #'(define (make-name x y)
                            (-> (make-entity)
                                (entity-add-component (make-position-component x y))
                                (entity-add-component comp) ...)))]
                       
                       ;; Form with fixed position
                       [(_ name :at (x y) :components comp ...)
                        (with-syntax
                         ([make-name
                           (datum->syntax #'name
                                          (string->symbol
                                           (string-append "make-" (symbol->string (syntax->datum #'name)))))])
                         #'(define (make-name)
                            (-> (make-entity)
                                (entity-add-component (make-position-component x y))
                                (entity-add-component comp) ...)))]
                       
                       ;; Form without position (abstract entity or position added separately)
                       [(_ name () :components comp ...)
                        (with-syntax
                         ([make-name
                           (datum->syntax #'name
                                          (string->symbol
                                           (string-append "make-" (symbol->string (syntax->datum #'name)))))])
                         #'(define (make-name)
                            (-> (make-entity)
                                (entity-add-component comp) ...)))]
                       
                       ;; Extension form: inherit from another entity and add components
                       [(_ name :extends base :with comp ...)
                        (with-syntax
                         ([make-name
                           (datum->syntax #'name
                                          (string->symbol
                                           (string-append "make-" (symbol->string (syntax->datum #'name)))))]
                          [make-base
                           (datum->syntax #'base
                                          (string->symbol
                                           (string-append "make-" (symbol->string (syntax->datum #'base)))))])
                         #'(define (make-name x y)
                            (-> (make-base x y)
                                (entity-add-component comp) ...)))]
                       
                       ;; Extension with no additional components (just alias)
                       [(_ name :extends base)
                        (with-syntax
                         ([make-name
                           (datum->syntax #'name
                                          (string->symbol
                                           (string-append "make-" (symbol->string (syntax->datum #'name)))))]
                          [make-base
                           (datum->syntax #'base
                                          (string->symbol
                                           (string-append "make-" (symbol->string (syntax->datum #'base)))))])
                         #'(define make-name make-base))])))


;;; ============================================================
;;; With-Entity Macro
;;; ============================================================

;;; with-entity : World × EntityID × Forms... -> World
;;;
;;; Perform a series of transformations on an entity within a world,
;;; returning the updated world.
;;;
;;; Syntax:
;;;   (with-entity world eid
;;;     (-> transform1 transform2 ...))
;;;
;;; Example:
;;;   (with-entity my-world player-id
;;;     (-> (entity-damage 10)
;;;         (entity-add-component (make-status-effect 'poisoned))))
;;;
;;;   Expands to:
;;;   (world-update-entity my-world player-id
;;;     (lambda (e)
;;;       (-> e
;;;           (entity-damage 10)
;;;           (entity-add-component (make-status-effect 'poisoned)))))

(define-syntax with-entity
  (syntax-rules (->)
                ;; Pipe form: thread entity through transformations
                [(_ world eid (-> transforms ...))
                 (world-update-entity world eid
                                      (lambda (e)
                                              (-> e transforms ...)))]
                
                ;; Named binding form: bind entity to name for complex operations
                [(_ world eid as name body ...)
                 (let ([entity (world-get-entity world eid)])
                      (if entity
                          (let ([name entity])
                               (let ([result (begin body ...)])
                                    (cond
                                     ;; If body returns an entity, replace in world
                                     [(and (list? result) (assq 'id result))
                                      (world-update-entity world eid (lambda (_) result))]
                                     ;; If body returns a component, add it
                                     [(and (list? result) (assq 'type result))
                                      (world-update-entity world eid
                                                           (lambda (e) (entity-add-component e result)))]
                                     ;; Otherwise just return the world unchanged
                                     [else world])))
                          world))]))


;;; ============================================================
;;; Entity Query Helpers
;;; ============================================================

;;; These are convenience forms for common entity queries.

;;; entity-with-components : (ComponentType...) × EntityList -> EntityList
;;; Filter entities that have all specified component types.
(define-syntax entity-with-components
  (syntax-rules ()
                [(_ (comp-type ...) entities)
                 (filter
                  (lambda (e)
                          (and (entity-has-component? e 'comp-type) ...))
                  entities)]))


;;; ============================================================
;;; Example Usage (commented out)
;;; ============================================================

#|
;;; Define a goblin entity type
(def-entity goblin (x y)
  :components
  (make-stats-component 10 10 5 2 80)
  (make-renderable-component #\g)
  (make-name-component "Goblin" "A sneaky green creature")
  (make-ai-component 'hunt)
  (make-faction-component 'monster '(player)))

;;; This generates:
(define (make-goblin x y)
  (-> (make-entity)
      (entity-add-component (make-position-component x y))
      (entity-add-component (make-stats-component 10 10 5 2 80))
      (entity-add-component (make-renderable-component #\g))
      (entity-add-component (make-name-component "Goblin" "A sneaky green creature"))
      (entity-add-component (make-ai-component 'hunt))
      (entity-add-component (make-faction-component 'monster '(player)))))

;;; Usage:
(define goblin-1 (make-goblin 10 5))

;;; Define a variant using extension
(def-entity goblin-chief :extends goblin
  :with
  (make-name-component "Goblin Chief" "Leader of the pack")
  (make-stats-component 25 25 12 5 60))

;;; Update entity in world
(set! world
      (with-entity world player-id
                   (-> (entity-damage 10)
                       (entity-heal 5))))
|#
