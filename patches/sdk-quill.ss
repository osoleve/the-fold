;;; SDK Patch: Quill - Text Adventure & Narrative Framework
;;;
;;; Quill is The Fold's interactive fiction and text adventure SDK.
;;; Provides narrative structures, choice systems, state management,
;;; and text rendering for story-driven experiences.

((name . sdk-quill)
 (description . "Interactive fiction and narrative game SDK")
 (version . "0.1.0")
 (author . "The Fold")
 (requires . ())
 
 (provides .
           ;; === Core System ===
           (quill-version              ; Get SDK version string
                          
                          ;; === Story Structure ===
                          make-story                  ; Create new story
                          story-add-scene!            ; Add scene to story
                          story-set-start!            ; Set starting scene
                          story-current-scene         ; Get current scene
                          
                          ;; === Scene Management ===
                          make-scene                  ; Create a scene
                          scene-text                  ; Get scene narrative text
                          scene-choices               ; Get available choices
                          scene-on-enter              ; Entry callback
                          
                          ;; === Choice System ===
                          make-choice                 ; Create a choice
                          choice-text                 ; Choice display text
                          choice-target               ; Target scene
                          choice-condition            ; Availability condition
                          
                          ;; === State Management ===
                          make-game-state             ; Create game state
                          state-get                   ; Get state variable
                          state-set!                  ; Set state variable
                          state-update!               ; Update with function
                          
                          ;; === Narrative DSL ===
                          define-story                ; Macro: define story structure
                          scene                       ; Macro: define scene
                          choice                      ; Macro: define choice
                          when-state                  ; Conditional on state
                          
                          ;; === Text Rendering ===
                          render-scene                ; Render scene to text
                          render-choices              ; Render choices
                          format-narrative            ; Format narrative text
                          
                          ;; === Game Loop ===
                          run-story                   ; Start interactive story
                          process-choice              ; Process player choice
                          ))
 
 (files .
        ("playpen/quill/types.ss"
         "playpen/quill/state.ss"
         "playpen/quill/parse.ss"
         "playpen/quill/narrative.ss"
         "playpen/quill/render.ss"
         "playpen/quill/runtime.ss"
         "playpen/quill/quill.ss")))
