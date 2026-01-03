;;; Turtle Graphics Patch
;;;
;;; Vector-based turtle graphics for a 640x480 viewport.
;;; Draw with a virtual turtle that moves and turns.

((name . turtle)
 (description . "Vector-based turtle graphics with SVG export")
 (version . "1.0.0")
 (author . "The Fold")
 (requires . ())
 
 (provides .
           ;; Core turtle operations
           (make-turtle          ; Create a new turtle at center
                        fd bk                ; Forward/backward by distance
                        rt lt                ; Right/left turn by degrees
                        pu pd                ; Pen up/down
                        home                 ; Return to center, heading north
                        cs                   ; Clear screen (reset paths)
                        setxy setx sety      ; Set position
                        setheading           ; Set heading in degrees
                        
                        ;; Pen properties
                        setpc                ; Set pen color
                        setpw                ; Set pen width
                        setbg                ; Set background color
                        st ht                ; Show/hide turtle
                        
                        ;; Query state
                        xcor ycor            ; Get x/y coordinate
                        heading              ; Get heading
                        pos                  ; Get (x . y) position
                        pendown?             ; Is pen down?
                        shown?               ; Is turtle visible?
                        
                        ;; Shapes
                        arc                  ; Draw arc
                        circle filled-circle ; Draw circle
                        polygon filled-polygon ; Draw polygon
                        square triangle star ; Convenience shapes
                        spiral               ; Draw spiral
                        
                        ;; Iteration
                        repeat               ; Repeat a thunk n times
                        
                        ;; Export
                        turtle->drawing      ; Convert turtle state to drawing
                        drawing->svg         ; Convert drawing to SVG string
                        turtle->svg          ; Convert turtle to SVG (convenience)
                        save-svg             ; Save SVG to file
                        
                        ;; Imperative interface (Logo-style)
                        *current-turtle*     ; The global turtle parameter
                        reset-turtle!        ; Reset to fresh turtle
                        fd! forward!         ; Move forward (mutating)
                        bk! back!            ; Move backward (mutating)
                        rt! right!           ; Turn right (mutating)
                        lt! left!            ; Turn left (mutating)
                        pu! penup!           ; Pen up (mutating)
                        pd! pendown!         ; Pen down (mutating)
                        home!                ; Return home (mutating)
                        setxy!               ; Set position (mutating)
                        setcolor!            ; Set color (mutating)
                        current-turtle       ; Get current turtle
                        current-svg          ; Get SVG of current drawing
                        
                        ;; Colors
                        make-color12         ; Create RGB color (12-bit)
                        color12-from-name    ; Color from name ('red, 'blue, etc.)
                        color12-names        ; List available color names
                        parse-color12        ; Parse color from various formats
                        
                        ;; Block storage
                        store-drawing!       ; Store drawing in CAS
                        fetch-drawing        ; Retrieve drawing from CAS
                        ))
 
 (files .
        ("shell/ui/turtle-color.ss"
         "shell/ui/turtle-path.ss"
         "shell/ui/turtle.ss"
         "shell/ui/turtle-svg.ss"
         "shell/ui/turtle-block.ss")))
