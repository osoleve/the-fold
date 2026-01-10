;;; scripture/satin-design.sexp — Satin DSL Architecture
;;;
;;; Satin is a declarative, education-first DSL that compiles into Quill stories.
;;; Named for smooth, lustrous fabric — authoring that feels good.
;;; Created: 2025-12-29

(satin-architecture
 (version "0.1.0")
 (status 'approved)

 ;;; ============================================================
 ;;; Design Principles
 ;;; ============================================================

 (principles
  (declarative-first "Satin is configuration, not code"
                     "Data describes what, not how"
                     "Minimize imperative escape hatches")

  (education-focus "Exercises and rubrics are first-class"
                   "Progress tracking built-in"
                   "Source-mapped errors for learner feedback")

  (deterministic "No ambient randomness or time"
                 "Same input → same output"
                 "Explicit random seeds when needed")

  (source-preservation "All constructs preserve source location"
                       "Error messages point to DSL source"
                       "Round-trip: DSL → Quill → validation → report")

  (composable "Stories can import other stories"
              "Library modules for reusable patterns"
              "Clean namespace management"))

 ;;; ============================================================
 ;;; Surface Syntax Overview
 ;;; ============================================================

 (syntax-overview
  (philosophy "S-expressions throughout"
              "Forms are self-documenting"
              "Optional keyword arguments where clarity helps")

  (top-level-forms
   '(satin-story ...)      "Root story definition"
   '(satin-module ...)     "Reusable library"
   '(satin-import ...)     "Import module")

  (story-components
   '(node ...)             "Narrative node/scene"
   '(choice ...)           "Player decision"
   '(when-cond ...)        "Conditional content"
   '(effect ...)           "State manipulation"
   '(dialogue ...)         "NPC conversation tree"
   '(quest ...)            "Quest/objective tracking"
   '(exercise ...)         "Educational exercise"
   '(rule ...)             "Global reactive rule"
   '(timeline ...)         "Time-triggered events"))

 ;;; ============================================================
 ;;; Core Forms
 ;;; ============================================================

 (core-forms
  (satin-story
   (description "Root container for a complete story/lesson")
   (syntax
    '(satin-story
      (id <symbol>)
      (title <string>)
      (author <string>)
      (version <string>)
      (start <node-id>)
      (meta (<key> <value>) ...)
      <node|dialogue|quest|exercise|rule|timeline> ...))
   (example
    '(satin-story
      (id intro-to-scheme)
      (title "Introduction to Scheme")
      (author "Shepherd")
      (version "1.0")
      (start welcome)

      (node welcome
        (title "Welcome!")
        (body "Let's learn about S-expressions.")
        (choice "Begin" first-lesson)))))

  (node
   (description "A narrative scene/location/state")
   (syntax
    '(node <id:symbol>
       (title <string>)
       (body <string|template>)
       (on-enter <effect> ...)
       (on-exit <effect> ...)
       <choice> ...))
   (body-templates "Body can interpolate state"
                   "{var:name} → variable value"
                   "{flag:visited?} → flag status"
                   "{item:key} → item description"))

  (choice
   (description "Player decision leading to another node")
   (syntax
    '(choice <label:string> <target:symbol>
       [:when <guard>]
       [:effects <effect> ...]))
   (guards "Guards determine visibility/availability"
           '(flag? <sym>)      "Check flag"
           '(has-item? <sym>)  "Check inventory"
           '(var>= <sym> <n>)  "Numeric comparison"
           '(and ...)          "Boolean combination"
           '(or ...)
           '(not ...)))

  (effect
   (description "State manipulation action")
   (forms
    '(set-flag! <sym>)      "Set boolean flag"
    '(clear-flag! <sym>)    "Clear flag"
    '(give-item! <sym>)     "Add to inventory"
    '(take-item! <sym>)     "Remove from inventory"
    '(set-var! <sym> <val>) "Set variable"
    '(inc-var! <sym> <n>)   "Increment variable"
    '(dec-var! <sym> <n>)   "Decrement variable"
    '(print! <string>)      "Output message"
    '(goto! <node>)         "Immediate transition"
    '(start-dialogue! <id>) "Begin dialogue"
    '(activate-quest! <id>) "Start quest"
    '(complete-quest! <id>) "Mark quest complete")))

 ;;; ============================================================
 ;;; Educational Forms
 ;;; ============================================================

 (education-forms
  (exercise
   (description "Interactive assessment with automated grading")
   (syntax
    '(exercise <id:symbol>
       (title <string>)
       (prompt <string|template>)
       (hints <string> ...)
       (rubric
        (check <id> <description> <predicate>)
        ...)
       (on-pass <effect> ...)
       (on-fail <effect> ...)))
   (predicates "Check functions receive (story run answer)"
               '(answer-eq? <value>)     "Exact match"
               '(answer-contains? <str>) "Substring"
               '(answer-matches? <re>)   "Regex match"
               '(answer-length>= <n>)    "Minimum length"
               '(custom <fn>)            "Custom predicate"))

  (lesson
   (description "Sequence of nodes + exercises forming a unit")
   (syntax
    '(lesson <id:symbol>
       (title <string>)
       (objectives <string> ...)
       (prerequisites <lesson-id> ...)
       (sequence <node-or-exercise-id> ...)
       (on-complete <effect> ...))))

  (progress
   (description "Track learner advancement")
   (forms
    '(progress-node <id>)    "Mark node visited"
    '(progress-complete?)    "All nodes visited?"
    '(exercise-passed? <id>) "Check exercise"
    '(lesson-complete? <id>) "Check lesson")))

 ;;; ============================================================
 ;;; Narrative Forms
 ;;; ============================================================

 (narrative-forms
  (dialogue
   (description "Branching conversation with NPC")
   (syntax
    '(dialogue <id:symbol>
       (start <node-id>)
       (participants <character> ...)
       (node <id>
         (speaker <character>)
         (text <string|template>)
         (on-enter <effect> ...)
         <choice> ...)))
   (special-targets 'end "End dialogue and return"))

  (quest
   (description "Multi-step objective tracking")
   (syntax
    '(quest <id:symbol>
       (title <string>)
       (description <string>)
       (steps
        (step <id> <description> <completion-guard>)
        ...)
       (on-activate <effect> ...)
       (on-complete <effect> ...)
       (on-fail <effect> ...))))

  (rule
   (description "Global reactive trigger")
   (syntax
    '(rule <id:symbol>
       (when <guard>)
       (once? <boolean>)
       (effects <effect> ...)))
   (usage "Rules fire on every state change when guard is true"))

  (timeline
   (description "Time-triggered events")
   (syntax
    '(timeline
       (event <id> (at <time>) (effects ...) [:once? #t])
       ...))
   (time-model "Integer clock, ticks on node transitions")))

 ;;; ============================================================
 ;;; Module System
 ;;; ============================================================

 (module-system
  (satin-module
   (description "Reusable library of definitions")
   (syntax
    '(satin-module <name:symbol>
       (provides <symbol> ...)
       (requires <module-name> ...)
       <definition> ...))
   (definitions "Nodes, dialogues, quests, exercises, helpers"))

  (satin-import
   (description "Import module into story")
   (syntax
    '(satin-import <module-name>
       [:prefix <prefix>]
       [:only (<symbol> ...)]
       [:except (<symbol> ...)]))
   (resolution "Search path: local, stdlib, scripture/satin/")))

 ;;; ============================================================
 ;;; Source Location Strategy
 ;;; ============================================================

 (source-locations
  (principle "Every form carries source span")

  (span-format '(span file line column end-line end-column))

  (attachment "Parser attaches spans to AST nodes"
              "Compiler preserves through transformation"
              "Validation errors reference original source")

  (error-format
   '(satin-error
     (code <symbol>)
     (message <string>)
     (span <span>)
     (suggestions <string> ...)))

  (example-error
   '(satin-error
     (code undefined-node)
     (message "Choice target 'nope' not found")
     (span (span "lesson.satin" 42 5 42 30))
     (suggestions "Did you mean 'next'?"))))

 ;;; ============================================================
 ;;; Compilation Pipeline
 ;;; ============================================================

 (compilation-pipeline
  (phases
   (1-parse "Satin source → Satin AST with spans")
   (2-resolve "Resolve imports, expand macros")
   (3-validate "Check references, types, constraints")
   (4-optimize "Inline simple helpers, dead code")
   (5-emit "Generate Quill runtime structures"))

  (validation-checks
   "All node targets exist"
   "No circular dialogues"
   "All variables used are defined"
   "Exercise predicates are well-formed"
   "Quest steps are ordered correctly"
   "No duplicate IDs in scope")

  (error-recovery "Continue after non-fatal errors"
                  "Collect all issues for batch report"
                  "Prioritize: errors > warnings > info"))

 ;;; ============================================================
 ;;; Naming Conventions
 ;;; ============================================================

 (naming-conventions
  (forms "lowercase-with-dashes"
         "satin-story, node, choice, exercise")

  (ids "lowercase-with-dashes or camelCase"
       "welcome, first-lesson, introToScheme")

  (flags "lowercase ending in ?"
         "visited?, completed?, has-key?")

  (variables "lowercase"
             "score, health, turns-remaining")

  (effects "lowercase ending in !"
           "set-flag!, give-item!, print!")

  (predicates "lowercase ending in ?"
              "flag?, has-item?, var>="))

 ;;; ============================================================
 ;;; Determinism Rules
 ;;; ============================================================

 (determinism
  (rule-1 "No implicit randomness"
          "All random must use explicit (random <seed>)")

  (rule-2 "No implicit time"
          "Clock only advances on explicit tick or node enter")

  (rule-3 "Evaluation order is deterministic"
          "Effects execute in declaration order"
          "Rules fire in definition order"
          "Quests check in definition order")

  (rule-4 "Same input produces same output"
          "Replay is exact reproduction"
          "Useful for testing and assessment"))

 ;;; ============================================================
 ;;; Runtime Mapping
 ;;; ============================================================

 (quill-mapping
  (satin-story -> quill-story)
  (node -> quill-node)
  (choice -> quill-choice)
  (dialogue -> quill-dialogue)
  (quest -> quill-quest)
  (exercise -> quill-exercise)
  (rule -> quill-rule)
  (timeline -> quill-timeline-event)

  (guards "Compile to (lambda (state) ...)")
  (effects "Compile to effect forms recognized by runtime")
  (body-templates "Compile to procedures or strings"))

 ;;; ============================================================
 ;;; Implementation Roadmap
 ;;; ============================================================

 (roadmap
  (phase-1 "Core Forms"
   (priority 0)
   (tasks
    "satin-story, node, choice compilation"
    "Guard compilation"
    "Effect compilation"
    "Basic validation"
    "Source span tracking"))

  (phase-2 "Education Forms"
   (priority 1)
   (tasks
    "exercise compilation"
    "Rubric predicates"
    "lesson sequencing"
    "Progress tracking"))

  (phase-3 "Narrative Forms"
   (priority 1)
   (tasks
    "dialogue compilation"
    "quest compilation"
    "rule compilation"
    "timeline compilation"))

  (phase-4 "Module System"
   (priority 2)
   (tasks
    "satin-module definition"
    "satin-import resolution"
    "Namespace management"
    "Standard library"))

  (phase-5 "Tooling"
   (priority 3)
   (tasks
    "Linter for style checks"
    "Pretty printer"
    "Documentation generator"
    "Test runner")))

 ;;; ============================================================
 ;;; Examples
 ;;; ============================================================

 (examples
  (minimal-story
   "Smallest valid Satin story"
   (code
    "(satin-story
      (id hello)
      (title \"Hello World\")
      (start intro)

      (node intro
        (title \"Hello\")
        (body \"Welcome to Satin!\")
        (choice \"Goodbye\" farewell))

      (node farewell
        (title \"Farewell\")
        (body \"Thanks for trying Satin.\")))"))

  (with-exercise
   "Story with educational exercise"
   (code
    "(satin-story
      (id scheme-lesson-1)
      (title \"Lesson 1: S-expressions\")
      (start intro)

      (node intro
        (title \"What is an S-expression?\")
        (body \"An S-expression is a list enclosed in parentheses.
               For example: (+ 1 2)\")
        (choice \"Try it\" exercise-node))

      (node exercise-node
        (title \"Your Turn\")
        (body \"Write an S-expression that adds 3 and 4.\")
        (exercise addition-ex))

      (exercise addition-ex
        (title \"Addition Exercise\")
        (prompt \"Write (+ 3 4):\")
        (hints
          \"Remember: operator comes first\"
          \"Use parentheses around the whole expression\")
        (rubric
          (check parens \"Has parentheses\" (answer-contains? \"(\"))
          (check plus \"Uses +\" (answer-contains? \"+\"))
          (check nums \"Has 3 and 4\"
                 (and (answer-contains? \"3\")
                      (answer-contains? \"4\"))))
        (on-pass
          (set-flag! addition-learned?)
          (goto! success)))

      (node success
        (title \"Correct!\")
        (body \"Great job! You've written your first S-expression.\")))"))

  (with-dialogue
   "Story with branching dialogue"
   (code
    "(satin-story
      (id tavern-talk)
      (title \"The Tavern\")
      (start entrance)

      (node entrance
        (title \"The Rusty Nail\")
        (body \"A weathered tavern stands before you.\")
        (choice \"Enter\" tavern))

      (node tavern
        (title \"Inside the Tavern\")
        (body \"A grizzled bartender eyes you.\")
        (choice \"Talk to bartender\" bartender-talk
          :effects (start-dialogue! bartender-dialogue)))

      (dialogue bartender-dialogue
        (start greeting)
        (participants bartender)

        (node greeting
          (speaker bartender)
          (text \"What'll it be, traveler?\")
          (choice \"Information\" info)
          (choice \"A drink\" drink)
          (choice \"Never mind\" end))

        (node info
          (speaker bartender)
          (text \"Information costs around here...\")
          (on-enter (set-flag! asked-for-info?))
          (choice \"How much?\" price)
          (choice \"Forget it\" end))

        (node drink
          (speaker bartender)
          (text \"Five coins for the house ale.\")
          (choice \"I'll take it\" end
            :when (var>= coins 5)
            :effects (dec-var! coins 5) (set-flag! has-drink?))
          (choice \"Too rich for me\" end))

        (node price
          (speaker bartender)
          (text \"Ten coins, and it better be worth it.\")
          (choice \"Deal\" end
            :when (var>= coins 10)
            :effects (dec-var! coins 10) (set-flag! got-info?))
          (choice \"I'll pass\" end)))))"))

 ;;; ============================================================
 ;;; Standard Library (planned)
 ;;; ============================================================

 (stdlib-planned
  (satin/common "Common helpers, standard effects")
  (satin/education "Exercise predicates, progress tracking")
  (satin/narrative "Dialogue utilities, quest helpers")
  (satin/debug "Testing, validation, replay"))
)
