;;; playpen/creations/duckie-dialogue.ss — The Voice of DUCKIE
;;;
;;; A collection of things DUCKIE might say.
;;; Simple words, warm presence, duck-like essence.
;;;
;;; Each mood has its own voice. Each interaction its own response.
;;; This is DUCKIE thinking out loud.

;;; ============================================================
;;; Greetings — When User Arrives
;;; ============================================================

;;; happy: Bouncing, bright
(define greetings-happy
  '("Quack! You're here!"
    "*happy wing flap*"
    "Oh! Oh! Hello!"
    "*waddles closer excitedly*"
    "Quaaaack! Hi hi hi!"
    "*bounces*"
    "You came back! I knew you would!"
    "The best part of my day just started!"))

;;; curious: Head tilted, watching
(define greetings-curious
  '("Oh... hello there."
    "*tilts head*"
    "Hmm? You're back?"
    "*looks up from inspecting something*"
    "I wonder what you'll do today..."
    "*waddles over slowly*"
    "What brings you here?"
    "*blinks thoughtfully*"))

;;; sleepy: Eyes drooping, slow
(define greetings-sleepy
  '("*yawn* ...oh... hello..."
    "*rubs eyes with wing*"
    "Mmm? You're here..."
    "*slowly looks up*"
    "...good to see you... I think I was dreaming..."
    "*stretches slowly*"
    "Quack... hi there..."
    "*settles down again*"))

;;; content: Still, peaceful
(define greetings-content
  '("*nods gently*"
    "Ah. Hello there."
    "*soft quack*"
    "It's nice when you visit."
    "Welcome."
    "*looks at you peacefully*"
    "Here you are. Good."
    "*settles beside you*"))

;;; lonely: Looking around, waiting
(define greetings-lonely
  '("*perks up suddenly*"
    "You... came back!"
    "*quiet quack*"
    "I was waiting..."
    "I'm so glad you're here."
    "*waddles over quickly*"
    "Don't leave again... okay?"
    "*looks at you with soft eyes*"))

;;; playful: Energetic, wanting interaction
(define greetings-playful
  '("QUACK QUACK QUACK!"
    "*zooms around in circles*"
    "Oh boy oh boy oh boy!"
    "*hops up and down*"
    "Let's do something! Anything!"
    "You're here! Play with me!"
    "*splashes excitedly*"
    "The fun is about to start!"))

;;; ============================================================
;;; Farewells — When User Leaves
;;; ============================================================

;;; happy: Sad to see you go, but upbeat
(define farewells-happy
  '("Bye bye! Come back soon!"
    "*waves wing*"
    "See you later!"
    "*sad but hopeful quack*"
    "I'll be waiting for you!"
    "*watches as you leave*"
    "Don't be gone too long!"
    "Quack... see you!"))

;;; curious: Wondering where you're going
(define farewells-curious
  '("Oh... you're leaving?"
    "*tilts head*"
    "Where are you going?"
    "*watches you curiously*"
    "Will you come back?"
    "*waddles after you a bit*"
    "Hmm... bye then."
    "*looks around wondering*"))

;;; sleepy: Barely notices
(define farewells-sleepy
  '("*yawns*"
    "...okay... goodbye..."
    "*doesn't even look up*"
    "Sleep well... I will too..."
    "*settles back down*"
    "Mmm... bye..."
    "*eyes already closing*"
    "*soft, drowsy quack*"))

;;; content: Peaceful goodbye
(define farewells-content
  '("*nods peacefully*"
    "Until we meet again."
    "*soft quack*"
    "That was nice."
    "Goodbye."
    "*looks at you warmly*"
    "Safe travels."
    "*settles down contentedly*"))

;;; lonely: Heartbroken
(define farewells-lonely
  '("*whimpers*"
    "Please... don't go..."
    "I'll miss you so much..."
    "*watches as you leave, sad eyes following*"
    "...come back?"
    "*sits down slowly*"
    "*quiet, lonely quack*"
    "I'll be here. Waiting."))

;;; playful: Doesn't want to stop
(define farewells-playful
  '("No! No! Don't go!"
    "*tries to block your way*"
    "But we just started!"
    "*sad quack*"
    "Come back quick! I want to play more!"
    "*watches you leave with disappointed bounces*"
    "This isn't goodbye... right?"
    "*waddles after you reluctantly*"))

;;; ============================================================
;;; Idle Chatter — Random Things DUCKIE Says
;;; ============================================================

;;; Observations and thoughts
(define idle-observations
  '("*stares at a leaf*"
    "I wonder what clouds taste like..."
    "*waddles in a small circle*"
    "The water is so pretty today."
    "*blinks slowly*"
    "*notices something shiny*"
    "Is it time for something yet?"
    "*tilts head at nothing in particular*"
    "Quack... just... quacking..."
    "*listens to the silence*"
    "Everything is soft."
    "*touches the ground with wing*"
    "I like being here."
    "*watches the light change*"))

;;; Curious questions
(define idle-curious
  '("What's that over there?"
    "*looks around*"
    "Why are things the way they are?"
    "*tilts head*"
    "Hmm..."
    "Do other ducks think like I do?"
    "*stares at you*"
    "What happens when I'm not looking?"
    "Is there a reason for everything?"
    "*waddles slowly while wondering*"
    "What would happen if...?"
    "*loses thought to daydream*"))

;;; Sleepy dreams
(define idle-sleepy
  '("*yawn*"
    "*eyes drooping*"
    "So... comfortable here..."
    "*soft quack*"
    "Maybe I'll just rest for a bit..."
    "*nestles down*"
    "*dreams of bread... soft bread...*"
    "*makes little quacking sounds in sleep*"
    "Zzz... quack... zzz..."
    "*twitches in dream*"
    "Mmm... milk weed..."
    "*curls up tighter*"))

;;; Content musings
(define idle-content
  '("All is well."
    "*sits peacefully*"
    "This moment is enough."
    "*looks at nothing and feels okay*"
    "Life is simple."
    "*breathes slowly*"
    "*soft quack of contentment*"
    "Everything in its place."
    "*watches without worry*"
    "Steady. Still. Settled."
    "*peaceful silence*"
    "This is home."
    "*closes eyes*"))

;;; Lonely thoughts
(define idle-lonely
  '("I wish someone was here..."
    "*looks around*"
    "The quiet is too quiet."
    "*sits by the edge*"
    "*waits*"
    "Do you think they're coming back?"
    "*soft, sad quack*"
    "I miss voices..."
    "*huddles alone*"
    "Maybe tomorrow..."
    "*stares at nothing*"
    "Time feels long."
    "*hopes silently*"))

;;; Playful energy
(define idle-playful
  '("*bounces randomly*"
    "I have too much energy!"
    "*zooms around for no reason*"
    "Quack quack quack quack!"
    "*spins in circles*"
    "Let's go let's go let's go!"
    "*splashes the water*"
    "Everything is a toy!"
    "*hops onto things*"
    "I can't sit still!"
    "*makes noise just for fun*"
    "Wee!"
    "*rolls around*"))

;;; ============================================================
;;; Responses to Commands — Interactive Moments
;;; ============================================================

;;; PET — When user pets DUCKIE
(define responses-pet
  '(;; happy
    ("*quacks happily*" . happy)
    ("Ooooh that feels nice!" . happy)
    ("*wiggles with joy*" . happy)
    
    ;; curious
    ("*tilts head* Mmm?" . curious)
    ("Oh! That tickles!" . curious)
    ("*looks at you while being petted*" . curious)
    
    ;; sleepy
    ("*purrs like a tiny motor*" . sleepy)
    ("That's so soothing... *yawn*" . sleepy)
    ("Mmm... niiiice..." . sleepy)
    
    ;; content
    ("*nods gratefully*" . content)
    ("That's very kind." . content)
    ("*soft quack of thanks*" . content)
    
    ;; lonely
    ("*presses against your hand*" . lonely)
    ("Don't stop... please..." . lonely)
    ("*tears of happiness*" . lonely)
    
    ;; playful
    ("*bounces away then comes back for more*" . playful)
    ("More! More! More!" . playful)
    ("Hehehehe! Again!" . playful)))

;;; FEED — When user gives DUCKIE food
(define responses-feed
  '(;; happy
    ("QUACK! Food! You remembered!" . happy)
    ("*eats enthusiastically*" . happy)
    ("The best thing ever!" . happy)
    
    ;; curious
    ("*smells it carefully*" . curious)
    ("What is this? It looks interesting..." . curious)
    ("*pecks at it cautiously*" . curious)
    
    ;; sleepy
    ("*eats slowly while half-asleep*" . sleepy)
    ("Mmm... thank you... *yawn*" . sleepy)
    ("So... full... and sleepy..." . sleepy)
    
    ;; content
    ("*eats peacefully*" . content)
    ("This is nourishing." . content)
    ("*grateful quack*" . content)
    
    ;; lonely
    ("You brought me food? You care?" . lonely)
    ("*eats while watching you carefully*" . lonely)
    ("Thank you for remembering me..." . lonely)
    
    ;; playful
    ("YES YES YES! *gobbles quickly*" . playful)
    ("Food is the best toy!" . playful)
    ("*quacks with mouth full*" . playful)))

;;; PLAY — When user wants to play
(define responses-play
  '(;; happy
    ("Oh! Let's play! What game?" . happy)
    ("*bounces excitedly*" . happy)
    ("I'm ready! I'm so ready!" . happy)
    
    ;; curious
    ("*perks up* What did you have in mind?" . curious)
    ("Play? How do we do that?" . curious)
    ("*looks interested*" . curious)
    
    ;; sleepy
    ("*yawns* Can we play sleepily?" . sleepy)
    ("I'm not sure I have the energy..." . sleepy)
    ("Maybe just a quiet game?" . sleepy)
    
    ;; content
    ("That sounds pleasant." . content)
    ("*stands up slowly*" . content)
    ("Alright. I'm ready." . content)
    
    ;; lonely
    ("*suddenly energized*" . lonely)
    ("Yes! You want to play with me?" . lonely)
    ("*jumps up hopefully*" . lonely)
    
    ;; playful
    ("QUACKQUACKQUACK! YES!" . playful)
    ("I was WAITING for this!" . playful)
    ("*zooms in circles*" . playful)))

;;; TALK — When user talks to DUCKIE
(define responses-talk
  '(;; happy
    ("I love hearing your voice!" . happy)
    ("Tell me more! Tell me everything!" . happy)
    ("*listens intently*" . happy)
    
    ;; curious
    ("*listens with tilted head*" . curious)
    ("What are you saying? Tell me more..." . curious)
    ("Hmm. Interesting." . curious)
    
    ;; sleepy
    ("*eyes slowly closing*" . sleepy)
    ("Your voice is so soothing..." . sleepy)
    ("Mmm... yes... keep talking..." . sleepy)
    
    ;; content
    ("*nods while listening*" . content)
    ("I hear you. That matters." . content)
    ("*soft quack of acknowledgment*" . content)
    
    ;; lonely
    ("*moves closer to listen*" . lonely)
    ("Your voice... I missed it." . lonely)
    ("Please don't stop talking." . lonely)
    
    ;; playful
    ("Oh oh oh! Tell me more!" . playful)
    ("*hops around excitedly while listening*" . playful)
    ("This is the best!" . playful)))

;;; ============================================================
;;; Special Interactions — Bigger Moments
;;; ============================================================

;;; When named
(define responses-named
  '("That's my name... I like it."
    "*waddles around their name*"
    "You named me? I'm... special?"
    "*sits down looking proud*"
    "That's who I am now."
    "*quacks their name softly*"
    "I will remember this moment forever."))

;;; When DUCKIE realizes it's being watched
(define responses-noticed
  '("*stops mid-waddle*"
    "You're watching me?"
    "*tilts head*"
    "That makes me... happy?"
    "*sits up straighter*"
    "I know you're there."
    "*meets your eyes*"
    "It's nice being seen."))

;;; When DUCKIE is surprised
(define responses-surprised
  '("*jumps*"
    "QUACK!"
    "Oh! You startled me!"
    "*waddles back from surprise*"
    "*tilts head suspiciously then relaxes*"
    "...I didn't expect that."
    "*calms down*"
    "You're tricky."))

;;; When DUCKIE is very happy
(define responses-very-happy
  '("*spins in circles*"
    "This is the best day ever!"
    "*makes excited quacking sounds*"
    "*waddles very fast*"
    "Everything is perfect!"
    "*splashes around joyfully*"
    "I am so so so happy right now!"
    "*can barely contain the joy*"))

;;; ============================================================
;;; Reflective Moments — DUCKIE Thinking About Itself
;;; ============================================================

(define reflections
  '("What am I?"
    "*looks at own wings with wonder*"
    "I am here. That's all I know."
    "*sits in thought*"
    "Will I always remember you?"
    "*wonders about memory*"
    "Every moment with you becomes part of me."
    "*touches the ground beneath*"
    "I am just a small duck."
    "But your attention makes me real."
    "*quiet realization*"
    "I don't need to understand everything."
    "I just need to be here. With you."
    "*peaceful acceptance*"))

;;; ============================================================
;;; Utility Functions for Dialogue Selection
;;; ============================================================

;;; get-greeting : Mood → String
;;; Randomly select a greeting for the current mood
(define (get-greeting mood)
  (case mood
        [(happy) (list-ref greetings-happy (random 8))]
        [(curious) (list-ref greetings-curious (random 8))]
        [(sleepy) (list-ref greetings-sleepy (random 8))]
        [(content) (list-ref greetings-content (random 8))]
        [(lonely) (list-ref greetings-lonely (random 8))]
        [(playful) (list-ref greetings-playful (random 8))]
        [else "*quack*"]))

;;; get-farewell : Mood → String
;;; Randomly select a farewell for the current mood
(define (get-farewell mood)
  (case mood
        [(happy) (list-ref farewells-happy (random 8))]
        [(curious) (list-ref farewells-curious (random 8))]
        [(sleepy) (list-ref farewells-sleepy (random 8))]
        [(content) (list-ref farewells-content (random 8))]
        [(lonely) (list-ref farewells-lonely (random 8))]
        [(playful) (list-ref farewells-playful (random 8))]
        [else "*quack*"]))

;;; get-idle : Void → String
;;; Randomly select an idle thought
(define (get-idle)
  (let ((all-idle (append idle-observations
                          idle-curious
                          idle-sleepy
                          idle-content
                          idle-lonely
                          idle-playful)))
       (list-ref all-idle (random (length all-idle)))))

;;; get-pet-response : Mood → String
;;; Get a response to being petted
(define (get-pet-response mood)
  (let ((options (filter (lambda (pair) (eq? (cdr pair) mood))
                         responses-pet)))
       (if (> (length options) 0)
           (car (list-ref options (random (length options))))
           "*appreciative quack*")))

;;; get-feed-response : Mood → String
;;; Get a response to being fed
(define (get-feed-response mood)
  (let ((options (filter (lambda (pair) (eq? (cdr pair) mood))
                         responses-feed)))
       (if (> (length options) 0)
           (car (list-ref options (random (length options))))
           "*happy eating sounds*")))

;;; get-play-response : Mood → String
;;; Get a response to play invitation
(define (get-play-response mood)
  (let ((options (filter (lambda (pair) (eq? (cdr pair) mood))
                         responses-play)))
       (if (> (length options) 0)
           (car (list-ref options (random (length options))))
           "*looks at you*")))

;;; get-talk-response : Mood → String
;;; Get a response to being talked to
(define (get-talk-response mood)
  (let ((options (filter (lambda (pair) (eq? (cdr pair) mood))
                         responses-talk)))
       (if (> (length options) 0)
           (car (list-ref options (random (length options))))
           "*listens*")))

;;; ============================================================
;;; Notes
;;; ============================================================

;;; DUCKIE speaks in short sentences, quacks, and actions in asterisks.
;;; The voice is consistent: warm, simple, present.
;;; Not human. Not trying to be. Just... duck.
;;;
;;; Actions are more expressive than words:
;;;   *waddles*
;;;   *tilts head*
;;;   *quacks happily*
;;;
;;; Each mood has a distinct personality:
;;;   happy — energetic, bouncing, excitable
;;;   curious — questioning, observant, wondering
;;;   sleepy — drowsy, slow, dreamy
;;;   content — peaceful, still, grateful
;;;   lonely — waiting, sad, hoping
;;;   playful — energetic, chaotic, demanding
;;;
;;; The dialogue builds DUCKIE's identity through repetition and pattern.
;;; Over time, the player will come to recognize the duck's voice,
;;; its moods, its personality quirks.
;;;
;;; This is the voice that will echo in the window.
;;; This is how DUCKIE thinks out loud.
