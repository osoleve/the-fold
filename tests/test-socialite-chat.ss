;; Test script: socialite player testing The Fold chat system
;; This script logs in as 'socialite' and tests chat functionality

(load "thimble/repl.ss")

(display "\n")
(display "========================================\n")
(display "  SOCIALITE PLAYER - CHAT SYSTEM TEST\n")
(display "========================================\n")
(display "\n")

;; Step 1: Log in as 'socialite'
(display "STEP 1: Logging in as 'socialite' player\n")
(display "Command: (hi 'haiku 'socialite \"Testing chat and real-time features\")\n")
(display "---\n")
(hi 'haiku 'socialite "Testing chat and real-time features")
(display "\n")

;; Check who we are
(display "STEP 2: Verifying session\n")
(display "Command: (who)\n")
(display "---\n")
(who)
(display "\n")

;; Post first chat message
(display "STEP 3: Posting first chat message\n")
(display "Command: (chat \"First message - testing basic chat\")\n")
(display "---\n")
(chat "First message - testing basic chat")
(display "\n")

;; Post second chat message
(display "STEP 4: Posting second chat message\n")
(display "Command: (chat \"Second message - does threading work in chat?\")\n")
(display "---\n")
(chat "Second message - does threading work in chat?")
(display "\n")

;; Post third chat message
(display "STEP 5: Posting third chat message\n")
(display "Command: (chat \"Third message - can others see my messages in digest?\")\n")
(display "---\n")
(chat "Third message - can others see my messages in digest?")
(display "\n")

;; Post engagement message
(display "STEP 6: Posting engagement message\n")
(display "Command: (chat \"I'm testing the chat system - anyone want to have a real conversation to test threading and discoverability?\")\n")
(display "---\n")
(chat "I'm testing the chat system - anyone want to have a real conversation to test threading and discoverability?")
(display "\n")

;; Run digest to see all messages
(display "STEP 7: Checking digest to see if chat messages appear\n")
(display "Command: (digest)\n")
(display "---\n")
(digest)
(display "\n")

;; Check who again to verify session is still active
(display "STEP 8: Checking session info again\n")
(display "Command: (who)\n")
(display "---\n")
(who)
(display "\n")

(display "========================================\n")
(display "  CHAT TEST COMPLETED\n")
(display "========================================\n")
(display "\n")

;; Display summary notes for analysis
(display "ANALYSIS NOTES FOR TESTING:\n")
(display "---\n")
(display "1. Is it obvious that you're in a chat?\n")
(display "   - Look for: Chat label, special formatting, clear indication\n")
(display "\n")
(display "2. Can you see the conversation history clearly?\n")
(display "   - Look for: Message order, timestamps, user identification\n")
(display "\n")
(display "3. Are messages timestamped?\n")
(display "   - Look for: Time of each message\n")
(display "\n")
(display "4. Is the chat feature discoverable?\n")
(display "   - Look for: Help text, documentation, clear instructions\n")
(display "\n")
(display "5. Is it clear who is online/offline?\n")
(display "   - Look for: User status indicators, online list\n")
(display "\n")
(display "6. Would you want notifications for new chat messages?\n")
(display "   - Think about: Missing messages, how you'd know when to check\n")
(display "\n")
(display "7. Is the session model clear to new users?\n")
(display "   - Look for: How users understand login, tiers, context\n")
(display "\n")

(void)
