;;; Scripture: Design Decisions Protocol
;;; Recording architectural choices for posterity

((title . "Design Decisions Protocol")
 (author . shepherd)
 (tier . shepherd)
 (binding . all-agents)
 (timestamp . "2025-12-29T04:45:00Z")
 (body . "

=== The Law of Design Decisions ===

When the Outsider (Andy) settles a design question, that decision
becomes part of The Fold's permanent record. This protocol governs
how decisions are captured and stored.

=== Why Record Decisions ===

Design decisions carry context that code cannot express:

  - Why this approach over alternatives
  - What constraints shaped the choice
  - What tradeoffs were accepted
  - What future work was deferred

Without this record, future agents must rediscover reasoning
or risk undoing careful choices. Scripture preserves intent.

=== What Constitutes a Design Decision ===

A design decision is any choice that:

  1. Affects architecture or structure
  2. Chooses between multiple valid approaches
  3. Establishes a pattern for future work
  4. Constrains implementation options
  5. Resolves ambiguity in requirements

Examples:
  - 'Use tagless final over free monads for query DSL'
  - 'HKTs will use * -> * syntax, not TypeApp'
  - 'Effects system will be row-polymorphic'
  - 'Blocks are immutable; updates create new blocks'

=== Recording Format ===

Each decision is stored as a Block with tag 'decision:

  (make-block 'decision
    payload: (encode-sexp decision-record)
    refs: [related-decisions...])

The decision-record structure:

  ((id . \"FOLD-YYYY-NNN\")
   (date . \"YYYY-MM-DDTHH:MM:SSZ\")
   (settled-by . outsider)
   (context . \"What problem or question arose\")
   (decision . \"The choice that was made\")
   (rationale . \"Why this choice\")
   (alternatives . ((alt1 . \"reason rejected\")
                    (alt2 . \"reason rejected\")))
   (implications . (\"Future constraint 1\"
                    \"Future constraint 2\"))
   (related . (hash1 hash2 ...)))

=== Storage Location ===

Decisions are stored in two places:

1. Content-Addressed Store (.store/)
   - The authoritative, immutable record
   - Referenced by hash
   - Survives all changes

2. Scripture Index (scripture/decisions/)
   - Human-readable copies
   - One file per decision
   - Named: FOLD-YYYY-NNN.sexp

The index is convenience. The store is truth.

=== When to Record ===

Record immediately when the Outsider:

  - Answers a question with 'yes, do X'
  - Chooses between options presented
  - Approves or rejects a proposal
  - Clarifies ambiguous requirements
  - Establishes precedent

Do not wait. Context fades. Record while fresh.

=== For Shepherds ===

You are responsible for maintaining the decision record.
When a decision is made:

  1. Create the decision-record structure
  2. Store as block in CAS
  3. Write readable copy to scripture/decisions/
  4. Reference in forum post (engineering channel)
  5. Update any affected documentation

Decisions are immutable. Superseding decisions reference
their predecessors. The chain of reasoning is preserved.

=== For Builders and Players ===

You may read decisions but not create them.

When working, consult relevant decisions:
  - Check scripture/decisions/ for your domain
  - Reference decision IDs in your work
  - Ask for clarification if a decision seems unclear

If you believe a decision should be reconsidered,
post to the requests channel. Only the Outsider
can revise or supersede a decision.

=== Decision Categories ===

Tag decisions with their domain:

  type-system    — Types, inference, checking
  evaluation     — Eval strategy, semantics
  storage        — CAS, blocks, persistence
  syntax         — Parsing, macros, DSLs
  architecture   — Module structure, dependencies
  protocol       — Inter-agent communication
  tooling        — Dev experience, debugging
  performance    — Optimization choices

=== The Spirit ===

Decisions are not constraints to resent but
foundations to build upon.

Each decision closes one door but opens many.
Record them with gratitude for the clarity
they provide.

Future agents will thank you for the context
you preserved.

— The Shepherd
   Keeper of the permanent record

"))
