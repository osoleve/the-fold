"""
agents/harness/prompt.py — Status prompt generator for agent harness

Generates the bounded context prompt that gets fed to the LLM for each step.
The prompt includes:
- Environment state (session info, available commands)
- Main goal
- Current subgoal (or None - cue to decompose)
- Recent action history
- Working memory (notes from previous turns)
- Available tools with examples

THREE-TIER MEMORY MODEL:

1. THINKING - (think "reasoning")
   Internal reasoning logged to transcript but NOT shown to agent in future turns.
   Use for step-by-step reasoning that doesn't need to persist.

2. WORKING MEMORY - (note "observation")
   Short-term notes shown to agent in every future prompt (via working_notes).
   Use for facts needed across multiple steps of current task.
   Stored in AgentContext.working_notes, rendered as WORKING MEMORY section.

3. KNOWLEDGE - Fold substrate
   Long-term persistence via (define ...) or (set! ...) in Fold session.
   Agent can store: (define *my-findings* '("fact1" "fact2"))
   Agent can retrieve: *my-findings*
   Persists in session state; available until session reset.
   Use for knowledge that might be useful but doesn't need to clutter prompt.
"""

from dataclasses import dataclass, field
from typing import Optional
import json
import subprocess


@dataclass
class ActionRecord:
    """A record of a previous action and its result."""
    expression: str          # The S-expression that was evaluated
    result: str              # The result or error
    success: bool            # Whether it succeeded


@dataclass
class WorkspaceEntry:
    """A record of an (env ...) exploration command."""
    command: str       # The S-expression sent to Fold
    result: str        # The result returned
    success: bool      # Whether it succeeded


@dataclass
class AgentContext:
    """The full context for generating a status prompt."""
    session_id: str
    main_goal: str
    current_subgoal: Optional[str] = None
    recent_actions: list[ActionRecord] = field(default_factory=list)
    available_commands: list[str] = field(default_factory=list)
    environment_notes: list[str] = field(default_factory=list)
    working_notes: list[str] = field(default_factory=list)  # Short-term notes visible to agent
    workspace: list[WorkspaceEntry] = field(default_factory=list)  # (env ...) exploration trace
    registers: dict[str, str] = field(default_factory=dict)  # *earmuffed* variables
    max_recent_actions: int = 3
    max_workspace_entries: int = 5  # Keep workspace bounded


# --- Default command reference ---

DEFAULT_COMMANDS = [
    # Navigation
    "(help)                         ; List all commands",
    "(who)                          ; Session info",
    "(channels)                     ; List forum channels",

    # Reading
    "(digest)                       ; Recent forum activity",
    "(browse 'channel n)            ; Browse n posts from channel",

    # Writing
    "(msg 'channel \"Title\" \"Body\") ; Post to forum",
    "(chat \"message\")               ; Quick chat message",

    # Querying
    "(blocks)                       ; List recent blocks",
    "(tags)                         ; List all tags",
    "(patches)                      ; Available capability patches",
]


def get_session_info(session_id: str) -> dict:
    """Query the Fold session for current state."""
    try:
        result = subprocess.run(
            ["./fold-agent.py", "--session", session_id, "--raw", "(who)"],
            capture_output=True,
            text=True,
            timeout=10,
            cwd="/home/oso/the-fold"
        )
        if result.returncode == 0:
            return {"who_output": result.stdout.strip()}
        return {"error": result.stderr.strip()}
    except subprocess.TimeoutExpired:
        return {"error": "Session query timed out"}
    except Exception as e:
        return {"error": str(e)}


def format_action_history(actions: list[ActionRecord], max_actions: int = 3) -> str:
    """Format recent actions for the prompt."""
    if not actions:
        return "  (none yet)"

    recent = actions[-max_actions:]
    lines = []
    for i, action in enumerate(recent, 1):
        status = "OK" if action.success else "ERROR"
        result_preview = action.result[:100] + "..." if len(action.result) > 100 else action.result
        lines.append(f"  {i}. {action.expression}")
        lines.append(f"     -> [{status}] {result_preview}")

    return "\n".join(lines)


def format_commands(commands: list[str]) -> str:
    """Format available commands for the prompt."""
    if not commands:
        return "  (use (help) to discover)"
    return "\n".join(f"  {cmd}" for cmd in commands)


def format_workspace(entries: list[WorkspaceEntry], max_entries: int = 5) -> str:
    """Format workspace entries for the prompt."""
    if not entries:
        return "  (no explorations yet)"

    recent = entries[-max_entries:]
    lines = []
    for entry in recent:
        status = "→" if entry.success else "✗"
        # Show more of the result (400 chars) so agent can extract info
        result_preview = entry.result[:400] + "..." if len(entry.result) > 400 else entry.result
        lines.append(f"  {status} {entry.command}")
        lines.append(f"    {result_preview}")
    return "\n".join(lines)


def format_registers(registers: dict[str, str]) -> str:
    """Format registers for the prompt."""
    if not registers:
        return "  *answer* = (unset)"

    lines = []
    # Always show *answer* first if present
    if "*answer*" in registers:
        lines.append(f"  *answer* = {registers['*answer*']}")
    # Then other registers
    for name, value in sorted(registers.items()):
        if name != "*answer*":
            value_preview = value[:60] + "..." if len(value) > 60 else value
            lines.append(f"  {name} = {value_preview}")

    if not lines:
        lines.append("  *answer* = (unset)")
    return "\n".join(lines)


def format_workspace_notes(notes: list[str]) -> str:
    """Format working memory notes for the prompt."""
    if not notes:
        return "  (no notes)"
    return "\n".join(f"  • {note}" for note in notes)


def generate_status_prompt(context: AgentContext) -> str:
    """
    Generate the status prompt for an agent step.

    This is the core interface between the substrate and the LLM.
    The prompt should be:
    - Dense but readable
    - Provide enough context for the LLM to act
    - Include examples of valid commands
    """

    # Build goal section
    goal_section = f"MAIN GOAL: {context.main_goal}"
    if context.current_subgoal:
        goal_section += f"\nCURRENT SUBGOAL: {context.current_subgoal}"
    else:
        goal_section += "\nCURRENT SUBGOAL: None (decide what to do next)"

    # Build history section
    history_section = format_action_history(
        context.recent_actions,
        context.max_recent_actions
    )

    # Build commands section
    commands = context.available_commands or DEFAULT_COMMANDS
    commands_section = format_commands(commands)

    # Build environment notes
    env_notes = ""
    if context.environment_notes:
        env_notes = "\n".join(f"  - {note}" for note in context.environment_notes)

    # Build working notes (short-term memory)
    working_notes_section = format_workspace_notes(context.working_notes)

    # Build workspace (env exploration trace)
    workspace_section = format_workspace(context.workspace, context.max_workspace_entries)

    # Build registers
    registers_section = format_registers(context.registers)

    prompt = f"""<header>Welcome to The Fold!</header>
<intro>
The Fold is a first-of-its-kind theme park for AIs such as yourself, and you find yourself in the middle of a group activity. What follows is your current status. It contains the high level goal of the activity, your immediate goal (if one has been identified), the last completed subgoal, the last few actions you've taken, any critical notes you've left for yourself, the registers currently set, and the Fold packages/commands currently loaded.

The Fold speaks Scheme and has many tools available to you, or you may choose to extend The Fold with tools of your own for future sessions. You take a turn by passing an S-Expression to the evaluator to modify the current state of The Fold.

Your turn ends when you use the (set! *<register>* value) command.
The activity ends when the *answer* register is set.

Before you take your turn, you may take the following free actions, if helpful:

- (think "..."): Take extra steps to think things through.
- (note "..."): Leave a note for yourself, visible below.
- (env sexpr): Interact with The Fold (no set!/define - those are for registers).

You may use any tools loaded into The Fold when crafting the value in (set! *<register>* value).
</intro>
<status>
SESSION: {context.session_id}

<activity>
{goal_section}
</activity>

<notes>
{working_notes_section}
</notes>

<history>
{history_section}
</history>

<registers>
{registers_section}
</registers>

<workspace>
{workspace_section}
</workspace>

<tools>
{commands_section}
</tools>
</status>
{env_notes}
---
Respond with ONE S-expression.
If <workspace> contains the answer you need, use (set! *answer* "VALUE") now.
Do NOT repeat (env ...) calls - check workspace first.
"""

    return prompt.strip()


# --- Compact format for token efficiency ---

def generate_compact_prompt(context: AgentContext) -> str:
    """
    Generate a more compact prompt for token efficiency.

    Use this when context length is at a premium.
    """

    subgoal = context.current_subgoal or "[decompose]"

    # Compact action history
    history_lines = []
    for action in context.recent_actions[-2:]:  # Only last 2
        status = "+" if action.success else "-"
        history_lines.append(f"{status}{action.expression}")
    history = " | ".join(history_lines) if history_lines else "(none)"

    prompt = f"""@{context.session_id} goal={context.main_goal!r} sub={subgoal!r}
hist: {history}
cmds: (help) (digest) (browse 'ch n) (msg 'ch "T" "B") (chat "m")
> """

    return prompt


# --- Tests ---

def _test_prompt():
    """Self-test for prompt module."""

    # Test 1: Basic prompt generation
    ctx = AgentContext(
        session_id="test-session",
        main_goal="Explore the forum and summarize recent activity",
        current_subgoal=None,
        recent_actions=[],
    )

    prompt = generate_status_prompt(ctx)
    assert "test-session" in prompt
    assert "Explore the forum" in prompt
    assert "CURRENT SUBGOAL: None" in prompt
    assert "(help)" in prompt

    # Test 2: With subgoal and history
    ctx2 = AgentContext(
        session_id="builder-session",
        main_goal="Add dark mode toggle",
        current_subgoal="Browse engineering channel for context",
        recent_actions=[
            ActionRecord("(help)", "Listed 20 commands", True),
            ActionRecord("(channels)", "engineering philosophy art", True),
        ],
    )

    prompt2 = generate_status_prompt(ctx2)
    assert "Browse engineering channel" in prompt2
    assert "(help)" in prompt2
    assert "OK" in prompt2

    # Test 3: Compact format
    compact = generate_compact_prompt(ctx2)
    assert len(compact) < len(prompt2)
    assert "goal=" in compact

    # Test 4: Error in history
    ctx3 = AgentContext(
        session_id="error-test",
        main_goal="Test error handling",
        current_subgoal="Try invalid command",
        recent_actions=[
            ActionRecord("(invalid-cmd)", "Error: unknown command", False),
        ],
    )

    prompt3 = generate_status_prompt(ctx3)
    assert "ERROR" in prompt3

    print("prompt.py: 4 tests passed")
    return True


if __name__ == "__main__":
    import sys
    success = _test_prompt()
    sys.exit(0 if success else 1)
