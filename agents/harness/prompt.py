"""
agents/harness/prompt.py — Status prompt generator for agent harness

Generates the bounded context prompt that gets fed to the LLM for each step.
The prompt includes:
- Environment state (session info, available commands)
- Main goal
- Current subgoal (or None - cue to decompose)
- Recent action history
- Available tools with examples
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
class AgentContext:
    """The full context for generating a status prompt."""
    session_id: str
    main_goal: str
    current_subgoal: Optional[str] = None
    recent_actions: list[ActionRecord] = field(default_factory=list)
    available_commands: list[str] = field(default_factory=list)
    environment_notes: list[str] = field(default_factory=list)
    max_recent_actions: int = 3


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
        env_notes = "\nNOTES:\n" + "\n".join(f"  - {note}" for note in context.environment_notes)

    prompt = f"""SESSION: {context.session_id}

{goal_section}

RECENT ACTIONS:
{history_section}

AVAILABLE COMMANDS:
{commands_section}
{env_notes}
---
Respond with a single S-expression to evaluate. Use code fences if helpful.
If CURRENT SUBGOAL is None, first decide what subgoal to pursue.
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
