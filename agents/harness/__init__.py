"""
agents/harness — Agent harness for The Fold

The harness provides the interface between LLMs and The Fold REPL.
LLMs are stateless guests; the substrate (Fold) holds state.

Core components:
- parse.py: Extract S-expressions from LLM completions
- prompt.py: Generate status prompts from agent context
- step.py: Execute single atomic steps (prompt → completion → parse → eval)
"""

from .parse import parse_completion, ParseResult
from .prompt import generate_status_prompt, generate_compact_prompt, AgentContext, ActionRecord
from .step import step, StepResult, StepMetrics

__all__ = [
    # Parse
    "parse_completion",
    "ParseResult",

    # Prompt
    "generate_status_prompt",
    "generate_compact_prompt",
    "AgentContext",
    "ActionRecord",

    # Step
    "step",
    "StepResult",
    "StepMetrics",
]
