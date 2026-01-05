"""
agents/harness/loop.py — The main execution loop for autonomous agents.

This module implements the "Agency" - the control flow that drives an agent
towards a goal. It orchestrates:
1. Context management (history, subgoals)
2. Step execution (prompt -> step -> result)
3. Meta-command handling (intercepting harness-level actions)
4. Loop detection and safety limits

Design (from Gemini Pro review):
- MetaEvaluator wraps real evaluator to intercept harness commands
- Generator pattern for step-by-step observability
- State hash for loop detection
"""

import hashlib
import re
from dataclasses import dataclass, field
from typing import Callable, Generator, Optional, Set

from .parse import parse_completion
from .prompt import ActionRecord, AgentContext, WorkspaceEntry, generate_status_prompt
from .step import Evaluator, EvalResult, StepResult, fold_evaluator, step


# --- Configuration ---

@dataclass
class LoopConfig:
    """Configuration for the agent loop."""
    max_steps: int = 20           # Hard limit on steps
    stop_on_done: bool = True     # Stop if agent says (done)
    stop_on_failure: bool = False # Stop on first eval/api failure
    detect_loops: bool = True     # Stop if state repeats
    loop_window: int = 3          # Number of recent actions to include in state hash
    retry_budget: int = 3         # Retries for API/Parsing per step
    eval_timeout_ms: int = 30000  # Timeout per evaluation


@dataclass
class LoopResult:
    """Final result of a loop run."""
    finish_reason: str            # done, max_steps, loop_detected, failure, interrupted
    steps_completed: int
    total_duration_ms: int
    final_context: AgentContext   # Context at end of loop
    final_eval: Optional[EvalResult]  # Last evaluation result


# --- Meta-Command Handling ---

# Patterns for meta-commands (harness-level, not sent to Fold)
# Use (?:[^"\\]|\\.)* to handle escaped quotes like \"
_SUBGOAL_PATTERN = re.compile(r'^\(subgoal\s+"((?:[^"\\]|\\.)*)"\s*\)$')
_DONE_PATTERN = re.compile(r'^\((done|task-complete)\s*\)$')
_THINK_PATTERN = re.compile(r'^\(think\s+"((?:[^"\\]|\\.)*)"\s*\)$')
_NOTE_PATTERN = re.compile(r'^\(note\s+"((?:[^"\\]|\\.)*)"\s*\)$')
_ENV_PATTERN = re.compile(r'^\(env\s+(.+)\)$', re.DOTALL)  # (env sexpr) - capture inner
_REGISTER_PATTERN = re.compile(r'^\(set!\s+(\*[a-zA-Z_][a-zA-Z0-9_-]*\*)\s+')  # (set! *name* ...)


def _unescape_string(s: str) -> str:
    """Unescape common escape sequences in a captured string."""
    return s.replace('\\"', '"').replace('\\n', '\n').replace('\\\\', '\\')


@dataclass
class MetaCommand:
    """A parsed meta-command."""
    kind: str  # "subgoal", "done", "think", "note", "env", "register", "none"
    payload: Optional[str] = None
    register_name: Optional[str] = None  # For register commands


def parse_meta_command(expression: str) -> MetaCommand:
    """
    Check if expression is a meta-command (handled specially by harness).

    Meta-commands:
    - (subgoal "description") - Update current subgoal
    - (done) or (task-complete) - Complete current subgoal (clear it)
    - (think "reasoning") - Internal reasoning (logged, not shown to agent)
    - (note "text") - Working memory note (shown to agent in future turns)
    - (env sexpr) - Explore Fold, results go to workspace (no set!/define allowed)
    - (set! *register* ...) - Set a register; *answer* ends the task
    """
    expr = expression.strip()

    # Check for env (exploration command)
    match = _ENV_PATTERN.match(expr)
    if match:
        inner = match.group(1).strip()
        # Reject set!/define inside env
        if inner.startswith('(set!') or inner.startswith('(define'):
            return MetaCommand(kind="env_error", payload="set!/define not allowed in (env ...)")
        return MetaCommand(kind="env", payload=inner)

    # Check for register set (set! *name* ...)
    match = _REGISTER_PATTERN.match(expr)
    if match:
        register_name = match.group(1)
        return MetaCommand(kind="register", payload=expr, register_name=register_name)

    # Check for subgoal
    match = _SUBGOAL_PATTERN.match(expr)
    if match:
        return MetaCommand(kind="subgoal", payload=_unescape_string(match.group(1)))

    # Check for done (completes subgoal, not task)
    if _DONE_PATTERN.match(expr):
        return MetaCommand(kind="done")

    # Check for think
    match = _THINK_PATTERN.match(expr)
    if match:
        return MetaCommand(kind="think", payload=_unescape_string(match.group(1)))

    # Check for note
    match = _NOTE_PATTERN.match(expr)
    if match:
        return MetaCommand(kind="note", payload=_unescape_string(match.group(1)))

    return MetaCommand(kind="none")


class MetaEvaluator:
    """
    Wraps a real Evaluator to intercept meta-commands.

    Allows the agent to control the harness (e.g., changing subgoals)
    without those functions needing to exist in the Fold runtime.
    """

    def __init__(self, real_evaluator: Evaluator, context: AgentContext):
        self.real_evaluator = real_evaluator
        self.context = context
        self.task_complete = False
        self.thoughts: list[str] = []

    def __call__(self, session_id: str, expression: str, timeout_ms: int) -> EvalResult:
        meta = parse_meta_command(expression)

        if meta.kind == "env":
            # Exploration command - evaluate in Fold, record in workspace
            result = self.real_evaluator(session_id, meta.payload, timeout_ms)
            # Add to workspace - use output if value is empty (display-only commands)
            workspace_result = result.value if result.value else result.output
            if not result.success:
                workspace_result = result.output
            entry = WorkspaceEntry(
                command=meta.payload,
                result=workspace_result,
                success=result.success
            )
            self.context.workspace.append(entry)
            # Trim workspace if too long
            max_entries = self.context.max_workspace_entries
            if len(self.context.workspace) > max_entries:
                self.context.workspace = self.context.workspace[-max_entries:]
            return result

        if meta.kind == "env_error":
            # Rejected env command (tried to use set!/define)
            return EvalResult(
                success=False,
                value="",
                output=f"[harness] Error: {meta.payload}",
                session=session_id
            )

        if meta.kind == "register":
            # Set a register - evaluate in Fold, update registers dict
            result = self.real_evaluator(session_id, meta.payload, timeout_ms)
            if result.success:
                # Extract the value that was set (from result or re-query)
                self.context.registers[meta.register_name] = result.value
                # *answer* ends the task
                if meta.register_name == "*answer*":
                    self.task_complete = True
                    return EvalResult(
                        success=True,
                        value=result.value,
                        output="[harness] Answer set, task complete",
                        session=session_id
                    )
                return EvalResult(
                    success=True,
                    value=result.value,
                    output=f"[harness] Register {meta.register_name} set",
                    session=session_id
                )
            return result

        if meta.kind == "subgoal":
            self.context.current_subgoal = meta.payload
            return EvalResult(
                success=True,
                value=f"Subgoal set: {meta.payload}",
                output="[harness] Subgoal updated",
                session=session_id
            )

        if meta.kind == "done":
            # (done) completes the current subgoal, not the task
            completed_subgoal = self.context.current_subgoal
            self.context.current_subgoal = None
            return EvalResult(
                success=True,
                value=f"Subgoal complete: {completed_subgoal}",
                output="[harness] Subgoal completed, ready for next",
                session=session_id
            )

        if meta.kind == "think":
            self.thoughts.append(meta.payload)
            return EvalResult(
                success=True,
                value=f"Thought recorded: {meta.payload}",
                output="[harness] Internal reasoning logged",
                session=session_id
            )

        if meta.kind == "note":
            self.context.working_notes.append(meta.payload)
            return EvalResult(
                success=True,
                value=f"Note saved: {meta.payload}",
                output="[harness] Working note added",
                session=session_id
            )

        # Not a meta-command, delegate to real evaluator
        return self.real_evaluator(session_id, expression, timeout_ms)


# --- Loop Detection ---

def compute_state_hash(context: AgentContext, window: int = 3) -> str:
    """
    Compute a hash of the current agent state for loop detection.

    State includes:
    - Current subgoal
    - Last N actions (expression + success)

    Note: Results are intentionally excluded from the hash. Including results
    would cause false negatives (agent repeatedly calling same command with
    slightly different results). The current approach may cause false positives
    for legitimate polling patterns, but these are less harmful than missing
    infinite loops.

    Repeated state hash indicates the agent is stuck in a loop.
    """
    components = [context.current_subgoal or "None"]

    for action in context.recent_actions[-window:]:
        components.append(action.expression)
        components.append("ok" if action.success else "err")

    data = "|".join(components).encode("utf-8")
    return hashlib.md5(data).hexdigest()[:12]  # Short hash is enough


# --- The Loop ---

def run_loop(
    context: AgentContext,
    api_client: Callable[[str], str],
    evaluator: Evaluator = fold_evaluator,
    config: LoopConfig = None,
    on_step: Optional[Callable[[int, StepResult], None]] = None
) -> Generator[StepResult, None, LoopResult]:
    """
    Run the agent loop until goal completion or limit reached.

    This is a generator that yields each StepResult. The final LoopResult
    is returned when the generator completes (access via StopIteration.value).

    Args:
        context: Agent context (will be mutated with action history)
        api_client: LLM completion callable
        evaluator: Expression evaluator (default: fold_evaluator)
        config: Loop configuration
        on_step: Optional callback after each step (for logging)

    Yields:
        StepResult for each step

    Returns:
        LoopResult with final status

    Usage:
        gen = run_loop(context, api_client)
        for result in gen:
            print(f"Step: {result.parsed_action}")
        # Access final result:
        # final = gen.value  (only after iteration complete)
    """
    config = config or LoopConfig()
    state_hashes: Set[str] = set()
    total_duration_ms = 0
    steps_completed = 0
    last_eval: Optional[EvalResult] = None
    finish_reason = "running"

    # Wrap evaluator to handle meta-commands
    meta_eval = MetaEvaluator(evaluator, context)

    while steps_completed < config.max_steps:
        # --- Loop detection ---
        if config.detect_loops:
            state_hash = compute_state_hash(context, config.loop_window)
            if state_hash in state_hashes:
                finish_reason = "loop_detected"
                break
            state_hashes.add(state_hash)

        # --- Generate prompt ---
        prompt = generate_status_prompt(context)

        # --- Execute step ---
        result = step(
            session_id=context.session_id,
            status_prompt=prompt,
            api_client=api_client,
            evaluator=meta_eval,
            retry_budget=config.retry_budget,
            eval_timeout_ms=config.eval_timeout_ms
        )

        steps_completed += 1
        total_duration_ms += result.metrics.total_duration_ms
        last_eval = result.eval

        # --- Update context with action history ---
        if result.parsed_action:
            # Combine value and output for full result visibility
            # Many Fold commands print to stdout (captured in output) rather than returning
            if result.eval:
                # Prefer output if value is empty (common for display commands)
                if result.eval.value:
                    action_result = result.eval.value
                elif result.eval.output:
                    # Strip debug lines from output
                    output_lines = result.eval.output.split('\n')
                    clean_lines = [l for l in output_lines if not l.startswith('DEBUG:')]
                    action_result = '\n'.join(clean_lines).strip()
                else:
                    action_result = "(no output)"
            else:
                action_result = result.error or "unknown"

            record = ActionRecord(
                expression=result.parsed_action,
                result=action_result[:500],  # Truncate for context window
                success=result.eval_success
            )
            context.recent_actions.append(record)

        # --- Callback ---
        if on_step:
            on_step(steps_completed, result)

        # --- Yield result ---
        yield result

        # --- Check stop conditions ---
        if meta_eval.task_complete and config.stop_on_done:
            finish_reason = "done"
            break

        if config.stop_on_failure and result.status != "success":
            finish_reason = f"failure:{result.status}"
            break

        # Infrastructure failures are always fatal
        if result.status == "eval_failure":
            finish_reason = "infrastructure_failure"
            break

    # If we exited the while loop without breaking
    if finish_reason == "running":
        finish_reason = "max_steps"

    return LoopResult(
        finish_reason=finish_reason,
        steps_completed=steps_completed,
        total_duration_ms=total_duration_ms,
        final_context=context,
        final_eval=last_eval
    )


# --- Convenience wrapper ---

def run_loop_to_completion(
    context: AgentContext,
    api_client: Callable[[str], str],
    evaluator: Evaluator = fold_evaluator,
    config: LoopConfig = None,
    verbose: bool = False
) -> LoopResult:
    """
    Run the agent loop and return the final result.

    Convenience wrapper that consumes the generator and returns LoopResult.
    """
    config = config or LoopConfig()
    step_count = 0

    def log_step(n: int, result: StepResult):
        if verbose:
            status = "+" if result.eval_success else "-"
            print(f"  [{n}] {status} {result.parsed_action[:50]}")

    gen = run_loop(context, api_client, evaluator, config, on_step=log_step if verbose else None)

    # Manually consume generator to capture the return value
    try:
        while True:
            next(gen)
            step_count += 1
    except StopIteration as e:
        return e.value

    # Fallback (shouldn't reach here - generator always returns LoopResult)
    return LoopResult(
        finish_reason="unknown",
        steps_completed=step_count,
        total_duration_ms=0,
        final_context=context,
        final_eval=None
    )


# --- Tests ---

def _test_loop():
    """Self-tests for loop module."""
    from .step import EvalResult, mock_api_client, mock_evaluator

    print("Testing loop module...")

    # Test 1: Basic loop with answer signal
    print("  Test 1: Basic loop with answer signal")
    ctx = AgentContext(
        session_id="test-loop-1",
        main_goal="Count to 2",
        current_subgoal=None
    )

    api_responses = [
        '(subgoal "Start counting")',
        "(+ 1 0)",
        "(+ 1 1)",
        '(set! *answer* "2")'  # Answer triggers completion
    ]
    api = mock_api_client(api_responses)

    eval_map = {
        "(+ 1 0)": EvalResult(True, "1", "", "s"),
        "(+ 1 1)": EvalResult(True, "2", "", "s"),
        '(set! *answer* "2")': EvalResult(True, "#<void>", "", "s"),
    }
    base_eval = mock_evaluator(eval_map)

    gen = run_loop(ctx, api, base_eval)
    results = []
    loop_result = None
    try:
        for r in gen:
            results.append(r)
    except StopIteration as e:
        loop_result = e.value

    # Generator return value is tricky - let's check what we got
    assert len(results) == 4, f"Expected 4 steps, got {len(results)}"
    assert ctx.current_subgoal == "Start counting", f"Subgoal not updated: {ctx.current_subgoal}"
    print("    PASS")

    # Test 2: Loop detection
    print("  Test 2: Loop detection")
    ctx2 = AgentContext(
        session_id="test-loop-2",
        main_goal="Get stuck",
        current_subgoal="Repeat"
    )

    # Same response every time -> should detect loop
    api2 = mock_api_client(["(help)"] * 10)
    eval2 = mock_evaluator({"*": EvalResult(True, "ok", "", "s")})

    gen2 = run_loop(ctx2, api2, eval2, LoopConfig(max_steps=10, detect_loops=True))
    results2 = list(gen2)

    # Should stop before 10 due to loop detection
    # After first (help), state is recorded. Second (help) with same subgoal = loop
    assert len(results2) < 10, f"Loop detection failed, ran {len(results2)} steps"
    print(f"    PASS (stopped at step {len(results2)})")

    # Test 3: Meta-command parsing
    print("  Test 3: Meta-command parsing")
    assert parse_meta_command('(subgoal "test goal")').kind == "subgoal"
    assert parse_meta_command('(subgoal "test goal")').payload == "test goal"
    assert parse_meta_command("(done)").kind == "done"
    assert parse_meta_command("(task-complete)").kind == "done"
    assert parse_meta_command('(think "I should try X")').kind == "think"
    assert parse_meta_command('(note "Remember this")').kind == "note"
    assert parse_meta_command('(note "Remember this")').payload == "Remember this"
    assert parse_meta_command("(+ 1 2)").kind == "none"
    # Test register pattern (any *earmuffed* var)
    assert parse_meta_command('(set! *answer* "42")').kind == "register"
    assert parse_meta_command('(set! *answer* "42")').register_name == "*answer*"
    assert parse_meta_command('(set! *scratch* (+ 1 2))').kind == "register"
    assert parse_meta_command('(set! *scratch* (+ 1 2))').register_name == "*scratch*"
    assert parse_meta_command('(set! other "x")').kind == "none"  # Non-earmuffed not special
    # Test env pattern
    assert parse_meta_command('(env (help))').kind == "env"
    assert parse_meta_command('(env (help))').payload == "(help)"
    assert parse_meta_command('(env (browse \'art 5))').kind == "env"
    assert parse_meta_command('(env (set! x 1))').kind == "env_error"  # No set! in env
    assert parse_meta_command('(env (define y 2))').kind == "env_error"  # No define in env
    # Test escaped quotes
    assert parse_meta_command('(subgoal "goal with \\"quotes\\"")').kind == "subgoal"
    assert parse_meta_command('(subgoal "goal with \\"quotes\\"")').payload == 'goal with "quotes"'
    assert parse_meta_command('(think "I said \\"hello\\"")').payload == 'I said "hello"'
    assert parse_meta_command('(note "Value is \\"42\\"")').payload == 'Value is "42"'
    print("    PASS")

    # Test 4: Max steps limit
    print("  Test 4: Max steps limit")
    ctx4 = AgentContext(
        session_id="test-loop-4",
        main_goal="Run forever",
        current_subgoal="Step N"
    )

    # Different responses each time to avoid loop detection
    api4 = mock_api_client([f"(step-{i})" for i in range(20)])
    eval4 = mock_evaluator({"*": EvalResult(True, "ok", "", "s")})

    gen4 = run_loop(ctx4, api4, eval4, LoopConfig(max_steps=5, detect_loops=False))
    results4 = list(gen4)

    assert len(results4) == 5, f"Expected 5 steps, got {len(results4)}"
    print("    PASS")

    print("\nloop.py: 4 tests passed")
    return True


if __name__ == "__main__":
    import sys
    success = _test_loop()
    sys.exit(0 if success else 1)
