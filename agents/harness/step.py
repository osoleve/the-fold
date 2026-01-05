"""
agents/harness/step.py — Atomic single step executor for agent harness

The step is the interface boundary between the probabilistic world of the LLM
and the deterministic world of the Fold runtime.

Core loop:
    Status Prompt → Completion API → Parse → Eval → Result

Design principles (from Gemini review):
- Retry on interface errors (API, parse), NOT on runtime errors
- Runtime errors are valid observations the agent must see
- Side-effect on session, pure on agent state
"""

import subprocess
import time
from dataclasses import dataclass
from typing import Callable, Literal, Optional

from .parse import parse_completion, ParseResult


# --- Result Types ---

@dataclass(frozen=True)
class StepMetrics:
    """Telemetry for a step."""
    api_duration_ms: int       # Time spent waiting for LLM
    parse_duration_ms: int     # Time spent parsing
    eval_duration_ms: int      # Time spent evaluating in Fold
    total_duration_ms: int     # Total step time
    retry_count: int           # Number of parse retries needed
    tokens_in: Optional[int] = None   # If API provides
    tokens_out: Optional[int] = None  # If API provides


@dataclass(frozen=True)
class StepResult:
    """
    Result of a single agent step.

    The status reflects INFRASTRUCTURE success, not agent success:
    - "success": We got a valid expression and evaluated it (even if eval errored)
    - "malformed_failure": Couldn't parse a valid expression after retries
    - "api_failure": Couldn't get a response from the LLM
    """
    status: Literal["success", "malformed_failure", "api_failure"]

    # Inputs
    prompt_snapshot: str       # The exact prompt sent

    # Outputs
    raw_completion: str        # Exact string from LLM
    parsed_action: str         # The S-expression we extracted (if any)
    parse_result: Optional[ParseResult]  # Full parse details

    # Execution outcomes (if we got to eval)
    stdout: str                # Captured stdout from Fold
    stderr: str                # Captured stderr from Fold
    eval_result: str           # Final value or error string
    eval_success: bool         # Did the Scheme eval succeed?

    # Telemetry
    metrics: StepMetrics

    # Error details
    error: Optional[str]       # Human-readable error if failed


# --- Fold Evaluation ---

def eval_in_fold(
    session_id: str,
    expression: str,
    timeout_ms: int = 30000
) -> tuple[str, str, str, bool]:
    """
    Evaluate an S-expression in The Fold.

    Returns: (stdout, stderr, result, success)
    """
    try:
        result = subprocess.run(
            ["./fold-agent.py", "--session", session_id, expression],
            capture_output=True,
            text=True,
            timeout=timeout_ms / 1000,
            cwd="/home/oso/the-fold"
        )

        stdout = result.stdout
        stderr = result.stderr

        # Check for error markers
        if "Error:" in stdout or result.returncode != 0:
            return (stdout, stderr, stdout.strip(), False)

        return (stdout, stderr, stdout.strip(), True)

    except subprocess.TimeoutExpired:
        return ("", "", "Evaluation timed out", False)
    except Exception as e:
        return ("", "", f"Evaluation failed: {e}", False)


# --- The Step ---

def step(
    session_id: str,
    status_prompt: str,
    api_client: Callable[[str], str],
    retry_budget: int = 3,
    eval_timeout_ms: int = 30000
) -> StepResult:
    """
    Execute one agent step: prompt → completion → parse → eval.

    Args:
        session_id: The Fold session for state persistence
        status_prompt: The current environment/goal/actions prompt
        api_client: A callable that takes a prompt and returns a completion
        retry_budget: Number of parse retries before giving up
        eval_timeout_ms: Timeout for Fold evaluation

    Returns:
        StepResult with full causal chain for debugging/training
    """
    total_start = time.monotonic()
    api_duration = 0
    parse_duration = 0
    eval_duration = 0
    retry_count = 0

    # --- Phase 1: Get completion from API ---
    api_start = time.monotonic()
    try:
        raw_completion = api_client(status_prompt)
    except Exception as e:
        api_duration = int((time.monotonic() - api_start) * 1000)
        return StepResult(
            status="api_failure",
            prompt_snapshot=status_prompt,
            raw_completion="",
            parsed_action="",
            parse_result=None,
            stdout="",
            stderr="",
            eval_result="",
            eval_success=False,
            metrics=StepMetrics(
                api_duration_ms=api_duration,
                parse_duration_ms=0,
                eval_duration_ms=0,
                total_duration_ms=api_duration,
                retry_count=0
            ),
            error=f"API error: {e}"
        )
    api_duration = int((time.monotonic() - api_start) * 1000)

    # --- Phase 2: Parse the completion (with retries) ---
    current_completion = raw_completion
    parse_result = None

    for attempt in range(retry_budget):
        parse_start = time.monotonic()
        parse_result = parse_completion(current_completion)
        parse_duration += int((time.monotonic() - parse_start) * 1000)

        if parse_result.success:
            break

        retry_count = attempt + 1

        # If we have retries left, we could feed the error back to the LLM
        # For now, we just try parsing the same thing again (placeholder)
        # TODO: Implement auto-fix loop where we ask LLM to fix syntax

        if attempt < retry_budget - 1:
            # Last attempt, no more retries
            pass

    if not parse_result or not parse_result.success:
        total_duration = int((time.monotonic() - total_start) * 1000)
        return StepResult(
            status="malformed_failure",
            prompt_snapshot=status_prompt,
            raw_completion=raw_completion,
            parsed_action=parse_result.expression if parse_result else "",
            parse_result=parse_result,
            stdout="",
            stderr="",
            eval_result="",
            eval_success=False,
            metrics=StepMetrics(
                api_duration_ms=api_duration,
                parse_duration_ms=parse_duration,
                eval_duration_ms=0,
                total_duration_ms=total_duration,
                retry_count=retry_count
            ),
            error=f"Parse error: {parse_result.error if parse_result else 'Unknown'}"
        )

    parsed_action = parse_result.expression

    # --- Phase 3: Evaluate in Fold ---
    eval_start = time.monotonic()
    stdout, stderr, eval_result, eval_success = eval_in_fold(
        session_id,
        parsed_action,
        eval_timeout_ms
    )
    eval_duration = int((time.monotonic() - eval_start) * 1000)

    total_duration = int((time.monotonic() - total_start) * 1000)

    # Note: Even if eval failed, this is a "success" from infrastructure perspective
    # The agent successfully produced a valid expression; it just happened to error
    return StepResult(
        status="success",
        prompt_snapshot=status_prompt,
        raw_completion=raw_completion,
        parsed_action=parsed_action,
        parse_result=parse_result,
        stdout=stdout,
        stderr=stderr,
        eval_result=eval_result,
        eval_success=eval_success,
        metrics=StepMetrics(
            api_duration_ms=api_duration,
            parse_duration_ms=parse_duration,
            eval_duration_ms=eval_duration,
            total_duration_ms=total_duration,
            retry_count=retry_count
        ),
        error=None
    )


# --- Mock API Client for Testing ---

def mock_api_client(responses: list[str]):
    """Create a mock API client that returns canned responses."""
    idx = [0]

    def client(prompt: str) -> str:
        if idx[0] >= len(responses):
            raise Exception("No more mock responses")
        response = responses[idx[0]]
        idx[0] += 1
        return response

    return client


# --- Tests ---

def _test_step():
    """Self-test for step module."""
    import os
    os.chdir("/home/oso/the-fold")

    # Test 1: Successful step with simple expression
    mock = mock_api_client(["(+ 1 2)"])
    result = step(
        session_id="test-step-1",
        status_prompt="Test prompt",
        api_client=mock
    )
    assert result.status == "success", f"Expected success, got {result.status}: {result.error}"
    assert result.parsed_action == "(+ 1 2)"
    assert "3" in result.eval_result or result.eval_success
    print("  Test 1: Simple expression - PASS")

    # Test 2: Code fence wrapped expression
    mock2 = mock_api_client(["```scheme\n(help)\n```"])
    result2 = step(
        session_id="test-step-2",
        status_prompt="Test prompt 2",
        api_client=mock2
    )
    assert result2.status == "success", f"Expected success, got {result2.status}: {result2.error}"
    assert result2.parsed_action == "(help)"
    print("  Test 2: Code fence - PASS")

    # Test 3: API failure
    def failing_client(prompt: str) -> str:
        raise Exception("Network error")

    result3 = step(
        session_id="test-step-3",
        status_prompt="Test prompt 3",
        api_client=failing_client
    )
    assert result3.status == "api_failure"
    assert "Network error" in result3.error
    print("  Test 3: API failure - PASS")

    # Test 4: Malformed response
    mock4 = mock_api_client(["This is not valid Scheme at all ((("])
    result4 = step(
        session_id="test-step-4",
        status_prompt="Test prompt 4",
        api_client=mock4
    )
    assert result4.status == "malformed_failure"
    print("  Test 4: Malformed response - PASS")

    # Test 5: Runtime error (should still be "success" infrastructure-wise)
    mock5 = mock_api_client(["(/ 1 0)"])
    result5 = step(
        session_id="test-step-5",
        status_prompt="Test prompt 5",
        api_client=mock5
    )
    # Infrastructure succeeded (we parsed and evaluated), but eval might have errored
    assert result5.status == "success", f"Expected success (infra), got {result5.status}"
    # The eval itself might have failed (division by zero)
    print(f"  Test 5: Runtime error - PASS (eval_success={result5.eval_success})")

    # Test 6: Metrics are populated
    assert result.metrics.total_duration_ms > 0
    assert result.metrics.api_duration_ms >= 0
    print("  Test 6: Metrics populated - PASS")

    print("\nstep.py: 6 tests passed")
    return True


if __name__ == "__main__":
    import sys
    success = _test_step()
    sys.exit(0 if success else 1)
