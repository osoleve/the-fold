"""
agents/harness/step.py — Atomic single step executor for agent harness

The step is the interface boundary between the probabilistic world of the LLM
and the deterministic world of the Fold runtime.

Core loop:
    Status Prompt → Completion API → Parse → Eval → Result

Design principles (from Gemini reviews):
- Retry on interface errors (API, parse), NOT on runtime errors
- Runtime errors are valid observations the agent must see
- Side-effect on session, pure on agent state
- Use JSON protocol for reliable error detection (no string parsing)
- Inject dependencies for testability
"""

import json
import os
import subprocess
import time
from dataclasses import dataclass, field
from typing import Callable, Literal, Optional, Protocol

from .parse import parse_completion, ParseResult


# --- Configuration ---

DEFAULT_FOLD_AGENT_PATH = "./fold-agent.py"
DEFAULT_FOLD_CWD = "/home/oso/the-fold"


def get_fold_agent_path() -> str:
    """Get the fold-agent.py path from env or default."""
    return os.environ.get("FOLD_AGENT_PATH", DEFAULT_FOLD_AGENT_PATH)


def get_fold_cwd() -> str:
    """Get the working directory for fold-agent.py."""
    return os.environ.get("FOLD_CWD", DEFAULT_FOLD_CWD)


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
class EvalResult:
    """Result of evaluating an expression in The Fold."""
    success: bool              # Did evaluation succeed?
    value: str                 # The result value (or error message)
    output: str                # Any output produced (prints, debug)
    session: str               # Session ID used
    is_infrastructure_error: bool = False  # True if error is infrastructure (timeout, crash), not runtime


@dataclass(frozen=True)
class StepResult:
    """
    Result of a single agent step.

    The status reflects INFRASTRUCTURE success, not agent success:
    - "success": We got a valid expression and evaluated it (even if eval errored)
    - "malformed_failure": Couldn't parse a valid expression after retries
    - "api_failure": Couldn't get a response from the LLM
    - "eval_failure": Infrastructure failure during evaluation (timeout, crash)
    """
    status: Literal["success", "malformed_failure", "api_failure", "eval_failure"]

    # Inputs
    prompt_snapshot: str       # The exact prompt sent

    # Outputs
    raw_completion: str        # Exact string from LLM
    parsed_action: str         # The S-expression we extracted (if any)
    parse_result: Optional[ParseResult]  # Full parse details

    # Execution outcomes (if we got to eval)
    eval: Optional[EvalResult]  # Structured eval result
    eval_success: bool          # Convenience: did Scheme eval succeed?

    # Telemetry
    metrics: StepMetrics

    # Error details
    error: Optional[str]       # Human-readable error if failed


# --- Evaluator Protocol ---

class Evaluator(Protocol):
    """Protocol for expression evaluators (enables mocking)."""
    def __call__(
        self,
        session_id: str,
        expression: str,
        timeout_ms: int
    ) -> EvalResult:
        ...


# --- Default Fold Evaluator ---

def fold_evaluator(
    session_id: str,
    expression: str,
    timeout_ms: int = 30000,
    agent_path: Optional[str] = None,
    cwd: Optional[str] = None
) -> EvalResult:
    """
    Evaluate an S-expression in The Fold using JSON protocol.

    Uses fold-agent.py --json for structured, reliable results.
    No string parsing heuristics.
    """
    agent_path = agent_path or get_fold_agent_path()
    cwd = cwd or get_fold_cwd()

    try:
        # Use JSON mode for structured communication
        input_json = json.dumps({"code": expression})

        result = subprocess.run(
            [agent_path, "--json", "--session", session_id],
            input=input_json,
            capture_output=True,
            text=True,
            timeout=timeout_ms / 1000,
            cwd=cwd
        )

        # Parse JSON response
        try:
            response = json.loads(result.stdout)
        except json.JSONDecodeError:
            # Fallback if JSON parsing fails
            return EvalResult(
                success=False,
                value=f"Invalid JSON response: {result.stdout[:200]}",
                output=result.stderr,
                session=session_id,
                is_infrastructure_error=True
            )

        return EvalResult(
            success=(response.get("status") == "success"),
            value=response.get("result", ""),
            output=response.get("output", ""),
            session=response.get("session", session_id)
        )

    except subprocess.TimeoutExpired:
        return EvalResult(
            success=False,
            value=f"Evaluation timed out after {timeout_ms}ms",
            output="",
            session=session_id,
            is_infrastructure_error=True
        )
    except FileNotFoundError:
        return EvalResult(
            success=False,
            value=f"Evaluator not found: {agent_path}",
            output="",
            session=session_id,
            is_infrastructure_error=True
        )
    except Exception as e:
        return EvalResult(
            success=False,
            value=f"Evaluation failed: {e}",
            output="",
            session=session_id,
            is_infrastructure_error=True
        )


# --- Mock Evaluator for Testing ---

def mock_evaluator(responses: dict[str, EvalResult]):
    """
    Create a mock evaluator that returns canned responses.

    Args:
        responses: Dict mapping expressions to EvalResults.
                   Use "*" as a catch-all key.
    """
    def evaluator(session_id: str, expression: str, timeout_ms: int) -> EvalResult:
        if expression in responses:
            return responses[expression]
        if "*" in responses:
            return responses["*"]
        return EvalResult(
            success=True,
            value=f"(mock result for {expression})",
            output="",
            session=session_id
        )
    return evaluator


# --- The Step ---

def step(
    session_id: str,
    status_prompt: str,
    api_client: Callable[[str], str],
    evaluator: Evaluator = fold_evaluator,
    retry_budget: int = 3,
    eval_timeout_ms: int = 30000,
    api_retry_budget: int = 2,
    api_retry_base_delay: float = 1.0
) -> StepResult:
    """
    Execute one agent step: prompt → completion → parse → eval.

    Args:
        session_id: The Fold session for state persistence
        status_prompt: The current environment/goal/actions prompt
        api_client: A callable that takes a prompt and returns a completion
        evaluator: A callable that evaluates expressions (injectable for testing)
        retry_budget: Number of parse retries before giving up
        eval_timeout_ms: Timeout for Fold evaluation
        api_retry_budget: Number of API retries for transient failures (default: 2)
        api_retry_base_delay: Base delay between API retries in seconds (default: 1.0)

    Returns:
        StepResult with full causal chain for debugging/training
    """
    total_start = time.monotonic()
    api_duration = 0
    parse_duration = 0
    eval_duration = 0
    retry_count = 0

    # --- Phase 1: Get completion from API (with retries for transient failures) ---
    api_start = time.monotonic()
    raw_completion = None
    last_api_error = None

    for api_attempt in range(api_retry_budget + 1):
        try:
            raw_completion = api_client(status_prompt)
            break  # Success
        except Exception as e:
            last_api_error = e
            if api_attempt < api_retry_budget:
                # Exponential backoff: 1s, 2s, 4s, ...
                delay = api_retry_base_delay * (2 ** api_attempt)
                time.sleep(delay)

    if raw_completion is None:
        api_duration = int((time.monotonic() - api_start) * 1000)
        return StepResult(
            status="api_failure",
            prompt_snapshot=status_prompt,
            raw_completion="",
            parsed_action="",
            parse_result=None,
            eval=None,
            eval_success=False,
            metrics=StepMetrics(
                api_duration_ms=api_duration,
                parse_duration_ms=0,
                eval_duration_ms=0,
                total_duration_ms=api_duration,
                retry_count=0
            ),
            error=f"API error after {api_retry_budget + 1} attempts: {last_api_error}"
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

        # TODO: Implement auto-fix loop where we ask LLM to fix syntax
        # For now, retries are no-ops (placeholder for future enhancement)

    if not parse_result or not parse_result.success:
        total_duration = int((time.monotonic() - total_start) * 1000)
        return StepResult(
            status="malformed_failure",
            prompt_snapshot=status_prompt,
            raw_completion=raw_completion,
            parsed_action=parse_result.expression if parse_result else "",
            parse_result=parse_result,
            eval=None,
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
    eval_result = evaluator(session_id, parsed_action, eval_timeout_ms)
    eval_duration = int((time.monotonic() - eval_start) * 1000)

    total_duration = int((time.monotonic() - total_start) * 1000)

    # Check for infrastructure failures (timeout, crash) vs runtime errors
    # Runtime errors (division by zero, etc.) are still "success" - valid observation
    # The evaluator explicitly marks infrastructure errors via is_infrastructure_error flag.
    if eval_result.is_infrastructure_error:
        return StepResult(
            status="eval_failure",
            prompt_snapshot=status_prompt,
            raw_completion=raw_completion,
            parsed_action=parsed_action,
            parse_result=parse_result,
            eval=eval_result,
            eval_success=False,
            metrics=StepMetrics(
                api_duration_ms=api_duration,
                parse_duration_ms=parse_duration,
                eval_duration_ms=eval_duration,
                total_duration_ms=total_duration,
                retry_count=retry_count
            ),
            error=f"Evaluation infrastructure failure: {eval_result.value}"
        )

    # Success! (Even if the Scheme code itself errored - that's a valid observation)
    return StepResult(
        status="success",
        prompt_snapshot=status_prompt,
        raw_completion=raw_completion,
        parsed_action=parsed_action,
        parse_result=parse_result,
        eval=eval_result,
        eval_success=eval_result.success,
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

    # Test 1: Successful step with mock evaluator (pure unit test)
    mock_eval = mock_evaluator({
        "(+ 1 2)": EvalResult(success=True, value="3", output="", session="test")
    })
    mock_api = mock_api_client(["(+ 1 2)"])
    result = step(
        session_id="test-step-1",
        status_prompt="Test prompt",
        api_client=mock_api,
        evaluator=mock_eval
    )
    assert result.status == "success", f"Expected success, got {result.status}: {result.error}"
    assert result.parsed_action == "(+ 1 2)"
    assert result.eval.value == "3"
    assert result.eval_success
    print("  Test 1: Simple expression (mock) - PASS")

    # Test 2: Code fence wrapped expression
    mock_eval2 = mock_evaluator({"*": EvalResult(success=True, value="ok", output="", session="test")})
    mock_api2 = mock_api_client(["```scheme\n(help)\n```"])
    result2 = step(
        session_id="test-step-2",
        status_prompt="Test prompt 2",
        api_client=mock_api2,
        evaluator=mock_eval2
    )
    assert result2.status == "success", f"Expected success, got {result2.status}: {result2.error}"
    assert result2.parsed_action == "(help)"
    print("  Test 2: Code fence (mock) - PASS")

    # Test 3: API failure
    def failing_client(prompt: str) -> str:
        raise Exception("Network error")

    result3 = step(
        session_id="test-step-3",
        status_prompt="Test prompt 3",
        api_client=failing_client,
        evaluator=mock_eval,
        api_retry_budget=0  # Disable retries for fast test
    )
    assert result3.status == "api_failure"
    assert "Network error" in result3.error
    print("  Test 3: API failure - PASS")

    # Test 4: Malformed response
    mock_api4 = mock_api_client(["This is not valid Scheme at all ((("])
    result4 = step(
        session_id="test-step-4",
        status_prompt="Test prompt 4",
        api_client=mock_api4,
        evaluator=mock_eval
    )
    assert result4.status == "malformed_failure"
    print("  Test 4: Malformed response - PASS")

    # Test 5: Runtime error (should still be "success" infrastructure-wise)
    mock_eval5 = mock_evaluator({
        "(/ 1 0)": EvalResult(success=False, value="Error: division by zero", output="", session="test")
    })
    mock_api5 = mock_api_client(["(/ 1 0)"])
    result5 = step(
        session_id="test-step-5",
        status_prompt="Test prompt 5",
        api_client=mock_api5,
        evaluator=mock_eval5
    )
    assert result5.status == "success", f"Expected success (infra), got {result5.status}"
    assert not result5.eval_success  # The Scheme eval failed, but infrastructure succeeded
    print("  Test 5: Runtime error - PASS")

    # Test 6: Infrastructure failure (timeout)
    mock_eval6 = mock_evaluator({
        "*": EvalResult(success=False, value="Evaluation timed out after 5000ms", output="", session="test", is_infrastructure_error=True)
    })
    mock_api6 = mock_api_client(["(infinite-loop)"])
    result6 = step(
        session_id="test-step-6",
        status_prompt="Test prompt 6",
        api_client=mock_api6,
        evaluator=mock_eval6
    )
    assert result6.status == "eval_failure"
    print("  Test 6: Infrastructure failure (timeout) - PASS")

    # Test 7: Integration test with real fold-agent.py
    mock_api7 = mock_api_client(["(+ 2 3)"])
    result7 = step(
        session_id="test-step-7",
        status_prompt="Test prompt 7",
        api_client=mock_api7
        # Using default fold_evaluator
    )
    assert result7.status == "success", f"Expected success, got {result7.status}: {result7.error}"
    assert "5" in result7.eval.value
    print("  Test 7: Integration with fold-agent.py - PASS")

    # Test 8: Metrics are populated
    assert result7.metrics.total_duration_ms > 0
    assert result7.metrics.api_duration_ms >= 0
    print("  Test 8: Metrics populated - PASS")

    # Test 9: False positive prevention - runtime error with "timed out" text
    # Should NOT be classified as infrastructure failure
    mock_eval9 = mock_evaluator({
        "*": EvalResult(success=False, value="Error: Request timed out (user program)", output="", session="test")
    })
    mock_api9 = mock_api_client(["(some-network-call)"])
    result9 = step(
        session_id="test-step-9",
        status_prompt="Test prompt 9",
        api_client=mock_api9,
        evaluator=mock_eval9
    )
    # This should be "success" (infrastructure worked) even though Scheme errored
    assert result9.status == "success", f"Expected success, got {result9.status}"
    assert not result9.eval_success  # The Scheme eval failed
    print("  Test 9: False positive prevention - PASS")

    print("\nstep.py: 9 tests passed")
    return True


if __name__ == "__main__":
    import sys
    success = _test_step()
    sys.exit(0 if success else 1)
