"""
agents/harness/observer.py — Human observability for agent runs

Provides real-time visibility into agent execution:
- Terminal output with clear formatting
- Transcript files for post-hoc review
- Summary statistics

Usage:
    from agents.harness.observer import Observer

    observer = Observer(save_transcript=True)

    gen = run_loop(context, api_client, on_step=observer.on_step)
    for result in gen:
        pass

    observer.print_summary()
"""

import json
import os
from dataclasses import dataclass, field
from datetime import datetime
from typing import Optional

from .prompt import AgentContext
from .step import StepResult


@dataclass
class StepRecord:
    """Record of a single step for transcript."""
    step_num: int
    timestamp: str
    action: str
    result: str
    success: bool
    subgoal: Optional[str]
    duration_ms: int
    raw_completion: str  # Full LLM output for debugging


@dataclass
class Observer:
    """
    Observes agent execution and provides human-readable output.

    Attach to run_loop via the on_step callback.
    """
    # Configuration
    verbose: bool = True           # Print to terminal
    save_transcript: bool = False  # Save to file
    transcript_dir: str = "transcripts"
    show_raw: bool = False         # Show raw LLM completions
    max_result_lines: int = 10     # Truncate long results

    # State (set during observation)
    context: Optional[AgentContext] = None
    session_id: str = ""
    start_time: Optional[datetime] = None
    records: list = field(default_factory=list)

    def begin(self, context: AgentContext):
        """Called before the loop starts."""
        self.context = context
        self.session_id = context.session_id
        self.start_time = datetime.now()
        self.records = []

        if self.verbose:
            print()
            print("=" * 70)
            print(f"  AGENT RUN: {self.session_id}")
            print("=" * 70)
            print(f"  Goal: {context.main_goal}")
            print(f"  Started: {self.start_time.strftime('%Y-%m-%d %H:%M:%S')}")
            print("=" * 70)
            print()

    def on_step(self, step_num: int, result: StepResult):
        """Callback for each step - attach to run_loop's on_step parameter."""
        timestamp = datetime.now().strftime('%H:%M:%S')

        # Get the action result from context (includes output fallback)
        action_result = ""
        if self.context and self.context.recent_actions:
            action_result = self.context.recent_actions[-1].result

        # Record for transcript
        record = StepRecord(
            step_num=step_num,
            timestamp=timestamp,
            action=result.parsed_action,
            result=action_result,
            success=result.eval_success,
            subgoal=self.context.current_subgoal if self.context else None,
            duration_ms=result.metrics.total_duration_ms,
            raw_completion=result.raw_completion
        )
        self.records.append(record)

        # Terminal output
        if self.verbose:
            self._print_step(record)

    def _print_step(self, record: StepRecord):
        """Print a step to terminal with formatting."""
        status = "✓" if record.success else "✗"

        print(f"[{record.timestamp}] Step {record.step_num} {status}")
        print(f"  Action: {record.action}")

        if record.subgoal:
            print(f"  Subgoal: {record.subgoal}")

        # Format result (truncate if needed)
        if record.result:
            lines = record.result.split('\n')
            if len(lines) > self.max_result_lines:
                display_lines = lines[:self.max_result_lines]
                display_lines.append(f"  ... ({len(lines) - self.max_result_lines} more lines)")
            else:
                display_lines = lines

            print(f"  Result:")
            for line in display_lines:
                print(f"    {line}")

        if self.show_raw:
            print(f"  Raw LLM: {record.raw_completion[:200]}...")

        print(f"  Time: {record.duration_ms}ms")
        print()

    def end(self, finish_reason: str):
        """Called after the loop ends."""
        end_time = datetime.now()
        duration = (end_time - self.start_time).total_seconds() if self.start_time else 0

        if self.verbose:
            print("=" * 70)
            print("  RUN COMPLETE")
            print("=" * 70)
            print(f"  Finish reason: {finish_reason}")
            print(f"  Steps: {len(self.records)}")
            print(f"  Duration: {duration:.1f}s")
            if self.context:
                print(f"  Final subgoal: {self.context.current_subgoal or '(none)'}")
            print("=" * 70)
            print()

        if self.save_transcript:
            self._save_transcript(finish_reason, duration)

    def _save_transcript(self, finish_reason: str, duration: float):
        """Save transcript to JSON file."""
        os.makedirs(self.transcript_dir, exist_ok=True)

        timestamp = self.start_time.strftime('%Y%m%d-%H%M%S') if self.start_time else 'unknown'
        filename = f"{self.transcript_dir}/run-{self.session_id}-{timestamp}.json"

        transcript = {
            "session_id": self.session_id,
            "goal": self.context.main_goal if self.context else "",
            "start_time": self.start_time.isoformat() if self.start_time else None,
            "finish_reason": finish_reason,
            "duration_seconds": duration,
            "steps": [
                {
                    "step": r.step_num,
                    "timestamp": r.timestamp,
                    "action": r.action,
                    "result": r.result,
                    "success": r.success,
                    "subgoal": r.subgoal,
                    "duration_ms": r.duration_ms,
                    "raw_completion": r.raw_completion
                }
                for r in self.records
            ],
            "final_subgoal": self.context.current_subgoal if self.context else None,
            "action_count": len(self.records)
        }

        with open(filename, 'w') as f:
            json.dump(transcript, f, indent=2)

        if self.verbose:
            print(f"  Transcript saved: {filename}")
            print()

    def print_summary(self):
        """Print a summary of the run."""
        if not self.records:
            print("No steps recorded.")
            return

        successes = sum(1 for r in self.records if r.success)
        failures = len(self.records) - successes
        total_time = sum(r.duration_ms for r in self.records)
        avg_time = total_time / len(self.records) if self.records else 0

        print()
        print("SUMMARY")
        print("-" * 40)
        print(f"  Steps: {len(self.records)} ({successes} success, {failures} failed)")
        print(f"  Total time: {total_time}ms ({avg_time:.0f}ms avg)")
        print()
        print("ACTION HISTORY:")
        for r in self.records:
            status = "✓" if r.success else "✗"
            action_short = r.action[:50] + "..." if len(r.action) > 50 else r.action
            print(f"  {r.step_num}. {status} {action_short}")
        print()


def run_observed(
    context: AgentContext,
    api_client,
    evaluator=None,
    config=None,
    verbose: bool = True,
    save_transcript: bool = True
):
    """
    Convenience function to run an agent loop with full observability.

    Returns (LoopResult, Observer).
    """
    from .loop import run_loop, LoopConfig
    from .step import fold_evaluator

    evaluator = evaluator or fold_evaluator
    config = config or LoopConfig()

    observer = Observer(
        verbose=verbose,
        save_transcript=save_transcript
    )
    observer.begin(context)

    gen = run_loop(
        context=context,
        api_client=api_client,
        evaluator=evaluator,
        config=config,
        on_step=observer.on_step
    )

    # Consume generator and capture return value
    loop_result = None
    try:
        while True:
            next(gen)
    except StopIteration as e:
        loop_result = e.value

    finish_reason = loop_result.finish_reason if loop_result else "unknown"
    observer.end(finish_reason)

    return loop_result, observer
