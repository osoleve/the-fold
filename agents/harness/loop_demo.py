#!/usr/bin/env python3
"""
agents/harness/loop_demo.py — Demo of the agent loop with a real LLM

Runs multiple steps using Gemini Flash to explore The Fold and complete a task.
"""

import subprocess
import sys
import os

# Add parent to path for imports
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.dirname(__file__))))

from agents.harness import (
    run_loop,
    LoopConfig,
    AgentContext,
)


def gemini_flash_client(prompt: str) -> str:
    """Call Gemini Flash via CLI."""
    # Prepend role and add harness command instructions
    augmented_prompt = """You are an autonomous Scheme agent in The Fold REPL.

HARNESS COMMANDS (handled locally, not sent to Fold):
  (subgoal "description")  ; Set your current subgoal
  (done)                   ; Signal task completion

CRITICAL: Respond with ONLY a single S-expression. No text before or after.
Example responses: (channels), (browse 'engineering 3), (done)

""" + prompt

    result = subprocess.run(
        ["gemini", "-m", "gemini-3-flash-preview", augmented_prompt],
        capture_output=True,
        text=True,
        timeout=60,
        cwd="/home/oso/the-fold"
    )

    if result.returncode != 0:
        raise Exception(f"Gemini error: {result.stderr}")

    return result.stdout.strip()


def main():
    print("=" * 60)
    print("Agent Loop Demo")
    print("=" * 60)

    # Create agent context
    context = AgentContext(
        session_id="loop-demo",
        main_goal="Explore The Fold forum, find the most active channel, and summarize one interesting post from it",
        current_subgoal=None,  # Let the agent decide
        recent_actions=[],
        environment_notes=[
            "You are an autonomous agent exploring The Fold",
            "Use (subgoal \"...\") to set intermediate goals",
            "Use (done) when you have completed the main goal",
        ]
    )

    config = LoopConfig(
        max_steps=10,
        stop_on_done=True,
        detect_loops=True,
        loop_window=3
    )

    print(f"\nGoal: {context.main_goal}")
    print(f"Max steps: {config.max_steps}")
    print("-" * 60)

    # Run the loop
    step_num = 0
    loop_result = None

    gen = run_loop(
        context=context,
        api_client=gemini_flash_client,
        config=config
    )

    # Manually iterate to capture both results and the generator's return value
    try:
        while True:
            result = next(gen)
            step_num += 1
            status = "+" if result.eval_success else "-"

            print(f"\n[Step {step_num}] {status}")
            print(f"  Action: {result.parsed_action}")
            if context.recent_actions:
                # Display from ActionRecord which includes output fallback
                last_result = context.recent_actions[-1].result
                if last_result:
                    truncated = last_result[:200] + "..." if len(last_result) > 200 else last_result
                    # Show first line for readability
                    first_line = truncated.split('\n')[0]
                    print(f"  Result: {first_line}")
            if context.current_subgoal:
                print(f"  Subgoal: {context.current_subgoal}")
            print(f"  Time: {result.metrics.total_duration_ms}ms")
    except StopIteration as e:
        loop_result = e.value

    print("\n" + "=" * 60)
    print("LOOP COMPLETE")
    print("=" * 60)
    print(f"Finish reason: {loop_result.finish_reason if hasattr(loop_result, 'finish_reason') else 'unknown'}")
    print(f"Steps completed: {step_num}")
    print(f"Final subgoal: {context.current_subgoal}")
    print(f"Actions taken: {len(context.recent_actions)}")

    if context.recent_actions:
        print("\nAction history:")
        for i, action in enumerate(context.recent_actions, 1):
            status = "+" if action.success else "-"
            print(f"  {i}. {status} {action.expression[:60]}")

    return True


if __name__ == "__main__":
    os.chdir("/home/oso/the-fold")
    success = main()
    sys.exit(0 if success else 1)
